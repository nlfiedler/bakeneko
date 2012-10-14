//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

import (
	"strings"
	"testing"
)

// expectedLexerResult is equivalent to a token and is used in comparing the
// results from the lexer.
type expectedLexerResult struct {
	typ tokenType
	val string
}

type expectedLexerError struct {
	err string // expected error message substring
	msg string // explanation of error condition
}

// verifyLexerResults calls lex() and checks that the resulting tokens
// match the expected results.
func verifyLexerResults(t *testing.T, input string, expected []expectedLexerResult) {
	c := lex("unit", input)
	for i, e := range expected {
		tok, ok := <-c
		if !ok {
			t.Errorf("lexer channel closed?")
		}
		if tok.typ != e.typ {
			t.Errorf("expected %d, got %d for '%s' (token %d)", e.typ, tok.typ, e.val, i)
		}
		if tok.val != e.val {
			t.Errorf("expected '%s', got '%s' (token %d, type %d)", e.val, tok.val, i, e.typ)
		}
	}
	drainLexer(c)
}

// verifyLexerResultsMap calls lex() for each key in the map and checks
// that the resulting tokens match the expected results.
func verifyLexerResultsMap(t *testing.T, expected map[string]tokenType) {
	for input, tipe := range expected {
		c := lex("unit", input)
		tok, ok := <-c
		if !ok {
			t.Errorf("lexer channel closed?")
		}
		if tok.typ != tipe {
			t.Errorf("expected %d, got %d for '%s'", tipe, tok.typ, input)
		}
		if tok.val != input {
			t.Errorf("expected '%s', got '%s' (type %d)", input, tok.val, tipe)
		}
		drainLexer(c)
	}
}

// verifyLexerErrors calls lex() and checks that the resulting tokens
// resulted in an error, and (optionally) verifies the error message.
func verifyLexerErrors(t *testing.T, input map[string]expectedLexerError) {
	for i, e := range input {
		c := lex("unit", i)
		tok, ok := <-c
		if !ok {
			t.Errorf("lexer channel closed?")
		}
		if tok.typ != tokenError {
			t.Errorf("expected '%s' to fail with '%s'", i, e.err)
		}
		if len(e.err) > 0 && !strings.Contains(tok.val, e.err) {
			t.Errorf("expected '%s' but got '%s'(%d) for input '%s'", e.err, tok.val, tok.typ, i)
		}
		drainLexer(c)
	}
}

func TestLexerComments(t *testing.T) {
	input := `; foo
; bar baz
; quux
`
	expected := make([]expectedLexerResult, 0)
	expected = append(expected, expectedLexerResult{tokenEOF, ""})
	verifyLexerResults(t, input, expected)
}

func TestLexerSimple(t *testing.T) {
	input := "(set foo bar)"
	expected := make([]expectedLexerResult, 0)
	expected = append(expected, expectedLexerResult{tokenOpenParen, "("})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "set"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "foo"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "bar"})
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	expected = append(expected, expectedLexerResult{tokenEOF, ""})
	verifyLexerResults(t, input, expected)
}

func TestLexerFactorial(t *testing.T) {
	input := `(define fact
    (lambda (n)
     (if (<= n 1)
         1
         (* n (fact (- n 1))))))
`
	expected := make([]expectedLexerResult, 0)
	// 0
	expected = append(expected, expectedLexerResult{tokenOpenParen, "("})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "define"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "fact"})
	expected = append(expected, expectedLexerResult{tokenOpenParen, "("})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "lambda"})
	// 5
	expected = append(expected, expectedLexerResult{tokenOpenParen, "("})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "n"})
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	expected = append(expected, expectedLexerResult{tokenOpenParen, "("})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "if"})
	// 10
	expected = append(expected, expectedLexerResult{tokenOpenParen, "("})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "<="})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "n"})
	expected = append(expected, expectedLexerResult{tokenInteger, "1"})
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	// 15
	expected = append(expected, expectedLexerResult{tokenInteger, "1"})
	expected = append(expected, expectedLexerResult{tokenOpenParen, "("})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "*"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "n"})
	expected = append(expected, expectedLexerResult{tokenOpenParen, "("})
	// 20
	expected = append(expected, expectedLexerResult{tokenIdentifier, "fact"})
	expected = append(expected, expectedLexerResult{tokenOpenParen, "("})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "-"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "n"})
	expected = append(expected, expectedLexerResult{tokenInteger, "1"})
	// 25
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	// 30
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	expected = append(expected, expectedLexerResult{tokenEOF, ""})
	verifyLexerResults(t, input, expected)
}

func TestLexerUnclosedQuotes(t *testing.T) {
	input := make(map[string]expectedLexerError)
	input[`"foo`] = expectedLexerError{"unclosed quoted string", "unclosed quote should fail"}
	verifyLexerErrors(t, input)
}

func TestLexerQuotes(t *testing.T) {
	input := "'(foo bar) `(backtick) ,(baz qux) ,@(commat)"
	expected := make([]expectedLexerResult, 0)
	expected = append(expected, expectedLexerResult{tokenQuote, "'"})
	expected = append(expected, expectedLexerResult{tokenOpenParen, "("})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "foo"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "bar"})
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	expected = append(expected, expectedLexerResult{tokenQuote, "`"})
	expected = append(expected, expectedLexerResult{tokenOpenParen, "("})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "backtick"})
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	expected = append(expected, expectedLexerResult{tokenQuote, ","})
	expected = append(expected, expectedLexerResult{tokenOpenParen, "("})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "baz"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "qux"})
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	expected = append(expected, expectedLexerResult{tokenQuote, ",@"})
	expected = append(expected, expectedLexerResult{tokenOpenParen, "("})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "commat"})
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	verifyLexerResults(t, input, expected)
}

func TestLexerCharacters(t *testing.T) {
	input := "#\\a #\\space #\\newline #\\t"
	expected := make([]expectedLexerResult, 0)
	expected = append(expected, expectedLexerResult{tokenCharacter, "#\\a"})
	expected = append(expected, expectedLexerResult{tokenCharacter, "#\\ "})
	expected = append(expected, expectedLexerResult{tokenCharacter, "#\\\n"})
	expected = append(expected, expectedLexerResult{tokenCharacter, "#\\t"})
	verifyLexerResults(t, input, expected)
}

func TestLexerVector(t *testing.T) {
	input := "'#(1 2 3)"
	expected := make([]expectedLexerResult, 0)
	expected = append(expected, expectedLexerResult{tokenQuote, "'"})
	expected = append(expected, expectedLexerResult{tokenStartVector, "#("})
	expected = append(expected, expectedLexerResult{tokenInteger, "1"})
	expected = append(expected, expectedLexerResult{tokenInteger, "2"})
	expected = append(expected, expectedLexerResult{tokenInteger, "3"})
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	verifyLexerResults(t, input, expected)
}

func TestLexerBadCharacter(t *testing.T) {
	input := make(map[string]expectedLexerError)
	input["#\\abc"] = expectedLexerError{"malformed character escape", "invalid char escape should fail"}
	input["#\\0"] = expectedLexerError{"malformed character escape", "invalid char escape should fail"}
	input["#\\a1"] = expectedLexerError{"malformed character escape", "invalid char escape should fail"}
	input["#\\nw"] = expectedLexerError{"malformed character escape", "invalid char escape should fail"}
	input["#\\sa"] = expectedLexerError{"malformed character escape", "invalid char escape should fail"}
	verifyLexerErrors(t, input)
}

func TestLexerNumbers(t *testing.T) {
	input := make(map[string]tokenType)
	input[".01"] = tokenFloat
	input["0"] = tokenInteger
	input["0.1"] = tokenFloat
	input["1.00"] = tokenFloat
	// XXX: weird case, returns as tokenFloat '0.0'
	// input["0.0.0"] = tokenFloat
	input["123"] = tokenInteger
	input["6e4"] = tokenFloat
	input["7.91e+16"] = tokenFloat
	input["#d1234"] = tokenInteger
	input["#d#e1234"] = tokenInteger
	input["#o366"] = tokenInteger
	input["#i#o366"] = tokenInteger
	input["#x7b5"] = tokenInteger
	input["#b01010100"] = tokenInteger
	input["3."] = tokenFloat
	input["6/10"] = tokenRational
	input["15##"] = tokenInteger
	input["12#.###"] = tokenFloat
	input["1.2345e"] = tokenFloat
	input["1.2345s"] = tokenFloat
	input["1.2345f"] = tokenFloat
	input["1.2345d"] = tokenFloat
	input["1.2345l"] = tokenFloat
	input["3+4i"] = tokenComplex
	input["3.0+4.0i"] = tokenComplex
	input["3.0@4.0"] = tokenComplex
	input["3.0-4.0i"] = tokenComplex
	input["-4.0i"] = tokenComplex
	input["+4.0i"] = tokenComplex
	input["3.0-i"] = tokenComplex
	input["3.0+i"] = tokenComplex
	input["-i"] = tokenComplex
	input["+i"] = tokenComplex
	verifyLexerResultsMap(t, input)
}

func TestLexerBadNumbers(t *testing.T) {
	input := make(map[string]expectedLexerError)
	input["0.a"] = expectedLexerError{"malformed number", "invalid number should fail"}
	input["0a"] = expectedLexerError{"malformed number", "invalid number should fail"}
	input["#dabc"] = expectedLexerError{"malformed number", "invalid number should fail"}
	input["#o888"] = expectedLexerError{"malformed number", "invalid number should fail"}
	input["#b123"] = expectedLexerError{"malformed number", "invalid number should fail"}
	input["#xzyw"] = expectedLexerError{"malformed number", "invalid number should fail"}
	input["#b#b00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#d#d00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#e#e00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#i#i00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#o#o00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#x#x00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#b#d00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#b#o00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#b#x00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#d#d00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#d#o00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#d#x00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#o#b00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#o#d00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#o#x00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#x#b00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#x#d00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#x#o00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#e#i00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	input["#i#e00"] = expectedLexerError{"malformed number prefix", "invalid number prefix should fail"}
	verifyLexerErrors(t, input)
}

func TestLexerIdentifiers(t *testing.T) {
	input := "lambda list->vector q soup V17a + <=? a34kTMNs the-word-recursion-has-many-meanings - . ... "
	expected := make([]expectedLexerResult, 0)
	expected = append(expected, expectedLexerResult{tokenIdentifier, "lambda"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "list->vector"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "q"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "soup"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "V17a"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "+"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "<=?"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "a34kTMNs"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "the-word-recursion-has-many-meanings"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "-"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "."})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "..."})
	verifyLexerResults(t, input, expected)
}

func TestLexerBadIdentifiers(t *testing.T) {
	input := make(map[string]expectedLexerError)
	input[".a"] = expectedLexerError{"malformed identifier", "invalid identifier should fail"}
	input["+a"] = expectedLexerError{"malformed identifier", "invalid identifier should fail"}
	input["-a"] = expectedLexerError{"malformed identifier", "invalid identifier should fail"}
	input[".. "] = expectedLexerError{"malformed identifier", "invalid identifier should fail"}
	input["...a"] = expectedLexerError{"malformed identifier", "invalid identifier should fail"}
	input[".... "] = expectedLexerError{"malformed identifier", "invalid identifier should fail"}
	verifyLexerErrors(t, input)
}
