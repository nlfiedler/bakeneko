//
// Copyright 2012-2013 Nathan Fiedler. All rights reserved.
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

// verifyLexerResults calls lex() and checks that the resulting tokens
// match the expected results.
func verifyLexerResults(t *testing.T, input string, expected []expectedLexerResult) {
	c := lex("unit", input)
	failed := false
	for i, e := range expected {
		tok, ok := <-c
		if !ok {
			t.Errorf("lexer channel closed?")
		}
		if tok.typ != e.typ {
			t.Errorf("expected %d, got %d for '%s' (token %d)", e.typ, tok.typ, e.val, i)
			failed = true
		}
		if tok.val != e.val {
			t.Errorf("expected '%s', got '%s' (token %d, type %d)", e.val, tok.val, i, e.typ)
			failed = true
		}
		// stop now since the rest of the inputs will also fail anyway
		if failed {
			break
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
func verifyLexerErrors(t *testing.T, input map[string]string) {
	for i, e := range input {
		c := lex("unit", i)
		tok, ok := <-c
		if !ok {
			t.Errorf("lexer channel closed?")
		}
		if tok.typ != tokenError {
			t.Errorf("expected '%s' to fail with '%s'", i, e)
		}
		if len(e) > 0 && !strings.Contains(tok.val, e) {
			t.Errorf("expected '%s' but got '%s'(%d) for input '%s'", e, tok.val, tok.typ, i)
		}
		drainLexer(c)
	}
}

func TestLexerComments(t *testing.T) {
	input := `; foo
; bar baz
; quux
(foo 1 2)
#| hey diddle diddle
   #| the cat and the fiddle |#
   the cow jumped over the moon |#
(bar 3 4)
#; (quote 1)
`
	expected := make([]expectedLexerResult, 0)
	expected = append(expected, expectedLexerResult{tokenOpenParen, "("})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "foo"})
	expected = append(expected, expectedLexerResult{tokenInteger, "1"})
	expected = append(expected, expectedLexerResult{tokenInteger, "2"})
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	expected = append(expected, expectedLexerResult{tokenOpenParen, "("})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "bar"})
	expected = append(expected, expectedLexerResult{tokenInteger, "3"})
	expected = append(expected, expectedLexerResult{tokenInteger, "4"})
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	expected = append(expected, expectedLexerResult{tokenComment, "#; "})
	expected = append(expected, expectedLexerResult{tokenOpenParen, "("})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "quote"})
	expected = append(expected, expectedLexerResult{tokenInteger, "1"})
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
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
	input := make(map[string]string)
	input[`"foo`] = "unclosed quoted string"
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
	input := `#\a #\space #\newline #\t
	#\alarm #\backspace #\delete #\escape #\null #\return #\tab`
	expected := make([]expectedLexerResult, 0)
	expected = append(expected, expectedLexerResult{tokenCharacter, "#\\a"})
	expected = append(expected, expectedLexerResult{tokenCharacter, "#\\ "})
	expected = append(expected, expectedLexerResult{tokenCharacter, "#\\\n"})
	expected = append(expected, expectedLexerResult{tokenCharacter, "#\\t"})
	expected = append(expected, expectedLexerResult{tokenCharacter, "#\\\a"})
	expected = append(expected, expectedLexerResult{tokenCharacter, "#\\\b"})
	expected = append(expected, expectedLexerResult{tokenCharacter, "#\\\u007f"})
	expected = append(expected, expectedLexerResult{tokenCharacter, "#\\\u001b"})
	expected = append(expected, expectedLexerResult{tokenCharacter, "#\\\u0000"})
	expected = append(expected, expectedLexerResult{tokenCharacter, "#\\\r"})
	expected = append(expected, expectedLexerResult{tokenCharacter, "#\\\t"})
	verifyLexerResults(t, input, expected)
}

func TestLexerBooleans(t *testing.T) {
	input := "#t #f #true #false"
	expected := make([]expectedLexerResult, 0)
	expected = append(expected, expectedLexerResult{tokenBoolean, "#t"})
	expected = append(expected, expectedLexerResult{tokenBoolean, "#f"})
	expected = append(expected, expectedLexerResult{tokenBoolean, "#true"})
	expected = append(expected, expectedLexerResult{tokenBoolean, "#false"})
	verifyLexerResults(t, input, expected)
}

func TestLexerBadBooleans(t *testing.T) {
	input := make(map[string]string)
	input["#truu"] = "invalid boolean"
	input["#Falsa"] = "invalid boolean"
	input["#Tree"] = "invalid boolean"
	input["#falls"] = "invalid boolean"
	input["#Fawlty"] = "invalid boolean"
	input["#Trooper"] = "invalid boolean"
	verifyLexerErrors(t, input)
}

func TestLexerVector(t *testing.T) {
	input := "'#(1 2 3)"
	expected := make([]expectedLexerResult, 0)
	expected = append(expected, expectedLexerResult{tokenQuote, "'"})
	expected = append(expected, expectedLexerResult{tokenVector, "#("})
	expected = append(expected, expectedLexerResult{tokenInteger, "1"})
	expected = append(expected, expectedLexerResult{tokenInteger, "2"})
	expected = append(expected, expectedLexerResult{tokenInteger, "3"})
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	verifyLexerResults(t, input, expected)
}

func TestLexerByteVector(t *testing.T) {
	input := "#u8(1 2 3)"
	expected := make([]expectedLexerResult, 0)
	expected = append(expected, expectedLexerResult{tokenByteVector, "#u8("})
	expected = append(expected, expectedLexerResult{tokenInteger, "1"})
	expected = append(expected, expectedLexerResult{tokenInteger, "2"})
	expected = append(expected, expectedLexerResult{tokenInteger, "3"})
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	verifyLexerResults(t, input, expected)
}

func TestLexerBadByteVector(t *testing.T) {
	input := make(map[string]string)
	input["#u(0 1 2)"] = "unrecognized hash value"
	input["#u8"] = "unrecognized hash value"
	verifyLexerErrors(t, input)
}

func TestLexerDatumLabels(t *testing.T) {
	input := "#1=(foo bar #0# baz) #1#"
	expected := make([]expectedLexerResult, 0)
	expected = append(expected, expectedLexerResult{tokenLabelDefinition, "#1="})
	expected = append(expected, expectedLexerResult{tokenOpenParen, "("})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "foo"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "bar"})
	expected = append(expected, expectedLexerResult{tokenLabelReference, "#0#"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "baz"})
	expected = append(expected, expectedLexerResult{tokenCloseParen, ")"})
	expected = append(expected, expectedLexerResult{tokenLabelReference, "#1#"})
	verifyLexerResults(t, input, expected)
}

func TestLexerBadDatumLabels(t *testing.T) {
	input := make(map[string]string)
	input["#1a"] = "unrecognized hash value"
	input["#11"] = "unrecognized hash value"
	verifyLexerErrors(t, input)
}

func TestLexerBadCharacter(t *testing.T) {
	input := make(map[string]string)
	input["#\\abc"] = "malformed character escape"
	input["#\\0"] = "malformed character escape"
	input["#\\a1"] = "malformed character escape"
	input["#\\nw"] = "malformed character escape"
	input["#\\sa"] = "malformed character escape"
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
	input := make(map[string]string)
	input["0.a"] = "malformed number"
	input["0a"] = "malformed number"
	input["#dabc"] = "malformed number"
	input["#o888"] = "malformed number"
	input["#b123"] = "malformed number"
	input["#xzyw"] = "malformed number"
	input["#b#b00"] = "malformed number prefix"
	input["#d#d00"] = "malformed number prefix"
	input["#e#e00"] = "malformed number prefix"
	input["#i#i00"] = "malformed number prefix"
	input["#o#o00"] = "malformed number prefix"
	input["#x#x00"] = "malformed number prefix"
	input["#b#d00"] = "malformed number prefix"
	input["#b#o00"] = "malformed number prefix"
	input["#b#x00"] = "malformed number prefix"
	input["#d#d00"] = "malformed number prefix"
	input["#d#o00"] = "malformed number prefix"
	input["#d#x00"] = "malformed number prefix"
	input["#o#b00"] = "malformed number prefix"
	input["#o#d00"] = "malformed number prefix"
	input["#o#x00"] = "malformed number prefix"
	input["#x#b00"] = "malformed number prefix"
	input["#x#d00"] = "malformed number prefix"
	input["#x#o00"] = "malformed number prefix"
	input["#e#i00"] = "malformed number prefix"
	input["#i#e00"] = "malformed number prefix"
	verifyLexerErrors(t, input)
}

func TestLexerIdentifiers(t *testing.T) {
	input := `lambda q list->vector +soup+ + V17a <=? a34kTMNs |two words|
 two\x20;words the-word-recursion-has-many-meanings - . ... || |foo @#$! bar|`
	expected := make([]expectedLexerResult, 0)
	expected = append(expected, expectedLexerResult{tokenIdentifier, "lambda"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "q"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "list->vector"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "+soup+"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "+"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "V17a"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "<=?"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "a34kTMNs"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "|two words|"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, `two words`})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "the-word-recursion-has-many-meanings"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "-"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "."})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "..."})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "||"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "|foo @#$! bar|"})
	verifyLexerResults(t, input, expected)
}

func TestLexerFoldCase(t *testing.T) {
	input := `#!fold-case lAMbdA
	#!no-fold-case
	lAMbdA
	#!fold-case
	LAMBDA
	#!no-fold-case
	lamBDA`
	expected := make([]expectedLexerResult, 0)
	expected = append(expected, expectedLexerResult{tokenIdentifier, "lambda"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "lAMbdA"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "lambda"})
	expected = append(expected, expectedLexerResult{tokenIdentifier, "lamBDA"})
	verifyLexerResults(t, input, expected)
}

func TestLexerBadIdentifiers(t *testing.T) {
	input := make(map[string]string)
	input[".a"] = "malformed identifier"
	input[".. "] = "malformed identifier"
	input["...a"] = "malformed identifier"
	input[".... "] = "malformed identifier"
	input["a\\xXYZ;"] = "invalid number"
	input["a|b"] = "invalid subsequent character"
	input["a\\bc"] = "missing 'x' after '\\'"
	input["|no\\allowed|"] = "not allowed in identifier"
	input["@no"] = "character not allowed to start identifier"
	verifyLexerErrors(t, input)
}
