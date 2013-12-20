//
// Copyright 2012-2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	gc "launchpad.net/gocheck"
	"strings"
	"testing"
)

// TestCore hooks up gocheck into the 'go test' runner. This registration is done
// exactly once per package. Suites are registered via gocheck.Suite().
func TestCore(t *testing.T) {
	gc.TestingT(t)
}

type LexerSuite struct {
}

var _ = gc.Suite(&LexerSuite{})

type LocationResult struct {
	char rune // expected character
	row  int  // expected row value
	col  int  // expected column value
}

func locationChecker(c *gc.C, l *lexer, expected []LocationResult) {
	for i, e := range expected {
		r := l.next()
		cm := gc.Commentf("result #%d (%q, %d:%d)", i, r, l.row, l.col)
		c.Assert(r, gc.Equals, e.char, cm)
		c.Assert(l.row, gc.Equals, e.row, cm)
		c.Assert(l.col, gc.Equals, e.col, cm)
	}
}

// TestRowCol ensures that the lexer updates the row and col fields correctly
// as next(), ignore(), backup(), and rewind() are called.
func (s *LexerSuite) TestRowCol(c *gc.C) {
	// TODO: reformulate this with \r\n and \r (and \n)
	input := `foo

bar baz
123
(a #\a #\space)
(cons 1 2)
`
	l := &lexer{
		name:   "unit",
		input:  input,
		tokens: make(chan token),
		row:    1,
	}
	expected := make([]LocationResult, 0)
	line := 1
	expected = append(expected, LocationResult{'f', line, 1})
	expected = append(expected, LocationResult{'o', line, 2})
	expected = append(expected, LocationResult{'o', line, 3})
	line++
	expected = append(expected, LocationResult{'\n', line, 0})
	locationChecker(c, l, expected)

	l.backup()
	expected = make([]LocationResult, 0)
	expected = append(expected, LocationResult{'\n', line, 0})
	line++
	expected = append(expected, LocationResult{'\n', line, 0})
	expected = append(expected, LocationResult{'b', line, 1})
	expected = append(expected, LocationResult{'a', line, 2})
	locationChecker(c, l, expected)

	l.backup()
	c.Assert(l.row, gc.Equals, line, gc.Commentf("after backup"))
	c.Assert(l.col, gc.Equals, 1, gc.Commentf("after backup"))
	expected = make([]LocationResult, 0)
	expected = append(expected, LocationResult{'a', line, 2})
	locationChecker(c, l, expected)

	// starting all over again...
	l.rewind()
	c.Assert(l.row, gc.Equals, 1, gc.Commentf("after first rewind"))
	c.Assert(l.col, gc.Equals, 0, gc.Commentf("after first rewind"))

	line = 1
	expected = make([]LocationResult, 0)
	expected = append(expected, LocationResult{'f', line, 1})
	expected = append(expected, LocationResult{'o', line, 2})
	expected = append(expected, LocationResult{'o', line, 3})
	line++
	expected = append(expected, LocationResult{'\n', line, 0})
	line++
	expected = append(expected, LocationResult{'\n', line, 0})
	expected = append(expected, LocationResult{'b', line, 1})
	expected = append(expected, LocationResult{'a', line, 2})
	expected = append(expected, LocationResult{'r', line, 3})
	expected = append(expected, LocationResult{' ', line, 4})
	expected = append(expected, LocationResult{'b', line, 5})
	expected = append(expected, LocationResult{'a', line, 6})
	expected = append(expected, LocationResult{'z', line, 7})
	line++
	expected = append(expected, LocationResult{'\n', line, 0})
	expected = append(expected, LocationResult{'1', line, 1})
	expected = append(expected, LocationResult{'2', line, 2})
	expected = append(expected, LocationResult{'3', line, 3})
	line++
	expected = append(expected, LocationResult{'\n', line, 0})
	locationChecker(c, l, expected)

	l.ignore()
	expected = make([]LocationResult, 0)
	expected = append(expected, LocationResult{'(', line, 1})
	expected = append(expected, LocationResult{'a', line, 2})
	expected = append(expected, LocationResult{' ', line, 3})
	expected = append(expected, LocationResult{'#', line, 4})
	expected = append(expected, LocationResult{'\\', line, 5})
	expected = append(expected, LocationResult{'a', line, 6})
	expected = append(expected, LocationResult{' ', line, 7})
	locationChecker(c, l, expected)

	// after getting (a ...), backup, rewind, and then verify row/col
	l.backup()
	l.rewind()
	c.Assert(l.row, gc.Equals, line, gc.Commentf("rewind after #\\a"))
	c.Assert(l.col, gc.Equals, 0, gc.Commentf("rewind after #\\a"))
	expected = make([]LocationResult, 0)
	expected = append(expected, LocationResult{'(', line, 1})
	expected = append(expected, LocationResult{'a', line, 2})
	expected = append(expected, LocationResult{' ', line, 3})
	expected = append(expected, LocationResult{'#', line, 4})
	expected = append(expected, LocationResult{'\\', line, 5})
	expected = append(expected, LocationResult{'a', line, 6})
	expected = append(expected, LocationResult{' ', line, 7})
	locationChecker(c, l, expected)
}

func (s *LexerSuite) TestRowColDefine(c *gc.C) {
	input := `(define #\z "abc")`
	l := &lexer{
		name:   "unit",
		input:  input,
		tokens: make(chan token),
		row:    1,
	}
	expected := make([]LocationResult, 0)
	line := 1
	expected = append(expected, LocationResult{'(', line, 1})
	expected = append(expected, LocationResult{'d', line, 2})
	expected = append(expected, LocationResult{'e', line, 3})
	expected = append(expected, LocationResult{'f', line, 4})
	expected = append(expected, LocationResult{'i', line, 5})
	expected = append(expected, LocationResult{'n', line, 6})
	expected = append(expected, LocationResult{'e', line, 7})
	expected = append(expected, LocationResult{' ', line, 8})
	expected = append(expected, LocationResult{'#', line, 9})
	expected = append(expected, LocationResult{'\\', line, 10})
	expected = append(expected, LocationResult{'z', line, 11})
	expected = append(expected, LocationResult{' ', line, 12})
	expected = append(expected, LocationResult{'"', line, 13})
	expected = append(expected, LocationResult{'a', line, 14})
	expected = append(expected, LocationResult{'b', line, 15})
	expected = append(expected, LocationResult{'c', line, 16})
	expected = append(expected, LocationResult{'"', line, 17})
	expected = append(expected, LocationResult{')', line, 18})
	locationChecker(c, l, expected)
}

func (s *LexerSuite) TestLexerTokenLocation(c *gc.C) {
	input := `(define #\z "abc" .123 +123)`
	ch := lex("unit", input)

	type LocationStrResult struct {
		typ tokenType // expected token type
		val string    // expected string
		row int       // expected row value
		col int       // expected column value
	}
	checker := func(expected []LocationStrResult) {
		for i, e := range expected {
			tok, ok := <-ch
			if !ok {
				c.Errorf("reached end of lexer output, expected %s", e)
			}
			cm := gc.Commentf("result #%d (%s, %d:%d)", i, tok.val, tok.row, tok.col)
			c.Assert(tok.val, gc.Equals, e.val, cm)
			c.Assert(tok.row, gc.Equals, e.row, cm)
			c.Assert(tok.col, gc.Equals, e.col, cm)
		}
	}

	expected := make([]LocationStrResult, 0)
	line := 1
	expected = append(expected, LocationStrResult{tokenOpenParen, "(", line, 1})
	expected = append(expected, LocationStrResult{tokenIdentifier, "define", line, 7})
	expected = append(expected, LocationStrResult{tokenCharacter, "#\\z", line, 11})
	expected = append(expected, LocationStrResult{tokenString, `"abc"`, line, 17})
	expected = append(expected, LocationStrResult{tokenFloat, ".123", line, 22})
	expected = append(expected, LocationStrResult{tokenFloat, "+123", line, 27})
	expected = append(expected, LocationStrResult{tokenCloseParen, ")", line, 28})
	checker(expected)
	drainLexer(ch)
}

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
	expected = append(expected, expectedLexerResult{tokenEOF, ""})
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
	expected = append(expected, expectedLexerResult{tokenEOF, ""})
	verifyLexerResults(t, input, expected)
}

func TestLexerBooleans(t *testing.T) {
	input := "#t #f #true #false"
	expected := make([]expectedLexerResult, 0)
	expected = append(expected, expectedLexerResult{tokenBoolean, "#t"})
	expected = append(expected, expectedLexerResult{tokenBoolean, "#f"})
	expected = append(expected, expectedLexerResult{tokenBoolean, "#true"})
	expected = append(expected, expectedLexerResult{tokenBoolean, "#false"})
	expected = append(expected, expectedLexerResult{tokenEOF, ""})
	verifyLexerResults(t, input, expected)
}

func TestLexerBadBooleans(t *testing.T) {
	input := make(map[string]string)
	input["#truu"] = "invalid boolean"
	input["#Falsa"] = "invalid boolean"
	input["#Tree"] = "invalid boolean"
	input["#falls"] = "invalid boolean"
	// These are a good test cases because they are pathological and have
	// caused the lexer to loop indefinitely.
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
	expected = append(expected, expectedLexerResult{tokenEOF, ""})
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
	expected = append(expected, expectedLexerResult{tokenEOF, ""})
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
	expected = append(expected, expectedLexerResult{tokenEOF, ""})
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
	expected = append(expected, expectedLexerResult{tokenEOF, ""})
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
	expected = append(expected, expectedLexerResult{tokenEOF, ""})
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
