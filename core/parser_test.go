//
// Copyright 2012-2014 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	"fmt"
	"io/ioutil"
	gc "launchpad.net/gocheck"
	"os"
	"strings"
	"testing"
)

type ParserSuite struct {
}

var _ = gc.Suite(&ParserSuite{})

func verifyExpandMap(mapping map[string]string, c *gc.C) {
	for input, expected := range mapping {
		parser := NewParser()
		pair, err := parser.Parse(input)
		c.Assert(err, gc.IsNil, gc.Commentf("failed to parse %q: %s", input, err))
		x, err := parser.Expand(pair.First())
		c.Assert(err, gc.IsNil, gc.Commentf("failed to expand %q: %s", input, err))
		c.Check(stringify(x), gc.Equals, expected)
	}
}

func verifyExpandError(c *gc.C, expected map[string]string) {
	for input, expected := range expected {
		parser := NewParser()
		pair, err := parser.Parse(input)
		if err != nil {
			c.Errorf("failed to parse %q: %s", input, err)
		} else {
			_, err = parser.Expand(pair)
			c.Check(err, gc.ErrorMatches, expected)
		}
	}
}

func verifyParse(input, expected string, t *testing.T) {
	parser := NewParser()
	result, err := parser.Parse(input)
	if err != nil {
		msg := fmt.Sprintf("failed to parse expression '%s', %s", input, err)
		t.Errorf(msg)
	} else {
		actual := stringify(result)
		if actual != expected {
			t.Errorf("expected <<%s>>, but got <<%s>>", expected, actual)
		}
	}
}

func verifyParseMap(mapping map[string]string, t *testing.T) {
	for input, expected := range mapping {
		verifyParse(input, expected, t)
	}
}

func verifyParseError(t *testing.T, expected map[string]string) {
	for input, errmsg := range expected {
		parser := NewParser()
		result, err := parser.Parse(input)
		if err == nil {
			t.Errorf("parse() of %q should have failed, got %q", input, result)
		} else if !strings.Contains(err.String(), errmsg) {
			t.Errorf("expected [%s] but got [%s] for input %q", errmsg, err, input)
		}
	}
}

func TestParseExprEmptyList(t *testing.T) {
	input := "()"
	expected := "(())"
	verifyParse(input, expected, t)
}

func TestParseExprSingletonList(t *testing.T) {
	input := "(foo)"
	expected := "((foo))"
	verifyParse(input, expected, t)
}

func TestParseExprList(t *testing.T) {
	mapping := make(map[string]string)
	// Input/Output tested with MIT/GNU Scheme (prior to simplifying results)
	mapping["(foo  bar    baz)"] = "((foo bar baz))"
	mapping["(0 . 1)"] = "((0 . 1))"
	mapping["(0 1)"] = "((0 1))"
	mapping["(0 . (1 . ()))"] = "((0 1))"
	mapping["(0 . (1 . 2))"] = "((0 1 . 2))"
	verifyParseMap(mapping, t)
}

func TestParseExprNestedList(t *testing.T) {
	input := `(foo
  (bar
    baz))`
	expected := "((foo (bar baz)))"
	verifyParse(input, expected, t)
}

func TestParseExprBoolean(t *testing.T) {
	input := "( #t #f #true #false )"
	expected := "((#t #f #t #f))"
	verifyParse(input, expected, t)
}

func TestParseExprString(t *testing.T) {
	input := `"foo"`
	expected := `("foo")`
	verifyParse(input, expected, t)
}

func TestParseCharacters(t *testing.T) {
	mapping := make(map[string]string)
	mapping["#\\a"] = "(#\\a)"
	mapping["#\\t"] = "(#\\t)"
	mapping["#\\newline"] = "(#\\newline)"
	mapping["#\\space"] = "(#\\space)"
	mapping["#\\M"] = "(#\\M)"
	mapping["#\\z"] = "(#\\z)"
	verifyParseMap(mapping, t)
}

func TestParseQuotes(t *testing.T) {
	mapping := make(map[string]string)
	mapping["(foo 'x)"] = "((foo (quote x)))"
	mapping["(foo `x)"] = "((foo (quasiquote x)))"
	mapping["(foo ,x)"] = "((foo (unquote x)))"
	mapping["(foo ,@x)"] = "((foo (unquote-splicing x)))"
	mapping["`(list ,(+ 1 2) 4)"] = "((quasiquote (list (unquote (+ 1 2)) 4)))"
	mapping["`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)"] =
		"((quasiquote (a (unquote (+ 1 2)) (unquote-splicing (map abs (quote (4 -5 6)))) b)))"
	mapping["`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)"] =
		"((quasiquote #(10 5 (unquote (sqrt 4)) (unquote-splicing (map sqrt (quote (16 9)))) 8)))"
	verifyParseMap(mapping, t)
}

func TestParseComments(t *testing.T) {
	mapping := make(map[string]string)
	mapping["#;(foo 'x)"] = "()"
	mapping["(bar #;(foo 'x) quux)"] = "((bar quux))"
	mapping["#; (foo 'x)"] = "()"
	mapping[`#1=#\b #; (foo #1=#\a 'x) #1#`] = `(#\b #\b)`
	mapping[`#1=#\b #;#1=#\a #1#`] = `(#\b #\b)`
	mapping["#;()"] = "()"
	mapping["#;(foo (+ 1 2) 'a)"] = "()"
	verifyParseMap(mapping, t)
}

func TestParseVector(t *testing.T) {
	input := "#(1 2 3)"
	parser := NewParser()
	pair, err := parser.Parse(input)
	if err != nil {
		msg := fmt.Sprintf("failed to parse expression '%s', %s", input, err)
		t.Errorf(msg)
	} else {
		result := pair.First()
		if slice, ok := result.(Vector); ok {
			if slice.Len() == 3 {
				expected := new([3]Integer)
				expected[0] = NewInteger(1)
				expected[1] = NewInteger(2)
				expected[2] = NewInteger(3)
				for i, e := range expected {
					thing := slice.Get(i)
					if num, ok := thing.(Integer); ok {
						if eq, _ := num.EqualTo(e); !eq {
							t.Errorf("expected %v in vector, but got %s", e, thing)
						}
					} else {
						t.Errorf("expected Integer but got %T", thing)
					}
				}
			} else {
				t.Errorf("expected slice of length three but got %d", slice.Len())
			}
		} else {
			t.Errorf("expected slice but got %T", result)
		}
	}
	verifyParse(input, fmt.Sprintf("(%s)", input), t)
}

func TestParseByteVector(t *testing.T) {
	input := "#u8(0 10 5)"
	parser := NewParser()
	pair, err := parser.Parse(input)
	if err != nil {
		msg := fmt.Sprintf("failed to parse expression '%s', %s", input, err)
		t.Errorf(msg)
	} else {
		result := pair.First()
		if bv, ok := result.(ByteVector); ok {
			if bv.Len() == 3 {
				expected := new([3]uint8)
				expected[0] = uint8(0)
				expected[1] = uint8(10)
				expected[2] = uint8(5)
				for i, e := range expected {
					if bv.Get(i) != e {
						t.Errorf("expected %v in byte vector, but got %s", e, bv.Get(i))
					}
				}
			} else {
				t.Errorf("expected slice of length three but got %d", bv.Len())
			}
		} else {
			t.Errorf("expected slice but got %T", result)
		}
	}
	verifyParse(input, fmt.Sprintf("(%s)", input), t)
}

func TestParseExprNumbers(t *testing.T) {
	mapping := make(map[string]string)
	mapping["1.2345"] = "(1.2345)"
	mapping[".1"] = "(0.1)"
	mapping["6e4"] = "(60000)"
	mapping["12345"] = "(12345)"
	mapping["2.1"] = "(2.1)"
	// weird test case, not sure this is correct
	mapping["0.0.0"] = "(0 0)"
	mapping["3."] = "(3)"
	mapping["7.91e+16"] = "(7.91e+16)"
	mapping[".000001"] = "(1e-06)"
	mapping["#b11111111"] = "(255)"
	mapping["#o777"] = "(511)"
	mapping["#x4dfCF0"] = "(5111024)"
	mapping["#d12345"] = "(12345)"
	mapping["#d#i12345"] = "(12345)"
	mapping["#d#e12345"] = "(12345)"
	mapping["#i#d12345"] = "(12345)"
	mapping["#e#d12345"] = "(12345)"
	// note that in Go, -0 is the same as 0, so sign will be lost
	mapping["3+4i"] = "(3+4i)"
	mapping["3-4i"] = "(3-4i)"
	mapping["3.0+4.0i"] = "(3+4i)"
	mapping["3.0-4.0i"] = "(3-4i)"
	mapping["3+i"] = "(3+1i)"
	mapping["3-i"] = "(3-1i)"
	mapping["+4i"] = "(0+4i)"
	mapping["-4i"] = "(0-4i)"
	mapping["+i"] = "(0+1i)"
	mapping["-i"] = "(0-1i)"
	mapping["1/1"] = "(1)"
	mapping["1/2"] = "(1/2)"
	mapping["1/3"] = "(1/3)"
	mapping["1/4"] = "(1/4)"
	mapping["3/4"] = "(3/4)"
	mapping["6/10"] = "(3/5)"
	mapping["100/1000"] = "(1/10)"
	verifyParseMap(mapping, t)
}

func (s *ParserSuite) TestExpand(c *gc.C) {
	mapping := make(map[string]string)
	mapping[`(if #t (display "foo"))`] = `(if #t (display "foo") ())`
	mapping[`(if #t 1 2)`] = `(if #t 1 2)`
	mapping[`((if #t 1 2))`] = `(if #t 1 2)`
	mapping[`(((if #t 1 2)))`] = `(if #t 1 2)`
	mapping[`(quote abc)`] = `(quote abc)`
	mapping[`(set! foo (quote bar))`] = `(set! foo (quote bar))`
	mapping[`(set! foo (if #t (quote bar)))`] = `(set! foo (if #t (quote bar) ()))`
	mapping[`(define (f args) body)`] = `(define f (lambda (args) (begin body)))`
	mapping["(define-macro foo (lambda args (if #t (quote bar))))"] =
		"(define-macro foo (lambda (args) (begin (if #t (quote bar) ()))))"
	mapping[`(begin (if #t (display "foo")))`] = `(begin (if #t (display "foo") ()))`
	mapping[`(begin (define foo 123) foo)`] = `(begin (define foo 123) foo)`
	mapping[`(lambda (x) e1)`] = `(lambda (x) (begin e1))`
	mapping[`(lambda (x) e1 e2)`] = `(lambda (x) (begin e1 e2))`
	mapping[`(foo (if #t (quote bar)))`] = `(foo (if #t (quote bar) ()))`
	verifyExpandMap(mapping, c)
}

func (s *ParserSuite) TestExpandQuotes(c *gc.C) {
	mapping := make(map[string]string)
	mapping["(foo 'x)"] = "(foo (quote x))"
	mapping["(foo `x)"] = "(foo (quote x))"
	mapping["(foo `,x)"] = "(foo x)"
	mapping["(foo `(,@x y))"] = "(foo (append x (cons (quote y) (quote ()))))"
	mapping["(foo ,x)"] = "(foo (unquote x))"
	mapping["(foo ,@x)"] = "(foo (unquote-splicing x))"
	mapping["`(list ,(+ 1 2) 4)"] =
		"(cons (quote list) (cons (+ 1 2) (cons (quote 4) (quote ()))))"
	mapping["`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)"] =
		"(cons (quote a) (cons (+ 1 2) (append (map abs (quote (4 -5 6))) (cons (quote b) (quote ())))))"
	mapping["`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)"] =
		"#(cons (quote 10) #(cons (quote 5) #(cons (sqrt 4) (append (map sqrt (quote (16 9))) #(cons (quote 8) (quote #()))))))"
	verifyExpandMap(mapping, c)
}

func (s *ParserSuite) TestExpandErrors(c *gc.C) {
	table := make(map[string]string)
	table["(if)"] = ".*if too many/few arguments.*"
	table["(if bar)"] = ".*if too many/few arguments.*"
	table["(if foo bar baz quux)"] = ".*if too many/few arguments.*"
	table["(set!)"] = ".*set requires 2 arguments.*"
	table["(set! foo)"] = ".*set requires 2 arguments.*"
	table["(set! (foo) bar)"] = ".*can only set! a symbol.*"
	table["(set! bar baz quux)"] = ".*set requires 2 arguments.*"
	table["(quote)"] = ".*quote requires datum.*"
	table["(quote foo bar)"] = ".*quote requires datum.*"
	table["(lambda foo)"] = ".*lambda requires 2\\+ arguments.*"
	table[`(lambda ("foo") bar)`] = ".*lambda arguments must be symbols.*"
	table[`(lambda "foo" bar)`] = ".*lambda arguments must be a list or a symbol.*"
	table["(include)"] = ".*include requires filenames.*"
	table["(include 123)"] = ".*include expects string arguments.*"
	verifyExpandError(c, table)
}

func TestParse(t *testing.T) {
	input := `; this binds x to 5 and yields  10
(let ((x 5)) (* x 2))
; this bind x to 10, z to 5 and yields 50.
(let ((x 10) (z 5)) (* x z))
`
	parser := NewParser()
	result, err := parser.Parse(input)
	if err != nil {
		t.Errorf("failed to parse program: %v", err)
	} else {
		if result.Len() != 2 {
			t.Error("expected two program elements")
		}
		part1 := result.First()
		if let1, ok := part1.(Pair); ok {
			if let1.Len() != 3 {
				t.Error("first let needs three parts")
			}
			part1sub1 := let1.Second()
			if let1args, ok := part1sub1.(Pair); ok {
				if let1args.Len() != 1 {
					t.Error("first let subpart 1 length must be 1")
				}
			} else {
				t.Error("first let subpart 1 not a pair")
			}
		} else {
			t.Error("first let not a pair")
		}
		part2 := result.Second()
		if let2, ok := part2.(Pair); ok {
			if let2.Len() != 3 {
				t.Error("second let needs three parts")
			}
			part2sub1 := let2.Second()
			if let2args, ok := part2sub1.(Pair); ok {
				if let2args.Len() != 2 {
					t.Error("second let subpart 1 length must be 2")
				}
			} else {
				t.Error("second let subpart 1 not a pair")
			}
		} else {
			t.Error("second let not a pair")
		}
	}
}

func TestParseSingle(t *testing.T) {
	input := `(if #t "true" "false")`
	parser := NewParser()
	pair, err := parser.Parse(input)
	pair, ok := pair.First().(Pair)
	if !ok {
		t.Error("result is not a tree!")
	}
	if err != nil {
		t.Errorf("failed to parse program: %v", err)
	} else {
		actual := stringify(pair)
		if actual != `(if #t "true" "false")` {
			t.Errorf("parse() returned wrong result: %s", actual)
		}
		if pair.Len() != 4 {
			t.Error("expected one program elements with four parts")
		}
		elem1 := pair.First()
		if sym, ok := elem1.(Symbol); ok {
			if !atomsEqual(sym, ifSym) {
				t.Error("first element expected to be 'if'")
			}
		} else {
			t.Error("first element not a symbol")
		}
		elem2 := pair.Second()
		if val, ok := elem2.(Boolean); ok {
			if val.Value() != true {
				t.Error("second element expected to be '#t'")
			}
		} else {
			t.Error("second element not a boolean")
		}
		elem3 := pair.Third()
		if val, ok := elem3.(String); ok {
			if val.String() != `"true"` {
				t.Errorf("third element expected to be 'true', but got '%v'", val)
			}
		} else {
			t.Errorf("third element not a string: %v(%T)", elem3, elem3)
		}
		elem4 := Cxr("cadddr", pair)
		if val, ok := elem4.(String); ok {
			if val.String() != `"false"` {
				t.Errorf("fourth element expected to be 'false', but got '%v'", val)
			}
		} else {
			t.Error("fourth element not a string: %v(%T)", elem4, elem4)
		}
	}
}

func TestParseDatumLabels(t *testing.T) {
	mapping := make(map[string]string)
	mapping[`#1=#\b (foo #1#)`] = `(#\b (foo #\b))`
	mapping[`(foo #1=#\a "bcb" #1#)`] = `((foo #\a "bcb" #\a))`
	mapping[`#1=#\b (foo #1=#\a "bcb" #1#) #1#`] = `(#\b (foo #\a "bcb" #\a) #\b)`
	mapping[`(foo #;(#1=#\a "bcb" #1#) 'bar)`] = `((foo (quote bar)))`
	mapping[`#1=#\b (foo "bcb" #1#) #1#`] = `(#\b (foo "bcb" #\b) #\b)`
	verifyParseMap(mapping, t)
}

func TestParseDatumLabelsError(t *testing.T) {
	mapping := make(map[string]string)
	mapping[`(foo "bcb" #101#)`] = "label reference before assignment"
	mapping[`#(foo "bcb" #102#)`] = "label reference before assignment"
	verifyParseError(t, mapping)
}

func (s *ParserSuite) TestErrorLocationString(c *gc.C) {
	input := `(define "abc" 123)`
	cm := gc.Commentf("location for %q", input)
	parser := NewParser()
	pair, err := parser.Parse(input)
	c.Assert(err, gc.IsNil, cm)
	_, err = parser.Expand(pair.First())
	c.Assert(err, gc.NotNil, cm)
	row, col := err.Location()
	c.Assert(row, gc.Equals, 1, cm)
	c.Assert(col, gc.Equals, 8, cm)
}

func (s *ParserSuite) TestErrorLocationBoolean(c *gc.C) {
	input := `(define #false 123)`
	cm := gc.Commentf("location for %q", input)
	parser := NewParser()
	pair, err := parser.Parse(input)
	c.Assert(err, gc.IsNil, cm)
	_, err = parser.Expand(pair.First())
	c.Assert(err, gc.NotNil, cm)
	row, col := err.Location()
	c.Assert(row, gc.Equals, 1, cm)
	c.Assert(col, gc.Equals, 8, cm)
}

func (s *ParserSuite) TestErrorLocationInteger(c *gc.C) {
	input := `(define 123 "abc")`
	cm := gc.Commentf("location for %q", input)
	parser := NewParser()
	pair, err := parser.Parse(input)
	c.Assert(err, gc.IsNil, cm)
	_, err = parser.Expand(pair.First())
	c.Assert(err, gc.NotNil, cm)
	row, col := err.Location()
	c.Assert(row, gc.Equals, 1, cm)
	c.Assert(col, gc.Equals, 8, cm)
}

func (s *ParserSuite) TestErrorLocationFloat(c *gc.C) {
	input := `(define 1.23 "abc")`
	cm := gc.Commentf("location for %q", input)
	parser := NewParser()
	pair, err := parser.Parse(input)
	c.Assert(err, gc.IsNil, cm)
	_, err = parser.Expand(pair.First())
	c.Assert(err, gc.NotNil, cm)
	row, col := err.Location()
	c.Assert(row, gc.Equals, 1, cm)
	c.Assert(col, gc.Equals, 8, cm)
}

func (s *ParserSuite) TestErrorLocationComplex(c *gc.C) {
	input := `(define 3+4i "abc")`
	cm := gc.Commentf("location for %q", input)
	parser := NewParser()
	pair, err := parser.Parse(input)
	c.Assert(err, gc.IsNil, cm)
	_, err = parser.Expand(pair.First())
	c.Assert(err, gc.NotNil, cm)
	row, col := err.Location()
	c.Assert(row, gc.Equals, 1, cm)
	c.Assert(col, gc.Equals, 8, cm)
}

func (s *ParserSuite) TestErrorLocationRational(c *gc.C) {
	input := `(define 3/4 "abc")`
	cm := gc.Commentf("location for %q", input)
	parser := NewParser()
	pair, err := parser.Parse(input)
	c.Assert(err, gc.IsNil, cm)
	_, err = parser.Expand(pair.First())
	c.Assert(err, gc.NotNil, cm)
	row, col := err.Location()
	c.Assert(row, gc.Equals, 1, cm)
	c.Assert(col, gc.Equals, 8, cm)
}

func (s *ParserSuite) TestErrorLocationCharacter(c *gc.C) {
	input := `(define #\z "abc")`
	cm := gc.Commentf("location for %q", input)
	parser := NewParser()
	pair, err := parser.Parse(input)
	c.Assert(err, gc.IsNil, cm)
	_, err = parser.Expand(pair.First())
	c.Assert(err, gc.NotNil, cm)
	row, col := err.Location()
	c.Assert(row, gc.Equals, 1, cm)
	c.Assert(col, gc.Equals, 8, cm)
}

func (s *ParserSuite) TestParsedSymbol(c *gc.C) {
	ps := NewParsedSymbol("foo", 1, 1)
	c.Assert(ps, gc.NotNil)
	sym := NewSymbol("eff")
	cmp, _ := sym.CompareTo(ps)
	c.Assert(cmp, gc.Equals, int8(-1))
}

// writeTempFile creates a temporary file and writes the given text to it,
// returning the os.File for further use.
func writeTempFile(text string, c *gc.C) *os.File {
	file, err := ioutil.TempFile("", "parser_test-")
	if err != nil {
		c.Fatal(err.Error())
		c.FailNow()
	}
	_, err = file.WriteString(text)
	if err != nil {
		c.Fatal(err.Error())
		c.FailNow()
	}
	err = file.Close()
	if err != nil {
		c.Fatal(err.Error())
		c.FailNow()
	}
	return file
}

func (s *ParserSuite) TestParserInclude(c *gc.C) {
	prog := `(define foo "abc")`
	fin := writeTempFile(prog, c)
	defer os.Remove(fin.Name())
	prog = fmt.Sprintf("(include \"%s\") foo", fin.Name())
	fout := writeTempFile(prog, c)
	defer os.Remove(fout.Name())
	parser := NewParser()
	pair, err := parser.ParseFile(fout.Name())
	if err != nil {
		c.Errorf("reading outer file failed: %s", err.Error())
	} else {
		expanded, err := parser.Expand(pair)
		if err != nil {
			c.Error(err.Error())
		} else {
			actual := stringify(expanded)
			expected := `((begin (define foo "abc")) foo)`
			c.Assert(actual, gc.Equals, expected)
		}
	}
}

func (s *ParserSuite) TestParserIncludeCase(c *gc.C) {
	prog := `(define FOO "abc")`
	fin := writeTempFile(prog, c)
	defer os.Remove(fin.Name())
	prog = fmt.Sprintf("(include-ci \"%s\") foo", fin.Name())
	fout := writeTempFile(prog, c)
	defer os.Remove(fout.Name())
	parser := NewParser()
	pair, err := parser.ParseFile(fout.Name())
	if err != nil {
		c.Errorf("reading outer file failed: %s", err.Error())
	} else {
		expanded, err := parser.Expand(pair)
		if err != nil {
			c.Error(err.Error())
		} else {
			actual := stringify(expanded)
			expected := `((begin (define foo "abc")) foo)`
			c.Assert(actual, gc.Equals, expected)
		}
	}
}
