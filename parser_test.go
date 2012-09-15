//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

import (
	"fmt"
	"strings"
	"testing"
)

type expectedExpandError struct {
	err string // expected error message substring
	msg string // explanation of error condition
}

func verifyExpandMap(mapping map[string]string, t *testing.T) {
	for input, expected := range mapping {
		x, err := parseExpr(input)
		if err != nil {
			t.Fatalf("failed to parse %q: %s", input, err)
		}
		_, ok := x.(Pair)
		if !ok {
			t.Fatalf("parser returned non-pair for %q", input)
		}
		x, err = expand(x, true)
		if err != nil {
			t.Fatalf("failed to expand %q: %s", input, err)
		}
		_, ok = x.(Pair)
		if !ok {
			t.Fatalf("expand returned non-pair for %q: %T", input, x)
		}
		s := stringify(x)
		if s != expected {
			t.Errorf(`expected <<%s>> but got <<%s>>`, expected, s)
		}
	}
}

func verifyExpandError(t *testing.T, expected map[string]expectedExpandError) {
	for input, error := range expected {
		x, err := parseExpr(input)
		if err != nil {
			t.Fatalf("failed to parse %q: %s", input, err)
		}
		x, err = expand(x, true)
		if err == nil {
			t.Fatalf("expand() should have failed with %q", input)
		}
		if !strings.Contains(err.String(), error.err) {
			t.Errorf("expected [%s] but got [%s] for input %q", error.err, err, input)
		}
	}
}

func verifyParse(input, expected string, t *testing.T) {
	result, err := parseExpr(input)
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

func TestParseExprEmptyList(t *testing.T) {
	input := "()"
	expected := "()"
	verifyParse(input, expected, t)
}

func TestParseExprSingletonList(t *testing.T) {
	input := "(foo)"
	expected := "(foo)"
	verifyParse(input, expected, t)
}

func TestParseExprList(t *testing.T) {
	mapping := make(map[string]string)
	// Input/Output tested with MIT/GNU Scheme
	mapping["(foo  bar    baz)"] = "(foo bar baz)"
	mapping["(0 . 1)"] = "(0 . 1)"
	mapping["(0 1)"] = "(0 1)"
	mapping["(0 . (1 . ()))"] = "(0 1)"
	mapping["(0 . (1 . 2))"] = "(0 1 . 2)"
	verifyParseMap(mapping, t)
}

func TestParseExprNestedList(t *testing.T) {
	input := `(foo
  (bar
    baz))`
	expected := "(foo (bar baz))"
	verifyParse(input, expected, t)
}

func TestParseExprBoolean(t *testing.T) {
	input := "( #t #f )"
	expected := "(#t #f)"
	verifyParse(input, expected, t)
}

func TestParseExprString(t *testing.T) {
	input := `"foo"`
	expected := `"foo"`
	verifyParse(input, expected, t)
}

func TestParseCharacters(t *testing.T) {
	mapping := make(map[string]string)
	mapping["#\\a"] = "#\\a"
	mapping["#\\t"] = "#\\t"
	mapping["#\\newline"] = "#\\newline"
	mapping["#\\space"] = "#\\space"
	mapping["#\\M"] = "#\\M"
	mapping["#\\z"] = "#\\z"
	verifyParseMap(mapping, t)
}

func TestParseQuotes(t *testing.T) {
	mapping := make(map[string]string)
	mapping["(foo 'x)"] = "(foo (quote x))"
	mapping["(foo `x)"] = "(foo (quasiquote x))"
	mapping["(foo ,x)"] = "(foo (unquote x))"
	mapping["(foo ,@x)"] = "(foo (unquote-splicing x))"
	mapping["`(list ,(+ 1 2) 4)"] = "(quasiquote (list (unquote (+ 1 2)) 4))"
	mapping["`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)"] =
		"(quasiquote (a (unquote (+ 1 2)) (unquote-splicing (map abs (quote (4 -5 6)))) b))"
	// TODO: support `#() vector quasi-quoting
	// mapping["`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)"] =
	// 	"(quasiquote #(10 5 (unquote (sqrt 4)) (unquote-splicing (map sqrt (quote (16 9)))) 8))"
	verifyParseMap(mapping, t)
}

func TestParseVector(t *testing.T) {
	input := "#(1 2 3)"
	result, err := parseExpr(input)
	if err != nil {
		msg := fmt.Sprintf("failed to parse expression '%s', %s", input, err)
		t.Errorf(msg)
	} else {
		if slice, ok := result.([]interface{}); ok {
			if len(slice) == 3 {
				if slice[0] != int64(1) && slice[1] != int64(2) && slice[2] != int64(3) {
					t.Errorf("expected 1, 2, 3 in slice, but got %s", slice)
				}
			} else {
				t.Errorf("expected slice of length three but got %d", len(slice))
			}
		} else {
			t.Errorf("expected slice but got %T", result)
		}
	}
}

func TestParseExprNumbers(t *testing.T) {
	mapping := make(map[string]string)
	mapping["1.2345"] = "1.2345"
	mapping[".1"] = "0.1"
	mapping["6e4"] = "60000"
	mapping["12345"] = "12345"
	mapping["2.1"] = "2.1"
	mapping["3."] = "3"
	mapping["7.91e+16"] = "7.91e+16"
	mapping[".000001"] = "1e-06"
	mapping["#b11111111"] = "255"
	mapping["#o777"] = "511"
	mapping["#x4dfCF0"] = "5111024"
	mapping["#d12345"] = "12345"
	mapping["#d#i12345"] = "12345"
	mapping["#d#e12345"] = "12345"
	mapping["#i#d12345"] = "12345"
	mapping["#e#d12345"] = "12345"
	// note that in Go, -0 is the same as 0, so sign will be lost
	mapping["3+4i"] = "3+4i"
	mapping["3-4i"] = "3-4i"
	mapping["3.0+4.0i"] = "3+4i"
	mapping["3.0-4.0i"] = "3-4i"
	mapping["3+i"] = "3+1i"
	mapping["3-i"] = "3-1i"
	mapping["+4i"] = "0+4i"
	mapping["-4i"] = "0-4i"
	mapping["+i"] = "0+1i"
	mapping["-i"] = "0-1i"
	mapping["1/1"] = "1"
	mapping["1/2"] = "0.5"
	mapping["1/3"] = "0.3333333333333333"
	mapping["1/4"] = "0.25"
	mapping["3/4"] = "0.75"
	mapping["6/10"] = "0.6"
	mapping["100/1000"] = "0.1"
	verifyParseMap(mapping, t)
}

func TestExpand(t *testing.T) {
	mapping := make(map[string]string)
	mapping[`(if #t (display "foo"))`] = `(if #t (display "foo") ())`
	mapping[`(if #t 1 2)`] = `(if #t 1 2)`
	mapping[`((if #t 1 2))`] = `(if #t 1 2)`
	mapping[`(quote abc)`] = `(quote abc)`
	mapping[`(set! foo (quote bar))`] = `(set! foo (quote bar))`
	mapping[`(set! foo (if #t (quote bar)))`] = `(set! foo (if #t (quote bar) ()))`
	mapping[`(define (f args) body)`] = `(define f (lambda (args) body))`
	mapping["(define-macro foo (lambda args (if #t (quote bar))))"] =
		"(define-macro foo (lambda (args) (if #t (quote bar) ())))"
	mapping[`(begin (if #t (display "foo")))`] = `(begin (if #t (display "foo") ()))`
	mapping[`(begin (define foo 123) foo)`] = `(begin (define foo 123) foo)`
	mapping[`(lambda (x) e1)`] = `(lambda (x) e1)`
	mapping[`(lambda (x) e1 e2)`] = `(lambda (x) (begin e1 e2))`
	mapping[`(foo (if #t (quote bar)))`] = `(foo (if #t (quote bar) ()))`
	mapping["(foo `x)"] = "(foo (quote x))"
	mapping["(foo `,x)"] = "(foo x)"
	// TODO: is this correct for Scheme?
	mapping["(foo `(,@x y))"] = "(foo (append x (cons (quote y) (quote ()))))"
	// TODO: test macro invocation
	verifyExpandMap(mapping, t)
}

func TestExpandErrors(t *testing.T) {
	input := make(map[string]expectedExpandError)
	input["(if)"] = expectedExpandError{"if too many/few arguments", "if requires 3 or 4 args"}
	input["(if bar)"] = expectedExpandError{"if too many/few arguments", "if requires 3 or 4 args"}
	input["(if foo bar baz quux)"] = expectedExpandError{"if too many/few arguments", "if requires 3 or 4 args"}
	input["(set!)"] = expectedExpandError{"set requires 2 arguments", "set requires 2 args"}
	input["(set! foo)"] = expectedExpandError{"set requires 2 arguments", "set requires 2 args"}
	input["(set! (foo) bar)"] = expectedExpandError{"can only set! a symbol", "cannot set non-symbols"}
	input["(set! bar baz quux)"] = expectedExpandError{"set requires 2 arguments", "set requires 2 args"}
	input["(quote)"] = expectedExpandError{"quote requires datum", "quote takes 1 arg"}
	input["(quote foo bar)"] = expectedExpandError{"quote requires datum", "quote takes 1 arg"}
	input["(lambda foo)"] = expectedExpandError{"lambda requires 2+ arguments", "lambda takes 2+ args"}
	input[`(lambda ("foo") bar)`] = expectedExpandError{"lambda arguments must be symbols", "lambda takes symbol args"}
	input[`(lambda "foo" bar)`] = expectedExpandError{"lambda arguments must be a list or a symbol", "lambda takes symbol args"}
	verifyExpandError(t, input)
}

func TestParse(t *testing.T) {
	input := `; this binds x to 5 and yields  10
(let ((x 5)) (* x 2))
; this bind x to 10, z to 5 and yields 50.
(let ((x 10) (z 5)) (* x z))
`
	result, err := parse(input)
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
	result, err := parse(input)
	if err != nil {
		t.Errorf("failed to parse program: %v", err)
	} else {
		actual := stringify(result)
		if actual != `(if #t "true" "false")` {
			t.Errorf("parse() returned wrong result: %s", actual)
		}
		if result.Len() != 4 {
			t.Error("expected one program elements with four parts")
		}
		elem1 := result.First()
		if sym, ok := elem1.(Symbol); ok {
			if sym != ifSym {
				t.Error("first element expected to be 'if'")
			}
		} else {
			t.Error("first element not a symbol")
		}
		elem2 := result.Second()
		if val, ok := elem2.(Boolean); ok {
			if val.Value() != true {
				t.Error("second element expected to be '#t'")
			}
		} else {
			t.Error("second element not a boolean")
		}
		elem3 := result.Third()
		if val, ok := elem3.(String); ok {
			if val.String() != `"true"` {
				t.Errorf("third element expected to be 'true', but got '%v'", val)
			}
		} else {
			t.Errorf("third element not a string: %v(%T)", elem3, elem3)
		}
		elem4 := Cxr("cadddr", result)
		if val, ok := elem4.(String); ok {
			if val.String() != `"false"` {
				t.Errorf("fourth element expected to be 'false', but got '%v'", val)
			}
		} else {
			t.Error("fourth element not a string: %v(%T)", elem4, elem4)
		}
	}
}
