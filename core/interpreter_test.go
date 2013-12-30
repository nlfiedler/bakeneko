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

type InterpreterSuite struct {
}

var _ = gc.Suite(&InterpreterSuite{})

// verifyInterpret takes a map of inputs to expected outputs, running the
// inputs through the interpreter and checking the output.
func verifyInterpret(t *testing.T, inputs map[string]string) {
	for k, v := range inputs {
		result, err := Interpret(k)
		if err != nil {
			t.Errorf("Interpret() failed for '%s' with: %v", k, err)
		}
		str := stringify(result)
		if str != v {
			t.Errorf("Interpret() yielded wrong result for '%s';"+
				" expected '%s' but got '%s'", k, v, str)
		}
	}
}

// checkInterpret takes a map of inputs to expected outputs, running the
// inputs through the interpreter and checking the output.
func checkInterpret(c *gc.C, inputs map[string]string) {
	for input, expected := range inputs {
		result, err := Interpret(input)
		if err != nil {
			c.Errorf("Interpret() failed for '%s' with: %v", input, err)
		} else {
			c.Check(stringify(result), gc.Equals, expected)
		}
	}
}

// verifyInterpretError takes a map of inputs to expected error messages,
// running the inputs through the interpreter and ensuring that each one fails
// with the associated error message.
func verifyInterpretError(t *testing.T, inputs map[string]string) {
	for k, v := range inputs {
		result, err := Interpret(k)
		if err == nil {
			t.Fatalf("Interpret() should have failed for '%s', but got %v", k, result)
		}
		str := err.ErrorMessage()
		if !strings.Contains(str, v) {
			t.Errorf("Interpret() yielded wrong error for '%s';"+
				" expected '%s' but got '%s'", k, v, str)
		}
	}
}

// checkInterpretError takes a map of inputs to expected error messages,
// running the inputs through the interpreter and ensuring that each one fails
// with the associated error message.
func checkInterpretError(c *gc.C, inputs map[string]string) {
	for input, expected := range inputs {
		result, err := Interpret(input)
		if err == nil {
			c.Errorf("Interpret() should have failed for '%s', but got %v", input, result)
		} else {
			c.Check(err, gc.ErrorMatches, expected)
		}
	}
}

func TestEnvironment(t *testing.T) {
	e := NewEnvironment(nil)
	if e == nil {
		t.Fatalf("constructing new environment failed")
	}
	foo := NewSymbol("foo")
	v := e.Find(foo)
	if v != nil {
		t.Errorf("expected undefined var to return nil")
	}
	err := e.Set(foo, "bar")
	if err == nil {
		t.Errorf("expected set of undefined var to fail")
	}
	e.Define(foo, "bar")
	v = e.Find(foo)
	if v != "bar" {
		t.Errorf("expected defined var to return 'bar', but got %s", v)
	}
}

func TestEnvironmentParent(t *testing.T) {
	p := NewEnvironment(nil)
	foo := NewSymbol("foo")
	p.Define(foo, "bar")
	e := NewEnvironment(p)
	if e == nil {
		t.Fatalf("constructing new environment failed")
	}
	v := e.Find(foo)
	if v != "bar" {
		t.Errorf("expected 'bar' but got '%s'", v)
	}
	err := e.Set(foo, "qux")
	if err != nil {
		t.Errorf("set of parent-defined var failed: %v", err)
	}
	// check parent
	v = p.Find(foo)
	if v != "qux" {
		t.Errorf("expected 'qux' but got '%s'", v)
	}
	// check child delegates to parent
	v = e.Find(foo)
	if v != "qux" {
		t.Errorf("expected 'qux' but got '%s'", v)
	}
}

func TestEnvironmentOverride(t *testing.T) {
	p := NewEnvironment(nil)
	foo := NewSymbol("foo")
	p.Define(foo, "bar")
	e := NewEnvironment(p)
	if e == nil {
		t.Fatalf("constructing new environment failed")
	}
	e.Define(foo, "qux")
	if e.Find(foo) != "qux" {
		t.Errorf("expected local-defined var to return 'qux'")
	}
	if p.Find(foo) != "bar" {
		t.Errorf("expected parent-defined var to return 'bar'")
	}
}

func TestInterpretIfTrue(t *testing.T) {
	input := `(if #t 1 2)`
	result, err := Interpret(input)
	if err != nil {
		t.Errorf("Interpret() failed: %v", err)
	}
	if num, ok := result.(Integer); ok {
		ii := num.ToInteger()
		if ii != 1 {
			t.Errorf("result wrong value: %v", num)
		}
	} else {
		t.Errorf("result of wrong type: %T: %v", result, result)
	}
}

func TestInterpreterIfFalse(t *testing.T) {
	input := `(if #f 1 2)`
	result, err := Interpret(input)
	if err != nil {
		t.Errorf("Interpret() failed: %v", err)
	}
	if num, ok := result.(Integer); ok {
		ii := num.ToInteger()
		if ii != 2 {
			t.Errorf("result wrong value: %v", num)
		}
	} else {
		t.Errorf("result of wrong type: %T: %v", result, result)
	}
	// false with no alternate
	input = `(if #f 1)`
	result, err = Interpret(input)
	if err != nil {
		t.Errorf("Interpret() failed: %v", err)
	}
	if result != theEmptyList {
		t.Error("expected if #f with no alternate to return empty list")
	}
}

func TestInterpretBegin(t *testing.T) {
	// define and set! (inside a begin, bonus!)
	input := `(begin (define foo 123) (set! foo 456) foo)`
	result, err := Interpret(input)
	if err != nil {
		t.Errorf("Interpret() failed: %v", err)
	}
	if num, ok := result.(Integer); ok {
		ii := num.ToInteger()
		if ii != 456 {
			t.Errorf("result wrong value: %v", num)
		}
	} else {
		t.Errorf("result of wrong type: %T: %v", result, result)
	}
}

func TestInterpretDefine(t *testing.T) {
	input := `(begin (define foo 123) foo)`
	result, err := Interpret(input)
	if err != nil {
		t.Errorf("Interpret() failed: %v", err)
	}
	if num, ok := result.(Integer); ok {
		ii := num.ToInteger()
		if ii != 123 {
			t.Errorf("result wrong value: %v", num)
		}
	} else {
		t.Errorf("result of wrong type: %T: %v", result, result)
	}
}

func TestInterpretQuote(t *testing.T) {
	input := `(begin (define foo (quote foo)) foo)`
	result, err := Interpret(input)
	if err != nil {
		t.Errorf("Interpret() failed: %v", err)
	}
	if num, ok := result.(Symbol); ok {
		if num.String() != "foo" {
			t.Errorf("result wrong value: %v", num)
		}
	} else {
		t.Errorf("result of wrong type: %T: %v", result, result)
	}
	// primitive lambdas cannot be derived from functions
	input = `((quote if) #f 1 2)`
	result, err = Interpret(input)
	if err == nil {
		t.Error("Interpret() should have failed")
	}
	if !strings.Contains(err.ErrorMessage(), "is not applicable") {
		t.Error("((quote if) ...) should have failed with 'not applicable'")
	}
	// atoms are not applicable
	input = `(1 2 3 4)`
	result, err = Interpret(input)
	if err == nil {
		t.Error("Interpret() should have failed")
	}
	if !strings.Contains(err.ErrorMessage(), "is not applicable") {
		t.Error("(1 2 3 4) should have failed with 'not applicable'")
	}
}

func TestInterpretLambda(t *testing.T) {
	input := `(define fun
  (lambda (x)
    (if x 'foo 'bar)))
(fun #t)
`
	result, err := Interpret(input)
	if err != nil {
		t.Errorf("Interpret() failed: %v", err)
	}
	expected := NewSymbol("foo")
	if sym, ok := result.(Symbol); ok {
		if equal, _ := sym.EqualTo(expected); !equal {
			t.Error("expected lambda 'fun' to return symbol foo")
		}
	} else {
		t.Errorf("expected lambda 'fun' to return symbol, got %T", result)
	}
}

func TestInterpretAnd(t *testing.T) {
	inputs := make(map[string]Boolean)
	inputs[`(and)`] = BooleanTrue
	inputs[`(and #t)`] = BooleanTrue
	inputs[`(and #t #t)`] = BooleanTrue
	inputs[`(and #t #t #t)`] = BooleanTrue
	inputs[`(and #f)`] = BooleanFalse
	inputs[`(and #t #f)`] = BooleanFalse
	inputs[`(and #t #t #f)`] = BooleanFalse
	for input, expected := range inputs {
		result, err := Interpret(input)
		if err != nil {
			t.Errorf("Interpret() failed: %v", err)
		}
		if b, ok := result.(Boolean); ok {
			if equal, _ := b.EqualTo(expected); !equal {
				t.Errorf("expected %s to return %v", input, expected)
			}
		} else {
			t.Errorf("expected and to return boolean, got %v", result)
		}
	}
}

func TestInterpretOr(t *testing.T) {
	inputs := make(map[string]Boolean)
	inputs[`(or)`] = BooleanFalse
	inputs[`(or #t)`] = BooleanTrue
	inputs[`(or #f #f)`] = BooleanFalse
	inputs[`(or #f #f #f)`] = BooleanFalse
	inputs[`(or #f)`] = BooleanFalse
	inputs[`(or #f #t)`] = BooleanTrue
	inputs[`(or #f #f #t)`] = BooleanTrue
	for input, expected := range inputs {
		result, err := Interpret(input)
		if err != nil {
			t.Fatalf("Interpret() failed: %v", err)
		}
		if b, ok := result.(Boolean); ok {
			if equal, _ := b.EqualTo(expected); !equal {
				t.Errorf("expected %s to return %v", input, expected)
			}
		} else {
			t.Errorf("expected and to return boolean, got %v", result)
		}
	}
}

func TestInterpretCond(t *testing.T) {
	foo := NewSymbol("foo")
	inputs := make(map[string]interface{})
	inputs[`(cond (else 'foo))`] = foo
	inputs[`(cond (#t 'foo))`] = foo
	inputs[`(cond (#f 'bar) (#t 'foo))`] = foo
	inputs[`(cond)`] = theEmptyList
	inputs[`(cond (#f 'bar) (#f 'bar) (#t 'foo))`] = foo
	inputs[`(cond (#f 'bar) (else 'foo))`] = foo
	inputs[`(cond (#t (define bar 'bar) (set! bar 'foo) bar))`] = foo
	inputs[`(cond (#f 'bar) (#f 'bar) (else 'foo))`] = foo
	inputs[`(cond (#f))`] = theEmptyList
	inputs[`(cond (#t))`] = BooleanTrue
	inputs[`(cond (#t => (lambda (x) (if x 'bar 'foo))))`] = NewSymbol("bar")
	for input, expected := range inputs {
		result, err := Interpret(input)
		if err != nil {
			t.Fatalf("Interpret() failed: %v", err)
		}
		switch thing := result.(type) {
		case Atom:
			if expectedAtom, ok := expected.(Atom); ok {
				if equal, _ := thing.EqualTo(expectedAtom); !equal {
					t.Errorf("expected %s to return %v", input, expected)
				}
			} else {
				t.Errorf("unexpected Atom result for %s", input)
			}
		case Pair:
			if expectedPair, ok := expected.(Pair); ok {
				// Directly comparing Pair instances...
				if thing != expectedPair {
					t.Errorf("expected %s to return %v", input, expected)
				}
			} else {
				t.Errorf("unexpected Pair result for %s", input)
			}
		default:
			t.Errorf("unexpected result type %T for %s", result, input)
		}
	}
	// test error cases
	errors := make(map[string]string)
	errors[`(cond (else))`] = "cond else clause must not be empty"
	errors[`(cond ())`] = "cond clause must not be empty"
	errors[`(cond #f)`] = "cond clause must be a pair"
	verifyInterpretError(t, errors)
}

func checkInteger(elem interface{}, expected int64, c *gc.C) {
	if num, is_num := elem.(Integer); is_num {
		c.Check(num.ToInteger(), gc.Equals, expected)
	} else {
		c.Errorf("actual value is not an Integer: %T", elem)
	}
}

func (s *InterpreterSuite) TestInterpreterArbitraryArguments(c *gc.C) {
	input := `((lambda (x y . z) z) 3 4 5 6)` // => (5 6)  # see R7RS 4.1.4
	result, err := Interpret(input)
	if err != nil {
		c.Errorf("Interpret() failed: %v", err)
	}
	if pair, is_pair := result.(Pair); is_pair {
		if pair.Len() != 2 {
			c.Error("expected lambda to return a list of two")
		} else {
			checkInteger(pair.First(), 5, c)
			checkInteger(pair.Second(), 6, c)
		}
	} else {
		c.Errorf("expected lambda to return a list, got %T", result)
	}
}

func (s *InterpreterSuite) TestInterpreterProcedures(c *gc.C) {
	table := make(map[string]string)
	table[`(define x ((lambda (x) (+ x x)) 4)) x`] = "8"
	table[`(define reverse-subtract (lambda (x y) (- y x))) (reverse-subtract 7 10)`] = "3"
	table[`(define x 10) ((lambda (x) (set! x 20)) 4) x`] = "10"
	for input, expected := range table {
		result, err := Interpret(input)
		if err != nil {
			c.Errorf("Interpret() failed: %v", err)
		} else {
			c.Check(stringify(result), gc.Equals, expected)
		}
	}
}

func (s *InterpreterSuite) TestInterpreterLambdaErrors(c *gc.C) {
	table := make(map[string]string)
	table[`((lambda x x) 3 4 5 6)`] = ".* too many arguments .*"
	table[`(1 2 3 4)`] = ".* is not applicable.*"
	for input, expected := range table {
		result, err := Interpret(input)
		if result != nil {
			c.Errorf("Interpret() should have failed, got %v", result)
		} else {
			c.Check(err, gc.ErrorMatches, expected)
		}
	}
}

func (s *InterpreterSuite) TestInterpreterFibRecursive(c *gc.C) {
	input := `(define fibonacci
  (lambda (n)
    (fibonacci-kernel 0 1 n)))

(define fibonacci-kernel
  (lambda (current next remaining)
    (if (= 0 remaining)
        current
        (fibonacci-kernel next (+ current next) (- remaining 1)))))

(fibonacci 100)`
	result, err := Interpret(input)
	if err != nil {
		c.Errorf("Interpret() failed: %v", err)
	} else {
		c.Check(result, gc.Equals, NewInteger(3736710778780434371))
	}
}

func (s *InterpreterSuite) TestInterpreterTailRecursive(c *gc.C) {
	table := make(map[string]string)
	table[`(define iffi (lambda (n) (if (> n 1000) n (iffi (+ n 1))))) (iffi 1)`] = "1001"
	table[`(define iffi (lambda (n) (if (< n 1000) (iffi (+ n 1)) n))) (iffi 1)`] = "1000"
	table[`(define cr (lambda (n) (cond ((< n 1000) (cr (+ n 1))) (else n)))) (cr 1)`] = "1000"
	table[`(define cr (lambda (n) (cond ((> n 1000) n) (else (cr (+ n 1)))))) (cr 1)`] = "1001"
	table[`(define andr (lambda (n) (and (< n 1000) (andr (+ n 1))))) (andr 1)`] = "#f"
	table[`(define orr (lambda (n) (or (> n 1000) (orr (+ n 1))))) (orr 1)`] = "#t"
	for input, expected := range table {
		result, err := Interpret(input)
		if err != nil {
			c.Errorf("Interpret() failed: %v", err)
		} else {
			c.Check(stringify(result), gc.Equals, expected)
		}
	}
}
