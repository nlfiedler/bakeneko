//
// Copyright 2012-2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	"strings"
	"testing"
)

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
		if num != 1 {
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
		if num != 2 {
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
		if num != 456 {
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
		if num != 123 {
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
	// syntactic keywords cannot be derived from functions
	input = `((quote if) #f 1 2)`
	result, err = Interpret(input)
	if err == nil {
		t.Error("Interpret() should have failed")
	}
	if !strings.Contains(err.ErrorMessage(), "is not applicable") {
		t.Error("((quote if) ...) should have failed with 'not applicable'")
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
