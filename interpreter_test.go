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
	foo := Symbol("foo")
	v := e.Find(foo)
	if v != nil {
		t.Errorf("unexpected undefined var to return nil")
	}
	err := e.Set(foo, "bar")
	if err == nil {
		t.Errorf("expected set of undefined var to fail")
	}
	e.Define(foo, "bar")
	v = e.Find(foo)
	if v != "bar" {
		t.Errorf("expected defined var to return 'bar'")
	}
}

func TestEnvironmentParent(t *testing.T) {
	p := NewEnvironment(nil)
	foo := Symbol("foo")
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
	foo := Symbol("foo")
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
		if num != "foo" {
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
	if result != Symbol("foo") {
		t.Error("expected lambda 'fun' to return symbol foo")
	}
}

func TestInterpretAnd(t *testing.T) {
	inputs := make(map[string]Boolean)
	inputs[`(and)`] = Boolean(true)
	inputs[`(and #t)`] = Boolean(true)
	inputs[`(and #t #t)`] = Boolean(true)
	inputs[`(and #t #t #t)`] = Boolean(true)
	inputs[`(and #f)`] = Boolean(false)
	inputs[`(and #t #f)`] = Boolean(false)
	inputs[`(and #t #t #f)`] = Boolean(false)
	for input, expected := range inputs {
		result, err := Interpret(input)
		if err != nil {
			t.Errorf("Interpret() failed: %v", err)
		}
		if b, ok := result.(Boolean); ok {
			if b != expected {
				t.Errorf("expected %s to return %v", input, expected)
			}
		} else {
			t.Errorf("expected and to return boolean, got %v", result)
		}
	}
}

func TestInterpretOr(t *testing.T) {
	inputs := make(map[string]Boolean)
	inputs[`(or)`] = Boolean(false)
	inputs[`(or #t)`] = Boolean(true)
	inputs[`(or #f #f)`] = Boolean(false)
	inputs[`(or #f #f #f)`] = Boolean(false)
	inputs[`(or #f)`] = Boolean(false)
	inputs[`(or #f #t)`] = Boolean(true)
	inputs[`(or #f #f #t)`] = Boolean(true)
	for input, expected := range inputs {
		result, err := Interpret(input)
		if err != nil {
			t.Fatalf("Interpret() failed: %v", err)
		}
		if b, ok := result.(Boolean); ok {
			if b != expected {
				t.Errorf("expected %s to return %v", input, expected)
			}
		} else {
			t.Errorf("expected and to return boolean, got %v", result)
		}
	}
}

func TestInterpretCond(t *testing.T) {
	foo := Symbol("foo")
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
	inputs[`(cond (#t))`] = Boolean(true)
	inputs[`(cond (#t => (lambda (x) (if x 'bar 'foo))))`] = Symbol("bar")
	for input, expected := range inputs {
		result, err := Interpret(input)
		if err != nil {
			t.Fatalf("Interpret() failed: %v", err)
		}
		if result != expected {
			t.Errorf("expected %s to return %v", input, expected)
		}
	}
	// test error cases
	errors := make(map[string]string)
	errors[`(cond (else))`] = "cond else clause must not be empty"
	errors[`(cond ())`] = "cond clause must not be empty"
	errors[`(cond #f)`] = "cond clause must be a pair"
	verifyInterpretError(t, errors)
}

// TODO: test tail-call optimization as defined in Scheme r5rs 3.5

// TODO: for testing the results of evaluation, can assign "stdout" in the global
//       environment to a bytes.Buffer then read the contents of that to see what
//       the s-expr yielded
