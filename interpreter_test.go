//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

import (
	"testing"
)

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
	if num, ok := result.(int64); ok {
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
	if num, ok := result.(int64); ok {
		if num != 2 {
			t.Errorf("result wrong value: %v", num)
		}
	} else {
		t.Errorf("result of wrong type: %T: %v", result, result)
	}
}

func TestInterpretBegin(t *testing.T) {
	// define and set! (inside a begin, bonus!)
	input := `(begin (define foo 123) (set! foo 456) foo)`
	result, err := Interpret(input)
	if err != nil {
		t.Errorf("Interpret() failed: %v", err)
	}
	if num, ok := result.(int64); ok {
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
	if num, ok := result.(int64); ok {
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
}

// TODO: for testing the results of evaluation, can assign "stdout" in the global
//       environment to a bytes.Buffer then read the contents of that to see what
//       the s-expr yielded
