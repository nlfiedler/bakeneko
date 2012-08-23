//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

import (
	"fmt"
	"testing"
)

func TestPairNil(t *testing.T) {
	var p *pair = nil
	if p.Len() != 0 {
		t.Errorf("nil Pair.Len() should be 0")
	}
	if p.First() != nil {
		t.Errorf("nil Pair.First() should be nil")
	}
	if p.Second() != nil {
		t.Errorf("nil Pair.Second() should be nil")
	}
	if p.Third() != nil {
		t.Errorf("nil Pair.Third() should be nil")
	}
	if p.String() != "()" {
		t.Errorf("nil Pair.String() should be ()")
	}
	if p.Reverse() != nil {
		t.Errorf("nil Pair.Reverse() should be nil")
	}
}

func TestPairSingle(t *testing.T) {
	foo := Symbol("foo")
	p := NewPair(foo)
	if p.Len() != 1 {
		t.Errorf("expected 1, but got %d", p.Len())
	}
	if p.First() != foo {
		t.Errorf("expected 'foo', but got '%s'", p.First())
	}
	if p.Second() != nil {
		t.Errorf("expected nil, but got '%s'", p.Second())
	}
	if p.Third() != nil {
		t.Errorf("expected nil, but got '%s'", p.Third())
	}
	if p.String() != "(foo)" {
		t.Errorf("expected '(foo)' but got '%s'", p.String())
	}
	p = p.Reverse()
	if p.Len() != 1 {
		t.Errorf("expected 1, but got %d", p.Len())
	}
	if p.First() != foo {
		t.Errorf("expected 'foo', but got '%s'", p.First())
	}
	if p.Second() != nil {
		t.Errorf("expected nil, but got '%s'", p.Second())
	}
	if p.Third() != nil {
		t.Errorf("expected nil, but got '%s'", p.Third())
	}
	if p.String() != "(foo)" {
		t.Errorf("expected (foo) but got '%s'", p.String())
	}
}

func TestCons(t *testing.T) {
	foo := Symbol("foo")
	bar := Symbol("bar")
	p := Cons(foo, bar)
	if p.Len() != 2 {
		t.Errorf("expected 2, but got %d", p.Len())
	}
	if p.First() != foo {
		t.Errorf("expected 'foo', but got '%s'", p.First())
	}
	if p.Second() != bar {
		t.Errorf("expected 'bar', but got '%s'", p.Second())
	}
	if p.Third() != nil {
		t.Errorf("expected nil, but got '%s'", p.Third())
	}
	if p.String() != "(foo . bar)" {
		t.Errorf("expected '(foo . bar)' but got '%s'", p.String())
	}
	p = p.Reverse()
	if p.Len() != 2 {
		t.Errorf("expected 2, but got %d", p.Len())
	}
	if p.First() != bar {
		t.Errorf("expected 'bar', but got '%s'", p.First())
	}
	if p.Second() != foo {
		t.Errorf("expected 'foo', but got '%s'", p.Second())
	}
	if p.Third() != nil {
		t.Errorf("expected nil, but got '%s'", p.Third())
	}
	if p.String() != "(bar . foo)" {
		t.Errorf("expected (bar . foo) but got '%s'", p.String())
	}
}

func TestList(t *testing.T) {
	foo := Symbol("foo")
	bar := Symbol("bar")
	p := List(foo, bar)
	if p.Len() != 2 {
		t.Errorf("expected 2, but got %d", p.Len())
	}
	if p.First() != foo {
		t.Errorf("expected 'foo', but got '%s'", p.First())
	}
	if p.Second() != bar {
		t.Errorf("expected 'bar', but got '%s'", p.Second())
	}
	if p.Third() != nil {
		t.Errorf("expected nil, but got '%s'", p.Third())
	}
	if p.String() != "(foo bar)" {
		t.Errorf("expected '(foo bar)' but got '%s'", p.String())
	}
	p = p.Reverse()
	if p.Len() != 2 {
		t.Errorf("expected 2, but got %d", p.Len())
	}
	if p.First() != bar {
		t.Errorf("expected 'bar', but got '%s'", p.First())
	}
	if p.Second() != foo {
		t.Errorf("expected 'foo', but got '%s'", p.Second())
	}
	if p.Third() != nil {
		t.Errorf("expected nil, but got '%s'", p.Third())
	}
	if p.String() != "(bar . foo)" {
		t.Errorf("expected (bar . foo) but got '%s'", p.String())
	}
}

func TestConsMultiple(t *testing.T) {
	foo := Symbol("foo")
	bar := Symbol("bar")
	baz := Symbol("baz")
	qux := Symbol("qux")
	p := Cons(baz, qux)
	p = Cons(bar, p)
	p = Cons(foo, p)
	if p.Len() != 4 {
		t.Errorf("expected 4, but got %d", p.Len())
	}
	if p.First() != foo {
		t.Errorf("expected 'foo', but got '%s'", p.First())
	}
	if p.Second() != bar {
		t.Errorf("expected 'bar', but got '%s'", p.Second())
	}
	if p.Third() != baz {
		t.Errorf("expected 'baz', but got '%s'", p.Third())
	}
	if p.String() != "(foo bar baz . qux)" {
		t.Errorf("expected (foo bar baz . qux) but got '%s'", p.String())
	}
	p = p.Reverse()
	if p.Len() != 4 {
		t.Errorf("expected 4, but got %d", p.Len())
	}
	if p.First() != qux {
		t.Errorf("expected 'qux', but got '%s'", p.First())
	}
	if p.Second() != baz {
		t.Errorf("expected 'baz', but got '%s'", p.Second())
	}
	if p.Third() != bar {
		t.Errorf("expected 'bar', but got '%s'", p.Third())
	}
	if p.String() != "(qux baz bar . foo)" {
		t.Errorf("expected (qux baz bar . foo) but got '%s'", p.String())
	}
}

func TestConsAppend(t *testing.T) {
	foo := Symbol("foo")
	bar := Symbol("bar")
	baz := Symbol("baz")
	qux := Symbol("qux")
	p := NewPair(foo)
	p.Append(bar)
	p.Append(baz)
	p.Append(qux)
	if p.Len() != 4 {
		t.Errorf("expected 4, but got %d", p.Len())
	}
	if p.First() != foo {
		t.Errorf("expected 'foo', but got '%s'", p.First())
	}
	if p.Second() != bar {
		t.Errorf("expected 'bar', but got '%s'", p.Second())
	}
	if p.Third() != baz {
		t.Errorf("expected 'baz', but got '%s'", p.Third())
	}
	if p.String() != "(foo bar baz qux)" {
		t.Errorf("expected (foo bar baz qux) but got '%s'", p.String())
	}
	p = p.Reverse()
	if p.Len() != 4 {
		t.Errorf("expected 4, but got %d", p.Len())
	}
	if p.First() != qux {
		t.Errorf("expected 'qux', but got '%s'", p.First())
	}
	if p.Second() != baz {
		t.Errorf("expected 'baz', but got '%s'", p.Second())
	}
	if p.Third() != bar {
		t.Errorf("expected 'bar', but got '%s'", p.Third())
	}
	if p.String() != "(qux baz bar . foo)" {
		t.Errorf("expected (qux baz bar . foo) but got '%s'", p.String())
	}
}

func TestCar(t *testing.T) {
	if Car(nil) != nil {
		t.Errorf("Car(nil) should be nil")
	}
	if Car("foo") != nil {
		t.Errorf("Car(\"foo\") should be nil")
	}
	foo := Symbol("foo")
	p := NewPair(foo)
	if Car(p) != foo {
		t.Errorf("Car() on non-empty pair should return first element")
	}
}

func TestCdr(t *testing.T) {
	if Cdr(nil) != nil {
		t.Errorf("Cdr(nil) should be nil")
	}
	if Cdr("foo") != nil {
		t.Errorf("Cdr(\"foo\") should be nil")
	}
	foo := Symbol("foo")
	p := NewPair(foo)
	if Cdr(p) != nil {
		t.Errorf("Cdr() of singleton pair should be nil")
	}
	bar := Symbol("bar")
	p.Append(bar)
	if r, ok := Cdr(p).(Pair); ok {
		if r.First() != bar {
			t.Errorf("first element of Cdr() incorrect")
		}
	} else {
		t.Errorf("Cdr() of list should return a pair")
	}
}

func verifyCxrSingle(t *testing.T, funk, expected string, p Pair) {
	x := Cxr(funk, p)
	if x == nil {
		t.Errorf("received nil unexpectedly for %s of %s", funk, p)
	} else {
		s := stringify(x)
		if s != expected {
			t.Errorf("expected %s for %s but got %s", expected, funk, s)
		}
	}
}

func verifyCxr(t *testing.T, expected map[string]string, p Pair) {
	for k, v := range expected {
		verifyCxrSingle(t, k, v, p)
	}
}

func verifyCxrTree(t *testing.T, expected map[string]string, input string) {
	for k, v := range expected {
		result, err := parseExpr(input)
		if err != nil {
			t.Error(err)
		}
		tree, ok := result.(Pair)
		if !ok {
			t.Error("result is not a tree!")
		}
		verifyCxrSingle(t, k, v, tree)
	}
}

func TestCxr(t *testing.T) {
	p := NewPair(Symbol("a"))
	p.Append(Symbol("b"))
	p.Append(Symbol("c"))
	p.Append(Symbol("d"))
	p.Append(Symbol("e"))
	p.Append(Symbol("f"))
	expected := make(map[string]string)
	expected["car"] = "a"
	expected["cdr"] = "(b c d e f)"
	expected["cddr"] = "(c d e f)"
	expected["cdddr"] = "(d e f)"
	expected["cddddr"] = "(e f)"
	expected["cadr"] = "b"
	expected["caddr"] = "c"
	expected["cadddr"] = "d"
	verifyCxr(t, expected, p)
	// The rest of the tests are not exactly thorough but writing a good
	// test is apparently too difficult for me. Already spent too much
	// time on this.
	expected = make(map[string]string)
	expected["caar"] = "#\\a"
	expected["cdar"] = "(#\\b #\\c . #\\d)"
	expected["cadar"] = "#\\b"
	expected["cddar"] = "(#\\c . #\\d)"
	expected["caddar"] = "#\\c"
	expected["cdddar"] = "#\\d"
	verifyCxrTree(t, expected, "((#\\a #\\b #\\c . #\\d) . #\\e)")
	expected = make(map[string]string)
	expected["caaar"] = "(#\\a (#\\b (#\\c . #\\d)))"
	expected["caaaar"] = "#\\a"
	expected["cdaar"] = "#\\e"
	expected["cdaaar"] = "((#\\b (#\\c . #\\d)))"
	verifyCxrTree(t, expected, "((((#\\a (#\\b (#\\c . #\\d))) . #\\e) . #\\f) . #\\g)")
}

func TestPairMap(t *testing.T) {
	foo := Symbol("foo")
	bar := Symbol("bar")
	baz := Symbol("baz")
	qux := Symbol("qux")
	p := NewPair(foo)
	p.Append(bar)
	p.Append(baz)
	p.Append(qux)
	f := func(a interface{}) interface{} {
		return fmt.Sprintf("%s", a)
	}
	newp := p.Map(f)
	if newp.String() != `("foo" "bar" "baz" "qux")` {
		t.Errorf("expected (\"foo\" \"bar\" \"baz\" \"qux\") but got '%s'", p.String())
	}
}

func TestNewList(t *testing.T) {
	foo := Symbol("foo")
	bar := Symbol("bar")
	baz := Symbol("baz")
	qux := Symbol("qux")
	p := NewList(foo, bar, baz, qux)
	if p.Len() != 4 {
		t.Errorf("expected 4, but got %d", p.Len())
	}
	if p.First() != foo {
		t.Errorf("expected 'foo', but got '%s'", p.First())
	}
	if p.Second() != bar {
		t.Errorf("expected 'bar', but got '%s'", p.Second())
	}
	if p.Third() != baz {
		t.Errorf("expected 'baz', but got '%s'", p.Third())
	}
	if p.String() != "(foo bar baz qux)" {
		t.Errorf("expected (foo bar baz qux) but got '%s'", p.String())
	}
}

func TestNestedList(t *testing.T) {
	foo := Symbol("foo")
	bar := Symbol("bar")
	baz := Symbol("baz")
	qux := Symbol("qux")
	s := NewList(bar, baz)
	p := NewList(foo, s, qux)
	if p.Len() != 3 {
		t.Errorf("expected 3, but got %d", p.Len())
	}
	if p.First() != foo {
		t.Errorf("expected 'foo', but got '%s'", p.First())
	}
	l := p.Second()
	if sp, ispair := l.(Pair); ispair {
		if sp.Len() != 2 {
			t.Errorf("expected 2, but got %d", sp.Len())
		}
		if sp.First() != bar {
			t.Errorf("expected 'bar', but got '%s'", p.First())
		}
		if sp.Second() != baz {
			t.Errorf("expected 'baz', but got '%s'", p.Second())
		}
	}
	if p.Third() != qux {
		t.Errorf("expected 'qux', but got '%s'", p.Third())
	}
	if p.String() != "(foo (bar baz) qux)" {
		t.Errorf("expected (foo (bar baz) qux) but got '%s'", p.String())
	}
}
