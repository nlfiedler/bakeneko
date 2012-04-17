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
	var p *Pair
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
	if r, ok := Cdr(p).(*Pair); ok {
		if r.First() != bar {
			t.Errorf("first element of Cdr() incorrect")
		}
	} else {
		t.Errorf("Cdr() of list should return a pair")
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
	// TODO: for the rest, need a nested structure
	// expected["caar"] = "b"
	// expected["caaar"] = "c"
	// expected["caaaar"] = "d"
	for k, v := range expected {
		x := Cxr(k, p)
		if x == nil {
			t.Errorf("received nil unexpectedly for %s of %s", k, p)
		} else {
			s := stringify(x)
			if s != v {
				t.Errorf("expected %s for %s but got %s", v, k, s)
			}
		}
	}
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
	if sp, ispair := l.(*Pair); ispair {
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
