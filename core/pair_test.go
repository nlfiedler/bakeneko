//
// Copyright 2012-2014 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	"fmt"
	gc "launchpad.net/gocheck"
	"testing"
)

type PairSuite struct {
}

var _ = gc.Suite(&PairSuite{})

func (ps *PairSuite) TestPairNil(c *gc.C) {
	var p *pair = nil
	c.Check(p.Len(), gc.Equals, 0, gc.Commentf("nil Pair.Len() should be 0"))
	c.Check(p.First(), gc.IsNil, gc.Commentf("nil Pair.First() should be nil"))
	c.Check(p.Second(), gc.IsNil, gc.Commentf("nil Pair.Second() should be nil"))
	c.Check(p.Third(), gc.IsNil, gc.Commentf("nil Pair.Third() should be nil"))
	c.Check(p.String(), gc.Equals, "()", gc.Commentf("nil Pair.String() should be ()"))
	c.Check(p.Reverse(), gc.IsNil, gc.Commentf("nil Pair.Reverse() should be nil"))
}

func (ps *PairSuite) TestPairSingle(c *gc.C) {
	foo := NewSymbol("foo")
	p := NewPair(foo)
	c.Check(p, gc.NotNil)
	var seq Sequence
	c.Check(p, gc.Implements, &seq)
	c.Check(p.Len(), gc.Equals, 1, gc.Commentf("pair single len"))
	c.Check(p.First(), gc.Equals, foo, gc.Commentf("pair single first"))
	c.Check(p.Second(), gc.IsNil, gc.Commentf("pair single second"))
	c.Check(p.Third(), gc.IsNil, gc.Commentf("pair single third"))
	c.Check(p.String(), gc.Equals, "(foo)", gc.Commentf("pair single string"))
	slice := p.ToSlice()
	c.Check(len(slice), gc.Equals, 1)
	c.Check(slice[0], gc.Equals, foo)
	p = p.Reverse()
	c.Check(p.Len(), gc.Equals, 1, gc.Commentf("pair single len"))
	c.Check(p.First(), gc.Equals, foo, gc.Commentf("pair single first"))
	c.Check(p.Second(), gc.IsNil, gc.Commentf("pair single second"))
	c.Check(p.Third(), gc.IsNil, gc.Commentf("pair single third"))
	c.Check(p.String(), gc.Equals, "(foo)", gc.Commentf("pair single string"))
}

func (ps *PairSuite) TestCons(c *gc.C) {
	foo := NewSymbol("foo")
	bar := NewSymbol("bar")
	p := Cons(foo, bar)
	c.Check(p.Len(), gc.Equals, 2, gc.Commentf("improper cons len"))
	c.Check(p.First(), gc.Equals, foo, gc.Commentf("improper cons first"))
	c.Check(p.Second(), gc.Equals, bar, gc.Commentf("improper cons second"))
	c.Check(p.Third(), gc.IsNil, gc.Commentf("improper cons third"))
	c.Check(p.String(), gc.Equals, "(foo . bar)", gc.Commentf("improper cons string"))
	slice := p.ToSlice()
	c.Check(len(slice), gc.Equals, 2)
	c.Check(slice[0], gc.Equals, foo)
	c.Check(slice[1], gc.Equals, bar)
	p = p.Reverse()
	c.Check(p.Len(), gc.Equals, 2, gc.Commentf("reversed improper cons len"))
	c.Check(p.First(), gc.Equals, bar, gc.Commentf("reversed improper cons first"))
	c.Check(p.Second(), gc.Equals, foo, gc.Commentf("reversed improper cons second"))
	c.Check(p.Third(), gc.IsNil, gc.Commentf("reversed improper cons third"))
	c.Check(p.String(), gc.Equals, "(bar . foo)", gc.Commentf("reversed improper cons string"))
}

func (ps *PairSuite) TestConsMultiple(c *gc.C) {
	foo := NewSymbol("foo")
	bar := NewSymbol("bar")
	baz := NewSymbol("baz")
	qux := NewSymbol("qux")
	p := Cons(baz, qux)
	p = Cons(bar, p)
	p = Cons(foo, p)
	c.Check(p.Len(), gc.Equals, 4, gc.Commentf("improper list len"))
	c.Check(p.First(), gc.Equals, foo, gc.Commentf("improper list first"))
	c.Check(p.Second(), gc.Equals, bar, gc.Commentf("improper list second"))
	c.Check(p.Third(), gc.Equals, baz, gc.Commentf("improper list third"))
	c.Check(p.String(), gc.Equals, "(foo bar baz . qux)", gc.Commentf("improper list string"))
	p = p.Reverse()
	c.Check(p.Len(), gc.Equals, 4, gc.Commentf("reversed improper list len"))
	c.Check(p.First(), gc.Equals, qux, gc.Commentf("reversed improper list first"))
	c.Check(p.Second(), gc.Equals, baz, gc.Commentf("reversed improper list second"))
	c.Check(p.Third(), gc.Equals, bar, gc.Commentf("reversed improper list third"))
	c.Check(p.String(), gc.Equals, "(qux baz bar . foo)", gc.Commentf("reversed improper list string"))
}

func (ps *PairSuite) TestAppend(c *gc.C) {
	foo := NewSymbol("foo")
	bar := NewSymbol("bar")
	baz := NewSymbol("baz")
	qux := NewSymbol("qux")
	p := NewPair(foo)
	p.Append(bar)
	p.Append(baz)
	p.Append(qux)
	c.Check(p.Len(), gc.Equals, 4, gc.Commentf("append list len"))
	c.Check(p.First(), gc.Equals, foo, gc.Commentf("append list first"))
	c.Check(p.Second(), gc.Equals, bar, gc.Commentf("append list second"))
	c.Check(p.Third(), gc.Equals, baz, gc.Commentf("append list third"))
	c.Check(p.String(), gc.Equals, "(foo bar baz qux)", gc.Commentf("append list string"))
	p = p.Reverse()
	c.Check(p.Len(), gc.Equals, 4, gc.Commentf("reversed append list len"))
	c.Check(p.First(), gc.Equals, qux, gc.Commentf("reversed append list first"))
	c.Check(p.Second(), gc.Equals, baz, gc.Commentf("reversed append list second"))
	c.Check(p.Third(), gc.Equals, bar, gc.Commentf("reversed append list third"))
	c.Check(p.String(), gc.Equals, "(qux baz bar foo)", gc.Commentf("reversed append list string"))
}

func (ps *PairSuite) TestAppendChain(c *gc.C) {
	foo := NewSymbol("foo")
	bar := NewSymbol("bar")
	baz := NewSymbol("baz")
	qux := NewSymbol("qux")
	p := NewPair(foo)
	p.Append(bar).Append(baz).Append(qux)
	c.Check(p.Len(), gc.Equals, 4, gc.Commentf("chain list len"))
	c.Check(p.First(), gc.Equals, foo, gc.Commentf("chain list first"))
	c.Check(p.Second(), gc.Equals, bar, gc.Commentf("chain list second"))
	c.Check(p.Third(), gc.Equals, baz, gc.Commentf("chain list third"))
	c.Check(p.String(), gc.Equals, "(foo bar baz qux)", gc.Commentf("chain list string"))
	p = p.Reverse()
	c.Check(p.Len(), gc.Equals, 4, gc.Commentf("reversed chain list len"))
	c.Check(p.First(), gc.Equals, qux, gc.Commentf("reversed chain list first"))
	c.Check(p.Second(), gc.Equals, baz, gc.Commentf("reversed chain list second"))
	c.Check(p.Third(), gc.Equals, bar, gc.Commentf("reversed chain list third"))
	c.Check(p.String(), gc.Equals, "(qux baz bar foo)", gc.Commentf("reversed chain list string"))
}

func (ps *PairSuite) TestCar(c *gc.C) {
	c.Check(Car(nil), gc.IsNil, gc.Commentf("Car(nil) should be nil"))
	c.Check(Car("foo"), gc.IsNil, gc.Commentf(`Car("foo") should be nil`))
	foo := NewSymbol("foo")
	p := NewPair(foo)
	c.Check(Car(p), gc.Equals, foo, gc.Commentf(`Car() should return first of list`))
}

func TestCdr(t *testing.T) {
	if Cdr(nil) != nil {
		t.Errorf("Cdr(nil) should be nil")
	}
	if Cdr("foo") != nil {
		t.Errorf("Cdr(\"foo\") should be nil")
	}
	foo := NewSymbol("foo")
	p := NewPair(foo)
	if Cdr(p) != nil {
		t.Errorf("Cdr() of singleton pair should be nil")
	}
	bar := NewSymbol("bar")
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
		parser := NewParser()
		result, err := parser.Parse(input)
		if err != nil {
			t.Error(err)
		}
		tree, ok := result.First().(Pair)
		if !ok {
			t.Error("result is not a tree!")
		}
		verifyCxrSingle(t, k, v, tree)
	}
}

func TestCxr(t *testing.T) {
	p := NewPair(NewSymbol("a"))
	p.Append(NewSymbol("b"))
	p.Append(NewSymbol("c"))
	p.Append(NewSymbol("d"))
	p.Append(NewSymbol("e"))
	p.Append(NewSymbol("f"))
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
	foo := NewSymbol("foo")
	bar := NewSymbol("bar")
	baz := NewSymbol("baz")
	qux := NewSymbol("qux")
	p := NewPair(foo)
	p.Append(bar)
	p.Append(baz)
	p.Append(qux)
	f := func(a interface{}) interface{} {
		return NewString(fmt.Sprint(a))
	}
	newp := p.Map(f)
	if newp.String() != `("foo" "bar" "baz" "qux")` {
		t.Errorf("expected (\"foo\" \"bar\" \"baz\" \"qux\") but got '%s'", p.String())
	}
}

func (ps *PairSuite) TestNewList(c *gc.C) {
	foo := NewSymbol("foo")
	bar := NewSymbol("bar")
	baz := NewSymbol("baz")
	qux := NewSymbol("qux")
	p := NewList(foo, bar, baz, qux)
	c.Check(p.Len(), gc.Equals, 4, gc.Commentf("new list len"))
	c.Check(p.First(), gc.Equals, foo, gc.Commentf("new list first"))
	c.Check(p.Second(), gc.Equals, bar, gc.Commentf("new list second"))
	c.Check(p.Third(), gc.Equals, baz, gc.Commentf("new list third"))
	c.Check(p.String(), gc.Equals, "(foo bar baz qux)", gc.Commentf("new list string"))
	p = p.Reverse()
	c.Check(p.Len(), gc.Equals, 4, gc.Commentf("new list len"))
	c.Check(p.First(), gc.Equals, qux, gc.Commentf("new list first"))
	c.Check(p.Second(), gc.Equals, baz, gc.Commentf("new list second"))
	c.Check(p.Third(), gc.Equals, bar, gc.Commentf("new list third"))
	c.Check(p.String(), gc.Equals, "(qux baz bar foo)", gc.Commentf("new list string"))
}

func TestNestedList(t *testing.T) {
	foo := NewSymbol("foo")
	bar := NewSymbol("bar")
	baz := NewSymbol("baz")
	qux := NewSymbol("qux")
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

func (ps *PairSuite) TestPairIterator(c *gc.C) {
	foo := NewSymbol("foo")
	bar := NewSymbol("bar")
	baz := NewSymbol("baz")
	qux := NewSymbol("qux")
	p := NewList(foo, bar, baz, qux)
	iter := p.Iterator()
	c.Check(iter.HasNext(), gc.Equals, true)
	c.Check(iter.Next(), gc.Equals, foo)
	c.Check(iter.IsProper(), gc.Equals, true)
	c.Check(iter.HasNext(), gc.Equals, true)
	c.Check(iter.Next(), gc.Equals, bar)
	c.Check(iter.IsProper(), gc.Equals, true)
	c.Check(iter.HasNext(), gc.Equals, true)
	c.Check(iter.Next(), gc.Equals, baz)
	c.Check(iter.IsProper(), gc.Equals, true)
	c.Check(iter.HasNext(), gc.Equals, true)
	c.Check(iter.Next(), gc.Equals, qux)
	c.Check(iter.IsProper(), gc.Equals, true)
	c.Check(iter.HasNext(), gc.Equals, false)
	c.Check(iter.Next(), gc.IsNil)
}

func (ps *PairSuite) TestPairIteratorImproper(c *gc.C) {
	foo := NewSymbol("foo")
	bar := NewSymbol("bar")
	p := Cons(foo, bar)
	iter := p.Iterator()
	c.Check(iter.IsProper(), gc.Equals, true)
	c.Check(iter.HasNext(), gc.Equals, true)
	c.Check(iter.Next(), gc.Equals, foo)
	c.Check(iter.IsProper(), gc.Equals, true)
	c.Check(iter.HasNext(), gc.Equals, true)
	c.Check(iter.Next(), gc.Equals, bar)
	c.Check(iter.IsProper(), gc.Equals, false)
	c.Check(iter.HasNext(), gc.Equals, false)
}

func (ps *PairSuite) TestPairImproperSlice(c *gc.C) {
	foo := NewSymbol("foo")
	bar := NewSymbol("bar")
	baz := NewSymbol("baz")
	qux := NewSymbol("qux")
	p := NewList(foo, bar, baz)
	p.Join(qux)
	slice := p.ToSlice()
	c.Check(len(slice), gc.Equals, 4)
	c.Check(slice[0], gc.Equals, NewSymbol("foo"))
	c.Check(slice[1], gc.Equals, NewSymbol("bar"))
	c.Check(slice[2], gc.Equals, NewSymbol("baz"))
	c.Check(slice[3], gc.Equals, NewSymbol("qux"))
}

func (ps *PairSuite) TestPairIteratorEmpty(c *gc.C) {
	iter := theEmptyList.Iterator()
	c.Check(iter.HasNext(), gc.Equals, false)
	c.Check(iter.IsProper(), gc.Equals, true)
	c.Check(iter.Next(), gc.IsNil)
}

func (ps *PairSuite) TestPairBuilder(c *gc.C) {
	joiner := NewPairBuilder()
	c.Check(joiner.List(), gc.Equals, theEmptyList)
	joiner.Append(NewSymbol("foo"))
	c.Check(joiner.List().String(), gc.Equals, "(foo)")
	joiner.Append(NewSymbol("bar"))
	c.Check(joiner.List().String(), gc.Equals, "(foo bar)")
	joiner.Append(NewSymbol("qux"))
	c.Check(joiner.List().String(), gc.Equals, "(foo bar qux)")
	joiner.Join(NewSymbol("baz"))
	c.Check(joiner.List().String(), gc.Equals, "(foo bar qux . baz)")
	slice := joiner.List().ToSlice()
	c.Check(len(slice), gc.Equals, 4)
	c.Check(slice[0], gc.Equals, NewSymbol("foo"))
	c.Check(slice[1], gc.Equals, NewSymbol("bar"))
	c.Check(slice[2], gc.Equals, NewSymbol("qux"))
	c.Check(slice[3], gc.Equals, NewSymbol("baz"))
}

func (ps *PairSuite) TestPairInfinite(c *gc.C) {
	foo := NewSymbol("foo")
	bar := NewSymbol("bar")
	pair := NewList(foo, bar)
	pair.Append(pair)
	c.Check(pair.Len(), gc.Equals, 3)
	reversed := pair.Reverse()
	c.Check(reversed.Len(), gc.Equals, 3)
	// TODO: should first be the new reversed list, instead of the old list?
	c.Check(reversed.First(), gc.Equals, pair)
	c.Check(reversed.Second(), gc.Equals, bar)
	c.Check(reversed.Third(), gc.Equals, foo)
	// TODO: test String(); should not loop indefinitely; perhaps use datum labels?
	// TODO: test String() with a sublist that refers to the superlist
}
