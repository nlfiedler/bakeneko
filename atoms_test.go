//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

import (
	"testing"
	"unicode/utf8"
)

func TestBoolean(t *testing.T) {
	b := NewBoolean("#t")
	if boo, ok := b.Eval().(bool); !ok || !boo {
		t.Error("#t Boolean is not true")
	}
	if !b.Value() {
		t.Error("#t Boolean.Value() returned false")
	}
	if b.String() != "#t" {
		t.Errorf("Boolean.String() returned %v", b)
	}
	// test EqualTo()
	if _, err := b.EqualTo(nil); err == nil {
		t.Error("Boolean.EqualTo(nil) should return error")
	}
	if _, err := b.EqualTo(Character('a')); err == nil {
		t.Error("Boolean.EqualTo(rune) should return error")
	}
	bt := NewBoolean("#t")
	bf := NewBoolean("#f")
	if eq, err := b.EqualTo(bt); err != nil || !eq {
		t.Error("Boolean.EqualTo() true did not work")
	}
	if eq, err := b.EqualTo(bf); err != nil || eq {
		t.Error("Boolean.EqualTo() false did not work")
	}
	// test CompareTo()
	if _, err := b.CompareTo(nil); err == nil {
		t.Error("Boolean.CompareTo(nil) should return error")
	}
	if _, err := b.CompareTo(Character('a')); err == nil {
		t.Error("Boolean.CompareTo(rune) should return error")
	}
	if cmp, err := b.CompareTo(bt); err != nil || cmp != 0 {
		t.Error("Boolean.CompareTo() true did not work")
	}
	if cmp, err := bt.CompareTo(bf); err != nil || cmp <= 0 {
		t.Error("Boolean.CompareTo() true<=>false did not work")
	}
	if cmp, err := bf.CompareTo(bt); err != nil || cmp >= 0 {
		t.Error("Boolean.CompareTo() true<=>false did not work")
	}
}

func TestSymbol(t *testing.T) {
	s := Symbol("")
	if s.Eval() != "" {
		t.Error("empty Symbol should evaluate to empty string")
	}
	if s.String() != "" {
		t.Error("empty Symbol.String() should return empty string")
	}
	if _, err := s.EqualTo(Boolean(true)); err == nil {
		t.Error("Symbol.EqualTo() wrong type should fail")
	}
	if _, err := s.CompareTo(Boolean(true)); err == nil {
		t.Error("Symbol.CompareTo() wrong type should fail")
	}
	if eq, err := s.EqualTo(Symbol("")); err != nil || !eq {
		t.Error("Symbol.EqualTo() did not work")
	}
	if eq, err := s.EqualTo(Symbol("foo")); err != nil || eq {
		t.Error("Symbol.EqualTo() did not work")
	}
	if cmp, err := s.CompareTo(Symbol("")); err != nil || cmp != 0 {
		t.Error("Symbol.CompareTo() did not work")
	}
	if cmp, err := s.CompareTo(Symbol("foo")); err != nil || cmp == 0 {
		t.Error("Symbol.CompareTo() did not work")
	}
	s = Symbol("foo")
	if eq, err := s.EqualTo(Symbol("")); err != nil || eq {
		t.Error("Symbol.EqualTo() did not work")
	}
	if eq, err := s.EqualTo(Symbol("foo")); err != nil || !eq {
		t.Error("Symbol.EqualTo() did not work")
	}
	if cmp, err := s.CompareTo(Symbol("")); err != nil || cmp == 0 {
		t.Error("Symbol.CompareTo() did not work")
	}
	if cmp, err := s.CompareTo(Symbol("foo")); err != nil || cmp != 0 {
		t.Error("Symbol.CompareTo() did not work")
	}
	if cmp, err := s.CompareTo(Symbol("bar")); err != nil || cmp <= 0 {
		t.Error("Symbol.CompareTo() did not work")
	}
	if cmp, err := s.CompareTo(Symbol("qux")); err != nil || cmp >= 0 {
		t.Error("Symbol.CompareTo() did not work")
	}
}

func TestString(t *testing.T) {
	// nil String
	var ns *StringImpl = nil
	if ns.Eval() != nil {
		t.Error("nil String.Eval() should return nil")
	}
	// nil String Set() should do nothing
	ns.Set(0, 'c')
	if ns.String() != "" {
		t.Error("nil String.String() should return the empty string")
	}
	if ns.Len() != -1 {
		t.Error("nil String.Len() should return -1")
	}
	if _, err := ns.EqualTo(nil); err == nil {
		t.Error("nil String.EqualTo() should return error")
	}
	if _, err := ns.CompareTo(nil); err == nil {
		t.Error("nil String.CompareTo() should return error")
	}
	// empty String
	s := NewString("")
	if s.Eval() != "" {
		t.Error("empty String.Eval() should return the empty string")
	}

	// callStringSet is used to trigger a panic in String.Set(), returning
	// true if the call caused a panic, and false otherwise.
	callStringSet := func(s String, i int) (okay bool) {
		defer func() {
			if e := recover(); e != nil {
				// mark the test a success on the way out
				okay = true
			}
		}()
		s.Set(i, 'a')
		panic("unreachable code")
	}

	if !callStringSet(s, 0) {
		t.Error("String.Set() of emtpy string should have paniced")
	}
	if s.String() != "\"\"" {
		t.Error("empty String.String() should return the empty string")
	}
	if s.Len() != 0 {
		t.Error("empty String.Len() should return 0")
	}
	os := NewString("")
	result, err := s.EqualTo(os)
	if err != nil {
		t.Fatalf("empty String.EqualTo() returned error: %v", err)
	}
	if !result {
		t.Error("empty String.EqualTo() should return true")
	}
	if _, err = s.EqualTo(Boolean(true)); err == nil {
		t.Error("String.EqualTo() wrong type should have failed")
	}
	// non-empty String
	s = NewString("abc")
	if s.Eval() != "abc" {
		t.Error("unmodified String.Eval() should return original value")
	}
	s.Set(0, 'A')
	if s.Eval() != "Abc" {
		t.Error("modified String.Eval() should return modified value")
	}
	if !callStringSet(s, 1000) {
		t.Error("String.Set() out of bounds should have paniced")
	}
	if s.String() != "\"Abc\"" {
		t.Error("modified String.String() should return the modified value")
	}
	if s.Len() != 3 {
		t.Error("3-character String.Len() should return 3")
	}
	// test EqualTo()
	if result, err = s.EqualTo(NewString("Abc")); err != nil || !result {
		t.Error("String.EqualTo() should return true")
	}
	if result, err = s.EqualTo(NewString("abd")); err != nil || result {
		t.Error("String.EqualTo() should return false")
	}
	if result, err = s.EqualTo(NewString("ab")); err != nil || result {
		t.Error("String.EqualTo() should return false")
	}
	// test CompareTo()
	if cmp, err := s.CompareTo(NewString("Abc")); err != nil || cmp != 0 {
		t.Error("String.CompareTo() returned wrong value")
	}
	if cmp, err := s.CompareTo(NewString("Abd")); err != nil || cmp >= 0 {
		t.Error("String.CompareTo() returned wrong value")
	}
	if cmp, err := s.CompareTo(NewString("Abb")); err != nil || cmp <= 0 {
		t.Error("String.CompareTo() returned wrong value")
	}
}

func TestCharacter(t *testing.T) {
	// invalid inputs
	ch := NewCharacter("")
	if ch.Eval() != utf8.RuneError {
		t.Error("short character input should return error character")
	}
	ch = NewCharacter("#\\")
	if ch.Eval() != utf8.RuneError {
		t.Error("short character input should return error character")
	}
	ch = NewCharacter("#\\new")
	if ch.Eval() != utf8.RuneError {
		t.Error("long character input should return error character")
	}
	// letter 'n'
	ch = NewCharacter("#\\n")
	if ch.Eval() != 'n' {
		t.Error("short character input should return error character")
	}
	if ch.String() != "#\\n" {
		t.Errorf("Character.String() returned wrong value: %v", ch)
	}
	// special case: newline
	ch = NewCharacter("#\\newline")
	if ch.Eval() != '\n' {
		t.Error("newline special case failed")
	}
	if ch.String() != "#\\newline" {
		t.Errorf("Character.String() returned wrong value: %v", ch)
	}
	// special case: space
	ch = NewCharacter("#\\space")
	if ch.Eval() != ' ' {
		t.Error("space special case failed")
	}
	if ch.String() != "#\\space" {
		t.Errorf("Character.String() returned wrong value: %v", ch)
	}
	// test EqualTo()
	oc := NewCharacter("#\\a")
	eq, err := ch.EqualTo(oc)
	if err != nil {
		t.Fatalf("Character.EqualTo() returned error: %v", err)
	}
	if eq {
		t.Error("Character.EqualTo() should have returned false")
	}
	ch = NewCharacter("#\\a")
	eq, err = ch.EqualTo(oc)
	if err != nil {
		t.Fatalf("Character.EqualTo() returned error: %v", err)
	}
	if !eq {
		t.Error("Character.EqualTo() should have returned true")
	}
	if _, err = ch.EqualTo(Boolean(true)); err == nil {
		t.Error("Character.EqualTo() wrong type should have failed")
	}
	// test CompareTo()
	cmp, err := ch.CompareTo(oc)
	if err != nil {
		t.Fatalf("Character.EqualTo() returned error: %v", err)
	}
	if cmp != 0 {
		t.Error("Character.CompareTo() should have returned zero")
	}
	if _, err = ch.CompareTo(Boolean(true)); err == nil {
		t.Error("Character.CompareTo() wrong type should have failed")
	}
	oc = NewCharacter("#\\b")
	cmp, err = ch.CompareTo(oc)
	if err != nil {
		t.Fatalf("Character.EqualTo() returned error: %v", err)
	}
	if cmp >= 0 {
		t.Error("Character.CompareTo() should have returned negative value")
	}
	cmp, err = oc.CompareTo(ch)
	if err != nil {
		t.Fatalf("Character.EqualTo() returned error: %v", err)
	}
	if cmp <= 0 {
		t.Error("Character.CompareTo() should have returned positive value")
	}
}

func TestVoid(t *testing.T) {
	if theVoid.Eval() != nil {
		t.Error("Void should evaluate to nil")
	}
	if theVoid.String() != "" {
		t.Error("Void.String() should return the empty string")
	}
	if _, err := theVoid.EqualTo(Boolean(true)); err == nil {
		t.Error("Void.EqualTo() wrong type should have failed")
	}
	if eq, err := theVoid.EqualTo(Void(1)); err != nil || !eq {
		t.Error("Void.EqualTo() should have worked")
	}
	if _, err := theVoid.CompareTo(Boolean(true)); err == nil {
		t.Error("Void.CompareTo() wrong type should have failed")
	}
	if eq, err := theVoid.CompareTo(Void(1)); err != nil || eq != 0 {
		t.Error("Void.CompareTo() should have worked")
	}
}

func TestInteger(t *testing.T) {
	i := NewInteger(123)
	// test EqualTo()
	if _, err := i.EqualTo(Boolean(true)); err == nil {
		t.Error("Integer.EqualTo() wrong type should fail")
	}
	if eq, err := i.EqualTo(Integer(123)); err != nil || !eq {
		t.Error("Integer.EqualTo() same value should be true")
	}
	if eq, err := i.EqualTo(Integer(234)); err != nil || eq {
		t.Error("Integer.EqualTo() different value should be false")
	}
	// test CompareTo()
	if _, err := i.CompareTo(Boolean(true)); err == nil {
		t.Error("Integer.CompareTo() wrong type should fail")
	}
	if cmp, err := i.CompareTo(Integer(123)); err != nil || cmp != 0 {
		t.Error("Integer.CompareTo() same value should be zero")
	}
	if cmp, err := i.CompareTo(Integer(234)); err != nil || cmp >= 0 {
		t.Error("Integer.CompareTo() greater value should be negative")
	}
	if cmp, err := i.CompareTo(Integer(12)); err != nil || cmp <= 0 {
		t.Error("Integer.CompareTo() lesser value should be positive")
	}
	// test Add(), Divide(), Eval(), Multiply(), Subtract()
	if num, ok := i.Add(Integer(10)).Eval().(int64); !ok || num != 133 {
		t.Errorf("Integer.Add(10) yielded wrong result: %v", num)
	}
	if num, ok := i.Subtract(Integer(10)).Eval().(int64); !ok || num != 113 {
		t.Errorf("Integer.Subtract(10) yielded wrong result: %v", num)
	}
	if num, ok := i.Multiply(Integer(10)).Eval().(int64); !ok || num != 1230 {
		t.Errorf("Integer.Multiply(10) yielded wrong result: %v", num)
	}
	if num, ok := i.Divide(Integer(10)).Eval().(int64); !ok || num != 12 {
		t.Errorf("Integer.Divide(10) yielded wrong result: %v", num)
	}
	// test String()
	if i.String() != "123" {
		t.Error("Integer.String() yield wrong result")
	}
}

func TestFloat(t *testing.T) {
	f := NewFloat(1.2)
	// test EqualTo()
	if _, err := f.EqualTo(Boolean(true)); err == nil {
		t.Error("Float.EqualTo() wrong type should fail")
	}
	if eq, err := f.EqualTo(Float(1.2)); err != nil || !eq {
		t.Error("Float.EqualTo() same value should be true")
	}
	if eq, err := f.EqualTo(Float(1.1)); err != nil || eq {
		t.Error("Float.EqualTo() different value should be false")
	}
	// test CompareTo()
	if _, err := f.CompareTo(Boolean(true)); err == nil {
		t.Error("Float.CompareTo() wrong type should fail")
	}
	if cmp, err := f.CompareTo(Float(1.2)); err != nil || cmp != 0 {
		t.Error("Float.CompareTo() same value should be zero")
	}
	if cmp, err := f.CompareTo(Float(2.0)); err != nil || cmp >= 0 {
		t.Error("Float.CompareTo() greater value should be negative")
	}
	if cmp, err := f.CompareTo(Float(1.1)); err != nil || cmp <= 0 {
		t.Error("Float.CompareTo() lesser value should be positive")
	}
	// test Add(), Divide(), Eval(), Multiply(), Subtract()
	if num, ok := f.Add(Float(0.1)).Eval().(float64); !ok || num != 1.3 {
		t.Errorf("Float.Add(0.1) yielded wrong result: %v", num)
	}
	if num, ok := f.Subtract(Float(0.2)).Eval().(float64); !ok || num != 1.0 {
		t.Errorf("Float.Subtract(0.2) yielded wrong result: %v", num)
	}
	if num, ok := f.Multiply(Float(0.2)).Eval().(float64); !ok || num != 0.24 {
		t.Errorf("Float.Multiply(0.2) yielded wrong result: %v", num)
	}
	if num, ok := f.Divide(Float(0.3)).Eval().(float64); !ok || num != 4.0 {
		t.Errorf("Float.Divide(0.3) yielded wrong result: %v", num)
	}
	// test String()
	if f.String() != "1.2" {
		t.Error("Float.String() yield wrong result")
	}
}

func TestComplex(t *testing.T) {
	f := NewComplex(complex(1.2, 1.0))
	// test EqualTo()
	if _, err := f.EqualTo(Boolean(true)); err == nil {
		t.Error("Complex.EqualTo() wrong type should fail")
	}
	if eq, err := f.EqualTo(Complex(complex(1.2, 1.0))); err != nil || !eq {
		t.Error("Complex.EqualTo() same value should be true")
	}
	if eq, err := f.EqualTo(Complex(complex(1.1, 1.0))); err != nil || eq {
		t.Error("Complex.EqualTo() different value should be false")
	}
	// test CompareTo()
	if _, err := f.CompareTo(Boolean(true)); err == nil {
		t.Error("Complex.CompareTo() wrong type should fail")
	}
	if cmp, err := f.CompareTo(Complex(complex(1.2, 1.0))); err != nil || cmp != 0 {
		t.Error("Complex.CompareTo() same value should be zero")
	}
	if cmp, err := f.CompareTo(Complex(complex(2.0, 1.0))); err != nil || cmp >= 0 {
		t.Error("Complex.CompareTo() greater value should be negative")
	}
	if cmp, err := f.CompareTo(Complex(complex(1.1, 1.0))); err != nil || cmp <= 0 {
		t.Error("Complex.CompareTo() lesser value should be positive")
	}
	// test Add(), Divide(), Eval(), Multiply(), Subtract()
	if num, ok := f.Add(Complex(complex(0.1, 1.0))).Eval().(complex128); !ok ||
		num != complex(1.3, 2.0) {
		t.Errorf("Complex.Add(0.1) yielded wrong result: %v", num)
	}
	if num, ok := f.Subtract(Complex(complex(0.2, 1.0))).Eval().(complex128); !ok || num != 1.0 {
		t.Errorf("Complex.Subtract(0.2) yielded wrong result: %v", num)
	}
	if num, ok := f.Multiply(Complex(complex(0.2, 1.0))).Eval().(complex128); !ok ||
		num != complex(-0.76, 1.4) {
		t.Errorf("Complex.Multiply(0.2) yielded wrong result: %v", num)
	}
	if num, ok := f.Divide(Complex(complex(0.3, 1.0))).Eval().(complex128); !ok ||
		num != complex(1.2477064220183485, -0.8256880733944952) {
		t.Errorf("Complex.Divide(0.3) yielded wrong result: %v", num)
	}
	// test String()
	if f.String() != "1.2+1i" {
		t.Errorf("Complex.String() yield wrong result: %s", f.String())
	}
}
