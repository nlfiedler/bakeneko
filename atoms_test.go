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

func TestStringNil(t *testing.T) {
	var s *StringImpl = nil
	if s.Eval() != nil {
		t.Error("nil String.Eval() should return nil")
	}
	// nil String Set() should do nothing
	s.Set(0, 'c')
	result := s.String()
	if result != "" {
		t.Error("nil String.String() should return the empty string")
	}
	if s.Len() != -1 {
		t.Error("nil String.Len() should return -1")
	}
}

func TestStringEmpty(t *testing.T) {
	s := NewString("")
	if s.Eval() != "" {
		t.Error("empty String.Eval() should return the empty string")
	}
	paniced := false
	defer func() {
		if e := recover(); e != nil {
			paniced = true
		}
	}()
	s.Set(0, 'c')
	if !paniced {
		t.Error("String.Set() of emtpy string should have paniced")
	}
	if s.String() != "\"\"" {
		t.Error("empty String.String() should return the empty string")
	}
	if s.Len() != 0 {
		t.Error("empty String.Len() should return 0")
	}
}

func TestStringNonEmpty(t *testing.T) {
	s := NewString("abc")
	if s.Eval() != "abc" {
		t.Error("unmodified String.Eval() should return original value")
	}
	s.Set(0, 'A')
	if s.Eval() != "Abc" {
		t.Error("modified String.Eval() should return modified value")
	}
	paniced := false
	defer func() {
		if e := recover(); e != nil {
			paniced = true
		}
	}()
	s.Set(1000, 'c')
	if !paniced {
		t.Error("String.Set() out of bounds should have paniced")
	}
	if s.String() != "Abc" {
		t.Error("modified String.String() should return the modified value")
	}
	if s.Len() != 3 {
		t.Error("3-character String.Len() should return 3")
	}
}

func TestCharacter(t *testing.T) {
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
	ch = NewCharacter("#\\n")
	if ch.Eval() != 'n' {
		t.Error("short character input should return error character")
	}
}

func TestVoid(t *testing.T) {
	if theVoid.Eval() != nil {
		t.Error("Void should evaluate to nil")
	}
	if theVoid.String() != "" {
		t.Error("Void.String() should return the empty string")
	}
}
