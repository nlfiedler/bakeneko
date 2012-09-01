//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

import (
	"bytes"
	"fmt"
	"unicode/utf8"
)

//
// Definition of atom types in our Scheme-like language, which includes
// (mutable) strings, numbers, booleans, and characters.
//

// Atom represents all things in our Scheme implementation which can be
// evaluated to a value outside of the context of a specific environment.
// These include strings, symbols, characters, and numbers.
type Atom interface {
	// Eval returns the result of evaluating this object.
	Eval() interface{}
	// String returns the string representation of this object.
	String() string
}

// Boolean represents a true/false value in Scheme.
type Boolean bool

// NewBoolean compares the input with the expected values for Scheme booleans
// (e.g. "#t", "#T", "#f", and "#F") and constructs a new Boolean atom. If val
// does not represent a boolean value then panic ensues.
func NewBoolean(val string) Boolean {
	if val == "#t" || val == "#T" {
		return Boolean(true)
	} else if val == "#f" || val == "#F" {
		// lexer already validated that it is #f or #F
		return Boolean(false)
	}
	panic(fmt.Sprintf("lexer/parser bug: '%s' is not boolean", val))
}

// Eval returns true or false depending on the value of the Boolean.
func (b Boolean) Eval() interface{} {
	return bool(b)
}

// String returns "#t" or "#f" depending on the value of the Boolean.
func (b Boolean) String() string {
	if bool(b) {
		return "#t"
	}
	return "#f"
}

// Symbol represents a variable or procedure name in a Scheme expression. It
// is essentially a string but is treated differently.
type Symbol string

// Eval returns the name of the symbol as a string.
func (s Symbol) Eval() interface{} {
	return string(s)
}

// String returns the name of the symbol.
func (s Symbol) String() string {
	return string(s)
}

// String represents a string of characters in Scheme. Strings in Scheme are
// mutable, unlike Go strings.
type String interface {
	Atom
	// Len returns the number of characters in this string.
	Len() int
	// Set changes the rune at the given zero-based position within the
	// string. If the position is out of bounds, panic ensues.
	Set(pos int, ch rune)
}

// StringImpl is an implementation of the String interface.
type StringImpl struct {
	// val is a mutable sequence of characters.
	val []rune
}

// NewString constructs a Scheme string from the given Go string.
func NewString(val string) String {
	// Convert the string to a slice of runes, which allows us to later
	// modify the string (i.e. when string-set! is called).
	sl := make([]rune, 0)
	pos := 0
	for pos < len(val) {
		r, width := utf8.DecodeRuneInString(val[pos:])
		sl = append(sl, r)
		pos += width
	}
	return &StringImpl{sl}
}

// Len returns the number of characters in this string.
func (s *StringImpl) Len() int {
	if s == nil {
		return -1
	}
	return len(s.val)
}

// Set changes the rune at the given zero-based position within the string.
// If the position is out of bounds, the function panics with OutOfBounds.
func (s *StringImpl) Set(pos int, ch rune) {
	if s == nil {
		return
	}
	if pos < 0 || pos >= len(s.val) {
		panic(OutOfBounds)
	}
	s.val[pos] = ch
}

// toString converts the slice of runes to a string.
func (s *StringImpl) toString() string {
	buf := new(bytes.Buffer)
	for _, r := range s.val {
		buf.WriteRune(r)
	}
	return buf.String()
}

// Eval returns the String as a Go string.
func (s *StringImpl) Eval() interface{} {
	if s == nil {
		return nil
	}
	return s.toString()
}

// String returns the String as a Go string.
func (s *StringImpl) String() string {
	if s == nil {
		return ""
	}
	return fmt.Sprintf("\"%s\"", s.toString())
}

// Character represents a single character (e.g. '#\\a' or '#\\space') in Scheme.
type Character rune

// NewCharacter creates an instance of Character to represent the given Scheme
// character. Characters are prefixed with #\, as in #\a for the letter 'a'.
// Special sequences are #\space for ' ' and #\newline for the newline
// character. Invalid input, such as a short string, will result in the
// uf8.RuneError character.
func NewCharacter(val string) Character {
	if val == "#\\space" {
		return Character(' ')
	} else if val == "#\\newline" {
		return Character('\n')
	} else if len(val) != 3 {
		return Character(utf8.RuneError)
	} else {
		// take whatever follows the #\ prefix
		return Character(val[2])
	}
	panic("unreachable code")
}

// Eval returns the character itself as a rune.
func (c Character) Eval() interface{} {
	return rune(c)
}

// String returns the Scheme representation of the character.
func (c Character) String() string {
	if c == ' ' {
		return "#\\space"
	} else if c == '\n' {
		return "#\\newline"
	}
	return fmt.Sprintf("#\\%c", c)
}

// Void is a placeholder in the environment for variables that have not been
// assigned a value, as in the letrec binding construct.
type Void int

// Eval returns nil since Void has no value.
func (v Void) Eval() interface{} {
	return nil
}

// String returns the empty string.
func (v Void) String() string {
	return ""
}

// TODO: introduce a Number type to aid converting between formats, performing operations

// Number represents all numbers in Scheme, from integers to rationals.
type Number interface {
	// Add the given value to this number and return a new number.
	Add(value interface{}) Number
	// Divide this number by the divisor and return a new Number.
	Divide(divisor interface{}) Number
	// Multiply this number by the multiplier and return a new Number.
	Multiply(multiplier interface{}) Number
	// Subtract the given value from this number and return a new number.
	Subtract(value interface{}) Number
}

type Integer int64

func NewInteger(val int64) Integer {
	return Integer(val)
}

func (i Integer) Add(value interface{}) Number {
	if fl, ok := value.(float64); ok {
		return NewFloat(float64(i) + fl)
	}

	if in, ok := value.(int64); ok {
		return NewInteger(int64(i) + in)
	}
	return nil
}

func (f Integer) Divide(divisor interface{}) Number {
	return nil // TODO: implement Integer.Divide()
}

func (f Integer) Multiply(muliplier interface{}) Number {
	return nil // TODO: implement Integer.Multiply()
}

func (f Integer) Subtract(value interface{}) Number {
	return nil // TODO: implement Integer.Subtract()
}

type Float float64

func NewFloat(val float64) Float {
	return Float(val)
}

func (f Float) Add(value interface{}) Number {
	return nil // TODO: implement Float.Add()
}

func (f Float) Divide(divisor interface{}) Number {
	return nil // TODO: implement Float.Divide()
}

func (f Float) Multiply(muliplier interface{}) Number {
	return nil // TODO: implement Float.Multiply()
}

func (f Float) Subtract(value interface{}) Number {
	return nil // TODO: implement Float.Subtract()
}
