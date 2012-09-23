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
	// CompareTo returns a negative integer, zero, or a positive integer
	// as this object is less than, equal to, or greater than the
	// specified object. An error is returned if the object is not of
	// a suitable type for comparison.
	CompareTo(other Atom) (int8, error)
	// EqualTo returns true if the values of this atom and the specified
	// object are equal, false otherwise. An error is returned if the
	// object is not of the same type.
	EqualTo(other Atom) (bool, error)
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

// CompareTo returns zero if this object represents the same boolean value as
// the argument; a positive value if this object represents true and the
// argument represents false; and a negative value if this object represents
// false and the argument represents true.
func (b Boolean) CompareTo(other Atom) (int8, error) {
	if ob, ok := other.(Boolean); ok {
		bb := bool(b)
		obb := bool(ob)
		if bb == obb {
			return 0, nil
		} else if bb {
			return 1, nil
		} else {
			return -1, nil
		}
	}
	return 0, TypeMismatch
}

func (b Boolean) EqualTo(other Atom) (bool, error) {
	if ob, ok := other.(Boolean); ok {
		return bool(b) == bool(ob), nil
	}
	return false, TypeMismatch
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

// Returns the boolean value.
func (b Boolean) Value() bool {
	return bool(b)
}

// Symbol represents a variable or procedure name in a Scheme expression. It
// is essentially a string but is treated differently.
type Symbol string

func (s Symbol) CompareTo(other Atom) (int8, error) {
	if os, ok := other.(Symbol); ok {
		if s == os {
			return 0, nil
		} else if s > os {
			return 1, nil
		} else {
			return -1, nil
		}
	}
	return 0, TypeMismatch
}

func (s Symbol) EqualTo(other Atom) (bool, error) {
	if os, ok := other.(Symbol); ok {
		return s == os, nil
	}
	return false, TypeMismatch
}

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
	// strng is the string value, if and only if the slice field is nil
	strng string
	// slice is the mutable version of this string, and if non-nil this
	// field overrides any value in the strng field
	slice []rune
}

// NewString constructs a Scheme string from the given Go string.
func NewString(val string) String {
	return &StringImpl{val, nil}
}

// Len returns the number of characters in this string.
func (s *StringImpl) Len() int {
	if s == nil {
		return -1
	}
	if s.slice != nil {
		return len(s.slice)
	}
	return len(s.strng)
}

// Set changes the rune at the given zero-based position within the string.
// If the position is out of bounds, the function panics with OutOfBounds.
func (s *StringImpl) Set(pos int, ch rune) {
	if s == nil {
		return
	}
	if s.slice == nil {
		// convert the string to a slice of runes
		s.slice = make([]rune, 0)
		idx := 0
		for idx < len(s.strng) {
			r, width := utf8.DecodeRuneInString(s.strng[idx:])
			s.slice = append(s.slice, r)
			idx += width
		}
	}
	if pos < 0 || pos >= len(s.slice) {
		panic(OutOfBounds)
	}
	s.slice[pos] = ch
}

// toString converts the slice of runes to a string, saving the string
// value to avoid the cost of conversion in the future.
func (s *StringImpl) toString() string {
	if s.slice != nil {
		// convert the slice of runes back to a string
		buf := new(bytes.Buffer)
		for _, r := range s.slice {
			buf.WriteRune(r)
		}
		s.strng = buf.String()
		s.slice = nil
	}
	return s.strng
}

func (s *StringImpl) CompareTo(other Atom) (int8, error) {
	if os, ok := other.(*StringImpl); ok {
		ost := os.toString()
		st := s.toString()
		if st == ost {
			return 0, nil
		} else if st > ost {
			return 1, nil
		} else {
			return -1, nil
		}
	}
	return 0, TypeMismatch
}

func (s *StringImpl) EqualTo(other Atom) (bool, error) {
	if os, ok := other.(*StringImpl); ok {
		ost := os.toString()
		st := s.toString()
		return st == ost, nil
	}
	return false, TypeMismatch
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
	}
	// take whatever follows the #\ prefix
	return Character(val[2])
}

func (c Character) CompareTo(other Atom) (int8, error) {
	if oc, ok := other.(Character); ok {
		if c == oc {
			return 0, nil
		} else if c < oc {
			return -1, nil
		}
		return 1, nil
	}
	return 0, TypeMismatch
}

func (c Character) EqualTo(other Atom) (bool, error) {
	if oc, ok := other.(Character); ok {
		return c == oc, nil
	}
	return false, TypeMismatch
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

func (v Void) CompareTo(other Atom) (int8, error) {
	if _, ok := other.(Void); ok {
		return 0, nil
	}
	return 0, TypeMismatch
}

func (v Void) EqualTo(other Atom) (bool, error) {
	if _, ok := other.(Void); ok {
		return true, nil
	}
	return false, TypeMismatch
}

// Eval returns nil since Void has no value.
func (v Void) Eval() interface{} {
	return nil
}

// String returns the empty string.
func (v Void) String() string {
	return ""
}

// Number represents all numbers in Scheme, from integers to rationals.
type Number interface {
	Atom
	// Add the given value to this number and return a new number.
	Add(value interface{}) Number
	// Divide this number by the divisor and return a new Number.
	Divide(divisor interface{}) Number
	// Multiply this number by the multiplier and return a new Number.
	Multiply(multiplier interface{}) Number
	// Subtract the given value from this number and return a new number.
	Subtract(value interface{}) Number
	// IntegerValue returns the number as an Integer.
	IntegerValue() Integer
	// FloatValue returns the number as an Float.
	FloatValue() Float
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

func (i Integer) Divide(divisor interface{}) Number {
	return nil // TODO: implement Integer.Divide()
}

func (i Integer) Multiply(muliplier interface{}) Number {
	return nil // TODO: implement Integer.Multiply()
}

func (i Integer) Subtract(value interface{}) Number {
	return nil // TODO: implement Integer.Subtract()
}

func (i Integer) CompareTo(other Atom) (int8, error) {
	if oi, ok := other.(Integer); ok {
		return int8(int64(i) - int64(oi)), nil
	}
	return 0, TypeMismatch
}

func (i Integer) EqualTo(other Atom) (bool, error) {
	if oi, ok := other.(Integer); ok {
		return int64(i) == int64(oi), nil
	}
	return false, TypeMismatch
}

func (i Integer) Eval() interface{} {
	return i
}

// IntegerValue returns the number as an Integer.
func (i Integer) IntegerValue() Integer {
	return i
}

func (i Integer) FloatValue() Float {
	return NewFloat(float64(i))
}

func (i Integer) String() string {
	return fmt.Sprint(int64(i))
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

func (f Float) CompareTo(other Atom) (int8, error) {
	if of, ok := other.(Float); ok {
		ff := float64(f)
		off := float64(of)
		if ff == off {
			return 0, nil
		} else if ff > off {
			return 1, nil
		} else {
			return -1, nil
		}
	}
	return 0, TypeMismatch
}

func (f Float) EqualTo(other Atom) (bool, error) {
	if of, ok := other.(Float); ok {
		return float64(f) == float64(of), nil
	}
	return false, TypeMismatch
}

func (f Float) Eval() interface{} {
	return f
}

// IntegerValue returns the number as an Integer.
func (f Float) IntegerValue() Integer {
	return NewInteger(int64(f))
}

func (f Float) FloatValue() Float {
	return f
}

func (f Float) String() string {
	return fmt.Sprint(float64(f))
}

// TODO: add rational number type (Go has rational number support in math.big package)

// TODO: add complex number type
