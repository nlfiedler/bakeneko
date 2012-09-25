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
	Add(value Number) Number
	// Divide this number by the divisor and return a new Number.
	Divide(divisor Number) Number
	// Multiply this number by the multiplier and return a new Number.
	Multiply(multiplier Number) Number
	// Subtract the given value from this number and return a new number.
	Subtract(value Number) Number
	// ComplexValue returns the number as a Complex.
	ComplexValue() Complex
	// IntegerValue returns the number as an Integer.
	IntegerValue() Integer
	// FloatValue returns the number as an Float.
	FloatValue() Float
}

type Integer int64

func NewInteger(val int64) Integer {
	return Integer(val)
}

func (i Integer) Add(value Number) Number {
	vi := value.IntegerValue()
	return NewInteger(int64(i) + int64(vi))
}

func (i Integer) Divide(divisor Number) Number {
	vi := divisor.IntegerValue()
	return NewInteger(int64(i) / int64(vi))
}

func (i Integer) Multiply(muliplier Number) Number {
	vi := muliplier.IntegerValue()
	return NewInteger(int64(i) * int64(vi))
}

func (i Integer) Subtract(value Number) Number {
	vi := value.IntegerValue()
	return NewInteger(int64(i) - int64(vi))
}

func (i Integer) CompareTo(other Atom) (int8, error) {
	if oi, ok := other.(Integer); ok {
		ii := int64(i)
		ioi := int64(oi)
		if ii < ioi {
			return -1, nil
		} else if ii > ioi {
			return 1, nil
		}
		return 0, nil
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
	return int64(i)
}

func (i Integer) ComplexValue() Complex {
	return NewComplex(complex(float64(i), 0.0))
}

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

func (f Float) Add(value Number) Number {
	vf := value.FloatValue()
	return NewFloat(float64(f) + float64(vf))
}

func (f Float) Divide(divisor Number) Number {
	vf := divisor.FloatValue()
	return NewFloat(float64(f) / float64(vf))
}

func (f Float) Multiply(muliplier Number) Number {
	vf := muliplier.FloatValue()
	return NewFloat(float64(f) * float64(vf))
}

func (f Float) Subtract(value Number) Number {
	vf := value.FloatValue()
	return NewFloat(float64(f) - float64(vf))
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
	return float64(f)
}

func (f Float) ComplexValue() Complex {
	return NewComplex(complex(f, 0.0))
}

func (f Float) IntegerValue() Integer {
	return NewInteger(int64(f))
}

func (f Float) FloatValue() Float {
	return f
}

func (f Float) String() string {
	return fmt.Sprint(float64(f))
}

type Complex complex128

func NewComplex(val complex128) Complex {
	return Complex(val)
}

func (c Complex) Add(value Number) Number {
	vc := value.ComplexValue()
	return NewComplex(complex128(c) + complex128(vc))
}

func (c Complex) Divide(divisor Number) Number {
	vc := divisor.ComplexValue()
	return NewComplex(complex128(c) / complex128(vc))
}

func (c Complex) Multiply(muliplier Number) Number {
	vc := muliplier.ComplexValue()
	return NewComplex(complex128(c) * complex128(vc))
}

func (c Complex) Subtract(value Number) Number {
	vc := value.ComplexValue()
	return NewComplex(complex128(c) - complex128(vc))
}

func (c Complex) CompareTo(other Atom) (int8, error) {
	if oc, ok := other.(Complex); ok {
		cc := complex128(c)
		occ := complex128(oc)
		if cc == occ {
			return 0, nil
		} else if real(cc) > real(occ) {
			return 1, nil
		} else {
			return -1, nil
		}
	}
	return 0, TypeMismatch
}

func (c Complex) EqualTo(other Atom) (bool, error) {
	if oc, ok := other.(Complex); ok {
		return complex128(c) == complex128(oc), nil
	}
	return false, TypeMismatch
}

func (c Complex) Eval() interface{} {
	return complex128(c)
}

func (c Complex) IntegerValue() Integer {
	return NewInteger(int64(real(complex128(c))))
}

func (c Complex) FloatValue() Float {
	return Float(real(complex128(c)))
}

func (c Complex) ComplexValue() Complex {
	return c
}

func (c Complex) String() string {
	// return the number without the parentheses
	str := fmt.Sprint(complex128(c))
	return str[1 : len(str)-1]
}

// TODO: add rational number type (Go has rational number support in math.big package)
