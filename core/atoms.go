//
// Copyright 2012-2014 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	"bytes"
	"fmt"
	"math/big"
	"strings"
	"unicode/utf8"
)

//
// Definition of atom types in our Scheme-like language, which includes
// symbols, (mutable) strings, numbers, booleans, and characters.
//

// Atom represents all things in our Scheme implementation which can be
// evaluated to a value outside of the context of a specific environment.
// These include strings, symbols, characters, and numbers.
type Atom interface {
	// CompareTo compares other to this atom and returns:
	//
	//   -1 if this <  other
	//    0 if this == other
	//    1 if this >  other
	//
	// An error is returned if the object is not of a suitable type for
	// comparison.
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
type Boolean interface {
	Atom
	// Value returns the boolean value.
	Value() bool
}

// booleanImpl is simply a bool with functions.
type booleanImpl bool

// BooleanTrue is the singleton instance of the 'true' value.
var BooleanTrue Boolean = booleanImpl(true)

// BooleanFalse is the singleton instance of the 'false' value.
var BooleanFalse Boolean = booleanImpl(false)

// NewBoolean compares the input with the expected values for Scheme booleans
// (e.g. "#t", "#T", "#f", and "#F") and constructs a new Boolean atom. If val
// does not represent a boolean value then panic ensues.
func NewBoolean(val string) Boolean {
	// lexer validated token is authentic boolean, just need value
	lower := strings.ToLower(val)
	if lower == "#t" || lower == "#true" {
		return BooleanTrue
	} else if val == "#f" || val == "#false" {
		return BooleanFalse
	}
	panic(fmt.Sprintf("lexer/parser bug: '%s' is not boolean", val))
}

// BooleanFromBool returns the singleton instance of the Boolean value that
// matches the value given (i.e. BooleanTrue or BooleanFalse).
func BooleanFromBool(val bool) Boolean {
	if val {
		return BooleanTrue
	}
	return BooleanFalse
}

// CompareTo returns zero if this object represents the same boolean value as
// the argument; a positive value if this object represents true and the
// argument represents false; and a negative value if this object represents
// false and the argument represents true.
func (b booleanImpl) CompareTo(other Atom) (int8, error) {
	if ob, ok := other.(Boolean); ok {
		bb := bool(b)
		obb := ob.Value()
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

func (b booleanImpl) EqualTo(other Atom) (bool, error) {
	if ob, ok := other.(Boolean); ok {
		return bool(b) == ob.Value(), nil
	}
	return false, TypeMismatch
}

// Eval returns true or false depending on the value of the Boolean.
func (b booleanImpl) Eval() interface{} {
	return bool(b)
}

// String returns "#t" or "#f" depending on the value of the Boolean.
func (b booleanImpl) String() string {
	if bool(b) {
		return "#t"
	}
	return "#f"
}

// Value returns the boolean value.
func (b booleanImpl) Value() bool {
	return bool(b)
}

// Symbol represents a variable or procedure name in a Scheme expression. It
// is essentially a string but is treated differently.
type Symbol interface {
	Atom
	// IsSymbol always returns true, it exists to distinquish this type from
	// Atom, otherwise the compiler treats all Atoms as Symbols.
	IsSymbol() bool
}

// atomsEqual compares two atoms for equality and returns true or false.
// Any type mismatch is ignored and false is returned.
func atomsEqual(a, b Atom) bool {
	eq, _ := a.EqualTo(b)
	return eq
}

// symbolImpl is an implementation of the Symbol interface.
type symbolImpl string

// NewSymbol wraps the given string in a Symbol implementation.
func NewSymbol(val string) Symbol {
	return symbolImpl(val)
}

func (s symbolImpl) CompareTo(other Atom) (int8, error) {
	if os, ok := other.(Symbol); ok {
		ss := string(s)
		oss := os.String()
		if ss == oss {
			return 0, nil
		} else if ss > oss {
			return 1, nil
		} else {
			return -1, nil
		}
	}
	return 0, TypeMismatch
}

func (s symbolImpl) EqualTo(other Atom) (bool, error) {
	if os, ok := other.(Symbol); ok {
		ss := string(s)
		oss := os.String()
		return ss == oss, nil
	}
	return false, TypeMismatch
}

// Eval returns the name of the symbol as a string.
func (s symbolImpl) Eval() interface{} {
	return string(s)
}

func (s symbolImpl) IsSymbol() bool {
	return true
}

// String returns the name of the symbol.
func (s symbolImpl) String() string {
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
	Set(pos int, ch rune) error
	// Value returns the string value itself, without quotes.
	Value() string
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

// Set changes the rune at the given zero-based position within the string. If
// pos is out of bounds, the function returns the OutOfBounds error.
func (s *StringImpl) Set(pos int, ch rune) error {
	if s == nil {
		return nil
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
		return OutOfBounds
	}
	s.slice[pos] = ch
	return nil
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
	if os, ok := other.(String); ok {
		ost := os.Value()
		st := s.Value()
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
	if os, ok := other.(String); ok {
		ost := os.Value()
		st := s.Value()
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

// Value returns the String as a Go string.
func (s *StringImpl) Value() string {
	if s == nil {
		return ""
	}
	return s.toString()
}

// ImmutableString is like a String but Set() returns an error.
type ImmutableString string

// NewImmutableString creates a new ImmutableString instance.
func NewImmutableString(val string) ImmutableString {
	return ImmutableString(val)
}

func (is ImmutableString) CompareTo(other Atom) (int8, error) {
	if os, ok := other.(String); ok {
		ost := os.Value()
		st := is.Value()
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

func (is ImmutableString) EqualTo(other Atom) (bool, error) {
	if os, ok := other.(String); ok {
		ost := os.Value()
		st := is.Value()
		return st == ost, nil
	}
	return false, TypeMismatch
}

func (is ImmutableString) Eval() interface{} {
	return string(is)
}

func (is ImmutableString) String() string {
	return fmt.Sprintf("\"%s\"", string(is))
}

func (is ImmutableString) Len() int {
	return len(string(is))
}

func (is ImmutableString) Set(pos int, ch rune) error {
	return StringIsImmutable
}

func (is ImmutableString) Value() string {
	return string(is)
}

// Character represents a single character (e.g. '#\\a' or '#\\space') in Scheme.
type Character interface {
	Atom
	// ToRune returns the rune this character represents.
	ToRune() rune
}

// characterImpl is the default implementation of a Character.
type characterImpl rune

// NewCharacter creates an instance of Character to represent the given Scheme
// character. Characters are prefixed with #\, as in #\a for the letter 'a'.
// Special sequences are #\space for ' ' and #\newline for the newline
// character. Invalid input, such as a short string, will result in the
// uf8.RuneError character.
func NewCharacter(val string) Character {
	if val == "#\\space" {
		return characterImpl(' ')
	} else if val == "#\\newline" {
		return characterImpl('\n')
	} else if len(val) != 3 {
		return characterImpl(utf8.RuneError)
	}
	// take whatever follows the #\ prefix
	return characterImpl(val[2])
}

func (c characterImpl) CompareTo(other Atom) (int8, error) {
	if oc, ok := other.(Character); ok {
		roc := oc.ToRune()
		rc := rune(c)
		if rc == roc {
			return 0, nil
		} else if rc < roc {
			return -1, nil
		}
		return 1, nil
	}
	return 0, TypeMismatch
}

func (c characterImpl) EqualTo(other Atom) (bool, error) {
	if oc, ok := other.(Character); ok {
		return c.ToRune() == oc.ToRune(), nil
	}
	return false, TypeMismatch
}

// Eval returns the character itself as a rune.
func (c characterImpl) Eval() interface{} {
	return rune(c)
}

func (c characterImpl) ToRune() rune {
	return rune(c)
}

// String returns the Scheme representation of the character.
func (c characterImpl) String() string {
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
	// RationalValue returns the number as a Rational.
	RationalValue() Rational
}

// Integer represents an integral numeric value.
type Integer interface {
	Number
	// Mod finds the remainder of dividing this number by the argument.
	Mod(modulus Integer) Integer
	// ToInteger converts the integer to an int64 value.
	ToInteger() int64
}

// integerImpl is the internal implementation of an Integer.
type integerImpl int64

// NewInteger creates an Integer object for the given int64 value.
func NewInteger(val int64) Integer {
	return integerImpl(val)
}

func (i integerImpl) Add(value Number) Number {
	vi := value.IntegerValue().ToInteger()
	return NewInteger(int64(i) + int64(vi))
}

func (i integerImpl) Divide(divisor Number) Number {
	vi := divisor.IntegerValue().ToInteger()
	return NewInteger(int64(i) / int64(vi))
}

func (i integerImpl) Mod(modulus Integer) Integer {
	vi := modulus.IntegerValue().ToInteger()
	return NewInteger(int64(i) % int64(vi))
}

func (i integerImpl) Multiply(muliplier Number) Number {
	vi := muliplier.IntegerValue().ToInteger()
	return NewInteger(int64(i) * int64(vi))
}

func (i integerImpl) Subtract(value Number) Number {
	vi := value.IntegerValue().ToInteger()
	return NewInteger(int64(i) - int64(vi))
}

func (i integerImpl) CompareTo(other Atom) (int8, error) {
	if oi, ok := other.(Integer); ok {
		ii := int64(i)
		ioi := oi.ToInteger()
		if ii < ioi {
			return -1, nil
		} else if ii > ioi {
			return 1, nil
		}
		return 0, nil
	}
	return 0, TypeMismatch
}

func (i integerImpl) EqualTo(other Atom) (bool, error) {
	if oi, ok := other.(Integer); ok {
		ioi := oi.ToInteger()
		return int64(i) == int64(ioi), nil
	}
	return false, TypeMismatch
}

func (i integerImpl) Eval() interface{} {
	return int64(i)
}

func (i integerImpl) ComplexValue() Complex {
	return NewComplex(complex(float64(i), 0.0))
}

func (i integerImpl) IntegerValue() Integer {
	return i
}

func (i integerImpl) FloatValue() Float {
	return NewFloat(float64(i))
}

func (i integerImpl) RationalValue() Rational {
	return NewRational(int64(i), 1)
}

func (i integerImpl) String() string {
	return fmt.Sprint(int64(i))
}

func (i integerImpl) ToInteger() int64 {
	return int64(i)
}

// Float represents a floating point numerical value.
type Float interface {
	Number
	// ToFloat converts the float to a float64 value.
	ToFloat() float64
}

// floatImpl is the internal implementation of a Float.
type floatImpl float64

// NewFloat creates an Integer object for the given float64 value.
func NewFloat(val float64) Float {
	return floatImpl(val)
}

func (f floatImpl) Add(value Number) Number {
	vf := value.FloatValue().ToFloat()
	return NewFloat(float64(f) + float64(vf))
}

func (f floatImpl) Divide(divisor Number) Number {
	vf := divisor.FloatValue().ToFloat()
	return NewFloat(float64(f) / float64(vf))
}

func (f floatImpl) Multiply(muliplier Number) Number {
	vf := muliplier.FloatValue().ToFloat()
	return NewFloat(float64(f) * float64(vf))
}

func (f floatImpl) Subtract(value Number) Number {
	vf := value.FloatValue().ToFloat()
	return NewFloat(float64(f) - float64(vf))
}

func (f floatImpl) CompareTo(other Atom) (int8, error) {
	if of, ok := other.(Float); ok {
		ff := float64(f)
		off := of.ToFloat()
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

func (f floatImpl) EqualTo(other Atom) (bool, error) {
	if of, ok := other.(Float); ok {
		return float64(f) == of.ToFloat(), nil
	}
	return false, TypeMismatch
}

func (f floatImpl) Eval() interface{} {
	return float64(f)
}

func (f floatImpl) ComplexValue() Complex {
	return NewComplex(complex(f, 0.0))
}

func (f floatImpl) IntegerValue() Integer {
	return NewInteger(int64(f))
}

func (f floatImpl) FloatValue() Float {
	return f
}

func (f floatImpl) RationalValue() Rational {
	return NewRational(int64(f), 1)
}

func (f floatImpl) String() string {
	return fmt.Sprint(float64(f))
}

func (f floatImpl) ToFloat() float64 {
	return float64(f)
}

// Complex represents a complex number, with real and imaginary parts.
type Complex interface {
	Number
	// ToComplex converts this Complex to a complex128 value.
	ToComplex() complex128
	// RealPart returns the floating-point real part of the complex number.
	RealPart() float64
	// ImagPart returns the floating-point imaginary part of the complex number.
	ImagPart() float64
}

// complexImpl is the internal representation of a Complex.
type complexImpl complex128

func NewComplex(val complex128) Complex {
	return complexImpl(val)
}

func (c complexImpl) Add(value Number) Number {
	vc := value.ComplexValue().ToComplex()
	return NewComplex(complex128(c) + complex128(vc))
}

func (c complexImpl) Divide(divisor Number) Number {
	vc := divisor.ComplexValue().ToComplex()
	return NewComplex(complex128(c) / complex128(vc))
}

func (c complexImpl) Multiply(muliplier Number) Number {
	vc := muliplier.ComplexValue().ToComplex()
	return NewComplex(complex128(c) * complex128(vc))
}

func (c complexImpl) Subtract(value Number) Number {
	vc := value.ComplexValue().ToComplex()
	return NewComplex(complex128(c) - complex128(vc))
}

func (c complexImpl) CompareTo(other Atom) (int8, error) {
	if oc, ok := other.(Complex); ok {
		cc := complex128(c)
		occ := oc.ToComplex()
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

func (c complexImpl) EqualTo(other Atom) (bool, error) {
	if oc, ok := other.(Complex); ok {
		return complex128(c) == oc.ToComplex(), nil
	}
	return false, TypeMismatch
}

func (c complexImpl) Eval() interface{} {
	return complex128(c)
}

func (c complexImpl) IntegerValue() Integer {
	return NewInteger(int64(real(complex128(c))))
}

func (c complexImpl) FloatValue() Float {
	return NewFloat(real(complex128(c)))
}

func (c complexImpl) ComplexValue() Complex {
	return c
}

func (c complexImpl) RationalValue() Rational {
	return NewRational(int64(real(complex128(c))), 1)
}

func (c complexImpl) String() string {
	// return the number without the parentheses
	str := fmt.Sprint(complex128(c))
	return str[1 : len(str)-1]
}

func (c complexImpl) ToComplex() complex128 {
	return complex128(c)
}

func (c complexImpl) RealPart() float64 {
	return real(complex128(c))
}

func (c complexImpl) ImagPart() float64 {
	return imag(complex128(c))
}

type Rational interface {
	Number
	// BigRat returns the actual math/big/Rat instance.
	BigRat() *big.Rat
	// toFloat returns the float64 of the rational number.
	toFloat() float64
}

type rational struct {
	*big.Rat
}

func NewRational(a, b int64) Rational {
	return &rational{big.NewRat(a, b)}
}

func fromRational(a *big.Rat) Rational {
	return &rational{a}
}

func (r *rational) BigRat() *big.Rat {
	return r.Rat
}

func (r *rational) Add(value Number) Number {
	vr := value.RationalValue()
	rat := big.NewRat(1, 1)
	rat.Add(r.Rat, vr.BigRat())
	return fromRational(rat)
}

func (r *rational) Divide(divisor Number) Number {
	vr := divisor.RationalValue()
	rat := big.NewRat(1, 1)
	rat.Quo(r.Rat, vr.BigRat())
	return fromRational(rat)
}

func (r *rational) Multiply(muliplier Number) Number {
	vr := muliplier.RationalValue()
	rat := big.NewRat(1, 1)
	rat.Mul(r.Rat, vr.BigRat())
	return fromRational(rat)
}

func (r *rational) Subtract(value Number) Number {
	vr := value.RationalValue()
	rat := big.NewRat(1, 1)
	rat.Sub(r.Rat, vr.BigRat())
	return fromRational(rat)
}

func (r *rational) CompareTo(other Atom) (int8, error) {
	if or, ok := other.(Rational); ok {
		return int8(r.Cmp(or.BigRat())), nil
	}
	return 0, TypeMismatch
}

func (r *rational) EqualTo(other Atom) (bool, error) {
	if or, ok := other.(Rational); ok {
		return r.Cmp(or.BigRat()) == 0, nil
	}
	return false, TypeMismatch
}

func (r *rational) Eval() interface{} {
	if r.Denom().Int64() == 1 {
		return r.Num().Int64()
	}
	return r.toFloat()
}

func (r *rational) toFloat() float64 {
	num := r.Num().Int64()
	denom := r.Denom().Int64()
	return float64(num) / float64(denom)
}

func (r *rational) IntegerValue() Integer {
	return NewInteger(int64(r.toFloat()))
}

func (r *rational) FloatValue() Float {
	return NewFloat(r.toFloat())
}

func (r *rational) ComplexValue() Complex {
	return NewComplex(complex(r.toFloat(), 0.0))
}

func (r *rational) RationalValue() Rational {
	return r
}

func (r *rational) String() string {
	return r.RatString()
}
