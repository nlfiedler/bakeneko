//
// Copyright 2012-2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	"math"
	"math/cmplx"
)

// mathRound rounds the given floating point number to the nearest integer,
// rounding to even if the fractional part is equal to .5, as required by
// Scheme R7RS (also happens to be the IEEE 754 recommended default).
// func mathRound(num float64) (int64, error) {
// 	in, fr := math.Modf(num)
// 	if math.IsNaN(fr) {
// 		return 0, NumberOutOfRange
// 	}
// 	fr = math.Abs(fr)
// 	ini := int64(in)
// 	if fr < 0.5 {
// 		return ini, nil
// 	}
// 	if fr > 0.5 {
// 		if ini > 0 {
// 			return ini + 1, nil
// 		}
// 		return ini - 1, nil
// 	}
// 	if ini&1 == 0 {
// 		return ini, nil
// 	}
// 	if ini > 0 {
// 		return ini + 1, nil
// 	}
// 	return ini - 1, nil
// }

// builtinIsNumber implements the number? predicate.
func builtinIsNumber(name string, args []interface{}) (interface{}, LispError) {
	_, ok := args[0].(Number)
	return BooleanFromBool(ok), nil
}

// builtinIsComplex implements the complex? predicate.
func builtinIsComplex(name string, args []interface{}) (interface{}, LispError) {
	// Treat all numbers as complex
	_, ok := args[0].(Number)
	return BooleanFromBool(ok), nil
}

// builtinIsReal implements the real? predicate.
func builtinIsReal(name string, args []interface{}) (interface{}, LispError) {
	_, ok := args[0].(Float)
	if ok {
		return BooleanFromBool(ok), nil
	}
	_, ok = args[0].(Integer)
	if ok {
		return BooleanFromBool(ok), nil
	}
	c, ok := args[0].(Complex)
	if ok {
		ok = c.ImagPart() == 0.0
	}
	return BooleanFromBool(ok), nil
}

// builtinIsRational implements the rational? predicate.
func builtinIsRational(name string, args []interface{}) (interface{}, LispError) {
	_, ok := args[0].(Rational)
	if !ok {
		// Otherwise, real numbers are also rational.
		return builtinIsReal(name, args)
	}
	return BooleanFromBool(ok), nil
}

// builtinIsInteger implements the integer? predicate.
func builtinIsInteger(name string, args []interface{}) (interface{}, LispError) {
	num, ok := args[0].(Float)
	if ok {
		_, fr := math.Modf(num.ToFloat())
		if math.IsNaN(fr) {
			return nil, NewLispError(EARGUMENT, "integer? argument out of range")
		}
		return BooleanFromBool(fr == 0.0), nil
	}
	_, ok = args[0].(Integer)
	return BooleanFromBool(ok), nil
}

// builtinIsExact implements the exact? predicate.
func builtinIsExact(name string, args []interface{}) (interface{}, LispError) {
	// All of our numbers are inexact
	return BooleanFalse, nil
}

// builtinIsInexact implements the exact? predicate.
func builtinIsInexact(name string, args []interface{}) (interface{}, LispError) {
	// All of our numbers are inexact
	_, ok := args[0].(Number)
	return BooleanFromBool(ok), nil
}

// coerceNumbers coerces the given arguments into compatible types. Order is
// as defined in Scheme's numeric tower: complex, real, rational, and finally
// integer.
func coerceNumbers(a, b Number) (Number, Number) {
	_, ac := a.(Complex)
	_, bc := b.(Complex)
	if ac || bc {
		n1 := a.ComplexValue()
		n2 := b.ComplexValue()
		return n1, n2
	}
	_, af := a.(Float)
	_, bf := b.(Float)
	if af || bf {
		n1 := a.FloatValue()
		n2 := b.FloatValue()
		return n1, n2
	}
	_, ar := a.(Rational)
	_, br := b.(Rational)
	if ar || br {
		n1 := a.RationalValue()
		n2 := b.RationalValue()
		return n1, n2
	}
	// Otherwise assume they are integers.
	n1 := a.IntegerValue()
	n2 := b.IntegerValue()
	return n1, n2
}

// compareNumbers coerces the given arguments into compatible types
// and invokes the CompareTo() method on them and returns the result.
func compareNumbers(a, b Number) (int8, error) {
	n1, n2 := coerceNumbers(a, b)
	return n1.CompareTo(n2)
}

// compareNumbersFn indicates if the given number satisfies the condition when
// comparing two numbers. Typically the value is either -1, 0, or 1 and the
// function is essentially performing the =, <, >, <=, or >= predicate for
// comparing a sequence of numbers.
type compareNumbersFn func(int8) bool

// compareNumbersequence compares a sequence of numbers, ensuring they satisfy
// a given property. All of the arugments must be a numeric value. The op
// argument is used in formatting error messages. The compare function is used
// to determine if the numeric comparisons satisfy the condition. This
// function returns Boolean(true) if the sequence satisfies the condition, and
// Boolean(false) if it does not. Otherwise nil and an error are returned if
// something went wrong.
func compareNumbersequence(args []interface{}, op string, fn compareNumbersFn) (interface{}, LispError) {
	prev, is_num := args[0].(Number)
	if !is_num {
		return nil, NewLispErrorf(EARGUMENT, "%s requires numeric arguments", op)
	}
	for idx := 1; idx < len(args); idx++ {
		curr, is_num := args[idx].(Number)
		if !is_num {
			return nil, NewLispErrorf(EARGUMENT, "%s requires numeric arguments", op)
		}
		cmp, err := compareNumbers(prev, curr)
		if err != nil {
			return nil, NewLispError(EARGUMENT, err.Error())
		}
		if !fn(cmp) {
			return BooleanFalse, nil
		}
		prev = curr
	}
	return BooleanTrue, nil
}

// builtinIsEqual implements the = predicate.
func builtinIsEqual(name string, args []interface{}) (interface{}, LispError) {
	cond := func(val int8) bool {
		return val == 0
	}
	return compareNumbersequence(args, "=", cond)
}

// builtinIsLess implements the < predicate.
func builtinIsLess(name string, args []interface{}) (interface{}, LispError) {
	cond := func(val int8) bool {
		return val < 0
	}
	return compareNumbersequence(args, "<", cond)
}

// builtinIsLessEqual implements the <= predicate.
func builtinIsLessEqual(name string, args []interface{}) (interface{}, LispError) {
	cond := func(val int8) bool {
		return val <= 0
	}
	return compareNumbersequence(args, "<=", cond)
}

// builtinIsGreater implements the > predicate.
func builtinIsGreater(name string, args []interface{}) (interface{}, LispError) {
	cond := func(val int8) bool {
		return val > 0
	}
	return compareNumbersequence(args, ">", cond)
}

// builtinIsGreaterEqual implements the >= predicate.
func builtinIsGreaterEqual(name string, args []interface{}) (interface{}, LispError) {
	cond := func(val int8) bool {
		return val >= 0
	}
	return compareNumbersequence(args, ">=", cond)
}

// builtinIsZero implements the zero? predicate.
func builtinIsZero(name string, args []interface{}) (interface{}, LispError) {
	num, is_num := args[0].(Number)
	if !is_num {
		return nil, NewLispError(EARGUMENT, "zero? requires a numeric argument")
	}
	zero := NewInteger(0)
	cmp, err := compareNumbers(zero, num)
	if err != nil {
		return nil, NewLispError(EARGUMENT, err.Error())
	}
	return BooleanFromBool(cmp == 0), nil
}

// builtinIsPositive implements the positive? predicate.
func builtinIsPositive(name string, args []interface{}) (interface{}, LispError) {
	num, is_num := args[0].(Number)
	if !is_num {
		return nil, NewLispError(EARGUMENT, "positive? requires a numeric argument")
	}
	zero := NewInteger(0)
	cmp, err := compareNumbers(zero, num)
	if err != nil {
		return nil, NewLispError(EARGUMENT, err.Error())
	}
	return BooleanFromBool(cmp < 0), nil
}

// builtinIsNegative implements the negative? predicate.
func builtinIsNegative(name string, args []interface{}) (interface{}, LispError) {
	num, is_num := args[0].(Number)
	if !is_num {
		return nil, NewLispError(EARGUMENT, "negative? requires a numeric argument")
	}
	zero := NewInteger(0)
	cmp, err := compareNumbers(zero, num)
	if err != nil {
		return nil, NewLispError(EARGUMENT, err.Error())
	}
	return BooleanFromBool(cmp > 0), nil
}

// builtinIsOdd implements the odd? predicate.
func builtinIsOdd(name string, args []interface{}) (interface{}, LispError) {
	num, is_num := args[0].(Integer)
	if !is_num {
		return nil, NewLispError(EARGUMENT, "odd? requires an integer argument")
	}
	two := NewInteger(2)
	result := num.Mod(two)
	zero := NewInteger(0)
	cmp, err := compareNumbers(zero, result)
	if err != nil {
		return nil, NewLispError(EARGUMENT, err.Error())
	}
	return BooleanFromBool(cmp != 0), nil
}

// builtinIsEven implements the even? predicate.
func builtinIsEven(name string, args []interface{}) (interface{}, LispError) {
	num, is_num := args[0].(Integer)
	if !is_num {
		return nil, NewLispError(EARGUMENT, "even? requires an integer argument")
	}
	two := NewInteger(2)
	result := num.Mod(two)
	zero := NewInteger(0)
	cmp, err := compareNumbers(zero, result)
	if err != nil {
		return nil, NewLispError(EARGUMENT, err.Error())
	}
	return BooleanFromBool(cmp == 0), nil
}

// reduceNumbersFn performs a reduction on two numbers, returning the result.
type reduceNumbersFn func(a, b Number) (Number, error)

// reduceSequence performs the given reduction operation on a series of
// numeric arguments, returning a single numeric result.
func reduceSequence(args []interface{}, op string, fn reduceNumbersFn) (interface{}, LispError) {
	curr, is_num := args[0].(Number)
	if !is_num {
		return nil, NewLispErrorf(EARGUMENT, "%s requires numeric arguments", op)
	}
	var err error
	for idx := 1; idx < len(args); idx++ {
		next, is_num := args[idx].(Number)
		if !is_num {
			return nil, NewLispErrorf(EARGUMENT, "%s requires numeric arguments; got %v (%T)",
				op, args[idx], args[idx])
		}
		curr, err = fn(curr, next)
		if err != nil {
			return nil, NewLispErrorf(EARGUMENT, "%v and %v yielded error '%s' for %s",
				curr, next, err, op)
		}
	}
	return curr, nil
}

// builtinMax implements the max procedure.
func builtinMax(name string, args []interface{}) (interface{}, LispError) {
	reduce := func(a, b Number) (Number, error) {
		cmp, err := compareNumbers(a, b)
		if err != nil {
			return nil, err
		}
		if cmp > 0 {
			return a, nil
		} else if cmp < 0 {
			return b, nil
		}
		return a, nil
	}
	return reduceSequence(args, "max", reduce)
}

// builtinMin implements the min procedure.
func builtinMin(name string, args []interface{}) (interface{}, LispError) {
	reduce := func(a, b Number) (Number, error) {
		cmp, err := compareNumbers(a, b)
		if err != nil {
			return nil, err
		}
		if cmp < 0 {
			return a, nil
		} else if cmp > 0 {
			return b, nil
		}
		return a, nil
	}
	return reduceSequence(args, "min", reduce)
}

// padArguments will prepend the given number to the arguments two times and
// return the results. This is used by the numeric operators (e.g. +) to get
// the expected behavior when the argument list contains less than two
// arguments.
func padArguments(args []interface{}, num Number) []interface{} {
	padding := make([]interface{}, 0)
	padding = append(padding, num)
	padding = append(padding, num)
	return append(padding, args...)
}

// builtinAdd implements the + procedure.
func builtinAdd(name string, args []interface{}) (interface{}, LispError) {
	reduce := func(a, b Number) (Number, error) {
		n1, n2 := coerceNumbers(a, b)
		return n1.Add(n2), nil
	}
	if len(args) < 2 {
		args = padArguments(args, NewInteger(0))
	}
	return reduceSequence(args, "+", reduce)
}

// builtinSubtract implements the - procedure.
func builtinSubtract(name string, args []interface{}) (interface{}, LispError) {
	reduce := func(a, b Number) (Number, error) {
		n1, n2 := coerceNumbers(a, b)
		return n1.Subtract(n2), nil
	}
	if len(args) < 2 {
		args = padArguments(args, NewInteger(0))
	}
	return reduceSequence(args, "-", reduce)
}

// builtinMultiply implements the * procedure.
func builtinMultiply(name string, args []interface{}) (interface{}, LispError) {
	reduce := func(a, b Number) (Number, error) {
		n1, n2 := coerceNumbers(a, b)
		return n1.Multiply(n2), nil
	}
	if len(args) < 2 {
		args = padArguments(args, NewInteger(1))
	}
	return reduceSequence(args, "*", reduce)
}

// builtinDivide implements the / procedure.
func builtinDivide(name string, args []interface{}) (val interface{}, err LispError) {
	defer func() {
		if x := recover(); x != nil {
			val = nil
			err = NewLispErrorf(EARGUMENT, "%v", x)
		}
	}()
	reduce := func(a, b Number) (Number, error) {
		n1, n2 := coerceNumbers(a, b)
		// convert integers to rational to get expected results
		// (e.g. (/ 3 4 5) => 3/20)
		_, ai := a.(Integer)
		_, bi := b.(Integer)
		if ai && bi {
			n1 = a.RationalValue()
			n2 = b.RationalValue()
		}
		return n1.Divide(n2), nil
	}
	if len(args) < 2 {
		args = padArguments(args, NewInteger(1))
	}
	val, err = reduceSequence(args, "/", reduce)
	if err == nil {
		// check for a rational result with denominator of 1 and simplify
		if vr, is_r := val.(Rational); is_r {
			if vr.BigRat().Denom().Int64() == 1 {
				val = NewInteger(vr.BigRat().Num().Int64())
			}
		}
	}
	return
}

// builtinAbs implements the abs procedure.
func builtinAbs(name string, args []interface{}) (interface{}, LispError) {
	num, is_num := args[0].(Number)
	if !is_num {
		return nil, NewLispError(EARGUMENT, "abs requires a numeric argument")
	}
	if _, is_cmplx := num.(Complex); is_cmplx {
		return nil, NewLispError(EARGUMENT, "abs cannot operate on complex types")
	}
	var zero Number = NewInteger(0)
	// do the coercion once since we may need it again
	zero, num = coerceNumbers(zero, num)
	cmp, err := compareNumbers(zero, num)
	if err != nil {
		return nil, NewLispError(EARGUMENT, err.Error())
	}
	if cmp > 0 {
		num = zero.Subtract(num)
	}
	return num, nil
}

// builtinQuotient implements the quotient procedure.
func builtinQuotient(name string, args []interface{}) (val interface{}, err LispError) {
	dividend, ok := args[0].(Integer)
	if !ok {
		return nil, NewLispError(EARGUMENT, "quotient requires integer arguments")
	}
	divisor, ok := args[1].(Integer)
	if !ok {
		return nil, NewLispError(EARGUMENT, "quotient requires integer arguments")
	}
	if divisor.ToInteger() == 0 {
		return nil, NewLispError(EARGUMENT, "division by zero")
	}
	return dividend.Divide(divisor), nil
}

// isqrt returns the integer square root of the input value.
func isqrt(x int64) uint32 {
	var res uint32 = 0
	var add uint32 = 0x80000000
	for i := 0; i < 32; i++ {
		var temp uint32 = res | add
		var g2 int64 = int64(temp) * int64(temp)
		if x >= g2 {
			res = temp
		}
		add >>= 1
	}
	return res
}

// builtinSqrt implements the sqrt procedure.
func builtinSqrt(name string, args []interface{}) (val interface{}, err LispError) {
	switch num := args[0].(type) {
	case Integer:
		in := num.ToInteger()
		if in < 0 {
			val = NewComplex(complex(0, float64(isqrt(in))))
		} else {
			val = isqrt(in)
		}
	case Float:
		val = NewFloat(math.Sqrt(num.ToFloat()))
	case Complex:
		val = NewComplex(cmplx.Sqrt(num.ToComplex()))
	case Rational:
		val = NewFloat(math.Sqrt(num.toFloat()))
	default:
		err = NewLispError(EARGUMENT, "sqrt requires a numeric argument")
	}
	return
}
