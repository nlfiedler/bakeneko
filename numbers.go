//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

import (
	"math"
)

// mathRound rounds the given floating point number to the nearest integer,
// rounding to even if the fractional part is equal to .5, as required by
// Scheme R5RS (also happens to be the IEEE 754 recommended default).
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
func builtinIsNumber(args []interface{}) (interface{}, LispError) {
	if len(args) != 1 {
		return nil, NewLispErrorf(EARGUMENT,
			"number? called with %d argument(s), takes only one", len(args))
	}
	_, ok := args[0].(Number)
	return Boolean(ok), nil
}

// builtinIsComplex implements the complex? predicate.
func builtinIsComplex(args []interface{}) (interface{}, LispError) {
	if len(args) != 1 {
		return nil, NewLispErrorf(EARGUMENT,
			"complex? called with %d argument(s), takes only one", len(args))
	}
	// Treat all numbers as complex
	_, ok := args[0].(Number)
	return Boolean(ok), nil
}

// builtinIsReal implements the real? predicate.
func builtinIsReal(args []interface{}) (interface{}, LispError) {
	if len(args) != 1 {
		return nil, NewLispErrorf(EARGUMENT,
			"real? called with %d argument(s), takes only one", len(args))
	}
	_, ok := args[0].(Float)
	if ok {
		return Boolean(ok), nil
	}
	_, ok = args[0].(Integer)
	if ok {
		return Boolean(ok), nil
	}
	c, ok := args[0].(Complex)
	if ok {
		ok = c.ImagPart() == 0.0
	}
	return Boolean(ok), nil
}

// builtinIsRational implements the rational? predicate.
func builtinIsRational(args []interface{}) (interface{}, LispError) {
	if len(args) != 1 {
		return nil, NewLispErrorf(EARGUMENT,
			"rational? called with %d argument(s), takes only one", len(args))
	}
	_, ok := args[0].(Rational)
	if !ok {
		// Otherwise, real numbers are also rational.
		return builtinIsReal(args)
	}
	return Boolean(ok), nil
}

// builtinIsInteger implements the integer? predicate.
func builtinIsInteger(args []interface{}) (interface{}, LispError) {
	if len(args) != 1 {
		return nil, NewLispErrorf(EARGUMENT,
			"integer? called with %d argument(s), takes only one", len(args))
	}
	num, ok := args[0].(Float)
	if ok {
		_, fr := math.Modf(num.ToFloat())
		if math.IsNaN(fr) {
			return nil, NewLispError(EARGUMENT, "integer? argument out of range")
		}
		return Boolean(fr == 0.0), nil
	}
	_, ok = args[0].(Integer)
	return Boolean(ok), nil
}

// builtinIsExact implements the exact? predicate.
func builtinIsExact(args []interface{}) (interface{}, LispError) {
	if len(args) != 1 {
		return nil, NewLispErrorf(EARGUMENT,
			"exact? called with %d argument(s), takes only one", len(args))
	}
	// All of our numbers are inexact
	return Boolean(false), nil
}

// builtinIsInexact implements the exact? predicate.
func builtinIsInexact(args []interface{}) (interface{}, LispError) {
	if len(args) != 1 {
		return nil, NewLispErrorf(EARGUMENT,
			"inexact? called with %d argument(s), takes only one", len(args))
	}
	// All of our numbers are inexact
	_, ok := args[0].(Number)
	return Boolean(ok), nil
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
	if len(args) < 2 {
		return nil, NewLispErrorf(EARGUMENT, "%s requires two or more arguments", op)
	}
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
			return Boolean(false), nil
		}
		prev = curr
	}
	return Boolean(true), nil
}

// builtinIsEqual implements the = predicate.
func builtinIsEqual(args []interface{}) (interface{}, LispError) {
	cond := func(val int8) bool {
		return val == 0
	}
	return compareNumbersequence(args, "=", cond)
}

// builtinIsLess implements the < predicate.
func builtinIsLess(args []interface{}) (interface{}, LispError) {
	cond := func(val int8) bool {
		return val < 0
	}
	return compareNumbersequence(args, "<", cond)
}

// builtinIsLessEqual implements the <= predicate.
func builtinIsLessEqual(args []interface{}) (interface{}, LispError) {
	cond := func(val int8) bool {
		return val <= 0
	}
	return compareNumbersequence(args, "<=", cond)
}

// builtinIsGreater implements the > predicate.
func builtinIsGreater(args []interface{}) (interface{}, LispError) {
	cond := func(val int8) bool {
		return val > 0
	}
	return compareNumbersequence(args, ">", cond)
}

// builtinIsGreaterEqual implements the >= predicate.
func builtinIsGreaterEqual(args []interface{}) (interface{}, LispError) {
	cond := func(val int8) bool {
		return val >= 0
	}
	return compareNumbersequence(args, ">=", cond)
}

// builtinIsZero implements the zero? predicate.
func builtinIsZero(args []interface{}) (interface{}, LispError) {
	if len(args) != 1 {
		return nil, NewLispErrorf(EARGUMENT,
			"zero? called with %d argument(s), takes only one", len(args))
	}
	num, is_num := args[0].(Number)
	if !is_num {
		return nil, NewLispError(EARGUMENT, "zero? requires a numeric argument")
	}
	zero := NewInteger(0)
	cmp, err := compareNumbers(zero, num)
	if err != nil {
		return nil, NewLispError(EARGUMENT, err.Error())
	}
	return Boolean(cmp == 0), nil
}

// builtinIsPositive implements the positive? predicate.
func builtinIsPositive(args []interface{}) (interface{}, LispError) {
	if len(args) != 1 {
		return nil, NewLispErrorf(EARGUMENT,
			"positive? called with %d argument(s), takes only one", len(args))
	}
	num, is_num := args[0].(Number)
	if !is_num {
		return nil, NewLispError(EARGUMENT, "positive? requires a numeric argument")
	}
	zero := NewInteger(0)
	cmp, err := compareNumbers(zero, num)
	if err != nil {
		return nil, NewLispError(EARGUMENT, err.Error())
	}
	return Boolean(cmp < 0), nil
}

// builtinIsNegative implements the negative? predicate.
func builtinIsNegative(args []interface{}) (interface{}, LispError) {
	if len(args) != 1 {
		return nil, NewLispErrorf(EARGUMENT,
			"negative? called with %d argument(s), takes only one", len(args))
	}
	num, is_num := args[0].(Number)
	if !is_num {
		return nil, NewLispError(EARGUMENT, "negative? requires a numeric argument")
	}
	zero := NewInteger(0)
	cmp, err := compareNumbers(zero, num)
	if err != nil {
		return nil, NewLispError(EARGUMENT, err.Error())
	}
	return Boolean(cmp > 0), nil
}

// builtinIsOdd implements the odd? predicate.
func builtinIsOdd(args []interface{}) (interface{}, LispError) {
	if len(args) != 1 {
		return nil, NewLispErrorf(EARGUMENT,
			"odd? called with %d argument(s), takes only one", len(args))
	}
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
	return Boolean(cmp != 0), nil
}

// builtinIsEven implements the even? predicate.
func builtinIsEven(args []interface{}) (interface{}, LispError) {
	if len(args) != 1 {
		return nil, NewLispErrorf(EARGUMENT,
			"even? called with %d argument(s), takes only one", len(args))
	}
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
	return Boolean(cmp == 0), nil
}

// reduceNumbersFn performs a reduction on two numbers, returning the result.
type reduceNumbersFn func(a, b Number) (Number, error)

// reduceSequence performs the given reduction operation on a series of
// numeric arguments, returning a single numeric result.
func reduceSequence(args []interface{}, op string, fn reduceNumbersFn) (interface{}, LispError) {
	if len(args) < 2 {
		return nil, NewLispErrorf(EARGUMENT, "%s requires two or more arguments", op)
	}
	curr, is_num := args[0].(Number)
	if !is_num {
		return nil, NewLispErrorf(EARGUMENT, "%s requires numeric arguments", op)
	}
	var err error
	for idx := 1; idx < len(args); idx++ {
		next, is_num := args[idx].(Number)
		if !is_num {
			return nil, NewLispErrorf(EARGUMENT, "%s requires numeric arguments", op)
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
func builtinMax(args []interface{}) (interface{}, LispError) {
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
func builtinMin(args []interface{}) (interface{}, LispError) {
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
func builtinAdd(args []interface{}) (interface{}, LispError) {
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
func builtinSubtract(args []interface{}) (interface{}, LispError) {
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
func builtinMultiply(args []interface{}) (interface{}, LispError) {
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
func builtinDivide(args []interface{}) (val interface{}, err LispError) {
	if len(args) < 1 {
		return nil, NewLispError(EARGUMENT, "/ requires one or more arguments")
	}
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
				val = vr.BigRat().Num().Int64()
			}
		}
	}
	return
}

// TODO: finish the remaining number predicates and procedures from section 6.2
