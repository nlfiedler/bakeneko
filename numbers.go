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

// TODO: implement > built-in
// TODO: implement < built-in
// TODO: implement >= built-in
// TODO: implement <= built-in

// compareNumbers coerces the given arguments into compatible types
// and invokes the CompareTo() method on them and returns the result.
func compareNumbers(a, b Number) (int8, error) {
	_, ac := a.(Complex)
	_, bc := b.(Complex)
	if ac || bc {
		n1 := a.ComplexValue()
		n2 := b.ComplexValue()
		return n1.CompareTo(n2)
	}
	_, af := a.(Float)
	_, bf := b.(Float)
	if af || bf {
		n1 := a.FloatValue()
		n2 := b.FloatValue()
		return n1.CompareTo(n2)
	}
	_, ar := a.(Rational)
	_, br := b.(Rational)
	if ar || br {
		n1 := a.RationalValue()
		n2 := b.RationalValue()
		return n1.CompareTo(n2)
	}
	// Otherwise assume they are integers.
	n1 := a.IntegerValue()
	n2 := b.IntegerValue()
	return n1.CompareTo(n2)
}

// compareFn indicates if the given number satisfies the condition
// when comparing two numbers. Typically the value is either -1, 0,
// or 1 and the function is essentially performing the =, <, >, <=,
// or >= predicate for comparing a sequence of numbers.
type compareFn func(int8) bool

// compareSequence compares a sequence of numbers, ensuring they satisfy a
// given property. All of the arugments must be a numeric value. The op
// argument is used in formatting error messages. The compare function is used
// to determine if the numeric comparisons satisfy the condition. This
// function returns Boolean(true) if the sequence satisfies the condition, and
// Boolean(false) if it does not. Otherwise nil and an error are returned if
// something went wrong.
func compareSequence(args []interface{}, op string, fn compareFn) (interface{}, LispError) {
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
	return compareSequence(args, "=", cond)
}

// builtinIsLess implements the < predicate.
func builtinIsLess(args []interface{}) (interface{}, LispError) {
	cond := func(val int8) bool {
		return val < 0
	}
	return compareSequence(args, "<", cond)
}

// builtinIsLessEqual implements the <= predicate.
func builtinIsLessEqual(args []interface{}) (interface{}, LispError) {
	cond := func(val int8) bool {
		return val <= 0
	}
	return compareSequence(args, "<=", cond)
}

// builtinIsGreater implements the > predicate.
func builtinIsGreater(args []interface{}) (interface{}, LispError) {
	cond := func(val int8) bool {
		return val > 0
	}
	return compareSequence(args, ">", cond)
}

// builtinIsGreaterEqual implements the >= predicate.
func builtinIsGreaterEqual(args []interface{}) (interface{}, LispError) {
	cond := func(val int8) bool {
		return val >= 0
	}
	return compareSequence(args, ">=", cond)
}
