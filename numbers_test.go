//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

import (
	"testing"
)

func TestBuiltinIsNumber(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(number? 'a)`] = `#f`
	inputs[`(number? 1)`] = `#t`
	inputs[`(number? 1/10)`] = `#t`
	inputs[`(number? 1.1)`] = `#t`
	inputs[`(number? 3+4i)`] = `#t`
	verifyInterpret(t, inputs)
}

func TestBuiltinIsComplex(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(complex? 'a)`] = `#f`
	inputs[`(complex? 1)`] = `#t`
	inputs[`(complex? 1/10)`] = `#t`
	inputs[`(complex? 1.1)`] = `#t`
	inputs[`(complex? 3+4i)`] = `#t`
	verifyInterpret(t, inputs)
}

func TestBuiltinIsReal(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(real? 'a)`] = `#f`
	inputs[`(real? 3)`] = `#t`
	inputs[`(real? -2.5+0.0i)`] = `#t`
	inputs[`(real? 3+4i)`] = `#f`
	inputs[`(real? 1/10)`] = `#f`
	inputs[`(real? 1.1)`] = `#t`
	verifyInterpret(t, inputs)
}

func TestBuiltinIsRational(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(rational? 'a)`] = `#f`
	inputs[`(rational? 3)`] = `#t`
	inputs[`(rational? -2.5+0.0i)`] = `#t`
	inputs[`(rational? 3+4i)`] = `#f`
	inputs[`(rational? 1/10)`] = `#t`
	inputs[`(rational? 1.1)`] = `#t`
	verifyInterpret(t, inputs)
}

func TestBuiltinIsInteger(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(integer? 'a)`] = `#f`
	inputs[`(integer? 3)`] = `#t`
	inputs[`(integer? -2.5+0.0i)`] = `#f`
	inputs[`(integer? 3+4i)`] = `#f`
	inputs[`(integer? 1/10)`] = `#f`
	inputs[`(integer? 1.1)`] = `#f`
	verifyInterpret(t, inputs)
}

func TestBuiltinIsExact(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(exact? 'a)`] = `#f`
	inputs[`(exact? 3)`] = `#f`
	inputs[`(exact? -2.5+0.0i)`] = `#f`
	inputs[`(exact? 3+4i)`] = `#f`
	inputs[`(exact? 1/10)`] = `#f`
	inputs[`(exact? 1.1)`] = `#f`
	verifyInterpret(t, inputs)
}

func TestBuiltinIsInexact(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(inexact? 'a)`] = `#f`
	inputs[`(inexact? 3)`] = `#t`
	inputs[`(inexact? -2.5+0.0i)`] = `#t`
	inputs[`(inexact? 3+4i)`] = `#t`
	inputs[`(inexact? 1/10)`] = `#t`
	inputs[`(inexact? 1.1)`] = `#t`
	verifyInterpret(t, inputs)
}

func TestBuiltinIsEqual(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(= 3 4)`] = `#f`
	inputs[`(= 3.0 4)`] = `#f`
	inputs[`(= 0+3i 4)`] = `#f`
	inputs[`(= 1/3 4)`] = `#f`
	inputs[`(= 3.0 3 9/3 3.0+0.0i)`] = `#t`
	verifyInterpret(t, inputs)
	inputs = make(map[string]string)
	inputs[`(= 3)`] = "two or more arguments"
	inputs[`(= 'a 4)`] = "numeric arguments"
	inputs[`(= 4 'b)`] = "numeric arguments"
	verifyInterpretError(t, inputs)
}

func TestBuiltinIsLess(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(< 4 3)`] = `#f`
	inputs[`(< 1 2 3 0)`] = `#f`
	inputs[`(< 3 4)`] = `#t`
	inputs[`(< 3.0 4)`] = `#t`
	inputs[`(< 0+3i 0)`] = `#t`
	inputs[`(< 1/3 4)`] = `#t`
	inputs[`(< 3.0 4 15/3 6.0+0.0i)`] = `#t`
	verifyInterpret(t, inputs)
	inputs = make(map[string]string)
	inputs[`(< 3)`] = "two or more arguments"
	inputs[`(< 'a 4)`] = "numeric arguments"
	inputs[`(< 4 'b)`] = "numeric arguments"
	verifyInterpretError(t, inputs)
}

func TestBuiltinIsLessEqual(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(<= 4 3)`] = `#f`
	inputs[`(<= 1 2 3 0)`] = `#f`
	inputs[`(<= 1 2 3 3 3)`] = `#t`
	inputs[`(<= 3 4)`] = `#t`
	inputs[`(<= 3.0 3)`] = `#t`
	inputs[`(<= 0+3i 0)`] = `#t`
	inputs[`(<= 1/3 4)`] = `#t`
	inputs[`(<= 3.0 4 15/3 6.0+0.0i)`] = `#t`
	verifyInterpret(t, inputs)
	inputs = make(map[string]string)
	inputs[`(<= 3)`] = "two or more arguments"
	inputs[`(<= 'a 4)`] = "numeric arguments"
	inputs[`(<= 4 'b)`] = "numeric arguments"
	verifyInterpretError(t, inputs)
}

func TestBuiltinIsGreater(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(> 3 4)`] = `#f`
	inputs[`(> 3 2 1 4)`] = `#f`
	inputs[`(> 4 3)`] = `#t`
	inputs[`(> 4.0 3)`] = `#t`
	inputs[`(> 0+3i 0)`] = `#f`
	inputs[`(> 4 1/3)`] = `#t`
	inputs[`(> 6.0+0.0i 15/3 4 3.0)`] = `#t`
	verifyInterpret(t, inputs)
	inputs = make(map[string]string)
	inputs[`(> 3)`] = "two or more arguments"
	inputs[`(> 'a 4)`] = "numeric arguments"
	inputs[`(> 4 'b)`] = "numeric arguments"
	verifyInterpretError(t, inputs)
}

func TestBuiltinIsGreaterEqual(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(>= 3 3)`] = `#t`
	inputs[`(>= 3 4)`] = `#f`
	inputs[`(>= 3 2 1 4)`] = `#f`
	inputs[`(>= 3 2 1 1 1)`] = `#t`
	inputs[`(>= 4 3)`] = `#t`
	inputs[`(>= 4.0 4)`] = `#t`
	inputs[`(>= 0+3i 0)`] = `#f`
	inputs[`(>= 4 1/3)`] = `#t`
	inputs[`(>= 6.0+0.0i 15/3 4 3.0)`] = `#t`
	verifyInterpret(t, inputs)
	inputs = make(map[string]string)
	inputs[`(>= 3)`] = "two or more arguments"
	inputs[`(>= 'a 4)`] = "numeric arguments"
	inputs[`(>= 4 'b)`] = "numeric arguments"
	verifyInterpretError(t, inputs)
}

func TestBuiltinIsZero(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(zero? 3)`] = `#f`
	inputs[`(zero? -2.5+0.0i)`] = `#f`
	inputs[`(zero? 3+4i)`] = `#f`
	inputs[`(zero? 1/10)`] = `#f`
	inputs[`(zero? 1.1)`] = `#f`
	inputs[`(zero? 0.0)`] = `#t`
	inputs[`(zero? 0/1)`] = `#t`
	inputs[`(zero? 0.0+0.0i)`] = `#t`
	inputs[`(zero? 0)`] = `#t`
	verifyInterpret(t, inputs)
	inputs = make(map[string]string)
	inputs[`(zero?)`] = "takes only one"
	inputs[`(zero? 1 2)`] = "takes only one"
	inputs[`(zero? 'a)`] = "numeric argument"
	verifyInterpretError(t, inputs)
}

func TestBuiltinIsPositive(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(positive? 3)`] = `#t`
	inputs[`(positive? -3)`] = `#f`
	inputs[`(positive? -2.5+0.0i)`] = `#f`
	inputs[`(positive? 3+4i)`] = `#t`
	inputs[`(positive? 1/10)`] = `#t`
	inputs[`(positive? -1/10)`] = `#f`
	inputs[`(positive? 1.1)`] = `#t`
	inputs[`(positive? -1.1)`] = `#f`
	inputs[`(positive? 0.0)`] = `#f`
	inputs[`(positive? 0/1)`] = `#f`
	inputs[`(positive? 0.0+0.0i)`] = `#f`
	inputs[`(positive? 0)`] = `#f`
	verifyInterpret(t, inputs)
	inputs = make(map[string]string)
	inputs[`(positive?)`] = "takes only one"
	inputs[`(positive? 1 2)`] = "takes only one"
	inputs[`(positive? 'a)`] = "numeric argument"
	verifyInterpretError(t, inputs)
}

func TestBuiltinIsNegative(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(negative? 3)`] = `#f`
	inputs[`(negative? -3)`] = `#t`
	inputs[`(negative? -2.5+0.0i)`] = `#t`
	inputs[`(negative? 3+4i)`] = `#f`
	inputs[`(negative? 1/10)`] = `#f`
	inputs[`(negative? -1/10)`] = `#t`
	inputs[`(negative? 1.1)`] = `#f`
	inputs[`(negative? -1.1)`] = `#t`
	inputs[`(negative? 0.0)`] = `#f`
	inputs[`(negative? 0/1)`] = `#f`
	inputs[`(negative? 0.0+0.0i)`] = `#f`
	inputs[`(negative? 0)`] = `#f`
	verifyInterpret(t, inputs)
	inputs = make(map[string]string)
	inputs[`(negative?)`] = "takes only one"
	inputs[`(negative? 1 2)`] = "takes only one"
	inputs[`(negative? 'a)`] = "numeric argument"
	verifyInterpretError(t, inputs)
}

func TestBuiltinIsOdd(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(odd? 1)`] = `#t`
	inputs[`(odd? 3)`] = `#t`
	inputs[`(odd? 5)`] = `#t`
	inputs[`(odd? 7)`] = `#t`
	inputs[`(odd? 2)`] = `#f`
	inputs[`(odd? 4)`] = `#f`
	inputs[`(odd? 6)`] = `#f`
	inputs[`(odd? 8)`] = `#f`
	inputs[`(odd? -3)`] = `#t`
	inputs[`(odd? 0)`] = `#f`
	verifyInterpret(t, inputs)
	inputs = make(map[string]string)
	inputs[`(odd?)`] = "takes only one"
	inputs[`(odd? 1 2)`] = "takes only one"
	inputs[`(odd? 'a)`] = "integer argument"
	inputs[`(odd? -2.5+0.0i)`] = "integer argument"
	inputs[`(odd? 3+4i)`] = "integer argument"
	inputs[`(odd? 1/10)`] = "integer argument"
	inputs[`(odd? -1/10)`] = "integer argument"
	inputs[`(odd? 1.1)`] = "integer argument"
	inputs[`(odd? -1.1)`] = "integer argument"
	inputs[`(odd? 0.0)`] = "integer argument"
	inputs[`(odd? 0/1)`] = "integer argument"
	inputs[`(odd? 0.0+0.0i)`] = "integer argument"
	verifyInterpretError(t, inputs)
}

func TestBuiltinIsEven(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(even? 1)`] = `#f`
	inputs[`(even? 3)`] = `#f`
	inputs[`(even? 5)`] = `#f`
	inputs[`(even? 7)`] = `#f`
	inputs[`(even? 2)`] = `#t`
	inputs[`(even? 4)`] = `#t`
	inputs[`(even? 6)`] = `#t`
	inputs[`(even? 8)`] = `#t`
	inputs[`(even? -3)`] = `#f`
	inputs[`(even? 0)`] = `#t`
	verifyInterpret(t, inputs)
	inputs = make(map[string]string)
	inputs[`(even?)`] = "takes only one"
	inputs[`(even? 1 2)`] = "takes only one"
	inputs[`(even? 'a)`] = "integer argument"
	inputs[`(even? -2.5+0.0i)`] = "integer argument"
	inputs[`(even? 3+4i)`] = "integer argument"
	inputs[`(even? 1/10)`] = "integer argument"
	inputs[`(even? -1/10)`] = "integer argument"
	inputs[`(even? 1.1)`] = "integer argument"
	inputs[`(even? -1.1)`] = "integer argument"
	inputs[`(even? 0.0)`] = "integer argument"
	inputs[`(even? 0/1)`] = "integer argument"
	inputs[`(even? 0.0+0.0i)`] = "integer argument"
	verifyInterpretError(t, inputs)
}

func TestBuiltinMax(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(max 3 4)`] = `4`
	inputs[`(max 4 3)`] = `4`
	inputs[`(max 1 1 1)`] = `1`
	inputs[`(max 3 2 1 4)`] = `4`
	inputs[`(max 3 2 1 1 1)`] = `3`
	inputs[`(max 4.0 4)`] = `4`
	inputs[`(max 0+3i 0)`] = `0`
	inputs[`(max 4 1/3)`] = `4`
	inputs[`(max 6.0+0.0i 15/3 4 3.0)`] = `6+0i`
	verifyInterpret(t, inputs)
	inputs = make(map[string]string)
	inputs[`(max 3)`] = "two or more arguments"
	inputs[`(max 'a 4)`] = "numeric arguments"
	inputs[`(max 4 'b)`] = "numeric arguments"
	verifyInterpretError(t, inputs)
}

func TestBuiltinMin(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(min 3 4)`] = `3`
	inputs[`(min 4 3)`] = `3`
	inputs[`(min 1 1 1)`] = `1`
	inputs[`(min 3 2 1 4)`] = `1`
	inputs[`(min 3 2 1 1 1)`] = `1`
	inputs[`(min 4.0 4)`] = `4`
	inputs[`(min 0+3i 0)`] = `0+3i`
	inputs[`(min 4 1/3)`] = `1/3`
	inputs[`(min 6.0+0.0i 15/3 4 3.0)`] = `3`
	verifyInterpret(t, inputs)
	inputs = make(map[string]string)
	inputs[`(min 3)`] = "two or more arguments"
	inputs[`(min 'a 4)`] = "numeric arguments"
	inputs[`(min 4 'b)`] = "numeric arguments"
	verifyInterpretError(t, inputs)
}

func TestBuiltinAdd(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(+)`] = `0`
	inputs[`(+ 3)`] = `3`
	inputs[`(+ 3 4)`] = `7`
	inputs[`(+ 4 3)`] = `7`
	inputs[`(+ 1 1 1)`] = `3`
	inputs[`(+ 3 2 1 4)`] = `10`
	inputs[`(+ 3 2 1 1 1)`] = `8`
	inputs[`(+ 4.0 4)`] = `8`
	inputs[`(+ 0+3i 0)`] = `0+3i`
	inputs[`(+ 4 1/3)`] = `13/3`
	inputs[`(+ 6.0+0.0i 15/3 4 3.0)`] = `18+0i`
	verifyInterpret(t, inputs)
	inputs = make(map[string]string)
	inputs[`(+ 'a 4)`] = "numeric arguments"
	inputs[`(+ 4 'b)`] = "numeric arguments"
	verifyInterpretError(t, inputs)
}

func TestBuiltinSubtract(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(-)`] = `0`
	inputs[`(- 3)`] = `-3`
	inputs[`(- 3 4)`] = `-1`
	inputs[`(- 4 3)`] = `1`
	inputs[`(- 1 1 1)`] = `-1`
	inputs[`(- 3 2 1 4)`] = `-4`
	inputs[`(- 3 2 1 1 1)`] = `-2`
	inputs[`(- 4.0 4)`] = `0`
	inputs[`(- 0+3i 0)`] = `0+3i`
	inputs[`(- 4 1/3)`] = `11/3`
	inputs[`(- 6.0+0.0i 15/3 4 3.0)`] = `-6+0i`
	verifyInterpret(t, inputs)
	inputs = make(map[string]string)
	inputs[`(- 'a 4)`] = "numeric arguments"
	inputs[`(- 4 'b)`] = "numeric arguments"
	verifyInterpretError(t, inputs)
}

func TestBuiltinMultiply(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(*)`] = `1`
	inputs[`(* 3)`] = `3`
	inputs[`(* 3 4)`] = `12`
	inputs[`(* 4 3)`] = `12`
	inputs[`(* 1 1 1)`] = `1`
	inputs[`(* 3 2 1 4)`] = `24`
	inputs[`(* 3 2 1 1 1)`] = `6`
	inputs[`(* 4.0 4)`] = `16`
	inputs[`(* 0+3i 0)`] = `0+0i`
	inputs[`(* 4 1/3)`] = `4/3`
	inputs[`(* 3 1/3)`] = `1`
	inputs[`(* 6.0+0.0i 15/3 4 3.0)`] = `360+0i`
	verifyInterpret(t, inputs)
	inputs = make(map[string]string)
	inputs[`(* 'a 4)`] = "numeric arguments"
	inputs[`(* 4 'b)`] = "numeric arguments"
	verifyInterpretError(t, inputs)
}

func TestBuiltinDivide(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(/ 3)`] = `1/3`
	inputs[`(/ 3.0)`] = `0.3333333333333333`
	inputs[`(/ 3.0+0.0i)`] = `0.3333333333333333+0i`
	inputs[`(/ 3/1)`] = `1/3`
	inputs[`(/ 3 4)`] = `3/4`
	inputs[`(/ 3 4 5)`] = `3/20`
	inputs[`(/ 3 1)`] = `3`
	inputs[`(/ 4.0 4)`] = `1`
	inputs[`(/ 4 1/3)`] = `12`
	inputs[`(/ 3.0+0.0i 2/1)`] = `1.5+0i`
	inputs[`(/ 1 0.0)`] = "+Inf"
	inputs[`(/ 1 0.0+0.0i)`] = "+Inf+Infi"
	verifyInterpret(t, inputs)
	inputs = make(map[string]string)
	inputs[`(/)`] = "one or more arguments"
	inputs[`(/ 'a 4)`] = "numeric arguments"
	inputs[`(/ 4 'b)`] = "numeric arguments"
	inputs[`(/ 1 0)`] = "division by zero"
	verifyInterpretError(t, inputs)
}

func TestBuiltinAbs(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(abs 3)`] = `3`
	inputs[`(abs -3)`] = `3`
	inputs[`(abs -0.1)`] = `0.1`
	inputs[`(abs 0.1)`] = `0.1`
	inputs[`(abs -1/5)`] = `1/5`
	inputs[`(abs 1/5)`] = `1/5`
	verifyInterpret(t, inputs)
	inputs = make(map[string]string)
	inputs[`(abs)`] = "takes only one"
	inputs[`(abs 3 3)`] = "takes only one"
	inputs[`(abs 'a)`] = "numeric argument"
	inputs[`(abs -1.0+0.5i)`] = "complex types"
	verifyInterpretError(t, inputs)
}
