//
// Copyright 2012-2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	gc "launchpad.net/gocheck"
	"testing"
)

type NumbersSuite struct {
}

var _ = gc.Suite(&NumbersSuite{})

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
	inputs[`(= 3)`] = "requires at least 2"
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
	inputs[`(< 3)`] = "requires at least 2"
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
	inputs[`(<= 3)`] = "requires at least 2"
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
	inputs[`(> 3)`] = "requires at least 2"
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
	inputs[`(>= 3)`] = "requires at least 2"
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
	inputs[`(zero?)`] = "requires 1"
	inputs[`(zero? 1 2)`] = "requires 1"
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
	inputs[`(positive?)`] = "requires 1"
	inputs[`(positive? 1 2)`] = "requires 1"
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
	inputs[`(negative?)`] = "requires 1"
	inputs[`(negative? 1 2)`] = "requires 1"
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
	inputs[`(odd?)`] = "requires 1"
	inputs[`(odd? 1 2)`] = "requires 1"
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
	inputs[`(even?)`] = "requires 1"
	inputs[`(even? 1 2)`] = "requires 1"
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
	inputs[`(max 3)`] = "requires at least 2"
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
	inputs[`(min 3)`] = "requires at least 2"
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
	inputs[`(+ 152399025 100)`] = `152399125`
	inputs[`(+ (+ 152399025 100) 1000)`] = `152400125`
	inputs[`(/ (+ 1 (/ 152399025 1)) 2)`] = `76199513`
	// TODO: parser really big integers
	// inputs[`(+ 19223372036854775807 100)`] = `19223372036854775907`
	inputs[`(+ 1 1 1)`] = `3`
	inputs[`(+ 3 2 1 4)`] = `10`
	inputs[`(+ 3 2 1 1 1)`] = `8`
	// TODO: parser really big floats
	// inputs[`(+ 19223372036854775807.5 100)`] = `19223372036854775907.5`
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
	inputs[`(/)`] = "requires at least 1"
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
	inputs[`(abs)`] = "requires 1"
	inputs[`(abs 3 3)`] = "requires 1"
	inputs[`(abs 'a)`] = "numeric argument"
	inputs[`(abs -1.0+0.5i)`] = "complex types"
	verifyInterpretError(t, inputs)
}

func TestBuiltinQuotient(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(quotient 5 2)`] = `2`
	inputs[`(quotient 4 2)`] = `2`
	inputs[`(quotient 1 2)`] = `0`
	inputs[`(quotient 4 5)`] = `0`
	inputs[`(quotient 5 4)`] = `1`
	verifyInterpret(t, inputs)
	inputs = make(map[string]string)
	inputs[`(quotient)`] = "requires 2"
	inputs[`(quotient 4)`] = "requires 2"
	inputs[`(quotient 'a 4)`] = "integer arguments"
	inputs[`(quotient 4 'b)`] = "integer arguments"
	inputs[`(quotient 3.0 4)`] = "integer arguments"
	inputs[`(quotient 4 3.0)`] = "integer arguments"
	inputs[`(quotient 1 0)`] = "division by zero"
	verifyInterpretError(t, inputs)
}

func (s *NumbersSuite) TestIsqrt(c *gc.C) {
	c.Check(isqrt(1), gc.Equals, uint32(1))
	c.Check(isqrt(2), gc.Equals, uint32(1))
	c.Check(isqrt(3), gc.Equals, uint32(1))
	c.Check(isqrt(4), gc.Equals, uint32(2))
	c.Check(isqrt(9), gc.Equals, uint32(3))
	c.Check(isqrt(16), gc.Equals, uint32(4))
	c.Check(isqrt(25), gc.Equals, uint32(5))
	c.Check(isqrt(36), gc.Equals, uint32(6))
	c.Check(isqrt(49), gc.Equals, uint32(7))
	c.Check(isqrt(64), gc.Equals, uint32(8))
	c.Check(isqrt(81), gc.Equals, uint32(9))
	c.Check(isqrt(100), gc.Equals, uint32(10))
	c.Check(isqrt(10000), gc.Equals, uint32(100))
}

func (s *NumbersSuite) TestBuiltinSqrt(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(sqrt 2)`] = `1`
	inputs[`(sqrt 4)`] = `2`
	inputs[`(sqrt 9)`] = `3`
	inputs[`(sqrt 16)`] = `4`
	inputs[`(sqrt 24.0)`] = `4.898979485566356`
	inputs[`(sqrt 36)`] = `6`
	inputs[`(sqrt 48.0)`] = `6.928203230275509`
	inputs[`(sqrt 64)`] = `8`
	inputs[`(sqrt 81)`] = `9`
	inputs[`(sqrt 100)`] = `10`
	inputs[`(sqrt -1)`] = `0+0i`
	inputs[`(sqrt 7+9i)`] = `3.033294764030639+1.4835353468979733i`
	inputs[`(sqrt 12/3)`] = `2`
	inputs[`(sqrt 4/9)`] = `0.6666666666666666`
	virtmachPassTest(c, inputs)
	inputs = make(map[string]string)
	inputs[`(sqrt)`] = ".* requires 1"
	inputs[`(sqrt 4 4)`] = ".* requires 1"
	inputs[`(sqrt "str")`] = ".* numeric argument"
	virtmachErrorTest(c, inputs)
}
