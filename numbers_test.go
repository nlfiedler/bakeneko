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
