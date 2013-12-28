//
// Copyright 2012-2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	"testing"
)

func TestBuiltinCons(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(cons 'a '())`] = `(a)`
	inputs[`(cons '(a) '(b c d))`] = `((a) b c d)`
	inputs[`(cons "a" '(b c))`] = `("a" b c)`
	inputs[`(cons 'a 3)`] = `(a . 3)`
	inputs[`(cons '(a b) 'c)`] = `((a b) . c)`
	verifyInterpret(t, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(cons 'a 'b 'c)`] = "requires 2"
	inputs[`(cons 'a)`] = "requires 2"
	verifyInterpretError(t, inputs)
}

func TestBuiltinAppend(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(append '(x) '(y))`] = `(x y)`
	inputs[`(append '(a) '(b c d))`] = `(a b c d)`
	inputs[`(append '(a (b)) '((c)))`] = `(a (b) (c))`
	inputs[`(append '(a b) '(c . d))`] = `(a b c . d)`
	inputs[`(append '(a b c) 'd)`] = `(a b c . d)`
	inputs[`(append '() 'a)`] = `a`
	verifyInterpret(t, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(append 'a '(b c))`] = "is not a list"
	inputs[`(append '(a . b) '(c d))`] = "is not a list"
	verifyInterpretError(t, inputs)
}

func TestBuiltinCar(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(car '(a b c))`] = `a`
	inputs[`(car '((a) b c d))`] = `(a)`
	inputs[`(car '(1 . 2))`] = `1`
	verifyInterpret(t, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(car 1)`] = "expects a pair"
	inputs[`(car ())`] = "expects a non-empty pair"
	verifyInterpretError(t, inputs)
}

func TestBuiltinCdr(t *testing.T) {
	inputs := make(map[string]string)
	inputs[`(cdr '(a b c))`] = `(b c)`
	inputs[`(cdr '((a) b c d))`] = `(b c d)`
	inputs[`(cdr '(1 . 2))`] = `2`
	verifyInterpret(t, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(cdr 1)`] = "expects a pair"
	inputs[`(cdr ())`] = "expects a non-empty pair"
	verifyInterpretError(t, inputs)
}
