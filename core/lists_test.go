//
// Copyright 2012-2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	gc "launchpad.net/gocheck"
)

type ListSuite struct {
}

var _ = gc.Suite(&ListSuite{})

func (ls *ListSuite) TestBuiltinCons(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(cons 'a '())`] = `(a)`
	inputs[`(cons '(a) '(b c d))`] = `((a) b c d)`
	inputs[`(cons "a" '(b c))`] = `("a" b c)`
	inputs[`(cons 'a 3)`] = `(a . 3)`
	inputs[`(cons '(a b) 'c)`] = `((a b) . c)`
	checkInterpret(c, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(cons 'a 'b 'c)`] = ".* requires 2"
	inputs[`(cons 'a)`] = ".* requires 2"
	checkInterpretError(c, inputs)
}

func (ls *ListSuite) TestBuiltinAppend(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(append '(x) '(y))`] = `(x y)`
	inputs[`(append '(a) '(b c d))`] = `(a b c d)`
	inputs[`(append '(a (b)) '((c)))`] = `(a (b) (c))`
	inputs[`(append '(a b) '(c . d))`] = `(a b c . d)`
	inputs[`(append '(a b c) 'd)`] = `(a b c . d)`
	inputs[`(append '() 'a)`] = `a`
	checkInterpret(c, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(append 'a '(b c))`] = ".* is not a list."
	inputs[`(append '(a . b) '(c d))`] = ".* is not a list."
	checkInterpretError(c, inputs)
}

func (ls *ListSuite) TestBuiltinCar(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(car '(a b c))`] = `a`
	inputs[`(car '((a) b c d))`] = `(a)`
	inputs[`(car '(1 . 2))`] = `1`
	checkInterpret(c, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(car 1)`] = ".* expects a pair.*"
	inputs[`(car ())`] = ".* expects a non-empty pair"
	checkInterpretError(c, inputs)
}

func (ls *ListSuite) TestBuiltinCdr(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(cdr '(a b c))`] = `(b c)`
	inputs[`(cdr '((a) b c d))`] = `(b c d)`
	inputs[`(cdr '(1 . 2))`] = `2`
	checkInterpret(c, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(cdr 1)`] = ".* expects a pair.*"
	inputs[`(cdr ())`] = ".* expects a non-empty pair"
	checkInterpretError(c, inputs)
}

func (ls *ListSuite) TestCxr(c *gc.C) {
	inputs := make(map[string]string)
	inputs["(caar '((1 2 3)))"] = "1"
	inputs["(cdar '((a b c) d e f))"] = "(b c)"
	inputs["(cddr '(a b c d e f))"] = "(c d e f)"
	inputs["(cdddr '(a b c d e f))"] = "(d e f)"
	inputs["(cddddr '(a b c d e f))"] = "(e f)"
	inputs["(cadr '(a b c d e f))"] = "b"
	inputs["(caddr '(a b c d e f))"] = "c"
	inputs["(cadddr '(a b c d e f))"] = "d"
	checkInterpret(c, inputs)
}
