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

func (ls *ListSuite) TestBuiltinIsPair(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(pair? '(x . y))`] = `#t`
	inputs[`(pair? '(a b c))`] = `#t`
	inputs[`(pair? '())`] = `#f`
	inputs[`(pair? '#(a b))`] = `#f`
	checkInterpret(c, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(pair? 'a 'b)`] = ".* requires 1"
	inputs[`(pair?)`] = ".* requires 1"
	checkInterpretError(c, inputs)
}

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

func (ls *ListSuite) TestBuiltinLength(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(length '(x . y))`] = `2`
	inputs[`(length '(a b c))`] = `3`
	inputs[`(length '(a b . c))`] = `3`
	inputs[`(length '(a (b) (c d e)))`] = `3`
	inputs[`(length '())`] = `0`
	inputs[`(length '#1=(a b . #1#))`] = `3`
	inputs[`(length '#2=(a b a b . #2#))`] = `5`
	inputs[`(length '#1=(a b #1#))`] = `3`
	inputs[`(length '#2=(a b a b #2#))`] = `5`
	// TODO: blows up the stack
	// inputs[`(length '#3=#(a b #3#))`] = `3`
	// inputs[`(length '#4=#(a b a b #4#))`] = `5`
	checkInterpret(c, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(length 'a)`] = ".* expects a list.*"
	inputs[`(length '(a) '(b))`] = ".* requires 1"
	inputs[`(length)`] = ".* requires 1"
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

func (ls *ListSuite) TestBuiltinReverse(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(reverse '(x . y))`] = `(y . x)`
	inputs[`(reverse '(a b c))`] = `(c b a)`
	inputs[`(reverse '(a (b c) d (e (f))))`] = `((e (f)) d (b c) a)`
	inputs[`(reverse '())`] = `()`
	checkInterpret(c, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(reverse 'a)`] = ".* expects a list.*"
	inputs[`(reverse '(a) '(b))`] = ".* requires 1"
	inputs[`(reverse)`] = ".* requires 1"
	checkInterpretError(c, inputs)
}

func (ls *ListSuite) TestBuiltinAssoc(c *gc.C) {
	inputs := make(map[string]string)
	// TODO: implement assoc and test
	// inputs[`(assoc (list 'a) '(((a)) ((b)) ((c))))`] = `((a))`
	// inputs[`(assoc 2.0 '((1 1) (2 4) (3 9)) =)`] = `(2 4)`
	// checkInterpret(c, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(assoc 'a 'b)`] = ".* expects a list.*"
	inputs[`(assoc 'a)`] = ".* requires at least 2"
	inputs[`(assoc)`] = ".* requires at least 2"
	checkInterpretError(c, inputs)
}

func (ls *ListSuite) TestBuiltinList(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(list 'x 'y)`] = `(x y)`
	inputs[`(list 'a (+ 3 4) 'c)`] = `(a 7 c)`
	inputs[`(list)`] = `()`
	checkInterpret(c, inputs)
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
	inputs[`(cdr '(a . (b c)))`] = `(b c)`
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

func (ls *ListSuite) TestBuiltinIsNull(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(null? '(x . y))`] = `#f`
	inputs[`(null? '())`] = `#t`
	inputs[`(null? 3)`] = `#f`
	checkInterpret(c, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(null? 'a 'b)`] = ".* requires 1"
	inputs[`(null?)`] = ".* requires 1"
	checkInterpretError(c, inputs)
}

func (ls *ListSuite) TestBuiltinIsList(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(list? '(a b c))`] = `#t`
	inputs[`(list? '())`] = `#t`
	inputs[`(list? '(x . y))`] = `#f`
	inputs[`(list? '#(a b))`] = `#f`
	inputs[`(list? 3)`] = `#f`
	checkInterpret(c, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(list? 'a 'b)`] = ".* requires 1"
	inputs[`(list?)`] = ".* requires 1"
	checkInterpretError(c, inputs)
}
