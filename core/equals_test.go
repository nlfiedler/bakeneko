//
// Copyright 2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	gc "launchpad.net/gocheck"
)

type EqualsSuite struct {
}

var _ = gc.Suite(&EqualsSuite{})

func (s *EqualsSuite) TestEqv(c *gc.C) {
	inputs := make(map[string]string)
	// symbols
	inputs[`(eqv? 'foo 'bar)`] = `#f`
	inputs[`(eqv? 'foo 'foo)`] = `#t`
	inputs[`(eqv? 'foo 'FOO)`] = `#f`
	// strings
	inputs[`(eqv? "foo" "bar")`] = `#f`
	inputs[`(eqv? "foo" "foo")`] = `#t`
	inputs[`(eqv? "foo" "FOO")`] = `#f`
	inputs[`(eqv? "" "")`] = `#t`
	// characters
	inputs[`(eqv? #\a #\b)`] = `#f`
	inputs[`(eqv? #\a #\A)`] = `#f`
	inputs[`(eqv? #\a #\a)`] = `#t`
	// lists
	inputs[`(eqv? '() '())`] = `#t`
	inputs[`(eqv? '() (cdr '(1)))`] = `#t`
	inputs[`(eqv? '#() '#())`] = `#t`
	inputs[`(eqv? '(1 2 3) '(1 2 3))`] = `#f`
	inputs[`(eqv? '#(1 2 3) '#(1 2 3))`] = `#f`
	inputs[`(eqv? '#u8() '#u8())`] = `#t`
	inputs[`(eqv? '#u8(1 2 3) '#u8(1 2 3))`] = `#f`
	// booleans
	inputs[`(eqv? #f #t)`] = `#f`
	inputs[`(eqv? #f #f)`] = `#t`
	inputs[`(eqv? #t #t)`] = `#t`
	// numbers
	inputs[`(eqv? 1.0 1)`] = `#f`
	inputs[`(eqv? 4/1 4)`] = `#f`
	inputs[`(eqv? 1 2)`] = `#f`
	inputs[`(eqv? 2 2)`] = `#t`
	// lambdas
	inputs[`(eqv? (lambda (x) 1) (lambda (x) 2))`] = `#f`
	// mixed
	inputs[`(eqv? 1 #\a)`] = `#f`
	inputs[`(eqv? 1 "a")`] = `#f`
	inputs[`(eqv? 'foo "a")`] = `#f`
	checkInterpret(c, inputs)
	inputs = make(map[string]string)
	inputs[`(eqv? 'foobar)`] = ".* requires 2"
	inputs[`(eqv?)`] = ".* requires 2"
	checkInterpretError(c, inputs)
}

func (s *EqualsSuite) TestEq(c *gc.C) {
	inputs := make(map[string]string)
	// symbols
	inputs[`(eq? 'foo 'bar)`] = `#f`
	inputs[`(eq? 'foo 'foo)`] = `#t`
	inputs[`(eq? 'foo 'FOO)`] = `#f`
	// strings
	inputs[`(eq? "foo" "bar")`] = `#f`
	inputs[`(eq? "foo" "foo")`] = `#t`
	inputs[`(eq? "foo" "FOO")`] = `#f`
	inputs[`(eq? "" "")`] = `#t`
	// characters
	inputs[`(eq? #\a #\b)`] = `#f`
	inputs[`(eq? #\a #\A)`] = `#f`
	inputs[`(eq? #\a #\a)`] = `#t`
	// lists
	inputs[`(eq? '() '())`] = `#t`
	inputs[`(eq? '() (cdr '(1)))`] = `#t`
	inputs[`(eq? '#() '#())`] = `#t`
	inputs[`(eq? '(1 2 3) '(1 2 3))`] = `#f`
	inputs[`(eq? '#(1 2 3) '#(1 2 3))`] = `#f`
	inputs[`(eq? '#u8() '#u8())`] = `#t`
	inputs[`(eq? '#u8(1 2 3) '#u8(1 2 3))`] = `#f`
	// booleans
	inputs[`(eq? #f #t)`] = `#f`
	inputs[`(eq? #f #f)`] = `#t`
	inputs[`(eq? #t #t)`] = `#t`
	// numbers
	inputs[`(eq? 1.0 1)`] = `#f`
	inputs[`(eq? 4/1 4)`] = `#f`
	inputs[`(eq? 1 2)`] = `#f`
	inputs[`(eq? 2 2)`] = `#t`
	// lambdas
	inputs[`(eq? (lambda (x) 1) (lambda (x) 2))`] = `#f`
	// mixed
	inputs[`(eq? 1 #\a)`] = `#f`
	inputs[`(eq? 1 "a")`] = `#f`
	inputs[`(eq? 'foo "a")`] = `#f`
	checkInterpret(c, inputs)
	inputs = make(map[string]string)
	inputs[`(eq? 'foobar)`] = ".* requires 2"
	inputs[`(eq?)`] = ".* requires 2"
	checkInterpretError(c, inputs)
}

func (s *EqualsSuite) TestEqual(c *gc.C) {
	inputs := make(map[string]string)
	// symbols
	inputs[`(equal? 'foo 'bar)`] = `#f`
	inputs[`(equal? 'foo 'foo)`] = `#t`
	inputs[`(equal? 'foo 'FOO)`] = `#f`
	// strings
	inputs[`(equal? "foo" "bar")`] = `#f`
	inputs[`(equal? "foo" "foo")`] = `#t`
	inputs[`(equal? "foo" "FOO")`] = `#f`
	inputs[`(equal? "" "")`] = `#t`
	// characters
	inputs[`(equal? #\a #\b)`] = `#f`
	inputs[`(equal? #\a #\A)`] = `#f`
	inputs[`(equal? #\a #\a)`] = `#t`
	// lists
	// inputs[`(equal? '() '())`] = `#t`
	// inputs[`(equal? '('a 'b 'c) '('a 'b 'c))`] = `#t`
	// inputs[`(equal? '('a ('b 'c)) '('a ('b 'c)))`] = `#t`
	// inputs[`(equal? '('a ('b 'c)) '(('a 'b) 'c))`] = `#f`
	// inputs[`(equal? '() (cdr '(1)))`] = `#t`
	// inputs[`(equal? '(#\a #(1 2 3) "joe") '(#\a #(1 2 3) "joe"))`] = `#t`
	// inputs[`(equal? '#1=(a b . #1#) '#2=(a b a b . #2#))`] = `#t`
	inputs[`(equal? '#() '#())`] = `#t`
	// inputs[`(equal? '(1 2 3) '(1 2 3))`] = `#t`
	inputs[`(equal? '#(1 2 3) '#(1 2 3))`] = `#t`
	// inputs[`(equal? '#1=#(a b . #1#) '#2=#(a b a b . #2#))`] = `#t`
	// inputs[`(equal? '#(0 (2 2 2 2) "Anna") '#(0 (2 2 2 2) "Anna"))`] = `#t`
	inputs[`(equal? '#u8() '#u8())`] = `#t`
	inputs[`(equal? '#u8(1 2 3) '#u8(1 2 3))`] = `#t`
	inputs[`(equal? '#u8(1 2 3) '#u8(1 2 4))`] = `#f`
	// booleans
	inputs[`(equal? #f #t)`] = `#f`
	inputs[`(equal? #f #f)`] = `#t`
	inputs[`(equal? #t #t)`] = `#t`
	// numbers
	inputs[`(equal? 1.0 1)`] = `#f`
	inputs[`(equal? 4/1 4)`] = `#f`
	inputs[`(equal? 1 2)`] = `#f`
	inputs[`(equal? 2 2)`] = `#t`
	// lambdas
	inputs[`(equal? (lambda (x) 1) (lambda (x) 2))`] = `#f`
	// mixed
	inputs[`(equal? 1 #\a)`] = `#f`
	inputs[`(equal? 1 "a")`] = `#f`
	inputs[`(equal? 'foo "a")`] = `#f`
	checkInterpret(c, inputs)
	inputs = make(map[string]string)
	inputs[`(equal? 'foobar)`] = ".* requires 2"
	inputs[`(equal?)`] = ".* requires 2"
	checkInterpretError(c, inputs)
}
