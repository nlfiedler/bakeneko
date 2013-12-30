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
