//
// Copyright 2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	gc "launchpad.net/gocheck"
)

type BooleansSuite struct {
}

var _ = gc.Suite(&BooleansSuite{})

func (ls *BooleansSuite) TestBuiltinNot(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(not #t)`] = `#f`
	inputs[`(not 3)`] = `#f`
	inputs[`(not (quote (3)))`] = `#f`
	inputs[`(not #f)`] = `#t`
	inputs[`(not '())`] = `#f`
	// inputs[`(not (list))`] = `#f` TODO when (list) is added
	inputs[`(not 'nil)`] = `#f`
	checkInterpret(c, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(not)`] = ".* requires 1"
	checkInterpretError(c, inputs)
}

func (ls *BooleansSuite) TestBuiltinIsBoolean(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(boolean? #t)`] = `#t`
	inputs[`(boolean? #f)`] = `#t`
	inputs[`(boolean? 0)`] = `#f`
	inputs[`(boolean? '())`] = `#f`
	inputs[`(boolean? #t #t #f)`] = `#t`
	inputs[`(boolean? 1 2 3)`] = `#f`
	checkInterpret(c, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(boolean?)`] = ".* requires at least 1"
	checkInterpretError(c, inputs)
}
