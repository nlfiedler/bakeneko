//
// Copyright 2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	gc "launchpad.net/gocheck"
)

type SymbolSuite struct {
}

var _ = gc.Suite(&SymbolSuite{})

func (s *SymbolSuite) TestSymbolPredicate(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(symbol? 'foo)`] = `#t`
	inputs[`(symbol? (car '(a 'b)))`] = `#t`
	inputs[`(symbol? "bar")`] = `#f`
	inputs[`(symbol? 'nil)`] = `#t`
	inputs[`(symbol? '())`] = `#f`
	inputs[`(symbol? #f)`] = `#f`
	checkInterpret(c, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(symbol? 'a 'b 'c)`] = ".* requires 1"
	inputs[`(symbol?)`] = ".* requires 1"
	checkInterpretError(c, inputs)
}

func (s *SymbolSuite) TestSymbolEqual(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(symbol=? 'foo 'foo)`] = `#t`
	inputs[`(symbol=? 'foo 'foo 'foo)`] = `#t`
	inputs[`(symbol=? 'foo 'foo 'foo 'foo)`] = `#t`
	inputs[`(symbol=? 'foo 'bar)`] = `#f`
	checkInterpret(c, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(symbol=?)`] = ".* requires at least 2"
	inputs[`(symbol=? 'a)`] = ".* requires at least 2"
	inputs[`(symbol=? "foo" "bar")`] = ".* requires symbol arguments"
	checkInterpretError(c, inputs)
}

func (s *SymbolSuite) TestSymbolToString(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(symbol->string 'flying-fish)`] = `"flying-fish"`
	inputs[`(symbol->string 'Martin)`] = `"Martin"`
	checkInterpret(c, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(symbol->string "str")`] = ".* requires symbol arguments"
	inputs[`(symbol->string 'a 'b)`] = ".* requires 1"
	inputs[`(symbol->string)`] = ".* requires 1"
	checkInterpretError(c, inputs)
}

func (s *SymbolSuite) TestSymbolFromString(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(string->symbol "mISSISSIppi")`] = `mISSISSIppi`
	checkInterpret(c, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(string->symbol 'foobar)`] = ".* requires string argument"
	inputs[`(string->symbol 'a 'b)`] = ".* requires 1"
	inputs[`(string->symbol)`] = ".* requires 1"
	checkInterpretError(c, inputs)
}
