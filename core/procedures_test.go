//
// Copyright 2012-2015 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	gc "launchpad.net/gocheck"
)

type ProcedureSuite struct {
}

var _ = gc.Suite(&ProcedureSuite{})

func (ls *ProcedureSuite) TestBuiltinApply(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(apply + (list 3 4))`] = `7`
	inputs[`(apply + (list 3 4) 10)`] = `17`
	compose := `; comment
(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))
((compose abs -) 12 75)`
	inputs[compose] = "63"
	checkInterpret(c, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(apply +)`] = ".* requires at least 2"
	inputs[`(apply)`] = ".* requires at least 2"
	checkInterpretError(c, inputs)
}

func (ls *ProcedureSuite) TestBuiltinMap(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(map cadr '((a b) (d e) (g h)))`] = `(b e h)`
	inputs[`(map (lambda (n) (* n n)) '(1 2 3 4 5))`] = `(1 4 9 16 25)`
	// TODO: when let is supported...
	// compose := `(let ((count 0))
	//   (map (lambda (ignored)
	//          (set! count (+ count 1))
	//          count)
	//        ’(a b)))`
	// inputs[compose] = "(1 2)" // or (2 1)
	checkInterpret(c, inputs)
	// error cases
	inputs = make(map[string]string)
	inputs[`(map +)`] = ".* requires at least 2"
	inputs[`(map)`] = ".* requires at least 2"
	checkInterpretError(c, inputs)
}
