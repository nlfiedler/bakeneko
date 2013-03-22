//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

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
