//
// Copyright 2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	gc "launchpad.net/gocheck"
)

type VectorSuite struct {
}

var _ = gc.Suite(&VectorSuite{})

// func (s *VectorSuite) TestVectorNil(c *gc.C) {
// 	var v Vector = nil
// 	c.Check(v, gc.IsNil)
// 	c.Check(v.Length(), gc.Equals, 0)
// 	c.Check(v.Get(0), gc.IsNil)
// 	c.Check(v.Get(1), gc.IsNil)
// 	v.Set(0, 'a')
// 	v.Set(1, 'a')
// }

func (s *VectorSuite) TestVector(c *gc.C) {
	data := make([]interface{}, 0)
	data = append(data, 1)
	data = append(data, 2)
	data = append(data, 3)
	data = append(data, 4)
	v := NewVector(data)
	c.Check(v, gc.Not(gc.IsNil))
	c.Check(v.Length(), gc.Equals, 4)
	c.Check(v.Get(0), gc.Equals, 1)
	c.Check(v.Get(1), gc.Equals, 2)
	c.Check(v.Get(2), gc.Equals, 3)
	c.Check(v.Get(3), gc.Equals, 4)
	v.Set(1, 'a')
	c.Check(v.Get(1), gc.Equals, 'a')
}
