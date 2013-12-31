//
// Copyright 2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	gc "launchpad.net/gocheck"
)

type ByteVectorSuite struct {
}

var _ = gc.Suite(&ByteVectorSuite{})

// func (s *ByteVectorSuite) TestVectorNil(c *gc.C) {
// 	var v ByteVector = nil
// 	c.Check(v, gc.IsNil)
// 	c.Check(v.Length(), gc.Equals, 0)
// 	c.Check(v.Get(0), gc.IsNil)
// 	c.Check(v.Get(1), gc.IsNil)
// 	v.Set(0, 'a')
// 	v.Set(1, 'a')
// }

func (s *ByteVectorSuite) TestVector(c *gc.C) {
	data := make([]uint8, 0)
	data = append(data, 1)
	data = append(data, 2)
	data = append(data, 3)
	data = append(data, 4)
	v := NewByteVector(data)
	c.Check(v, gc.Not(gc.IsNil))
	c.Check(v.Length(), gc.Equals, 4)
	c.Check(v.Get(0), gc.Equals, uint8(1))
	c.Check(v.Get(1), gc.Equals, uint8(2))
	c.Check(v.Get(2), gc.Equals, uint8(3))
	c.Check(v.Get(3), gc.Equals, uint8(4))
	v.Set(1, 10)
	c.Check(v.Get(1), gc.Equals, uint8(10))
}
