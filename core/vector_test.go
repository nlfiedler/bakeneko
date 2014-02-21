//
// Copyright 2013-2014 Nathan Fiedler. All rights reserved.
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

func (s *VectorSuite) TestVector(c *gc.C) {
	data := make([]interface{}, 0)
	data = append(data, 1)
	data = append(data, 2)
	data = append(data, 3)
	data = append(data, 4)
	v := NewVector(data)
	c.Check(v, gc.NotNil)
	var seq Sequence
	c.Check(v, gc.Implements, &seq)
	c.Check(v.Len(), gc.Equals, 4)
	c.Check(v.Get(0), gc.Equals, 1)
	c.Check(v.Get(1), gc.Equals, 2)
	c.Check(v.Get(2), gc.Equals, 3)
	c.Check(v.Get(3), gc.Equals, 4)
	c.Check(v.First(), gc.Equals, 1)
	c.Check(v.Second(), gc.Equals, 2)
	c.Check(v.Third(), gc.Equals, 3)
	rest := v.Rest()
	c.Check(rest, gc.FitsTypeOf, v)
	c.Check(rest.(Vector).Len(), gc.Equals, 3)
	c.Check(rest.(Vector).First(), gc.Equals, 2)
	v.Set(1, 'a')
	c.Check(v.Get(1), gc.Equals, 'a')
}

func (s *VectorSuite) TestVectorEmpty(c *gc.C) {
	data := make([]interface{}, 0)
	v := NewVector(data)
	c.Check(v, gc.NotNil)
	c.Check(v.Len(), gc.Equals, 0)
	c.Check(v.Get(-1), gc.IsNil)
	c.Check(v.Get(0), gc.IsNil)
	c.Check(v.Get(10), gc.IsNil)
	c.Check(v.Get(100), gc.IsNil)
}

func (s *VectorSuite) TestVectorNil(c *gc.C) {
	v := NewVector(nil)
	c.Check(v, gc.IsNil)
	c.Check(v.Len(), gc.Equals, 0)
	c.Check(v.Get(-1), gc.IsNil)
	c.Check(v.Get(0), gc.IsNil)
	c.Check(v.Get(10), gc.IsNil)
	c.Check(v.Get(100), gc.IsNil)
}

func (s *VectorSuite) TestVectorIterator(c *gc.C) {
	data := make([]interface{}, 0)
	data = append(data, 1)
	data = append(data, 2)
	data = append(data, 3)
	data = append(data, 4)
	v := NewVector(data)
	iter := v.Iterator()
	c.Check(iter.HasNext(), gc.Equals, true)
	c.Check(iter.Next(), gc.Equals, 1)
	c.Check(iter.HasNext(), gc.Equals, true)
	c.Check(iter.Next(), gc.Equals, 2)
	c.Check(iter.HasNext(), gc.Equals, true)
	c.Check(iter.Next(), gc.Equals, 3)
	c.Check(iter.HasNext(), gc.Equals, true)
	c.Check(iter.Next(), gc.Equals, 4)
	c.Check(iter.IsProper(), gc.Equals, true)
	c.Check(iter.HasNext(), gc.Equals, false)
	c.Check(iter.Next(), gc.IsNil)
}

func (s *VectorSuite) TestVectorIteratorEmpty(c *gc.C) {
	data := make([]interface{}, 0)
	v := NewVector(data)
	iter := v.Iterator()
	c.Check(iter.HasNext(), gc.Equals, false)
	c.Check(iter.IsProper(), gc.Equals, true)
	c.Check(iter.Next(), gc.IsNil)
}

func (s *VectorSuite) TestVectorIteratorNil(c *gc.C) {
	v := NewVector(nil)
	iter := v.Iterator()
	c.Check(iter.HasNext(), gc.Equals, false)
	c.Check(iter.IsProper(), gc.Equals, true)
	c.Check(iter.Next(), gc.IsNil)
}
