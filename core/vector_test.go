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

func (s *VectorSuite) TestListToVector(c *gc.C) {
	inputs := make(map[string]string)
	inputs["(list->vector '())"] = "#()"
	inputs["(list->vector '(1))"] = "#(1)"
	inputs["(list->vector '(1 2))"] = "#(1 2)"
	inputs["(list->vector '(1 2 3))"] = "#(1 2 3)"
	inputs["(list->vector '(1 2 3 4))"] = "#(1 2 3 4)"
	inputs[`(list->vector '(#\a "abc" '(12 24 48)))`] = `#(#\a "abc" (quote (12 24 48)))`
	checkInterpret(c, inputs)
	// test error cases
	errors := make(map[string]string)
	errors["(list->vector 1)"] = ".* expects a list, not 1"
	errors[`(list->vector "abc")`] = `.* expects a list, not "abc"`
	checkInterpretError(c, errors)
}

func (s *VectorSuite) TestVectorToList(c *gc.C) {
	inputs := make(map[string]string)
	inputs["(vector->list '#())"] = "()"
	inputs["(vector->list '#(1))"] = "(1)"
	inputs["(vector->list '#(1 2))"] = "(1 2)"
	inputs["(vector->list '#(1 2 3))"] = "(1 2 3)"
	inputs["(vector->list '#(1 2 3 4))"] = "(1 2 3 4)"
	inputs["(vector->list '#(1 2 3 4 5 6 7 8 9 10) 5)"] = "(6 7 8 9 10)"
	inputs["(vector->list '#(1 2 3 4 5 6 7 8 9 10) 3 7)"] = "(4 5 6 7)"
	checkInterpret(c, inputs)
	// test error cases
	errors := make(map[string]string)
	errors["(vector->list 1)"] = ".* expects a vector, not 1"
	errors[`(vector->list "abc")`] = `.* expects a vector, not "abc"`
	errors["(vector->list '#(1 2 3 4) #\\a)"] = ".* expects a number, .*"
	errors["(vector->list '#(1 2 3 4) 1 #\\a)"] = ".* expects a number, .*"
	errors["(vector->list '#(1 2 3 4) -2)"] = ".* index out of bounds .*"
	errors["(vector->list '#(1 2 3 4) 10)"] = ".* index out of bounds .*"
	errors["(vector->list '#(1 2 3 4) 2 10)"] = ".* index out of bounds .*"
	errors["(vector->list '#(1 2 3 4) -2 10)"] = ".* index out of bounds .*"
	errors["(vector->list '#(1 2 3 4) 2 -10)"] = ".* index out of bounds .*"
	errors["(vector->list '#(1 2 3 4) 10 2)"] = ".* index out of bounds .*"
	checkInterpretError(c, errors)
}
