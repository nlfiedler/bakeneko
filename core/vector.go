//
// Copyright 2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

//
// vector support for our Scheme-like language.
//

import (
	"bytes"
)

// Vector represents the vector type in Scheme.
type Vector interface {
	// ObjectId returns the unique identifier for this object.
	ObjectId() uint64
	// Length returns the number of elements in the vector.
	Length() int
	// Get returns the element in the specified (0-based) position.
	Get(pos int) interface{}
	// Set replaces the value at position pos with the given value.
	Set(pos int, val interface{})
}

// vector is the internal implementation of Vector.
type vector struct {
	id   uint64        // object identifier
	data []interface{} // vector data
}

// NewVector wraps the given data in a vector type.
func NewVector(data []interface{}) Vector {
	v := new(vector)
	v.id = newObjectId()
	v.data = data
	return v
}

func (b *vector) ObjectId() uint64 {
	if b == nil {
		return 0
	}
	return b.id
}

func (b *vector) Length() int {
	if b == nil {
		return 0
	}
	return len(b.data)
}

func (b *vector) Get(pos int) interface{} {
	if b == nil || pos < 0 || pos >= len(b.data) {
		return nil
	}
	return b.data[pos]
}

func (b *vector) Set(pos int, val interface{}) {
	if b != nil && pos >= 0 && pos < len(b.data) {
		b.data[pos] = val
	}
}

func (b *vector) String() string {
	if b == nil {
		return "#()"
	}
	buf := new(bytes.Buffer)
	buf.WriteString("#(")
	for _, v := range b.data {
		stringifyBuffer(v, buf)
		buf.WriteString(" ")
	}
	// lop off the trailing space
	if buf.Len() > 2 {
		buf.Truncate(buf.Len() - 1)
	}
	buf.WriteString(")")
	return buf.String()
}
