//
// Copyright 2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

//
// bytevector support for our Scheme-like language.
//

import (
	"bytes"
	"fmt"
)

// ByteVector represents the bytevector type in Scheme. It is a sequence of
// bytes whose values range from 0 to 255.
type ByteVector interface {
	// ObjectId returns the unique identifier for this object.
	ObjectId() uint64
	// Length returns the number of elements in the vector.
	Length() int
	// Get returns the element in the specified (0-based) position.
	Get(pos int) uint8
	// Set replaces the value at position pos with the given value.
	Set(pos int, val uint8)
}

// byteVector is the internal implementation of ByteVector.
type byteVector struct {
	id   uint64  // object identifer
	data []uint8 // byte vector data
}

// NewByteVector wraps the given data in a ByteVector type.
func NewByteVector(data []uint8) ByteVector {
	bv := new(byteVector)
	bv.id = newObjectId()
	bv.data = data
	return bv
}

func (b *byteVector) ObjectId() uint64 {
	if b == nil {
		return 0
	}
	return b.id
}

func (b *byteVector) Length() int {
	if b == nil {
		return 0
	}
	return len(b.data)
}

func (b *byteVector) Get(pos int) uint8 {
	if b == nil || pos < 0 || pos >= len(b.data) {
		return 0
	}
	return b.data[pos]
}

func (b *byteVector) Set(pos int, val uint8) {
	if b != nil && pos >= 0 && pos < len(b.data) {
		b.data[pos] = val
	}
}

// String converts the bytevector to a string representation of itself (e.g.
// "#u8(0 10 5)"). This is not to be confused with the procedures for
// converting the bytes in the vector to a UTF-8 string.
func (b *byteVector) String() string {
	if b == nil {
		return "#u8()"
	}
	buf := new(bytes.Buffer)
	buf.WriteString("#u8(")
	for _, v := range b.data {
		fmt.Fprintf(buf, "%v ", v)
	}
	// lop off the trailing space
	if buf.Len() > 2 {
		buf.Truncate(buf.Len() - 1)
	}
	buf.WriteString(")")
	return buf.String()
}
