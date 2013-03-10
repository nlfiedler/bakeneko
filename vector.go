//
// Copyright 2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

//
// vector support for our Scheme-like language.
//

import (
	"bytes"
)

// Vector represents the vector type in Scheme.
type Vector []interface{}

// NewVector wraps the given data in a Vector type.
func NewVector(data []interface{}) Vector {
	return Vector(data)
}

// Length returns the number of elements in the vector.
func (b Vector) Length() int {
	return len(b)
}

// Get returns the element in the specified (0-based) position within the
// vector.
func (b Vector) Get(pos int) interface{} {
	return b[pos]
}

// Set replaces the value at position pos with the given value in the vector.
func (b Vector) Set(pos int, val interface{}) {
	b[pos] = val
}

// String converts the vector to a string representation of itself (e.g. "(a
// 10 #\c)").
func (b Vector) String() string {
	buf := new(bytes.Buffer)
	buf.WriteString("#(")
	for _, v := range b {
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

// TODO: implement procedures for vector
