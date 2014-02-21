//
// Copyright 2013-2014 Nathan Fiedler. All rights reserved.
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
	"reflect"
)

// ByteVector represents the bytevector type in Scheme. It is a sequence of
// bytes whose values range from 0 to 255.
type ByteVector []uint8

// NewByteVector wraps the given data in a ByteVector type.
func NewByteVector(data []uint8) ByteVector {
	return ByteVector(data)
}

func (b ByteVector) ObjectId() uintptr {
	if b == nil {
		return 0
	}
	return reflect.ValueOf(b).Pointer()
}

func (b ByteVector) Len() int {
	if b == nil {
		return 0
	}
	return len(b)
}

func (b ByteVector) Get(pos int) uint8 {
	if b == nil || pos < 0 || pos >= len(b) {
		return 0
	}
	return b[pos]
}

func (b ByteVector) Set(pos int, val uint8) {
	if b != nil && pos >= 0 && pos < len(b) {
		b[pos] = val
	}
}

// String converts the bytevector to a string representation of itself (e.g.
// "#u8(0 10 5)"). This is not to be confused with the procedures for
// converting the bytes in the vector to a UTF-8 string.
func (b ByteVector) String() string {
	if b == nil {
		return "#u8()"
	}
	buf := new(bytes.Buffer)
	buf.WriteString("#u8(")
	for _, v := range b {
		fmt.Fprintf(buf, "%v ", v)
	}
	// lop off the trailing space
	if buf.Len() > 2 {
		buf.Truncate(buf.Len() - 1)
	}
	buf.WriteString(")")
	return buf.String()
}
