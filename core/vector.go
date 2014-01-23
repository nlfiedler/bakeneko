//
// Copyright 2013-2014 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

//
// vector support for our Scheme-like language.
//

import (
	"bytes"
	"reflect"
)

// Vector represents the vector type in Scheme.
type Vector []interface{}

// NewVector wraps the given data in a vector type, whose object identifier
// will be generated.
func NewVector(data []interface{}) Vector {
	return Vector(data)
}

func (v Vector) ObjectId() uintptr {
	if v == nil {
		return 0
	}
	return reflect.ValueOf(v).Pointer()
}

func (v Vector) Length() int {
	if v == nil {
		return 0
	}
	return len(v)
}

func (v Vector) Get(pos int) interface{} {
	if v == nil || pos < 0 || pos >= len(v) {
		return nil
	}
	return v[pos]
}

func (v Vector) Set(pos int, val interface{}) {
	if v != nil && pos >= 0 && pos < len(v) {
		v[pos] = val
	}
}

func (v Vector) String() string {
	if v == nil {
		return "#()"
	}
	buf := new(bytes.Buffer)
	buf.WriteString("#(")
	for _, v := range v {
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
