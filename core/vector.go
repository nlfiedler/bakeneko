//
// Copyright 2013-2014 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

//
// vector data types and procedures
//

import (
	"bytes"
	"fmt"
	"reflect"
)

// Sequence represents any collection of elements that can be visited in a
// sequential order. This includes (chained) pairs and vectors.
type Sequence interface {
	fmt.Stringer
	// First returns the first element of the sequence.
	First() interface{}
	// Second returns the first non-Sequence thing in the Rest() sequence.
	Second() interface{}
	// Third returns the second non-Sequence thing in the Rest() sequence.
	Third() interface{}
	// Rest returns the portion of the sequence following the first element.
	Rest() interface{}
	// Len returns the number of things in the sequence.
	Len() int
	// Iterator returns an iterator the sequence.
	Iterator() Iterator
	// Map calls the function for each thing in the sequence, returning
	// a new sequence containing the results.
	Map(f func(interface{}) interface{}) Sequence
}

// Iterator provides a mechanism for efficiently and easily visiting all of
// the elements in a sequence.
type Iterator interface {
	// HasNext indicates if there is another value in the sequence.
	HasNext() bool
	// IsProper indicates that the sequence was a proper list (e.g. (a b
	// c)) versus an improper list (e.g. (a b . c). This function will
	// always return true until the end of an improper list has been
	// reached. Vectors are always proper.
	IsProper() bool
	// Next returns the next value in the sequence, returning nil if the
	// end of the sequence has been reached.
	Next() interface{}
}

// SequenceAppend adds the given element to the end of the sequence,
// possibly returning a new sequence.
func SequenceAppend(seq interface{}, elem interface{}) Sequence {
	switch s := seq.(type) {
	case Pair:
		s.Append(elem)
		return s
	case Vector:
		return s.Append(elem)
	default:
		// just use Pair in all other cases
		return NewList(seq, elem)
	}
}

// Vector represents the vector type in Scheme.
type Vector []interface{}

// NewVector wraps the given data in a vector type, whose object identifier
// will be generated.
func NewVector(data []interface{}) Vector {
	return Vector(data)
}

// NewVectorFromValues is the variadic form of NewVector, receiving any
// number of arguments and building a Vector from them.
func NewVectorFromValues(a ...interface{}) Vector {
	return NewVector(a)
}

func (v Vector) ObjectId() uintptr {
	if v == nil {
		return 0
	}
	return reflect.ValueOf(v).Pointer()
}

func (v Vector) Len() int {
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

// Append adds the given element to the end of the vector, returning a new
// instance of Vector.
func (v Vector) Append(elem interface{}) Vector {
	r := append(v, elem)
	return Vector(r)
}

func (v Vector) First() interface{} {
	return v.Get(0)
}

func (v Vector) Second() interface{} {
	return v.Get(1)
}

func (v Vector) Third() interface{} {
	return v.Get(2)
}

func (v Vector) Rest() interface{} {
	return NewVector(v[1:])
}

func (v Vector) Iterator() Iterator {
	return &vectorIterator{v, 0}
}

func (v Vector) Map(f func(interface{}) interface{}) Sequence {
	results := make([]interface{}, 0)
	for _, e := range v {
		results = append(results, f(e))
	}
	return Vector(results)
}

func (v Vector) String() string {
	if v == nil {
		return "#()"
	}
	buf := new(bytes.Buffer)
	buf.WriteString("#(")
	space := false
	for _, e := range v {
		written := stringifyBuffer(e, buf)
		if written > 0 {
			buf.WriteString(" ")
			space = true
		} else {
			space = false
		}
	}
	// lop off the trailing space
	if space {
		buf.Truncate(buf.Len() - 1)
	}
	buf.WriteString(")")
	return buf.String()
}

type vectorIterator struct {
	v Vector
	p int
}

func (vi *vectorIterator) HasNext() bool {
	return vi.v.Len() > vi.p
}

func (vi *vectorIterator) IsProper() bool {
	// vectors are always proper
	return true
}

func (vi *vectorIterator) Next() (val interface{}) {
	val = vi.v.Get(vi.p)
	vi.p++
	return
}

// listToVector implements the list->vector procedure.
func listToVector(name string, args []interface{}) (interface{}, LispError) {
	if pair, ok := args[0].(Pair); ok {
		result := make([]interface{}, 0)
		iter := pair.Iterator()
		for iter.HasNext() {
			result = append(result, iter.Next())
		}
		return NewVector(result), nil
	}
	return nil, NewLispErrorf(EARGUMENT, "%s expects a list, not %v", name, args[0])
}

// extractNumericArg checks if the given argument is a number and returns
// it as a Number. Otherwise it returns an error.
func extractNumericArg(name string, arg interface{}) (Number, LispError) {
	if num, ok := arg.(Number); ok {
		return num, nil
	}
	return nil, NewLispErrorf(EARGUMENT, "%s expects a number, not %v", name, arg)
}

// vectorToList implements the vector->list procedure.
func vectorToList(name string, args []interface{}) (interface{}, LispError) {
	if vec, ok := args[0].(Vector); ok {
		start := 0
		end := vec.Len()
		if len(args) >= 2 {
			num, err := extractNumericArg(name, args[1])
			if err != nil {
				return nil, err
			}
			requested := int(num.IntegerValue().ToInteger())
			if requested < 0 || requested > end {
				return nil, NewLispErrorf(EARGUMENT, "%s index out of bounds %v", name, args[1])
			}
			start = requested
		}
		if len(args) == 3 {
			num, err := extractNumericArg(name, args[2])
			if err != nil {
				return nil, err
			}
			requested := int(num.IntegerValue().ToInteger())
			if requested < start || requested > end {
				return nil, NewLispErrorf(EARGUMENT, "%s index out of bounds %v", name, args[2])
			}
			end = requested
		}
		builder := NewPairBuilder()
		for pos := start; pos < end; pos++ {
			builder.Append(vec.Get(pos))
		}
		return builder.List(), nil
	}
	return nil, NewLispErrorf(EARGUMENT, "%s expects a vector, not %v", name, args[0])
}
