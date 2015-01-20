//
// Copyright 2012-2015 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	"bytes"
	"reflect"
)

// Pair represents a pair of items, which themselves may be pairs. Pairs can
// be assembled to form arbitrary tree structures, or more commonly, linked
// lists.
type Pair interface {
	Sequence
	// ObjectId returns the unique identifier for this object.
	ObjectId() uintptr
	// Reverse returns the chain of Pairs in reverse order.
	Reverse() Pair
	// Append adds the given item to the pair, forming a list. The element
	// to which the new value was added is returned, allowing the caller
	// to chain one append operation onto the next to form a chain.
	Append(a interface{}) Pair
	// Join finds the first available slot in the chained Pairs
	// and attaches the given thing there.
	Join(a interface{})
	// ToSlice constructs a Go slice containing the values in the list of pairs.
	ToSlice() []interface{}
	// setFirst replaces the car of the pair with a.
	setFirst(a interface{})
	// setRest replaces the cdr of the pair with a.
	setRest(a interface{})
}

// pair is a simple implementation of a Pair.
type pair struct {
	first interface{} // the car of the pair
	rest  interface{} // the cdr of the pair
}

// NewPair returns an instance of Pair to hold the single element a.
func NewPair(a interface{}) Pair {
	return Cons(a, theEmptyList)
}

// NewList constructs a list of pairs from the given inputs.
func NewList(a ...interface{}) Pair {
	var result Pair = theEmptyList
	for ii := len(a) - 1; ii >= 0; ii-- {
		result = Cons(a[ii], result)
	}
	return result
}

// SequenceToList converts a sequence to a list of Pairs.
func SequenceToList(seq Sequence) Pair {
	iter := seq.Iterator()
	builder := NewPairBuilder()
	for iter.HasNext() {
		builder.Append(iter.Next())
	}
	return builder.List()
}

// Cons constructs a pair to hold item a and b such that they are stored in a
// single instance of Pair. This may form an improper list if b is not already
// a proper list.
func Cons(a, b interface{}) Pair {
	return &pair{a, b}
}

// Car returns the first element in a sequence.
func Car(a interface{}) interface{} {
	if a == theEmptyList {
		return nil
	} else if p, ok := a.(Sequence); ok {
		return p.First()
	} else {
		return nil
	}
	panic("unreachable code")
}

// Cdr returns the second element in a sequence.
func Cdr(a interface{}) interface{} {
	if a == theEmptyList {
		return nil
	} else if p, ok := a.(Sequence); ok {
		if p.Rest() == theEmptyList {
			return nil
		}
		return p.Rest()
	} else {
		return nil
	}
	panic("unreachable code")
}

// Cxr implements all of the caar, cadr, ... cddddr procedures defined
// in Scheme, where name is the name of the procedure to process.
func Cxr(name string, a interface{}) interface{} {
	x := a
	for i := len(name) - 2; x != nil && i >= 1; i-- {
		if name[i] == 'a' {
			x = Car(x)
		} else if name[i] == 'd' {
			x = Cdr(x)
		} else {
			panic("unsupported function name: " + name)
		}
	}
	return x
}

// ObjectId returns the object identifer, or zero if nil.
func (p *pair) ObjectId() uintptr {
	if p != nil {
		return reflect.ValueOf(p).Pointer()
	}
	return 0
}

// setFirst stores the given item as the first item of the pair.
func (p *pair) setFirst(a interface{}) {
	if p != nil {
		p.first = a
	}
}

// setRest stores the given item as the second item of the pair.
func (p *pair) setRest(a interface{}) {
	if p != nil {
		p.rest = a
	}
}

// Append adds the given item to the pair, forming a list. The element to
// which the new value was added is returned, allowing the caller to chain one
// append operation onto the next to form a chain.
func (p *pair) Append(a interface{}) Pair {
	var r Pair = p
	// find the end of the list and append there
	for r != nil {
		// p as the empty list is handled by the EmptyList type
		if r.Rest() == theEmptyList {
			newr := Cons(a, r.Rest())
			r.setRest(newr)
			r = newr
			break
		} else if rr, ok := r.Rest().(Pair); ok {
			r = rr
		} else {
			newr := Cons(r.Rest(), a)
			r.setRest(newr)
			r = newr
			break
		}
	}
	return r
}

// Join connects the given object to this pair, forming one list.
// Unlike Append(), this does not convert the object into a pair.
// This may result in an improper list if a is not a proper list.
func (p *pair) Join(a interface{}) {
	var r Pair = p
	// find the end of the list and attach there
	for r != nil {
		if r.Rest() == theEmptyList {
			r.setRest(a)
			break
		} else if rr, ok := r.Rest().(Pair); ok {
			r = rr
		} else {
			r.setRest(Cons(r.Rest(), a))
			break
		}
	}
}

// First returns the first item in the pair.
func (p *pair) First() interface{} {
	if p != nil {
		return p.first
	}
	return nil
}

// Rest returns the second item in the pair.
func (p *pair) Rest() interface{} {
	if p != nil {
		return p.rest
	}
	return nil
}

// Second returns the second item in the list, or nil if there is no
// such item.
func (p *pair) Second() interface{} {
	if p != nil {
		if p.rest == theEmptyList {
			return nil
		} else if r, ok := p.rest.(Pair); ok {
			return r.First()
		}
		return p.rest
	}
	return nil
}

// Third returns the third item in the list, or nil if there is no such
// item.
func (p *pair) Third() interface{} {
	if p != nil {
		if r1, ok := p.rest.(Pair); ok {
			if r1.Rest() == theEmptyList {
				return nil
			} else if r2, ok := r1.Rest().(Pair); ok {
				return r2.First()
			}
			return r1.Rest()
		}
	}
	return nil
}

// Reverse returns a new list consisting of the elements in this list in
// reverse order.
func (p *pair) Reverse() Pair {
	if p == nil {
		return nil
	}
	var result Pair = theEmptyList
	var penultimate Pair = theEmptyList
	var first interface{} = nil
	iter := p.Iterator()
	for iter.HasNext() {
		elem := iter.Next()
		result = Cons(elem, result)
		if first == nil {
			first = elem
		} else if penultimate == theEmptyList {
			penultimate = result
		}
	}
	if !iter.IsProper() && penultimate != theEmptyList {
		// set an improper list
		penultimate.setRest(first)
	}
	return result
}

// Len finds the length of the pair, which may be greater than two if
// the pair is part of a list of items.
func (p *pair) Len() int {
	length := 0
	pairs_seen := make(map[uintptr]bool)
	var r Pair = p
	for p != nil {
		length++
		_, seen := pairs_seen[r.ObjectId()]
		if seen {
			// must be a loop in the pairs, terminate now
			break
		}
		pairs_seen[r.ObjectId()] = true
		if r.Rest() == theEmptyList {
			p = nil
		} else if rr, ok := r.Rest().(Pair); ok {
			r = rr
		} else {
			length++
			p = nil
		}
	}
	return length
}

// Map calls function f on each element of the pair, returning a new pair
// constructed from the values returned by f().
func (p *pair) Map(funk func(interface{}) interface{}) Sequence {
	joiner := NewPairBuilder()
	iter := p.Iterator()
	for iter.HasNext() {
		joiner.Append(funk(iter.Next()))
	}
	return joiner.List()
}

// ToSlice constructs a Go slice containing the values in the list of pairs.
func (p *pair) ToSlice() []interface{} {
	args := make([]interface{}, 0)
	var r Pair = p
	for p != nil {
		args = append(args, r.First())
		if r.Rest() == theEmptyList {
			p = nil
		} else if rr, ok := r.Rest().(Pair); ok {
			r = rr
		} else {
			args = append(args, r.Rest())
			p = nil
		}
	}
	return args
}

func (p *pair) Iterator() Iterator {
	return &PairIterator{p, true}
}

// String returns the string form of the pair.
func (p *pair) String() string {
	buf := new(bytes.Buffer)
	buf.WriteString("(")
	var r Pair = p
	for p != nil {
		written := stringifyBuffer(r.First(), buf)
		if r.Rest() == theEmptyList {
			p = nil
		} else if rr, ok := r.Rest().(Pair); ok {
			if written > 0 {
				buf.WriteString(" ")
			}
			r = rr
		} else {
			if written > 0 {
				buf.WriteString(" . ")
			} else {
				buf.WriteString(". ")
			}
			stringifyBuffer(r.Rest(), buf)
			p = nil
		}
	}
	buf.WriteString(")")
	return buf.String()
}

// EmptyList is a type unto itself, representing the "empty" value in
// Scheme, as well as marking the end of lists.
type EmptyList int

// Len returns the length of the empty list, which is always zero.
func (e EmptyList) Len() int {
	return 0
}

// String returns the string representation of the empty list, which is
// always "()".
func (e EmptyList) String() string {
	return "()"
}

// ObjectId of empty list is always zero.
func (e EmptyList) ObjectId() uintptr {
	return 0
}

// First always returns nil.
func (e EmptyList) First() interface{} {
	return nil
}

// Rest always returns nil.
func (e EmptyList) Rest() interface{} {
	return nil
}

// Second always returns nil.
func (e EmptyList) Second() interface{} {
	return nil
}

// Third always returns nil.
func (e EmptyList) Third() interface{} {
	return nil
}

// Reverse always returns the empty list.
func (e EmptyList) Reverse() Pair {
	return theEmptyList
}

// Map always returns the empty list without calling the function.
func (e EmptyList) Map(f func(interface{}) interface{}) Sequence {
	return theEmptyList
}

// Append on the empty list returns a new list whose cdr is the
// empty list, which allows additional append operations to work
// as expected.
func (e EmptyList) Append(a interface{}) Pair {
	return Cons(a, theEmptyList)
}

// Join does nothing on an empty list. EmptyList is empty.
func (e EmptyList) Join(a interface{}) {
}

// ToSlice on the empty list always returns nil.
func (e EmptyList) ToSlice() []interface{} {
	return nil
}

func (e EmptyList) Iterator() Iterator {
	return &PairIterator{nil, true}
}

// setFirst does nothing on an empty list. EmptyList is empty.
func (e EmptyList) setFirst(a interface{}) {
}

// setRest does nothing on an empty list. EmptyList is empty.
func (e EmptyList) setRest(a interface{}) {
}

// theEmptyList represents an empty list and may be used as a place holder
// in expressions that require a certain number of values.
var theEmptyList EmptyList = EmptyList(0)

// PairIterator iterates over a chained Pair, whether proper or not.
type PairIterator struct {
	curr   Pair // curr is the Pair currently under consideration
	proper bool // if true, list was proper (e.g. (a b c))
}

// HasNext indicates if there is another value in the Pair chain.
func (i *PairIterator) HasNext() bool {
	return i.curr != nil
}

// IsProper indicates that the list was a proper list (e.g. (a b c)) versus an
// improper list (e.g. (a b . c). This function will always return true until
// the end of an improper list has been reached.
func (i *PairIterator) IsProper() bool {
	return i.curr != nil || i.proper
}

// Next returns the next value in the Pair chain, returning nil if the end of
// the list has been reached.
func (i *PairIterator) Next() interface{} {
	var result interface{} = nil
	if i.curr != nil {
		if i.proper {
			result = i.curr.First()
			rest := i.curr.Rest()
			if rest == theEmptyList {
				i.curr = nil
			} else if rp, ok := rest.(Pair); ok {
				i.curr = rp
			} else {
				i.proper = false
			}
		} else {
			result = i.curr.Rest()
			i.curr = nil
		}
	}
	return result
}

// PairBuilder permits easily building a list by appending objects to the
// joiner, in order, and then requesting the completed list when finished.
type PairBuilder struct {
	head Pair // points to the beginning of the list
	tail Pair // the last element, used for efficiently appending
}

// NewPairBuilder constructs an empty PairBuilder for building a list.
func NewPairBuilder() *PairBuilder {
	return &PairBuilder{theEmptyList, theEmptyList}
}

// Append adds the given element to the end of the list managed by this
// PairBuilder instance.
func (pj *PairBuilder) Append(elem interface{}) *PairBuilder {
	if pj.head == theEmptyList {
		pj.head = NewPair(elem)
		pj.tail = pj.head
	} else {
		pj.tail = pj.tail.Append(elem)
	}
	return pj
}

// Join adds the given element to the end of the list managed by this
// PairBuilder instance in an improper fashion, forming an improper list.
func (pj *PairBuilder) Join(elem interface{}) *PairBuilder {
	if pj.head == theEmptyList {
		pj.head = NewPair(elem)
		pj.tail = pj.head
	} else {
		pj.tail.Join(elem)
	}
	return pj
}

// List returns the head of the list constructed by this PairBuilder.
func (pj *PairBuilder) List() Pair {
	return pj.head
}
