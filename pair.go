//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

import (
	"bytes"
)

// Pair represents a pair of items, which themselves may be pairs. Pairs can
// be assembled to form arbitrary tree structures, or more commonly, linked
// lists.
type Pair interface {
	// First returns the car of the pair.
	First() interface{}
	// Second returns the first non-Pair thing in cdr.
	Second() interface{}
	// Third returns the second non-Pair thing in cdr.
	Third() interface{}
	// Rest returns the cdr of the Pair.
	Rest() interface{}
	// Len returns the number of things in the list composed of Pairs.
	Len() int
	// String returns the string representation of the pair.
	String() string
	// Reverse returns the chain of Pairs in reverse order.
	Reverse() Pair
	// Map calls function for each thing in the chained pairs.
	Map(f func(interface{}) interface{}) Pair
	// Append adds the given item to the pair, forming a list. The element
	// to which the new value was added is returned, allowing the caller
	// to chain one append operation onto the next to form a chain.
	Append(a interface{}) Pair
	// Join finds the first available slot in the chained Pairs
	// and attaches the given thing there.
	Join(a interface{})
	// setFirst replaces the car of the pair with a.
	setFirst(a interface{})
	// setRest replaces the cdr of the pair with a.
	setRest(a interface{})
}

// TODO: read http://golang.org/doc/articles/laws_of_reflection.html
// TODO: read http://golang.org/doc/go_faq.html#nil_error
// TODO: change use of nil and interface references as described in the articles
// pair is a simple implementation of a Pair.
type pair struct {
	first interface{} // the car of the pair
	rest  interface{} // the cdr of the pair
}

// NewPair returns an instance of Pair to hold the single element a.
func NewPair(a interface{}) Pair {
	return &pair{a, emptyList}
}

// NewList constructs a list of pairs from the given inputs.
func NewList(a ...interface{}) Pair {
	var head *pair = nil
	var prev *pair = nil
	for _, v := range a {
		next := &pair{v, emptyList}
		if head == nil {
			head = next
		} else {
			prev.rest = next
		}
		prev = next
	}
	return head
}

// Cons constructs a pair to hold item a and b such that they are stored
// in a single instance of Pair. This forms an improper list.
func Cons(a, b interface{}) Pair {
	return &pair{a, b}
}

// List constructs a proper list to hold a and b such that a and b are in
// distinct instances of Pair, with the empty list marking the end.
func List(a, b interface{}) Pair {
	return &pair{a, &pair{b, emptyList}}
}

// Car returns the first element in a list.
func Car(a interface{}) interface{} {
	if a == emptyList {
		return nil
	} else if p, ok := a.(Pair); ok {
		return p.First()
	} else {
		return nil
	}
	panic("unreachable code")
}

// Cdr returns the second element in a list.
func Cdr(a interface{}) interface{} {
	if a == emptyList {
		return nil
	} else if p, ok := a.(Pair); ok {
		if p.Rest() == emptyList {
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
		if r.Rest() == emptyList {
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
		if r.Rest() == emptyList {
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
		if p.rest == emptyList {
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
			if r1.Rest() == emptyList {
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
	var result *pair = nil
	var penultimate *pair = nil
	var pp Pair = p
	for p != nil {
		if result == nil {
			result = &pair{pp.First(), emptyList}
		} else {
			result = &pair{pp.First(), result}
			if penultimate == nil {
				penultimate = result
			}
		}
		if pp.Rest() == emptyList {
			p = nil
		} else if r, ok := pp.Rest().(Pair); ok {
			pp = r
		} else {
			result = &pair{pp.Rest(), result}
			p = nil
		}
	}
	// tighten up the end of the list
	if penultimate != nil {
		if r, ok := penultimate.rest.(Pair); ok {
			penultimate.rest = r.First()
		}
	} else if result != nil && result.rest != emptyList {
		// special case of a single Pair
		if r, ok := result.rest.(Pair); ok {
			result.rest = r.First()
		}
	}
	return result
}

// Len finds the length of the pair, which may be greater than two if
// the pair is part of a list of items.
func (p *pair) Len() int {
	length := 0
	var r Pair = p
	for p != nil {
		length++
		if r.Rest() == emptyList {
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

// Map calls function f on each element of the pair, returning
// a new pair constructed from the values returned by f().
func (p *pair) Map(f func(interface{}) interface{}) Pair {
	var head *pair = nil
	var prev *pair = nil
	var r Pair = p
	for p != nil {
		q := f(r.First())
		var s interface{} = emptyList
		if r.Rest() == emptyList {
			p = nil
		} else if rr, ok := r.Rest().(Pair); ok {
			r = rr
		} else {
			s = f(r.Rest())
			p = nil
		}
		next := &pair{q, s}
		if head == nil {
			head = next
		} else {
			prev.rest = next
		}
		prev = next
	}
	return head
}

// String returns the string form of the pair.
func (p *pair) String() string {
	buf := new(bytes.Buffer)
	buf.WriteString("(")
	var r Pair = p
	for p != nil {
		stringifyBuffer(r.First(), buf)
		if r.Rest() == emptyList {
			p = nil
		} else if rr, ok := r.Rest().(Pair); ok {
			buf.WriteString(" ")
			r = rr
		} else {
			buf.WriteString(" . ")
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
	return emptyList
}

// Map always returns the empty list without calling the function.
func (e EmptyList) Map(f func(interface{}) interface{}) Pair {
	return emptyList
}

// Append does nothing on an empty list. EmptyList is empty.
func (e EmptyList) Append(a interface{}) Pair {
	return emptyList
}

// Join does nothing on an empty list. EmptyList is empty.
func (e EmptyList) Join(a interface{}) {
}

// setFirst does nothing on an empty list. EmptyList is empty.
func (e EmptyList) setFirst(a interface{}) {
}

// setRest does nothing on an empty list. EmptyList is empty.
func (e EmptyList) setRest(a interface{}) {
}

// emptyList represents an empty list and may be used as a place holder
// in expressions that require a certain number of values.
var emptyList EmptyList = EmptyList(0)
