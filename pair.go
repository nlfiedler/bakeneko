//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

import (
	"bytes"
)

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

// emptyList represents an empty list and may be used as a place holder
// in expressions that require a certain number of values.
var emptyList EmptyList = EmptyList(0)

// Pair represents a pair of items, which themselves may be pairs.
type Pair struct {
	first interface{} // the car of the pair
	rest  interface{} // the cdr of the pair
}

// NewPair returns an instance of Pair to hold the single element a.
func NewPair(a interface{}) *Pair {
	return &Pair{a, emptyList}
}

// NewList constructs a list of pairs from the given inputs.
func NewList(a ...interface{}) *Pair {
	var head *Pair = nil
	var prev *Pair = nil
	for _, v := range a {
		next := Cons(v, emptyList)
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
// in a single instance of Pair.
func Cons(a, b interface{}) *Pair {
	return &Pair{a, b}
}

// List constructs a list to hold a and b such that a and b are in
// distinct instances of Pair.
func List(a, b interface{}) *Pair {
	return &Pair{a, &Pair{b, emptyList}}
}

// Car returns the first element in a list.
func Car(a interface{}) interface{} {
	if a == emptyList {
		return nil
	} else if p, ok := a.(*Pair); ok {
		return p.first
	} else {
		return nil
	}
	panic("unreachable code")
}

// Cdr returns the second element in a list.
func Cdr(a interface{}) interface{} {
	if a == emptyList {
		return nil
	} else if p, ok := a.(*Pair); ok {
		if p.rest == emptyList {
			return nil
		}
		return p.rest
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

// Append adds the given item to the pair, forming a list.
func (p *Pair) Append(a interface{}) {
	if p == nil {
		// cannot append to nil
		return
	} else if p.rest == emptyList {
		p.rest = Cons(a, p.rest)
	} else if r, ok := p.rest.(*Pair); ok {
		// find the end of the list and append there
		for r != nil {
			if r.rest == emptyList {
				r.rest = Cons(a, r.rest)
				break
			} else if rr, ok := r.rest.(*Pair); ok {
				r = rr
			} else {
				r.rest = Cons(r.rest, a)
				break
			}
		}
	} else {
		// make a new pair to hold rest and a
		p.rest = Cons(p.rest, a)
	}
}

// Join connects the given object to this pair, forming one list.
// Unlike Append(), this does not convert the object into a pair.
func (p *Pair) Join(a interface{}) {
	if p == nil {
		// cannot join to nil
		return
	} else if p.rest == emptyList {
		p.rest = a
	} else if r, ok := p.rest.(*Pair); ok {
		// find the end of the list and attach there
		for r != nil {
			if r.rest == emptyList {
				r.rest = a
				break
			} else if rr, ok := r.rest.(*Pair); ok {
				r = rr
			} else {
				r.rest = Cons(r.rest, a)
				break
			}
		}
	} else {
		// make a new pair to hold rest and a
		p.rest = Cons(p.rest, a)
	}
}

// First returns the first item in the pair.
func (p *Pair) First() interface{} {
	if p != nil {
		return p.first
	}
	return nil
}

// Rest returns the second item in the pair.
func (p *Pair) Rest() interface{} {
	if p != nil {
		return p.rest
	}
	return nil
}

// Second returns the second item in the list, or nil if there is no
// such item.
func (p *Pair) Second() interface{} {
	if p != nil {
		if p.rest == emptyList {
			return nil
		} else if r, ok := p.rest.(*Pair); ok {
			return r.first
		}
		return p.rest
	}
	return nil
}

// Third returns the third item in the list, or nil if there is no such
// item.
func (p *Pair) Third() interface{} {
	if p != nil {
		if r1, ok := p.rest.(*Pair); ok {
			if r1.rest == emptyList {
				return nil
			} else if r2, ok := r1.rest.(*Pair); ok {
				return r2.first
			}
			return r1.rest
		}
	}
	return nil
}

// Reverse returns a new list consisting of the elements in this list in
// reverse order.
func (p *Pair) Reverse() *Pair {
	var result *Pair = nil
	var penultimate *Pair = nil
	for p != nil {
		if result == nil {
			result = Cons(p.first, emptyList)
		} else {
			result = Cons(p.first, result)
			if penultimate == nil {
				penultimate = result
			}
		}
		if p.rest == emptyList {
			p = nil
		} else if r, ok := p.rest.(*Pair); ok {
			p = r
		} else {
			result = Cons(p.rest, result)
			p = nil
		}
	}
	// tighten up the end of the list
	if penultimate != nil {
		if r, ok := penultimate.rest.(*Pair); ok {
			penultimate.rest = r.first
		}
	} else if result != nil && result.rest != emptyList {
		// special case of a single Pair
		if r, ok := result.rest.(*Pair); ok {
			result.rest = r.first
		}
	}
	return result
}

// Len finds the length of the pair, which may be greater than two if
// the pair is part of a list of items.
func (p *Pair) Len() int {
	length := 0
	for p != nil {
		length++
		if p.rest == emptyList {
			p = nil
		} else if r, ok := p.rest.(*Pair); ok {
			p = r
		} else {
			length++
			p = nil
		}
	}
	return length
}

// Map calls function f on each element of the pair, returning
// a new pair constructed from the values returned by f().
func (p *Pair) Map(f func(interface{}) interface{}) *Pair {
	var head *Pair = nil
	var prev *Pair = nil
	for p != nil {
		q := f(p.first)
		var s interface{} = emptyList
		if p.rest == emptyList {
			p = nil
		} else if r, ok := p.rest.(*Pair); ok {
			p = r
		} else {
			s = f(p.rest)
			p = nil
		}
		next := Cons(q, s)
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
func (p *Pair) String() string {
	buf := new(bytes.Buffer)
	buf.WriteString("(")
	for p != nil {
		stringifyBuffer(p.first, buf)
		if p.rest == emptyList {
			p = nil
		} else if r, ok := p.rest.(*Pair); ok {
			buf.WriteString(" ")
			p = r
		} else {
			buf.WriteString(" . ")
			stringifyBuffer(p.rest, buf)
			p = nil
		}
	}
	buf.WriteString(")")
	return buf.String()
}
