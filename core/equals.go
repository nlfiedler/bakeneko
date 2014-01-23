//
// Copyright 2013-2014 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

//
// Equivalence procedures
//

// Indentifiable objects implement this interface to provide their unique
// object identifier. While objects may mutate, their identifier should
// remain the same. The primary purpose of the identifier is to avoid
// infinite loops when traversing nested, possibly self-referencing,
// structures.
type Identifiable interface {
	// ObjectId returns the unique identifier for this object.
	ObjectId() uintptr
}

// builtinEqv implements the eqv? and eq? procedures (because no additional
// distinctions are made since none are required).
func builtinEqv(name string, args []interface{}) (interface{}, LispError) {
	vec1, is_vec1 := args[0].(Vector)
	vec2, is_vec2 := args[1].(Vector)
	if is_vec1 && is_vec2 {
		return BooleanFromBool(vec1.Length() == 0 && vec2.Length() == 0), nil
	}
	bv1, is_bv1 := args[0].(ByteVector)
	bv2, is_bv2 := args[1].(ByteVector)
	if is_bv1 && is_bv2 {
		return BooleanFromBool(bv1.Length() == 0 && bv2.Length() == 0), nil
	}
	pair1, is_pair1 := args[0].(Pair)
	pair2, is_pair2 := args[1].(Pair)
	if is_pair1 && is_pair2 {
		return BooleanFromBool(pair1.Len() == 0 && pair2.Len() == 0), nil
	}
	atom1, is_atom1 := args[0].(Atom)
	atom2, is_atom2 := args[1].(Atom)
	if is_atom1 && is_atom2 {
		eq, err := atom1.EqualTo(atom2)
		if err != nil {
			return BooleanFalse, nil
		}
		return BooleanFromBool(eq), nil
	}
	// assume they are not equal in any way
	return BooleanFalse, nil
}

// compareByteVectors compares the contents of two byte vectors for equality,
// returning true if their bytes are identical, false otherwise.
func compareByteVectors(bv1, bv2 ByteVector) bool {
	// byte vectors cannot have nested structures, so comparing the length
	// is a valid shortcut
	if bv1.Length() != bv2.Length() {
		return false
	}
	id1 := bv1.ObjectId()
	id2 := bv2.ObjectId()
	if id1 != id2 {
		for ii := bv1.Length() - 1; ii >= 0; ii-- {
			if bv1.Get(ii) != bv2.Get(ii) {
				return false
			}
		}
	}
	return true
}

// sequenceComparator compares to sequence types, which includes lists,
// vectors, and byte vectors. A deep comparison is made, visiting all members
// of all nested sequences.
func sequenceComparator(thing1, thing2 interface{}) bool {

	// chibi-scheme's approach:
	// 1. Perform a fast comparison bounded by depth and iterations
	//    (native code, directly comparing the bytes of the structs);
	//    tail-call optimized, visiting last element in the sequence
	//    using 'goto' beginning of the equalp function
	// 2. If that returns #f, then done
	// 3. If that returns a positive number, return #t
	// 4. Else, in Scheme: memoize the results of comparing a thing to other things
	//    (map of maps: first map keyed by element 'a', second map keyed by
	//     element 'b'; it's value is the result (always #t since otherwise
	//     the elements did not match and the function would exit)),
	//    iterate through all lists and vectors without bound or depth.

	var compareThings func(thing1, thing2 interface{}) bool = nil
	compareVectors := func(vec1 Vector, vec2 Vector) bool {
		id1 := vec1.ObjectId()
		id2 := vec2.ObjectId()
		if id1 != id2 {
			var length int = 0
			if vec1.Length() > vec2.Length() {
				length = vec1.Length()
			} else {
				length = vec2.Length()
			}
			for ii := 0; ii < length; ii++ {
				if !compareThings(vec1.Get(ii), vec2.Get(ii)) {
					return false
				}
			}
		}
		// if we got this far, they are the same
		return true
	}
	comparePairs := func(pair1 Pair, pair2 Pair) bool {
		id1 := pair1.ObjectId()
		id2 := pair2.ObjectId()
		if id1 == id2 {
			return true
		}
		len1 := pair1.Len()
		len2 := pair2.Len()
		if len1 > len2 {
			iter1 := NewPairIterator(pair1)
			iter2 := NewPairIterator(pair2)
			for ii := 0; ii < len1; ii++ {
				p1 := iter1.Next()
				p2 := iter2.Next()
				if !compareThings(p1, p2) {
					return false
				}
			}
		}
		return true
	}
	compareThings = func(thing1, thing2 interface{}) bool {
		vec1, is_vec1 := thing1.(Vector)
		vec2, is_vec2 := thing2.(Vector)
		if is_vec1 && is_vec2 {
			return compareVectors(vec1, vec2)
		}
		bv1, is_bv1 := thing1.(ByteVector)
		bv2, is_bv2 := thing2.(ByteVector)
		if is_bv1 && is_bv2 {
			return compareByteVectors(bv1, bv2)
		}
		pair1, is_pair1 := thing1.(Pair)
		pair2, is_pair2 := thing2.(Pair)
		if is_pair1 && is_pair2 {
			return comparePairs(pair1, pair2)
		}
		atom1, is_atom1 := thing1.(Atom)
		atom2, is_atom2 := thing2.(Atom)
		if is_atom1 && is_atom2 {
			eq, err := atom1.EqualTo(atom2)
			if err != nil {
				return false
			}
			return eq
		}
		// assume they are not equal
		return false
	}
	return compareThings(thing1, thing2)
}

// builtinEqual implements the equal? procedure.
func builtinEqual(name string, args []interface{}) (interface{}, LispError) {
	vec1, is_vec1 := args[0].(Vector)
	vec2, is_vec2 := args[1].(Vector)
	if is_vec1 && is_vec2 {
		return BooleanFromBool(sequenceComparator(vec1, vec2)), nil
	}
	bv1, is_bv1 := args[0].(ByteVector)
	bv2, is_bv2 := args[1].(ByteVector)
	if is_bv1 && is_bv2 {
		return BooleanFromBool(compareByteVectors(bv1, bv2)), nil
	}
	pair1, is_pair1 := args[0].(Pair)
	pair2, is_pair2 := args[1].(Pair)
	if is_pair1 && is_pair2 {
		return BooleanFromBool(sequenceComparator(pair1, pair2)), nil
	}
	atom1, is_atom1 := args[0].(Atom)
	atom2, is_atom2 := args[1].(Atom)
	if is_atom1 && is_atom2 {
		eq, err := atom1.EqualTo(atom2)
		if err != nil {
			return BooleanFalse, nil
		}
		return BooleanFromBool(eq), nil
	}
	// assume they are not equal in any way
	return BooleanFalse, nil
}
