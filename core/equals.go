//
// Copyright 2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	"hash/fnv"
	"math/rand"
	"time"
)

//
// Equivalence procedures
//

// Indentifiable objects implement this interface to provide their unique
// object identifier.
type Identifiable interface {
	// ObjectId returns the unique identifier for this object.
	ObjectId() uint64
}

// newObjectId produces a unique unsigned 64-bit identifier to be used in
// uniquely identifying objects created by the interpreter. Typically this is
// used in pairs, vectors, and byte vectors in order to detect loops.
func newObjectId() uint64 {
	// On a 2.4 GHz Intel Core 2 Duo this produces 1 million values in less
	// than a second, so should be fast enough for our needs.
	hasher := fnv.New64()
	// Nanosecond time and a random number should provide unique values.
	now_bytes, _ := time.Now().MarshalBinary()
	hasher.Write(now_bytes)
	rand_int := rand.Int63()
	rand_buf := make([]byte, 8, 8)
	rand_buf[0] = byte(rand_int >> 56)
	rand_buf[1] = byte(rand_int >> 48)
	rand_buf[2] = byte(rand_int >> 40)
	rand_buf[3] = byte(rand_int >> 32)
	rand_buf[4] = byte(rand_int >> 24)
	rand_buf[5] = byte(rand_int >> 16)
	rand_buf[6] = byte(rand_int >> 8)
	rand_buf[7] = byte(rand_int)
	hasher.Write(rand_buf)
	return hasher.Sum64()
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
	if bv1.Length() != bv2.Length() {
		return false
	}
	for ii := bv1.Length() - 1; ii >= 0; ii-- {
		if bv1.Get(ii) != bv2.Get(ii) {
			return false
		}
	}
	return true
}

// sequenceComparator compares to sequence types, which includes lists,
// vectors, and byte vectors. A deep comparison is made, visiting all members
// of all nested sequences.
func sequenceComparator(thing1, thing2 interface{}) bool {
	// seqs_seen := make(map[uint64]bool)
	var compareThings func(thing1, thing2 interface{}) bool = nil
	compareVectors := func(vec1 Vector, vec2 Vector) bool {
		if vec1.Length() != vec2.Length() {
			// TODO: what of data labels and infinite and equal sequences
			// (equal? ’#1=(a b . #1#) ’#2=(a b a b . #2#)) => #t
			return false
		}
		id1 := vec1.ObjectId()
		id2 := vec2.ObjectId()
		if id1 == id2 {
			return true
		}
		// _, seen1 := seqs_seen[id1]
		// _, seen2 := seqs_seen[id2]
		// if seen1 && seen2 {
		// 	// TODO: this is not really correct, seeing is not the same as equality and vice versa
		// 	return true
		// }
		// seqs_seen[id1] = true
		// seqs_seen[id2] = true
		for ii := vec1.Length() - 1; ii >= 0; ii-- {
			if !compareThings(vec1.Get(ii), vec2.Get(ii)) {
				return false
			}
		}
		// if we got this far, they are the same
		return true
	}
	// comparePairs := func(pair1 Pair, pair2 Pair) bool {
	// 	id1 := pair1.ObjectId()
	// 	id2 := pair2.ObjectId()
	// 	if id1 == id2 {
	// 		return true
	// 	}
	// 	len1 := pair1.Len()
	// 	len2 := pair2.Len()
	// 	if len1 > len2 {
	// TODO: use the longest length and iterate that many times; infinite lists should continue infinitely
	// 		iter1 := NewPairIterator(pair1)
	// 		iter2 := NewPairIterator(pair2)
	// 		for ii := 0; ii < len1; ii++ {
	// 			p1 := iter1.Next()
	// 			p2 := iter2.Next()
	// 			if !compareThings(p1, p2) {
	// 				return false
	// 			}
	// 		}
	// 		// } else if len1 < len2 {
	// 	}
	// 	return true
	// }
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
		// pair1, is_pair1 := thing1.(Pair)
		// pair2, is_pair2 := thing2.(Pair)
		// if is_pair1 && is_pair2 {
		// 	return comparePairs(pair1, pair2)
		// }
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
