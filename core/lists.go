//
// Copyright 2012-2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

//
// Set of built-in procedures for handling pairs and lists.
//

// builtinCar extracts the first element (car) of the given pair.
func builtinCar(args []interface{}) (interface{}, LispError) {
	if pair, ok := args[0].(Pair); ok {
		if pair.Len() == 0 {
			return nil, NewLispError(EARGUMENT, "car expects a non-empty pair")
		}
		return pair.First(), nil
	}
	return nil, NewLispErrorf(EARGUMENT, "car expects a pair, not %v", args[0])
}

// builtinCdr extracts the rest (cdr) of the given pair.
func builtinCdr(args []interface{}) (interface{}, LispError) {
	if pair, ok := args[0].(Pair); ok {
		if pair.Len() == 0 {
			return nil, NewLispError(EARGUMENT, "cdr expects a non-empty pair")
		}
		return pair.Rest(), nil
	}
	return nil, NewLispErrorf(EARGUMENT, "cdr expects a pair, not %v", args[0])
}

// builtinCons takes the first two arguments and returns a new Pair
// consisting of those arguments.
func builtinCons(args []interface{}) (interface{}, LispError) {
	return Cons(args[0], args[1]), nil
}

var notAListMsg = "The object %v, passed as an argument to append, is not a list."

// builtinAppend builds a new Pair consisting of the arguments.
func builtinAppend(args []interface{}) (interface{}, LispError) {
	var results Pair = theEmptyList
	var tail Pair = theEmptyList
	last := len(args) - 1
	for idx, arg := range args {
		if pair, ok := arg.(Pair); ok {
			iter := NewPairIterator(pair)
			for iter.HasNext() {
				elem := iter.Next()
				if results == theEmptyList {
					results = NewPair(elem)
					tail = results
				} else if iter.IsProper() {
					tail = tail.Append(elem)
				} else if idx == last {
					tail.Join(elem)
				} else {
					return nil, NewLispErrorf(EARGUMENT, notAListMsg, elem)
				}
				if !iter.IsProper() && idx < last {
					return nil, NewLispErrorf(EARGUMENT, notAListMsg, elem)
				}
			}
		} else if idx == last {
			if results == theEmptyList {
				return arg, nil
			} else {
				tail.Join(arg)
			}
		} else {
			return nil, NewLispErrorf(EARGUMENT, notAListMsg, arg)
		}
	}
	return results, nil
}
