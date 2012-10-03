//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

//
// Set of built-in procedures for handling pairs and lists.
//

// builtinCons takes the first two arguments and returns a new Pair
// consisting of those arguments.
func builtinCons(args []interface{}) (interface{}, LispError) {
	if len(args) != 2 {
		return nil, NewLispErrorf(EARGUMENT,
			"cons called with %d argument(s), requires two arguments", len(args))
	}
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
