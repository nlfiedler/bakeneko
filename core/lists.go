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
func builtinCar(name string, args []interface{}) (interface{}, LispError) {
	if pair, ok := args[0].(Pair); ok {
		if pair.Len() == 0 {
			return nil, NewLispErrorf(EARGUMENT, "%s expects a non-empty pair", name)
		}
		return pair.First(), nil
	}
	return nil, NewLispErrorf(EARGUMENT, "%s expects a pair, not %v", name, args[0])
}

// builtinCaar implements the caar procedure.
func builtinCaar(name string, args []interface{}) (interface{}, LispError) {
	if pair, ok := args[0].(Pair); ok {
		if pair.Len() == 0 {
			return nil, NewLispErrorf(EARGUMENT, "%s expects a non-empty pair", name)
		}
		return Car(Car(pair)), nil
	}
	return nil, NewLispErrorf(EARGUMENT, "%s expects a pair, not %v", name, args[0])
}

// builtinCdr extracts the rest (cdr) of the given pair.
func builtinCdr(name string, args []interface{}) (interface{}, LispError) {
	if pair, ok := args[0].(Pair); ok {
		if pair.Len() == 0 {
			return nil, NewLispErrorf(EARGUMENT, "%s expects a non-empty pair", name)
		}
		return pair.Rest(), nil
	}
	return nil, NewLispErrorf(EARGUMENT, "%s expects a pair, not %v", name, args[0])
}

// builtinCadr implements the cadr procedure.
func builtinCadr(name string, args []interface{}) (interface{}, LispError) {
	if pair, ok := args[0].(Pair); ok {
		if pair.Len() == 0 {
			return nil, NewLispErrorf(EARGUMENT, "%s expects a non-empty pair", name)
		}
		return Car(Cdr(pair)), nil
	}
	return nil, NewLispErrorf(EARGUMENT, "%s expects a pair, not %v", name, args[0])
}

// builtinCdar implements the cdar procedure.
func builtinCdar(name string, args []interface{}) (interface{}, LispError) {
	if pair, ok := args[0].(Pair); ok {
		if pair.Len() == 0 {
			return nil, NewLispErrorf(EARGUMENT, "%s expects a non-empty pair", name)
		}
		return Cdr(Car(pair)), nil
	}
	return nil, NewLispErrorf(EARGUMENT, "%s expects a pair, not %v", name, args[0])
}

// builtinCddr implements the cddr procedure.
func builtinCddr(name string, args []interface{}) (interface{}, LispError) {
	if pair, ok := args[0].(Pair); ok {
		if pair.Len() == 0 {
			return nil, NewLispErrorf(EARGUMENT, "%s expects a non-empty pair", name)
		}
		return Cdr(Cdr(pair)), nil
	}
	return nil, NewLispErrorf(EARGUMENT, "%s expects a pair, not %v", name, args[0])
}

// builtinCxr implements the 24 procedures defined in the CxR library.
func builtinCxr(name string, args []interface{}) (interface{}, LispError) {
	if pair, ok := args[0].(Pair); ok {
		if pair.Len() == 0 {
			return nil, NewLispErrorf(EARGUMENT, "%s expects a non-empty pair", name)
		}
		return Cxr(name, pair), nil
	}
	return nil, NewLispErrorf(EARGUMENT, "%s expects a pair, not %v", name, args[0])
}

// builtinCons takes the first two arguments and returns a new Pair
// consisting of those arguments.
func builtinCons(name string, args []interface{}) (interface{}, LispError) {
	return Cons(args[0], args[1]), nil
}

var notAListMsg = "The object %v, passed as an argument to append, is not a list."

// builtinAppend builds a new Pair consisting of the arguments.
func builtinAppend(name string, args []interface{}) (interface{}, LispError) {
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
