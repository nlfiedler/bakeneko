//
// Copyright 2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

//
// Set of built-in procedures for control features.
//

// builtinApply calls the procedure with the remaining arguments in a list.
func builtinApply(name string, args []interface{}) (interface{}, LispError) {
	result, err := builtinAppend(name, args[1:])
	if err != nil {
		return nil, err
	}
	arg_list, ok := result.(Pair)
	if !ok {
		return nil, NewLispError(EINTERNAL, "append returned a non-Pair")
	}
	if proc, ok := args[0].(Procedure); ok {
		pargs := arg_list.ToSlice()
		return proc.Call(pargs)
	} else if clos, ok := args[0].(Closure); ok {
		env, err := clos.Bind(arg_list)
		if err != nil {
			return nil, err
		}
		return clos.Apply(env)
	}
	return nil, NewLispErrorf(EARGUMENT, "%s: %v is not a procedure", name, args[0])
}

// mapListsFunc is invoked once for each element in a list.
type mapListsFunc func(interface{}) (interface{}, LispError)

// mapSliceOfLists takes a slice of lists (chained Pairs) and invokes the
// given function for each element of each list. The results of each
// function invocation are added to a list, which is returned.
func mapSliceOfLists(name string, args []interface{}, f mapListsFunc) (Pair, LispError) {
	joinr := NewPairBuilder()
	for _, arg := range args {
		arg_list, ok := arg.(Pair)
		if !ok {
			return nil, NewLispErrorf(EARGUMENT, notAListMsg, arg, name)
		}
		iter := arg_list.Iterator()
		for iter.HasNext() {
			thing, err := f(iter.Next())
			if err != nil {
				return nil, err
			}
			joinr.Append(thing)
		}
	}
	return joinr.List(), nil
}

// builtinMap calls the procedure with each of the arguments in turn,
// returning a list of results.
func builtinMap(name string, args []interface{}) (interface{}, LispError) {
	var mapper mapListsFunc = nil
	if proc, ok := args[0].(Procedure); ok {
		mapper = func(elem interface{}) (interface{}, LispError) {
			slice := []interface{}{elem}
			return proc.Call(slice)
		}
	} else if clos, ok := args[0].(Closure); ok {
		mapper = func(elem interface{}) (interface{}, LispError) {
			env, err := clos.Bind(NewPair(elem))
			if err != nil {
				return nil, err
			}
			return clos.Apply(env)
		}
	} else {
		return nil, NewLispErrorf(EARGUMENT, "%s: %v is not a procedure", name, args[0])
	}
	return mapSliceOfLists(name, args[1:], mapper)
}
