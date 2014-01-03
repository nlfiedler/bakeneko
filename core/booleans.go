//
// Copyright 2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

//
// Set of built-in procedures for handling booleans.
//

// builtinNot returns true if the argument is false, otherwise false.
func builtinNot(name string, args []interface{}) (interface{}, LispError) {
	if b, ok := args[0].(Boolean); ok {
		return BooleanFromBool(!b.Value()), nil
	}
	return BooleanFalse, nil
}

// builtinIsBoolean returns true if the argument is a boolean object,
// otherwise false.
func builtinIsBoolean(name string, args []interface{}) (interface{}, LispError) {
	for _, arg := range args {
		_, ok := arg.(Boolean)
		if !ok {
			return BooleanFalse, nil
		}
	}
	return BooleanTrue, nil
}
