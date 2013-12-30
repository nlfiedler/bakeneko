//
// Copyright 2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

//
// Symbol-related procedures
//

// symbolPredicate tests if the given object is a symbol, returning true if so
// and false otherwise.
func symbolPredicate(name string, args []interface{}) (interface{}, LispError) {
	_, ok := args[0].(Symbol)
	return BooleanFromBool(ok), nil
}

var notASymbolMsg string = "%v requires symbol arguments"

// symbolEqual returns true if all of the arguments are symbols and are equal
// in the sense of string=?.
func symbolEqual(name string, args []interface{}) (interface{}, LispError) {
	prev, is_num := args[0].(Symbol)
	if !is_num {
		return nil, NewLispErrorf(EARGUMENT, notASymbolMsg, "symbol=?")
	}
	for idx := 1; idx < len(args); idx++ {
		curr, is_num := args[idx].(Symbol)
		if !is_num {
			return nil, NewLispErrorf(EARGUMENT, notASymbolMsg, "symbol=?")
		}
		eq, err := prev.EqualTo(curr)
		if err != nil {
			return nil, NewLispError(EARGUMENT, err.Error())
		}
		if !eq {
			return BooleanFalse, nil
		}
		prev = curr
	}
	return BooleanTrue, nil
}

// symbolToString returns the symbol name as a string.
func symbolToString(name string, args []interface{}) (interface{}, LispError) {
	sym, ok := args[0].(Symbol)
	if !ok {
		return nil, NewLispErrorf(EARGUMENT, notASymbolMsg, "symbol->string")
	}
	return NewImmutableString(sym.String()), nil
}

// symbolFromString returns the string as a symbol.
func symbolFromString(name string, args []interface{}) (interface{}, LispError) {
	str, ok := args[0].(String)
	if !ok {
		return nil, NewLispError(EARGUMENT, "string->symbol requires string argument")
	}
	return NewSymbol(str.Value()), nil
}
