//
// Copyright 2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

//
// Equivalence procedures
//

// Equaler implementations are capable of comparing themselves to another
// object, according to the equivalence rules defined in R7RS 6.1. In
// particular, the eqv?, eq?, and equal? procedures are implemented using this
// interface.
type Equaler interface {
	// Eqv compares this object to the other using the rules defined for the
	// eqv? procedure (R7RS 6.1). Type mismatches are regarded as being not
	// equal for the purpose of eqv?.
	Eqv(other interface{}) bool
}

// builtinEqv implements the eqv? and eq? procedures (because no additional
// distinctions are made since none are required).
func builtinEqv(name string, args []interface{}) (interface{}, LispError) {
	eq1, is_eq1 := args[0].(Equaler)
	eq2, is_eq2 := args[1].(Equaler)
	if is_eq1 && is_eq2 {
		return BooleanFromBool(eq1.Eqv(eq2)), nil
	}
	// otherwise must be the same object
	return BooleanFromBool(args[0] == args[1]), nil
}

// // equal? on numbers requires coercion
// num1, is_num1 := args[0].(Number)
// num2, is_num2 := args[1].(Number)
// if is_num1 && is_num2 {
// 	num1, num2 = coerceNumbers(num1, num2)
// 	eq, err := num1.EqualTo(num2)
// 	if err != nil {
// 		return nil, NewLispError(EARGUMENT, err.Error())
// 	}
// 	return BooleanFromBool(eq), nil
// }
