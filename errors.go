//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

import (
	"errors"
	"syscall"
)

// Error constants
const (
	_          = iota
	EOK        // no error
	ESYNTAX    // syntax error (e.g. unexpected close parenthesis)
	EVARUNDEF  // variable not defined
	EBADTYPE   // found wrong type of value (e.g. not a procedure when one was expected)
	EINVALNUM  // invalid numeric expression
	ENUMRANGE  // numeric value out of supported range
	ELEXER     // lexer tokenization failed
	ESUPPORT   // feature unsupported
	EARGUMENTS // illegal or wrong number of arguments
)

// OutOfBounds indicates an index into a string or vector is outside of the
// bounds of that object (e.g. negative or greater than the length).
var OutOfBounds = errors.New("liswat: index out of bounds")

// NameNotSymbol indicates that a (parameter) name was expected to be a Symbol
// but was in fact something else. This generally indicates as parser error.
var NameNotSymbol = errors.New("liswat: name was not a symbol")

// TODO: read http://golang.org/doc/go_faq.html#nil_error and change all this
// LispError is used to provide information on the type of error that
// occurred while parsing or evaluating the Lisp script. It implements
// the error interface.
type LispError struct {
	Errno   syscall.Errno
	Message string
}

// NewLispError creates a new LispError based on the given values.
func NewLispError(err int, msg string) *LispError {
	return &LispError{syscall.Errno(err), msg}
}

// String returns the string representation of the error.
func (e *LispError) String() string {
	return e.Message
}

// Error returns the string representation of the error.
func (e *LispError) Error() string {
	return e.Message
}
