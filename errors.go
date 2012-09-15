//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

import (
	"errors"
	"fmt"
)

var OutOfBounds = errors.New("liswat: index out of bounds")
var ParserError = errors.New("liswat: unexpected parser error")

// ErrorCode indicates the error status of the Scheme evaluation, with EOK
// representing no error.
type ErrorCode int

// Error constants
const (
	EOK       ErrorCode = iota // no error
	EARGUMENT                  // e.g. illegal, missing
	ECOMMAND                   // e.g. undefined, unsupported, unknown
	ESUPPORT                   // feature unsupported
	ESYNTAX                    // e.g. invalid number syntax
	ESYMBOL                    // e.g. undefined
)

// LispError is used to provide information on the type of error that occurred
// while parsing or evaluating the Lisp script. It implements the error
// interface.
type LispError interface {
	error
	// ErrorCode returns the error code associated with this result.
	ErrorCode() ErrorCode
	// ErrorMessage returns the error message associated with this result.
	ErrorMessage() string
	// Ok indicates if the result is a non-error, indicating that
	// the result is suitable for consumption.
	Ok() bool
	// String returns a human readable error message.
	String() string
}

// lispError is an implemention of the LispError interface.
type lispError struct {
	ecode  ErrorCode
	errmsg string
}

// NewLispError creates a new LispError based on the given values.
func NewLispError(err ErrorCode, msg string) LispError {
	return &lispError{err, msg}
}

// NewLispErrorf creates a new LispError, formatting the message according
// to the given format and optional arguments.
func NewLispErrorf(err ErrorCode, form string, args ...interface{}) LispError {
	detail := fmt.Sprintf(form, args...)
	return NewLispError(err, detail)
}

// Returns the error portion of this result in string form.
func (e *lispError) Error() string {
	return e.String()
}

// Error returns the error code, or EOK if undefined.
func (e *lispError) ErrorCode() ErrorCode {
	if e != nil {
		return e.ecode
	}
	return EOK
}

// ErrorMessage returns the error message, if any.
func (e *lispError) ErrorMessage() string {
	if e != nil {
		return e.errmsg
	}
	return ""
}

// Ok returns true if the error code is EOK, false otherwise.
func (e *lispError) Ok() bool {
	return e == nil || e.ecode == EOK
}

// String returns a human readable form of the result.
func (e *lispError) String() string {
	if e == nil || e.Ok() {
		return "(no error)"
	}
	// Would be nice to print code as text
	return fmt.Sprintf("ERR-%04d: %s", int(e.ecode), e.errmsg)
}
