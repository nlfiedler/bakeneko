//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	"errors"
	"fmt"
)

var NumberOutOfRange = errors.New("number value of out range")
var InvalidUtf8String = errors.New("input contains invalid UTF-8 bytes")
var OutOfBounds = errors.New("index out of bounds")
var ParserError = errors.New("unexpected parser error")
var TypeMismatch = errors.New("type mismatch in atom comparison")

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
	ELEXER                     // lexer error
	EIO                        // I/O error
)

// Locatable is any Scheme element whose location within the parsed text is
// known, and is defined by the (1-based) row and column of the input text.
type Locatable interface {
	// Location returns the row and column (1-based) of the element.
	Location() (int, int)
}

// LispError is used to provide information on the type of error that occurred
// while parsing or evaluating the Lisp script. It implements the error
// interface.
type LispError interface {
	error
	Locatable
	// ErrorCode returns the error code associated with this result.
	ErrorCode() ErrorCode
	// ErrorMessage returns the error message associated with this result.
	ErrorMessage() string
	// Ok indicates if the result is a non-error, indicating that
	// the result is suitable for consumption.
	Ok() bool
	// String returns a human readable error message.
	String() string
	// SetLocation may be used to specify where in the text the error was found.
	SetLocation(row, col int)
}

// lispError is an implemention of the LispError interface.
type lispError struct {
	ecode  ErrorCode
	errmsg string
	row    int
	col    int
}

// NewLispError creates a new LispError based on the given values.
func NewLispError(err ErrorCode, msg string) LispError {
	return &lispError{err, msg, -1, -1}
}

// NewLispErrorf creates a new LispError, formatting the message according
// to the given format and optional arguments.
func NewLispErrorf(err ErrorCode, form string, args ...interface{}) LispError {
	detail := fmt.Sprintf(form, args...)
	return NewLispError(err, detail)
}

// NewLispErrorl returns a LispError of the given type, for the selected
// element, with the clarifying message. If the element has location
// information, it will be incorporated into the error message.
func NewLispErrorl(err ErrorCode, elem interface{}, msg string) LispError {
	str := stringify(elem)
	result := NewLispError(err, msg+": "+str)
	if le, ok := elem.(Locatable); ok {
		result.SetLocation(le.Location())
	}
	return result
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

// Location returns the location of the error within the parsed text.
// The values will be -1 if undefined.
func (e *lispError) Location() (int, int) {
	return e.row, e.col
}

// SetLocation sets the location information for this error.
func (e *lispError) SetLocation(row, col int) {
	e.row = row
	e.col = col
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
