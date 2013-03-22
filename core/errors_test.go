//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

import (
	"testing"
)

func TestLispErrorNil(t *testing.T) {
	var r *lispError = nil
	if r.ErrorCode() != EOK {
		t.Error("nil LispError.ErrorCode() should return EBADSTATE")
	}
	if r.ErrorMessage() != "" {
		t.Error("nil LispError.ErrorMessage() should return ''")
	}
	if !r.Ok() {
		t.Error("nil LispError.Ok() should return false")
	}
	if r.String() != "(no error)" {
		t.Error("nil LispError.String() should return '(no error)'")
	}
}

func TestLispErrorOk(t *testing.T) {
	r := NewLispError(EOK, "foo")
	if r.ErrorCode() != EOK {
		t.Error("EOK LispError.Error() should return EOK")
	}
	if r.ErrorMessage() != "foo" {
		t.Error("EOK LispError.ErrorMessage() should return ''")
	}
	if !r.Ok() {
		t.Error("EOK LispError.Ok() should return true")
	}
}

func TestLispError(t *testing.T) {
	r := NewLispError(EARGUMENT, "illegal argument")
	if r.ErrorCode() != EARGUMENT {
		t.Error("EARGUMENT LispError.Error() should return EARGUMENT")
	}
	if r.ErrorMessage() != "illegal argument" {
		t.Error("EARGUMENT LispError.ErrorMessage() should return ''")
	}
	if r.Ok() {
		t.Error("EARGUMENT LispError.Ok() should return false")
	}
	if r.String() != "ERR-0001: illegal argument" {
		t.Error("EARGUMENT LispError.String() not formatted correctly")
	}
}
