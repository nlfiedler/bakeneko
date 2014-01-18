//
// Copyright 2014 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	gc "launchpad.net/gocheck"
)

type VirtMachSuite struct {
}

var _ = gc.Suite(&VirtMachSuite{})

func (ps *VirtMachSuite) TestConstant(c *gc.C) {
	expr := NewInteger(123)
	name := "TestConstant"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	env := theReportEnvironment
	result, err := EvaluateCode(code, env)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to evaluate code: %s", err))
	c.Assert(result, gc.NotNil, gc.Commentf("failed to produce result"))
	c.Check(result, gc.FitsTypeOf, expr)
	ri := result.(Integer)
	c.Check(ri.ToInteger(), gc.Equals, int64(123))
}

func (vms *VirtMachSuite) TestSymbol(c *gc.C) {
	expr := NewSymbol("foo")
	name := "TestSymbol"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	env := NewEnvironment(theReportEnvironment)
	val := NewInteger(101)
	env.Define(expr, val)
	result, err := EvaluateCode(code, env)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to evaluate code: %s", err))
	c.Assert(result, gc.NotNil)
	c.Check(result, gc.FitsTypeOf, val)
	ri := result.(Integer)
	c.Check(ri.ToInteger(), gc.Equals, int64(101))
}

func (vms *VirtMachSuite) TestSymbolUndef(c *gc.C) {
	expr := NewSymbol("foo")
	name := "TestSymbol"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	env := theReportEnvironment
	result, err := EvaluateCode(code, env)
	c.Assert(result, gc.IsNil, gc.Commentf("result should be nil when error"))
	c.Assert(err, gc.NotNil, gc.Commentf("failure expected for %q", expr))
	c.Check(err.ErrorCode(), gc.Equals, EARGUMENT)
	c.Check(err.ErrorMessage(), gc.Matches, ".*unbound variable: foo")
}

func (vms *VirtMachSuite) TestSetVar(c *gc.C) {
	set := NewSymbol("set!")
	foo := NewSymbol("foo")
	val := NewInteger(1973)
	expr := NewList(set, foo, val)
	name := "TestSetVar"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	env := NewEnvironment(theReportEnvironment)
	env.Define(foo, NewInteger(101))
	result, err := EvaluateCode(code, env)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to evaluate code: %s", err))
	c.Assert(result, gc.IsNil, gc.Commentf("set should not yield result"))
	result = env.Find(foo)
	c.Check(result, gc.FitsTypeOf, val)
	ri := result.(Integer)
	c.Check(ri.ToInteger(), gc.Equals, int64(1973))
}

func (vms *VirtMachSuite) TestDefVar(c *gc.C) {
	set := NewSymbol("define")
	foo := NewSymbol("foo")
	val := NewInteger(1972)
	expr := NewList(set, foo, val)
	name := "TestDefVar"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	env := NewEnvironment(theReportEnvironment)
	result, err := EvaluateCode(code, env)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to evaluate code: %s", err))
	c.Assert(result, gc.IsNil, gc.Commentf("set should not yield result"))
	result = env.Find(foo)
	c.Check(result, gc.FitsTypeOf, val)
	ri := result.(Integer)
	c.Check(ri.ToInteger(), gc.Equals, int64(1972))
}

func (vms *VirtMachSuite) TestQuote(c *gc.C) {
	expr := parseAndExpandForTest(`(quote (+ 1 2))`, c)
	name := "TestQuote"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	env := theReportEnvironment
	result, err := EvaluateCode(code, env)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to evaluate code: %s", err))
	c.Assert(result, gc.NotNil, gc.Commentf("quote failed to yield result"))
	c.Check(result, gc.FitsTypeOf, expr)
	pair := result.(Pair)
	first := pair.First()
	expected := NewParsedSymbol("+", 1, 8)
	c.Check(first, gc.FitsTypeOf, expected)
	sym := first.(Symbol)
	c.Check(sym.String(), gc.Equals, "+")
}

func (vms *VirtMachSuite) TestIf(c *gc.C) {
	expr := parseAndExpandForTest(`(if #t 1)`, c)
	name := "TestIf"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	env := theReportEnvironment
	result, err := EvaluateCode(code, env)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to evaluate code: %s", err))
	c.Assert(result, gc.NotNil, gc.Commentf("if failed to yield result"))
	num := result.(Integer)
	c.Check(num.ToInteger(), gc.Equals, int64(1))
}

func (vms *VirtMachSuite) TestIfElse(c *gc.C) {
	expr := parseAndExpandForTest(`(if #t 1 2)`, c)
	name := "TestIfElse"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	env := theReportEnvironment
	result, err := EvaluateCode(code, env)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to evaluate code: %s", err))
	c.Assert(result, gc.NotNil, gc.Commentf("if failed to yield result"))
	num := result.(Integer)
	c.Check(num.ToInteger(), gc.Equals, int64(1))
}

func (vms *VirtMachSuite) TestBegin(c *gc.C) {
	expr := parseAndExpandForTest(`(begin (if #t 1 2) (if #f 3 4))`, c)
	name := "TestBegin"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	env := theReportEnvironment
	result, err := EvaluateCode(code, env)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to evaluate code: %s", err))
	c.Assert(result, gc.NotNil, gc.Commentf("begin failed to yield result"))
	num := result.(Integer)
	c.Check(num.ToInteger(), gc.Equals, int64(4))
}

func (vms *VirtMachSuite) TestProcedure(c *gc.C) {
	expr := parseAndExpandForTest(`(+ 1 2 3)`, c)
	name := "TestProcedure"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	env := theReportEnvironment
	result, err := EvaluateCode(code, env)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to evaluate code: %s", err))
	c.Assert(result, gc.NotNil, gc.Commentf("(+) failed to yield result"))
	num := result.(Integer)
	c.Check(num.ToInteger(), gc.Equals, int64(6))
}

func (vms *VirtMachSuite) TestClosure(c *gc.C) {
	expr := `(define (f x) (+ 1 2 x)) (f 7)`
	name := "TestClosure"
	code, err := CompileString(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	env := NewEnvironment(theReportEnvironment)
	result, err := EvaluateCode(code, env)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to evaluate code: %s", err))
	c.Assert(result, gc.NotNil, gc.Commentf("lambda failed to yield result"))
	num := result.(Integer)
	c.Check(num.ToInteger(), gc.Equals, int64(10))
}
