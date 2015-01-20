//
// Copyright 2014-2015 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	gc "launchpad.net/gocheck"
	"testing"
)

type VirtMachSuite struct {
}

var _ = gc.Suite(&VirtMachSuite{})

func virtmachErrorTest(c *gc.C, table map[string]string) {
	for input, expected := range table {
		expr := parseAndExpandForTest(input, c)
		code, err := Compile("compilerErrorTest", expr)
		c.Assert(err, gc.IsNil, gc.Commentf("compilation failed: %s", err))
		c.Assert(code, gc.NotNil, gc.Commentf("compiler did not produce code"))
		env := NewEnvironment(theReportEnvironment)
		result, err := EvaluateCode(code, env)
		c.Assert(err, gc.NotNil, gc.Commentf("VM should have raised error"))
		c.Assert(result, gc.IsNil, gc.Commentf("error in VM should yield nil result"))
		c.Check(err, gc.ErrorMatches, expected)
	}
}

func virtmachPassTest(c *gc.C, table map[string]string) {
	for input, expected := range table {
		expr := parseAndExpandForTest(input, c)
		code, err := Compile("virtmachPassTest", expr)
		c.Assert(err, gc.IsNil, gc.Commentf("compilation failed: %s", err))
		c.Assert(code, gc.NotNil)
		env := NewEnvironment(theReportEnvironment)
		result, err := EvaluateCode(code, env)
		c.Assert(err, gc.IsNil)
		c.Assert(result, gc.NotNil)
		c.Check(stringify(result), gc.Equals, expected)
	}
}

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
	expr := `(define (f (x)) (+ 1 2 x)) (f 7)`
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

func (vms *VirtMachSuite) TestLambdaArgs(c *gc.C) {
	// All cases described in R7RS 4.1.4
	// improper list, extra values assigned to last name
	input := `((lambda (x y . z) z) 3 4 5 6)` // => (5 6)
	name := "TestLambdaArgs"
	code, err := CompileString(name, input)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	env := NewEnvironment(theReportEnvironment)
	result, err := EvaluateCode(code, env)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to evaluate code: %s", err))
	c.Assert(result, gc.NotNil, gc.Commentf("lambda failed to yield result"))
	pair := result.(Pair)
	c.Assert(pair.Len(), gc.Equals, 2)
	checkInteger(pair.First(), 5, c)
	checkInteger(pair.Second(), 6, c)

	// single argument symbol receives all values
	input = `((lambda args args) 3 4 5 6)`
	code, err = CompileString(name, input)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	env = NewEnvironment(theReportEnvironment)
	result, err = EvaluateCode(code, env)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to evaluate code: %s", err))
	c.Assert(result, gc.NotNil, gc.Commentf("lambda failed to yield result"))
	pair = result.(Pair)
	c.Assert(pair.Len(), gc.Equals, 4)
	checkInteger(pair.First(), 3, c)
	checkInteger(pair.Second(), 4, c)
	checkInteger(pair.Third(), 5, c)
	checkInteger(Cxr("cadddr", pair), 6, c)

	// proper list with arguments
	input = `((lambda (x y) (+ x y)) 3 4)`
	code, err = CompileString(name, input)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	env = NewEnvironment(theReportEnvironment)
	result, err = EvaluateCode(code, env)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to evaluate code: %s", err))
	c.Assert(result, gc.NotNil, gc.Commentf("lambda failed to yield result"))
	checkInteger(result, 7, c)

	// list with one argument receives one value (R7RS 4.1.4)
	input = `((lambda (x) x) 3)`
	code, err = CompileString(name, input)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	env = NewEnvironment(theReportEnvironment)
	result, err = EvaluateCode(code, env)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to evaluate code: %s", err))
	c.Assert(result, gc.NotNil, gc.Commentf("lambda failed to yield result"))
	checkInteger(result, 3, c)
}

func (vms *VirtMachSuite) TestRecursiveFib(c *gc.C) {
	name := "TestRecursiveFib"
	code, err := CompileString(name, fibonacciRecursiveText)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	env := NewEnvironment(theReportEnvironment)
	result, err := EvaluateCode(code, env)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to evaluate code: %s", err))
	c.Assert(result, gc.NotNil, gc.Commentf("lambda failed to yield result"))
	c.Check(result, gc.Equals, NewInteger(3736710778780434371))
}

func (vms *VirtMachSuite) TestRecursiveSqrt(c *gc.C) {
	c.Skip("TODO: builtinDivide() use of BigRat appears going wrong")
	name := "TestRecursiveSqrt"
	code, err := CompileString(name, squareRootSicp)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	env := NewEnvironment(theReportEnvironment)
	result, err := EvaluateCode(code, env)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to evaluate code: %s", err))
	c.Assert(result, gc.NotNil, gc.Commentf("lambda failed to yield result"))
	c.Check(result, gc.Equals, NewInteger(12345))
}

func (cs *CompilerSuite) TestCompilerLambdaErrors(c *gc.C) {
	table := make(map[string]string)
	table[`(1 2 3 4)`] = ".* is not applicable.*"
	virtmachErrorTest(c, table)
}

func (cs *CompilerSuite) TestLambdaApplication(c *gc.C) {
	inputs := make(map[string]string)
	inputs[`(apply + (list 3 4))`] = `7`
	inputs[`(apply + (list 3 4) 10)`] = `17`
	inputs[`(map cadr '((a b) (d e) (g h)))`] = `(b e h)`
	inputs[`(map (lambda (n) (* n n)) '(1 2 3 4 5))`] = `(1 4 9 16 25)`
	virtmachPassTest(c, inputs)
}

func (cs *CompilerSuite) TestQuasiquotation(c *gc.C) {
	inputs := make(map[string]string)
	inputs["'(foo x)"] = "(foo x)"
	inputs["`(list ,(+ 1 2) 4)"] = "(list 3 4)"
	inputs["`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)"] = "(a 3 4 5 6 b)"
	inputs["`(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)"] = "(10 5 2 4 3 8)"
	inputs["`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))"] = "((foo 7) . cons)"
	inputs["`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)"] = "#(10 5 2 4 3 8)"
	virtmachPassTest(c, inputs)
}

func BenchmarkEvalBytes(b *testing.B) {
	name := "BenchmarkEvalBytes"
	code, err := CompileString(name, fibonacciRecursiveText)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		env := NewEnvironment(theReportEnvironment)
		_, err = EvaluateCode(code, env)
		if err != nil {
			b.Fatalf("error running input: %s", err)
		}
	}
}
