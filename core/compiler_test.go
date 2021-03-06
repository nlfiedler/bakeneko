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

type CompilerSuite struct {
}

var _ = gc.Suite(&CompilerSuite{})

// parseAndExpandForTest parses and expands the input text into an
// expression ready for compilation.
func parseAndExpandForTest(input string, c *gc.C) interface{} {
	var err LispError
	parser := NewParser()
	body, err := parser.Parse(input)
	c.Assert(err, gc.IsNil, gc.Commentf("error parsing %q: %s", input, err))
	expr, err := parser.Expand(body)
	c.Assert(err, gc.IsNil, gc.Commentf("error expanding %q: %s", input, err))
	return expr
}

func (cs *CompilerSuite) TestConstant(c *gc.C) {
	expr := NewInteger(123)
	name := "TestConstant"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	c.Assert(code.CodeLen(), gc.Equals, uint(1))
	c.Check(code.GetInstruction(0).Code(), gc.Equals, OP_CONST)
	c.Check(code.SymbolLen(), gc.Equals, uint(0))
	c.Check(code.ConstantLen(), gc.Equals, uint(1))
	c.Check(code.Name(), gc.Equals, name)
	c.Check(code.GetConstant(0), gc.Equals, expr)
}

func (cs *CompilerSuite) TestSymbol(c *gc.C) {
	expr := NewSymbol("foo")
	name := "TestSymbol"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	c.Check(code.Name(), gc.Equals, name)
	c.Assert(code.CodeLen(), gc.Equals, uint(1))
	c.Check(code.GetInstruction(0).Code(), gc.Equals, OP_LOADVAR)
	c.Assert(code.SymbolLen(), gc.Equals, uint(1))
	c.Check(code.GetSymbol(0), gc.Equals, expr)
	c.Check(code.ConstantLen(), gc.Equals, uint(0))
}

func (cs *CompilerSuite) TestSetVar(c *gc.C) {
	set := NewSymbol("set!")
	foo := NewSymbol("foo")
	val := NewInteger(1973)
	expr := NewList(set, foo, val)
	name := "TestSetVar"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	c.Assert(code.CodeLen(), gc.Equals, uint(2))
	c.Check(code.GetInstruction(0).Code(), gc.Equals, OP_CONST)
	c.Check(code.GetInstruction(1).Code(), gc.Equals, OP_STOREVAR)
	c.Check(code.Name(), gc.Equals, name)
	c.Assert(code.SymbolLen(), gc.Equals, uint(1))
	c.Check(code.GetSymbol(0), gc.Equals, foo)
	c.Assert(code.ConstantLen(), gc.Equals, uint(1))
	c.Check(code.GetConstant(0), gc.Equals, val)
}

func (cs *CompilerSuite) TestDefine(c *gc.C) {
	def := NewSymbol("define")
	foo := NewSymbol("foo")
	val := NewInteger(1972)
	expr := NewList(def, foo, val)
	name := "TestDefine"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	c.Check(code.Name(), gc.Equals, name)
	c.Assert(code.CodeLen(), gc.Equals, uint(2))
	c.Check(code.GetInstruction(0).Code(), gc.Equals, OP_CONST)
	c.Check(code.GetInstruction(0).Argument(), gc.Equals, uint(0))
	c.Check(code.GetInstruction(1).Code(), gc.Equals, OP_DEFVAR)
	c.Check(code.GetInstruction(1).Argument(), gc.Equals, uint(0))
	c.Assert(code.SymbolLen(), gc.Equals, uint(1))
	c.Check(code.GetSymbol(0), gc.Equals, foo)
	c.Assert(code.ConstantLen(), gc.Equals, uint(1))
	c.Check(code.GetConstant(0), gc.Equals, val)
}

func (cs *CompilerSuite) TestQuote(c *gc.C) {
	expr := parseAndExpandForTest(`(quote (+ 1 2))`, c)
	name := "TestQuote"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	c.Assert(code.CodeLen(), gc.Equals, uint(1))
	c.Check(code.GetInstruction(0).Code(), gc.Equals, OP_CONST)
	c.Check(code.Name(), gc.Equals, name)
	c.Assert(code.SymbolLen(), gc.Equals, uint(0))
	c.Assert(code.ConstantLen(), gc.Equals, uint(1))
	thing := code.GetConstant(0)
	c.Check(thing, gc.FitsTypeOf, expr)
}

func (cs *CompilerSuite) TestIf(c *gc.C) {
	expr := parseAndExpandForTest(`(if #t 1)`, c)
	name := "TestIf"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	c.Assert(code.CodeLen(), gc.Equals, uint(5))
	c.Check(code.GetInstruction(0).Code(), gc.Equals, OP_CONST)
	c.Check(code.GetInstruction(0).Argument(), gc.Equals, uint(0))
	c.Check(code.GetInstruction(1).Code(), gc.Equals, OP_FJUMP)
	c.Check(code.GetInstruction(1).Argument(), gc.Equals, uint(4))
	c.Check(code.GetInstruction(2).Code(), gc.Equals, OP_CONST)
	c.Check(code.GetInstruction(2).Argument(), gc.Equals, uint(1))
	c.Check(code.GetInstruction(3).Code(), gc.Equals, OP_JUMP)
	c.Check(code.GetInstruction(3).Argument(), gc.Equals, uint(5))
	c.Check(code.GetInstruction(4).Code(), gc.Equals, OP_CONST)
	c.Check(code.GetInstruction(4).Argument(), gc.Equals, uint(2))
	c.Check(code.Name(), gc.Equals, name)
	c.Assert(code.SymbolLen(), gc.Equals, uint(0))
	c.Assert(code.ConstantLen(), gc.Equals, uint(3))
	boo := code.GetConstant(0).(Boolean)
	c.Check(boo.Value(), gc.Equals, true)
	num := code.GetConstant(1).(Integer)
	c.Check(num.ToInteger(), gc.Equals, int64(1))
	list := code.GetConstant(2).(Pair)
	c.Check(list.Len(), gc.Equals, 0)
}

func (cs *CompilerSuite) TestIfElse(c *gc.C) {
	expr := parseAndExpandForTest(`(if #t 1 2)`, c)
	name := "TestIfElse"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	c.Assert(code.CodeLen(), gc.Equals, uint(5))
	c.Check(code.GetInstruction(0).Code(), gc.Equals, OP_CONST)
	c.Check(code.GetInstruction(0).Argument(), gc.Equals, uint(0))
	c.Check(code.GetInstruction(1).Code(), gc.Equals, OP_FJUMP)
	c.Check(code.GetInstruction(1).Argument(), gc.Equals, uint(4))
	c.Check(code.GetInstruction(2).Code(), gc.Equals, OP_CONST)
	c.Check(code.GetInstruction(2).Argument(), gc.Equals, uint(1))
	c.Check(code.GetInstruction(3).Code(), gc.Equals, OP_JUMP)
	c.Check(code.GetInstruction(3).Argument(), gc.Equals, uint(5))
	c.Check(code.GetInstruction(4).Code(), gc.Equals, OP_CONST)
	c.Check(code.GetInstruction(4).Argument(), gc.Equals, uint(2))
	c.Check(code.Name(), gc.Equals, name)
	c.Assert(code.SymbolLen(), gc.Equals, uint(0))
	c.Assert(code.ConstantLen(), gc.Equals, uint(3))
	boo := code.GetConstant(0).(Boolean)
	c.Check(boo.Value(), gc.Equals, true)
	num := code.GetConstant(1).(Integer)
	c.Check(num.ToInteger(), gc.Equals, int64(1))
	num = code.GetConstant(2).(Integer)
	c.Check(num.ToInteger(), gc.Equals, int64(2))
}

func (cs *CompilerSuite) TestBegin(c *gc.C) {
	expr := parseAndExpandForTest(`(begin (if #t 1 2) (if #f 3 4))`, c)
	name := "TestBegin"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	c.Assert(code.CodeLen(), gc.Equals, uint(11))
	c.Check(code.GetInstruction(0).Code(), gc.Equals, OP_CONST)
	c.Check(code.GetInstruction(0).Argument(), gc.Equals, uint(0))
	c.Check(code.GetInstruction(1).Code(), gc.Equals, OP_FJUMP)
	c.Check(code.GetInstruction(1).Argument(), gc.Equals, uint(4))
	c.Check(code.GetInstruction(2).Code(), gc.Equals, OP_CONST)
	c.Check(code.GetInstruction(2).Argument(), gc.Equals, uint(1))
	c.Check(code.GetInstruction(3).Code(), gc.Equals, OP_JUMP)
	c.Check(code.GetInstruction(3).Argument(), gc.Equals, uint(5))
	c.Check(code.GetInstruction(4).Code(), gc.Equals, OP_CONST)
	c.Check(code.GetInstruction(4).Argument(), gc.Equals, uint(2))
	c.Check(code.GetInstruction(5).Code(), gc.Equals, OP_POP)
	c.Check(code.GetInstruction(5).Argument(), gc.Equals, uint(0))
	c.Check(code.GetInstruction(6).Code(), gc.Equals, OP_CONST)
	c.Check(code.GetInstruction(6).Argument(), gc.Equals, uint(3))
	c.Check(code.GetInstruction(7).Code(), gc.Equals, OP_FJUMP)
	c.Check(code.GetInstruction(7).Argument(), gc.Equals, uint(10))
	c.Check(code.GetInstruction(8).Code(), gc.Equals, OP_CONST)
	c.Check(code.GetInstruction(8).Argument(), gc.Equals, uint(4))
	c.Check(code.GetInstruction(9).Code(), gc.Equals, OP_JUMP)
	c.Check(code.GetInstruction(9).Argument(), gc.Equals, uint(11))
	c.Check(code.GetInstruction(10).Code(), gc.Equals, OP_CONST)
	c.Check(code.GetInstruction(10).Argument(), gc.Equals, uint(5))
	c.Check(code.Name(), gc.Equals, name)
	c.Assert(code.SymbolLen(), gc.Equals, uint(0))
	c.Assert(code.ConstantLen(), gc.Equals, uint(6))
	boo := code.GetConstant(0).(Boolean)
	c.Check(boo.Value(), gc.Equals, true)
	num := code.GetConstant(1).(Integer)
	c.Check(num.ToInteger(), gc.Equals, int64(1))
	num = code.GetConstant(2).(Integer)
	c.Check(num.ToInteger(), gc.Equals, int64(2))
	boo = code.GetConstant(3).(Boolean)
	c.Check(boo.Value(), gc.Equals, false)
	num = code.GetConstant(4).(Integer)
	c.Check(num.ToInteger(), gc.Equals, int64(3))
	num = code.GetConstant(5).(Integer)
	c.Check(num.ToInteger(), gc.Equals, int64(4))
}

func (cs *CompilerSuite) TestLambdaArgList(c *gc.C) {
	expr := parseAndExpandForTest(`(lambda (x y) (if #t x y))`, c)
	name := "TestLambda"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	c.Check(code.Name(), gc.Equals, name)
	c.Assert(code.CodeLen(), gc.Equals, uint(1))
	c.Assert(code.SymbolLen(), gc.Equals, uint(0))
	c.Assert(code.ConstantLen(), gc.Equals, uint(1))
	c.Check(code.GetInstruction(0).Code(), gc.Equals, OP_FUNCTION)
	c.Check(code.GetInstruction(0).Argument(), gc.Equals, uint(0))
	fun := code.GetConstant(0).(CodeObject)
	c.Check(stringify(fun.Arguments()), gc.Equals, "(x y)")
}

func (cs *CompilerSuite) TestLambdaArgsImproper(c *gc.C) {
	expr := parseAndExpandForTest(`(lambda (x . y) (if #t x y))`, c)
	name := "TestLambda"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	c.Check(code.Name(), gc.Equals, name)
	c.Assert(code.CodeLen(), gc.Equals, uint(1))
	c.Assert(code.SymbolLen(), gc.Equals, uint(0))
	c.Assert(code.ConstantLen(), gc.Equals, uint(1))
	c.Check(code.GetInstruction(0).Code(), gc.Equals, OP_FUNCTION)
	c.Check(code.GetInstruction(0).Argument(), gc.Equals, uint(0))
	fun := code.GetConstant(0).(CodeObject)
	c.Check(stringify(fun.Arguments()), gc.Equals, "(x . y)")
}

func (cs *CompilerSuite) TestLambdaArgSymbol(c *gc.C) {
	expr := parseAndExpandForTest(`(lambda x (if #t x ()))`, c)
	name := "TestLambda"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	c.Check(code.Name(), gc.Equals, name)
	c.Assert(code.CodeLen(), gc.Equals, uint(1))
	c.Assert(code.SymbolLen(), gc.Equals, uint(0))
	c.Assert(code.ConstantLen(), gc.Equals, uint(1))
	c.Check(code.GetInstruction(0).Code(), gc.Equals, OP_FUNCTION)
	c.Check(code.GetInstruction(0).Argument(), gc.Equals, uint(0))
	fun := code.GetConstant(0).(CodeObject)
	c.Check(stringify(fun.Arguments()), gc.Equals, "x")
}

func (cs *CompilerSuite) TestDefLambda(c *gc.C) {
	expr := parseAndExpandForTest(`(define (test (x y)) (if #t x y))`, c)
	name := "TestDefLambda"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	c.Check(code.Name(), gc.Equals, name)
	c.Assert(code.CodeLen(), gc.Equals, uint(2))
	c.Assert(code.SymbolLen(), gc.Equals, uint(1))
	c.Check(code.GetSymbol(0).String(), gc.Equals, "test")
	c.Assert(code.ConstantLen(), gc.Equals, uint(1))
	c.Check(code.GetInstruction(0).Code(), gc.Equals, OP_FUNCTION)
	c.Check(code.GetInstruction(0).Argument(), gc.Equals, uint(0))
	c.Check(code.GetInstruction(1).Code(), gc.Equals, OP_DEFVAR)
	c.Check(code.GetInstruction(1).Argument(), gc.Equals, uint(0))
	fun := code.GetConstant(0).(CodeObject)
	c.Check(fun.Name(), gc.Equals, "test")
	c.Check(stringify(fun.Arguments()), gc.Equals, "(x y)")
}

func (cs *CompilerSuite) TestApplication(c *gc.C) {
	expr := parseAndExpandForTest(`(+ 1 2 3)`, c)
	name := "TestApplication"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	c.Check(code.Name(), gc.Equals, name)
	c.Assert(code.CodeLen(), gc.Equals, uint(5))
	c.Assert(code.SymbolLen(), gc.Equals, uint(1))
	c.Assert(code.ConstantLen(), gc.Equals, uint(3))
	c.Check(code.GetInstruction(0).Code(), gc.Equals, OP_CONST)
	c.Check(code.GetInstruction(0).Argument(), gc.Equals, uint(0))
	c.Check(code.GetInstruction(1).Code(), gc.Equals, OP_CONST)
	c.Check(code.GetInstruction(1).Argument(), gc.Equals, uint(1))
	c.Check(code.GetInstruction(2).Code(), gc.Equals, OP_CONST)
	c.Check(code.GetInstruction(2).Argument(), gc.Equals, uint(2))
	c.Check(code.GetInstruction(3).Code(), gc.Equals, OP_LOADVAR)
	c.Check(code.GetInstruction(3).Argument(), gc.Equals, uint(0))
	c.Check(code.GetInstruction(4).Code(), gc.Equals, OP_CALL)
	c.Check(code.GetInstruction(4).Argument(), gc.Equals, uint(3))
	num := code.GetConstant(0).(Integer)
	c.Check(num.ToInteger(), gc.Equals, int64(1))
	num = code.GetConstant(1).(Integer)
	c.Check(num.ToInteger(), gc.Equals, int64(2))
	num = code.GetConstant(2).(Integer)
	c.Check(num.ToInteger(), gc.Equals, int64(3))
	c.Check(code.GetSymbol(0).String(), gc.Equals, "+")
}

func (cs *CompilerSuite) TestLocationInfo(c *gc.C) {
	// Use something simple that spans multiple lines; using a closure and
	// application results in nested code objects and weird location info.
	input := `(*
  (+ 1 2)
  (/ 10 5)
  (- 12 4)
  (* 4 2))`
	expr := parseAndExpandForTest(input, c)
	name := "TestLocationInfo"
	code, err := Compile(name, expr)
	c.Assert(err, gc.IsNil, gc.Commentf("failed to compile code: %s", err))
	c.Assert(code, gc.NotNil, gc.Commentf("failed to produce code"))
	c.Check(code.Name(), gc.Equals, name)
	c.Assert(code.CodeLen(), gc.Equals, uint(18))
	// LineInfo:
	//     0 => 1
	//     1 => 2
	//     4 => 3
	//     8 => 4
	//     12 => 5
	c.Check(code.LineForOffset(0), gc.Equals, 1)
	c.Check(code.LineForOffset(1), gc.Equals, 2)
	c.Check(code.LineForOffset(2), gc.Equals, 2)
	c.Check(code.LineForOffset(3), gc.Equals, 2)
	c.Check(code.LineForOffset(4), gc.Equals, 3)
	c.Check(code.LineForOffset(5), gc.Equals, 3)
	c.Check(code.LineForOffset(6), gc.Equals, 3)
	c.Check(code.LineForOffset(7), gc.Equals, 3)
	c.Check(code.LineForOffset(8), gc.Equals, 4)
	c.Check(code.LineForOffset(9), gc.Equals, 4)
	c.Check(code.LineForOffset(10), gc.Equals, 4)
	c.Check(code.LineForOffset(11), gc.Equals, 4)
	c.Check(code.LineForOffset(12), gc.Equals, 5)
	c.Check(code.LineForOffset(13), gc.Equals, 5)
	c.Check(code.LineForOffset(14), gc.Equals, 5)
	c.Check(code.LineForOffset(15), gc.Equals, 5)
	c.Check(code.LineForOffset(16), gc.Equals, 5)
	c.Check(code.LineForOffset(17), gc.Equals, 5)
}

func BenchmarkCompile(b *testing.B) {
	parser := NewParser()
	body, err := parser.Parse(fibonacciRecursiveText)
	if err != nil {
		b.Fatalf("error parsing input: %s", err)
	}
	expr, err := parser.Expand(wrapWithBegin(body))
	if err != nil {
		b.Fatalf("error expanding input: %s", err)
	}
	name := "BenchmarkCompile"
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err = Compile(name, expr)
		if err != nil {
			b.Fatalf("error compiling input: %s", err)
		}
	}
}
