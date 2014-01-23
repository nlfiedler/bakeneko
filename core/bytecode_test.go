//
// Copyright 2014 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	"bytes"
	"encoding/gob"
	gc "launchpad.net/gocheck"
)

type ByteCodeSuite struct {
}

var _ = gc.Suite(&ByteCodeSuite{})

func (s *ByteCodeSuite) TestEncodeEmpty(c *gc.C) {
	codes := make([]Instruction, 0)
	consts := make([]interface{}, 0)
	syms := make([]Symbol, 0)
	lines := make([]byteLinePair, 0)
	empty := NewCodeObject("empty", theEmptyList, codes, consts, syms, lines)
	buff := new(bytes.Buffer)
	err := EncodeCode(empty, buff)
	c.Check(err, gc.IsNil)
	code, err := DecodeCode(buff)
	c.Check(err, gc.IsNil)
	c.Assert(code, gc.NotNil)
	c.Check(code.Name(), gc.Equals, "empty")
}

func (s *ByteCodeSuite) TestDecodeBadMagic(c *gc.C) {
	var buff bytes.Buffer
	enc := gob.NewEncoder(&buff)
	enc.Encode(int64(0x01010101))
	code, err := DecodeCode(&buff)
	c.Check(code, gc.IsNil)
	c.Check(err, gc.Equals, BadMagicNumber)
}

func (s *ByteCodeSuite) TestDecodeBadVersion(c *gc.C) {
	var buff bytes.Buffer
	enc := gob.NewEncoder(&buff)
	enc.Encode(int64(codeMagicNumber))
	enc.Encode(int64(0xffffffff))
	code, err := DecodeCode(&buff)
	c.Check(code, gc.IsNil)
	c.Check(err, gc.Equals, BadCodeVersion)
}

func compareCodeObjects(obtained, expected CodeObject, c *gc.C) {
	// compare names
	c.Check(obtained.Name(), gc.Equals, expected.Name())
	// compare symbols
	c.Assert(obtained.SymbolLen(), gc.Equals, expected.SymbolLen())
	length := obtained.SymbolLen()
	var pos uint
	for pos = 0; pos < length; pos++ {
		c.Check(obtained.GetSymbol(pos).String(), gc.Equals, expected.GetSymbol(pos).String())
	}
	// compare constants
	c.Assert(obtained.ConstantLen(), gc.Equals, expected.ConstantLen())
	length = obtained.ConstantLen()
	for pos = 0; pos < length; pos++ {
		oc := obtained.GetConstant(pos)
		ec := expected.GetConstant(pos)
		c.Check(stringify(oc), gc.Equals, stringify(ec))
		// this would have been ideal but seems like pairs don't compare
		// c.Check(obtained.GetConstant(pos), gc.DeepEquals, expected.GetConstant(pos))
	}
	// compare arguments
	oiter := NewPairIterator(obtained.Arguments())
	eiter := NewPairIterator(expected.Arguments())
	for oiter.HasNext() && eiter.HasNext() {
		oelem := stringify(oiter.Next())
		eelem := stringify(eiter.Next())
		c.Check(oelem, gc.Equals, eelem)
	}
	c.Check(oiter.HasNext(), gc.Equals, false)
	c.Check(eiter.HasNext(), gc.Equals, false)
	// compare instructions
	c.Assert(obtained.CodeLen(), gc.Equals, expected.CodeLen())
	length = obtained.CodeLen()
	for pos = 0; pos < length; pos++ {
		oi := obtained.GetInstruction(pos)
		ei := expected.GetInstruction(pos)
		c.Check(oi.Argument(), gc.Equals, ei.Argument())
		c.Check(oi.Code(), gc.Equals, ei.Code())
	}
	// compare line number information
	length = obtained.CodeLen()
	for pos = 0; pos < length; pos++ {
		c.Check(obtained.LineForOffset(pos), gc.Equals, expected.LineForOffset(pos))
	}
}

func (cs *CompilerSuite) TestEncodeLambda(c *gc.C) {
	expr := parseAndExpandForTest(`(define (test x y) (if #t x y))`, c)
	name := "TestEncodeLambda"
	expected, lerr := Compile(name, expr)
	c.Assert(lerr, gc.IsNil, gc.Commentf("failed to compile code: %s", lerr))
	c.Assert(expected, gc.NotNil, gc.Commentf("failed to produce code"))
	buff := new(bytes.Buffer)
	err := EncodeCode(expected, buff)
	c.Check(err, gc.IsNil)
	result, err := DecodeCode(buff)
	c.Check(err, gc.IsNil)
	c.Assert(result, gc.NotNil)
	compareCodeObjects(result, expected, c)
}

func (cs *CompilerSuite) testEncodeValue(expected interface{}, c *gc.C) interface{} {
	// encode
	buff := new(bytes.Buffer)
	enc := gob.NewEncoder(buff)
	encoder := newBytecodeEncoder(enc)
	err := encoder.encodeValue(expected)
	c.Check(err, gc.IsNil)
	// decode
	dec := gob.NewDecoder(buff)
	decoder := newBytecodeDecoder(dec)
	obtained, err := decoder.decodeValue()
	c.Check(err, gc.IsNil)
	// compare
	if _, is_pair := obtained.(Pair); !is_pair {
		if _, is_vector := obtained.(Vector); !is_vector {
			// avoid checking pairs and vectors, they may have mutually
			// recursive structure, for which reflect.DeepEquals does not
			// know how to avoid looping indefinitely
			c.Check(obtained, gc.DeepEquals, expected)
		}
	}
	return obtained
}

func (cs *CompilerSuite) TestEncodingValues(c *gc.C) {
	cs.testEncodeValue(BooleanTrue, c)
	cs.testEncodeValue(BooleanFalse, c)
	cs.testEncodeValue(NewInteger(0), c)
	cs.testEncodeValue(NewInteger(12345), c)
	cs.testEncodeValue(NewFloat(123.45), c)
	cs.testEncodeValue(NewComplex(complex(1.2, 1.0)), c)
	cs.testEncodeValue(NewRational(1, 3), c)
	cs.testEncodeValue(NewCharacter("#\\m"), c)
	cs.testEncodeValue(NewSymbol("foobar"), c)

	// byte vector
	bdata := make([]uint8, 5)
	bdata[0] = 10
	bdata[1] = 20
	bdata[2] = 30
	bdata[3] = 40
	bdata[4] = 50
	cs.testEncodeValue(NewByteVector(bdata), c)

	// vector
	data := make([]interface{}, 5)
	data[0] = NewSymbol("foo")
	data[1] = NewSymbol("bar")
	data[2] = NewSymbol("baz")
	data[3] = NewSymbol("qux")
	data[4] = NewSymbol("blargh")
	cs.testEncodeValue(NewVector(data), c)

	// test round-trip of vector (with loop)
	vector := NewVector(data)
	vector.Set(4, vector)
	thing1 := cs.testEncodeValue(vector, c)
	if ovector, ok := thing1.(Vector); ok {
		c.Check(ovector.Length(), gc.Equals, 5)
		c.Check(ovector.Get(0), gc.DeepEquals, data[0])
		c.Check(ovector.Get(1), gc.DeepEquals, data[1])
		c.Check(ovector.Get(2), gc.DeepEquals, data[2])
		c.Check(ovector.Get(3), gc.DeepEquals, data[3])
		thing3 := ovector.Get(4)
		if nvector, ok := thing3.(Vector); ok {
			c.Check(nvector.Length(), gc.Equals, 5)
		} else {
			c.Error("nested object is not a Vector")
		}
	} else {
		c.Error("obtained object is not a Vector")
	}

	// pairs
	cs.testEncodeValue(NewPair(data[0]), c)
	cs.testEncodeValue(NewList(data[0], data[1]), c)
	cs.testEncodeValue(Cons(data[0], data[1]), c)
	cs.testEncodeValue(NewList(data[0], data[1], data[2]), c)
	cs.testEncodeValue(Cons(data[0], Cons(data[1], data[2])), c)

	// test round-trip of pair (with loop)
	pair := NewList(data[0], data[1])
	pair.Append(pair)
	thing2 := cs.testEncodeValue(pair, c)
	if opair, ok := thing2.(Pair); ok {
		c.Check(opair.Len(), gc.Equals, 3)
		c.Check(opair.First(), gc.DeepEquals, data[0])
		c.Check(opair.Second(), gc.DeepEquals, data[1])
		c.Check(opair.Third(), gc.Equals, opair)
	} else {
		c.Error("obtained object is not a Pair")
	}
}
