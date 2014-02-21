//
// Copyright 2014 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	"bytes"
	"encoding/gob"
	"fmt"
	"io"
	"math/big"
)

//
// byte code encoder/decoder and verifier
//

// Opcode represents an operation byte code, such as "load variable",
// "return", or "function call". The compiler produces a set of opcodes
// which are typically followed by a single argument. Other values are
// typically pushed onto an argument stack prior to the opcode being
// processed.
type Opcode byte

// Operation code constants
const (
	OP_NOP      Opcode = iota // no operation, used to signal end of code
	OP_LABEL                  // placeholder in intermediate code; arg is label number
	OP_CONST                  // arg: constants index
	OP_LOADVAR                // arg: symbols index
	OP_STOREVAR               // arg: symbols index; stack: value
	OP_DEFVAR                 // arg: symbols index; stack: value
	OP_FUNCTION               // arg: constants index
	OP_POP                    // arg: <none>; stack: <removes one>
	OP_JUMP                   // arg: bytecode offset
	OP_FJUMP                  // arg: bytecode offset; stack: predicate
	OP_RETURN                 // arg: <none>; stack: <none>
	OP_CALL                   // arg: number of arguments; stack: arguments
)

func (o Opcode) String() string {
	switch o {
	case OP_NOP:
		return "NOP"
	case OP_LABEL:
		return "LABEL"
	case OP_CONST:
		return "CONST"
	case OP_LOADVAR:
		return "LOADVAR"
	case OP_STOREVAR:
		return "STOREVAR"
	case OP_DEFVAR:
		return "DEFVAR"
	case OP_FUNCTION:
		return "FUNCTION"
	case OP_POP:
		return "POP"
	case OP_JUMP:
		return "JUMP"
	case OP_FJUMP:
		return "FJUMP"
	case OP_RETURN:
		return "RETURN"
	case OP_CALL:
		return "CALL"
	default:
		return "OP_???"
	}
}

// Instruction represents a single opcode and its argument, if any.
type Instruction interface {
	// Code returns the opcode for this instruction (e.g. OP_CALL).
	Code() Opcode
	// Argument returns the optional argument for this opcode. Not all
	// operations take an argument, such as OP_POP and OP_RETURN. Typically
	// this is an index into either the constants or symbols table.
	Argument() uint
}

// instruction is the internal implementation of an Instruction.
type instruction struct {
	code Opcode // opcode for this instruction
	arg  uint   // argument for this opcode
}

// NewInstruction creates an instance of Instruction based on the
// arguments.
func NewInstruction(code Opcode, arg uint) Instruction {
	return instruction{code, arg}
}

func (i instruction) Code() Opcode {
	return i.code
}

func (i instruction) Argument() uint {
	return i.arg
}

func (i instruction) String() string {
	return fmt.Sprintf("<%s: %d>", i.code, i.arg)
}

// CodeObject encapsulates a compiled script or procedure.
type CodeObject interface {
	fmt.Stringer
	// Name returns the name of the original source file, primarily used
	// for error reporting.
	Name() string
	// setName is used to modify the name of the code object after it has
	// been compiled, but prior to producing the final result.
	setName(name string)
	// Arguments returns the arguments for the compiled procedure.
	Arguments() Pair
	// LineForOffset returns the line number in the original source code
	// which corresponds to the given byte code offset, primarily used
	// for error reporting.
	LineForOffset(pos uint) int
	// ConstantLen returns the number of constants contained within.
	ConstantLen() uint
	// GetConstant returns a constant from the constants table.
	GetConstant(pos uint) interface{}
	// SymbolLen returns the number of symbols contained within.
	SymbolLen() uint
	// GetSymbol returns a symbol from the symbols table.
	GetSymbol(pos uint) Symbol
	// CodeLen returns the number of instructions contained within.
	CodeLen() uint
	// GetInstruction returns an instruction from the set of instructions.
	GetInstruction(pos uint) Instruction
	// Encode writes this CodeObject to the given gob encoder.
	encode(encoder *bytecodeEncoder) error
	// Decode populates this (empty) CodeObject based on the values
	// provided by the gob decoder.
	decode(decoder *bytecodeDecoder) error
}

// byteLinePair associates a byte code offset with a line number.
type byteLinePair struct {
	offset uint // byte code offset
	line   int  // line number
}

type byteLinePairs []byteLinePair

func (a byteLinePairs) Len() int {
	return len(a)
}

func (a byteLinePairs) Swap(i, j int) {
	a[i], a[j] = a[j], a[i]
}

func (a byteLinePairs) Less(i, j int) bool {
	return a[i].offset < a[j].offset && a[i].line < a[j].line
}

func (blp byteLinePair) String() string {
	return fmt.Sprintf("%d => %d", blp.offset, blp.line)
}

// bytecode is the in-memory representation of compiled byte code.
type bytecode struct {
	name      string         // name for this code object, typically source file
	arguments Pair           // procedure arguments
	byte_code []Instruction  // the byte code instructions
	constants []interface{}  // table of constant values (typically Atoms)
	symbols   []Symbol       // table of symbol names
	bylines   []byteLinePair // byte code offsets and their line numbers
}

// NewCodeObject constructs a new instance of CodeObject given the name,
// arguments, byte codes, constants, symbols, and mapping of line numbers
// to byte code offsets.
func NewCodeObject(name string, args Pair, codes []Instruction, cons []interface{},
	syms []Symbol, lines []byteLinePair) CodeObject {
	bc := new(bytecode)
	bc.name = name
	bc.arguments = args
	// CodeObject instances are meant to be immutable, so copy the incoming
	// data to private structures.
	bc.byte_code = make([]Instruction, len(codes))
	copy(bc.byte_code, codes)
	bc.constants = make([]interface{}, len(cons))
	copy(bc.constants, cons)
	bc.symbols = make([]Symbol, len(syms))
	copy(bc.symbols, syms)
	bc.bylines = make([]byteLinePair, len(lines))
	copy(bc.bylines, lines)
	return bc
}

// emptyCodeObject creates an empty CodeObject which is useful at times.
func emptyCodeObject(name string) CodeObject {
	codes := make([]Instruction, 0)
	consts := make([]interface{}, 0)
	syms := make([]Symbol, 0)
	lines := make([]byteLinePair, 0)
	return NewCodeObject(name, theEmptyList, codes, consts, syms, lines)
}

func (bc *bytecode) Name() string {
	return bc.name
}

func (bc *bytecode) setName(name string) {
	bc.name = name
}

func (bc *bytecode) Arguments() Pair {
	return bc.arguments
}

func (bc *bytecode) LineForOffset(pos uint) int {
	line := 1
	for _, blp := range bc.bylines {
		if blp.offset > pos {
			break
		}
		line = blp.line
	}
	return line
}

func (bc *bytecode) ConstantLen() uint {
	return uint(len(bc.constants))
}

func (bc *bytecode) GetConstant(pos uint) interface{} {
	return bc.constants[pos]
}

func (bc *bytecode) SymbolLen() uint {
	return uint(len(bc.symbols))
}

func (bc *bytecode) GetSymbol(pos uint) Symbol {
	return bc.symbols[pos]
}

func (bc *bytecode) CodeLen() uint {
	return uint(len(bc.byte_code))
}

func (bc *bytecode) GetInstruction(pos uint) Instruction {
	return bc.byte_code[pos]
}

func (bc *bytecode) String() string {
	buf := new(bytes.Buffer)
	fmt.Fprintf(buf, "Name: %s\n", bc.name)
	fmt.Fprintf(buf, "Arguments: %s\n", bc.arguments)
	buf.WriteString("Constants:\n")
	for _, cons := range bc.constants {
		fmt.Fprintf(buf, "    %v\n", cons)
	}
	buf.WriteString("Symbols:\n")
	for _, sym := range bc.symbols {
		fmt.Fprintf(buf, "    %v\n", sym)
	}
	buf.WriteString("Instructions:\n")
	for _, instr := range bc.byte_code {
		fmt.Fprintf(buf, "    %v\n", instr)
	}
	buf.WriteString("LineInfo:\n")
	for _, line := range bc.bylines {
		fmt.Fprintf(buf, "    %v\n", line)
	}
	return buf.String()
}

func (bc *bytecode) encode(encoder *bytecodeEncoder) error {
	enc := encoder.encoder
	err := enc.Encode(bc.name)
	if err != nil {
		return err
	}
	// encode the arguments
	err = encoder.encodePair(bc.arguments)
	if err != nil {
		return err
	}
	// encode the constants
	err = enc.Encode(len(bc.constants))
	if err != nil {
		return err
	}
	for _, cons := range bc.constants {
		err = encoder.encodeValue(cons)
		if err != nil {
			return err
		}
	}
	// encode the symbols
	err = enc.Encode(len(bc.symbols))
	if err != nil {
		return err
	}
	for _, sym := range bc.symbols {
		err = enc.Encode(sym.String())
		if err != nil {
			return err
		}
	}
	// encode the line number information
	err = enc.Encode(len(bc.bylines))
	if err != nil {
		return err
	}
	for _, byline := range bc.bylines {
		err = enc.Encode(byline.offset)
		if err != nil {
			return err
		}
		err = enc.Encode(byline.line)
		if err != nil {
			return err
		}
	}
	if err != nil {
		return err
	}
	// encode the instructions
	err = enc.Encode(len(bc.byte_code))
	if err != nil {
		return err
	}
	for _, instr := range bc.byte_code {
		err = enc.Encode(instr.Code())
		if err != nil {
			return err
		}
		err = enc.Encode(instr.Argument())
		if err != nil {
			return err
		}
	}
	return nil
}

func (bc *bytecode) decode(decoder *bytecodeDecoder) error {
	dec := decoder.decoder
	err := dec.Decode(&bc.name)
	if err != nil {
		return err
	}
	// decode the arguments
	bc.arguments, err = decoder.decodePair()
	if err != nil {
		return err
	}
	// decode the constants
	var length int
	err = dec.Decode(&length)
	if err != nil {
		return err
	}
	for count := 0; count < length; count++ {
		var cons interface{}
		cons, err = decoder.decodeValue()
		if err != nil {
			return err
		}
		bc.constants = append(bc.constants, cons)
	}
	// decode the symbols
	err = dec.Decode(&length)
	if err != nil {
		return err
	}
	for count := 0; count < length; count++ {
		var sym string
		err = dec.Decode(&sym)
		if err != nil {
			return err
		}
		bc.symbols = append(bc.symbols, NewSymbol(sym))
	}
	// decode the line offsets
	err = dec.Decode(&length)
	if err != nil {
		return err
	}
	for count := 0; count < length; count++ {
		var offset uint
		err = dec.Decode(&offset)
		if err != nil {
			return err
		}
		var line int
		err = dec.Decode(&line)
		if err != nil {
			return err
		}
		bc.bylines = append(bc.bylines, byteLinePair{offset, line})
	}
	if err != nil {
		return err
	}
	// decode the instructions
	err = dec.Decode(&length)
	if err != nil {
		return err
	}
	for count := 0; count < length; count++ {
		var code Opcode
		err = dec.Decode(&code)
		if err != nil {
			return err
		}
		var arg uint
		err = dec.Decode(&arg)
		if err != nil {
			return err
		}
		bc.byte_code = append(bc.byte_code, NewInstruction(code, arg))
	}
	return nil
}

// codeMagicNumber is the magic number for bakeneko code.
const codeMagicNumber int64 = 0xBACE2EC0

// codeVersion is the version of the byte code.
const codeVersion int64 = 0x01010101

// constKind represents the specific kind of type that a constant value
// represents, such as a number, string, symbol, vector, or pair.
type constKind uint

const (
	constInvalid constKind = iota
	constBool
	constInt
	constFloat
	constComplex
	constRational
	constCharacter
	constString
	constSymbol
	constVector
	constVectorRef
	constByteVector
	constPair
	constPairRef
	constCode
)

// bytecodeEncoder handles encoding a single CodeObject. It detects
// circular references among the vectors and pairs and serializes them such
// that the connections can be established upon deserialization.
type bytecodeEncoder struct {
	encoder     *gob.Encoder       // gob encoder to which everything is written
	all_vectors map[uintptr]Vector // all Vectors encountered while decoding
	all_pairs   map[uintptr]Pair   // all Pairs encountered while decoding
}

// newBytecodeEncoder constructs an instance of bytecodeEncoder.
func newBytecodeEncoder(enc *gob.Encoder) *bytecodeEncoder {
	bce := new(bytecodeEncoder)
	bce.encoder = enc
	bce.all_vectors = make(map[uintptr]Vector)
	bce.all_pairs = make(map[uintptr]Pair)
	return bce
}

func (bce *bytecodeEncoder) encodeValue(val interface{}) (err error) {
	enc := bce.encoder
	switch i := val.(type) {
	case Boolean:
		err = enc.Encode(constBool)
		if err == nil {
			err = enc.Encode(i.Value())
		}
	case Integer:
		err = enc.Encode(constInt)
		if err == nil {
			err = enc.Encode(i.ToInteger())
		}
	case Float:
		err = enc.Encode(constFloat)
		if err == nil {
			err = enc.Encode(i.ToFloat())
		}
	case Complex:
		err = enc.Encode(constComplex)
		if err == nil {
			err = enc.Encode(i.ToComplex())
		}
	case Rational:
		err = enc.Encode(constRational)
		if err == nil {
			err = enc.Encode(i.BigRat())
		}
	case Character:
		err = enc.Encode(constCharacter)
		if err == nil {
			err = enc.Encode(i.ToRune())
		}
	case String:
		err = enc.Encode(constString)
		if err == nil {
			err = enc.Encode(i.Value())
		}
	case Symbol:
		err = enc.Encode(constSymbol)
		if err == nil {
			err = enc.Encode(i.String())
		}
	case Pair:
		pid := i.ObjectId()
		if _, exists := bce.all_pairs[pid]; exists {
			// already saw this pair, insert a reference
			err = bce.encoder.Encode(constPairRef)
			if err == nil {
				err = bce.encoder.Encode(pid)
			}
		} else {
			bce.all_pairs[pid] = i
			err = bce.encoder.Encode(constPair)
			if err == nil {
				err = bce.encoder.Encode(pid)
				if err == nil {
					err = bce.encodePair(i)
				}
			}
		}
	case Vector:
		pid := i.ObjectId()
		if _, exists := bce.all_vectors[pid]; exists {
			// already saw this vector, insert a reference
			err = bce.encoder.Encode(constVectorRef)
			if err == nil {
				err = bce.encoder.Encode(pid)
			}
		} else {
			bce.all_vectors[pid] = i
			err = bce.encoder.Encode(constVector)
			if err == nil {
				err = bce.encoder.Encode(pid)
				if err == nil {
					err = bce.encodeVector(i)
				}
			}
		}
	case ByteVector:
		err = enc.Encode(constByteVector)
		if err == nil {
			err = enc.Encode(i)
		}
	case CodeObject:
		err = enc.Encode(constCode)
		if err == nil {
			err = i.encode(bce)
		}
	default:
		err = UnknownType
	}
	return
}

func (bce *bytecodeEncoder) encodePair(p Pair) (err error) {
	// discover if the list is proper, and determine its length
	iter := p.Iterator()
	count := 0
	for iter.HasNext() {
		iter.Next()
		count++
		if !iter.IsProper() {
			count = 0 - count
		}
	}
	// encode the length and properness of the pair
	err = bce.encoder.Encode(count)
	if err != nil {
		return
	}
	iter = p.Iterator()
	for iter.HasNext() {
		val := iter.Next()
		err = bce.encodeValue(val)
		if err != nil {
			return
		}
	}
	return
}

func (bce *bytecodeEncoder) encodeVector(v Vector) (err error) {
	length := v.Len()
	err = bce.encoder.Encode(length)
	if err != nil {
		return
	}
	for pos := 0; pos < length; pos++ {
		err = bce.encodeValue(v.Get(pos))
		if err != nil {
			return
		}
	}
	return
}

// bytecodeDecoder handles decoding an instance of serialized bytecode. It
// keeps track of the vectors and pairs encountered so as to detect and
// connect circular references within the structures.
type bytecodeDecoder struct {
	decoder     *gob.Decoder       // gob decoder from which everything is read
	all_vectors map[uintptr]Vector // all Vectors encountered while decoding
	all_pairs   map[uintptr]Pair   // all Pairs encountered while decoding
}

// newBytecodeDecoder constructs an instance of bytecodeDecoder.
func newBytecodeDecoder(dec *gob.Decoder) *bytecodeDecoder {
	bcd := new(bytecodeDecoder)
	bcd.decoder = dec
	bcd.all_vectors = make(map[uintptr]Vector)
	bcd.all_pairs = make(map[uintptr]Pair)
	return bcd
}

// forwardDecodeRef is a placeholder for datum references that are found
// within the decoded byte stream.
type forwardDecodeRef struct {
	refid   uintptr          // reference to the object
	decoder *bytecodeDecoder // our decoder for resolving references
}

func (fr *forwardDecodeRef) Label() string {
	return fmt.Sprintf("%p", fr.refid)
}

func (fr *forwardDecodeRef) Resolve() interface{} {
	if val, ok := fr.decoder.all_pairs[fr.refid]; ok {
		return val
	}
	if val, ok := fr.decoder.all_vectors[fr.refid]; ok {
		return val
	}
	return nil
}

func (bcd *bytecodeDecoder) decodeValue() (val interface{}, err error) {
	dec := bcd.decoder
	var kind constKind
	err = dec.Decode(&kind)
	if err != nil {
		return
	}
	switch kind {
	case constBool:
		var b bool
		err = dec.Decode(&b)
		if err == nil {
			val = BooleanFromBool(b)
		}
	case constInt:
		var i int64
		err = dec.Decode(&i)
		if err == nil {
			val = NewInteger(i)
		}
	case constFloat:
		var f float64
		err = dec.Decode(&f)
		if err == nil {
			val = NewFloat(f)
		}
	case constComplex:
		var c complex128
		err = dec.Decode(&c)
		if err == nil {
			val = NewComplex(c)
		}
	case constRational:
		var rat big.Rat
		err = dec.Decode(&rat)
		if err == nil {
			val = fromRational(&rat)
		}
	case constCharacter:
		var c rune
		err = dec.Decode(&c)
		if err == nil {
			val = characterImpl(c)
		}
	case constSymbol:
		var s string
		err = dec.Decode(&s)
		if err == nil {
			val = NewSymbol(s)
		}
	case constPair:
		var pid uintptr
		err = dec.Decode(&pid)
		if err == nil {
			var pair Pair
			pair, err = bcd.decodePair()
			if err == nil {
				bcd.all_pairs[pid] = pair
				val, err = resolveForwardRefs(pair)
			}
		}
	case constPairRef:
		var pid uintptr
		err = dec.Decode(&pid)
		if err == nil {
			var ok bool
			val, ok = bcd.all_pairs[pid]
			if !ok {
				// return a placeholder that can be fixed up later
				val = &forwardDecodeRef{pid, bcd}
			}
		}
	case constVector:
		var pid uintptr
		err = dec.Decode(&pid)
		if err == nil {
			var vector Vector
			vector, err = bcd.decodeVector()
			if err == nil {
				bcd.all_vectors[pid] = vector
				val, err = resolveForwardRefs(vector)
			}
		}
	case constVectorRef:
		var pid uintptr
		err = dec.Decode(&pid)
		if err == nil {
			var ok bool
			val, ok = bcd.all_vectors[pid]
			if !ok {
				// return a placeholder that can be fixed up later
				val = &forwardDecodeRef{pid, bcd}
			}
		}
	case constByteVector:
		bv := NewByteVector(make([]uint8, 0))
		err = dec.Decode(&bv)
		val = bv
	case constCode:
		code := emptyCodeObject("empty")
		err = code.decode(bcd)
		val = code
	default:
		panic(fmt.Sprintf("unknown kind of object: %v", kind))
	}
	return
}

func (bcd *bytecodeDecoder) decodePair() (val Pair, err error) {
	var count int
	err = bcd.decoder.Decode(&count)
	if err != nil {
		return
	}
	proper := true
	if count < 0 {
		proper = false
		count = 0 - count
	}
	joinr := NewPairBuilder()
	for count > 0 {
		count--
		var elem interface{}
		elem, err = bcd.decodeValue()
		if err != nil {
			return
		}
		if count == 0 && !proper {
			joinr.Join(elem)
		} else {
			joinr.Append(elem)
		}
	}
	val = joinr.List()
	return
}

func (bcd *bytecodeDecoder) decodeVector() (val Vector, err error) {
	var count int
	err = bcd.decoder.Decode(&count)
	if err != nil {
		return
	}
	data := make([]interface{}, count)
	for pos := 0; pos < count; pos++ {
		var elem interface{}
		elem, err = bcd.decodeValue()
		if err != nil {
			return
		}
		data[pos] = elem
	}
	val = NewVector(data)
	return
}

// EncodeCode writes the given code object to given writer.
func EncodeCode(code CodeObject, out io.Writer) (err error) {
	enc := gob.NewEncoder(out)
	err = enc.Encode(codeMagicNumber)
	if err == nil {
		err = enc.Encode(codeVersion)
		if err == nil {
			encoder := newBytecodeEncoder(enc)
			err = code.encode(encoder)
		}
	}
	return
}

func DecodeCode(in io.Reader) (code CodeObject, err error) {
	dec := gob.NewDecoder(in)
	var magic int64
	err = dec.Decode(&magic)
	if err != nil {
		return
	}
	if magic != codeMagicNumber {
		return nil, BadMagicNumber
	}
	var vers int64
	err = dec.Decode(&vers)
	if err != nil {
		return
	}
	if vers != codeVersion {
		return nil, BadCodeVersion
	}
	// create an empty code object in which to decode values
	code = emptyCodeObject("empty")
	decoder := newBytecodeDecoder(dec)
	err = code.decode(decoder)
	return
}

// TODO: byte code verifier
//    a. ensure branch offsets within bounds
//    b. ensure symbol indices within bounds
//    c. ensure constant indices within bounds
//    d. validate nested code objects (be careful to avoid infinite loops!)
