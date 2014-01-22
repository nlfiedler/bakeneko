//
// Copyright 2014 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	"fmt"
	"sort"
)

//
// byte code compiler borrowed from Eli Bendersky's bobscheme
// (https://github.com/eliben/bobscheme)
//

// TODO: serialized data needs to have a magic number (4 bytes) at the start of the file
// TODO: serialized data needs to have a major.minor version of the byte code
// TODO: serialized data basically looks like this:
//   version          uint32      // should incorporate Go version if we use gob for anything
//   source_name      string      // path/name of source file
//   source_lines     [...]???    // list of byte-code-offsets => line-numbers
//   code_length      uint32      // number of bytes of byte codes
//   byte_code        [...]byte   // the byte codes
//   constants_length uint32      // number of bytes of serialized constants
//   constants        [...]byte   // serialized Go objects; probably need a type code preceding each value
//   symbols_length   uint32      // number of bytes of serialized strings
//   symbols          [...]string // serialized Go strings

// TODO: derived expressions (using Scheme and macros); see R7RS 7.3
//  cond
//  case
//  and
//  or
//  when
//  unless
//  let
//  let*
//  letrec
//  letrec*
//  let-values
//  let*-values
//  define-values
//  do
//  delay-force
//  delay
//  make-promise
//  make-parameter
//  parameterize
//  guard
//  case-lambda
//  cond-expand

// TODO: see chibi-scheme for more good stuff, all BSD licensed
// TODO: see http://srfi.schemers.org for SRFI implementations

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
	return &instruction{code, arg}
}

func (i *instruction) Code() Opcode {
	return i.code
}

func (i *instruction) Argument() uint {
	return i.arg
}

func (i *instruction) String() string {
	return fmt.Sprintf("<%s: %d>", i.code, i.arg)
}

// CodeObject encapsulates a compiled script or procedure.
type CodeObject interface {
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
}

// byteLinePair associates a byte code offset with a line number.
type byteLinePair struct {
	offset uint // byte code offset
	line   int  // line number
}

type byteLinePairs []byteLinePair

func (a byteLinePairs) Len() int           { return len(a) }
func (a byteLinePairs) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a byteLinePairs) Less(i, j int) bool { return a[i].offset < a[j].offset }

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
	syms []Symbol, lines map[int]uint) CodeObject {
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
	bylines := make([]byteLinePair, 0, len(lines))
	for k, v := range lines {
		bylines = append(bylines, byteLinePair{v, k})
	}
	sort.Sort(byteLinePairs(bylines))
	bc.bylines = bylines
	return bc
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
	for _, blp := range bc.bylines {
		if blp.offset >= pos {
			return blp.line
		}
	}
	return 0
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

// Compiler constructs a CodeObject one piece at a time and assembles the
// final result on demand.
type Compiler interface {
	// AddInstruction adds the given instruction to the accumulated results.
	AddInstruction(code Opcode, arg interface{})
	// FindOrAddConstant ensures the constants table holds the given value,
	// returning the offset for that constant.
	FindOrAddConstant(con interface{}) uint
	// FindOrAddSymbol ensures the symbols table holds the given name,
	// returning the offset for that symbol.
	FindOrAddSymbol(sym Symbol) uint
	// Compile produces the byte codes for the given expression
	Compile(expr interface{}) LispError
	// Assemble pulls the final result into a cohesive set of instructions
	// in which all intermediate values have been stripped.
	Assemble(name string, args Pair) CodeObject
}

// compiler is the internal implementation of a Compiler.
type compiler struct {
	codes     []Opcode      // sequence of opcodes for the code object under construction
	arguments []interface{} // arguments, one for each opcode; may be nil
	constants []interface{} // table of constant values (typically Atoms)
	symbols   []Symbol      // table of symbol names
	labels    uint          // number of labels so far, used to generate unique labels
	lineno    map[int]uint  // map of line numbers to smallest byte code offset
}

// NewCompiler constructs an instance of Compiler.
func NewCompiler() Compiler {
	comp := new(compiler)
	comp.codes = make([]Opcode, 0)
	comp.arguments = make([]interface{}, 0)
	comp.constants = make([]interface{}, 0)
	comp.symbols = make([]Symbol, 0)
	comp.lineno = make(map[int]uint)
	return comp
}

func (c *compiler) AddInstruction(code Opcode, arg interface{}) {
	c.codes = append(c.codes, code)
	c.arguments = append(c.arguments, arg)
}

func (c *compiler) FindOrAddConstant(con interface{}) uint {
	for idx, val := range c.constants {
		if val == con {
			return uint(idx)
		}
	}
	idx := len(c.constants)
	c.constants = append(c.constants, con)
	return uint(idx)
}

func (c *compiler) FindOrAddSymbol(sym Symbol) uint {
	for idx, val := range c.symbols {
		if val == sym {
			return uint(idx)
		}
	}
	idx := len(c.symbols)
	c.symbols = append(c.symbols, sym)
	return uint(idx)
}

func (c *compiler) createLabel() Instruction {
	// starting from zero, go up one each time
	c.labels++
	return NewInstruction(OP_LABEL, uint(c.labels))
}

// addLineNumber adds line number information to the pending CodeObject for
// the purpose of error reporting.
func (c *compiler) addLineNumber(expr interface{}) {
	if pair, is_pair := expr.(Pair); is_pair {
		expr = pair.First()
	}
	if loco, is_loc := expr.(Locatable); is_loc {
		line, _ := loco.Location()
		if bite, have := c.lineno[line]; have {
			if uint(len(c.codes)) < bite {
				c.lineno[line] = uint(len(c.codes))
			}
		} else {
			c.lineno[line] = uint(len(c.codes))
		}
	}
}

func (c *compiler) Compile(expr interface{}) LispError {
	c.addLineNumber(expr)
	// start with the primitive expressions (R7RS 4.1)
	switch obj := expr.(type) {
	case Symbol:
		// symbol reference
		c.AddInstruction(OP_LOADVAR, obj)
	case Pair:
		// most of the time they're pairs
		length := obj.Len()
		if length == 0 {
			// empty list
			c.AddInstruction(OP_CONST, obj)
			break
		}
		first := obj.First()
		// assume it is one of the primitives until we learn otherwise
		application := false
		if sym, issym := first.(Symbol); issym {
			if atomsEqual(sym, quoteSym) {
				// (quote exp)
				c.AddInstruction(OP_CONST, obj.Second())
			} else if atomsEqual(sym, ifSym) {
				// (if test conseq alt)
				labelElse := c.createLabel()
				labelAfterElse := c.createLabel()
				err := c.Compile(obj.Second())
				if err != nil {
					return err
				}
				c.AddInstruction(OP_FJUMP, labelElse)
				err = c.Compile(obj.Third())
				if err != nil {
					return err
				}
				c.AddInstruction(OP_JUMP, labelAfterElse)
				c.AddInstruction(OP_LABEL, labelElse.Argument())
				err = c.Compile(Cxr("cadddr", obj))
				if err != nil {
					return err
				}
				c.AddInstruction(OP_LABEL, labelAfterElse.Argument())
			} else if atomsEqual(sym, setSym) {
				// (set! var exp)
				err := c.Compile(obj.Third())
				if err != nil {
					return err
				}
				name := obj.Second()
				if ns, ok := name.(Symbol); ok {
					c.AddInstruction(OP_STOREVAR, ns)
				} else {
					return NewLispErrorf(EARGUMENT, "name %v not a symbol", name)
				}
			} else if atomsEqual(sym, defineSym) {
				// (define name exp)
				err := c.Compile(obj.Third())
				if err != nil {
					return err
				}
				name := obj.Second().(Symbol)
				// If the value is a lambda, assign the name for error reporting.
				if c.codes[len(c.codes)-1] == OP_FUNCTION {
					co := c.arguments[len(c.arguments)-1].(CodeObject)
					co.setName(name.String())
				}
				c.AddInstruction(OP_DEFVAR, name)
			} else if atomsEqual(sym, lambdaSym) {
				// (lambda (var*) exp)
				vars := obj.Second()
				if vlist, ok := vars.(Pair); ok {
					co, err := CompileLambda(vlist, obj.Third())
					if err != nil {
						return err
					}
					c.AddInstruction(OP_FUNCTION, co)
				} else {
					return NewLispErrorf(EARGUMENT, "lambda arguments wrong type: %v", vars)
				}
			} else if atomsEqual(sym, beginSym) {
				// (begin exp+)
				iter := NewPairIterator(obj)
				iter.Next()
				for iter.HasNext() {
					err := c.Compile(iter.Next())
					if err != nil {
						return err
					}
					// last entry does _not_ get a POP
					if iter.HasNext() {
						c.AddInstruction(OP_POP, nil)
					}
				}
			} else {
				application = true
			}
		} else {
			application = true
		}
		if application {
			// compile all of the arguments in left-to-right order
			iter := NewPairIterator(obj)
			iter.Next()
			var arg_count uint = 0
			for iter.HasNext() {
				err := c.Compile(iter.Next())
				if err != nil {
					return err
				}
				arg_count++
			}
			c.Compile(obj.First())
			c.AddInstruction(OP_CALL, arg_count)
		}
	default:
		// must be an atom
		c.AddInstruction(OP_CONST, expr)
	}
	return nil
}

func (c *compiler) Assemble(name string, args Pair) CodeObject {
	// Compute the label offsets, not counting the labels themselves toward
	// the final result as they will be pruned.
	offsets := make(map[uint]uint)
	var offset uint = 0
	for idx, code := range c.codes {
		if code == OP_LABEL {
			label := c.arguments[idx].(uint)
			offsets[label] = offset
		} else {
			offset++
		}
	}

	// codes will be a little oversized because the intermediate code has
	// the label placeholders, while the final result will not
	codes := make([]Instruction, 0, len(c.codes))
	for idx, code := range c.codes {
		arg_value := c.arguments[idx]
		var arg_num uint = 0
		switch code {
		case OP_LABEL:
			// labels do not contribute to the final result
			continue
		case OP_FJUMP, OP_JUMP:
			instr := arg_value.(Instruction)
			arg_num = offsets[instr.Argument()]
		case OP_CONST, OP_FUNCTION:
			arg_num = c.FindOrAddConstant(arg_value)
		case OP_LOADVAR, OP_STOREVAR, OP_DEFVAR:
			arg_num = c.FindOrAddSymbol(arg_value.(Symbol))
		case OP_CALL:
			arg_num = arg_value.(uint)
		}
		codes = append(codes, NewInstruction(code, arg_num))
	}
	return NewCodeObject(name, args, codes, c.constants, c.symbols, c.lineno)
}

// Compile converts the parsed and expanded s-expression into a CodeObject
// which can be executed by the stack-based byte code interpreter.
func Compile(name string, expr interface{}) (CodeObject, LispError) {
	compiler := NewCompiler()
	err := compiler.Compile(expr)
	if err != nil {
		return nil, err
	}
	return compiler.Assemble(name, theEmptyList), nil
}

// CompileLambda compiles the given expression assuming that it is a
// lambda expression which accepts the given arguments.
func CompileLambda(args Pair, expr interface{}) (CodeObject, LispError) {
	compiler := NewCompiler()
	err := compiler.Compile(expr)
	if err != nil {
		return nil, err
	}
	compiler.AddInstruction(OP_RETURN, nil)
	return compiler.Assemble("lambda", args), nil
}

// CompileString parses, expands, and compiles the given program.
func CompileString(name string, prog string) (CodeObject, LispError) {
	// Thus begins the "inject" operation in CESK, Byte Code Edition (TM).
	var err LispError
	parser := NewParser()
	parsed, err := parser.Parse(prog)
	if err != nil {
		return nil, err
	}
	// By ensuring that the program is wrapped inside a (begin ...) we
	// create what constitutes the "halt" continuation, as well as the
	// "step" function.
	if parsed.Len() >= 1 {
		if sym, ok := parsed.First().(Symbol); !ok || !atomsEqual(sym, beginSym) {
			parsed = Cons(beginSym, parsed)
		}
	}
	expanded, err := parser.Expand(parsed)
	if err != nil {
		return nil, err
	}
	return Compile(name, expanded)
}
