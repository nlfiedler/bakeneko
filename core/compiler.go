//
// Copyright 2014 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	"sort"
)

//
// byte code compiler borrowed from Eli Bendersky's bobscheme
// (https://github.com/eliben/bobscheme)
//

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
	codes     []Opcode       // sequence of opcodes for the code object under construction
	arguments []interface{}  // arguments, one for each opcode; may be nil
	constants []interface{}  // table of constant values (typically Atoms)
	symbols   []Symbol       // table of symbol names
	labels    uint           // number of labels so far, used to generate unique labels
	lines     []byteLinePair // byte offset and line number pairings
}

// NewCompiler constructs an instance of Compiler.
func NewCompiler() Compiler {
	comp := new(compiler)
	comp.codes = make([]Opcode, 0)
	comp.arguments = make([]interface{}, 0)
	comp.constants = make([]interface{}, 0)
	comp.symbols = make([]Symbol, 0)
	comp.lines = make([]byteLinePair, 0)
	return comp
}

func (c *compiler) AddInstruction(code Opcode, arg interface{}) {
	c.codes = append(c.codes, code)
	c.arguments = append(c.arguments, arg)
}

func (c *compiler) FindOrAddConstant(con interface{}) uint {
	con_a, con_is_atom := con.(Atom)
	for idx, val := range c.constants {
		if con_is_atom {
			if val_a, val_is_atom := val.(Atom); val_is_atom {
				if atomsEqual(con_a, val_a) {
					return uint(idx)
				}
			}
		}
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
		if atomsEqual(val, sym) {
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
	if pair, is_pair := expr.(Pair); is_pair && pair.Len() > 0 {
		expr = pair.First()
	}
	if loco, is_loc := expr.(Locatable); is_loc {
		offset := uint(len(c.codes))
		line, _ := loco.Location()
		c.lines = append(c.lines, byteLinePair{offset, line})
	}
}

func (c *compiler) Compile(expr interface{}) LispError {
	c.addLineNumber(expr)
	// start with the primitive expressions (R7RS 4.1)
	switch obj := expr.(type) {
	case Symbol:
		// symbol reference
		c.AddInstruction(OP_LOADVAR, obj)
	// TODO: use Sequence instead of Pair?
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
				iter := obj.Iterator()
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
			iter := obj.Iterator()
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
			if ws, ok := arg_value.(WireStripper); ok {
				arg_num = c.FindOrAddConstant(ws.StripLocation())
			} else {
				arg_num = c.FindOrAddConstant(arg_value)
			}
		case OP_LOADVAR, OP_STOREVAR, OP_DEFVAR:
			// strip the parser's location information
			sym := NewSymbol(arg_value.(Symbol).String())
			arg_num = c.FindOrAddSymbol(sym)
		case OP_CALL:
			arg_num = arg_value.(uint)
		}
		codes = append(codes, NewInstruction(code, arg_num))
	}

	// sort the byte-line pairings and remove duplicates
	lines := make([]byteLinePair, 0)
	if len(c.lines) > 0 {
		sort.Sort(byteLinePairs(c.lines))
		lines = append(lines, c.lines[0])
		prev_pair := c.lines[0]
		for _, line := range c.lines[1:] {
			if line.line > prev_pair.line && line.offset > prev_pair.offset {
				lines = append(lines, line)
				prev_pair = line
			}
		}
	}
	return NewCodeObject(name, args, codes, c.constants, c.symbols, lines)
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
	expanded, err := parser.Expand(wrapWithBegin(parsed))
	if err != nil {
		return nil, err
	}
	return Compile(name, expanded)
}
