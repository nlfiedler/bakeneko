//
// Copyright 2014 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

//
// byte code interpreter (VM) borrowed from Eli Bendersky's bobscheme
// (https://github.com/eliben/bobscheme)
//

// ExecutionFrame represents the a frame of execution of a code object. It
// has a program counter that advances as each instruction in the code
// object is requested (and presumably executed).
type ExecutionFrame interface {
	// NextInstruction returns the next instruction and advances the
	// program counter. When the end of the code object is reached, an
	// instruction whose code is OP_NOP is returned.
	NextInstruction() Instruction
	// Jump moves the program counter to the given byte code offset.
	Jump(pos uint)
	// Environment returns the environment instance for this frame.
	Environment() Environment
	// Constant retrieves the constant value at the index within the
	// constants table contained in the code object in this frame.
	Constant(pos uint) interface{}
	// Symbol retrieves the symbol at the index within the symbols table
	// contained in the code object in this frame.
	Symbol(pos uint) Symbol
}

// executionFrame is the internal implementation of an ExecutionFrame.
type executionFrame struct {
	code CodeObject  // the code being executed
	pc   uint        // program counter, offset into code object
	env  Environment // Scheme environment in which code is executed
}

// NewExecutionFrame constructs an instance of ExecutionFrame.
func NewExecutionFrame(code CodeObject, env Environment) ExecutionFrame {
	ef := new(executionFrame)
	ef.code = code
	ef.env = env
	return ef
}

func (ef *executionFrame) NextInstruction() Instruction {
	if ef.pc >= ef.code.CodeLen() {
		return NewInstruction(OP_NOP, 0)
	}
	instr := ef.code.GetInstruction(ef.pc)
	ef.pc++
	return instr
}

func (ef *executionFrame) Jump(pos uint) {
	// Allow seemingly jumping out of bounds to jump past the end of the
	// object code and bring execution to an end.
	ef.pc = pos
}

func (ef *executionFrame) Environment() Environment {
	return ef.env
}

func (ef *executionFrame) Constant(pos uint) interface{} {
	return ef.code.GetConstant(pos)
}

func (ef *executionFrame) Symbol(pos uint) Symbol {
	return ef.code.GetSymbol(pos)
}

// byteClosure represents a compiled procedure that can be invoked. It has an
// associated environment in which the procedure was defined, and any
// evaluations will be done in the context of that environment.
type byteClosure struct {
	body CodeObject     // procedure definition
	env  Environment    // defining environment
	vm   VirtualMachine // for running the closure
}

// NewByteClosure returns an implementation of Closure that applies to a
// compiled procedure, bound to the given environment and virtual machine.
func NewByteClosure(body CodeObject, env Environment, vm VirtualMachine) Closure {
	return &byteClosure{body, env, vm}
}

func (bc *byteClosure) Bind(values Pair) (Environment, LispError) {
	return BindArguments(bc.env, bc.body.Arguments(), values)
}

func (bc *byteClosure) Body() interface{} {
	return bc.body
}

func (bc *byteClosure) Apply(env Environment) (interface{}, LispError) {
	return bc.vm.RunClosure(bc.body, env)
}

// VirtualMachine contains the state of a virtual machine that executes
// compiled Scheme code.
type VirtualMachine interface {
	// Run evaluates the compiled byte code of a Scheme program within the
	// context of a virtual machine and returns the result, or an error if
	// evaluation failed for any reason.
	Run(code CodeObject, env Environment) (interface{}, LispError)
	// RunClosure wraps the Run() function with an additional frame on the
	// stack since the closure will pop a frame when it returns. When applied
	// via built-in procedures, there is no OP_CALL executed and thus no frame
	// pushed onto the stack.
	RunClosure(code CodeObject, env Environment) (interface{}, LispError)
}

// virtualMachine is the internal implementation of a VirtualMachine.
type virtualMachine struct {
	values []interface{}    // stack of argument values
	frames []ExecutionFrame // stack of execution frames
	empty  CodeObject       // empty code object to force closures to end
}

// NewVirtualMachine constructs an instance of VirtualMachine.
func NewVirtualMachine() VirtualMachine {
	vm := new(virtualMachine)
	vm.values = make([]interface{}, 0)
	vm.frames = make([]ExecutionFrame, 0)
	// Construct an artificial code object to which all closures "return" when
	// they are finished, so the Run() will terminate appropriately.
	vm.empty = emptyCodeObject("closure")
	return vm
}

// popValue removes a single value from the argument stack and returns it.
func (vm *virtualMachine) popValue() interface{} {
	var val interface{}
	val, vm.values = vm.values[len(vm.values)-1], vm.values[:len(vm.values)-1]
	return val
}

// popFrame removes the most recently pushed frame and returns it.
func (vm *virtualMachine) popFrame() ExecutionFrame {
	var frame ExecutionFrame
	frame, vm.frames = vm.frames[len(vm.frames)-1], vm.frames[:len(vm.frames)-1]
	return frame
}

func (vm *virtualMachine) RunClosure(code CodeObject, env Environment) (interface{}, LispError) {
	// add a frame that forces a return from the closure
	vm.frames = append(vm.frames, NewExecutionFrame(vm.empty, env))
	return vm.Run(code, env)
}

func (vm *virtualMachine) Run(code CodeObject, env Environment) (interface{}, LispError) {
	frame := NewExecutionFrame(code, env)
MainLoop:
	for {
		instr := frame.NextInstruction()
		opcode := instr.Code()
		switch opcode {
		case OP_NOP:
			// we've reached the end of the code object
			if len(vm.frames) == 0 {
				break MainLoop
			} else {
				return nil, NewLispErrorf(EINTERNAL, "reached end of code: %s", code.Name())
			}
		case OP_CONST:
			// retrieve the constant's value and push onto the values stack
			val := frame.Constant(instr.Argument())
			vm.values = append(vm.values, val)
		case OP_LOADVAR:
			// retrieve the symbol's assigned value and push onto the values stack
			sym := frame.Symbol(instr.Argument())
			val := frame.Environment().Find(sym)
			if val == nil {
				return nil, NewLispErrorf(EARGUMENT, "unbound variable: %s", sym)
			}
			vm.values = append(vm.values, val)
		case OP_STOREVAR:
			// assign value to an existing symbol in some environment
			val := vm.popValue()
			sym := frame.Symbol(instr.Argument())
			err := frame.Environment().Set(sym, val)
			if err != nil {
				return nil, err
			}
		case OP_DEFVAR:
			// assign value to an unbound symbol in this environment
			val := vm.popValue()
			sym := frame.Symbol(instr.Argument())
			frame.Environment().Define(sym, val)
		case OP_POP:
			if len(vm.values) > 0 {
				vm.popValue()
			}
		case OP_JUMP:
			// unconditionally move the program counter
			frame.Jump(instr.Argument())
		case OP_FJUMP:
			// test the predicate value on the stack and jump if false
			test := vm.popValue()
			if !isTrue(test) {
				frame.Jump(instr.Argument())
			}
		case OP_FUNCTION:
			funco := frame.Constant(instr.Argument()).(CodeObject)
			clos := NewByteClosure(funco, frame.Environment(), vm)
			vm.values = append(vm.values, clos)
		case OP_CALL:
			// arguments are in reverse order on the argument stack
			proc := vm.popValue()
			arg_count := instr.Argument()
			var args Pair = theEmptyList
			for arg_count > 0 {
				args = Cons(vm.popValue(), args)
				arg_count--
			}
			if builtin, ok := proc.(Procedure); ok {
				result, err := builtin.Call(args.ToSlice())
				if err != nil {
					return nil, err
				}
				vm.values = append(vm.values, result)
			} else if clos, ok := proc.(Closure); ok {
				proc_env, err := clos.Bind(args)
				if err != nil {
					return nil, err
				}
				vm.frames = append(vm.frames, frame)
				body := clos.Body().(CodeObject)
				frame = NewExecutionFrame(body, proc_env)
			} else {
				return nil, NewLispErrorf(ESUPPORT, "The object %v is not applicable.", proc)
			}
		case OP_RETURN:
			frame = vm.popFrame()
		default:
			return nil, NewLispErrorf(EINTERNAL, "unknown opcode: %s", opcode)
		}
	}
	// assemble whatever is left over and return it
	var result interface{}
	switch l := len(vm.values); true {
	case l > 1:
		result = NewList(vm.values...)
	case l == 1:
		result = vm.values[0]
	default:
		result = nil
	}
	// wipe out the stored values lest they accumulate indefinitely
	vm.values = vm.values[0:0]
	return result, nil
}

// EvaluateCode evaluates the compiled byte code of a Scheme program within
// the given environment and returns the result, or an error if evaluation
// failed for any reason.
func EvaluateCode(code CodeObject, env Environment) (interface{}, LispError) {
	vm := NewVirtualMachine()
	return vm.Run(code, env)
}
