//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

import (
	"code.google.com/p/goswat/container/vector"
)

//
// Interpreter for our Scheme-like language, which turns a tree of expressions
// into a final, evaluated result. This is modeled in the CESK-style as
// described at http://matt.might.net/articles/cesk-machines/
//

// theVoid is used for holding the place of an unassigned variable in an
// environment. Since all instances of Void are treated the same we can use a
// single instance for all.
var theVoid = Void(0)

// Environment encapsulates all of the visible symbols in the current scope.
// It may consist of parent environments, such that a symbol missing in this
// environment may be found in a parent.
type Environment interface {
	// Find looks for the given symbol in this environment, and the parent
	// environment, if one is associated with this environment.
	Find(sym Symbol) interface{}
	// Define sets the value for a symbol in this environment, creating
	// a new mapping if one does not already exist.
	Define(sym Symbol, val interface{})
	// Set assigns the value to the symbol in this environment, but only
	// if there is a previously defined value for that symbol.
	Set(sym Symbol, val interface{}) LispError
	// Eval evaluates the given thing in the context of this environment.
	// Typically expr is the result of parsing an expression.
	Eval(expr interface{}) (interface{}, LispError)
	// Parent returns the parent environment for this environment, or nil
	// if this is the global environment.
	Parent() Environment
}

// environment is an implementation of the Environment interface, which
// supports mapping symbols to values, as well as delegating to a parent when
// a symbol is not found in this environment.
type environment struct {
	vars   map[Symbol]interface{} // mapping of variable names to values
	parent Environment            // environment to delegate to
}

// NewEnvironment constructs an Environment with the given parent to provide a
// fallback for finding variables. The parent may be nil.
func NewEnvironment(parent Environment) Environment {
	e := new(environment)
	e.vars = make(map[Symbol]interface{})
	e.parent = parent
	return e
}

// newGlobalEnvironment creates an environment with the default Scheme
// procedures and variables in place for use in a global context.
func newGlobalEnvironment() Environment {
	ge := NewEnvironment(nil)
	// TODO: add the standard Scheme procedures and variables
	// TODO: define the "stdin" and "stdout" streams as variables
	return ge
}

// globalEnv is the global environment which contains all of the built-in
// functions, and is used for defining macros.
var globalEnv Environment = newGlobalEnvironment()

// Find retrieves the value for the given symbol. If it is not found in this
// environment, the parent environment will be consulted. If no value is
// found, nil is returned.
func (e *environment) Find(sym Symbol) interface{} {
	val, ok := e.vars[sym]
	if !ok {
		if e.parent != nil {
			return e.parent.Find(sym)
		}
		return nil
	}
	return val
}

// Define assigns the given value to the symbol in this environment.
func (e *environment) Define(sym Symbol, val interface{}) {
	e.vars[sym] = val
}

// Set assigns a value to the given symbol, if and only if that symbol
// has a value already associated with it. If the symbol does not appear
// in this environment, the parent will be consulted.
func (e *environment) Set(sym Symbol, val interface{}) LispError {
	_, ok := e.vars[sym]
	if !ok {
		if e.parent != nil {
			return e.parent.Set(sym, val)
		}
		return NewLispErrorf(ESYMBOL, "symbol '%v' not yet defined", val)
	} else {
		e.vars[sym] = val
	}
	return nil
}

// Parent returns the parent environment, or nil if this is the global
// environment.
func (e *environment) Parent() Environment {
	return e.parent
}

// Lambda is a function body and its set of parameters.
type Lambda struct {
	body   Pair // procedure definition
	params Pair // formal parameter list
}

// NewLambda constructs a new Lambda for the given function body and parameters.
func NewLambda(body, params Pair) *Lambda {
	return &Lambda{body, params}
}

// Closure represents a procedure that can be invoked. It has an associated
// environment in which the procedure was defined, and any evaluations will be
// done in the context of that environment.
type Closure interface {
	// Invoke this closure with the given arguments and return the result,
	// along with any error.
	Invoke(values Pair) (interface{}, LispError)
}

// closure is an implementation of the Closure interface. It consists of a
// Lambda and an environment in which to invoke the closure.
type closure struct {
	*Lambda             // function to be invoked
	env     Environment // defining environment
}

// NewClosure constructs a new Closure with the given definition, defining
// environment, and parameters.
func NewClosure(body Pair, env Environment, params Pair) Closure {
	lam := NewLambda(body, params)
	return &closure{lam, env}
}

// Invoke evaluates this closure using the parameter values. Its environment
// is the one in which the procedure was defined, and it will be evaluated in
// a new environment in which the parameters are bound to the given values.
func (c *closure) Invoke(values Pair) (interface{}, LispError) {
	// TODO: support arbitrary numbers of arguments (e.g. (list 1 2 3 ...))
	if c.params.Len() != values.Len() {
		str := c.params.String()
		return nil, NewLispErrorf(EARGUMENT, "wrong number of arguments for %v", str)
	}
	env := NewEnvironment(c.env)
	// map the symbols in c.params to given values, storing in env
	var names interface{} = c.params
	var valuse interface{} = values
	for names != nil {
		name := Car(names)
		sym, ok := name.(Symbol)
		if !ok {
			// parser should have handled this already
			return nil, NewLispErrorf(EARGUMENT, "name %s is not a symbol", name)
		}
		value := Car(valuse)
		env.Define(sym, value)
		names = Cdr(names)
		valuse = Cdr(valuse)
	}
	return env.Eval(c.body)
}

// Interpret parses the given Scheme expression, evaluates each of the top-
// level elements, returning the result.
func Interpret(prog string) (interface{}, LispError) {
	// TODO: parse the program into a Pair chain
	// TODO: construct the "halt" continuation which awaits the program result
	// TODO: the result of the halt continuation is returned to the caller
	// TODO: establish program counter ("control") starting at beginning
	// TODO: Eval() each (top-level) program element
	return nil, nil
}

// TODO: need a program counter (i.e. "control" in CESK)
//     * during parsing, store program elements in Pair chain to avoid parsing again
//     * control holds lambda ref and ref of Pair in lambda being run
//     * after each step, move control to Pair.Rest()
//     * when lambda pairs are exhausted, pop the frame from the stack
// TODO: need a "frame" to hold associated environment and "control"
// TODO: need a continuation, a stack of frames; each func call adds a new frame
// TODO: root continuation is referred to as "halt", which waits for program to finish

// TODO: some of what is below may be right; use formal definition to derive better functions

// frame represents a single entry in the continuation, or stack of frames.
type frame struct {
	env Environment // env is this frame's environment
	//head Pair        // head references to the beginning of the s-expression
	curr Pair // curr references the current element being evaluated
}

type continuation struct {
	head   Pair
	curr   Pair
	frames vector.Vector
}

// step moves execution forward until this continuation is exhausted, at which
// point the result of the last element is returned.
func (c *continuation) step() (result interface{}, err LispError) {
	// while there is something to evaluate...
	for c.curr != nil && c.curr.Len() > 0 {
		// evaluate it and see what happened
		result, err = Eval(c.curr)
		if err != nil {
			// exit early upon error
			break
		}
		// move forward along the list of elements
		next := c.curr.Rest()
		if np, ok := next.(Pair); ok {
			c.curr = np
		} else {
			c.curr = nil
		}
	}
	c.frames.Pop()
	// the last element evaluate is the result
	return result, err
}

// Eval evaluates the given s-expression in the global context and returns
// the result. This is used primarily for defining macros in the parser.
func Eval(expr interface{}) (interface{}, LispError) {
	return globalEnv.Eval(expr)
}

// eval evaluates the given s-expression using a specific environment.
func (e *environment) Eval(expr interface{}) (interface{}, LispError) {
	// TODO: implement Eval
	// This is essentially the "step" function in a CESK machine.
	// TODO: needs the continuation, no?
	// while True:
	// 	if isa(x, Symbol):       # variable reference
	// 	    return env.find(x)[x]
	// 	elif not isa(x, list):   # constant literal
	// 	    return x
	// 	elif x[0] is _quote:     # (quote exp)
	// 	    (_, exp) = x
	// 	    return exp
	// 	elif x[0] is _if:        # (if test conseq alt)
	// 	    (_, test, conseq, alt) = x
	// 	    x = (conseq if eval(test, env) else alt)
	// 	elif x[0] is _set:       # (set! var exp)
	// 	    (_, var, exp) = x
	// 	    env.find(var)[var] = eval(exp, env)
	// 	    return None
	// 	elif x[0] is _define:    # (define var exp)
	// 	    (_, var, exp) = x
	// 	    env[var] = eval(exp, env)
	// 	    return None
	// 	elif x[0] is _lambda:    # (lambda (var*) exp)
	// 	    (_, vars, exp) = x
	// TODO: a Lambda "evaluates" to a Closure
	// 	    return Procedure(vars, exp, env)
	// 	elif x[0] is _begin:     # (begin exp+)
	// 	    for exp in x[1:-1]:
	// 		eval(exp, env)
	// 	    x = x[-1]
	// 	else:                    # (proc exp*)
	// 	    exps = [eval(exp, env) for exp in x]
	// 	    proc = exps.pop(0)
	// 	    if isa(proc, Procedure):
	// 		x = proc.exp
	// 		env = Env(proc.parms, exps, proc.env)
	// 	    else:
	// 		return proc(*exps)
	return nil, nil
}

// TODO: write define, set!, let, letrec, define-syntax
// TODO: for let and letrec, see http://matt.might.net/articles/cesk-machines/
