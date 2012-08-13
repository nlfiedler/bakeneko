//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

//
// Interpreter for our Scheme-like language, which turns a tree of
// expressions into a final, evaluated result.
//

type Environment struct {
	vars   map[Symbol]interface{} // mapping of variable names to values
	parent *Environment           // environment to delegate to
}

// NewEnvironment constructs an Environment with the given parent to
// provide a fallback for finding variables. The parent may be nil.
func NewEnvironment(parent *Environment) *Environment {
	e := new(Environment)
	e.vars = make(map[Symbol]interface{})
	e.parent = parent
	return e
}

// newGlobalEnvironment creates an environment with the default Scheme
// procedures and variables in place for use in a global context.
func newGlobalEnvironment() *Environment {
	ge := NewEnvironment(nil)
	// TODO: add the standard Scheme procedures and variables
	return ge
}

// globalEnv is the global environment
var globalEnv *Environment = newGlobalEnvironment()

// Find retrieves the value for the given symbol. If it is not found in
// this environment, the parent environment will be consulted. If no
// value is found, nil is returned.
func (e *Environment) Find(sym Symbol) interface{} {
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
func (e *Environment) Define(sym Symbol, val interface{}) {
	e.vars[sym] = val
}

// Set assigns a value to the given symbol, if and only if that symbol
// has a value already associated with it. If the symbol does not appear
// in this environment, the parent will be consulted.
func (e *Environment) Set(sym Symbol, val interface{}) *LispError {
	_, ok := e.vars[sym]
	if !ok {
		if e.parent != nil {
			return e.parent.Set(sym, val)
		}
		return NewLispError(EVARUNDEF, string(sym)+" undefined")
	} else {
		e.vars[sym] = val
	}
	return nil
}

// Callable represents a procedure that can be invoked. It has an
// environment in which the procedure was defined.
type Callable struct { // TODO: change to interface
	body   Pair        // procedure definition
	params Pair        // parameter list
	env    Environment // defining environment
}

// NewCallable constructs a new Callable with the given definition,
// defining environment, and parameters.
func NewCallable(body Pair, env Environment, params Pair) *Callable {
	return &Callable{body, params, env}
}

// Call invokes the given callable using the parameter values.
// Its environment is the one in which the procedure was defined.
func (c *Callable) Call(values Pair) (interface{}, *LispError) {
	// TODO: implement Callable.Call()
	return nil, nil
}

// Eval evaluates the given parse tree in the global context and returns
// the result.
func Eval(expr interface{}) (interface{}, *LispError) {
	return globalEnv.eval(expr)
}

// eval evaluates the given parse tree using a specific environment.
func (e *Environment) eval(expr interface{}) (interface{}, *LispError) {
	// TODO: implement eval
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
