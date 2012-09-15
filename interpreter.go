//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

// import (
// 	// "code.google.com/p/goswat/container/vector"
// )

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
}

// environment is an implementation of the Environment interface, which
// supports mapping symbols to values, as well as delegating to a parent when
// a symbol is not found in this environment.
type environment struct {
	writable bool                   // true if environment allows changes
	vars     map[Symbol]interface{} // mapping of variable names to values
	parent   Environment            // environment to delegate to
}

// NewEnvironment constructs an Environment with the given parent to provide a
// fallback for finding variables. The parent may be nil.
func NewEnvironment(parent Environment) Environment {
	e := new(environment)
	e.writable = true
	e.vars = make(map[Symbol]interface{})
	e.parent = parent
	return e
}

// NewRestrictedEnvironment constructs an Environment with the given set of
// mappings. The environment cannot be modified.
func NewRestrictedEnvironment(parent Environment, mapping map[Symbol]interface{}) Environment {
	e := new(environment)
	e.writable = false
	e.vars = mapping
	e.parent = parent
	return e
}

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
	if e.writable {
		e.vars[sym] = val
	}
}

// Set assigns a value to the given symbol, if and only if that symbol
// has a value already associated with it. If the symbol does not appear
// in this environment, the parent will be consulted.
func (e *environment) Set(sym Symbol, val interface{}) LispError {
	if !e.writable {
		return NewLispError(ESUPPORT, "restricted environment not writable")
	}
	if _, ok := e.vars[sym]; ok {
		e.vars[sym] = val
		return nil
	} else if e.parent != nil {
		return e.parent.Set(sym, val)
	}
	return NewLispErrorf(ESYMBOL, "symbol '%v' not yet defined", sym)
}

// newNullEnvironment constructs the "null" environment as defined in R5RS.
func newNullEnvironment() Environment {
	mapping := make(map[Symbol]interface{})
	// TODO: add the syntactic bindings for all syntactic keywords in r5rs
	ne := NewRestrictedEnvironment(nil, mapping)
	return ne
}

// nullEnvironment is the "null" environment in Scheme.
var theNullEnvironment Environment = newNullEnvironment()

// newReportEnvironment creates an environment with the default Scheme
// procedures and variables in place for use in a global context.
func newReportEnvironment() Environment {
	mapping := make(map[Symbol]interface{})
	// TODO: add the standard bindings defined in r5rs
	ge := NewRestrictedEnvironment(theNullEnvironment, mapping)
	return ge
}

// reportEnv is the global environment which contains all of the built-in
// functions, and is used for defining macros.
var theReportEnv Environment = newReportEnvironment()

// lambda is a function body and its set of parameters.
type lambda struct {
	body   Pair // procedure definition
	params Pair // formal parameter list
}

// NewLambda constructs a new lambda for the given function body and parameters.
func NewLambda(body, params Pair) *lambda {
	return &lambda{body, params}
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
// lambda and an environment in which to invoke the closure.
type closure struct {
	*lambda             // function to be invoked
	env     Environment // defining environment
}

// NewClosure constructs a new Closure with the given definition, defining
// environment, and parameters.
func NewClosure(body, params Pair, env Environment) Closure {
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
	return Eval(c.body, env)
}

// Interpret parses the given Scheme expression, evaluates each of the top-
// level elements, returning the result.
func Interpret(prog string) (interface{}, LispError) {
	// this is the "inject" step in CESK
	pair, err := parse(prog)
	if err != nil {
		return nil, err
	}
	env := NewEnvironment(theReportEnv)
	// this is the "step" in CESK
	return Eval(pair, env)
}

// isTrue determines if the given thing represents a "true" value in Scheme.
// Only #f counts as false in Scheme, everything else is treated as true.
func isTrue(test interface{}) bool {
	if b, ok := test.(Boolean); ok {
		return b.Value()
	}
	return true
}

// Eval evaluates the given s-expression using a specific environment.
func Eval(expr interface{}, env Environment) (interface{}, LispError) {
	// TODO: implement Eval, which is effectively the 'step' in CESK
	for {
		if sym, ok := expr.(Symbol); ok {
			// symbolic reference
			return env.Find(sym), nil
		}
		pair, is_pair := expr.(Pair)
		if !is_pair {
			// atom
			return expr, nil
		}
		first := pair.First()
		if sym, issym := first.(Symbol); issym {
			if sym == quoteSym {
				// (quote exp)
				return pair.Rest(), nil
			} else if sym == ifSym {
				// (if test conseq alt)
				test := pair.Second()
				okay, err := Eval(test, env)
				if err != nil {
					return nil, err
				}
				if isTrue(okay) {
					expr = pair.Third()
				} else {
					expr = Cxr("cadddr", pair)
				}
			} else if sym == setSym {
				// (set! var exp)
				exp := pair.Third()
				val, err := Eval(exp, env)
				if err != nil {
					return nil, err
				}
				name := pair.Second()
				if ns, ok := name.(Symbol); ok {
					env.Set(ns, val)
				} else {
					// this should _not_ happen
					panic(ParserError)
				}
				return nil, nil
			} else if sym == defineSym {
				// (define var exp)
				exp := pair.Third()
				val, err := Eval(exp, env)
				if err != nil {
					return nil, err
				}
				name := pair.Second()
				if ns, ok := name.(Symbol); ok {
					env.Define(ns, val)
				} else {
					// this should _not_ happen
					panic(ParserError)
				}
				return nil, nil
			} else if sym == lambdaSym {
				// 	elif x[0] is _lambda:    # (lambda (var*) exp)
				// 	    (_, vars, exp) = x
				// TODO: a Lambda "evaluates" to a Closure
				// 	    return Procedure(vars, exp, env)
			} else if sym == beginSym {
				// (begin exp+)
				rest := pair.Rest()
				var result interface{}
				var err LispError
				if next, ok := rest.(Pair); ok {
					for next.Len() > 0 {
						result, err = Eval(next.First(), env)
						if err != nil {
							return nil, err
						}
						rest = next.Rest()
						next, _ = rest.(Pair)
					}
				}
				if rest != nil {
					result, err = Eval(rest, env)
					if err != nil {
						return nil, err
					}
				}
				return result, nil
			} else {
				// 	else:                    # (proc exp*)
				// 	    exps = [eval(exp, env) for exp in x]
				// 	    proc = exps.pop(0)
				// 	    if isa(proc, Procedure):
				// TODO: invoke the closure inside the loop to support tail recursion
				// 		x = proc.exp
				// 		env = Env(proc.parms, exps, proc.env)
				// 	    else:
				// 		return proc(*exps)
			}
		} else {
			// TODO: what to do with nested lists? e.g. ((if #t "true" "false"))
			return nil, NewLispErrorf(EARGUMENT, "nested list: %v", pair)
		}
	}
	return nil, nil
}

// TODO: write define, set!, let, letrec, define-syntax
// TODO: for let and letrec, see http://matt.might.net/articles/cesk-machines/

// Not really convinced we need all this, seems like lispy.py-style Eval() is enough.
//     * during parsing, store program elements in Pair chain to avoid parsing again
//     * control holds lambda ref and ref of Pair in lambda being run
//     * after each step, move control to Pair.Rest()
//     * when an atom is encountered, return as the result of the continuation
// // frame represents a single entry in the continuation, or stack of frames.
// type frame struct {
// 	env  Environment // env is this frame's environment
// 	curr Pair        // curr references the current element being evaluated
// }
// type continuation struct {
// 	head   Pair
// 	curr   Pair
// 	frames vector.Vector
// }
// // step moves execution forward until this continuation is exhausted, at which
// // point the result of the last element is returned.
// func (c *continuation) step() (result interface{}, err LispError) {
// 	// TODO: get environment from current frame
// 	env := reportEnv
// 	// while there is something to evaluate...
// 	for c.curr.Len() > 0 {
// 		// evaluate it and see what happened
// 		result, err = env.Eval(c.curr)
// 		if err != nil {
// 			// exit early upon error
// 			break
// 		}
// 		// TODO: if result is an atomic expression, 'return' it to the current continuation
// 		// TODO: if result is a procedure call (i.e. closure), call Invoke() on it
// 		// move forward along the list of elements
// 		next := c.curr.Rest()
// 		if np, ok := next.(Pair); ok {
// 			c.curr = np
// 		} else {
// 			c.curr = nil
// 		}
// 	}
// 	c.frames.Pop()
// 	// the last element evaluated is the result
// 	return result, err
// }
