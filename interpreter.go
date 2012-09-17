//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

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
	mapping[Symbol("append")] = NewBuiltin(builtinAppend)
	mapping[Symbol("cons")] = NewBuiltin(builtinCons)
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

// builtinProcFunc is the type of the function that implements a built-in
// Scheme procedure (e.g. builtinCons).
type builtinProcFunc func([]interface{}) (interface{}, LispError)

// Procedure represents a callable function in Scheme.
type Procedure interface {
	Call(values []interface{}) (interface{}, LispError)
}

// builtinProc is an implementation of Procedure for built-in functions.
type builtinProc struct {
	// builtin is the reference to the procedure implementation.
	builtin builtinProcFunc
}

// NewBuiltin constructs a Procedure for the given built-in function.
func NewBuiltin(f builtinProcFunc) Procedure {
	return &builtinProc{f}
}

// Call invokes the built-in procedure implementation and returns the result.
func (b *builtinProc) Call(values []interface{}) (interface{}, LispError) {
	return b.builtin(values)
}

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

// evalToSlice evaluates the elements of the pair and appends the results to a
// slice. The evaluation of the elements is made within the given environment.
func evalToSlice(pair Pair, env Environment) ([]interface{}, LispError) {
	results := make([]interface{}, 0)
	var elems interface{} = pair
	for elems != nil {
		thing := Car(elems)
		if thing != nil {
			result, err := Eval(thing, env)
			if err != nil {
				return nil, err
			}
			results = append(results, result)
		}
		elems = Cdr(elems)
	}
	return results, nil
}

// Eval evaluates the given s-expression using a specific environment and
// returns the results. This handles tail-call recursion and invoking
// procedures, both built-in and user-defined.
func Eval(expr interface{}, env Environment) (interface{}, LispError) {
	// this is effectively the 'step' function in CESK
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
		if pair.Len() == 0 {
			// empty list
			return pair, nil
		}
		first := pair.First()
		if sym, issym := first.(Symbol); issym {
			if sym == quoteSym {
				// (quote exp)
				return pair.Second(), nil
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
						thing := next.First()
						result, err = Eval(thing, env)
						if err != nil {
							return nil, err
						}
						rest = next.Rest()
						next, _ = rest.(Pair)
						if next == theEmptyList {
							// we're done
							rest = nil
						}
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
				// (proc args*)
				list, err := evalToSlice(pair, env)
				if err != nil {
					return nil, err
				}
				proc := list[0]
				list = list[1:]
				// TODO: handle user procedures, create new env with params, invoke
				// TODO: how is a user-defined procedure different from a lambda?
				if fun, ok := proc.(Procedure); ok {
					return fun.Call(list)
				} else {
					return nil, NewLispErrorf(ESUPPORT,
						"unhandled proc '%s': %v <%T>", sym, proc, proc)
				}
			}
		} else {
			return nil, NewLispErrorf(EARGUMENT, "thing not handled: %v", pair)
		}
	}
	return nil, nil
}

// TODO: write let, letrec, define-syntax
// TODO: for let and letrec, see http://matt.might.net/articles/cesk-machines/
