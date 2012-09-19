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
var theReportEnvironment Environment = newReportEnvironment()

// builtinProcFunc is the type of the function that implements a built-in
// Scheme procedure (e.g. builtinCons).
type builtinProcFunc func([]interface{}) (interface{}, LispError)

// Procedure represents a callable function in Scheme.
type Procedure interface {
	// Call invokes the built-in procedure with the given values,
	// we are evaluated with the given environment.
	Call(values interface{}, env Environment) (interface{}, LispError)
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
// The argument values are evaluated within the given environment before being
// passed to the built-in procedure.
func (b *builtinProc) Call(values interface{}, env Environment) (interface{}, LispError) {
	args := make([]interface{}, 0)
	for values != nil {
		arg := Car(values)
		if arg != nil {
			result, err := Eval(arg, env)
			if err != nil {
				return nil, err
			}
			args = append(args, result)
		}
		values = Cdr(values)
	}
	return b.builtin(args)
}

// lambda is a function body and its set of parameters.
type lambda struct {
	body   interface{} // procedure definition
	params Pair        // formal parameter list
}

// NewLambda constructs a new lambda for the given function body and parameters.
func NewLambda(body interface{}, params Pair) *lambda {
	return &lambda{body, params}
}

// Closure represents a procedure that can be invoked. It has an associated
// environment in which the procedure was defined, and any evaluations will be
// done in the context of that environment.
type Closure interface {
	// Bind this closure with the given arguments and return the new
	// environment, suitable for evaluating this closure.
	Bind(values Pair) (Environment, LispError)
	// Body returns the body of the closure for evaluation.
	Body() interface{}
}

// closure is an implementation of the Closure interface. It consists of a
// lambda and an environment in which to invoke the closure.
type closure struct {
	*lambda             // function to be invoked
	env     Environment // defining environment
}

// NewClosure constructs a new Closure with the given definition, defining
// environment, and parameters.
func NewClosure(body interface{}, params Pair, env Environment) Closure {
	lam := NewLambda(body, params)
	return &closure{lam, env}
}

// Bind evaluates the given values in the closure's associated environment,
// and returns a new environment suitable for invoking the closure.
func (c *closure) Bind(values Pair) (Environment, LispError) {
	// TODO: support arbitrary numbers of arguments (e.g. (list 1 2 3 ...))
	if c.params.Len() != values.Len() {
		str := c.params.String()
		return nil, NewLispErrorf(EARGUMENT, "wrong number of arguments for %v", str)
	}
	env := NewEnvironment(c.env)
	// map the symbols in c.params to given values, storing in env
	var names interface{} = c.params
	var valuse interface{} = values
	var err LispError = nil
	for names != nil {
		name := Car(names)
		sym, ok := name.(Symbol)
		if !ok {
			// parser should have handled this already
			return nil, NewLispErrorf(EARGUMENT, "name %s is not a symbol", name)
		}
		value := Car(valuse)
		value, err = Eval(value, c.env)
		if err != nil {
			return nil, err
		}
		env.Define(sym, value)
		names = Cdr(names)
		valuse = Cdr(valuse)
	}
	return env, nil
}

// Body returns the body of the closure for evaluation.
func (c *closure) Body() interface{} {
	return c.body
}

// Interpret parses the given Scheme program, evaluates each of the top-
// level elements, returning the final result.
func Interpret(prog string) (interface{}, LispError) {
	// thus begins the "inject" operation in CESK
	var err LispError
	body, err := parse(prog)
	if err != nil {
		return nil, err
	}
	// By ensuring that the program is wrapped inside a (begin ...) we
	// create what constitutes the "halt" continuation, as well as the
	// "step" function.
	if body.Len() >= 1 && body.First() != beginSym {
		body = Cons(beginSym, body)
	}
	expr, err := expand(body, true)
	if err != nil {
		return nil, err
	}
	env := NewEnvironment(theReportEnvironment)
	return Eval(expr, env)
}

// isTrue determines if the given thing represents a "true" value in Scheme.
// Only #f counts as false in Scheme, everything else is treated as true.
func isTrue(test interface{}) bool {
	if b, ok := test.(Boolean); ok {
		return b.Value()
	}
	return true
}

// Eval evaluates the given s-expression using a specific environment and
// returns the results. This handles tail-call recursion and invoking
// procedures, both built-in and user-defined.
func Eval(expr interface{}, env Environment) (interface{}, LispError) {
	// this is effectively the 'step' function in CESK
	for {
		if sym, ok := expr.(Symbol); ok {
			// symbolic reference
			val := env.Find(sym)
			if val != nil {
				return val, nil
			} else {
				return nil, NewLispErrorf(EARGUMENT, "Unbound variable: %s", sym)
			}
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
		// assume that the first is a syntactic keyword until we learn otherwise
		keyword := true
		if sym, issym := first.(Symbol); issym {
			// TODO: handle cond syntactic keyword
			// TODO: handle and syntactic keyword
			// TODO: handle or syntactic keyword
			// TODO: handle case syntactic keyword
			// TODO: handle let syntactic keyword
			// TODO: handle let* syntactic keyword
			// TODO: handle letrec syntactic keyword
			// TODO: for let and letrec, see http://matt.might.net/articles/cesk-machines/
			// TODO: handle do syntactic keyword
			// TODO: handle => syntactic keyword
			// TODO: handle delay syntactic keyword
			// TODO: handle else syntactic keyword
			// TODO: handle quasiquote syntactic keyword
			// TODO: handle unquote syntactic keyword
			// TODO: handle unquote-splicing syntactic keyword
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
				return syntaxSet(pair, env)
			} else if sym == defineSym {
				// (define var exp)
				return syntaxDefine(pair, env)
			} else if sym == lambdaSym {
				// (lambda (var*) exp)
				return syntaxLambda(pair, env)
			} else if sym == beginSym {
				// (begin exp+)
				return syntaxBegin(pair, env)
			} else {
				// nope, was not a syntactic keyword
				keyword = false
			}
		} else {
			// nope, was not a syntactic keyword
			keyword = false
		}
		if !keyword {
			// (proc args*)
			proc, err := Eval(first, env)
			if err != nil {
				return nil, err
			}
			if builtin, ok := proc.(Procedure); ok {
				return builtin.Call(pair.Rest(), env)
			} else if clos, ok := proc.(Closure); ok {
				if args, ok := pair.Rest().(Pair); ok {
					expr = clos.Body()
					env, err = clos.Bind(args)
					if err != nil {
						return nil, err
					}
				} else {
					return nil, NewLispErrorf(EARGUMENT,
						"Combination must be a proper list: %v", pair)
				}
			} else {
				return nil, NewLispErrorf(ESUPPORT,
					"The object %v is not applicable.", proc)
			}
		}
	}
	panic("unreachable code")
}

// syntaxSet implements the syntactic keyword set!
func syntaxSet(pair Pair, env Environment) (interface{}, LispError) {
	exp := pair.Third()
	val, err := Eval(exp, env)
	if err != nil {
		return nil, err
	}
	name := pair.Second()
	if ns, ok := name.(Symbol); ok {
		env.Set(ns, val)
	} else {
		return nil, NewLispErrorf(EARGUMENT, "name %v not a symbol", name)
	}
	return nil, nil
}

// syntaxDefine implements the syntactic keyword define
func syntaxDefine(pair Pair, env Environment) (interface{}, LispError) {
	exp := pair.Third()
	val, err := Eval(exp, env)
	if err != nil {
		return nil, err
	}
	name := pair.Second()
	if ns, ok := name.(Symbol); ok {
		env.Define(ns, val)
	} else {
		return nil, NewLispErrorf(EARGUMENT, "name %v not a symbol", name)
	}
	return nil, nil
}

// syntaxLambda implements the syntactic keyword lambda
func syntaxLambda(pair Pair, env Environment) (interface{}, LispError) {
	vars := pair.Second()
	body := pair.Third()
	if vlist, ok := vars.(Pair); ok {
		return NewClosure(body, vlist, env), nil
	}
	return nil, NewLispErrorf(EARGUMENT, "lambda arguments wrong type: %v", vars)
}

// syntaxBegin implements the syntactic keyword begin
func syntaxBegin(pair Pair, env Environment) (interface{}, LispError) {
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
}
