//
// Copyright 2012-2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

import (
	"github.com/petar/GoLLRB/llrb"
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
	// Lookup looks for the given symbol pairing (where the value is ignored)
	// and retrieves the value found in this environment. This has little
	// practical value outside of Environment implementations.
	Lookup(key SymbolPair) interface{}
	// Define sets the value for a symbol in this environment, creating
	// a new mapping if one does not already exist.
	Define(sym Symbol, val interface{})
	// Set assigns the value to the symbol in this environment, but only
	// if there is a previously defined value for that symbol.
	Set(sym Symbol, val interface{}) LispError
}

// SymbolPair represents an item stored in an Environment, containing the
// symbol used as the key, and its associated value.
type SymbolPair interface {
	llrb.Item
	Symbol() Symbol
	Value() interface{}
}

// environment is an implementation of the Environment interface, which
// supports mapping symbols to values, as well as delegating to a parent when
// a symbol is not found in this environment.
type environment struct {
	writable bool        // true if environment allows changes
	vars     *llrb.LLRB  // mapping of variable names to values
	parent   Environment // environment to delegate to
}

type environmentItem struct {
	sym Symbol
	val interface{}
}

// Less returns true if this item is less than the other item.
func (e *environmentItem) Less(than llrb.Item) bool {
	if ti, ok := than.(*environmentItem); ok {
		less, _ := e.sym.CompareTo(ti.sym)
		return less < 0
	}
	return false
}

// Symbol returns the symbol associated with this item.
func (e *environmentItem) Symbol() Symbol {
	return e.sym
}

// Symbol returns the symbol associated with this item.
func (e *environmentItem) Value() interface{} {
	return e.val
}

// newEnvItem returns a new environment symbol<->value mapping.
func newEnvItem(sym Symbol, val interface{}) *environmentItem {
	return &environmentItem{sym, val}
}

// NewEnvironment constructs an Environment with the given parent to provide a
// fallback for finding variables. The parent may be nil.
func NewEnvironment(parent Environment) Environment {
	e := new(environment)
	e.writable = true
	e.vars = llrb.New()
	e.parent = parent
	return e
}

// NewRestrictedEnvironment constructs an Environment with the given set of
// mappings. The environment cannot be modified.
func NewRestrictedEnvironment(parent Environment, mapping map[Symbol]interface{}) Environment {
	e := new(environment)
	e.writable = false
	e.vars = llrb.New()
	for sym, val := range mapping {
		item := newEnvItem(sym, val)
		e.vars.ReplaceOrInsert(item)
	}
	e.parent = parent
	return e
}

// Find retrieves the value for the given symbol. If it is not found in this
// environment, the parent environment will be consulted. If no value is
// found, nil is returned.
func (e *environment) Find(sym Symbol) interface{} {
	key := newEnvItem(sym, nil)
	return e.Lookup(key)
}

// Lookup retrieves the value for the given environment item. If it is not
// found in this environment, the parent environment will be consulted. If no
// value is found, nil is returned.
func (e *environment) Lookup(key SymbolPair) interface{} {
	item := e.vars.Get(key)
	if item == nil {
		if e.parent == nil {
			return nil
		}
		return e.parent.Lookup(key)
	}
	return item.(SymbolPair).Value()
}

// Define assigns the given value to the symbol in this environment.
func (e *environment) Define(sym Symbol, val interface{}) {
	if e.writable {
		item := newEnvItem(sym, val)
		e.vars.ReplaceOrInsert(item)
	}
}

// Set assigns a value to the given symbol, if and only if that symbol
// has a value already associated with it. If the symbol does not appear
// in this environment, the parent will be consulted.
func (e *environment) Set(sym Symbol, val interface{}) LispError {
	if !e.writable {
		return NewLispError(ESUPPORT, "restricted environment not writable")
	}
	item := newEnvItem(sym, val)
	if e.vars.Has(item) {
		e.vars.ReplaceOrInsert(item)
		return nil
	} else if e.parent != nil {
		return e.parent.Set(sym, val)
	}
	return NewLispErrorf(ESYMBOL, "symbol '%v' not yet defined", sym)
}

// newNullEnvironment constructs the "null" environment as defined in R7RS.
func newNullEnvironment() Environment {
	mapping := make(map[Symbol]interface{})
	mapping[NewSymbol("append")] = NewBuiltin(builtinAppend)
	mapping[NewSymbol("cons")] = NewBuiltin(builtinCons)
	// number support
	mapping[NewSymbol("number?")] = NewBuiltin(builtinIsNumber)
	mapping[NewSymbol("complex?")] = NewBuiltin(builtinIsComplex)
	mapping[NewSymbol("real?")] = NewBuiltin(builtinIsReal)
	mapping[NewSymbol("rational?")] = NewBuiltin(builtinIsRational)
	mapping[NewSymbol("integer?")] = NewBuiltin(builtinIsInteger)
	mapping[NewSymbol("exact?")] = NewBuiltin(builtinIsExact)
	mapping[NewSymbol("inexact?")] = NewBuiltin(builtinIsInexact)
	mapping[NewSymbol("=")] = NewBuiltin(builtinIsEqual)
	mapping[NewSymbol("<")] = NewBuiltin(builtinIsLess)
	mapping[NewSymbol("<=")] = NewBuiltin(builtinIsLessEqual)
	mapping[NewSymbol(">")] = NewBuiltin(builtinIsGreater)
	mapping[NewSymbol(">=")] = NewBuiltin(builtinIsGreaterEqual)
	mapping[NewSymbol("zero?")] = NewBuiltin(builtinIsZero)
	mapping[NewSymbol("positive?")] = NewBuiltin(builtinIsPositive)
	mapping[NewSymbol("negative?")] = NewBuiltin(builtinIsNegative)
	mapping[NewSymbol("odd?")] = NewBuiltin(builtinIsOdd)
	mapping[NewSymbol("even?")] = NewBuiltin(builtinIsEven)
	mapping[NewSymbol("max")] = NewBuiltin(builtinMax)
	mapping[NewSymbol("min")] = NewBuiltin(builtinMin)
	mapping[NewSymbol("+")] = NewBuiltin(builtinAdd)
	mapping[NewSymbol("-")] = NewBuiltin(builtinSubtract)
	mapping[NewSymbol("*")] = NewBuiltin(builtinMultiply)
	mapping[NewSymbol("/")] = NewBuiltin(builtinDivide)
	mapping[NewSymbol("abs")] = NewBuiltin(builtinAbs)
	mapping[NewSymbol("quotient")] = NewBuiltin(builtinQuotient)
	ne := NewRestrictedEnvironment(nil, mapping)
	return ne
}

// nullEnvironment is the "null" environment in Scheme.
var theNullEnvironment Environment = newNullEnvironment()

// newReportEnvironment creates an environment with the default Scheme
// procedures and variables in place for use in a global context.
func newReportEnvironment() Environment {
	mapping := make(map[Symbol]interface{})
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
	body   interface{} // procedure definition TODO: should be a Pair
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
	if c.params.Len() != values.Len() {
		str := c.params.String()
		return nil, NewLispErrorf(EARGUMENT,
			"wrong number of arguments for %v, expected %d, got %d", str,
			c.params.Len(), values.Len())
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
	if body.Len() >= 1 {
		if sym, ok := body.First().(Symbol); !ok || !atomsEqual(sym, beginSym) {
			body = Cons(beginSym, body)
		}
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
		length := pair.Len()
		if length == 0 {
			// empty list
			return pair, nil
		}
		first := pair.First()
		// assume that the first is a syntactic keyword until we learn otherwise
		keyword := true
		if sym, issym := first.(Symbol); issym {
			if atomsEqual(sym, quoteSym) {
				// (quote exp)
				return pair.Second(), nil
			} else if atomsEqual(sym, ifSym) {
				// (if test conseq alt)
				test := pair.Second()
				result, err := Eval(test, env)
				if err != nil {
					return nil, err
				}
				if isTrue(result) {
					expr = pair.Third()
				} else {
					expr = Cxr("cadddr", pair)
				}
			} else if atomsEqual(sym, setSym) {
				// (set! var exp)
				return syntaxSet(pair, env)
			} else if atomsEqual(sym, defineSym) {
				// (define var exp)
				return syntaxDefine(pair, env)
			} else if atomsEqual(sym, lambdaSym) {
				// (lambda (var*) exp)
				return syntaxLambda(pair, env)
			} else if atomsEqual(sym, beginSym) {
				// (begin exp+)
				exp, err := evalForTail(pair.Rest(), env)
				if err != nil {
					return nil, err
				}
				expr = exp
			} else if atomsEqual(sym, andSym) {
				// (and exp*)
				exp, val, err := syntaxAnd(length, pair, env)
				if exp != nil {
					expr = exp
				} else {
					return val, err
				}
			} else if atomsEqual(sym, orSym) {
				// (or exp*)
				exp, val, err := syntaxOr(length, pair, env)
				if exp != nil {
					expr = exp
				} else {
					return val, err
				}
			} else if atomsEqual(sym, condSym) {
				// (cond <clause>+)
				val, exp, err := syntaxCond(length, pair, env)
				if exp != nil {
					expr = exp
				} else {
					return val, err
				}
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

// evalForTail implements a begin-style evaluation of a list of expressions,
// returning the last expression as-is to be evaluated in a tail-call fashion.
func evalForTail(expr interface{}, env Environment) (interface{}, LispError) {
	// if the input is not a list, return it for evaluation
	if pair, ok := expr.(Pair); ok {
		// otherwise, evaluate all but the last expression in the list
		length := pair.Len()
		iter := NewPairIterator(pair)
		for index := 1; index < length; index++ {
			_, err := Eval(iter.Next(), env)
			if err != nil {
				return nil, err
			}
		}
		// the last expression is a tail expression
		expr = iter.Next()
	}
	return expr, nil
}

// extractFirst extracts the single expression from the list, if the argument
// is indeed a list and it contains exactly one item. Otherwise the input is
// returned unchanged.
func extractFirst(expr interface{}) interface{} {
	if pair, ok := expr.(Pair); ok {
		if pair.Len() == 1 {
			expr = pair.First()
		}
	}
	return expr
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

// syntaxAnd implements the syntactic keyword and
func syntaxAnd(length int, pair Pair, env Environment) (interface{}, interface{}, LispError) {
	if length == 1 {
		// (and) => #t
		return nil, BooleanTrue, nil
	}
	iter := NewPairIterator(pair)
	iter.Next()
	// up to the last expression is evaluated in place
	for index := 2; index < length; index++ {
		test := iter.Next()
		result, err := Eval(test, env)
		if err != nil {
			return nil, nil, err
		}
		if !isTrue(result) {
			return nil, BooleanFalse, nil
		}
	}
	// the last expression is a tail expression
	return iter.Next(), nil, nil
}

// syntaxOr implements the syntactic keyword or
func syntaxOr(length int, pair Pair, env Environment) (interface{}, interface{}, LispError) {
	if length == 1 {
		// (or) => #f
		return nil, BooleanFalse, nil
	}
	iter := NewPairIterator(pair)
	iter.Next()
	// up to the last expression is evaluated in place
	for index := 2; index < length; index++ {
		test := iter.Next()
		result, err := Eval(test, env)
		if err != nil {
			return nil, nil, err
		}
		if isTrue(result) {
			return nil, BooleanTrue, nil
		}
	}
	// the last expression is a tail expression
	return iter.Next(), nil, nil
}

// syntaxCond implements the syntactic keyword cond, returning the evaluated
// result, if not a tail-call, or nil for tail call, with the second return
// value being the expression to be evaluated in tail-call fashion, while the
// third return value is any error that occurred.
func syntaxCond(length int, pair Pair, env Environment) (interface{}, interface{}, LispError) {
	// evalExpr evaluates the expression(s) for the clause, returning the
	// last expression in the sequence for evaluation in tail-call fashion.
	evalExpr := func(test, expr interface{}) (interface{}, LispError) {
		// we assume this is not an 'else' clause and that there are
		// non-zero expressions to be evaluated
		if epair, ok := expr.(Pair); ok {
			if sym, ok := epair.First().(Symbol); ok && atomsEqual(sym, arrowSym) {
				// rest of expression is assumed to be a unary procedure,
				// return it as a procedure invocation to be evaluated
				expr = extractFirst(epair.Rest())
				return List(expr, test), nil
			}
			return evalForTail(epair, env)
		}
		// should not get here
		return nil, NewLispError(ESYNTAX, "malformed cond clause?")
	}

	iter := NewPairIterator(pair)
	iter.Next()
	for iter.HasNext() {
		clause := iter.Next()
		if clair, ok := clause.(Pair); ok {
			if clair.Len() < 1 {
				return nil, nil, NewLispErrorf(EARGUMENT,
					"cond clause must not be empty: %v", clause)
			}
			test := clair.First()
			if sym, ok := test.(Symbol); ok && atomsEqual(sym, elseSym) {
				if clair.Len() < 2 {
					return nil, nil, NewLispErrorf(EARGUMENT,
						"cond else clause must not be empty: %v", clause)
				}
				exp, err := evalForTail(clair.Rest(), env)
				return nil, exp, err
			}
			result, err := Eval(test, env)
			if err != nil {
				return nil, nil, err
			}
			if isTrue(result) {
				if clair.Len() < 2 {
					// return test result as result of cond
					return result, nil, nil
				}
				exp, err := evalExpr(result, clair.Rest())
				return nil, exp, err
			}
		} else {
			return nil, nil, NewLispErrorf(EARGUMENT,
				"cond clause must be a pair: %v", clause)
		}
	}
	// unspecified!
	return theEmptyList, nil, nil
}

// syntaxCase implements the syntactic keyword case
func syntaxCase(length int, pair Pair, env Environment) (interface{}, interface{}, LispError) {
	// implement syntactic case (need Atom.EqualTo() first)
	return nil, nil, nil
}
