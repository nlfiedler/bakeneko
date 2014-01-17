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
	// derived tail recursive functions
	mapping[NewSymbol("and")] = NewRecursive(derivedAnd)
	mapping[NewSymbol("or")] = NewRecursive(derivedOr)
	mapping[NewSymbol("cond")] = NewRecursive(derivedCond)
	builtins := make([]Procedure, 0, 32)
	// equivalence procedures (R7RS 6.1)
	builtins = append(builtins, NewBuiltin(builtinEqv, "eqv?", 2, 2))
	builtins = append(builtins, NewBuiltin(builtinEqv, "eq?", 2, 2))
	builtins = append(builtins, NewBuiltin(builtinEqual, "equal?", 2, 2))
	// boolean procedures (R7RS 6.3)
	builtins = append(builtins, NewBuiltin(builtinNot, "not", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinIsBoolean, "boolean?", 1, -1))
	// symbol procedures (R7RS 6.5)
	builtins = append(builtins, NewBuiltin(symbolPredicate, "symbol?", 1, 1))
	builtins = append(builtins, NewBuiltin(symbolEqual, "symbol=?", 2, -1))
	builtins = append(builtins, NewBuiltin(symbolToString, "symbol->string", 1, 1))
	builtins = append(builtins, NewBuiltin(symbolFromString, "string->symbol", 1, 1))
	// pair and list procedures (R7RS 6.4)
	builtins = append(builtins, NewBuiltin(builtinAppend, "append", -1, -1))
	builtins = append(builtins, NewBuiltin(builtinCar, "car", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinCdr, "cdr", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinCaar, "caar", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinCadr, "cadr", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinCdar, "cdar", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinCddr, "cddr", 1, 1))
	// CxR library (R7RS Appendix A)
	cxr_lib := []string{
		"caaaar", "caaadr", "caaar", "caadar", "caaddr", "caadr",
		"cadaar", "cadadr", "cadar", "caddar", "cadddr", "caddr",
		"cdaaar", "cdaadr", "cdaar", "cdadar", "cdaddr", "cdadr",
		"cddaar", "cddadr", "cddar", "cdddar", "cddddr", "cdddr",
	}
	for _, fname := range cxr_lib {
		builtins = append(builtins, NewBuiltin(builtinCxr, fname, 1, 1))
	}
	builtins = append(builtins, NewBuiltin(builtinCons, "cons", 2, 2))
	builtins = append(builtins, NewBuiltin(builtinIsPair, "pair?", 1, 1))
	// builtins = append(builtins, NewBuiltin(builtinSetCar, "set-car!", 2, 2))
	// builtins = append(builtins, NewBuiltin(builtinSetCdr, "set-cdr!", 2, 2))
	builtins = append(builtins, NewBuiltin(builtinIsNull, "null?", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinIsList, "list?", 1, 1))
	// builtins = append(builtins, NewBuiltin(builtinMakeList, "make-list", 1, -1))
	builtins = append(builtins, NewBuiltin(builtinList, "list", 0, -1))
	builtins = append(builtins, NewBuiltin(builtinLength, "length", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinReverse, "reverse", 1, 1))
	// builtins = append(builtins, NewBuiltin(builtinListTail, "list-tail", 2, 2))
	// builtins = append(builtins, NewBuiltin(builtinListRef, "list-ref", 2, 2))
	// builtins = append(builtins, NewBuiltin(builtinListSet, "list-set!", 3, 3))
	// builtins = append(builtins, NewBuiltin(builtinMemq, "memq", 2, 2))
	// builtins = append(builtins, NewBuiltin(builtinMemv, "memv", 2, 2))
	// builtins = append(builtins, NewBuiltin(builtinMember, "member", 2, 2))
	// builtins = append(builtins, NewBuiltin(builtinAssq, "assq", 2, 2))
	// builtins = append(builtins, NewBuiltin(builtinAssv, "assv", 2, 2))
	builtins = append(builtins, NewBuiltin(builtinAssoc, "assoc", 2, 3))
	// builtins = append(builtins, NewBuiltin(builtinListCopy, "list-copy", 1, 1))
	// number procedures (R7RS 6.2)
	builtins = append(builtins, NewBuiltin(builtinIsNumber, "number?", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinIsComplex, "complex?", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinIsReal, "real?", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinIsRational, "rational?", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinIsInteger, "integer?", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinIsExact, "exact?", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinIsInexact, "inexact?", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinIsEqual, "=", 2, -1))
	builtins = append(builtins, NewBuiltin(builtinIsLess, "<", 2, -1))
	builtins = append(builtins, NewBuiltin(builtinIsLessEqual, "<=", 2, -1))
	builtins = append(builtins, NewBuiltin(builtinIsGreater, ">", 2, -1))
	builtins = append(builtins, NewBuiltin(builtinIsGreaterEqual, ">=", 2, -1))
	builtins = append(builtins, NewBuiltin(builtinIsZero, "zero?", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinIsPositive, "positive?", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinIsNegative, "negative?", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinIsOdd, "odd?", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinIsEven, "even?", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinMax, "max", 2, -1))
	builtins = append(builtins, NewBuiltin(builtinMin, "min", 2, -1))
	builtins = append(builtins, NewBuiltin(builtinAdd, "+", -1, -1))
	builtins = append(builtins, NewBuiltin(builtinSubtract, "-", -1, -1))
	builtins = append(builtins, NewBuiltin(builtinMultiply, "*", -1, -1))
	builtins = append(builtins, NewBuiltin(builtinDivide, "/", 1, -1))
	builtins = append(builtins, NewBuiltin(builtinAbs, "abs", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinQuotient, "quotient", 2, 2))
	// control features (R7RS 6.10)
	// builtins = append(builtins, NewBuiltin(builtinIsProcedure, "procedure?", 1, 1))
	builtins = append(builtins, NewBuiltin(builtinApply, "apply", 2, -1))
	builtins = append(builtins, NewBuiltin(builtinMap, "map", 2, -1))
	// builtins = append(builtins, NewBuiltin(builtinStringMap, "string-map", 2, -1))
	// builtins = append(builtins, NewBuiltin(builtinVectorMap, "vector-map", 2, -1))
	// builtins = append(builtins, NewBuiltin(builtinForEach, "for-each", 2, -1))
	// builtins = append(builtins, NewBuiltin(builtinStringForEach, "string-for-each", 2, -1))
	// builtins = append(builtins, NewBuiltin(builtinVectorForEach, "vector-for-each", 2, -1))
	// builtins = append(builtins, NewBuiltin(builtinCallcc, "call-with-current-continuation", 1, 1))
	// builtins = append(builtins, NewBuiltin(builtinCallcc, "call/cc", 1, 1))
	// builtins = append(builtins, NewBuiltin(builtinValues, "values", 1, -1))
	// builtins = append(builtins, NewBuiltin(builtinCallWithValues, "call-with-values", 2, 2))
	// builtins = append(builtins, NewBuiltin(builtinDynamicWind, "dynamic-wind", 3, 3))
	// map the procedures into the environment
	for _, fun := range builtins {
		mapping[NewSymbol(fun.Name())] = fun
	}
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
// Scheme procedure (e.g. builtinCons). The first argument is the name by
// which the procedure is bound in the environment. The second argument
// is the collection of evaluated arguments.
type builtinProcFunc func(string, []interface{}) (interface{}, LispError)

// Procedure represents a callable function in Scheme.
type Procedure interface {
	// Name returns the name of the procedure as a symbol.
	Name() string
	// Call invokes the built-in procedure with the given values.
	Call(values []interface{}) (interface{}, LispError)
}

// builtinProc is an implementation of Procedure for built-in functions.
type builtinProc struct {
	name    string          // name of the procedure
	argMin  int             // minimum number of required arguments (-1 skips checks)
	argMax  int             // maximum number of allowed arguments (-1 skips checks)
	builtin builtinProcFunc // reference to the procedure implementation
}

// NewBuiltin constructs a Procedure for the given built-in function, with an
// associated name (for error reporting), and a minimum and maximum number of
// arguments, which permits validation of inputs.
func NewBuiltin(f builtinProcFunc, name string, min, max int) Procedure {
	return &builtinProc{name, min, max, f}
}

// Name returns the name assigned to the procedure via NewBuiltin().
func (b *builtinProc) Name() string {
	return b.name
}

// Call invokes the built-in procedure implementation and returns the result.
func (b *builtinProc) Call(values []interface{}) (interface{}, LispError) {
	if b.argMin >= 0 || b.argMax >= 0 {
		// validate the correct number of arguments were given
		count := len(values)
		if b.argMin == b.argMax {
			if count != b.argMin {
				return nil, NewLispErrorf(EARGUMENT,
					"%v called with %d argument(s), requires %d", b.name, count, b.argMin)
			}
		} else if b.argMin >= 0 && count < b.argMin {
			return nil, NewLispErrorf(EARGUMENT,
				"%v called with %d argument(s), requires at least %d", b.name, count, b.argMin)
		} else if b.argMax >= 0 && count > b.argMax {
			return nil, NewLispErrorf(EARGUMENT,
				"%v called with %d argument(s), allows at most %d", b.name, count, b.argMax)
		}
	}
	return b.builtin(b.name, values)
}

// tailRecursiveFunc takes a set of arguments, evalutes those arguments, and
// either returns the tail expression to be evaluated by the caller, or the
// final return value. Examples include if, case, cond, and, or, and so on.
type tailRecursiveFunc func(int, Pair, Environment) (interface{}, interface{}, LispError)

// TailRecursive represents a tail recursive function.
type TailRecursive interface {
	// Call invokes the tail recursive function with the given values.
	Call(int, Pair, Environment) (interface{}, interface{}, LispError)
}

// recursiveProc is the internal implementation of TailRecursive.
type recursiveProc struct {
	// recursive is the reference to the tail recursive function.
	recursive tailRecursiveFunc
}

// NewRecursive constructs a TailRecursive for the given function.
func NewRecursive(f tailRecursiveFunc) TailRecursive {
	return &recursiveProc{f}
}

// Call invokes the tail recursive function and returns the results.
func (r *recursiveProc) Call(length int, pair Pair, env Environment) (interface{}, interface{}, LispError) {
	return r.recursive(length, pair, env)
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

// Bind creates a new environment in which the given values are associated
// with the arguments expected by the contained lambda. An improper argument
// list will have the remaining values assigned to the final argument as a
// single list of values.
func (c *closure) Bind(values Pair) (Environment, LispError) {
	env := NewEnvironment(c.env)
	// map the symbols in c.params to given values, storing in env
	name_iter := NewPairIterator(c.params)
	value_iter := NewPairIterator(values)
	for name_iter.HasNext() {
		name := name_iter.Next()
		sym, ok := name.(Symbol)
		if !ok {
			// parser should have handled this already
			return nil, NewLispErrorf(EARGUMENT, "name %s is not a symbol", name)
		}
		if !value_iter.HasNext() {
			return nil, NewLispErrorf(EARGUMENT, "too few arguments to (lambda %v)", c.params)
		}
		if !name_iter.IsProper() {
			// join the remaining values into a list and assign to the final argument
			results := NewPairJoiner()
			for value_iter.HasNext() {
				value := value_iter.Next()
				results.Append(value)
			}
			env.Define(sym, results.List())
		} else {
			value := value_iter.Next()
			env.Define(sym, value)
		}
	}
	if value_iter.HasNext() {
		return nil, NewLispErrorf(EARGUMENT, "too many arguments to (lambda %v)", c.params)
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
	parser := NewParser()
	body, err := parser.Parse(prog)
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
	expr, err := parser.Expand(body)
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
		// start with the primitive expressions (R7RS 4.1)
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
		// assume that the first is a primitive lambda until we learn otherwise
		keyword := true
		if sym, issym := first.(Symbol); issym {
			// the primitive lambdas
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
				return primitiveSet(pair, env)
			} else if atomsEqual(sym, defineSym) {
				// (define var exp)
				return primitiveDefine(pair, env)
			} else if atomsEqual(sym, lambdaSym) {
				// (lambda (var*) exp)
				return primitiveLambda(pair, env)
			} else if atomsEqual(sym, beginSym) {
				// (begin exp+)
				exp, err := evalForTail(pair.Rest(), env)
				if err != nil {
					return nil, err
				}
				expr = exp
			} else {
				val := env.Find(sym)
				if tr, ok := val.(TailRecursive); ok {
					// invoke the tail recursive function
					exp, val, err := tr.Call(length, pair, env)
					if exp != nil {
						expr = exp
					} else {
						return val, err
					}
					continue
				}
				// nope, was not a primitive or recursive lambda
				keyword = false
			}
		} else {
			keyword = false
		}
		if !keyword {
			// evaluate all of the list elements in order
			joinr := NewPairJoiner()
			iter := NewPairIterator(pair)
			for iter.HasNext() {
				exp := iter.Next()
				val, err := Eval(exp, env)
				if err != nil {
					return nil, err
				}
				joinr.Append(val)
			}
			// invoke the lambda or built-in procedure as appropriate
			exps := joinr.List()
			fun, args := exps.First(), exps.Rest()
			if builtin, ok := fun.(Procedure); ok {
				return builtin.Call(exps.ToSlice()[1:])
			} else if clos, ok := fun.(Closure); ok {
				if arg_list, ok := args.(Pair); ok {
					expr = clos.Body()
					var err LispError
					env, err = clos.Bind(arg_list)
					if err != nil {
						return nil, err
					}
				} else {
					return nil, NewLispErrorf(EARGUMENT,
						"Combination must be a proper list: %v", pair)
				}
			} else {
				return nil, NewLispErrorf(ESUPPORT,
					"The object %v is not applicable.", fun)
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

// primitiveSet implements the derived lambda set!
func primitiveSet(pair Pair, env Environment) (interface{}, LispError) {
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

// primitiveDefine implements the derived lambda define
func primitiveDefine(pair Pair, env Environment) (interface{}, LispError) {
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

// primitiveLambda implements the derived lambda lambda
func primitiveLambda(pair Pair, env Environment) (interface{}, LispError) {
	vars := pair.Second()
	body := pair.Third()
	if vlist, ok := vars.(Pair); ok {
		return NewClosure(body, vlist, env), nil
	}
	return nil, NewLispErrorf(EARGUMENT, "lambda arguments wrong type: %v", vars)
}

// derivedAnd implements the derived lambda and
func derivedAnd(length int, pair Pair, env Environment) (interface{}, interface{}, LispError) {
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

// derivedOr implements the derived lambda or
func derivedOr(length int, pair Pair, env Environment) (interface{}, interface{}, LispError) {
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

// derivedCond implements the derived lambda cond
func derivedCond(length int, pair Pair, env Environment) (interface{}, interface{}, LispError) {
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
				return NewList(expr, test), nil
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
				return exp, nil, err
			}
			result, err := Eval(test, env)
			if err != nil {
				return nil, nil, err
			}
			if isTrue(result) {
				if clair.Len() < 2 {
					// return test result as result of cond
					return nil, result, nil
				}
				exp, err := evalExpr(result, clair.Rest())
				return exp, nil, err
			}
		} else {
			return nil, nil, NewLispErrorf(EARGUMENT,
				"cond clause must be a pair: %v", clause)
		}
	}
	// unspecified!
	return nil, theEmptyList, nil
}

// derivedCase implements the derived lambda case
func derivedCase(length int, pair Pair, env Environment) (interface{}, interface{}, LispError) {
	// implement syntactic case (need Atom.EqualTo() first)
	return nil, nil, nil
}
