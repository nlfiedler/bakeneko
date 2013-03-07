//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

//
// Parser for our Scheme-like language, which turns tokens from the
// lexer into a tree of expressions to be evaluated.
//

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"
)

var endOfStreamMsg = "unexpectedly reached end of expression"

var eofObject = Symbol("#<eof-object>")
var andSym = Symbol("and")
var arrowSym = Symbol("=>")
var beginSym = Symbol("begin")
var condSym = Symbol("cond")
var defineSym = Symbol("define")
var definesyntaxSym = Symbol("define-syntax")
var elseSym = Symbol("else")
var ifSym = Symbol("if")
var lambdaSym = Symbol("lambda")
var orSym = Symbol("or")
var quasiquoteSym = Symbol("quasiquote")
var quoteSym = Symbol("quote")
var setSym = Symbol("set!")
var unquoteSym = Symbol("unquote")
var unquotesplicingSym = Symbol("unquote-splicing")
var appendSym = Symbol("append")
var consSym = Symbol("cons")

// macroTable stores the globally defined macros, mapping instances of
// Symbol to instances of Closure.
var macroTable = make(map[Symbol]Closure)

// TODO: introduce a new type (Any) that represents anything in Scheme, use this over interface{}
// TODO: may want a new type (Value) to represent all values(?)

// stringify takes a tree of elements and converts it to a string in
// Scheme format (e.g. true becomes "#t", lists become "(...)", etc).
func stringify(x interface{}) string {
	buf := new(bytes.Buffer)
	stringifyBuffer(x, buf)
	return buf.String()
}

// stringifyBuffer converts the tree of elements to a string, which is
// written to the given buffer.
func stringifyBuffer(x interface{}, buf *bytes.Buffer) {
	switch i := x.(type) {
	case nil:
		buf.WriteString("()")
	case []interface{}:
		// to print vectors?
		buf.WriteString("(")
		for _, v := range i {
			stringifyBuffer(v, buf)
			buf.WriteString(" ")
		}
		// lop off the trailing space
		if buf.Len() > 2 {
			buf.Truncate(buf.Len() - 1)
		}
		buf.WriteString(")")
	default:
		// this handles Atom and Pair
		fmt.Fprintf(buf, "%v", i)
	}
}

// parse parses a Scheme program and returns the result, which will be a Pair
// consisting of program elements (i.e. function calls).
func parse(expr string) (Pair, LispError) {
	c := lex("parse", expr)
	defer drainLexer(c)
	var results Pair = theEmptyList
	var tail Pair = theEmptyList
	for {
		t, ok := <-c
		if !ok || t.typ == tokenEOF {
			return results, nil
		}
		if t.typ == tokenComment {
			// ignore the next datum (r7rs 7.1.2)
			t, ok = <-c
			if !ok || t.typ == tokenEOF {
				return results, nil
			}
			parserRead(t, c)
			// read from the channel to get the next thing
			t, ok = <-c
			if !ok || t.typ == tokenEOF {
				return results, nil
			}
			// otherwise continue on as usual
		}
		elem, err := parserRead(t, c)
		if err != nil {
			return nil, err
		}
		if results == theEmptyList {
			results = NewPair(elem)
			tail = results
		} else {
			tail = tail.Append(elem)
		}
	}
	panic("unreachable code")
}

// parseNext reads a complete expression from the channel of tokens.
func parseNext(c chan token) (interface{}, LispError) {
	t, ok := <-c
	if !ok {
		return nil, NewLispError(ESYNTAX, endOfStreamMsg)
	}
	return parserRead(t, c)
}

// parserRead reads a complete expression from the channel of tokens,
// starting with the initial token value provided.
func parserRead(t token, c chan token) (interface{}, LispError) {
	switch t.typ {
	case tokenError:
		return nil, NewLispError(ESYNTAX, t.val)
	case tokenEOF:
		return nil, NewLispError(ESYNTAX, endOfStreamMsg)
	case tokenOpenParen:
		t, ok := <-c
		if !ok {
			return nil, NewLispError(ESYNTAX, endOfStreamMsg)
		}
		return parserReadPair(t, c)
	case tokenStartVector:
		slice := make([]interface{}, 0, 16)
		for t = range c {
			if t.typ == tokenCloseParen {
				return slice, nil
			}
			val, err := parserRead(t, c)
			if err != nil {
				return nil, err
			}
			slice = append(slice, val)
		}
		return nil, NewLispError(ESYNTAX, endOfStreamMsg)
	case tokenCloseParen:
		return nil, NewLispError(ESYNTAX, "unexpected ')'")
	case tokenString:
		return NewString(t.contents()), nil
	case tokenInteger:
		val, err := atoi(t.val)
		if err != nil {
			return nil, err
		}
		return NewInteger(val), nil
	case tokenFloat:
		val, err := atof(t.val)
		if err != nil {
			return nil, err
		}
		return NewFloat(val), nil
	case tokenComplex:
		val, err := atoc(t.val)
		if err != nil {
			return nil, err
		}
		return NewComplex(val), nil
	case tokenRational:
		a, b, err := ator(t.val)
		if err != nil {
			return nil, err
		}
		return NewRational(a, b), nil
	case tokenBoolean:
		return NewBoolean(t.val), nil
	case tokenCharacter:
		if len(t.val) > 3 {
			// lexer probably messed up
			return nil, NewLispErrorf(ESYNTAX, "unrecognized character: %s", t.val)
		}
		return NewCharacter(t.val), nil
	case tokenQuote:
		var quote Symbol
		switch t.val {
		case "'":
			quote = quoteSym
		case "`":
			quote = quasiquoteSym
		case ",":
			quote = unquoteSym
		case ",@":
			quote = unquotesplicingSym
		default:
			return nil, NewLispErrorf(ESYNTAX, "unrecognized quote symbol: %s", t.val)
		}
		pair, err := parseNext(c)
		if err != nil {
			return nil, err
		}
		return NewList(quote, pair), nil
	case tokenIdentifier:
		// TODO: if #!fold-case enabled, then lower, else not
		return Symbol(strings.ToLower(t.val)), nil
		// case tokenComment:
		// 	// TODO: is this appropriate? maybe tests should not call parserRead()
		// 	return "", nil
	}
	panic("unreachable code")
}

// parserReadPair expects to read the contents of a list, whether a proper
// list (one with '.' separating elements) or otherwise. This function assumes
// that the previous token was an open parenthesis.
func parserReadPair(t token, c chan token) (interface{}, LispError) {
	if t.typ == tokenCloseParen {
		return theEmptyList, nil
	}
	// read the first element in the list
	car_obj, err := parserRead(t, c)
	if err != nil {
		return nil, err
	}
	// check if the next token is a dot
	t, ok := <-c
	if !ok {
		return nil, NewLispError(ESYNTAX, endOfStreamMsg)
	}
	if t.typ == tokenIdentifier && t.val == "." {
		// read an improper list
		// skip over the dot and start parsing the next element
		cdr_obj, err := parseNext(c)
		if err != nil {
			return nil, err
		}
		t, ok = <-c
		if !ok {
			return nil, NewLispError(ESYNTAX, endOfStreamMsg)
		}
		if t.typ != tokenCloseParen {
			return nil, NewLispErrorf(ESYNTAX, "expected ')', but got %s", t.val)
		}
		return Cons(car_obj, cdr_obj), nil
	} else {
		// read a proper list
		cdr_obj, err := parserReadPair(t, c)
		if err != nil {
			return nil, err
		}
		return Cons(car_obj, cdr_obj), nil
	}
	panic("unreachable code")
}

// atof attempts to coerce the given text into a floating point value,
// returning an error if unsuccessful.
func atof(text string) (float64, LispError) {
	if len(text) > 2 && text[0] == '#' {
		// handle #e and #i prefixes for exactness
		switch text[1] {
		case 'e', 'E':
			return 0.0, NewLispErrorf(ESUPPORT, "exactness prefix unsupported: %s", text)
		case 'i', 'I':
			return 0.0, NewLispErrorf(ESUPPORT, "exactness prefix unsupported: %s", text)
		default:
			// the lexer messed up if this happens
			return 0.0, NewLispError(ESYNTAX, text)
		}
	}
	v, err := strconv.ParseFloat(text, 64)
	if err != nil {
		if err == strconv.ErrSyntax {
			// the lexer messed up if this happens
			return 0.0, NewLispErrorf(ESYNTAX, "invalid number syntax: %s", text)
		}
		if err == strconv.ErrRange {
			return 0.0, NewLispErrorf(ESYNTAX, "number out of range: %s", text)
		}
	}
	return v, nil
}

// atoi attempts to coerce the given text into an integer value,
// returning an error if unsuccessful.
func atoi(text string) (int64, LispError) {
	// assume base 10 numbers, unless otherwise specified
	base := 10
	if len(text) > 2 {
		// handle #b, #d, #o, #x prefixes for different number bases
		idx := 0
		for text[idx] == '#' {
			idx++
			switch text[idx] {
			case 'b', 'B':
				base = 2
			case 'o', 'O':
				base = 8
			case 'd', 'D':
				base = 10
			case 'x', 'X':
				base = 16
			case 'e', 'E', 'i', 'I':
				// ignored, all integers are exact
			default:
				// the lexer messed up if this happens
				return 0, NewLispErrorf(ESYNTAX, "invalid number syntax: %s", text)
			}
			idx++
		}
		text = text[idx:]
	}
	v, err := strconv.ParseInt(text, base, 64)
	if err != nil {
		if err == strconv.ErrSyntax {
			// the lexer messed up if this happens
			return 0, NewLispErrorf(ESYNTAX, "invalid number syntax: %s", text)
		}
		if err == strconv.ErrRange {
			return 0, NewLispErrorf(ESYNTAX, "number out of range: %s", text)
		}
	}
	return v, nil
}

// atoc attempts to coerce the given text into a complex numeric value,
// returning an error if unsuccessful.
func atoc(text string) (complex128, LispError) {
	var zero complex128 = complex(0.0, 0.0)
	if split := strings.IndexRune(text, '@'); split > 0 {
		// <real R> @ <real R>
		reel, err := atof(text[:split])
		if err != nil {
			return zero, err
		}
		imaj, err := atof(text[split+1:])
		if err != nil {
			return zero, err
		}
		return complex(reel, imaj), nil
	} else {
		// <real R> + <ureal R> i | <real R> - <ureal R> i |
		// <real R> + i | <real R> - i |
		// + <ureal R> i | - <ureal R> i |
		// + i | - i
		split := strings.IndexAny(text, "+-")
		if split == -1 {
			// there must be a sign, otherwise lexer messed up
			return zero, NewLispErrorf(ESYNTAX, "invalid number syntax: %s", text)
		} else if split == 0 {
			// see if there is a second sign
			split = strings.IndexAny(text[1:], "+-") + 1
		}
		var err LispError
		var reel float64
		if split > 0 {
			reel, err = atof(text[:split])
			if err != nil {
				return zero, err
			}
		} else {
			reel = 0.0
		}
		var imaj float64
		ip := text[split : len(text)-1]
		if ip == "+" {
			imaj = 1.0
		} else if ip == "-" {
			imaj = -1.0
		} else {
			imaj, err = atof(ip)
			if err != nil {
				return zero, err
			}
		}
		return complex(reel, imaj), nil
	}
	panic("unreachable code")
}

// ator attempts to coerce the given text into a rational numeric value,
// returning an error if unsuccessful. The numerator and denominator are
// returned separately to faciliate conversion to a Rational.
func ator(text string) (int64, int64, LispError) {
	if split := strings.IndexRune(text, '/'); split > 0 {
		num, err := atoi(text[:split])
		if err != nil {
			return 0, 0, err
		}
		denom, err := atoi(text[split+1:])
		if err != nil {
			return 0, 0, err
		}
		return num, denom, nil
	} else {
		// lexer messed up if this happens
		return 0, 0, NewLispErrorf(ESYNTAX, "invalid number syntax: %s", text)
	}
	panic("unreachable code")
}

// newParserError returns a LispError of the given type, for the
// selected parser token, with the clarifying message.
func newParserError(err ErrorCode, elem interface{}, msg string) LispError {
	str := stringify(elem)
	return NewLispError(err, msg+": "+str)
}

// expandListSafely calls expand() on each element of the given list and
// returns any error that occurs.
func expandListSafely(list Pair, toplevel bool) (val Pair, err LispError) {
	expandWithPanic := func(x interface{}) interface{} {
		val, err := expand(x, toplevel)
		if err != nil {
			panic(err)
		}
		return val
	}
	defer func() {
		if e := recover(); e != nil {
			val = nil
			err = e.(LispError)
		}
	}()
	return list.Map(expandWithPanic), nil
}

// Walk the tree of parser tokens, making optimizations and obvious
// fixes to enable easier interpretation, possibly signaling a syntax
// error if appropriate.
func expand(x interface{}, toplevel bool) (interface{}, LispError) {
	if x == nil {
		return nil, NewLispError(ESYNTAX, "empty input")
	}
	pair, ispair := x.(Pair)
	if !ispair {
		return x, nil
	}
	token := pair.First()
	for pair.Len() == 1 {
		// Check if thing inside is another pair, in which case we
		// extract it and pretend the enclosing pair did not exist.
		if np, ok := token.(Pair); ok {
			pair = np
			token = pair.First()
		} else {
			break
		}
	}
	if sym, issym := token.(Symbol); issym {
		if sym == quoteSym {
			if pair.Len() != 2 {
				return nil, newParserError(ESYNTAX, pair, "quote requires datum")
			}
			return x, nil

		} else if sym == ifSym {
			if pair.Len() == 3 {
				// (if t c) => (if t c ())
				pair.Append(theEmptyList)
			}
			if pair.Len() != 4 {
				return nil, newParserError(ESYNTAX, pair, "if too many/few arguments")
			}
			return expandListSafely(pair, false)

		} else if sym == setSym {
			if pair.Len() != 3 {
				return nil, newParserError(ESYNTAX, pair, "set requires 2 arguments")
			}
			name := pair.Second()
			// (set! non-var exp) => Error
			if _, ok := name.(Symbol); !ok {
				return nil, newParserError(ESYNTAX, name, "can only set! a symbol")
			}
			val, err := expand(pair.Third(), false)
			if err != nil {
				return nil, err
			}
			return NewList(setSym, name, val), nil

		} else if sym == defineSym || sym == definesyntaxSym {
			if pair.Len() < 3 {
				return nil, newParserError(ESYNTAX, pair, "define/define-syntax require 2+ arguments")
			}
			v := pair.Second()
			body := Cdr(Cdr(pair))
			if list, islist := v.(Pair); islist && list.Len() > 0 {
				// (define (f args) body) => (define f (lambda (args) body))
				f, args := list.First(), list.Rest()
				lambda := NewList(lambdaSym, args)
				lambda.Join(body)
				pair = NewList(sym, f, lambda)
				return expandListSafely(pair, false)
			} else {
				// (define non-var/list exp) => Error
				sym2, issym := v.(Symbol)
				if !issym {
					return nil, newParserError(ESYNTAX, v, "can define only a symbol")
				}
				val, err := expand(pair.Third(), false)
				if err != nil {
					return nil, err
				}
				if sym == definesyntaxSym {
					if !toplevel {
						return nil, newParserError(ESYNTAX, pair,
							"define-syntax only allowed at top level")
					}
					proc, err := Eval(val, theReportEnvironment)
					if err != nil {
						return nil, err
					}
					closure, isproc := proc.(Closure)
					if !isproc {
						return nil, newParserError(EARGUMENT, pair,
							"macro must be a procedure")
					}
					// (define-syntax v proc)
					macroTable[sym2] = closure
					return nil, nil
				}
				result := NewList(defineSym, sym2, val)
				return result, nil
			}

		} else if sym == beginSym {
			if pair.Len() == 1 {
				// (begin) => None
				return nil, nil
			}
			return expandListSafely(pair, toplevel)

		} else if sym == lambdaSym {
			// (lambda (x) e1 e2) => (lambda (x) (begin e1 e2))
			if pair.Len() < 3 {
				return nil, newParserError(ESYNTAX, pair, "lambda requires 2+ arguments")
			}
			vars := pair.Second()
			body := Cxr("cddr", pair)
			vlist, islist := vars.(Pair)
			_, issym := vars.(Symbol)
			if islist && vlist.Len() > 0 {
				var thing interface{} = vlist
				for thing != nil {
					elem := Car(thing)
					if _, issym := elem.(Symbol); !issym {
						return nil, NewLispError(ESYNTAX,
							"lambda arguments must be symbols")
					}
					thing = Cdr(thing)
				}
			} else if issym {
				vlist = NewPair(vars)
			} else {
				return nil, newParserError(ESYNTAX, pair, "lambda arguments must be a list or a symbol")
			}
			if blist, islist := body.(Pair); islist {
				if blist.Len() == 1 {
					body = blist.First()
				} else {
					body = Cons(beginSym, body)
				}
			} else {
				return nil, newParserError(ESYNTAX, pair, "lambda body must be a list")
			}
			body, err := expand(body, false)
			if err != nil {
				return nil, err
			}
			return NewList(lambdaSym, vlist, body), nil

		} else if sym == quasiquoteSym {
			// `x => expand quasiquote of x
			if pair.Len() != 2 {
				return nil, newParserError(ESYNTAX, pair, "quasiquote (`) require 2 arguments")
			}
			return expandQuasiquote(pair.Second())

		} else if _, ok := macroTable[sym]; ok {
			// (m arg...)
			if pair, ispair := pair.Rest().(Pair); !ispair {
				pair = NewPair(pair)
			}
			return nil, nil
			// TODO: implement macros
			// result, err := macro.Invoke(pair)
			// if err != nil {
			// 	return nil, err
			// }
			// return expand(result, toplevel)
		}
	}

	// if we reached this point, it must be a procedure call
	return expandListSafely(pair, false)
}

// expandQuasiquote processes the quotes, expanding the quoted elements.
func expandQuasiquote(x interface{}) (interface{}, LispError) {
	// Expand `x => 'x; `,x => x; `(,@x y) => (append x y)
	pair, ispair := x.(Pair)
	if !ispair || pair.Len() == 0 {
		return NewList(quoteSym, x), nil
	}
	token := pair.First()
	sym, issym := token.(Symbol)
	if issym && sym == unquotesplicingSym {
		return nil, newParserError(ESYNTAX, pair, "can't splice here")
	}
	if issym && sym == unquoteSym {
		if pair.Len() != 2 {
			return nil, newParserError(ESYNTAX, pair, "unquote requires 1 argument")
		}
		return pair.Second(), nil
	}
	if npair, ispair := token.(Pair); ispair && npair.Len() > 0 {
		if sym, issym := npair.First().(Symbol); issym && sym == unquotesplicingSym {
			if npair.Len() != 2 {
				return nil, newParserError(ESYNTAX, pair, "unquote splicing requires 1 argument")
			}
			expr, err := expandQuasiquote(pair.Rest())
			if err != nil {
				return nil, err
			}
			return NewList(appendSym, npair.Second(), expr), nil
		}
	}
	fexpr, err := expandQuasiquote(pair.First())
	if err != nil {
		return nil, err
	}
	rexpr, err := expandQuasiquote(pair.Rest())
	if err != nil {
		return nil, err
	}
	return NewList(consSym, fexpr, rexpr), nil
}
