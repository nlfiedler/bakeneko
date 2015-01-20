//
// Copyright 2012-2015 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

//
// Parser for our Scheme-like language, which turns tokens from the
// lexer into a tree of expressions to be evaluated.
//

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"path"
	"strconv"
	"strings"
)

var endOfStreamMsg = "unexpectedly reached end of expression"

var eofObject = NewSymbol("#<eof-object>")
var andSym = NewSymbol("and")
var arrowSym = NewSymbol("=>")
var beginSym = NewSymbol("begin")
var condSym = NewSymbol("cond")
var defineSym = NewSymbol("define")
var definesyntaxSym = NewSymbol("define-syntax")
var elseSym = NewSymbol("else")
var ifSym = NewSymbol("if")
var includeSym = NewSymbol("include")
var includeCaseSym = NewSymbol("include-ci")
var lambdaSym = NewSymbol("lambda")
var orSym = NewSymbol("or")
var quasiquoteSym = NewSymbol("quasiquote")
var quoteSym = NewSymbol("quote")
var setSym = NewSymbol("set!")
var unquoteSym = NewSymbol("unquote")
var unquotesplicingSym = NewSymbol("unquote-splicing")
var appendSym = NewSymbol("append")
var consSym = NewSymbol("cons")
var listToVectorSym = NewSymbol("list->vector")

// None represents the result of a line comment, or other results of parsing
// that resolve to nothing.
type None int

// theNone is the singleton None instance.
var theNone = None(0)

// String for None returns the empty string.
func (n None) String() string {
	return ""
}

// ForwardReference provides a means for temporarily referring to an object
// that is being referenced before it is completely defined. An example
// would be a list of pairs that contains a reference to itself.
type ForwardReference interface {
	// Label returns the identifier for the forward reference.
	Label() string
	// Resolve returns the datum referred to by this reference. A nil value
	// signals an error (Scheme nil is represented using an instance of the
	// None type).
	Resolve() interface{}
}

// forwardDatumRef is a placeholder for datum references that are found
// within the expression to which the datum label is being applied, and
// hence the final expression is not yet available (e.g. #1=(a b . #1#)).
type forwardDatumRef struct {
	label  string      // the datum label, e.g. "1"
	parser *parserImpl // parser from which we can resolve labels
}

func (fr *forwardDatumRef) Label() string {
	return fr.label
}

func (fr *forwardDatumRef) Resolve() interface{} {
	for idx := len(fr.parser.labels) - 1; idx >= 0; idx-- {
		if val, ok := fr.parser.labels[idx][fr.label]; ok {
			return val
		}
	}
	return nil
}

// macroTable stores the globally defined macros, mapping instances of
// Symbol to instances of Closure.
var macroTable = make(map[Symbol]Closure)

// stringify takes a tree of elements and converts it to a string in
// Scheme format (e.g. true becomes "#t", lists become "(...)", etc).
func stringify(x interface{}) string {
	buf := new(bytes.Buffer)
	stringifyBuffer(x, buf)
	return buf.String()
}

// stringifyBuffer converts the tree of elements to a string, which is
// written to the given buffer. Returns the number of bytes written.
func stringifyBuffer(x interface{}, buf *bytes.Buffer) int {
	pos := buf.Len()
	switch i := x.(type) {
	case nil:
		buf.WriteString("()")
	default:
		// this handles Atom and Pair
		fmt.Fprintf(buf, "%v", i)
	}
	return buf.Len() - pos
}

// wrapWithBegin ensures that the given s-expression is written with (begin).
func wrapWithBegin(expr interface{}) interface{} {
	if seq, is_seq := expr.(Sequence); is_seq && seq.Len() > 0 {
		if sym, is_sym := seq.First().(Symbol); !is_sym || !atomsEqual(sym, beginSym) {
			if vec, is_vec := seq.(Vector); is_vec {
				result := make([]interface{}, 0)
				result = append(result, beginSym)
				for pos := 0; pos < vec.Len(); pos++ {
					result = append(result, vec.Get(pos))
				}
				return NewVector(result)
			} else {
				return Cons(beginSym, seq)
			}
		}
	} else {
		return Cons(beginSym, expr)
	}
	return expr
}

// Parser knows how to convert a string represetation of Scheme code into
// a runnable program, which can be passed to the Eval() function.
type Parser interface {
	// Parse will return a collection of Scheme objects parsed from the given
	// string, returning an error if the lexing or parsing fails.
	Parse(expr string) (Sequence, LispError)
	// ParseFile reads the named file as a Scheme program, returning the
	// parsed results, or an error if anything went wrong.
	ParseFile(filename string) (Sequence, LispError)
	// Expand takes the parsed results and performs some basic validation
	// and expands the results into the canoncial form.
	Expand(x interface{}) (interface{}, LispError)
}

// parserImpl holds the state of the parser during parsing.
type parserImpl struct {
	tokens    chan token               // channel of lexer tokens
	inComment bool                     // true if parsing a datum comment
	withLabel bool                     // true if parsing a labeled datum
	labels    []map[string]interface{} // collection of labeled datum, organized by scope
	name      string                   // name of input; usually a file name
	include   string                   // path for file includes, may be '.'
	foldcase  bool                     // if true, implicitly #!fold-case next Parse()
}

// NewParser constructs a new Parser instance.
func NewParser() Parser {
	// start with an initial datum label scope
	labels := make([]map[string]interface{}, 0)
	scope := make(map[string]interface{})
	labels = append(labels, scope)
	return &parserImpl{nil, false, false, labels, "<input>", ".", false}
}

// Parse is the default implementation the Parse() method of Parser.
func (p *parserImpl) Parse(expr string) (Sequence, LispError) {
	var err error
	if p.foldcase {
		expr = "#!fold-case\n" + expr
	}
	p.tokens, err = lex(p.name, expr)
	if err != nil {
		return nil, NewLispError(ELEXER, err.Error())
	}
	defer drainLexer(p.tokens)
	results := NewPairBuilder()
	for {
		t, ok := <-p.tokens
		if !ok || t.typ == tokenEOF {
			break
		}
		elem, err := p.parserRead(t)
		if err != nil {
			return nil, err
		}
		results.Append(elem)
	}
	return results.List(), nil
}

// ParseFile is the default implementation the ParseFile() method of Parser.
func (p *parserImpl) ParseFile(filename string) (Sequence, LispError) {
	bytes, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, NewLispError(EIO, err.Error())
	}
	expr := string(bytes)
	p.include = path.Dir(filename)
	p.name = path.Base(filename)
	return p.Parse(expr)
}

// parseNext reads a complete expression from the channel of tokens.
func (p *parserImpl) parseNext() (interface{}, LispError) {
	t, ok := <-p.tokens
	if !ok {
		return nil, NewLispError(ESYNTAX, endOfStreamMsg)
	}
	return p.parserRead(t)
}

// parserRead reads a complete expression from the channel of tokens,
// starting with the initial token value provided.
func (p *parserImpl) parserRead(t token) (interface{}, LispError) {
	switch t.typ {
	case tokenError:
		return nil, NewLispError(ESYNTAX, t.val)
	case tokenEOF:
		return nil, NewLispError(ESYNTAX, endOfStreamMsg)
	case tokenOpenParen, tokenVector, tokenByteVector:
		return p.parseWithinScope(t)
	case tokenCloseParen:
		return nil, NewLispError(ESYNTAX, "unexpected ')'")
	case tokenString:
		return NewParsedString(t.contents(), t.row, t.col-len(t.val)), nil
	case tokenInteger:
		val, err := atoi(t.val)
		if err != nil {
			return nil, err
		}
		return NewParsedInteger(val, t.row, t.col-len(t.val)), nil
	case tokenFloat:
		val, err := atof(t.val)
		if err != nil {
			return nil, err
		}
		return NewParsedFloat(val, t.row, t.col-len(t.val)), nil
	case tokenComplex:
		val, err := atoc(t.val)
		if err != nil {
			return nil, err
		}
		return NewParsedComplex(val, t.row, t.col-len(t.val)), nil
	case tokenRational:
		a, b, err := ator(t.val)
		if err != nil {
			return nil, err
		}
		return NewParsedRational(a, b, t.row, t.col-len(t.val)), nil
	case tokenBoolean:
		return NewParsedBoolean(t.val, t.row, t.col-len(t.val)), nil
	case tokenCharacter:
		if len(t.val) > 3 {
			// lexer probably messed up
			return nil, NewLispErrorf(ESYNTAX, "unrecognized character: %s", t.val)
		}
		return NewParsedCharacter(t.val, t.row, t.col-len(t.val)), nil
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
		pair, err := p.parseNext()
		if err != nil {
			return nil, err
		}
		return NewList(quote, pair), nil
	case tokenIdentifier:
		return NewParsedSymbol(t.val, t.row, t.col-len(t.val)), nil
	case tokenComment:
		// ignore the next datum (r7rs 7.1.2)
		p.inComment = true
		_, err := p.parseNext()
		p.inComment = false
		if err != nil {
			return nil, err
		}
		return theNone, nil
	case tokenLabelDefinition:
		// datum label definition
		p.withLabel = true
		datum, err := p.parseNext()
		p.withLabel = false
		if err != nil {
			return nil, err
		}
		if !p.inComment {
			// disregard datum label definitions while parsing a comment
			label := t.val[1 : len(t.val)-1]
			p.labels[len(p.labels)-1][label] = datum
			return resolveForwardRefs(datum)
		}
		return datum, nil
	case tokenLabelReference:
		// datum label reference
		if p.inComment {
			// disregard datum label references while parsing a comment
			return theNone, nil
		}
		label := t.val[1 : len(t.val)-1]
		for idx := len(p.labels) - 1; idx >= 0; idx-- {
			if datum, ok := p.labels[idx][label]; ok {
				return datum, nil
			}
		}
		if p.withLabel {
			return &forwardDatumRef{label, p}, nil
		}
		return nil, NewLispErrorf(ESYNTAX, "label reference before assignment: %s", label)
	}
	panic("unreachable code")
}

// resolveForwardRefs finds any ForwardReference instances and replaces
// them with the actual datum to which they refer. If any cannot be
// resolved, an error is returned.
func resolveForwardRefs(datum interface{}) (interface{}, LispError) {
	var examineThing func(thing interface{}) (interface{}, LispError) = nil
	examinePair := func(pair Pair) (val interface{}, err LispError) {
		var r Pair = pair
		for {
			val, err := examineThing(r.First())
			if err != nil {
				return nil, err
			}
			r.setFirst(val)
			if r.Rest() == theEmptyList {
				break
			} else if rr, ok := r.Rest().(Pair); ok {
				r = rr
			} else {
				val, err := examineThing(r.Rest())
				if err != nil {
					return nil, err
				}
				r.setRest(val)
				break
			}
		}
		return pair, nil
	}
	examineVector := func(vec Vector) (interface{}, LispError) {
		for ii := vec.Len() - 1; ii >= 0; ii-- {
			val, err := examineThing(vec.Get(ii))
			if err != nil {
				return nil, err
			}
			vec.Set(ii, val)
		}
		return vec, nil
	}
	examineThing = func(thing interface{}) (interface{}, LispError) {
		switch v := thing.(type) {
		case Pair:
			return examinePair(v)
		case Vector:
			return examineVector(v)
		case ForwardReference:
			val := v.Resolve()
			if val == nil {
				form := "label reference before assignment: %s"
				return nil, NewLispErrorf(ESYNTAX, form, v.Label())
			} else {
				return val, nil
			}
		default:
			return v, nil
		}
	}
	return examineThing(datum)
}

// parserReadPair expects to read the contents of a list, whether a proper
// list (one with '.' separating elements) or otherwise. This function assumes
// that the previous token was an open parenthesis.
func (p *parserImpl) parserReadPair(t token) (interface{}, LispError) {
	if t.typ == tokenCloseParen {
		return theEmptyList, nil
	}
	// read the first element in the list
	car_obj, err := p.parserRead(t)
	if err != nil {
		return nil, err
	}
	// check if the next token is a dot
	t, ok := <-p.tokens
	if !ok {
		return nil, NewLispError(ESYNTAX, endOfStreamMsg)
	}
	if t.typ == tokenIdentifier && t.val == "." {
		// read an improper list
		// skip over the dot and start parsing the next element
		cdr_obj, err := p.parseNext()
		if err != nil {
			return nil, err
		}
		t, ok = <-p.tokens
		if !ok {
			return nil, NewLispError(ESYNTAX, endOfStreamMsg)
		}
		if t.typ != tokenCloseParen {
			return nil, NewLispErrorf(ESYNTAX, "expected ')', but got %s", t.val)
		}
		return Cons(car_obj, cdr_obj), nil
	} else {
		// read a proper list
		cdr_obj, err := p.parserReadPair(t)
		if err != nil {
			return nil, err
		}
		return Cons(car_obj, cdr_obj), nil
	}
	panic("unreachable code")
}

// parseWithinScope constructs a new datum scope, parses whatever token is
// given, then tears down the scope upon exit. This is generally done when an
// open parenthesis (() is encountered, as well as vector and byte vector.
func (p *parserImpl) parseWithinScope(t token) (interface{}, LispError) {
	// set up a new scope for datum labels
	scope := make(map[string]interface{})
	p.labels = append(p.labels, scope)
	// defer removal of that scope
	defer func() {
		p.labels = p.labels[:len(p.labels)-1]
	}()
	switch t.typ {
	case tokenOpenParen:
		t, ok := <-p.tokens
		if !ok {
			return nil, NewLispError(ESYNTAX, endOfStreamMsg)
		}
		return p.parserReadPair(t)
	case tokenVector:
		slice := make([]interface{}, 0, 16)
		for t = range p.tokens {
			if t.typ == tokenCloseParen {
				return NewVector(slice), nil
			}
			val, err := p.parserRead(t)
			if err != nil {
				return nil, err
			}
			slice = append(slice, val)
		}
		return nil, NewLispError(ESYNTAX, endOfStreamMsg)
	case tokenByteVector:
		slice := make([]uint8, 0, 16)
		for t = range p.tokens {
			if t.typ == tokenCloseParen {
				return NewByteVector(slice), nil
			} else if t.typ == tokenInteger {
				val, err := atoi(t.val)
				if err != nil {
					return nil, err
				}
				if val < 0 || val > 255 {
					return nil, NewLispErrorf(ESYNTAX,
						"byte vector value out of range: %s", t.val)
				}
				slice = append(slice, uint8(val))
			} else {
				return nil, NewLispErrorf(ESYNTAX,
					"invalid byte vector element: %s", t.val)
			}
		}
		return nil, NewLispError(ESYNTAX, endOfStreamMsg)
	default:
		return nil, NewLispErrorf(ESYNTAX, "unexpected token: %s", t.val)
	}
	panic("unreachable code")
}

// Expand is the default implementation the Expand() method of Parser.
func (p *parserImpl) Expand(x interface{}) (interface{}, LispError) {
	return p.expand(x, true)
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
			// TODO: should use math/big.Int
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

// expandListSafely calls expand() on each element of the given list and
// returns any error that occurs.
func (p *parserImpl) expandListSafely(seq Sequence, toplevel bool) (val Sequence, err LispError) {
	expandWithPanic := func(x interface{}) interface{} {
		val, err := p.expand(x, toplevel)
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
	return seq.Map(expandWithPanic), nil
}

// Walk the tree of parser tokens, making optimizations and obvious
// fixes to enable easier interpretation, possibly signaling a syntax
// error if appropriate.
func (p *parserImpl) expand(x interface{}, toplevel bool) (interface{}, LispError) {
	if x == nil {
		return nil, NewLispError(ESYNTAX, "empty input")
	}
	seq, is_seq := x.(Sequence)
	if !is_seq {
		return x, nil
	}
	first := seq.First()
	for seq.Len() == 1 {
		// Check if thing inside is another sequence, in which case we
		// extract it and pretend the enclosing sequence did not exist.
		if ns, ok := first.(Sequence); ok {
			seq = ns
			first = seq.First()
		} else {
			break
		}
	}
	if sym, issym := first.(Symbol); issym {
		if atomsEqual(sym, quoteSym) {
			if seq.Len() != 2 {
				return nil, NewLispErrorl(ESYNTAX, seq, "quote requires datum")
			}
			return seq, nil

		} else if atomsEqual(sym, ifSym) {
			if seq.Len() == 3 {
				// (if t c) => (if t c ())
				seq = SequenceAppend(seq, theEmptyList)
			}
			if seq.Len() != 4 {
				return nil, NewLispErrorl(ESYNTAX, seq, "if too many/few arguments")
			}
			return p.expandListSafely(seq, false)

		} else if atomsEqual(sym, setSym) {
			if seq.Len() != 3 {
				return nil, NewLispErrorl(ESYNTAX, seq, "set requires 2 arguments")
			}
			name := seq.Second()
			// (set! non-var exp) => Error
			if _, ok := name.(Symbol); !ok {
				return nil, NewLispErrorl(ESYNTAX, name, "can only set! a symbol")
			}
			val, err := p.expand(seq.Third(), false)
			if err != nil {
				return nil, err
			}
			return NewList(setSym, name, val), nil

		} else if atomsEqual(sym, defineSym) || atomsEqual(sym, definesyntaxSym) {
			if seq.Len() < 3 {
				return nil, NewLispErrorl(ESYNTAX, seq, "define/define-syntax require 2+ arguments")
			}
			v := seq.Second()
			body := Cdr(Cdr(seq))
			if list, islist := v.(Pair); islist && list.Len() > 0 {
				// (define (f args) body) => (define f (lambda args body))
				fun := list.First()
				args := list.Second()
				lambda := NewList(lambdaSym, args)
				lambda.Join(body)
				seq = NewList(sym, fun, lambda)
				return p.expandListSafely(seq, false)
			} else {
				// (define non-var/list exp) => Error
				sym2, issym := v.(Symbol)
				if !issym {
					return nil, NewLispErrorl(ESYNTAX, v, "can define only a symbol")
				}
				val, err := p.expand(seq.Third(), false)
				if err != nil {
					return nil, err
				}
				if atomsEqual(sym, definesyntaxSym) {
					if !toplevel {
						return nil, NewLispErrorl(ESYNTAX, seq,
							"define-syntax only allowed at top level")
					}
					proc, err := Eval(val, theReportEnvironment)
					if err != nil {
						return nil, err
					}
					closure, isproc := proc.(Closure)
					if !isproc {
						return nil, NewLispErrorl(EARGUMENT, seq,
							"macro must be a procedure")
					}
					// (define-syntax v proc)
					macroTable[sym2] = closure
					return nil, nil
				}
				result := NewList(defineSym, sym2, val)
				return result, nil
			}

		} else if atomsEqual(sym, beginSym) {
			if seq.Len() == 1 {
				// (begin) => None
				return nil, nil
			}
			return p.expandListSafely(seq, toplevel)

		} else if atomsEqual(sym, includeSym) {
			return p.expandInclude(seq, toplevel)

		} else if atomsEqual(sym, includeCaseSym) {
			save_case := p.foldcase
			p.foldcase = true
			defer func() {
				p.foldcase = save_case
			}()
			return p.expandInclude(seq, toplevel)

		} else if atomsEqual(sym, lambdaSym) {
			// (lambda (x) e1 e2) => (lambda (x) (begin e1 e2))
			if seq.Len() < 3 {
				return nil, NewLispErrorl(ESYNTAX, seq, "lambda requires 2+ arguments")
			}
			vars := seq.Second()
			body := Cxr("cddr", seq)
			if vlist, islist := vars.(Pair); islist {
				// verify that all list elements are symbols
				iter := vlist.Iterator()
				for iter.HasNext() {
					elem := iter.Next()
					if _, issym := elem.(Symbol); !issym {
						return nil, NewLispErrorl(ESYNTAX, vars, "lambda arguments must be symbols")
					}
				}
			} else if _, issym := vars.(Symbol); !issym {
				// otherwise the args must be a single symbol
				return nil, NewLispErrorl(ESYNTAX, seq, "lambda argument must be a symbol")
			}
			body = wrapWithBegin(body)
			body, err := p.expand(body, false)
			if err != nil {
				return nil, err
			}
			return NewList(lambdaSym, vars, body), nil

		} else if atomsEqual(sym, quasiquoteSym) {
			// `x => expand quasiquote of x
			if seq.Len() != 2 {
				return nil, NewLispErrorl(ESYNTAX, seq, "quasiquote (`) require 2 arguments")
			}
			return expandQuasiquote(seq.Second())

		} else if _, ok := macroTable[sym]; ok {
			// (m arg...)
			if pair, ispair := seq.Rest().(Pair); !ispair {
				pair = NewPair(pair)
			}
			return nil, nil
			// pending macro implementation...
			// result, err := macro.Invoke(pair)
			// if err != nil {
			// 	return nil, err
			// }
			// return p.expand(result, toplevel)
		}
	}

	// if we reached this point, it must be a procedure call
	return p.expandListSafely(seq, false)
}

// expandInclude processes the (include ...) expression by reading the
// named files into the parsed syntax tree.
func (p *parserImpl) expandInclude(seq Sequence, toplevel bool) (Pair, LispError) {
	if seq.Len() < 2 {
		return nil, NewLispErrorl(EARGUMENT, seq, "include requires filenames")
	}
	// temporarily modify this parser's context
	save_name := p.name
	save_include := p.include
	defer func() {
		p.name = save_name
		p.include = save_include
	}()
	results := NewPair(beginSym)
	iter := seq.Iterator()
	// ignore the 'include' symbol we've already parsed
	iter.Next()
	for iter.HasNext() {
		elem := iter.Next()
		if filename, ok := elem.(String); ok {
			// parse the file's contents, expand, and join with the results
			inc, err := p.ParseFile(filename.Value())
			if err != nil {
				return nil, err
			}
			exp, err := p.expandListSafely(inc, true)
			if err != nil {
				return nil, err
			}
			results.Join(exp)
		} else {
			return nil, NewLispErrorl(EARGUMENT, seq, "include expects string arguments")
		}
	}
	return results, nil
}

// expandQuasiquote processes the (quasi-)quotation expression contained
// within an object (atom, pair, or vector), expanding them into a form
// suitable for evaluation.
func expandQuasiquote(x interface{}) (interface{}, LispError) {
	// Expand `x => 'x; `,x => x; `(,@x y) => (append x y)
	switch seq := x.(type) {
	case Pair:
		return expandQuasiPair(seq)
	case Vector:
		// convert to a list, expand quasi quotes, wrap in list->vector
		builder := NewPairBuilder()
		limit := seq.Len()
		for pos := 0; pos < limit; pos++ {
			builder.Append(seq.Get(pos))
		}
		list := builder.List()
		result, err := expandQuasiPair(list)
		if err != nil {
			return nil, err
		}
		return NewList(listToVectorSym, result), nil
	default:
		return NewList(quoteSym, x), nil
	}
}

// expandQuasiPair processes the (quasi-)quotation expression contained
// within a chained pair, expanding them into a form suitable for
// evaluation.
func expandQuasiPair(pair Pair) (interface{}, LispError) {
	if pair.Len() == 0 {
		return NewList(quoteSym, pair), nil
	}
	token := pair.First()
	sym, issym := token.(Symbol)
	if issym && atomsEqual(sym, unquotesplicingSym) {
		return nil, NewLispErrorl(ESYNTAX, pair, "can't splice here")
	}
	if issym && atomsEqual(sym, unquoteSym) {
		if pair.Len() != 2 {
			return nil, NewLispErrorl(ESYNTAX, pair, "unquote requires 1 argument")
		}
		return pair.Second(), nil
	}
	if nseq, is_seq := token.(Sequence); is_seq && nseq.Len() > 0 {
		if sym, issym := nseq.First().(Symbol); issym && atomsEqual(sym, unquotesplicingSym) {
			if nseq.Len() != 2 {
				return nil, NewLispErrorl(ESYNTAX, pair, "unquote splicing requires 1 argument")
			}
			expr, err := expandQuasiquote(pair.Rest())
			if err != nil {
				return nil, err
			}
			return NewList(appendSym, nseq.Second(), expr), nil
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

// WireStripper removes the location information from a locatable element
// and returns the underlying element. That is, a parsed string becomes a
// normal string, and a parsed number becomes a normal number.
type WireStripper interface {
	// StripLocation returns this element without location information.
	StripLocation() interface{}
}

// ParsedString is a Locatable String type.
type ParsedString struct {
	// embedding String would have been nice, but having the same name for
	// the type and a method confuses the compiler
	str String // String object
	row int    // line of text where string was encountered
	col int    // column where string started
}

// NewParsedString returns a Locatable String object.
func NewParsedString(val string, row, col int) String {
	return &ParsedString{NewString(val), row, col}
}

func (ps *ParsedString) CompareTo(other Atom) (int8, error) {
	if s, ok := other.(String); ok {
		return ps.str.CompareTo(s)
	}
	return 0, TypeMismatch
}

func (ps *ParsedString) EqualTo(other Atom) (bool, error) {
	if s, ok := other.(String); ok {
		return ps.str.EqualTo(s)
	}
	return false, TypeMismatch
}

func (ps *ParsedString) Eval() interface{} {
	return ps.str.Eval()
}

func (ps *ParsedString) String() string {
	return ps.str.String()
}

func (ps *ParsedString) Len() int {
	return ps.str.Len()
}

func (ps *ParsedString) Set(pos int, ch rune) error {
	return ps.str.Set(pos, ch)
}

func (ps *ParsedString) Value() string {
	return ps.str.Value()
}

func (ps *ParsedString) Location() (int, int) {
	return ps.row, ps.col
}

func (ps *ParsedString) StripLocation() interface{} {
	return ps.str
}

// ParsedBoolean is a Locatable Boolean type.
type ParsedBoolean struct {
	Boolean     // Boolean object
	row     int // line of text where string was encountered
	col     int // column where string started
}

// NewParsedBoolean returns a Locatable Boolean object.
func NewParsedBoolean(val string, row, col int) Boolean {
	return &ParsedBoolean{NewBoolean(val), row, col}
}

func (pb *ParsedBoolean) Location() (int, int) {
	return pb.row, pb.col
}

func (pb *ParsedBoolean) StripLocation() interface{} {
	return pb.Boolean
}

// ParsedSymbol is a Locatable Symbol type.
type ParsedSymbol struct {
	Symbol     // Symbol object
	row    int // line of text where string was encountered
	col    int // column where string started
}

// NewParsedSymbol returns a Locatable Symbol object.
func NewParsedSymbol(val string, row, col int) Symbol {
	return &ParsedSymbol{NewSymbol(val), row, col}
}

func (ps *ParsedSymbol) IsSymbol() bool {
	return true
}

func (ps *ParsedSymbol) Location() (int, int) {
	return ps.row, ps.col
}

func (ps *ParsedSymbol) StripLocation() interface{} {
	return ps.Symbol
}

// ParsedInteger is a Locatable Integer type.
type ParsedInteger struct {
	Integer     // Integer object
	row     int // line of text where string was encountered
	col     int // column where string started
}

// NewParsedInteger returns a Locatable Integer object.
func NewParsedInteger(val int64, row, col int) Integer {
	return &ParsedInteger{NewInteger(val), row, col}
}

func (pi *ParsedInteger) Location() (int, int) {
	return pi.row, pi.col
}

func (pi *ParsedInteger) StripLocation() interface{} {
	return pi.Integer
}

// ParsedFloat is a Locatable Float type.
type ParsedFloat struct {
	Float     // Float object
	row   int // line of text where string was encountered
	col   int // column where string started
}

// NewParsedFloat returns a Locatable Float object.
func NewParsedFloat(val float64, row, col int) Float {
	return &ParsedFloat{NewFloat(val), row, col}
}

func (pf *ParsedFloat) Location() (int, int) {
	return pf.row, pf.col
}

func (pf *ParsedFloat) StripLocation() interface{} {
	return pf.Float
}

// ParsedComplex is a Locatable Complex type.
type ParsedComplex struct {
	Complex     // Complex object
	row     int // line of text where string was encountered
	col     int // column where string started
}

// NewParsedComplex returns a Locatable Complex object.
func NewParsedComplex(val complex128, row, col int) Complex {
	return &ParsedComplex{NewComplex(val), row, col}
}

func (pc *ParsedComplex) Location() (int, int) {
	return pc.row, pc.col
}

func (pc *ParsedComplex) StripLocation() interface{} {
	return pc.Complex
}

// ParsedRational is a Locatable Rational type.
type ParsedRational struct {
	Rational     // Rational object
	row      int // line of text where string was encountered
	col      int // column where string started
}

// NewParsedRational returns a Locatable Rational object.
func NewParsedRational(a, b int64, row, col int) Rational {
	return &ParsedRational{NewRational(a, b), row, col}
}

func (pr *ParsedRational) Location() (int, int) {
	return pr.row, pr.col
}

func (pr *ParsedRational) StripLocation() interface{} {
	return pr.Rational
}

// ParsedCharacter is a Locatable Character type.
type ParsedCharacter struct {
	Character     // Character object
	row       int // line of text where character was encountered
	col       int // column where character started
}

// NewParsedCharacter returns a Locatable Character object.
func NewParsedCharacter(val string, row, col int) Character {
	return &ParsedCharacter{NewCharacter(val), row, col}
}

func (pc *ParsedCharacter) Location() (int, int) {
	return pc.row, pc.col
}

func (pc *ParsedCharacter) StripLocation() interface{} {
	return pc.Character
}
