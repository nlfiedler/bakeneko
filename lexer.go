//
// Copyright 2012 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package liswat

//
// This lexer is fashioned after that which was presented by Rob Pike in the
// "Lexical Scanning in Go" talk (http://cuddle.googlecode.com/hg/talk/lex.html).
//

import (
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

// tokenType is the type of a lexer token (e.g. string, number).
type tokenType int

// eof marks the end of the input text.
const eof = unicode.UpperLower

// token types
const (
	_                tokenType = iota // undefined
	tokenError                        // error occurred
	tokenString                       // string literal
	tokenQuote                        // quoted list
	tokenCharacter                    // character literal
	tokenIdentifier                   // identifier token
	tokenInteger                      // integer literal
	tokenFloat                        // floating point literal
	tokenComplex                      // complex number
	tokenRational                     // rational number
	tokenBoolean                      // boolean value (#t or #f)
	tokenOpenParen                    // open parenthesis
	tokenStartVector                  // beginning of vector
	tokenCloseParen                   // close parenthesis
	tokenEOF                          // end-of-file token
)

// token represents a token returned from the scanner.
type token struct {
	typ tokenType // Type, such as tokenFloat.
	val string    // Value, such as "23.2".
}

// String returns the string representation of the lexer token.
func (t *token) String() string {
	switch t.typ {
	case tokenEOF:
		return "EOF"
	case tokenError:
		return t.val
	}
	if len(t.val) > 10 {
		return fmt.Sprintf("%.10q...", t.val)
	}
	return fmt.Sprintf("%q", t.val)
}

// quotes indicates whether the token value starts and ends with double
// quotes ("). The first boolean return value is true if the token value
// starts with ", while the second return value is true if the token
// value ends with ". If the token value is a single character, the
// second return value is always false. If the token is not a quoted
// token, the return values will be false, false.
func (t *token) quotes() (bool, bool) {
	if t.typ == tokenString && len(t.val) > 0 {
		l := len(t.val)
		if l == 1 {
			return t.val[0] == '"', false
		} else {
			return t.val[0] == '"', t.val[l-1] == '"'
		}
	} else {
		return false, false
	}
	panic("unreachable code")
}

// contents returns the unique portion of the token text, minus any
// markers such as braces or brackets. For tokenEOF this will return
// nil.
func (t *token) contents() string {
	switch t.typ {
	case tokenString:
		qb, qe := t.quotes()
		l := len(t.val)
		if qb {
			if qe {
				return t.val[1 : l-1]
			}
			return t.val[1:]
		} else if qe {
			return t.val[:l-1]
		}
		return t.val
	default:
		return t.val
	}
	panic("unreachable code")
}

// lexer holds the state of the scanner.
type lexer struct {
	name   string     // used only for error reports.
	input  string     // the string being scanned.
	start  int        // start position of this token.
	pos    int        // current position in the input.
	width  int        // width of last rune read from input.
	tokens chan token // channel of scanned tokens.
}

// String returns a string representation of the lexer, useful for
// debugging.
func (l *lexer) String() string {
	return fmt.Sprintf("%s: '%s' [%d:%d]", l.name, l.input[l.start:l.pos], l.start, l.pos)
}

// stateFn represents the state of the scanner as a function that
// returns the next state.
type stateFn func(*lexer) stateFn

// lex initializes the lexer to lex the given Tcl command text,
// returning the channel from which tokens are received.
func lex(name, input string) chan token {
	l := &lexer{
		name:   name,
		input:  input,
		tokens: make(chan token),
	}
	go l.run() // Concurrently run state machine.
	return l.tokens
}

// run lexes the input by executing state functions until the state is
// nil, which marks the end of the input.
func (l *lexer) run() {
	for state := lexStart; state != nil; {
		state = state(l)
	}
	close(l.tokens) // No more tokens will be delivered.
}

// emit passes a token back to the client via the channel.
func (l *lexer) emit(t tokenType) {
	l.tokens <- token{t, l.input[l.start:l.pos]}
	l.start = l.pos
}

// next returns the next rune in the input.
func (l *lexer) next() (r rune) {
	if l.pos >= len(l.input) {
		l.width = 0
		return eof
	}
	r, l.width = utf8.DecodeRuneInString(l.input[l.pos:])
	l.pos += l.width
	return r
}

// ignore skips over the pending input before this point.
func (l *lexer) ignore() {
	l.start = l.pos
}

// backup steps back one rune. Can be called only once per call to next.
func (l *lexer) backup() {
	l.pos -= l.width
	l.width = 0
}

// rewind moves the current position back to the start of the current
// token.
func (l *lexer) rewind() {
	l.pos = l.start
	l.width = 0
}

// peek returns but does not consume the next rune in the input.
func (l *lexer) peek() rune {
	r := l.next()
	l.backup()
	return r
}

// accept consumes the next rune if it's from the valid set.
func (l *lexer) accept(valid string) bool {
	if strings.ContainsRune(valid, l.next()) {
		return true
	}
	l.backup()
	return false
}

// acceptRun consumes a run of runes from the valid set.
func (l *lexer) acceptRun(valid string) bool {
	pos := l.pos
	for strings.ContainsRune(valid, l.next()) {
	}
	l.backup()
	return pos < l.pos
}

// errorf returns an error token and terminates the scan by passing back
// a nil pointer that will be the next state.
func (l *lexer) errorf(format string, args ...interface{}) stateFn {
	l.tokens <- token{
		tokenError,
		fmt.Sprintf(format, args...),
	}
	return lexStart
}

// lexStart reads the next token from the input and determines
// what to do with that token, returning the appropriate state
// function.
func lexStart(l *lexer) stateFn {
	r := l.next()
	switch r {
	case eof:
		l.emit(tokenEOF)
		return nil
	case '(':
		l.emit(tokenOpenParen)
		return lexStart
	case ')':
		l.emit(tokenCloseParen)
		return lexStart
	case ' ', '\t', '\r', '\n':
		return lexSeparator
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		// let lexNumber sort out what type of number it is
		l.backup()
		return lexNumber
	case ';':
		return lexComment
	case '"':
		return lexString
	case '#':
		return lexHash
	case '\'', '`', ',':
		return lexQuote
	default:
		// let lexIdentifier sort out what exactly this is
		l.backup()
		return lexIdentifier
	}
	panic("unreachable code")
}

// lexString expects the current character to be a double-quote and
// scans the input to find the end of the quoted string.
func lexString(l *lexer) stateFn {
	for {
		r := l.next()
		switch r {
		case eof:
			return l.errorf("unclosed quoted string: %q", l.input[l.start:l.pos])
		case '\\':
			// pass over escaped characters
			l.next()
		case '"':
			// reached the end of the string
			l.emit(tokenString)
			return lexStart
		}
	}
	panic("unreachable code")
}

// lexSeparator expects the current position to be the start of a
// separator and advances until it finds the end of that separator.
// No token will be emitted since separators are ignored.
func lexSeparator(l *lexer) stateFn {
	l.acceptRun(" \t\n\r")
	l.ignore()
	return lexStart
}

// lexComment expects the current position to be the start of a
// comment and advances until it finds the end of the line/file.
// No token will be emitted since comments are ignored.
func lexComment(l *lexer) stateFn {
	for {
		r := l.next()
		switch r {
		case eof, '\n', '\r':
			// whitespace after comment is significant (r5rs 2.2),
			// but we ignore whitespace anyway
			l.ignore()
			return lexStart
		}
	}
	panic("unreachable code")
}

// lexIdentifier processes the text at the current location as if it were
// an identifier.
func lexIdentifier(l *lexer) stateFn {
	r := l.next()
	// check for special case first characters that may be the start
	// of a number or used as identifiers all by themselves, but
	// cannot be at the beginning of an identifier: + - . ...
	// (r5rs 2.1, 2.3, 4.1.4)
	if r == '.' {
		r = l.next()
		if r == '.' {
			r = l.next()
			if r == '.' {
				// ... must be followed by whitespace
				if !l.accept(" \t\r\n") {
					return l.errorf("malformed identifier: %q", l.input[l.start:l.pos])
				}
			} else {
				// there is no .. in r5rs
				return l.errorf("malformed identifier: %q", l.input[l.start:l.pos])
			}
		} else if unicode.IsDigit(r) {
			l.rewind()
			return lexNumber
		} else if r != ' ' && r != '\t' && r != '\r' && r != '\n' {
			// period must be whitespace delimited to be an identifier
			return l.errorf("malformed identifier: %q", l.input[l.start:l.pos])
		}
		l.backup()
		l.emit(tokenIdentifier)
		return lexStart

	} else if r == '+' || r == '-' {
		// +/- must be whitespace delimited to be a identifier
		r = l.peek()
		if unicode.IsDigit(r) || r == 'i' || r == 'I' {
			l.rewind()
			return lexNumber
		}
		if !l.accept(" \t\r\n") {
			// period must be whitespace delimited to be a identifier
			return l.errorf("malformed identifier: %q", l.input[l.start:l.pos])
		}
		l.backup()
		l.emit(tokenIdentifier)
		return lexStart
	}

	for {
		if r == eof {
			return l.errorf("unexpectedly reached end at %q", l.input[l.start:l.pos])
		}
		// check for the end of the identifier (note that these are assumed
		// to not appear as the first character, as lexStart would have
		// sent control to some other state function)
		if strings.ContainsRune("'\",`;() \t\n\r", r) {
			l.backup()
			l.emit(tokenIdentifier)
			return lexStart
		}
		// identifiers are letters, numbers, and extended characters (r5rs 2.1)
		if !isAlphaNumeric(r) && !strings.ContainsRune("!$%&*+-./:<=>?@^_~", r) {
			return l.errorf("malformed identifier: %q", l.input[l.start:l.pos])
		}
		r = l.next()
	}
	panic("unreachable code")
}

// lexNumber expects the current position to be the start of a numeric
// literal, and advances to the end of the literal. It will parse both
// integer and floating point decimal values.
func lexNumber(l *lexer) stateFn {

	//
	// See r5rs 7.1.1 for detailed format for numeric constants
	//
	float := false
	cmplx := false
	rational := false
	exact := true
	digits := "0123456789"

	// returns nil if okay, otherwise the error state function
	acceptPrefixR := func(l *lexer) stateFn {
		// we expect either exactness, radix, or both, in any order;
		// however, if we see more than one of either, that's an error
		baseSet := 0
		exactnessSet := 0
		for l.accept("#") {
			r := l.next()
			switch r {
			case 'd', 'D':
				baseSet++
			case 'b', 'B':
				baseSet++
				digits = "01"
			case 'o', 'O':
				baseSet++
				digits = "01234567"
			case 'x', 'X':
				baseSet++
				digits = "0123456789abcdefABCDEF"
			case 'e', 'E':
				exactnessSet++
			case 'i', 'I':
				exactnessSet++
			default:
				// unrecognized letter, signal an error
				baseSet = 10
			}
		}
		if baseSet > 1 || exactnessSet > 1 {
			return l.errorf("malformed number prefix: %q", l.input[l.start:l.pos])
		}
		return nil
	}

	// returns nil if okay, otherwise the error state function
	acceptUintegerR := func(tentative bool, l *lexer) stateFn {
		ok := l.acceptRun(digits)
		if !tentative && !ok {
			return l.errorf("malformed number: %q", l.input[l.start:l.pos])
		}
		if l.acceptRun("#") {
			exact = false
		}
		return nil
	}

	// returns nil if okay, otherwise the error state function
	acceptUrealR := func(tentative bool, l *lexer) stateFn {
		pos := l.pos
		if fn := acceptUintegerR(tentative, l); fn != nil {
			return fn
		}
		if l.accept("/") {
			if pos-l.pos == 1 {
				// there has to be something before the /
				return l.errorf("malformed rational: %q", l.input[l.start:l.pos])
			}
			rational = true
			if fn := acceptUintegerR(false, l); fn != nil {
				return fn
			}
		} else if len(digits) == 10 && l.accept(".") {
			float = true
			if exact {
				l.acceptRun(digits)
			} else {
				l.acceptRun("#")
			}
		}
		if l.accept("dDeEfFlLsS") {
			float = true
			l.accept("+-")
			l.acceptRun(digits)
		}
		return nil
	}

	// returns nil if okay, otherwise the error state function
	acceptRealR := func(tentative bool, l *lexer) stateFn {
		l.accept("+-")
		return acceptUrealR(tentative, l)
	}

	// scan for a scheme number
	if fn := acceptPrefixR(l); fn != nil {
		return fn
	}
	if fn := acceptRealR(true, l); fn != nil {
		return fn
	}
	if l.accept("@") {
		cmplx = true
		if fn := acceptRealR(false, l); fn != nil {
			return fn
		}
	} else if l.accept("+-") {
		cmplx = true
		if fn := acceptUrealR(true, l); fn != nil {
			return fn
		}
		if !l.accept("iI") {
			return l.errorf("malformed complex: %q", l.input[l.start:l.pos])
		}
	}
	if l.accept("iI") {
		cmplx = true
	}

	// Next thing must _not_ be alphanumeric.
	if isAlphaNumeric(l.peek()) {
		l.next()
		return l.errorf("malformed number: %q", l.input[l.start:l.pos])
	}
	if cmplx {
		l.emit(tokenComplex)
	} else if rational {
		l.emit(tokenRational)
	} else if float {
		l.emit(tokenFloat)
	} else {
		l.emit(tokenInteger)
	}
	return lexStart
}

// isAlphaNumeric indicates if the given rune is a letter or number.
func isAlphaNumeric(r rune) bool {
	return unicode.IsDigit(r) || unicode.IsLetter(r)
}

// lexHash process all of the # tokens.
func lexHash(l *lexer) stateFn {
	r := l.next()
	switch r {
	case 't', 'f', 'T', 'F':
		l.emit(tokenBoolean)
		return lexStart
	case '(':
		l.emit(tokenStartVector)
		return lexStart
	case 'b', 'd', 'e', 'i', 'o', 'x':
		// let lexNumber sort out the prefix
		l.rewind()
		return lexNumber
	case '\\':
		// check if 'space' or 'newline'
		l.acceptRun("aceilnpsw")
		sym := l.input[l.start+2 : l.pos]
		if sym == "newline" {
			l.tokens <- token{tokenCharacter, "#\\\n"}
			l.start = l.pos
		} else if sym == "space" {
			l.tokens <- token{tokenCharacter, "#\\ "}
			l.start = l.pos
		} else {
			// go back to #, consume #\...
			l.rewind()
			l.next()
			l.next()
			// ...and assert that it is a single character
			if !unicode.IsLetter(l.next()) {
				return l.errorf("malformed character escape: %q", l.input[l.start:l.pos])
			}
			if isAlphaNumeric(l.peek()) {
				l.next()
				return l.errorf("malformed character escape: %q", l.input[l.start:l.pos])
			}
			l.emit(tokenCharacter)
		}
		return lexStart
	default:
		return l.errorf("unrecognized hash value: %q", l.input[l.start:l.pos])
	}
	panic("unreachable code")
}

// lexQuote processes the special quoting characters.
func lexQuote(l *lexer) stateFn {
	// we already know it's one of the quoting characters, just need
	// to check if it is the two character ,@ form
	l.backup()
	r := l.next()
	if r == ',' {
		r = l.next()
		if r != '@' {
			l.backup()
		}
	}
	l.emit(tokenQuote)
	return lexStart
}
