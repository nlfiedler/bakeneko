//
// Copyright 2012-2013 Nathan Fiedler. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//

package core

//
// This lexer is fashioned after that which was presented by Rob Pike in the
// "Lexical Scanning in Go" talk (http://cuddle.googlecode.com/hg/talk/lex.html).
//

import (
	"bytes"
	"fmt"
	"strconv"
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
	_                    tokenType = iota // undefined
	tokenError                            // error occurred
	tokenComment                          // #; comment of next datum
	tokenString                           // string literal
	tokenQuote                            // quoted list
	tokenCharacter                        // character literal
	tokenIdentifier                       // identifier token
	tokenInteger                          // integer literal
	tokenFloat                            // floating point literal
	tokenComplex                          // complex number
	tokenRational                         // rational number
	tokenBoolean                          // boolean value (#t or #f)
	tokenOpenParen                        // open parenthesis
	tokenVector                           // beginning of vector
	tokenByteVector                       // beginning of byte vector
	tokenCloseParen                       // close parenthesis
	tokenLabelDefinition                  // datum label definition
	tokenLabelReference                   // datum label reference
	tokenEOF                              // end-of-file token
)

// token represents a token returned from the scanner.
type token struct {
	typ tokenType // Type, such as tokenFloat.
	val string    // Value, such as "23.2".
	row int       // line of program text where token was found
	col int       // column where token begins
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
	name    string     // used only for error reports
	input   string     // the string being scanned
	start   int        // start position of this token
	pos     int        // current position in the input
	width   int        // width of last rune read from input
	row     int        // current line of program text being read
	col     int        // current column of text being read
	folding bool       // true if fold-case is enabled
	tokens  chan token // channel of scanned tokens
}

// String returns a string representation of the lexer, useful for
// debugging.
func (l *lexer) String() string {
	return fmt.Sprintf("%s: '%s' [%d:%d]", l.name, l.input[l.start:l.pos], l.start, l.pos)
}

// stateFn represents the state of the scanner as a function that
// returns the next state.
type stateFn func(*lexer) stateFn

// lex initializes the lexer to lex the given Scheme command text, returning
// the channel from which tokens are received. Callers should follow this with
// a defer drainLexer(chan token) to ensure the channel is drained and the
// goroutine emitting tokens can exit.
func lex(name, input string) (chan token, error) {
	// simplify end-of-line characters
	if !utf8.ValidString(input) {
		return nil, InvalidUtf8String
	}
	input = strings.Replace(input, "\r\n", "\n", -1)
	input = strings.Replace(input, "\r", "\n", -1)
	l := &lexer{
		name:   name,
		input:  input,
		tokens: make(chan token),
		row:    1,
	}
	go l.run() // Concurrently run state machine.
	return l.tokens, nil
}

// drainLexer reads from the lexer channel until nothing is left, allowing the
// goroutine feeding the channel to exit normally.
func drainLexer(ch chan token) {
	for _ = range ch {
	}
}

// run lexes the input by executing state functions until the state is
// nil, which marks the end of the input.
func (l *lexer) run() {
	for state := lexStart; state != nil; {
		state = state(l)
	}
	close(l.tokens) // No more tokens will be delivered.
}

// emit passes the current token back to the client via the channel.
func (l *lexer) emit(t tokenType) {
	l.emitText(t, l.input[l.start:l.pos])
}

// emitText passes the given token back to the client via the channel.
func (l *lexer) emitText(t tokenType, text string) {
	l.tokens <- token{t, text, l.row, l.col}
	l.start = l.pos
}

// next returns the next rune in the input.
func (l *lexer) next() (r rune) {
	if l.pos >= len(l.input) {
		// signal that nothing was read this time
		l.width = 0
		return eof
	}
	r, l.width = utf8.DecodeRuneInString(l.input[l.pos:])
	l.pos += l.width
	// advance row/col values in lexer
	if r == '\n' {
		l.row++
		l.col = 0
	} else {
		// counting characters, not bytes
		l.col += 1
	}
	return r
}

// ignore skips over the pending input before this point.
func (l *lexer) ignore() {
	l.start = l.pos
}

// backup steps back one rune. Can be called only once per call to next.
func (l *lexer) backup() {
	// if width is zero, next() reached eof, don't adjust anything this time
	if l.width > 0 {
		l.pos -= l.width
		if l.input[l.pos] == '\n' {
			// move row/col to end of previously scanned line
			l.row--
			l.computeColumn()
		} else {
			l.col -= l.width
		}
	}
	_, l.width = utf8.DecodeLastRuneInString(l.input[:l.pos])
}

// rewind moves the current position back to the start of the current token.
func (l *lexer) rewind() {
	for ii := l.pos - 1; ii >= l.start; ii-- {
		// No need to be concerned with rune widths here, any byte in a UTF-8
		// rune that equals 0x0A is going to be a newline and nothing else.
		if l.input[ii] == '\n' {
			l.row--
		}
	}
	l.pos = l.start
	l.computeColumn()
	_, l.width = utf8.DecodeLastRuneInString(l.input[:l.pos])
}

// peek returns but does not consume the next rune in the input.
func (l *lexer) peek() rune {
	r := l.next()
	l.backup()
	return r
}

// computeColumn updates the col field to the correct value after having moved
// the pos to its new position within the input text.
func (l *lexer) computeColumn() {
	nl := strings.LastIndex(l.input[:l.pos], "\n")
	if nl == -1 {
		nl = 0
	} else {
		// don't count the newline itself
		nl++
	}
	l.col = utf8.RuneCountInString(l.input[nl:l.pos])
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
	old_pos := l.pos
	for strings.ContainsRune(valid, l.next()) {
	}
	l.backup()
	return old_pos < l.pos
}

// errorf returns an error token and terminates the scan by passing back
// a nil pointer that will be the next state.
func (l *lexer) errorf(format string, args ...interface{}) stateFn {
	l.emitText(tokenError, fmt.Sprintf(format, args...))
	lexError := func(l *lexer) stateFn {
		// At this point we just give up.
		return nil
	}
	return lexError
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
	case '[', ']', '{', '}':
		return l.errorf("use of reserved character: %c", r)
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
			// whitespace after comment is significant (R7RS 2.2),
			// but we ignore whitespace anyway
			l.ignore()
			return lexStart
		}
	}
	panic("unreachable code")
}

// lexBlockComment expects the current position to be the start of a block
// comment (#|...|#) and advances until it finds the end of the comment.
// Comments may be nested (#|..#|..|#..|#) but must be properly so, as stated
// in R7RS 2.2.
func lexBlockComment(l *lexer) stateFn {
	nesting := 1
	for {
		r := l.next()
		switch r {
		case '#':
			if r = l.next(); r == '|' {
				nesting++
			}
		case '|':
			if r = l.next(); r == '#' {
				if nesting--; nesting == 0 {
					l.ignore()
					return lexStart
				}
			}
		case eof:
			return l.errorf("improperly ended block comment starting at %d", l.start)
		}
	}
	panic("unreachable code")
}

// emitIdentifier will fold the case of the identifier if the #!fold-case
// directive is enabled, then emit the identifier to the token channel.
// Otherwise, no folding is performed before emitting the token, per the
// default. If the ident parameter is empty, the current token text will
// be emitted, otherwise the value of ident is emitted.
func (l *lexer) emitIdentifier(ident string) {
	if ident == "" {
		ident = l.input[l.start:l.pos]
	}
	if l.folding {
		ident = strings.ToLower(ident)
	}
	l.emitText(tokenIdentifier, ident)
}

// lexIdentifier processes the text at the current location as if it were
// an identifier.
func lexIdentifier(l *lexer) stateFn {
	r := l.next()
	// check for special case first characters that may be the start of
	// a number or used as identifiers all by themselves: + - . ...
	// (R7RS 2.1, 2.3, 4.1.4)
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
				// there is no .. in R7RS
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
		l.emitIdentifier("")
		return lexStart

	} else if r == '+' || r == '-' {
		// +/- may be the start of a number or an identifier
		r = l.peek()
		if unicode.IsDigit(r) || r == 'i' || r == 'I' {
			l.rewind()
			return lexNumber
		}
	} else if r == '|' {
		// form |identifier| allows anything except \
		for {
			r = l.next()
			if r == '\\' {
				return l.errorf("character %c not allowed in identifier %q",
					r, l.input[l.start:l.pos])
			} else if r == '|' {
				l.emitIdentifier("")
				return lexStart
			}
		}
	} else if r == '@' {
		return l.errorf("character not allowed to start identifier: %c", r)
	}

	// average case identifier that may contain \x escapes
	l.backup()
	ident := new(bytes.Buffer)
	for {
		r = l.next()
		if r == '\\' {
			// allow for \xXX[X[X]]; hex character escapes in identifiers
			if l.next() != 'x' {
				return l.errorf("missing 'x' after '\\': %q", l.input[l.start:l.pos])
			}
			hex := new(bytes.Buffer)
			for {
				r = l.next()
				if r == ';' {
					// skip over the semicolon
					r = l.next()
					break
				} else if r == eof {
					return l.errorf("unexpectedly reached end at %q",
						l.input[l.start:l.pos])
				}
				hex.WriteRune(r)
			}
			// verify this is a valid inline hex escape value
			ch, err := strconv.ParseInt(hex.String(), 16, 32)
			if err != nil {
				return l.errorf("invalid number: %v for %q", err, l.input[l.start:l.pos])
			}
			// convert the character now to save the trouble later
			ident.WriteRune(rune(ch))
		}
		// check for the end of the identifier (note that these are assumed
		// to not appear as the first character, as lexStart would have
		// sent control to some other state function)
		if r == eof || strings.ContainsRune("'\",`;() \t\n\r", r) {
			if r != eof {
				l.backup()
			}
			l.emitIdentifier(ident.String())
			return lexStart
		}
		// identifiers are letters, numbers, and extended characters (R7RS 2.1)
		if !isAlphaNumeric(r) && !strings.ContainsRune("!$%&*+-./:<=>?@^_~", r) {
			return l.errorf("invalid subsequent character: %q", l.input[l.start:l.pos])
		}
		ident.WriteRune(r)
	}
	panic("unreachable code")
}

// lexNumber expects the current position to be the start of a numeric
// literal, and advances to the end of the literal. It will parse both
// integer and floating point decimal values.
func lexNumber(l *lexer) stateFn {

	//
	// See R7RS 7.1.1 for detailed format for numeric constants
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
			return l.errorf("malformed unsigned integer: %q", l.input[l.start:l.pos])
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

// lexHash processes all of the # tokens.
func lexHash(l *lexer) stateFn {
	r := l.next()
	switch r {
	case 't', 'f', 'T', 'F':
		// allow for #true and #false
		l.acceptRun("aelrsu")
		sym := l.input[l.start+1 : l.pos]
		if len(sym) > 1 && sym != "true" && sym != "false" {
			return l.errorf("invalid boolean: %q", l.input[l.start:l.pos])
		}
		l.emit(tokenBoolean)
		return lexStart
	case '|':
		return lexBlockComment
	case '(':
		l.emit(tokenVector)
		return lexStart
	case 'b', 'd', 'e', 'i', 'o', 'x':
		// let lexNumber sort out the prefix
		l.rewind()
		return lexNumber
	case ';':
		// line comment, with optional space; parser does the real work
		l.accept(" ")
		l.emit(tokenComment)
		return lexStart
	case '\\':
		// check for one of the many special character names
		if l.folding {
			l.acceptRun("abcdeiklmnoprstuwABCDEIKLMNOPRSTUW")
		} else {
			l.acceptRun("abcdeiklmnoprstuw")
		}
		sym := l.input[l.start+2 : l.pos]
		if l.folding {
			sym = strings.ToLower(sym)
		}
		if sym == "newline" {
			l.emitText(tokenCharacter, "#\\\n")
		} else if sym == "space" {
			l.emitText(tokenCharacter, "#\\ ")
		} else if sym == "alarm" {
			l.emitText(tokenCharacter, "#\\\a")
		} else if sym == "backspace" {
			l.emitText(tokenCharacter, "#\\\b")
		} else if sym == "delete" {
			l.emitText(tokenCharacter, "#\\\u007f")
		} else if sym == "escape" {
			l.emitText(tokenCharacter, "#\\\u001b")
		} else if sym == "null" {
			l.emitText(tokenCharacter, "#\\\u0000")
		} else if sym == "return" {
			l.emitText(tokenCharacter, "#\\\r")
		} else if sym == "tab" {
			l.emitText(tokenCharacter, "#\\\t")
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
	case 'u':
		// byte vector support (e.g. #u8(...))
		if r := l.next(); r == '8' {
			if r := l.next(); r == '(' {
				l.emit(tokenByteVector)
				return lexStart
			}
		}
		return l.errorf("unrecognized hash value: %q", l.input[l.start:l.pos])
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		l.acceptRun("0123456789")
		if r := l.next(); r == '#' {
			l.emit(tokenLabelReference)
		} else if r == '=' {
			l.emit(tokenLabelDefinition)
		} else {
			return l.errorf("unrecognized hash value: %q", l.input[l.start:l.pos])
		}
		return lexStart
	case '!':
		// handle #!fold-case and #!no-fold-case directives (R7RS 2.1)
		l.acceptRun("no-fldcase")
		sym := l.input[l.start+2 : l.pos]
		if sym == "fold-case" {
			l.folding = true
		} else if sym == "no-fold-case" {
			l.folding = false
		} else {
			return l.errorf("invalid directive: %q", l.input[l.start:l.pos])
		}

		l.ignore()
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
