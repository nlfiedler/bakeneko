## An incomplete Scheme R7RS interpreter in Go ##

Eventually this will be a complete Scheme interpreter written in the
[Go](http://golang.org) programming language. The current target is
Scheme R7RS. The basic lexer, parser, and interpreter are in place and
functional for a limited subset of Scheme. There is still a lot of
work to be done, touched on briefly in the TODO section below.

## Installation ##

Run the `go` tool like so:

    go get github.com/nlfiedler/bakeneko

## TODO ##

- Syntactic keywords:
    - case
    - let
    - let*
    - letrec
    - do
    - =>
    - delay
    - else
    - quasiquote
    - unquote
    - unquote-splicing
- Support arbitrary numbers of procedure arguments
- Macro expansion
- Quasi-quoting
- Many standard procedures
- Defining and using libraries
- REPL
