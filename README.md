## An incomplete Scheme R7RS interpreter in Go ##

Eventually this will be a complete [Scheme](http://scheme-reports.org)
interpreter written in the [Go](http://golang.org) programming language. The
current target is Scheme R7RS. The lexer, parser, interpreter, and byte code
compiler and corresponding stack-based virtual machine are in place and
functional for a limited subset of Scheme. There is still a lot of work to be
done, touched on briefly in the TODO section below.

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
- Macro expansion
- Quasi-quoting
- Many standard procedures
- Defining and using libraries
- REPL

## License ##

The bakeneko project is licensed under the
[New BSD](http://opensource.org/licenses/BSD-3-Clause) license.
