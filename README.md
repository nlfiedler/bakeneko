## An incomplete Scheme R7RS interpreter in Go ##

Eventually this will be a complete [Scheme](http://scheme-reports.org)
interpreter written in the [Go](http://golang.org) programming language.
The current target is Scheme R7RS. The lexer, parser, interpreter, and byte
code compiler and corresponding stack-based virtual machine are in place
and functional for a limited subset of Scheme. There is still a lot of work
to be done, touched on briefly in the TODO section below.

## Installation ##

1. Install the latest release of the Go compiler and tools; bakeneko will
always be using the latest released version.

1. Install [Bazaar](http://bazaar.canonical.com/en/) in order to retrieve
the [gocheck](http://labix.org/gocheck) package, which is used by the unit
tests.

1. Install [Git](http://git-scm.com) in order to fetch the other
dependencies.

1. Run the `go` tool like so:

    go get -t github.com/nlfiedler/bakeneko

## TODO ##

- Macro expansion
- Derived expressions: case, let, do, etc
- Quasi-quoting
- Many standard procedures
- Defining and using libraries
- REPL

## License ##

The bakeneko project is licensed under the
[New BSD](http://opensource.org/licenses/BSD-3-Clause) license.
