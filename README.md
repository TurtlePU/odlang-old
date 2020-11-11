# odlang

Pretty odd lang.

Includes lexer & parser of the most part of Od Lang' expressions, because the
rest is not even designed.

See `examples/` for examples. All of them can be fed to the parser as input to
see parser in action.

To start parser, run `stack run`. It consumes all input and tries to parse it.
Result of a parser is an AST, which can be output in two ways: concise (by
default) or verbose, with flags `-v` or `--verbose` passed through `stack run`.
