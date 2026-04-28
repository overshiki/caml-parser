# Changelog

## 0.1.0.0 -- 2025-04-21

* Initial release.
* Full Caml-Light lexer with support for integers (decimal, hex, octal, binary),
  floats, strings, characters with escapes, nested comments, and extensible
  keyword tables.
* Precedence-climbing expression parser covering `let`, `if`, `match`,
  `function`, `fun`, `try`, sequences, assignments, and all infix levels.
* Pattern parser supporting wildcards, variables, constructors, tuples, lists,
  cons, or-patterns, aliases, and record patterns.
* Type parser for arrows, tuples, constructors, and variables.
* Declaration parser for `let`, `let rec`, `type`, `exception`, and directives.
* Plugin architecture via `CamlParserPlugin` for registering keywords and
  injecting custom parsers.
* Example MLQE plugin validating quantum gate definitions (`qdef`).
* Comprehensive test suite (88 tests).
