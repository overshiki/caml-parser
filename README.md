# caml-parser

An extensible parser-combinator library for **Caml-Light**, written in Haskell, with a plugin architecture for embedding domain-specific languages such as the quantum DSL **MLQE**.

---

## Background

Caml-Light is the minimal yet complete core of the ML family—expressions, pattern matching, algebraic data types, and call-by-value evaluation. It has been a pedagogical and research vehicle since 1990. This project treats Caml-Light not as a closed language, but as an **extensible platform**: the core syntax is parsed by a combinator library, while EDSL authors can register new keywords, inject grammar productions, and extend the AST without modifying the core source code.

The immediate motivation is [**MLQE**](https://github.com/overshiki/mlqe/blob/master/doc/proposal.md), a proposed quantum programming extension that adds qubits, gates, channels, and pulse-level constructs on top of Caml-Light. Because "all valid Caml-Light programs remain valid MLQE programs," the parser must accept the full Caml-Light grammar and allow MLQE to plug in seamlessly.

---

## Design

### Three-Layer Architecture

```
┌─────────────────────────────────────────────┐
│  Layer 3: EDSL Plugins (MLQE, etc.)         │
│  - Register keywords                        │
│  - Inject grammar productions               │
│  - Extend AST via declarative hooks         │
├─────────────────────────────────────────────┤
│  Layer 2: Core Parser Combinators           │
│  - Precedence-climbing expression parser    │
│  - Pattern, type, and declaration parsers   │
│  - Built on Megaparsec                      │
├─────────────────────────────────────────────┤
│  Layer 1: Lexer & Token Stream              │
│  - Extensible keyword table                 │
│  - Source-location tracking                 │
│  - Full Caml-Light lexical structure        │
└─────────────────────────────────────────────┘
```

### Key Modules

| Module | Responsibility |
|--------|----------------|
| `CamlParser.Lexer.Lexer` | Tokenizes source text; merges core + plugin keywords |
| `CamlParser.Syntax.Expr` / `Pattern` / `Decl` | Core AST with `EExt` / `DExt` extension hooks |
| `CamlParser.Parser.Expr` | Full precedence ladder: `let` → `;` → `:=` → `if` → `,` → `\|\|` → `&&` → `not` → comparisons → `@` → `::` → `+` → `*` → `**` → unary `-` → application → projections → atoms |
| `CamlParser.Plugin.Interface` | `CamlParserPlugin` record: name, keywords, optional expr atom parser, optional declaration parser |
| `CamlParser.Parser.Assembly` | Combines lexer + plugins + parser into a runnable pipeline |
| `MLQE.Plugin` | Example plugin implementing `qdef` declarations and quantum expression syntax |

### Plugin Interface

A plugin is a simple record:

```haskell
data CamlParserPlugin = CamlParserPlugin
  { pluginName     :: String
  , pluginKeywords :: Map String Token
  , pluginExprAtom :: Maybe (Parser Expr)
  , pluginDecl     :: Maybe (Parser (Decl Expr Pattern))
  }
```

Registering a plugin extends the lexer keyword table and the toplevel declaration parser automatically:

```haskell
let reg = registerPlugin mlqePlugin emptyRegistry
    toks = runLexer reg "qdef bell = hadamard @ cnot;;"
    ast  = toks >>= parseToplevelTokens reg
```

### AST Extensibility

The core AST provides typed extension points:

```haskell
-- Expression extension: tag + sub-expressions
data ExprF r = … | EExt String [r] | …

-- Declaration extension: tag + opaque payload
data Decl expr pat = … | DExt String (DeclExt expr pat) | …
```

MLQE uses `DExt "mlqe"` to embed `qdef` declarations without changing core types.

---

## Usage

### Building

```bash
cabal build
```

### Running Tests

```bash
# Full suite (88 tests)
cabal test

# Single test (tasty awk patterns)
cabal test --test-options="-p 'qdef abstract'"
```

### Interactive Exploration

```bash
cabal repl
```

Example REPL session:

```haskell
> import CamlParser.Parser.Assembly
> import CamlParser.Plugin.Registry
> import MLQE.Plugin
>
> let reg = registerPlugin mlqePlugin emptyRegistry
> runLexer reg "let x = 1 + 2;;"
Right [Located TokLet ..., Located (TokIdent "x") ..., Located TokEqual ...,
       Located (TokInt 1) ..., Located (TokInfix 2 "+") ..., Located (TokInt 2) ...,
       Located TokSemiSemi ...]
>
> runLexer reg "qdef hadamard : 1 gate;;" >>= parseToplevelTokens reg
Right (TImpl (DExt "mlqe" (DeclExt {declExtName = "mlqe", declExtValue = QDefAbstract "hadamard" (Just (TConstr "gate" [TConstr "1" []]))})))
```

### Writing a Plugin

1. Define your keywords as a `Map String Token` using `TokKeywordExt`.
2. Implement any new declaration parsers.
3. Package them into a `CamlParserPlugin` record.
4. Register it with `registerPlugin` before lexing/parsing.

See [`src/MLQE/Plugin.hs`](./src/MLQE/Plugin.hs) for a complete working example.

---

## Test Suite

The test suite lives in [`test/Main.hs`](./test/Main.hs) and covers the full Caml-Light grammar plus plugin extensibility:

| Group | Count | Coverage |
|-------|-------|----------|
| **Lexer** | 15 | Integers (decimal, hex, octal, binary), floats (including `5.` and exponent notation), identifiers with accents/underscores, keyword boundaries, multi-character symbols (`->`, `::`, `:=`, `.[`, etc.), string/char escapes, nested comments, plugin keyword registration |
| **Expression Parser** | 28 | Literals, `let/in` (nested, multiple bindings, `rec`, mutual, `where`), `if/then/else` (dangling else), `fun`, `function`, `match`, `try`, sequences, assignments, full precedence/associativity (left for `+`/`-`/`*`/`@`; right for `::`/`**`/`@`), unary `-` / `-.`, curried application, tuples, list literals, vector/string projections |
| **Pattern Parser** | 10 | Wildcard, variables, constants, tuples, constructor application, cons (`::`) right-associative, list sugar, or-patterns (`\|`), aliases, record patterns |
| **Type Parser** | 6 | Variables, arrows (right-assoc), tuples (`*`), constructors (including multi-param `(int, string) either`), abbreviations |
| **Declaration Parser** | 11 | `let`/`let rec`, `type` (variant, record, abbreviation, parameterized), `exception` (with/without args), multiple `and`-separated decls, directives (`#open`), tuple patterns in bindings |
| **Projections & Assignments** | 5 | Record access (`r.field`), vector index (`v.(i)`), string index (`s.[i]`), reference assign (`:=`), record field update (`<-`) |
| **Plugin Integration** | 7 | Non-interference (same AST with/without plugin), keyword presence/absence, abstract/concrete/parameterized `qdef`, multiple plugins |
| **Negative Tests** | 6 | Unclosed strings/comments, unmatched parens, missing `in`, trailing `else`, missing operand |

Run all tests:

```bash
$ cabal test
…
All 88 tests passed (0.02s)
Test suite caml-parser-test: PASS
```

---

## License

MIT
