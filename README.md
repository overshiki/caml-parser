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
| `CamlParser.Parser.Expr` | Full precedence ladder: `let` → `if` → `,` → `\|\|` → `&&` → `=` → `::` → `+` → `*` → `**` → application → atoms |
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
# Full suite (20 tests)
cabal test

# Single test
cabal test --test-options="-p '/qdef abstract/'"
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

The test suite lives in [`test/Main.hs`](./test/Main.hs) and is organized into four groups:

| Group | Count | Coverage |
|-------|-------|----------|
| **Lexer** | 7 | Integers, identifiers, keywords, operators, strings, comments, plugin keyword registration |
| **Expression Parser** | 7 | Literals, `let/in`, `if/then/else`, application, tuples, binary ops, list literals |
| **Declaration Parser** | 4 | `let`, `let rec`, `type` variants, `exception` |
| **MLQE Plugin** | 2 | Abstract `qdef` (`: type`), concrete `qdef` (`= expr`) |

Run all tests:

```bash
$ cabal test
…
All 20 tests passed (0.01s)
Test suite caml-parser-test: PASS
```

---

## License

MIT
