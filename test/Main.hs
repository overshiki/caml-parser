{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Map (Map)
import qualified Data.Map as Map

import CamlParser.Lexer.Token
import CamlParser.Lexer.Lexer
import CamlParser.Syntax.Expr
import CamlParser.Syntax.Pattern
import CamlParser.Syntax.Decl
import CamlParser.Syntax.Type
import CamlParser.Syntax.Constant
import CamlParser.Syntax.Location
import CamlParser.Parser.Assembly
import CamlParser.Parser.Combinators (keyword, identP, tok)
import CamlParser.Parser.Expr (parseExpr)
import CamlParser.Parser.Pattern (parsePattern)
import CamlParser.Parser.Type (parseType)
import CamlParser.Plugin.Registry
import CamlParser.Plugin.Interface
import MLQE.Plugin (mlqePlugin)
import Control.Monad (void)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "caml-parser Tests"
  [ lexerTests
  , exprTests
  , patternTests
  , typeTests
  , declTests
  , projTests
  , pluginTests
  , negativeTests
  ]

-- =====================================================================
-- Helpers
-- =====================================================================

lexString :: String -> Either String [Located Token]
lexString = runLexer emptyRegistry

parseExprTokens :: [Located Token] -> Either String Expr
parseExprTokens = runParserOnTokens parseExpr

parseToplevelTokens :: PluginRegistry -> [Located Token] -> Either String (Toplevel Expr Pattern)
parseToplevelTokens reg = runParserOnTokens (assembleToplevelParser reg)

parseExprStr :: String -> Either String Expr
parseExprStr s = lexString s >>= parseExprTokens

parseToplevelStr :: PluginRegistry -> String -> Either String (Toplevel Expr Pattern)
parseToplevelStr reg s = runLexer reg s >>= parseToplevelTokens reg

-- Shape helpers for precedence tests
exprShape :: Expr -> String
exprShape (Expr (EApply (Expr (EIdent (Local op)) _) args) _) = "(" ++ op ++ " " ++ unwords (map exprShape args) ++ ")"
exprShape (Expr (EConstant (CInt n)) _) = show n
exprShape (Expr (EConstant (CFloat f)) _) = show f
exprShape (Expr (EIdent (Local i)) _) = i
exprShape (Expr (ETuple es) _) = "(" ++ unwords (map exprShape es) ++ ")"
exprShape (Expr (EConstruct0 c) _) = c
exprShape (Expr (ESeq e1 e2) _) = "(; " ++ exprShape e1 ++ " " ++ exprShape e2 ++ ")"
exprShape (Expr (ELet _ _ _) _) = "let"
exprShape (Expr (EIf _ _ _) _) = "if"
exprShape (Expr (EFunction _) _) = "fun"
exprShape (Expr (ERecordAccess e f) _) = "(. " ++ exprShape e ++ " " ++ f ++ ")"
exprShape _ = "?"

patShape :: Pattern -> String
patShape (Pattern (PVar v) _) = v
patShape (Pattern PWild _) = "_"
patShape (Pattern (PConstant (CInt n)) _) = show n
patShape (Pattern (PConstr0 c) _) = c
patShape (Pattern (PConstr1 c p) _) = "(" ++ c ++ " " ++ patShape p ++ ")"
patShape (Pattern (PTuple ps) _) = "(" ++ unwords (map patShape ps) ++ ")"
patShape (Pattern (POr p1 p2) _) = "(| " ++ patShape p1 ++ " " ++ patShape p2 ++ ")"
patShape _ = "?pat"

typeShape :: TypeExpr -> String
typeShape (TVar v) = v
typeShape (TConstr c []) = c
typeShape (TConstr c [a]) = "(" ++ c ++ " " ++ typeShape a ++ ")"
typeShape (TConstr c as) = "(" ++ c ++ " " ++ unwords (map typeShape as) ++ ")"
typeShape (TTuple ts) = "(* " ++ unwords (map typeShape ts) ++ ")"
typeShape (TArrow a b) = "(-> " ++ typeShape a ++ " " ++ typeShape b ++ ")"

assertParseExpr :: String -> String -> Assertion
assertParseExpr src expected = case parseExprStr src of
  Right e -> assertEqual "expression shape" expected (exprShape e)
  Left err -> assertFailure $ "parse failed: " ++ err

assertParsePattern :: String -> String -> Assertion
assertParsePattern src expected = case lexString src >>= parsePatternTokens of
  Right p -> assertEqual "pattern shape" expected (patShape p)
  Left err -> assertFailure $ "parse failed: " ++ err
  where
    parsePatternTokens = runParserOnTokens parsePattern

assertParseType :: String -> String -> Assertion
assertParseType src expected = case lexString src >>= parseTypeTokens of
  Right t -> assertEqual "type shape" expected (typeShape t)
  Left err -> assertFailure $ "parse failed: " ++ err
  where
    parseTypeTokens = runParserOnTokens parseType

assertParseFails :: String -> Assertion
assertParseFails src = case parseExprStr src of
  Left _ -> return ()
  Right e -> assertFailure $ "expected parse failure, got: " ++ show e

assertLexFails :: String -> Assertion
assertLexFails src = case lexString src of
  Left _ -> return ()
  Right toks -> assertFailure $ "expected lex failure, got: " ++ show toks

-- =====================================================================
-- A. Lexer Edge Cases
-- =====================================================================

lexerTests :: TestTree
lexerTests = testGroup "Lexer"
  [ testCase "integer" $ do
      let toks = lexString "42"
      case toks of
        Right [Located (TokInt 42) _] -> return ()
        _ -> assertFailure $ "unexpected: " ++ show toks
  , testCase "identifier" $ do
      let toks = lexString "foo"
      case toks of
        Right [Located (TokIdent "foo") _] -> return ()
        _ -> assertFailure $ "unexpected: " ++ show toks
  , testCase "keyword let" $ do
      let toks = lexString "let"
      case toks of
        Right [Located TokLet _] -> return ()
        _ -> assertFailure $ "unexpected: " ++ show toks
  , testCase "operator" $ do
      let toks = lexString "+"
      case toks of
        Right [Located (TokInfix 2 "+") _] -> return ()
        _ -> assertFailure $ "unexpected: " ++ show toks
  , testCase "string literal" $ do
      let toks = lexString "\"hello\""
      case toks of
        Right [Located (TokString "hello") _] -> return ()
        _ -> assertFailure $ "unexpected: " ++ show toks
  , testCase "comment" $ do
      let toks = lexString "(* this is a comment *) 42"
      case toks of
        Right [Located (TokInt 42) _] -> return ()
        _ -> assertFailure $ "unexpected: " ++ show toks
  , testCase "mlqe keyword with plugin" $ do
      let toks = runLexer (registerPlugin mlqePlugin emptyRegistry) "qdef"
      case toks of
        Right [Located (TokKeywordExt "mlqe" "qdef") _] -> return ()
        _ -> assertFailure $ "unexpected: " ++ show toks
  -- New lexer tests
  , testCase "nested comments" $ do
      let toks = lexString "(* (* inner *) *) 42"
      case toks of
        Right [Located (TokInt 42) _] -> return ()
        _ -> assertFailure $ "unexpected: " ++ show toks
  , testCase "string escapes" $ do
      let toks = lexString "\"hello\\n\\t\\\\\\\"\""
      case toks of
        Right [Located (TokString "hello\n\t\\\"") _] -> return ()
        _ -> assertFailure $ "unexpected: " ++ show toks
  , testCase "char escapes" $ do
      let toks = lexString "`\\n` `\\t`"
      case toks of
        Right [Located (TokChar '\n') _, Located (TokChar '\t') _] -> return ()
        _ -> assertFailure $ "unexpected: " ++ show toks
  , testCase "float exponent" $ do
      let toks = lexString "1.5e-3"
      case toks of
        Right [Located (TokFloat 1.5e-3) _] -> return ()
        _ -> assertFailure $ "unexpected: " ++ show toks
  , testCase "alternative int bases" $ do
      let toks = lexString "0xFF 0o77 0b1010"
      case toks of
        Right [Located (TokInt 255) _, Located (TokInt 63) _, Located (TokInt 10) _] -> return ()
        _ -> assertFailure $ "unexpected: " ++ show toks
  , testCase "keyword boundary" $ do
      let toks = lexString "letrec"
      case toks of
        Right [Located (TokIdent "letrec") _] -> return ()
        _ -> assertFailure $ "unexpected: " ++ show toks
  , testCase "multi-char symbols" $ do
      let toks = lexString "-> :: := -. .("
      case toks of
        Right [Located TokMinusGreater _, Located TokColonColon _, Located TokColonEqual _,
               Located (TokSubtractive "-.") _, Located TokDotLParen _] -> return ()
        _ -> assertFailure $ "unexpected: " ++ show toks
  , testCase "accent/underscore idents" $ do
      let toks = lexString "foo' _foo foo_bar"
      case toks of
        Right [Located (TokIdent "foo'") _, Located (TokIdent "_foo") _, Located (TokIdent "foo_bar") _] -> return ()
        _ -> assertFailure $ "unexpected: " ++ show toks
  ]

-- =====================================================================
-- B. Expression Precedence & Associativity
-- =====================================================================

exprTests :: TestTree
exprTests = testGroup "Expression Parser"
  [ testCase "integer literal" $
      assertParseExpr "42;;" "42"
  , testCase "let in" $
      assertParseExpr "let x = 1 in x;;" "let"
  , testCase "if then else" $
      assertParseExpr "if true then 1 else 2;;" "if"
  , testCase "function application" $
      assertParseExpr "f x y;;" "(f x y)"
  , testCase "tuple" $
      assertParseExpr "(1, 2);;" "(1 2)"
  , testCase "binary operator" $
      assertParseExpr "1 + 2;;" "(+ 1 2)"
  , testCase "list literal" $ do
      let result = parseExprStr "[1; 2];;"
      case result of
        Right e -> case unExpr e of
          EApply _ [_, _] -> return ()
          _ -> assertFailure $ "unexpected list shape: " ++ show e
        Left err -> assertFailure err
  -- New expression tests
  , testCase "prec: mul over add" $
      assertParseExpr "1 + 2 * 3;;" "(+ 1 (* 2 3))"
  , testCase "assoc: left sub" $
      assertParseExpr "1 - 2 - 3;;" "(- (- 1 2) 3)"
  , testCase "assoc: right pow" $
      assertParseExpr "2 ** 3 ** 2;;" "(** 2 (** 3 2))"
  , testCase "assoc: right cons" $
      assertParseExpr "1 :: 2 :: [];;" "(:: 1 (:: 2 []))"
  , testCase "assoc: right append" $
      assertParseExpr "a @ b @ c;;" "(@ a (@ b c))"
  , testCase "prec: not over and" $
      assertParseExpr "not a && b;;" "(&& (not a) b)"
  , testCase "unary minus" $
      assertParseExpr "- 5;;" "(- 5)"
  , testCase "unary minus float" $
      assertParseExpr "-. 5.;;" "(-. 5.0)"
  , testCase "curried application" $
      assertParseExpr "f a b c;;" "(f a b c)"
  , testCase "mixed precedence" $
      assertParseExpr "a + b * c - d;;" "(- (+ a (* b c)) d)"
  , testCase "comparison chain" $
      assertParseExpr "a = b == c;;" "(== (= a b) c)"
  , testCase "nested let" $
      assertParseExpr "let x = 1 in let y = 2 in x + y;;" "let"
  , testCase "multiple bindings" $
      assertParseExpr "let x = 1 and y = 2 in x + y;;" "let"
  , testCase "fun multiple args" $
      assertParseExpr "fun x y z -> x + y + z;;" "fun"
  , testCase "function cases" $ do
      let result = parseExprStr "function | [] -> 0 | x::xs -> 1;;"
      case result of
        Right (Expr (EFunction _) _) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  , testCase "match cases" $ do
      let result = parseExprStr "match e with | A -> 1 | B -> 2 | _ -> 3;;"
      case result of
        Right (Expr (EMatch _ _) _) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  , testCase "try with" $ do
      let result = parseExprStr "try f x with | Failure _ -> 0;;"
      case result of
        Right (Expr (ETry _ _) _) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  , testCase "dangling else" $
      assertParseExpr "if a then if b then c else d;;" "if"
  , testCase "sequence" $
      assertParseExpr "a; b; c;;" "(; a (; b c))"
  , testCase "let rec mutual" $ do
      let result = parseExprStr "let rec f x = g x and g x = f x in f 1;;"
      case result of
        Right (Expr (ELet True _ _) _) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  , testCase "where clause" $
      assertParseExpr "x where x = 1;;" "let"
  ]

-- =====================================================================
-- C. Pattern Matching
-- =====================================================================

patternTests :: TestTree
patternTests = testGroup "Pattern Parser"
  [ testCase "wildcard" $
      assertParsePattern "_" "_"
  , testCase "variable" $
      assertParsePattern "x" "x"
  , testCase "constant" $
      assertParsePattern "42" "42"
  , testCase "tuple" $
      assertParsePattern "(x, y, z)" "(x y z)"
  , testCase "cons" $
      assertParsePattern "x :: xs" "(:: (x xs))"
  , testCase "list" $
      assertParsePattern "[a; b; c]" "(:: (a (:: (b (:: (c []))))))"
  , testCase "or pattern" $
      assertParsePattern "A | B" "(| A B)"
  , testCase "nested cons" $
      assertParsePattern "x :: y :: zs" "(:: (x (:: (y zs))))"
  , testCase "nested pattern" $
      assertParsePattern "(x, y) :: zs" "(:: ((x y) zs))"
  , testCase "record pattern" $ do
      let result = lexString "{a = x; b = y}" >>= runParserOnTokens parsePattern
      case result of
        Right (Pattern (PRecord _) _) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  ]

-- =====================================================================
-- D. Type Expressions
-- =====================================================================

typeTests :: TestTree
typeTests = testGroup "Type Parser"
  [ testCase "type variable" $
      assertParseType "'a" "a"
  , testCase "arrow right-assoc" $
      assertParseType "'a -> 'b -> 'c" "(-> a (-> b c))"
  , testCase "tuple type" $
      assertParseType "int * string * bool" "(* int string bool)"
  , testCase "constructor chain" $
      assertParseType "int list list" "(list (list int))"
  , testCase "multi-param ctor" $
      assertParseType "(int, string) either" "(either int string)"
  , testCase "mixed arrow tuple" $
      assertParseType "'a -> 'b * 'c -> 'd" "(-> a (-> (* b c) d))"
  ]

-- =====================================================================
-- E. Declarations
-- =====================================================================

declTests :: TestTree
declTests = testGroup "Declaration Parser"
  [ testCase "let def" $ do
      let result = parseToplevelStr emptyRegistry "let x = 1;;"
      case result of
        Right (TImpl (DLet False _)) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  , testCase "let rec def" $ do
      let result = parseToplevelStr emptyRegistry "let rec f x = x;;"
      case result of
        Right (TImpl (DLet True _)) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  , testCase "type def variant" $ do
      let result = parseToplevelStr emptyRegistry "type foo = A | B;;"
      case result of
        Right (TImpl (DType [("foo", _, TDVariant [CD0 "A", CD0 "B"])])) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  , testCase "exception def" $ do
      let result = parseToplevelStr emptyRegistry "exception Error;;"
      case result of
        Right (TImpl (DExc [CD0 "Error"])) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  -- New declaration tests
  , testCase "parameterized variant" $ do
      let result = parseToplevelStr emptyRegistry "type 'a option = None | Some of 'a;;"
      case result of
        Right (TImpl (DType [("option", ["a"], TDVariant [CD0 "None", CD1 "Some" (TVar "a") False])])) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  , testCase "type abbreviation" $ do
      let result = parseToplevelStr emptyRegistry "type 'a pair == 'a * 'a;;"
      case result of
        Right (TImpl (DType [("pair", ["a"], TDAbbrev (TTuple [TVar "a", TVar "a"]))])) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  , testCase "record type" $ do
      let result = parseToplevelStr emptyRegistry "type point = {x : int; y : int};;"
      case result of
        Right (TImpl (DType [("point", _, TDRecord [("x", TConstr "int" [], False), ("y", TConstr "int" [], False)])])) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  , testCase "exception with arg" $ do
      let result = parseToplevelStr emptyRegistry "exception Error of string;;"
      case result of
        Right (TImpl (DExc [CD1 "Error" (TConstr "string" []) False])) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  , testCase "multiple type decls" $ do
      let result = parseToplevelStr emptyRegistry "type a = A and b = B;;"
      case result of
        Right (TImpl (DType [("a", _, _), ("b", _, _)])) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  , testCase "directive" $ do
      let result = parseToplevelStr emptyRegistry "#open \"foo\";;"
      case result of
        Right (TImpl (DDirective "open" "foo")) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  , testCase "let tuple pat" $ do
      let result = parseToplevelStr emptyRegistry "let f (x, y) = x + y;;"
      case result of
        Right (TImpl (DLet False _)) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  ]

-- =====================================================================
-- F. Projections & Assignments
-- =====================================================================

projTests :: TestTree
projTests = testGroup "Projections & Assignments"
  [ testCase "record access" $
      assertParseExpr "r.field;;" "(. r field)"
  , testCase "vector access" $
      assertParseExpr "v.(i);;" "(vect_item v i)"
  , testCase "string access" $
      assertParseExpr "s.[i];;" "(nth_char s i)"
  , testCase "reference assign" $ do
      let result = parseExprStr "x := 1;;"
      case result of
        Right (Expr (EAssign "x" (Expr (EConstant (CInt 1)) _)) _) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  , testCase "record field update" $
      assertParseExpr "r.field <- e;;" "(<- (. r field) e)"
  ]

-- =====================================================================
-- G. Plugin Integration
-- =====================================================================

pluginTests :: TestTree
pluginTests = testGroup "Plugin Integration"
  [ testCase "plugin non-interference" $ do
      let core = parseToplevelStr emptyRegistry "let x = 1 in x + 2;;"
      let withMlqe = parseToplevelStr (registerPlugin mlqePlugin emptyRegistry) "let x = 1 in x + 2;;"
      case (core, withMlqe) of
        (Right (TImpl (DExpr ce)), Right (TImpl (DExpr me)))
          | exprShape ce == exprShape me -> return ()
        _ -> assertFailure "AST differs with/without plugin"
  , testCase "keyword absent without plugin" $ do
      let toks = lexString "gate"
      case toks of
        Right [Located (TokIdent "gate") _] -> return ()
        _ -> assertFailure $ "unexpected: " ++ show toks
  , testCase "keyword present with plugin" $ do
      let toks = runLexer (registerPlugin mlqePlugin emptyRegistry) "gate"
      case toks of
        Right [Located (TokKeywordExt "mlqe" "gate") _] -> return ()
        _ -> assertFailure $ "unexpected: " ++ show toks
  , testCase "qdef abstract" $ do
      let reg = registerPlugin mlqePlugin emptyRegistry
      let result = parseToplevelStr reg "qdef hadamard : 1 gate;;"
      case result of
        Right (TImpl (DExt "mlqe" _)) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  , testCase "qdef concrete" $ do
      let reg = registerPlugin mlqePlugin emptyRegistry
      let result = parseToplevelStr reg "qdef bell = hadamard @ cnot;;"
      case result of
        Right (TImpl (DExt "mlqe" _)) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  , testCase "qdef parameterized" $ do
      let reg = registerPlugin mlqePlugin emptyRegistry
      let result = parseToplevelStr reg "qdef phase of float : 1 gate;;"
      case result of
        Right (TImpl (DExt "mlqe" _)) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show result
  , testCase "multiple plugins" $ do
      let mock1 = CamlParserPlugin "mock1"
            (Map.fromList [("foo", TokKeywordExt "mock1" "foo")])
            Nothing
            (Just $ do keyword "foo"; n <- identP; void (tok TokSemiSemi)
                       return $ DExt "mock1" (DeclExt "mock1" ("foo:" ++ n)))
      let mock2 = CamlParserPlugin "mock2"
            (Map.fromList [("bar", TokKeywordExt "mock2" "bar")])
            Nothing
            (Just $ do keyword "bar"; n <- identP; void (tok TokSemiSemi)
                       return $ DExt "mock2" (DeclExt "mock2" ("bar:" ++ n)))
      let reg = registerPlugin mock2 (registerPlugin mock1 emptyRegistry)
      let r1 = parseToplevelStr reg "foo x;;"
      let r2 = parseToplevelStr reg "bar y;;"
      case (r1, r2) of
        (Right (TImpl (DExt "mock1" _)), Right (TImpl (DExt "mock2" _))) -> return ()
        _ -> assertFailure $ "unexpected: " ++ show (r1, r2)
  ]

-- =====================================================================
-- H. Negative Tests
-- =====================================================================

negativeTests :: TestTree
negativeTests = testGroup "Negative Tests"
  [ testCase "unclosed string" $ assertLexFails "\"hello"
  , testCase "unclosed comment" $ assertLexFails "(* hello"
  , testCase "unmatched paren" $ assertParseFails "(1 + 2;;"
  , testCase "missing in after let" $ assertParseFails "let x = 1 x + 2;;"
  , testCase "trailing else" $ assertParseFails "if true then 1 else;;"
  , testCase "missing operand" $ assertParseFails "1 + * 2;;"
  ]
