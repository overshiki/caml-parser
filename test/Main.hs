{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import CamlParser.Lexer.Token
import CamlParser.Lexer.Lexer
import CamlParser.Syntax.Expr
import CamlParser.Syntax.Pattern
import CamlParser.Syntax.Decl
import CamlParser.Syntax.Type
import CamlParser.Syntax.Constant
import CamlParser.Syntax.Location
import CamlParser.Parser.Assembly
import CamlParser.Parser.Expr (parseExpr)
import CamlParser.Plugin.Registry
import CamlParser.Plugin.Interface
import MLQE.Plugin (mlqePlugin)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "caml-parser Tests"
  [ lexerTests
  , exprTests
  , declTests
  , mlqeTests
  ]

-- Helper to run lexer
lexString :: String -> Either String [Located Token]
lexString = runLexer emptyRegistry

-- Helper to parse tokens into an expression
parseExprTokens :: [Located Token] -> Either String Expr
parseExprTokens = runParserOnTokens parseExpr

-- Helper to parse tokens into a toplevel phrase
parseToplevelTokens :: PluginRegistry -> [Located Token] -> Either String (Toplevel Expr Pattern)
parseToplevelTokens reg = runParserOnTokens (assembleToplevelParser reg)

lexerTests :: TestTree
lexerTests = testGroup "Lexer"
  [ testCase "integer" $ do
      let toks = lexString "42"
      case toks of
        Right [Located (TokInt 42) _] -> return ()
        _ -> assertFailure $ "unexpected tokens: " ++ show toks
  , testCase "identifier" $ do
      let toks = lexString "foo"
      case toks of
        Right [Located (TokIdent "foo") _] -> return ()
        _ -> assertFailure $ "unexpected tokens: " ++ show toks
  , testCase "keyword let" $ do
      let toks = lexString "let"
      case toks of
        Right [Located TokLet _] -> return ()
        _ -> assertFailure $ "unexpected tokens: " ++ show toks
  , testCase "operator" $ do
      let toks = lexString "+"
      case toks of
        Right [Located (TokInfix 2 "+") _] -> return ()
        _ -> assertFailure $ "unexpected tokens: " ++ show toks
  , testCase "string literal" $ do
      let toks = lexString "\"hello\""
      case toks of
        Right [Located (TokString "hello") _] -> return ()
        _ -> assertFailure $ "unexpected tokens: " ++ show toks
  , testCase "comment" $ do
      let toks = lexString "(* this is a comment *) 42"
      case toks of
        Right [Located (TokInt 42) _] -> return ()
        _ -> assertFailure $ "unexpected tokens: " ++ show toks
  , testCase "mlqe keyword with plugin" $ do
      let toks = runLexer (registerPlugin mlqePlugin emptyRegistry) "qdef"
      case toks of
        Right [Located (TokKeywordExt "mlqe" "qdef") _] -> return ()
        _ -> assertFailure $ "unexpected tokens: " ++ show toks
  ]

exprTests :: TestTree
exprTests = testGroup "Expression Parser"
  [ testCase "integer literal" $ do
      let result = lexString "42;;" >>= parseExprTokens
      case result of
        Right (Expr (EConstant (CInt 42)) _) -> return ()
        _ -> assertFailure $ "unexpected parse: " ++ show result
  , testCase "let in" $ do
      let result = lexString "let x = 1 in x;;" >>= parseExprTokens
      case result of
        Right (Expr (ELet False [(_, Expr (EConstant (CInt 1)) _)] (Expr (EIdent (Local "x")) _)) _) -> return ()
        Right e -> assertFailure $ "parsed but unexpected shape: " ++ show e
        Left err -> assertFailure err
  , testCase "if then else" $ do
      let result = lexString "if true then 1 else 2;;" >>= parseExprTokens
      case result of
        Right (Expr (EIf _ (Expr (EConstant (CInt 1)) _) (Expr (EConstant (CInt 2)) _)) _) -> return ()
        _ -> assertFailure $ "unexpected parse: " ++ show result
  , testCase "function application" $ do
      let result = lexString "f x y;;" >>= parseExprTokens
      case result of
        Right (Expr (EApply (Expr (EIdent (Local "f")) _) [Expr (EIdent (Local "x")) _, Expr (EIdent (Local "y")) _]) _) -> return ()
        _ -> assertFailure $ "unexpected parse: " ++ show result
  , testCase "tuple" $ do
      let result = lexString "(1, 2);;" >>= parseExprTokens
      case result of
        Right (Expr (ETuple [Expr (EConstant (CInt 1)) _, Expr (EConstant (CInt 2)) _]) _) -> return ()
        _ -> assertFailure $ "unexpected parse: " ++ show result
  , testCase "binary operator" $ do
      let result = lexString "1 + 2;;" >>= parseExprTokens
      case result of
        Right (Expr (EApply (Expr (EIdent (Local "+")) _) [Expr (EConstant (CInt 1)) _, Expr (EConstant (CInt 2)) _]) _) -> return ()
        _ -> assertFailure $ "unexpected parse: " ++ show result
  , testCase "list literal" $ do
      let result = lexString "[1; 2];;" >>= parseExprTokens
      case result of
        Right e -> case unExpr e of
          EApply _ [_, _] -> return ()
          _ -> assertFailure $ "unexpected list shape: " ++ show e
        _ -> assertFailure $ "unexpected parse: " ++ show result
  ]

declTests :: TestTree
declTests = testGroup "Declaration Parser"
  [ testCase "let def" $ do
      let result = lexString "let x = 1;;" >>= parseToplevelTokens emptyRegistry
      case result of
        Right (TImpl (DLet False [(_, Expr (EConstant (CInt 1)) _)])) -> return ()
        _ -> assertFailure $ "unexpected parse: " ++ show result
  , testCase "let rec def" $ do
      let result = lexString "let rec f x = x;;" >>= parseToplevelTokens emptyRegistry
      case result of
        Right (TImpl (DLet True _)) -> return ()
        _ -> assertFailure $ "unexpected parse: " ++ show result
  , testCase "type def" $ do
      let result = lexString "type foo = A | B;;" >>= parseToplevelTokens emptyRegistry
      case result of
        Right (TImpl (DType [("foo", _, TDVariant [CD0 "A", CD0 "B"])])) -> return ()
        _ -> assertFailure $ "unexpected parse: " ++ show result
  , testCase "exception def" $ do
      let result = lexString "exception Error;;" >>= parseToplevelTokens emptyRegistry
      case result of
        Right (TImpl (DExc [CD0 "Error"])) -> return ()
        _ -> assertFailure $ "unexpected parse: " ++ show result
  ]

mlqeTests :: TestTree
mlqeTests = testGroup "MLQE Plugin"
  [ testCase "qdef abstract" $ do
      let reg = registerPlugin mlqePlugin emptyRegistry
      let result = runLexer reg "qdef hadamard : 1 gate;;" >>= parseToplevelTokens reg
      case result of
        Right (TImpl (DExt "mlqe" _)) -> return ()
        _ -> assertFailure $ "unexpected parse: " ++ show result
  , testCase "qdef concrete" $ do
      let reg = registerPlugin mlqePlugin emptyRegistry
      let result = runLexer reg "qdef bell = hadamard @ cnot;;" >>= parseToplevelTokens reg
      case result of
        Right (TImpl (DExt "mlqe" _)) -> return ()
        _ -> assertFailure $ "unexpected parse: " ++ show result
  ]
