{-# LANGUAGE OverloadedStrings #-}
module MLQE.Plugin where

import Control.Monad (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Megaparsec hiding (Token)
import CamlParser.Lexer.Token
import CamlParser.Syntax.Expr
import CamlParser.Syntax.Pattern
import CamlParser.Syntax.Type
import CamlParser.Syntax.Decl
import CamlParser.Syntax.Location
import CamlParser.Parser.Combinators
import CamlParser.Parser.Expr (parseExpr)
import CamlParser.Parser.Type (parseType)
import CamlParser.Plugin.Interface
import MLQE.Syntax

mlqeKeywords :: Map String Token
mlqeKeywords = Map.fromList
  [ ("qubit",    TokKeywordExt mlqeTag "qubit")
  , ("qdef",     TokKeywordExt mlqeTag "qdef")
  , ("qalloc",   TokKeywordExt mlqeTag "qalloc")
  , ("qlift",    TokKeywordExt mlqeTag "qlift")
  , ("measure",  TokKeywordExt mlqeTag "measure")
  , ("unitary",  TokKeywordExt mlqeTag "unitary")
  , ("kraus",    TokKeywordExt mlqeTag "kraus")
  , ("gate",     TokKeywordExt mlqeTag "gate")
  , ("channel",  TokKeywordExt mlqeTag "channel")
  , ("pulse",    TokKeywordExt mlqeTag "pulse")
  , ("complex",  TokKeywordExt mlqeTag "complex")
  , ("matrix",   TokKeywordExt mlqeTag "matrix")
  ]

mlqePlugin :: CamlParserPlugin
mlqePlugin = CamlParserPlugin
  { pluginName = "mlqe"
  , pluginKeywords = mlqeKeywords
  , pluginExprAtom = Nothing  -- qalloc, qlift, measure parse as ordinary applications
  , pluginDecl = Just mlqeDeclParser
  }

mlqeDeclParser :: Parser (Decl Expr Pattern)
mlqeDeclParser = do
  keyword "qdef"
  name <- identP
  choice
    [ parseQDefConcrete name
    , parseQDefParamAbstract name
    , parseQDefAbstract name
    ]

parseQDefAbstract :: String -> Parser (Decl Expr Pattern)
parseQDefAbstract name = do
  tok TokColon
  ty <- parseType
  void (tok TokSemiSemi)
  return $ DExt mlqeTag (DeclExt mlqeTag (QDefAbstract name (Just ty)))

parseQDefParamAbstract :: String -> Parser (Decl Expr Pattern)
parseQDefParamAbstract name = do
  tok TokOf
  pty <- parseType
  tok TokColon
  ty <- parseType
  void (tok TokSemiSemi)
  return $ DExt mlqeTag (DeclExt mlqeTag (QDefParamAbstract name pty ty))

parseQDefConcrete :: String -> Parser (Decl Expr Pattern)
parseQDefConcrete name = do
  tok TokEqual
  qe <- parseQExpr
  void (tok TokSemiSemi)
  return $ DExt mlqeTag (DeclExt mlqeTag (QDefConcrete name qe))

parseQExpr :: Parser QExpr
parseQExpr = parseQTensor

parseQTensor :: Parser QExpr
parseQTensor = do
  e1 <- parseQSeq
  (do tok TokStar
      e2 <- parseQTensor
      return $ QTensor e1 e2
   ) <|> return e1

parseQSeq :: Parser QExpr
parseQSeq = do
  e1 <- parseQAtom
  (do tok (TokInfix 1 "@")
      e2 <- parseQSeq
      return $ QSeq e1 e2
   ) <|> return e1

parseQAtom :: Parser QExpr
parseQAtom = choice
  [ do n <- identP
       (do e <- parens parseExpr
           return $ QParam n e
        ) <|> return (QVar n)
  , QId <$ keyword "id"
  ]
