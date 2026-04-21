{-# LANGUAGE OverloadedStrings #-}
module CamlParser.Parser.Toplevel where

import Control.Monad (void)
import Data.Functor (($>))
import Text.Megaparsec hiding (Token)
import CamlParser.Lexer.Token
import CamlParser.Syntax.Location
import CamlParser.Syntax.Expr
import CamlParser.Syntax.Pattern
import CamlParser.Syntax.Type
import CamlParser.Syntax.Decl
import CamlParser.Parser.Combinators
import CamlParser.Parser.Expr (parseExpr)
import CamlParser.Parser.Pattern (parsePattern)
import CamlParser.Parser.Type (parseType)

parseToplevel :: Parser (Toplevel Expr Pattern)
parseToplevel = choice
  [ parseImpl
  , parseIntf
  ]

parseImpl :: Parser (Toplevel Expr Pattern)
parseImpl = TImpl <$> parseImplPhrase

parseIntf :: Parser (Toplevel Expr Pattern)
parseIntf = TIntf <$> parseIntfPhrase

parseImplPhrase :: Parser (Decl Expr Pattern)
parseImplPhrase = choice
  [ parseLetDef
  , parseTypeDef
  , parseExcDef
  , parseDirective
  , parseExprDef
  ]

parseExprDef :: Parser (Decl Expr Pattern)
parseExprDef = do
  e <- parseExpr
  void (tok TokSemiSemi)
  return $ DExpr e

parseLetDef :: Parser (Decl Expr Pattern)
parseLetDef = do
  tok TokLet
  rec <- option False (tok TokRec $> True)
  binds <- parseBindingList
  void (tok TokSemiSemi)
  return $ DLet rec binds

parseBindingList :: Parser [(Pattern, Expr)]
parseBindingList = sepBy1 parseBinding (tok TokAnd)

parseBinding :: Parser (Pattern, Expr)
parseBinding = do
  pat <- parsePattern
  void equal
  e <- parseExpr
  return (pat, e)

parseTypeDef :: Parser (Decl Expr Pattern)
parseTypeDef = do
  tok TokType
  decls <- sepBy1 parseTypeDecl (tok TokAnd)
  void (tok TokSemiSemi)
  return $ DType decls

parseTypeDecl :: Parser (String, [String], TypeDecl)
parseTypeDecl = do
  params <- parseTypeParams
  name <- identP
  def <- parseTypeDefBody
  return (name, params, def)

parseTypeParams :: Parser [String]
parseTypeParams = choice
  [ parens (sepBy1 parseTypeVar comma)
  , fmap return parseTypeVar
  , return []
  ]

parseTypeVar :: Parser String
parseTypeVar = tok TokQuote >> identP

parseTypeDefBody :: Parser TypeDecl
parseTypeDefBody = choice
  [ do equal
       optionalBar
       TDVariant <$> sepBy1 parseConstrDecl bar
  , do equal
       TDRecord <$> braces (sepBy1 parseLabelDecl semi)
  , do tok TokEqualEqual
       TDAbbrev <$> parseType
  , return TDAbstract
  ]

parseConstrDecl :: Parser ConstrDecl
parseConstrDecl = choice
  [ try $ do name <- identP
             tok TokOf
             ty <- parseType
             return $ CD1 name ty False
  , CD0 <$> identP
  ]

parseLabelDecl :: Parser (String, TypeExpr, Bool)
parseLabelDecl = do
  mut <- option False (tok TokMutable $> True)
  name <- identP
  colon
  ty <- parseType
  return (name, ty, mut)

parseExcDef :: Parser (Decl Expr Pattern)
parseExcDef = do
  tok TokException
  decls <- sepBy1 parseConstrDecl (tok TokAnd)
  void (tok TokSemiSemi)
  return $ DExc decls

parseDirective :: Parser (Decl Expr Pattern)
parseDirective = do
  tok TokSharp
  dir <- identP
  arg <- option "" stringP
  void (tok TokSemiSemi)
  return $ DDirective dir arg

parseIntfPhrase :: Parser (IntfDecl Expr Pattern)
parseIntfPhrase = choice
  [ parseValueDecl
  , parseIntfTypeDef
  , parseIntfExcDef
  , parseIntfDirective
  ]

parseValueDecl :: Parser (IntfDecl Expr Pattern)
parseValueDecl = do
  tok TokValue
  decls <- sepBy1 parseValue1Decl (tok TokAnd)
  void (tok TokSemiSemi)
  return $ IValue decls

parseValue1Decl :: Parser (String, TypeExpr, PrimDesc)
parseValue1Decl = do
  name <- identP
  colon
  ty <- parseType
  desc <- option NotPrim (equal >> parsePrimDecl)
  return (name, ty, desc)

parsePrimDecl :: Parser PrimDesc
parsePrimDecl = do
  n <- intP
  s <- stringP
  return $ Prim (fromIntegral n) s

parseIntfTypeDef :: Parser (IntfDecl Expr Pattern)
parseIntfTypeDef = do
  tok TokType
  decls <- sepBy1 parseTypeDecl (tok TokAnd)
  void (tok TokSemiSemi)
  return $ IType decls

parseIntfExcDef :: Parser (IntfDecl Expr Pattern)
parseIntfExcDef = do
  tok TokException
  decls <- sepBy1 parseConstrDecl (tok TokAnd)
  void (tok TokSemiSemi)
  return $ IExc decls

parseIntfDirective :: Parser (IntfDecl Expr Pattern)
parseIntfDirective = do
  tok TokSharp
  dir <- identP
  arg <- option "" stringP
  void (tok TokSemiSemi)
  return $ IDirective dir arg
