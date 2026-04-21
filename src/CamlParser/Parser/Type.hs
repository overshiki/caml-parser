{-# LANGUAGE OverloadedStrings #-}
module CamlParser.Parser.Type where

import Text.Megaparsec hiding (Token)
import CamlParser.Lexer.Token
import CamlParser.Syntax.Type
import CamlParser.Parser.Combinators

parseType :: Parser TypeExpr
parseType = parseArrowType

parseArrowType :: Parser TypeExpr
parseArrowType = do
  t <- parseTupleType
  (do tok TokMinusGreater
      r <- parseArrowType
      return $ TArrow t r
   ) <|> return t

parseTupleType :: Parser TypeExpr
parseTupleType = do
  ts <- sepBy1 parseAppType (tok TokStar)
  case ts of
    [t] -> return t
    _   -> return $ TTuple ts

parseAppType :: Parser TypeExpr
parseAppType = do
  ts <- some parseAtomicType
  case ts of
    [t] -> return t
    _   -> let args = init ts
               con = case last ts of
                       TConstr s _ -> s
                       TVar s -> s
                       _ -> error "invalid type constructor in application"
           in return $ TConstr con args

parseAtomicType :: Parser TypeExpr
parseAtomicType = choice
  [ TVar <$> (tok TokQuote >> identP)
  , TConstr <$> identP <*> return []
  , TConstr . show <$> intP <*> return []
  , parens parseType
  ]
