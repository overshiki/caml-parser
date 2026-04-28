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
    _   -> return $ foldl1 (\acc t2 -> case t2 of
        TConstr s _ -> TConstr s [acc]
        TVar s -> TConstr s [acc]
        _ -> error "invalid type constructor in application") ts

parseAtomicType :: Parser TypeExpr
parseAtomicType = choice
  [ TVar <$> (tok TokQuote >> identP)
  , TConstr <$> identP <*> return []
  , TConstr . show <$> intP <*> return []
  , do tok TokLParen
       t <- parseType
       (do comma
           ts <- sepBy1 parseType comma
           tok TokRParen
           c <- identP
           return $ TConstr c (t : ts)
        ) <|> (tok TokRParen >> return t)
  ]
