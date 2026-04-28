{-# LANGUAGE OverloadedStrings #-}
module CamlParser.Parser.Pattern where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Text.Megaparsec hiding (Token)
import CamlParser.Lexer.Token
import CamlParser.Syntax.Location
import CamlParser.Syntax.Constant
import CamlParser.Syntax.Pattern
import CamlParser.Syntax.Type
import CamlParser.Parser.Combinators
import CamlParser.Parser.Type (parseType)

parsePattern :: Parser Pattern
parsePattern = parseOrPattern

locatedPattern :: Parser (PatternF Pattern) -> Parser Pattern
locatedPattern p = do
  Located _ loc1 <- lookAhead anySingle
  pf <- p
  mbLoc <- optional (locatedLoc <$> lookAhead anySingle)
  return $ Pattern pf (loc1 <> fromMaybe loc1 mbLoc)

parseOrPattern :: Parser Pattern
parseOrPattern = do
  p <- parseConsPattern
  rest <- many (bar >> parseConsPattern)
  return $ foldr1 mkOr (p : rest)
  where
    mkOr a b = Pattern (POr a b) (patLoc a <> patLoc b)

parseConsPattern :: Parser Pattern
parseConsPattern = do
  p <- parseCtorAppOrAtomic
  (do tok TokColonColon
      q <- parseConsPattern
      return $ Pattern (PConstr1 "::" (Pattern (PTuple [p, q]) (patLoc p <> patLoc q))) (patLoc p <> patLoc q)
   ) <|> return p

parseCtorAppOrAtomic :: Parser Pattern
parseCtorAppOrAtomic = choice
  [ try parseCtorApp
  , parseAtomicPattern
  ]

parseCtorApp :: Parser Pattern
parseCtorApp = do
  c <- identP
  args <- some parseAtomicPattern
  return $ Pattern (PConstr1 c (mkTuplePat args)) emptyLoc

mkTuplePat :: [Pattern] -> Pattern
mkTuplePat [p] = p
mkTuplePat ps  = Pattern (PTuple ps) emptyLoc

parseAtomicPattern :: Parser Pattern
parseAtomicPattern = locatedPattern $ choice
  [ PWild <$ tok TokUnderscore
  , parseConstPattern
  , PVar <$> identP
  , PTuple <$> parens (sepBy1 parsePattern comma)
  , parseListPattern
  , parseRecordPattern
  , PConstraint <$> parens parsePattern <*> (colon >> parseType)
  ]

parseConstPattern :: Parser (PatternF Pattern)
parseConstPattern = choice
  [ PConstant . CInt <$> intP
  , PConstant . CFloat <$> floatP
  , PConstant . CString <$> stringP
  , PConstant . CChar <$> charP
  , do s <- subtractiveP
       case s of
         "-" -> PConstant . CInt . negate <$> intP
         "-." -> PConstant . CFloat . negate <$> floatP
         _ -> empty
  ]

parseListPattern :: Parser (PatternF Pattern)
parseListPattern = do
  ps <- brackets (sepBy parsePattern semi)
  return $ foldr (\x r -> PConstr1 "::" (Pattern (PTuple [x, Pattern r emptyLoc]) emptyLoc)) (PConstr0 "[]") ps

parseRecordPattern :: Parser (PatternF Pattern)
parseRecordPattern = do
  fields <- braces (sepBy fieldP semi)
  return $ PRecord fields
  where
    fieldP = do
      f <- identP
      void equal
      p <- parsePattern
      return (f, p)
