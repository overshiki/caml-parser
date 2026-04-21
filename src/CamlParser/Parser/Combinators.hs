{-# LANGUAGE OverloadedStrings #-}
module CamlParser.Parser.Combinators where

import Control.Monad (void)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import CamlParser.Lexer.Token
import CamlParser.Syntax.Location
import CamlParser.Syntax.Expr
import CamlParser.Syntax.Pattern

type Parser = Parsec Void [Located Token]

tok :: Token -> Parser Token
tok t = token testTok Set.empty
  where
    testTok (Located t' _)
      | t == t'   = Just t'
      | otherwise = Nothing

matchTok :: (Token -> Maybe a) -> Parser a
matchTok f = token (\(Located t' _) -> f t') Set.empty

identP :: Parser String
identP = matchTok isIdentToken <?> "identifier"
  where
    isIdentToken (TokIdent s) = Just s
    isIdentToken (TokKeywordExt _ s) = Just s
    isIdentToken _ = Nothing

intP :: Parser Integer
intP = matchTok isInt <?> "integer"
  where
    isInt (TokInt n) = Just n
    isInt _ = Nothing

floatP :: Parser Double
floatP = matchTok isFloat <?> "float"
  where
    isFloat (TokFloat f) = Just f
    isFloat _ = Nothing

stringP :: Parser String
stringP = matchTok isStr <?> "string"
  where
    isStr (TokString s) = Just s
    isStr _ = Nothing

charP :: Parser Char
charP = matchTok isCh <?> "char"
  where
    isCh (TokChar c) = Just c
    isCh _ = Nothing

keyword :: String -> Parser String
keyword kw = matchTok test <?> ("keyword " ++ kw)
  where
    test (TokKeywordExt _ s) | s == kw = Just s
    test _ = Nothing

located :: Parser a -> Parser (Located a)
located p = do
  Located _ loc1 <- lookAhead anySingle
  v <- p
  Located _ loc2 <- lookAhead anySingle
  return $ Located v (loc1 <> loc2)

parens :: Parser a -> Parser a
parens = between (tok TokLParen) (tok TokRParen)

brackets :: Parser a -> Parser a
brackets = between (tok TokLBracket) (tok TokRBracket)

braces :: Parser a -> Parser a
braces = between (tok TokLBrace) (tok TokRBrace)

semi, comma, dot, colon, equal, minusGreater :: Parser ()
semi = void (tok TokSemi)
comma = void (tok TokComma)
dot = void (tok TokDot)
colon = void (tok TokColon)
equal = void (tok TokEqual)
minusGreater = void (tok TokMinusGreater)

bar :: Parser ()
bar = void (tok TokBar)

andP :: Parser ()
andP = void (tok TokAnd)

infixOpP :: Int -> Parser String
infixOpP level = matchTok test <?> ("infix operator level " ++ show level)
  where
    test (TokInfix l s) | l == level = Just s
    test (TokStar)      | level == 3 = Just "*"
    test _ = Nothing

prefixOpP :: Parser String
prefixOpP = matchTok test <?> "prefix operator"
  where
    test (TokPrefix s) = Just s
    test _ = Nothing

subtractiveP :: Parser String
subtractiveP = matchTok test <?> "subtractive operator"
  where
    test (TokSubtractive s) = Just s
    test _ = Nothing

consP :: Parser ()
consP = void (tok TokColonColon)

assignP :: Parser ()
assignP = void (tok TokColonEqual)

lessMinusP :: Parser ()
lessMinusP = void (tok TokLessMinus)

optionalBar :: Parser ()
optionalBar = optional (tok TokBar) >> return ()
