module CamlParser.Lexer.Lexer where

import Control.Monad (void)
import Data.Char (chr, isLetter, isDigit, digitToInt)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec hiding (Token, token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import CamlParser.Lexer.Token
import CamlParser.Syntax.Location

type Lexer = Parsec Void String

spaceConsumer :: Lexer ()
spaceConsumer = L.space
  (void (some (oneOf " \t\n\r\f\v")))
  (L.skipLineComment "--" <|> L.skipBlockCommentNested "(*" "*)")
  empty

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Lexer String
symbol = L.symbol spaceConsumer

keywordTableCore :: Map String Token
keywordTableCore = Map.fromList
  [ ("and", TokAnd), ("as", TokAs), ("begin", TokBegin)
  , ("do", TokDo), ("done", TokDone), ("downto", TokDownto)
  , ("else", TokElse), ("end", TokEnd), ("exception", TokException)
  , ("for", TokFor), ("fun", TokFun), ("function", TokFunction)
  , ("if", TokIf), ("in", TokIn), ("let", TokLet)
  , ("match", TokMatch), ("mutable", TokMutable), ("not", TokNot)
  , ("of", TokOf), ("or", TokOr), ("prefix", TokPref)
  , ("rec", TokRec), ("then", TokThen), ("to", TokTo)
  , ("try", TokTry), ("type", TokType), ("value", TokValue)
  , ("when", TokWhen), ("where", TokWhere), ("while", TokWhile)
  , ("with", TokWith)
  , ("mod", TokInfix 3 "mod"), ("quo", TokInfix 3 "quo")
  , ("land", TokInfix 3 "land"), ("lor", TokInfix 2 "lor")
  , ("lxor", TokInfix 2 "lxor"), ("lsl", TokInfix 4 "lsl")
  , ("lsr", TokInfix 4 "lsr"), ("asr", TokInfix 4 "asr")
  ]

tokenP :: Map String Token -> Lexer (Located Token)
tokenP kwTable = located $ choice
  [ stringLiteral
  , charLiteral
  , try floatLiteral
  , intLiteral
  , operatorOrSymbol
  , identifier kwTable
  ]

located :: Lexer a -> Lexer (Located a)
located p = do
  pos1 <- getSourcePos
  v <- p
  pos2 <- getSourcePos
  return $ Located v (Location pos1 pos2)

identifier :: Map String Token -> Lexer Token
identifier kwTable = lexeme $ try $ do
  c <- letterChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  let s = c:cs
  return $ fromMaybe (TokIdent s) (Map.lookup s kwTable)

intLiteral :: Lexer Token
intLiteral = lexeme $ TokInt <$> choice
  [ try $ string "0x" *> L.hexadecimal
  , try $ string "0o" *> L.octal
  , try $ string "0b" *> L.binary
  , L.decimal
  ]

floatLiteral :: Lexer Token
floatLiteral = lexeme $ TokFloat <$> L.float

stringLiteral :: Lexer Token
stringLiteral = lexeme $ TokString <$> (char '"' *> many stringChar <* char '"')
  where
    stringChar = escapeSeq <|> satisfy (/= '"')
    escapeSeq = char '\\' *> choice
      [ '\n' <$ char 'n', '\r' <$ char 'r', '\t' <$ char 't'
      , '\b' <$ char 'b', '\\' <$ char '\\', '"' <$ char '"'
      , chr . read <$> count 3 digitChar
      ]

charLiteral :: Lexer Token
charLiteral = lexeme $ TokChar <$> between (char '`') (char '`') (escapeSeq <|> anySingleBut '`')
  where
    escapeSeq = char '\\' *> choice
      [ '\n' <$ char 'n', '\r' <$ char 'r', '\t' <$ char 't'
      , '\b' <$ char 'b', '\\' <$ char '\\', '`' <$ char '`'
      , chr . read <$> count 3 digitChar
      ]

operatorOrSymbol :: Lexer Token
operatorOrSymbol = lexeme $ choice $ map try
  [ TokMinusGreater <$ string "->"
  , TokDotDot <$ string ".."
  , TokDotLParen <$ string ".("
  , TokDotLBracket <$ string ".["
  , TokColonColon <$ string "::"
  , TokColonEqual <$ string ":="
  , TokSemiSemi <$ string ";;"
  , TokLessMinus <$ string "<-"
  , TokEqualEqual <$ string "=="
  , TokUnderUnder <$ string "__"
  , TokBarRBracket <$ string "|]"
  , TokLBracketBar <$ string "[|"
  , TokLBracketLess <$ string "[<"
  , TokGreaterRBracket <$ string ">]"
  , TokAmpersandAmpersand <$ string "&&"
  , TokBarBar <$ string "||"
  , TokEqual <$ string "="
  , TokSharp <$ string "#"
  , TokAmpersand <$ string "&"
  , TokQuote <$ string "'"
  , TokLParen <$ string "("
  , TokRParen <$ string ")"
  , TokStar <$ string "*"
  , TokComma <$ string ","
  , TokDot <$ string "."
  , TokColon <$ string ":"
  , TokSemi <$ string ";"
  , TokLBracket <$ string "["
  , TokRBracket <$ string "]"
  , TokUnderscore <$ string "_"
  , TokLBrace <$ string "{"
  , TokBar <$ string "|"
  , TokRBrace <$ string "}"
  , subtractiveOp
  , prefixOp
  , infixOp
  ]

subtractiveOp :: Lexer Token
subtractiveOp = choice [ TokSubtractive "-." <$ string "-.", TokSubtractive "-" <$ string "-" ]

prefixOp :: Lexer Token
prefixOp = TokPrefix <$> ((:) <$> oneOf "!?" <*> many camlSymbolChar)

infixOp :: Lexer Token
infixOp = choice
  [ TokInfix 0 <$> ((:) <$> oneOf "=<>|&~$" <*> many camlSymbolChar)
  , TokInfix 1 <$> ((:) <$> oneOf "@^" <*> many camlSymbolChar)
  , TokInfix 2 <$> ((:) <$> oneOf "+-" <*> many camlSymbolChar)
  , TokInfix 4 <$> (string "**" <> many camlSymbolChar)
  , TokInfix 3 <$> ((:) <$> oneOf "*/%" <*> many camlSymbolChar)
  ]

camlSymbolChar :: Lexer Char
camlSymbolChar = oneOf "!$%&*+-./:<=>?@^|~"

lexer :: Map String Token -> Lexer [Located Token]
lexer kwTable = spaceConsumer *> many (tokenP kwTable) <* eof

mkKeywordTable :: [(String, String)] -> Map String Token
mkKeywordTable pairs = Map.union keywordTableCore (Map.fromList [(kw, TokKeywordExt pid kw) | (pid, kw) <- pairs])
