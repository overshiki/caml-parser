module CamlParser.Lexer.Token where

data Token
  = TokIdent String
  | TokPrefix String
  | TokInfix Int String
  | TokSubtractive String
  | TokInt Integer
  | TokChar Char
  | TokFloat Double
  | TokString String
  | TokEOF
  -- Special symbols
  | TokEqual | TokEqualEqual | TokSharp
  | TokAmpersand | TokQuote | TokLParen | TokRParen
  | TokStar | TokComma | TokMinusGreater | TokDot
  | TokDotDot | TokDotLParen | TokDotLBracket
  | TokColon | TokColonColon | TokColonEqual
  | TokSemi | TokSemiSemi | TokLBracket | TokLBracketBar
  | TokLBracketLess | TokLessMinus | TokRBracket
  | TokUnderscore | TokUnderUnder | TokLBrace
  | TokBar | TokBarRBracket | TokGreaterRBracket | TokRBrace
  | TokAmpersandAmpersand | TokBarBar
  -- Keywords
  | TokAnd | TokAs | TokBegin | TokDo | TokDone | TokDownto
  | TokElse | TokEnd | TokException | TokFor | TokFun
  | TokFunction | TokIf | TokIn | TokLet | TokMatch
  | TokMutable | TokNot | TokOf | TokOr | TokPref | TokRec
  | TokThen | TokTo | TokTry | TokType | TokValue | TokWhen
  | TokWhere | TokWhile | TokWith
  -- Plugin-extensible tokens
  | TokKeywordExt String String
  deriving (Eq, Ord, Show)

isIdentToken :: Token -> Maybe String
isIdentToken (TokIdent s) = Just s
isIdentToken _ = Nothing
