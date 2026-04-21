{-# LANGUAGE OverloadedStrings #-}
module CamlParser.Parser.Expr where

import Control.Monad (void)
import Data.Functor (($>))
import Data.List (foldl')
import Text.Megaparsec hiding (Token)
import CamlParser.Lexer.Token
import CamlParser.Syntax.Location
import CamlParser.Syntax.Constant
import CamlParser.Syntax.Type
import CamlParser.Syntax.Pattern
import CamlParser.Syntax.Expr
import CamlParser.Parser.Combinators
import CamlParser.Parser.Pattern (parsePattern, parseAtomicPattern)
import CamlParser.Parser.Type (parseType)

parseExpr :: Parser Expr
parseExpr = parseLetExpr

locatedExpr :: Parser (ExprF Expr) -> Parser Expr
locatedExpr p = do
  Located _ loc1 <- lookAhead anySingle
  e <- p
  Located _ loc2 <- lookAhead anySingle
  return $ Expr e (loc1 <> loc2)

parseLetExpr :: Parser Expr
parseLetExpr = choice
  [ parseLetIn
  , parseFun
  , parseFunction
  , parseMatch
  , parseTry
  , parseIfExpr
  , parseWhile
  , parseFor
  , parseWhere
  , parseSeqExpr
  ]

parseLetIn :: Parser Expr
parseLetIn = locatedExpr $ do
  tok TokLet
  rec <- option False (tok TokRec $> True)
  binds <- parseBindingList
  tok TokIn
  body <- parseExpr
  return $ ELet rec binds body

parseBindingList :: Parser [(Pattern, Expr)]
parseBindingList = sepBy1 parseBinding (tok TokAnd)

parseBinding :: Parser (Pattern, Expr)
parseBinding = do
  pat <- parsePattern
  void equal
  e <- parseExpr
  return (pat, e)

parseFun :: Parser Expr
parseFun = locatedExpr $ do
  tok TokFun
  optionalBar
  cases <- parseFunCases
  return $ EFunction cases

parseFunction :: Parser Expr
parseFunction = locatedExpr $ do
  tok TokFunction
  optionalBar
  cases <- parseFunctionCases
  return $ EFunction (map (\(p, e) -> ([p], e)) cases)

parseMatch :: Parser Expr
parseMatch = locatedExpr $ do
  tok TokMatch
  e <- parseExpr
  tok TokWith
  optionalBar
  cases <- parseFunctionCases
  return $ EMatch e (map (\(p, e) -> (p, e)) cases)

parseTry :: Parser Expr
parseTry = locatedExpr $ do
  tok TokTry
  e <- parseExpr
  tok TokWith
  optionalBar
  cases <- parseTryCases
  return $ ETry e cases

parseFunCases :: Parser [([Pattern], Expr)]
parseFunCases = sepBy1 parseFunCase bar
  where
    parseFunCase = do
      pats <- some parseAtomicPattern
      void minusGreater
      e <- parseExpr
      return (pats, e)

parseFunctionCases :: Parser [(Pattern, Expr)]
parseFunctionCases = sepBy1 parseFunctionCase bar
  where
    parseFunctionCase = do
      pat <- parsePattern
      void minusGreater
      e <- parseExpr
      return (pat, e)

parseTryCases :: Parser [(Pattern, Expr)]
parseTryCases = sepBy1 parseTryCase bar
  where
    parseTryCase = do
      pat <- parsePattern
      void minusGreater
      e <- parseExpr
      return (pat, e)

parseIfExpr :: Parser Expr
parseIfExpr = locatedExpr $ do
  tok TokIf
  cond <- parseExpr
  tok TokThen
  then_ <- parseExpr
  (do tok TokElse
      else_ <- parseExpr
      return $ EIf cond then_ else_
   ) <|> return (EIf cond then_ (Expr (EConstruct0 "()") emptyLoc))

parseWhile :: Parser Expr
parseWhile = locatedExpr $ do
  tok TokWhile
  cond <- parseExpr
  tok TokDo
  body <- parseOptExpr
  tok TokDone
  return $ EWhile cond body

parseFor :: Parser Expr
parseFor = locatedExpr $ do
  tok TokFor
  i <- identP
  void equal
  start <- parseExpr
  up <- (tok TokTo $> True) <|> (tok TokDownto $> False)
  end <- parseExpr
  tok TokDo
  body <- parseOptExpr
  tok TokDone
  return $ EFor (Local i) start end up body

parseWhere :: Parser Expr
parseWhere = do
  e <- parseSeqExpr
  (do tok TokWhere
      rec <- option False (tok TokRec $> True)
      binds <- parseBindingList
      return $ Expr (ELet rec binds e) (exprLoc e)
   ) <|> return e

parseSeqExpr :: Parser Expr
parseSeqExpr = do
  e <- parseAssignExpr
  (do semi
      e2 <- parseSeqExpr
      return $ Expr (ESeq e e2) (exprLoc e <> exprLoc e2)
   ) <|> return e

parseAssignExpr :: Parser Expr
parseAssignExpr = do
  e <- parseCommaExpr
  choice
    [ do assignP
         r <- parseAssignExpr
         return $ Expr (EAssign (extractId e) r) (exprLoc e)
    , do lessMinusP
         r <- parseAssignExpr
         return $ Expr (EApply (Expr (EIdent (Local "<-")) emptyLoc) [e, r]) (exprLoc e)
    , return e
    ]
  where
    extractId (Expr (EIdent (Local s)) _) = s
    extractId _ = ""

parseCommaExpr :: Parser Expr
parseCommaExpr = do
  es <- sepBy1 parseOrExpr comma
  case es of
    [e] -> return e
    _   -> return $ Expr (ETuple es) (foldl1 (<>) (map exprLoc es))

parseOrExpr :: Parser Expr
parseOrExpr = parseLeftAssoc [TokOr, TokBarBar] parseAndExpr mkBinop
  where
    mkBinop e1 e2 = Expr (EApply (Expr (EIdent (Local "or")) emptyLoc) [e1, e2]) (exprLoc e1 <> exprLoc e2)

parseAndExpr :: Parser Expr
parseAndExpr = parseLeftAssoc [TokAmpersand, TokAmpersandAmpersand] parseNotExpr mkBinop
  where
    mkBinop e1 e2 = Expr (EApply (Expr (EIdent (Local "&&")) emptyLoc) [e1, e2]) (exprLoc e1 <> exprLoc e2)

parseNotExpr :: Parser Expr
parseNotExpr = choice
  [ do tok TokNot
       e <- parseNotExpr
       return $ Expr (EApply (Expr (EIdent (Local "not")) emptyLoc) [e]) (exprLoc e)
  , parseCmpExpr
  ]

parseCmpExpr :: Parser Expr
parseCmpExpr = parseLeftAssocOp [0] parseAppendExpr mkBinop
  where
    mkBinop op e1 e2 = Expr (EApply (Expr (EIdent (Local op)) emptyLoc) [e1, e2]) (exprLoc e1 <> exprLoc e2)

parseAppendExpr :: Parser Expr
parseAppendExpr = parseRightAssocOp [1] parseConsExpr mkBinop
  where
    mkBinop op e1 e2 = Expr (EApply (Expr (EIdent (Local op)) emptyLoc) [e1, e2]) (exprLoc e1 <> exprLoc e2)

parseConsExpr :: Parser Expr
parseConsExpr = do
  e1 <- parseAddExpr
  (do consP
      e2 <- parseConsExpr
      return $ Expr (EApply (Expr (EIdent (Local "::")) emptyLoc) [e1, e2]) (exprLoc e1 <> exprLoc e2)
   ) <|> return e1

parseAddExpr :: Parser Expr
parseAddExpr = parseLeftAssocOp [2] parseMulExpr mkBinop
  where
    mkBinop op e1 e2 = Expr (EApply (Expr (EIdent (Local op)) emptyLoc) [e1, e2]) (exprLoc e1 <> exprLoc e2)

parseMulExpr :: Parser Expr
parseMulExpr = parseLeftAssocOp [3] parsePowExpr mkBinop
  where
    mkBinop op e1 e2 = Expr (EApply (Expr (EIdent (Local op)) emptyLoc) [e1, e2]) (exprLoc e1 <> exprLoc e2)

parsePowExpr :: Parser Expr
parsePowExpr = parseRightAssocOp [4] parseNegExpr mkBinop
  where
    mkBinop op e1 e2 = Expr (EApply (Expr (EIdent (Local op)) emptyLoc) [e1, e2]) (exprLoc e1 <> exprLoc e2)

parseNegExpr :: Parser Expr
parseNegExpr = choice
  [ do s <- subtractiveP
       e <- parseNegExpr
       case s of
         "-"  -> return $ Expr (EApply (Expr (EIdent (Local "-")) emptyLoc) [e]) (exprLoc e)
         "-." -> return $ Expr (EApply (Expr (EIdent (Local "-.")) emptyLoc) [e]) (exprLoc e)
         _    -> fail "unknown subtractive"
  , parseAppExpr
  ]

parseAppExpr :: Parser Expr
parseAppExpr = do
  e <- parseProjExpr
  args <- many parseProjExpr
  case args of
    [] -> return e
    _  -> return $ Expr (EApply e args) (exprLoc e <> foldl1 (<>) (map exprLoc args))

parseProjExpr :: Parser Expr
parseProjExpr = do
  e <- parsePrefixExpr
  projections e
  where
    projections e = choice
      [ do dot
           f <- identP
           projections (Expr (ERecordAccess e f) (exprLoc e))
      , do tok TokDotLParen
           i <- parseExpr
           tok TokRParen
           projections (Expr (EApply (Expr (EIdent (Local "vect_item")) emptyLoc) [e, i]) (exprLoc e))
      , do tok TokDotLBracket
           i <- parseExpr
           tok TokRBracket
           projections (Expr (EApply (Expr (EIdent (Local "nth_char")) emptyLoc) [e, i]) (exprLoc e))
      , return e
      ]

parsePrefixExpr :: Parser Expr
parsePrefixExpr = choice
  [ do op <- prefixOpP
       e <- parsePrefixExpr
       return $ Expr (EApply (Expr (EIdent (Local op)) emptyLoc) [e]) (exprLoc e)
  , parseAtomicExpr
  ]

parseAtomicExpr :: Parser Expr
parseAtomicExpr = locatedExpr $ choice
  [ EConstant <$> parseConstant
  , EIdent <$> parseIdent
  , parseListExpr
  , parseVectorExpr
  , parseStreamExpr
  , parseRecordExpr
  , parseUnit
  , do tok TokLParen
       e <- parseExpr
       tok TokRParen
       return $ unExpr e
  , do tok TokLParen
       e <- parseExpr
       tok TokColon
       t <- parseType
       tok TokRParen
       return $ EConstraint e t
  , parseBeginEnd
  ]

parseUnit :: Parser (ExprF Expr)
parseUnit = EConstruct0 "()" <$ try (tok TokLParen >> tok TokRParen)

parseBeginEnd :: Parser (ExprF Expr)
parseBeginEnd = tok TokBegin >> EConstruct0 "()" <$ tok TokEnd  -- TODO: proper begin..end

parseIdent :: Parser Ident
parseIdent = do
  s <- identP
  (do tok TokUnderUnder
      m <- identP
      return $ Global s m
   ) <|> return (Local s)

parseConstant :: Parser Constant
parseConstant = choice
  [ CInt <$> intP
  , CFloat <$> floatP
  , CString <$> stringP
  , CChar <$> charP
  ]

parseListExpr :: Parser (ExprF Expr)
parseListExpr = do
  es <- brackets (sepBy parseExpr semi)
  return $ foldr (\x r -> EApply (Expr (EIdent (Local "::")) emptyLoc) [x, Expr r emptyLoc]) (EConstruct0 "[]") es

parseVectorExpr :: Parser (ExprF Expr)
parseVectorExpr = do
  tok TokLBracketBar
  es <- sepBy parseExpr semi
  tok TokBarRBracket
  return $ EVector es

parseRecordExpr :: Parser (ExprF Expr)
parseRecordExpr = do
  fields <- braces (sepBy fieldP semi)
  return $ ERecord fields
  where
    fieldP = do
      f <- identP
      void equal
      e <- parseExpr
      return (f, e)

parseStreamExpr :: Parser (ExprF Expr)
parseStreamExpr = do
  tok TokLBracketLess
  comps <- sepBy parseStreamComponent semi
  tok TokGreaterRBracket
  return $ EStream comps

parseStreamComponent :: Parser (StreamComponent Expr)
parseStreamComponent = choice
  [ tok TokQuote >> STerm <$> parseExpr
  , SNonterm <$> parseExpr
  ]

parseOptExpr :: Parser Expr
parseOptExpr = parseExpr <|> return (Expr (EConstruct0 "()") emptyLoc)

parseLeftAssoc :: [Token] -> Parser Expr -> (Expr -> Expr -> Expr) -> Parser Expr
parseLeftAssoc toks next mk = do
  e1 <- next
  rest <- many ((choice (map tok toks)) >> next)
  return $ foldl' mk e1 rest

parseLeftAssocOp :: [Int] -> Parser Expr -> (String -> Expr -> Expr -> Expr) -> Parser Expr
parseLeftAssocOp levels next mk = do
  e1 <- next
  rest <- many ((do op <- choice (map infixOpP levels)
                    return op) >>= \op -> next >>= \e2 -> return (op, e2))
  return $ foldl' (\e (op, e2) -> mk op e e2) e1 rest

parseRightAssocOp :: [Int] -> Parser Expr -> (String -> Expr -> Expr -> Expr) -> Parser Expr
parseRightAssocOp levels next mk = do
  e1 <- next
  rest <- many ((do op <- choice (map infixOpP levels)
                    return op) >>= \op -> next >>= \e2 -> return (op, e2))
  return $ foldr (\(op, e1') e2 -> mk op e1' e2) e1 (reverse rest)
