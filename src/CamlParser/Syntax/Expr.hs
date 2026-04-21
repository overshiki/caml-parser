{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module CamlParser.Syntax.Expr where

import CamlParser.Syntax.Location
import CamlParser.Syntax.Constant
import CamlParser.Syntax.Type
import CamlParser.Syntax.Pattern

data Ident
  = Local String
  | Global String String
  deriving (Eq, Show)

data StreamComponent r
  = STerm r
  | SNonterm r
  deriving (Eq, Show, Functor, Foldable, Traversable)

data StreamPattern
  = SPTerm Pattern
  | SPNonterm Expr Pattern
  | SPStream String
  deriving (Eq, Show)

data ExprF r
  = EIdent Ident
  | EConstant Constant
  | ETuple [r]
  | EConstruct0 String
  | EConstruct1 String r
  | EApply r [r]
  | ELet Bool [(Pattern, r)] r
  | EFunction [( [Pattern], r)]
  | EMatch r [(Pattern, r)]
  | ETry r [(Pattern, r)]
  | ESeq r r
  | EIf r r r
  | EWhile r r
  | EFor Ident r r Bool r
  | EConstraint r TypeExpr
  | EVector [r]
  | EAssign String r
  | ERecord [(String, r)]
  | ERecordAccess r String
  | ERecordUpdate r String r
  | EStream [StreamComponent r]
  | EParser [(StreamPattern, r)]
  | EWhen r r
  | EExt String [r]
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Expr = Expr
  { unExpr  :: ExprF Expr
  , exprLoc :: Location
  } deriving (Eq, Show)
