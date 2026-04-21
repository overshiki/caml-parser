{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module CamlParser.Syntax.Pattern where

import CamlParser.Syntax.Location
import CamlParser.Syntax.Constant
import CamlParser.Syntax.Type

data PatternF r
  = PWild
  | PVar String
  | PAlias r String
  | PConstant Constant
  | PTuple [r]
  | PConstr0 String
  | PConstr1 String r
  | POr r r
  | PConstraint r TypeExpr
  | PRecord [(String, r)]
  | PExt String [r]
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Pattern = Pattern
  { unPattern :: PatternF Pattern
  , patLoc    :: Location
  } deriving (Eq, Show)
