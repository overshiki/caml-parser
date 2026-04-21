{-# LANGUAGE DeriveFunctor #-}
module CamlParser.Syntax.Type where

data TypeExpr
  = TVar String
  | TArrow TypeExpr TypeExpr
  | TTuple [TypeExpr]
  | TConstr String [TypeExpr]
  deriving (Eq, Show)
