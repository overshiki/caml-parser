{-# LANGUAGE OverloadedStrings #-}
module MLQE.Syntax where

import CamlParser.Syntax.Expr
import CamlParser.Syntax.Pattern
import CamlParser.Syntax.Type
import CamlParser.Syntax.Decl

-- | MLQE-specific declaration extension
data MLQEDecl
  = QDefAbstract String (Maybe TypeExpr)      -- name, optional type
  | QDefConcrete String QExpr                 -- name, quantum expression
  | QDefParamAbstract String TypeExpr TypeExpr -- name, param type, result type
  deriving (Eq, Show)

-- | Quantum expression
data QExpr
  = QVar String
  | QSeq QExpr QExpr      -- @
  | QTensor QExpr QExpr   -- *
  | QId
  | QParam String Expr    -- name(classical_expr)
  deriving (Eq, Show)

-- We encode MLQE declarations in the generic extension mechanism
-- by storing them as DExt with a tag.
mlqeTag :: String
mlqeTag = "mlqe"
