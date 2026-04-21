{-# LANGUAGE ExistentialQuantification #-}
module CamlParser.Syntax.Decl where

import CamlParser.Syntax.Location
import CamlParser.Syntax.Type
import CamlParser.Syntax.Pattern
import CamlParser.Syntax.Expr

data ConstrDecl
  = CD0 String
  | CD1 String TypeExpr Bool
  deriving (Eq, Show)

data TypeDecl
  = TDAbstract
  | TDVariant [ConstrDecl]
  | TDRecord [(String, TypeExpr, Bool)]
  | TDAbbrev TypeExpr
  deriving (Eq, Show)

data PrimDesc = NotPrim | Prim Int String
  deriving (Eq, Show)

data Decl expr pat
  = DLet Bool [(pat, expr)]
  | DType [(String, [String], TypeDecl)]
  | DExc [ConstrDecl]
  | DExpr expr
  | DDirective String String
  | forall ext. DExt String (DeclExt expr pat ext)

data DeclExt expr pat ext = DeclExt
  { declExtName :: String
  , declExtValue :: ext
  }

data Toplevel expr pat
  = TImpl (Decl expr pat)
  | TIntf (IntfDecl expr pat)
  deriving (Eq, Show)

data IntfDecl expr pat
  = IValue [(String, TypeExpr, PrimDesc)]
  | IType [(String, [String], TypeDecl)]
  | IExc [ConstrDecl]
  | IDirective String String
  deriving (Eq, Show)

instance (Eq expr, Eq pat) => Eq (Decl expr pat) where
  DLet b1 xs1 == DLet b2 xs2 = b1 == b2 && xs1 == xs2
  DType xs1 == DType xs2 = xs1 == xs2
  DExc xs1 == DExc xs2 = xs1 == xs2
  DExpr e1 == DExpr e2 = e1 == e2
  DDirective a1 b1 == DDirective a2 b2 = a1 == a2 && b1 == b2
  DExt n1 _ == DExt n2 _ = n1 == n2
  _ == _ = False

instance (Show expr, Show pat) => Show (Decl expr pat) where
  show (DLet b xs) = "DLet " ++ show b ++ " " ++ show xs
  show (DType xs) = "DType " ++ show xs
  show (DExc xs) = "DExc " ++ show xs
  show (DExpr e) = "DExpr " ++ show e
  show (DDirective a b) = "DDirective " ++ show a ++ " " ++ show b
  show (DExt n _) = "DExt " ++ show n ++ " <ext>"
