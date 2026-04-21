module CamlParser.Syntax.Constant where

data Constant
  = CInt Integer
  | CFloat Double
  | CString String
  | CChar Char
  deriving (Eq, Show)
