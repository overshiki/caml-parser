{-# LANGUAGE DeriveFunctor #-}
module CamlParser.Syntax.Location where

import Text.Megaparsec (SourcePos)

data Location = Location
  { locStart :: SourcePos
  , locEnd   :: SourcePos
  } deriving (Eq, Ord, Show)

data Located a = Located
  { locatedValue :: a
  , locatedLoc   :: Location
  } deriving (Eq, Ord, Show, Functor)

instance Semigroup Location where
  Location s1 _ <> Location _ e2 = Location s1 e2

emptyLoc :: Location
emptyLoc = Location undefined undefined
