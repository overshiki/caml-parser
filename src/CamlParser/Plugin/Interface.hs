module CamlParser.Plugin.Interface where

import Data.Map (Map)
import CamlParser.Lexer.Token
import CamlParser.Syntax.Expr
import CamlParser.Syntax.Pattern
import CamlParser.Syntax.Decl
import CamlParser.Parser.Combinators

-- | A plugin can extend the lexer, expression parser, and declaration parser.
data CamlParserPlugin = CamlParserPlugin
  { pluginName :: String
  , pluginKeywords :: Map String Token
  , pluginExprAtom :: Maybe (Parser Expr)
  , pluginDecl :: Maybe (Parser (Decl Expr Pattern))
  }

nullPlugin :: String -> CamlParserPlugin
nullPlugin name = CamlParserPlugin name mempty Nothing Nothing
