{-# LANGUAGE OverloadedStrings #-}
module CamlParser.Parser.Assembly where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.Megaparsec hiding (Token)
import CamlParser.Lexer.Token
import CamlParser.Lexer.Lexer
import CamlParser.Syntax.Expr
import CamlParser.Syntax.Pattern
import CamlParser.Syntax.Decl
import CamlParser.Syntax.Location
import CamlParser.Parser.Combinators
import CamlParser.Parser.Toplevel
import CamlParser.Plugin.Interface
import CamlParser.Plugin.Registry

assembleKeywordTable :: PluginRegistry -> Map String Token
assembleKeywordTable reg = Map.union (registryKeywords reg) keywordTableCore

runLexer :: PluginRegistry -> String -> Either String [Located Token]
runLexer reg input =
  case runParser (lexer kwTable) "" input of
    Left err -> Left (show err)
    Right toks -> Right toks
  where
    kwTable = assembleKeywordTable reg

runParserOnTokens :: Parser a -> [Located Token] -> Either String a
runParserOnTokens p toks =
  case runParser p "" toks of
    Left err -> Left (show err)
    Right x -> Right x

-- Assemble a toplevel parser that includes plugin declarations
assembleToplevelParser :: PluginRegistry -> Parser (Toplevel Expr Pattern)
assembleToplevelParser reg = do
  let declParsers = concatMap mkDecl (registryPlugins reg)
  if null declParsers
    then parseToplevel
    else choice (map (fmap TImpl) declParsers ++ [parseToplevel])
  where
    mkDecl p = case pluginDecl p of
      Nothing -> []
      Just d  -> [d]
