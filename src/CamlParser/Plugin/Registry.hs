module CamlParser.Plugin.Registry where

import Data.Map (Map)
import qualified Data.Map as Map
import CamlParser.Lexer.Token
import CamlParser.Plugin.Interface

newtype PluginRegistry = PluginRegistry
  { registryPlugins :: [CamlParserPlugin]
  }

emptyRegistry :: PluginRegistry
emptyRegistry = PluginRegistry []

registerPlugin :: CamlParserPlugin -> PluginRegistry -> PluginRegistry
registerPlugin p (PluginRegistry ps) = PluginRegistry (p : ps)

registryKeywords :: PluginRegistry -> Map String Token
registryKeywords (PluginRegistry ps) = Map.unions (map pluginKeywords ps)
