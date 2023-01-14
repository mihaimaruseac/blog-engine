{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module: Config
Description: Parsing of the global config file
Copyright: (c) Mihai Maruseac 2023
License: BSD-3-Clause
Maintainer: mihai.maruseac@gmail.com
Stability: experimental
Portability: portable

Since a majority of configuration options are not passed as flags to Hakyll,
we put them in this config file. This way we don't need to always pass them as
flags to any invocation of the generator. Furthermore, we can also add
configuration options for other flags used in the supported commands, for
simplicity.

See also "CLI" module.
-}

module Config
  ( -- * Config parsing API
    parseConfig
    -- * Configuration structure
  , Config(..)
    -- * Patterns for configuring the generator
  , Patterns(..)
  ) where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import qualified Data.Yaml as Yaml
import Data.Yaml as Yaml ((.:), (.:?), (.!=))
import qualified Hakyll as HK
import System.Exit (die)

-- | The 'parseConfig' function handles the command line arguments.
parseConfig :: FilePath -> IO Config
parseConfig = Yaml.decodeFileEither @Config >=> \case
  Right result -> return result
  Left err -> die $ Yaml.prettyPrintParseException err

-- | The configuration structure.
--
-- We need to control flags that @hakyll@ uses in its internal configuration
-- as well as flags that are replacements for commonly used CLI flags, to
-- simplify invocation.
--
-- We might add other configurations in future, as needed, though ideally
-- everything will be optional, to preserve backwards compatibility.
data Config = Config
  { -- | The location of the blog articles and templates. This will be passed
    -- to @hakyll@'s 'Configuration.providerDirectory'.
    contentPath :: FilePath
  , -- | Default host for the @watch --host=HOST/IP@ option
    host :: Maybe String
  , -- | Default port for the @watch --port=PORT@ option
    port :: Maybe Int
  , -- | Patterns for the paths used by the generator. See
    -- 'Patterns' and "Rules" module.
    patterns :: Patterns
  } deriving (Show)

instance Yaml.FromJSON Config where
  parseJSON = Yaml.withObject "Config" $ \v -> Config
    <$> v .: "contentPath"
    <*> v .:? "host"
    <*> v .:? "port"
    <*> v .:? "patterns" .!= defaultPatterns

-- | The configuration of the patterns used by the generator.
--
-- This allows customizing the paths used in the generator to match in rules.
-- For example, the config file could specify a different path for the main
-- index file, or for the posts.
--
-- This structure is passed directly to the rules generator
-- ('Rules.siteRules').
data Patterns = Patterns
  { -- | pattern for @index.html@ at root of website (default: @index.html@)
    indexPattern :: HK.Pattern
  , -- | pattern for CSS files
    cssPattern :: HK.Pattern
  } deriving (Show)

instance Yaml.FromJSON Patterns where
  parseJSON = Yaml.withObject "Patterns" $ \v -> Patterns
    <$> parseOrDefault v "index" indexPattern
    <*> parseOrDefault v "css" cssPattern
    where
      parseOrDefault v key def = toHK (v .:? key) .!= def defaultPatterns
      toHK = fmap $ fmap unwrap

-- | Default patterns.
--
-- If the config file doesn't specify any pattern or some of these are
-- missing, we will fill the missing ones with the values from this.
--
-- Strings here are glob-patterns. Regexps are constructed explicitly.
defaultPatterns :: Patterns
defaultPatterns = Patterns
  { indexPattern = "index.html"
  , cssPattern = "css/*"
  }

-- | Netwtype to wrap around 'Hakyll.Core.Indetifier.Pattern.Pattern' to add a
-- 'FromJSON' instance
newtype WrappedPattern = WP { unwrap :: HK.Pattern } deriving (Show)

instance Yaml.FromJSON WrappedPattern where
  parseJSON = Yaml.withObject "Pattern" $ \v -> parseGlob v <|> parseRegex v
    where
      parseGlob v = WP . HK.fromGlob <$> v .: "glob"
      parseRegex v = WP . HK.fromRegex <$> v .: "regex"
