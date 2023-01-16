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
    -- * Site local configuration (patterns, constant strings, etc.)
  , SiteConfig(..)
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
  , -- | Site configuration, parameterizing the generator.
    --
    -- This contains patterns to various rules and files as well as global
    -- constant strings. This makes it easy to alter the site configuration
    -- (e.g., change title, change posts location) without needing to
    -- recompile the generator.
    --
    -- See 'SiteConfig' and "Rules" module. Default values are given by
    -- 'defaultSiteConfig'.
    siteConfig :: SiteConfig
  } deriving (Show)

instance Yaml.FromJSON Config where
  parseJSON = Yaml.withObject "Config" $ \v -> Config
    <$> v .: "contentPath"
    <*> v .:? "host"
    <*> v .:? "port"
    <*> v .:? "config" .!= defaultSiteConfig

-- | The configuration of the generator.
--
-- This allows customizing the paths used in the generator to match in rules.
-- For example, the config file could specify a different path for the main
-- index file, or for the posts.
--
-- Similarly, we can use this to change site default title and other constant
-- strings that might not be specified in the templates. This way, we can
-- change them and rebuild the site without needing to recompile the
-- generator.
--
-- Note that it is better to push constants into the templates, this config
-- should only contain those config strings which would result in template
-- duplication if we push them into the template.
--
-- This structure is passed directly to the rules generator
-- ('Rules.siteRules').
data SiteConfig = SiteConfig
  { -- | default html page template
    defaultTemplate :: HK.Identifier
  , -- | template for index pages (e.g. root @index.html@)
    indexTemplate :: HK.Identifier
  , -- | template for post pages
    postTemplate :: HK.Identifier
  , -- | pattern for @index.html@ at root of website
    indexPattern :: HK.Pattern
  , -- | pattern for posts
    postPattern :: HK.Pattern
  , -- | pattern for comments
    commentPattern :: HK.Pattern
  , -- | strip prefix from 'postPattern' on publishing
    -- e.g., if 'postPattern' is @posts/*.md@ and 'stripPostOnPublish' is
    -- @posts/@, the resulting posts will be in the root of the site. If
    -- 'stripPostOnPublish' is @pos@ (partial match), the results will be
    -- in @ts/*@. If prefix doesn't match, no replacement is being done
    stripPostOnPublish :: String
  , -- | pattern for templates
    templatesPattern :: HK.Pattern
  , -- | pattern for CSS files
    cssPattern :: HK.Pattern
  , -- | pattern for custom fonts
    fontPattern :: HK.Pattern
  } deriving (Show)

instance Yaml.FromJSON SiteConfig where
  parseJSON = Yaml.withObject "SiteConfig" $ \v -> SiteConfig
    <$> parseOrDefaultI v "default_template" defaultTemplate
    <*> parseOrDefaultI v "index_template" indexTemplate
    <*> parseOrDefaultI v "post_template" postTemplate
    <*> parseOrDefaultP v "index" indexPattern
    <*> parseOrDefaultP v "post" postPattern
    <*> parseOrDefaultP v "comment" commentPattern
    <*> v .:? "post_prefix" .!= stripPostOnPublish defaultSiteConfig
    <*> parseOrDefaultP v "templates" templatesPattern
    <*> parseOrDefaultP v "css" cssPattern
    <*> parseOrDefaultP v "fonts" fontPattern
    where
      parseOrDefaultI v key def = toHKI (v .:? key) .!= def defaultSiteConfig
      parseOrDefaultP v key def = toHKP (v .:? key) .!= def defaultSiteConfig
      toHKP = fmap $ fmap unwrapP
      toHKI = fmap $ fmap unwrapI

-- | Default site configuration.
--
-- If the configuration option is missing, use the default from here.
defaultSiteConfig :: SiteConfig
defaultSiteConfig = SiteConfig
  { defaultTemplate = "templates/default.html"
  , indexTemplate = "templates/index.html"
  , postTemplate = "templates/post.html"
  , indexPattern = HK.fromGlob "index.md"
  , postPattern = HK.fromRegex "posts/.*/index.md"
  , commentPattern = HK.fromRegex "posts/.*/comments.md"
  , stripPostOnPublish = "posts/"
  , templatesPattern = HK.fromGlob "templates/*"
  , cssPattern = HK.fromGlob "css/*"
  , fontPattern = HK.fromGlob "fonts/*"
  }

-- | Netwtype to wrap around 'Hakyll.Core.Indetifier.Pattern.Pattern' to add a
-- 'FromJSON' instance
newtype WrappedPattern = WP { unwrapP :: HK.Pattern } deriving (Show)

instance Yaml.FromJSON WrappedPattern where
  parseJSON = Yaml.withObject "Pattern" $ \v -> parseGlob v <|> parseRegex v
    where
      parseGlob v = WP . HK.fromGlob <$> v .: "glob"
      parseRegex v = WP . HK.fromRegex <$> v .: "regex"

-- | Netwtype to wrap around 'Hakyll.Core.Indetifier.Pattern.Identifier' to
-- add a 'FromJSON' instance
newtype WrappedIdentifier = WI { unwrapI :: HK.Identifier } deriving (Show)

instance Yaml.FromJSON WrappedIdentifier where
  parseJSON = Yaml.withObject "Identifier" parsePath
    where
      parsePath v = WI . HK.fromFilePath <$> v .: "path"
