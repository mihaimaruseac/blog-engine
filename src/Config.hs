{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
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
  ) where

import Control.Monad ((>=>))
import qualified Data.Yaml as Yaml
import GHC.Generics
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
  } deriving (Eq, Show, Generic)

instance Yaml.FromJSON Config
