{-# LANGUAGE RecordWildCards #-}

{-|
Module: Main
Description: The entry point of the site generator
Copyright: (c) Mihai Maruseac 2023
License: BSD-3-Clause
Maintainer: mihai.maruseac@gmail.com
Stability: experimental
Portability: portable

The entry point of the site generator.
-}

module Main where

import Data.Maybe (fromMaybe)
import Data.Monoid (First(..))
import qualified Hakyll as HK
import qualified Hakyll.Commands as HK
import qualified Hakyll.Core.Logger as HK
import qualified Hakyll.Core.Runtime as HK
import System.Exit (exitWith)

import CLI
import Config

-- | Main function: parse CLI, config, then merge and hand over to Hakyll.
--
-- We need to build several internals of Hakyll since we don't call it from
-- its main function.
main :: IO ()
main = do
  CLI{..} <- parseCLI
  Config{..} <- parseConfig configFile
  logger <- HK.new $ if verbose then HK.Debug else HK.Message
  let config = HK.defaultConfiguration { HK.providerDirectory = contentPath }
  let rules = undefined -- TODO(mihaimaruseac): Define generator rules
  case command of
    Build dry -> HK.build (whatMode dry) config logger rules >>= exitWith
    Clean -> HK.clean config logger
    Check internal -> HK.check config logger (whatLinks internal) >>= exitWith
    Watch host' port' runServer -> do
      let config' = mergeWatchConfig config (host' <?> host) (port' <?> port)
      HK.watch config' logger
        (HK.previewHost config') (HK.previewPort config')
        runServer rules

-- | Merge value from command line with value from config, if any are present.
--
-- If no value is present, returns 'Nothing', otherwise gives priority to CLI.
(<?>)
  :: Maybe a -- ^ value from command line (if present)
  -> Maybe a -- ^ value from config (if present)
  -> Maybe a -- ^ resulting value
cli <?> cfg = getFirst . mconcat $ map First [cli, cfg]

-- | Updates the hakyll 'HK.Configuration' to take into account server
-- arguments for the @watch@ command, if they are provided in CLI/config.
mergeWatchConfig
  :: HK.Configuration
  -> Maybe String
  -> Maybe Int
  -> HK.Configuration
mergeWatchConfig cfg@HK.Configuration{..} host port = cfg
  { HK.previewHost = fromMaybe previewHost host
  , HK.previewPort = fromMaybe previewPort port
  }

-- | Builds the 'HK.Check' argument to check all/internal links.
whatLinks :: Bool -> HK.Check
whatLinks True = HK.InternalLinks
whatLinks _ = HK.All

-- | Builds the 'HK.RunMode' argument to do full run or only dry run.
whatMode :: Bool -> HK.RunMode
whatMode True = HK.RunModePrintOutOfDate
whatMode _ = HK.RunModeNormal
