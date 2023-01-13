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

import qualified Hakyll as HK
import qualified Hakyll.Core.Logger as HK

import CLI
import Config

-- | Main function: parse CLI, config, then merge and hand over to Hakyll.
main :: IO ()
main = do
  opts@CLI{..} <- parseCLI
  print opts
  cfs <- parseConfig configFile
  print cfs
  logger <- HK.new $ if verbose then HK.Debug else HK.Message
  HK.debug logger "All is good"
