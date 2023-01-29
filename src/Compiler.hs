{-# LANGUAGE OverloadedStrings #-}

{-|
Module: Compiler
Description: Custom pandoc compiler
Copyright: (c) Mihai Maruseac 2023
License: BSD-3-Clause
Maintainer: mihai.maruseac@gmail.com
Stability: experimental
Portability: portable

Custom pandoc compiler.

We create a custom pandoc compiler to enable various pandoc extensions and
features which are needed for the desired output. For example, displaying math
properly is done here.
-}

module Compiler
  ( -- * Custom compiler, replacement for 'Hakyll.pandocCompiler'
    blogCompiler
  ) where

import Data.Maybe (fromMaybe)
import Hakyll
import Text.Pandoc

-- | The custom compiler.
--
-- Configurations:
--   * Handle citations by reading bibtex and cls files from metadata
--   * Enable MathJax for writing math code
blogCompiler :: Compiler (Item String)
blogCompiler = do
  -- 1. Handle bibliography: extract items from metadata
  underlying <- getUnderlying
  bibFile <- fmap fromFilePath <$> getMetadataField underlying "bibfile"
  cslFile <- fmap fromFilePath <$> getMetadataField underlying "cslfile"
  -- 2. Determine pandoc reader: we use a different one for bibliographies
  reader <- case bibFile of
    Nothing -> return $ readPandocWith readOptions
    Just bib -> do
      bibItem <- load bib
      cslItem <- load $ fromMaybe "default.csl" cslFile
      return $ readPandocBiblio readOptions cslItem bibItem
  -- 3. Now compile everything
  cached "blogCompiler" $ fmap (writePandocWith writeOptions) $
    getResourceBody >>= reader

-- | Read options for 'blogCompiler'.
readOptions :: ReaderOptions
readOptions = defaultHakyllReaderOptions

-- | Write options for 'blogCompiler'.
writeOptions :: WriterOptions
writeOptions = defaultHakyllWriterOptions
  { writerHTMLMathMethod = MathJax ""
  }
