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

import Hakyll
import Text.Pandoc

-- | The custom compiler
--
-- Configurations:
--   * TODO: Describe configurations
blogCompiler :: Compiler (Item String)
blogCompiler = pandocCompilerWith readOptions writeOptions

-- | Read options for 'blogCompiler'
readOptions :: ReaderOptions
readOptions = defaultHakyllReaderOptions

-- | Write options for 'blogCompiler'
writeOptions :: WriterOptions
writeOptions = defaultHakyllWriterOptions
  { writerHTMLMathMethod = MathJax ""
  }
