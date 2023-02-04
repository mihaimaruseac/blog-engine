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
    -- * Highlight style
  , highlightStyle
  ) where

import Hakyll
import Text.Pandoc
import Text.Pandoc.Highlighting

-- | The default highlight style.
--
-- A global constant, used to color code.
highlightStyle :: Style
highlightStyle = pygments

-- | The custom compiler.
--
-- Configurations:
--   * Enable MathJax for writing math code
blogCompiler :: Compiler (Item String)
blogCompiler = pandocCompilerWith readOptions writeOptions

-- | Read options for 'blogCompiler'.
readOptions :: ReaderOptions
readOptions = defaultHakyllReaderOptions

-- | Write options for 'blogCompiler'.
writeOptions :: WriterOptions
writeOptions = defaultHakyllWriterOptions
  { writerHTMLMathMethod = MathJax ""
  , writerHighlightStyle = Just highlightStyle
  }
