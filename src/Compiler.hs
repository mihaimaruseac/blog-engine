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
import Text.Pandoc.Walk

-- | The default highlight style.
--
-- A global constant, used to color code.
highlightStyle :: Style
highlightStyle = pygments

-- | The custom compiler.
--
-- Configurations:
--   * Enable MathJax for writing math code
--   * Add links to each section, on hover
blogCompiler :: Compiler (Item String)
blogCompiler = pandocCompilerWithTransform readOptions writeOptions xforms
  where
    xforms = addSectionLink

-- | Read options for 'blogCompiler'.
readOptions :: ReaderOptions
readOptions = defaultHakyllReaderOptions

-- | Write options for 'blogCompiler'.
writeOptions :: WriterOptions
writeOptions = defaultHakyllWriterOptions
  { writerHTMLMathMethod = MathJax ""
  , writerHighlightStyle = Just highlightStyle
  }

-- | Add links to sections, on hover
-- Inspired by https://frasertweedale.github.io/blog-fp/posts/2020-12-10-hakyll-section-links.html
addSectionLink :: Pandoc -> Pandoc
addSectionLink = walk transformSectionHeaders

-- | Pandoc walker over section headers to add section links.
transformSectionHeaders :: Block -> Block
transformSectionHeaders (Header n attr@(aid, _, _) next) =
  Header n attr $ next <> [Space, Link nullAttr [Str "🔗"] ("#" <> aid, "")]
transformSectionHeaders x = x
