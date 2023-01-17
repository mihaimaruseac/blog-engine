{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module: Rules
Description: Rules for generating the website
Copyright: (c) Mihai Maruseac 2023
License: BSD-3-Clause
Maintainer: mihai.maruseac@gmail.com
Stability: experimental
Portability: portable

Rules for generating the website.

This module contains all rules that are being used to generate various parts
of the website: how to create the index page(s), how to compile the templates
and generate the posts, how to translate the resources (CSS, fonts, images,
etc.).
-}

module Rules
  ( -- * Rules generator
    siteRules
  ) where

import Hakyll
import System.FilePath

import Config (SiteConfig(..))

-- | The rules to generate the site.
--
-- In general, the pattern of these rules is to match on a file pattern (glob
-- or regex, as configured in "Config") and apply a set of rules (routing and
-- compilation directives).
siteRules :: SiteConfig -> Rules ()
siteRules SiteConfig{..} = do
  match cssPattern cssRules
  match fontPattern fontRules
  match templatesPattern templatesRules
  match indexPattern $ indexRules $
    indexCompiler defaultTemplate indexTemplate
  match postPattern $ postRules stripPostOnPublish $
    postCompiler defaultTemplate postTemplate
  match commentPattern $ compile commentCompiler

-- | The rules to generate CSS files.
--
-- This just minimizes and copies the template CSS files.
cssRules :: Rules ()
cssRules = do
  route idRoute
  compile compressCssCompiler

-- | The rules to generate the font CSS style.
--
-- Just copy them to destination.
fontRules :: Rules ()
fontRules = do
  route idRoute
  compile copyFileCompiler

-- | The rules to compile the templates.
templatesRules :: Rules ()
templatesRules = compile templateCompiler

-- | The rules to build the root @index.html@ page.
indexRules :: Compiler (Item String) -> Rules ()
indexRules compiler = do
  route $ setExtension "html"
  compile compiler

-- | The compiler for index pages.
indexCompiler :: Identifier -> Identifier -> Compiler (Item String)
indexCompiler defaultTemplate indexTemplate = pandocCompiler >>=
  loadAndApplyTemplate indexTemplate defaultContext >>=
  loadAndApplyTemplate defaultTemplate defaultContext

-- | The rules to build the pages for each post.
postRules :: String -> Compiler (Item String) -> Rules ()
postRules prefix compiler = do
  route $ gsubRoute prefix (const "") `composeRoutes` setExtension "html"
  compile compiler

-- | The compiler for posts.
postCompiler :: Identifier -> Identifier -> Compiler (Item String)
postCompiler defaultTemplate postTemplate = pandocCompiler >>=
  return . fmap (demoteHeadersBy 3) >>=
  loadAndApplyTemplate postTemplate postContext >>=
  loadAndApplyTemplate defaultTemplate defaultContext
  where
    postContext = mconcat
      [ dateField "fpublished" "%B %e, %Y" -- readable published date
      , modificationTimeField "changed" "%F" -- changed date
      , modificationTimeField "fchanged" "%B %e, %Y" -- readable changed date
      , field "comments" postCommentsCompiler
      , defaultContext
      ]

-- | The compiler for comment snippets.
commentCompiler :: Compiler (Item String)
commentCompiler = pandocCompiler >>= saveSnapshot "comments"

-- | The compiler to generate comments for the current 'Item'
postCommentsCompiler :: Show a => Item a -> Compiler String
postCommentsCompiler item = loadSnapshotBody cid "comments"
  where
    itemLocation = toFilePath . itemIdentifier $ item
    commentsFile = replaceBaseName itemLocation "comments" -- TODO: not ok
    cid = fromFilePath commentsFile
