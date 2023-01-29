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

import Data.List (sortOn)
import Hakyll
import System.FilePath
import Text.Printf

import Compiler (blogCompiler)
import Config (SiteConfig(..))

import Debug.Trace

-- | The rules to generate the site.
--
-- In general, the pattern of these rules is to match on a file pattern (glob
-- or regex, as configured in "Config") and apply a set of rules (routing and
-- compilation directives).
siteRules :: SiteConfig -> Rules ()
siteRules sc@SiteConfig{..} = do
  match cssPattern cssRules
  match fontPattern fontRules
  match indexPattern $ indexRules $ indexCompiler sc
  match postPattern $ postRules stripOnPublish $ postCompiler sc
  -- These items don't have a file for their own in output
  match templatesPattern $ compile templateCompiler
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

-- | The rules to build the root @index.html@ page.
indexRules :: Compiler (Item String) -> Rules ()
indexRules compiler = do
  route $ setExtension "html"
  compile compiler

-- | The compiler for index pages.
indexCompiler :: SiteConfig -> Compiler (Item String)
indexCompiler SiteConfig{..} = blogCompiler >>=
  loadAndApplyTemplate indexTemplate defaultContext >>=
  loadAndApplyTemplate defaultTemplate defaultContext

-- | The rules to build the pages for each post.
postRules :: String -> Compiler (Item String) -> Rules ()
postRules prefix compiler = do
  route $ gsubRoute prefix (const "") `composeRoutes` setExtension "html"
  compile compiler

-- | The compiler for posts.
--
-- Includes rules for comments, etc.
postCompiler :: SiteConfig -> Compiler (Item String)
postCompiler SiteConfig{..} = do
  -- Testing
  underlying <- getUnderlying
  metadata <- getMetadata underlying
  let meta = lookupStringList "references" metadata
  traceM $ printf "%s - %s" (show metadata) (show meta)
  let refContext = case meta of
        Just l -> listField "references" (field "reference" (return .  itemBody)) (return $ map (Item underlying) l)
        Nothing -> mempty
  -- 1. Extract comments (if any) to generate the proper context
  commentContext <- processComments localCommentPattern
  -- 2. Compile the post, insert the proper snapshots and contexts
  let postContext = mconcat
        [ formattedPublishedDateContext
        , modificationTimeField "changed" machineTimeFormat
        , modificationTimeField "fchanged" readableTimeFormat
        , refContext
        , defaultContext
        ]
  blogCompiler >>=
    return . fmap (demoteHeadersBy 2) >>=
    loadAndApplyTemplate postTemplate postContext >>=
    -- TODO: save snapshot for RSS
    loadAndApplyTemplate commentTemplate commentContext >>=
    loadAndApplyTemplate defaultTemplate defaultContext

-- | The compiler for comment snippets.
commentCompiler :: Compiler (Item String)
commentCompiler = blogCompiler

-- | Process te comments for a post.
processComments :: String -> Compiler (Context String)
processComments commentPattern = do
  current <- dropFileName . toFilePath <$> getUnderlying
  comments <- loadAll (fromGlob $ current </> commentPattern) >>= sortById
  return $ mconcat
    [ constField "numComments" $ printf "%d" $ length comments
    , listField "comments" localCommentContext (return comments)
    , defaultContext
    ]
  where
    localCommentContext = formattedPublishedDateContext <> defaultContext

-- | Sort a set of 'Hakyll.Item's by their @id@ (assumes each contains an @id@
-- field in their corresponding @Hakyll.Metadata@.
sortById :: [Item a] -> Compiler [Item a]
sortById items = mapM getId items >>= return . map snd . sortOn fst
  where
    getId :: Item a -> Compiler (Int, Item a)
    getId i = do
      let iid = itemIdentifier i
      parsed <- fmap reads <$> getMetadataField iid "id"
      case parsed of
        Just [(i', "")] -> return (i', i)
        _ ->  error $ printf "Failed to get id from %s" $ show iid

-- | Formatted date of publication
formattedPublishedDateContext :: Context a
formattedPublishedDateContext = dateField "fpublished" readableTimeFormat

-- | Readable time format
readableTimeFormat :: String
readableTimeFormat = "%B %e, %Y"

-- | Machine time format
machineTimeFormat :: String
machineTimeFormat = "%F"
