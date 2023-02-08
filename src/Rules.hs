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
import Text.Pandoc.Highlighting
import Text.Printf

import Compiler (blogCompiler, highlightStyle)
import Config (FeedConfig(..), SiteConfig(..))
import References (getReferenceContext)

-- | The rules to generate the site.
--
-- In general, the pattern of these rules is to match on a file pattern (glob
-- or regex, as configured in "Config") and apply a set of rules (routing and
-- compilation directives).
siteRules :: String -> FeedConfig -> SiteConfig -> Rules ()
siteRules siteTitle fc@FeedConfig{..} sc@SiteConfig{..} = do
  -- Style rules
  match cssPattern cssRules
  match fontPattern fontRules
  create [fromFilePath cssSyntaxPath] syntaxRules
  -- Actual content rules
  match indexPattern $ indexRules $ indexCompiler metaContext sc
  match postPattern $ postRules stripOnPublish $ postCompiler metaContext sc
  create [fromFilePath rssFeedPath] $ feedRules siteTitle fc sc
  create ["404.html"] $ pageNotFound $ pageNotFoundCompiler metaContext sc
  -- These items don't have a file for their own in output
  match templatesPattern $ compile templateCompiler
  match commentPattern $ compile commentCompiler
  match updatePattern $ compile updateCompiler
  -- Images and other static files
  match imagePattern staticRules
  where
    imagePattern = foldl1 (.||.) $ map fromGlob
      [ "**.ico"
      , "**.png"
      , "**.jpg"
      , "**.jpeg"
      ]
    metaContext = mconcat
      [ constField "author" cfgFeedAuthorName
      , constField "site" siteTitle
      , constField "root" cfgFeedRoot
      , descField cfgFeedDescription
      , fieldStrip
      , defaultContext
      ]

-- | Rules for static files (images, etc.)
staticRules :: Rules ()
staticRules = do
  route idRoute
  compile copyFileCompiler

-- | The rules to generate CSS files.
--
-- This just minimizes and copies the template CSS files.
cssRules :: Rules ()
cssRules = do
  route idRoute
  compile compressCssCompiler

-- | The rules to generate the code highlighting style.
syntaxRules :: Rules ()
syntaxRules = do
  route idRoute
  compile . makeItem $ compressCss . styleToCss $ highlightStyle

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
indexCompiler :: Context String -> SiteConfig -> Compiler (Item String)
indexCompiler metaContext SiteConfig{..} = do
  -- Load all posts to display in the index
  posts <- loadAllSnapshots postPattern postSnap >>= (reverse <$>) . sortById
  let indexContext = mconcat
        [ constField "numArticles" $ printf "%d" $ length posts
        , listField "articles" (fieldStrip <> defaultContext) (return posts)
        , defaultContext
        ]
  -- Compile the index, with the corresponding contexts
  blogCompiler >>=
    loadAndApplyTemplate indexTemplate indexContext >>=
    loadAndApplyTemplate defaultTemplate (rootContext <> metaContext)

-- | The rules to build the pages for each post.
postRules :: String -> Compiler (Item String) -> Rules ()
postRules prefix compiler = do
  route $ gsubRoute prefix (const "") `composeRoutes` setExtension "html"
  compile compiler

-- | The compiler for posts.
--
-- Includes rules for comments, etc.
postCompiler :: Context String -> SiteConfig -> Compiler (Item String)
postCompiler metaContext SiteConfig{..} = do
  -- Extract ancillary data (if any) to generate the proper contexts
  commentContext <- processComments localCommentPattern
  updateContext <- processUpdates localUpdatePattern
  referencesContext <- processReferences
  -- Compile the post, insert the proper snapshots and contexts
  blogCompiler >>=
    return . fmap (demoteHeadersBy 2) >>=
    saveSnapshot postSnap >>=
    loadAndApplyTemplate postTemplate (referencesContext <> postContext) >>=
    loadAndApplyTemplate updateTemplate updateContext >>=
    loadAndApplyTemplate commentTemplate commentContext >>=
    loadAndApplyTemplate defaultTemplate (changed <> metaContext)
  where
    changed = modificationTimeField "changed" feedTimeFormat
    postContext = mconcat
      [ formattedPublishedDateContext
      , modificationTimeField "changed" machineTimeFormat
      , modificationTimeField "fchanged" readableTimeFormat
      , fieldStrip
      , defaultContext
      ]

-- | The compiler for comment snippets.
commentCompiler :: Compiler (Item String)
commentCompiler = blogCompiler

-- | Processes the comments for a post.
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

-- | The compiler for update snippets.
updateCompiler :: Compiler (Item String)
updateCompiler = blogCompiler

-- | Processes the updates for a post.
processUpdates :: String -> Compiler (Context String)
processUpdates updatePattern = do
  current <- dropFileName . toFilePath <$> getUnderlying
  updates <- loadAll (fromGlob $ current </> updatePattern) >>= sortById
  case updates of
    [] -> return defaultContext
    us -> return $ mconcat
      [ boolField "hasUpdates" $ const True
      , listField "updates" localUpdatesContext $ return us
      , defaultContext
      ]
  where
    localUpdatesContext = formattedPublishedDateContext <> defaultContext

-- | Create the actual RSS feed
feedRules :: String -> FeedConfig -> SiteConfig -> Rules ()
feedRules siteTitle fc sc = do
  route idRoute
  compile $ feedCompiler siteTitle fc sc

-- | Compile the feed
feedCompiler :: String -> FeedConfig -> SiteConfig -> Compiler (Item String)
feedCompiler siteTitle FeedConfig{..} SiteConfig{..} = do
  -- Load all posts to display in the index
  posts <- loadAllSnapshots postPattern postSnap >>= (tk . reverse <$>) . sortById
  -- Load the templates needed for the feed, then compile the feed
  feedTpl <- loadBody feedTemplate
  itemTpl <- loadBody feedItemTemplate
  renderRssWithTemplates feedTpl itemTpl feedConfiguration feedContext posts
  where
    tk = maybe id take cfgFeedItems
    feedConfiguration = FeedConfiguration
      { feedTitle = siteTitle
      , feedDescription = cfgFeedDescription
      , feedAuthorName = cfgFeedAuthorName
      , feedAuthorEmail = cfgFeedAuthorEmail
      , feedRoot = cfgFeedRoot
      }
    feedContext = mconcat
      [ teaserField "teaser" postSnap
      , bodyField "description"
      , dateField "published" feedTimeFormat
      , fieldStrip
      , defaultContext
      ]

-- | Add a 404 page
pageNotFound :: Compiler (Item String) -> Rules ()
pageNotFound compiler = do
  route idRoute
  compile compiler

-- | Compiler for 404 page
pageNotFoundCompiler :: Context String -> SiteConfig -> Compiler (Item String)
pageNotFoundCompiler metaContext SiteConfig{..} = makeItem "404" >>=
  loadAndApplyTemplate notFoundTemplate defaultContext >>=
  loadAndApplyTemplate defaultTemplate (rootContext <> metaContext)

-- | Processes the references for a post.
processReferences :: Compiler (Context a)
processReferences = getUnderlying >>= getMetadata >>= getReferenceContext

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

-- | Context to use for global root pages
rootContext :: Context a
rootContext = boolField "global" (const True)

-- | Formatted date of publication
formattedPublishedDateContext :: Context a
formattedPublishedDateContext = dateField "fpublished" readableTimeFormat

-- | Readable time format
readableTimeFormat :: String
readableTimeFormat = "%B %e, %Y"

-- | Machine time format
machineTimeFormat :: String
machineTimeFormat = "%F"

-- | Time format for RSS and SEO
feedTimeFormat :: String
feedTimeFormat = "%a, %d %b %Y %H:%M:%S %z"

-- | Post snapshot for index and RSS pages
postSnap :: Snapshot
postSnap = "content"

-- | Function field to strip parts of the context value.
--
-- Removals are done in order.
fieldStrip :: Context a
fieldStrip = functionField "remove" $ \args _ -> case args of
  f:toStrip -> return $ strip f toStrip
  _ -> fail "remove needs at least the field argument to start stripping"
  where
    strip s [] = s
    strip s (p:ps) = strip (replaceAll p (const "") s) ps

-- | Field to generate the description of an item
descField :: String -> Context String
descField defaultDescription = field f $ \item -> do
  metaDescr <- getMetadataField (itemIdentifier item) f
  case metaDescr of
    Just df -> return df
    Nothing -> return defaultDescription
  where
    f = "description"
