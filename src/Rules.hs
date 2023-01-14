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

import Config (Patterns(..))

-- | The rules to generate the site.
--
-- In general, the pattern of these rules is to match on a file pattern (glob
-- or regex, as configured in "Config") and apply a set of rules (routing and
-- compilation directives).
siteRules :: Patterns -> Rules ()
siteRules Patterns{..} = do
  match cssPattern cssRules
  match fontPattern fontRules
  match templatesPattern templatesRules
  match indexPattern indexRules

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
indexRules :: Rules ()
indexRules = do
  route idRoute
  compile copyFileCompiler
