{-# LANGUAGE OverloadedStrings #-}

{-|
Module: References
Description: Utilities for handling references in articles
Copyright: (c) Mihai Maruseac 2023
License: BSD-3-Clause
Maintainer: mihai.maruseac@gmail.com
Stability: experimental
Portability: portable

Utilities for handling references in articles

Because Hakyll/Pandoc/Citeproc don't properly handle citations (at least to
the level desired by this blog, they might be handled properly in general!),
we need to manually parse the references from the post's YAML front matter.

However, Hakyll considers that the post's YAML front matter is only a
collection of @key:value@ pairs where @value@ is either a "String" or a list
"[String]". Because citations have a sructure, we'd prefer instead to process
them in this structured way.

We are replicating parts of the internals of Hakyll to make this work.
-}

module References
  ( -- * Context for references in a post
    getReferenceContext
  ) where

import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isJust, fromJust)
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import Data.Yaml ((.:), (.:?))
import qualified Hakyll as HK

-- | Obtain the context for references in a post by processing the metadata.
getReferenceContext :: HK.Metadata -> HK.Compiler (HK.Context a)
getReferenceContext metadata = case citations of
  Just cs -> return $ HK.listField "references" citeContext $ compiler cs
  Nothing -> return mempty
  where
    citations = lookupCitationList metadata
    compiler = sequence . map HK.makeItem

-- | The context for representing a citation in the rendered article.
--
-- Implicitly, it operates on 'Item Citation' objects.
--
-- Note that for DOI we extract two fields into context: @hasdoi@ and @doi@.
-- It is an error to use @doi@ without first checking @hasdoi@
-- (@$if(hasdoi)$@) and it is an error to use $hasdoi$ outside of @$if()$@.
citeContext :: HK.Context Citation
citeContext = mconcat
  [ getMandatoryField "author" author
  , getMandatoryField "title" title
  , getMandatoryField "venue" venue
  , HK.boolField "hasdoi" (isJust . doi . HK.itemBody)
  , HK.field "doi" (return . fromJust . doi . HK.itemBody)
  ]
  where
    getMandatoryField name f = HK.field name (return . f . HK.itemBody)

-- | Looks up the formatted citations in the YAML metadata for a post.
lookupCitationList :: HK.Metadata -> Maybe [Citation]
lookupCitationList meta = KM.lookup "references" meta >>= toList >>= mapM toC
  where
    toList :: Yaml.Value -> Maybe [Yaml.Value]
    toList (Yaml.Array a) = Just $ V.toList a
    toList _ = Nothing
    toC :: Yaml.Value -> Maybe Citation
    toC = Yaml.parseMaybe Yaml.parseJSON

-- | The structure of a citation.
--
-- Author, title and venue are mandatory. DOI might be missing, so we mark it
-- as optional.
--
-- To get this data, use Zoterobib (https://zbib.org/).
data Citation = Citation
  { author ::  String
  , title :: String
  , venue :: String
  , doi :: Maybe String
  } deriving (Show)

instance Yaml.FromJSON Citation where
  parseJSON = Yaml.withObject "Citation" $ \v -> Citation
    <$> v .: "author"
    <*> v .: "title"
    <*> v .: "venue"
    <*> v .:? "doi"
