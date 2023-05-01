{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module: Diagrams
Description: Inline post diagrams
Copyright: (c) Mihai Maruseac 2023
License: BSD-3-Clause
Maintainer: mihai.maruseac@gmail.com
Stability: experimental
Portability: portable

Diagrams support to insert inline in post input.

We want to allow posts to contain images generated from the same editor. This
support is given by @diagrams*@ libraries.

See https://mihai.page/ai-diagrams/.
-}

module Diagrams
  ( -- * Compile and ender a diagram from a post
    compileDiagram
    -- * Result
  , CDResult(..)
  ) where

import Codec.Picture
import Data.Text (Text, pack, unpack)
import Diagrams.Backend.Rasterific
import Diagrams.Builder
import Diagrams.Builder.Opts
import Diagrams.Prelude
import System.FilePath
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | Results of compiling a diagram.
--
-- We wrap the @diagrams-builder@ error types into different type so that the
-- post compiler does not need to know about the @diagrams*@ details.
data CDResult
  = Success Text
  | Failure Text
  deriving (Eq, Show)

-- | Compile a diagram to insert into a Pandoc document.
--
-- Errors are printed to standard error and in the article.
--
-- On success, return image contents wrapped in Success.
compileDiagram :: [(Text, Text)] -> Text -> IO CDResult
compileDiagram as src = buildDiagram (diagramDef as src) >>= \case
  -- failures
  ParseErr  err -> logAndFail $ printf "ParseErr: %s" err
  InterpErr err -> logAndFail $ printf "InterpErr: %s" $ ppInterpError err
  -- successes
  Skipped hash    -> markSuccess hash
  OK hash result  -> do
    savePngImage (buildFile hash) (ImageRGBA8 result)
    markSuccess hash

-- | Build the diagram definition from a given source code and attributes.
--
-- For now, we only generate PNGs using Rasterific.
diagramDef :: [(Text, Text)] -> Text -> BuildOpts Rasterific V2 Double
diagramDef as src = mkBuildOpts Rasterific zero defaultOptions
  & imports .~ [ "Diagrams.Backend.Rasterific" ]
  & snippets .~ [unpack src]
  & diaExpr .~ "diagram"
  & postProcess .~ (pad 1.1 . centerXY)
  & decideRegen .~ hashedRegenerate (const id) imgDir
  where
    defaultOptions = RasterificOptions $ mkSizeSpec2D h w
    h = parseAttribute "height" as
    w = parseAttribute "width"  as

-- | Helper to create image file and return success
--
-- Need to drop root prefix in the HTML to not include the directory where the
-- generated blog goes to.
markSuccess :: Hash -> IO CDResult
markSuccess = return . Success . pack . drop 5 . buildFile

-- | Helper to log errors and put them in blog post too.
logAndFail :: String -> IO CDResult
logAndFail err = do
  logErrorOutput err
  return $ Failure $ pack err

-- | Logs an error output to the build/watch output stream.
--
-- Currently, this is stdout.
logErrorOutput :: String -> IO ()
logErrorOutput = putStrLn

-- | Builds the output filepath for an image given its hash.
buildFile :: Hash -> FilePath
buildFile hash =  imgDir </> hashToHexStr hash <.> "png"

-- | Directory where to store all the images generated from the diagrams.
imgDir :: FilePath
imgDir = "_site/imgs"

-- | Helper to extract the numerical value of an attribute.
parseAttribute :: Read a => Text -> [(Text, Text)] -> Maybe a
parseAttribute a as = lookup a as >>= (readMaybe . unpack)
