{-|
Module: Main
Description: The entry point of the site generator
Copyright: (c) Mihai Maruseac 2023
License: BSD-3-Clause
Maintainer: mihai.maruseac@gmail.com
Stability: experimental
Portability: portable

The entry point of the site generator.
-}

module Main where

import CLI

main :: IO ()
main = do
  opts <- parseCLI
  print opts
