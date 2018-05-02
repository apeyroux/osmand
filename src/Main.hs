{-# LANGUAGE OverloadedStrings #-} 

module Main where

import Data.Monoid
import OsmAnd

main :: IO ()
main = do
  c <- osmAndContentFromXml Voice
  mapM_ (print . osmAndContentDescription) c
  where
    mkMsg str = "-> " + str
