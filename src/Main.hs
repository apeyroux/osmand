{-# LANGUAGE OverloadedStrings #-} 

module Main where

import Data.Monoid
import Control.Monad.Writer
import OsmAnd

main :: IO ()
main = do
  (w, c) <- runWriter (osmAndContentFromXml Voice)
  mapM_ (print . osmAndContentDescription) c
  where
    mkMsg str = "-> " + str
