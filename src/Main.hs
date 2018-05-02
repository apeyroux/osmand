{-# LANGUAGE OverloadedStrings #-} 

module Main where

import Data.Monoid
import Control.Monad.Writer
import OsmAnd

main :: IO ()
main = do
  (c, w) <- runWriterT $ do
    osmAndContentFromXml Voice ["fran", "fren"]
    osmAndContentFromXml Map ["fran", "fren"]
    osmAndContentFromXml (read "wikimap"::OsmAndType) ["fran", "fren"]
  osmAndContentToXmlFIle "/tmp/osmand.xml" w
