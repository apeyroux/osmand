{-# LANGUAGE OverloadedStrings #-} 

module Main where

import Data.Monoid
import Control.Monad.Writer
import Control.Monad.Reader
import OsmAnd

main :: IO ()
main = do

  osmAndIndexes <- parseOsmAndIndexes
  w <- execOsmAnd (ctx osmAndIndexes) $ do
    osmAndContentFromXml Voice -- voice
      >> osmAndContentFromXml Map -- map 
      >> osmAndContentFromXml (read "wikimap"::OsmAndType) -- wikimap
      >> osmAndContentFromXml Fonts -- fonts
      >> osmAndContentFromXml Depth -- depth
      >> osmAndContentFromXml WikiVoyage -- wikivoyage
      >> osmAndContentFromXml RoadMap -- road_map
      >> osmAndContentFromXml SrtmMap -- srtm_map
      >> osmAndContentFromXml Hillshade -- hillshade

  case w of
    (osmAndContent, osmAndXmlTree) -> do
      osmAndContentToXmlFIle "/tmp/osmand.xml" osmAndXmlTree
      osmAndContent >>= mapM (print . osmAndContentName)

  return ()
  
  where
    filters = ["french"
              , "france"
              , "World"]
    ctx idx = OsmAndContext Nothing idx (Just filters)
