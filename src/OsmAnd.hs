{-# LANGUAGE OverloadedStrings #-} 

module OsmAnd (
  OsmAndType (Map
             , Voice
             , Fonts
             , Depth
             , WikiMap
             , WikiVoyage
             , RoadMap
             , SrtmMap
             , Hillshade)
  , OsmAndContent
  , osmAndXml
  ) where

import Data.Text
import Data.Monoid
import Text.XML.HXT.Core
import Text.XML.HXT.HTTP

data OsmAndContent = OsmAndContent {
  osmAndContentType :: OsmAndType
  , osmAndContentContainerSize :: Integer
  , osmAndContentContentSize :: Integer
  , osmAndContentTimeStamp :: Integer
  , osmAndContentDate :: Text
  , osmAndContentSize :: Float
  , osmAndContentTargetSize :: Float
  , osmAndContentName :: Text
  , osmAndContentDescription :: Text
  } deriving Show

data OsmAndType = Map -- map
                | Voice -- voice
                | Fonts -- fonts
                | Depth -- depth
                | WikiMap -- wikimap
                | WikiVoyage -- wikivoyage
                | RoadMap -- road_map
                | SrtmMap -- srtm_map
                | Hillshade -- hillshade

instance Show OsmAndType where
  show Map = "map"
  show Voice = "voice"
  show Fonts = "fonts"
  show Depth = "depth"
  show WikiMap = "wikimap"
  show WikiVoyage = "wikivoyage"
  show RoadMap = "road_map"
  show SrtmMap = "srtm_map"
  show Hillshade = "hillshade"

-- get_indexes?xml
basews :: String
basews = "http://download.osmand.net"

-- option: withProxy "www-cache:3128"
-- https://hackage.haskell.org/package/hxt-9.3.1.16/docs/Text-XML-HXT-Arrow-ReadDocument.html
osmAndXml :: OsmAndType -> IO [XmlTree]
osmAndXml o = runX $ readDocument [withHTTP []] (basews <> "/get_indexes?xml") //> hasAttrValue "type" (\i -> i == show o) 
