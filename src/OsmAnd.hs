{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

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
  , osmAndContentFromXml
  ) where

import Data.Monoid
import Text.XML.HXT.Core
import Text.XML.HXT.HTTP

data OsmAndContent = OsmAndContent {
  osmAndContentRoot :: String
  , osmAndContentType :: OsmAndType
  , osmAndContentContainerSize :: Integer
  , osmAndContentContentSize :: Integer
  , osmAndContentTimeStamp :: Integer
  , osmAndContentDate :: String
  , osmAndContentSize :: Float
  , osmAndContentTargetSize :: Float
  , osmAndContentName :: String
  , osmAndContentDescription :: String
  } deriving (Show, Read)

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

instance Read OsmAndType where
  -- http://book.realworldhaskell.org/read/using-typeclasses.html
  readsPrec _ value = tryParse [("map", Map)
                               , ("voice", Voice)
                               , ("fonts", Fonts)
                               , ("depth", Depth)
                               , ("wikimap", WikiMap)
                               , ("wikivoyage", WikiVoyage)
                               , ("road_map", RoadMap)
                               , ("strm_map", SrtmMap)
                               , ("hillshade", Hillshade)]
    where tryParse [] = []
          tryParse ((attempt, result):xs) =
            if (take (length attempt) value) == attempt
            then [(result, drop (length attempt) value)]
            else tryParse xs

-- get_indexes?xml
basews :: String
basews = "http://download.osmand.net"

parseOsmAndXml = readDocument [withHTTP []] (basews <> "/get_indexes?xml")

getOsmAndType = proc l -> do
  root <- getName -< l
  name <- getAttrValue "name" -< l
  otype <- getAttrValue "type" -< l
  containerSize <- getAttrValue "containerSize" -< l
  contentSize <- getAttrValue "contentSize" -< l
  timestamp <- getAttrValue "timestamp" -< l
  date <- getAttrValue "date" -< l
  size <- getAttrValue "size" -< l
  targetsize <- getAttrValue "targetsize" -< l
  description <- getAttrValue "description" -< l
  returnA -< OsmAndContent { osmAndContentRoot = root
                           , osmAndContentType = read otype::OsmAndType
                           , osmAndContentContainerSize = (read containerSize::Integer)
                           , osmAndContentContentSize = (read contentSize::Integer)
                           , osmAndContentTimeStamp = (read timestamp::Integer)
                           , osmAndContentDate = date
                           , osmAndContentSize = (read size::Float)
                           , osmAndContentTargetSize = (read targetsize::Float)
                           , osmAndContentName = name
                           , osmAndContentDescription = description
                           }

-- option: withProxy "www-cache:3128"
-- https://hackage.haskell.org/package/hxt-9.3.1.16/docs/Text-XML-HXT-Arrow-ReadDocument.html
osmAndContentFromXml :: OsmAndType -> IO [OsmAndContent]
osmAndContentFromXml o = runX $ parseOsmAndXml //> hasAttrValue "type" ((==) (show o)) >>> getOsmAndType
