{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module OsmAnd (
  OsmAndType (..)
  , OsmAndContent (..)
  , OsmAndContext (..)
  , parseOsmAndIndexes
  , osmAndContentFromXml
  , osmAndContentToXmlFIle
  , osmAndContentToXml
  , execOsmAnd
  , version
  ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Char
import Data.List
import Data.Version
import Text.XML.HXT.Core
import Text.XML.HXT.HTTP

type Filter = String

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

data OsmAndContext = OsmAndContext {
  osmAndContextProxy :: Maybe String
  , osmAndContextIndexes :: [XmlTree]
  , osmAndContextFilters :: Maybe [Filter] -- todo: Maybe for all or Just x
  }

-- instance Default OsmAndContext where
--   def = OsmAndContext Nothing Nothing Nothing

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

version :: Version
version = makeVersion [1, 3, 0]

-- get_indexes?xml
basews :: String
basews = "http://download.osmand.net"

parseOsmAndIndexes :: Reader (Maybe String) (IO [XmlTree])
parseOsmAndIndexes = do
  proxy <- ask
  let opts = case proxy of
        Just p -> [withHTTP [], withProxy p]
        Nothing -> [withHTTP []]
  return $ runX $ readDocument opts (basews <> "/get_indexes?xml")

-- osmAndDescriptionContain :: String -> [XmlTree] -> IO [XmlTree]
-- osmAndDescriptionContain q xtree = runX $ constL xtree >>> hasAttrValue "description" (\d -> (isInfixOf ((toUpper <$> q)::String) ((toUpper <$> d)::String)))

getOsmAndType :: IOSLA (XIOState ()) XmlTree OsmAndContent
getOsmAndType = do
  proc l -> do
    rootXml       <- getName                      -< l
    name          <- getAttrValue "name"          -< l
    otype         <- getAttrValue "type"          -< l
    containerSize <- getAttrValue "containerSize" -< l
    contentSize   <- getAttrValue "contentSize"   -< l
    timestamp     <- getAttrValue "timestamp"     -< l
    date          <- getAttrValue "date"          -< l
    size          <- getAttrValue "size"          -< l
    targetsize    <- getAttrValue "targetsize"    -< l
    description   <- getAttrValue "description"   -< l
    returnA -< OsmAndContent { osmAndContentRoot = rootXml
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
osmAndContentFromXml :: OsmAndType -> ReaderT OsmAndContext (WriterT (IO [OsmAndContent], [XmlTree]) IO) (IO [OsmAndContent])
osmAndContentFromXml o = do
  ctx <- ask
  d <- return $ runX $ (xmlT ctx) >>> getOsmAndType
  liftIO (runX (xmlT ctx)) >>= (\x -> lift $ tell (d, x))
  return $ runX $ (xmlT ctx) >>> getOsmAndType
  where
    ctxIndexes ctx = osmAndContextIndexes ctx
    baseXmlT x = (constL x) //> hasAttrValue "type" ((==) (show o))
    toUpperString s = toUpper <$> s::String
    xmlT ctx = case osmAndContextFilters ctx of
                 Just [] -> baseXmlT (ctxIndexes ctx)
                 Just filters -> baseXmlT (ctxIndexes ctx)
                   >>> hasAttrValue "description" (\d -> elem True (map (\s -> isInfixOf (toUpperString s) (toUpperString d)) filters))
                 Nothing -> baseXmlT (ctxIndexes ctx)

osmAndBaseXmlDoc :: ArrowXml a => [XmlTree] -> a n XmlTree
osmAndBaseXmlDoc xtree = root [] [mkelem "osmand_regions" [sattr "mapversion" "1"] [constL xtree]]

osmAndContentToXml :: [XmlTree] -> IO String
osmAndContentToXml xtree = (runX $ osmAndBaseXmlDoc xtree >>> writeDocumentToString []) >>= return . concat

osmAndContentToXmlFIle :: String -> [XmlTree] -> IO ()
osmAndContentToXmlFIle f xtree = (runX $ osmAndBaseXmlDoc xtree >>> writeDocument [] f) >> return ()

execOsmAnd :: Monad m => r -> ReaderT r (WriterT w m) a -> m w
execOsmAnd ctx f = execWriterT $ runReaderT f ctx
