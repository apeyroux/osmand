{-# LANGUAGE OverloadedStrings #-} 

module Main where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Monoid
import Options.Applicative
import OsmAnd

data OptArgs = OptArgs {
  optArgsDestination :: String
  , optArgsFilters :: Maybe [String]
  }

optArgs :: Parser OptArgs
optArgs = OptArgs
  <$> strOption (long "destination" <> short 'd' <> help "mirror destination")
  <*> (optional $ many $ strOption $ long "filters" <> short 'f')

osmand :: OptArgs -> IO ()
osmand (OptArgs d f) = do
  osmAndIndexes <- parseOsmAndIndexes
  w <- execOsmAnd (ctx osmAndIndexes f) $ do
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
      osmAndContentToXmlFIle (d ++ "/osmand.xml") osmAndXmlTree
      osmAndContent >>= mapM (print . osmAndContentName)

  return ()
  where
    ctx idx filters = OsmAndContext Nothing idx filters

main :: IO ()
main = do
  osmand =<< execParser opts  
  where
    opts = info (optArgs <**> helper)
      ( fullDesc
        <> progDesc "OsmAnd mirror"
        <> header "osmand - mirror OsmAnd" )
