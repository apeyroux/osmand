{-# LANGUAGE OverloadedStrings #-} 

module Main where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (length)
import           Data.ByteString.Char8 as ByteString (unpack)
import           Data.Conduit (ConduitM
                              , await
                              , yield
                              , ($$+-)
                              , ($=+))
import           Data.Conduit.List (sinkNull)
import           Data.Monoid
import           Network.HTTP.Conduit (http
                                      , parseUrl
                                      , responseBody
                                      , responseHeaders
                                      , withManager)
import           Network.HTTP.Types (hContentLength)
import           Options.Applicative
import           OsmAnd
import           System.Console.AsciiProgress

data OptArgs = OptArgs {
  optArgsDestination :: String
  , optArgsProxy :: Maybe String
  , optArgsFilters :: Maybe [String]
  }

optArgs :: Parser OptArgs
optArgs = OptArgs
  <$> strOption (long "destination" <> short 'd' <> help "mirror destination")
  <*> (optional $ strOption $ long "proxy" <> short 'p' <> help "proxy ex: host:3128")
  <*> (optional $ many $ strOption $ long "filters" <> short 'f' <> help "filters")

updateProgress :: MonadIO m => ProgressBar -> ConduitM ByteString ByteString m ()
updateProgress pg = await >>= maybe (return ()) (\chunk -> do
    let len = ByteString.length chunk
    liftIO $ tickN pg len
    yield chunk
    updateProgress pg)

osmand :: OptArgs -> IO ()
osmand (OptArgs d p f) = do
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
      osmAndContent >>= mapM (\oaContent -> do
                                 let prefixdwl = case (osmAndContentRoot oaContent) of
                                       "region" -> "/download.php?standard=yes&file="
                                       "road_region" -> "/road-indexes/"
                                       "srtmcountry" -> "/srtm-countries/"
                                       p -> "/" ++ p ++ "/"
                                 withManager $ \manager -> displayConsoleRegions $ do
                                   -- Start the request
                                   req <- parseUrl ("http://download.osmand.net" ++ prefixdwl ++ (osmAndContentName oaContent))
                                   res <- http req manager
                                   -- Get the Content-Length and initialize the progress bar
                                   let Just cl = lookup hContentLength (responseHeaders res)
                                   pg <- liftIO $ newProgressBar def { pgTotal = read (ByteString.unpack cl)
                                                                     , pgWidth = 100
                                                                     , pgOnCompletion = Just $ "Download " ++ (osmAndContentName oaContent) ++ " done"
                                                                     }
                                   -- Consume the response updating the progress bar
                                   responseBody res $=+ updateProgress pg $$+- sinkNull
                                   -- Force the progress bar to complete
                                   liftIO $ complete pg
                             )

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
