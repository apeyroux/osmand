{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import           Control.Monad (filterM)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Monad.Writer
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (length)
import           Data.ByteString.Char8 as ByteString (unpack, pack)
import           Data.Conduit (ConduitM
                              , Sink
                              , await
                              , yield
                              , runConduit
                              , (.|)
                              , ($$+-)
                              , ($=+))
import           Data.Conduit.Combinators (sinkFile)
import           Data.Monoid
import           Network.HTTP.Conduit
import           Network.HTTP.Types (hContentLength)
import           Options.Applicative
import           OsmAnd
import           System.Console.AsciiProgress
import           System.Directory

data OptArgs = OptArgs {
  optArgsDestination :: String
  , optArgsProxyHost :: Maybe String
  , optArgsProxyPort :: Maybe Int
  , optArgsFilters :: Maybe [String]
  }

optArgs :: Parser OptArgs
optArgs = OptArgs
  <$> strOption (long "destination" <> short 'd' <> help "mirror destination")
  <*> (optional $ strOption $ long "proxy-host" <> help "proxy host")
  <*> (optional $ option auto $ long "proxy-port" <> help "proxy port")
  <*> (optional $ many $ strOption $ long "filter" <> short 'f' <> help "filter")

updateProgress :: MonadIO m => ProgressBar -> ConduitM ByteString ByteString m ()
updateProgress pg = await >>= maybe (return ()) (\chunk -> do
    let len = ByteString.length chunk
    liftIO $ tickN pg len
    yield chunk
    updateProgress pg)

osmand :: OptArgs -> IO ()
osmand (OptArgs d ph pp f) = do

  fileExist <- doesFileExist d
  case fileExist of
    True -> return ()
    False -> createDirectoryIfMissing True d

  let proxy = case (ph, pp) of
                (Just h, Just p) -> (ph <> Just ":" <> (show <$> pp))
                _ -> Nothing
  osmAndIndexes <- runReader parseOsmAndIndexes proxy
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
      osmAndContentToXmlFIle (d ++ "/indexes.xml") osmAndXmlTree
      osmAndContent >>= (\lo -> do
                            liftIO $ filterM (\o -> do
                                                 fileExist <- doesFileExist (d ++ "/" ++ (osmAndContentName o))
                                                 case fileExist of
                                                   True -> do
                                                     fileSize <- getFileSize (d ++ "/" ++ (osmAndContentName o))
                                                     return $ not ((osmAndContentContainerSize o) == fileSize)
                                                   False -> return True
                                             ) lo
                        ) >>= mapM (\oaContent -> do
                                       let prefixdwl = case (osmAndContentRoot oaContent) of
                                             "region" -> "/download.php?standard=yes&file="
                                             "road_region" -> "/road-indexes/"
                                             "srtmcountry" -> "/srtm-countries/"
                                             p -> "/" ++ p ++ "/"
                                       displayConsoleRegions $ do
                                         req <- parseRequest ("http://download.osmand.net" ++ prefixdwl ++ (osmAndContentName oaContent))
                                         case (ph, pp) of
                                           (Just h, Just p) -> return $ addProxy (ByteString.pack h) p req
                                           _ -> return req
                                         manager <- newManager tlsManagerSettings
                                         runResourceT $ do
                                           res <- http req manager
                                           let Just cl = lookup hContentLength (responseHeaders res)
                                           pg <- liftIO $ newProgressBar def { pgTotal = read (ByteString.unpack cl)
                                                                             , pgWidth = 100
                                                                             , pgOnCompletion = Just $ "Download " ++ (osmAndContentName oaContent) ++ " done"
                                                                             }
                                           runConduit $ responseBody res .| updateProgress pg .| sinkFile (d ++ "/" ++ (osmAndContentName oaContent))
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
      (fullDesc
        <> progDesc ("OsmAnd mirror " ++ version)
        <> header ("osmand - " ++ version))
