{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad (filterM)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (length)
import           Data.ByteString.Char8 as ByteString (unpack, pack)
import           Data.Conduit (ConduitM
                              , (.|)
                              , await
                              , yield
                              , runConduit)
import           Data.Conduit.Combinators (sinkFile)
import           Data.Version
import           Development.GitRev
import           Network.HTTP.Conduit
import           Network.HTTP.Types (hContentLength)
import           Options.Applicative
import           OsmAnd
import           System.Console.AsciiProgress
import           System.Directory
import           System.FilePath.Posix

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
  targetDirExist <- doesFileExist d
  case targetDirExist of
    True -> return ()
    False -> createDirectoryIfMissing True d

  let proxyCfg = case (ph, pp) of
                   (Just _, Just _) -> (ph <> Just ":" <> (show <$> pp))
                   _ -> Nothing
  osmAndIndexes <- runReader parseOsmAndIndexes proxyCfg
  w <- execOsmAnd (ctx osmAndIndexes f) $ osmAndContentFromXml Voice -- voice
       >> osmAndContentFromXml Map -- map 
       >> osmAndContentFromXml (read "wikimap"::OsmAndType) -- wikimap
       >> osmAndContentFromXml Fonts -- fonts
       >> osmAndContentFromXml Depth -- depth
       >> osmAndContentFromXml WikiVoyage -- wikivoyage
       >> osmAndContentFromXml RoadMap -- road_map
       >> osmAndContentFromXml SrtmMap -- srtm_map
       >> osmAndContentFromXml Hillshade -- hillshade

  _ <- case w of
    (osmAndContent, osmAndXmlTree) -> do
      osmAndContentToXmlFIle (d ++ "/indexes.xml") osmAndXmlTree
      osmAndContent >>= liftIO . filterM (\o -> do
                                             targetExist <- doesFileExist (d </> osmAndContentName o)
                                             case targetExist of
                                               True -> do
                                                 fileSize <- getFileSize (d </> osmAndContentName o)
                                                 return $ osmAndContentContainerSize o /= fileSize
                                               False -> return True)
                    >>= mapConcurrently (\oaContent -> do
                                       let prefixdwl = case (osmAndContentRoot oaContent) of
                                             "region" -> "/download.php?standard=yes&file="
                                             "road_region" -> "/download.php?road=yes&file="
                                             "hillshade" -> "/download.php?hillshade=yes&file="
                                             "wiki" -> "/download.php?wiki=yes&file="
                                             "srtmcountry" -> "/download.php?srtmcountry=yes&file="
                                             p -> "/" ++ p ++ "/"
                                       displayConsoleRegions $ do
                                         req <- parseRequest ("https://download.osmand.net" ++ prefixdwl ++ (osmAndContentName oaContent))
                                         req' <- case (ph, pp) of
                                                   (Just h, Just p) -> return $ addProxy (ByteString.pack h) p (req { responseTimeout = responseTimeoutNone })
                                                   _ -> return (req { responseTimeout = responseTimeoutNone })
                                         manager <- newManager tlsManagerSettings
                                         runResourceT $ do
                                           res <- http req' manager
                                           case lookup hContentLength (responseHeaders res) of
                                             Just cl -> do
                                               -- liftIO $ putStrLn $ "Try to dwl: " <> "https://download.osmand.net" <> prefixdwl <> osmAndContentName oaContent
                                               pg <- liftIO $ newProgressBar def { pgTotal = read (ByteString.unpack cl)
                                                                                 , pgWidth = 100
                                                                                 , pgOnCompletion = Just $ "Download " ++ (osmAndContentName oaContent) ++ " done :percent after :elapsed seconds"
                                                                                 }
                                               runConduit $ responseBody res .| updateProgress pg .| sinkFile (d </> (osmAndContentName oaContent))
                                               liftIO $ complete pg
                                             Nothing -> do
                                               -- pure ()
                                               liftIO $ print (responseHeaders res)
                                               liftIO $ print req'
                                               liftIO $ putStrLn "Can't fetch hContentLength"
                                   )
  return ()
  where
    ctx idx filters = OsmAndContext Nothing idx filters

main :: IO ()
main = osmand =<< execParser opts
  where
    opts = info (optArgs <**> helper)
      (fullDesc
        <> progDesc ("OsmAnd mirror " ++ showVersion version ++ " " ++ $(gitBranch) ++ " " ++ $(gitHash))
        <> header ("osmand - " ++ showVersion version))
