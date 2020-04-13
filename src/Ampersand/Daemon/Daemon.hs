{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Library for spawning and working with Ghci sessions.
-- _Acknoledgements_: This is mainly copied from Neil Mitchells ghcid.
module Ampersand.Daemon.Daemon(
    DaemonState(loadResults),
    loads,
    messages,
    loaded,
    startAmpersandDaemon
    ) where

import           Ampersand.Basics
import           Ampersand.Daemon.Parser
import           Ampersand.Daemon.Types
import           Ampersand.Misc.HasClasses
import qualified RIO.List as L
import qualified RIO.Text as T
import           System.Directory
import           System.FilePath
import           Ampersand.Types.Config

messages :: DaemonState -> [Load]
messages = filter isMessage . loads
loaded :: DaemonState -> [FilePath]
loaded = loadResults
data DaemonState = DaemonState
    { loads :: [Load]
    , loadResults :: [FilePath]
    }
instance Show DaemonState where
  show x
   = "DaemonState: #loads = "++(show .length . loads $ x)++" #loadResults = "++(show .length . loadResults $ x)

startAmpersandDaemon :: (HasDaemonOpts env, HasRunner env) =>
        RIO env DaemonState
startAmpersandDaemon = do
    init <- initialState
    case init of
        Left msg -> exitWith . NoConfigurationFile $ msg
        Right s -> pure s  

initialState :: (HasDaemonOpts env, HasRunner env) =>
                RIO env (Either [Text] DaemonState)
initialState = do
    daemonConfig <- view daemonConfigL 
    curDir <- liftIO $ getCurrentDirectory
    dotAmpersand <- liftIO $ makeAbsolute $ curDir </> daemonConfig
    result <- readUTF8File dotAmpersand
    case result of
     Right content -> do
            let files = map T.unpack
                      . filter (\fn -> T.length fn > 0  --discard empty lines
                                    && (not $ "#"  `T.isPrefixOf` fn)  -- line commented out yaml style
                                    && (not $ "--" `T.isPrefixOf` fn)  -- line commented out haskellish style
                               )
                      . L.nub                           --discard doubles
                      . T.lines $ content
            (ls,loadedFiles) <- do
                xs <- mapM parseProject files
                return ( L.nub . concatMap fst $ xs
                       , L.nub . concatMap snd $ xs
                       )
            return $ Right DaemonState
              { loads = ls
              , loadResults = L.nub $ [dotAmpersand] ++ loadedFiles
              }
     Left err -> return . Left $
        [ tshow err
        , "File not found: "<>T.pack dotAmpersand
        , "  Your workspace should contain a file called .ampersand. However,"
        , "  it could not be found. Please provide that file, containing the "
        , "  name of the top file(s) of your Ampersand project. One name per line."
        ] 
 