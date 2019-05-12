{-# LANGUAGE RecordWildCards #-}
module Main(main) where

import           Ampersand
import qualified RIO.List as L
import qualified Data.List.NonEmpty as NEL
import           System.Environment    (getArgs, getProgName)
-- This datatype contains global information available throughout the lifefime of the application.
data App = App
  { options :: !Options
  , appHandle :: !Handle
  }

main :: IO ()
main = do
  opts@Options{..} <- getOptions
  let app = App
       { options = opts
       , appHandle = stderr
       }
  runRIO app $ do
     ampersand

ampersand :: RIO App ()
ampersand = do
 app <- ask
 do let opts@Options{..} = options app
    liftIO . sequence_ . map snd . filter fst $ actionsWithoutScript opts-- There are commands that do not need a single filename to be speciied
    case fileName of
      Just _ -> liftIO $ do -- An Ampersand script is provided that can be processed
            { putStrLn "Processing your model..."
            ; gMulti <- createMulti opts
            ; case gMulti of
                Errors err    -> 
                   exitWith . NoValidFSpec . L.intersperse  (replicate 30 '=') 
                 . fmap show . NEL.toList $ err
                Checked multi ws -> do
                   mapM_  putStrLn . concatMap (lines . show) $ ws
                   generateAmpersandOutput opts multi
                   putStrLn "Finished processing your model"
                   putStrLn . ("Your script has no errors " ++) $
                      case ws of
                        []  -> "and no warnings"
                        [_] -> ", but one warning was found"
                        _   -> ", but "++show (length ws)++" warnings were found"
            }
      Nothing -> liftIO $ -- No Ampersand script is provided 
         if or (map fst $ actionsWithoutScript opts)
         then verboseLn "No further actions, because no ampersand script is provided"
         else do
            args     <- getArgs
            progName <- getProgName
            exitWith . NoAmpersandScript $
                 [ "No ampersand script provided. Use --help for usage information"
                 , "   " <> progName <> (concat $ fmap (" " <>) args) ]

 where
   actionsWithoutScript :: Options -> [(Bool, IO())]
   actionsWithoutScript opts@Options{..} = 
      [ ( test                     , putStrLn $ "Executable: " ++ show dirExec )
      , ( showVersion  || verboseP , putStrLn $ versionText opts)
      , ( genSampleConfigFile      , writeConfigFile)
      , ( showHelp                 , putStrLn $ usageInfo' opts)
      , ( runAsDaemon              , runDaemon opts)
      ]
   
   versionText :: Options -> String
   versionText opts = preVersion opts ++ ampersandVersionStr ++ postVersion opts
