{-# LANGUAGE RecordWildCards #-}
module Main(main) where

import           Ampersand
import qualified RIO.List as L
import qualified Data.List.NonEmpty as NEL
import           System.Environment    (getArgs, getProgName)
-- This datatype contains global information available throughout the lifefime of the application.

main :: IO ()
main = do
  opts@Options{..} <- getOptions
  let app = App
       { opts = opts
       , appHandle = stdout
       }
  runRIO app $ do
     ampersand

ampersand :: RIO App ()
ampersand = do
 app <- ask
 do let opts'@Options{..} = opts app
    sequence_ . map snd . filter fst $ actionsWithoutScript opts'-- There are commands that do not need a single filename to be speciied
    case fileName of
      Just _ -> do -- An Ampersand script is provided that can be processed
            { liftIO $ putStrLn "Processing your model..."
            ; gMulti <- liftIO $ createMulti opts'
            ; liftIO $ case gMulti of
                Errors err    -> 
                   exitWith . NoValidFSpec . L.intersperse  (replicate 30 '=') 
                 . fmap show . NEL.toList $ err
                Checked multi ws -> do
                   mapM_  putStrLn . concatMap (lines . show) $ ws
                   generateAmpersandOutput opts' multi
                   putStrLn "Finished processing your model"
                   putStrLn . ("Your script has no errors " ++) $
                      case ws of
                        []  -> "and no warnings"
                        [_] -> ", but one warning was found"
                        _   -> ", but "++show (length ws)++" warnings were found"
            }
      Nothing -> liftIO $ -- No Ampersand script is provided 
         if or (map fst $ actionsWithoutScript opts')
         then verboseLn "No further actions, because no ampersand script is provided"
         else do
            args     <- getArgs
            progName <- getProgName
            exitWith . NoAmpersandScript $
                 [ "No ampersand script provided. Use --help for usage information"
                 , "   " <> progName <> (concat $ fmap (" " <>) args) ]

 where
   actionsWithoutScript :: Options -> [(Bool, RIO App ())]
   actionsWithoutScript opts@Options{..} = 
      [ ( test                     , liftIO . putStrLn $ "Executable: " ++ show dirExec )
      , ( showVersion  || verboseP , liftIO . putStrLn $ versionText opts)
      , ( genSampleConfigFile      , liftIO $ writeConfigFile)
      , ( showHelp                 , liftIO . putStrLn $ usageInfo' opts)
      , ( runAsDaemon              , liftIO $ runDaemon opts)
      ]
   
   versionText :: Options -> String
   versionText opts = preVersion opts ++ ampersandVersionStr ++ postVersion opts
