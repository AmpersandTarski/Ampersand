{-# LANGUAGE RecordWildCards #-}
module Main(main) where

import           Ampersand
import           Data.List
import qualified Data.List.NonEmpty as NEL (toList)
import System.Environment    (getArgs, getProgName)

main :: IO ()
main =
 do opts@Options{..} <- getOptions
    sequence_ . map snd . filter fst $ actionsWithoutScript opts-- There are commands that do not need a single filename to be speciied
    case fileName of
      Just _ -> do -- An Ampersand script is provided that can be processed
            { putStrLn "Processing your model..."
            ; gMulti <- createMulti opts
            ; case gMulti of
                Errors err    -> 
                   exitWith . NoValidFSpec . intersperse  (replicate 30 '=') 
                 . fmap showErr . NEL.toList $ err
                Checked multi ws -> do
                   showWarnings ws
                   generateAmpersandOutput opts multi
                   putStrLn "Finished processing your model"
                   putStrLn . ("Your script has no errors " ++) $
                      case ws of
                        []  -> "and no warnings"
                        [_] -> ", but one warning was found"
                        _   -> ", but "++show (length ws)++" warnings were found"
            }
      Nothing -> -- No Ampersand script is provided 
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
