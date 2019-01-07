module Main where

import           Ampersand
import           Control.Monad
import           Data.List
import qualified Data.List.NonEmpty as NEL (toList)

main :: IO ()
main =
 do opts <- getOptions
    verboseLn opts ampersandVersionStr
    mapM_ doWhen (actionsWithoutScript opts) -- There are commands that do not need a single filename to be speciied
    case fileName opts of
      Just _ -> do -- An Ampersand script is provided that can be processed
            { putStrLn "Processing your model..."
            ; gMulti <- createMulti opts
            ; case gMulti of
                Errors err    -> 
                   exitWith . NoValidFSpec . intersperse  (replicate 30 '=') 
                 . fmap showErr . NEL.toList $ err
                Checked multi -> 
                   generateAmpersandOutput multi
            ; putStrLn "Finished processing your model"
            }
      Nothing -> -- No Ampersand script is provided 
         if or (map fst $ actionsWithoutScript opts)
         then verboseLn opts $ "No further actions, because no ampersand script is provided"
         else putStrLn "No ampersand script provided. Use --help for usage information"

 where
   doWhen :: (Bool, IO ()) -> IO()
   doWhen (b,x) = when (b) x

   actionsWithoutScript :: Options -> [(Bool, IO())]
   actionsWithoutScript options = 
      [ ( test options                  , putStrLn $ "Executable: " ++ show (dirExec options) )
      , ( showVersion options           , putStrLn $ versionText options  )
      , ( genSampleConfigFile options   , writeConfigFile                 )
      , ( showHelp options              , putStrLn $ usageInfo' options   )
      ]
   
   versionText :: Options -> String
   versionText opts = preVersion opts ++ ampersandVersionStr ++ postVersion opts
