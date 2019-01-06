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
      Just _ -> do
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
      Nothing -> 
         if orList (map fst $ actionsWithoutScript opts)
         then verboseLn opts $ "No further actions, because no ampersand script is provided"
         else putStrLn "No ampersand script provided. Use --help for usage information"

 where
   doWhen :: (Bool, IO ()) -> IO()
   doWhen (b,x) = when (b) x

   orList :: [Bool] -> Bool
   orList bools = foldr (||) False bools

   actionsWithoutScript :: Options -> [(Bool, IO())]
   actionsWithoutScript options = 
      [ ( showVersion options || showHelp options , mapM_ putStr (helpNVersionTexts ampersandVersionStr options) )
      , ( genSampleConfigFile options             , writeConfigFile)
      ]
