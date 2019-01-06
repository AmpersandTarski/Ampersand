module Main where

import           Ampersand
import           Control.Monad
import           Data.List
import qualified Data.List.NonEmpty as NEL (toList)

main :: IO ()
main =
 do opts <- getOptions
    mapM_ doWhen (actionsWithoutScript opts) -- There are commands that do not need a single filename to be speciied
    if orList (map fst $ actionsWithoutScript opts)
    then do { verboseLn opts $ "Skipping model processing because special action is requested"}
    else do { verboseLn opts $ ampersandVersionStr
            ; putStrLn "Processing your model..."
            ; gMulti <- createMulti opts
            ; case gMulti of
                Errors err    -> 
                   exitWith . NoValidFSpec . intersperse  (replicate 30 '=') 
                 . fmap showErr . NEL.toList $ err
                Checked multi -> 
                   generateAmpersandOutput multi
            ; putStrLn "Finished processing your model"
            }
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
