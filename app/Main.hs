module Main where

import           Ampersand
import           Data.List
import qualified Data.List.NonEmpty as NEL (toList)

main :: IO ()
main =
 do opts <- getOptions
    if showVersion opts || showHelp opts --HJO 20161127 TODO: There are more commands that do not 
                                         --  need a single filename to be specified, 
                                         --  like --sampleConfigFile. Currently, this
                                         --  does not work properly. A more generic
                                         --  approach for handling those options should
                                         --  be thought of. 
    then mapM_ putStr (helpNVersionTexts ampersandVersionStr opts)
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

