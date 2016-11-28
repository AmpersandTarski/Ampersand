module Main where

import Data.List
import Prelude hiding (putStr)
import Ampersand

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
    else do gMulti <- createMulti opts
            case gMulti of
              Errors err    -> exitWith . NoValidFSpec . intersperse  (replicate 30 '=') . map showErr $ err
              Checked multi -> generateAmpersandOutput multi
                                  

