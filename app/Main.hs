module Main where

import Data.List
import Prelude hiding (putStr)
import Ampersand

main :: IO ()
main =
 do opts <- getOptions
    if showVersion opts || showHelp opts
    then mapM_ putStr (helpNVersionTexts ampersandVersionStr opts)
    else do gMulti <- createMulti opts
            case gMulti of
              Errors err    -> exitWith . NoValidFSpec . intersperse  (replicate 30 '=') . map showErr $ err
              Checked multi -> generateAmpersandOutput multi
                                  

