module Main where

import Data.List
import Prelude hiding (putStr)
import Ampersand

main :: IO ()
main =
 do opts <- getOptions
    if showVersion opts || showHelp opts
    then mapM_ putStr (helpNVersionTexts ampersandVersionStr opts)
    else do gFSpec <- createFSpec opts
            case gFSpec of
              Errors err    -> exitWith . NoValidFSpec . intersperse  (replicate 30 '=') . map showErr $ err
              Checked fSpec -> generateAmpersandOutput fSpec
                                  

