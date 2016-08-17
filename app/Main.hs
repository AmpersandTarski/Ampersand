module Main where

import Data.List
import System.Exit
import Prelude hiding (putStr,putStrLn)
import Ampersand

main :: IO ()
main =
 do opts <- getOptions
    if showVersion opts || showHelp opts
    then mapM_ putStr (helpNVersionTexts ampersandVersionStr opts)
    else do gFSpec <- createFSpec opts
            case gFSpec of
              Errors err    -> do mapM_ putStrLn (intersperse  (replicate 30 '=') (map showErr err))
                                  exitWith $ ExitFailure 10
              Checked fSpec -> generateAmpersandOutput fSpec
                                  

