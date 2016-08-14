module Main where

import Control.Monad
import Data.List
import System.Exit
import Prelude hiding (putStr,putStrLn,readFile,writeFile)
import Ampersand.Misc
import Ampersand.Basics
import Ampersand.Input
import Ampersand.FSpec
import Ampersand.Components

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
                                  

