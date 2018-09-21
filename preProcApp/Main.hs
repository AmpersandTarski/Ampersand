module Main where

import Ampersand
import System.Environment
import Ampersand.Input.PreProcessor
import Ampersand.Basics.UTF8 (readUTF8File)

main :: IO ()
main =
  do
    filename:defs <- getArgs;
    input       <- readUTF8File filename
    inputString <- return $ either id id input
    putStr $ show (preProcess filename defs inputString) ++ "\n"
