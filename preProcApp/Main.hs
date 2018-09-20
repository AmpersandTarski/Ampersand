module Main where

import Ampersand.Input.PreProcessor
import Ampersand.Basics.UTF8

main :: IO ()
main =
  do
    filename:defs <- getArgs;
    fileContents  <- readUTF8File fileName;
    return (preProcess defs fileContents);