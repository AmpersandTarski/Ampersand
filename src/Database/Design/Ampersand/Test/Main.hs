module Main where

import Database.Design.Ampersand.Misc.Options
import Database.Design.Ampersand.Test.Parser.RunTests

main :: IO ()
main =
 do opts <- getOptions
    runTests opts
