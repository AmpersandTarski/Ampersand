module Main (main) where

import Database.Design.Ampersand.Test.Parser.ParseScripts
import Database.Design.Ampersand.Test.Parser.QuickChecks

runTests :: IO ()
runTests =
     do scr <- scripts
        success <- testScripts scr
        if success then parserQuickChecks
        else return ()
        return ()

main :: IO ()
main = runTests
