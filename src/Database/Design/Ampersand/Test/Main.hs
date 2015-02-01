module Main (main) where

import Database.Design.Ampersand.Test.Parser.ParseScripts
-- import Database.Design.Ampersand.Test.Parser.QuickChecks
import System.Exit (ExitCode(..))

runTests :: IO Bool
runTests =
     do scr <- scripts
        success <- testScripts scr
        if success then return True -- TODO: parserQuickChecks
        else return False

main :: IO ExitCode
main = do res <- runTests
          if res then return ExitSuccess
          else return $ ExitFailure 1
