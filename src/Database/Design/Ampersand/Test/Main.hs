module Main (main) where

import Database.Design.Ampersand.Test.Parser.ParseScripts
import Database.Design.Ampersand.Test.Parser.QuickChecks
import System.Exit (ExitCode(..))

runTests :: IO Bool
runTests =
     do scr <- scripts
        putStrLn $ "Parsing " ++ show (length scr) ++ " scripts."
        success <- testScripts scr
        if success then
             do putStrLn $ "Running automatic quick checks"
                parserQuickChecks
        else return False

main :: IO ExitCode
main = do res <- runTests
          if res then return ExitSuccess
          else return $ ExitFailure 1
