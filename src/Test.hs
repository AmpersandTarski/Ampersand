module Main (main) where

import Database.Design.Ampersand.Test.RunAmpersand (ampersand)
import Database.Design.Ampersand.Test.TestScripts (getTestScripts)
import Database.Design.Ampersand.Test.Parser.ParserTest (parseScripts)
import Database.Design.Ampersand.Test.Parser.QuickChecks (parserQuickChecks)
import System.Exit (ExitCode, exitFailure, exitSuccess)

testFunctions :: IO [(String, IO Bool)]
testFunctions = do scr <- getTestScripts
                   return [ ("Parsing " ++ show (length scr) ++ " scripts.", parseScripts scr)
                          , ("Executing ampersand chain", ampersand scr)
                          , ("Running automatic quick checks", parserQuickChecks)
                          ]

main :: IO ExitCode
main = do funcs <- testFunctions
          tests funcs
    where tests :: [(String, IO Bool)] -> IO ExitCode
          tests [] = exitSuccess
          tests ((msg,test):xs) =
            do putStrLn msg
               success <- test
               if success then tests xs
               else exitFailure
