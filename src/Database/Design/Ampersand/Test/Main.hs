module Main (main) where

import Database.Design.Ampersand.Test.Parser.ParseScripts
import Database.Design.Ampersand.Test.Parser.QuickChecks
import System.Exit (ExitCode, exitFailure, exitSuccess)

testFunctions :: IO [(String, IO Bool)]
testFunctions = do scr <- scripts
                   return [
                     ("Parsing " ++ show (length scr) ++ " scripts.", testScripts scr),
                     ("Running automatic quick checks", parserQuickChecks)]

main :: IO ExitCode
main = do funcs <- testFunctions
          tests funcs
    where tests :: [(String, IO Bool)] -> IO ExitCode
          tests [] = exitSuccess
          tests ((msg,fun):xs) =
            do putStrLn msg
               success <- fun
               if success then tests xs
               else exitFailure
