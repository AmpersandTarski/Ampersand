module Main (main) where

import Database.Design.Ampersand.Test.Parser.ParseScripts
import Database.Design.Ampersand.Test.Parser.QuickChecks
import System.Exit (ExitCode(..))

testFunctions :: IO [(String, IO Bool)]
testFunctions = do scr <- scripts
                   return [
                     ("Running automatic quick checks", parserQuickChecks),
                     ("Parsing " ++ show (length scr) ++ " scripts.", testScripts scr)]

runTests :: IO Bool
runTests = do funcs <- testFunctions
              tests funcs
    where tests :: [(String, IO Bool)] -> IO Bool
          tests [] = return True
          tests ((msg,fun):xs) =
            do putStrLn msg
               success <- fun
               if success then tests xs
               else return False

main :: IO ExitCode
main = do res <- runTests
          if res then return ExitSuccess
          else return $ ExitFailure 1
