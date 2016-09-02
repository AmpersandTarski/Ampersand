module Main (main) where

--import Ampersand.Misc.Options(getOptions,Options)
--import Ampersand.Test.TestScripts (getTestScripts,testAmpersandScripts)
--import Ampersand.Test.Parser.ParserTest (parseScripts)
--import Ampersand.Test.Parser.QuickChecks (parserQuickChecks)
import System.Exit (ExitCode, exitFailure, exitSuccess)
import Ampersand
import Prelude hiding (putStrLn)

testFunctions :: Options -> IO [([String], IO Bool)]
testFunctions opts =
    do scr <- getTestScripts
       (parserCheckResult, msg) <- parserQuickChecks
       return [ (["Parsing " ++ show (length scr) ++ " scripts."], parseScripts opts scr)
            --  , ("Executing ampersand chain", ampersand scr)
              , ( if parserCheckResult  
                  then ["Parser & prettyprinter test PASSED."]
                  else (  ["QuickCheck found errors in the roundtrip in parsing/prettyprinting for the following case:"]
                        ++map ("\n   "++) (lines msg)
                       )
                , return parserCheckResult
                )
              ]

main :: IO ExitCode
main = do opts <- getOptions
          funcs <- testFunctions opts
          testAmpersandScripts
          tests funcs
    where tests :: [([String], IO Bool)] -> IO ExitCode
          tests [] = exitSuccess
          tests ((msg,tst):xs) =
            do mapM_ putStrLn msg
               success <- tst
               if success then tests xs
               else do putStrLn "*** Something went wrong here***" 
                       exitFailure
