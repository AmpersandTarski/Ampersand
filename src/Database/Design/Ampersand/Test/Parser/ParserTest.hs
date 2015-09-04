{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}
module Database.Design.Ampersand.Test.Parser.ParserTest (
    parseReparse, parseScripts, showErrors
) where

import Prelude hiding (readFile)
import Database.Design.Ampersand.ADL1.PrettyPrinters(prettyPrint)
import Database.Design.Ampersand.Core.ParseTree
import Database.Design.Ampersand.Input.ADL1.CtxError (Guarded(..),whenChecked,CtxError)
import Database.Design.Ampersand.Input.ADL1.Parser
import Database.Design.Ampersand.Input.Parsing
import Database.Design.Ampersand.Misc.Options(Options)
import System.IO (hPutStrLn, stderr)

-- Tries to parse all the given files
parseScripts :: Options -> [FilePath] -> IO Bool
parseScripts _ [] = return True
parseScripts opts (f:fs) =
     do parsed <- parseADL opts (Left f)
        case parsed of
            Checked _ -> do { putStrLn ("Parsed: " ++ f); parseScripts opts fs }
            Errors  e -> do { putStrLn ("Cannot parse: " ++ f); showErrors e; return False }

printErrLn :: Show a => a -> IO ()
printErrLn a = hPutStrLn stderr (show a)

showErrors :: [CtxError] -> IO ()
showErrors [] = return ()
showErrors (e:es) = do { printErrLn e; showErrors es }

parse :: FilePath -> String -> Guarded P_Context
parse file txt = whenChecked (runParser pContext file txt) (Checked . fst)

parseReparse :: FilePath -> String -> Guarded P_Context
parseReparse file txt = whenChecked (parse file txt) reparse
                  where reparse p = parse (file ++ "**pretty") (prettyPrint p)
