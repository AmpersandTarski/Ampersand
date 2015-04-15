{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}
module Database.Design.Ampersand.Test.Parser.ParserTest (
    parse, parseReparse, parseScripts
) where

import Prelude hiding (readFile)
import Database.Design.Ampersand.ADL1.PrettyPrinters(prettyPrint)
import Database.Design.Ampersand.Core.ParseTree
import Database.Design.Ampersand.Input.ADL1.CtxError (Guarded(..),whenChecked)
import Database.Design.Ampersand.Input.ADL1.Parser
import Database.Design.Ampersand.Input.Parsing
import Database.Design.Ampersand.Misc.Options(Options)
import Debug.Trace

-- Tries to parse all the given files
parseScripts :: Options -> [FilePath] -> IO Bool
parseScripts _ [] = return True
parseScripts opts (f:fs) =
     do parsed <- parseADL opts f
        case parsed of
            Checked _ -> parseScripts opts fs
            _         -> return False

parse :: FilePath -> String -> Guarded P_Context
parse file txt =
    case runParser pContext file txt of
        Errors  e     -> trace (show e ++ "\n" ++ txt) (Errors e)
        Checked (p,_) -> trace ("Parsed: " ++ file)    (Checked p)

parseReparse :: FilePath -> String -> Guarded P_Context
parseReparse file txt = whenChecked (parse file txt) reparse
                  where reparse p = parse (file ++ "**pretty") (prettyPrint p)
