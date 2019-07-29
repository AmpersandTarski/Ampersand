{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}
module Ampersand.Test.Parser.ParserTest (
    parseReparse, parseScripts, showErrors
) where

import           Ampersand.ADL1.PrettyPrinters(prettyPrint)
import           Ampersand.Basics
import           Ampersand.Core.ParseTree
import           Ampersand.Input.ADL1.CtxError (Guarded(..),whenChecked,CtxError)
import           Ampersand.Input.ADL1.Parser
import           Ampersand.Input.Parsing
import           Ampersand.Misc
import qualified Data.List.NonEmpty as NEL

-- Tries to parse all the given files
parseScripts :: (HasParseOptions env, HasLogFunc env) => 
                [FilePath] ->  RIO env Bool
parseScripts paths =
  case paths of
    [] -> return True
    (f:fs) -> do
        parsed <- snd <$> parseADL f
        case parsed of
            Checked _ ws -> do
                sayLn ("Parsed: " ++ f)
                mapM_  sayLn . concatMap (lines . show) $ ws
                parseScripts fs
            Errors  e -> do 
                sayLn ("Cannot parse: " ++ f)
                showErrors (NEL.toList e)
                return False

showErrors :: (HasLogFunc env) => [CtxError] ->  RIO env ()  -- TODO: Use error logger to write the errors to. ( See http://hackage.haskell.org/package/rio-0.1.9.2/docs/RIO.html#g:8 )
showErrors = mapM_ $ mapM_ sayLn . lines . show

parse :: FilePath -> String -> Guarded P_Context
parse file txt = whenChecked (runParser pContext file txt) (pure . fst)

parseReparse :: FilePath -> String -> Guarded P_Context
parseReparse file txt = whenChecked (parse file txt) reparse
                  where reparse p = parse (file ++ "**pretty") (prettyPrint p)
