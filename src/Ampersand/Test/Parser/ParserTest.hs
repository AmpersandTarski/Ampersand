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
parseScripts :: (HasOptions env, HasHandles env, HasVerbosity env) => 
                [FilePath] ->  RIO env Bool
parseScripts paths =
  case paths of
    [] -> return True
    (f:fs) -> do
        parsed <- snd <$> parseADL f
        case parsed of
            Checked _ ws -> do
                putStrLn ("Parsed: " ++ f)
                mapM_  putStrLn . concatMap (lines . show) $ ws
                parseScripts fs
            Errors  e -> do 
                putStrLn ("Cannot parse: " ++ f)
                showErrors (NEL.toList e)
                return False

showErrors :: (HasHandles env) => [CtxError] ->  RIO env ()  -- TODO: Use error logger to write the errors to. ( See http://hackage.haskell.org/package/rio-0.1.9.2/docs/RIO.html#g:8 )
showErrors = mapM_ $ mapM_ putStrLn . lines . show

parse :: FilePath -> String -> Guarded P_Context
parse file txt = whenChecked (runParser pContext file txt) (pure . fst)

parseReparse :: FilePath -> String -> Guarded P_Context
parseReparse file txt = whenChecked (parse file txt) reparse
                  where reparse p = parse (file ++ "**pretty") (prettyPrint p)
