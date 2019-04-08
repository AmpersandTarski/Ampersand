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
import qualified Data.List.NonEmpty as NEL (toList)

-- Tries to parse all the given files
parseScripts :: Options -> [FilePath] -> IO Bool
parseScripts _ [] = return True
parseScripts opts (f:fs) =
     do parsed <- snd <$> parseADL opts f
        case parsed of
            Checked _ ws -> do
                putStrLn ("Parsed: " ++ f)
                mapM_  putStrLn . concatMap (lines . show) $ ws
                parseScripts opts fs
            Errors  e -> do 
                putStrLn ("Cannot parse: " ++ f)
                showErrors (NEL.toList e)
                return False

printErrLn :: Show a => a -> IO ()
printErrLn = hPrint stderr

showErrors :: [CtxError] -> IO ()
showErrors [] = return ()
showErrors (e:es) = do { printErrLn e; showErrors es }

parse :: FilePath -> String -> Guarded P_Context
parse file txt = whenChecked (runParser pContext file txt) (pure . fst)

parseReparse :: FilePath -> String -> Guarded P_Context
parseReparse file txt = whenChecked (parse file txt) reparse
                  where reparse p = parse (file ++ "**pretty") (prettyPrint p)
