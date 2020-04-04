{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.Test.Parser.ParserTest (
    parseReparse, parseScripts, showErrors
) where

import           Ampersand.ADL1.PrettyPrinters(prettyPrint)
import           Ampersand.Basics
import           Ampersand.Core.ParseTree
import           Ampersand.Input.ADL1.CtxError (Guarded(..),whenChecked,CtxError)
import           Ampersand.Input.ADL1.Parser
import           Ampersand.Input.Parsing
import           Ampersand.Options.FSpecGenOptsParser
import           Ampersand.Types.Config
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
-- Tries to parse all the given files
parseScripts :: (HasRunner env) => 
                [FilePath] ->  RIO env Bool
parseScripts paths =
  case paths of
    [] -> return True
    (f:fs) -> do
        let fSpecGenOpts = defFSpecGenOpts f
        parsed <- snd <$> extendWith fSpecGenOpts (parseFileTransitive f)
        case parsed of
            Checked _ ws -> do
                logInfo $ "Parsed: " <> display (T.pack f)
                mapM_ logWarn (fmap displayShow ws)
                parseScripts fs
            Errors  e -> do 
                logError $ "Cannot parse: " <> display (T.pack f)
                showErrors (NE.toList e)
                return False

showErrors :: (HasLogFunc env) => [CtxError] ->  RIO env ()
showErrors = sequence_ . fmap logError . map displayShow 

parse :: FilePath -> Text -> Guarded P_Context
parse file txt = whenChecked (runParser pContext file txt) (pure . fst)

parseReparse :: FilePath -> Text -> Guarded P_Context
parseReparse file txt = whenChecked (parse file txt) reparse
                  where reparse p = parse (file ++ "**pretty") (prettyPrint p)
