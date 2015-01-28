{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Database.Design.Ampersand.Test.Parser.ParserTest (parseFile, parse) where

import Database.Design.Ampersand.ADL1.P2A_Converters(Guarded(..),pCtx2aCtx)
import Database.Design.Ampersand.Core.AbstractSyntaxTree (A_Context(..))
import Database.Design.Ampersand.Input.ADL1.Parser
import Database.Design.Ampersand.InputProcessing
import Database.Design.Ampersand.Misc.Options
import Debug.Trace

options :: Options
options = Options {}

unguard :: String -> Guarded a -> Bool
unguard str grd = case grd of
   Errors  e -> trace ("Cannot parse: " ++ show e ++ ": " ++ str) True
   Checked _ -> trace ("Parsed: " ++ str) True

parseFile :: FilePath -> IO Bool
parseFile name = do contents <- readFile name
                    let pResult = runParser pContext name contents
                    return $ unguard name pResult

parse :: String -> Guarded A_Context
parse str = let pResult = runParser pContext "Sample.hs" str
            in case pResult of
               Errors  parseErr -> Errors parseErr
               Checked (pctx,_) -> pCtx2aCtx options pctx
