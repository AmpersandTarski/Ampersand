{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Database.Design.Ampersand.Test.Parser.ParserTest (parseFile, parse) where

import Database.Design.Ampersand.ADL1.P2A_Converters(Guarded(..))
import Database.Design.Ampersand.ADL1.PrettyPrinters
import Database.Design.Ampersand.Core.ParseTree
import Database.Design.Ampersand.Input.ADL1.Parser
import Database.Design.Ampersand.InputProcessing
import Debug.Trace

reparse p = show p ++ show (runParser pContext "Reparse" (pretty p))

unguard :: String -> Guarded (P_Context,[String]) -> Bool
unguard str grd = case grd of
   Errors  e     -> trace ("Cannot parse: " ++ show e ++ ": " ++ str) False
   Checked (p,_) -> trace ("Parsed: " ++ str ++ "\n" ++ pretty p ++ "\n" ++ reparse p) False
   -- Checked (p,_) -> trace ("Parsed: " ++ str ++ "\n" ++ pretty p) False
   -- Checked _     -> trace ("Parsed: " ++ str) True

parseFile :: FilePath -> IO Bool
parseFile name =
     do contents <- readFile name
        if null contents then return True
        else return $ unguard name (run contents)
     where run = runParser pContext name

parse :: String -> Guarded P_Context
parse str = let pResult = runParser pContext "Sample.hs" str
            in case pResult of
               Errors  parseErr -> Errors parseErr
               Checked (pctx,_) -> Checked pctx
