{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Database.Design.Ampersand.Test.Parser.QuickChecks (parserQuickChecks) where

import Database.Design.Ampersand.Test.Parser.ParserTest (parseReparse)
import Database.Design.Ampersand.Test.Parser.ArbitraryTree()
import Database.Design.Ampersand.ADL1.PrettyPrinters(pretty)
import Database.Design.Ampersand.Core.ParseTree (P_Context)

import Test.QuickCheck

-- Tries to parse a string, and if successful, tests the result with the given function
testParse :: String -> (P_Context -> Bool) -> Bool
testParse text check = if success then check ctx else False
        where (ctx, success) = parseReparse "QuickChecks.hs" text

-- Tests whether the parsed context is equal to the original one
prop_pretty :: P_Context -> Bool
prop_pretty ctx = testParse (pretty ctx) (\p -> ctx == p)

parserQuickChecks :: IO Bool
parserQuickChecks =
         do res <- quickCheckResult prop_pretty
            case res of
                Success _ _ _ -> return True
                _             -> return False
