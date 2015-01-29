{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Database.Design.Ampersand.Test.Parser.QuickChecks (parserQuickChecks) where

import Database.Design.Ampersand.Test.Parser.ParserTest
import Database.Design.Ampersand.Test.Parser.ArbitraryTree()

import Database.Design.Ampersand.Core.AbstractSyntaxTree (A_Context(..))
import Database.Design.Ampersand.FSpec.ShowADL
import Database.Design.Ampersand.ADL1.P2A_Converters(Guarded(..))

import Debug.Trace
import Test.QuickCheck

-- Tries to parse a string, and if successful, tests the result with the given function
testParse :: String -> (A_Context -> Bool) -> Bool
testParse text check =
            case parse text of
                Errors e   -> trace (text ++ show e) False
                Checked p  -> check p

-- Tests whether the parsed context is equal to the original one
prop_pretty :: A_Context -> Bool
prop_pretty ctx = testParse (showADL ctx) (\p -> ctx == p)

parserQuickChecks :: IO ()
parserQuickChecks = quickCheck prop_pretty
