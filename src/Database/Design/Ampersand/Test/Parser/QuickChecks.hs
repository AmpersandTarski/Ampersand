{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Database.Design.Ampersand.Test.Parser.QuickChecks (parserQuickChecks) where

import Database.Design.Ampersand.Test.Parser.ParserTest (parseReparse)
import Database.Design.Ampersand.Test.Parser.ArbitraryTree()
import Database.Design.Ampersand.ADL1.PrettyPrinters(pretty_print)
import Database.Design.Ampersand.Core.ParseTree (P_Context)

import Test.QuickCheck(Args(..), quickCheckWithResult, Testable, Result(..))
import Debug.Trace

-- Tries to parse a string, and if successful, tests the result with the given function
testParse :: String -> (P_Context -> Bool) -> Bool
testParse text check = if success then check ctx else False
        where (ctx, success) = parseReparse "QuickChecks.hs" text

-- Tests whether the parsed context is equal to the original one
prop_pretty :: P_Context -> Bool
prop_pretty ctx = testParse (pretty_print ctx) (\p -> pretty_print ctx == pretty_print p)

checkArgs :: Args
checkArgs = Args
  { replay          = Nothing
  , maxSuccess      = 100
  , maxDiscardRatio = 10
  , maxSize         = 100
  , chatty          = False
  }

-- TODO: Improve the messages given here, remove all trace's
test :: Testable prop => prop -> IO Bool
test p = do res <- quickCheckWithResult checkArgs p
            case trace (show res) res of
                Success _ _ _ -> return True
                _             -> return False

parserQuickChecks :: IO Bool
parserQuickChecks =
         do res <- test prop_pretty
            return res
