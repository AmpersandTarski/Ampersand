{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}
module Database.Design.Ampersand.Test.Parser.QuickChecks (parserQuickChecks) where

import Database.Design.Ampersand.Test.Parser.ParserTest (parseReparse)
import Database.Design.Ampersand.Test.Parser.ArbitraryTree()
import Database.Design.Ampersand.ADL1.PrettyPrinters(prettyPrint)
import Database.Design.Ampersand.Core.ParseTree (P_Context)
import Database.Design.Ampersand.Input.ADL1.CtxError (Guarded(..))

import Test.QuickCheck(Args(..), quickCheckWithResult, Testable, Result(..))
import Debug.Trace

-- Tries to parse a string, and if successful, tests the result with the given function
testParse :: String -> (P_Context -> Bool) -> Bool
testParse text check = case parseReparse "QuickChecks.hs" text of
            Checked a -> check a
            _  -> False
            -- TODO: Errors e  -> do { showErrors e; return False }

-- Tests whether the parsed context is equal to the original one
prop_pretty :: P_Context -> Bool
prop_pretty ctx = testParse prettyCtx eq
        where eq p = ctx == p || trace ("Printed versions are different: " ++ prettyCtx ++ "\n\n---------\n\n" ++ prettyPrint p) False
              prettyCtx = prettyPrint ctx

checkArgs :: Args
checkArgs = Args
  { replay          = Nothing
  , maxSuccess      = 64
  , maxDiscardRatio = 8
  , maxSize         = 8      -- otherwise there's nothing quick about it.
  , chatty          = False
  }

-- TODO: Improve the messages given here, remove all trace's
test :: Testable prop => prop -> IO Bool
test p = do res <- quickCheckWithResult checkArgs p
            case trace (show res) res of
                Success {} -> return True
                _          -> return False

parserQuickChecks :: IO Bool
parserQuickChecks = test prop_pretty
