{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Database.Design.Ampersand.Test.Parser.QuickChecks (parserQuickChecks) where

import Database.Design.Ampersand.Test.Parser.ParserTest
import Database.Design.Ampersand.Test.Parser.ArbitraryTree

import Test.QuickCheck

import Database.Design.Ampersand.ADL1.P2A_Converters(Guarded(..),pCtx2aCtx)
import Database.Design.Ampersand.Core.AbstractSyntaxTree (A_Context(..))
import Database.Design.Ampersand.Core.ParseTree (P_Context(..))
import Database.Design.Ampersand.FSpec.ShowADL
import Database.Design.Ampersand.Input.ADL1.Parser
import Database.Design.Ampersand.Input.ADL1.UU_Scanner
import Database.Design.Ampersand.InputProcessing
import Database.Design.Ampersand.Misc.Options
import Debug.Trace
import System.Directory
import Control.Monad

checkResult :: ShowADL a => Guarded a -> (a -> Bool) -> Bool
checkResult guard check =
            case guard of
                Errors e   -> trace (show e) False
                Checked p  -> check p

prop_pretty :: A_Context -> Bool
prop_pretty xs = checkResult (parse $ showADL xs) (\p -> xs == p)

parserQuickChecks = quickCheck prop_pretty