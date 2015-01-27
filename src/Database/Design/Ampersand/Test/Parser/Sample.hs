{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Database.Design.Ampersand.Test.Parser.Sample (runTests) where

import Database.Design.Ampersand.Test.Parser.ArbitraryTree

import Test.QuickCheck

import Database.Design.Ampersand.ADL1.P2A_Converters(Guarded(..),pCtx2aCtx)
import Database.Design.Ampersand.Core.AbstractSyntaxTree (A_Context(..))
import Database.Design.Ampersand.FSpec.ShowADL
import Database.Design.Ampersand.Input.ADL1.Parser
import Database.Design.Ampersand.Input.ADL1.UU_Scanner
import Database.Design.Ampersand.InputProcessing
import Database.Design.Ampersand.Misc.Options
import Debug.Trace

options :: Options
options = Options {}

noCtx :: A_Context
noCtx = ACtx{ctxnm="NO_CONTEXT"}

parse :: String -> Guarded A_Context
parse str = let pResult = runParser pContext "Sample.hs" str
            in case pResult of
               Errors  parseErr -> Errors parseErr
               Checked (pctx,_) -> pCtx2aCtx options pctx

checkResult :: ShowADL a => Guarded a -> (a -> Bool) -> Bool
checkResult guard check =
            case guard of
                Errors e   -> trace ("Cannot parse: " ++ show e) False
                Checked p  -> trace ("Parsed: " ++ showADL p) (check p)

prop_pretty :: A_Context -> Bool
prop_pretty xs = checkResult (parse $ showADL xs) (\p -> xs == p)

runTests :: IO ()
runTests = do quickCheck prop_pretty
              --- quickCheck prop_2
-- main = $quickCheckAll >>= print
