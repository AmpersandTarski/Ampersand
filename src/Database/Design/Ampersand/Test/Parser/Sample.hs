{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Database.Design.Ampersand.Test.Parser.Sample (runTests) where

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

options :: Options
options = Options {}

noCtx :: A_Context
noCtx = ACtx{ctxnm="NO_CONTEXT"}

unguard :: String -> Guarded (P_Context, [String]) -> Bool
unguard str guard = case guard of
   Errors  parseErr -> trace ("Cannot parse: " ++ (show parseErr) ++ ": " ++ str) False
   Checked (_,_)    -> trace (str ++ " parsed succesfully") True

parseFile :: FilePath -> IO Bool
parseFile name = do contents <- readFile name
                    let pResult = runParser pContext name contents
                    return $ unguard name pResult

parse :: String -> Guarded A_Context
parse str = let pResult = runParser pContext "Sample.hs" str
            in case pResult of
               Errors  parseErr -> trace ("Cannot parse " ++ (show parseErr) ++ ": " ++ str) $ Errors parseErr
               Checked (pctx,_) -> pCtx2aCtx options pctx

checkResult :: ShowADL a => Guarded a -> (a -> Bool) -> Bool
checkResult guard check =
            case guard of
                Errors e   -> trace ("Cannot parse: " ++ show e) False
                Checked p  -> trace ("Parsed: " ++ showADL p) (check p)

prop_pretty :: A_Context -> Bool
prop_pretty xs = checkResult (parse $ showADL xs) (\p -> xs == p)

testScripts :: [String] -> IO Bool
testScripts [] = return True
testScripts fs = do
    parsed <- parseFile (head fs)
    if parsed then testScripts (tail fs)
    else return False

scripts :: [FilePath]
scripts = [ "../ampersand-models/Misc/Arbeidsduur.adl"
          , "../ampersand-models/Misc/ArchiTest1.adl"
          , "../ampersand-models/Misc/ArchiTest3.adl"
          , "../ampersand-models/Misc/ArchiTest4.adl"
          , "../ampersand-models/Misc/ArchiTest5.adl"
          , "../ampersand-models/Misc/ARM-Test1.adl"
          , "../ampersand-models/Misc/ARM-Test2.adl"
          , "../ampersand-models/Misc/ARM20-Test1.adl"
          , "../ampersand-models/Misc/ARM20-Test2.adl"
          , "../ampersand-models/Misc/ARM20-Test3.adl"
          , "../ampersand-models/Misc/ARM20-Test4.adl"
          , "../ampersand-models/Misc/ARM20-Test5.adl"
          , "../ampersand-models/Misc/ARM20-Test6.adl"
          , "../ampersand-models/Misc/ARM20-Test7.adl"
          , "../ampersand-models/Misc/ARM20-Test8.adl"
          , "../ampersand-models/Misc/FraakTest1.adl"
          , "../ampersand-models/Misc/FraakTest2.adl"
          , "../ampersand-models/Misc/Hello.adl"
          , "../ampersand-models/Misc/Kernmodel.adl"]

runTests :: IO ()
runTests = do success <- testScripts scripts
              if success then quickCheck prop_pretty
              else return ()
              --- quickCheck prop_2
-- main = $quickCheckAll >>= print
