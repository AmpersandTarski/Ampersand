{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Database.Design.Ampersand.Test.Parser.RunTests (runTests) where

import Database.Design.Ampersand.Test.Parser.ParseScripts
import Database.Design.Ampersand.Test.Parser.QuickChecks
import Database.Design.Ampersand.Test.Parser.ArbitraryTree

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

runTests :: IO ()
runTests = do scr <- scripts
              success <- testScripts scr
              if success then parserQuickChecks
              else return ()

-- main = $quickCheckAll >>= print
