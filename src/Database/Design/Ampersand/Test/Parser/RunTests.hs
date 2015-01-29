{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Database.Design.Ampersand.Test.Parser.RunTests (runTests) where

import Database.Design.Ampersand.Misc.Options
import Database.Design.Ampersand.Test.Parser.ParseScripts
import Database.Design.Ampersand.Test.Parser.QuickChecks

runTests :: Options -> IO ()
runTests _   = do scr <- scripts
                  success <- testScripts scr
                  if success then parserQuickChecks
                  else return ()
                  return ()

-- main = $quickCheckAll >>= print
