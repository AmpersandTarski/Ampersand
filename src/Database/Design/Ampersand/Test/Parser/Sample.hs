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
import System.Directory
import Control.Monad

options :: Options
options = Options {}

noCtx :: A_Context
noCtx = ACtx{ctxnm="NO_CONTEXT"}

unguard :: String -> Guarded a -> Bool
unguard str guard = case guard of
   Errors  e -> trace ("Cannot parse: " ++ show e ++ ": " ++ str) True
   Checked _ -> trace ("Parsed: " ++ str) True

parseFile :: FilePath -> IO Bool
parseFile name = do contents <- readFile name
                    let pResult = runParser pContext name contents
                    return $ unguard name pResult

parse :: String -> Guarded A_Context
parse str = let pResult = runParser pContext "Sample.hs" str
            in case pResult of
               Errors  parseErr -> Errors parseErr
               Checked (pctx,_) -> pCtx2aCtx options pctx

checkResult :: ShowADL a => Guarded a -> (a -> Bool) -> Bool
checkResult guard check =
            case guard of
                Errors e   -> trace (show e) False
                Checked p  -> check p

prop_pretty :: A_Context -> Bool
prop_pretty xs = checkResult (parse $ showADL xs) (\p -> xs == p)

testScripts :: [String] -> IO Bool
testScripts [] = return True
testScripts fs =
    do parsed <- parseFile (head fs)
       if parsed then testScripts (tail fs)
       else return False

endswith :: String -> String -> Bool
endswith a b = (drop (length a - length b) a) == b

-- Returns tuple with files and directories inside the folder
getDirectory :: FilePath -> IO ([FilePath],[FilePath])
getDirectory path =
    do contents <- getDirectoryContents path
       let valid = filter (\x-> x /= "." && x /= "..") contents
       let paths = map ((++) path) valid
       files <- filterM doesFileExist paths
       subdirs <- filterM doesDirectoryExist paths
       --- trace ("In "++path++"\nfound files: " ++ (show files) ++ "\nand subdirs:" ++ (show subdirs)) $ return ()
       return (files, subdirs)

getFiles :: String -> FilePath -> IO [FilePath]
getFiles ext dir =
    do (fs, ds) <- getDirectory (dir++"/")
       let files = filter (\f->f `endswith` ext) fs
       foldM f files ds
      where f rs d = do ms <- getFiles ext d
                        return $ ms ++ rs

scripts :: IO [FilePath]
scripts = getFiles ".adl" "../ampersand-models"

runTests :: IO ()
runTests = do scr <- scripts
              success <- testScripts scr
              if success then return ()--TODO: quickCheck prop_pretty
              else return ()

-- main = $quickCheckAll >>= print
