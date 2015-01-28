{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Database.Design.Ampersand.Test.Parser.ParseScripts (testScripts, scripts) where

import Database.Design.Ampersand.Test.Parser.ParserTest
import System.Directory
import Control.Monad

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
       foldM recursive files ds
      where recursive rs d =
                do ms <- getFiles ext d
                   return $ ms ++ rs

scripts :: IO [FilePath]
scripts = getFiles ".adl" "../ampersand-models"
