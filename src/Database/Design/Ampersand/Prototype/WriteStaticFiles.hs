{-# LANGUAGE CPP #-}
module Database.Design.Ampersand.Prototype.WriteStaticFiles (writeStaticFiles) where

import Control.Monad
import System.FilePath
import System.Directory
import qualified Data.ByteString.Char8 as BS
import Database.Design.Ampersand (Options(..),verboseLn)
import Prelude hiding (writeFile,readFile,getContents)
import Database.Design.Ampersand.Prototype.StaticFiles_Generated

writeStaticFiles :: Options -> IO()
writeStaticFiles opts =
  if genStaticFiles opts
  then sequence_ [ writeStaticFile opts sf 
                 | sf <- filter isRequired allStaticFiles
                 ]
  else verboseLn opts "Skipping static files (because of command line argument)"
 where isRequired :: StaticFile -> Bool
       isRequired sf = fileKind sf == ZwolleFrontEnd
writeStaticFile :: Options -> StaticFile -> IO()
writeStaticFile opts sf =
  do { createDirectoryIfMissing True (takeDirectory (absFilePath opts sf))
     ; write (absFilePath opts sf) (contentString sf)
     }
 where write a b = BS.writeFile a (BS.pack b)

absFilePath :: Options -> StaticFile -> FilePath
absFilePath opts sf = combine (dirPrototype opts) (filePath sf)
