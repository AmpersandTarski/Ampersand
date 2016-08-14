{-# LANGUAGE CPP #-}
module Ampersand.Prototype.WriteStaticFiles (writeStaticFiles) where

import System.FilePath
import System.Directory
import qualified Data.ByteString.Char8 as BS
import Ampersand.Misc (Options(..),verboseLn)
import Prelude hiding (writeFile,readFile,getContents)
import Ampersand.Prototype.StaticFiles_Generated
import qualified Data.Conduit.List as CL
import Conduit

writeStaticFiles :: Options -> IO()
writeStaticFiles opts =
  if genStaticFiles opts
  then runResourceT $ source $$ sink 
  else verboseLn opts "Skipping static files (because of command line argument)"
 where
    source :: Source (ResourceT IO) StaticFile
    source = yieldMany $ filter isRequired allStaticFiles
    sink :: Sink StaticFile (ResourceT IO) ()
    sink = CL.mapM_ writeStaticFile

    writeStaticFile :: StaticFile -> (ResourceT IO) ()
    writeStaticFile sf = 
       do { lift $ createDirectoryIfMissing True (takeDirectory absFilePath)
          ; lift $ write absFilePath (contentString sf)
          }
      where write a b = BS.writeFile a (BS.pack b)
            absFilePath :: FilePath
            absFilePath = combine (dirPrototype opts) (filePath sf)
    isRequired :: StaticFile -> Bool
    isRequired sf = fileKind sf == ZwolleFrontEnd



