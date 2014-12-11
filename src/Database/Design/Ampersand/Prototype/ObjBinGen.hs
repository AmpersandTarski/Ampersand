{-# LANGUAGE CPP #-}
module Database.Design.Ampersand.Prototype.ObjBinGen  (phpObjInterfaces) where

import Database.Design.Ampersand.Prototype.Installer           (installerDBstruct,installerDefPop,dumpPopulationToADL)
import Database.Design.Ampersand.Prototype.RelBinGenBasics     (addSlashes)
import Database.Design.Ampersand.Prototype.Apps
import Database.Design.Ampersand.Prototype.Generate            (generateAll)
import Control.Monad
import System.FilePath
import System.Directory
import qualified Data.ByteString as Bin
import Database.Design.Ampersand.Prototype.CoreImporter
import Prelude hiding (writeFile,readFile,getContents)

import Database.Design.Ampersand.Prototype.StaticFiles_Generated
#ifdef MIN_VERSION_MissingH
import System.Posix.Files  -- If MissingH is not available, we are on windows and cannot set file
import Data.Time.Format
import System.Locale
#endif

phpObjInterfaces :: FSpec -> IO()
phpObjInterfaces fSpec =
 do { writeStaticFiles (getOpts fSpec)
    ; verboseLn (getOpts fSpec) "---------------------------"
    ; verboseLn (getOpts fSpec) "Generating php Object files with Ampersand"
    ; verboseLn (getOpts fSpec) "---------------------------"
    ; write "InstallerDBstruct.php"     (installerDBstruct fSpec)
--    ; write "InstallerTriggers.php"     (installerTriggers fSpec)
    ; write "InstallerDefPop.php"       (installerDefPop fSpec)
    ; write "DumpPopulationToADL.php"   (dumpPopulationToADL fSpec)

    ; let dbSettingsFilePath = combine targetDir "dbSettings.php"
    ; dbSettingsExists <- doesFileExist dbSettingsFilePath
    -- we generate a dbSettings.php only if it does not exist already.
    ; if dbSettingsExists
      then verboseLn (getOpts fSpec) "  Using existing dbSettings.php."
      else do { verboseLn (getOpts fSpec) "  Writing dbSettings.php."
              ; writeFile dbSettingsFilePath dbsettings
              }

    ; generateAll fSpec
    ; when (genAtlas (getOpts fSpec)) $ doGenAtlas fSpec
    ; verboseLn (getOpts fSpec) "\n"
    }
   where
    write fname content =
     do { verboseLn (getOpts fSpec) ("  Generating "++fname)
        ; writeFile (combine targetDir fname) content
        }
    dbsettings = unlines
       [ "<?php"
       , ""
       , "global $DB_host,$DB_user,$DB_pass;"
       , "$DB_host='"++addSlashes (sqlHost (getOpts fSpec))++"';"
       , "$DB_user='"++addSlashes (sqlLogin (getOpts fSpec))++"';"
       , "$DB_pass='"++addSlashes (sqlPwd (getOpts fSpec))++"';"
       , ""
       , "$DB_link=mysqli_connect($DB_host, $DB_user, $DB_pass)"
       , "      or exit(\"Error connecting to the database: username / password are probably incorrect.\");"
       , ""
       , "?>"
       ]
    targetDir = dirPrototype (getOpts fSpec)

doGenAtlas :: FSpec -> IO()
doGenAtlas fSpec =
 do { verboseLn (getOpts fSpec) "Installing the Atlas application:"
    ; verboseLn (getOpts fSpec) ("Importing "++show (importfile (getOpts fSpec))++" into namespace "++ show (namespace (getOpts fSpec)) ++" of the Atlas ...")
    ; verboseLn (getOpts fSpec) ("The atlas application should have been installed in " ++ show (dirPrototype (getOpts fSpec)) ++ ".")
    ; fillAtlas fSpec
    }

writeStaticFiles :: Options -> IO()
writeStaticFiles opts =
  if genStaticFiles opts
  then
 do {
#ifdef MIN_VERSION_MissingH
      verboseLn opts "Updating static files"
#else
      verboseLn opts "Writing static files"
#endif
    ; sequence_ [ writeWhenMissingOrOutdated opts sf (writeStaticFile opts sf) | sf <- allStaticFiles ]
    }
  else
      verboseLn opts "Skipping static files (because of command line argument)"

writeWhenMissingOrOutdated :: Options -> StaticFile -> IO () -> IO ()
writeWhenMissingOrOutdated opts staticFile writeFileAction =
#ifdef MIN_VERSION_MissingH
-- On Mac/Linux we set the modification time for generated static files to the modification time of the compiled versions
-- in StaticFiles_Generated.hs. This allows us to only replace those static files that are outdated (or missing.) 
 do { exists <- doesFileExist $ absFilePath opts staticFile
    ; if exists then
       do { oldTimeStampUTC <- getModificationTime $ absFilePath opts staticFile
          ; let oldTimeStamp = read $ formatTime defaultTimeLocale "%s" oldTimeStampUTC -- convert to epoch seconds
          ; if oldTimeStamp < timeStamp staticFile then
              do { verboseLn opts $ "  Replacing static file "++ filePath staticFile ++" with current version."
                 ; writeFileAction
                 }
            else
              return () -- skip is not really worth logging
          }
      else
       do { verboseLn opts $ "  Writing static file "++ filePath staticFile
          ; writeFileAction
          }
    }
#else
-- On windows we cannot set the file modification time without requiring a cygwin or mingw build environment,
-- so we simply replace all static files on each generation.
 do { verboseLn opts $ "  Writing static file "++ filePath staticFile
    ; writeFileAction
    }
#endif

writeStaticFile :: Options -> StaticFile -> IO()
writeStaticFile opts sf =
  do { createDirectoryIfMissing True (takeDirectory (absFilePath opts sf))
     ; write (absFilePath opts sf) (contentString sf)
#ifdef MIN_VERSION_MissingH
     ; let t = (fromIntegral $ timeStamp sf)
     ; setFileTimes (absFilePath opts sf) t t
#endif
     }
 where write a b = if isBinary sf
                   then Bin.writeFile a (read b)
                   else writeFile a b

absFilePath :: Options -> StaticFile -> FilePath
absFilePath opts sf = combine (dirPrototype opts) (filePath sf)
