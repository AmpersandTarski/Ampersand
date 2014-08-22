{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Database.Design.Ampersand_Prototype.ObjBinGen  (phpObjInterfaces) where

import Database.Design.Ampersand_Prototype.Installer           (installerDBstruct,installerDefPop,dumpPopulationToADL)
import Database.Design.Ampersand_Prototype.RelBinGenBasics     (addSlashes)
import Database.Design.Ampersand_Prototype.Apps
import Database.Design.Ampersand_Prototype.Generate            (generateAll)
import Control.Monad
import System.FilePath
import System.Directory
import qualified Data.ByteString as Bin
import Database.Design.Ampersand_Prototype.CoreImporter
import Prelude hiding (writeFile,readFile,getContents)

import Database.Design.Ampersand_Prototype.StaticFiles_Generated
#ifdef MIN_VERSION_MissingH
import System.Posix.Files  -- If MissingH is not available, we are on windows and cannot set file
import System.Time

import Data.Time.Clock.POSIX
#endif

phpObjInterfaces :: Fspc -> IO()
phpObjInterfaces fSpec =
 do { writeStaticFiles (flags fSpec)
    ; verboseLn (flags fSpec) "---------------------------"
    ; verboseLn (flags fSpec) "Generating php Object files with Ampersand"
    ; verboseLn (flags fSpec) "---------------------------"
    ; write "InstallerDBstruct.php"     (installerDBstruct fSpec)
--    ; write "InstallerTriggers.php"     (installerTriggers fSpec)
    ; write "InstallerDefPop.php"       (installerDefPop fSpec)
    ; write "DumpPopulationToADL.php"   (dumpPopulationToADL fSpec)

    ; let dbSettingsFilePath = combine targetDir "dbSettings.php"
    ; dbSettingsExists <- doesFileExist dbSettingsFilePath
    -- we generate a dbSettings.php only if it does not exist already.
    ; if dbSettingsExists
      then verboseLn (flags fSpec) "  Using existing dbSettings.php."
      else do { verboseLn (flags fSpec) "  Writing dbSettings.php."
              ; writeFile dbSettingsFilePath dbsettings
              }

    ; generateAll fSpec
    ; when (genAtlas (flags fSpec)) $ doGenAtlas fSpec
    ; verboseLn (flags fSpec) "\n"
    }
   where
    write fname content =
     do { verboseLn (flags fSpec) ("  Generating "++fname)
        ; writeFile (combine targetDir fname) content
        }
    dbsettings = unlines
       [ "<?php"
       , ""
       , "global $DB_host,$DB_user,$DB_pass;"
       , "$DB_host='"++addSlashes (sqlHost (flags fSpec))++"';"
       , "$DB_user='"++addSlashes (sqlLogin (flags fSpec))++"';"
       , "$DB_pass='"++addSlashes (sqlPwd (flags fSpec))++"';"
       , ""
       , "$DB_link=mysqli_connect($DB_host, $DB_user, $DB_pass)"
       , "      or exit(\"Error connecting to the database: username / password are probably incorrect.\");"
       , ""
       , "?>"
       ]
    targetDir = dirPrototype (flags fSpec)

doGenAtlas :: Fspc -> IO()
doGenAtlas fSpec =
 do { verboseLn (flags fSpec) "Installing the Atlas application:"
    ; verboseLn (flags fSpec) ("Importing "++show (importfile (flags fSpec))++" into namespace "++ show (namespace (flags fSpec)) ++" of the Atlas ...")
    ; verboseLn (flags fSpec) ("The atlas application should have been installed in " ++ show (dirPrototype (flags fSpec)) ++ ".")
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
writeWhenMissingOrOutdated opts staticFile act =
#ifdef MIN_VERSION_MissingH
 do { exists <- doesFileExist $ absFilePath opts staticFile
    ; if exists then
       do { oldTimeStamp <- getModificationTime $ absFilePath opts staticFile
          ; if oldTimeStamp < timeStamp staticFile then
             do { verboseLn opts $ "  Replacing static file "++ filePath staticFile ++" with current version."
                ; act
                }
            else
              return () -- skip is not really worth logging
          }
      else
       do { verboseLn opts $ "  Writing static file "++ filePath staticFile
          ; act
          }
    }
#else
-- On windows we cannot set the file modification time without requiring a cygwin or mingw build environment,
-- so we simply replace all static files on each generation.
 do { verboseLn opts $ "  Writing static file "++ filePath staticFile
    ; act
    }
#endif

writeStaticFile :: Options -> StaticFile -> IO()
writeStaticFile opts sf =
  do { createDirectoryIfMissing True (takeDirectory (absFilePath opts sf))
     ; write (absFilePath opts sf) (contentString sf)
#ifdef MIN_VERSION_MissingH
     ; let t = (fromIntegral . fromEnum . utcTimeToPOSIXSeconds) (timeStamp sf)
     ; setFileTimes (absFilePath opts sf) t t
#endif
     }
 where write a b = if isBinary sf
                   then Bin.writeFile a (read b)
                   else writeFile a b

absFilePath :: Options -> StaticFile -> FilePath
absFilePath opts sf = combine (dirPrototype opts) (filePath sf)
