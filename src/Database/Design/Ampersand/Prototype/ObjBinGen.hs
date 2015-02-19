{-# LANGUAGE CPP #-}
module Database.Design.Ampersand.Prototype.ObjBinGen  (phpObjInterfaces) where

import Database.Design.Ampersand.Prototype.Installer           (installerDBstruct,installerDefPop)
import Database.Design.Ampersand.Prototype.ProtoUtil
import Database.Design.Ampersand.Prototype.Apps
import Database.Design.Ampersand.Prototype.Generate            (generateAll)
import Control.Monad
import System.FilePath
import System.Directory
import qualified Data.ByteString.Char8 as BS
import Database.Design.Ampersand.Prototype.CoreImporter
import Prelude hiding (writeFile,readFile,getContents)
import Database.Design.Ampersand.Prototype.StaticFiles_Generated

phpObjInterfaces :: FSpec -> IO()
phpObjInterfaces fSpec =
 do { writeStaticFiles (getOpts fSpec)
    ; verboseLn (getOpts fSpec) "---------------------------"
    ; verboseLn (getOpts fSpec) "Generating php Object files with Ampersand"
    ; verboseLn (getOpts fSpec) "---------------------------"
    ; writePrototypeFile fSpec "InstallerDBstruct.php"     (installerDBstruct fSpec)
--    ; writePrototypeFile fSpec "InstallerTriggers.php"     (installerTriggers fSpec)
    ; writePrototypeFile fSpec "InstallerDefPop.php"       (installerDefPop fSpec)

    ; let dbSettingsFilePath = getGenericsDir fSpec </> "dbSettings.php"
    ; dbSettingsExists <- doesFileExist dbSettingsFilePath
    -- we generate a dbSettings.php only if it does not exist already.
    ; if dbSettingsExists
      then verboseLn (getOpts fSpec) "  Using existing dbSettings.php."
      else do { verboseLn (getOpts fSpec) "  Writing dbSettings.php."
              ; writePrototypeFile fSpec dbSettingsFilePath dbsettings
              }

    ; generateAll fSpec
    ; when (genAtlas (getOpts fSpec)) $ doGenAtlas fSpec
    ; verboseLn (getOpts fSpec) "\n"
    }
   where
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
  then sequence_ [ writeStaticFile opts sf 
                 | sf@SF{isNewFrontend=isNew} <- allStaticFiles, isNew == newFrontend opts
                 ]
  else verboseLn opts "Skipping static files (because of command line argument)"

writeStaticFile :: Options -> StaticFile -> IO()
writeStaticFile opts sf =
  do { createDirectoryIfMissing True (takeDirectory (absFilePath opts sf))
     ; write (absFilePath opts sf) (contentString sf)
     }
 where write a b = BS.writeFile a (BS.pack b)

absFilePath :: Options -> StaticFile -> FilePath
absFilePath opts sf = combine (dirPrototype opts) (filePath sf)
