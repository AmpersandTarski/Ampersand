{-# LANGUAGE CPP #-}
module Database.Design.Ampersand.Prototype.ObjBinGen (generatePhp, writeStaticFiles) where

import Database.Design.Ampersand.Prototype.Installer           (installerDBstruct,installerDefPop)
import Database.Design.Ampersand.Prototype.ProtoUtil
import Control.Monad
import System.FilePath
import System.Directory
import qualified Data.ByteString.Char8 as BS
import Database.Design.Ampersand
import Prelude hiding (writeFile,readFile,getContents)
import Database.Design.Ampersand.Prototype.StaticFiles_Generated
import Database.Design.Ampersand.Prototype.PHP

generatePhp :: FSpec -> IO()
generatePhp fSpec =
 do { verboseLn (getOpts fSpec) "---------------------------"
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
    }
   where
    dbsettings = unlines $
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
       ]++setSqlModePHP++
       [ "?>"
       ]

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
