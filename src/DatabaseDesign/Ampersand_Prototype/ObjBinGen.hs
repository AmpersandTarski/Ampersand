{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.ObjBinGen  (phpObjInterfaces) where
 
import DatabaseDesign.Ampersand_Prototype.ConnectToDataBase   (connectToDataBase)
import DatabaseDesign.Ampersand_Prototype.Object              (objectInterfaces)
import DatabaseDesign.Ampersand_Prototype.Wrapper             (objectWrapper)
import DatabaseDesign.Ampersand_Prototype.Installer           (installer)
import DatabaseDesign.Ampersand_Prototype.InterfaceDef        (interfaceDef)
import DatabaseDesign.Ampersand_Prototype.Index               (htmlindex)
import DatabaseDesign.Ampersand_Prototype.RelBinGenBasics     (addSlashes)
import DatabaseDesign.Ampersand_Prototype.ContextGen          (contextGen)
import DatabaseDesign.Ampersand_Prototype.Apps
import DatabaseDesign.Ampersand_Prototype.Generate            (generateAll)
import Data.Maybe
import Control.Monad
import System.FilePath               
import System.Directory
import qualified Data.ByteString as Bin
import DatabaseDesign.Ampersand_Prototype.CoreImporter  
import Prelude hiding (writeFile,readFile,getContents)

import DatabaseDesign.Ampersand_Prototype.StaticFiles_Generated
#ifdef MIN_VERSION_MissingH 
import System.Posix.Files  -- If MissingH is not available, we're on windows and cannot set file 
import System.Time.Utils   -- modification time.
#endif

phpObjInterfaces :: Fspc -> Options -> IO()
phpObjInterfaces fSpec opts =
 do { writeStaticFiles opts
    ; verboseLn opts "---------------------------"
    ; verboseLn opts "Generating php Object files with Ampersand"
    ; verboseLn opts "---------------------------"
    ; write "Installer.php"             (installer fSpec opts)
    
    ; let dbSettingsFilePath = combine targetDir "dbSettings.php"
    ; dbSettingsExists <- doesFileExist dbSettingsFilePath
    -- we generate a dbSettings.php if it doesn't exists, or if a host, login, or password has been specified
    ; if not dbSettingsExists ||  any (isJust) [sqlHost opts, sqlLogin opts, sqlPwd opts]
      then do { verboseLn opts $ "  Writing dbSettings.php."
              ; writeFile dbSettingsFilePath dbsettings
              }
      else verboseLn opts $ "  Using existing dbSettings.php."

    ; if not $ deprecated opts then
       do { generateAll fSpec opts
          }
      else
       do { putStrLn "\nWARNING: Using old php generator because of --deprecated option."
          ; putStrLn "  This generator has known bugs and is no longer being" 
          ; putStrLn "  maintained. Its use is strongly discouraged!\n"
          ; write "connectToDataBase.inc.php" (connectToDataBase fSpec opts)
          ; write "index.htm"                 (htmlindex fSpec ifcs opts)
          ; write (name fSpec++".php")        (contextGen fSpec)
          ; write "interfaceDef.inc.php"      (interfaceDef fSpec ifcs opts)
          ; verboseLn opts "Includable files for all objects:"
          ; sequence_
              [ write (addExtension (name ifc) ".inc.php") (objectInterfaces opts fSpec (ifcObj ifc))
              | ifc <- ifcs
              ]
          ; verboseLn opts "Wrapper files for all objects:"
          ; sequence_
              [ write (addExtension (name ifc) ".php") (objectWrapper fSpec ifcs ifc opts)
              | ifc <- ifcs
              ]
          }
          
    ; when (genAtlas opts) $ doGenAtlas fSpec opts
    ; verboseLn opts "\n"
    }
   where
    write fname content =
     do { verboseLn opts ("  Generating "++fname)
        ; writeFile (combine targetDir fname) content
        }
    dbsettings = "<?php $DB_link=mysql_connect("
                 ++  "$DB_host='"++addSlashes (fromMaybe "localhost" $ sqlHost opts)++"'"
                 ++", $DB_user='"++addSlashes (fromMaybe "root" $ sqlLogin opts)++"'"
                 ++", $DB_pass='"++addSlashes (fromMaybe "" $ sqlPwd opts)++"'"
                 ++") or exit(\"Username / password are probably incorrect.\"); $DB_debug = 3; ?>"
    targetDir = dirPrototype opts
    ifcs = interfaceS fSpec++ interfaceG fSpec

doGenAtlas :: Fspc -> Options -> IO()
doGenAtlas fSpec opts =
 do { verboseLn opts ("Installing the Atlas application:")
    ; verboseLn opts ("Importing "++show (importfile opts)++" into namespace "++ show (namespace opts) ++" of the Atlas ...")
    ; verboseLn opts ("The atlas application should have been installed in " ++ show (dirPrototype opts) ++ ".")
    ; fillAtlas fSpec opts
    }             
                
writeStaticFiles :: Options -> IO()
writeStaticFiles opts =  
 do {
#ifdef MIN_VERSION_MissingH 
      verboseLn opts $ "Updating static files"
#else
      verboseLn opts $ "Writing static files"
#endif
    ; sequence_ [ writeWhenMissingOrOutdated opts sf (writeStaticFile opts sf) | sf <- allStaticFiles ]
    }
    
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
     ; let t = clockTimeToEpoch (timeStamp sf)
     ; setFileTimes (absFilePath opts sf) t t
#endif
     }
 where write a b = case isBinary sf of
                     True  -> Bin.writeFile a (toBin b)
                     False ->     writeFile a b
       toBin :: String -> Bin.ByteString
       toBin x = read x

absFilePath :: Options -> StaticFile -> FilePath
absFilePath opts sf = combine (dirPrototype opts) (filePath sf)
