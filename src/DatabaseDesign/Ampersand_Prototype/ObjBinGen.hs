{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.ObjBinGen  (phpObjInterfaces)
  where
 
   import DatabaseDesign.Ampersand_Prototype.ConnectToDataBase   (connectToDataBase)
   import DatabaseDesign.Ampersand_Prototype.Object              (objectInterfaces)
   import DatabaseDesign.Ampersand_Prototype.Wrapper             (objectWrapper)
   import DatabaseDesign.Ampersand_Prototype.Installer           (installer)
   import DatabaseDesign.Ampersand_Prototype.InterfaceDef        (interfaceDef)
   import DatabaseDesign.Ampersand_Prototype.Index               (htmlindex)
   import DatabaseDesign.Ampersand_Prototype.RelBinGenBasics     (addSlashes)
   import DatabaseDesign.Ampersand_Prototype.ContextGen          (contextGen)
   import DatabaseDesign.Ampersand_Prototype.Apps
   import System.FilePath               
   import System.Directory
   import qualified Data.ByteString as Bin
   import DatabaseDesign.Ampersand_Prototype.CoreImporter  
   import Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)
   
   import DatabaseDesign.Ampersand_Prototype.StaticFiles_Generated

   phpObjInterfaces :: Fspc -> Options -> IO()
   phpObjInterfaces fSpec flags
     = writeStaticFiles flags
      >> verboseLn flags "---------------------------"
      >> verboseLn flags "Generating php Object files with Ampersand"
      >> verboseLn flags "---------------------------"
      >> write "index.htm"                 (htmlindex fSpec ifcs flags)
      >> write "Installer.php"             (installer fSpec flags)
      >> write (name fSpec++".php")        (contextGen fSpec)
      >> write "interfaceDef.inc.php"      (interfaceDef fSpec ifcs flags)
      >> write "connectToDataBase.inc.php" (connectToDataBase fSpec flags)
      >> verboseLn flags ("  Writing: dbsettings.php")
      >> writeFile (combine targetDir "dbsettings.php") dbsettings
      >> verboseLn flags ("Includable files for all objects:")
      >> sequence_
         [ write (addExtension (name ifc) ".inc.php") (objectInterfaces flags fSpec (ifcObj ifc))
         | ifc <- ifcs
         ]
      >> verboseLn flags ("Wrapper files for all objects:")
      >> sequence_
         [ write (addExtension (name ifc) ".php") (objectWrapper fSpec ifcs ifc flags)
         | ifc <- ifcs
         ]
      >> sequence_  [ doGenAtlas    fSpec flags | genAtlas     flags] 
      >> verboseLn flags ("\n")
      where
       write fname content
          =   verboseLn flags ("  Generating "++fname)
           >> writeFile (combine targetDir fname) content
       dbsettings = "<?php $DB_link=mysql_connect("
                    ++  "$DB_host='"++addSlashes (sqlHost flags)++"'"
                    ++", $DB_user='"++addSlashes (sqlLogin flags)++"'"
                    ++", $DB_pass='"++addSlashes (sqlPwd flags)++"'"
                    ++") or exit(\"Username / password are probably incorrect.\"); $DB_debug = 3; ?>"
       targetDir = dirPrototype flags
       ifcs = interfaceS fSpec++ interfaceG fSpec

   doGenAtlas :: Fspc -> Options -> IO()
   doGenAtlas fSpec flags =
        verboseLn flags ("Installing the Atlas application:")
     >> verboseLn flags ("Importing "++show (importfile flags)++" into namespace "++ show (namespace flags) ++" of the Atlas ...")
     >> verboseLn flags ("The atlas application should have been installed in " ++ show (dirPrototype flags) ++ ".")
     >> fillAtlas fSpec flags
                   
                   
   writeStaticFiles :: Options -> IO()
   writeStaticFiles flags =  sequence_ [ writeWhenMissingOrOutdated flags sf (writeStaticFile flags sf)
                                       | sf <- allStaticFiles 
                                       ]

   writeWhenMissingOrOutdated :: Options -> StaticFile -> IO () -> IO ()
   writeWhenMissingOrOutdated flags staticFile act =
    do { exists <- doesFileExist $ absFilePath flags staticFile 
       ; if exists then
          do { oldTimeStamp <- getModificationTime $ absFilePath flags staticFile
             ; if oldTimeStamp < timeStamp staticFile then
                do { verboseLn flags $ "  Replacing static file "++ absFilePath flags staticFile ++" with current version."
                   ; act
                   }
               else
                 return () -- skip is not really worth logging
             }
         else
          do { verboseLn flags $ "  Writing static file "++ absFilePath flags staticFile
             ; act
             }
       }       
                                       
   writeStaticFile :: Options -> StaticFile -> IO()
   writeStaticFile flags sf = 
     do { createDirectoryIfMissing True (takeDirectory (absFilePath flags sf))
        ; write (absFilePath flags sf) (contentString sf) 
        }
    where write a b = case isBinary sf of
                        True  -> Bin.writeFile a (toBin b)
                        False ->     writeFile a b
          toBin :: String -> Bin.ByteString
          toBin x = read x

   absFilePath :: Options -> StaticFile -> FilePath
   absFilePath flags sf = combine (dirPrototype flags) (filePath sf)
   