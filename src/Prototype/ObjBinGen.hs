  module Prototype.ObjBinGen where
   import Directory
   --import Auxiliaries
   import Adl

   import CommonClasses
   import Data.Fspec
   import Prototype.ObjBinGenLocalsettings
   import Prototype.ObjBinGenConnectToDataBase
   import Prototype.ObjBinGenObject
   import Prototype.ObjBinGenObjectWrapper
   import Prototype.Installer

   phpObjServices :: Fspc    -- should take over from Context in due time.
                  -> String  -- the directory to which the result is written
                  -> Bool    -- a boolean that tells whether to generate services or compile services.
                  -> IO()
   phpObjServices fSpec
                  targetDir
                  servGen
     =   putStr ("\n---------------------------\nGenerating php Object files with ADL\n---------------------------")
      >> do { d <- doesDirectoryExist targetDir
            ; if d
              then putStr ""
              else createDirectory (targetDir) }
      >> putStr ("\n  Generating Install.php")
      >> writeFile (targetDir++"Install.php") ins
      >> putStr ("\n  Generating localsettings.inc.php")
      >> writeFile (targetDir++"localsettings.inc.php") ls
      >> putStr ("\n  Generating connectToDataBase.inc.php")
      >> writeFile (targetDir++"connectToDataBase.inc.php") ctdb
      >> putStr ("\nIncludable files for all objects:")
      >> sequence_
         [ putStr ("\n  Generating "++name o++".inc.php")
           >> writeFile (targetDir++name o++".inc.php") (ojs o)
         | o <- serviceObjects
         ]
      >> putStr ("\nWrapper files for all objects:")
      >> sequence_
         [ putStr ("\n  Generating "++name o++".php")
           >> writeFile (targetDir++name o++".php") (wrapper o)
         | o <- serviceObjects
         ]
      >> putStr ("\n\n")
      where
       ins  = installer fSpec dbName
       ls   = localsettings appname serviceObjects
       ctdb = connectToDataBase fSpec dbName
       wrapper o = objectWrapper (name o)
       ojs o = objectServices fSpec filename o
       serviceObjects = if servGen then serviceG fSpec else serviceS fSpec --serviceG->generated|serviceS->from ADL script
       FS_id appname =  (fsfsid fSpec)
       filename = appname
       dbName = appname
