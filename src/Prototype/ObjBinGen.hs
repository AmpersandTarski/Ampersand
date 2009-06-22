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

   phpObjServices :: Fspc    -- should take over from Context in due time.
                  -> String  -- the directory to which the result is written
                  -> IO()
   phpObjServices fSpec
                  targetDir
     =   putStr ("\n---------------------------\nGenerating php Object files with ADL\n---------------------------")
      >> putStr ("\n  Generating localsettings.inc.php")
      >> do { d <- doesDirectoryExist targetDir
            ; if d
              then putStr ""
              else createDirectory (targetDir) }
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
       ls   = localsettings appname serviceObjects
       ctdb = connectToDataBase fSpec dbName
       wrapper o = objectWrapper (name o)
       ojs o = objectServices fSpec filename o
       serviceObjects = serviceG fSpec
       FS_id appname =  (fsfsid fSpec)
       filename = appname
       dbName = appname
