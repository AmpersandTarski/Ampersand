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

   phpObjServices :: Context -- should become obsolete, as soon as fSpec takes over...
                  -> Fspc    -- should take over from Context in due time.
                  -> String  -- the file name to which these services are written
                  -> String  -- the database name
                  -> String  -- the directory to which the result is written
                  -> Bool    -- a boolean that tells whether to generate services or compile services.
                  -> IO()
   phpObjServices context
                  fSpec
                  filename
                  dbName
                  targetDir
                  servGen
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
       ls   = let FS_id appname =  (fsfsid fSpec)
              in localsettings appname serviceObjects
       ctdb = connectToDataBase fSpec dbName
       wrapper o = objectWrapper (name o)
       ojs o = objectServices fSpec filename o
       serviceObjects = if servGen then serviceG fSpec else attributes context
