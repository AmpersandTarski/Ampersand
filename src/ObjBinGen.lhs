> module ObjBinGen where
>  import Directory
>  import Auxiliaries
>  import CC_aux
>  import CommonClasses
>  import ObjBinGenLocalsettings
>  import ObjBinGenConnectToDataBase
>  import ObjBinGenObject
>  import ObjBinGenObjectWrapper

This module is the controller, which creates the architectural framework for the generated code.
It follows the well known Model-View-Control pattern, in which the Compliant Services Layer (CSL) acts as model.
The foundation for all views is laid by the component "localsettings.inc.php",
supplemented by one component for every view (called <view name>.php).
The foudation for the model (the CSL) is the component "connectToDataBase.inc.php",
which creates a consistent CSL.
It is supplemented by a set of services for each view, each placed in a component called <view name>.inc.php

>  phpObjServices contexts
>                 contextname
>                 filename
>                 dbName
>                 targetDir
>    =   putStr ("\n---------------------------\nGenerating php Object files with ADL version "++adlVersion++"\n---------------------------")
>     >> putStr ("\n  Generating localsettings.inc.php")
>     >> do { d <- doesDirectoryExist targetDir
>           ; if d
>             then putStr ""
>             else createDirectory (targetDir) }
>     >> writeFile (targetDir++"localsettings.inc.php") ls
>     >> putStr ("\n  Generating connectToDataBase.inc.php")
>     >> writeFile (targetDir++"connectToDataBase.inc.php") ctdb
>     >> putStr ("\nIncludable files for all objects:")
>     >> sequence_
>        [ putStr ("\n  Generating "++(name o)++".inc.php")
>          >> writeFile (targetDir++(name o)++".inc.php") (ojs o)
>        | o <- attributes context
>        ]
>     >> putStr ("\nWrapper files for all objects:")
>     >> sequence_
>        [ putStr ("\n  Generating "++(name o)++".php")
>          >> writeFile (targetDir++(name o)++".php") (wrapper o)
>        | o <- attributes context
>        ]
>     >> putStr ("\n\n")
>     where
>      context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
>      ctxs    = [c| c<-contexts, name c==contextname]
>      ls   = localsettings context dbName
>      ctdb = connectToDataBase context dbName
>      wrapper o = objectWrapper (name o)
>      ojs o = objectServices context filename o