> module ObjBinGen where
>  import Auxiliaries
>  import CC_aux
>  import CommonClasses
>  import ObjBinGenLocalsettings
>  import ObjBinGenConnectToDataBase
>  import ObjBinGenObject
>  import ObjBinGenObjectWrapper

>  phpObjServices contexts
>                 contextname
>                 filename
>                 dbName
>                 targetDir
>    =   putStr ("\n---------------------------\nGenerating php Object files with ADL version "++adlVersion++"\n---------------------------")
>     >> putStr ("\n  Generating localsettings.inc.php")
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
>      context = head ([{-recalc-} c| c<-contexts, name c==contextname]++
>                      [Ctx (contextname++" is not defined") [] empty [] [] [] [] [] [] []])
>      ls   = localsettings context dbName
>      ctdb = connectToDataBase context dbName
>      wrapper o = objectWrapper (name o)
>      ojs o = objectServices context filename o