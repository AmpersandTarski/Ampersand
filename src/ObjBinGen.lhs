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
>    = putStr ("\nGenerating nothing\n")