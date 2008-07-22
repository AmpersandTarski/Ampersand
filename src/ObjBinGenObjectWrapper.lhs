> module ObjBinGenObjectWrapper where
>  import Char
>  import Auxiliaries
>  import Calc(informalRule, shrink, disjNF, computeOrder, ComputeRule, triggers)
>  import CC_aux
>  import CommonClasses
>  import ERmodel
>  import PredLogic -- (for error messages by dbCorrect)
>  import Hatml     -- (for converting error messages to HTML)
>  import Atlas     -- (for converting error messages to HTML)
>  import RelBinGenBasics
> 

>  objectWrapper objectName
>   = (chain "\n  "
>     ([ "<?php // generated with "++adlVersion
>      , ""
>      , "require \"localsettings.inc.php\";"
>      , "require \""++objectName++".inc.php\";"
>      , ""
>      , "$view = new view(parseRequest(getObject_"++objectName++"()),getObject_"++objectName++"());"
>      , "$view->display();"
>      , ""
>      ]
>     )) ++ "?>"