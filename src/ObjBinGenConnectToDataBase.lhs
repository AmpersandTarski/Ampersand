> module ObjBinGenConnectToDataBase where
>  import Char
>  import Auxiliaries
>  import Calc(informalRule, shrink, disjNF, computeOrder, ComputeRule, triggers)
>  import CC_aux
>  import CommonClasses
>  import ERmodel
>  import PredLogic -- (for error messages by dbCorrect)
>  import Hatml     -- (for converting error messages to HTML)
>  import Atlas     -- (for converting error messages to HTML)
>--  import RelBinGenBasics
>  import RelBinGenServiceLayer (dbError, ruleFunctions, createAndSelectDB)
> 

>  connectToDataBase context dbName
>   = (chain "\n  " 
>     ([ "<?php // generated with "++adlVersion
>      , "$DB_link = @mysql_connect($DB_host,$DB_user,$DB_pass) or die('Could not connect to MySql.');"
>      , "$DB_slct = mysql_select_db("++dbName++",$DB_link);"
>      , ""
>      ] ++ (ruleFunctions context)
>       ++
>      (createAndSelectDB context dbName False) ++
>      [""
>      , "if($DB_debug>=3){"
>      ] ++
>         [ "  checkRule"++show (nr r)++"();"
>         | r<-rules context ] ++
>      [ "}"
>      ]
>     )) ++ "?>"
