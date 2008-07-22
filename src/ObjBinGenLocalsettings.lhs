> module ObjBinGenLocalsettings where
>  import Char
>  import Auxiliaries
>  import Calc(informalRule, shrink, disjNF, computeOrder, ComputeRule, triggers)
>  import CC_aux (ObjectDef(Obj), ObjDefs, Attribute(Att), Attributes, KeyDef(Kd), KeyDefs, Object(objects))
>  import CommonClasses (Identified(name))
>  import PredLogic -- (for error messages by dbCorrect)
>  import Hatml     -- (for converting error messages to HTML)
>  import Atlas     -- (for converting error messages to HTML)
>  import RelBinGenBasics
> 

>  localsettings context dbName = chain "\n"
>   (["<?php"
>    ] ++ (map ((++) "  ") (
>      ["// Select the monastir view"
>      ,"$incPath = \"../inc/\";"
>      ,"$dbName  = \"" ++ dbName ++ "\"; // mysql internal name"
>      ,"$appName = \"" ++ (name context) ++ "\"; // full text name"
>      ,"require $incPath.\"globalsettings.inc.php\";"
>      ,"require \"connectToDataBase.inc.php\";"
>      ,"require $incPath.\"monastir.inc.php\";"
>      ,""
>      ,"class view Extends monastir {"
>      ] ++ (map ((++) "  ") (
>        ["function view($obj,$object){"
>        ] ++ (map ((++) "  ") (
>          ["global $action;"
>          ,"$this->action=$action;"
>          ,"$menu = array();"
>          ,"$menu[] = array"
>          ,"  ("++ (chain "\n        ,"
>           [ "new menuItem('"++objname++".php','Show all "++objname++" objects','menuItem','"++objname++"')"
>           | object<-objects context, objname <- [addslashes (name object)]
>           ])
>          ,"  );"
>          ,"parent::monastir($menu,$obj,$object,ucfirst($object->name));"
>          ])) ++
>        ["}"
>        ])) ++
>       ["}"
>       ])) ++
>      ["?>"
>    ])
>  