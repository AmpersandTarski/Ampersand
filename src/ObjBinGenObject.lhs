> module ObjBinGenObject where
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

>  objectServices context filename object
>   = (chain "\n  "
>     ([ "<?php // generated with "++adlVersion
>      , ""
>      , "/********* file "++filename++" on line "++(show (pos object))
>      ] ++ (map ((++) " * ") (
>         ("OBJECT "++(name object)++" "++(name (concept object)))
>       :[ "    " ++ (name att) ++ " : " ++ (show e)
>        | att@(Att _ _ _ e) <- attributes object
>        ] ++ ["ENDOBJECT"] )) ++
>      [" *********/"
>      , ""
>      , "function getobject_"++(name object)++"(){"
>      , "  return new object(\""++(name object)++",array"
>      , "    (" ++ (chain "\n      ,"
>        [ "new oRef( new oMulti( " ++ (hasm Inj m) ++ ","
>                                   ++ (hasm Uni m) ++ ","
>                                   ++ (hasm Sur m) ++ ","
>                                   ++ (hasm Tot m) ++ " ) // derived from "++(showADL e)
>           ++ "\n             , new object(\""++nm++"\",array()"++ (phpage c) ++ ")"
>           ++ "\n             )"
>        | att@(Att nm _ c e) <- attributes object, m <- [multiplicities e]
>        ])
>      , "    ),\""++(name object)++".php\");"
>      , "}"
>      , ""
>      , "class "++(name object)++" {"] ++ (map ((++) "  ") (
>        ["var $id;"]
>        ++ ["var $"++(name a)++";"| a <- attributes object]++
>        ["function "++(name object)++"($id=null, "
>                                   ++  (chain ", " ["$"++(name a) | a<-attributes object])
>                                   ++"){"
>        ,"    $this->id=$id;"]
>        ++ ["    $this->"++(name a)++"=$"++(name a)++";"| a <- attributes object] ++
>        ["}"]
>        ++ (concat
>           [ ["function add_"++(name a)++"("++(name object)++"_"++(name a)++" $"++(name a)++")"
>             ,"  return $this->"++(name a)++"[]=$"++(name a)++";"
>             ,"}"
>             ]
>           | a <- attributes object
>           ]
>           )++
>        ["function addGen($type,$value){"
>        ]++ [ "  if($type=='"++(name a)++"') return $this->add_"++(name a)++"($value);"
>            | a <- attributes object
>            ] ++
>        ["  else return false;"
>        ,"}"
>        ]
>        )) ++
>      [ "}"
>      ] ++ (concat
>           [ ["class "++(name object)++"_"++(name a)++" {"
>             ,"    var $id;"
>             ,"    function "++(name object)++"_"++(name a)++"($id) {"
>             ,"        $this->id=$id;"
>             ,"    }"
>             ,"}"]
>           |a <- attributes object
>           ]
>           ) ++
>      ["function getEach"++(capname)++"(){"
>      ,"    return DB_doquer('"++(addslashes
>                                 (selectExpr context
>                                             6
>                                             (sqlAttConcept context (concept object))
>                                             (sqlAttConcept context (concept object))
>                                             (Tm (I [] (concept object) (concept object) True))
>                                 ))++"');"
>      ,"}"
>      ,"function create"++capname++"("++(name object)++" &$obj){"
>      ,"    return update"++capname++"($obj,true);"
>      ,"}"
>      ,"function read"++capname++"($id){"
>      ,"}"
>      ,"function update"++capname++"($id){"
>      ,"}"
>      ,"function delete"++capname++"($id){"
>      ,"}"
>      ]
>     )) ++ "\n?>"
>    where
>     hasm m ms = if elem m ms then "true" else "false"
>     capname = (toUpper (head (name object))):(tail (name object))
>     phpage c = mystr (objectOfConcept context c)
>       where mystr Nothing = ""
>             mystr (Just o)= ", \""++(name o)++".php\""
