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

>  objectServices :: Context -> String -> ObjectDef -> String
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
>      , "  return new object(\""++(name object)++"\",array"
>      , "    (" {- ++ (chain "\n      ,"
>        [ "new oRef( new oMulti( " ++ (hasm Inj m) ++ ","
>                                   ++ (hasm Uni m) ++ ","
>                                   ++ (hasm Sur m) ++ ","
>                                   ++ (hasm Tot m) ++ " ) // derived from "++(showADL e)
>           ++ "\n             , new object(\""++nm++"\",array()"++ (phpage c) ++ ")"
>           ++ "\n             )"
>        | att@(Att nm _ c e) <- attributes object, m <- [multiplicities e]
>        ]) -}
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
>      ,"    return DB_doquer('"++(selectExpr context
>                                             25
>                                             (sqlAttConcept context (concept object))
>                                             (sqlAttConcept context (concept object))
>                                             (Tm (I [] (concept object) (concept object) True))
>                                 )++"');"
>      ,"}"
>      ,"function create"++capname++"("++(name object)++" &$obj){"
>      ,"    return update"++capname++"($obj,true);"
>      ,"}"
>      ,"function read"++capname++"($id){"
>      ,"    $ctx = DB_doquer('"++(selectExpr context
>                                             25
>                                             (sqlAttConcept context (concept object))
>                                             (sqlAttConcept context (concept object))
>                                             (Fi [ Tm (I [] (concept object) (concept object) True)
>                                                 , Tm (Mp1 ("\\''.addslashes($id).'\\'") (concept object))
>                                                 ]
>                                             )
>                                 )++"');"
>      ,"    if(count($ctx)==0) return false;"
>      ,"    $obj = new "++(name object)++"($id" ++ (concat [", array()" | a<-attributes object]) ++ ");"
>      ] ++ (concat (map (map ((++) "    "))
>             [ [ "$ctx = DB_doquer('');"
>               , "foreach($tex as $i=>$v){"
>               , "    $obj->add_"++(name a)++"(new "++(name object)++"_"++(name a)++"($v['"++(sqlAttConcept context (concept object))++"']));"
>               , "}"
>               ]
>             | a <-attributes object
>             ]
>            )) ++
>      ["    return $obj;"
>      ,"}"
>      ,"function update"++capname++"("++(name object)++" $"++(name object)++",$new=false){"
>      ,"    global $DB_link,$DB_err,$DB_lastquer;"
>      ,"    $preErr= $new ? 'Cannot create new "++(addslashes (name (concept object)))++": ':'Cannot update "++(addslashes (name (concept object)))++": ';"
>      ,"    DB_doquer('START TRANSACTION');"
>      ,"    if($new){ // create a new object"
>      ,"      if(!isset($"++(name object)++")){ // find a unique id"
>      ,"         $nextNum = DB_doquer('SELECT max(1+"++(sqlAttConcept context (concept object))
>                     ++") FROM "++(sqlConcept context (concept object))++" GROUP BY \\'1\\'');"
>      ,"      }"
>      ,"      if(DB_plainquer('INSERT INTO "++(sqlConcept context (concept object))++" ("
>                 ++(sqlAttConcept context (concept object))++") VALUES (\\''.addslashes($"
>                 ++(name object)++"->id).'\\')',$errno==false){"
>      ,"          $DB_err=$preErr.(($errno==1062) ? '" ++(addslashes (name (concept object)))
>                 ++" \\''.$"++(name object)++
>                 "->id.'\\' allready exists' : 'Error '.$errno.' in query '.$DB_lastquer;"
>      ,"          DB_doquer('ROLLBACK');"
>      ,"          return false;"
>      ,"      }"
>      ,"    }else{"
>      ,"      // destroy old attribute values"
>      ,"    }"
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
