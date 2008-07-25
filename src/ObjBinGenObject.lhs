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
>      , "    (" ++ (chain "\n      ,"
>        [ "new oRef( new oMulti( " ++ (hasm Inj m) ++ ","
>                                   ++ (hasm Uni m) ++ ","
>                                   ++ (hasm Sur m) ++ ","
>                                   ++ (hasm Tot m) ++ " ) // derived from "++(showADL e)
>           ++ "\n             , new object(\""++nm++"\",array()"++ (phpage (concept att)) ++ ") // "++(show (concept att))
>           ++ "\n             )"
>        | att@(Att nm _ c e) <- attributes object, m <- [multiplicities e]
>        ])
>      , "    )"++(phpage (concept object))++");"
>      , "}"
>      , ""
>      , "class "++(name object)++" {"] ++ (map ((++) "  ") (
>        ["var $id;"]
>        ++ ["var $"++(name a)++";"| a <- attributes object]++
>        ["function "++(name object)++"($id=null, "
>                                   ++  (chain ", " ["$"++(name a)++"=array()" | a<-attributes object])
>                                   ++"){"
>        ,"    $this->id=$id;"]
>        ++ ["    $this->"++(name a)++"=$"++(name a)++";"| a <- attributes object] ++
>        ["}"]
>        ++ (concat
>           [ ["function add_"++(name a)++"("++(name object)++"_"++(name a)++" $"++(name a)++"){"
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
>             [ [ "$ctx = DB_doquer('"++ (selectExprForAttr a object "$id") ++"');"
>               , "foreach($ctx as $i=>$v){"
>               , "    $obj->add_"++(name a)++"(new "++(name object)++"_"++(name a)++"($v['"++(sqlExprTrg e)++"']));"
>               , "}"
>               ]
>             | a@(Att _ _ _ e) <-attributes object
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
>      ,"         $nextNum = DB_doquer('"++(autoIncQuer (concept object))++"');"
>      ,"         $"++(name object)++"->id = $nextNum[0][0];"
>      ,"      }"
>      ,"      if(DB_plainquer('" ++
>          (insertConcept (concept object) ("$"++(name object)++"->id") False)
>                               ++"',$errno)===false){"
>      ,"          $DB_err=$preErr.(($errno==1062) ? '"
>         ++(addslashes (name (concept object))) ++" \\''.$"++(name object)++
>         "->id.'\\' allready exists' : 'Error '.$errno.' in query '.$DB_lastquer);"
>      ,"          DB_doquer('ROLLBACK');"
>      ,"          return false;"
>      ,"      }"
>      ,"    }else{"
>      ,"      // destroy old attribute values"
>      ] ++ (concat (map (map ((++) "      "))
>             [ [ "$effected = DB_doquer('"++ (selectExprForAttr a object ("$"++(name object)++"->id")) ++"');"
>               , "$arr=array();"
>               , "foreach($effected as $i=>$v){"
>               , "    $arr[]='\\''.addslashes($v['"++(sqlExprTrg e)++"']).'\\'';"
>               , "}"
>               , "$"++(name a)++"_str=join(',',$arr);"
>               , "DB_doquer('"++(deleteExprForAttr a object)++"');"
>               ]
>             | a@(Att _ _ _ e@(Tm _)) <- termAtts
>             ]
>            )) ++
>      ["    }"
>      ] ++ (concat (map (map ((++) "    "))
>             [ [ "foreach($"++(name object)++"->"++(name a)++" as $i=>$v){"
>               , "  if(!isset($v->id)){"
>               , "     $nextNum = DB_doquer('"++(autoIncQuer (concept a))++"');"
>               , "     $v->id = $nextNum[0][0];"
>               , "  }else{"
>               , "     // check cardinalities..."
>               , "  }"
>               , "  DB_doquer('"++(insertConcept (concept a) "$v->id" True)++"');"
>               , "  DB_doquer('INSERT IGNORE INTO "
>                 ++(sqlMorName context m)++" ("++(sqlExprSrc e)++","++(sqlExprTrg e)++")"
>                 ++" VALUES (\\''.addslashes($"++(name object)++"->id).'\\'"
>                 ++        ",\\''.addslashes($v->id).'\\')');"
>               , "}"
>               ]
>             | a@(Att _ _ _ e@(Tm m)) <- termAtts
>             ]
>            )) ++ (concat (map (map ((++) "    "))
>             [ [ "if(!$new && strlen($"++(name a)++"_str))"
>               ] ++ (do_del_quer a)
>             | a <- termAtts
>             ])) ++ checkRuls ++
>      ["    if(true){ // all rules are met"
>      ,"        DB_doquer('COMMIT');"
>      ,"        return $"++(name object)++"->id;"
>      ,"    }"
>      ,"    DB_doquer('ROLLBACK');"
>      ,"    return false;"
>      ,"}"
>      ,"function delete"++capname++"($id){"
>      ,"  global $DB_err;"
>      ,"  DB_doquer('START TRANSACTION');"
>      ,"  "] ++ concat (map (map ((++) "    "))
>             [ ["$taken = DB_doquer('"++(selectExprWithF (Tm m) cpt "$id")++"');"
>               ,"if(count($taken)) {"
>               ,"  $DB_err = 'Cannot delete "++(name object)++": "
>                ++(prag d "\\''.addslashes($id).'\\'" "\\''.addslashes($taken[0]['"
>                ++(sqlExprTrg (Tm m))++"']).'\\'" )++"';" -- pragma
>               ,"  DB_doquer('ROLLBACK');"
>               ,"  return false;"
>               ,"}"
>               ]
>             | cpt <- [concept object]
>             , m@(Mph _ _ _ _ _ d) <- morsWithCpt cpt
>             , not (elem (makeInline m) (map makeInline [ms|(Att _ _ _ (Tm ms)) <- termAtts]))
>             ])
>          ++ (concat (map (map ((++) "      "))
>             [ [ "$effected = DB_doquer('"++ (selectExprForAttr a object "$id") ++"');"
>               , "$arr=array();"
>               , "foreach($effected as $i=>$v){"
>               , "    $arr[]='\\''.addslashes($v['"++(sqlExprTrg e)++"']).'\\'';"
>               , "}"
>               , "$"++(name a)++"_str=join(',',$arr);"
>               , "DB_doquer('"++(deleteExprForAttr a object)++"');"
>               ]
>             | a@(Att _ _ _ e@(Tm _)) <- termAtts
>             ]
>            )) ++
>      ["  DB_doquer('DELETE FROM "++(sqlConcept context (concept object))
>       ++" WHERE "++(sqlAttConcept context (concept object))++"=\\''.addslashes($id).'\\'');"
>      ] ++ (concat (map (map ((++) "  "))
>             [ [ "if(strlen($"++(name a)++"_str))"
>               ] ++ (do_del_quer a)
>             | a <- termAtts
>             ])) ++ checkRuls ++
>      ["  if(true) {"
>      ,"    DB_doquer('COMMIT');"
>      ,"    return true;"
>      ,"  }"
>      ,"  DB_doquer('ROLLBACK');"
>      ,"  return false;"
>      ,"}"
>      ]
>     )) ++ "\n?>"
>    where
>     checkRuls
>           = (concat
>             [ ["  if (!checkRule"++show (nr rul)++"()){"
>               ,"    $DB_err=$preErr.'"++(addslashes (show(explain rul)))++"';"
>               ,"  } else"
>               ]
>             | rul <- (rules context)++(multRules context),
>               or (map (\m -> elem m (map makeInline (mors rul))) -- rule contains an element
>                       (map makeInline (mors object)) -- effected mors
>                  )
>             ])
>     do_del_quer a@(Att _ _ _ e@(Tm _))
>           = [ "  DB_doquer('DELETE FROM "++(sqlConcept context (concept a))
>               , "    WHERE "++(sqlExprTrg e)++" IN ('.$"++(name a)++"_str.')"
>               ] ++ concat (
>                  [ andNEXISTquer e m
>                  | m <- morsWithCpt (concept a)
>                  ]
>                  ) ++
>               [ "  ');"]
>     andNEXISTquer e m = ["      AND NOT EXISTS (SELECT * FROM "++(sqlMorName context m)
>                    ,"                       WHERE "
>                     ++ (sqlConcept context (target e))++"."++(sqlAttConcept context (target e))++" = "
>                     ++ (sqlMorName context m)++"."++(sqlMorTrg context m)
>                    ,"                     )"
>                    ]
>     prag (Sgn _ _ _ _ p1 p2 p3 _ _ _ _ _) s1 s2 = (addslashes p1) ++ s1 ++ (addslashes p2) ++ s2 ++ (addslashes p3)
>     autoIncQuer cpt = "SELECT max(1+"++(sqlAttConcept context cpt)
>                     ++") FROM "++(sqlConcept context cpt)++" GROUP BY \\'1\\'"
>     insertConcept cpt var ignore = "INSERT "++(if ignore then "IGNORE " else "") ++ "INTO "++(sqlConcept context cpt)++" ("
>                 ++(sqlAttConcept context cpt)++") VALUES (\\''.addslashes("++var++").'\\')"
>     selectExprForAttr (Att _ _ _ e) parent id
>       = selectExprWithF e (concept parent) id
>     selectExprWithF e cpt id
>       = selectExpr context 25 (sqlExprSrc e) (sqlExprTrg e)
>                     (F [Tm (Mp1 ("\\''.addslashes("++id++").'\\'") cpt), e])
>         
>     deleteExprForAttr (Att _ _ _ e@(Tm m)) parent
>       = "DELETE FROM "++(sqlMorName context m)++" WHERE "++(sqlExprSrc e)++"=\\'addslashes($id)\\'"
>     hasm m ms = if elem m ms then "true" else "false"
>     capname = (toUpper (head (name object))):(tail (name object))
>     termAtts = [a|a@(Att _ _ _ (Tm _)) <-attributes object]
>     morsWithCpt cpt = rd ([m|m<-mors context, source m == cpt] ++ [flp m|m<-mors context, target m==cpt])
>     phpage c = mystr (objectOfConcept context c)
>       where mystr Nothing = ""
>             mystr (Just o)= ", \""++(name o)++".php\""
