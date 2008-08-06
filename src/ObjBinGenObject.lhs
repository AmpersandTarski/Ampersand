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
>      , "/********* on "++(show (pos object))
>      ] ++ (map ((++) " * ") (
>                showObjDef object )) ++
>      [" *********/"
>      , ""
>      , "function getobject_"++(name object)++"(){"]
>      ++ addFstLst "  return " ";" (map ((++) "  ") (getObject object)) ++
>      [ "}"
>      , ""] ++ showClasses [] object ++
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
>      ,"    // check existance of $id"
>      ,"    $ctx = DB_doquer('"++(doesExistQuer object "$id")++"');"
>      ,"    if(count($ctx)==0) return false;"
>      ,"    $obj1 = new "++(name object)++"($id" ++ (concat [", array()" | a<-attributes object]) ++ ");"
>      ]
>      ++ (concat (map (map ((++) "    "))
>             [ [ "$ctx = DB_doquer('"++ (selectExprForAttr a object "$id") ++"');"
>               , "foreach($ctx as $i=>$v1){"
>               ] ++ readObject a [name object] ++ [ "}" ]
>             | a <-attributes object
>             ]
>            )) ++
>      ["    return $obj1;"
>      ,"}"]
>      ++
>      ["function update"++capname++"("++(name object)++" $"++(name object)++",$new=false){"
>      ,"    global $DB_link,$DB_err,$DB_lastquer;"
>      ,"    $preErr= $new ? 'Cannot create new "++(addslashes (name (concept object)))++": ':'Cannot update "++(addslashes (name (concept object)))++": ';"
>      ,"    DB_doquer('START TRANSACTION');"
>      ,"    if($new){ // create a new object"
>      ,"      if(!isset($"++(name object)++"->id)){ // find a unique id"
>      ,"         $nextNum = DB_doquer('"++(autoIncQuer (concept object))++"');"
>      ,"         $"++(name object)++"->id = @$nextNum[0][0]+0;"
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
>      ,"    }else"]
>      ++ updateObject [name object] object
>      ++ checkRuls ++
>      ["    if(true){ // all rules are met"
>      ,"        DB_doquer('COMMIT');"
>      ,"        return $"++(name object)++"->id;"
>      ,"    }"
>      ,"    DB_doquer('ROLLBACK');"
>      ,"    return false;"
>      ,"}"]
>      ++

>      ["function delete"++capname++"($id){"
>      ,"  global $DB_err;"
>      ,"  DB_doquer('START TRANSACTION');"
>      ,"  "] ++
>      concat (map (map ((++) "    "))
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
>             , not (elem (makeInline m) (mors (map ctx (termAtts object))))  -- mors yields all morphisms inline.
>             ])
>      ++["/*"]
>      ++ map show (mors (map ctx (termAtts object)))
>      ++["*************"]
>      ++ map show (mors (morsWithCpt (concept object)))
>      ++["*/"]
>      ++ deleteObject (name object) object ++ checkRuls ++
>      ["  if(true) {"
>      ,"    DB_doquer('COMMIT');"
>      ,"    return true;"
>      ,"  }"
>      ,"  DB_doquer('ROLLBACK');"
>      ,"  return false;"
>      ,"}"
>      ]
>

>     )) ++ "\n?>"
>    where
>     showObjDef a | null (attributes a) =
>      [  (name a)++"["++(name (concept a))++"] : "++ (showADL (ctx a))
>       ]
>     showObjDef a =
>      (  (name a)++"["++(name (concept a))++"] : "++ (showADL (ctx a))
>       ):(concat (mapHeadTail (mapHeadTail ((++) " = [ ")
>                                           ((++) "     "))
>                              (mapHeadTail ((++) "   , ")
>                                           ((++) "     "))
>                              [ (showObjDef as)
>                              | as <- attributes a
>                              ]
>                 )
>         ) ++ ["  ]"]

>     getObject o | null (attributes o) = 
>      [ "new object(\""++(name o)++"\", array()"++phpage (concept o)++")"
>      ]
>     getObject o =
>      [ "new object(\""++(name o)++"\", array"
>      ]
>      ++ (concat(mapHeadTail  (mapHeadTail ((++) "   ( ")
>                                           ((++) "     "))
>                              (mapHeadTail ((++) "   , ")
>                                           ((++) "     "))
>                              [ concat
>                                 [["new oRef( new oMulti( " ++ (hasm Inj m) ++ ","
>                                                           ++ (hasm Uni m) ++ ","
>                                                           ++ (hasm Sur m) ++ ","
>                                                           ++ (hasm Tot m) ++ " ) // derived from "++(showADL (ctx a))
>                                 ], (mapHeadTail ((++) "  , ") ((++) "   ") (getObject a))
>                                  , ["  ) "]]
>                              | a <- attributes o, m <- [multiplicities (ctx a)]
>                              ]
>         )       )++["   )"++phpage (concept o)++")"]
>
>     showClasses nm o =
>        [ "class "++concat [n++"_"|n<-nm] ++(name o) ++" {"] ++
>        (map ((++) "  ") (
>         ["var $id;"]
>         ++ ["var $"++(name a)++";"| a <- attributes o]++
>         ["function "++concat [n++"_"|n<-nm]++(name o)++"($id=null"
>                                    ++  (concat [", $"++(name a)++"=array()" | a<-attributes o])
>                                    ++"){"
>         ,"    $this->id=$id;"]
>         ++ ["    $this->"++(name a)++"=$"++(name a)++";"| a <- attributes o] ++
>         ["}"]
>         ++ (concat
>            [ ["function add_"++(name a)++"("++concat [n++"_"|n<-nm++[name o]]++(name a)++" $"++(name a)++"){"
>              ,"  return $this->"++(name a)++"[]=$"++(name a)++";"
>              ,"}"
>              ]
>            | a <- attributes o
>            ]
>            )++
>         ["function addGen($type,$value){"
>         ]++ [ "  if($type=='"++(name a)++"') return $this->add_"++(name a)++"($value);"
>             | a <- attributes o
>             ] ++
>         ["  else return false;"|length (attributes o) > 0] ++
>         ["}"
>         ]
>         )) ++
>        [ "}"
>        ] ++ (concat [ showClasses (nm++[name o]) a |a <- attributes o ] )

>     readObject a nm =
>      ["    $obj"++(show (n+1))++"=$obj"++(show n)++"->add_"++(name a)++"(new "++concat [m++"_"|m<-nm]++(name a)++"($v"++(show n)++"['"++(sqlExprTrg (ctx a))++"']));"
>      ]
>      ++ (concat (map (map ((++) "    "))
>             [ [ "$ctx"++(show (n+1))++" = DB_doquer('"++ (selectExprForAttr as object ("$v"++(show n)++"['"++(sqlExprTrg (ctx a))++"']")) ++"');"
>               , "foreach($ctx"++(show (n+1))++" as $i=>$v"++(show (n+1))++"){"
>               ] ++ readObject as (nm++[name a]) ++ [ "}" ]
>             | as <-attributes a
>             ]
>            ))
>      where n = length nm

>--     updateObject :: [String] -> a -> [String]
>     updateObject nms o =
>      ["    if(!$new){"
>      ,"      // destroy old attribute values"
>      ] ++ (concat (map (map ((++) "      "))
>             [ [ "$effected = DB_doquer('"++ (selectExprForAttr a o ("$"++nm++"->id")) ++"');"
>               , "$arr=array();"
>               , "foreach($effected as $i=>$v){"
>               , "    $arr[]='\\''.addslashes($v['"++(sqlExprTrg (ctx a))++"']).'\\'';"
>               , "}"
>               , ("$"++nm++"_"++(name a)++"_str")++"=join(',',$arr);"
>               , "DB_doquer( '"++(deleteExprForAttr a o ("$"++nm++"->id"))++"');"
>               ]
>             | a <- termAtts o -- door de definitie van termAtts heeft de expressie "ctx a" precies één morfisme.
>             ]
>            )) ++
>      ["    }"
>      ] ++ (concat (map (map ((++) "    "))
>             [ [ "foreach($"++nm++"->"++(name a)++" as $i=>$"++nm++"_"++(name a)++"){"
>               , "  if(!isset($"++nm++"_"++(name a)++"->id)){"
>               , "     $nextNum = DB_doquer('"++(autoIncQuer (concept a))++"');"
>               , "     $"++nm++"_"++(name a)++"->id = @$nextNum[0][0]+0;"
>               , "  }else{"
>               , "     // check cardinalities..."
>               , "  }"
>               , "  if(count(DB_doquer('"++doesExistQuer a ("$"++nm++"_"++(name a)++"->id")++"'))==0)"
>               , "    DB_doquer('"++(insertConcept (concept a) ("$"++nm++"_"++(name a)++"->id") True)++"');"
>               ] ++ updateObject (nms++[name a]) a ++
>               [ "}"
>               , "$"++nm++"->id = $"++nm++"_"++(name a)++";"
>               ]
>             | a <- attributes o -- De expressie ctx a bevat precies één morfisme.
>             , (Tm (I _ _ _ _)) <- [ctx a]   -- De morfismen uit 'mors' zijn allemaal inline.
>             ]
>            ))
>         ++ (concat (map (map ((++) "    "))
>             [ [ "foreach($"++nm++"->"++(name a)++" as $i=>$"++nm++"_"++(name a)++"){"
>               , "  if(!isset($"++nm++"_"++(name a)++"->id)){"
>               , "     $nextNum = DB_doquer('"++(autoIncQuer (concept a))++"');"
>               , "     $"++nm++"_"++(name a)++"->id = @$nextNum[0][0]+0;"
>               , "  }else{"
>               , "     // check cardinalities..."
>               , "  }"
>               , "  DB_doquer('"++(insertConcept (concept a) ("$"++nm++"_"++(name a)++"->id") True)++"');"
>               , "  DB_doquer('INSERT IGNORE INTO "
>                 ++(sqlMorName context (head (mors m)))++" ("++(sqlExprSrc m)++","++(sqlExprTrg m)++")"
>                 ++" VALUES (\\''.addslashes($"++nm++"->id).'\\'"
>                 ++        ",\\''.addslashes($"++nm++"_"++(name a)++"->id).'\\')');"
>               ] ++ updateObject (nms++[name a]) a ++
>               [ "}"
>               ]
>             | a <- termAtts o -- De expressie ctx a bevat precies één morfisme.
>             , m <- [ctx a]   -- De morfismen uit 'mors' zijn allemaal inline.
>             ]
>            ))
>      ++ concat (map (map ((++) "    "))
>                     [ [ "if(!$new && strlen($"++nm++"_"++(name a)++"_str))"
>                       ] ++ (do_del_quer a ("$"++nm++"_"++(name a)++"_str"))
>                     | a <- termAtts o
>                     ]
>                )
>      where nm = chain "_" nms

>     deleteObject nm o =
>      (concat (map (map ((++) "      "))
>             [ [ "$effected = DB_doquer('"++ (selectExprForAttr a o "$id") ++"');"
>               , "$arr=array();"
>               , "foreach($effected as $i=>$v){"
>               , "    $arr[]='\\''.addslashes($v['"++(sqlExprTrg (ctx a))++"']).'\\'';"
>               , "}"
>               , "$"++(name a)++"_str=join(',',$arr);"
>               , "DB_doquer ('"++(deleteExprForAttr a object "$id")++"');"
>               ]
>             | a <- termAtts o
>             ]
>            )) ++
>      ["  DB_doquer('DELETE FROM "++(sqlConcept context (concept object))
>       ++" WHERE "++(sqlAttConcept context (concept object))++"=\\''.addslashes($id).'\\'');"
>      ] ++ (concat (map (map ((++) "  "))
>             [ [ "if(strlen($"++(name a)++"_str))"
>               ] ++ (do_del_quer a ("$"++(name a)++"_str"))
>             | a <- termAtts object
>             ])) 

>     mapTail f (a:as) = a:(map f as)
>     mapHead f (a:as) = (f a):as
>     addFstLst f1 f2 (a:as) = (f1++a):(addLst f2 as)
>     addLst f (a:[]) = [a++f]
>     addLst f (a:as) = a:(addLst f (as))
>     mapHeadTail f1 f2 (a:as) = (f1 a):(map f2 as)

>     checkRuls
>           = (concat
>             [ ["  if (!checkRule"++show (nr rul)++"()){"
>               ,"    $DB_err=$preErr.'"++(addslashes (show(explain rul)))++"';"
>               ,"  } else"
>               ]
>             | rul <- (rules context)++(multRules context),
>               or (map (\m -> elem m (mors rul)) -- rule contains an element
>                       (mors object) -- effected mors  ; SJ: mors yields all morphisms inline.
>                  )
>             ])
>     do_del_quer a str
>           = [ "  DB_doquer('DELETE FROM "++(sqlConcept context (concept a))
>               , "    WHERE "++(sqlExprTrg (ctx a))++" IN ('."++str++".')"
>               ] ++ concat (
>                  [ andNEXISTquer (ctx a) m
>                  | m@(Mph _ _ _ _ _ _) <- morsWithCpt (concept a)
>                  ]
>                  ) ++
>               [ "  ');"]
>     andNEXISTquer e m = ["      AND NOT EXISTS (SELECT * FROM "++(sqlMorName context m)
>                    ,"                       WHERE "
>                     ++ (sqlConcept context (target e))++"."++(sqlAttConcept context (target e))++" = "
>                     ++ (sqlMorName context m)++"."++(sqlMorSrc context m)
>                    ,"                     )"
>                    ]
>     prag (Sgn _ _ _ _ p1 p2 p3 _ _ _ _ _) s1 s2 = (addslashes p1) ++ s1 ++ (addslashes p2) ++ s2 ++ (addslashes p3)
>     autoIncQuer cpt = "SELECT max(1+"++(sqlAttConcept context cpt)
>                     ++") FROM "++(sqlConcept context cpt)++" GROUP BY \\'1\\'"
>     insertConcept cpt var ignore = "INSERT "++(if ignore then "IGNORE " else "") ++ "INTO "++(sqlConcept context cpt)++" ("
>                 ++(sqlAttConcept context cpt)++") VALUES (\\''.addslashes("++var++").'\\')"
>     selectExprForAttr a parent id
>       = selectExprWithF (ctx a) (concept parent) id
>     selectExprWithF e cpt id
>       = selectExpr context 25 (sqlExprSrc e) (sqlExprTrg e)
>                     (F [Tm (Mp1 ("\\''.addslashes("++id++").'\\'") cpt), e])
>     doesExistQuer object id = ( selectExpr context
>                                            25
>                                            (sqlAttConcept context (concept object))
>                                            (sqlAttConcept context (concept object))
>                                            (Fi [ Tm (I [] (concept object) (concept object) True)
>                                                , Tm (Mp1 ("\\''.addslashes("++id++").'\\'") (concept object))
>                                                ]
>                                            )
>                               )


> --Precondition: ctx a bevat precies één morfisme
>     deleteExprForAttr a parent id
>       = "DELETE FROM "++sqlMorName context ((head.mors.ctx) a)++" WHERE "++(sqlExprSrc (ctx a))++"=\\''.addslashes("++id++").'\\'"
>     hasm m ms = if elem m ms then "true" else "false"
>     capname = (toUpper (head (name object))):(tail (name object))
>     termAtts o = [a|a<-attributes o, (Tm (Mph _ _ _ _ _ _))<-[ctx a]] -- Dit betekent: de expressie ctx a bevat precies één morfisme.
>     morsWithCpt cpt = rd ([m|m<-mors context, source m == cpt] ++ [flp m|m<-mors context, target m==cpt])
>     phpage c = mystr (objectOfConcept context c)
>       where mystr Nothing = ""
>             mystr (Just o)= ", \""++(name o)++".php\""
