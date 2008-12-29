{-# LINE 1 "ObjBinGenObject.lhs" #-}
#line 1 "ObjBinGenObject.lhs"
  module ObjBinGenObject where
   import Char(toUpper)
   import Auxiliaries(chain, adlVersion)
   import Calc(informalRule, disjNF, computeOrder, ComputeRule, triggers)
   import ADLdef
   import ShowADL
   import CC_aux ( tot, fun
                 , objectOfConcept
                 )
   import CommonClasses
   import Collection (Collection(rd))
   import ERmodel
   import PredLogic -- (for error messages by dbCorrect)
   import Hatml     -- (for converting error messages to HTML)
   import Atlas     -- (for converting error messages to HTML)
   import RelBinGenBasics

 -- The service "getobject" communicates metadata to the interface.

   generateService_getobject :: Context -> ObjectDef -> String
   generateService_getobject context object
    = -- vastgesteld op 2008/12/6: gos/=[] dus is de volgende regel overbodig en uitgecommentarieerd:
      -- if null gos then error("Fail (Module ObjBinGenObject): unexpected pattern in generateService_getobject ("++show object++")") else
      "function getobject_"++ phpIdentifier (name object) ++"(){\n  "
      ++ chain "\n  " (("  return "++a):(addLst ";" as)) ++
      "\n  }\n"
       where a:as = [str++"  "| str<-gos]
             gos  = getObject context object
             addLst f (a:[]) = [a++f]
             addLst f (a:as) = a: addLst f as
             addLst f [] = [f]



   objectServices :: Context -> String -> ObjectDef -> String
   objectServices context filename object
    = (chain "\n  "
      ([ "<?php // generated with "++adlVersion
       , ""
       , "/********* on "++(show (pos object))
       ] ++ (map ((++) " * ") (
                 showObjDef object )) ++
       [" *********/"
       , ""
       , generateService_getobject context object   -- generate metadata for "object"
       ] ++ showClasses context [] object ++
       [ generateService_getEach context capname object
       , generateService_create  context capname object
       , generateService_read    context capname object
       , generateService_update  context capname object
       , generateService_delete  context capname object]
      )) ++ "\n?>"
     where
      showObjDef a | null (attributes a) =
       [  phpIdentifier (name a)++"["++phpIdentifier (name (concept a))++"] : "++ (showADL (ctx a))
        ]
      showObjDef a =
       (  phpIdentifier (name a)++"["++phpIdentifier (name (concept a))++"] : "++ (showADL (ctx a))
        ):(concat (mapHeadTail (mapHeadTail ((++) " = [ ")
                                            ((++) "     "))
                               (mapHeadTail ((++) "   , ")
                                            ((++) "     "))
                               [ (showObjDef as)
                               | as <- attributes a
                               ]
                  )
          ) ++ ["  ]"]
      capname = if null (name object)
                then error ("!Fail: (Module ObjBinGenObject) empty name in object:\n"++show object)
                else (toUpper (head (name object))):(tail (name object))



   generateService_getEach :: Context -> String -> ObjectDef -> String
   generateService_getEach context capname object
    = "function getEach"++phpIdentifier capname++"(){"++
      "\n      return DB_doquer('"++(selectExpr context
                                             25
                                             (sqlExprTrg (ctx object)) -- was:  (sqlAttConcept context (concept object))
                                             (sqlExprSrc (ctx object)) -- was:  (sqlAttConcept context (concept object))
                                             (flp (ctx object)) -- was: (Tm (mIs (concept object)))
                                 )++"');\n  }"



   generateService_create :: Context -> String -> ObjectDef -> String
   generateService_create context capname object
    = "function create"++phpIdentifier capname++"("++phpIdentifier (name object)++" &$obj){\n  "++
      "    return update"++phpIdentifier capname++"($obj,true);\n  }"



   generateService_read :: Context -> String -> ObjectDef -> String
   generateService_read context capname object
    = chain "\n  "
      (["function read"++phpIdentifier capname++"($id){"
       ,"    // check existence of $id"
       ,"    $ctx = DB_doquer('"++(doesExistQuer context object "$id")++"');"
       ,"    if(count($ctx)==0) return false;"
       ,"    $obj = new "++phpIdentifier (name object)++"($id);"
       ,"    return $obj;"
       ,"}"])




   phpVar :: String -> String
   phpVar x = "$"++phpIdentifier x

   generateService_update :: Context -> String -> ObjectDef -> String
   generateService_update context capname object
    = chain "\n  "
      (["function update"++phpIdentifier capname++"("++phpIdentifier (name object)++" "++(phpVar (name object))++",$new=false){"
       ,"    global $DB_link,$DB_err,$DB_lastquer;"
       ,"    $preErr= $new ? 'Cannot create new "++(addSlashes (name (concept object)))++": ':'Cannot update "++(addSlashes (name (concept object)))++": ';"
       ,"    DB_doquer('START TRANSACTION');"
       ,"    if($new){ // create a new object"
       ,"      if(!isset("++phpVar (name object)++"->id)){ // find a unique id"
       ,"         $nextNum = DB_doquer('"++(autoIncQuer context (concept object))++"');"
       ,"         "++phpVar (name object)++"->id = @$nextNum[0][0]+0;"
       ,"      }"
       ,"      if(DB_plainquer('" ++
           (insertConcept context (concept object) (phpVar (name object)++"->id") False)
                                ++"',$errno)===false){"
       ,"          $DB_err=$preErr.(($errno==1062) ? '"
          ++(addSlashes (name (concept object))) ++" \\''."++phpVar (name object)++
          "->id.'\\' allready exists' : 'Error '.$errno.' in query '.$DB_lastquer);"
       ,"          DB_doquer('ROLLBACK');"
       ,"          return false;"
       ,"      }"
       ,"    }else"]
       ++ updateObject context [name object] object
       ++ checkRuls context object ++
       ["    if(true){ // all rules are met"
       ,"        DB_doquer('COMMIT');"
       ,"        return "++phpVar (name object)++"->id;"
       ,"    }"
       ,"    DB_doquer('ROLLBACK');"
       ,"    return false;"
       ,"}"])

   updateObject :: Object a => Context -> [String] -> a -> [String]
   updateObject context nms o =
       ["    if(!$new){"
       ,"      // destroy old attribute values"
       ] ++ (concat (map (map ((++) "      "))
              [ [ "$effected = DB_doquer('"++ (selectExprForAttr context a o (phpVar nm++"->id")) ++"');"
                , "$arr=array();"
                , "foreach($effected as $i=>$v){"
                , "    $arr[]='\\''.addSlashes($v['"++(sqlExprTrg (ctx a))++"']).'\\'';"
                , "}"
                , phpVar (nm++"_"++name a++"_str")++"=join(',',$arr);"
                , "DB_doquer( '"++(deleteExprForAttr context a o (phpVar nm++"->id"))++"');"
                ]
              | a <- termAtts o -- door de definitie van termAtts heeft de expressie "ctx a" precies één morfisme.
              ]
             )) ++
       ["    }"
       ]  ++ (concat (map (map ((++) "    "))
              [ [ "foreach("++phpVar nm++"->"++phpIdentifier (name a)++" as $i=>"++phpVar (nm++"_"++name a)++"){"
                ] ++ (concat (map (map ((++) "  "))
                      [ [ "if(isset("++phpVar (nm++"_"++name a)++"->"++phpIdentifier (name as)++"[0]->id)){"
                        , "  if(count(DB_doquer('"++doesExistQuer context a (phpVar (nm++"_"++name a)++"->"++phpIdentifier (name as)++"[0]->id")++"'))==0)"
                        , "    DB_doquer('"++(insertConcept context (concept a) (phpVar (nm++"_"++name a)++"->id") True)++"');"
                        ] ++ updateObject context (nms++[name a,name as]) as ++
                        [ "}"
                        , phpVar (nm++"_"++name a)++"->id = @"++phpVar (nm++"_"++name a)++"->"++phpIdentifier (name as)++"[0]->id;"
                        ]
                      | as <- attributes a -- De expressie ctx a bevat precies één morfisme.
                      , (Tm (I _ _ _ _)) <- [ctx as]   -- De morfismen uit 'mors' zijn allemaal inline.
                      ]
                     ))
                 ++
                [ "  if(!isset("++phpVar (nm++"_"++name a)++"->id)){"
                , "     $nextNum = DB_doquer('"++autoIncQuer context (concept a)++"');"
                , "     "++phpVar (nm++"_"++name a)++"->id = @$nextNum[0][0]+0;"
                , "  }"
                , "  DB_doquer('"++(insertConcept context (concept a) (phpVar (nm++"_"++name a)++"->id") True)++"');"
                , "  DB_doquer('INSERT IGNORE INTO "
                  ++(sqlMorName context (head (mors m)))++" ("++(sqlExprSrc m)++","++(sqlExprTrg m)++")"
                  ++" VALUES (\\''.addSlashes("++phpVar nm++"->id).'\\'"
                  ++        ",\\''.addSlashes("++phpVar (nm++"_"++name a)++"->id).'\\')');"
                ] ++ updateObject context (nms++[name a]) a ++
                [ "}"
                ]
              | a <- termAtts o -- De expressie ctx a bevat precies één morfisme.
              , m <- [ctx a]   -- De morfismen uit 'mors' zijn allemaal inline.
              ]
             ))
       ++ concat (map (map ((++) "    "))
                      [ [ "if(!$new && strlen("++phpVar (nm++"_"++name a)++"_str))"
                        ] ++ (do_del_quer context a (phpVar (nm++"_"++name a)++"_str"))
                      | a <- termAtts o
                      ]
                 )
       where nm = chain "_" nms



   generateService_delete :: Context -> String -> ObjectDef -> String
   generateService_delete context capname object
    = chain "\n  "
      (["function delete"++phpIdentifier capname++"($id){"
       ,"  global $DB_err;"
       ,"  $preErr= 'Cannot delete "++(addSlashes (name (concept object)))++": ';"
       ,"  DB_doquer('START TRANSACTION');"
       ,"  "] ++
       concat (map (map ((++) "    "))
              [ ["$taken = DB_doquer('"++(selectExprWithF context (Tm m) cpt "$id")++"');"
                ,"if(count($taken)) {"
                ,"  $DB_err = 'Cannot delete "++(name object)++": "
                 ++(prag d "\\''.addSlashes($id).'\\'" "\\''.addSlashes($taken[0]['"
                 ++(sqlExprTrg (Tm m))++"']).'\\'" )++"';" -- pragma
                ,"  DB_doquer('ROLLBACK');"
                ,"  return false;"
                ,"}"
                ]
              | cpt <- [concept object]
              , m@(Mph _ _ _ _ _ d) <- morsWithCpt context cpt
              , not (elem (makeInline m) (mors (map ctx (termAtts object))))  -- mors yields all morphisms inline.
              ])
       ++["/*"]
       ++ map show (mors (map ctx (termAtts object)))
       ++["*************"]
       ++ map show (mors (morsWithCpt context (concept object)))
       ++["*/"]
       ++ deleteObject context object ++ checkRuls context object ++
       ["  if(true) {"
       ,"    DB_doquer('COMMIT');"
       ,"    return true;"
       ,"  }"
       ,"  DB_doquer('ROLLBACK');"
       ,"  return false;"
       ,"}"
       ])

 --  deleteObject :: Context -> a -> [String]
   deleteObject context object =
       (concat (map (map ((++) "      "))
              [ [ "$effected = DB_doquer('"++ (selectExprForAttr context a object "$id") ++"');"
                , "$arr=array();"
                , "foreach($effected as $i=>$v){"
                , "    $arr[]='\\''.addSlashes($v['"++(sqlExprTrg (ctx a))++"']).'\\'';"
                , "}"
                , phpVar (name a)++"_str=join(',',$arr);"
                , "DB_doquer ('"++(deleteExprForAttr context a object "$id")++"');"
                ]
              | a <- termAtts object
              ]
             )) ++
       ["  DB_doquer('DELETE FROM "++(sqlConcept context (concept object))
        ++" WHERE "++(sqlAttConcept context (concept object))++"=\\''.addSlashes($id).'\\'');"
       ] ++ (concat (map (map ((++) "  "))
              [ [ "if(strlen("++phpVar (name a++"_str")++"))"
                ] ++ (do_del_quer context a (phpVar (name a++"_str")))
              | a <- termAtts object
              ])) 

   prag (Sgn _ _ _ _ p1 p2 p3 _ _ _ _ _) s1 s2 = (addSlashes p1) ++ s1 ++ (addSlashes p2) ++ s2 ++ (addSlashes p3)
   morsWithCpt context cpt = rd ([m|m<-mors context, source m == cpt] ++ [flp m|m<-mors context, target m==cpt])

  --Precondition: ctx a  contains precisely one morphism and it is not V.
   deleteExprForAttr context a parent id
    | isTrue (ctx a)  = error "Fatal: DELETE FROM V is no valid SQL"
    | isIdent (ctx a) = "DELETE FROM "++sqlConcept context ((target.head.mors.ctx) a)++" WHERE "++sqlAttConcept context ((target.head.mors.ctx) a)++"=\\''.addSlashes("++id++").'\\'"
    | otherwise       = "DELETE FROM "++sqlMorName context ((head.mors.ctx) a)++" WHERE "++(sqlExprSrc (ctx a))++"=\\''.addSlashes("++id++").'\\'"

   andNEXISTquer context e m
    | isTrue m  = [ "      AND FALSE" ]
    | otherwise = [ "      AND NOT EXISTS (SELECT * FROM "++(sqlMorName context m)
                  , "                       WHERE "
                    ++ (sqlConcept context (target e))++"."++(sqlAttConcept context (target e))++" = "
                    ++ (sqlMorName context m)++"."++(sqlMorSrc context m)
                  , "                     )"
                  ]

   autoIncQuer context cpt
    = "SELECT max(1+"++(sqlAttConcept context cpt)
      ++") FROM "++(sqlConcept context cpt)++" GROUP BY \\'1\\'"

   insertConcept context cpt var ignore
    = "INSERT "++(if ignore then "IGNORE " else "") ++ "INTO "++(sqlConcept context cpt)++" ("
      ++(sqlAttConcept context cpt)++") VALUES (\\''.addSlashes("++var++").'\\')"

   checkRuls context object
    = (concat
      [ ["  if (!checkRule"++show (nr rul)++"()){"
        ,"    $DB_err=$preErr.'"++(addSlashes (show(explain rul)))++"';"
        ,"  } else"
        ]
      | rul <- rules context
      , or (map (\m -> elem m (mors rul)) -- rule contains an element
                (mors object) -- effected mors  ; SJ: mors yields all morphisms inline.
           )
      ])

   doesExistQuer context object id
    = ( selectExpr context
                   25
                   (sqlAttConcept context (concept object))
                   (sqlAttConcept context (concept object))
                   (Fi [ Tm (mIs (concept object))
                       , Tm (Mp1 ("\\''.addSlashes("++id++").'\\'") (concept object))
                       ]
                   )
      )

   do_del_quer context a str
            = [ "  DB_doquer('DELETE FROM "++(sqlConcept context (concept a))
                , "    WHERE "++(sqlAttConcept context (concept a))++" IN ('."++str++".')"
                ] ++ concat (
                   [ andNEXISTquer context (ctx a) m
                   | m@(Mph _ _ _ _ _ _) <- morsWithCpt context (concept a)
                   ]
                   ) ++
                [ "  ');"]
   termAtts o = [a|a<-attributes o, Tm (Mph _ _ _ _ _ _)<-[ctx a]] -- Dit betekent: de expressie ctx a bevat precies één morfisme.
   selectExprForAttr context a parent id
     = selectExprWithF context (ctx a) (concept parent) id
   selectExprWithF context e cpt id
     = selectExpr context 25 (sqlExprSrc e) (sqlExprTrg e)
                   (F [Tm (Mp1 ("\\''.addSlashes("++id++").'\\'") cpt), e])





   showClasses context nm o
    = [ "class "++phpIdentifier (chain "_" (nm++[name o])) ++" {"] ++
      (map ((++) "  ") (
       ["var $id;"]
       ++ ["var "++phpVar (name a)++";"| a <- attributes o]++
       ["function "++phpIdentifier (chain "_" (nm++[name o]))++"($id=null"
                                  ++  (concat [", "++phpVar (name a)++"=null" | a<-attributes o])
                                  ++"){"
       ,"    $this->id=$id;"]
       ++ ["    $this->"++phpIdentifier (name a)++"="++phpVar (name a)++";"| a <- attributes o]
       ++ (concat (map (map ((++) "    "))
              [ [ "if(!isset("++phpVar (name a)++")){"
                , "  if(isset($id)){"
                , "    $this->"++phpIdentifier (name a)++" = array();"
                , "    foreach(DB_doquer('"++ selectExprForAttr context a o "$id" ++"') as $i=>$v){"
                , "      $this->"++phpIdentifier (name a)++"[]=new "++phpObjRelName nm o a++"($v['" ++ sqlExprTrg (ctx a) ++ "']);"
                , "    }"
                , "  } else $this->"++phpIdentifier (name a)++"=array();"
                , "}"] ++
                concat [ ["if(count($this->"++phpIdentifier (name a)++")==0) $this->"++phpIdentifier (name a)++"[] = new "++phpObjRelName nm o a++"();"]
                       | tot (multiplicities (ctx a))
                       ] ++
                concat [ ["if(count($this->"++phpIdentifier (name a)++")>1){"
                         , "  $last=$this->"++phpIdentifier (name a)++"[count($this->"++phpIdentifier (name a)++")-1];"
                         , "  $this->"++phpIdentifier (name a)++" = array();"
                         , "  $this->"++phpIdentifier (name a)++"[] = $last;"
                         , "}"]
                       | fun (multiplicities (ctx a))
                       ]
              | a <-attributes o
              ]
             )) ++
       ["}"]
       ++ (concat
          [ ["function add_"++phpIdentifier (name a)++"("++phpObjRelName nm o a++" "++phpVar (name a)++"){"
            ,"  return $this->"++phpIdentifier (name a)++(if (fun (multiplicities (ctx a))) then "[0]" else "[]")++"="++phpVar (name a)++";"
            ,"}"
            ,"function getEach_"++phpIdentifier (name a)++"(){"
            ,"  // currently, this returns all concepts.. why not let it return only the valid ones?"
            ,"  $v = DB_doquer('"++selectExpr context
                                           30
                                           (sqlAttConcept context (concept a))
                                           (sqlAttConcept context (concept a))
                                           (Tm (mIs (concept a)))++"');"
            ,"  $res = array();"
            ,"  foreach($v as $i=>$j){"
            ,"    $res[]=$j['"++addSlashes (sqlAttConcept context (concept a))++"'];"
            ,"  }"
            ,"  return $res;"
            ,"}"
            ]
          | a <- attributes o
          ]
          )++
       ["function addGen($type,$value){"
       ]++ [ "  if($type=='"++phpIdentifier (name a)++"') return $this->add_"++phpIdentifier (name a)++"($value);"
           | a <- attributes o
           ] ++
       ["  else return false;"|length (attributes o) > 0] ++
       ["}"
      ]
      )) ++
      [ "}"
      ] ++ (concat [ showClasses context (nm++[name o]) a |a <- attributes o ] )

   phpObjRelName pth o r = phpIdentifier (chain "_" (pth++[name o,name r]))


   getObject context o | null (attributes o) = 
    [ "new object(\""++phpIdentifier (name o)++"\", array()"++mystrs (objectOfConcept context (concept o)) (isOne o)++")"
    ]
   getObject context o =
    [ "new object(\""++phpIdentifier (name o)++"\", array"
    ]
    ++ (concat(mapHeadTail  (mapHeadTail ((++) "   ( ")
                                         ((++) "     "))
                            (mapHeadTail ((++) "   , ")
                                         ((++) "     "))
                            [ concat
                               [["new oRef( new oMulti( " ++ phpBool (Inj `elem` m) ++ ","
                                                          ++ phpBool (Uni `elem` m) ++ ","
                                                          ++ phpBool (Sur `elem` m) ++ ","
                                                          ++ phpBool (Tot `elem` m) ++ " ) // derived from "++(showADL (ctx a))
                               ], (mapHeadTail ((++) "  , ") ((++) "   ") (getObject context a))
                                , ["  ) "]]
                            | a <- attributes o, m <- [multiplicities (ctx a)]
                            ]
       )       )++["   )"++mystrs (objectOfConcept context (concept o)) (isOne o) ++")"]

   isOne o = (fun.multiplicities.disjNF.F) [v (source (ctx o),source (ctx o)),ctx o]
   mapTail f (a:as) = a:(map f as)
   mapHead f (a:as) = (f a):as
   mapHeadTail f1 f2 (a:as) = (f1 a):(map f2 as)
   mystrs Nothing False = ""
   mystrs (Just o) False = ", \""++name o++".php\""
   mystrs Nothing True = ", null, true"
   mystrs (Just o) True = ", \""++name o++".php\", true"
   phpBool b = if b then "true" else "false"
