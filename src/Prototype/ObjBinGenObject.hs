{-# LANGUAGE ScopedTypeVariables#-}
  module Prototype.ObjBinGenObject where
   import Char(toUpper)
   import Strings(chain)
--   import Calc( disjNF, triggers, allClauses, conjuncts, doClause)
   import Calc( doClause)
   import NormalForms (disjNF) --TODO -> correct replacement of Calc?
   import ComputeRule (triggers,conjuncts,allClauses) --TODO -> correct replacement of Calc?

   import Adl
   import ShowADL
   import CC_aux ( tot, fun
      --          , objectOfConcept
                 )
   import Collection (Collection(rd))
   import Prototype.RelBinGenBasics
   import Data.Fspec




 -- The service "getobject" communicates metadata to the interface.

   generateService_getobject :: Fspc -> ObjectDef -> String
   generateService_getobject fSpec object
    = -- vastgesteld op 2008/12/6: gos/=[] dus is de volgende regel overbodig en uitgecommentarieerd:
      -- if null gos then error("Fail (Module ObjBinGenObject): unexpected pattern in generateService_getobject ("++show object++")") else
      "function getobject_"++ phpIdentifier (name object) ++"(){\n  "
      ++ chain "\n  " (("  return "++a):(addLst ";" as)) ++
      "\n  }\n"
       where a:as = [str++"  "| str<-gos]
             gos  = getObject fSpec object
             addLst f (a:[]) = [a++f]
             addLst f (a:as) = a: addLst f as
             addLst f [] = [f]



   objectServices :: Fspc -> String -> ObjectDef -> String
   objectServices fSpec filename object
    = (chain "\n  "
      ([ "<?php // generated with ADL"
       , ""
       , "/********* on "++(show (pos object))
       , showADL object
       , " *********/"
       , ""
       , generateService_getobject fSpec object   -- generate metadata for "object"
       ] ++ showClasses fSpec triggers [] object ++
       [ generateService_getEach fSpec capname object
       , generateService_create  capname object
       , generateService_read    fSpec capname object
       , generateService_update  fSpec capname object
       , generateService_delete  fSpec capname object]
      )) ++ "\n?>"
     where
   -- The expressions of which the population can change inside this object (i.e. the transaction boundary)
      tboundary :: [Expression]
       = rd [objctx o | o<-atts object ]
         where atts o = o: [e| a<-attributes o, e<-atts a]
   -- The compute rules that may be used within this service.
      ecarules
       = [ eca | rule<-vrules fSpec     --was: declaredRules context
               , conjunct<-conjuncts rule
               , clause<-allClauses conjunct
               , eca<-doClause clause  -- was: hc<-hornCs rule clause
               ]
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



   generateService_getEach :: Fspc -> String -> ObjectDef -> String
   generateService_getEach fSpec capname object
    = "function getEach"++phpIdentifier capname++"(){"++
      "\n      return DB_doquer('"++(selectExpr fSpec
                                             25
                                             (sqlExprTrg (ctx object)) -- was:  (sqlAttConcept context (concept object))
                                             (sqlExprSrc (ctx object)) -- was:  (sqlAttConcept context (concept object))
                                             (flp (ctx object)) -- was: (Tm (mIs (concept object)))
                                 )++"');\n  }"



   generateService_create :: String -> ObjectDef -> String
   generateService_create capname object
    = "function create"++phpIdentifier capname++"("++phpIdentifier (name object)++" &$obj){\n  "++
      "    return update"++phpIdentifier capname++"($obj,true);\n  }"



   generateService_read :: Fspc -> String -> ObjectDef -> String
   generateService_read fSpec capname object
    = chain "\n  "
      (["function read"++phpIdentifier capname++"($id){"
       ,"    // check existence of $id"
       ,"    $ctx = DB_doquer('"++(doesExistQuer fSpec object "$id")++"');"
       ,"    if(count($ctx)==0) return false;"
       ,"    $obj = new "++phpIdentifier (name object)++"($id);"
       ,"    return $obj;"
       ,"}"])




   phpVar :: String -> String
   phpVar x = "$"++phpIdentifier x

   generateService_update :: Fspc -> String -> ObjectDef -> String
   generateService_update fSpec capname object
    = chain "\n  "
      (["function update"++phpIdentifier capname++"("++phpIdentifier (name object)++" "++(phpVar (name object))++",$new=false){"
       ,"    global $DB_link,$DB_err,$DB_lastquer;"
       ,"    $preErr= $new ? 'Cannot create new "++(addSlashes (name (concept object)))++": ':'Cannot update "++(addSlashes (name (concept object)))++": ';"
       ,"    DB_doquer('START TRANSACTION');"
       ,"    if($new){ // create a new object"
       ,"      if(!isset("++phpVar (name object)++"->id)){ // find a unique id"
       ,"         $nextNum = DB_doquer('"++(autoIncQuer fSpec (concept object))++"');"
       ,"         "++phpVar (name object)++"->id = @$nextNum[0][0]+0;"
       ,"      }"
       ,"      if(DB_plainquer('" ++
           (insertConcept fSpec (concept object) (phpVar (name object)++"->id") False)
                                ++"',$errno)===false){"
       ,"          $DB_err=$preErr.(($errno==1062) ? '"
          ++(addSlashes (name (concept object))) ++" \\''."++phpVar (name object)++
          "->id.'\\' already exists' : 'Error '.$errno.' in query '.$DB_lastquer);"
       ,"          DB_doquer('ROLLBACK');"
       ,"          return false;"
       ,"      }"
       ,"    }else"]
       ++ updateObject fSpec [name object] object
       ++ checkRuls fSpec object ++
       ["    if(true){ // all rules are met"
       ,"        DB_doquer('COMMIT');"
       ,"        return "++phpVar (name object)++"->id;"
       ,"    }"
       ,"    DB_doquer('ROLLBACK');"
       ,"    return false;"
       ,"}"])

   updateObject :: Object a => Fspc -> [String] -> a -> [String]
   updateObject fSpec nms o =
     ( if null (termAtts o) then [] else
       ["    if(!$new){"
       ,"      // destroy old attribute values"
       ] ++ (concat (map (map ((++) "      "))
              [ [ ""
                , "// When changed, retain the value of "++name a++" in "++phpVar (nm++"_"++name a++"_str")++"."
                , "// It is obtained from "++showADL (objctx a)++"."
                , "$affected = DB_doquer('"++ (selectExprForAttr fSpec a o (phpVar nm++"->id")) ++"');"
                , "$arr=array();"
                , "foreach($affected as $i=>$v){"
                , "    $arr[]='\\''.addSlashes($v['"++(sqlExprTrg (ctx a))++"']).'\\'';"
                , "}"
                , phpVar (nm++"_"++name a++"_str")++"=join(',',$arr);"
                , "// destroy old value of "++name a++" in the database."
                , "DB_doquer( '"++(deleteExprForAttr fSpec a o (phpVar nm++"->id"))++"');"
                ]
              | a <- termAtts o -- door de definitie van termAtts heeft de expressie "ctx a" precies één morfisme.
              ]
             )) ++
       ["    }"]
     )   ++ (concat (map (map ((++) "    "))
              [ [ "foreach("++phpVar nm++"->"++phpIdentifier (name a)++" as $i=>"++phpVar (nm++"_"++name a)++"){"
                ] ++ (concat (map (map ((++) "  "))
                      [ [ "if(isset("++phpVar (nm++"_"++name a)++"->"++phpIdentifier (name as)++"[0]->id)){"
                        , "  if(count(DB_doquer('"++doesExistQuer fSpec a (phpVar (nm++"_"++name a)++"->"++phpIdentifier (name as)++"[0]->id")++"'))==0)"
                        , "    DB_doquer('"++(insertConcept fSpec (concept a) (phpVar (nm++"_"++name a)++"->id") True)++"');"
                        , "    print '"++(insertConcept fSpec (concept a) (phpVar (nm++"_"++name a)++"->id") True)++"';"
                        ] ++ updateObject fSpec (nms++[name a,name as]) as ++
                        [ "}"
                        , phpVar (nm++"_"++name a)++"->id = @"++phpVar (nm++"_"++name a)++"->"++phpIdentifier (name as)++"[0]->id;"
                        ]
                      | as <- attributes a -- De expressie ctx a bevat precies één morfisme.
                      , (Tm (I _ _ _ _)) <- [ctx as]   -- De morfismen uit 'mors' zijn allemaal inline.
                      ]
                     ))
                 ++
                [ "  if(!isset("++phpVar (nm++"_"++name a)++"->id)){"
                , "     $nextNum = DB_doquer('"++autoIncQuer fSpec (concept a)++"');"
                , "     "++phpVar (nm++"_"++name a)++"->id = @$nextNum[0][0]+0;"
                , "  }"
                , "  DB_doquer('"++(insertConcept fSpec (concept a) (phpVar (nm++"_"++name a)++"->id") True)++"');"
                , "  DB_doquer('INSERT IGNORE INTO "
                  ++(sqlMorName fSpec (head (mors m)))++" ("++(sqlExprSrc m)++","++(sqlExprTrg m)++")"
                  ++" VALUES (\\''.addSlashes("++phpVar nm++"->id).'\\'"
                  ++        ",\\''.addSlashes("++phpVar (nm++"_"++name a)++"->id).'\\')');"
                ] ++ updateObject fSpec (nms++[name a]) a ++
                [ "}"
                ]
              | a <- termAtts o -- De expressie ctx a bevat precies één morfisme.
              , m <- [ctx a]   -- De morfismen uit 'mors' zijn allemaal inline.
              ]
             ))
       ++ concat (map (map ((++) "    "))
                      [ [ "if(!$new && strlen("++phpVar (nm++"_"++name a)++"_str))"
                        ] ++ (do_del_quer fSpec a (phpVar (nm++"_"++name a)++"_str"))
                      | a <- termAtts o
                      ]
                 )
       where nm = chain "_" nms



   generateService_delete :: Fspc -> String -> ObjectDef -> String
   generateService_delete fSpec capname object
    = chain "\n  "
      (["function delete"++phpIdentifier capname++"($id){"
       ,"  global $DB_err;"
       ,"  $preErr= 'Cannot delete "++(addSlashes (name (concept object)))++": ';"
       ,"  DB_doquer('START TRANSACTION');"
       ,"  "] ++
       concat (map (map ((++) "    "))
              [ ["$taken = DB_doquer('"++(selectExprWithF fSpec (Tm m) cpt "$id")++"');"
                ,"if(count($taken)) {"
                ,"  $DB_err = 'Cannot delete "++(name object)++": "
                 ++(prag d "\\''.addSlashes($id).'\\'" "\\''.addSlashes($taken[0]['"
                 ++(sqlExprTrg (Tm m))++"']).'\\'" )++"';" -- pragma
                ,"  DB_doquer('ROLLBACK');"
                ,"  return false;"
                ,"}"
                ]
              | cpt <- [concept object]
              , m@(Mph _ _ _ _ _ d) <- morsWithCpt fSpec cpt
              , not (elem (makeInline m) (mors (map ctx (termAtts object))))  -- mors yields all morphisms inline.
              ])
       ++["/*"]
       ++ map show (mors (map ctx (termAtts object)))
       ++["*************"]
       ++ map show (mors (morsWithCpt fSpec (concept object)))
       ++["*/"]
       ++ deleteObject fSpec object ++ checkRuls fSpec object ++
       ["  if(true) {"
       ,"    DB_doquer('COMMIT');"
       ,"    return true;"
       ,"  }"
       ,"  DB_doquer('ROLLBACK');"
       ,"  return false;"
       ,"}"
       ])

 --  deleteObject :: Context -> a -> [String]
   deleteObject fSpec object =
       (concat (map (map ((++) "      "))
              [ [ "$affected = DB_doquer('"++ (selectExprForAttr fSpec a object "$id") ++"');"
                , "$arr=array();"
                , "foreach($affected as $i=>$v){"
                , "    $arr[]='\\''.addSlashes($v['"++(sqlExprTrg (ctx a))++"']).'\\'';"
                , "}"
                , phpVar (name a)++"_str=join(',',$arr);"
                , "DB_doquer ('"++(deleteExprForAttr fSpec a object "$id")++"');"
                ]
              | a <- termAtts object
              ]
             )) ++
       ["  DB_doquer('DELETE FROM "++(sqlConcept fSpec (concept object))
        ++" WHERE "++(sqlAttConcept fSpec (concept object))++"=\\''.addSlashes($id).'\\'');"
       ] ++ (concat (map (map ((++) "  "))
              [ [ "if(strlen("++phpVar (name a++"_str")++"))"
                ] ++ (do_del_quer fSpec a (phpVar (name a++"_str")))
              | a <- termAtts object
              ])) 

   prag (Sgn _ _ _ _ p1 p2 p3 _ _ _ _ _) s1 s2 = (addSlashes p1) ++ s1 ++ (addSlashes p2) ++ s2 ++ (addSlashes p3)
   morsWithCpt fSpec cpt = rd ([m|m<-mors fSpec, source m == cpt] ++ [flp m|m<-mors fSpec, target m==cpt])

  --Precondition: ctx a  contains precisely one morphism and it is not V.
   deleteExprForAttr fSpec a parent id
    | isTrue (ctx a)  = error "Fatal: DELETE FROM V is no valid SQL"
    | isIdent (ctx a) = "DELETE FROM "++sqlConcept fSpec ((target.head.mors.ctx) a)++" WHERE "++sqlAttConcept fSpec ((target.head.mors.ctx) a)++"=\\''.addSlashes("++id++").'\\'"
    | otherwise       = "DELETE FROM "++sqlMorName fSpec ((head.mors.ctx) a)++" WHERE "++(sqlExprSrc (ctx a))++"=\\''.addSlashes("++id++").'\\'"

   andNEXISTquer fSpec e m
    | isTrue m  = [ "      AND FALSE" ]
    | otherwise = [ "      AND NOT EXISTS (SELECT * FROM "++(sqlMorName fSpec m)
                  , "                       WHERE "
                    ++ (sqlConcept fSpec (target e))++"."++(sqlAttConcept fSpec (target e))++" = "
                    ++ (sqlMorName fSpec m)++"."++(sqlMorSrc fSpec m)
                  , "                     )"
                  ]

   autoIncQuer fSpec cpt
    = "SELECT max(1+"++(sqlAttConcept fSpec cpt)
      ++") FROM "++(sqlConcept fSpec cpt)++" GROUP BY \\'1\\'"

   insertConcept fSpec cpt var ignore
    = "INSERT "++(if ignore then "IGNORE " else "") ++ "INTO "++(sqlConcept fSpec cpt)++" ("
      ++(sqlAttConcept fSpec cpt)++") VALUES (\\''.addSlashes("++var++").'\\')"

   checkRuls fSpec object
    = (concat
      [ ["  if (!checkRule"++show (nr rul)++"()){"
        ,"    $DB_err=$preErr.'"++(addSlashes (show(explain rul)))++"';"
        ,"  } else"
        ]
      | rul <- vrules fSpec
      , or (map (\m -> elem m (mors rul)) -- rule contains an element
                (mors object) -- effected mors  ; SJ: mors yields all morphisms inline.
           )
      ])

   doesExistQuer fSpec object id
    = ( selectExpr fSpec
                   25
                   (sqlAttConcept fSpec (concept object))
                   (sqlAttConcept fSpec (concept object))
                   (Fi [ Tm (mIs (concept object))
                       , Tm (Mp1 ("\\''.addSlashes("++id++").'\\'") (concept object))
                       ]
                   )
      )

   do_del_quer fSpec a str
            = [ "  DB_doquer('DELETE FROM "++(sqlConcept fSpec (concept a))
                , "    WHERE "++(sqlAttConcept fSpec (concept a))++" IN ('."++str++".')"
                ] ++ concat (
                   [ andNEXISTquer fSpec (ctx a) m
                   | m@(Mph _ _ _ _ _ _) <- morsWithCpt fSpec (concept a)
                   ]
                   ) ++
                [ "  ');"]
   termAtts o = [a|a<-attributes o, Tm (Mph _ _ _ _ _ _)<-[ctx a]] -- Dit betekent: de expressie ctx a bevat precies één morfisme.
   selectExprForAttr fSpec a parent id
     = selectExprWithF fSpec (ctx a) (concept parent) id
   selectExprWithF fSpec e cpt id
     = selectExpr fSpec 25 (sqlExprSrc e) (sqlExprTrg e)
                   (F [Tm (Mp1 ("\\''.addSlashes("++id++").'\\'") cpt), e])


   -- | The function showClasses defines the PHP class of an object o and also (recursively) the
   --   PHP-classes of all subordinate objects (i.e. the attributes) of o.
   --   context  : the Context of object o. In due time, this will be replaced by an Fspc
   --   triggers : a [possibly empty] set of triggers, that is used to generate automated functionality.
   --              Precondition: This set contains precisely those triggers that may be used within the transaction
   --              boundary of the class.
   --              In due time, this parameter will become a selection from the entire set of triggers in Fspc.
   --   nms      : the name trail of all super-objects until the root of this service. This is used to generate unique names for every field.
   --   o        : the object to be transformed in a class.

   showClasses fSpec triggers nms o
    = [ "class "++phpIdentifier (chain "_" (nms++[name o])) ++" {"] ++
      (map ((++) "  ") (
       ["var $id;"]
       ++ ["var "++phpVar (name a)++";"| a <- attributes o]++
       ["function "++phpIdentifier (chain "_" (nms++[name o]))++"($id=null"
                                  ++  (concat [", "++phpVar (name a)++"=null" | a<-attributes o])
                                  ++"){"
       ,"    $this->id=$id;"]
       ++ ["    $this->"++phpIdentifier (name a)++"="++phpVar (name a)++";"| a <- attributes o]
       ++ (concat (map (map ((++) "    "))
              [ [ "if(!isset("++phpVar (name a)++")){"
                , "  if(isset($id)){"
                , "    $this->"++phpIdentifier (name a)++" = array();"
                , "    foreach(DB_doquer('"++ selectExprForAttr fSpec a o "$id" ++"') as $i=>$v){"
                , "      $this->"++phpIdentifier (name a)++"[]=new "++phpObjRelName nms o a++"($v['" ++ sqlExprTrg (ctx a) ++ "']);"
                , "    }"
                , "  } else $this->"++phpIdentifier (name a)++"=array();"
                , "}"] ++
                concat [ ["if(count($this->"++phpIdentifier (name a)++")==0) $this->"++phpIdentifier (name a)++"[] = new "++phpObjRelName nms o a++"();"]
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
          [ ["function add_"++phpIdentifier (name a)++"("++phpObjRelName nms o a++" "++phpVar (name a)++"){"
            ,"  return $this->"++phpIdentifier (name a)++(if (fun (multiplicities (ctx a))) then "[0]" else "[]")++"="++phpVar (name a)++";"
            ,"}"
      -- The following function fills the drop-down boxes of an individual field in edit mode
            ,"function getEach_"++phpIdentifier (name a)++"(){"
            ,"  // currently, this returns all concepts.. why not let it return only the valid ones?"
            ,"  $v = DB_doquer('"++selectExpr fSpec
                                           30
                                           (sqlAttConcept fSpec (concept a))
                                           (sqlAttConcept fSpec (concept a))
                                           (Tm (mIs (concept a)))++"');"
            ,"// triggers:"++[]
            ,"  $res = array();"
            ,"  foreach($v as $i=>$j){"
            ,"    $res[]=$j['"++addSlashes (sqlAttConcept fSpec (concept a))++"'];"
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
      ] ++ (concat [ showClasses fSpec triggers (nms++[name o]) a |a <- attributes o ] )

   phpObjRelName pth o r = phpIdentifier (chain "_" (pth++[name o,name r]))


   getObject fSpec o | null (attributes o) = 
    [ "new object(\""++phpIdentifier (name o)++"\", array()"++mystrs (objectOfConcept fSpec (concept o)) (isOne o)++")"
    ]
   getObject fSpec o =
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
                               ], (mapHeadTail ((++) "  , ") ((++) "   ") (getObject fSpec a))
                                , ["  ) "]]
                            | a <- attributes o, m <- [multiplicities (ctx a)]
                            ]
       )       )++["   )"++mystrs (objectOfConcept fSpec (concept o)) (isOne o) ++")"]


   objectOfConcept :: Fspc -> Concept -> Maybe ObjectDef
   objectOfConcept fSpec cpt = if length os == 0 then Nothing else Just (head os)
     where os = [o|o<-serviceG fSpec,concept o == cpt]
--        copied from CC_aux (and commented, becuase no one else used it)
--        objectOfConcept :: Context -> Concept -> Maybe ObjectDef
--        objectOfConcept context cpt = if length os == 0 then Nothing else Just (head os)
--          where os = [o|o<-attributes context,concept o == cpt]

   isOne o = (fun.multiplicities.disjNF.F) [v (source (ctx o),source (ctx o)),ctx o]
   mapTail f (a:as) = a:(map f as)
   mapHead f (a:as) = (f a):as
   mapHeadTail f1 f2 (a:as) = (f1 a):(map f2 as)
   mystrs Nothing False = ""
   mystrs (Just o) False = ", \""++name o++".php\""
   mystrs Nothing True = ", null, true"
   mystrs (Just o) True = ", \""++name o++".php\", true"
   phpBool b = if b then "true" else "false"
