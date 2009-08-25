{-# LANGUAGE ScopedTypeVariables#-}
  module Prototype.ObjBinGenObject(objectServices) where
   import Char(toUpper)
   import Strings(chain)
   import NormalForms (disjNF)
   import Auxiliaries (eqCl,sort')
   import Adl (source,target
              ,Concept(..),ObjectDef(..),Numbered(..),Declaration(..)
              ,Identified(..),mors,explain,Morphism(..),Prop(..)
              ,Object(..),multiplicities,isIdent,Expression(..),mIs
              ,isTrue,makeInline,flp)
   import ShowADL
   import CC_aux ( fun )
   import Collection (Collection(rd,(>-)))
   import Prototype.RelBinGenBasics(sqlExprSrc,sqlExprTrg,naming,selectExprBrac,indentBlock
     ,sqlRelPlugs,addToLast,isOne,phpIdentifier,sqlAttConcept,selectExpr,sqlConcept
     ,sqlMorName,addSlashes,sqlMorSrc,commentBlock,sqlPlugFields)
   import Data.Fspec
   import Data.Plug
   -- import Debug.Trace
   import Version (versionbanner)


   objectServices :: Fspc
                  -> String -- filename
                  -> ObjectDef
                  -> String
   objectServices fSpec _ o
    = (chain "\n  "
      ([ "<?php // generated with "++versionbanner
       , ""
       , "/********* on "++(show (pos o))
       , showADL o
       , " *********/"
       , ""
       ] ++ showClasses fSpec o ++
       ( if isOne o
         then []
         else [ generateService_getEach fSpec capname o
              , generateService_create        capname o
              , generateService_read    fSpec capname o
              , generateService_delete  fSpec capname o]
       )
      )) ++ "\n?>"
     where
   -- The compute rules that may be used within this service.
      capname = if null (name o)
                then error ("!Fail: (Module ObjBinGenObject) empty name in object:\n"++show o)
                else (toUpper (head (name o))):(tail (name o))

   generateService_getEach :: Fspc -> String -> ObjectDef -> String
   generateService_getEach fSpec capname o
    = "function getEach"++phpIdentifier capname++"(){"++
      "\n      return DB_doquer('"++(selectExpr fSpec
                                             25
                                             (sqlExprTrg fSpec (ctx o))
                                             (sqlExprSrc fSpec (ctx o))
                                             (flp (ctx o))
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

   generateService_delete :: Fspc -> String -> ObjectDef -> String
   generateService_delete fSpec capname object
    = chain "\n  "
      (["function delete"++phpIdentifier capname++"($id){"
       ,"  global $DB_err;"
       ,"  $preErr= 'Cannot delete "++(addSlashes (name (concept object)))++": ';"
       ,"  DB_doquer('START TRANSACTION');"
       ,"  "] ++
       concat (map (indentBlock 4)
              [ ["$taken = DB_doquer('"++(selectExprWithF fSpec (Tm mpm) cpt "$id")++"');"
                ,"if(count($taken)) {"
                ,"  $DB_err = 'Cannot delete "++(name object)++": "
                 ++(prag d "\\''.addSlashes($id).'\\'" "\\''.addSlashes($taken[0]['"
                 ++(sqlExprTrg fSpec (Tm mpm))++"']).'\\'" )++"';" -- pragma
                ,"  DB_doquer('ROLLBACK');"
                ,"  return false;"
                ,"}"
                ]
              | cpt <- [concept object]
              , mpm@(Mph _ _ _ _ _ d) <- morsWithCpt fSpec cpt
              , not (elem (makeInline mpm) (mors (map ctx (termAtts object))))  -- mors yields all morphisms inline.
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

   deleteObject ::  Fspc -> ObjectDef -> [String]
   deleteObject fSpec object =
       (concat (map (map ((++) "      "))
              [ [ "$affected = DB_doquer('"++ (selectExprForAttr fSpec a object "$id") ++"');"
                , "$arr=array();"
                , "foreach($affected as $i=>$v){"
                , "    $arr[]='\\''.addSlashes($v['"++(sqlExprTrg fSpec (ctx a))++"']).'\\'';"
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
   prag :: Declaration -> String -> String -> String
   prag (Sgn _ _ _ _ p1 p2 p3 _ _ _ _ _) s1 s2 = (addSlashes p1) ++ s1 ++ (addSlashes p2) ++ s2 ++ (addSlashes p3)
   prag _ _ _ = error "Error in prag in ObjBinGenObject.hs: pattern not matched"
   morsWithCpt :: Fspc -> Concept -> [Morphism]
   morsWithCpt fSpec cpt = rd ([mpm|mpm<-mors fSpec, source mpm == cpt] ++ [flp mpm|mpm<-mors fSpec, target mpm==cpt])

  --Precondition: ctx a  contains precisely one morphism and it is not V.
   deleteExprForAttr :: forall t a. (Object a) => Fspc -> a -> t -> [Char] -> [Char]
   deleteExprForAttr fSpec a _ var
    | isTrue (ctx a)  = error "Fatal: DELETE FROM V is no valid SQL"
    | isIdent (ctx a) = "DELETE FROM "++sqlConcept fSpec ((target.head.mors.ctx) a)++" WHERE "++sqlAttConcept fSpec ((target.head.mors.ctx) a)++"=\\''.addSlashes("++var++").'\\'"
    | otherwise       = "DELETE FROM "++sqlMorName fSpec ((head.mors.ctx) a)++" WHERE "++(sqlExprSrc fSpec (ctx a))++"=\\''.addSlashes("++var++").'\\'"

   andNEXISTquer :: Fspc -> Expression -> Morphism -> [[Char]]
   andNEXISTquer fSpec e' mpm
    | isTrue mpm = [ "      AND FALSE" ]
    | otherwise  = [ "      AND NOT EXISTS (SELECT * FROM "++(sqlMorName fSpec mpm)
                   , "                       WHERE "
                     ++ (sqlConcept fSpec (target e'))++"."
                     ++(sqlAttConcept fSpec (target e'))++" = "
                     ++ (sqlMorName fSpec mpm)++"."++(sqlMorSrc fSpec mpm)
                   , "                     )"
                   ]

   autoIncQuer :: Fspc -> Concept -> [Char]
   autoIncQuer fSpec cpt
    = "SELECT max(1+"++(sqlAttConcept fSpec cpt)
      ++") FROM "++(sqlConcept fSpec cpt)++" GROUP BY \\'1\\'"

   checkRuls :: Fspc -> ObjectDef -> [String]
   checkRuls fSpec object
    = (concat
      [ ["  if (!checkRule"++show (nr rul)++"()){"
        ,"    $DB_err=$preErr.'"++(addSlashes (show(explain rul)))++"';"
        ,"  } else"
        ]
      | rul <- vrules fSpec
      , or (map (\mpm -> elem mpm (mors rul)) -- rule contains an element
                (mors object) -- effected mors  ; SJ: mors yields all morphisms inline.
           )
      ])
   
   doesExistQuer :: Fspc -> ObjectDef -> [Char] -> String
   doesExistQuer fSpec object var
    = ( selectExpr fSpec
                   25
                   (sqlAttConcept fSpec (concept object))
                   (sqlAttConcept fSpec (concept object))
                   (Fi [ Tm (mIs (concept object))
                       , Tm (Mp1 ("\\''.addSlashes("++var++").'\\'") (concept object))
                       ]
                   )
      )
   
   do_del_quer :: Fspc -> ObjectDef -> [Char] -> [[Char]]
   do_del_quer fSpec a str
            = [ "  DB_doquer('DELETE FROM "++(sqlConcept fSpec (concept a))
                , "    WHERE "++(sqlAttConcept fSpec (concept a))++" IN ('."++str++".')"
                ] ++ concat (
                   [ andNEXISTquer fSpec (ctx a) mpm
                   | mpm@(Mph _ _ _ _ _ _) <- morsWithCpt fSpec (concept a)
                   ]
                   ) ++
                [ "  ');"]
   termAtts :: ObjectDef -> [ObjectDef]
   termAtts o = [a|a<-attributes o, Tm (Mph _ _ _ _ _ _)<-[ctx a]] -- Dit betekent: de expressie ctx a bevat precies één morfisme.
   selectExprForAttr :: Fspc -> ObjectDef -> ObjectDef -> [Char] -> String
   selectExprForAttr fSpec a parent var
     = selectExprWithF fSpec (ctx a) (concept parent) var
   selectExprWithF :: Fspc
                   -> Expression
                   -> Concept
                   -> String
                   -> String
   selectExprWithF fSpec e' cpt var
     = selectExpr fSpec 25 (sqlExprSrc fSpec e') (sqlExprTrg fSpec e')
                   (F [Tm (Mp1 ("\\''.addSlashes("++var++").'\\'") cpt), e'])


   -- | The function showClasses defines the PHP class of an object o and also (recursively) the
   --   PHP-classes of all subordinate objects (i.e. the attributes) of o.
   --   context  : the Context of object o. In due time, this will be replaced by an Fspc
   --   triggers : a [possibly empty] set of triggers, that is used to generate automated functionality.
   --              Precondition: This set contains precisely those triggers that may be used within the transaction
   --              boundary of the class.
   --              In due time, this parameter will become a selection from the entire set of triggers in Fspc.
   --   nms      : the name trail of all super-objects until the root of this service. This is used to generate unique names for every field.
   --   o        : the object to be transformed in a class.
   showClasses :: Fspc -> ObjectDef -> [String]
   showClasses fSpec o
    = [ "class "++myName ++" {"] ++
      (map ((++) "  ") (
       ["protected $_id=false;"| not (isOne o)]
       ++ ["private $_"++phpIdentifier (name a)++";"| a <- attributes o]++
       ["function "++myName++"(" ++ (if isOne o then "" else "$id=null, ")
                                  ++ (chain ", " [phpVar (name a)++"=null" | a<-attributes o])
                                  ++"){"
       ]++["  $this->_id=$id;" | not (isOne o)]
       ++ ["  $this->_"++phpIdentifier (name a)++"="++phpVar (name a)++";"| a <- attributes o]
       ++ concat (take 1 [  [ "  if(!isset("++phpVar (name a')++")"++(if isOne o then "" else "&& isset($id)")++"){"
                            , "    // get a "++(myName)++" based on its identifier"
                            , "    // this function will fill the attributes"
                            ] ++
                            ( if null [a|a<-objats o,Uni `elem` multiplicities (objctx a)]
                              then ["    $me=array();"]
                              else []
                            ) ++ indentBlock 4 (doPhpGet fSpec
                                                         "$me"
                                                         0
                                                         (o  {objnm =if isOne o then "1" else "$id"
                                                             ,objctx=Tm(mIs(concept o))
                                                             ,objats=[]}
                                                         )
                                                         o
                                               )
                            ++["    $this->set_"++phpIdentifier (name a)++"($me['"++(name a)++"']);"
                              |a<-attributes o]
                            ++["  }"]
                         | a' <- attributes o]
                 ) ++
       ["}"
       ,""
       ,"function save(){"]++
       [] ++
       indentBlock 2 (saveTransaction fSpec o) ++
       [ "}"]
       ++ (concat
          [ ["function add_"++phpIdentifier (name a)
                    ++"("++phpObjRelName [] o a++" "++phpVar (name a)++"){"
            ,"  return $this->"++phpIdentifier (name a)
                               ++(if (fun (multiplicities (ctx a))) then "[0]" else "[]")
                               ++"="++phpVar (name a)++";"
            ,"}"
      -- The following function fills the drop-down boxes of an individual field in edit mode
            ,"function getEach_"++phpIdentifier (name a)++"(){"
            ,"  $v = DB_doquer('"++selectExpr fSpec
                                              30
                                              (sqlAttConcept fSpec (concept a))
                                              (sqlAttConcept fSpec (concept a))
                                              (Tm (mIs (concept a)))++"');"
            ,"  $res = array();"
            ,"  foreach($v as $i=>$j){"
            ,"    $res[]=$j['"++addSlashes (sqlAttConcept fSpec (concept a))++"'];"
            ,"  }"
            ,"  return $res;"
            ,"}"
            ,"function set_"++phpIdentifier (name a)++"($val){"
            ,"  $this->_"++phpIdentifier (name a)++"=$val;"
            ,"}"
            ,"function get_"++phpIdentifier (name a)++"(){"
            ,"  return ($this->_"++phpIdentifier (name a)++");"
            ,"}"
            ]
          | a <- attributes o
          ]
          )++
       ( if isOne o
         then []
         else ["function setId($id){"
              ,"  $this->_id=$id;"
              ,"  return $this->_id;"
              ,"}"
              ,"function getId(){"
              ,"  return $this->_id;"
              ,"}"]
       ) ++
       ["function addGen($type,$value){"
       ]++ [ "  if($type=='"++phpIdentifier (name a)++"') return $this->add_"
               ++ phpIdentifier (name a)++"($value);"
           | a <- attributes o
           ] ++
       ["  else return false;"|length (attributes o) > 0] ++
       ["}"]
      )) ++
      [ "}"
      ]
    where myName = name o
   
   saveTransaction :: Fspc -> ObjectDef -> [String]
   saveTransaction fSpec object
    = [ "DB_doquer('START TRANSACTION');"] ++
      ( if isOne object then [] else
          ["if($this->getId()===false){ // find a unique id"
          ,"  $nextNum = DB_doquer('"++(autoIncQuer fSpec (concept object))++"');"
          ,"  $this->setId(@$nextNum[0][0]+0);"
          ,"  $new=true;"
          ,"} else $new=false;"]
      ) ++
      [ "$me=array(\"id\"=>" ++ (if isOne object then "1" else "$this->getId()")
        ++ concat [ ", "++show (name a) ++ " => $this->_"++phpIdentifier (name a)
                  | a<-objats object ]
        ++ ");"
      , "$preErr='';"]
      ++ saveCodeLines
      ++ (commentBlock (["Attributes that have not been saved are:"
                        ,"----------------------------------------"]++map name unsavedAtts))
      ++ checkRuls fSpec object ++
      [ "if(true){ // all rules are met"
      , "  DB_doquer('COMMIT');"
      , "  return "++ if isOne object then "true;" else "$this->getId();"
      , "}"
      , "DB_doquer('ROLLBACK');"
      , "return false;"
      ]
    where
      saveCodeLines = concat $ map fst saveCodeElems
      saveCodeAtts  = rd $ concat $ map snd saveCodeElems
      saveCodeElems = [saveCodeElem object p | p<-objPlugs fSpec object]
      allAtts :: ObjectDef->[ObjectDef]
      allAtts o = objats o ++ concat (map allAtts (objats o))
      unsavedAtts :: [ObjectDef]
      unsavedAtts = allAtts object >- saveCodeAtts

   saveCodeElem :: ObjectDef->Plug->([String],[ObjectDef])
   saveCodeElem object plug
    = ( -- only delete if we have ALL information needed for refill, so use fullOccurences
        -- if we delete more, the plug would violate its transaction boundary
          concat (map delcode fullOccurences)
       ++ concat (map delUpdt [(o,s)|(((o,s),_):_)<-occurences >- fullOccurences,fldnull s])
        -- only insert if every mandatory field has a value, so use largeOccurences
       ++ concat (map inscode occurences)
      , -- function should send back all ObjectDef's that have been saved everywhere properly
       rd $ map (fst . snd) (concat occurences))
    where
      delUpdt (o,s)
       = nestTo o (\var -> [ "DB_doquer(\"UPDATE `"++name plug++"` SET `"++fldname s++
                             "`=NULL WHERE `"++fldname s++
                             "`='\".addslashes("++var++"['id']).\"'\",5);"]
                  )
      delcode [] = [] -- there are probably no empty groups, and we cannot delete them anyways
      delcode (((a,f),_):_) -- this code is a lot like updel
       = nestTo a
                (\var->["DB_doquer(\"DELETE FROM `"++name plug++"` WHERE `"++(fldname f)
                        ++"`='\".addslashes("++var++"['id']).\"'\",5);"])
      inscode [] = [] -- there are probably no empty groups, and we cannot modify them anyways
      inscode is@(((a,s),_):_)
       = if not (isLargeOccurance is) && null keys  -- cannot generate code...
         then ["// no code for "++name a++","++fldname s++" in "++name plug]
         else
         nestTo a
                (\var
                  -> concat [indentBlock (2*n)
                                         ( ( if fldnull f
                                             then (:)("if(count("++var++"['"++(name o)++"'])==0) "
                                                      ++var++"['"++(name o)++"'][] = null;")
                                             else id
                                           )
                                           ["foreach  (" ++ var ++ "['"++(name o)++"'] as $"
                                             ++(phpIdentifier $ name o)++"){" -- de } staat verderop
                                           ]
                                         )
                            | ((o,f),n) <-zip nunios [0..]] ++
                     indentBlock (2*length nunios)
                     ( if isLargeOccurance is -- attempt INSERT first
                       then ( if (isFullOccurance is || null keys)
                              then -- na een DELETE, of bij null keys kán geen UPDATE
                                   -- reden: een UPDATE heeft een key nodig om zich te hechten
                                   -- (en na een delete hebben we die waarde nét weggegooid)
                                   [ "$lastid="++insQuery var++";" ] ++
                                   
                                   [ "if($lastid!==false && !isset("++var++"['id']))"
                                   , "  "++var++"['id']=$lastid;" ]
                              else
                              [ insQuery var ++";"
                                -- zoals hierboven gezegd: een key is nodig voor een UPDATE
                                -- daarom moeten we checken of var[id] een waarde heeft
                                -- zo niet, dan is ofwel de key null, ofwel de te inserten waarde
                              , "if(mysql_affected_rows()==0 && "++var++"['id']!=null){"
                              , "  //nothing inserted, try updating:"
                              , "  "++updQuery var++";"
                              , "}"
                              ]
                            )
                       else -- we cannot INSERT because we do not have all required information
                            [ "if(isset("++var++"['id']))"
                            , "  "++ updQuery var++";"
                            ]
                     ) ++
                     [take (2*n) (repeat ' ')                          -- de { staat een stuk eerder
                                              ++ "}"
                     | (_,n)<-reverse $ zip nunios [0..]
                     ]
                )
         where attrs  = ownAts ++ -- ook het identiteit-veld toevoegen (meestal de SQL-`id`)
                        [ (a, s) | s `notElem` (map snd ownAts)]
               names  = chain "," $ map (fldname.snd) attrs
               keys :: [(ObjectDef,SqlField)]
               keys   = if length attrs==1
                        then [] -- het kan gebeuren dat er maar één attr is
                                -- aangezien dit keys voor UPDATE zijn, stelt deze key dan niet
                                -- echt veel meer voor. Gevolg van deze keuze is dat de UPDATE
                                -- expressies met UPDATE .. SET (lege lijst) WHERE key=val
                                -- niet meer voorkomen, netjes weggefilterd worden
                        else filter (iskey.snd) $ reverse attrs -- eerst de voor de hand liggende
               key    = if null keys
                        then error ("\nObjBinGenObject-saveCodeElem-inscode: Cannot get a key for the plug "++name plug)
                        else head keys
                -- nunios: Not UNI ObjectS: objects that are not Uni
               nunios = [(o,f)|(o,f)<-ownAts, a/=o, not $ Uni `elem` multiplicities (objctx o)]
               ownAts = map snd is
               insQuery :: String -> String  -- (var as returned by nestTo) -> (query)
               insQuery = query ("INSERT IGNORE INTO `"++name plug++"` ("++ names ++") VALUES (")
                                (\f -> fldnull f || fldauto f)
                                (\_ s'->s')
                                ")"
                                (\_->True)
               updQuery var = query ("UPDATE `"++name plug++"` SET ")
                                    (\f -> fldnull f && not (f==s))
                                    (\f s'->f++"="++s')
                                    (" WHERE `"++(fldname$snd key)
                                      ++"`='\".addslashes("++varname var (fst key)++").\"'")
                                    (\o->fst key/=o)
                                    var
               varname var o = if a == o
                               then var++"['id']"
                               else ( if Uni `elem` multiplicities (objctx o)
                                      then var++"['"++(name o)++"']"
                                      else "$"++(phpIdentifier $ name o)
                                    ) ++
                                    ( if null $ objats o
                                      then ""
                                      else "['id']"
                                    )
               query :: String  -- header: initial part of the query (such as "INSERT..")
                                -- dit is exclucief de " aan het begin
                     -> ( SqlField -> Bool) -- should we test for NULL?
                     -> ( String  -- fieldname: escaped name of the Sql Field
                        ->String  -- fieldvalue: PHP value (NULL or '.addslashes(..).')
                        ->String) -- result to be part-of-query, such as 'fieldname=fieldvalue'
                     -> String  -- tail: final part of the query (such as "WHERE..")
                                -- dit is exclucief de " aan het eind
                     -> (ObjectDef->Bool) -- should this object participate in the query?
                     -> String  -- variable name given by nestTo
                     -> String  -- resulting query
               query hdr cond fnc tl selector var
                = "DB_doquer(\"" ++ hdr ++
                  chain ", "
                        [ fnc ("`"++fldname f++"`")
                          ( if cond f
                            then "\".(isset(" ++ varname var o ++ ")?\"'\".addslashes("
                                 ++ varname var o ++ ").\"'\":\"NULL\").\""
                            else "'\".addslashes("++varname var o++").\"'"
                          )
                        | (o,f)<-attrs
                        , selector o
                        ] ++ tl ++"\", 5)"
      nestTo :: ObjectDef -> (String->[String]) -> [String]
      nestTo attr fnc = if null nt then error ("saveCodeElem: Cannot nestTo "++show attr++" (ObjBinGenObject)")
                        else head nt
        where nt = nestToRecur attr fnc object "$me" 0
      nestToRecur :: ObjectDef -> (String->[String]) -> ObjectDef -> String -> Integer -> [[String]]
      nestToRecur attr fnc a var d
        | attr==a   = [ if null (objats a) then fnc var else fnc (var)]
        | otherwise = [ if Uni `elem` multiplicities (objctx a') then ans
                        else ["foreach("++var++"['"++name a'++"'] as $i"++show d
                                ++"=>$v"++show d++"){"]
                             ++ indentBlock 2 ans ++
                             ["}"]
                      | a'<-objats a
                      , let mvar = if Uni `elem` multiplicities (objctx a')
                                   then var++"['"++name a'++"']"
                                   else "$v"++show d
                      , ans<-nestToRecur attr fnc a' (mvar) (d+1)]
      occurences = (eqCl fst) $ rd $ concat (map (plugAts plug object) (objats object))
      fullOccurences = filter isFullOccurance occurences
      isFullGroup set a = null (set >- (map (snd . snd) a ++ [snd$fst$head a]))
      isFullOccurance = isFullGroup (fields plug)
      isLargeOccurance = isFullGroup requiredFields
      requiredFields  = [f| f <- fields plug
                          , Tot `elem` multiplicities (fldexpr f)
                          -- strictly speaking, ident fields ARE required, but we can fill in their
                          -- values easily since their value IS their source
                          , not $ isIdent $ fldexpr f
                          ]
   plugAts :: Plug -> ObjectDef           -- parent (wrong values are allowed, see source)
              -> ObjectDef                -- object itself
              -> [((ObjectDef, SqlField), -- source (may include the wrong-valued-'parent')
                 (ObjectDef,SqlField))]   -- target
   plugAts plug p o = ( [ ((o,sf),(o,tf))
                        | not $ isIdent $ objctx o
                        , (sf,tf)<-sqlPlugFields plug (Tm$mIs$target$objctx o)
                        ] ++
                        [ ((p,sf),(o,tf))
                        | not $ isIdent $ objctx o
                        , (sf,tf)<-sqlPlugFields plug $ objctx o
                        ]
                      ) ++ concat (map (plugAts plug o) (objats o))
                           
   objPlugs :: Fspc -> ObjectDef -> [Plug]
   objPlugs fSpec object
     = [plug|plug<-plugs fSpec,((_,_),(_,_))<-take 1 $ plugAts plug object object]
   
   isObjUni :: ObjectDef -> Bool
   isObjUni obj = Uni `elem` multiplicities (objctx obj)
   
   doPhpGet :: Fspc -> String->Integer-> ObjectDef -> ObjectDef -> [String]
   doPhpGet fSpec objVar depth objIn objOut
     = (if (null unisNeeded) then []
        else
          doQuer (objVar++"=firstRow") sqlUnis
       )++ 
       concat [singleQuer aout
              |aout <- arrsNeeded
              ] ++
       concat [ if isObjUni ( aout)
                then [idvar++" = "++var++";"] ++ nest
                else ["foreach("++mname aout++" as $i"++show depth
                      ++"=>&$v"++show depth++"){"]++
                     indentBlock 2 nest ++
                     ["}"
                     ,"unset("++var++");"]
              |aout <- depthNeeded
              ,let (var,idvar) = if isObjUni aout
                                 then (mname aout,"$v"++show depth)
                                 else ("$v"++show depth,"$v"++show depth++"['id']")
              ,let nest = nesting var idvar aout
              ,not$null$nest]
     where
       unisNeeded = [aout|aout<-allNeeded,isObjUni ( aout)]
       arrsNeeded = [aout|aout<-allNeeded,not$isObjUni aout]
       depthNeeded= [aout|aout<-objats objOut,null[a|a<-objats objIn,a `msubset` aout]]
       allNeeded  = [aout|aout<-objats objOut,null[a|a<-objats objIn,objctx a==objctx aout]]
       doQuer fl [] = error ("ObjBinGenObject.doPhpGet: doQuer has no query for "++fl);
       doQuer firstLine
              quer = addToLast "\"));"
                               ([ firstLine++"(DB_doquer(\""++head quer]
                                   ++ map ((++) (take (12+length firstLine) (repeat ' ')))
                                          (tail quer)
                               )
       idAt = objOut{objctx=targetCpt,objnm="id",objats=[]}
       targetCpt = Tm$mIs$target$objctx objOut
       sqlUnis = doSqlGet fSpec objIn (objOut{objats= idAt : map trunc unisNeeded,objctx=targetCpt})
       trunc        att = att{objats=[]}
       truncKeepUni = trunc -- de SQL functie gaat (nog) niet goed om met recursie!
                            -- bovendien doet deze PHP functie dit evenmin als de recursie
                            -- verder gaat dan 1 diep, dwz dat onderstaande definitie:
       -- truncKeepUni att = att{objats=[truncKeepUni a|a<-objats att,isObjUni ( a)]}
                            -- ook niet werkt in deze functie, en dat onderstaande:
       -- truncKeepUni att = att{objats=[trunc a|a<-objats att,isObjUni ( a)]}
                            -- alleen niet werkt in de SQL functie
       truncKeepUniNamed nm att = att{objats=[a{objnm=nm++"["++(show$name a)++"]"}|a<-objats (truncKeepUni att)]}
       mname aout = objVar++"['"++(name aout)++"']"
       -- singleQuer below: code for leaves or for nodes (in the objDef tree) is different. For leaves: use firstCol to show them
       singleQuer aout = doQuer (mname aout ++ "="++(if null(objats aout) then "firstCol" else ""))
                                (doSqlGet fSpec
                                          objIn
                                          objOut{objats=[truncKeepUni (if null(objats aout) then aout else aout{objnm="id"})]})
       nesting var idvar aout = (doPhpGet fSpec
                                          var
                                          (depth+1)
                                          (truncKeepUniNamed (var) aout){objnm=idvar}
                                          aout)
       msubset i o= and ((objctx i==objctx o) -- must contain same elems
                        :[or [msubset i' o'
                             |i'<-objats i -- and at least every attribute of o must be in i
                             ]
                         |o'<-objats o
                         ]
                        )
   
   flattenOdef :: ObjectDef->ObjectDef
   flattenOdef objDef = objDef{objats=map prefixwithrel atts,objctx=Tm$mIs$source$objctx objDef}
     where atts :: [ObjectDef]
           atts = objats objDef ++ concat ((map (objats . flattenOdef) $ objats objDef))
           prefixwithrel :: ObjectDef->ObjectDef
           prefixwithrel att = att{objctx = disjNF (F [objctx objDef,objctx att])}

    -- onderstaande functie selecteert alle attributen plat op de source gemapt, dwz:
    -- a=[b,c] wordt I=[a;b,a;b], wat alleent hetzelfde is als a UNI is
   doSqlGet :: Fspc -> ObjectDef -> ObjectDef -> [String]
   doSqlGet fSpec objIn objOut = ["SELECT DISTINCT " ++ head fieldNames      ]
                                    ++ map ((++) "     , ") (tail fieldNames  ) ++
                                (if null tbls
                                 then []
                                 else ["  FROM " ++ head (fst (head tbls))]
                                         ++ (indentBlock 7 (tail (fst (head tbls))))
                                         ++ concat (map joinOn (tail tbls)) ++
                                      (if isOne' then [] else [" WHERE " ++ snd (head tbls)])
                                )
      where comboGroups'::[((Plug,(ObjectDef,SqlField)),[(ObjectDef,SqlField)])]
            comboGroups'     = reduce (sort' (length) (eqCl fst combos))
            comboGroups = keyGroups ++ (comboGroups' >- keyGroups)
            keyGroups   = take 1 ( [gr|gr@((_,(_,s)),_)<-comboGroups',not $ fldnull s] ++ 
                                   [((p,(objIn,s)),[(objIn,t)])
                                   | (p,s,t)<-sqlRelPlugs fSpec (Tm$mIs$target (objctx objIn))]
                                   -- in het geval van I[ONE] geeft sqlRelPlugs niets terug
                                   -- dan hebben we dus geen keyGroup, maar dat geeft niet voor ONE
                                   -- in andere gevallen geeft dat wel.
                                   -- reden: het eerste item in de SELECT expressie wordt in de
                                   -- FROM neergezet ipv in de LEFT JOIN, met een WHERE daarbij
                                   -- het gevolg is dat zo de mogelijkheid ontstaat dat er geen
                                   -- resultaten teruggegeven worden, terwijl een rij met NULL
                                   -- verwacht werd. Dit levert een PHP-runtime fout, omdat PHP
                                   -- tenminste 1 rij verwacht. (bovendien is het semantisch fout)
                                   -- Voor debuggen onderstaande trace regel uitcommentaren
                                   -- en Debug.trace aan de imports toevoegen
                                   -- ++ trace ("Geen keyGroup voor "++name objOut) []
                                   ++ if isOne' then [] else error ("doSqlGet in ObjBinGenObject: Cannot create keyGroups")
                                 )
            reduce :: [[((Plug,(ObjectDef,SqlField)),(ObjectDef,SqlField))]]
                      -> [((Plug,(ObjectDef,SqlField)),[(ObjectDef,SqlField)])]
            reduce [] = []
            reduce (g:gs)    = (fst (head g),map snd g):reduce
                               [ res
                               | group<-gs
                               , let ga=fst$snd$head g
                               , let res=[((plug,(ai,sf)),(a,tf))
                                         |((plug,(ai,sf)),(a,tf))<-group,ga/=a]
                               , not (null res)]
            combos           = [ ((plug,(ai,sf)),(a,tf))
                               | ai<-(objats (flattenOdef objIn))++[objIn]
                               , a<-aOuts
                               , Just e' <-[takeOff (objctx ai) (objctx a)]
                               , (plug,sf,tf)<-sqlRelPlugs fSpec e'
                               ]
            takeOff :: Expression->Expression->(Maybe Expression)
            takeOff (F (a:as)) (F (b:bs)) | disjNF a==disjNF b = takeOff (F as) (F bs)
            takeOff a (F (b:bs)) | disjNF a== disjNF b = Just (F bs)
            takeOff a e' | isIdent a = Just e'
            takeOff _ _ = Nothing
            isOne' = isOne objOut
            aOuts           = [a|a<-objats(flattenOdef objOut)]
            rest :: [(ObjectDef,Integer)]
            rest            = zip [ a | a<-aOuts
                                      , a `notElem` [a' | g <- comboGroups, (a',_) <- snd g] 
                                  ] [(1::Integer)..]
            fieldNames      = [ "`"++tableReName p++"`.`"++(fldname f)++"`"
                                ++(if fldname f == name a then [] else " AS `"++name a++"`")
                              | ((p,_),as)<-comboGroups',(a,f)<-as
                              ] ++
                              [if isIdent (objctx a)
                               then "'\".addslashes("++name (objIn)++").\"'"++" AS `"++name a++"`"
                               else "`f"++(show n)++"`.`"++(sqlExprTrg fSpec (objctx a))++"`" ++
                                    (if name a == (sqlExprTrg fSpec (objctx a))
                                     then []
                                     else " AS `"++name a++"`")
                                   |(a,n)<-rest]
            tbls            = [ ([tableName p]
                                ,"`"++tableReName p++"`.`"++(fldname f)++
                                 "`='\".addslashes("++(name a)++").\"'"
                                )
                              |((p,(a,f)),_)<-comboGroups
                              ] ++
                              [ (r
                                 ,"`f"++(show n)++"`.`"++(sqlExprSrc fSpec (objctx a))++
                                  "`='\".addslashes("++name (objIn)++").\"'"
                                 )
                              | (a,n)<-rest
                              , not (isIdent (objctx a))
                              , let l=restLines (a,n), not (null l)
                              , let r=if null$head l then tail l else l
                              , not (null r)
                              ]
            joinOn ([t],jn) = [ (if isOne' then "     , "      else "  LEFT JOIN ")++t
                              ++(if isOne' then "" else " ON "++jn)]
            joinOn (ts,jn)  = [ (if isOne' then "     , " else "  LEFT JOIN ")++(head ts)]
                              ++indentBlock (if isOne' then 7 else 12) (tail ts)++(if isOne' then [] else (["    ON "++jn]))
            restLines (outAtt,n)
              = splitLineBreak (selectExprBrac fSpec (-4)
                                               (if isOne' then "" else sqlExprSrc fSpec (objctx outAtt))
                                               (sqlExprTrg fSpec (objctx outAtt))
                                               (objctx outAtt) ++ " AS f"++show n) -- better names?
            tableName p      = if name p == tableReName p then "`"++name p++"`" else "`"++name p ++ "` AS "++tableReName p
            tableReName :: Plug -> String
            tableReName p    = head [nm | (nm,p')<-renamedTables,p'==p]
            renamedTables    = naming (\p nm->(nm,p))                               -- function used to asign name a to element b
                                      (name:[(\_->"plug"++show n)|n<-[(1::Integer)..]]) -- infinite list of functions to create a name for b
                                      ['f':show n|n<-[1..(length rest)]]      -- list of forbidden names (names already taken)
                                      (plugs fSpec)                                 -- list of elements b that need a name

   
   splitLineBreak :: String -> [String]
   splitLineBreak [] = []
   splitLineBreak ('\n':s) = []:(splitLineBreak s)
   splitLineBreak [letter] = [[letter]]
   splitLineBreak (l:s) | null (splitLineBreak s) = [(l:head (splitLineBreak s))]
   splitLineBreak (l:s) | otherwise               =  (l:head (splitLineBreak s)):(tail$splitLineBreak s)

   phpObjRelName :: forall a a1. (Identified a, Identified a1) =>
                                             [String] -> a -> a1 -> String
   phpObjRelName pth o r = phpIdentifier (chain "_" (pth++[name o,name r]))
   