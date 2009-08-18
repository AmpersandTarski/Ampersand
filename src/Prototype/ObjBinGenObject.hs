{-# LANGUAGE ScopedTypeVariables#-}
  module Prototype.ObjBinGenObject(objectServices) where
   import Char(toUpper)
   import Strings(chain)
   import NormalForms (disjNF,conjNF)
   import Auxiliaries (eqCl,sort')
   import Adl (source,target
              ,Concept(..),ObjectDef(..),Numbered(..),Declaration(..)
              ,Identified(..),mors,explain,Morphism(..),Prop(..)
              ,Object(..),multiplicities,isIdent,Expression(..),mIs
              ,isTrue,makeInline,flp)
   import ShowADL
   import CC_aux ( fun )
   import Collection (Collection(rd,(>-)))
   import Prototype.RelBinGenBasics(sqlExprSrc,sqlExprTrg,naming,plugs,selectExprBrac,indentBlock
     ,sqlRelPlugs,addToLast,isOne,phpIdentifier,sqlAttConcept,selectExpr,sqlConcept
     ,sqlMorName,addSlashes,sqlMorSrc,commentBlock)
   import Data.Fspec
   import Data.Plug
   --import Debug.Trace
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
       ]++["    $this->_id=$id;" | not (isOne o)]
       ++ ["    $this->_"++phpIdentifier (name a)++"="++phpVar (name a)++";"| a <- attributes o]
       ++ concat (take 1 [  [ "    if(!isset("++phpVar (name a')++")"++(if isOne o then "" else "&& isset($id)")++"){"
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
                            ++["  $this->set_"++phpIdentifier (name a)++"($me['"++(name a)++"']);"
                              |a<-attributes o]
                            ++["}"]
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
      [ "$me=array(\"id\"=>$this->getId()"
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
      , "  return $this->getId();"
      , "}"
      , "DB_doquer('ROLLBACK');"
      , "return false;"
      ]
    where
      saveCodeLines = concat (map fst saveCodeElems)
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
        -- only insert if every mandatory field has a value, so use largeOccurences
       ++ concat (map inscode largeOccurences)
        -- I don't really know when to update
        -- I do know that if delete and insert took place, it is not needed to update
       ++ concat (map updcode nonFullOccurences)
      , -- function should send back all ObjectDef's that have been saved somewhere
       rd $ map (fst . snd) (concat largeOccurences))
    where
      delcode (((a,f),_):_)
       = nestTo a
                (\var->["DB_doquer(\"DELETE FROM `"++name plug++"` WHERE `"++(fldname f)
                        ++"`='\".addslashes("++var++"['id']).\"'\",5);"])
      inscode is@(((a,s),_):_)
       = ["$vals = ''; // list of ("++names++") to INSERT into "++name plug] ++
         nestTo a
                (\var ->
                 [take (2*n) (repeat ' ') ++
                  "foreach(" ++ var ++ "['"++(name o)++"'] as $"++(phpIdentifier $ name o)++"){"
                 |(o,n)<-zip nunios [0..]] ++
                 indentBlock (2*length nunios)
                 [ "$vals .= \",(" ++
                   chain ", "
                   [ "'\".addslashes("++phpvar++").\"'"
                   | (o,_)<-attrs
                   , let phpvar = if a == o
                                  then var++"['id']"
                                  else
                                  ( if Uni `elem` multiplicities (objctx o)
                                    then var++"['"++(name o)++"']"
                                    else "$"++(phpIdentifier $ name o)
                                  ) ++
                                  ( if null $ objats o
                                    then ""
                                    else "['id']"
                                  )
                   ] ++ ")\";"
                 ] ++
                 [take (2*n) (repeat ' ') ++ "}"|(_,n)<-reverse $ zip nunios [0..]]
                ) ++
         ["if(strlen($vals)) DB_doquer(\"INSERT IGNORE INTO `"++name plug++"` ("
           ++names ++") VALUES \".substr($vals,1),5);"
         ]
         where attrs  = ownAts ++ -- ook alle indentiteit-velden toevoegen (meestal de SQL-`id`)
                        [ (a, s) | s `notElem` (map snd ownAts)]
               names  = chain "," $ map (fldname.snd) attrs
                -- nunios: Not UNI ObjectS: objects that are not Uni
               nunios = [o|(o,_)<-(map snd is), not $ Uni `elem` multiplicities (objctx o)]
               ownAts = map snd is
      updcode attrs = []
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
      {-
      // finally update
      // update code is for attributes that are uni
      // in our case there is gebied
      foreach($me['Bewoners'] as $i0=>$v0){
              // gebied is Uni, so UPDATE:
        if(!isset($v0['naam'])){
          DB_doquer("INSERT INTO inhabitant (area) VALUES ('".addslashes($v0['gebied'])."')",1);
        }else
        if(DB_doquer("UPDATE inhabitant SET area='".addslashes($v0['gebied'])."' WHERE person='".addslashes($v0['naam'])."'",1)==0){
          DB_doquer("INSERT INTO inhabitant (area,person) VALUES ('".addslashes($v0['gebied'])."','".addslashes($v0['naam'])."')",1);
        }
      }
      -}
      -- occurencesForDelete = 
      occurences = group $ rd $ concat (map (plugAts' plug object) (objats object))
      group lsts = eqCl fst lsts
      fullOccurences = fullGroups (fields plug) occurences
      largeOccurences = fullGroups requiredFields occurences
      fullGroups :: [SqlField] -> [[((ObjectDef,SqlField),(ObjectDef,SqlField))]]
                   -> [[((ObjectDef,SqlField),(ObjectDef,SqlField))]]
      fullGroups set as = [a | a<-as, null (set >- (map (snd . snd) a ++ [snd$fst$head a]))]
      requiredFields  = [f| f <- fields plug
                          , Tot `elem` multiplicities (fldexpr f)
                          -- strictly speaking, ident fields ARE required, but we can fill in their
                          -- values easily since their value IS their source
                          , not $ isIdent $ fldexpr f
                          ]
      nonFullOccurences = occurences >- fullOccurences
   {-
   plugAts :: Plug -> ObjectDef -> [(ObjectDef,SqlField)]
   plugAts plug o =   ( [(o,r) | s<-[objctx o],r<-fields plug,(fldexpr r == s) || (fldexpr r == flp s)]
                         ++ concat (map (plugAts plug) (objats o))
                       )
   -}
   plugAts' :: Plug -> ObjectDef           -- parent
               -> ObjectDef                -- object itself
               -> [((ObjectDef, SqlField), -- source
                  (ObjectDef,SqlField))]   --target
   plugAts' plug p o = [ ((p,sf),(o,tf))
                       | let e'=objctx o
                       , not $ isIdent e'
                       , sf<-[f|f<-fields plug,target (fldexpr f)==source e']
                       , tf<-[f|f<-fields plug,target (fldexpr f)==target e']
                       ,  ((Sur `elem` multiplicities (fldexpr sf))
                           &&(conjNF (F [fldexpr  sf,    e'])==fldexpr tf)
                          )
                       || ((Sur `elem` multiplicities (fldexpr tf))
                           &&(conjNF (F [fldexpr  tf,flp e'])==fldexpr sf)
                          )
                       || (isIdent (fldexpr sf) && fldexpr tf ==     e')
                       || (isIdent (fldexpr tf) && fldexpr sf == flp e')
                       ] ++ concat (map (plugAts' plug o) (objats o))
                       
   
   objPlugs :: Fspc -> ObjectDef -> [Plug]
   objPlugs fSpec object
     = rd ([p|(p,_,_)<-sqlRelPlugs fSpec (objctx object)] ++ concat [objPlugs fSpec a|a<-objats object])
   
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
      where comboGroups::[((Plug,(ObjectDef,SqlField)),[(ObjectDef,SqlField)])]
            comboGroups      = reduce (sort' (length) (eqCl fst combos))
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
                              | ((p,_),as)<-comboGroups,(a,f)<-as
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
            joinOn ([t],jn) = [(if isOne' then "     , "      else "  LEFT JOIN ")++t
                              ++(if isOne' then "" else " ON "++jn)]
            joinOn (ts,jn)  = [(if isOne' then "     , " else "  LEFT JOIN ")++(head ts)]
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
                   
                   {- -- een aanzet om het bovenstaande te verbeteren:
                      recurDown::([a]->[b])->(a->[a])->[a]->[b]
                   recurDown f1 f2 itms = f1 itms ++ concat (map ((recurDown f1 f2).f2) itms)
                
                   -- onderstaande functie selecteert alle attributen plat op de source gemapt, dwz:
                   -- a=[b,c] wordt I=[a;b,a;b], wat alleent hetzelfde is als a UNI is
                   doSqlGet :: Fspc -> ObjectDef -> ObjectDef -> [String]
                   doSqlGet fSpec objIn objOut = ["SELECT DISTINCT " ++ head allFieldNames  ]
                                                    ++ map ((++) "     , ") (tail allFieldNames  ) ++
                                                 ["  FROM " ++ head (fst (head allTbls))]
                                                    ++ (indentBlock 7 (tail (fst (head allTbls))))
                                                    ++ concat (map joinOn (tail allTbls)) ++
                                                 ( if null$snd$head allTbls
                                                   then []
                                                   else [" WHERE " ++ snd (head allTbls)]
                                                 )
                     where comboGroups::[ObjectDef]->[((Plug,(ObjectDef,SqlField)),[(ObjectDef,SqlField)])]
                           comboGroups atts     = reduce (sort' (length) (eqCl fst (combos atts)))
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
                           combos atts       = [ ((plug,(ai,sf)),(a,tf))
                                              | ai<-(objats (flattenOdef objIn))++[objIn]
                                              , a<-atts
                                              , Just e' <-[takeOff (objctx ai) (objctx a)]
                                              , (plug,sf,tf)<-sqlRelPlugs fSpec e'
                                              ]
                           takeOff :: Expression->Expression->(Maybe Expression)
                           takeOff (F (a:as)) (F (b:bs)) | disjNF a==disjNF b = takeOff (F as) (F bs)
                           takeOff a (F (b:bs)) | disjNF a== disjNF b = Just (F bs)
                           takeOff a e' | isIdent a = Just e'
                           takeOff _ _ = Nothing
                           isOne' = isOne objOut
                           rest :: [ObjectDef]->[(ObjectDef,Integer)]
                           rest atts        = zip [ a | a<-atts
                                                      , a `notElem` [a' | g <- comboGroups atts, (a',_) <- snd g] 
                                                  ] [(1::Integer)..]
                           fieldNames atts  = [ "`"++tableReName atts p++"`.`"++(fldname f)++"`"
                                                ++(if fldname f == name a then [] else " AS `"++name a++"`")
                                              | ((p,_),as)<-comboGroups atts,(a,f)<-as
                                              ] ++ ["`f"++(show n)++"`.`"++(sqlExprTrg fSpec (objctx a))++"`" ++
                                                    (if name a == (sqlExprTrg fSpec (objctx a))
                                                     then []
                                                     else " AS `"++name a++"`")
                                                   |(a,n)<-rest atts]
                           allFieldNames = recurDown fieldNames objats (objats objOut)
                           allTbls = tbls objOut{objnm = (name objIn)}
                           tbls obj         = [ ( [tableName (objats obj) p]
                                                , "`"++tableReName (objats obj) p++"`.`"++(fldname f)++
                                                  "`='\".addslashes("++(name a)++").\"'"
                                                )
                                              | ((p,(a,f)),_)<-comboGroups (objats obj)
                                              ] ++
                                              [ ( r
                                                , if source (objctx a)==cptS
                                                  then ""
                                                  else "`f"++(show n)++"`.`"++(sqlExprSrc fSpec (objctx a))++
                                                       "`='\".addslashes("++name obj++").\"'"
                                                )
                                              | (a,n)<-rest (objats obj)
                                              , let l=restLines (a,n), not (null l)
                                              , let r=if null$head l then tail l else l
                                              , not (null r)
                                              ]
                           joinOn ([t],jn)  = [(if null jn then "     , "      else "  LEFT JOIN ")++t
                                              ++(if null jn then "" else " ON "++jn)]
                           joinOn (ts,jn)   = [(if null jn then "     , " else "  LEFT JOIN ")++(head ts)]
                                              ++indentBlock (if null jn then 7 else 12) (tail ts)
                                              ++(if isOne' then [] else (["    ON "++jn]))
                           restLines (outAtt,n)
                             = splitLineBreak (selectExprBrac fSpec (-4)
                                                              ( if (source$objctx outAtt)==cptS
                                                                then ""
                                                                else sqlExprSrc fSpec (objctx outAtt))
                                                              (sqlExprTrg fSpec (objctx outAtt))
                                                              (objctx outAtt) ++ " AS f"++show n) -- better names?
                           tableName atts p  = if name p == tableReName atts p then "`"++name p++"`" else "`"++name p ++ "` AS "++tableReName atts p
                           tableReName :: [ObjectDef]->Plug -> String
                           tableReName atts p = head [nm | (nm,p')<-renamedTables atts,p'==p]
                           renamedTables atts = naming (\p nm->(nm,p))                               -- function used to asign name a to element b
                                                       (name:[(\_->"plug"++show n)|n<-[(1::Integer)..]]) -- infinite list of functions to create a name for b
                                                       ['f':show n|n<-[1..(length (rest atts))]]      -- list of forbidden names (names already taken)
                                                       (plugs fSpec)                                 -- list of elements b that need a name
                   -}
   
   
   
   splitLineBreak :: String -> [String]
   splitLineBreak [] = []
   splitLineBreak ('\n':s) = []:(splitLineBreak s)
   splitLineBreak [letter] = [[letter]]
   splitLineBreak (l:s) | null (splitLineBreak s) = [(l:head (splitLineBreak s))]
   splitLineBreak (l:s) | otherwise               =  (l:head (splitLineBreak s)):(tail$splitLineBreak s)

   phpObjRelName :: forall a a1. (Identified a, Identified a1) =>
                                             [String] -> a -> a1 -> String
   phpObjRelName pth o r = phpIdentifier (chain "_" (pth++[name o,name r]))
   