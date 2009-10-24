{-# LANGUAGE ScopedTypeVariables#-}
  module Prototype.Object(objectServices) where
   --import Char(toUpper)
   import Strings(chain)
   import NormalForms (disjNF,simplify)
   import Auxiliaries (eqCl,sort')
   import Adl (target
              --,Concept(..),Declaration(..),isTrue,makeInline
              ,ObjectDef(..),Numbered(..),Rule
              ,Identified(..),mors,explain,Morphism(..),Prop(..)
              ,Object(..),multiplicities,isIdent,Expression(..),mIs
              ,flp)
   import ShowADL
   import Collection (Collection (rd,rd',(>-)))
   import Prototype.RelBinGenBasics(sqlExprSrc,sqlExprTrg,naming,selectExprBrac,indentBlock
     ,sqlRelPlugs,addToLast,isOne,phpIdentifier,selectExpr
     ,addSlashes,commentBlock,sqlPlugFields)
   import Data.Fspec
   import Data.Plug
   -- import Debug.Trace
   import Version (versionbanner)
   import Options
   import PredLogic

   objectServices :: Options 
                  -> Fspc
                  -> ObjectDef
                  -> String
   objectServices flags fSpec o
    = (chain "\n  "
      ([ "<?php // generated with "++versionbanner
       , ""
       , "/********* on "++(show (pos o))
       , showADL o
       , " *********/"
       , ""
       ] ++ showClasses flags fSpec o ++
       ( if isOne o
         then []
         else generateService_getEach fSpec (name o) o ++
              generateService_read    fSpec (name o) o ++
              generateService_delete  fSpec (name o) o
       )
      )) ++ "\n?>"

   generateService_getEach :: Fspc -> String -> ObjectDef -> [String]
   generateService_getEach fSpec nm o
    = ["function getEach"++phpIdentifier nm++"(){"
      ,"  return firstCol(DB_doquer('" ++
        ( selectExpr fSpec 31 (sqlExprTrg fSpec (ctx o)) "" (flp (ctx o)))
        ++"'));"
      ,"}\n"]

   generateService_read :: Fspc -> String -> ObjectDef -> [String]
   generateService_read _ nm object
    = ["function read"++phpIdentifier nm++"($id){"
      ,"    // check existence of $id"
      ,"    $obj = new "++phpIdentifier (name object)++"($id);"
      ,"    if($obj->isNew()) return false; else return $obj;"
      ,"}\n"]

   phpVar :: String -> String
   phpVar x = "$"++phpIdentifier x

   generateService_delete :: Fspc -> String -> ObjectDef -> [String]
   generateService_delete _ nm _
    = ["function del"++phpIdentifier nm++"($id){"
      ,"  $tobeDeleted = new "++phpIdentifier nm++"($id);"
      ,"  if($tobeDeleted->isNew()) return true; // item never existed in the first place"
      ,"  if($tobeDeleted->del()) return true; else return $tobeDeleted;"
      ,"}\n"]

   
   -- | The function showClasses defines the PHP class of an object o and also (recursively) the
   --   PHP-classes of all subordinate objects (i.e. the attributes) of o.
   --   context  : the Context of object o. In due time, this will be replaced by an Fspc
   --   triggers : a [possibly empty] set of triggers, that is used to generate automated functionality.
   --              Precondition: This set contains precisely those triggers that may be used within the transaction
   --              boundary of the class.
   --              In due time, this parameter will become a selection from the entire set of triggers in Fspc.
   --   nms      : the name trail of all super-objects until the root of this service. This is used to generate unique names for every field.
   --   o        : the object to be transformed in a class.
   showClasses :: Options -> Fspc -> ObjectDef -> [String]
   showClasses flags fSpec o
    = [ "class "++myName ++" {"] ++
      indentBlock 2 (
            ( if isOne o then [] else ["protected $_id=false;","protected $_new=true;"] )
            ++ ["private $_"++phpIdentifier (name a)++";"| a <- attributes o]++
            ["function "++myName++"(" ++ (if isOne o then "" else "$id=null, ")
                                        ++ (chain ", " [phpVar (name a)++"=null" | a<-attributes o])
                                        ++"){"
            ]++["  $this->_id=$id;" | not (isOne o)]
            ++ ["  $this->_"++phpIdentifier (name a)++"="++phpVar (name a)++";"| a <- attributes o]
            ++ concat (take 1 [  [ "  if(!isset("++phpVar (name a')++")"++(if isOne o then "" else " && isset($id)")++"){"
                                  , "    // get a "++(myName)++" based on its identifier"] ++
                                  ( if isOne o then [] else
                                    [ "    // check if it exists:"
                                    , "    $ctx = DB_doquer('"++(doesExistQuer "$id")++"');"
                                    , "    if(count($ctx)==0) $this->_new=true; else"
                                    , "    {"
                                    , "      $this->_new=false;"] ) ++
                                  indentBlock (if isOne o then 4 else 6)
                                  ( [ "// fill the attributes"
                                    ] ++
                                    ( if null [a|a<-objats o,Uni `elem` multiplicities (objctx a)]
                                      then ["$me=array();"]
                                      else []            
                                    ) ++ (doPhpGet fSpec
                                                  "$me"
                                                  0
                                                  (o  {objnm =if isOne o then "1" else "$id"
                                                      ,objctx=Tm(mIs(concept o))
                                                      ,objats=[]}
                                                  )
                                                  o
                                        ) 
                                    ++["$this->set_"++phpIdentifier (name a)++"($me['"++(name a)++"']);"
                                      |a<-attributes o]
                                  ) ++
                                  ( if isOne o then [] else ["    }"] )
                                  ++["  }"] ++
                                  if isOne o then [] else
                                  [ "  else if(isset($id)){ // just check if it exists"    
                                  , "    $ctx = DB_doquer('"++(doesExistQuer "$id")++"');"
                                  , "    $this->_new=(count($ctx)==0);"
                                  , "  }" ]
                              | a' <- attributes o]
                      ) ++
            ["}\n"]++
            saveTransactions flags fSpec o
            ++ (concat
                [ ["function set_"++phpIdentifier (name a)++"($val){"
                  ,"  $this->_"++phpIdentifier (name a)++"=$val;"
                  ,"}"
                  ,"function get_"++phpIdentifier (name a)++"(){"] ++
                  ( if (isObjUni a) then [] else
                    ["  if(!isset($this->_"++phpIdentifier (name a)++")) return array();"]
                  ) ++
                  ["  return $this->_"++phpIdentifier (name a)++";"
                  ,"}"
                  ]
                | a <- attributes o
                ]
                )++
            if isOne o
            then []
            else ["function setId($id){"
                  ,"  $this->_id=$id;"
                  ,"  return $this->_id;"
                  ,"}"
                  ,"function getId(){"
                  ,"  if($this->_id===null) return false;"
                  ,"  return $this->_id;"
                  ,"}"
                  ,"function isNew(){"
                  ,"  return $this->_new;"
                  ,"}"
                  ]
            ) ++
      [ "}\n" ]
    where
     myName = name o
     doesExistQuer :: [Char] -> String
     doesExistQuer var
      = selectExpr fSpec
                   25
                   (sqlExprSrc fSpec ctx')
                   ""
                   expr
      where expr = if null fs then F [ tm, ctx'] else F (tm:head fs)
            tm   = Tm (Mp1 ("\\''.addSlashes("++var++").'\\'") (concept o))
            ctx' = simplify $ flp (ctx o)
            fs   = [es' | F es' <- [ctx']]
   saveTransactions :: Options -> Fspc -> ObjectDef -> [String]
   saveTransactions flags fSpec object
    = [ "function save(){"
      , "  DB_doquer('START TRANSACTION');"
      ] ++ indentBlock 2
      ( (commentBlock ( ["Attributes that will not be saved are:"
                        ,"--------------------------------------"]++map name unsavedAtts))
        ++ ( if isOne object then [] else ["$newID = ($this->getId()===false);"]) ++
        setMe ++ saveCodeLines ++ close (if isOne object then "true" else "$this->getId()")
      ) ++ [ "}" ] ++ 
      if isOne object then [] else
      [ "function del(){"
      , "  DB_doquer('START TRANSACTION');"
      ] ++ indentBlock 2
      ( setMe ++ delCodeLines ++ close "true"
      ) ++
      [ "}" ]
    where
     maybeId a = ( if null $ objats a then "" else "['id']" )
     setMe = [ "$me=array(\"id\"=>" ++ (if isOne object then "1" else "$this->getId()")
               ++ concat [ ", "++show (name a) ++ " => $this->_"++phpIdentifier (name a)
                         | a<-objats object ]
               ++ ");"
             ]
     close result
      = (concat
          [ ["if (!checkRule"++show (nr rul)++"()){"
            ,"  $DB_err='"++(addSlashes (show(explainArt flags fSpec rul)))++"';"
            ,"} else"
            ]
          | rul <- vrules fSpec
          , or (map (\mpm -> elem mpm (mors rul)) -- rule contains an element
                    (mors object) -- effected mors  ; SJ: mors yields all morphisms inline.
              )
          ]
        ) ++
        [ "if(true){ // all rules are met"
        , "  DB_doquer('COMMIT');"
        , "  return "++ result ++ ";"
        , "}"
        , "DB_doquer('ROLLBACK');"
        , "return false;"
        ]
     saveCodeLines = concat $ map fst saveCodeElems
     delCodeLines  = concat $ map fst delCodeElems
     saveCodeAtts  = rd $ concat $ map snd saveCodeElems
     saveCodeElems = [saveCodeElem p | p<-objPlugs fSpec object]
     delCodeElems  = [delCodeElem p | p<-objPlugs fSpec object]
     unsavedAtts   = allAtts object >- saveCodeAtts
     allAtts :: ObjectDef->[ObjectDef]
     allAtts o = objats o ++ concat (map allAtts (objats o))
     nestTo :: ObjectDef -> (String->[String]) -> [String]
     nestTo attr fnc = if null nt then error ("saveCodeElem: Cannot nestTo "++show attr++" (ObjBinGenObject)")
                       else head nt
       where nt = nestToRecur attr fnc object "$me" 0
     nestToRecur :: ObjectDef -> (String->[String]) -> ObjectDef -> String -> Integer -> [[String]]
     nestToRecur attr fnc a var d
       | attr==a   = [ if null (objats a) then fnc var else fnc (var)]
       | otherwise = [ if Uni `elem` multiplicities (objctx a') then ans
                       else ["foreach("++var++"['"++name a'++"'] as $i"++show d++"=>$v"++show d++"){"]
                           ++ indentBlock 2 ans ++ ["}"]
                     | a'<-objats a
                     , let mvar = if Uni `elem` multiplicities (objctx a')
                                 then var++"['"++name a'++"']"
                                 else "$v"++show d
                     , ans<-nestToRecur attr fnc a' (mvar) (d+1)]
     occurences       plug = (eqCl fst) $ rd $ concat (map (plugAts plug object) (objats object))
     fullOccurences   plug = filter (isFullOccurance plug) (occurences plug)
     isFullGroup      set a = null (set >- (map (snd . snd) a ++ [snd$fst$head a]))
     isFullOccurance  plug = isFullGroup (fields plug)
     isLargeOccurance plug = isFullGroup (requiredFields plug)
     requiredFields   plug = [f| f <- fields plug
                               , Tot `elem` multiplicities (fldexpr f)
                               -- I used to remove ident fields from the requiredFields, but they ARE required
                               -- The reason I did this, was because auto increment fields are not
                               -- So let's remove the auto increment fields:
                               , not (fldauto f)
                               ]
     delUpdt plug (o,s) var
       = [ "if(isset("++var++maybeId o++")) DB_doquer(\"UPDATE `"++name plug
           ++"` SET `"++fldname s++"`=NULL WHERE `"
           ++ fldname s++"`='\".addslashes("++var++maybeId o++").\"'\",5);"]
     delcode plug (((a,f),_):_)
       = nestTo a
                (\var->["DB_doquer(\"DELETE FROM `"++name plug++"` WHERE `"++(fldname f)
                        ++"`='\".addslashes("++var++ maybeId a ++ ").\"'\",5);"])
     delcode _ [] = error "Fatal (module Object.hs): should not occur" -- , but generating no code for no request seems OK
     delCodeElem :: Plug->([String],[ObjectDef])
     delCodeElem plug
       = (   concat (map (delcode plug) (fullOccurences plug))
          ++ concat (map updelcd (occurences plug))
         ,   rd $ map (fst . snd) (concat (fullOccurences plug))
         )
       where
         updelcd [] = []
         updelcd is@(((a,s),_):_)
          = if is `elem` fullOccurences plug || not (fldnull s)
            then []--"//Cannot (or should not) delete using UPDATE in plug "++name plug++", field "++fldname s]
            else
            nestTo a
                   (\var -> delUpdt plug (a,s) var)
     saveCodeElem :: Plug->([String],[ObjectDef])
     saveCodeElem plug
       = ( -- only delete if we have ALL information needed for refill, so use fullOccurences
           -- if we delete more, the plug would violate its transaction boundary
             concat (map (delcode plug) (fullOccurences plug))
           -- only insert if every mandatory field has a value, so use largeOccurences
         ++ concat (map inscode (occurences plug))
         , -- function should send back all ObjectDef's that have been saved everywhere properly
         rd $ map (fst . snd) (concat (occurences plug)))
       where
        inscode [] = [] -- there are probably no empty groups, and we cannot modify them anyways
        inscode is@(((a,s),_):_)
         = if not (isLargeOccurance plug is) && null keys -- if we have keys, we may use them to UPDATE
           then ["// no code for "++name a++","++fldname s++" in "++name plug]
           else
           nestTo a
                   (\var
                     -> (if is `elem` fullOccurences plug || not (fldnull s) then [] else delUpdt plug (a,s) var) ++
                        concat [indentBlock (2*n)
                                           ( ( if fldnull f
                                               then (:)("if(count("++var++"['"++name o++"'])==0) "
                                                         ++var++"['"++(name o)++"'][] = null;")
                                               else id
                                             )
                                             ["foreach  (" ++ var ++ "['"++(name o)++"'] as $"
                                               ++(phpIdentifier $ name o)++"){" -- de } staat verderop
                                             ]
                                           )
                               | ((o,f),n) <-zip nunios [0..]] ++
                       indentBlock (2*length nunios)
                       ( if (isLargeOccurance plug) is -- attempt INSERT first
                         then ( if ((isFullOccurance plug) is || null keys)
                                 then -- na een DELETE, of bij null keys kan geen UPDATE
                                     -- reden: een UPDATE heeft een key nodig om zich te hechten
                                     -- (en na een delete hebben we die waarde net weggegooid)
                                     [ "$res="++insQuery var++";" ] ++
                                     if null keys
                                     then []
                                     else if(a==object)
                                          then [ "if($newID) $this->setId($me['id']=mysql_insert_id());"]
                                          else [ "if($res!==false && !isset("++var++maybeId a++"))"
                                               , "  "++var++maybeId a++"=mysql_insert_id();" ]
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
                 insQuery var
                   = "DB_doquer(\"" ++ "INSERT IGNORE INTO `"++name plug
                     ++ "` ("++chain "," ["`"++fldname f++"`" | (_,f)<-rd' (fldname.snd) attrs]
                     ++ ") VALUES (" ++
                     chain ", "
                           [ if fldnull f || fldauto f
                             then "\".(" ++ ( if fldauto f && o==object
                                              then "!$newID"
                                              else "(null!=" ++ varname var o ++ ")"
                                            ) ++ "?\"'\".addslashes("
                                 ++ varname var o ++ ").\"'\":\"NULL\").\""
                             else "'\".addslashes("++varname var o++").\"'"
                           | (o,f)<-rd' (fldname.snd) attrs
                           ] ++ ")\", 5)"
                 updQuery var
                   = "DB_doquer(\"" ++ "UPDATE `"++name plug++"` SET " ++
                     chain ", "
                           [ "`"++fldname f++"`="++
                             if fldnull f && not (f==s)
                             then "\".(" ++ ( if fldauto f && o==object
                                              then "!$newID"
                                              else "(null!=" ++ varname var o ++ ")"
                                            ) ++ "?\"'\".addslashes("
                                 ++ varname var o ++ ").\"'\":\"NULL\").\""
                             else "'\".addslashes("++varname var o++").\"'"
                           | (o,f)<-attrs, fst key/=o
                           ] ++ " WHERE `"++fldname (snd key)++"`='\".addslashes("++varname var (fst key)++").\"'" ++"\", 5)"
                 varname var o = ( if a == o
                                   then var
                                   else if Uni `elem` multiplicities (objctx o)
                                         then var++"['"++(name o)++"']"
                                         else "$"++(phpIdentifier $ name o)
                                 ) ++ maybeId o

   plugAts :: Plug -> ObjectDef           -- parent (wrong values are allowed, see source)
              -> ObjectDef                -- object itself
              -> [((ObjectDef, SqlField), -- source (may include the wrong-valued-'parent')
                 (ObjectDef,SqlField))]   -- target
   plugAts plug p o = ( [ ((o,sf),(o,tf))
                        | (sf,tf)<-sqlPlugFields plug (Tm$mIs$target$objctx o)
                        ] ++
                        [ ((p,sf),(o,tf))
                        | (sf,tf)<-sqlPlugFields plug $ objctx o
                        ]
                      ) ++ concat (map (plugAts plug o) (noIdents o))
     where noIdents obj = [att | att <- objats obj]--, not$isIdent$objctx obj] ++ concat [noIdents att | att<-objats obj,isIdent$objctx obj]

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
       sqlUnis = doSqlGet fSpec False objIn (objOut{objats= idAt : map trunc unisNeeded,objctx=targetCpt})
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
                                (doSqlGet fSpec True
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
   
{- objIn representeert een PHP-object dat een subset is van PHP-object objOut.
   objIn representeert het deel van objOut dat bij aanroep reeds gevuld is.
   Dit voorkomt onnodige database accessen.
   De parameter 'isArr' vertelt of het een array betreft of een enkel veld.
-}
   doSqlGet :: Fspc -> Bool -> ObjectDef -> ObjectDef -> [String]
   doSqlGet fSpec isArr objIn objOut
     = ["SELECT DISTINCT " ++ head fieldNames      ]
       ++ map ((++) "     , ")
              (tail fieldNames) 
               ++ (if null tbls
                   then []
                   else ["  FROM " ++ head (fst (head tbls))]
                        ++ (indentBlock 7 (tail (fst (head tbls))))
                        ++ concat (map joinOn (tail tbls)) ++
                        (if isOne' then [] else [" WHERE " ++ snd (head tbls)])
                  )
      where comboGroups'::[((Plug,(ObjectDef,SqlField)),[(ObjectDef,SqlField)])]
            comboGroups'= reduce (sort' (length) (eqCl fst combos))
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
                               | ai<-(objats (objIn))++[objIn]
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
            aOuts           = [a|a<-objats( objOut)]
            rest :: [(ObjectDef,Integer)]
            rest            = zip [ a | a<-aOuts
                                      , a `notElem` [a' | g <- comboGroups, (a',_) <- snd g] 
                                  ] [(1::Integer)..]
            fieldNames      = [ "`"++tableReName gr++"`.`"++(fldname f)++"`"
                                ++(if fldname f == name a then [] else " AS `"++name a++"`")
                              | (gr@(_,_),as)<-comboGroups',(a,f)<-as
                              ] ++
                              [if isIdent (objctx a)
                               then "'\".addslashes("++name (objIn)++").\"'"++" AS `"++name a++"`"
                               else "`f"++(show n)++"`.`"++(sqlExprTrg fSpec (objctx a))++"`" ++
                                    (if name a == (sqlExprTrg fSpec (objctx a))
                                     then []
                                     else " AS `"++name a++"`")
                                   |(a,n)<-rest]
            tbls            = [ ([tableName gr]
                                ,"`"++tableReName gr++"`.`"++(fldname f)++
                                 "`='\".addslashes("++(name a)++").\"'"
                                )
                              |(gr@(_,(a,f)),_)<-comboGroups
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
            joinOn ([t],jn) = [ (if isOne' then "     , "      else (if isArr then " " else "  LEFT")++" JOIN ")++t
                              ++(if isOne' then "" else " ON "++jn)]
            joinOn (ts,jn)  = [ (if isOne' then "     , " else (if isArr then " " else "  LEFT")++" JOIN ")++(head ts)]
                              ++indentBlock (if isOne' then 7 else 12) (tail ts)++(if isOne' then [] else (["    ON "++jn]))
            restLines (outAtt,n)
              = splitLineBreak (selectExprBrac fSpec (-4)
                                               (if isOne' then "" else sqlExprSrc fSpec (objctx outAtt))
                                               (sqlExprTrg fSpec (objctx outAtt))
                                               (objctx outAtt) ++ " AS f"++show n) -- better names?
            tableName gr@(p,_) = if name p == tableReName gr then "`"++name p++"`" else "`"++name p ++ "` AS "++tableReName gr
            tableReName :: (Plug,(ObjectDef,SqlField)) -> String
            tableReName gr   = head [nm | (nm,gr')<-renamedTables,gr'==gr]
            renamedTables :: [(String,(Plug,(ObjectDef,SqlField)))]
            renamedTables
              = naming (\p nm->(nm,p))                                                   -- function used to asign name a to element b
                       ( (\x->name(fst x))                                               
                        :[(\x->name(fst x)++show n)|n<-[(1::Integer)..]])                -- infinite list of functions to create a name for b
                       (['f':show n|n<-[1..(length rest)]])                              -- list of forbidden names (names already taken)
                       (map fst comboGroups)                                             -- list of elements b that need a name

   splitLineBreak :: String -> [String]
   splitLineBreak [] = []
   splitLineBreak ('\n':s) = []:(splitLineBreak s)
   splitLineBreak [letter] = [[letter]]
   splitLineBreak (l:s)
     | null (splitLineBreak s) = [(l:head (splitLineBreak s))]
     | otherwise               =  (l:head (splitLineBreak s)):(tail$splitLineBreak s)
   