{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables#-}
module Prototype.Object(objectServices) where
--import Char(toUpper)
import NormalForms (disjNF,simplify)
import Auxiliaries (eqCl)
import Adl (target,source
           --,Concept(..),Declaration(..),isTrue,makeInline
           ,ObjectDef(..),Numbered(..),Morphic(..)
           ,Identified(..),mors,Morphism(..),ViewPoint(..)
           ,Object(..),isIdent,Expression(..),mIs
           )
import ShowADL (showADLcode)
import Collection (Collection (rd,rd',(>-)))
import Prototype.RelBinGenSQL(sqlExprSrc,sqlExprTrg,sqlRelPlugs,selectExprBrac,isOne,isOne',selectExpr,sqlPlugFields)
import Prototype.RelBinGenBasics(naming,indentBlock,addToLast,phpIdentifier,addSlashes,commentBlock)
import Data.Fspec
import Data.Plug
import Data.Maybe
import Data.List  hiding (group)
import Version (versionbanner)
import Options
import Rendering.AdlExplanation (explain, format, ExplainOutputFormat(PlainText))

objectServices :: Options 
               -> Fspc
               -> ObjectDef
               -> String
objectServices flags fSpec o
 = (intercalate "\n  "
   ([ "<?php // generated with "++versionbanner
    , ""
    , "/********* on "++(show (pos o))
    , showADLcode fSpec o
    , " *********/"
    , ""
    ] ++ showClasses flags fSpec o ++
    ( if isOne o  -- If the current object is the universal singleton...
      then []
      else generateService_getEach fSpec (name o) o ++
           generateService_read    fSpec (name o) o ++
           generateService_delete  fSpec (name o) o
    )
   )) ++ "\n?>"

generateService_getEach :: Fspc -> String -> ObjectDef -> [String]
generateService_getEach fSpec nm o
 = if sql==Nothing then error "Cannot generate getEach code in Object.hs (line 49)"
   else
   ["function getEach"++phpIdentifier nm++"(){"
   ,"  return firstCol(DB_doquer('" ++
     (fromJust sql)  -- TODO: use PHP code instead, this might yield more getEach functions!
     ++"'));"
   ,"}\n"]
 where sql = ( selectExpr fSpec 31 (sqlExprTrg fSpec (ctx o)) "" (flp (ctx o)))
 
generateService_read :: Fspc -> String -> ObjectDef -> [String]
generateService_read _ nm object
 = ["function read"++phpIdentifier nm++"($id){"
   ,"    // check existence of $id"
   ,"    $obj = new "++phpIdentifier (name object)++"($id);"
   ,"    if($obj->isNew()) return false; else return $obj;"
   ,"}\n"]

phpVar :: String -> String
phpVar x = "$_"++phpIdentifier x

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
         ( if isOne o then [] else ["protected $id=false;","protected $_new=true;"] )
         ++ ["private $_"++phpIdentifier (name a)++";"| a <- attributes o]++
         ["function "++myName++"(" ++ (if isOne o then "" else "$id=null, ")
                                     ++ (intercalate ", " [phpVar (name a)++"=null" | a<-attributes o])
                                     ++"){"
         ]++["  $this->id=$id;" | not (isOne o)]
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
                                 ( if null [a|a<-objats o,isUni (objctx a)]
                                   then ["$me=array();"]
                                   else []            
                                 ) ++ (doPhpGet fSpec
                                               "$me"
                                               0
                                               (o  {objnm =if isOne o then "1" else "$id"
                                                   ,objctx=Tm(mIs(concept o))(-1)
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
              ,"  $this->id=$id;"
              ,"  return $this->id;"
              ,"}"
              ,"function getId(){"
              ,"  if($this->id===null) return false;"
              ,"  return $this->id;"
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
   = if sql==Nothing then error "Cannot check if exists in Object.hs" else fromJust sql
   where expr = if null fs then F [ tm, ctx'] else F (tm:head fs)
         tm   = Tm (Mp1 ("\\''.addSlashes("++var++").'\\'") [] (concept o))(-1)
         ctx' = simplify $ flp (ctx o)
         fs   = [es' | F es' <- [ctx']]
         sql  = selectExpr fSpec 25 (sqlExprSrc fSpec ctx') "" expr
saveTransactions :: Options -> Fspc -> ObjectDef -> [String]
saveTransactions flags fSpec object
  -- | True = error $ show [name plug ++ show([(fldname fld,map fldname (requiredFields plug fld))|fld<-fields plug])|PlugSql plug@(TblSQL{})<-plugs fSpec]
 | otherwise
 = [ "function save(){"
   , "  DB_doquer('START TRANSACTION');"
   ] ++ indentBlock 2
   ( (if null unsavedAtts then commentBlock ["All attributes are saved"] else
     (commentBlock ( ["Attributes that will not be saved are:"
                     ,"--------------------------------------"]++map name unsavedAtts)))
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
         ,"  $DB_err='"++(addSlashes (show(format PlainText (explain fSpec flags rul))))++"';"
         ,"} else"
         ]
       | rul <- rules fSpec
       , or (map (\mpm -> elem mpm (mors rul)) -- rule contains an element
                 (mors object) -- affected mors  ; SJ: mors yields all morphisms inline.
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
  nestTo attr fnc = if null nt then error ("!Fatal (module Prototype>Object 228): saveCodeElem: Cannot nestTo "++show attr++" (ObjBinGenObject)")
                    else head nt
    where nt = nestToRecur attr fnc object "$me" 0
  nestToRecur :: ObjectDef -> (String->[String]) -> ObjectDef -> String -> Integer -> [[String]]
  nestToRecur attr fnc a var d
    | attr==a   = [ if null (objats a) then fnc var else fnc (var)]
    | otherwise = [ if isUni (objctx a') then ans
                    else ["foreach("++var++"['"++name a'++"'] as $i"++show d++"=>$v"++show d++"){"]
                        ++ indentBlock 2 ans ++ ["}"]
                  | a'<-objats a
                  , let mvar = if isUni (objctx a')
                              then var++"['"++name a'++"']"
                              else "$v"++show d
                  , ans<-nestToRecur attr fnc a' (mvar) (d+1)]
  --occurences = ((obj,srcfld),(obj/objatt,trgfld)) clustered by (obj,srcfld)
  --REMARK151210 -> a plug attr field always has a srcfld that is a kernel field, i.e. srcfld is always a kernel field (iskey plug srcfld = True)
  --                as long as fldexpr of attr fields are morphisms, a cluster in occurences is the cluster of a kernel field
  --                (cluster of kernel field = all required fields of kernel field [note: some kernel fields may contain NULL])
  --                when fldexpr of attr fields is an univalent and total complex expr e.g. r;s;t with source C
  --                     and I[C] is a kernel fldexpr
  --                     and SERVICE svc: I[C] = [theR : r = [theS : s = [theT : t]]] 
  --                     then trgfld with fldexpr r;s;t is required, but it will not be in occurences => isLargeOccurance=False
  --                     however it has been covered by the service, so isLargeOccurance should be True
  --                Thus, the php save function may be incorrect for plugs with complex exprs as fldexpr of attr fields
  occurences       plug = (eqCl fst) $ rd $ plugAts plug object
  --fullOccurences are occurences of some object covering all fields in a plug
  --fullOccurences are deleted and inserted again instead of updated
  fullOccurences   plug = filter (isFullOccurance plug) (occurences plug)
  occurencesfields a = (map (snd . snd) a ++ [snd$fst$head a])
  isFullGroup      set a = null (set >- (occurencesfields a))
  isFullOccurance  plug = isFullGroup (tblfields plug)
  --isLargeOccurance tells whether all required fields of attsrc are in some cluster in (occurences plug)
  --fields are required from the perspective of the field for the $id of some object which is not necessarily a UNIQUE KEY of the objects's plug
  isLargeOccurance plug attsrc 
    = isFullGroup (requiredFields plug attsrc)
  delUpdt plug (o,s) var
    = [ "if(isset("++var++maybeId o++")) DB_doquer(\"UPDATE `"++name plug
        ++"` SET `"++fldname s++"`=NULL WHERE `"
        ++ fldname s++"`='\".addslashes("++var++maybeId o++").\"'\",5);"]
  delcode plug (((a,f),_):_)
    = nestTo a
             (\var->["DB_doquer(\"DELETE FROM `"++name plug++"` WHERE `"++(fldname f)
                     ++"`='\".addslashes("++var++ maybeId a ++ ").\"'\",5);"])
  delcode _ [] = error "!Fatal (module Prototype>Object 262): should not occur" -- , but generating no code for no request seems OK
  delCodeElem :: PlugSQL->([String],[ObjectDef])
  delCodeElem plug
    = (   concat (map (delcode plug) (fullOccurences plug))
       ++ concat (map updelcd (occurences plug))
      ,   rd $ map (fst . snd) (concat (fullOccurences plug))
      )
    where
      updelcd [] = []
      updelcd ids@(((attobj,attsrc),_):_)
       = if isFullOccurance plug ids || iskey plug attsrc || not (fldnull attsrc) 
         then []--"//Cannot (or should not) delete using UPDATE in plug "++name plug++", field "++fldname s]
                --WHY1511210 -> why not?
         else
         nestTo attobj
                (\var -> delUpdt plug (attobj,attsrc) var)
  saveCodeElem :: PlugSQL->([String],[ObjectDef])
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
     inscode ids@(((attobj,attsrc),_):_)
      = if not (isLargeOccurance plug attsrc ids) && null keys -- if we have keys, we may use them to UPDATE
        then ["// no code for "++name attobj++","++fldname attsrc++" in "++name plug]
        else
        nestTo attobj
                (\var
                  -> (if isFullOccurance plug ids || iskey plug attsrc || not (fldnull attsrc) then [] else delUpdt plug (attobj,attsrc) var) ++
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
                    ( if isLargeOccurance plug attsrc ids -- attempt INSERT first
                      then ( if (isFullOccurance plug ids || null keys)
                              then -- na een DELETE, of bij null keys kan geen UPDATE
                                  -- reden: een UPDATE heeft een key nodig om zich te hechten
                                  -- (en na een delete hebben we die waarde net weggegooid)
                                  [ "$res="++insQuery var++";" ] ++
                                  if null keys
                                  then []
                                  else if(attobj==object)
                                       then [ "if($newID) $this->setId($me['id']=mysql_insert_id());"]
                                       else [ "if($res!==false && !isset("++var++maybeId attobj++"))"
                                            , "  "++var++maybeId attobj++"=mysql_insert_id();" ]
                              else
                              -- try insert
                              -- the var[id] can correspond to a UNIQUE KEY or UNIQUE INDEX
                              -- if an insert of a UNIQUE KEY fails then we may try to update
                              -- if an insert of a UNIQUE INDEX fails we may not
                              -- we need to cut the kernel element var[id] and everything depending on it from its current index (UPDATE)
                              --         ++ copy deep everything where var[id] depends upon (SELECT)
                              -- and paste it (with potentially changed values) as a new index (INSERT)
                              [ insQuery var ++";"
                                -- zoals hierboven gezegd: een key is nodig voor een UPDATE
                                -- daarom moeten we checken of var[id] een waarde heeft
                                -- zo niet, dan is ofwel de key null, ofwel de te inserten waarde
                              , "if(mysql_affected_rows()==0 && "++var++"['id']!=null){"
                              , "  //nothing inserted, try updating:"
                              ]
                              ++ (if fldnull (snd objkfld)
                                 then [ "  "++line++";"|line<-copycutinsQuery var]
                                 else [ "  "++updQuery var++";"])
                              ++ [ "}" ]
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
                        [ (attobj, attsrc) | attsrc `notElem` (map snd ownAts)]
              keys :: [(ObjectDef,SqlField)]
              keys   = if length attrs==1
                        then [] -- het kan gebeuren dat er precies een attr is
                                -- aangezien dit keys voor UPDATE zijn, stelt deze key dan niet
                                -- echt veel meer voor. Gevolg van deze keuze is dat de UPDATE
                                -- expressies met UPDATE .. SET (lege lijst) WHERE key=val
                                -- niet meer voorkomen, netjes weggefilterd worden
                        else filter ((iskey plug).snd) $ reverse attrs -- eerst de voor de hand liggende
              objkfld --this is the kernel field with instances of $id of this object
                | null (findkfld++keys) = error ("!Fatal (module Prototype>Object 351): There is no key for this object in plug "++name plug)
                | otherwise = head (findkfld++keys)
              findkfld = [(svc,fld)|(svc,fld)<-keys,concept attobj==target(fldexpr fld)]
                -- nunios: Not UNI ObjectS: objects that are not Uni
              nunios = [(o,f)|(o,f)<-ownAts, attobj/=o, not $ isUni (objctx o)]
              ownAts = map snd ids
              insQuery :: String -> String  -- (var as returned by nestTo) -> (query)
              insQuery var
                = "DB_doquer(\"" ++ "INSERT IGNORE INTO `"++name plug
                  ++ "` ("++intercalate "," ["`"++fldname f++"`" | (_,f)<-rd' (fldname.snd) attrs]
                  ++ ") VALUES (" ++
                  intercalate ", "
                        [ if fldnull f || fldauto f
                          then "\".(" ++ ( if fldauto f && o==object
                                           then "!$newID"
                                           else "(null!=" ++ varname var o ++ ")"
                                         ) ++ "?\"'\".addslashes("
                              ++ varname var o ++ ").\"'\":\"NULL\").\""
                          else "'\".addslashes("++varname var o++").\"'"
                        | (o,f)<-rd' (fldname.snd) attrs
                        ] ++ ")\", 5)"
              --like insQuery, only also taking values not in $me from $old (i.e. copyflds)
              --the names of array old are fldnames
              copyinsQuery :: String -> String  
              copyinsQuery var
                = "DB_doquer(\"" ++ "INSERT IGNORE INTO `"++name plug
                  ++ "` ("
                  ++  intercalate "," (["`"++fldname f++"`" | (_,f)<-rd' (fldname.snd) attrs]
                                     ++["`"++fldname f++"`" |f<-copyflds])
                  ++ ") VALUES ("
                  ++ intercalate ", "
                        ([ if fldnull f || fldauto f
                           then "\".(" ++ ( if fldauto f && o==object
                                            then "!$newID"
                                            else "(null!=" ++ varname var o ++ ")"
                                          ) ++ "?\"'\".addslashes("
                               ++ varname var o ++ ").\"'\":\"NULL\").\""
                           else "'\".addslashes("++varname var o++").\"'"
                         | (o,f)<-rd' (fldname.snd) attrs
                         ]
                         ++
                         [if fldnull f || fldauto f
                           then "\".((null!=$old['" ++ fldname f ++ "'])"
                                           ++ "?\"'\".addslashes($old['" ++ fldname f ++ "']).\"'\":\"NULL\").\""
                           else "'\".addslashes($old['" ++ fldname f ++ "']).\"'"
                         |f<-copyflds] 
                        )
                  ++ ")\", 5)"
              --var and everything that requires var is set to NULL in the current record of var (WHERE varfld=var)
              --all these vars must be covered in this service to be able to insert them in 
              --TODO151210 -> service generation does not put fldexpr=(flp m) [UNI] in the service of its kernel field 
              --              but in the service of its target field
              --              Put it in both!! 
--      //(tblfields behalve wat ik al heb) && required
--      $old = firstRow(DB_doquer("SELECT `Datatype`.`I` FROM `Datatype` WHERE `Datatype`.`value1`='".addslashes($me['id'])."'")); 
--	//set to NULL value1 en alles dat value1 als requiredFld heeft
--	DB_doquer("UPDATE `Datatype` SET `value1`=NULL, `attr1`=NULL, `attr2`=NULL WHERE `value1`='".addslashes($me['id'])."'", 5);
--	//nog een keer insert
              copycutinsQuery var
               | isFullGroup requiresFld ids
                = [ if null copyflds then "//all required fields are available"
                    else "$old = firstRow(DB_doquer(\"SELECT "
                                      ++ (intercalate ", " ["`"++fldname f++"`"| f<-copyflds])
                                      ++ " FROM `"++name plug++"`"
                                      ++ " WHERE `"++fldname (snd objkfld)++"`='\".addslashes("++varname var (fst objkfld)++").\"'\"));"
                  ,"DB_doquer(\"" ++ "UPDATE `"++name plug++"` SET " ++
                   intercalate ", "
                         [ "`"++fldname f++"`="++
                           if fldnull f then "NULL"
                           else error "!Fatal (module Prototype>Object 410): you cannot use copycutupdinsQuery for objkfld=UNIQUE KEY"
                         | (o,f)<-attrs, elem f requiresFld
                         ] ++ " WHERE `"++fldname (snd objkfld)++"`='\".addslashes("++varname var (fst objkfld)++").\"'" ++"\", 5)"
                  , if null copyflds then insQuery var 
                    else copyinsQuery var
                  ]
               | otherwise = ["//Service is not suitable for updates in plug "++ name plug]
              requiresFld = [f |f<-tblfields plug, requires plug (f,snd objkfld)]
              --copyflds is tblfields which are required by $id (objkfld) except what's in $me (i.e. occurencesfields ids)
              --this way I do not require a very large service object only to be able to edit an object
              copyflds = [f|f<-tblfields plug, elem f (requiredFields plug (snd objkfld)),not(elem f (occurencesfields ids))]
              updQuery var
                = "DB_doquer(\"" ++ "UPDATE `"++name plug++"` SET " ++
                  intercalate ", "
                        [ "`"++fldname f++"`="++
                          if fldnull f && not (f==attsrc)
                          then "\".(" ++ ( if fldauto f && o==object
                                           then "!$newID"
                                           else "(null!=" ++ varname var o ++ ")"
                                         ) ++ "?\"'\".addslashes("
                              ++ varname var o ++ ").\"'\":\"NULL\").\""
                          else "'\".addslashes("++varname var o++").\"'"
                        | (o,f)<-attrs, fst objkfld/=o
                        ] ++ " WHERE `"++fldname (snd objkfld)++"`='\".addslashes("++varname var (fst objkfld)++").\"'" ++"\", 5)"
              varname var o = ( if attobj == o
                                then var
                                else if isUni (objctx o)
                                      then var++"['"++(name o)++"']"
                                      else "$"++(phpIdentifier $ name o)
                              ) ++ maybeId o

--objPlugs returns the plugs needed to save data visible in this service.
--A plug is needed if sqlPlugFields returns two fields for some objctx of object/objats or the identify of its target (see plugAts)
--REMARK: only used for php function save()
--WHY151210 -> (see also Data.FSpec and Rendering.ClassDiagram) can't a php plug be a (php-)function for saving things?
objPlugs :: Fspc -> ObjectDef -> [PlugSQL]
objPlugs fSpec object = [plug|PlugSql plug<-plugs fSpec, not (null (plugAts plug object))]

--plugAts returns the source/target fields related to object=>objats for objctx of object=>objats
--REMARK: only used for php function save()
plugAts :: PlugSQL -> ObjectDef -> [((ObjectDef, SqlField), (ObjectDef,SqlField))]
plugAts plug object = plugAts' object object --you do not want to forget to mention where the (objctx object) is stored (it is not always I)
  where
  plugAts' :: ObjectDef           -- parent (wrong values are allowed, see source)
             -> ObjectDef                -- object itself
             -> [((ObjectDef, SqlField), -- source (may include the wrong-valued-'parent')
                (ObjectDef,SqlField))]   -- target
  plugAts' p o 
   = nub$
     ([ ((o,sf),(o,tf))
      | (sf,tf)<-sqlPlugFields plug (Tm (mIs (target (objctx o))) (-1))
      ]
--     ++ TODO -> I also want fields if some flip matches, sqlPlugFields 
--      [ ((p,sf),(o,tf))
--      | (sf,tf)<-sqlPlugFields plug (flp(objctx o))
--      ]
     ++
      [ ((p,sf),(o,tf))
      | (sf,tf)<-sqlPlugFields plug (objctx o)
      ])
     ++ concat (map (plugAts' o) [att | att <- objats o])


isObjUni :: ObjectDef -> Bool
isObjUni obj = isUni (objctx obj)

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
    doQuer fl [] = error ("!Fatal (module Prototype>Object 438): doPhpGet: doQuer has no query for "++fl);
    doQuer firstLine
           quer = addToLast "\"));"
                            ([ firstLine++"(DB_doquer(\""++head quer]
                                ++ map ((++) (take (12+length firstLine) (repeat ' ')))
                                       (tail quer)
                            )
    idAt = objOut{objctx=targetCpt,objnm="id",objats=[]}
    targetCpt = Tm(mIs$target$objctx objOut)(-1)
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

{- doSqlGet genereert de SQL-query die nodig is om het PHP-object objOut van inhoud te voorzien.
   objIn representeert een PHP-object dat een subset is van PHP-object objOut.
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
                     (if isOne' objOut then [] else [" WHERE " ++ snd (head tbls)])
               )
   where comboGroups'::[((PlugSQL,(ObjectDef,SqlField)),[(ObjectDef,SqlField)])]
         comboGroups'= reduce ({-sort' (length)-} (eqCl fst combos)) --WHY: wordt dit op lengte gesorteerd, waarom zijn langere lijsten belangrijker? Ik heb het gedisabled omdat het fouten gaf in SELECT queries met morphisms die gekoppeld zijn aan binaire tabellen
         comboGroups = keyGroups ++ (comboGroups' >- keyGroups)
-- keyGroups representeert de plug-informatie die nodig is voor het atoom aan de rand van objIn, wat de bron is van waaruit objOut wordt opgebouwd.
-- 10dec2010: (fldnull s) kan true zijn voor keyGroup als s: INJ, maar niet SUR, en in kernel.
--      N.B. als sqlRelPlugs (zie combos) aangeeft dat (plug,fld0,fld1) gebruikt kan worden dan maakt het toch niet uit of (fldnull s) waar is of niet?
         keyGroups   = take 1 ( comboGroups' {-[gr|gr@((_,(_,s)),_)<-comboGroups',not $ fldnull s]-} ++ 
                                [((p,(objIn,s)),[(objIn,t)])
                                | (p,s,t)<-sqlRelPlugs fSpec (Tm(mIs$target (objctx objIn))(-1))]   -- zoek een conceptentabel op....
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
                                ++ if isOne' objOut then [] else error ("!Fatal (module Prototype>Object 511): doSqlGet in ObjBinGenObject: Cannot create keyGroups for " ++name objOut)
                              )
         reduce :: [[((PlugSQL,(ObjectDef,SqlField)),(ObjectDef,SqlField))]]
                   -> [((PlugSQL,(ObjectDef,SqlField)),[(ObjectDef,SqlField)])]
         reduce [] = []
         reduce (g:gs)    = (fst (head g),map snd g):reduce
                            [ res
                            | group<-gs
                            , let ga=fst$snd$head g
                            , let res=[((plug,(ai,sf)),(a,tf))
                                      |((plug,(ai,sf)),(a,tf))<-group,ga/=a]
                            , not (null res)]
         combos           = [ ((plug,(ai,fld0)),(a,fld1))
                            | ai<-objats (objIn)++[objIn]
                            , a<-aOuts
                            , Just e' <-[takeOff (objctx ai) (objctx a)]
                            , (plug,fld0,fld1)<-sqlRelPlugs fSpec e'
                            ]
         takeOff :: Expression->Expression->Maybe Expression
         takeOff (F (a:as)) (F (b:bs)) | disjNF a==disjNF b = takeOff (F as) (F bs)
         takeOff a (F (b:bs)) | disjNF a== disjNF b = Just (F bs)
         takeOff a e' | isIdent a = Just e'
         takeOff _ _ = Nothing
         aOuts           = [a|a<-objats( objOut)]
         rest :: [(ObjectDef,Integer)]
         rest            = zip [ a | a<-aOuts
                                   , a `notElem` [a' | g <- comboGroups, (a',_) <- snd g] 
                               ] [(1::Integer)..]
         --the list of fields that are selected (SELECT DISTINCT fieldNames FROM ...)
         fieldNames      = [ "`"++tableReName gr++"`.`"++fldname f++"`"
                             ++(if fldname f == name a then [] else " AS `"++name a++"`")
                           | (gr@(_,_),as)<-comboGroups',(a,f)<-as
                           ] ++
                           [if isIdent (objctx a)
                            then "'\".addslashes("++name (objIn)++").\"'"++" AS `"++name a++"`"
                            else "`f"++(show n)++"`.`"++sqlExprTrg fSpec (objctx a)++"`" ++
                                 (if name a == sqlExprTrg fSpec (objctx a)
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
                           , let l=restLines (a,n)
                           , not (null l)
                           , let r=if null$head l then tail l else l
                           , not (null r)
                           ]
         joinOn :: ([String],String)->[String]
         joinOn ([t],jn) = [ (if isOne' objOut then "     , "      else (if isArr then " " else "  LEFT")++" JOIN ")++t
                           ++(if isOne' objOut then "" else " ON "++jn)]
         joinOn (ts,jn)  = [ (if isOne' objOut then "     , " else (if isArr then " " else "  LEFT")++" JOIN ")++(head ts)]
                           ++indentBlock (if isOne' objOut then 7 else 12) (tail ts)++(if isOne' objOut then [] else (["    ON "++jn]))
         restLines (outAtt,n)
           = if sql==Nothing then error$ "Cannot get a query for "
                                     ++(show( objctx outAtt
                                       ,sqlExprTrg fSpec (objctx outAtt),isOne' objOut,sqlExprSrc fSpec (objctx outAtt) 
                                       ))++" in Object.hs (line 578)"
             else splitLineBreak ((fromJust sql) ++ " AS f"++show n) -- better names?
           where sql = selectExprBrac fSpec (-4)
                                            (if isOne' objOut then "" else sqlExprSrc fSpec (objctx outAtt))
                                            (sqlExprTrg fSpec (objctx outAtt))
                                            (objctx outAtt)
         tableName gr@(p,_) = if name p == tableReName gr then "`"++name p++"`" else "`"++name p ++ "` AS "++tableReName gr
         tableReName :: (PlugSQL,(ObjectDef,SqlField)) -> String
         tableReName gr   = head [nm | (nm,gr')<-renamedTables,gr'==gr]
         renamedTables :: [(String,(PlugSQL,(ObjectDef,SqlField)))]
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

