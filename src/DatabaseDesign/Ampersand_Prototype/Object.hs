{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables#-}
module DatabaseDesign.Ampersand_Prototype.Object
   (objectInterfaces) 
where 
import Data.Maybe
import Data.List  hiding (group)

import DatabaseDesign.Ampersand_Prototype.CoreImporter
import DatabaseDesign.Ampersand_Prototype.RelBinGenSQL 
import DatabaseDesign.Ampersand_Prototype.RelBinGenBasics(indentBlock,addToLast,phpIdentifier)
import DatabaseDesign.Ampersand_Prototype.Version 

fatal :: Int -> String -> a
fatal = fatalMsg "Object"

objectInterfaces :: Options 
                     -> Fspc
                     -> ObjectDef
                     -> String
objectInterfaces flags fSpec o
 = (intercalate "\n  "
   ([ "<?php // generated with "++ampersandPrototypeVersionBanner
    , ""
    , "/********* on "++(show (origin o))
    , (showADL . disambiguate fSpec) o
    , " *********/"
    , ""
    ]
    ++
    showClasses flags fSpec o
    ++
    concat[showClasses flags fSpec att |att<-attributes o,(not.null)(attributes att),not(isOne att)]
                                --      ,let att=if isOne att then att{objctx=ERel(mIs(target(objctx att)))} else att']
    ++
    ( if isOne o  -- If the current object is the universal singleton...
      then []
      else generateInterface_getEach fSpec (name o) o ++
           generateInterface_read    fSpec (name o) o ++
           generateInterface_delete  fSpec (name o) o
    )
   )) ++ "\n?>"

generateInterface_getEach :: Fspc -> String -> ObjectDef -> [String]
generateInterface_getEach fSpec nm o
 = if sql==Nothing then fatal 42 "Cannot generate getEach code"
   else
   ["function getEach"++phpIdentifier nm++"(){"
   ,"  return firstCol(DB_doquer('" ++
     (fromJust sql)  -- TODO: use PHP code instead, this might yield more getEach functions!
     ++"'));"
   ,"}\n"]
 where  sql = ( selectExpr fSpec 31 (sqlExprTrg fSpec (ctx o)) "" (flp (ctx o)))
 
generateInterface_read :: Fspc -> String -> ObjectDef -> [String]
generateInterface_read _ nm object
 = ["function read"++phpIdentifier nm++"($id){"
   ,"    // check existence of $id"
   ,"    $obj = new "++phpIdentifier (name object)++"($id);"
   ,"    if($obj->isNew()) return false; else return $obj;"
   ,"}\n"]

phpVar :: String -> String
phpVar x = "$_"++phpIdentifier x

generateInterface_delete :: Fspc -> String -> ObjectDef -> [String]
generateInterface_delete _ nm _
 = ["function del"++phpIdentifier nm++"($id){"
   ,"  $tobeDeleted = new "++phpIdentifier nm++"($id);"
   ,"  if($tobeDeleted->isNew()) return true; // item never existed in the first place"
   ,"  if($tobeDeleted->del()) return true; else return $tobeDeleted;"
   ,"}\n"]


-- | The function showClasses defines the PHP class of an object o and also (recursively) the
--   PHP-classes of all subordinate objects (i.e. the attributes) of o.
--   context  : the P_Context of object o. In due time, this will be replaced by an Fspc
--   triggers : a [possibly empty] set of triggers, that is used to generate automated functionality.
--              Precondition: This set contains precisely those triggers that may be used within the transaction
--              boundary of the class.
--              In due time, this parameter will become a selection from the entire set of triggers in Fspc.
--   nms      : the name trail of all super-objects until the root of this interface. This is used to generate unique names for every field.
--   o        : the object to be transformed in a class.
showClasses :: Options -> Fspc -> ObjectDef -> [String]
showClasses flags fSpec o
 = [ "class "++myName ++" {"] ++
   indentBlock 2 (
         ( if isOne o then [] else ["protected $id=false;","protected $_new=true;"] )
         ++ ["private $_"++phpIdentifier (name a)++";" | a <- attributes o]++
         ["function "++myName++"(" ++ (if isOne o then "" else "$id=null" ++ [',' |not(null(attributes o))])
                                     ++ (intercalate ", " [phpVar (name a)++"=null" | a<-attributes o])
                                     ++",$sel=True){"
         ]++["  $this->id=$id;" | not (isOne o)]
         ++ ["  $this->_"++phpIdentifier (name a)++"="++phpVar (name a)++";" | a <- attributes o]
         ++ concat
            [["  // check if it exists:"
             ,"  $ctx = DB_doquer('"++(doesExistQuer "$id")++"');"
             ,"  if(count($ctx)==0) $this->_new=true; else $this->_new=false;"]
            |null(attributes o),not(target(ctx o)==ONE)] --INTERFACE o: ctx where  target ctx/=ONE and objats=[]
         ++  (           [ "  if(" -- ++(head (["!isset("++phpVar (name a')++")" |a'<-attributes o,mayedit (objctx a') editable]++["True"]))
                                        ++(if isOne o then "$sel" else "$sel && isset($id)")++"){"
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
                                 ( if null [a |a<-objats o,isUni (objctx a)]
                                   then ["$me=array();"]
                                   else []            
                                 ) ++ (doPhpGet fSpec
                                               "$me"
                                               0
                                               (o  {objnm =if isOne o then "1" else "$id"
                                                   ,objctx=ERel(I(concept o))
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
                   ) ++
         ["}\n"]
         ++ savefunction flags fSpec "save" 0 o
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
  myName = phpIdentifier(name o)
  doesExistQuer :: [Char] -> String
  doesExistQuer var
   = if sql==Nothing then fatal 167 "Cannot check if exists in Object.hs" else fromJust sql
   where expr = if null fs then ECps [ tm, ctx'] else ECps (tm:head fs)
         tm   = ERel (Mp1 ("\\''.addSlashes("++var++").'\\'") (concept o))
         ctx' = simplify $ flp (ctx o)
         fs   = [es' | ECps es' <- [ctx']]
         sql  = selectExpr fSpec 25 (sqlExprSrc fSpec ctx') "" expr
{- still working on new save() and del()
saveTransactions :: Options -> Fspc -> ObjectDef -> [String]
saveTransactions flags fSpec object
  -- | True = fatal 176 (show [name plug ++ show([(fldname fld,map fldname (requiredFields plug fld)) |fld<-fields plug]) |PlugSql plug@(TblSQL{})<-plugs fSpec])
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
         ,"  $DB_err='"++(addSlashes (show(format PlainText (purpose fSpec flags rul))))++"';"
         ,"} else"
         ]
       | rul <- rules fSpec
       , or (map (\mpm -> elem mpm (mors rul)) -- rule contains an element
                 (mors object) -- affected mors  ; SJ: mors yields all relations inline.
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
  nestTo attr fnc = if null nt then fatal 229 $ "saveCodeElem: Cannot nestTo "++show attr++" (ObjBinGenObject)"
                    else head nt
    where  nt = nestToRecur attr fnc object "$me" 0
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
  --                as long as fldexpr of attr fields are relations, a cluster in occurences is the cluster of a kernel field
  --                (cluster of kernel field = all required fields of kernel field [note: some kernel fields may contain NULL])
  --                when fldexpr of attr fields is an univalent and total complex expr e.g. r;s;t with source C
  --                     and I[C] is a kernel fldexpr
  --                     and INTERFACE ifc: I[C] = [theR : r = [theS : s = [theT : t]]] 
  --                     then trgfld with fldexpr r;s;t is required, but it will not be in occurences => isLargeOccurance=False
  --                     however it has been covered by the interface, so isLargeOccurance should be True
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
  delcode _ [] = fatal 272 "should not occur" -- , but generating no code for no request seems OK
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
                              --         ++ copy deep everything where  var[id] depends upon (SELECT)
                              -- and paste it (with potentially changed values) as a new index (INSERT)
                              [ insQuery var ++";"
                                -- zoals hierboven gezegd: een key is nodig voor een UPDATE
                                -- daarom moeten we checken of var[id] een waarde heeft
                                -- zo niet, dan is ofwel de key null, ofwel de te inserten waarde
                              , "if(mysql_affected_rows()==0 && "++var++"['id']!=null){"
                              , "  //nothing inserted, try updating:"
                              ]
                              ++ (if fldnull (snd objkfld)
                                 then [ "  "++line++";" |line<-copycutinsQuery var]
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
        where  attrs  = ownAts ++ -- ook het identiteit-veld toevoegen (meestal de SQL-`id`)
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
                | null (findkfld++keys) = fatal 371 $ "There is no key for this object in plug "++name plug
                | otherwise = head (findkfld++keys)
              findkfld = [(ifc,fld) |(ifc,fld)<-keys,concept attobj==target(fldexpr fld)]
                -- nunios: Not UNI ObjectS: objects that are not Uni
              nunios = [(o,f) |(o,f)<-ownAts, attobj/=o, not $ isUni (objctx o)]
              ownAts = map snd ids
              insQuery :: String -> String  -- (var as returned by nestTo) -> (query)
              insQuery var
                = "DB_doquer(\"" ++ "INSERT IGNORE INTO `"++name plug
                  ++ "` ("++intercalate "," ["`"++fldname f++"`" | (_,f)<-nubBy (fldname.snd) attrs]
                  ++ ") VALUES (" ++
                  intercalate ", "
                        [ if fldnull f || fldauto f
                          then "\".(" ++ ( if fldauto f && o==object
                                           then "!$newID"
                                           else "(null!=" ++ varname var o ++ ")"
                                         ) ++ "?\"'\".addslashes("
                              ++ varname var o ++ ").\"'\":\"NULL\").\""
                          else "'\".addslashes("++varname var o++").\"'"
                        | (o,f)<-nubBy (fldname.snd) attrs
                        ] ++ ")\", 5)"
              --like insQuery, only also taking values not in $me from $old (i.e. copyflds)
              --the names of array old are fldnames
              copyinsQuery :: String -> String  
              copyinsQuery var
                = "DB_doquer(\"" ++ "INSERT IGNORE INTO `"++name plug
                  ++ "` ("
                  ++  intercalate "," (["`"++fldname f++"`" | (_,f)<-nubBy (fldname.snd) attrs]
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
                         | (o,f)<-nubBy (fldname.snd) attrs
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
              --all these vars must be covered in this interface to be able to insert them in 
              --TODO151210 -> interface generation does not put fldexpr=(flp r) [UNI] in the interface of its kernel field 
              --              but in the interface of its target field
              --              Put it in both!! 
--      //(tblfields behalve wat ik al heb) && required
--      $old = firstRow(DB_doquer("SELECT `Datatype`.`I` FROM `Datatype` WHERE `Datatype`.`value1`='".addslashes($me['id'])."'")); 
--        //set to NULL value1 en alles dat value1 als requiredFld heeft
--        DB_doquer("UPDATE `Datatype` SET `value1`=NULL, `attr1`=NULL, `attr2`=NULL WHERE `value1`='".addslashes($me['id'])."'", 5);
--        //nog een keer insert
              copycutinsQuery var
               | isFullGroup requiresFld ids
                = [ if null copyflds then "//all required fields are available"
                    else "$old = firstRow(DB_doquer(\"SELECT "
                                      ++ (intercalate ", " ["`"++fldname f++"`" | f<-copyflds])
                                      ++ " FROM `"++name plug++"`"
                                      ++ " WHERE `"++fldname (snd objkfld)++"`='\".addslashes("++varname var (fst objkfld)++").\"'\"));"
                  ,"DB_doquer(\"" ++ "UPDATE `"++name plug++"` SET " ++
                   intercalate ", "
                         [ "`"++fldname f++"`="++
                           if fldnull f then "NULL"
                           else fatal 440 "you cannot use copycutupdinsQuery for objkfld=UNIQUE KEY"
                         | (_,f)<-attrs, elem f requiresFld
                         ] ++ " WHERE `"++fldname (snd objkfld)++"`='\".addslashes("++varname var (fst objkfld)++").\"'" ++"\", 5)"
                  , if null copyflds then insQuery var 
                    else copyinsQuery var
                  ]
               | otherwise = ["//Interface is not suitable for updates in plug "++ name plug]
              requiresFld = [f |f<-tblfields plug, requires plug (f,snd objkfld)]
              --copyflds is tblfields which are required by $id (objkfld) except what's in $me (i.e. occurencesfields ids)
              --this way I do not require a very large interface object only to be able to edit an object
              copyflds = [f |f<-tblfields plug, elem f (requiredFields plug (snd objkfld)),not(elem f (occurencesfields ids))]
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

--objPlugs returns the plugs needed to save data visible in this interface.
--A plug is needed if sqlPlugFields returns two fields for some objctx of object/objats or the identify of its target (see plugAts)
--REMARK: only used for php function save()
--WHY151210 -> (see also Data.FSpec and Rendering.ClassDiagram) can't a php plug be a (php-)function for saving things?
objPlugs :: Fspc -> ObjectDef -> [PlugSQL]
objPlugs fSpec object = [plug |InternalPlug plug<-plugInfos fSpec, not (null (plugAts plug object))]

--plugAts returns the source/target fields related to object=>objats for objctx of object=>objats
--REMARK: only used for php function save()
plugAts :: PlugSQL -> ObjectDef -> [((ObjectDef, SqlField), (ObjectDef,SqlField))]
plugAts plug object = plugAts' object object --you do not want to forget to mention where  the (objctx object) is stored (it is not always I)
  where
  plugAts' :: ObjectDef           -- parent (wrong values are allowed, see source)
             -> ObjectDef                -- object itself
             -> [((ObjectDef, SqlField), -- source (may include the wrong-valued-'parent')
                (ObjectDef,SqlField))]   -- target
  plugAts' p o 
   = nub$
     ([ ((o,sf),(o,tf))
      | (sf,tf)<-sqlPlugFields plug (ERel (I (target (objctx o))))
      ]
     ++
      [ ((p,sf),(o,tf))
      | (sf,tf)<-sqlPlugFields plug (objctx o)
      ])
     ++ concat (map (plugAts' o) [att | att <- objats o])
-}

isObjUni :: ObjectDef -> Bool
isObjUni obj = isUni (objctx obj)

--TODO-> (zie probleem in commentaar bij truncKeepUni)
--  Zolang objIn{objnm=$id,objats=[]} en truncKeepUni att = trunc att = att{objats=[]} zullen alle objIn geen objats hebben
--  Dus (objats objOut) == depthNeeded == allNeeded
--  Code gebaseerd op (objats objIn) heb ik daarom in commentaar gezet {-% code %-}, omdat deze nooit geraakt wordt.
--  en truncKeepUniNamed nm = truncKeepUni = trunc = (\att->att{objats=[]}) allemaal vervangen door trunc
--  Daardoor is het duidelijker wat er nu kan gebeuren.
--  Weer aanzetten als problem bij truncKeepUni is opgelost
doPhpGet :: Fspc -> String->Integer-> ObjectDef -> ObjectDef -> [String]
doPhpGet fSpec objVar depth objIn objOut
  = --fill id's of attributes of objOut
    getRowOfUnis
    ++ 
    concat [getArrayOf aout |aout <- arrsNeeded]
    ++
    --fill attributes of attributes of objOut (recursion on objats of INTERFACE a.k.a. objOut)
    concat [ if isObjUni aout
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
           ,let nest = (doPhpGet fSpec var (depth+1) ({-% truncKeepUniNamed var %-}trunc aout){objnm=idvar} aout)
           ,not$null$nest]
  where
    mname aout = objVar++"['"++name aout++"']"
    unisNeeded = [aout |aout<-allNeeded,isObjUni aout]
    arrsNeeded = [aout |aout<-allNeeded,not$isObjUni aout]
    allNeeded  = [aout |aout<-objats objOut {-% ,null[a |a<-objats objIn,objctx a==objctx aout] %-}]
    depthNeeded= [aout |aout<-objats objOut {-% ,null[a |a<-objats objIn,a `msubset` aout] %-} ]
    {-% msubset i o= and ((objctx i==objctx o) -- must contain same elems
                          :[or [msubset i' o' |i'<-objats i] -- and at least every attribute of o must be in i
                           |o'<-objats o]) %-}
    --template to do a query => firstline (DB_doquer( quer ));
    doQuer fl [] = fatal 540 $ "doPhpGet: doQuer has no query for "++fl
    doQuer firstLine quer 
     = addToLast "\"));" ([ firstLine++"(DB_doquer(\""++head quer] ++ map ((++) (take (12+length firstLine) (repeat ' '))) (tail quer))
    trunc att = att{objats=[]}
    ----------fill id's of attributes of objOut functions
    --one query to get a row containing all UNI values
    --objVar=$me, or $(mname aout) (UNI), or $v++depth (ARRAY)
    getRowOfUnis 
     | null unisNeeded = []
     | otherwise
        = let idAt = trunc(objOut{objctx=targetCpt,objnm="id"})
              targetCpt = ERel(I$target$objctx objOut)
          in doQuer (objVar++"=firstRow") (doSqlGet fSpec False objIn (objOut{objats= idAt : map trunc unisNeeded,objctx=targetCpt}))
    -- getArrayOf: code for leaves or for nodes (in the objDef tree) is different. For leaves: use firstCol to show them
    getArrayOf aout 
     = doQuer (mname aout ++ "="++ (if null(objats aout) then "firstCol" else ""))
              (doSqlGet fSpec True objIn objOut{objats=[{-% truncKeepUni %-}trunc (if null(objats aout) then aout else aout{objnm="id"}) ]})
    {-% 
    truncKeepUni = trunc -- (zie TODO boven bij doPhpGet)
                         -- WHY is dit een probleem m.a.w. wil je wel recursie in SQL && PHP?
                         -- de SQL functie gaat (nog) niet goed om met recursie!
                         -- bovendien doet deze PHP functie dit evenmin als de recursie
                         -- verder gaat dan 1 diep, dwz dat onderstaande definitie:
    -- truncKeepUni att = att{objats=[truncKeepUni a |a<-objats att,isObjUni ( a)]}
                         -- ook niet werkt in deze functie, en dat onderstaande:
    -- truncKeepUni att = att{objats=[trunc a |a<-objats att,isObjUni ( a)]}
                         -- alleen niet werkt in de SQL functie
    truncKeepUniNamed nm att = att{objats=[ a{objnm=nm++"["++(show$name a)++"]"} |a<-objats (truncKeepUni att) ]}
    %-}

{- doSqlGet genereert de SQL-query die nodig is om het PHP-object objOut van inhoud te voorzien.
   objIn representeert een PHP-object dat een subset is van PHP-object objOut.
   objIn representeert het deel van objOut dat bij aanroep reeds gevuld is.
   Dit voorkomt onnodige database accessen.
   De parameter 'isArr' vertelt of het een array betreft of een enkel veld.
-}
doSqlGet :: Fspc -> Bool -> ObjectDef -> ObjectDef -> [String]
doSqlGet fSpec isArr objIn objOut
 | length(objats objOut)==1 && isIdent(objctx objOut)
   --different query composer is used to prevent NULL in lists of INTERFACE Concepts:I[ONE] = [listOfConcepts:V[ONE*Concept]]
   && source(objctx objOut)==ONE && isTrue((objctx.head.objats)objOut) = [showsql(SqlSel1(selectdomain fSpec ((target.objctx.head.objats)objOut)))]
 | otherwise
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
   where  
   aOuts = [a |a<-objats objOut]
   rest :: [(ObjectDef,Integer)]
   rest = zip [ a | a<-aOuts, a `notElem` [a' | g <- comboGroups, (a',_) <- snd g]]
              [(1::Integer)..]
   --WHY: wordt dit op lengte gesorteerd, waarom zijn langere lijsten belangrijker?
   --Ik heb het gedisabled omdat het fouten gaf in SELECT queries met relations die gekoppeld zijn aan binaire tabellen
   comboGroups'::[((PlugSQL,(ObjectDef,SqlField)),[(ObjectDef,SqlField)])]
   comboGroups'= reduce ({-sort' (length)-} (eqCl fst combos)) 
      where
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
      combos 
       = [ ((plug,(ai,fld0)),(a,fld1))
         | ai<-objats objIn --(see TODO doPhpGet => altijd objats objIn=[])
         , a<-aOuts
         , Just e' <-[takeOff (objctx ai) (objctx a)]
         , (plug,fld0,fld1)<-sqlRelPlugs fSpec e'
         ]
         ++
         [ ((plug,(ai,fld0)),(a,fld1))
         | ai<-[objIn] 
         , a<-aOuts
         , source(objctx ai) == target (objctx a) --just to be sure
         , (plug,fld0,fld1)<-sqlRelPlugs fSpec (ECps [objctx ai,objctx a])
         ]
   takeOff :: Expression->Expression->Maybe Expression
   takeOff (ECps (a:as)) (ECps (b:bs)) | disjNF a==disjNF b = takeOff (ECps as) (ECps bs)
   takeOff a (ECps (b:bs)) | disjNF a== disjNF b = Just (ECps bs)
   takeOff a e' | isIdent a = Just e'
   takeOff _ _ = Nothing
   comboGroups = keyGroups `uni` comboGroups'
      where
      -- keyGroups representeert de plug-informatie die nodig is voor het atoom aan de rand van objIn,
      -- wat de bron is van waaruit objOut wordt opgebouwd.
      -- 10dec2010: (fldnull s) kan true zijn voor keyGroup als s: INJ, maar niet SUR, en in kernel.
      --N.B. als sqlRelPlugs (zie combos) aangeeft dat (plug,fld0,fld1) gebruikt kan worden
      --     dan maakt het toch niet uit of (fldnull s) waar is of niet?
      keyGroups   
       = take 1 
         (  comboGroups' {-[gr |gr@((_,(_,s)),_)<-comboGroups',not $ fldnull s]-}
         ++ 
            [((p,(objIn,s)),[(objIn,t)])
            | (p,s,t)<-sqlRelPlugs fSpec (ERel(I$target (objctx objIn)))]   -- zoek een conceptentabel op....
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
         ++ if isOne' objOut then [] 
            else fatal 653 $ "doSqlGet in ObjBinGenObject: Cannot create keyGroups for " ++name objOut
         )
   --the list of fields that are selected (SELECT DISTINCT fieldNames FROM (see tbls))
   --based on comboGroups' ++ rest
   fieldNames      = [ "`"++tableReName gr++"`.`"++fldname f++"`"
                       ++(if fldname f == name a then [] else " AS `"++name a++"`")
                     | (gr@(_,_),as)<-comboGroups',(a,f)<-as]
                     ++
                     [if isIdent (objctx a)
                      then "'\".addslashes("++name (objIn)++").\"'"++" AS `"++name a++"`"
                      else "`f"++(show n)++"`.`"++sqlExprTrg fSpec (objctx a)++"`" ++
                           (if name a == sqlExprTrg fSpec (objctx a)
                            then []
                            else " AS `"++name a++"`")
                     |(a,n)<-rest]
   --tbls contains the rest of the query
   --based on comboGroups ++ rest
   --[( ["( SELECT DISTINCT cfst.Datatype"    --head fst head tbls => FROM
   --   ,"  FROM `Datatype` AS cfst ) AS f1"] --tail fst head tbls => completing FROM
   -- ,"`f1`.`I`='\".addslashes(1).\"'"       --snd head tbls => WHERE
   -- )]                                      --tail tbls => map joinOn (tail tbls)
   tbls =
      let       
      restLines (outAtt,n)
          = if sql outAtt==Nothing then fatal 677 $ "Cannot get a query for "++(show( objctx outAtt))
            else splitLineBreak ((fromJust (sql outAtt)) ++ " AS f"++show n) -- better names?
      sql outAtt = selectExprBrac 
            fSpec (-4)
            (if isOne' objOut then "" else sqlExprSrc fSpec (objctx outAtt))
            (sqlExprTrg fSpec (objctx outAtt))
            (objctx outAtt)
      in
      [ ([tableName gr]
        ,"`"++tableReName gr++"`.`"++(fldname f)++"`='\".addslashes("++(name a)++").\"'"
        )
      |(gr@(_,(a,f)),_)<-comboGroups]
      ++
      [ (r
         ,"`f"++(show n)++"`.`"++(sqlExprSrc fSpec (objctx a))++
          "`='\".addslashes("++name (objIn)++").\"'"
         )
      | (a,n)<-rest
      , not (isIdent (objctx a)) --do not join on fldexpr of kernelfields in the same plug
      , let l=restLines (a,n)
      , not (null l)
      , let r=if null$head l then tail l else l
      , not (null r)
      ]
   --compose JOINs for all elements in tail tbls 
   joinOn :: ([String],String)->[String]
   joinOn ([t],jn) = [ (if isOne' objOut then "     , "      else (if isArr then " " else "  LEFT")++" JOIN ")++t
                     ++(if isOne' objOut then "" else " ON "++jn)]
   joinOn (ts,jn)  = [ (if isOne' objOut then "     , " else (if isArr then " " else "  LEFT")++" JOIN ")++(head ts)]
                     ++indentBlock (if isOne' objOut then 7 else 12) (tail ts)++(if isOne' objOut then [] else (["    ON "++jn]))
   --tblname AS tblrename
   tableName gr@(p,_) = if name p == tableReName gr then "`"++name p++"`" else "`"++name p ++ "` AS "++tableReName gr
   tableReName :: (PlugSQL,(ObjectDef,SqlField)) -> String
   tableReName gr   = head [nm | (nm,gr')<-renamedTables,gr'==gr]
   renamedTables :: [(String,(PlugSQL,(ObjectDef,SqlField)))]
   renamedTables
     = naming (\p nm->(nm,p))                                                   -- function used to asign name a to element b
              ( (\x->name(fst x))                                               
               :[(\x->name(fst x)++show n) |n<-[(1::Integer)..]])                -- infinite list of functions to create a name for b
              (['f':show n |n<-[1..(length rest)]])                              -- list of forbidden names (names already taken)
              (map fst comboGroups)                                             -- list of elements b that need a name

splitLineBreak :: String -> [String]
splitLineBreak [] = []
splitLineBreak ('\n':s) = []:(splitLineBreak s)
splitLineBreak [letter] = [[letter]]
splitLineBreak (l:s)
  | null (splitLineBreak s) = [(l:head (splitLineBreak s))]
  | otherwise               =  (l:head (splitLineBreak s)):(tail$splitLineBreak s)


--insert a complete record in the plug of this concept (assume there is one plug with one kernelfield that stores this concept)
--     to insert this->id, all requiredfields must be inserted at the same time, otherwise insert fails
--     thus binary inserts are not possible for kernelfields
--update (binary) relations in this instance stored in other records (maybe other tblplugs, not expecting expressions yet)
--update the binary relations in this instance stored in binplugs (DEL $oldthis->rel, INS $newthis->rel)
--TODO -> Move start and close transaction to wrapper and session, add rules to check to session
--TODO -> error reporting (Bas uses jQuery somehow, checkout edit.js)
--TODO -> block inserts with plugid=NULL or "" (or suggest a new page if transactions can be larger than one page)
--the php name of the save function (uniqfnm) is fnm+depth except when depth=0 then it is "save"
savefunction :: Options -> Fspc -> String -> Int -> ObjectDef -> [String]
savefunction flags fSpec fnm depth this
  | isOne this = ["function "++uniqfnm fnm depth++"{}"] --TODO
  | otherwise 
  = let thiscpt = target(objctx this)
        -----------me
        inplugs cpt = [(plug,fld) |InternalPlug plug@(TblSQL{})<-plugInfos fSpec, (c,fld)<-cLkpTbl plug,c==cpt]
                      ++ [(plug,column plug) |InternalPlug plug@(ScalarSQL{})<-plugInfos fSpec, cLkp plug==cpt]
        (myplug,idfld) = if not(null (inplugs thiscpt)) then head (inplugs thiscpt)
                           else fatal 746 "this concept is not stored in any SQL plug."
        plugkey | null (tblfields myplug) = fatal 747 "no tblfields in plug."
                | otherwise = head (tblfields myplug) --TODO -> implement KEYs in plug
        insme :: String -> PlugSQL -> String
        insme mevar plug --e.g. $oldme, $newme, ..
          = "DB_doquer(\"INSERT INTO `"++name plug
            ++ "` ("++intercalate "," ["`"++fldname f++"`" | f<-tblfields plug, not(fldauto f)]
            ++ ") VALUES (" ++
            intercalate ", "
                  [ "\".(" ++ "(null!=" ++ mevar ++ "['"++ fldname f ++ "'])"
                                ++ "?\"'\".addslashes("++ mevar ++ "['"++ fldname f ++ "']).\"'\":\"NULL\").\""
                  | f<-tblfields plug, not(fldauto f)
                  ] ++ ")\", 5);"
        inime :: String -> PlugSQL -> String
        inime mevar plug --e.g. $oldme, $newme, ..
          = mevar ++ " = array("++ intercalate "," ["'"++fldname f++"'=>null" | f<-tblfields plug, not(fldauto f)] ++ ");"
        thisme  -- -- $me holds the values on the screen (i.e. $this) stored in myplug ["private $_"++phpIdentifier (name a)++";" | a <- attributes o]
          = "$me = array("++ intercalate "," 
                             ["'"++fldname fld++"'=>"++arrayattpath att e | (fld,(att,e))<-thismematch]
                   ++ ");" 
        thismematch = nubBy (\x y -> fst x == fst y) 
                      --TODO -> identical expressions stored in plugs do not need to be saved twice, but in edit mode they are not synced on the screen (yet)
                   ((idfld,(this,objctx this)):
                    [(f,(a,e)) 
                    | f<-tblfields myplug, not(fldauto f)
                    , a <- attributes this
                    , e <- foldedattributes a
                    ,(plug,fld0,fld1)<-sqlRelPlugs fSpec e
                    ,plug==myplug,fld0==idfld, fld1==f]) --REMARK -> only the objctx a from idfld to f (at least UNI) not the flipped one (at least INJ)
        arrayattpath att e --e is the foldedattribute in att for which we want the php array path
           | att==this = "$this->getId()"
           --if att has no attributes then $this->_att is a value (not an array)
           | null(attributes att) = "$this->_"++phpIdentifier (name att)
           --if att has attributes then $this->_att is an array with at least 'id' for the target of objctx att
           | objctx att==e  = "$this->_"++phpIdentifier (name att)++"['id']"
           | otherwise  = "$this->_"++phpIdentifier (name att)++"['"++(name (head (attributes att)))++"']"
        notreqid_and_notinthis_flds = [fld |fld<-tblfields myplug, not(requires myplug (fld,idfld)), not(elem fld (map fst thismematch))]
        reqbyid_and_notinthis_flds = [fld |fld<-requiredFields myplug idfld, not(elem fld (map fst thismematch))]
        reqbyid_and_notreqid_flds = [fld |fld<-requiredFields myplug idfld, not(requires myplug (fld,idfld))]
        notinthis_flds =  [fld |fld<-tblfields myplug, not(elem fld (map fst thismematch))]
        onscreen =  "$onscreen = array("++ intercalate "," ["'"++fldname f++"'=>1" |f<-tblfields myplug, elem f (map fst thismematch)] ++ ");"
        shouldcut = not(null notreqid_and_notinthis_flds)
        cancut = null reqbyid_and_notinthis_flds
        --editable
        editable | theme flags==StudentTheme =  [r |("Student",r)<-fRoleRels fSpec]
                 | otherwise = map makeRelation (declarations fSpec) ++map I (concs fSpec)
        mayedit :: Expression -> Bool
        mayedit item = let rexprs=[ERel r |r<-editable] in elem item (rexprs++map flp rexprs)
        ---------myatts(editable only)
        myattsinthis = [((plug,fld0,fld1),a)
                   |a<-attributes this, not(elem (a,objctx a) (map snd thismematch))
                   ,(plug@(TblSQL{}),fld0,fld1)<-sqlRelPlugs fSpec (objctx a) --fld0 => src objat (find id), fld1 => trg objat (kernelfld)
                   ,mayedit (objctx a)]
        ---------myassociations(editable only)
        myassocinthis = [((plug,fld0,fld1),a)
                   |a<-attributes this, not(elem (a,objctx a) (map snd thismematch))
                   ,(plug@(BinSQL{}),fld0,fld1)<-sqlRelPlugs fSpec (objctx a) --fld0 => src objat (find id), fld1 => trg objat (assocfld)
                   ,mayedit (objctx a)]
        ---------myattflds(editable only)
        myattflds = [(attfld,cfld,p)
                    |(attfld,_)<-thismematch
                    ,not(iskey myplug attfld)
                    ,mayedit (fldexpr attfld)
                    ,(p,cfld)<-inplugs (target (fldexpr attfld))]
        checkinstances mevar attflds
         = [["//check the existence of attributes"
            ,"if(isset("++var++") && count(DB_doquer(\"SELECT `"++fldname cfld++"` FROM `"++name p
                                                 ++"` WHERE `"++fldname cfld++"`='\".addslashes("++var++").\"'\")) != 1){"
            ,"   " ++ inime "$rec" p
            ,"   $rec['"++fldname cfld++"']="++var++";"
            ,"   "++ insme "$rec" p
            ,"   if(mysql_errno()!=0) {"
            ,"      $err = mysql_error();"
            ,"      rollbacktransaction();"
            ,"      $myerrors[] = print_r(array('observation'=>'you try to set an attribute to a new instance '."++var++".' of concept "++show(target(fldexpr cfld))++".','erroranalysis'=>'you can only refer to existing instances of "++show(target(fldexpr cfld))++" on this page, because it has required attributes that are not or cannot be specified on this page.','suggestion'=>'first create '."++var++".' on a page that can create instances of "++show(target(fldexpr cfld))++".','sqlerror'=>$err)).print_r("++mevar++");"
            ,"      return false;"
            ,"   }"
            ,"}"]
           |(attfld,cfld,p)<-attflds, let var=mevar++"['"++fldname attfld++"']"]
    in
    --save functions for attributes of this with attributes
 --   concat [savefunction flags fSpec (attfuncnm att) (depth+1) att | att<-attfuncs]
 --   ++ --save function for this
    ["function "++uniqfnm fnm depth++"{"]
    ++ indentBlock 3 (
    ["global $myerrors;"
    ,if depth==0 then "starttransaction(); //transaction runs untill closed by somebody" else ""
    ,""
    ,"//me is a record with a kernelfield for this->id "
    ,"//this class/interface can only create me records (and the identity of concepts in them => kernelfields)"
    ,"//this->id is an instance of some concept"
    ,"//$this contains possibly new values from the screen"
    ,"//one value has one certain relation r with this->id"
    ,"//at this moment, we assume that r is not an expression, but declared"
    ,"//r may be stored in me or in some other plug"
    ,"//not all relations in me have to be in this"
    ,"//me must be completed with oldme values if this->id is not new."
    ,"//if me is new, then all required fields for this->id in me must be present to be able to insert this->id"
    ,"//the me part of this cannot be inserted the binary way, because it is not stored in a binplug"
    ,"//the other relations in this will be inserted the binary way"
    ,"//this implies updates of morattfields in other tblplugs than me and del/ins in binplugs where  src or trg = this->id"
    ,"//updates of morattfields will not check whether it was already set in the old situation i.e. overwrite."
    ]
    ++
 --   map runfunc attfuncs
 --   ++ 
    [inime "$oldme" myplug
    ,thisme
    ,onscreen
    ,inime "$newme" myplug
    ]
    ++
    ["if (!$this->isNew()) {"]
    ++ indentBlock 3 (
    ["$oldme = firstRow(DB_doquer(\"SELECT * FROM `"++name myplug++"` WHERE `"++fldname idfld++"`='\".addslashes($this->getId()).\"'\"));"
    ,"DB_doquer(\"DELETE FROM `"++name myplug++"` WHERE `"++fldname idfld++"`='\".addslashes($this->getId()).\"'\",5);"
--                    //if there is a change in one of the keys (this->id of me must be the same, because it is used to select oldme)
--                    // and fields not requiring this->id and not in this exist (see Haskell)
--                    // then there should be a new record for this => cut this->id, everything requiring id (these maybe fields not in this)
--                    // thus,the cluster of this->id is cut
    ] ++ 
    (if shouldcut then
      ["$shouldcut = $oldme[\""++fldname plugkey++"\"]!=$me[\""++fldname plugkey++"\"];"
--                      // every kernelfield required by this->id, but not requiring this->id back must have changed (and be in this) to be able to cut it ()
--                      // those values stay in cutoldme and have a UNIQUE INDEX
--                      // paste (i.e. insert) may still fail, because of UNIQUE INDEX of some kernelfield of this => TODO, now rollback
--      ,"$allrequiredinthis = true; //hide by haskell, just $cancut=false;"
      , if cancut
        then "$cancut = true;"
        else "$cancut = false; //" ++ intercalate ", " [fldname fld |fld<-reqbyid_and_notinthis_flds]
      ,"$cutoldme = false;"
      ,"if ($shouldcut){"
      ,"   if ($cancut){"
      ,"      $cutoldme = array(" 
                 ++ intercalate ", " 
                 --the old record keeps the old plugkey + fields not in this + fields in this required by but not requiring the id of this 
                 [if elem fld notinthis_flds || fld==plugkey || elem fld reqbyid_and_notreqid_flds
                  then "\""++fldname fld++"\"=>$oldme[\""++fldname fld++"\"]"
                  else "\""++fldname fld++"\"=>null" | fld<-tblfields myplug, not(fldauto fld)]
                 ++");"
      ,"      foreach ($oldme as $fld => $oldval){"
      ,"         if (isset($onscreen[$fld]))"
      ,"            $newme[$fld] = $me[$fld]; //the value on the screen is at least in newme"
      ,"         elseif (!isset($cutoldme[$fld]))"
      ,"            $newme[$fld] = $oldval; //cut old values not in cutoldme"
      ,"      }"
      ,"   } else { //should cut but can't because kernelfields will not unique or not all requiredfields for this->id are in this"
      ,"      rollbacktransaction();"
      ,"      $myerrors[] = print_r(array('xxx'=>'xxx'));"
      ,"      return false;"
      ,"   }"
      ,"} else { "
      ]
    else []) ++
    ["//no cutting just copy me to newme completed with old values (requires allrequiredinthis)"
    ,"   foreach ($oldme as $fld => $oldval){"
    ,"      if (isset($onscreen[$fld]))"
    ,"         $newme[$fld] = $me[$fld]; //the value on the screen is at least in newme"
    ,"      else"
    ,"         $newme[$fld] = $oldval;"
    ,"   }"
    ] ++
    (if shouldcut then
      ["}"
      ,"if ($cutoldme){ //try INS cutoldme (failure would be strange)"
      ,"   "++insme "$cutoldme" myplug
      ,"   if(mysql_errno()!=0) {"
      ,"      $err = mysql_error();"
      ,"      rollbacktransaction();"
      ,"      $myerrors[] = print_r(array('yyy'=>'yyy','error'=>$err));"
      ,"      return false;"
      ,"   }"
      ]
      ++ concat (checkinstances "$cutoldme" myattflds)
      ++ ["}"] 
    else [])) --end indentBlock if(!new)                    
    ++
    ["} else {"--else if(new)
    ,"   foreach ($newme as $fld => $nullval){"
    ,"      if (isset($onscreen[$fld]))"
    ,"         $newme[$fld] = $me[$fld]; //the value on the screen is at least in newme"
    ,"   }"
    ,"}"] 
    ++
    --you have a newme with a newkey that may already exist
    --if so you may want to merge newme with the record with this key
    --we only merge if non-null kernelfields do not change (otherwise error)
    --(the old concept instance attached to the key should be assigned to some other key or deleted first)
    --otherwise we would delete concept instances (kernelfield holds I of some concept)
    --attRels in this overwrite the old record
    --if so than update this key with the values of this and keep the old ones not in this
    (if shouldcut then
      ["$selkey = DB_doquer(\"SELECT * FROM `"++name myplug++"` WHERE `"++fldname plugkey++"`='\".addslashes($newme['"++fldname plugkey++"']).\"'\");"
      ,"if (count($selkey)>0){"
      ,"   $oldkey = firstRow($selkey);"
      ,"   if ("++intercalate " && " 
                  ["($oldkey[\""++fldname fld++"\"]==$newme[\""++fldname fld++"\"]"
                  ++ " || !isset($oldkey[\""++fldname fld++"\"])"
                  ++ " || !isset($newme[\""++fldname fld++"\"]))"
                  |fld<-tblfields myplug,not(fldauto fld),iskey myplug fld]
                ++"){"
      ,"      DB_doquer(\"DELETE FROM `"++name myplug++"` WHERE `"++fldname plugkey++"`='\".addslashes($newme['"++fldname plugkey++"']).\"'\",5);"
      ,"      foreach ($newme as $fld => $newval){"
      ,"         if (isset($onscreen[$fld]))"
      ,"            $newme[$fld] = $newval; //the value on the screen is at least in newme"
      ,"         else"
      ,"            $newme[$fld] = $oldkey[$fld];"
      ,"      }"
      ,"   } else {"
      ,"      rollbacktransaction();"
      ]
      ++
      ["      if ($oldkey[\""++fldname fld++"\"]!=$newme[\""++fldname fld++"\"]"
                  ++ " && isset($oldkey[\""++fldname fld++"\"])"
                  ++ " && isset($newme[\""++fldname fld++"\"]))"
                  ++ " $myerrors[] = print_r(array('error'=>\""++ name(target(fldexpr plugkey)) ++" \".$newme['"++fldname plugkey++"'].\" already identifies "
                  ++ name(target(fldexpr fld)) ++ " \".$oldkey['"++fldname fld++"'].\" overwriting it with \".$newme['" ++fldname fld++"'].\""
                  ++ " would delete \".$oldkey['"++fldname fld++"'].\". Please assign it to a different "++ name(target(fldexpr plugkey)) 
                  ++ " or delete it first.\"));"
      |fld<-tblfields myplug,not(fldauto fld),iskey myplug fld]
      ++
      ["      return false;"
      ,"   }"
      ,"}"
      ]    
    else []) ++
    --             //try INS newme (failure could be some UNIQUE INDEX or a missing required field, but not the UNIQUE KEY)
    --            //ins($newme);
    [insme "$newme" myplug
    ,"if(mysql_errno()!=0) {"
    ,"   $err = mysql_error();"
    ,"   rollbacktransaction();"
    ,"   $myerrors[] = print_r(array('observation'=>'sql error while saving instance.'.$this->getId(),'erroranalysis'=>'you may have missing required fields','sqlerror'=>$err)).print_r($newme);"
    ,"   return false;"
    ,"}"
    --REMARK -> the rest can be done the binary way!
    --            //update attribute relations (morAtt) in other records (maybe my, maybe other tblplug) (if attribute already set, then there is a choice: overwrite or not)
    --            //    (thus I am a partial function from a kernelfield of another plug to this->Id)
    --            
    ,""]
    ++ concat (checkinstances "$newme" myattflds)
    ++
    concat --TODO -> select myatts, then try to insert each myatt with or without an association to me (which fails in case of without and myatt is total)
    [["//myatts"
     ,"//check if the atts exist"
     ,"foreach ($this->_"++phpIdentifier (name att)++" as $k => $val){"
     ,"   $att = DB_doquer(\"SELECT * FROM `"++name plug++"` WHERE `"++fldname fld1++"`='\".addslashes($val).\"' \");"
     ,"   if(count($att)==0){"
     ,"      $err = mysql_error();"
     ,"      rollbacktransaction();"
     ,"      $myerrors[] = print_r(array('observation'=>'sql error while saving instance.'.$this->getId(),'erroranalysis'=>'Instance '.$val.' of "++name att++" does not exist.','sqlerror'=>$err));"
     ,"      return false;"
     ,"   }"
     ,"}"
     ,"//first delete myatt relations (SET to NULL)"
     ,"$rows = DB_doquer(\"SELECT * FROM `"++name plug++"` \");"
     ,"DB_doquer(\"DELETE FROM `"++name plug++"` \");"
     ,"//then insert myatt relations as defined in this (key=$val is assumed to exist)"
     ,"foreach ($rows as $row){"
     ,"  $reinserted=False;" --every row must be reinserted
     ,"  foreach ($this->_"++phpIdentifier (name att)++" as $k => $val){"
     ,"     if($row['"++fldname fld1++"']==$val){" --reinsert with association to me
     ,"        "++insqry True True
     ,"        if(mysql_errno()!=0) {"
     ,"           $err = mysql_error();"
     ,"           rollbacktransaction();"
     ,"           $myerrors[] = print_r(array('observation'=>'sql error while saving instance.'.$this->getId(),'erroranalysis'=>'Unexpected behaviour (a bug). Notify the administrator','sqlerror'=>$err)).print_r($row);"
     ,"           return false;"
     ,"        }"
     ,"        $reinserted=True;"
     ,"     }"
     ,"   }"
     ,"   if (!$reinserted) {"
     ,"      if($row['"++fldname fld0++"']==$this->getId())"
     ,"         "++insqry False True
     ,"      else"
     ,"         "++insqry True False
     ,"      if(mysql_errno()!=0) {"
     ,"         $err = mysql_error();"
     ,"         rollbacktransaction();"
     ,"         $myerrors[] = print_r(array('observation'=>'sql error while saving instance.'.$this->getId(),'erroranalysis'=>'Each "++name att++" x requires aparent like '.$this->getId().'. Navigate to x to assign a different parent to it.','sqlerror'=>$err)).print_r($row);"
     ,"         return false;"
     ,"      }"
     ,"   }" --reinsert without association to me
     ,"}"]
    |((plug,fld0,fld1),att)<-myattsinthis
    , let insqry with mine ="DB_doquer(\"INSERT INTO `"++name plug
                 ++"` ("       ++ (intercalate ", " ["`"++fldname f++"`" |f<-tblfields plug])
                 ++") VALUES ("++ (intercalate ", " [if f/=fld0 || with then "'\".addslashes("++fval++").\"'" else "NULL"
                                                    |f<-tblfields plug
                                                    ,let fval=if f==fld0 && mine then "$this->getId()" else "$row['"++fldname f++"']"])
                 ++")\");"]
    --            //insert binrels of this instance in the BinPlug
    --            //, "binrel" => $this->_binrel
 
    --            //$ctxenv["transaction"]["check"][] = rule(); //add rules, don't know how yet (rule() is a function that needs to be checked before commit)
    ++
    concat --TODO -> check if the things this is associated with exist
    [["//myassociations"
     ,"//check if the associated exist"
     ,"foreach ($this->_"++phpIdentifier (name att)++" as $k => $val){"
     ,"   $att = DB_doquer(\"SELECT * FROM `"++name trgplug++"` WHERE `"++fldname trgfld++"`='\".addslashes($val).\"' \");"
     ,"   if(count($att)==0){"
     ,"      $err = mysql_error();"
     ,"      rollbacktransaction();"
     ,"      $myerrors[] = print_r(array('observation'=>'sql error while saving instance.'.$this->getId(),'erroranalysis'=>'Instance '.$val.' of "++name att++" does not exist.','sqlerror'=>$err));"
     ,"      return false;"
     ,"   }"
     ,"}"
     ,"//first delete myassociation with fld0=id"
     ,"DB_doquer(\"DELETE FROM `"++name plug++"` WHERE `"++fldname fld0++"`='\".addslashes($this->getId()).\"' \");"
     ,"//then insert myassociation for fld0=id as defined in this (key=$val is assumed to exist)"
     ,"foreach ($this->_"++phpIdentifier (name att)++" as $k => $val){"
     ,"   DB_doquer(\"INSERT IGNORE INTO  `"++name plug
                 ++"` ("++fldname fld0++","++fldname fld1
                 ++") VALUES ('\".addslashes($this->getId()).\"','\".addslashes($val).\"') \");"
     ,"}"]
    |((plug,fld0,fld1),att)<-myassocinthis
    ,let trgtbls = sqlRelPlugs fSpec (ERel(I(target(objctx att))))
    ,let (trgplug,trgfld,_) = if null trgtbls then error "no target tabel" else head trgtbls
    ]
    ++            
    [""
    ,if depth==0 then "if (closetransaction()) {return $this->getId();} else { return false;}" else ""
    ]) --end indentBlock save() for this
    ++
    ["}"] --end of save() for this
    where  uniqfnm nm dpth = if dpth==0 then "save()" else phpIdentifier(nm++show dpth)++"()"
