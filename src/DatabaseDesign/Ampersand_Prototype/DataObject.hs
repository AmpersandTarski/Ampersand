{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables#-}
module DatabaseDesign.Ampersand_Prototype.DataObject(dataInterfaces) where
--import Adl (
--           --,Concept(..),Declaration(..),isTrue
--           ObjectDef(..),Morphic(..)
--       --    ,Identified(..)
--           ,Object(..),Expression(..), Concept, FilePos(..), F_TracePos(..)
--           )
import DatabaseDesign.Ampersand_Prototype.RelBinGenSQL(sqlExprTrg,selectExpr)
import DatabaseDesign.Ampersand_Prototype.RelBinGenBasics(indentBlock,phpIdentifier)
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Misc.Plug
import Data.List  hiding (group)
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import DatabaseDesign.Ampersand_Prototype.Version 

--For each data object (i.e. generated kernel-based SQL plugs) there is one dataInterfaces file (data_*.inc.php).
--It contains:
-- + classes for all concepts in the data object incl. save and del functions
-- + getEach functions for the concepts in the plug
--The attributes of the class of some concept are those relations that require this concept (all UNI relations with source r==concept)
--Thus a class may contain other classes.
--The main purpose of a data class is to save any delta on a class instance initiated from some *.inc.php.
--The data in a INTERFACE is composed of parts from DATAOBJECTS (possibly entire objects).
--A INTERFACE puts all its (possibly changed) data in partially filled class instances of data objects and calls save() or del() on them as appropriate.
--Everything not filled cannot have changed by the INTERFACE.
--The delta may be small (e.g. only change in one of the attributes in a tiny class contained in this class)
--Each class has its own save (and del) function, which
-- -> is used by classes in which it is contained (one implementation).
--    -> only if there is a delta on this (sub)class (know the delta on your class so you can take the appropriate actions)
--
--note: scalars are unions of source or target of certain relations
--       -> insert/delete actions may be required after delta on the field of one of these relations 
--            => scalars require recalculate function or even better, no tabel, but some SELECT DISTINCT query = (I/\m1;m1~\/m2;m2~\/..)
--      A composition of binaries or just one binary is an association (not uni nor inj) between two kernel fields (maybe scalar)
--       -> the nature of a change on a binary plug is hard to discover.
--          any change breaks the key, is it a new key (keep old,INSERT new) or an updated key (DELETE old, INSERT new)
--          binary relations must be maintained in a special INTERFACE
--          for convenience, a garbage collector can automatically clean up binary tables (delete non-existing source or target instances)
--

dataInterfaces :: Options 
                   -> Fspc
                   -> DataObject --(DataObject PlugSQL)
                   -> String
dataInterfaces _ _ (DataObject (ScalarSQL{})) = "//TODO -> Scalar" --error "TODO DataObject.hs 97"
dataInterfaces _ _ (DataObject (BinSQL{})) = "//TODO -> Binary" --error "TODO DataObject.hs 97"
dataInterfaces flags fSpec (DataObject dobj)
 = intercalate "\n  "
   ([ "<?php // generated with "++ampersandPrototypeVersionBanner
    , ""
    , "/*********"
    , showADLcode fSpec dobjdef
    , " *********/"
    , ""
    , "error_reporting(E_ALL); "
    , "ini_set(\"display_errors\", 1);"
    , "require \"connectToDataBase.inc.php\";"
    , ""
    ]
    ++
    [ case phpfile p of Just f -> "  require \""++f++"\";\n"; _ -> ""
    | (PlugPhp p)<-plugs fSpec ]
    ++
    generateInterface_getEach fSpec (DataObject dobj) 
    ++
    concat[conceptclass flags fSpec (DataObject dobj) x |x<-cLkpTbl dobj]
   )
   ++ "\n?>"
   where
   dobjdef = Obj (name dobj) Nowhere (ctx dobj) Nothing (attributes (DataObject dobj)) []

generateInterface_getEach :: Fspc -> DataObject -> [String]
generateInterface_getEach fSpec (DataObject dobj)
 = concat
   [ funcs c (sql expr)
   | (c,expr)<-objcLkp dobj]
  where
  sql expr = selectExpr fSpec 31 (sqlExprTrg fSpec expr) "" (flp expr)
  --TODO -> move to class Object
  objcLkp::PlugSQL->[(Concept,Expression)]
  objcLkp plug = case plug of
    ScalarSQL{} -> [(cLkp plug,fldexpr (column plug))]
    _           -> [(c,fldexpr fld) |(c,fld)<-cLkpTbl plug]
  funcs _ Nothing = error "Cannot generate getEach code in DataObject.hs (line 49)"
  funcs c (Just x)
    = ["function getEach"++phpIdentifier (name c)++"(){"
      ,"  return firstCol(DB_doquer('" ++ x ++"'));"
      ,"}\n"]

phpVar :: String -> String
phpVar x = "$_"++phpIdentifier x

--TODO -> a class for every key in dataobject (kernel concept)
--the cfld is a kernel field => "SELECT flds WHERE cfld=$id" is a perfect query
--
--Als ik iets wil doen met een Datatype, dan moet ik er rekening mee houden dat zowel value1::Object*Datatype als value2::Att*Datatype [uni inj sur]
--    function Datatype($id=null, $_value1=null, $_value2=null, $_attr1=null, $_attr2=null){
--attr1 en attr2 kan ik vrij wijzigen in class Obj
--    function Obj($id=null, $_attr1=null, $_attr2=null){
--In *.inc.php kan ik gebruik maken van classes in data_*.inc.php
--Als ik een datatype update, dan kan het zo zijn dat ik alleen een attr1 heb gewijzigd.
--  $x = new Datatype(<waardes>); $x.save();
--  function save(){ $old=new readDatatype($id); vgl($old,$this) and UPDATE(new Obj with old datatype where  Obj=NULL)
--                                                                   UPDATE(changed Obj with unchanged datatype)
--                                                                   INSERT()
--                                                                   CUTUPDATE(changed datatype)}
--In de save van *.inc.php moet ik een array() van data*.inc.php class instances vullen voor zover bekend en ieder instance saven.
--Dus de READ van interface objecten gaat niet via data*.inc.php!
--Wel zullen getEach functies in (een aparte?) data*.inc.php komen
--
conceptclass :: Options -> Fspc -> DataObject -> (Concept,SqlField) -> [String]
conceptclass flags fSpec (DataObject dobj) (c,cfld)
-- | isOne dobjdef = error "not expecting $id=ONE in DataObject.hs (line 97)"
-- | not(attributes dobjdef==(attributes (DataObject dobj))&&ctx dobjdef==ctx (DataObject dobj))
-- = error$show( "dobjdef should be the Object definition of dobj in DataObject.hs (line 98)",ctx dobjdef,(ctx (DataObject dobj)))
-- | otherwise
 = let 
   --alle velden die van cfld afhankelijk zijn
   requiresFld = [f |f<-tblfields dobj, requires dobj (f,cfld)]
   flds = [fld |fld<-requiresFld, fld/=cfld] --class fields
   commaflds = if null flds then [] else ", "
   myName = name c
   in
   [ "class "++myName ++" {"]
   ++
   indentBlock 2 
   (["protected $id=false;"
    ,"protected $_new=false; //true if $id is not known in the current database (also false if !isset($id))"
    ,"protected $_old=false; //true if $this holds the current values in the database of instance $id, otherwise $this holds potentially changed values"
    ]
    ++
    ["private "++phpVar (fldname fld)++";"
    | fld<-flds]
    ++
    ["function "++myName++"($id=null" ++ commaflds
                ++ (intercalate ", " [phpVar (fldname fld)++"=null" | fld<-flds])
                ++"){"
    ]
   )
   ++
   indentBlock 4
   (
    ["$this->id=$id;"]
    ++
    ["$this->_"++phpIdentifier (fldname fld)++"="++phpVar (fldname fld)++";"
    | fld<-flds]
    ++ 
    [ "if(isset($id)){"
    , "  $ctx = DB_doquer(\"SELECT `" ++ name dobj ++ "`.`" ++fldname cfld ++"` AS `id`" ++ commaflds
                        ++ intercalate ", " ["`"++ name dobj ++"`.`" ++ fldname fld ++"`" | fld<-flds]
                        ++ " FROM `"++name dobj++"`"
                        ++ " WHERE `"++ name dobj ++ "`.`id`='\".addslashes($id).\"'\");"
    , "  if(count($ctx)==0) $this->_new=true;"
    , "  else "
         ++if null flds then "{"
           else "if("++ intercalate " && " ["!isset($_"++phpIdentifier (fldname fld)++")" | fld<-flds] ++"){"
    , "    // only fill the attributes when only the $id isset, otherwise $this contains potentially changed values"
    , "    $this->_old=true;"
    , "    $me=firstRow($ctx);"
    ]
    ++
    [ "    $this->set_"++phpIdentifier (fldname fld)++"($me['"++fldname fld++"']);"
    | fld<-flds]
    ++
    [ "  }" 
    , "}"
    ]
   )
   ++
   indentBlock 2
   (
    ["}\n"]
    ++
    saveTransactions flags fSpec (DataObject dobj) (c,cfld)
    ++
    (concat
      [ ["function set_"++phpIdentifier (fldname fld)++"($val){"
        ,"  $this->_"++phpIdentifier (fldname fld)++"=$val;"
        ,"}"
        ,"function get_"++phpIdentifier (fldname fld)++"(){"
        ]
        ++
        ( if isUni (fldexpr fld)
          then []
          else ["  if(!isset($this->_"++phpIdentifier (fldname fld)++")) return array();"]
        )
        ++
        ["  return $this->_"++phpIdentifier (fldname fld)++";"
        ,"}"
        ]
      | fld <- flds]
    )
    ++
    ["function setId($id){"
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
   )
   ++
   [ "}\n" ]
  
--TODO -> save and del function of class
--
--save or del is called on some $this with values that may or may not have changed.
--thus, $this contains values from the screen, which may be equal to $this->id in the current database a.k.a. $old.
--to be sure that $this contains values from the screen and $old is read from the database, you can check protected $_old
-- $this has to be compared with $old=new ClassOfThis($this->getId())
--note: INTERFACE initiates and closes TRANSACTION
--
--saving/deleting this where  !isset($id):
-- find the largest class instance id that isset (the class that requires the least number of fields) and call save/delete on that class
--
--saving this where  isset($id::A) and $_new:
--  INSERT $this => complain if there are missing required fields
--                  check existence of all morAtts in $this (see NOTE on changing a morAtt)
--
--saving this where  isset($id::A) and !$_new:
-- -> check changes in morAtt attfld attached to $id or a kernel sibling sibfld (uni,tot,inj,sur) 
--    (fldexpr attfld==m::A*B) (relatie sibfld=r::A*C)
--    UPDATE? => complain if there are missing required fields
--               check existence of all morAtts in $this (see NOTE on changing a morAtt)
--    =>if isset($this->getsibfld())
--      then if $this->getsibfld()!=$old->getsibfld()) 
--           then UPDATE
--           else nochange     
--      if isset($this->getattfld())
--      then if $this->getattfld()!=$old->getattfld()) 
--           then UPDATE
--           else nochange
--      else if isTot(m) 
--           then nochange
--           else if !isset($old->getattfld())
--                then nochange
--                else TODO15122010 -> ??? is the univalent attribute set to null or does the INTERFACE not include this attribute ??? 
--                                     assume nochange if only $id isset, UPDATE otherwise
-- -> check changes in kernel fields in kernelcluster of kfld of $id (fldexpr kfld=m::A*B uni,tot,sur)
--    if all kfld' unchanged then save(kfld) 
--    else check if kfld' already exists => cut/paste it
--
-- check changes in kernelFld kfld in kernelcluster of $id (expr::A*B [uni,tot,inj,sur])
-- check changes in kernelFld kfld attached to $id (fldexpr kfld==m::A*B)
--                                                                   UPDATE(new Obj with old datatype where  Obj=NULL)
--                                                                   UPDATE(changed Obj with unchanged datatype)
--                                                                   INSERT()
--                                                                   CUTUPDATE(changed datatype)}
--deleting this where  isset($id::A): 
--  if $_new then DO NOTHING
--  else if not(fldnull $id) then delete $this (DELETE WHERE $id)
--       else set $this to NULL (UPDATE $this=array of null WHERE $old->id)
--
--NOTE: changing a morAtt with m::A*B is picking a new $x::B which may or may not exist in class B.
--  If $x exists in class B or scalar then OK
--  If $x does not exist in class B then 
--     if B is scalar then OK => recalculate scalar
--     else (class B exists) (new B($x)).save() => save will complain if there are required fields i.e. $x has to be created first
--          note: if INTERFACE contains requiredfields for $x then the INTERFACE should save $x before $this
saveTransactions :: Options -> Fspc -> DataObject -> (Concept,SqlField) -> [String]
saveTransactions _ _ _ _ = []
--saveTransactions flags fSpec (DataObject dobj) (c,cfld) = []



