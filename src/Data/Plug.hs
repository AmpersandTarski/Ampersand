{-# OPTIONS_GHC -Wall -XMultiParamTypeClasses #-}  
module Data.Plug (Plugable(..), PlugInfo(..), PlugInfos
                 ,SqlField(..)
                 ,SqlType(..)
                 ,showSQL
                 ,requiredFields,requires,plugpath,eLkpTbl
                 ,tblfields
                 ,tblcontents
                 ,entityfield,entityconcept
                 ,fldauto
                 ,iskey,kernelrels,attrels,bijectivefields
                 ,PlugSQL(..)
                 ,DataObject(..))
where
import Ampersand ( Concept(..), Signaling(..), cptnew
           , Relation(..), Association(..), Relational(..), mIs, Identified(..)
           , Expression(..)
           , ObjectDef(..)
           , FilePos(..)
           , Paire
           , isSur,isTot,isInj,isUni)
import Collection((>-))
import Classes.Object (Object(..))
import Classes.Populated (contents')
import Classes.ConceptStructure (ConceptStructure(..))
import FPA (FPA(..),FPAble(..))
import Auxiliaries (sort',eqClass)
import List(elemIndex,nub)
 
--a data object is always generated.
--There is one data object for each generated sql plug.
--We can rely on a well-formed set of data objects:
-- 1) no duplication of relation elements (population)
--    -> data services harvest (read) and distribute (edit) data from and to [Plug]
-- 2) Rules within the scope of a data object can and will be maintained in the data service.
--    -> (Composite) services get their data (unpredicatable which data) from one or more data objects.
--       From a service object it is complex to determine whether you may commit (e.g. are all requiredFields for this edit action available)
--       A data object has implemented the rules that determine just that.
--       Separate data and composite services is a solution.
--       Composite services maintain rules that are not within the scope of any data object.
data DataObject = DataObject PlugSQL deriving (Show,Eq)

--in imaginary ASCII: the reverse of makeSqlPlug => DATAOBJECT dataobject: ctx = attributes
instance Object DataObject where
 --one kernel field is chosen to represent the entityconcept
 concept (DataObject p)
  = concept p  
 --(note: concept p does not only represent the entityconcept, it is also itself i.e. an attribute of the entityconcept)
 attributes (DataObject p@(ScalarSQL{}))
  = [(fld2objdef (column p) [])]
 attributes (DataObject p@(BinSQL{}))
  = (fld2objdef (fst(columns p)) []):dobj2objats ("bin"++name p) (fst(columns p)) []
 attributes (DataObject p@(TblSQL{}))
  = if length mbfld==1 
    then (fld2objdef (head mbfld) []):(dobj2objats ("tbl"++name p) (head mbfld) (mLkpTbl p))
    else error "!fatal (module Data.Plug 63): cannot find field of dataobject"
    where mbfld = [fld|(c,fld)<-cLkpTbl p, c==concept p]
 ctx (DataObject p@(ScalarSQL{}))
  = fldexpr (column p)
 ctx (DataObject p@(BinSQL{}))
  = fldexpr (fst(columns p))
 ctx (DataObject p@(TblSQL{}))
  = if length mbexpr==1 
    then head mbexpr 
    else error "!fatal (module Data.Plug 72): cannot find ctx of dataobject"
    where mbexpr = [fldexpr fld|(c,fld)<-cLkpTbl p, c==concept p] 
 --TODO -> (see tblcontents)
 populations (DataObject p) 
  = error ("!TODO (module Data.Plug 76): evaluate population of plug "++name p++".")

dobj2objats::String->SqlField->[(Relation Concept,SqlField,SqlField)]->[ObjectDef]
dobj2objats _ fld mlkp = objats(fld2objdef fld (fld2objats fld mlkp))
fld2objdef::SqlField->[ObjectDef]->ObjectDef
fld2objdef fld ats = Obj (fldname fld) Nowhere (fldexpr fld) Nothing ats []
fld2objats::SqlField->[(Relation Concept,SqlField,SqlField)]->[ObjectDef]
fld2objats fld mlkp = [fld2objdef t (fld2objats t mlkp)|(_,s,t)<-mlkp,fld==s,s/=t]


----------------------------------------------
--Plug
----------------------------------------------
--TODO151210 -> define what a plug is and what it should do
--Plugs are of the class Object just like Services(??? => PHP plug isn't an instance of Object)
--An Object is an entity to do things with like reading, updating, creating,deleting.
--A Service is an Object using only Plugs for reading and writing data; a Plug is a data service maintaining the rules for one object:
-- + GEN Service,Plug ISA Object
-- + cando::Operation*Object
-- + uses::Service*Plug [TOT].
-- + maintains::Plug*Rule.
-- + signals::Service*SignalRule.
--
--Plugs can currently be implemented in PHP or SQL.
--type Plugs = [Plug]
--data Plug = PlugSql PlugSQL | PlugPhp PlugPHP deriving (Show,Eq)
data PlugInfo = InternalPlug PlugSQL 
              | ExternalPlug ObjectDef
                deriving (Show, Eq)
instance Identified PlugInfo where
  name (InternalPlug psql) = name psql
  name (ExternalPlug obj)  = name obj

type PlugInfos = [PlugInfo]   

class (FPAble p, Identified p, Eq p, Show p) => Plugable p where
  makePlug :: PlugInfo -> p
  
instance Plugable PlugSQL where
  
instance FPAble PlugSQL where
  fpa p = sqlfpa p

instance Identified PlugSQL where
  name p = sqlname p
  rename p x = p{sqlname=x}

--DESCR -> plugs are sorted to optimize some algoritms. 
instance Eq PlugSQL where
  x==y = name x==name y


----------------------------------------------
--PlugSQL
----------------------------------------------
--TblSQL, BinSQL, and ScalarSQL hold different entities.
--BinSQL -> (see the only constructor function mor2plug in ADL2Plug for detailed comments)
--          stores one morphism m in two ordered columns
--          i.e. a tuple of SqlField -> (source m,target m) with (fldexpr=I/\m;m~, fldexpr=m) 
--            (note: if m TOT then (I/\m;m~ = I). Thus, the concept (source m) is stored in this plug too)
--          with tblcontents = [[x,y]|(x,y)<-contents' m]. 
--          Typical for BinSQL is that it has exactly two columns that are not unique and may not contain NULL values
--ScalarSQL -> stores one concept c in one column
--             i.e. a SqlField -> c
--             with tblcontents = [[x]|(x,_)<-contents' c].
--             Typical for ScalarSQL is that it has exactly one column that is unique and may not contain NULL values i.e. fldexpr=I[c]
--TblSQL -> stores a related collection of relations: a kernel of concepts and attribute relations of this kernel
--           i.e. a list of SqlField given some A -> [target m | m::A*B,isUni m,isTot m, isInj m] 
--                                                ++ [target m | m::A*B,isUni m, not(isTot m), not(isSur m)]
--             kernel = A closure of concepts A,B for which there exists a m::A->B[INJ] 
--                      (m=fldexpr of kernel field holding instances of B, in practice m is I or a makeRelation(flipped declaration))
--             attribute relations = All concepts B, A in kernel for which there exists a m::A*B[UNI] and m not TOT and SUR
--                      (m=fldexpr of attMor field, in practice m is a makeRelation(declaration))
--           all kernel fields can be related to an imaginary concept ID for the plug (a SqlField with type=SQLID)
--             i.e. For all kernel fields k1,k2, where concept k1=A, concept k2=B, fldexpr k1=r~, fldexpr k2=s~
--                  You can imagine :
--                    - a morphism value::ID->A[INJ] or value::ID->A[INJ,SUR]
--                    - a morphism value::ID->B[INJ] or value::ID->B[INJ,SUR]
--                    such that s~=value~;value;r~ and r~=value~;value;s~
--                    because value is at least uni,tot,inj, all NULL in k0 imply NULL in k1 xor v.v.
--                    if value also sur then all NULL in k0 imply NULL in k1 and v.v.
--           Without such an ID, the surjective or total property between any two kernel fields is required.
--           Because you can imagine an ID concept the surjective or total property between two kernel field has become a design choice.
--
--           With or without ID we choose to keep kernel = A closure of concepts A,B for which there exists a m::A->B[INJ] instead of m::A*B[UNI,INJ]
--           By making this choice:
--             - nice database table size
--             - we do not need the imaginary concept ID  (and relation value::ID->A[INJ] or value::ID->A[INJ,SUR]), because:
--                  with ID    -> there will always be one or more kernel field k1 such that (value;(fldexpr k1)~)[UNI,INJ,TOT,SUR].
--                                any of those k1 can serve as ID of the plug (a.k.a. concept p / source p)
--                  without ID -> any of those k1 can still serve as ID of the plug (a.k.a. concept p / source p)
--               In other words, the imaginary concept is never needed 
--                               because there always is an existing one with the correct properties by definition of kernel.
--               Implementation without optional ID:
--                        -> fldexpr of some kernel field k1 will be r~
--                           k1 holds the target of r~
--                           the source of r~ is a kernel concept too
--                           r~ may be I
--                        -> fldexpr of some attMor field a1 will be s
--                           a1 holds the target of s
--                           the source of s is a kernel concept
--                        -> sqlRelFields m = (m,k1,a1) (or (m,k1,k2)) in mLkpTbl
--                           is used to generate SQL code and PHP-objects without needing the ID field.
--                           The ID field can be ignored and does not have to be generated because m=(fldexpr k1)~;(fldexpr a1)
--                           You could generate the ID-field with autonum if you want, because it will not be used
--                        -> TODO151210 -> sqlRelFields e where e is not in mLkpTbl
--                           option1) Generate the ID field (see entityfield)
--                                    sqlRelFields e = (e, idfld;k1, idfld;a1) where e=(fldexpr k1)~;value~;value;(fldexpr a1)
--                                    remark: binary tables can be binary tables without kernels, but with ID field
--                                            (or from a different perspective: ID is the only kernel field)
--                                            sqlRelFields m = (m,idfld/\m;m~,idfld;m1) where m = (idfld/\m;m~)~;idfld;(fldexpr m1)
--                                            (sqlRelFields m~  to get the target of m)
--                                            (scalar tables can of course also have an ID field)
--                           option2) sqlRelFields e = (e, k1;k2;..kn, a1) 
--                                    where e=(fldexpr kn)~;..;(fldexpr k2)~;(fldexpr k1)~;(fldexpr k1)(fldexpr k2);..;(fldexpr kn);(fldexpr a1)
--                           If I am right the function isTrue tries to support sqlRelFields e by ignoring the type error in kn;a1.
--                           That is wrong! 

--the entityfield is not implemented as part of the data type PlugSQL
--It is a constant which may or may not be used (you may always imagine it)
--TODO151210 -> generate the entityfield if options = --autoid -p
--REMARK151210 -> one would expect I[entityconcept p], 
--                but any p (as instance of Object) has one always existing concept p suitable to replace entityconcept p.
--                concept p and entityconcept p are related uni,tot,inj,sur.
entityfield :: PlugSQL -> SqlField
entityfield p
  = Fld (name (entityconcept p)) --name of imaginary entity concept stored in plug
        (Tm (mIs (concept p)) (-1)) --fldexpr
        SQLId --fldtype
        False --isnull
        True --isuniq
--the entity stored in a plug is an imaginary concept, that is uni,tot,inj,sur with (concept p)
--REMARK: there is a (concept p) because all kernel fields are related SUR with (concept p)
entityconcept :: PlugSQL -> Concept
entityconcept p@(BinSQL{}) --create the entityconcept of the plug, and an instance of ID for each instance of mLkp
  = (cptnew "ID"){cptos=Just [show idnr | (idnr,_)<-zip [(1::Int)..] (contents' (mLkp p))]}  
entityconcept p --copy (concept p) to create the entityconcept of the plug, using instances of (concept p) as instances of ID
  = (concept p){cptnm=name(concept p)++ "ID"} 


data PlugSQL
 = TblSQL  { sqlname   :: String
           , fields    :: [SqlField]
           , cLkpTbl   :: [(Concept,SqlField)]           -- lookup table that links all kernel concepts to fields in the plug
           , mLkpTbl   :: [(Relation Concept,SqlField,SqlField)] -- lookup table that links concepts to column names in the plug (kernel+attMors)
           , sqlfpa    :: FPA -- ^ functie punten analyse
           }
 | BinSQL  { --see mor2plug in ADL2Fspec.hs
             sqlname   :: String
           , columns   :: (SqlField,SqlField)
           , cLkpTbl   :: [(Concept,SqlField)] --given that mLkp cannot be (UNI or INJ) (because then m would be in a TblSQL plug)
                                                --if mLkp is TOT, then the concept (source mLkp) is stored in this plug
                                                --if mLkp is SUR, then the concept (target mLkp) is stored in this plug
           , mLkp      :: Relation Concept -- the morphism links concepts implemented by this plug
           , sqlfpa    :: FPA -- ^ functie punten analyse
           }
 | ScalarSQL
           { sqlname   :: String
           , column    :: SqlField
           , cLkp      :: Concept -- the concept implemented by this plug
           , sqlfpa    :: FPA -- ^ functie punten analyse
           }
   deriving (Show) 



--Maintain rule: Object ObjectDef = Object (makeSqlPlug :: ObjectDef -> PlugSQL)
--TODO151210 -> Build a check which checks this rule for userdefined/showADL generated plugs(::[ObjectDef]) 
--TODO151210 -> The ObjectDef of a BinSQL plug for morphism m is that:
--           1) SQLPLUG mybinplug: m      , or
--           2) SQLPLUG labelforsourcem : I /\ m;m~ --(or just I if m is TOT)
--               = [labelfortargetm : m]
--           The first option has been implemented in instance ObjectPlugSQL i.e. attributes=[], ctx=Tm m (-1)
instance Object PlugSQL where
 concept p = case p of
   TblSQL{mLkpTbl = []} -> error ("!Fatal (module Data.Plug 277): empty lookup table for plug "++name p++".")
   TblSQL{}             -> --TODO151210-> deze functieimplementatie zou beter moeten matchen met onderstaande beschrijving
                            --        nu wordt aangenomen dat de source van het 1e rel in mLkpTbl de source van de plug is.
                            --a relation between kernel concepts r::A*B is at least [UNI,INJ]
                            --to be able to point out one concept to be the source we are looking for one without NULLs in its field
                            -- i.e. there is a concept A such that
                            --      for all kernel field expr (s~)::B*C[UNI,INJ]:
                            --      s~ is total and there exists an expr::A*B[UNI,INJ,TOT,SUR] (possibly A=B => I[A][UNI,INJ,TOT,SUR]) 
                            --If A is such a concept,
                            --   and A is not B,
                            --   and there exist an expr::A*B[UNI,INJ,TOT,SUR]
                            --then (concept PlugSQL{}) may be A or B
                            --REMARK -> (source p) used to be implemented as (source . fldexpr . head . fields) p. That is different!
                            head [source m|(m,_,_)<-mLkpTbl p]
   BinSQL{} -> source (mLkp p) --REMARK151210 -> the concept is actually ID such that I[ID]=I[source m]/\m;m~
   ScalarSQL{} -> cLkp p
-- Usually source a==concept p. Otherwise, the attribute computation is somewhat more complicated. See ADL2Fspec for explanation about kernels.
 attributes p@(TblSQL{})
  = [ Obj (fldname tFld)                                                   -- objnm 
          Nowhere                                                          -- objpos
          (if source a==concept p then Tm a (-1) else f (source a) [[a]])  -- objctx
          Nothing                                                          -- objctx_proof
          [] []                                                            -- objats and objstrs
    | (a,_,tFld)<-mLkpTbl p]
    where
     f c mms = if null stop                                     -- a path from c to a is not found (yet)
               then f c mms'                                    -- so add another step to the recursion
               else if null (sort' length stop) 
                    then error "!Fatal (module Data.Plug 305): null (sort' length stop)." 
                    else F [Tm m (-1)| m<-head (sort' length stop)]  -- pick the shortest path and turn it into an expression.
               where
                 mms' = if elem [] mms 
                        then error "!Fatal (module Data.Plug 309): null in mms."
                        else [a:ms | ms<-mms, (a,_,_)<-mLkpTbl p, target a==source (head ms)]
                 stop = if elem [] mms'
                        then error "!Fatal (module Data.Plug 310): null in mms'."
                        else [ms | ms<-mms', source (head ms)==c]  -- contains all found paths from c to a 
 attributes _ = [] --no attributes for BinSQL and ScalarSQL
 ctx p@(BinSQL{}) = Tm (mLkp p) (-1)
 ctx p = Tm (mIs (concept p)) (-1)
 populations p = error ("!TODO (module Data.Plug 317): evaluate population of plug "++name p++".") --TODO -> (see tblcontents)

{-WHY151210 -> why do I need PlugSQL to be an Association
--       in other words why do I need a (target p) for BinSQL and ScalarSQL only
--       (remark: source p=concept p and target PlugSQL{}=error)
instance Association PlugSQL Concept where
   source p               = concept p
   target p@(BinSQL{})    = target (mLkp p)
   target p@(ScalarSQL{}) = cLkp p
   target p               = error ("!Fatal (module Data.Plug 326): cannot compute the target of plug "++name p++", because it is not binary.")
-}

--WHY151210 -> why can only binary plugs be signals?
instance Signaling PlugSQL where
 isSignal p@(BinSQL{}) = isSignal (mLkp p)
 isSignal _            = False

data SqlField = Fld { fldname     :: String
                    , fldexpr     :: Expression (Relation Concept)
                    , fldtype     :: SqlType
                    , fldnull     :: Bool -- can there be empty field-values?
                    , flduniq     :: Bool -- are all field-values unique?
                    } deriving (Eq, Show)
fldauto::SqlField->Bool -- is the field auto increment?
fldauto f = (fldtype f==SQLId) && not (fldnull f) && flduniq f -- && isIdent (fldexpr f)

data SqlType = SQLChar    Int
             | SQLBlob              -- cannot compare, but can show (as a file)
             | SQLPass              -- password, encrypted: cannot show, but can compare
             | SQLSingle  
             | SQLDouble  
             | SQLText              -- cannot compare, but can show (as a text)
             | SQLuInt    Int
             | SQLsInt    Int
             | SQLId                -- autoincrement integer
             | SQLVarchar Int
             | SQLBool              -- exists y/n
             deriving (Eq,Show)

showSQL :: SqlType -> String
showSQL (SQLChar    n) = "CHAR("++show n++")"
showSQL (SQLBlob     ) = "BLOB"
showSQL (SQLPass     ) = "VARCHAR(255)"
showSQL (SQLSingle   ) = "FLOAT" -- todo
showSQL (SQLDouble   ) = "FLOAT"
showSQL (SQLText     ) = "TEXT"
showSQL (SQLuInt    n) = "INT("++show n++") UNSIGNED"
showSQL (SQLsInt    n) = "INT("++show n++")"
showSQL (SQLId       ) = "INT"
showSQL (SQLVarchar n) = "VARCHAR("++show n++")"
showSQL (SQLBool     ) = "BOOLEAN"
          
--every kernel field is a key, kernel fields are in cLkpTbl or the column of ScalarSQL
--iskey refers to UNIQUE INDEX and not UNIQUE KEY!!!
--iskey may contain NULL, but their index (the entityfield of the plug) must be unique for a kernel field (iskey=True)
--the field that is isIdent and iskey (i.e. concept plug), or any similar (uni,inj,sur,tot) field is also UNIQUE KEY
--KeyDefs define UNIQUE KEY (fld1,fld2,..,fldn)
--TODO151210->iskey is a bad name, 'key' is misused in more cases => CLEAN UP!
--REMARK -> a kernel field does not have to be in cLkpTbl, in that cast there is another kernel field that is 
--          thus I must check whether fldexpr isUni && isInj && isSur
iskey :: PlugSQL->SqlField->Bool
iskey plug@(ScalarSQL{}) f = column plug==f
iskey plug@(BinSQL{}) _ --mLkp is not uni or inj by definition of BinSQL, if mLkp total then the (fldexpr srcfld)=I/\m;m~=I i.e. a key for this plug
  | isUni(mLkp plug) || isInj(mLkp plug) = error "!Fatal (module Data.Plug 380): BinSQL may not store a univalent or injective rel, use TblSQL instead."
  | otherwise              = False --binary does not have key, but I could do a SELECT DISTINCT iff f==fst(columns plug) && (isTot(mLkp plug)) 
iskey plug@(TblSQL{}) f    = elem f (fields plug) && isUni(fldexpr f) && isInj(fldexpr f) && isSur(fldexpr f)

--mLkpTbl stores the relation of some target field with one source field
--an iskey target field is a kernel field related to some similar or larger kernel field
--any other target field is an attribute field related to its kernel field
kernelrels::PlugSQL ->[(SqlField,SqlField)]
kernelrels plug@(ScalarSQL{}) = [(column plug,column plug)]
kernelrels (BinSQL{})         = error "!Fatal (module Data.Plug 389): Binary plugs do not know the concept of kernel fields."
kernelrels plug@(TblSQL{})    = [(sfld,tfld)|(_,sfld,tfld)<-mLkpTbl plug,iskey plug tfld] 
attrels::PlugSQL ->[(SqlField,SqlField)]
attrels plug@(ScalarSQL{}) = [(column plug,column plug)]
attrels (BinSQL{})         = error "!Fatal (module Data.Plug 393): Binary plugs do not know the concept of attribute fields."
attrels plug@(TblSQL{})    = [(sfld,tfld)|(_,sfld,tfld)<-mLkpTbl plug,not(iskey plug tfld)] 



--the kernel of SqlFields is ordered by existence of elements for some instance of the entity stored in the plug.
--fldexpr of key is the relation with a similar or larger key.
--(similar = uni,tot,inj,sur, includes = uni,inj,sur)
--
--each kernel field is a key to attributes and itself (kfld), and each attribute field is related to one kernel field (kfld)
--kfld may be smaller than the ID of the plug, but larger than other kernel fields in the plug
--All (kernel) fields larger than or similar to kfld and their total attributes are required.
--(remark that the total property of an attribute points to the relation of the att with its key, which is not the ID of the plug per se)
--Smaller (kernel) fields and their total attributes may contain NULL where kfld does not and are not required.
--
--auto increment fields are not considered to be required
requiredFields :: PlugSQL -> SqlField ->[SqlField]
requiredFields plug@(ScalarSQL{}) _ = [column plug]
requiredFields plug@(BinSQL{}) _ = [fst(columns plug),snd(columns plug)]
requiredFields plug@(TblSQL{}) fld 
 = [f|f<-(requiredkeys++requiredatts), not (fldauto f)] 
  where
  kfld | null findfld = error ("!Fatal (module Data.Plug 415): fld "++fldname fld++" must be in the plug "++name plug++".")
       | iskey plug fld = fld
       | otherwise = fst(head findfld) --fld is an attribute field, take its kernel field
  findfld = [(k,maybek)|(_,k,maybek)<-mLkpTbl plug,fld==maybek]
  requiredkeys = similar++requiredup 
  requiredatts = [a|k<-requiredkeys,(k',a)<-attrels plug,k==k',isTot(fldexpr a)]
  -----------
  --kernelclusters is a list of kernel field clusters clustered by similarity
  --similar is the cluster where kfld is in
  similar = [c|Cluster cs<-kernelclusters plug,kfld `elem` cs,c<-cs]
  --the kernel fields in which a similar field is included, but not a similar field
  --(clusterBy includeskey [Cluster [x]] (kernelrels plug) returns one inclusion chain (cluster) from ID to x 
  --Thus, similar elements of elements in the chain (except x) are not taken into account yet (see similarskeysup and requiredup)
  keysup = (nub[rf|x<-similar
                  ,cs<-map cslist(clusterBy includeskey [Cluster [x]] (kernelrels plug))
                  ,rf<-cs]
           ) >- similar
  --there can be a key1 similar to a key2 in keysup, but key1 is not in keysup.
  --key1 is required just like key2 because they are similar
  similarskeysup = nub[key1|Cluster cs<-kernelclusters plug
                           ,key1<-cs 
                           ,key2<-keysup
                           ,elem key2 cs
                           ,not(elem key1 keysup)]
  --the similarskeysup may have required fields not in keysup (recursion)
  --add those which are not in keysup yet
  requiredup = nub(keysup++requiredbysimilarkeysup)
  requiredbysimilarkeysup = nub[rf|x<-similarskeysup,rf<-requiredFields plug x]
  -----------

--fld1 requires fld2 in plug?
requires :: PlugSQL -> (SqlField,SqlField) ->Bool
requires plug (fld1,fld2) = elem fld2 (requiredFields plug fld1)

--composition from srcfld to trgfld
plugpath :: PlugSQL -> SqlField -> SqlField -> Expression (Relation Concept)
plugpath p@(BinSQL{}) srcfld trgfld
  | srcfld==trgfld = let tm=Tm (mLkp p)(-1) --(note: mLkp p is the relation from fst to snd column of BinSQL)
                     in if srcfld==fst(columns p) 
                        then F [tm,flp tm] --domain of m
                        else F [flp tm,tm] --codomain of m
  | srcfld==fst(columns p) && trgfld==snd(columns p) = fldexpr trgfld
  | trgfld==fst(columns p) && srcfld==snd(columns p) = flp(fldexpr srcfld)
  | otherwise = error ("!Fatal (module Data.Plug 458): BinSQL has only two fields:"++show(fldname srcfld,fldname trgfld,name p))
plugpath p@(ScalarSQL{}) srcfld trgfld
  | srcfld==trgfld = fldexpr trgfld
  | otherwise = error ("!Fatal (module Data.Plug 461): scalarSQL has only one field:"++show(fldname srcfld,fldname trgfld,name p))
plugpath p@(TblSQL{}) srcfld trgfld  
  | srcfld==trgfld && iskey p trgfld = Tm (mIs (target(fldexpr trgfld))) (-1)
  | srcfld==trgfld && not(iskey p trgfld) = F [flp (fldexpr trgfld),(fldexpr trgfld)] --codomain of m of morAtt
  | otherwise = path
  where
  path = if (not.null) (paths srcfld trgfld) 
         then if length(head (paths srcfld trgfld))==1 then head(head (paths srcfld trgfld)) else F (head (paths srcfld trgfld))
         else
         if (not.null) (paths trgfld srcfld) 
         then if length(head (paths trgfld srcfld))==1 then flp(head(head (paths trgfld srcfld))) else flp(F (head (paths trgfld srcfld)))
         else pathoverI
  --paths from s to t by connecting m from mLkpTbl
  --the (m,srcfld,trgfld) from mLkpTbl form paths longer paths if connected: (trgfld m1==srcfld m2) => (m1;m2,srcfld m1,trgfld m2)
  paths s t = [e|(e,es,et)<-eLkpTbl p,s==es,t==et]
  --bijective kernel fields, which are bijective with ID of plug have fldexpr=I[X].
  --thus, path closures of these kernel fields are disjoint (path closure=set of fields reachable by paths),
  --      because these kernel fields connect to themselves by m=I[X] (i.e. end of path).
  --connect two paths over I[X] (I[X];srce)~;(I[X];trge) => filter I[X] => srcpath~;trgpath
  pathoverI 
       = if (not.null) (pathsoverIs srcfld trgfld) 
         then F (head (pathsoverIs srcfld trgfld))
         else
         if (not.null) (pathsoverIs trgfld srcfld) 
         then flp(F (head (pathsoverIs trgfld srcfld)))
         else error ("!Fatal (module Data.Plug 486): no kernelpath:"++show(fldname srcfld,fldname trgfld,name p,[(show es,fldname s,fldname t)|(es,s,t)<-eLkpTbl p]))
  --paths from I to field t
  pathsfromIs t = [(e,es,et)|(e,es,et)<-eLkpTbl p,et==t,not (null e),isIdent(head e)] 
  --paths from s to t over I[X]
  pathsoverIs s t = [flpsrce++(tail trge) 
                    |(srce,srces,_)<-pathsfromIs s
                    ,(trge,trges,_)<-pathsfromIs t
                    ,srces==trges, let F flpsrce= flp(F (tail srce))] 
  

--[Expression (Relation Concept)] implies a 'composition' from SqlField to SqlField which may be empty (no path found) or length==1 (no composition but just head)
--use plugpath to get the Expression from srcfld to trgfld
eLkpTbl::PlugSQL -> [([Expression (Relation Concept)],SqlField,SqlField)]
eLkpTbl p = let mst=[(m,s,t)|(m,s,t)<-mLkpTbl p, s/=t] in addIs (eLkpTbl' mst [([Tm m (-1)],s,t)|(m,s,t)<-mst])
  where
  addIs est = let ist=[(i,ifld)|(i,ifld,ifld')<-mLkpTbl p, ifld==ifld']
              in est ++ [((Tm i(-1)):e,ifld,et)|(i,ifld)<-ist,(e,es,et)<-est,es==ifld]
  eLkpTbl'::[(Relation Concept,SqlField,SqlField)]->[([Expression (Relation Concept)],SqlField,SqlField)]->[([Expression (Relation Concept)],SqlField,SqlField)]
  eLkpTbl' mst est = if null things2add then nub est else recur
    where
    addfront mt = [(e,es,et)|(e,es,et)<-est,mt==es]
    addback  ms = [(e,es,et)|(e,es,et)<-est,ms==et]
    things2add = [()|(_,ms,mt)<-mst,_<-addfront mt++addback ms]
    recur = eLkpTbl'
      [(m,ms,mt)|(m,ms,mt)<-mst,(not.null)(addfront mt),(not.null)(addback ms)] --keep the mst that will not be added to the front or the back (yet)
      (est --keep what you got
       ++ [(((Tm m (-1)):e),ms,et)|(m,ms,mt)<-mst,(e,_,et)<-addfront mt] --add m to the front (except identities)
       ++ [(e++[Tm m (-1)] ,es,mt)|(m,ms,mt)<-mst,(e,es,_)<-addback ms] --add m to the back  (except identities) 
      )

--bijective fields of f (incl. f)
bijectivefields::PlugSQL -> SqlField -> [SqlField]
bijectivefields p f = [bij|Cluster fs<-kernelclusters p,elem f fs,bij<-fs]

--the clusters of kernel sqlfields that are similar because they relate uni,inj,tot,sur
kernelclusters ::PlugSQL -> [Cluster SqlField]
kernelclusters plug@(ScalarSQL{}) = [Cluster [column plug]]
kernelclusters (BinSQL{})         = [] --a binary plugs has no kernel (or at most (entityfield plug))
kernelclusters plug@(TblSQL{})    = clusterBy similarkey [] (kernelrels plug)

--similar key: some source key s that is not equal to target key t (i.e. not the identity), but related uni,tot,inj,sur in some other way
similarkey::(SqlField,SqlField)->Bool
similarkey (s,t) = s/=t && isTot (fldexpr t) && isSur (fldexpr t) && isInj (fldexpr t) && isUni (fldexpr t)

--includes key: some target key t that is related to source key s uni,inj,sur but not tot
includeskey::(SqlField,SqlField)->Bool
includeskey (_,t) = not(isTot (fldexpr t)) && isSur (fldexpr t) && isInj (fldexpr t) && isUni (fldexpr t)

--clusterBy clusters similar items like eqClass clusters equal items
--[(a,a)] defines flat relations between items (not closed)
--((a,a) -> Bool) defines some transitive relation between two items (for example similarity, equality, inclusion)
--[Cluster a] defines the initial set of clusters which may be [] 
--            EXAMPLE USE -> 
--            if the relation is not symmetric and you need one chain from x to the top
--            then set [Cluster [x]]
--            (note: ClusterBy does not take into account any other relation than the one provided!)
--TODO -> test plugs that require more than one run (i.e. a composition of kernel fields n>2: ID(fld1;fld2;..;fldn)KernelConcept )
--REMARK151210 -> I have made a data type of cluster instead of just list to distinguish between lists and clusters (type checked and better readable code)
--                It is an idea to do the same for eqCl and eqClass (Class=Cluster or v.v.)
data Cluster a = Cluster [a] deriving (Eq,Show)
cslist :: Cluster a -> [a]
cslist (Cluster xs) = xs
clusterBy :: (Show a,Eq a) => ((a,a) -> Bool) -> [Cluster a] -> [(a,a)] -> [Cluster a]
clusterBy f [] xs = clusterBy f [Cluster [b]|(_,b)<-xs] xs --initial clusters, for every target there will be a cluster at first (see mergeclusters)
clusterBy f cs xs  
   | cs==nxtrun = mergeclusters cs 
   | otherwise = clusterBy f (mergeclusters nxtrun) xs
   where 
   nxtrun = [Cluster (addtohead (head ys)++ys)|Cluster ys<-cs, not(null ys)]
   addtohead y =[fst x| x<-xs, snd x==y, f x] --if x=(fst x);y and (f x), then fst x is chained to y 
   --we can merge clusters with equal heads, because
   -- + similar things are chained to the head of the cluster
   -- + and the head of mergeclusters == head of every cluster in cs' because we mergecluster each time we add one thing to the head of some cluster
   mergeclusters cs' = [Cluster (nub(concat cl))|cl<-eqClass eqhead (map cslist cs')] 
   eqhead c1 c2 
     | null (c1++c2) = error ("!Fatal (module Data.Plug 561): clusters are not expected to be empty at this point.")
     | otherwise = head c1==head c2

--TODO151210 -> revise ConceptStructure SqlField & PlugSQL
--   concs f = [target e'|let e'=fldexpr f,isSur e']
--   concs p = concs     (fields p)
--   this implies that concs of p are only the targets of kernel fields i.e. kernel concepts  
--   class ConceptStructure describes concs as "the set of all concepts used in data structure a"
--   The question arises (and should be answered in this comment and implemented)
--      WHAT IS THE DATA STRUCTURE PlugSQL?
--I expect that instance ConceptStructure SqlField is only used for instance ConceptStructure PlugSQL as its implementation 
--is tailored to the needs of PlugSQL as a data structure, not SqlField as a Data structure!
--For convenience, I implemented localfunction, which should be removed at revision
instance ConceptStructure SqlField Concept where
  concs     f = [target e'|let e'=fldexpr f,isSur e']
  morlist   f = morlist   (fldexpr f)
-- closExprs f = closExprs (fldexpr f)  
instance ConceptStructure PlugSQL Concept where
  concs     p = concs     (localfunction p)
  mors      p = mors      (localfunction p)
  morlist   p = morlist   (localfunction p)
-- closExprs p = closExprs (localfunction p)

localfunction::PlugSQL -> [SqlField]
localfunction p@(TblSQL{}) = fields p
localfunction p@(BinSQL{}) = [fst (columns p),snd (columns p)]
localfunction p@(ScalarSQL{}) = [column p]

tblfields::PlugSQL->[SqlField]
tblfields plug = case plug of
    TblSQL{}    -> fields plug
    BinSQL{}    -> [fst(columns plug),snd(columns plug)]
    ScalarSQL{} -> [column plug]

type TblRecord = [String]
tblcontents :: PlugSQL -> [TblRecord]
tblcontents plug@(ScalarSQL{})
   = [[x]|(x,_)<-contents'(cLkp plug)]
tblcontents plug@(BinSQL{})
   = [[x,y]|(x,y)<-contents'(mLkp plug)]
tblcontents plug@(TblSQL{})
 --TODO151210 -> remove the assumptions (see comment data PlugSQL)
 --fields are assumed to be in the order kernel+other, 
 --where NULL in a kernel field implies NULL in the following kernel fields
 --and the first field is unique and not null
 --(m,s,t)<-mLkpTbl: s is assumed to be in the kernel, fldexpr t is expected to hold m or (flp m), s and t are assumed to be different
 | null(fields plug) = error ("!Fatal (module Data.Plug 607): no fields in plug.")
 | flduniq idfld && not(fldnull idfld) && isIdent (fldexpr idfld)
   = let 
     pos fld = case elemIndex fld (fields plug) of 
       Just n  -> n+1
       Nothing -> error ("!Fatal (module Data.Plug 612): field is expected.")
     rels fld = [ ((pos s,pos t),xy) | (_,s,t)<-mLkpTbl plug
                , not(s==t)
                , fld==s
                , xy<-contents' (fldexpr t)
                ]
     in --add relation values to the record, from left to right field (concat=rels with source idfld++rels with source fld2++..) 
     [ foldl (insertrel) --(a -> b -> a)
             (take (length (fields plug)) (idval:[[]|_<-[(1::Int)..]])) --new record for id
             (concat (map rels (fields plug)))  
     | idval<-map fst (contents' (fldexpr idfld))  ]
 | otherwise = error ("!Fatal (module Data.Plug 623): fields are assumed to be in the order kernel+other, starting with an id-field.")
   where idfld = head (fields plug)
--if x at position n of some record, then position m is replaced by y (position starts at 1, not 0!)
insertrel::TblRecord->((Int,Int),Paire)->TblRecord
insertrel rec ((n,m),(x,y))
 | length rec < n || length rec < m 
   = error ("!Fatal (module Data.Plug 629): cannot take position "++show n++" or "++show m++" of "++show rec++".")
 | x==head (drop (n-1) rec) --x at position n of rec
   = (take (m-1) rec)++(y:(drop m rec)) --position m is replaced by y
 | otherwise = rec --unchanged

