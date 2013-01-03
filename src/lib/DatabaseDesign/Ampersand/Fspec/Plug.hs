{-# OPTIONS_GHC -Wall #-}  
module DatabaseDesign.Ampersand.Fspec.Plug
     (Plugable(..), PlugInfo(..)
     ,SqlField(..)
     ,SqlType(..)
     ,showSQL
     ,requiredFields,requires,plugpath,eLkpTbl
     ,tblfields
     ,tblcontents
     ,entityfield
     ,fldauto
     ,iskey,kernelrels,attrels,bijectivefields
     ,PlugSQL(..)
     )
where
import DatabaseDesign.Ampersand.ADL1 
import DatabaseDesign.Ampersand.Classes (Object(..),Populated(..),atomsOf,ConceptStructure(..),Relational(..))
import DatabaseDesign.Ampersand.Basics
import Data.List(elemIndex,nub)
import GHC.Exts (sortWith)
import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Fspec.FPA (FPAble(fpa))
import DatabaseDesign.Ampersand.Core.Poset(Ordering(..))
import Prelude hiding (Ordering(..))


fatal :: Int -> String -> a
fatal = fatalMsg "Fspec.Plug"

----------------------------------------------
--Plug
----------------------------------------------
--TODO151210 -> define what a plug is and what it should do
--Plugs are of the class Object just like Activities(??? => PHP plug isn't an instance of Object)
--An Object is an entity to do things with like reading, updating, creating,deleting.
--A Interface is an Object using only Plugs for reading and writing data; a Plug is a data service maintaining the rules for one object:
-- + GEN Interface,Plug ISA Object
-- + cando::Operation*Object
-- + uses::Interface*Plug [TOT].
-- + maintains::Plug*Rule.
-- + signals::Interface*SignalRule.
--
--Plugs can currently be implemented in PHP or SQL.
--type Plugs = [Plug]
--data Plug = PlugSql PlugSQL | PlugPhp PlugPHP deriving (Show,Eq)

class (Identified p, Eq p, Show p) => Plugable p where
  makePlug :: PlugInfo -> p
  
instance Plugable PlugSQL where
  makePlug (InternalPlug p) = p
  makePlug (ExternalPlug _) = fatal 112 "external plug is not Plugable"
 
instance FPAble PlugInfo where
  fpa (InternalPlug _) = fatal 55 "FPA analysis of internal plugs is currently not supported"
  fpa (ExternalPlug _)  = fatal 56 "FPA analysis of external plugs is currently not supported"

instance ConceptStructure PlugInfo where
  concs   (InternalPlug psql) = concs   psql
  concs   (ExternalPlug obj)  = concs   obj
  mors    (InternalPlug psql) = mors    psql
  mors    (ExternalPlug obj)  = mors    obj
  morlist (InternalPlug psql) = morlist psql
  morlist (ExternalPlug obj)  = morlist obj
  mp1Rels (InternalPlug psql) = mp1Rels psql
  mp1Rels (ExternalPlug obj)  = mp1Rels obj
   


----------------------------------------------
--PlugSQL
----------------------------------------------
--TblSQL, BinSQL, and ScalarSQL hold different entities.
--BinSQL -> (see the only constructor function rel2plug in ADL2Plug for detailed comments)
--          stores one relation r in two ordered columns
--          i.e. a tuple of SqlField -> (source r,target r) with (fldexpr=I/\r;r~, fldexpr=r) 
--            (note: if r TOT then (I/\r;r~ = I). Thus, the concept (source r) is stored in this plug too)
--          with tblcontents = [[x,y] |(x,y)<-contents r]. 
--          Typical for BinSQL is that it has exactly two columns that are not unique and may not contain NULL values
--ScalarSQL -> stores one concept c in one column
--             i.e. a SqlField -> c
--             with tblcontents = [[x] |(x,_)<-contents c].
--             Typical for ScalarSQL is that it has exactly one column that is unique and may not contain NULL values i.e. fldexpr=I[c]
--TblSQL -> stores a related collection of relations: a kernel of concepts and attribute relations of this kernel
--           i.e. a list of SqlField given some A -> [target r | r::A*B,isUni r,isTot r, isInj r] 
--                                                ++ [target r | r::A*B,isUni r, not(isTot r), not(isSur r)]
--             kernel = A closure of concepts A,B for which there exists a r::A->B[INJ] 
--                      (r=fldexpr of kernel field holding instances of B, in practice r is I or a makeRelation(flipped declaration))
--             attribute relations = All concepts B, A in kernel for which there exists a r::A*B[UNI] and r not TOT and SUR
--                      (r=fldexpr of attMor field, in practice r is a makeRelation(declaration))
--           all kernel fields can be related to an imaginary concept ID for the plug (a SqlField with type=SQLID)
--             i.e. For all kernel fields k1,k2, where concept k1=A, concept k2=B, fldexpr k1=r~, fldexpr k2=s~
--                  You can imagine :
--                    - a relation value::ID->A[INJ] or value::ID->A[INJ,SUR]
--                    - a relation value::ID->B[INJ] or value::ID->B[INJ,SUR]
--                    such that s~=value~;value;r~ and r~=value~;value;s~
--                    because value is at least uni,tot,inj, all NULL in k0 imply NULL in k1 xor v.v.
--                    if value also sur then all NULL in k0 imply NULL in k1 and v.v.
--           Without such an ID, the surjective or total property between any two kernel fields is required.
--           Because you can imagine an ID concept the surjective or total property between two kernel field has become a design choice.
--
--           With or without ID we choose to keep kernel = A closure of concepts A,B for which there exists a r::A->B[INJ] instead of r::A*B[UNI,INJ]
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
--                        -> sqlRelFields r = (r,k1,a1) (or (r,k1,k2)) in mLkpTbl
--                           is used to generate SQL code and PHP-objects without needing the ID field.
--                           The ID field can be ignored and does not have to be generated because r=(fldexpr k1)~;(fldexpr a1)
--                           You could generate the ID-field with autonum if you want, because it will not be used
--                        -> TODO151210 -> sqlRelFields e where e is not in mLkpTbl
--                           option1) Generate the ID field (see entityfield)
--                                    sqlRelFields e = (e, idfld;k1, idfld;a1) where e=(fldexpr k1)~;value~;value;(fldexpr a1)
--                                    remark: binary tables can be binary tables without kernels, but with ID field
--                                            (or from a different perspective: ID is the only kernel field)
--                                            sqlRelFields r = (r,idfld/\r;r~,idfld;m1) where r = (idfld/\r;r~)~;idfld;(fldexpr m1)
--                                            (sqlRelFields r~  to get the target of r)
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
        (ERel (I (concept p)) ) --fldexpr
        SQLId --fldtype
        False --isnull
        True --isuniq
--the entity stored in a plug is an imaginary concept, that is uni,tot,inj,sur with (concept p)
--REMARK: there is a (concept p) because all kernel fields are related SUR with (concept p)
entityconcept :: PlugSQL -> A_Concept
entityconcept BinSQL{} --create the entityconcept of the plug, and an instance of ID for each instance of mLkp
  = C { cptnm = "ID"
      , cptgE = (\x y -> if x==y then EQ else NC,[])  -- TODO: Is this the right place to define this ordering??
      , cpttp = []
      , cptdf = []
      }
entityconcept p --copy (concept p) to create the entityconcept of the plug, using instances of (concept p) as instances of ID
  = case concept p of
     C{} -> (concept p){cptnm=name(concept p)++ "ID"} 
     _   ->  fatal 225 $ "entityconcept error in PlugSQL: "++name p++"."





--Maintain rule: Object ObjectDef = Object (makeSqlPlug :: ObjectDef -> PlugSQL)
--TODO151210 -> Build a check which checks this rule for userdefined/showADL generated plugs(::[ObjectDef]) 
--TODO151210 -> The ObjectDef of a BinSQL plug for relation r is that:
--           1) SQLPLUG mybinplug: r      , or
--           2) SQLPLUG labelforsourcem : I /\ r;r~ --(or just I if r is TOT)
--               = [labelfortargetm : r]
--           The first option has been implemented in instance ObjectPlugSQL i.e. attributes=[], ctx=ERel r 
instance Object PlugSQL where
 concept p = case p of
   TblSQL{mLkpTbl = []} -> fatal 263 $ "empty lookup table for plug "++name p++"."
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
                            head [source r |(r,_,_)<-mLkpTbl p]
   BinSQL{} -> source (mLkp p) --REMARK151210 -> the concept is actually ID such that I[ID]=I[source r]/\r;r~
   ScalarSQL{} -> cLkp p
-- Usually source a==concept p. Otherwise, the attribute computation is somewhat more complicated. See ADL2Fspec for explanation about kernels.
 attributes p@(TblSQL{})
  = [ Obj (fldname tFld)                                                   -- objnm 
          (Origin "This object is generated by attributes (Object PlugSQL)")                        -- objpos
          (if source a==concept p then a  else f (source a) [[a]])  -- objctx
          Nothing []                                                            -- objats and objstrs
    | (a,_,tFld)<-mLkpTbl p]
    where
     f c mms
       | null stop = f c mms'  -- a path from c to a is not found (yet), so add another step to the recursion
       | null (sortWith length stop) = fatal 243 "null (sortWith length stop)."
       | otherwise = case head (sortWith length stop) of [e] -> e ; es -> ECps es  -- pick the shortest path and turn it into an expression.
      where
        mms' = if [] `elem` mms 
               then fatal 295 "null in mms."
               else [a:ms | ms<-mms, (a,_,_)<-mLkpTbl p, target a==source (head ms)]
        stop = if [] `elem` mms'
               then fatal 298 "null in mms'."
               else [ms | ms<-mms', source (head ms)==c]  -- contains all found paths from c to a 
 attributes _ = [] --no attributes for BinSQL and ScalarSQL
 contextOf p@(BinSQL{}) = mLkp p 
 contextOf p = ERel (I (concept p)) 

{-WHY151210 -> why do I need PlugSQL to be an Association
--       in other words why do I need a (target p) for BinSQL and ScalarSQL only
--       (remark: source p=concept p and target PlugSQL{}=error)
instance Association PlugSQL Concept where
   source p               = concept p
   target p@(BinSQL{})    = target (mLkp p)
   target p@(ScalarSQL{}) = cLkp p
   target p               = fatal 312 $ "cannot compute the target of plug "++name p++", because it is not binary."
-}

--WHY151210 -> why can only binary plugs be signals?
--instance Signaling PlugSQL where
-- isSignal p@(BinSQL{}) = isSignal (mLkp p)
-- isSignal _            = False

fldauto::SqlField->Bool -- is the field auto increment?
fldauto f = (fldtype f==SQLId) && not (fldnull f) && flduniq f -- && isIdent (fldexpr f)


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
iskey plug@(ScalarSQL{}) f = sqlColumn plug==f
iskey plug@(BinSQL{}) _ --mLkp is not uni or inj by definition of BinSQL, if mLkp total then the (fldexpr srcfld)=I/\r;r~=I i.e. a key for this plug
  | isUni(mLkp plug) || isInj(mLkp plug) = fatal 366 "BinSQL may not store a univalent or injective rel, use TblSQL instead."
  | otherwise              = False --binary does not have key, but I could do a SELECT DISTINCT iff f==fst(columns plug) && (isTot(mLkp plug)) 
iskey plug@(TblSQL{}) f    = elem f (fields plug) && isUni(fldexpr f) && isInj(fldexpr f) && isSur(fldexpr f)

--mLkpTbl stores the relation of some target field with one source field
--an iskey target field is a kernel field related to some similar or larger kernel field
--any other target field is an attribute field related to its kernel field
kernelrels::PlugSQL ->[(SqlField,SqlField)]
kernelrels plug@(ScalarSQL{}) = [(sqlColumn plug,sqlColumn plug)]
kernelrels (BinSQL{})         = fatal 375 "Binary plugs do not know the concept of kernel fields."
kernelrels plug@(TblSQL{})    = [(sfld,tfld) |(_,sfld,tfld)<-mLkpTbl plug,iskey plug tfld] 
attrels::PlugSQL ->[(SqlField,SqlField)]
attrels plug@(ScalarSQL{}) = [(sqlColumn plug,sqlColumn plug)]
attrels (BinSQL{})         = fatal 379 "Binary plugs do not know the concept of attribute fields."
attrels plug@(TblSQL{})    = [(sfld,tfld) |(_,sfld,tfld)<-mLkpTbl plug,not(iskey plug tfld)] 



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
requiredFields plug@(ScalarSQL{}) _ = [sqlColumn plug]
requiredFields plug@(BinSQL{}) _ = [fst(columns plug),snd(columns plug)]
requiredFields plug@(TblSQL{}) fld 
 = [f |f<-requiredkeys++requiredatts, not (fldauto f)] 
  where
  kfld | null findfld = fatal 401 $ "fld "++fldname fld++" must be in the plug "++name plug++"."
       | iskey plug fld = fld
       | otherwise = fst(head findfld) --fld is an attribute field, take its kernel field
  findfld = [(k,maybek) |(_,k,maybek)<-mLkpTbl plug,fld==maybek]
  requiredkeys = similar++requiredup 
  requiredatts = [a |k<-requiredkeys,(k',a)<-attrels plug,k==k',isTot(fldexpr a)]
  -----------
  --kernelclusters is a list of kernel field clusters clustered by similarity
  --similar is the cluster where kfld is in
  similar = [c |Cluster cs<-kernelclusters plug,kfld `elem` cs,c<-cs]
  --the kernel fields in which a similar field is included, but not a similar field
  --(clusterBy includeskey [Cluster [x]] (kernelrels plug) returns one inclusion chain (cluster) from ID to x 
  --Thus, similar elements of elements in the chain (except x) are not taken into account yet (see similarskeysup and requiredup)
  keysup = nub[rf |x<-similar
                  ,cs<-map cslist(clusterBy includeskey [Cluster [x]] (kernelrels plug))
                  ,rf<-cs]
            >- similar
  --there can be a key1 similar to a key2 in keysup, but key1 is not in keysup.
  --key1 is required just like key2 because they are similar
  similarskeysup = nub[key1 | Cluster cs<-kernelclusters plug
                            , key1<-cs 
                            , key2<-keysup
                            , key2 `elem` cs
                            , key1 `notElem` keysup]
  --the similarskeysup may have required fields not in keysup (recursion)
  --add those which are not in keysup yet
  requiredup = nub(keysup++requiredbysimilarkeysup)
  requiredbysimilarkeysup = nub[rf |x<-similarskeysup,rf<-requiredFields plug x]
  -----------

--fld1 requires fld2 in plug?
requires :: PlugSQL -> (SqlField,SqlField) ->Bool
requires plug (fld1,fld2) = fld2 `elem` requiredFields plug fld1

--composition from srcfld to trgfld
plugpath :: PlugSQL -> SqlField -> SqlField -> Expression
plugpath p@(BinSQL{}) srcfld trgfld
  | srcfld==trgfld = let tm=mLkp p --(note: mLkp p is the relation from fst to snd column of BinSQL)
                     in if srcfld==fst(columns p) 
                        then ECps [tm,flp tm] --domain of r
                        else ECps [flp tm,tm] --codomain of r
  | srcfld==fst(columns p) && trgfld==snd(columns p) = fldexpr trgfld
  | trgfld==fst(columns p) && srcfld==snd(columns p) = flp(fldexpr srcfld)
  | otherwise = fatal 444 $ "BinSQL has only two fields:"++show(fldname srcfld,fldname trgfld,name p)
plugpath p@(ScalarSQL{}) srcfld trgfld
  | srcfld==trgfld = fldexpr trgfld
  | otherwise = fatal 447 $ "scalarSQL has only one field:"++show(fldname srcfld,fldname trgfld,name p)
plugpath p@(TblSQL{}) srcfld trgfld  
  | srcfld==trgfld && iskey p trgfld = ERel (I (target(fldexpr trgfld))) 
  | srcfld==trgfld && not(iskey p trgfld) = ECps [flp (fldexpr srcfld), fldexpr trgfld] --codomain of r of morAtt
  | (not . null) (paths srcfld trgfld) = if length (head (paths srcfld trgfld)) == 1
                                         then head (head (paths srcfld trgfld))
                                         else ECps (head (paths srcfld trgfld))
      
  | (not . null) (paths trgfld srcfld) = if length (head (paths trgfld srcfld)) == 1
                                         then flp (head (head (paths trgfld srcfld)))
                                         else flp (ECps (head (paths trgfld srcfld)))
  --bijective kernel fields, which are bijective with ID of plug have fldexpr=I[X].
  --thus, path closures of these kernel fields are disjoint (path closure=set of fields reachable by paths),
  --      because these kernel fields connect to themselves by r=I[X] (i.e. end of path).
  --connect two paths over I[X] (I[X];srce)~;(I[X];trge) => filter I[X] => srcpath~;trgpath
  | (not.null) (pathsoverIs srcfld trgfld) =      ECps (head (pathsoverIs srcfld trgfld))
  | (not.null) (pathsoverIs trgfld srcfld) = flp (ECps (head (pathsoverIs trgfld srcfld)))
  | otherwise = fatal 406 $ "no kernelpath:"++show(fldname srcfld,fldname trgfld,name p,[(show es,fldname s,fldname t) |(es,s,t)<-eLkpTbl p])
  --paths from s to t by connecting r from mLkpTbl
  --the (r,srcfld,trgfld) from mLkpTbl form paths longer paths if connected: (trgfld m1==srcfld m2) => (m1;m2,srcfld m1,trgfld m2)
  where
  paths s t = [e |(e,es,et)<-eLkpTbl p,s==es,t==et]
  --paths from I to field t
  pathsfromIs t = [(e,es,et) |(e,es,et)<-eLkpTbl p,et==t,not (null e),isIdent(head e)] 
  --paths from s to t over I[X]
  pathsoverIs s t = [flpsrce++tail trge 
                    |(srce,srces,_)<-pathsfromIs s
                    ,(trge,trges,_)<-pathsfromIs t
                    ,srces==trges, let ECps flpsrce= flp(ECps (tail srce))] 

--the expression LkpTbl of a plug is the transitive closure of the mLkpTbl of the plug
--Warshall's transitive closure algorithm clos1 :: (Eq a) => [(a,a)] -> [(a,a)] is extended to combine paths i.e. r++r'
--[Expression] implies a 'composition' from a kernel SqlField to another SqlField
--use plugpath to get the Expression from srcfld to trgfld
--plugpath also combines expressions with head I like (I;tail1)~;(I;tail2) <=> tail1;tail2
eLkpTbl::PlugSQL -> [([Expression],SqlField,SqlField)]
eLkpTbl p = clos1 [([r],s,t)|(r,s,t)<-mLkpTbl p]
  where
  clos1 :: [([Expression],SqlField,SqlField)] -> [([Expression],SqlField,SqlField)]     -- e.g. a list of SqlField pairs
  clos1 xs
     = foldl f xs (nub (map (\(_,x,_)->x) xs) `isc` nub (map (\(_,_,x)->x) xs))
       where
        f q x = q `uni` [( r++r' , a, b') | (r ,a, b) <- q, b == x, (r', a', b') <- q, a' == x]

--bijective fields of f (incl. f)
bijectivefields::PlugSQL -> SqlField -> [SqlField]
bijectivefields p f = [bij |Cluster fs<-kernelclusters p, f `elem` fs,bij<-fs]

--the clusters of kernel sqlfields that are similar because they relate uni,inj,tot,sur
kernelclusters ::PlugSQL -> [Cluster SqlField]
kernelclusters plug@(ScalarSQL{}) = [Cluster [sqlColumn plug]]
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
clusterBy f [] xs = clusterBy f [Cluster [b] |(_,b)<-xs] xs --initial clusters, for every target there will be a cluster at first (see mergeclusters)
clusterBy f cs xs  
   | cs==nxtrun = mergeclusters cs 
   | otherwise = clusterBy f (mergeclusters nxtrun) xs
   where 
   nxtrun = [Cluster (addtohead (head ys)++ys) |Cluster ys<-cs, not(null ys)]
   addtohead y =[fst x | x<-xs, snd x==y, f x] --if x=(fst x);y and (f x), then fst x is chained to y 
   --we can merge clusters with equal heads, because
   -- + similar things are chained to the head of the cluster
   -- + and the head of mergeclusters == head of every cluster in cs' because we mergecluster each time we add one thing to the head of some cluster
   mergeclusters cs' = [Cluster (nub(concat cl)) |cl<-eqClass eqhead (map cslist cs')] 
   eqhead c1 c2 
     | null (c1++c2) = fatal 547 "clusters are not expected to be empty at this point."
     | otherwise = head c1==head c2

--TODO151210 -> revise ConceptStructure SqlField & PlugSQL
--   concs f = [target e' |let e'=fldexpr f,isSur e']
--   concs p = concs     (fields p)
--   this implies that concs of p are only the targets of kernel fields i.e. kernel concepts  
--   class ConceptStructure describes concs as "the set of all concepts used in data structure a"
--   The question arises (and should be answered in this comment and implemented)
--      WHAT IS THE DATA STRUCTURE PlugSQL?
--I expect that instance ConceptStructure SqlField is only used for instance ConceptStructure PlugSQL as its implementation 
--is tailored to the needs of PlugSQL as a data structure, not SqlField as a Data structure!
--For convenience, I implemented localfunction, which should be removed at revision
instance ConceptStructure SqlField where
  concs     f = [target e' |let e'=fldexpr f,isSur e']
  morlist   f = morlist   (fldexpr f)
  mp1Rels = fatal 452 "mp1Rels is not ment to be for a plug."

instance ConceptStructure PlugSQL where
  concs     p = concs     (localfunction p)
  mors      p = mors      (localfunction p)
  morlist   p = morlist   (localfunction p)
  mp1Rels = fatal 458 "mp1Rels is not ment to be for a plug."

localfunction::PlugSQL -> [SqlField]
localfunction p@(TblSQL{}) = fields p
localfunction p@(BinSQL{}) = [fst (columns p),snd (columns p)]
localfunction p@(ScalarSQL{}) = [sqlColumn p]

tblfields::PlugSQL->[SqlField]
tblfields plug = case plug of
    TblSQL{}    -> fields plug
    BinSQL{}    -> [fst(columns plug),snd(columns plug)]
    ScalarSQL{} -> [sqlColumn plug]

type TblRecord = [String]
tblcontents :: [UserDefPop] -> PlugSQL -> [TblRecord]
tblcontents udp plug@(ScalarSQL{})
   = [[x] | x<-atomsOf udp (cLkp plug)]
tblcontents udp plug@(BinSQL{})
   = [[x,y] |(x,y)<-fullContents udp (mLkp plug)]
tblcontents udp plug@(TblSQL{})
 --TODO151210 -> remove the assumptions (see comment data PlugSQL)
 --fields are assumed to be in the order kernel+other, 
 --where NULL in a kernel field implies NULL in the following kernel fields
 --and the first field is unique and not null
 --(r,s,t)<-mLkpTbl: s is assumed to be in the kernel, fldexpr t is expected to hold r or (flp r), s and t are assumed to be different
 | null(fields plug) = fatal 593 "no fields in plug."
 | flduniq idfld && not(fldnull idfld) && isIdent (fldexpr idfld)
   = let 
     pos fld = case elemIndex fld (fields plug) of 
       Just n  -> n+1
       Nothing -> fatal 598 "field is expected."
     rels fld = [ ((pos s,pos t),xy) | (_,s,t)<-mLkpTbl plug
                , s /= t 
                , fld==s
                , xy<-fullContents udp (fldexpr t)
                ]
     in --add relation values to the record, from left to right field (concat=rels with source idfld++rels with source fld2++..) 
     [ foldl insertrel --(a -> b -> a)
             (take (length (fields plug)) (idval:[[] |_<-[(1::Int)..]])) --new record for id
             (concatMap rels (fields plug))  
     | idval<-map fst (fullContents udp (fldexpr idfld))  ]
 | otherwise = fatal 609 "fields are assumed to be in the order kernel+other, starting with an id-field."
   where idfld = head (fields plug)
--if x at position n of some record, then position r is replaced by y (position starts at 1, not 0!)
insertrel::TblRecord->((Int,Int),Paire)->TblRecord
insertrel rec ((n,r),(x,y))
 | length rec < n || length rec < r 
   = fatal 615 $ "cannot take position "++show n++" or "++show r++" of "++show rec++"."
 | x==(rec !! (n - 1)) --x at position n of rec
   = take (r-1) rec++y:drop r rec --position r is replaced by y
 | otherwise = rec --unchanged
