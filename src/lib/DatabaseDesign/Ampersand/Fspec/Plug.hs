{-# OPTIONS_GHC -Wall #-}
-- SJC: is it possible to move this to the prototype part of ampersand? I mean,
--      do functions like plugFields and plug-path really need to be here?
--      perhaps we can at least move the largest part?
module DatabaseDesign.Ampersand.Fspec.Plug
     (Plugable(..), PlugInfo(..)
     ,SqlField(..)
     ,SqlFieldUsage(..)
     ,SqlType(..)
     ,showSQL
     ,requiredFields,requires,plugpath,eLkpTbl
     
     ,tblcontents
     ,fldauto
     ,isPlugIndex,kernelrels,attrels,bijectivefields
     ,PlugSQL(..)
     )
where
import DatabaseDesign.Ampersand.ADL1 
import DatabaseDesign.Ampersand.Classes (Populated(..),atomsOf,Relational(..))
import DatabaseDesign.Ampersand.Basics
import Data.List(elemIndex,nub)
import GHC.Exts (sortWith)
import DatabaseDesign.Ampersand.Fspec.Fspec
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
 


----------------------------------------------
--PlugSQL
----------------------------------------------
--TblSQL, BinSQL, and ScalarSQL hold different entities. See their definition Fspec.hs

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
--data SqlFieldUsage = PrimKey A_Concept     -- The field is the primary key of the table
--                   | ForeignKey A_Concept  -- The field is a reference (containing the primary key value of) a TblSQL
--                   | PlainAttr             -- None of the above
--                   | NonMainKey            -- Key value of an Specialization of the Primary key. (field could be null)
--                   | UserDefinedUsage
--                   | FillInLater          -- Must be filled in later....

--the entity stored in a plug is an imaginary concept, that is uni,tot,inj,sur with (concept p)
--REMARK: there is a (concept p) because all kernel fields are related SUR with (concept p)


--Maintain rule: Object ObjectDef = Object (makeUserDefinedSqlPlug :: ObjectDef -> PlugSQL)
--TODO151210 -> Build a check which checks this rule for userdefined/showADL generated plugs(::[ObjectDef]) 
--TODO151210 -> The ObjectDef of a BinSQL plug for relation r is that:
--           1) SQLPLUG mybinplug: r      , or
--           2) SQLPLUG labelforsourcem : I /\ r;r~ --(or just I if r is TOT)
--               = [labelfortargetm : r]
--           The first option has been implemented in instance ObjectPlugSQL i.e. attributes=[], ctx=ERel r _
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
 attributes p@TblSQL{}
  = [ Obj (fldname tFld)                                                   -- objnm 
          (Origin "This object is generated by attributes (Object PlugSQL)")                        -- objpos
          (if source a==concept p then a  else f (source a) [[a]])  -- objctx
          Nothing []                                                            -- objats and objstrs
    | (a,_,tFld)<-mLkpTbl p]
    where
     f c mms
      = case sortWith length stop of
         []  -> f c mms'  -- a path from c to a is not found (yet), so add another step to the recursion
         (hd:_) -> case hd of
                    []  -> fatal 201 "Empty head should be impossible."
                    _  -> case [(l,r) | (l,r)<-zip (init hd) (tail hd), target l/=source r] of
                            [] -> foldr1 (.:.) hd  -- pick the shortest path and turn it into an expression.
                            lrs -> fatal 204 ("illegal compositions " ++show lrs)
      where
        mms' = if [] `elem` mms 
               then fatal 295 "null in mms."
               else [a:ms | ms<-mms, (a,_,_)<-mLkpTbl p, target a==source (head ms)]
        stop = if [] `elem` mms'
               then fatal 298 "null in mms'."
               else [ms | ms<-mms', source (head ms)==c]  -- contains all found paths from c to a 
 attributes _ = [] --no attributes for BinSQL and ScalarSQL
 contextOf p@BinSQL{} = mLkp p 
 contextOf p = EDcI (concept p)


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
          
-- Every kernel field is a key, kernel fields are in cLkpTbl or the column of ScalarSQL (which has one column only)
-- isPlugIndex refers to UNIQUE key -- TODO: this is wrong
--isPlugIndex may contain NULL, but their key (the entityfield of the plug) must be unique for a kernel field (isPlugIndex=True)
--the field that is isIdent and isPlugIndex (i.e. concept plug), or any similar (uni,inj,sur,tot) field is also UNIQUE key
--IdentityDefs define UNIQUE key (fld1,fld2,..,fldn)
--REMARK -> a kernel field does not have to be in cLkpTbl, in that cast there is another kernel field that is 
--          thus I must check whether fldexpr isUni && isInj && isSur
isPlugIndex :: PlugSQL->SqlField->Bool
isPlugIndex plug f =
  case plug of 
    ScalarSQL{} -> sqlColumn plug==f
    BinSQL{}  --mLkp is not uni or inj by definition of BinSQL, if mLkp total then the (fldexpr srcfld)=I/\r;r~=I i.e. a key for this plug
     | isUni(mLkp plug) || isInj(mLkp plug) -> fatal 366 "BinSQL may not store a univalent or injective rel, use TblSQL instead."
     | otherwise                            -> False --binary does not have key, but I could do a SELECT DISTINCT iff f==fst(columns plug) && (isTot(mLkp plug)) 
    TblSQL{}    -> elem f (fields plug) && isUni(fldexpr f) && isInj(fldexpr f) && isSur(fldexpr f)

--mLkpTbl stores the relation of some target field with one source field
--an isPlugIndex target field is a kernel field related to some similar or larger kernel field
--any other target field is an attribute field related to its kernel field
kernelrels::PlugSQL ->[(SqlField,SqlField)]
kernelrels plug@ScalarSQL{} = [(sqlColumn plug,sqlColumn plug)]
kernelrels (BinSQL{})       = fatal 375 "Binary plugs do not know the concept of kernel fields."
kernelrels plug@TblSQL{}    = [(sfld,tfld) |(_,sfld,tfld)<-mLkpTbl plug,isPlugIndex plug tfld] 
attrels::PlugSQL ->[(SqlField,SqlField)]
attrels plug@ScalarSQL{}    = [(sqlColumn plug,sqlColumn plug)]
attrels BinSQL{}            = fatal 379 "Binary plugs do not know the concept of attribute fields."
attrels plug@TblSQL{}       = [(sfld,tfld) |(_,sfld,tfld)<-mLkpTbl plug,not(isPlugIndex plug tfld)] 



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
requiredFields plug@ScalarSQL{} _ = [sqlColumn plug]
requiredFields plug@BinSQL{}    _ = [fst(columns plug),snd(columns plug)]
requiredFields plug@TblSQL{} fld 
 = [f |f<-requiredkeys++requiredatts, not (fldauto f)] 
  where
  kfld | null findfld = fatal 401 $ "fld "++fldname fld++" must be in the plug "++name plug++"."
       | isPlugIndex plug fld = fld
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

composeCheck :: Expression -> Expression -> Expression
composeCheck l r
 = if target l/=source r then fatal 316 ("\nl: "++show l++"with target "++show (target l)++"\nl: "++show r++"with source "++show (source r)) else
   l .:. r
 
--composition from srcfld to trgfld, if there is an expression for that
plugpath :: PlugSQL -> SqlField -> SqlField -> Maybe Expression
plugpath p srcfld trgfld =
 case p of
  BinSQL{}
   | srcfld==trgfld -> let tm=mLkp p --(note: mLkp p is the relation from fst to snd column of BinSQL)
                       in if srcfld==fst(columns p) 
                          then Just$ tm .:. flp tm --domain of r
                          else Just$ flp tm .:. tm --codomain of r
   | srcfld==fst(columns p) && trgfld==snd(columns p) -> Just$ fldexpr trgfld
   | trgfld==fst(columns p) && srcfld==snd(columns p) -> Just$ flp(fldexpr srcfld)
   | otherwise -> fatal 444 $ "BinSQL has only two fields:"++show(fldname srcfld,fldname trgfld,name p)
  ScalarSQL{}
   | srcfld==trgfld -> Just$ fldexpr trgfld
   | otherwise -> fatal 447 $ "scalarSQL has only one field:"++show(fldname srcfld,fldname trgfld,name p)
  TblSQL{}  
   | srcfld==trgfld && isPlugIndex p trgfld -> Just$ EDcI (target (fldexpr trgfld))
   | srcfld==trgfld && not(isPlugIndex p trgfld) -> Just$ composeCheck (flp (fldexpr srcfld)) (fldexpr trgfld) --codomain of r of morAtt
   | (not . null) (paths srcfld trgfld)
      -> case head (paths srcfld trgfld) of
          []    -> fatal 338 ("Empty head (paths srcfld trgfld) should be impossible.")
          ps    -> Just$ foldr1 composeCheck ps
   --bijective kernel fields, which are bijective with ID of plug have fldexpr=I[X].
   --thus, path closures of these kernel fields are disjoint (path closure=set of fields reachable by paths),
   --      because these kernel fields connect to themselves by r=I[X] (i.e. end of path).
   --connect two paths over I[X] (I[X];srce)~;(I[X];trge) => filter I[X] => srcpath~;trgpath
   | (not.null) (pathsoverIs srcfld trgfld) -> Just$      foldr1 composeCheck (head (pathsoverIs srcfld trgfld))
   | (not.null) (pathsoverIs trgfld srcfld) -> Just$ flp (foldr1 composeCheck (head (pathsoverIs trgfld srcfld)))
   | otherwise -> Nothing
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
                    ,srces==trges, let flpsrce = (map flp.reverse.tail) srce] 

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
kernelclusters plug@ScalarSQL{} = [Cluster [sqlColumn plug]]
kernelclusters (BinSQL{})       = [] --a binary plugs has no kernel (or at most (entityfield plug))
kernelclusters plug@TblSQL{}    = clusterBy similarkey [] (kernelrels plug)

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


type TblRecord = [String]
tblcontents :: [A_Gen] -> [Population] -> PlugSQL -> [TblRecord]
tblcontents gens udp plug@ScalarSQL{}
   = [[x] | x<-atomsOf gens udp (cLkp plug)]
tblcontents gens udp plug@BinSQL{}
   = [[x,y] |(x,y)<-fullContents gens udp (mLkp plug)]
tblcontents gens udp plug@TblSQL{}
 --TODO15122010 -> remove the assumptions (see comment data PlugSQL)
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
                , xy<-fullContents gens udp (fldexpr t)
                ]
     in --add relation values to the record, from left to right field (concat=rels with source idfld++rels with source fld2++..) 
     [ foldl insertrel --(a -> b -> a)
             (take (length (fields plug)) (idval:[[] |_<-[(1::Int)..]])) --new record for id
             (concatMap rels (fields plug))  
     | idval<-map fst (fullContents gens udp (fldexpr idfld))  ]
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
