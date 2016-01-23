{-# OPTIONS_GHC -fno-warn-orphans #-}
-- SJC: is it possible to move this to the prototype part of ampersand? I mean,
--      do functions like plugAttributes and plug-path really need to be here?
--      perhaps we can at least move the largest part?
module Database.Design.Ampersand.FSpec.Plug
     (Plugable(..), PlugInfo(..)
     ,SqlAttribute(..)
     ,SqlAttributeUsage(..)
     ,SqlTType(..)
     ,showSQL
     ,plugpath
     ,fldauto
     ,PlugSQL(..)
     )
where
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.Classes (Relational(..))
import Database.Design.Ampersand.Basics
import Data.List
import GHC.Exts (sortWith)
import Database.Design.Ampersand.FSpec.FSpec
import Prelude hiding (Ordering(..))

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

class (Named p, Eq p, Show p) => Plugable p where
  makePlug :: PlugInfo -> p

instance Plugable PlugSQL where
  makePlug (InternalPlug p) = p
  makePlug (ExternalPlug _) = fatal 112 "external plug is not Plugable"

----------------------------------------------
--PlugSQL
----------------------------------------------
--TblSQL, BinSQL, and ScalarSQL hold different entities. See their definition FSpec.hs

--           all kernel attributes can be related to an imaginary concept ID for the plug (a SqlAttribute with type=SQLID)
--             i.e. For all kernel attributes k1,k2, where concept k1=A, concept k2=B, attExpr k1=r~, attExpr k2=s~
--                  You can imagine :
--                    - a relation value::ID->A[INJ] or value::ID->A[INJ,SUR]
--                    - a relation value::ID->B[INJ] or value::ID->B[INJ,SUR]
--                    such that s~=value~;value;r~ and r~=value~;value;s~
--                    because value is at least uni,tot,inj, all NULL in k0 imply NULL in k1 xor v.v.
--                    if value also sur then all NULL in k0 imply NULL in k1 and v.v.
--           Without such an ID, the surjective or total property between any two kernel attributes is required.
--           Because you can imagine an ID concept the surjective or total property between two kernel attribute has become a design choice.
--
--           With or without ID we choose to keep kernel = A closure of concepts A,B for which there exists a r::A->B[INJ] instead of r::A*B[UNI,INJ]
--           By making this choice:
--             - nice database table size
--             - we do not need the imaginary concept ID  (and relation value::ID->A[INJ] or value::ID->A[INJ,SUR]), because:
--                  with ID    -> there will always be one or more kernel attribute k1 such that (value;(attExpr k1)~)[UNI,INJ,TOT,SUR].
--                                any of those k1 can serve as ID of the plug (a.k.a. concept p / source p)
--                  without ID -> any of those k1 can still serve as ID of the plug (a.k.a. concept p / source p)
--               In other words, the imaginary concept is never needed
--                               because there always is an existing one with the correct properties by definition of kernel.
--               Implementation without optional ID:
--                        -> attExpr of some kernel attribute k1 will be r~
--                           k1 holds the target of r~
--                           the source of r~ is a kernel concept too
--                           r~ may be I
--                        -> attExpr of some attMor attribute a1 will be s
--                           a1 holds the target of s
--                           the source of s is a kernel concept
--                        -> sqlRelAttributes r = (r,k1,a1) (or (r,k1,k2)) in mLkpTbl
--                           is used to generate SQL code and PHP-objects without needing the ID attribute.
--                           The ID attribute can be ignored and does not have to be generated because r=(attExpr k1)~;(attExpr a1)
--                           You could generate the ID-attribute with autonum if you want, because it will not be used
--                        -> TODO151210 -> sqlRelAttributes e where e is not in mLkpTbl
--                           option1) Generate the ID attribute (see entityattribute)
--                                    sqlRelAttributes e = (e, idfld;k1, idfld;a1) where e=(attExpr k1)~;value~;value;(attExpr a1)
--                                    remark: binary tables can be binary tables without kernels, but with ID attribute
--                                            (or from a different perspective: ID is the only kernel attribute)
--                                            sqlRelAttributes r = (r,idfld/\r;r~,idfld;m1) where r = (idfld/\r;r~)~;idfld;(attExpr m1)
--                                            (sqlRelAttributes r~  to get the target of r)
--                                            (scalar tables can of course also have an ID attribute)
--                           option2) sqlRelAttributes e = (e, k1;k2;..kn, a1)
--                                    where e=(attExpr kn)~;..;(attExpr k2)~;(attExpr k1)~;(attExpr k1)(attExpr k2);..;(attExpr kn);(attExpr a1)
--                           If I am right the function isTrue tries to support sqlRelAttributes e by ignoring the type error in kn;a1.
--                           That is wrong!

--the entityattribute is not implemented as part of the data type PlugSQL
--It is a constant which may or may not be used (you may always imagine it)
--TODO151210 -> generate the entityattribute if options = --autoid -p
--REMARK151210 -> one would expect I[entityconcept p],
--                but any p (as instance of Object) has one always existing concept p suitable to replace entityconcept p.
--                concept p and entityconcept p are related uni,tot,inj,sur.

--the entity stored in a plug is an imaginary concept, that is uni,tot,inj,sur with (concept p)
--REMARK: there is a (concept p) because all kernel attributes are related SUR with (concept p)

--Maintain rule: Object ObjectDef = Object (makeUserDefinedSqlPlug :: ObjectDef -> PlugSQL)
--TODO151210 -> Build a check which checks this rule for userdefined/showADL generated plugs(::[ObjectDef])
--TODO151210 -> The ObjectDef of a BinSQL plug for relation r is that:
--           1) SQLPLUG mybinplug: r      , or
--           2) SQLPLUG labelforsourcem : I /\ r;r~ --(or just I if r is TOT)
--               = [labelfortargetm : r]
--           The first option has been implemented in instance ObjectPlugSQL i.e. fields=[], ctx=ERel r _
instance Object PlugSQL where
 concept p = case p of
   TblSQL{mLkpTbl = []} -> fatal 263 $ "empty lookup table for plug "++name p++"."
   TblSQL{}             -> --TODO151210-> deze functieimplementatie zou beter moeten matchen met onderstaande beschrijving
                            --        nu wordt aangenomen dat de source van het 1e rel in mLkpTbl de source van de plug is.
                            --a relation between kernel concepts r::A*B is at least [UNI,INJ]
                            --to be able to point out one concept to be the source we are looking for one without NULLs in its attribute
                            -- i.e. there is a concept A such that
                            --      for all kernel attribute expr (s~)::B*C[UNI,INJ]:
                            --      s~ is total and there exists an expr::A*B[UNI,INJ,TOT,SUR] (possibly A=B => I[A][UNI,INJ,TOT,SUR])
                            --If A is such a concept,
                            --   and A is not B,
                            --   and there exist an expr::A*B[UNI,INJ,TOT,SUR]
                            --then (concept PlugSQL{}) may be A or B
                            --REMARK -> (source p) used to be implemented as (source . attExpr . head . fields) p. That is different!
                            head [source r |(r,_,_)<-mLkpTbl p]
   BinSQL{} -> source (mLkp p) --REMARK151210 -> the concept is actually ID such that I[ID]=I[source r]/\r;r~
   ScalarSQL{} -> cLkp p
-- Usually source a==concept p. Otherwise, the attribute computation is somewhat more complicated. See ADL2FSpec for explanation about kernels.
 fields p@TblSQL{}
  = [ Obj (attName tFld)                                                        -- objnm
          (Origin "This object is generated by fields (Object PlugSQL)")    -- objpos
          (if source a==concept p then a  else f (source a) [[a]])              -- objctx
          def
          Nothing 
          Nothing
          []                                                            -- objats and objstrs
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
 fields _ = [] --no fields for BinSQL and ScalarSQL
 contextOf p@BinSQL{} = mLkp p
 contextOf p = EDcI (concept p)

fldauto::SqlAttribute->Bool -- is the attribute auto increment?
fldauto f = case attType f of
              SQLSerial -> if not (attNull f) && attUniq f
                           then True
                           else fatal 171 "AutoIncrement is not allowed at this place." --TODO: build check in P2Aconverters
              _         -> False
              
showSQL :: SqlTType -> String
showSQL (SQLFloat    ) = "FLOAT"
showSQL (SQLVarchar n) = "VARCHAR("++show n++")"
showSQL (SQLText     ) = "TEXT"
showSQL (SQLMediumText ) = "MEDIUMTEXT"
showSQL (SQLBlob     ) = "BLOB"
showSQL (SQLMediumBlob ) = "MEDIUMBLOB"
showSQL (SQLLongBlob ) = "LONGBLOB"
showSQL (SQLDate     ) = "DATE"
showSQL (SQLDateTime ) = "DATETIME"
showSQL (SQLBigInt   ) = "BIGINT"
showSQL (SQLBool     ) = "BOOLEAN"
showSQL (SQLSerial   ) = "SERIAL"

-- Every kernel attribute is a key, kernel attributes are in cLkpTbl or the column of ScalarSQL (which has one column only)
-- isPlugIndex refers to UNIQUE key -- TODO: this is wrong
--isPlugIndex may contain NULL, but their key (the entityattribute of the plug) must be unique for a kernel attribute (isPlugIndex=True)
--the attribute that is isIdent and isPlugIndex (i.e. concept plug), or any similar (uni,inj,sur,tot) attribute is also UNIQUE key
--IdentityDefs define UNIQUE key (fld1,fld2,..,fldn)
--REMARK -> a kernel attribute does not have to be in cLkpTbl, in that cast there is another kernel attribute that is
--          thus I must check whether attExpr isUni && isInj && isSur
isPlugIndex :: PlugSQL->SqlAttribute->Bool
isPlugIndex plug f =
  case plug of
    ScalarSQL{} -> sqlColumn plug==f
    BinSQL{}  --mLkp is not uni or inj by definition of BinSQL, if mLkp total then the (attExpr srcfld)=I/\r;r~=I i.e. a key for this plug
     | isUni(mLkp plug) || isInj(mLkp plug) -> fatal 366 "BinSQL may not store a univalent or injective rel, use TblSQL instead."
     | otherwise                            -> False --binary does not have key, but I could do a SELECT DISTINCT iff f==fst(columns plug) && (isTot(mLkp plug))
    TblSQL{}    -> elem f (attributes plug) && isUni(attExpr f) && isInj(attExpr f) && isSur(attExpr f)



composeCheck :: Expression -> Expression -> Expression
composeCheck l r
 = if target l/=source r then fatal 316 ("\nl: "++show l++"with target "++show (target l)++"\nl: "++show r++"with source "++show (source r)) else
   l .:. r

--composition from srcfld to trgfld, if there is an expression for that
plugpath :: PlugSQL -> SqlAttribute -> SqlAttribute -> Maybe Expression
plugpath p srcfld trgfld =
 case p of
  BinSQL{}
   | srcfld==trgfld -> let tm=mLkp p --(note: mLkp p is the relation from fst to snd column of BinSQL)
                       in if srcfld==fst(columns p)
                          then Just$ tm .:. flp tm --domain of r
                          else Just$ flp tm .:. tm --codomain of r
   | srcfld==fst(columns p) && trgfld==snd(columns p) -> Just$ attExpr trgfld
   | trgfld==fst(columns p) && srcfld==snd(columns p) -> Just$ flp(attExpr srcfld)
   | otherwise -> fatal 444 $ "BinSQL has only two attributes:"++show(attName srcfld,attName trgfld,name p)
  ScalarSQL{}
   | srcfld==trgfld -> Just$ attExpr trgfld
   | otherwise -> fatal 447 $ "scalarSQL has only one attribute:"++show(attName srcfld,attName trgfld,name p)
  TblSQL{}
   | srcfld==trgfld && isPlugIndex p trgfld -> Just$ EDcI (target (attExpr trgfld))
   | srcfld==trgfld && not(isPlugIndex p trgfld) -> Just$ composeCheck (flp (attExpr srcfld)) (attExpr trgfld) --codomain of r of morAtt
   | (not . null) (paths srcfld trgfld)
      -> case head (paths srcfld trgfld) of
          []    -> fatal 338 ("Empty head (paths srcfld trgfld) should be impossible.")
          ps    -> Just$ foldr1 composeCheck ps
   --bijective kernel attributes, which are bijective with ID of plug have attExpr=I[X].
   --thus, path closures of these kernel attributes are disjoint (path closure=set of attributes reachable by paths),
   --      because these kernel attributes connect to themselves by r=I[X] (i.e. end of path).
   --connect two paths over I[X] (I[X];srce)~;(I[X];trge) => filter I[X] => srcpath~;trgpath
   | (not.null) (pathsoverIs srcfld trgfld) -> Just$      foldr1 composeCheck (head (pathsoverIs srcfld trgfld))
   | (not.null) (pathsoverIs trgfld srcfld) -> Just$ flp (foldr1 composeCheck (head (pathsoverIs trgfld srcfld)))
   | otherwise -> Nothing
  --paths from s to t by connecting r from mLkpTbl
  --the (r,srcfld,trgfld) from mLkpTbl form paths longer paths if connected: (trgfld m1==srcfld m2) => (m1;m2,srcfld m1,trgfld m2)
  where
  paths s t = [e |(e,es,et)<-eLkpTbl p,s==es,t==et]
  --paths from I to attribute t
  pathsfromIs t = [(e,es,et) |(e,es,et)<-eLkpTbl p,et==t,not (null e),isIdent(head e)]
  --paths from s to t over I[X]
  pathsoverIs s t = [flpsrce++tail trge
                    |(srce,srces,_)<-pathsfromIs s
                    ,(trge,trges,_)<-pathsfromIs t
                    ,srces==trges, let flpsrce = (map flp.reverse.tail) srce]

--the expression LkpTbl of a plug is the transitive closure of the mLkpTbl of the plug
--Warshall's transitive closure algorithm clos1 :: (Eq a) => [(a,a)] -> [(a,a)] is extended to combine paths i.e. r++r'
--[Expression] implies a 'composition' from a kernel SqlAttribute to another SqlAttribute
--use plugpath to get the Expression from srcfld to trgfld
--plugpath also combines expressions with head I like (I;tail1)~;(I;tail2) <=> tail1;tail2
eLkpTbl::PlugSQL -> [([Expression],SqlAttribute,SqlAttribute)]
eLkpTbl p = clos1 [([r],s,t)|(r,s,t)<-mLkpTbl p]
  where
  clos1 :: [([Expression],SqlAttribute,SqlAttribute)] -> [([Expression],SqlAttribute,SqlAttribute)]     -- e.g. a list of SqlAttribute pairs
  clos1 xs
     = foldl f xs (nub (map (\(_,x,_)->x) xs) `isc` nub (map (\(_,_,x)->x) xs))
       where
        f q x = q `uni` [( r++r' , a, b') | (r ,a, b) <- q, b == x, (r', a', b') <- q, a' == x]


