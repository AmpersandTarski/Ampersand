{-# OPTIONS_GHC -Wall #-}  
module Data.Plug (Plug(..),Plugs
                 ,SqlField(..)
                 ,SqlType(..)
                 ,showSQL
                 ,tblfields
                 ,tblcontents
                 ,entityfield,entityconcept
                 ,field
                 ,fldauto
                 ,PhpValue(..)
                 ,PhpType(..)
                 ,PhpArgs
                 ,PhpReturn(..)
                 ,PhpAction(..)
                 ,iskey
                 ,ActionType(..)
                 ,IsPlug(..),PlugSQL(..),PlugPHP(..),plname,plfpa,renamePlug)
where
import Adl.Concept (Concept(..),Association(..),Signaling(..),cptnew)
import Adl.MorphismAndDeclaration
import Adl.Expression (Expression(..))
import Adl.ObjectDef (ObjectDef(..))
import Adl.FilePos (FilePos(..))
import Adl.Pair (Paire)
import Adl (isSur)
import Classes.Object (Object(..))
import Classes.Populated (contents')
import Classes.Morphical (Morphical(..))
import CommonClasses (Identified(..))
import FPA (FPA(..))
import Maybe (fromMaybe)
import Auxiliaries (sort')
import Prototype.CodeVariables (CodeVar(..))
import List(elemIndex)

class IsPlug a where
  makePlug :: a -> Plug
  pickTypedPlug :: [Plug] -> [a]

instance IsPlug Plug where
  makePlug x = x
  pickTypedPlug x = x
instance IsPlug PlugSQL where
  makePlug x = PlugSql (try2specific x) 
  pickTypedPlug x = [p | (PlugSql p) <- x]
instance IsPlug PlugPHP where
  makePlug x = PlugPhp x
  pickTypedPlug x = [p | (PlugPhp p) <- x]

--use the most specific constructor for PlugSQL
try2specific :: PlugSQL -> PlugSQL
try2specific p@(TblSQL nm (fld1:_) cs ms fp)
  | (foldr (&&) True [isIdent m|(m,_,_)<-ms]) && length cs==1 
    --scalar if one concept and all morphisms are identities
     = let (c,_) = head cs
       in ScalarSQL nm (field (name c) (Tm (mIs c) (-1)) (Just (fldtype fld1)) False True) c fp
  | length (fields p)==2 && null [fld|fld<-fields p, flduniq fld] 
    --the imaginary ID field is assumed not to exist (you may imagine it later)
    --as there is no unique field, the plug must be binary
    --thus, this condition could never be met as p could never be a TblSQL in the first place
     = error ("!Fatal (module Data.Plug 60): TblSQL "++nm++" is binary???.")
  | otherwise = p
try2specific p@(BinSQL nm (fld1,_) ((c,_):[]) m fp)  
  | isIdent m 
    --scalar if one concept and the morphism is an identity
     = ScalarSQL nm (field (name c) (Tm (mIs c) (-1)) (Just (fldtype fld1)) False True) c fp
  | otherwise = p
try2specific p = p

plfpa :: (IsPlug a) => a -> FPA
plfpa p = case (makePlug p) of
           PlugSql x -> sqlfpa x
           PlugPhp x -> phpfpa x
plname :: (IsPlug a) => a -> String
plname p = case (makePlug p) of
            PlugSql x -> sqlname x
            PlugPhp x -> phpname x
renamePlug :: (IsPlug a) => a -> String -> a
renamePlug p nname
 = case (makePlug p) of
            PlugSql x -> head$ pickTypedPlug [PlugSql (x{sqlname=nname})]
            PlugPhp x -> head$ pickTypedPlug [PlugPhp (x{phpname=nname})]

type Plugs = [Plug]
data Plug = PlugSql PlugSQL | PlugPhp PlugPHP deriving (Show,Eq)

--TblSQL, BinSQL, and ScalarSQL hold different entities.
--BinSQL -> stores one morphism m in two ordered columns
--          i.e. a tuple of SqlField -> (source m,target m) with (fldexpr=I/\m;m~, fldexpr=m) 
--            (note: if m TOT then (I/\m;m~ = I). Thus, the concept (source m) is stored in this plug too)
--          with tblcontents = [[x,y]|(x,y)<-contents' m]. 
--ScalarSQL -> stores one concept c in one column
--             i.e. a SqlField -> c
--             with tblcontents = [[x]|(x,_)<-contents' c]. 
--TblSQL -> stores a related collection of morphisms: a kernel of concepts and attribute morphisms of this kernel
--           i.e. a list of SqlField given some A -> [target m | m::A*B,isUni m,isTot m, isInj m] 
--                                                ++ [target m | m::A*B,isUni m, not(isTot m), not(isSur m)]
--             kernel = A closure of concepts A,B for which there exists a m::A->B[INJ] 
--                      (m=fldexpr of kernel field holding instances of B, in practice m is I or a makeMph(flipped declaration))
--             attribute morphisms = All concepts B, A in kernel for which there exists a m::A*B[UNI] and m not TOT and SUR
--                      (m=fldexpr of attMor field, in practice m is a makeMph(declaration))
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

--TODO151210 -> generate the entityfield if options = --autoid -p
entityfield :: PlugSQL -> SqlField
entityfield p
  = field (name (entityconcept p)) --name of imaginary entity concept stored in plug
          (Tm (mIs (concept p)) (-1)) --fldexpr
          (Just SQLId) --fldtype
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
 = TblSQL { sqlname   :: String
           , fields    :: [SqlField]
           , cLkpTbl   :: [(Concept,SqlField)]           -- lookup table that links all kernel concepts to fields in the plug
           , mLkpTbl   :: [(Morphism,SqlField,SqlField)] -- lookup table that links concepts to column names in the plug (kernel+attMors)
           , sqlfpa    :: FPA -- ^ functie punten analyse
           }
 | BinSQL  { --see mor2plug in ADL2Fspec.hs
             sqlname   :: String
           , columns   :: (SqlField,SqlField)
           , cLkpTbl   :: [(Concept,SqlField)] --given that mLkp cannot be (UNI or INJ) (because then m would be in a TblSQL plug)
                                                --if mLkp is TOT, then the concept (source mLkp) is stored in this plug
                                                --if mLkp is SUR, then the concept (target mLkp) is stored in this plug
           , mLkp      :: Morphism -- the morphism links concepts implemented by this plug
           , sqlfpa    :: FPA -- ^ functie punten analyse
           }
 | ScalarSQL
           { sqlname   :: String
           , column    :: SqlField
           , cLkp      :: Concept -- the concept implemented by this plug
           , sqlfpa    :: FPA -- ^ functie punten analyse
           }
   deriving (Show) 


data PlugPHP
 = PlugPHP { phpname   :: String       -- ^ the name of the function
           , phpfile	 :: Maybe String -- ^ the file in which the plug is located (Nothing means it is built in already)
           , phpinArgs :: [CodeVar]    -- ^ the input of this plug (list of arguments)
           , phpOut    :: CodeVar      -- ^ the output of this plug. When the input does not exist, the function should return false instead of an object of this type
           , phpSafe   :: Bool         -- ^ whether the input of this plug is verified. False means that the function can be called with non-existant input, such that it does not return false as output or causes undesired side effects
           , phpfpa    :: FPA          -- ^ functie punten analyse
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
   TblSQL{mLkpTbl = []} -> error ("!Fatal (module Data.Plug 48): empty lookup table for plug "++plname p++".")
   TblSQL{}             -> --TODO151210-> deze functieimplementatie zou beter moeten matchen met onderstaande beschrijving
                            --        nu wordt aangenomen dat de source van het 1e mph in mLkpTbl de source van de plug is.
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
   BinSQL{} -> source (mLkp p)
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
               else F [Tm m (-1)| m<-head (sort' length stop)]  -- pick the shortest path and turn it into an expression.
               where
                 mms' = [a:ms | ms<-mms, (a,_,_)<-mLkpTbl p, target a==source (head ms)]
                 stop = [ms | ms<-mms', source (head ms)==c]  -- contains all found paths from c to a 
 attributes _ = [] --no attributes for BinSQL and ScalarSQL
 ctx p@(BinSQL{}) = Tm (mLkp p) (-1)
 ctx p = Tm (mIs (concept p)) (-1)
 populations p = error ("!TODO (module Data.Plug 42): evaluate population of plug "++plname p++".") --TODO -> (see tblcontents)

--WHY151210 -> why do I need PlugSQL to be an Association
--       in other words why do I need a (target p) for BinSQL and ScalarSQL only
--       (remark: source p=concept p and target PlugSQL{}=error)
instance Association PlugSQL where
   source p               = concept p
   target p@(BinSQL{})    = target (mLkp p)
   target p@(ScalarSQL{}) = cLkp p
   target p               = error ("!Fatal (module Data/Plug 77): cannot compute the target of plug "++plname p++", because it is not binary.")

--WHY151210 -> why can only binary plugs be signals?
instance Signaling PlugSQL where
 isSignal p@(BinSQL{}) = isSignal (mLkp p)
 isSignal _            = False



data PhpValue = PhpNull | PhpObject {objectdf::ObjectDef,phptype::PhpType} deriving (Show)
data PhpType = PhpString | PhpInt | PhpFloat | PhpArray deriving (Show)
type PhpArgs = [(Int,PhpValue)]
data PhpReturn = PhpReturn {retval::PhpValue} deriving (Show)
--DO you need on::[Morphism]? makeFspec sets an empty list
data PhpAction = PhpAction {action::ActionType, on::[Morphism]} deriving (Show)
data ActionType = Create | Read | Update | Delete deriving (Show)

field :: String->Expression->(Maybe SqlType)->Bool->Bool->SqlField
field nm expr maybeTp nul uniq = Fld { fldname = nm
                                     , fldexpr = expr
                                     , fldtype = typ
                                     , fldnull = nul
                                     , flduniq = uniq
                                     }
                           where typ = fromMaybe (fldtyp (target expr)) maybeTp

instance Identified PhpValue where
   name p = case p of {PhpNull -> "0"; PhpObject{objectdf=x} -> objnm x}

--DESCR -> plugs are sorted to optimize some algoritms. 
instance Eq PlugSQL where
  x==y = plname x==plname y
instance Eq PlugPHP where
  x==y = plname x==plname y && phpfile x == phpfile y && phpinArgs x == phpinArgs y
instance Ord Plug where -- WAAROM (SJ) Waarom is Plug een instance van Ord?
  compare x y = compare (plname x) (plname y)

data SqlField = Fld { fldname     :: String
                    , fldexpr     :: Expression
                    , fldtype     :: SqlType
                    , fldnull     :: Bool -- can there be empty field-values?
                    , flduniq     :: Bool -- are all field-values unique?
                    } deriving (Eq, Show)
fldauto::SqlField->Bool -- is the field auto increment?
fldauto f = (fldtype f==SQLId) && not (fldnull f) && flduniq f && isIdent (fldexpr f)

instance Ord SqlField where
  compare a b = compare (fldname a) (fldname b)

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
             deriving (Eq, Ord,Show)

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
          
iskey :: SqlField->Bool
iskey f = flduniq f && not (fldnull f)
--TODO -> does this imply that "BLOB","PASS", etc are reserved concept names?
fldtyp :: Concept->SqlType
fldtyp c = case cptnm c of { "BLOB"   -> SQLBlob;
                            "PASS"   -> SQLPass;
                            "STRING" -> SQLVarchar 255;
                            "TEXT"   -> SQLText;
                            _        -> SQLVarchar 255
                            }


--TODO151210 -> revise Morphical SqlField & PlugSQL
--   concs f = [target e'|let e'=fldexpr f,isSur e']
--   concs p = concs     (fields p)
--   this implies that concs of p are only the targets of kernel fields i.e. kernel concepts  
--   class Morphical describes concs as "the set of all concepts used in data structure a"
--   The question arises (and should be answered in this comment and implemented)
--      WHAT IS THE DATA STRUCTURE PlugSQL?
--I expect that instance Morphical SqlField is only used for instance Morphical PlugSQL as its implementation 
--is tailored to the needs of PlugSQL as a data structure, not SqlField as a Data structure!
--For convenience, I implemented localfunction, which should be removed at revision
instance Morphical SqlField where
  concs     f = [target e'|let e'=fldexpr f,isSur e']
  morlist   f = morlist   (fldexpr f)
  decls     f = decls     (fldexpr f)
  closExprs f = closExprs (fldexpr f)  
instance Morphical PlugSQL where
  concs     p = concs     (localfunction p)
  mors      p = mors      (localfunction p)
  morlist   p = morlist   (localfunction p)
  decls     p = decls     (localfunction p)
  closExprs p = closExprs (localfunction p)
localfunction::PlugSQL -> [SqlField]
localfunction p@(TblSQL{}) = fields p
localfunction p@(BinSQL{}) = [fst (columns p),snd (columns p)]
localfunction p@(ScalarSQL{}) = [column p]

instance Identified Plug where
   name x = plname x
instance Identified PlugSQL where
   name x = plname x
instance Identified PlugPHP where
   name x = plname x

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
 | flduniq idfld && not(fldnull idfld) && isIdent (fldexpr idfld)
   = let 
     pos fld = case elemIndex fld (fields plug) of 
       Just n  -> n+1
       Nothing -> error ("!Fatal (module Data/Plug 245): field is expected.")
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
 | otherwise = error ("!Fatal (module Data/Plug 255): fields are assumed to be in the order kernel+other, starting with an id-field.")
   where idfld = head (fields plug)
--if x at position n of some record, then position m is replaced by y (position starts at 1, not 0!)
insertrel::TblRecord->((Int,Int),Paire)->TblRecord
insertrel rec ((n,m),(x,y))
 | length rec < n || length rec < m 
   = error ("!Fatal (module Data/Plug 262): cannot take position "++show n++" or "++show m++" of "++show rec++".")
 | x==head (drop (n-1) rec) --x at position n of rec
   = (take (m-1) rec)++(y:(drop m rec)) --position m is replaced by y
 | otherwise = rec --unchanged

