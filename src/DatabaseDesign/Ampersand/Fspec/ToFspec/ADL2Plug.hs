{-# OPTIONS_GHC -Wall #-}  
module DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Plug 
  (rel2plug --make a binary sqlplug for a morphism that is neither inj nor uni
  ,makeEntities --generate non-binary sqlplugs for relations that are at least inj or uni, but not already in some user defined sqlplug
  ,makeSqlPlug --make a sqlplug from an ObjectDef (user-defined sql plug)
  ,rel2fld --create field for TblSQL or ScalarSQL plugs 
  )
where
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.Core.Poset
import Prelude hiding (Ord(..))
import DatabaseDesign.Ampersand.Basics     (fatalMsg,Collection(..),Identified(..),eqCl, sort')
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Fspec.Plug
import DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms (isI)
import DatabaseDesign.Ampersand.Fspec.Fspec 
import Data.Char
import Data.List (nub)

fatal :: Int -> String -> a
fatal = fatalMsg "Fspec.ToFspec.ADL2Plug"

-----------------------------------------
--rel2plug
-----------------------------------------
-- rel2plug creates associations (BinSQL) between plugs that represent wide tables.
-- Typical for BinSQL is that it has exactly two columns that are not unique and may not contain NULL values
--
-- this concerns relations that are not univalent nor injective, i.e. flduniq=False for both columns
-- Univalent relations and injective relations cannot be associations, because they are used as attributes in wide tables.
-- REMARK -> imagine a context with only one univalent relation r::A*B.
--           Then r can be found in a wide table plug (TblSQL) with a list of two columns [I[A],r], 
--           and not in a BinSQL with a pair of columns (I/\r;r~, r)
--
-- a relation r (or r~) is stored in this plug
-- the domain of r is stored in the first column with fldexpr=I/\r;r~
-- the codomain of r is stored in the second column with fldexpr=r
-- REMARK -> NULL is not an element of the domain or codomain of r i.e. fldnull=False for both columns
--
-- a total property of r (or r~) implies that the domain of r equals the domain of I[source r] i.e. I/\r;r~ = I
-- Thus, this plug can be used to lookup concept (source r) 
-- REMARK -> whether the total property holds is decided by multiplicities r and totals, where totals is a function in ADL2Fspec
--
-- REMARK -> fldtype is set by the constructor function field
-- REMARK -> because r cannot be INJ or UNI, r must be a BinSQL and cannot be a ScalarSQL or TblSQL
-- REMARK -> a BinSQL has the same meaning as a TblSQL with mLkpTbl=[(r,fld1,fld2)]
--  i.e. fldexpr fld2 holds the relation from fld1 to fld2, which is r
--       and the rule (fldexpr fld1)~;(fldexpr fld1);r = r holds (see comments rel2fld)
--  to get this meaning, fld1 and fld2 cannot be constructed with rel2fld, because fld1 is not a kernel field!
--  let id::(source r)->(source r)[INJ] such that id=I /\ r;r~:
--  + fld1={fldexpr=id,fldnull=not(isTot r),flduniq=isInj r}
--  + fld2={fldexpr=r ,fldnull=not(isTot (id;r) ,flduniq=isInj (id;r)}
--  if isTot r then id=I else not(isSur id)
rel2plug :: Relation -> [Expression] -> PlugSQL
rel2plug  r totals
  | Inj `elem` multiplicities r || Uni `elem` multiplicities r 
    = fatal 55 $ "unexpected call of rel2plug("++show r++"), because it is injective or univalent."
  | not is_Tot && is_Sur 
    = rel2plug (r{reldcl = flpDecl (reldcl r), relsgn=(\(Sign s t) -> Sign t s) (sign r)}) totals
  | otherwise
    = BinSQL { sqlname = name r
             , columns = (srcFld,trgFld)
             , cLkpTbl = [(source r,srcFld)| is_Tot]++[(target r,trgFld)| is_Sur]
             , mLkp    = r
             , sqlfpa  = NO
             }
   where

   srcNm = (if isEndo r then "s" else "")++name (source r)
   srcExpr = if   is_Tot
             then ERel (I (source r)) 
             else EIsc [ERel (I (source r)),ECps [ERel r,flp (ERel r)]]
   trgNm = (if isEndo r then "t" else "")++name (target r)
   trgExpr = ERel r
   srcFld = Fld { fldname = srcNm                       
                , fldexpr = srcExpr
                , fldtype = makeSqlType (target srcExpr)
                , fldnull = False
                , flduniq = isUni r {- will be False -}
                } 
   trgFld = Fld { fldname = trgNm                       
                , fldexpr = ERel r 
                , fldtype = makeSqlType (target trgExpr)
                , fldnull = False
                , flduniq = isInj r {- will be False -}
                } 
   is_Tot = Tot `elem` multiplicities r || ERel r `elem` totals
   is_Sur = Sur `elem` multiplicities r || EFlp (ERel r) `elem` totals

-----------------------------------------
--rel2fld
-----------------------------------------
-- Each relation yields one field f1 in the plug...
-- r is the relation from some kernel field k1 to f1
-- (fldexpr k1) is the relation from the plug's imaginary ID to k1
-- (fldexpr k1);r is the relation from ID to f1
-- the rule (fldexpr k1)~;(fldexpr k1);r = r holds because r is uni and (fldexpr k1) is uni,inj,sur
-- REMARK -> r may be tot or sur, but not inj. (fldexpr k1) may be tot.
--
-- fldnull and fldunique are based on the multiplicity of the relation (kernelpath);r) from ID to (target r)
-- it is given that ID is unique and not null
-- fldnull=not(isTot (kernelpath);r)
-- flduniq=isInj (kernelpath);r
-- 
-- (kernel++plugAtts) defines the name space, making sure that all fields within a plug have unique names.
--
-- WHY151210 -> why sqltype=SQLID if there are any keys around and (isIdent r) and the field does not contain strings?
--              what is the motivation for this implementation?
rel2fld :: [KeyDef] -> [Expression] -> [Expression] -> Expression -> SqlField
rel2fld keyds                                       -- > 
        kernel                                      -- > all relations (in the form either ERel r or EFlp (ERel r)) that may be represented as attributes of this entity.
        plugAtts                                    -- > all relations (in the form either ERel r or EFlp (ERel r)) that are defined as attributes by the user.
        e                                           -- > either ERel r or EFlp (ERel r), representing the relation from some kernel field k1 to f1
 = Fld fldName                                      -- fldname : 
       e                                            -- fldexpr : De target van de expressie geeft de waarden weer in de SQL-tabel-kolom.
       (if isSQLId then SQLId else makeSqlType (target e))  -- fldtype :
       (maybenull e)                                -- fldnull : can there be empty field-values? (intended for data dictionary of DB-implementation)
                                                    --           Error: only if source e is the I-field of this plug.
       (isInj e)                                    -- flduniq : are all field-values unique? (intended for data dictionary of DB-implementation)
                                                    -- all kernel fldexprs are inj
                                                    -- Therefore, a composition of kernel expr (I;kernelpath;e) will also be inj.
                                                    -- It is enough to check isInj e
   where 
   fldName = if null [nm | (r',nm)<-table, e==r'] 
             then fatal 117 $ "null names in table for e: " ++ show (e,table)
             else head [nm | (r',nm)<-table, e==r']
   isSQLId = isIdent e 
              && not (null [key | key<-keyds, kdcpt key==target e]) -- if there are any keys around, make this plug autoincrement.
              && contents e==[] -- and the the field may not contain any strings; WHY is that?
   table   = [ entry
             | cl<-eqCl (map toLower.niceidname) (kernel++plugAtts)
             , entry<-if length cl==1 then [(rel,niceidname rel) |rel<-cl] else tbl cl]
   tbl rs  = [ entry
             | cl<-eqCl (map toLower.name.source) rs
             , entry<-if length cl==1
                      then [(rel,niceidname rel++name (source rel)) |rel<-cl]
                      else [(rel,niceidname rel++show i)|(rel,i)<-zip cl [(0::Int)..]]]
   niceidname rel = if (name.head.mors) rel == "I" then name(target rel) else (name.head.mors) rel
   --in a wide table, m can be total, but the field for its target may contain NULL values,
   --because (why? ...)
   --A kernel field may contain NULL values if
   --  + its field expr is not total OR
   --  + its field expr is not the identity relation AND the (kernel) field for its source may contain NULL values
   --(if the fldexpr of a kernel field is the identity, 
   -- then the fldexpr defines the relation between this kernel field and this kernel field (fldnull=not(isTot I) and flduniq=isInj I)
   -- otherwise it is the relation between this kernel field and some other kernel field)
   maybenull expr
    | length(map target kernel) > length(nub(map target kernel))
       = fatal 146 "more than one kernel field for the same concept"
    | otherwise = case expr of
                   ERel rel -> not $
                                 isTot rel && 
                                 (not.null) [()|k<-kernelpaths, target k==source rel, isTot k]
                   EFlp (ERel rel)
                            -> not $ 
                                 isSur rel &&
                                 (not.null) [()|k<-kernelpaths, target k==source rel, isSur k]
                   _ -> fatal 152 "Illegal Plug Expression"
   kernelpaths = clos kernel
   --    Warshall's transitive closure algorithm, adapted for this purpose:
   clos :: [Expression] -> [Expression]
   clos xs
    = foldl f [ECps [x]| x<-xs] (nub (map source xs) `isc` nub (map target xs))
      where
       f q x = q ++
               [ECps (ls ++ rs) | lft@(ECps ls) <- q, x <= target lft,
                rgt@(ECps rs) <- q, x <= source rgt, null (ls `isc` rs)]
                  
-- ^ Explanation:  rel is a relation from some kernel field k to f
-- ^ (fldexpr k) is the relation from the plug's ID to k
-- ^ (fldexpr k);rel is the relation from ID to f

-----------------------------------------
--makeEntities  (formerly called: makeTblPlugs)
-----------------------------------------
{- makeEntities computes a set of plugs to obtain wide tables with little redundancy.
   It computes entities with their attributes.
   It is based on the principle that each concept is represented in at most one plug, and each relation in at most one plug.
   First, we determine the kernels for all plugs.
   For that, we collect all relations that are univalent, injective, and surjective (the kernel relations).
   By the way, that includes all isa-relations, since they are univalent, injective, and surjective by themselves.
   Two concepts of those relations end up in the same entity iff
   there is a path between them in the concept graph of the kernel relations.
   Of all concepts in an entity, one most generic concept is designated as root.
   Secondly, we take all univalent relations that are not in the kernel, but depart from this kernel.
   These relations serve as attributes. Code:  [a| a<-attRels, source a `elem` concs kernel]
   Then, all these relations are made into fields. Code: plugFields = [rel2fld plugMors a| a<-plugMors]
   We also define two lookup tables, one for the concepts that are stored in the kernel, and one for the attributes of these concepts.
   For the fun of it, we sort the plugs on length, the longest first. Code:   sort' ((0-).length.fields)
   By the way, parameter allRels contains all relations that are declared in context, enriched with extra multiplicities.
   This parameter allRels was added to makePlugs to avoid recomputation of the extra multiplicities.
   The parameter exclusions was added in order to exclude certain concepts and relations from the process.
-}
makeEntities :: ConceptStructure a => A_Context -> [Relation] -> [a] -> [PlugSQL]
makeEntities context allRels exclusions
 = sort' ((0-).length.tblfields)
    [ if and [isIdent r |(r,_,_)<-attributeLookuptable] && length conceptLookuptable==1  
      then --the TblSQL could be a scalar tabel, which is a table that only stores the identity of one concept
      ScalarSQL (name c) (rel2fld [] [ERel (I c)] [] (ERel (I c))) c (ILGV Eenvoudig)
      else
      TblSQL (name c)               -- plname
             plugFields             -- fields
             conceptLookuptable     -- cLkpTbl
             attributeLookuptable   -- mLkpTbl
             (ILGV Eenvoudig)       -- plfpa
    | kernel<-kernels
    , let mainkernel = [head cl |cl<-eqCl target kernel] -- the part of the kernel for concept lookups (cLkpTbl) and linking rels to (mLkpTbl)
                                                         -- note that eqCl guarantees that cl is not empty.
          restkernel = kernel >- mainkernel --the complement of mainkernel
          c = if null mainkernel
              then fatal 198 "null mainkernel."
              else target (head mainkernel)       -- one concept from the kernel is designated to "lead" this plug.
          plugAtts              = [a | a <-attRels, source a `elem` concs mainkernel] --plugAtts link directly to some kernelfield
          plugMors              = mainkernel++restkernel++plugAtts --all relations for which the target is stored in the plug
          plugFields            = [fld a | a<-plugMors]      -- Each field comes from a relation.
          conceptLookuptable   :: [(A_Concept,SqlField)]
          conceptLookuptable    = [(target r,fld r) |r<-mainkernel]
          attributeLookuptable :: [(Relation,SqlField,SqlField)]
          attributeLookuptable  = -- kernel attributes are always surjective from left to right. So do not flip the lookup table!
                                  [((head.mors) e,lookupC (source e),fld e) | e<-plugMors] 
          lookupC cpt           = if null [f |(c',f)<-conceptLookuptable, cpt==c'] 
                                  then fatal 209 "null cLkptable."
                                  else head [f |(c',f)<-conceptLookuptable, cpt==c']
          fld                   = rel2fld (keyDefs context) mainkernel (restkernel++plugAtts)
    ]
   where   
-- The first step is to determine which entities to generate.
-- All concepts and relations mentioned in exclusions are excluded from the process.
    rels,unis :: [Relation]
    rels = [rel | rel <- allRels>-mors exclusions, not (isIdent rel)]
    unis = [r | r<-rels, isUni r, isInj r]
-- In order to make kernels as large as possible,
-- all relations that are univalent and injective are flipped if that makes them surjective.
-- kernelRels contains all relations that occur in kernels.
    kernelRels  :: [Expression]
    kernelRels   = [ERel r |r<-unis, isSur r]++[EFlp (ERel r) |r<-unis, not (isSur r), isTot r]
-- attRels contains all relations that will be attribute of a kernel.
    attRels     :: [Expression]
    attRels      = [ERel r | r<-rs, isUni r]
                ++ [EFlp (ERel r) | r<-rs, not (isUni r), isInj r]
                   where rs = rels>-mors kernelRels
{- The second step is to make kernels for all plugs. In principle, every concept would yield one plug.
However, if two concepts are mutually connected through a surjective, univalent and injective relation, they are combined in one plug.
So the first step is create the kernels ...   -}
--fst kernels = subset of kernel where no two kernel fields have the same target i.e. cLkpTbl
--              attRels will link (see mLkpTbl) to these kernel fields
--snd kernels = complement of (fst kernels) (thus, we will not link attRels to these kernel fields directly)
    kernels :: [[Expression]]
    kernels
     = --error ("Diag ADL2Plug "++show (kernelRels)++"\n"++show (concs rels)++"\n"++show (expand [(c,[])| c<-concs rels]))++
       -- The recursion (function f) starts with the set of kernels that would arise if kernelRels were empty.
       [ ERel (I c): ms  -- at least one relation for each concept in the kernel
       | (c,ms)<-f [(c,[]) | c<-concs rels]    -- the initial kernels
       ]
       where
         f :: [(A_Concept,[Expression])] -> [(A_Concept,[Expression])]
         f ks = if ks==nks then merge (reverse ks) else f (merge nks)      -- all r<-kernelRels are surjective, univalent and injective
          where nks = expand ks
         expand ks = [(c, ms++[e |e<-kernelRels, e `notElem` ms, source e `elem` c:concs ms]) | (c,ms)<-ks] -- expand a kernel (c,ms) by one step
         merge ks = if nks==ks then ks else merge nks
          where nks = oneRun ks
                oneRun [] = []
                oneRun ((c,ms):ks') = (c, ms++[r |(c',ms')<-ks', c' `elem` c:concs ms, r<-ms', r `notElem` ms]):
                                      oneRun [k |k@(c',_)<-ks', c' `notElem` c:concs ms]
    {- Kernels are built recursively. Kernels expand by adding (sur, uni and inj) relations until there are none left.
       Step 1: compute the expansion of each kernel (code: ms++[r |r<-rs, source r `elem` concs ms])
       Step 2: merge kernels if possible (code: recursion over oneRun)
       Step 3: compute the remaining relations (code: [r | r<-rs, source r `notElem` concs [ms | (_,ms)<-kernels]] )
       And call recursively until there are none left. -}


-----------------------------------------
--makeSqlPlug
-----------------------------------------
--makeSqlPlug is used to make user defined plugs. One advantage is that the field names and types can be controlled by the user. 
--
--TODO151210 -> (see also Instance Object PlugSQL)
--              cLkpTbl TblSQL{} can have more than one concept i.e. one for each kernel field
--              a kernel may have more than one concept that is uni,tot,inj,sur with some imaginary ID of the plug (i.e. fldnull=False)
--              When is an ObjectDef a ScalarPlug or BinPlug?
--              When do you want to define your own Scalar or BinPlug
--rel2fld  (keyDefs context) kernel plugAtts r

makeSqlPlug :: A_Context -> ObjectDef -> PlugSQL
makeSqlPlug context obj
 | null(objats obj) && isI(objctx obj)
   = ScalarSQL (name obj) (rel2fld [] [ERel (I c)] [] (ERel (I c))) c (ILGV Eenvoudig)
 | null(objats obj) --TODO151210 -> assuming objctx obj is Rel{} if it is not I{}
   = fatal 2372 "TODO151210 -> implement defining binary plugs in ASCII"
 | isI(objctx obj) --TODO151210 -> a kernel may have more than one concept that is uni,tot,inj,sur with some imaginary ID of the plug
   = TblSQL (name obj)     -- plname (table name)
     plugFields             -- fields
     conceptLookuptable     -- cLkpTbl is een lijst concepten die in deze plug opgeslagen zitten, en hoe je ze eruit kunt halen
     attributeLookuptable   -- mLkpTbl is een lijst met relaties die in deze plug opgeslagen zitten, en hoe je ze eruit kunt halen
     (ILGV Eenvoudig)       -- function point analysis
 | otherwise = fatal 279 "Implementation expects one concept for plug object (SQLPLUG tblX: I[Concept])."
  where       
   c   -- one concept from the kernel is designated to "lead" this plug, this is user-defined.
     = source(objctx obj) 
   rels --fields are user-defined as one deep objats with objctx=r. note: type incorrect or non-relation objats are ignored
     = [(objctx att,sqltp att) | att<-objats obj, source (objctx att)==c]   
   kernel --I[c] and every non-endo r or r~ which is at least uni,inj,sur are kernel fields 
          --REMARK -> endo r or r~ which are at least uni,inj,sur are inefficient in a way
          --          if also TOT than r=I => duplicates, 
          --          otherwise if r would be implemented as GEN (target r) ISA C then (target r) could become a kernel field
     = [(ERel (I c),sqltp obj)] 
       ++ [(r,tp) |(r,tp)<-rels,not (isEndo r),isUni r, isInj r, isSur r]
       ++ [(r,tp) |(r,tp)<-rels,not (isEndo r),isUni r, isInj r, isTot r, not (isSur r)]
   attRels --all user-defined non-kernel fields are attributes of (rel2fld (objctx c))
     = (rels >- kernel) >- [(flp r,tp) |(r,tp)<-kernel] --note: r<-rels where r=objctx obj are ignored (objctx obj=I)
   plugMors              = kernel++attRels
   plugFields            = [fld r tp | (r,tp)<-plugMors] 
   fld r tp              = (rel2fld (keyDefs context) (map fst kernel) (map fst attRels) r){fldtype=tp} --redefine sqltype
   conceptLookuptable    = [(target e,fld e tp) |(e,tp)<-kernel]
   attributeLookuptable  = [(r,lookupC (source r),fld (ERel r) tp) | (ERel r,tp)<-plugMors] ++
                           [(r,lookupC (target r),fld (EFlp (ERel r)) tp) | (EFlp (ERel r),tp)<-plugMors]
   lookupC cpt           = if null [f |(c',f)<-conceptLookuptable, cpt==c'] 
                           then fatal 300 "null cLkptable."
                           else head [f |(c',f)<-conceptLookuptable, cpt==c']
   sqltp :: ObjectDef -> SqlType
   sqltp att = head $ [makeSqltype' sqltp' | strs<-objstrs att,('S':'Q':'L':'T':'Y':'P':'E':'=':sqltp')<-strs]
                      ++[SQLVarchar 255]

makeSqlType :: A_Concept -> SqlType
makeSqlType ONE = fatal 324 "ONE has no type."
makeSqlType c = makeSqltype' (cpttp c)
makeSqltype' :: String -> SqlType
makeSqltype' str = case str of
       ('V':'a':'r':'c':'h':'a':'r':_) -> SQLVarchar 255 --TODO number
       "Pass" -> SQLPass
       ('C':'h':'a':'r':_) -> SQLChar 255 --TODO number
       "Blob" -> SQLBlob
       "Single" -> SQLSingle
       "Double" -> SQLDouble
       ('u':'I':'n':'t':_) -> SQLuInt 4 --TODO number
       ('s':'I':'n':'t':_) -> SQLsInt 4 --TODO number
       "Id" -> SQLId 
       ('B':'o':'o':'l':_) -> SQLBool
       "" -> SQLVarchar 255
       _ -> fatal 335 ("Unknown type: "++str)
