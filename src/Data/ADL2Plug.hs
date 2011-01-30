{-# OPTIONS_GHC -Wall -XFlexibleContexts #-}  
module Data.ADL2Plug 
  (rel2plug --make a binary sqlplug for a morphism that is neither inj nor uni
  ,makeEntities --generate non-binary sqlplugs for relations that are at least inj or uni, but not already in some user defined sqlplug
  ,makeSqlPlug --make a sqlplug from an ObjectDef (user-defined sql plug)
  ,rel2fld --create field for TblSQL or ScalarSQL plugs 
  )
where
import DatabaseDesign.Ampersand.Core.Basics     (Collection((>-)))
import Ampersand
import DatabaseDesign.Ampersand.Core.Basics    (eqCl, sort')
import Data.Plug
import Char
import FPA
import Data.List (nub)

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
-- the relation m (or m~) is stored in this plug
-- the domain of m is stored in the first column with fldexpr=I/\m;m~
-- the codomain of m is stored in the second column with fldexpr=m
-- REMARK -> NULL is not an element of the domain or codomain of m i.e. fldnull=False for both columns
--
-- a total property of m (or m~) implies that the domain of m equals the domain of I[source m] i.e. I/\m;m~ = I
-- Thus, this plug can be used to lookup concept (source m) 
-- REMARK -> whether the total property holds is decided by multiplicities m and totals, where totals is a function in ADL2Fspec
--
-- REMARK -> fldtype is set by the constructor function field
-- REMARK -> because m cannot be INJ or UNI, m must be a BinSQL and cannot be a ScalarSQL or TblSQL
-- REMARK -> a BinSQL has the same meaning as a TblSQL with mLkpTbl=[(m,fld1,fld2)]
--  i.e. fldexpr fld2 holds the relation from fld1 to fld2, which is m
--       and the rule (fldexpr fld1)~;(fldexpr fld1);m = m holds (see comments rel2fld)
--  to get this meaning, fld1 and fld2 cannot be constructed with rel2fld, because fld1 is not a kernel field!
--  let id::(source m)->(source m)[INJ] such that id=I /\ m;m~:
--  + fld1={fldexpr=id,fldnull=not(isTot m),flduniq=isInj m}
--  + fld2={fldexpr=m ,fldnull=not(isTot (id;m) ,flduniq=isInj (id;m)}
--  if isTot m then id=I else not(isSur id)
rel2plug :: Relation Concept -> [Relation Concept] -> PlugSQL
rel2plug  m totals
  | Inj `elem` (multiplicities m) || Uni `elem` (multiplicities m) 
    = error ("!Fatal (module ADL2Plug 55): unexpected call of rel2plug("++show m++"), because it is injective or univalent.")
  | not is_Tot && is_Sur 
    = rel2plug (flp m) totals
  | otherwise
    = BinSQL (name m)                                                    -- plname
              (srcFld,trgFld)                                             -- columns
              ([(source m,srcFld)| is_Tot]++[(target m,trgFld)| is_Sur])  -- cLkpTbl
              m                                                           -- mLkp
              NO                                                          -- plfpa 
   where
   srcNm = (if name (source m)==name (target m) then "s_" else "")++name (source m)
   trgNm = (if name (source m)==name (target m) then "t_" else "")++name (target m)
   srcFld = Fld srcNm                         --fldname
                (if   is_Tot
                 then Tm (mIs (source m)) (-1)
                 else Fix [Tm (mIs (source m))(-1),F [Tm m(-1),flp (Tm m(-1))]]
                )                             --fldexpr
                (SQLVarchar 255)              --fldtype
                False                         --fldnull
                (isUni m {- will be False -}) --flduniq
   trgFld = Fld trgNm                         --fldname
                (Tm m (-1))                   --fldexpr
                (SQLVarchar 255)              --fldtype
                False                         --fldnull
                (isInj m {- will be False -}) --flduniq
   is_Tot = Tot `elem` (multiplicities m) || m `elem` totals
   is_Sur = Sur `elem` (multiplicities m) || flp m `elem` totals

-----------------------------------------
--rel2fld
-----------------------------------------
-- Each morphism yields one field f1 in the plug...
-- m is the relation from some kernel field k1 to f1
-- (fldexpr k1) is the relation from the plug's imaginary ID to k1
-- (fldexpr k1);m is the relation from ID to f1
-- the rule (fldexpr k1)~;(fldexpr k1);m = m holds because m is uni and (fldexpr k1) is uni,inj,sur
-- REMARK -> m may be tot or sur, but not inj. (fldexpr k1) may be tot.
--
-- fldnull and fldunique are based on the multiplicity of the relation (kernelpath);m) from ID to (target m)
-- it is given that ID is unique and not null
-- fldnull=not(isTot (kernelpath);m)
-- flduniq=isInj (kernelpath);m
-- 
-- (kernel++plugAtts) defines the name space, making sure that all fields within a plug have unique names.
--
-- WHY151210 -> why sqltype=SQLID if there are any keys around and (isIdent m) and the field does not contain strings?
--              what is the motivation for this implementation?
rel2fld :: [KeyDef] -> [Relation Concept] -> [Relation Concept] -> Relation Concept -> SqlField
rel2fld keyds kernel plugAtts m
 = Fld fldName                                      -- fldname : 
       (Tm m (-1))                                  -- fldexpr : De target van de expressie geeft de waarden weer in de SQL-tabel-kolom.
       (if isSQLId then SQLId else SQLVarchar 255)  -- fldtype :
       (maybenull m)                                -- fldnull : can there be empty field-values? (intended for data dictionary of DB-implementation)
                                                    --           Error: only if source m is the I-field of this plug.
       (isInj m)                                    -- flduniq : are all field-values unique? (intended for data dictionary of DB-implementation)
                                                    -- all kernel fldexprs are inj
                                                    -- Therefore, a composition of kernel expr (I;kernelpath;m) will also be inj.
                                                    -- It is enough to check isInj m
   where 
   fldName = if null [nm| (m',nm)<-table, m==m'] 
             then error ("!Fatal (module ADL2Plug 271): null names in table for m: " ++ show (m,table))
             else head [nm| (m',nm)<-table, m==m']
   isSQLId = isIdent m 
              && not (null [key| key<-keyds, kdcpt key==target m]) -- if there are any keys around, make this plug autoincrement.
              && (contents m==Nothing || contents m==Just []) -- and the the field may not contain any strings
   table   = [ entry
             | cl<-eqCl (map toLower.niceidname) (kernel++plugAtts)
             , entry<-if length cl==1 then [(r,niceidname r)|r<-cl] else tbl cl]
   tbl rs  = [ entry
             | cl<-eqCl (map toLower.name.source) rs
             , entry<-if length cl==1 then [(r,niceidname r++name (source r))|r<-cl] else [(r,niceidname r++show i)|(r,i)<-zip cl [(0::Int)..]]]
   niceidname r = if name r == "I" then name(target r) else name r
   --in a wide table, m can be total, but the field for its target may contain NULL values if the (kernel) field for its source may
   --a kernel field may contain NULL values if
   --  + its field expr is not total OR
   --  + its field expr is not the identity relation AND the (kernel) field for its source may contain NULL values
   --(if the fldexpr of a kernel field is the identity, 
   -- then the fldexpr defines the relation between this kernel field and this kernel field (fldnull=not(isTot I) and flduniq=isInj I)
   -- otherwise it is the relation between this kernel field and some other kernel field)
   maybenull x 
    | length(map target kernel) > length(nub(map target kernel))
       = error "more than one kernel field for the same concept"
    | otherwise = null [()|k<-kernel,target k==source x, isTot x, isIdent k || not(maybenull k)]

-----------------------------------------
--makeEntities  (formerly called: makeTblPlugs)
-----------------------------------------
{- makeEntities computes a set of plugs to obtain wide tables with little redundancy.
   It computes entities with their attributes, which is the essence of the data model generator.
   It is based on the principle that each concept is represented in one plug, and each relation in at most one plug.
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
makeEntities :: ConceptStructure a Concept => Context -> [Relation Concept] -> [a] -> [PlugSQL]
makeEntities context allRels exclusions
 = sort' ((0-).length.tblfields)
    [ if ((foldr (&&) True [isIdent m|(m,_,_)<-attributeLookuptable]) && length conceptLookuptable==1)  
      then --the TblSQL could be a scalar tabel, which is a table that only stores the identity of one concept
      ScalarSQL (name c) (rel2fld [] [mIs c] [] (mIs c)) c (ILGV Eenvoudig)
      else
      TblSQL (name c)               -- plname
             plugFields             -- fields
             conceptLookuptable     -- cLkpTbl
             attributeLookuptable   -- mLkpTbl
             (ILGV Eenvoudig)       -- plfpa
    | kernel<-kernels
    , let mainkernel = [m|cl<-eqCl target kernel, let m=head cl] -- ^ the part of the kernel for concept lookups (cLkpTbl) and linking rels to (mLkpTbl)
                                                                 -- ^ note that eqCl guarantees that cl is not empty.
          restkernel = kernel >- mainkernel --the complement of mainkernel
          c = if null mainkernel
              then error "!Fatal (module ADL2Plug 172): null mainkernel."
              else target (head mainkernel)               -- one concept from the kernel is designated to "lead" this plug.
          plugAtts              = [a| a<-attRels, source a `elem` concs mainkernel] --plugAtts link directly to some kernelfield
          plugMors              = mainkernel++restkernel++plugAtts --all relations for which the target is stored in the plug
          plugFields            = [fld a| a<-plugMors]      -- Each field comes from a relation.
          conceptLookuptable   :: [(Concept,SqlField)]
          conceptLookuptable    = [(target m,fld m)|m<-mainkernel]
          attributeLookuptable :: [(Relation Concept,SqlField,SqlField)]
          attributeLookuptable  = -- kernel attributes are always surjective from left to right. So do not flip the lookup table!
                                  [(m,lookupC (source m),fld m)| m<-plugMors] 
          lookupC cpt           = if null [f|(c',f)<-conceptLookuptable, cpt==c'] 
                                  then error "!Fatal (module ADL2Plug 182): null cLkptable."
                                  else head [f|(c',f)<-conceptLookuptable, cpt==c']
          fld                   = rel2fld (keyDefs context) mainkernel (restkernel++plugAtts)
    ]
   where   
-- The first step is to determine which entities to generate.
-- All concepts and relations mentioned in exclusions are excluded from the process.
    rels = [rel| rel <- allRels>-mors exclusions, not (isIdent rel)]
-- For making kernels as large as possible, the univalent and injective declarations are flipped if that makes them surjective.
-- kernelMors contains all relations that occur in kernels.
    kernelMors   = [m|m<-ms, isSur m]++[flp m|m<-ms, not (isSur m), isTot m]
                      where ms = [d| d<-rels, isUni d, isInj d]
-- iniKernels contains the set of kernels that would arise if kernelMors were empty.
-- From that starting point, the kernels are computed recursively in code that follows (kernels).
    iniKernels   = [(c,[])| c<-concs rels]
-- attRels contains all relations that will be attribute of a kernel.
    attRels      = [m| m<-rs, isUni m] ++ [flp m | m<-rs, not (isUni m), isInj m] ++
                   [m| m<-rs, isSym m && isAsy m] -- ^ the boolean properties
                   where rs = rels>-(kernelMors++map flp kernelMors)
{- The second step is to make kernels for all plugs. In principle, every concept would yield one plug.
However, if two concepts are mutually connected through a surjective, univalent and injective relation, they are combined in one plug.
So the first step is create the kernels ...   -}
--fst kernels = subset of kernel where no two kernel fields have the same target i.e. cLkpTbl
--              attRels will link (see mLkpTbl) to these kernel fields
--snd kernels = complement of (fst kernels) (thus, we will not link attRels to these kernel fields directly)
    kernels :: [[Relation Concept]]
    kernels
     = --error ("Diag ADL2Plug "++show (kernelMors)++"\n"++show (map fst iniKernels)++"\n"++show (expand iniKernels))++
       [ mIs c: ms  -- at least one morphism for each concept in the kernel
       | (c,ms)<-f iniKernels    -- the initial kernels
       ]
       where
         f :: [(Concept,[Relation Concept])] -> [(Concept,[Relation Concept])]
         f ks = if ks==nks then merge (reverse ks) else f (merge nks)      -- all r<-kernelMors are surjective, univalent and injective
          where nks = expand ks
         expand ks = [(c, ms++[r|r<-kernelMors, r `notElem` ms, source r `elem` c:concs ms])| (c,ms)<-ks] -- expand a kernel (c,ms) by one step
         merge ks = if nks==ks then ks else merge nks
          where nks = oneRun ks
                oneRun [] = []
                oneRun ((c,ms):ks') = (c, ms++[m|(c',ms')<-ks', c' `elem` c:concs ms, m<-ms', m `notElem` ms]):
                                      oneRun [k|k@(c',_)<-ks', c' `notElem` c:concs ms]
    {- Kernels are built recursively. Kernels expand by adding (sur, uni and inj) relations until there are none left.
       Step 1: compute the expansion of each kernel (code: ms++[r|r<-rs, source r `elem` concs ms])
       Step 2: merge kernels if possible (code: recursion over oneRun)
       Step 3: compute the remaining relations (code: [r| r<-rs, source r `notElem` concs [ms| (_,ms)<-kernels]] )
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
--rel2fld  (keyDefs context) kernel plugAtts m

makeSqlPlug :: Context -> ObjectDef -> PlugSQL
makeSqlPlug context obj
 | null(objats obj) && isI(objctx obj)
   = ScalarSQL (name obj) (rel2fld [] [mIs c] [] (mIs c)) c (ILGV Eenvoudig)
 | null(objats obj) --TODO151210 -> assuming objctx obj is Rel{} if it is not I{}
   = error "!Fatal (module Data.Adl2Plug 230): TODO151210 -> implement defining binary plugs in ASCII"
 | isI(objctx obj) --TODO151210 -> a kernel may have more than one concept that is uni,tot,inj,sur with some imaginary ID of the plug
   = TblSQL (name obj)     -- plname (table name)
     plugFields             -- fields
     conceptLookuptable     -- cLkpTbl is een lijst concepten die in deze plug opgeslagen zitten, en hoe je ze eruit kunt halen
     attributeLookuptable   -- mLkpTbl is een lijst met morphismen die in deze plug opgeslagen zitten, en hoe je ze eruit kunt halen
     (ILGV Eenvoudig)       -- functie punten analyse
 | otherwise = error "!Fatal (module ADL2Plug 237): Implementation expects one concept for plug object (SQLPLUG tblX: I[Concept])."
  where       
   c   -- one concept from the kernel is designated to "lead" this plug, this is user-defined.
     = source(objctx obj) 
   rels --fields are user-defined as one deep objats with objctx=m. note: type incorrect or non-morphism objats are ignored
     = [(m,sqltp att)|att<-objats obj, (Tm m@(Rel{}) _)<-[objctx att],source m==c]   
   kernel --I[c] and every non-homogeneous m or m~ which is at least uni,inj,sur are kernel fields 
          --REMARK -> homogeneous m or m~ which are at least uni,inj,sur are inefficient in a way
          --          if also TOT than m=I => duplicates, 
          --          otherwise if m would be implemented as GEN (target m) ISA C then (target m) could become a kernel field
     = [(mIs c,sqltp obj)] 
       ++ [(m,tp)|(m,tp)<-rels,source m/=target m,isUni m, isInj m, isSur m]
       ++ [(flp m,tp)|(m,tp)<-rels,source m/=target m,isUni m, isInj m, isTot m, not (isSur m)]
   attRels --all user-defined non-kernel fields are attributes of (rel2fld (objctx c))
     = (rels >- kernel) >- [(flp m,tp)|(m,tp)<-kernel] --note: m<-rels where m=objctx obj are ignored (objctx obj=I)
   plugMors              = kernel++attRels
   plugFields            = [fld m tp| (m,tp)<-plugMors] 
   fld m tp              = (rel2fld (keyDefs context) (map fst kernel) (map fst attRels) m){fldtype=tp} --redefine sqltype
   conceptLookuptable    = [(target m,fld m tp)|(m,tp)<-kernel]
   attributeLookuptable  = [(m,lookupC (source m),fld m tp)| (m,tp)<-plugMors] 
   lookupC cpt           = if null [f|(c',f)<-conceptLookuptable, cpt==c'] 
                           then error "!Fatal (module ADL2Plug 271): null cLkptable."
                           else head [f|(c',f)<-conceptLookuptable, cpt==c']
   sqltp :: ObjectDef -> SqlType
   sqltp att = head $ [makeSqltype sqltp' | strs<-objstrs att,('S':'Q':'L':'T':'Y':'P':'E':'=':sqltp')<-strs]
                      ++[SQLVarchar 255]
   makeSqltype :: String -> SqlType
   makeSqltype str = case str of
       ('V':'a':'r':'c':'h':'a':'r':_) -> SQLVarchar 255 --TODO number
       ('P':'a':'s':'s':_) -> SQLPass
       ('C':'h':'a':'r':_) -> SQLChar 255 --TODO number
       ('B':'l':'o':'b':_) -> SQLBlob
       ('S':'i':'n':'g':'l':'e':_) -> SQLSingle
       ('D':'o':'u':'b':'l':'e':_) -> SQLDouble
       ('u':'I':'n':'t':_) -> SQLuInt 4 --TODO number
       ('s':'I':'n':'t':_) -> SQLsInt 4 --TODO number
       ('I':'d':_) -> SQLId 
       ('B':'o':'o':'l':_) -> SQLBool
       _ -> SQLVarchar 255 --TODO number


