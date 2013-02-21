{-# OPTIONS_GHC -Wall #-}  
module DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Plug 
  (makeGeneratedSqlPlugs
  ,makeUserDefinedSqlPlug 
  )
where
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree hiding (sortWith)
import DatabaseDesign.Ampersand.Core.Poset as Poset hiding (sortWith)
import Prelude hiding (Ord(..))
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Fspec.Plug
import Data.Char
import Data.List (nub,intercalate)
import GHC.Exts (sortWith)

fatal :: Int -> String -> a
fatal = fatalMsg "Fspec.ToFspec.ADL2Plug"


makeGeneratedSqlPlugs :: A_Context
              -> [Expression]  
              -> [Relation]    -- ^ declarations to be saved in generated database plugs. 
              -> [PlugSQL]
makeGeneratedSqlPlugs context totsurs entityRels = gTables
  where
        vsqlplugs = [ (makeUserDefinedSqlPlug context p) | p<-ctxsql context] --REMARK -> no optimization like try2specific, because these plugs are user defined
        gTables = gPlugs1 ++ relPlugs1
        gPlugs1 :: [PlugSQL]
        gPlugs1   = makeEntities entityRels vsqlplugs
        -- all plugs for relations not touched by definedplugs and gPlugs
        relPlugs1 :: [PlugSQL]
        relPlugs1 = [ rel2plug rel totals surjectives --(see rel2plug in Plug.hs)
                   | rel<-entityRels
                   , Inj `notElem` multiplicities rel
                   , Uni `notElem` multiplicities rel]
          where
            totals      = [ makeDeclaration r |       ERel r@Rel{} _    <- totsurs]
            surjectives = [ makeDeclaration r | EFlp (ERel r@Rel{} _) _ <- totsurs]



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
-- a relation r (or r~) is stored in the trgFld of this plug


-- | Make a binary sqlplug for a relation that is neither inj nor uni
rel2plug :: Relation -> [Declaration] -> [Declaration] -> PlugSQL
rel2plug r@Rel{} totals surjectives
  | Inj `elem` multiplicities r || Uni `elem` multiplicities r 
    = fatal 55 $ "unexpected call of rel2plug("++show r++"), because it is injective or univalent."
  | otherwise
    = BinSQL { sqlname = name r
             , columns = ( -- The source field:
                           Fld { fldname = srcNm                       
                               , fldexpr = srcExpr
                               , fldtype = makeSqlType (target srcExpr)
                               , flduse  = ForeignKey (target srcExpr)
                               , fldnull = isTot trgExpr
                               , flduniq = isUni trgExpr
                               } 
                         , -- The target field:
                           Fld { fldname = trgNm                       
                               , fldexpr = trgExpr
                               , fldtype = makeSqlType (target trgExpr)
                               , flduse  = ForeignKey (target trgExpr)
                               , fldnull = isSur trgExpr
                               , flduniq = isInj trgExpr
                               }
                          ) 
             , cLkpTbl = [] --in case of TOT or SUR you might use a binary plug to lookup a concept (don't forget to nub)
             , mLkp    = trgExpr
          --   , sqlfpa  = NO
             }
   where
    r_is_Tot = Tot `elem` multiplicities r || makeDeclaration r `elem` totals
    r_is_Sur = Sur `elem` multiplicities r || makeDeclaration r `elem` surjectives
    srcNm = (if isEndo r then "s" else "")++name (source trgExpr)
    trgNm = (if isEndo r then "t" else "")++name (target trgExpr)
    --the expr for the source of r
    srcExpr
     | r_is_Tot = iExpr (source r)
     | r_is_Sur = iExpr (target r)
     | otherwise = let er=ERel r (sign r) in iExpr (source r) ./\. (er .:. flp er)
    --the expr for the target of r
    trgExpr 
     | not r_is_Tot && r_is_Sur = flp (ERel r (sign r))
     | otherwise                = ERel r (sign r)
rel2plug _ _ _ = fatal 77 "Do not call rel2plug on relations other than Rel{}"

-----------------------------------------
--rel2fld
-----------------------------------------
-- Each relation yields one field f1 in the plug...
-- r is the relation from some kernel field k1 to f1
-- (fldexpr k1) is the relation from the plug's imaginary ID to k1
-- (fldexpr k1);r is the relation from ID to f1
-- the rule (fldexpr k1)~;(fldexpr k1);r = r holds because (fldexpr k1) is uni and sur, which means that (fldexpr k1)~;(fldexpr k1) = I
-- REMARK -> r may be tot or sur, but not inj. (fldexpr k1) may be tot.
--
-- fldnull and fldunique are based on the multiplicity of the relation (kernelpath);r from ID to (target r)
-- it is given that ID is unique and not null
-- fldnull=not(isTot (kernelpath);r)
-- flduniq=isInj (kernelpath);r
-- 
-- (kernel++plugAtts) defines the name space, making sure that all fields within a plug have unique names.

-- | Create field for TblSQL or ScalarSQL plugs 
rel2fld :: [Expression] -- ^ all relations (in the form either ERel r or EFlp (ERel r) _) that may be represented as attributes of this entity.
        -> [Expression] -- ^ all relations (in the form either ERel r or EFlp (ERel r) _) that are defined as attributes by the user.
        -> Expression   -- ^ either ERel r or EFlp (ERel r) _, representing the relation from some kernel field k1 to f1
        -> SqlField
rel2fld kernel
        plugAtts
        e
 = Fld { fldname = fldName 
       , fldexpr = e
       , fldtype = makeSqlType (target e)
       , flduse  = Undetermined
       , fldnull = maybenull e
       , flduniq = isInj e      -- all kernel fldexprs are inj
                                -- Therefore, a composition of kernel expr (I;kernelpath;e) will also be inj.
                                -- It is enough to check isInj e
       }
   where 
   fldName = if null [nm | (r',nm)<-table, e==r'] 
             then fatal 117 $ "null names in table for e: " ++ show (e,table)
             else head [nm | (r',nm)<-table, e==r']
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
                   ERel rel _ -> not $
                                 isTot rel && 
                                 (not.null) [()|k<-kernelpaths, target k==source rel && isTot k || target k==target rel && isSur k ]
                   EFlp (ERel rel _) _
                            -> not $ 
                                 isSur rel &&
                                 (not.null) [()|k<-kernelpaths, target k==source rel && isSur k || target k==target rel && isTot k]
                   _ -> fatal 152 ("Illegal Plug Expression: "++show expr ++"\n"++
                                   " ***kernel:*** \n   "++
                                   intercalate "\n   " (map show kernel)++"\n"++
                                   " ***Attributes:*** \n   "++
                                   intercalate "\n   " (map show plugAtts)++"\n"++
                                   " ***e:*** \n   "++
                                   ( show e)
                                  )
                                   
                                   
   kernelpaths = clos kernel
    where
     -- Warshall's transitive closure algorithm, adapted for this purpose:
     clos :: [Expression] -> [Expression]
     clos xs
      = [ foldr1 (.:.) expr | expr<-exprList ]
        where
         exprList :: [[Expression]]
         exprList = foldl f [[x] | x<-nub xs]
                          (nub [c `meet` c' | c<-nub (map source xs), c'<-nub (map target xs), compare c c' `elem` [Poset.EQ,Poset.LT,Poset.GT]])
         f :: [[Expression]] -> A_Concept -> [[Expression]]
         f q x = q ++
                 [ls ++ rs | ls <- q, x <= target (last ls)
                           , rs <- q, x <= source (head rs), null (ls `isc` rs)]
                  
-- ^ Explanation:  rel is a relation from some kernel field k to f
-- ^ (fldexpr k) is the relation from the plug's ID to k
-- ^ (fldexpr k);rel is the relation from ID to f

-----------------------------------------
--makeEntities  (formerly called: makeTblPlugs)
-----------------------------------------
{- makeEntities computes a set of plugs to obtain wide tables with little redundancy.
   It computes entities with their attributes.
   It is based on the principle that each concept is represented in at most one plug,
   and each relation in at most one plug.
   First, we determine the kernels for all plugs.
   A kernel contains the concept table(s) for all concepts that are administered in the same entity.
   For that, we collect all relations that are univalent, injective, and surjective (the kernel relations).
   By the way, that includes all isa-relations, since they are univalent, injective, and surjective by definition.
   If two concepts a and b are in the same entity, there is a concept g such that a isa g and b isa g.
   Of all concepts in an entity, one most generic concept is designated as root.
   Secondly, we take all univalent relations that are not in the kernel, but depart from this kernel.
   These relations serve as attributes. Code:  [a| a<-attRels, source a `elem` concs kernel]
   Then, all these relations are made into fields. Code: plugFields = [rel2fld plugMors a| a<-plugMors]
   We also define two lookup tables, one for the concepts that are stored in the kernel, and one for the attributes of these concepts.
   For the fun of it, we sort the plugs on length, the longest first. Code:   sortWith ((0-).length.fields)
   By the way, parameter allRels contains all relations that are declared in context, enriched with extra multiplicities.
   This parameter allRels was added to makePlugs to avoid recomputation of the extra multiplicities.
   The parameter exclusions was added in order to exclude certain concepts and relations from the process.
-}
-- | Generate non-binary sqlplugs for relations that are at least inj or uni, but not already in some user defined sqlplug
makeEntities :: ConceptStructure a => [Relation] -> [a] -> [PlugSQL]
makeEntities allRels exclusions
 = {- The following may be useful for debugging: 
   error 
    ("\nallRels:"++concat ["\n  "++show r | r<-allRels]++
     "\nrels:"++concat ["\n  "++show r | r<-rels]++
     "\nunis:"++concat ["\n  "++show r | r<-unis]++
     "\nkernelSurRels:"++concat ["\n  "++show e | e<-kernelSurRels]++
     "\nkernelTotRels:"++concat ["\n  "++show e | e<-kernelTotRels]++
     "\nattRels:"++concat ["\n  "++show e | e<-attRels]++
     "\nkernels:"++concat ["\n  "++show kernel | kernel<-kernels]++
     "\nmainkernels:"++concat ["\n  "++show [head cl |cl<-eqCl target kernel] | kernel<-kernels]
    ) ++ -}
   sortWith ((0-).length.plugFields)
    [ if and [isIdent r |(r,_,_)<-attributeLookuptable] && length conceptLookuptable==1  
      then --the TblSQL could be a scalar tabel, which is a table that only stores the identity of one concept
           let r = iExpr c 
           in ScalarSQL { sqlname   = name c
                        , sqlColumn = rel2fld [r] [] r
                        , cLkp      = c 
                        }
      else
      TblSQL { sqlname = name c
             , fields  = plugFields
             , cLkpTbl = conceptLookuptable
             , mLkpTbl = attributeLookuptable
             }
             
    | kernel<-kernels
    , let mainkernel = [head cl |cl<-eqCl target kernel] -- the part of the kernel for concept lookups (cLkpTbl) and linking rels to (mLkpTbl)
                                                         -- note that eqCl guarantees that cl is not empty.
    {- Examples of mainkernel:
            [ERel I[A] [A*A],EFlp (ERel ISA[D*A] [D*A]) [A*D]]
            [ERel I[B] [B*B],EFlp (ERel ISA[D*B] [D*B]) [B*D]]
            [ERel I[X] [X*X]]
            [ERel I[Y] [Y*Y]]  -}
          restkernel = kernel >- mainkernel --the complement of mainkernel
          c = if null mainkernel
              then fatal 198 "null mainkernel. Is this kernel empty???"
              else target (head mainkernel)       -- one concept from the kernel is designated to "lead" this plug.
          plugAtts              = [a | a <-attRels, source a `elem` concs mainkernel] --plugAtts link directly to some kernelfield
          plugMors              = mainkernel++restkernel++plugAtts --all relations for which the target is stored in the plug
          plugFields            = [fld a | a<-plugMors]      -- Each field comes from a relation.
          conceptLookuptable :: [(A_Concept,SqlField)]
          conceptLookuptable    = [(target r,fld r) |r<-mainkernel]
          attributeLookuptable :: [(Expression,SqlField,SqlField)]
          attributeLookuptable  = -- kernel attributes are always surjective from left to right. So do not flip the lookup table!
                                  [(e,lookupC (source e),fld e) | e<-plugMors] 
          lookupC cpt           = if null [f |(c',f)<-conceptLookuptable, cpt==c'] 
                                  then fatal 209 "null cLkptable."
                                  else head [f |(c',f)<-conceptLookuptable, cpt==c']
          fld a                 = rel2fld mainkernel (restkernel++plugAtts) a
    ]
   where   
-- The first step is to determine which entities to generate.
-- All concepts and relations mentioned in exclusions are excluded from the process.
    rels,unis :: [Relation]
    rels = [rel | rel <- allRels>-mors exclusions, not (isIdent rel)]
    unis = [r | r<-rels, isUni r, isInj r]
    kernelSurRels :: [Relation]
    kernelSurRels   = [ r | r<-unis, isSur r]
    kernelTotRels :: [Relation]
    kernelTotRels   =  [r | r<-unis, not (isSur r), isTot r]
-- attRels contains all relations that will be attribute of a kernel.
-- The type is the largest possible type, which is the declared type, because that contains all atoms (also the atoms of subtypes) needed in the operation.
    attRels :: [Expression]
    attRels      = [     ERel r (sign (makeDeclaration r))  | r<-rs,      isUni r] ++
                   [flp (ERel r (sign (makeDeclaration r))) | r<-rs, not (isUni r), isInj r]
                   where rs = rels>-(kernelSurRels++kernelTotRels)
    kernels :: [[Expression]]
    kernels
     = case [c | c@C{} <- concs allRels] of
         []  -> [] -- an Ampersand script without declarations is conceivable. In that case cptgE is undefined for lack of concepts.
         c:_ -> kerns++[ [iExpr c] ++
                         [ rs | rs<-otherFields, source rs==c, target rs/=c]
                       | c<-concs rels, not (c `elem` (map target (concat kerns)))
                       ]
       where
       -- One kernel starts with a set of concepts, which were put into the same class by the type checker. Here, it is called an island of concepts.
       -- The first concept of that class is the most generic of the lot. The concepts of one island are placed in the kernel in the same order.
       -- That order reflects the size of the population. A subset (i.e. a more specific concept) is placed more to the right. This is for no reason in particular.  
         (_,islands,_,_,_) = cptgE (head [c | c@C{} <- concs allRels])
         kerns :: [[Expression]]
         kerns =  [ [iExpr c | c<-island ] ++
                    [ rs | rs<-otherFields, source rs `elem` island, target rs `notElem` island]
                  | island<-islands ]
         otherFields :: [Expression]
         otherFields = [case head (sortWith length cl) of [e] -> e; es -> foldr1 (.:.) es | cl<-eqCl (\rs->(src rs,trg rs)) closure ]
         closure :: [[Expression]]
       -- The kernel relations whose source and targets are already contained in an island, need not be taken into account.
-- In order to make kernels as large as possible,
-- all relations that are univalent and injective will be flipped if that makes them surjective.
         closure = clos1 ([ [ERel r (sign r)] | r<-kernelSurRels]++[ [flp (ERel r (sign r))] | r<-kernelTotRels])
         clos1 :: [[Expression]] -> [[Expression]]
         clos1 xs
            = foldl f xs (nub (map src xs) `isc` nub (map trg xs))
              where
               f q x = q `uni` [r++r' | r<-q, trg r==x, r'<-q, src r'== x]
         src = source.head
         trg = target.last
    {- Kernels are built recursively. Kernels expand by adding (sur, uni and inj) relations until there are none left.
       Step 1: compute the expansion of each kernel (code: ms++[r |r<-rs, source r `elem` concs ms])
       Step 2: merge kernels if possible (code: recursion over oneRun)
       Step 3: compute the remaining relations (code: [r | r<-rs, source r `notElem` concs [ms | (_,ms)<-kernels]] )
       And call recursively until there are none left. -}


-----------------------------------------
--makeUserDefinedSqlPlug
-----------------------------------------
--makeUserDefinedSqlPlug is used to make user defined plugs. One advantage is that the field names and types can be controlled by the user. 
--
--TODO151210 -> (see also Instance Object PlugSQL)
--              cLkpTbl TblSQL{} can have more than one concept i.e. one for each kernel field
--              a kernel may have more than one concept that is uni,tot,inj,sur with some imaginary ID of the plug (i.e. fldnull=False)
--              When is an ObjectDef a ScalarPlug or BinPlug?
--              When do you want to define your own Scalar or BinPlug
--rel2fld  (keyDefs context) kernel plugAtts r

-- | Make a sqlplug from an ObjectDef (user-defined sql plug)
makeUserDefinedSqlPlug :: A_Context -> ObjectDef -> PlugSQL
makeUserDefinedSqlPlug _ obj
 | null(objatsLegacy obj) && isIdent(objctx obj)
    = ScalarSQL { sqlname   = name obj
                , sqlColumn = rel2fld [iExpr c] [] (iExpr c)
                , cLkp      = c
                } 
 | null(objatsLegacy obj) --TODO151210 -> assuming objctx obj is Rel{} if it is not I{}
   = fatal 2372 "TODO151210 -> implement defining binary plugs in ASCII"
 | isIdent(objctx obj) --TODO151210 -> a kernel may have more than one concept that is uni,tot,inj,sur with some imaginary ID of the plug
   = {- error 
    ("\nc: "++show c++
     "\nrels:"++concat ["\n  "++show r | r<-rels]++
     "\nkernel:"++concat ["\n  "++show r | r<-kernel]++
     "\nattRels:"++concat ["\n  "++show e | e<-attRels]++
     "\nplugFields:"++concat ["\n  "++show plugField | plugField<-plugFields]
    ) -}
     TblSQL { sqlname = name obj
            , fields  = plugFields
            , cLkpTbl = conceptLookuptable     
            , mLkpTbl = attributeLookuptable
            }   
 | otherwise = fatal 279 "Implementation expects one concept for plug object (SQLPLUG tblX: I[Concept])."
  where       
   c   -- one concept from the kernel is designated to "lead" this plug, this is user-defined.
     = source(objctx obj) 
   rels --fields are user-defined as one deep objats with objctx=r. note: type incorrect or non-relation objats are ignored
     = [(objctx att,sqltp att) | att<-objatsLegacy obj, source (objctx att)==c]   
   kernel --I[c] and every non-endo r or r~ which is at least uni,inj,sur are kernel fields 
          --REMARK -> endo r or r~ which are at least uni,inj,sur are inefficient in a way
          --          if also TOT than r=I => duplicates, 
          --          otherwise if r would be implemented as GEN (target r) ISA C then (target r) could become a kernel field
     = [(iExpr c,sqltp obj)] 
       ++ [(r,tp) |(r,tp)<-rels,not (isEndo r),isUni r, isInj r, isSur r]
       ++ [(r,tp) |(r,tp)<-rels,not (isEndo r),isUni r, isInj r, isTot r, not (isSur r)]
   attRels --all user-defined non-kernel fields are attributes of (rel2fld (objctx c))
     = (rels >- kernel) >- [(flp r,tp) |(r,tp)<-kernel] --note: r<-rels where r=objctx obj are ignored (objctx obj=I)
   plugMors              = kernel++attRels
   plugFields            = [fld r tp | (r,tp)<-plugMors] 
   fld r tp              = (rel2fld (map fst kernel) (map fst attRels) r){fldtype=tp} --redefine sqltype
   conceptLookuptable    = [(target e,fld e tp) |(e,tp)<-kernel]
   attributeLookuptable  = [(er,lookupC (source er),fld er tp) | (er,tp)<-plugMors]
   lookupC cpt           = if null [f |(c',f)<-conceptLookuptable, cpt==c'] 
                           then fatal 300 "null cLkptable."
                           else head [f |(c',f)<-conceptLookuptable, cpt==c']
   sqltp :: ObjectDef -> SqlType
   sqltp att = head $ [makeSqltype' sqltp' | strs<-objstrs att,('S':'Q':'L':'T':'Y':'P':'E':'=':sqltp')<-strs]
                      ++[SQLVarchar 255]

makeSqlType :: A_Concept -> SqlType
makeSqlType ONE = SQLBool -- TODO (SJ):  Martijn, why should ONE have a representation? Or should this rather be a fatal?
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
