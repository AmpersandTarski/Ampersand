{-# OPTIONS_GHC -Wall #-}  
module DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Plug 
  (makeGeneratedSqlPlugs
  ,makeUserDefinedSqlPlug 
  )
where
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree hiding (sortWith)
import DatabaseDesign.Ampersand.Core.Poset as Poset hiding (sortWith)
import Prelude hiding (Ord(..),head)
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Fspec.Plug
import DatabaseDesign.Ampersand.Misc
-- import DatabaseDesign.Ampersand.Fspec.ShowHS --for debugging
import Data.Maybe
import Data.Char
import Data.List (nub,intercalate,partition)
import GHC.Exts (sortWith)

fatal :: Int -> String -> a
fatal = fatalMsg "Fspec.ToFspec.ADL2Plug"

head :: Int -> [a] -> a
head i [] = fatal i "head must not be used on an empty list!"
head _ (a:_) = a


makeGeneratedSqlPlugs :: Options
              -> A_Context
              -> [Expression]  
              -> [Declaration]    -- ^ declarations to be saved in generated database plugs. 
              -> [PlugSQL]
makeGeneratedSqlPlugs flags context totsurs entityDcls = gTables
  where
        vsqlplugs = [ (makeUserDefinedSqlPlug context p) | p<-ctxsql context] --REMARK -> no optimization like try2specific, because these plugs are user defined
        gTables = gPlugs ++ gLinkTables
        gPlugs :: [PlugSQL]
        gPlugs   = makeEntityTables flags entityDcls vsqlplugs
        -- all plugs for relations not touched by definedplugs and gPlugs
        gLinkTables :: [PlugSQL]
        gLinkTables = [ makeLinkTable dcl totsurs
                      | dcl<-entityDcls
                      , Inj `notElem` multiplicities dcl
                      , Uni `notElem` multiplicities dcl]


-----------------------------------------
--makeLinkTable
-----------------------------------------
-- makeLinkTable creates associations (BinSQL) between plugs that represent wide tables.
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
makeLinkTable :: Declaration -> [Expression] -> PlugSQL
makeLinkTable dcl totsurs = 
  case dcl of
    Sgn{} 
     | Inj `elem` multiplicities dcl || Uni `elem` multiplicities dcl 
        -> fatal 55 $ "unexpected call of makeLinkTable("++show dcl++"), because it is injective or univalent."
     | otherwise 
        -> BinSQL
             { sqlname = name dcl
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
    _  -> fatal 90 "Do not call makeLinkTable on relations other than Rel{}"
   where
    r_is_Tot = Tot `elem` multiplicities dcl || dcl `elem` [ d |       EDcD d _    <- totsurs]
    r_is_Sur = Sur `elem` multiplicities dcl || dcl `elem` [ d | EFlp (EDcD d _) _ <- totsurs]
    srcNm = (if isEndo dcl then "s" else "")++name (source trgExpr)
    trgNm = (if isEndo dcl then "t" else "")++name (target trgExpr)
    --the expr for the source of r
    srcExpr
     | r_is_Tot = iExpr (source dcl)
     | r_is_Sur = iExpr (target dcl)
     | otherwise = let er=EDcD dcl (sign dcl) in iExpr (source dcl) ./\. (er .:. flp er)
    --the expr for the target of r
    trgExpr 
     | not r_is_Tot && r_is_Sur = flp (EDcD dcl (sign dcl))
     | otherwise                = EDcD dcl (sign dcl)

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
rel2fld :: [Expression] -- ^ all relations (in the form either EDcD r, EDcI or EFlp (EDcD r) _) that may be represented as attributes of this entity.
        -> [Expression] -- ^ all relations (in the form either EDcD r or EFlp (EDcD r) _) that are defined as attributes by the user.
        -> Expression   -- ^ either EDcD r, EDcI or EFlp (ERel r) _, representing the relation from some kernel field k1 to f1
        -> SqlFieldUsage
        -> SqlField
rel2fld kernel
        plugAtts
        e
        usage
 = Fld { fldname = fldName 
       , fldexpr = e
       , fldtype = makeSqlType (target e)
       , flduse  = usage
       , fldnull = maybenull e
       , flduniq = isInj e      -- all kernel fldexprs are inj
                                -- Therefore, a composition of kernel expr (I;kernelpath;e) will also be inj.
                                -- It is enough to check isInj e
       }
   where 
   fldName = case [nm | (r',nm)<-table, e==r'] of 
               []   -> fatal 117 $ "null names in table for e: " ++ show (e,table)
               n:_  -> n
     where 
       table :: [(Expression, String)]
       table   = [ entry
                 | cl<-eqCl (map toLower.niceidname) (kernel++plugAtts)
                 , entry<-if length cl==1 then [(rel,niceidname rel) |rel<-cl] else tbl cl]
       tbl rs  = [ entry
                 | cl<-eqCl (map toLower.name.source) rs
                 , entry<-if length cl==1
                          then [(rel,niceidname rel++name (source rel)) |rel<-cl]
                          else [(rel,niceidname rel++show i)|(rel,i)<-zip cl [(0::Int)..]]]
       niceidname (EFlp x _ ) = niceidname x
       niceidname (EDcD d _ ) = name d
       niceidname (EDcI  sgn) = name (target sgn)
       niceidname (ETyp (EDcI _) sgn) = name (target sgn) -- this occurs in the kernel of plugs, for concepts that are a specialization.
       niceidname rel         = fatal 162 ( "Unexpected relation found:\n"++
                                           intercalate "\n  "
                                           [ "***rel:"
                                           , show rel
                                           , "***kernel:"
                                           , show kernel
                                           , "***plugAtts:"
                                           , show plugAtts
                                           ]
                                       )
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
                   EDcD dcl _ -> not $
                                 isTot dcl && 
                                 (not.null) [()|k<-kernelpaths, target k==source dcl && isTot k || target k==target dcl && isSur k ]
                   EFlp (EDcD dcl _) _
                            -> not $ 
                                 isSur dcl &&
                                 (not.null) [()|k<-kernelpaths, target k==source dcl && isSur k || target k==target dcl && isTot k]
                   EDcI   sgn -> source sgn /= target sgn
                   ETyp (EDcI  _) _
                            -> True
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
                           , rs <- q, x <= source ((head 206)rs), null (ls `isc` rs)]
                  
-- ^ Explanation:  rel is a relation from some kernel field k to f
-- ^ (fldexpr k) is the relation from the plug's ID to k
-- ^ (fldexpr k);rel is the relation from ID to f

-----------------------------------------
--makeEntityTables  (formerly called: makeTblPlugs)
-----------------------------------------
{- makeEntityTables computes a set of plugs to obtain wide tables with little redundancy.
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
makeEntityTables :: Options -> ConceptStructure a => [Declaration] -> [a] -> [PlugSQL]
makeEntityTables _ {-flags-} allDcls exclusions
 = {- The following may be useful for debugging:
   error 
    ("\nallRels:"++concat ["\n  "++show r | r<-allDcls]++
     "\nrels:"++concat ["\n  "++show r++(show.multiplicities) r  | r<-[rel | rel <- allDcls>-declsUsedIn exclusions, not (isIdent rel)]]++
     "\nattRels:"++concat ["\n  "++show e | e<-attRels]++
     "\nkernels:"++concat ["\n  "++show kernel ++show (nub (map target kernel))| kernel<-kernels]++
     "\nmainkernels:"++concat ["\n  "++show [(head 256) cl |cl<-eqCl target kernel] | kernel<-kernels]++
  --   "\nplugs:"++concat ["\n   "++showHS flags "\n   " (kernel2Plug kernel) | kernel <-kernels]++

     "\nDistribution of Attributes:"++ concat ["\n   "++show dist | dist <-distributionOfAtts]++
     "") ++  -}
   sortWith ((0-).length.plugFields)
    (map kernel2Plug distributionOfAtts)
   where
    distributionOfAtts = dist attRels kernels []
      where 
   --     dist :: [attrib] -> [kernel] -> [(kernel,[attrib])] -> [(kernel,[attrib])]
        dist []   []     result = result
        dist _    []     _      = fatal 246 "No kernel found for atts"
        dist atts (k:ks) result = dist rest ks (result ++ [(k,attsOfKernel)])
           where (attsOfKernel,rest) = partition belongsInK atts
                 belongsInK att = source att `elem` map target k
    -- | converts a kernel into a plug
    kernel2Plug :: ([Expression],[Expression]) -> PlugSQL
    kernel2Plug (kernel, atts) =  --TODO: Use atts to build the table.
      if and [isIdent r |(r,_,_)<-attributeLookuptable] && length conceptLookuptable==1  
      then --the TblSQL could be a scalar tabel, which is a table that only stores the identity of one concept
           let r = iExpr c 
           in ScalarSQL { sqlname   = name c
                        , sqlColumn = rel2fld [r] [] r (PrimKey c)
                        , cLkp      = c 
                        }
      else  {- -}
           error   
             ("\n *** kernel:"
           ++ "\n  "++show [(head 256) cl |cl<-eqCl target kernel]
           ++ "\n *** atts:"++concat ["\n  "++show a | a<-atts]
           ++ "\n *** plugAtts:"++concat ["\n  "++show a | a<-plugAtts]
           ++ "\n *** attRels:"++concat ["\n  "++show a | a<-attRels]
           ++ "\n *** mainkernel:"++concat ["\n  "++show a | a<-mainkernel]
             )
--        TblSQL { sqlname = name c
--             , fields  = plugfields
--             , cLkpTbl = conceptLookuptable
--             , mLkpTbl = attributeLookuptable
--             } 
        where
          mainkernel = [(head 276) cl |cl<-eqCl target kernel] -- the part of the kernel for concept lookups (cLkpTbl) and linking rels to (mLkpTbl)
                                                         -- note that eqCl guarantees that cl is not empty.
    {- Examples of mainkernel:
            [ERel I[A] [A*A],EFlp (ERel ISA[D*A] [D*A]) [A*D]]
            [ERel I[B] [B*B],EFlp (ERel ISA[D*B] [D*B]) [B*D]]
            [ERel I[X] [X*X]]
            [ERel I[Y] [Y*Y]]  -}
          restkernel = kernel >- mainkernel --the complement of mainkernel
          c = if null mainkernel
              then fatal 198 "null mainkernel. Is this kernel empty???"
              else target ((head 286) mainkernel)       -- one concept from the kernel is designated to "lead" this plug.
          plugAtts              = [a | a <-attRels, source a `elem` concs mainkernel] --plugAtts link directly to some kernelfield
          -- | all relations for which the target is stored in the plug
          plugMors :: [(Expression, SqlFieldUsage)]
          plugMors              = zip mainkernel (PrimKey c : repeat NonMainKey)
                                ++zip restkernel (repeat FillInLater)
                                ++zip plugAtts   (repeat PlainAttr ) 
          plugfields            = [fld a u | (a,u)<-plugMors]      -- Each field comes from a relation.
          conceptLookuptable :: [(A_Concept,SqlField)]
          conceptLookuptable    = [(target r,fld r u) |(r,u) <-zip mainkernel (PrimKey c : repeat NonMainKey)]
          attributeLookuptable :: [(Expression,SqlField,SqlField)]
          attributeLookuptable  = -- kernel attributes are always surjective from left to right. So do not flip the lookup table!
                                  [(e,lookupC (source e),fld e u) | (e,u) <-plugMors] 
          lookupC cpt           = if null [f |(c',f)<-conceptLookuptable, cpt==c'] 
                                  then fatal 209 "null cLkptable."
                                  else (head 301) [f |(c',f)<-conceptLookuptable, cpt==c']
          fld a u                 = rel2fld mainkernel (restkernel++plugAtts) a u
              
-- The first step is to determine which entities to generate.
-- All concepts and relations mentioned in exclusions are excluded from the process.
    kernels :: [[Expression]]
    kernels = case [c | c@PlainConcept{} <- concs allDcls] of
                [] -> []   -- or maybe:   fatal 286 "empty set of concepts"
                cs -> let (_,islands,_,_,_) = cptgE ((head 309) cs) in
                      [ iExpr root: [ ETyp (iExpr root) (Sign root c) | c<-specifics ]  | (root:specifics)<-islands ]
              
-- attRels contains all relations that will be attribute of a kernel.
-- The type is the largest possible type, which is the declared type, because that contains all atoms (also the atoms of subtypes) needed in the operation.
    attRels :: [Expression]
    attRels = mapMaybe attExprOf (allDcls>-declsUsedIn exclusions)
    attExprOf :: Declaration -> Maybe Expression
    attExprOf d =
     case d of  --make explicit what happens with each possible decl...
       Isn{} -> Nothing -- These relations are already in the kernel
       Vs{}  -> Nothing -- Vs are not implemented at all
       Sgn{} ->
            case (isInj d, isUni d, isSur d, isTot d) of
                 (False  , False  , _      , _      ) --Will become a link-table
                     -> Nothing 
                 (False  , True   , _      , _      )
                     -> Just $      EDcD d (sign d)
                 (True   , False  , _      , _      )
                     -> Just $ flp (EDcD d (sign d))
                 (True   , True   , False  , False  )
                     -> Just $      EDcD d (sign d)
                 (True   , True   , False  , True   ) --Equivalent to SPEC t ISA s
                     -> Just $ ETyp (iExpr s) (Sign s t)
                 (True   , True   , True   , False  ) --Equivalent to SPEC s ISA t
                     -> Just $ ETyp (iExpr t) (Sign t s)
                 (True   , True   , True   , True   )
                     -> fatal 350 "The source and target are synonymous."
              where (s,t) = (source d,target d)   


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
--rel2fld  (identities context) kernel plugAtts r

-- | Make a sqlplug from an ObjectDef (user-defined sql plug)
makeUserDefinedSqlPlug :: A_Context -> ObjectDef -> PlugSQL
makeUserDefinedSqlPlug _ obj
 | null(objatsLegacy obj) && isIdent(objctx obj)
    = ScalarSQL { sqlname   = name obj
                , sqlColumn = rel2fld [iExpr c] [] (iExpr c) (PrimKey c)
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
     "\nplugfields:"++concat ["\n  "++show plugField | plugField<-plugfields]
    ) -}
     TblSQL { sqlname = name obj
            , fields  = plugfields
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
   plugfields            = [fld r tp | (r,tp)<-plugMors] 
   fld r tp              = (rel2fld (map fst kernel) (map fst attRels) r UserDefinedUsage){fldtype=tp}  --redefine sqltype
   conceptLookuptable    = [(target e,fld e tp) |(e,tp)<-kernel]
   attributeLookuptable  = [(er,lookupC (source er),fld er tp) | (er,tp)<-plugMors]
   lookupC cpt           = if null [f |(c',f)<-conceptLookuptable, cpt==c'] 
                           then fatal 300 "null cLkptable."
                           else (head 377) [f |(c',f)<-conceptLookuptable, cpt==c']
   sqltp :: ObjectDef -> SqlType
   sqltp att = (head 379) $ [makeSqltype' sqltp' | strs<-objstrs att,('S':'Q':'L':'T':'Y':'P':'E':'=':sqltp')<-strs]
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
