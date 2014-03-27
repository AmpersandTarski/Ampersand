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
import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec.ShowHS --for debugging
import Data.Maybe
import Data.Char
import Data.List (nub,intercalate,intersect,partition,group,delete)
import GHC.Exts (sortWith)
import Debug.Trace

fatal :: Int -> String -> a
fatal = fatalMsg "Fspec.ToFspec.ADL2Plug"


makeGeneratedSqlPlugs :: Options
              -> A_Context
              -> [Expression]  
              -> [Declaration]    -- ^ relations to be saved in generated database plugs. 
              -> [PlugSQL]
makeGeneratedSqlPlugs flags context totsurs entityDcls = gTables
  where
        vsqlplugs = [ (makeUserDefinedSqlPlug context p) | p<-ctxsql context] --REMARK -> no optimization like try2specific, because these plugs are user defined
        gTables = gPlugs ++ gLinkTables
        gPlugs :: [PlugSQL]
        gPlugs   = --trace "---\nStart makeEntityTables " $
                   --trace ("with "++show (length entityDcls)++" relations and "++show(length vsqlplugs)++" userdefined plugs.\n")
                         (makeEntityTables flags context entityDcls (gens context) (ctxgenconcs context) (relsUsedIn vsqlplugs))
        -- all plugs for relations not touched by definedplugs and gPlugs
        gLinkTables :: [PlugSQL]
        gLinkTables = [ makeLinkTable context dcl totsurs
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
makeLinkTable :: A_Context -> Declaration -> [Expression] -> PlugSQL
makeLinkTable context dcl totsurs = 
  case dcl of
    Sgn{} 
     | isInj dcl || isUni dcl 
        -> fatal 55 $ "unexpected call of makeLinkTable("++show dcl++"), because it is injective or univalent."
     | otherwise 
        -> BinSQL
             { sqlname = name dcl
             , columns = ( -- The source field:
                           Fld { fldname = concat["Src" | isEndo dcl]++name (source trgExpr)                       
                               , fldexpr = srcExpr
                               , fldtype = sqlTypeOf context (target srcExpr)
                               , flduse  = ForeignKey (target srcExpr)
                               , fldnull = isTot trgExpr
                               , flduniq = isUni trgExpr
                               } 
                         , -- The target field:
                           Fld { fldname = concat["Tgt" | isEndo dcl]++name (target trgExpr)                       
                               , fldexpr = trgExpr
                               , fldtype = sqlTypeOf context (target trgExpr)
                               , flduse  = ForeignKey (target trgExpr)
                               , fldnull = isSur trgExpr
                               , flduniq = isInj trgExpr
                               }
                          ) 
             , cLkpTbl = [] --in case of TOT or SUR you might use a binary plug to lookup a concept (don't forget to nub)
             , mLkp    = trgExpr
          --   , sqlfpa  = NO
             }
    _  -> fatal 90 "Do not call makeLinkTable on relations other than Sgn{}"
   where
    r_is_Tot = isTot dcl || dcl `elem` [ d |       EDcD d  <- totsurs]
    r_is_Sur = isSur dcl || dcl `elem` [ d | EFlp (EDcD d) <- totsurs]
    --the expr for the source of r
    srcExpr
     | r_is_Tot = EDcI (source dcl)
     | r_is_Sur = EDcI (target dcl)
     | otherwise = let er=EDcD dcl in EDcI (source dcl) ./\. (er .:. flp er)
    --the expr for the target of r
    trgExpr 
     | not r_is_Tot && r_is_Sur = flp (EDcD dcl)
     | otherwise                = EDcD dcl

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
rel2fld :: A_Context
        -> [Expression] -- ^ all relations (in the form either EDcD r, EDcI or EFlp (EDcD r)) that may be represented as attributes of this entity.
        -> [Expression] -- ^ all relations (in the form either EDcD r or EFlp (EDcD r)) that are defined as attributes by the user.
        -> Expression   -- ^ either EDcD r, EDcI c or EFlp (EDcD r), representing the relation from some kernel field k1 to f1
        -> SqlField
rel2fld context
        kernel
        plugAtts
        e
 = Fld { fldname = fldName 
       , fldexpr = e
       , fldtype = sqlTypeOf context (target e)
       , flduse  =  
          let f expr =
                 case expr of
                    EDcI c   -> PrimKey c
                    EDcD _   -> PlainAttr
                    EFlp e'  -> f e'
                    _        -> fatal 144 ("No flduse defined for "++show expr)
          in f e
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
       niceidname (EFlp x) = niceidname x
       niceidname (EDcD d) = name d
       niceidname (EDcI c) = name c
       niceidname rel      = fatal 162 ( "Unexpected relation found:\n"++
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
       = fatal 146 $"more than one kernel field for the same concept:\n    expr = " ++(show expr)++
           intercalate "\n  *** " ( "" : (map (name.target) kernel)) 
    | otherwise = case expr of
                   EDcD dcl
                        | (not.isTot) dcl -> True
                        | otherwise -> (not.null) [()|k<-kernelpaths, target k==source dcl && isTot k || target k==target dcl && isSur k ]
                   EFlp (EDcD dcl)
                        | (not.isSur) dcl -> True
                        | otherwise -> (not.null) [()|k<-kernelpaths, target k==source dcl && isSur k || target k==target dcl && isTot k]
                   EDcI _ -> False
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
      -- SJ 20131117. The following code (exprList and f) assumes no ISA's in the A-structure. Therefore, this works due to the introduction of EEps.
         exprList = foldl f [[x] | x<-nub xs]
                          (nub [c | c<-nub (map source xs), c'<-nub (map target xs), c==c'])
         f :: [[Expression]] -> A_Concept -> [[Expression]]
         f q x = q ++ [ls ++ rs | ls <- q, x == target (last ls)
                                , rs <- q, x == source (head rs), null (ls `isc` rs)]
                  
-- ^ Explanation:  rel is a relation from some kernel field k to f
-- ^ (fldexpr k) is the relation from the plug's ID to k
-- ^ (fldexpr k);rel is the relation from ID to f

-----------------------------------------
--makeEntityTables  (formerly called: makeTblPlugs)
-----------------------------------------
{- makeEntityTables computes a set of plugs to obtain tables in a transactional database with minimal redundancy.
   We call them "wide tables".
   makeEntityTables computes entities with their attributes.
   It is based on the principle that each concept is represented in at most one plug,
   and each relation in at most one plug.
   First, we determine the kernels for all plugs.
   A kernel contains the concept table(s) for all concepts that are administered in the same entity.
   For that, we collect all relations that are univalent, injective, and surjective (the kernel relations).
      By the way, that includes all isa-relations, since they are univalent, injective, and surjective by definition.
      Since isa-relations are not declared explicitly, they are generated separately.
   If two concepts a and b are in the same entity, there is a concept g such that a isa g and b isa g.
   Of all concepts in an entity, one most generic concept is designated as root, and is positioned in the first column of the table.
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
makeEntityTables :: Options 
                -> A_Context
                -> [Declaration] -- ^ all relations in scope
                -> [A_Gen]
                -> [[A_Concept]] -- ^ concepts `belonging' together.
                                 --   for each class<-conceptss: c,c'<-class:   c `join` c' exists (although there is not necessarily a concept d=c `join` c'    ...)
                -> [Declaration] -- ^ relations that should be excluded, because they wil not be implemented using generated sql plugs. 
                -> [PlugSQL]
makeEntityTables flags context allDcls isas conceptss exclusions
 = sortWith ((0-).length.plugFields)
    (map kernel2Plug kernelsWithAttributes)
   where
    diagnostics
      = "\nallDcls:" ++     concat ["\n  "++showHSName r           | r<-allDcls]++
        "\nallDcls:" ++     concat ["\n  "++showHS flags "\n  " r  | r<-allDcls]++
        "\nconceptss:" ++   concat ["\n  "++showHS flags "    " cs | cs<-conceptss]++
        "\nexclusions:" ++  concat ["\n  "++showHSName r           | r<-exclusions]++
        "\nattRels:" ++     concat ["\n  "++showHS flags "    " e  | e<-attRels]++
        "\n"
-- | kernels are computed, starting with the set of concepts, on the basis of generalization tuples.
    kernPartition :: [A_Gen] -> [[A_Concept]] -- ^ This function contains the recipe to derive a set of kernels from a set of isa-pairs.
    kernPartition specialzs
     = foldl f (group (delete ONE (concs context))) specialzs
       where f disjuncLists g = concat haves : nohaves
               where
                 (haves,nohaves) = partition (not.null.intersect (concs g)) disjuncLists
    preKernels = kernPartition (gens context) -- ^ Step 1: compute the kernels from the isa-pairs from the context
    extraIsas  -- Step 2: Maybe extra isa-pairs are needed to ensure that each kernel has precisely one largest concept
       = concat
         [ case [c | c<-kernel, null (largerConcepts isas c)] of -- determine how many concepts in one kernel are largest
             [_] -> []
             rs  -> [ Isa{gengen=rootConcept, genspc=c} | c<-rs ]
         | (rootConcept,kernel) <- zip [rc | i<-[0::Int ..]
                                           , let rc=PlainConcept ("rootConcept"++show i)
                                           , rc `notElem` concs context ]
                                       preKernels
         ]
    kernels     -- Step 3: compute the kernels
     = [ largerCs++[ c | c<-kernel, c `notElem` largerCs ]              -- put the largest element up front
       | kernel <- kernPartition (extraIsas++gens context)                -- recompute the kernels with the extra isa-pairs.
       , let largerCs = [c | c<-kernel, null (largerConcepts isas c)]   -- get the set of largest concepts (each kernel has precisely one)
       ]
    kernelsWithAttributes = dist attRels kernels []
      where 
        dist :: (Association attrib, Show attrib) => [attrib] -> [[A_Concept]] -> [([A_Concept], [attrib])] -> [([A_Concept], [attrib])]
        dist []   []     result = result
        dist atts []     _      = fatal 246 ("No kernel found for atts: "++show atts++"\n"++diagnostics)
        dist atts (kernel:ks) result = dist otherAtts ks ([(kernel,attsOfK)] ++ result)
           where (attsOfK,otherAtts) = partition belongsInK atts
                 belongsInK att = source att `elem` kernel
    -- | converts a kernel into a plug
    kernel2Plug :: ([A_Concept],[Expression]) -> PlugSQL
    kernel2Plug (kernel, attsAndIsaRels)
     =  TblSQL 
             { sqlname = name (head kernel) -- ++ " !!Let op: De ISA relaties zie ik hier nergens terug!! (TODO. HJO 20131201"
             , fields  = map fld plugMors      -- Each field comes from a relation.
             , cLkpTbl = conceptLookuptable
             , mLkpTbl = attributeLookuptable ++ isaLookuptable
             } 
        where
          (isaAtts,atts) = partition isISA attsAndIsaRels
            where -- isISA (EDcD r) = decISA r
                  isISA (EDcI _) = True
                  isISA _        = False
          mainkernel = map EDcI kernel
          plugMors :: [Expression]
          plugMors = mainkernel++atts 
          conceptLookuptable :: [(A_Concept,SqlField)]
          conceptLookuptable    = [(target r,fld r) | r <-mainkernel]
          attributeLookuptable :: [(Expression,SqlField,SqlField)]
          attributeLookuptable  = -- kernel attributes are always surjective from left to right. So do not flip the lookup table!
                                  [(e,lookupC (source e),fld e) | e <-plugMors] 
          lookupC cpt           = head [f |(c',f)<-conceptLookuptable, cpt==c']
          fld a                 = rel2fld context mainkernel atts a
          isaLookuptable = [(e,lookupC (source e),lookupC (target e)) | e <- isaAtts ]    
-- attRels contains all relations that will be attribute of a kernel.
-- The type is the largest possible type, which is the declared type, because that contains all atoms (also the atoms of subtypes) needed in the operation.
    attRels :: [Expression]
    attRels = mapMaybe attExprOf (allDcls>- exclusions)
     where
       attExprOf :: Declaration -> Maybe Expression
       attExprOf d =
        case d of  --make explicit what happens with each possible decl...
          Isn{} -> Nothing -- These relations are already in the kernel
          Vs{}  -> Nothing -- Vs are not implemented at all
          Sgn{} ->
               case (isInj d, isUni d, isTot d, isSur d) of
                    (False  , False  , _      , _      ) --Will become a link-table
                        -> Nothing 
                    (True   , False  , _      , _      )
                        -> Just $ flp (EDcD d)
                    (True   , True   , True   , False  ) --Equivalent to CLASSIFY s ISA t, however, it is named, so it must be stored in a plug!
                        -> Just $ flp (EDcD d)
                    _   -> Just $      EDcD d

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
makeUserDefinedSqlPlug context obj
 | null(attributes obj) && isIdent(objctx obj)
    = ScalarSQL { sqlname   = name obj
                , sqlColumn = rel2fld context [EDcI c] [] (EDcI c)
                , cLkp      = c
                } 
 | null(attributes obj) --TODO151210 -> assuming objctx obj is Rel{} if it is not I{}
   = fatal 2372 "TODO151210 -> implement defining binary plugs in ASCII"
 | isIdent(objctx obj) --TODO151210 -> a kernel may have more than one concept that is uni,tot,inj,sur with some imaginary ID of the plug
   = {- The following may be useful for debugging:
     error 
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
     = [(objctx att,sqltp att) | att<-attributes obj, source (objctx att)==c]   
   kernel --I[c] and every non-endo r or r~ which is at least uni,inj,sur are kernel fields 
          --REMARK -> endo r or r~ which are at least uni,inj,sur are inefficient in a way
          --          if also TOT than r=I => duplicates, 
          --          otherwise if r would be implemented as GEN (target r) ISA C then (target r) could become a kernel field
     = [(EDcI c,sqltp obj)] 
       ++ [(r,tp) |(r,tp)<-rels,not (isEndo r),isUni r, isInj r, isSur r]
       ++ [(r,tp) |(r,tp)<-rels,not (isEndo r),isUni r, isInj r, isTot r, not (isSur r)]
   attRels --all user-defined non-kernel fields are attributes of (rel2fld context (objctx c))
     = (rels >- kernel) >- [(flp r,tp) |(r,tp)<-kernel] --note: r<-rels where r=objctx obj are ignored (objctx obj=I)
   plugMors              = kernel++attRels
   plugfields            = [fld r tp | (r,tp)<-plugMors] 
   fld r tp              = (rel2fld context (map fst kernel) (map fst attRels) r){fldtype=tp}  --redefine sqltype
   conceptLookuptable    = [(target e,fld e tp) |(e,tp)<-kernel]
   attributeLookuptable  = [(er,lookupC (source er),fld er tp) | (er,tp)<-plugMors]
   lookupC cpt           = head [f |(c',f)<-conceptLookuptable, cpt==c']
   sqltp :: ObjectDef -> SqlType
   sqltp att = head $ [sqlTypeOf' sqltp' | strs<-objstrs att,('S':'Q':'L':'T':'Y':'P':'E':'=':sqltp')<-strs]
                      ++[SQLVarchar 255]

sqlTypeOf :: A_Context -> A_Concept -> SqlType
sqlTypeOf _ ONE = SQLBool -- TODO (SJ):  Martijn, why should ONE have a representation? Or should this rather be a fatal?
sqlTypeOf context c
    = case nub [ cdtyp cdef | cdef<-ctxcds context, name c==name cdef ] of
       [str] -> sqlTypeOf' str
       []    -> sqlTypeOf' ""
       _     -> fatal 396 ("Multiple SQL types defined for concept "++name c)
       
sqlTypeOf' :: String -> SqlType
sqlTypeOf' str = case str of
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
