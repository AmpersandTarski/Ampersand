module Database.Design.Ampersand.FSpec.ToFSpec.ADL2Plug
  (makeGeneratedSqlPlugs
  ,makeUserDefinedSqlPlug
  ,representationOf)
where
import Database.Design.Ampersand.Core.AbstractSyntaxTree hiding (sortWith)
import GHC.Exts (sortWith)
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.FSpec.ShowHS --for debugging
import Data.Maybe
import Data.Char
import Data.List (nub,intercalate,intersect,partition,group,delete)

makeGeneratedSqlPlugs :: Options
              -> A_Context
              -> [Expression]
              -> [Declaration]    -- ^ relations to be saved in generated database plugs.
              -> [PlugSQL]
makeGeneratedSqlPlugs opts context totsurs entityDcls = gTables
  where
        vsqlplugs = [ (makeUserDefinedSqlPlug context p) | p<-ctxsql context] --REMARK -> no optimization like try2specific, because these plugs are user defined
        gTables = gPlugs ++ gLinkTables
        gPlugs :: [PlugSQL]
        gPlugs   = makeEntityTables opts context entityDcls (gens context) (ctxgenconcs context) (relsUsedIn vsqlplugs)
        -- all plugs for relations not touched by definedplugs and gPlugs
        gLinkTables :: [PlugSQL]
        gLinkTables = [ makeLinkTable (contextInfoOf context) dcl totsurs
                      | dcl<-entityDcls
                      , Inj `notElem` properties dcl
                      , Uni `notElem` properties dcl]

-----------------------------------------
--makeLinkTable
-----------------------------------------
-- makeLinkTable creates associations (BinSQL) between plugs that represent wide tables.
-- Typical for BinSQL is that it has exactly two columns that are not unique and may not contain NULL values
--
-- this concerns relations that are not univalent nor injective, i.e. attUniq=False for both columns
-- Univalent relations and injective relations cannot be associations, because they are used as attributes in wide tables.
-- REMARK -> imagine a context with only one univalent relation r::A*B.
--           Then r can be found in a wide table plug (TblSQL) with a list of two columns [I[A],r],
--           and not in a BinSQL with a pair of columns (I/\r;r~, r)
--
-- a relation r (or r~) is stored in the target attribute of this plug

-- | Make a binary sqlplug for a relation that is neither inj nor uni
makeLinkTable :: ContextInfo -> Declaration -> [Expression] -> PlugSQL
makeLinkTable ci dcl totsurs =
  case dcl of
    Sgn{}
     | isInj dcl || isUni dcl
        -> fatal 55 $ "unexpected call of makeLinkTable("++show dcl++"), because it is injective or univalent."
     | otherwise
        -> BinSQL
             { sqlname = unquote . name $ dcl
             , columns = ( -- The source attribute:
                           Att { attName = concat["Src" | isEndo dcl]++(unquote . name . source) trgExpr
                               , attExpr = srcExpr
                               , attType = tType2SqlType . representationOf ci . source $ srcExpr
                               , attUse  = if suitableAsKey . representationOf ci . source $ srcExpr
                                           then ForeignKey (target srcExpr)
                                           else PlainAttr
                               , attNull = isTot trgExpr
                               , attUniq = isUni trgExpr
                               }
                         , -- The target attribute:
                           Att { attName = concat["Tgt" | isEndo dcl]++(unquote . name . target) trgExpr
                               , attExpr = trgExpr
                               , attType = tType2SqlType . representationOf ci . target $ trgExpr
                               , attUse  = if suitableAsKey . representationOf ci . target $ trgExpr
                                           then ForeignKey (target trgExpr)
                                           else PlainAttr
                               , attNull = isSur trgExpr
                               , attUniq = isInj trgExpr
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
unquote :: String -> String
unquote str 
  | length str Prelude.< 2 = str
  | head str == '"' && last str == '"' = reverse . tail . reverse .tail $ str 
  | otherwise = str
      
suitableAsKey :: TType -> Bool
suitableAsKey st =
  case st of
    Alphanumeric     -> True
    BigAlphanumeric  -> False
    HugeAlphanumeric -> False
    Password         -> False
    Binary           -> False
    BigBinary        -> False
    HugeBinary       -> False
    Date             -> True
    DateTime         -> True
    Boolean          -> True
    Integer          -> True
    Float            -> False
    Object           -> True
    TypeOfOne        -> fatal 143 $ "ONE has no key at all. does it?"
-----------------------------------------
--rel2att
-----------------------------------------
-- Each relation yields one attribute f1 in the plug...
-- r is the relation from some kernel attribute k1 to f1
-- (attExpr k1) is the relation from the plug's imaginary ID to k1
-- (attExpr k1);r is the relation from ID to f1
-- the rule (attExpr k1)~;(attExpr k1);r = r holds because (attExpr k1) is uni and sur, which means that (attExpr k1)~;(attExpr k1) = I
-- REMARK -> r may be tot or sur, but not inj. (attExpr k1) may be tot.
--
-- attNull and attUnique are based on the multiplicity of the relation (kernelpath);r from ID to (target r)
-- it is given that ID is unique and not null
-- attNull=not(isTot (kernelpath);r)
-- attUniq=isInj (kernelpath);r
--
-- (kernel++plugAtts) defines the name space, making sure that all attributes within a plug have unique names.

-- | Create attribute for TblSQL or ScalarSQL plugs
rel2att :: ContextInfo
        -> [Expression] -- ^ all relations (in the form either EDcD r, EDcI or EFlp (EDcD r)) that may be represented as attributes of this entity.
        -> [Expression] -- ^ all relations (in the form either EDcD r or EFlp (EDcD r)) that are defined as attributes by the user.
        -> Expression   -- ^ either EDcD r, EDcI c or EFlp (EDcD r), representing the relation from some kernel attribute k1 to f1
        -> SqlAttribute
rel2att ci
        kernel
        plugAtts
        e
 = Att { attName = attrName
       , attExpr = e
       , attType = tType2SqlType (representationOf ci (target e))
       , attUse  =
          let f expr =
                 case expr of
                    EDcI c   -> if suitableAsKey (representationOf ci c)
                                then TableKey ((not.maybenull) e) c
                                else PlainAttr
                    EDcD _   -> PlainAttr
                    EFlp e'  -> f e'
                    _        -> fatal 144 ("No attUse defined for "++show expr)
          in f e
       , attNull = maybenull e
       , attUniq = isInj e      -- all kernel fldexprs are inj
                                -- Therefore, a composition of kernel expr (I;kernelpath;e) will also be inj.
                                -- It is enough to check isInj e
       }
   where
   attrName = case [nm | (r',nm)<-table, e==r'] of
               []   -> fatal 117 $ "null names in table for e: " ++ show (e,table)
               n:_  -> n
     where
       table :: [(Expression, String)]
       table   = [ entry
                 | cl<-eqCl (map toLower.mkColumnName) (kernel++plugAtts)
                 , entry<-if length cl==1 then [(rel,mkColumnName rel) |rel<-cl] else tbl cl]
       tbl rs  = [ entry
                 | cl<-eqCl (map toLower.name.source) rs
                 , entry<-if length cl==1
                          then [(rel,mkColumnName rel++"_"++(unquote . name . source) rel) |rel<-cl]
                          else [(rel,mkColumnName rel++"_"++show i)|(rel,i)<-zip cl [(0::Int)..]]]
       
       mkColumnName expr = mkColumnName' False expr
         where  mkColumnName' isFlipped (EFlp x) = mkColumnName' (not isFlipped) x
                mkColumnName' isFlipped (EDcD d) = (if isFlipped then "src" else "tgt")++"_"++(unquote . name) d  --TODO: This has to be made more generic, to enable writing of populations from tables. (Excell spreadsheets)
                mkColumnName' _         (EDcI c) = (unquote . name) c
                mkColumnName' _ rel = fatal 162 ( "Unexpected relation found:\n"++
                                                  intercalate "\n  "
                                                    [ "***rel:"
                                                    , show rel
                                                    , "***kernel:"
                                                    , show kernel
                                                    , "***plugAtts:"
                                                    , show plugAtts
                                                    ]
                                                )
   --in a wide table, m can be total, but the attribute for its target may contain NULL values,
   --because (why? ...)
   --A kernel attribute may contain NULL values if
   --  + its attribute expr is not total OR
   --  + its attribute expr is not the identity relation AND the (kernel) attribute for its source may contain NULL values
   --(if the attExpr of a kernel attribute is the identity,
   -- then the attExpr defines the relation between this kernel attribute and this kernel attribute (attNull=not(isTot I) and attUniq=isInj I)
   -- otherwise it is the relation between this kernel attribute and some other kernel attribute)
   maybenull expr
    | length(map target kernel) Prelude.> length(nub(map target kernel))
       = fatal 146 $"more than one kernel attribute for the same concept:\n    expr = " ++(show expr)++
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

-- ^ Explanation:  rel is a relation from some kernel attribute k to f
-- ^ (attExpr k) is the relation from the plug's ID to k
-- ^ (attExpr k);rel is the relation from ID to f

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
   Then, all these relations are made into attributes. Code: plugAttributes = [rel2att plugMors a| a<-plugMors]
   We also define two lookup tables, one for the concepts that are stored in the kernel, and one for the attributes of these concepts.
   For the fun of it, we sort the plugs on length, the longest first. Code:   sortWith ((0-).length.attributes)
   By the way, parameter allRels contains all relations that are declared in context, enriched with extra properties.
   This parameter allRels was added to makePlugs to avoid recomputation of the extra properties.
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
makeEntityTables opts context allDcls isas conceptss exclusions
 = sortWith ((0-).length.plugAttributes)
    (map (kernel2Plug (contextInfoOf context)) kernelsWithAttributes)
   where
    diagnostics
      = "\nallDcls:" ++     concat ["\n  "++showHSName r           | r<-allDcls]++
        "\nallDcls:" ++     concat ["\n  "++showHS opts "\n  " r  | r<-allDcls]++
        "\nconceptss:" ++   concat ["\n  "++showHS opts "    " cs | cs<-conceptss]++
        "\nexclusions:" ++  concat ["\n  "++showHSName r           | r<-exclusions]++
        "\nattRels:" ++     concat ["\n  "++showHS opts "    " e  | e<-attRels]++
        "\n"
    -- | kernels are computed, starting with the set of concepts, on the basis of generalization tuples.
    kernPartition :: [A_Gen] -> [[A_Concept]] -- ^ This function contains the recipe to derive a set of kernels from a set of isa-pairs.
    kernPartition specialzs
     = foldl f (group (delete ONE (concs context))) specialzs
       where f disjuncLists g = concat haves : nohaves
               where
                 (haves,nohaves) = partition (not.null.intersect (concs g)) disjuncLists
    preKernels = kernPartition (gens context) -- Step 1: compute the kernels from the isa-pairs from the context
    extraIsas  -- Step 2: Maybe extra isa-pairs are needed to ensure that each kernel has precisely one largest concept
       = concat
         [ case [c | c<-kernel, null (largerConcepts isas c)] of -- determine how many concepts in one kernel are largest
             [_] -> []
             rs  -> [ Isa{gengen=rootConcept, genspc=c} | c<-rs ]
         | (rootConcept,kernel) <- zip [rc | i<-[0::Int ..]
                                           , let rc=PlainConcept { cptnm = "rootConcept"++show i
                                                                 }
                                           , rc `notElem` concs context ]
                                       preKernels
         ]
    kernls     -- Step 3: compute the kernels
     = [ largerCs++[ c | c<-kernel, c `notElem` largerCs ]              -- put the largest element up front
       | kernel <- kernPartition (extraIsas++gens context)                -- recompute the kernels with the extra isa-pairs.
       , let largerCs = [c | c<-kernel, null (largerConcepts isas c)]   -- get the set of largest concepts (each kernel has precisely one)
       ]
    kernelsWithAttributes = dist attRels kernls []
      where
        dist :: (Association attrib, Show attrib) => [attrib] -> [[A_Concept]] -> [([A_Concept], [attrib])] -> [([A_Concept], [attrib])]
        dist []   []     result = result
        dist atts []     _      = fatal 246 ("No kernel found for atts: "++show atts++"\n"++diagnostics)
        dist atts (kernel:ks) result = dist otherAtts ks ([(kernel,attsOfK)] ++ result)
           where (attsOfK,otherAtts) = partition belongsInK atts
                 belongsInK att = source att `elem` kernel
    -- | converts a kernel into a plug
    kernel2Plug :: ContextInfo -> ([A_Concept],[Expression]) -> PlugSQL
    kernel2Plug ci (kernel, attsAndIsaRels)
     =  TblSQL
             { sqlname    = unquote . name . head $ kernel -- ++ " !!Let op: De ISA relaties zie ik hier nergens terug!! (TODO. HJO 20131201"
             , attributes = map attrib plugMors            -- Each attribute comes from a relation.
             , cLkpTbl    = conceptLookuptable
             , mLkpTbl    = attributeLookuptable ++ isaLookuptable
             }
        where
          (isaAtts,atts) = partition isISA attsAndIsaRels
            where isISA (EDcI _) = True
                  isISA _        = False
          mainkernel = map EDcI kernel
          plugMors :: [Expression]
          plugMors = let exprs = mainkernel++atts in
                     if (suitableAsKey . representationOf ci . target . head) exprs || True --TODO: This check might not be required here. 
                     then exprs
                     else -- TODO: make a nice user error of the following:
                          fatal 360 $ "The concept `"++(name .target .head) exprs++"` would be used as primary key of its table. \n"
                                     ++"  However, its representation ("
                                     ++(show . representationOf ci . target . head) exprs
                                     ++") is not suitable as a key." 
                     
          conceptLookuptable :: [(A_Concept,SqlAttribute)]
          conceptLookuptable    = [(target r,attrib r) | r <-mainkernel]
          attributeLookuptable :: [(Expression,SqlAttribute,SqlAttribute)]
          attributeLookuptable  = -- kernel attributes are always surjective from left to right. So do not flip the lookup table!
                                  [(e,lookupC (source e),attrib e) | e <-plugMors]
          lookupC cpt           = head [f |(c',f)<-conceptLookuptable, cpt==c']
          attrib a              = rel2att (contextInfoOf context) mainkernel atts a
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
--makeUserDefinedSqlPlug is used to make user defined plugs. One advantage is that the attribute names and types can be controlled by the user.
--
--TODO151210 -> (see also Instance Object PlugSQL)
--              cLkpTbl TblSQL{} can have more than one concept i.e. one for each kernel attribute
--              a kernel may have more than one concept that is uni,tot,inj,sur with some imaginary ID of the plug (i.e. attNull=False)
--              When is an ObjectDef a ScalarPlug or BinPlug?
--              When do you want to define your own Scalar or BinPlug
--rel2att  (identities context) kernel plugAtts r

-- | Make a sqlplug from an ObjectDef (user-defined sql plug)
makeUserDefinedSqlPlug :: A_Context -> ObjectDef -> PlugSQL
makeUserDefinedSqlPlug context obj
 | null(fields obj) && isIdent(objctx obj)
    = ScalarSQL { sqlname   = unquote . name $ obj
                , sqlColumn = rel2att (contextInfoOf context) [EDcI c] [] (EDcI c)
                , cLkp      = c
                }
 | null(fields obj) --TODO151210 -> assuming objctx obj is Rel{} if it is not I{}
   = fatal 2372 "TODO151210 -> implement defining binary plugs in ASCII"
 | isIdent(objctx obj) --TODO151210 -> a kernel may have more than one concept that is uni,tot,inj,sur with some imaginary ID of the plug
   = {- The following may be useful for debugging:
     error
      ("\nc: "++show c++
       "\nrels:"++concat ["\n  "++show r | r<-rels]++
       "\nkernel:"++concat ["\n  "++show r | r<-kernel]++
       "\nattRels:"++concat ["\n  "++show e | e<-attRels]++
       "\nplugattributes:"++concat ["\n  "++show plugAttribute | plugAttribute<-plugattributes]
      ) -}
     TblSQL { sqlname    = unquote . name $ obj
            , attributes = plugattributes
            , cLkpTbl    = conceptLookuptable
            , mLkpTbl    = attributeLookuptable
            }
 | otherwise = fatal 279 "Implementation expects one concept for plug object (SQLPLUG tblX: I[Concept])."
  where
   c   -- one concept from the kernel is designated to "lead" this plug, this is user-defined.
     = source(objctx obj)
   rels --attributes are user-defined as one deep objats with objctx=r. note: type incorrect or non-relation objats are ignored
     = [(objctx att,sqltp att) | att<-fields obj, source (objctx att)==c]
   kernel --I[c] and every non-endo r or r~ which is at least uni,inj,sur are kernel attributes
          --REMARK -> endo r or r~ which are at least uni,inj,sur are inefficient in a way
          --          if also TOT than r=I => duplicates,
          --          otherwise if r would be implemented as GEN (target r) ISA C then (target r) could become a kernel attribute
     = [(EDcI c,sqltp obj)]
       ++ [(r,tp) |(r,tp)<-rels,not (isEndo r),isUni r, isInj r, isSur r]
       ++ [(r,tp) |(r,tp)<-rels,not (isEndo r),isUni r, isInj r, isTot r, not (isSur r)]
   attRels --all user-defined non-kernel attributes are attributes of (rel2att context (objctx c))
     = (rels >- kernel) >- [(flp r,tp) |(r,tp)<-kernel] --note: r<-rels where r=objctx obj are ignored (objctx obj=I)
   plugMors              = kernel++attRels
   plugattributes        = [attrib r tp | (r,tp)<-plugMors]
   attrib r tp           = (rel2att (contextInfoOf context) (map fst kernel) (map fst attRels) r){attType=tp}  --redefine sqltype
   conceptLookuptable    = [(target e, attrib e tp) | (e,tp)<-kernel]
   attributeLookuptable  = [(er,lookupC (source er), attrib er tp) | (er,tp)<-plugMors]
   lookupC cpt           = head [f |(c',f)<-conceptLookuptable, cpt==c']
   sqltp :: ObjectDef -> SqlTType
   sqltp _ = fatal 448 "The Sql type of a user defined plug has bitrotteted. The syntax should support a Representation."

tType2SqlType :: TType -> SqlTType
tType2SqlType dom 
 = case dom of
     Alphanumeric     -> SQLVarchar 255
     BigAlphanumeric  -> SQLText
     HugeAlphanumeric -> SQLMediumText
     Password         -> SQLVarchar 255
     Binary           -> SQLBlob
     BigBinary        -> SQLMediumBlob
     HugeBinary       -> SQLLongBlob
     Date             -> SQLDate
     DateTime         -> SQLDateTime
     Boolean          -> SQLBool
     Integer          -> SQLBigInt
     Float            -> SQLFloat
     Object           -> SQLVarchar 255
     TypeOfOne        -> fatal 461 $ "ONE is not represented in SQL" 

