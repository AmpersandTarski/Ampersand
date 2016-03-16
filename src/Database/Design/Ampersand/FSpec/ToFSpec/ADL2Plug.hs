module Database.Design.Ampersand.FSpec.ToFSpec.ADL2Plug
  (makeGeneratedSqlPlugs
  ,makeUserDefinedSqlPlug
  ,typologies)
where
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.FSpec.FSpec
import Data.Maybe
import Data.Char
import Data.List (nub,intercalate)

makeGeneratedSqlPlugs :: A_Context 
              -> (Declaration -> Declaration) -- Function to add calculated properties to a declaration
              -> [PlugSQL]
-- | Sql plugs database tables. A database table contains the administration of a set of concepts and relations.
--   if the set conains no concepts, a linktable is created.
makeGeneratedSqlPlugs context calcProps = conceptTables ++ linkTables
  where 
    conceptTables = map makeConceptTable conceptTableParts
    linkTables    = map makeLinkTable    linkTableParts
    calculatedDecls = map calcProps .filter (not . decplug) . relsDefdIn $ context 
    (conceptTableParts, linkTableParts) = dist calculatedDecls conceptsPerTable
    makeConceptTable :: ([A_Concept], [Declaration]) -> PlugSQL
    makeConceptTable (cpts , dcls) =
      TblSQL
             { sqlname    = unquote . name $ tableKey
             , attributes = map attrib plugMors            -- Each attribute comes from a relation.
             , cLkpTbl    = conceptLookuptable
             , mLkpTbl    = nub (attributeLookuptable ++ isaLookuptable)
             }
        where
          tableKey = head cpts
          declExprs = map mkExpr dcls
            where
              mkExpr d 
                | isUni d =       EDcD d
                | isInj d = EFlp (EDcD d)
                | otherwise = fatal 43 $ "relation `"++name d++"`. "++show (properties d)++"\n\n"++show d
          conceptExprs = map (\cpt -> EEps cpt (Sign tableKey cpt)) cpts
          plugMors :: [Expression]
          plugMors = let exprs = conceptExprs++declExprs 
                         reprOfID = representationOf (ctxInfo context) . head $ cpts in
                     if (suitableAsKey reprOfID) || True --TODO: This check might not be required here. 
                     then exprs
                     else -- TODO: make a nice user error of the following:
                          fatal 360 $ "The concept `"++(name . head) cpts++"` would be used as primary key of its table. \n"
                                     ++"  However, its representation ("
                                     ++ show reprOfID
                                     ++") is not suitable as a key." 
                     
          conceptLookuptable :: [(A_Concept,SqlAttribute)]
          conceptLookuptable    = [(target r,attrib r) | r <-conceptExprs]
          attributeLookuptable :: [(Expression,SqlAttribute,SqlAttribute)]
          attributeLookuptable  = -- kernel attributes are always surjective from left to right. So do not flip the lookup table!
                                  [(e,lookupC (source e),attrib e) | e <-plugMors]
          lookupC cpt           = case [f |(c',f)<-conceptLookuptable, cpt==c'] of
                                    []  -> fatal 70 $ "Concept `"++name cpt++"` is not in the lookuptable."
                                         ++"\ncpts: "++show cpts
                                         ++"\ndcls: "++show (map (\d -> name d++show (sign d)++" "++show (properties d)) dcls)
                                         ++"\nlookupTable: "++show (map fst conceptLookuptable)
                                    x:_ -> x
          attrib :: Expression -> SqlAttribute
          attrib a              = rel2att (ctxInfo context) conceptExprs declExprs a
          isaLookuptable = [(e,lookupC (source e),lookupC (target e)) | e <- conceptExprs ]
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
    makeLinkTable :: Declaration -> PlugSQL
    makeLinkTable dcl 
         = BinSQL
             { sqlname = unquote . name $ dcl
             , columns = ( -- The source attribute:
                           Att { attName = concat["Src" | isEndo dcl]++(unquote . name . source) trgExpr
                               , attExpr = srcExpr
                               , attType = repr . source $ srcExpr
                               , attUse  = if suitableAsKey . repr . source $ srcExpr
                                           then ForeignKey (target srcExpr)
                                           else PlainAttr
                               , attNull = isTot trgExpr
                               , attUniq = isUni trgExpr
                               , attFlipped = isFlipped trgExpr
                               }
                         , -- The target attribute:
                           Att { attName = concat["Tgt" | isEndo dcl]++(unquote . name . target) trgExpr
                               , attExpr = trgExpr
                               , attType = repr . target $ trgExpr
                               , attUse  = if suitableAsKey . repr . target $ trgExpr
                                           then ForeignKey (target trgExpr)
                                           else PlainAttr
                               , attNull = isSur trgExpr
                               , attUniq = isInj trgExpr
                               , attFlipped = isFlipped trgExpr
                               }
                          )
             , cLkpTbl = [] --in case of TOT or SUR you might use a binary plug to lookup a concept (don't forget to nub)
             , mLkp    = trgExpr
          --   , sqlfpa  = NO
             }
      where
       repr = representationOf (ctxInfo context)
       --the expr for the source of r
       srcExpr
        | isTot dcl = EDcI (source dcl)
        | isSur dcl = EDcI (target dcl)
        | otherwise = let er=EDcD dcl in EDcI (source dcl) ./\. (er .:. flp er)
       --the expr for the target of r
       trgExpr
        | not (isTot dcl) && isSur dcl = flp (EDcD dcl)
        | otherwise                    = EDcD dcl

    -- | In some cases, concepts can be administrated in the same conceptTable. Each concept will be administrated in exactly one 
    --   conceptTable. This function returns all concepts grouped properly. 
    --   Two concepts, A and B belong in the same group iff:
    --      1) They belong to the same typology or
    --      2) a. They do no belong to the same typology and 
    --         b. There exists an univalent, injective and surjective relation from A to B and
    --         c. All other concepts in the typology of B are more specific than B
    --      3) one of the above is true for any concept A' in the same group as A and concept B' in the same group as B.
    conceptsPerTable :: [[A_Concept]]
    conceptsPerTable = map tyCpts . typologies $ context

    -- | dist will distribute the declarations amongst the sets of concepts. 
    --   Preconditions: The sets of concepts are supposed to be sets of 
    --                  concepts that are to be represented in a single table. 
    dist :: [Declaration]   -- all declarations that are to be distributed
         -> [[A_Concept]]   -- the sets of concepts, each one contains all concepts that will go into a single table.
         -> ( [([A_Concept], [Declaration])]  -- tuples of a set of concepts and all declarations that can be
                                              -- stored into that table. The order of concepts is not modified.
            , [Declaration]  -- The declarations that cannot be stored into one of the concept tables.
            ) 
    dist dcls cptLists = 
       ( [ (cpts, declsInTable cpts) | cpts <- cptLists]
       , [ d | d <- dcls, conceptTableOf d == Nothing])
      where
        declsInTable cpts = [ dcl | dcl <- dcls
                            , not . null $ maybeToList (conceptTableOf dcl) `isc` cpts ]
        conceptTableOf :: Declaration -> Maybe A_Concept
        conceptTableOf d =
          case d of 
          Isn{} -> fatal 38 "I is not expected here." -- These relations are already in the kernel
          Vs{}  -> fatal 39 "V is not expected here" -- Vs are not implemented at all
          Sgn{} ->
               case (isInj d, isUni d) of
                    (False  , False  ) -> Nothing --Will become a link-table
                    (True   , False  ) -> Just . target $ d
                    (False  , True   ) -> Just . source $ d
                    (True   , True   ) ->
                      case (isTot d, isSur d) of
                           (False  , False  ) -> Just . target $ d
                           (True   , False  ) -> Just . source $ d
                           (False  , True   ) -> Just . source $ d
                           (True   , True   ) -> Just . source $ d



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
       , attType = representationOf ci (target e)
       , attUse  =
          let f expr =
                 case expr of
                    EEps c _ -> if suitableAsKey (representationOf ci c)
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
       , attFlipped = isFlipped e
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
         where  mkColumnName' isFlipped' (EFlp x) = mkColumnName' (not isFlipped') x
                mkColumnName' isFlipped' (EDcD d) = (if isFlipped' then "src" else "tgt")++"_"++(unquote . name) d  --TODO: This has to be made more generic, to enable writing of populations from tables. (Excell spreadsheets)
                mkColumnName' _         (EEps c _) = (unquote . name) c
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
           intercalate "\n  *** " ( "" : (map (name.target) kernel))++"\n\n-----------\n"++ show kernel
    | otherwise = case expr of
                   EDcD dcl
                        | (not.isTot) dcl -> True
                        | otherwise -> (not.null) [()|k<-kernelpaths, target k==source dcl && isTot k || target k==target dcl && isSur k ]
                   EFlp (EDcD dcl)
                        | (not.isSur) dcl -> True
                        | otherwise -> (not.null) [()|k<-kernelpaths, target k==source dcl && isSur k || target k==target dcl && isTot k]
                   EEps{} -> (not.isTot) expr
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


isFlipped :: Expression -> Bool
isFlipped e = 
  case e of
    EFlp _ -> True
    _      -> False


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
                , sqlColumn = rel2att (ctxInfo context) [EDcI c] [] (EDcI c)
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
   attrib r tp           = (rel2att (ctxInfo context) (map fst kernel) (map fst attRels) r){attType=tp}  --redefine sqltype
   conceptLookuptable    = [(target e, attrib e tp) | (e,tp)<-kernel]
   attributeLookuptable  = [(er,lookupC (source er), attrib er tp) | (er,tp)<-plugMors]
   lookupC cpt           = head [f |(c',f)<-conceptLookuptable, cpt==c']
   sqltp :: ObjectDef -> TType
   sqltp _ = fatal 448 "The Sql type of a user defined plug has bitrotteted. The syntax should support a Representation."

typologies :: A_Context -> [Typology]
typologies context = 
   (multiKernels . ctxInfo $ context) ++ 
   [Typology { tyroot = c
             , tyCpts = [c]
             } | c <- concs context >- concs (gens context)]
