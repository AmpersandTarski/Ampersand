module Ampersand.FSpec.ToFSpec.ADL2Plug
  (makeGeneratedSqlPlugs
  ,typologies
  ,suitableAsKey)
where
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.FSpec.FSpec
import Ampersand.Misc
import Data.Maybe
import Data.Char

makeGeneratedSqlPlugs :: Options -> A_Context 
              -> (Declaration -> Declaration) -- Function to add calculated properties to a declaration
              -> [PlugSQL]
-- | Sql plugs database tables. A database table contains the administration of a set of concepts and relations.
--   if the set conains no concepts, a linktable is created.
makeGeneratedSqlPlugs opts context calcProps = conceptTables ++ linkTables
  where 
    repr = representationOf (ctxInfo context)
    conceptTables = map makeConceptTable conceptTableParts
    linkTables    = map makeLinkTable    linkTableParts
    calculatedDecls = (map calcProps .filter (not . decplug) . relsDefdIn) context 
    (conceptTableParts, linkTableParts) = dist calculatedDecls (typologies context)
    makeConceptTable :: (Typology, [Declaration]) -> PlugSQL
    makeConceptTable (typ , dcls) = 
      TblSQL
             { sqlname    = unquote . name $ tableKey
             , attributes = map cptAttrib cpts ++ map dclAttrib dcls
             , cLkpTbl    = conceptLookuptable
             , dLkpTbl    = dclLookuptable
             }
        where
          cpts = reverse $ sortSpecific2Generic (gens context) (tyCpts typ)
          -- | Make sure each attribute in the table has a unique name. Take into account that sql 
          --   is not case sensitive about names of coloums. 
          colNameMap :: [ (Either A_Concept Declaration, String) ]
          colNameMap = f [] (cpts, dcls)
            where f :: [ (Either A_Concept Declaration, String) ] -> ([A_Concept],[Declaration]) -> [ (Either A_Concept Declaration, String) ]
                  f names (cs,ds) =
                     case (cs,ds) of
                       ([],[]) -> names
                       ([], _) -> f (insert (Right . head $ ds) names) ([],tail ds)
                       _       -> f (insert (Left  . head $ cs) names) (tail cs,ds)
                  insert :: (Either A_Concept Declaration) -> [(Either A_Concept Declaration, String)] -> [(Either A_Concept Declaration, String)]
                  insert item = tryInsert item 0
                    where 
                      tryInsert :: Either A_Concept Declaration -> Int -> [(Either A_Concept Declaration,String)] -> [(Either A_Concept Declaration,String)]
                      tryInsert x n names =
                        let nm = (either name name x) ++ (if n == 0 then "" else "_"++show n)
                        in if map toLower nm `elem` map (map toLower . snd) names -- case insencitive compare, because SQL needs that.
                           then tryInsert x (n+1) names
                           else (x,nm):names


          tableKey = tyroot typ
          isStoredFlipped :: Declaration -> Bool
          isStoredFlipped d
            = snd . fromMaybe ftl . wayToStore $ d
              where ftl = fatal 52 $ "relation `"++name d++"` cannot be stored in this table. "++show (properties d)++"\n\n"++show d
          conceptLookuptable :: [(A_Concept,SqlAttribute)]
          conceptLookuptable    = [(cpt,cptAttrib cpt) | cpt <-cpts]
          dclLookuptable :: [RelStore]
          dclLookuptable = map f dcls
            where f d 
                   = if isStoredFlipped d
                     then RelStore { rsDcl       = d
                                   , rsSrcAtt    = dclAttrib d
                                   , rsTrgAtt    = lookupC (target d)
                                   }
                     else RelStore { rsDcl       = d
                                   , rsSrcAtt    = lookupC (source d)
                                   , rsTrgAtt    = dclAttrib d
                                   }

          lookupC :: A_Concept -> SqlAttribute
          lookupC cpt           = case [f |(c',f)<-conceptLookuptable, cpt==c'] of
                                    []  -> fatal 70 $ "Concept `"++name cpt++"` is not in the lookuptable."
                                         ++"\ncpts: "++show cpts
                                         ++"\ndcls: "++show (map (\d -> name d++show (sign d)++" "++show (properties d)) dcls)
                                         ++"\nlookupTable: "++show (map fst conceptLookuptable)
                                    x:_ -> x
          cptAttrib :: A_Concept -> SqlAttribute
          cptAttrib cpt = Att { attName = case lookup (Left cpt) colNameMap of
                                            Nothing -> fatal 99 $ "No name found for `"++name cpt++"`. "
                                            Just nm -> nm
                              , attExpr = expr
                              , attType = repr cpt
                              , attUse  = if cpt == tableKey 
                                             && repr cpt == Object -- For scalars, we do not want a primary key. This is a workaround fix for issue #341
                                          then PrimaryKey cpt  
                                          else PlainAttr
                              , attNull   = not . isTot $ expr
                              , attDBNull = cpt /= tableKey 
                              , attUniq = True
                              , attFlipped = False
                              }
                where expr = if cpt == tableKey
                             then EDcI cpt
                             else EEps cpt (Sign tableKey cpt)
          dclAttrib :: Declaration -> SqlAttribute
          dclAttrib dcl = Att { attName = case lookup (Right dcl) colNameMap of
                                            Nothing -> fatal 113 $ "No name found for `"++name dcl++"`. "
                                            Just nm -> nm                                         
                              , attExpr = dclAttExpression
                              , attType = repr (target dclAttExpression)
                              , attUse  = if suitableAsKey . repr . target $ dclAttExpression
                                          then ForeignKey (target dclAttExpression)
                                          else PlainAttr
                              , attNull = not . isTot $ keyToTargetExpr
                              , attDBNull = True -- to prevent database errors. Ampersand checks for itself. 
                              , attUniq = isInj keyToTargetExpr
                              , attFlipped = isStoredFlipped dcl
                              }
              where dclAttExpression = if isStoredFlipped dcl
                                       then EFlp (EDcD dcl)
                                       else      (EDcD dcl)
                    keyToTargetExpr = (attExpr . cptAttrib $ (source dclAttExpression) ) .:. dclAttExpression
          
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
             , cLkpTbl = [] --TODO: in case of TOT or SUR you might use a binary plug to lookup a concept (don't forget to nub)
                            --given that dcl cannot be (UNI or INJ) (because then dcl would be in a TblSQL plug)
                            --if dcl is TOT, then the concept (source dcl) is stored in this plug
                            --if dcl is SUR, then the concept (target dcl) is stored in this plug
             , dLkpTbl = [theRelStore]
             }
      where
       theRelStore =  if isFlipped trgExpr
                         then RelStore
                               { rsDcl       = dcl
                               , rsSrcAtt    = trgAtt
                               , rsTrgAtt    = srcAtt
                               }
                         else RelStore
                               { rsDcl       = dcl
                               , rsSrcAtt    = srcAtt
                               , rsTrgAtt    = trgAtt
                               }
       --the expr for the source of r
       srcExpr
        | isTot dcl = EDcI (source dcl)
        | isSur dcl = EDcI (target dcl)
        | otherwise = let er=EDcD dcl in EDcI (source dcl) ./\. (er .:. flp er)
       --the expr for the target of r
       trgExpr
        | not (isTot dcl) && isSur dcl = flp (EDcD dcl)
        | otherwise                    = EDcD dcl
       srcAtt = Att { attName = concat["Src" | isEndo dcl]++(unquote . name . source) trgExpr
                    , attExpr = srcExpr
                    , attType = repr . source $ srcExpr
                    , attUse  = if suitableAsKey . repr . source $ srcExpr
                                then ForeignKey (target srcExpr)
                                else PlainAttr
                    , attNull = isTot trgExpr
                    , attDBNull = False  -- false for link tables
                    , attUniq = isUni trgExpr
                    , attFlipped = isFlipped trgExpr
                    }
       trgAtt = Att { attName = concat["Tgt" | isEndo dcl]++(unquote . name . target) trgExpr
                    , attExpr = trgExpr
                    , attType = repr . target $ trgExpr
                    , attUse  = if suitableAsKey . repr . target $ trgExpr
                                then ForeignKey (target trgExpr)
                                else PlainAttr
                    , attNull = isSur trgExpr
                    , attDBNull = False  -- false for link tables
                    , attUniq = isInj trgExpr
                    , attFlipped = isFlipped trgExpr
                    }

    -- | dist will distribute the declarations amongst the sets of concepts. 
    --   Preconditions: The sets of concepts are supposed to be sets of 
    --                  concepts that are to be represented in a single table. 
    dist :: [Declaration]   -- all declarations that are to be distributed
         -> [Typology]   -- the sets of concepts, each one contains all concepts that will go into a single table.
         -> ( [(Typology, [Declaration])]  -- tuples of a set of concepts and all declarations that can be
                                              -- stored into that table. The order of concepts is not modified.
            , [Declaration]  -- The declarations that cannot be stored into one of the concept tables.
            ) 
    dist dcls cptLists = 
       ( [ (t, declsInTable t) | t <- cptLists]
       , [ d | d <- dcls, conceptTableOf d == Nothing])
      where
        declsInTable typ = [ dcl | dcl <- dcls
                            , not . null $ maybeToList (conceptTableOf dcl) `isc` tyCpts typ ]
        
        conceptTableOf :: Declaration -> Maybe A_Concept
        conceptTableOf d = if sqlBinTables opts
                           then Nothing
                           else fmap fst $ wayToStore d

-- | this function tells in what concepttable a given declaration is to be stored. If stored
--   in a concept table, it returns the concept and a boolean, telling wether or not the relation
--   is stored flipped.
wayToStore :: Declaration -> Maybe (A_Concept,Bool)
wayToStore d =
  case d of 
  Isn{} -> fatal 38 "I is not expected here." -- These relations are already in the kernel
  Vs{}  -> fatal 39 "V is not expected here" -- Vs are not implemented at all
  Sgn{} ->
       case (isInj d, isUni d) of
            (False  , False  ) -> Nothing --Will become a link-table
            (True   , False  ) -> Just flipped
            (False  , True   ) -> Just plain
            (True   , True   ) ->
              case (isTot d, isSur d) of
                   (False  , False  ) -> Just plain 
                   (True   , False  ) -> Just plain
                   (False  , True   ) -> Just plain
                   (True   , True   ) -> Just plain
  where plain  = (source d,False)
        flipped = (target d, True)


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

 

isFlipped :: Expression -> Bool
isFlipped e = 
  case e of
    EFlp _ -> True
    _      -> False



typologies :: A_Context -> [Typology]
typologies context = 
   (multiKernels . ctxInfo $ context) ++ 
   [Typology { tyroot = c
             , tyCpts = [c]
             } | c <- concs context >- concs (gens context)]
