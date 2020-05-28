{-# LANGUAGE OverloadedStrings #-}
module Ampersand.FSpec.ToFSpec.ADL2Plug
  (makeGeneratedSqlPlugs
  ,typologies
  ,suitableAsKey
  ,attributesOfConcept)
where
import           Ampersand.Basics
import           Ampersand.Classes
import           Ampersand.ADL1
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.ToFSpec.Populated (sortSpecific2Generic)
import           Ampersand.Misc.HasClasses
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T

attributesOfConcept :: FSpec -> A_Concept -> [SqlAttribute]
attributesOfConcept fSpec c
 = [  att | att<-NE.tail (plugAttributes (getConceptTableFor fSpec c)), not (inKernel att), source (attExpr att)==c]
   where
     inKernel :: SqlAttribute -> Bool
     inKernel att = isUni expr 
                 && isInj expr
                 && isSur expr
                 && (not . isProp) expr
         where expr = attExpr att 
              --was : null(Set.fromList [Uni,Inj,Sur]Set.\\properties (attExpr att)) && not (isPropty att)

makeGeneratedSqlPlugs :: (HasFSpecGenOpts env) 
       => env -> A_Context 
              -> (Relation -> Relation) -- Function to add calculated properties to a relation
              -> [PlugSQL]
-- | Sql plugs database tables. A database table contains the administration of a set of concepts and relations.
--   if the set conains no concepts, a linktable is created.
makeGeneratedSqlPlugs env context calcProps = conceptTables <> linkTables
  where 
    repr = representationOf (ctxInfo context)
    conceptTables = map makeConceptTable conceptTableParts
    linkTables    = map makeLinkTable    linkTableParts
    calculatedDecls :: Relations
    calculatedDecls = Set.map calcProps . relsDefdIn $ context 
    (conceptTableParts, linkTableParts) = dist calculatedDecls (typologies context)
    makeConceptTable :: (Typology, [Relation]) -> PlugSQL
    makeConceptTable (typ , dcls) = 
      TblSQL
             { sqlname    = unquote . name $ tableKey
             , attributes = map cptAttrib cpts <> map dclAttrib dcls
             , cLkpTbl    = conceptLookuptable
             , dLkpTbl    = dclLookuptable
             }
        where
          cpts = reverse $ sortSpecific2Generic (gens context) (tyCpts typ)
          -- | Make sure each attribute in the table has a unique name. Take into account that sql 
          --   is not case sensitive about names of coloums. 
          colNameMap :: [ (Either A_Concept Relation, Text) ]
          colNameMap = f [] (cpts, dcls)
            where f :: [ (Either A_Concept Relation, Text) ] -> ([A_Concept],[Relation]) -> [ (Either A_Concept Relation, Text) ]
                  f names (cs,ds) =
                     case (cs,ds) of
                       ([],[]) -> names
                       ([], h:tl) -> f (insert (Right h) names) ([],tl)
                       (h:tl,_  ) -> f (insert (Left h) names) (tl,ds)
                  insert :: Either A_Concept Relation -> [(Either A_Concept Relation, Text)] -> [(Either A_Concept Relation, Text)]
                  insert item = tryInsert item 0
                    where 
                      tryInsert :: Either A_Concept Relation -> Int -> [(Either A_Concept Relation,Text)] -> [(Either A_Concept Relation,Text)]
                      tryInsert x n names =
                        let nm = either name name x <> (if n == 0 then "" else "_"<>tshow n)
                        in if T.toLower nm `elem` map (T.toLower . snd) names -- case insencitive compare, because SQL needs that.
                           then tryInsert x (n+1) names
                           else (x,nm):names


          tableKey = tyroot typ
          conceptLookuptable :: [(A_Concept,SqlAttribute)]
          conceptLookuptable    = [(cpt,cptAttrib cpt) | cpt <-cpts]
          dclLookuptable :: [RelStore]
          dclLookuptable = map f dcls
            where f d =
                   RelStore { rsDcl       = d
                            , rsStoredFlipped = isStoredFlipped d
                            , rsSrcAtt    = if isStoredFlipped d then dclAttrib d else lookupC (source d)
                            , rsTrgAtt    = if isStoredFlipped d then lookupC (target d) else dclAttrib d
                            }

          lookupC :: A_Concept -> SqlAttribute
          lookupC cpt           = case [f |(c',f)<-conceptLookuptable, cpt==c'] of
                                    []  -> fatal $ "Concept `"<>name cpt<>"` is not in the lookuptable."
                                         <>"\ncpts: "<>tshow cpts
                                         <>"\ndcls: "<>tshow (map (\d -> name d<>tshow (sign d)<>" "<>tshow (properties d)) dcls)
                                         <>"\nlookupTable: "<>tshow (map fst conceptLookuptable)
                                    x:_ -> x
          cptAttrib :: A_Concept -> SqlAttribute
          cptAttrib cpt = Att { attName = fromMaybe (fatal ("No name found for `"<>name cpt<>"`. "))
                                                    (lookup (Left cpt) colNameMap) 
                              , attExpr    = expr
                              , attType    = repr cpt
                              , attUse     = if cpt == tableKey 
                                                && repr cpt == Object -- For scalars, we do not want a primary key. This is a workaround fix for issue #341
                                             then PrimaryKey cpt  
                                             else PlainAttr
                              , attNull    = not . isTot $ expr
                              , attDBNull  = cpt /= tableKey 
                              , attUniq    = True
                              , attFlipped = False
                              }
                where expr = if cpt == tableKey
                             then EDcI cpt
                             else EEps cpt (Sign tableKey cpt)
          dclAttrib :: Relation -> SqlAttribute
          dclAttrib dcl = Att { attName = fromMaybe (fatal ("No name found for `"<>name dcl<>"`. "))
                                                    (lookup (Right dcl) colNameMap)
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
              where dclAttExpression = (if isStoredFlipped dcl then EFlp else id) (EDcD dcl)
                    keyToTargetExpr = (attExpr . cptAttrib . source $ dclAttExpression)  .:. dclAttExpression
          
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
    makeLinkTable :: Relation -> PlugSQL
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
       bindedExp :: Expression
       bindedExp = EDcD dcl
       theRelStore = RelStore
            { rsDcl       = dcl
            , rsStoredFlipped = isStoredFlipped dcl
            , rsSrcAtt    = if isStoredFlipped dcl then trgAtt else srcAtt
            , rsTrgAtt    = if isStoredFlipped dcl then srcAtt else trgAtt
            }
       --the expr for the source of r
       srcExpr
        | isTot bindedExp = EDcI (source bindedExp)
        | isSur bindedExp = EDcI (target bindedExp)
        | otherwise = EDcI (source bindedExp) ./\. (bindedExp .:. flp bindedExp)
       --the expr for the target of r
       trgExpr
        | not (isTot bindedExp) && isSur bindedExp = flp bindedExp
        | otherwise                                =     bindedExp
       srcAtt = Att { attName = T.concat["Src" | isEndo dcl]<>(unquote . name . source) trgExpr
                    , attExpr = srcExpr
                    , attType = repr . source $ srcExpr
                    , attUse  = if suitableAsKey . repr . source $ srcExpr
                                then ForeignKey (target srcExpr)
                                else PlainAttr
                    , attNull = False  -- false for link tables. This was 'isTot trgExpr' (was this a mistake?)
                    , attDBNull = False
                    , attUniq = isUni trgExpr
                    , attFlipped = isStoredFlipped dcl
                    }
       trgAtt = Att { attName = T.concat["Tgt" | isEndo dcl]<>(unquote . name . target) trgExpr
                    , attExpr = trgExpr
                    , attType = repr . target $ trgExpr
                    , attUse  = if suitableAsKey . repr . target $ trgExpr
                                then ForeignKey (target trgExpr)
                                else PlainAttr
                    , attNull = isSur trgExpr
                    , attDBNull = False  -- false for link tables
                    , attUniq = isInj trgExpr
                    , attFlipped = isStoredFlipped dcl
                    }

    -- | dist will distribute the relations amongst the sets of concepts. 
    --   Preconditions: The sets of concepts are supposed to be sets of 
    --                  concepts that are to be represented in a single table. 
    dist :: Relations   -- all relations that are to be distributed
         -> [Typology]   -- the sets of concepts, each one contains all concepts that will go into a single table.
         -> ( [(Typology, [Relation])]  -- tuples of a set of concepts and all relations that can be
                                              -- stored into that table. The order of concepts is not modified.
            , [Relation]  -- The relations that cannot be stored into one of the concept tables.
            ) 
    dist dcls cptLists = 
       ( [ (t, declsInTable t) | t <- cptLists]
       , [ d | d <- Set.elems dcls, isNothing (conceptTableOf d)]
       )
      where
        declsInTable typ = [ dcl | dcl <- Set.elems dcls
                           , case conceptTableOf dcl of
                               Nothing -> False
                               Just x  -> x `elem` tyCpts typ
                           ]
    conceptTableOf :: Relation -> Maybe A_Concept
    conceptTableOf  = fst . wayToStore env
    isStoredFlipped :: Relation -> Bool
    isStoredFlipped = snd . wayToStore env

-- | this function tells how a given relation is to be stored. If stored
--   in a concept table, it returns that concept. It allways returns a boolean
--   telling wether or not the relation is stored flipped.
wayToStore :: (HasFSpecGenOpts env) => env -> Relation -> (Maybe A_Concept,Bool)
wayToStore env dcl = 
   if view sqlBinTablesL env 
   then (Nothing, False)
   else case (isInj d, isUni d) of
            (True   , False  ) -> inConceptTableFlipped
            (_      , True   ) -> inConceptTablePlain
            (False  , False  ) -> inLinkTable --Will become a link-table
  where d = EDcD dcl
        inConceptTablePlain   = (Just $ source d,False)
        inConceptTableFlipped = (Just $ target d, True)
        inLinkTable = ( Nothing
                      , -- The order of columns in a linked table could
                        -- potentially speed up queries, in cases where
                        -- the relation is TOT or SUR. In that case there
                        -- should be no need to look in the concept table,
                        -- for all atoms are in the first colum of the link table
                        not (isTot d) && isSur d
                      )

unquote :: Text -> Text
unquote str =
  case T.uncons str of
   Just ('"',tl)
       -> case T.uncons (T.reverse tl) of
            Just ('"',mid) -> T.reverse mid
            _              -> str
   _   -> str
      
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
    TypeOfOne        -> fatal "ONE has no key at all. does it?"

 



typologies :: A_Context -> [Typology]
typologies context = 
   (multiKernels . ctxInfo $ context) <> 
   [Typology { tyroot = c
             , tyCpts = [c]
             } 
   | c <- Set.elems $ concs context Set.\\ concs (gens context)
   ]
