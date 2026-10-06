{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ampersand.FSpec.ToFSpec.ADL2Plug
  ( makeGeneratedSqlPlugs,
    suitableAsKey,
    attributesOfConcept,
    foreignTable,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.Core.AbstractSyntaxTree (Guarded (..), makeTypologies)
import Ampersand.Core.ParseTree (foreignContexts, foreignLabelViews)
import Ampersand.FSpec.FSpec
import Ampersand.Misc.HasClasses
import qualified Algebra.Graph.AdjacencyMap as Graph
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T

maxLengthOfDatabaseTableName :: Int
maxLengthOfDatabaseTableName = 64

shaLength :: Int
shaLength = 7

-- | The attributes that the concept table of `c` holds for `c`. A concept
--   without a concept table has none (issue #1672).
attributesOfConcept :: FSpec -> A_Concept -> [SqlAttribute]
attributesOfConcept fSpec c = case lookupCpt fSpec c of
  [] -> []
  (plug, _) : _ ->
    [att | att <- NE.tail (plugAttributes plug), not (inKernel att), source (attExpr att) == c]
  where
    inKernel :: SqlAttribute -> Bool
    inKernel att =
      isUni expr
        && isInj expr
        && isSur expr
        && (not . isProp) expr
      where
        expr = attExpr att

-- was : null(Set.fromList [Uni,Inj,Sur]Set.\\properties (attExpr att)) && not (isPropty att)

-- | Sql plugs database tables. A database table contains the administration of a set of concepts and relations.
--   if the set contains no concepts, a linktable is created.
--
--   `neededConcepts` are the concepts that some generated query enumerates, as
--   computed by `Ampersand.FSpec.ToFSpec.ConceptTables.conceptsNeedingATable`.
--   A typology whose table would hold no relation columns and none of whose
--   concepts is enumerated gets no table: such a table would only administrate
--   atoms that nothing ever reads. See issue #1672.
makeGeneratedSqlPlugs :: (HasFSpecGenOpts env) => env -> A_Context -> [A_Concept] -> [PlugSQL]
makeGeneratedSqlPlugs env context neededConcepts = inspectedCandidateTables -- ++tableForONE toevoegen is onjuist omdat ONE al in inspectedCandidateTables zit.
  where
    inspectedCandidateTables :: [PlugSQL]
    inspectedCandidateTables
      | null candidateTables = []
      | otherwise = case filter (not . isSingleton) . eqCl sqlname $ candidateTables of
          [] -> case filter hasNameConflict candidateTables of
            [] -> candidateTables
            xs ->
              fatal
                . T.intercalate "\n   "
                $ [ "The following " <> tshow (length xs) <> " generated tables have a name conflict:"
                  ]
                <> concatMap showNameConflict (L.sortOn sqlname xs)
                <> hint
          xs ->
            fatal
              . T.intercalate "\n   "
              $ [ "The following names are used for different tables:"
                ]
              <> concatMap myShow xs
              <> hint
      where
        hint :: [Text]
        hint =
          [ "",
            "Please report this as a bug! ",
            "When these fatals are thrown, it is good to know that these sqlnames are disambiguated by adding a gitLikeSha. This is a hash",
            "where only the first 7 digits are used. There is a very tiny chance that this disambiguation isn't good enough.",
            "That is why after the generation of the tables this check is done.",
            "This text is here to help the developer of ampersand to investigate."
          ]
        myShow :: NonEmpty PlugSQL -> [Text]
        myShow x =
          [ "The name `" <> (tshow . sqlname . NE.head $ x) <> "` is used for " <> (tshow . NE.length $ x) <> " tables:"
          ]
            <> map tshow (toList x)
        hasNameConflict :: PlugSQL -> Bool
        hasNameConflict = not . all isSingleton . NE.toList . eqClassNE (sameBy attSQLColName) . plugAttributes
        showNameConflict :: PlugSQL -> [Text]
        showNameConflict plug =
          ("    " <>)
            <$> [ "Table: " <> tshow (sqlname plug)
                ]
            <> ( ("    " <>)
                   <$> (L.sort . map (tshow . attSQLColName) . toList . plugAttributes $ plug)
               )
        sameBy foo a b = foo a == foo b
        isSingleton :: NonEmpty a -> Bool
        isSingleton (_ NE.:| []) = True
        isSingleton (_ NE.:| _) = False

    candidateTables :: [PlugSQL]
    candidateTables = concatMap tablesOf scopes

    -- In a system of contexts, every context has a database of its own.
    -- The tables of a context are a function of its own declarations, so that
    -- the context that owns a table and a context that reads it agree on its layout.
    -- That is why the tables are made per owner.
    labels :: [Text]
    labels = map fst (foreignContexts (ctxmetas context))
    ownerOf :: Name -> Maybe Text
    ownerOf nm = case nameSpaceOf nm of
      h : _ | namePartToText h `elem` labels -> Just (namePartToText h)
      _ -> Nothing
    inSystem :: Bool
    inSystem = not (null labels) || view allConceptTablesL env
    scopes :: [Scope]
    scopes
      | inSystem = scopeOf Nothing : map (scopeOf . Just) labels
      | otherwise =
          [ Scope
              { scMine = const True,
                scView = id,
                scTypologies = multiKernels (ctxInfo context),
                scPrune = True,
                scLocalise = id
              }
          ]
    scopeOf :: Maybe Text -> Scope
    scopeOf owner =
      Scope
        { scMine = (== owner) . ownerOf,
          scView = maybe id viewOf owner,
          scTypologies = concatMap (split owner) (multiKernels (ctxInfo context)),
          scPrune = False,
          scLocalise = maybe id localise owner
        }
    -- The part of a typology that one context owns. A classification that relates concepts
    -- of two contexts does not make them share a table.
    split :: Maybe Text -> Typology -> [Typology]
    split owner typol =
      case makeTypologies (Graph.induce ((== owner) . ownerOfSet) (tyGrph typol)) of
        Checked ts _ -> ts
        Errors _ -> fatal ("The concepts of one context in the typology of " <> tshow (tyroot typol) <> " have no single most generic concept.")
      where
        ownerOfSet aliasSet = case Set.toList aliasSet of
          nm : _ -> ownerOf nm
          [] -> Nothing
    -- The name that the owner of a table has for a thing, given the name in this context.
    viewOf :: Text -> Name -> Name
    viewOf owner nm = case nameSpaceOf nm of
      h : rest
        | namePartToText h == owner -> rename rest
        | otherwise -> case [o | (l, k, o) <- foreignLabelViews (ctxmetas context), l == owner, k == namePartToText h] of
            o : _ -> case try2Namepart o of
              Right np -> rename (np : rest)
              _ -> nm
            [] -> nm
      [] -> nm
      where
        rename ns = withNameSpace ns (mkName (nameType nm) (localName nm NE.:| []))
    -- A table of another context is known here by the label of that context and the name that its owner gave it.
    localise :: Text -> PlugSQL -> PlugSQL
    localise owner plug
      | T.length localName' > maxLengthOfDatabaseTableName =
          fatal ("The name " <> localName' <> " is too long for a table. Give the context " <> owner <> " a shorter alias.")
      | otherwise = plug {sqlname = text1ToSqlName (toText1Unsafe localName')}
      where
        localName' = owner <> "." <> text1ToText (sqlColumNameToText1 (sqlname plug))

    tablesOf :: Scope -> [PlugSQL]
    tablesOf sc = map (scLocalise sc . makeTable) components
      where
        components :: [(Maybe Typology, [Relation])]
        components =
          -- trace ("7. components count: " <> tshow (length comps) <> "\n   components: " <> tshow comps)
          comps
          where
            -- Orphan relations are relations that cause link tables in the database,
            -- i.e relation that are neither univalent nor injective.
            comps =
              (filter tableIsWorthGenerating . map componentsForTypology . scTypologies $ sc)
                <> (map componentsForOrphanRelation . filter isOrphan $ allRelationsInContext)
            -- A concept table earns its keep by storing relations, or by answering
            -- "what are the atoms of this concept?" for a query that asks. A table
            -- that does neither administrates atoms that nothing reads. (issue #1672)
            tableIsWorthGenerating :: (Maybe Typology, [Relation]) -> Bool
            tableIsWorthGenerating (mTypol, rels) =
              not (scPrune sc) || not (null rels) || case mTypol of
                Nothing -> True
                Just typol -> any isNeeded (conceptsOfTypology typol)
            -- Eq on A_Concept is alias intersection, so compare with `elem` rather
            -- than through a Set (whose Ord instance compares whole alias sets).
            isNeeded :: A_Concept -> Bool
            isNeeded cpt = cpt `elem` neededConcepts
            componentsForTypology typol =
              (Just typol, filter (relationBelongsToConceptTable typol) allRelationsInContext)
            componentsForOrphanRelation rel = (Nothing, [rel])
            isOrphan = isNothing . conceptTableOf
            relationBelongsToConceptTable :: Typology -> Relation -> Bool
            relationBelongsToConceptTable typol rel =
              case conceptTableOf rel of
                Just x@(PlainConcept {}) -> aliases x `elem` tyCpts typol
                Just ONE -> True -- ONE is in every typology, but does not belong to any concept table. However, it has no attributes, so this clause is only here for theorecal completeness.
                Just _ -> False
                Nothing -> False

        allRelationsInContext = filter (scMine sc . name) (toList (relsDefdIn context))

        makeTable :: (Maybe Typology, [Relation]) -> PlugSQL
        makeTable (mTypol, rels) = case (mTypol, rels) of
          (Nothing, []) -> fatal "At least a typology or a relation must be present to build a table."
          (Nothing, [rel]) -> makeLinkTable rel
          (Nothing, _) -> fatal "Cannot build a link table with more than one relation."
          (Just typol, _) -> makeConceptTable typol rels
        allKeyConcepts :: [A_Concept]
        allKeyConcepts = [cpt | typol <- scTypologies sc, Checked cpt _warning <- [pCpt2aCpt (tyroot typol)]]
        pCpt2aCpt :: P_Concept -> Guarded A_Concept
        pCpt2aCpt = conceptMap (ctxInfo context) OriginUnknown
        allLinkTableRelations :: [Relation]
        allLinkTableRelations = concatMap snd . filter (isNothing . fst) $ components
        makeConceptTable :: Typology -> [Relation] -> PlugSQL
        makeConceptTable typol allRelationsInTable =
          TblSQL
            { sqlname = determineWideTableName tableKey,
              attributes =
                map cptAttrib allConceptsInTable
                  <> map dclAttrib allRelationsInTable,
              cLkpTbl = conceptLookuptable,
              dLkpTbl = dclLookuptable,
              mainItem = toConceptOrRelation tableKey
            }
          where
            allConceptsInTable :: [A_Concept]
            allConceptsInTable = conceptsOfTypology typol
            determineWideTableName :: A_Concept -> SqlName
            determineWideTableName keyConcept =
              determineSqlName
                (scView sc)
                (map toConceptOrRelation allKeyConcepts)
                (toConceptOrRelation keyConcept)
            tableScope = map toConceptOrRelation allConceptsInTable <> map toConceptOrRelation allRelationsInTable
            tableKey = case pCpt2aCpt (tyroot typol) of
              Checked cpt _warning -> cpt
              _ -> fatal ("The root of a typology " <> tshow (tyroot typol) <> " should always be a PlainConcept, so this should not happen.")
            conceptLookuptable :: [(A_Concept, SqlAttribute)]
            conceptLookuptable = [(cpt, cptAttrib cpt) | cpt <- allConceptsInTable]
            dclLookuptable :: [RelStore]
            dclLookuptable = map f allRelationsInTable
              where
                f d =
                  RelStore
                    { rsDcl = d,
                      rsStoredFlipped = isStoredFlipped d,
                      rsSrcAtt = if isStoredFlipped d then dclAttrib d else lookupC (source d),
                      rsTrgAtt = if isStoredFlipped d then lookupC (target d) else dclAttrib d
                    }

            lookupC :: A_Concept -> SqlAttribute
            lookupC cpt = case [f | (c', f) <- conceptLookuptable, cpt == c'] of
              [] ->
                fatal
                  $ "Concept `"
                  <> fullName cpt
                  <> "` is not in the lookuptable."
                  <> "\nallConceptsInTable: "
                  <> tshow allConceptsInTable
                  <> "\nallRelationsInTable: "
                  <> tshow (map (\d -> fullName d <> tshow (sign d) <> " " <> tshow (properties d)) allRelationsInTable)
                  <> "\nlookupTable: "
                  <> tshow (map fst conceptLookuptable)
              x : _ -> x
            cptAttrib :: A_Concept -> SqlAttribute
            cptAttrib cpt =
              Att
                { attSQLColName = determineSqlName (scView sc) tableScope (toConceptOrRelation cpt),
                  attExpr = expr,
                  attType = ctxReprType context cpt,
                  attUse =
                    if cpt
                      == tableKey
                      && ctxReprType context cpt
                      == Object -- For scalars, we do not want a primary key. This is a workaround fix for issue #341
                      then PrimaryKey cpt
                      else PlainAttr,
                  attNull = cpt /= tableKey, -- column for specializations can be NULL, but not the first column (tableKey)
                  attDBNull = cpt /= tableKey, -- column for specializations can be NULL, but not the first column (tableKey)
                  attUniq = True,
                  attFlipped = False
                }
              where
                expr = EDcI cpt
            dclAttrib :: Relation -> SqlAttribute
            dclAttrib dcl =
              Att
                { attSQLColName = determineSqlName (scView sc) tableScope (toConceptOrRelation dcl),
                  attExpr = dclAttExpression,
                  attType = ctxReprType context (target dclAttExpression),
                  attUse =
                    if suitableAsKey . ctxReprType context . target $ dclAttExpression
                      then ForeignKey (target dclAttExpression)
                      else PlainAttr,
                  attNull = not . isTot $ keyToTargetExpr,
                  attDBNull = True, -- always allow NULL values in the table structure. We use the invariant rules to check if column is mandatory
                  attUniq = isInj keyToTargetExpr,
                  attFlipped = isStoredFlipped dcl
                }
              where
                dclAttExpression = (if isStoredFlipped dcl then EFlp else id) (EDcD dcl)
                keyToTargetExpr = (attExpr . cptAttrib . source $ dclAttExpression) .:. dclAttExpression

        -----------------------------------------
        -- makeLinkTable
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
        makeLinkTable :: Relation -> PlugSQL
        makeLinkTable dcl =
          BinSQL
            { sqlname = determineLinkTableName dcl,
              cLkpTbl = [], -- TODO: #1558 in case of TOT or SUR you might use a binary plug to lookup a concept (don't forget to nub)
              -- given that dcl cannot be (UNI or INJ) (because then dcl would be in a TblSQL plug)
              -- if dcl is TOT, then the concept (source dcl) is stored in this plug
              -- if dcl is SUR, then the concept (target dcl) is stored in this plug
              dLkpTbl = [theRelStore],
              mainItem = toConceptOrRelation dcl
            }
          where
            determineLinkTableName :: Relation -> SqlName
            determineLinkTableName rel =
              determineSqlName
                (scView sc)
                scope
                (toConceptOrRelation rel)
              where
                scope =
                  map toConceptOrRelation allKeyConcepts
                    <> map toConceptOrRelation allLinkTableRelations

            bindedExp :: Expression
            bindedExp = EDcD dcl
            theRelStore =
              RelStore
                { rsDcl = dcl,
                  rsStoredFlipped = isStoredFlipped dcl,
                  rsSrcAtt = if isStoredFlipped dcl then trgAtt else srcAtt,
                  rsTrgAtt = if isStoredFlipped dcl then srcAtt else trgAtt
                }
            -- the expr for the domain of r
            domExpr
              | isTot bindedExp = EDcI (source bindedExp)
              | isSur bindedExp = EDcI (target bindedExp)
              | otherwise = EDcI (source bindedExp) ./\. (bindedExp .:. flp bindedExp)
            -- the expr for the codomain of r
            codExpr
              | not (isTot bindedExp) && isSur bindedExp = flp bindedExp
              | otherwise = bindedExp
            srcAtt =
              Att
                { attSQLColName = text1ToSqlName $ fullName1 . (if isEndo dcl then prependToPlainName "Src" else id) . scView sc . name . source $ codExpr,
                  attExpr = domExpr,
                  attType = ctxReprType context (source domExpr),
                  attUse =
                    if suitableAsKey . ctxReprType context . source $ domExpr
                      then ForeignKey (source domExpr)
                      else PlainAttr,
                  attNull = False, -- false for link tables
                  attDBNull = False, -- false for link tables
                  attUniq = isUni codExpr,
                  attFlipped = isStoredFlipped dcl
                }
            trgAtt =
              Att
                { attSQLColName = text1ToSqlName $ fullName1 . (if isEndo dcl then prependToPlainName "Tgt" else id) . scView sc . name . target $ codExpr,
                  attExpr = codExpr,
                  attType = ctxReprType context (target codExpr),
                  attUse =
                    if suitableAsKey . ctxReprType context . target $ codExpr
                      then ForeignKey (target codExpr)
                      else PlainAttr,
                  attNull = False, -- false for link tables
                  attDBNull = False, -- false for link tables
                  attUniq = isInj codExpr,
                  attFlipped = isStoredFlipped dcl
                }
        -- tableForONE :: [PlugSQL]
        -- tableForONE =
        --   [ TblSQL
        --       { sqlname = text1ToSqlName (fullName1 ONE),
        --         attributes = [oneAttribute],
        --         cLkpTbl = [(ONE, oneAttribute)],
        --         dLkpTbl = [],
        --         mainItem = toConceptOrRelation ONE
        --       }
        --   ]
        --   where
        --     oneAttribute :: SqlAttribute
        --     oneAttribute =
        --       Att
        --         { attSQLColName = text1ToSqlName (fullName1 ONE),
        --           attExpr = EDcI ONE,
        --           attType = ctxReprType context ONE, -- = Object
        --           attUse = PrimaryKey ONE,
        --           attNull = False,
        --           attDBNull = False,
        --           attUniq = True,
        --           attFlipped = False
        --         }
        conceptsOfTypology :: Typology -> [A_Concept]
        conceptsOfTypology typol = map aliasSetToConcept (tyCpts typol)
          where
            aliasSetToConcept :: Set.Set Name -> A_Concept
            aliasSetToConcept aliasSet =
              case Set.toList aliasSet of
                [] -> fatal "Empty alias set in concept table"
                nm : _
                  | nm == nameOfONE -> ONE
                  | otherwise -> PlainConcept {aliases = aliasSet, typology = wholeTypology typol}
        -- The typology that a concept has in this context, of which the given one is the part of one owner.
        wholeTypology :: Typology -> Typology
        wholeTypology typol =
          case [t | t <- multiKernels (ctxInfo context), any (`elem` tyCpts t) (tyCpts typol)] of
            t : _ -> t
            [] -> typol
        conceptTableOf :: Relation -> Maybe A_Concept
        conceptTableOf = fst . wayToStore env (scMine sc . name)
        isStoredFlipped :: Relation -> Bool
        isStoredFlipped = snd . wayToStore env (scMine sc . name)

-- | this function tells how a given relation is to be stored. If stored
--   in a concept table, it returns that concept. It returns a boolean
--   that tells wether or not the relation is stored flipped.
wayToStore :: (HasFSpecGenOpts env) => env -> (A_Concept -> Bool) -> Relation -> (Maybe A_Concept, Bool)
wayToStore env sameOwner dcl
  | view sqlBinTablesL env = (Nothing, False) -- binary tables only
  | isUni (EDcD dcl) && sameOwner (source d) = (Just $ source d, False) -- to concept table, plain
  | isInj (EDcD dcl) && sameOwner (target d) = (Just $ target d, True) -- to concept table, flipped
  | otherwise = (Nothing, not (isTot d) && isSur d) -- to link-table
  -- The order of columns in a linked table could
  -- potentially speed up queries, in cases where
  -- the relation is TOT or SUR. In that case there
  -- should be no need to look in the concept table,
  -- for all atoms are in the first colum of the link table
  where
    d = EDcD dcl

-- | The tables of one context: which things belong to it, and how they are named and laid out.
data Scope = Scope
  { -- | whether a concept or relation with this name belongs to the context
    scMine :: Name -> Bool,
    -- | the name that the context itself has for a thing, given its name in the compiled context
    scView :: Name -> Name,
    -- | the typologies, restricted to the concepts of the context
    scTypologies :: [Typology],
    -- | whether a concept table that no query reads is left out (issue #1672)
    scPrune :: Bool,
    -- | the name by which the compiled context knows a table of the context
    scLocalise :: PlugSQL -> PlugSQL
  }

-- | A table that another context owns: the label of that context and the name of the table in its database.
--   The compiled context reads and writes it through a view with the name of the plug.
foreignTable :: [Text] -> PlugSQL -> Maybe (Text, Text)
foreignTable labels plug =
  case nameSpaceOf (either name name (mainItem plug)) of
    h : _
      | namePartToText h `elem` labels ->
          Just (namePartToText h, T.drop (T.length (namePartToText h) + 1) (text1ToText (sqlColumNameToText1 (sqlname plug))))
    _ -> Nothing

suitableAsKey :: TType -> Bool
suitableAsKey st =
  case st of
    Alphanumeric -> True
    BigAlphanumeric -> False
    HugeAlphanumeric -> False
    Password -> False
    Binary -> False
    BigBinary -> False
    HugeBinary -> False
    Date -> True
    DateTime -> True
    Boolean -> True
    Integer -> True
    Float -> False
    Object -> True
    TypeOfOne -> True

-- | ConceptOrRelation is meant to be things that can end up in a database. It is designed
-- to have Concepts and Relations as instances.
type ConceptOrRelation = Either A_Concept Relation

instance Named ConceptOrRelation where
  name (Left x) = name x
  name (Right x) = name x

disambiguatedName :: (Name -> Name) -> ConceptOrRelation -> Text1
disambiguatedName vw x = toText1Unsafe $ basepart <> "_" <> gitLikeSha vw x
  where
    basepart = case unsnoc firstPart of
      Nothing -> fatal "Impossible to have an empty name."
      Just (init, last)
        | last == '.' -> init
        | otherwise -> firstPart
    firstPart = T.take maxLengthOfDatabaseTableName . viewedName vw $ x

gitLikeSha :: (Name -> Name) -> ConceptOrRelation -> Text
gitLikeSha vw = T.take shaLength . tshow . sha1hash . hashText vw

hashText :: (Name -> Name) -> ConceptOrRelation -> Text
hashText vw x = case x of
  Left cpt -> viewedName vw cpt
  Right rel ->
    viewedName vw rel
      <> viewedName vw (source rel)
      <> viewedName vw (target rel)

-- | The full name of a thing, as the owner of its table sees it.
viewedName :: (Named a) => (Name -> Name) -> a -> Text
viewedName vw = fullName . vw . name

class (Named a) => TableArtefact a where
  toConceptOrRelation :: a -> ConceptOrRelation

instance TableArtefact A_Concept where
  toConceptOrRelation = Left

instance TableArtefact Relation where
  toConceptOrRelation = Right

determineSqlName :: (HasCallStack) => (Name -> Name) -> [ConceptOrRelation] -> ConceptOrRelation -> SqlName
determineSqlName vw scope conceptOrRelation =
  text1ToSqlName
    . (if mustBeDisambiguated then disambiguatedName vw else toText1Unsafe . viewedName vw)
    $ conceptOrRelation
  where
    mustBeDisambiguated :: Bool
    mustBeDisambiguated =
      case filter (conceptOrRelation `elem`) . map toList $ eqClass equality scope of
        [clazz] -> (T.length . viewedName vw $ conceptOrRelation) > maxLengthOfDatabaseTableName || length clazz > 1
        _ -> fatal "Concept must be found exactly in one list."
      where
        equality :: (Named a) => a -> a -> Bool
        equality a b = (T.toLower . viewedName vw) a == (T.toLower . viewedName vw) b
