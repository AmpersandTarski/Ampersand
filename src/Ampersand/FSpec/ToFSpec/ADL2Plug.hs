module Ampersand.FSpec.ToFSpec.ADL2Plug
  ( makeGeneratedSqlPlugs,
    typologies,
    suitableAsKey,
    attributesOfConcept,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.ToFSpec.Populated (sortSpecific2Generic)
import Ampersand.Misc.HasClasses
import Data.Hashable (hash)
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T

maxLengthOfDatabaseTableName :: Int
maxLengthOfDatabaseTableName = 64

shaLength :: Int
shaLength = 7

attributesOfConcept :: FSpec -> A_Concept -> [SqlAttribute]
attributesOfConcept fSpec c =
  [att | att <- NE.tail (plugAttributes (getConceptTableFor fSpec c)), not (inKernel att), source (attExpr att) == c]
  where
    inKernel :: SqlAttribute -> Bool
    inKernel att =
      isUni expr
        && isInj expr
        && isSur expr
        && (not . isProp) expr
      where
        expr = attExpr att

--was : null(Set.fromList [Uni,Inj,Sur]Set.\\properties (attExpr att)) && not (isPropty att)

makeGeneratedSqlPlugs ::
  (HasFSpecGenOpts env) =>
  env ->
  A_Context ->
  [PlugSQL]

-- | Sql plugs database tables. A database table contains the administration of a set of concepts and relations.
--   if the set contains no concepts, a linktable is created.
makeGeneratedSqlPlugs env context = map makeTable components
  where
    components :: [(Maybe Typology, [Relation])]
    components =
      map componentsForTypology (typologies context)
        <> (map componentsForOrphanRelation . filter isOrphan $ allRelationsInContext)
      where
        componentsForTypology typol =
          (Just typol, filter (relationBelongsToConceptTable typol) allRelationsInContext)
        componentsForOrphanRelation rel = (Nothing, [rel])
        isOrphan = isNothing . conceptTableOf
        relationBelongsToConceptTable :: Typology -> Relation -> Bool
        relationBelongsToConceptTable typol rel =
          case conceptTableOf rel of
            Nothing -> False
            Just x -> x `elem` tyCpts typol

    repr = representationOf (ctxInfo context)
    allRelationsInContext = toList (relsDefdIn context)
    allConceptsInContext = toList (concs context)
    allKeyConcepts = map tyroot . mapMaybe fst $ components
    allNameClasses = map toList $ eqClass equality allKeyConcepts
      where
        equality a b = (T.toLower . fullName) a == (T.toLower . fullName) b

    makeTable :: (Maybe Typology, [Relation]) -> PlugSQL
    makeTable (mTypol, rels) = case (mTypol, rels) of
      (Nothing, []) -> fatal "At least a typology or a relation must be present to build a table."
      (Nothing, [rel]) -> makeLinkTable rel
      (Nothing, _) -> fatal "Cannot build a link table with more than one relation."
      (Just typol, _) -> makeConceptTable typol rels
    makeConceptTable :: Typology -> [Relation] -> PlugSQL
    makeConceptTable typol allRelationsInTable =
      TblSQL
        { sqlname = determineTableName tableKey,
          attributes = map cptAttrib allConceptsInTable <> map dclAttrib allRelationsInTable,
          cLkpTbl = conceptLookuptable,
          dLkpTbl = dclLookuptable
        }
      where
        mustBeDisambiguated :: A_Concept -> Bool
        mustBeDisambiguated cpt = case filter (cpt `elem`) allNameClasses of
          [x] -> length x == 1
          _ -> fatal "Concept must be found exactly in one list."
        allConceptsInTable =
          -- All concepts from the typology, orderd from generic to specific
          reverse $ sortSpecific2Generic (gens context) (tyCpts typol)

        determineTableName :: A_Concept -> Name
        determineTableName keyConcept =
          -- TODO: Notes van Michiel
          --   The sql table name of a wide table is ideally the full name of the root concept, if it is not too long.
          --   For link tables, it is ideally the full name of the relation. However, this
          --   could lead to name conflicts, as the sql table name of all tables must be unique. In those cases,
          --   names that would conflict are postfixed with the first 7 digits of a hash that is constructed on uniquely
          --   identification of the concept or relation.
          if (T.length . fullName $ keyConcept) > maxLengthOfDatabaseTableName || mustBeDisambiguated keyConcept
            then name keyConcept
            else
              fatal $
                "Your name is too long or unambiguate to create a valid database name." <> "\n  "
                  <> fullName keyConcept
          where
            -- mkName SqlTableName ....
            -- (take (maxLengthOfDatabaseTableName - shaLength - 1) . fullName $ keyConcept)
            -- <> "_"
            -- <> gitLikeSha keyConcept

            namePart = case toNamePart1 determineTableNameText of
              Nothing -> fatal $ "Not a valid namepart: " <> text1ToText determineTableNameText
              Just np -> np
            determineTableNameText :: Text1
            determineTableNameText =
              if hasUnambigousLocalName (map toConceptOrRelation allConceptsInContext <> map toConceptOrRelation allRelationsInContext) keyConcept
                then classifierOf . toConceptOrRelation $ keyConcept
                else disambiguatedLocalName keyConcept
        tableArtefacts = map toConceptOrRelation allConceptsInTable <> map toConceptOrRelation allRelationsInTable

        tableKey = tyroot typol
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
            fatal $
              "Concept `" <> fullName cpt <> "` is not in the lookuptable."
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
            { attSQLColName = determineAttributeName tableArtefacts cpt,
              attExpr = expr,
              attType = repr cpt,
              attUse =
                if cpt == tableKey
                  && repr cpt == Object -- For scalars, we do not want a primary key. This is a workaround fix for issue #341
                  then PrimaryKey cpt
                  else PlainAttr,
              attNull = cpt /= tableKey, -- column for specializations can be NULL, but not the first column (tableKey)
              attDBNull = cpt /= tableKey, -- column for specializations can be NULL, but not the first column (tableKey)
              attUniq = True,
              attFlipped = False
            }
          where
            expr =
              if cpt == tableKey
                then EDcI cpt
                else EEps cpt (Sign tableKey cpt)
        dclAttrib :: Relation -> SqlAttribute
        dclAttrib dcl =
          Att
            { attSQLColName = determineAttributeName tableArtefacts dcl,
              attExpr = dclAttExpression,
              attType = repr (target dclAttExpression),
              attUse =
                if suitableAsKey . repr . target $ dclAttExpression
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
    makeLinkTable :: Relation -> PlugSQL
    makeLinkTable dcl =
      BinSQL
        { sqlname = name dcl,
          cLkpTbl = [], --TODO: in case of TOT or SUR you might use a binary plug to lookup a concept (don't forget to nub)
          --given that dcl cannot be (UNI or INJ) (because then dcl would be in a TblSQL plug)
          --if dcl is TOT, then the concept (source dcl) is stored in this plug
          --if dcl is SUR, then the concept (target dcl) is stored in this plug
          dLkpTbl = [theRelStore]
        }
      where
        bindedExp :: Expression
        bindedExp = EDcD dcl
        theRelStore =
          RelStore
            { rsDcl = dcl,
              rsStoredFlipped = isStoredFlipped dcl,
              rsSrcAtt = if isStoredFlipped dcl then trgAtt else srcAtt,
              rsTrgAtt = if isStoredFlipped dcl then srcAtt else trgAtt
            }
        --the expr for the domain of r
        domExpr
          | isTot bindedExp = EDcI (source bindedExp)
          | isSur bindedExp = EDcI (target bindedExp)
          | otherwise = EDcI (source bindedExp) ./\. (bindedExp .:. flp bindedExp)
        --the expr for the codomain of r
        codExpr
          | not (isTot bindedExp) && isSur bindedExp = flp bindedExp
          | otherwise = bindedExp
        srcAtt =
          Att
            { attSQLColName = text1ToSqlColumName $ fullName1 . (if isEndo dcl then prependToPlainName "Src" else id) . name . source $ codExpr,
              attExpr = domExpr,
              attType = repr (source domExpr),
              attUse =
                if suitableAsKey . repr . source $ domExpr
                  then ForeignKey (source domExpr)
                  else PlainAttr,
              attNull = False, -- false for link tables
              attDBNull = False, -- false for link tables
              attUniq = isUni codExpr,
              attFlipped = isStoredFlipped dcl
            }
        trgAtt =
          Att
            { attSQLColName = text1ToSqlColumName $ fullName1 . (if isEndo dcl then prependToPlainName "Tgt" else id) . name . target $ codExpr,
              attExpr = codExpr,
              attType = repr (target codExpr),
              attUse =
                if suitableAsKey . repr . target $ codExpr
                  then ForeignKey (target codExpr)
                  else PlainAttr,
              attNull = False, -- false for link tables
              attDBNull = False, -- false for link tables
              attUniq = isInj codExpr,
              attFlipped = isStoredFlipped dcl
            }
    conceptTableOf :: Relation -> Maybe A_Concept
    conceptTableOf = fst . wayToStore env
    isStoredFlipped :: Relation -> Bool
    isStoredFlipped = snd . wayToStore env

-- | this function tells how a given relation is to be stored. If stored
--   in a concept table, it returns that concept. It returns a boolean
--   that tells wether or not the relation is stored flipped.
wayToStore :: (HasFSpecGenOpts env) => env -> Relation -> (Maybe A_Concept, Bool)
wayToStore env dcl
  | view sqlBinTablesL env = (Nothing, False) -- binary tables only
  | isUni (EDcD dcl) = (Just $ source d, False) -- to concept table, plain
  | isInj (EDcD dcl) = (Just $ target d, True) -- to concept table, flipped
  | otherwise = (Nothing, not (isTot d) && isSur d) -- to link-table
  -- The order of columns in a linked table could
  -- potentially speed up queries, in cases where
  -- the relation is TOT or SUR. In that case there
  -- should be no need to look in the concept table,
  -- for all atoms are in the first colum of the link table
  where
    d = EDcD dcl

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
    TypeOfOne -> fatal "ONE has no key at all. does it?"

typologies :: A_Context -> [Typology]
typologies context =
  (multiKernels . ctxInfo $ context)
    <> [ Typology
           { tyroot = c,
             tyCpts = [c]
           }
         | c <- toList $ concs context Set.\\ concs (gens context)
       ]

-- | ConceptOrRelation is ment to be things that can end up in a database. It is designed
-- to have Concepts and Relations as instances.
type ConceptOrRelation = Either A_Concept Relation

class Named a => TableArtefacts a where
  hasUnambigousLocalName :: [ConceptOrRelation] -> a -> Bool
  hasUnambigousLocalName artefacts x = case filter hasSameClassifier artefacts of
    [] -> fatal "x should have the same classifier as x!"
    [_] -> True
    _ -> False
    where
      hasSameClassifier :: ConceptOrRelation -> Bool
      hasSameClassifier y = classifierOf (toConceptOrRelation x) == classifierOf y
  toConceptOrRelation :: a -> ConceptOrRelation

  disambiguatedLocalName :: a -> Text1
  disambiguatedLocalName x =
    toText1Unsafe $
      (namePartToText . localName) x
        <> "_"
        <> gitLikeSha x
  gitLikeSha :: a -> Text
  gitLikeSha = T.take shaLength . tshow . abs . hash . hashText
  hashText :: a -> Text
  determineAttributeName :: [ConceptOrRelation] -> a -> SqlColumName
  determineAttributeName tableArtefacts x = text1ToSqlColumName determineAttributeNameText
    where
      determineAttributeNameText :: Text1
      determineAttributeNameText =
        if hasUnambigousLocalName tableArtefacts x
          then classifierOf . toConceptOrRelation $ x
          else disambiguatedLocalName x

classifierOf :: ConceptOrRelation -> Text1
classifierOf = toText1Unsafe . T.toLower . namePartToText . either localName localName

instance TableArtefacts A_Concept where
  toConceptOrRelation = Left
  hashText = tshow . fullName1

instance TableArtefacts Relation where
  toConceptOrRelation = Right
  hashText rel =
    (tshow . fullName1) rel
      <> (tshow . fullName1 . source) rel
      <> (tshow . fullName1 . target) rel
