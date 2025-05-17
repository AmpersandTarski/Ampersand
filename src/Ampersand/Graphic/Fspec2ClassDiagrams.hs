{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ampersand.Graphic.Fspec2ClassDiagrams
  ( clAnalysis,
    cdAnalysis,
    tdAnalysis,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.FSpec
import Ampersand.FSpec.ToFSpec.ADL2Plug
import Ampersand.FSpec.Transformers (nameSpaceFormalAmpersand)
import Ampersand.Graphic.ClassDiagram
import Ampersand.Misc.HasClasses
import Data.Tuple.Extra (fst3, snd3, thd3)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

-- | This function makes the classification diagram.
-- It focuses on generalizations and specializations.
clAnalysis :: FSpec -> ClassDiag
clAnalysis fSpec =
  OOclassdiagram
    { cdName = prependToPlainName "classification_" $ name fSpec,
      classes = map clas . toList . concs . vgens $ fSpec,
      assocs = [],
      geners = map OOGener . vgens $ fSpec,
      ooCpts = toList . concs $ fSpec
    }
  where
    clas :: A_Concept -> (Class, Maybe Name)
    clas root =
      ( OOClass
          { clName = name root,
            clcpt = Just (root, cptTType fSpec root),
            clAtts = (map makeAttr . attributesOfConcept fSpec) root
          },
        Nothing
      )
    makeAttr :: SqlAttribute -> CdAttribute
    makeAttr att =
      OOAttr
        { attNm = sqlAttToName att,
          attTyp = if isProp (attExpr att) then propTypeName else (name . target . attExpr) att,
          attOptional = attNull att, -- optional if NULL is allowed
          attProps = [Uni | isUni (attExpr att)] <> [Tot | isTot (attExpr att)]
        }

propTypeName :: Name
propTypeName = withNameSpace nameSpaceFormalAmpersand . mkName PropertyName $ toNamePart' (toText1Unsafe "Prop")

toNamePart' :: Text1 -> NonEmpty NamePart
toNamePart' x = toNamePart'' <$> splitOnDots x

toNamePart'' :: Text1 -> NamePart
toNamePart'' x = case toNamePart1 x of
  Nothing -> fatal $ "Not a valid NamePart: " <> tshow x
  Just np -> np

class (ConceptStructure a, Language a) => CDAnalysable a where
  {-# MINIMAL cdAnalysis, relations, classCandidates #-}

  -- | This function, cdAnalysis, generates a conceptual data model.
  -- It creates a class diagram in which generalizations and specializations remain distinct entity types.
  -- This yields more classes than plugs2classdiagram does, as plugs contain their specialized concepts.
  -- Properties and identities are not shown.
  -- The first parameter (Bool) indicates wether or not the entities should be grouped by patterns.
  cdAnalysis :: (HasDocumentOpts env) => Bool -> env -> FSpec -> a -> ClassDiag

  -- | This function returns the relations of the given a.
  -- It is used to filter the relations that are shown in the class diagram.
  relations :: a -> Relations

  -- | This function returns the concepts that could become a class, together with an identifying name
  --   for the group in which they may be grouped.
  classCandidates :: a -> [(A_Concept, Maybe Name)]

  classesAndAssociations :: (HasDocumentOpts env) => env -> FSpec -> a -> ([(Class, Maybe Name)], [Association])
  -- ^ This function returns all the classes in the given datamodel that should be drawn.
  --   Note: classes without attributes are included as well.
  -- The idea is as follows:
  --   - Concepts with an univalent attribute or with a generalisation relation must be drawn as separate class.
  --   - Relations that are UNI and/or INJ are drawn as attributes of the class they belong to. Additionally,
  --       If the relation is UNI and/or INJ, an edge is drawn between source and target.
  --   - Relations that are UNI nor INJ are drawn based on the fact if source and or target have attributes or generalisation, as follows:
  --     - If both the source and the target are, the relation is drawn as an association.
  --     - If only the source is, the relation is drawn as a multi-attribute of the source.
  --     - If only the target is, the relation is drawn as a multi-attribute of the target.
  --     - If neither the source nor the target is, the relation is drawn as an association,
  --        and the source and target are drawn as standalone class.
  --   - Concepts that are not in the source or target of any relation are drawn as a standalone class.
  classesAndAssociations env fSpec a =
    ( map (buildClass . addAttributes) mustBeDrawnAsClass,
      map
        rel2Association
        ( relations2draw
            <> (if view uniEdgesL env then additionalRelations else mempty)
        )
    )
    where
      mustBeDrawnAsClass = L.nub $ conceptsWithUniOrGens <> standalonConcepts
      uniOrInjs, nonUniOrInjs :: [Relation]
      (uniOrInjs, nonUniOrInjs) = L.partition criterium (toList $ relations a)
        where
          criterium d = isUni d || isInj d
      uniAttributes :: [Expression]
      uniAttributes = map (flipWhenNeeded . EDcD) uniOrInjs
        where
          flipWhenNeeded x = if isInj x && (not . isUni) x then flp x else x
      additionalRelations :: [Relation]
      additionalRelations =
        [ rel
          | rel <- uniOrInjs,
            source rel `elem` map fst mustBeDrawnAsClass,
            target rel `elem` map fst mustBeDrawnAsClass
        ]
      conceptsWithUniOrGens, conceptsWithoutUniOrGens :: [(A_Concept, Maybe Name)]
      (conceptsWithUniOrGens, conceptsWithoutUniOrGens) =
        L.partition (isConceptWithUniOrGen . fst) (toList $ classCandidates a)
        where
          isConceptWithUniOrGen :: A_Concept -> Bool
          isConceptWithUniOrGen cpt =
            isConceptWithGen cpt || isConceptWithUni cpt
          isConceptWithUni :: A_Concept -> Bool
          isConceptWithUni cpt = cpt `elem` map source uniAttributes
          isConceptWithGen :: A_Concept -> Bool
          isConceptWithGen cpt = cpt `elem` concs (gens a)

      multiAttributes :: [Expression]
      relations2draw :: [Relation]
      standalonConcepts :: [(A_Concept, Maybe Name)]
      (multiAttributes, relations2draw, standalonConcepts) =
        ( concatMap fst3 results,
          concatMap snd3 results,
          concatMap thd3 results
        )
        where
          results = map handleRelation nonUniOrInjs
          handleRelation :: Relation -> ([Expression], [Relation], [(A_Concept, Maybe Name)])
          handleRelation d =
            case (source d `elem` map fst conceptsWithUniOrGens, target d `elem` map fst conceptsWithUniOrGens) of
              (True, True) ->
                ([], [d], [])
              (True, False) ->
                ([EDcD d], [], [])
              (False, True) ->
                ([flp (EDcD d)], [], [])
              (False, False) ->
                ([], [d], filter srcOrtgt conceptsWithoutUniOrGens)
                where
                  srcOrtgt :: (A_Concept, Maybe Name) -> Bool
                  srcOrtgt (cpt, _) = cpt == source d || cpt == target d
      addAttributes :: (A_Concept, Maybe Name) -> (A_Concept, Maybe Name, [Expression])
      addAttributes (cpt, group) = (cpt, group, attribs)
        where
          attribs = filter ((cpt ==) . source) (uniAttributes <> multiAttributes)
      rel2Association :: Relation -> Association
      rel2Association rel =
        OOAssoc
          { assSrc = name $ source rel,
            assSrcPort = name rel,
            asslhm = mults . flp $ EDcD rel,
            asslhr = Nothing,
            assTgt = name $ target rel,
            assrhm = mults $ EDcD rel,
            assrhr = Just $ name rel,
            assmdcl = Just rel
          }

      buildClass :: (A_Concept, Maybe Name, [Expression]) -> (Class, Maybe Name)
      buildClass (root, mName, exprs) =
        ( OOClass
            { clName = name root,
              clcpt = cptWithTType fSpec <$> Just root,
              clAtts = fmap ooAttr exprs
            },
          mName
        )

ooAttr :: Expression -> CdAttribute
ooAttr r =
  OOAttr
    { attNm = case toList $ bindedRelationsIn r of
        [] -> fatal $ "No bindedRelations in expression: " <> tshow r
        h : _ -> name h,
      attTyp = if isProp r then propTypeName else (name . target) r,
      attOptional = (not . isTot) r,
      attProps = [Uni | isUni r] <> [Tot | isTot r]
    }

instance CDAnalysable Pattern where
  cdAnalysis _ env fSpec pat =
    OOclassdiagram
      { cdName = prependToPlainName "logical_" $ name pat,
        classes = classes',
        assocs = associations',
        geners = map OOGener (gens pat),
        ooCpts = toList (concs pat)
      }
    where
      (classes', associations') = classesAndAssociations env fSpec pat
  relations = ptdcs
  classCandidates :: Pattern -> [(A_Concept, Maybe Name)]
  classCandidates pat = map foo . toList . concs $ pat
    where
      foo :: A_Concept -> (A_Concept, Maybe Name)
      foo cpt =
        ( cpt,
          if cpt `elem` map acdcpt (ptcds pat)
            then Just (name pat)
            else Nothing
        )

instance CDAnalysable A_Context where
  cdAnalysis grouped env fSpec ctx =
    OOclassdiagram
      { cdName = prependToPlainName "logical_" $ name ctx,
        classes = map handleGrouping classes',
        assocs = associations',
        geners = map OOGener (gens ctx),
        ooCpts = toList (concs ctx)
      }
    where
      handleGrouping (cl, mName) = (cl, if grouped then mName else Nothing)
      (classes', associations') = classesAndAssociations env fSpec ctx
  relations = relsDefdIn
  classCandidates :: A_Context -> [(A_Concept, Maybe Name)]
  classCandidates ctx = map foo . toList . concs $ ctx
    where
      foo :: A_Concept -> (A_Concept, Maybe Name)
      foo cpt =
        ( cpt,
          case L.sort
            [ (cd, n) | (cd, n) <- cDefs, name cd == name cpt
            ] of
            [] -> Nothing
            [(_, n)] -> Just n
            ns ->
              fatal
                ( "A problem for drawing the logical datamodel:\nConcept "
                    <> tshow (name cpt)
                    <> " is defined in multiple patterns: "
                    <> (T.concat . L.intersperse "\n  " . map showIt $ ns)
                )
        )
        where
          showIt (cd, n) = tshow (origin cd) <> ": " <> tshow n
      cDefs :: [(AConceptDef, Name)]
      cDefs =
        [ (cd, name pat)
          | pat <- ctxpats ctx,
            cd <- ptcds pat
        ]

-- | This function generates a technical data model.
-- It is based on the plugs that are calculated.
tdAnalysis :: FSpec -> ClassDiag
tdAnalysis fSpec =
  OOclassdiagram
    { cdName = prependToPlainName "technical_" $ name fSpec,
      classes = allClasses',
      assocs = allAssocs,
      geners = [],
      ooCpts = roots
    }
  where
    allClasses' =
      [ ( OOClass
            { clName = name . mainItem $ table,
              clcpt = cptWithTType fSpec <$> primKey table,
              clAtts = case table of
                TblSQL {} ->
                  let kernelAtts = map snd $ cLkpTbl table -- extract kernel attributes from kernel lookup table
                   in map (ooAtt kernelAtts) kernelAtts
                        <> map (ooAtt kernelAtts . rsTrgAtt) (dLkpTbl table)
                BinSQL {} ->
                  NE.toList
                    $ fmap mkOOattr (plugAttributes table)
                  where
                    mkOOattr a =
                      OOAttr
                        { attNm = sqlAttToName a,
                          attTyp = (name . target . attExpr) a,
                          attOptional = False, -- A BinSQL contains pairs, so NULL cannot occur.
                          attProps = [Uni, Tot]
                        }
            },
          name <$> primKey table
        )
        | table <- tables,
          length (plugAttributes table) > 1
      ]

    tables = [pSql | InternalPlug pSql <- plugInfos fSpec]
    roots :: [A_Concept]
    roots = mapMaybe primKey tables
    primKey :: PlugSQL -> Maybe A_Concept
    primKey TblSQL {attributes = (att : _)} = Just (source (attExpr att))
    primKey _ = Nothing
    ooAtt :: [SqlAttribute] -> SqlAttribute -> CdAttribute
    ooAtt kernelAtts att =
      OOAttr
        { attNm = sqlAttToName att,
          attTyp =
            if isProp (attExpr att) && att `notElem` kernelAtts
              then propTypeName
              else (name . target . attExpr) att,
          attOptional = attNull att, -- optional if NULL is allowed
          attProps = [Uni | isUni (attExpr att)] <> [Tot | isTot (attExpr att)]
        }
    allAssocs = concatMap (filter isAssocBetweenClasses . relsOf) tables
      where
        isAssocBetweenClasses a = let allClassNames = map (clName . fst) allClasses' in assSrc a `elem` allClassNames && assTgt a `elem` allClassNames
        kernelConcepts = map fst (concatMap cLkpTbl tables)
        relsOf t =
          case t of
            TblSQL {} -> map (mkRel t) . mapMaybe relOf . attributes $ t
            BinSQL {} -> NE.toList $ fmap mkOOAssoc (plugAttributes t)
              where
                mkOOAssoc a =
                  OOAssoc
                    { assSrc = name . mainItem $ t,
                      assSrcPort = sqlAttToName a,
                      asslhm = Mult MinZero MaxMany,
                      asslhr = Nothing,
                      assTgt = name . mainItem . getConceptTableFor fSpec . target . attExpr $ a,
                      assrhm = Mult MinOne MaxOne,
                      assrhr = Nothing,
                      assmdcl = Nothing
                    }
        relOf att =
          let expr = attExpr att
           in case expr of
                EDcI {} -> Nothing
                EEps {} -> Nothing
                EDcD d -> if target d `elem` kernelConcepts then Just (expr, att) else Nothing
                EFlp (EDcD d) -> if source d `elem` kernelConcepts then Just (expr, att) else Nothing
                _ -> fatal ("Unexpected expression: " <> tshow expr)
        mkRel :: PlugSQL -> (Expression, SqlAttribute) -> Association
        mkRel t (expr, att) =
          OOAssoc
            { assSrc = name . mainItem $ t,
              assSrcPort = sqlAttToName att,
              asslhm = (mults . flp) expr,
              asslhr = Just $ sqlAttToName att,
              assTgt = name . mainItem . getConceptTableFor fSpec . target $ expr,
              assrhm = mults expr,
              assrhr = case toList . toList $ bindedRelationsIn expr of
                h : _ -> Just (name h)
                _ -> fatal "no relations used in expr",
              assmdcl = Nothing
            }

sqlAttToName :: SqlAttribute -> Name
sqlAttToName = mkName SqlAttributeName . toNamePart' . sqlColumNameToText1 . attSQLColName

mults :: Expression -> Multiplicities
mults r =
  Mult
    (if isTot r then MinOne else MinZero)
    (if isUni r then MaxOne else MaxMany)

cptWithTType :: FSpec -> A_Concept -> (A_Concept, TType)
cptWithTType fSpec cpt = (cpt, cptTType fSpec cpt)
