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
import qualified RIO.NonEmpty as NE

-- | This function makes the classification diagram.
-- It focuses on generalizations and specializations.
clAnalysis :: FSpec -> ClassDiag
clAnalysis fSpec =
  OOclassdiagram
    { cdName = prependToPlainName "classification_" $ name fSpec,
      groups = [],
      classes = map clas . toList . concs . vgens $ fSpec,
      assocs = [],
      aggrs = [],
      geners = map OOGener . vgens $ fSpec,
      ooCpts = toList . concs $ fSpec
    }
  where
    clas :: A_Concept -> Class
    clas c =
      OOClass
        { clName = name c,
          clcpt = Just c,
          clAtts = (map makeAttr . attributesOfConcept fSpec) c,
          clMths = []
        }
    makeAttr :: SqlAttribute -> CdAttribute
    makeAttr att =
      OOAttr
        { attNm = sqlAttToName att,
          attTyp = if isProp (attExpr att) then propTypeName else (name . target . attExpr) att,
          attOptional = attNull att -- optional if NULL is allowed
        }

propTypeName :: Name
propTypeName = withNameSpace nameSpaceFormalAmpersand . mkName PropertyName $ toNamePart' (toText1Unsafe "Prop")

toNamePart' :: Text1 -> NonEmpty NamePart
toNamePart' x = toNamePart'' <$> splitOnDots x

toNamePart'' :: Text1 -> NamePart
toNamePart'' x = case toNamePart1 x of
  Nothing -> fatal $ "Not a valid NamePart: " <> tshow x
  Just np -> np

class CDAnalysable a where
  cdAnalysis :: Bool -> FSpec -> a -> ClassDiag
-- ^ This function, cdAnalysis, generates a conceptual data model.
-- It creates a class diagram in which generalizations and specializations remain distinct entity types.
-- This yields more classes than plugs2classdiagram does, as plugs contain their specialized concepts.
-- Properties and identities are not shown.
-- The first parameter (Bool) indicates wether or not the entities should be grouped by patterns.

buildClass :: FSpec -> A_Concept -> Class
buildClass fSpec root =
  case classOf fSpec root of
    Nothing -> fatal $ "Concept is not a class: `" <> fullName root <> "`."
    Just exprs ->
      OOClass
        { clName = name root,
          clcpt = Just root,
          clAtts = NE.toList $ fmap ooAttr exprs,
          clMths = []
        }

cptIsClass :: FSpec -> A_Concept -> Bool
cptIsClass fSpec cpt = isInScope cpt && hasClass cpt
  where
    isInScope _ = True
    hasClass = isJust . classOf fSpec

classOf :: FSpec -> A_Concept -> Maybe (NE.NonEmpty Expression)
classOf fSpec cpt =
  case filter isOfCpt . eqCl source $ attribs of -- an equivalence class wrt source yields the attributes that constitute an OO-class.
    [] -> Nothing
    [es] -> Just es
    _ -> fatal "Only one list of terms is expected here"
  where
    isOfCpt :: NE.NonEmpty Expression -> Bool
    isOfCpt es = source (NE.head es) == cpt
    attribs = fmap (flipWhenNeeded . EDcD) (attribDcls fSpec)
    flipWhenNeeded x = if isInj x && (not . isUni) x then flp x else x

ooAttr :: Expression -> CdAttribute
ooAttr r =
  OOAttr
    { attNm = case toList $ bindedRelationsIn r of
        [] -> fatal $ "No bindedRelations in expression: " <> tshow r
        h : _ -> name h,
      attTyp = if isProp r then propTypeName else (name . target) r,
      attOptional = (not . isTot) r
    }

attribDcls :: FSpec -> [Relation]
attribDcls fSpec = [d | d <- toList (vrels fSpec), isUni (EDcD d) || isInj (EDcD d)]

-- Aggregates are disabled for now, as the conditions we use to regard a relation as an aggregate still seem to be too weak
--   decl2assocOrAggr :: Relation -> Either Association Aggregation
--   decl2assocOrAggr d | isUni d && isTot d = Right $ OOAggr {aggDel = Close, aggChild = source d, aggParent = target d}
--   decl2assocOrAggr d | isInj d && isSur d = Right $ OOAggr {aggDel = Close, aggChild = target d, aggParent = source d}
decl2assocOrAggr :: Relation -> Either Association b
decl2assocOrAggr d =
  Left
    OOAssoc
      { assSrc = name $ source d,
        assSrcPort = name d,
        asslhm = mults . flp $ EDcD d,
        asslhr = Nothing,
        assTgt = name $ target d,
        assrhm = mults $ EDcD d,
        assrhr = Just $ name d,
        assmdcl = Just d
      }

dclIsShown :: FSpec -> [A_Concept] -> Relation -> Bool
dclIsShown fSpec nodeConcepts d =
  (not . isProp . EDcD) d
    && ( (d `notElem` attribDcls fSpec)
           || ( source d
                  `elem` nodeConcepts
                  && target d
                  `elem` nodeConcepts
                  && source d
                  /= target d
              )
       )

instance CDAnalysable Pattern where
  cdAnalysis _ fSpec pat =
    OOclassdiagram
      { cdName = prependToPlainName "logical_" $ name pat,
        groups = [],
        classes = map (buildClass fSpec) entities,
        assocs = lefts assocsAndAggrs,
        aggrs = rights assocsAndAggrs,
        geners = map OOGener (gens pat),
        ooCpts = toList (concs pat)
      }
    where
      entities = (filter (isJust . classOf fSpec) . toList . concs) pat
      assocsAndAggrs =
        ( map decl2assocOrAggr
            . filter (dclIsShown fSpec nodeConcepts)
            . toList
            . ptdcs
        )
          pat
      nodeConcepts = L.nub . concatMap (tyCpts . typologyOf fSpec) $ entities

instance CDAnalysable FSpec where
  cdAnalysis grouped _ fSpec =
    OOclassdiagram
      { cdName = prependToPlainName "logical_" $ name fSpec,
        groups = groups',
        classes = classes',
        assocs = lefts assocsAndAggrs,
        aggrs = rights assocsAndAggrs,
        geners = map OOGener (gens fSpec),
        ooCpts = toList (concs fSpec)
      }
    where
      groups' :: [(Name, NonEmpty Class)]
      (groups', classes')
        | grouped =
            ( [ ( name pat,
                  case classesOfPattern (Just pat) of
                    [] -> fatal "Shouldn't be empty here"
                    h : tl -> h :| tl
                )
                | pat :: Pattern <- instanceList fSpec,
                  let cls = classesOfPattern (Just pat),
                  not (null cls)
              ],
              classesOfPattern Nothing
            )
        | otherwise = ([], map (buildClass fSpec) entities)
      classesOfPattern :: Maybe Pattern -> [Class]
      classesOfPattern pat =
        map snd
          . filter ((==) pat . fst)
          . map (addPatternInfo . buildClass fSpec)
          $ entities
        where
          addPatternInfo :: Class -> (Maybe Pattern, Class)
          addPatternInfo cl = (patternOf cl, cl)
          patternOf :: Class -> Maybe Pattern
          patternOf cl =
            case clcpt cl of
              Nothing -> Nothing
              Just cpt -> case [ p | p :: Pattern <- instanceList fSpec, cdef <- ptcds p, name cdef == name cpt
                               ] of
                [] -> Nothing
                (h : _) -> Just h
      entities = (filter (cptIsClass fSpec) . toList . concs) fSpec
      assocsAndAggrs =
        ( map decl2assocOrAggr
            . filter (dclIsShown fSpec nodeConcepts)
            . toList
            . vrels
        )
          fSpec
      nodeConcepts = L.nub $ concatMap (tyCpts . typologyOf fSpec) entities

-- | This function generates a technical data model.
-- It is based on the plugs that are calculated.
tdAnalysis :: FSpec -> ClassDiag
tdAnalysis fSpec =
  OOclassdiagram
    { cdName = prependToPlainName "technical_" $ name fSpec,
      groups = [],
      classes = allClasses,
      assocs = allAssocs,
      aggrs = [],
      geners = [],
      ooCpts = roots
    }
  where
    allClasses =
      [ OOClass
          { clName = name . mainItem $ table,
            clcpt = primKey table,
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
                        attOptional = False -- A BinSQL contains pairs, so NULL cannot occur.
                      },
            clMths = []
          }
        | table <- tables,
          length (plugAttributes table) > 1
      ]

    tables = [pSql | InternalPlug pSql <- plugInfos fSpec]
    roots :: [A_Concept]
    roots = mapMaybe primKey tables
    primKey :: PlugSQL -> Maybe A_Concept
    primKey TblSQL {attributes = (f : _)} = Just (source (attExpr f))
    primKey _ = Nothing
    ooAtt :: [SqlAttribute] -> SqlAttribute -> CdAttribute
    ooAtt kernelAtts f =
      OOAttr
        { attNm = sqlAttToName f,
          attTyp =
            if isProp (attExpr f) && (f `notElem` kernelAtts)
              then propTypeName
              else (name . target . attExpr) f,
          attOptional = attNull f -- optional if NULL is allowed
        }
    allAssocs = concatMap (filter isAssocBetweenClasses . relsOf) tables
      where
        isAssocBetweenClasses a = let allClassNames = map clName allClasses in assSrc a `elem` allClassNames && assTgt a `elem` allClassNames
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
        relOf f =
          let expr = attExpr f
           in case expr of
                EDcI {} -> Nothing
                EEps {} -> Nothing
                EDcD d -> if target d `elem` kernelConcepts then Just (expr, f) else Nothing
                EFlp (EDcD d) -> if source d `elem` kernelConcepts then Just (expr, f) else Nothing
                _ -> fatal ("Unexpected expression: " <> tshow expr)
        mkRel :: PlugSQL -> (Expression, SqlAttribute) -> Association
        mkRel t (expr, f) =
          OOAssoc
            { assSrc = name . mainItem $ t,
              assSrcPort = sqlAttToName f,
              asslhm = (mults . flp) expr,
              asslhr = Just $ sqlAttToName f,
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
