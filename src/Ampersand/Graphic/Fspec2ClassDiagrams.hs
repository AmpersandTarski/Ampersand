{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Graphic.Fspec2ClassDiagrams (
  clAnalysis, cdAnalysis, tdAnalysis
) 
where
import           Ampersand.ADL1
import           Ampersand.Basics
import           Ampersand.Classes
import           Ampersand.FSpec
import           Ampersand.FSpec.ToFSpec.ADL2Plug
import           Ampersand.Graphic.ClassDiagram
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set

-- | This function makes the classification diagram.
-- It focuses on generalizations and specializations.
clAnalysis :: FSpec -> ClassDiag
clAnalysis fSpec =
    OOclassdiagram { cdName  = "classification_"<>name fSpec
                   , classes = map classOf . Set.elems . concs . vgens $ fSpec
                   , assocs  = []
                   , aggrs   = []
                   , geners  = map OOGener . vgens $ fSpec
                   , ooCpts  = Set.elems . concs $ fSpec
                   }
 where
   classOf :: A_Concept -> Class
   classOf c = OOClass { clName = name c
                       , clcpt  = Just c
                       , clAtts = (map makeAttr . attributesOfConcept fSpec) c
                       , clMths = []
                       }
   makeAttr :: SqlAttribute -> CdAttribute
   makeAttr att = OOAttr { attNm       = attName att
                         , attTyp      = if isProp (attExpr att) then "Prop" else (name.target.attExpr) att
                         , attOptional = attNull att -- optional if NULL is allowed
                         }

-- | This function, cdAnalysis, generates a conceptual data model.
-- It creates a class diagram in which generalizations and specializations remain distinct entity types.
-- This yields more classes than plugs2classdiagram does, as plugs contain their specialized concepts.
-- Properties and identities are not shown.
cdAnalysis :: FSpec -> ClassDiag
cdAnalysis fSpec =
  OOclassdiagram { cdName  = "logical_"<>name fSpec
                 , classes = map buildClass 
                           . filter cptIsShown
                           . Set.elems 
                           . allConcepts $ fSpec
                 , assocs  = lefts assocsAndAggrs
                 , aggrs   = rights assocsAndAggrs
                 , geners  = map OOGener (vgens fSpec)
                 , ooCpts  = filter cptIsShown
                           . Set.elems 
                           . allConcepts $ fSpec
                 }

 where
   buildClass :: A_Concept -> Class
   buildClass root 
     = case classOf root of
         Nothing -> fatal $ "Concept is not a class: `"<>name root<>"`."
         Just exprs ->
           OOClass { clName = name root
                   , clcpt  = Just root
                   , clAtts = NE.toList $ fmap ooAttr exprs
                   , clMths = []
                   }
   cptIsShown :: A_Concept -> Bool
   cptIsShown cpt = isInScope cpt && hasClass cpt
     where 
      isInScope _ = True 
      hasClass = isJust . classOf
   classOf :: A_Concept -> Maybe (NE.NonEmpty Expression)
   classOf cpt = 
     case filter isOfCpt . eqCl source $ attribs of -- an equivalence class wrt source yields the attributes that constitute an OO-class.
        []   -> Nothing
        [es] -> Just es
        _    -> fatal "Only one list of expressions is expected here"
     where
      isOfCpt :: NE.NonEmpty Expression -> Bool
      isOfCpt es = source (NE.head es) == cpt
      attribs = fmap (flipWhenNeeded . EDcD) attribDcls
      flipWhenNeeded x = if isInj x && (not.isUni) x then flp x else x
   ooAttr :: Expression -> CdAttribute
   ooAttr r = OOAttr { attNm = case Set.elems $ bindedRelationsIn r of
                                []  -> fatal $ "No bindedRelations in an expression: " <> tshow r
                                h:_ -> name h
                     , attTyp = if isProp r then "Prop" else (name.target) r
                     , attOptional = (not.isTot) r
                     }
   allDcls = vrels $ fSpec
   assocsAndAggrs = map decl2assocOrAggr 
                  . filter dclIsShown 
                  . Set.elems $ allDcls
     where
       dclIsShown :: Relation -> Bool
       dclIsShown d = 
             (not . isProp . EDcD) d
          && (   (d `notElem` attribDcls)
              || (   source d `elem` nodeConcepts
                  && target d `elem` nodeConcepts
                  && source d /= target d
                 )
             )      
          where nodeConcepts = concatMap (tyCpts . typologyOf fSpec)
                             . filter cptIsShown
                             . Set.elems 
                             . allConcepts $ fSpec
                            

   -- Aggregates are disabled for now, as the conditions we use to regard a relation as an aggregate still seem to be too weak
--   decl2assocOrAggr :: Relation -> Either Association Aggregation
--   decl2assocOrAggr d | isUni d && isTot d = Right $ OOAggr {aggDel = Close, aggChild = source d, aggParent = target d}
--   decl2assocOrAggr d | isInj d && isSur d = Right $ OOAggr {aggDel = Close, aggChild = target d, aggParent = source d}
   decl2assocOrAggr d = Left
     OOAssoc { assSrc = name $ source d
             , assSrcPort = name d
             , asslhm = mults . flp $ EDcD d
             , asslhr = ""
             , assTgt = name $ target d
             , assrhm = mults $ EDcD d
             , assrhr = name d
             , assmdcl = Just d
             }
   
   attribDcls = [ d | d <- Set.elems allDcls, isUni (EDcD d) || isInj (EDcD d) ]
    

-- | This function generates a technical data model.
-- It is based on the plugs that are calculated.
tdAnalysis :: FSpec -> ClassDiag
tdAnalysis fSpec =
  OOclassdiagram {cdName  = "technical_"<>name fSpec
                 ,classes = allClasses
                 ,assocs  = allAssocs
                 ,aggrs   = []
                 ,geners  = []
                 ,ooCpts  = roots
                 }
  where
   allClasses =
      [ OOClass{ clName = sqlname table
               , clcpt  = primKey table
               , clAtts = case table of
                            TblSQL{} -> 
                              let kernelAtts = map snd $ cLkpTbl table -- extract kernel attributes from kernel lookup table
                              in  map (ooAttr kernelAtts) kernelAtts
                                <>map (ooAttr kernelAtts . rsTrgAtt) (dLkpTbl table) 
                            BinSQL{}      -> NE.toList $
                              fmap mkOOattr (plugAttributes table)
                                where mkOOattr a =
                                        OOAttr { attNm       = attName a
                                               , attTyp      = (name.target.attExpr) a
                                               , attOptional = False -- A BinSQL contains pairs, so NULL cannot occur.
                                               }
               , clMths = []
               }
      | table <- tables
      , length (plugAttributes table) > 1
      ]

   tables = [ pSql | InternalPlug pSql <- plugInfos fSpec]
   roots :: [A_Concept]
   roots = mapMaybe primKey tables
   primKey :: PlugSQL -> Maybe A_Concept
   primKey TblSQL{attributes=(f:_)} = Just (source (attExpr f))
   primKey _                    = Nothing
   ooAttr :: [SqlAttribute] -> SqlAttribute -> CdAttribute
   ooAttr kernelAtts f =
     OOAttr { attNm = attName f
            , attTyp = if isProp (attExpr f) && (f `notElem` kernelAtts)
                       then "Prop"
                       else (name.target.attExpr) f
            , attOptional = attNull f -- optional if NULL is allowed
            }
   allAssocs = filter isAssocBetweenClasses $ concatMap relsOf tables
     where
       isAssocBetweenClasses a = let allClassNames = map clName allClasses in assSrc a `elem` allClassNames && assTgt a `elem` allClassNames
       
       kernelConcepts = map fst (concatMap cLkpTbl tables)

       relsOf t =
         case t of
           TblSQL{} -> map (mkRel t) . mapMaybe relOf . attributes $ t
           BinSQL{} -> NE.toList $ fmap mkOOAssoc (plugAttributes t)
                        where mkOOAssoc a =
                                OOAssoc { assSrc = sqlname t
                                        , assSrcPort = attName a
                                        , asslhm = Mult MinZero MaxMany
                                        , asslhr = ""
                                        , assTgt = name . getConceptTableFor fSpec . target . attExpr $ a
                                        , assrhm = Mult MinOne MaxOne
                                        , assrhr = ""
                                        , assmdcl = Nothing
                                        }
       relOf f =
         let expr = attExpr f in
         case expr of
           EDcI{} -> Nothing
           EEps{} -> Nothing
           EDcD d -> if target d `elem` kernelConcepts then Just (expr,f) else Nothing
           EFlp (EDcD d) -> if source d `elem` kernelConcepts then Just (expr,f) else Nothing
           _ -> fatal ("Unexpected expression: "<>tshow expr)
       mkRel :: PlugSQL -> (Expression,SqlAttribute) -> Association
       mkRel t (expr,f) =
            OOAssoc { assSrc = sqlname t
                    , assSrcPort = attName f
                    , asslhm = (mults.flp) expr
                    , asslhr = attName f
                    , assTgt = name . getConceptTableFor fSpec . target $ expr
                    , assrhm = mults expr
                    , assrhr = case [name d | d<-Set.elems $ bindedRelationsIn expr] of h:_ -> h ; _ -> fatal "no relations used in expr"
                    , assmdcl = Nothing
                    }

mults :: Expression -> Multiplicities
mults r = Mult (if isTot r then MinOne else MinZero)
               (if isUni r then MaxOne else MaxMany)
          