module Ampersand.Graphic.Fspec2ClassDiagrams (
  clAnalysis, cdAnalysis, tdAnalysis
) 
where
import Data.List
import Ampersand.ADL1
import Ampersand.Classes
import Ampersand.Basics
import Ampersand.FSpec
import Data.Maybe
import Data.Either
import Ampersand.Graphic.ClassDiagram

-- | This function makes the classification diagram.
-- It focuses on generalizations and specializations.
clAnalysis :: FSpec -> ClassDiag
clAnalysis fSpec =
    OOclassdiagram { cdName  = "classification_"++name fSpec
                   , classes = map classOf . concs . gensInScope $ fSpec
                   , assocs  = []
                   , aggrs   = []
                   , geners  = map OOGener . gensInScope $ fSpec
                   , ooCpts  = concs fSpec
                   }

 where
    classOf :: A_Concept -> Class
    classOf c = OOClass { clName = name c
                        , clcpt  = Just c
                        , clAtts = attrs c
                        , clMths = []
                        }
    attrs c    = [ makeAttr att 
                 | att<-tail (plugAttributes (getConceptTableFor fSpec c)), not (inKernel att), source (attExpr att)==c]
    makeAttr :: SqlAttribute -> CdAttribute
    makeAttr att 
              = OOAttr { attNm       = attName att
                       , attTyp      = if isPropty att then "Prop" else (name.target.attExpr) att
                       , attOptional = attNull att
                       }
    inKernel :: SqlAttribute -> Bool
    inKernel att = null([Uni,Inj,Sur]>-properties (attExpr att)) && not (isPropty att)
    isPropty att = isProp (attExpr att)

-- | This function, cdAnalysis, generates a conceptual data model.
-- It creates a class diagram in which generalizations and specializations remain distinct entity types.
-- This yields more classes than plugs2classdiagram does, as plugs contain their specialized concepts.
-- Properties and identities are not shown.
cdAnalysis :: FSpec -> ClassDiag
cdAnalysis fSpec =
  OOclassdiagram { cdName  = "logical_"++name fSpec
                 , classes = map buildClass 
                           . filter cptIsShown
                           . allConcepts $ fSpec
                 , assocs  = lefts assocsAndAggrs
                 , aggrs   = rights assocsAndAggrs
                 , geners  = map OOGener (gensInScope fSpec)
                 , ooCpts  = filter cptIsShown
                           . allConcepts $ fSpec
                 }

 where
   buildClass :: A_Concept -> Class
   buildClass root 
     = case classOf root of
         Nothing -> fatal 67 $ "Concept is not a class: `"++name root++"`."
         Just exprs ->
           OOClass { clName = name root
                   , clcpt  = Just root
                   , clAtts = map ooAttr exprs
                   , clMths = []
                   }
   cptIsShown :: A_Concept -> Bool
   cptIsShown cpt = isInScope cpt && hasClass cpt
     where 
      isInScope _ = True 
      hasClass = isJust . classOf
   classOf :: A_Concept -> Maybe [Expression]
   classOf cpt = 
     case filter isOfCpt . eqCl source $ attribs of -- an equivalence class wrt source yields the attributes that constitute an OO-class.
        []   -> Nothing
        [es] -> Just es
        _    -> fatal 82 "Only one list of expressions is expected here"
     where
      isOfCpt :: [Expression] -> Bool
      isOfCpt []    = fatal 85 "List must not be empty!"
      isOfCpt (e:_) = source e == cpt
      attribs = [ if isInj d && (not . isUni) d then flp (EDcD d) else EDcD d | d<-attribDcls ]

   dclIsInScope :: Declaration -> Bool
   dclIsInScope dcl =
      dcl `elem` (topLevelDcls `uni` pattInScopeDcls)
     where   
       topLevelDcls = vrels fSpec \\ (concatMap relsDefdIn . vpatterns $ fSpec)
       pattInScopeDcls = nub . concatMap dclsInPat . pattsInScope $ fSpec
          where 
            dclsInPat :: Pattern -> [Declaration]
            dclsInPat p = relsDefdIn p `uni` relsMentionedIn p
   ooAttr :: Expression -> CdAttribute
   ooAttr r = OOAttr { attNm = (name . head . relsMentionedIn) r
                     , attTyp = if isProp r then "Prop" else (name.target) r
                     , attOptional = (not.isTot) r
                     }
   allDcls = filter dclIsInScope . vrels $ fSpec
   assocsAndAggrs = map decl2assocOrAggr 
                  . filter dclIsShown $ allDcls
     where
       dclIsShown :: Declaration -> Bool
       dclIsShown d = 
             (not . isProp) d
          && (   (d `notElem` attribDcls)
              || (   source d `elem` nodeConcepts
                  && target d `elem` nodeConcepts
                  && source d /= target d
                 )
             )      
          where nodeConcepts = concatMap (tyCpts . typologyOf fSpec)
                             . filter cptIsShown
                             . allConcepts $ fSpec
                            

   -- Aggregates are disabled for now, as the conditions we use to regard a relation as an aggregate still seem to be too weak
--   decl2assocOrAggr :: Declaration -> Either Association Aggregation
--   decl2assocOrAggr d | isUni d && isTot d = Right $ OOAggr {aggDel = Close, aggChild = source d, aggParent = target d}
--   decl2assocOrAggr d | isInj d && isSur d = Right $ OOAggr {aggDel = Close, aggChild = target d, aggParent = source d}
   decl2assocOrAggr d = Left
     OOAssoc { assSrc = name $ source d
             , assSrcPort = name d
             , asslhm = mults . flp $ EDcD d
             , asslhr = ""
             , assTgt = name $ target d
             , assrhm = mults d
             , assrhr = name d
             , assmdcl = Just d
             }
   attribDcls = [ d | d <- allDcls, isUni d || isInj d ]
    

-- | This function generates a technical data model.
-- It is based on the plugs that are calculated.
tdAnalysis :: FSpec -> ClassDiag
tdAnalysis fSpec =
  OOclassdiagram {cdName  = "technical_"++name fSpec
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
                                ++map (ooAttr kernelAtts . rsTrgAtt) (dLkpTbl table) 
                            BinSQL{}      ->
                              map mkOOattr (plugAttributes table)
                                where mkOOattr a =
                                        OOAttr { attNm       = attName a
                                               , attTyp      = (name.target.attExpr) a
                                               , attOptional = False
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
            , attOptional = attNull f
            }
   allAssocs = filter isAssocBetweenClasses $ concatMap relsOf tables
     where
       isAssocBetweenClasses a = let allClassNames = map clName allClasses in assSrc a `elem` allClassNames && assTgt a `elem` allClassNames
       
       kernelConcepts = map fst (concatMap cLkpTbl tables)

       relsOf t =
         case t of
           TblSQL{} -> map (mkRel t) . mapMaybe relOf . attributes $ t
           BinSQL{} -> map mkOOAssoc (plugAttributes t)
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
           _ -> fatal 200 ("Unexpected expression: "++show expr)
       mkRel :: PlugSQL -> (Expression,SqlAttribute) -> Ampersand.Graphic.ClassDiagram.Association
       mkRel t (expr,f) =
            OOAssoc { assSrc = sqlname t
                    , assSrcPort = attName f
                    , asslhm = (mults.flp) expr
                    , asslhr = attName f
                    , assTgt = name . getConceptTableFor fSpec . target $ expr
                    , assrhm = mults expr
                    , assrhr = case [name d | d@Sgn{}<-relsMentionedIn expr] of h:_ -> h ; _ -> fatal 229 "no relations used in expr"
                    , assmdcl = Nothing
                    }

----             
mults :: Relational r => r -> Multiplicities
mults r = let minVal = if isTot r then MinOne else MinZero
              maxVal = if isUni r then MaxOne else MaxMany
          in  Mult minVal maxVal
             