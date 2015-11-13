module Database.Design.Ampersand.Graphic.Fspec2ClassDiagrams (
  clAnalysis, cdAnalysis, tdAnalysis
) 
where
import Data.List
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.FSpec
import Database.Design.Ampersand.FSpec.FSpec(getConceptTableFor)
import Data.Maybe
import Data.Either
import Database.Design.Ampersand.Graphic.ClassDiagram

-- | This function makes the classification diagram.
-- It focuses on generalizations and specializations.
clAnalysis :: FSpec -> ClassDiag
clAnalysis fSpec =
    OOclassdiagram { cdName  = "classification_"++name fSpec
                   , classes = [ OOClass { clName = name c
                                         , clcpt  = Just c
                                         , clAtts = attrs c
                                         , clMths = []
                                         } | c<-cpts]
                   , assocs  = []
                   , aggrs   = []
                   , geners  = map OOGener (gensInScope fSpec)
                   , ooCpts  = concs fSpec
                   }

 where
    cpts       = concs (gensInScope fSpec)
    attrs c    = [ OOAttr (attName att) (if isPropty att then "Bool" else  (name.target.attExpr) att) (attNull att)
                 | plug<-lookup' c, att<-tail (plugAttributes plug), not (inKernel att), source (attExpr att)==c]
                 where inKernel att = null([Uni,Inj,Sur]>-properties (attExpr att)) && not (isPropty att)
    lookup' c = [plug |InternalPlug plug@TblSQL{}<-plugInfos fSpec , (c',_)<-cLkpTbl plug, c'==c]
    isPropty att = null([Sym,Asy]>-properties (attExpr att))

-- | This function, cdAnalysis, generates a conceptual data model.
-- It creates a class diagram in which generalizations and specializations remain distinct entity types.
-- This yields more classes than plugs2classdiagram does, as plugs contain their specialized concepts.
-- Properties and identities are not shown.
cdAnalysis :: FSpec -> ClassDiag
cdAnalysis fSpec =
  OOclassdiagram { cdName  = "logical_"++name fSpec
                 , classes =
                     [ OOClass{ clName = name root
                              , clcpt  = Just root
                              , clAtts = map ooAttr ooClass
                              , clMths = []
                              }
                     | ooClass <- ooClasses, let root=source (head ooClass)]
                 , assocs  = lefts assocsAndAggrs
                 , aggrs   = rights assocsAndAggrs
                 , geners  = map OOGener (gensInScope fSpec)
                 , ooCpts  = roots
                 }

 where
   ooAttr :: Expression -> CdAttribute
   ooAttr r = OOAttr { attNm = (name . head . relsMentionedIn) r
                     , attTyp = if isPropty r then "Bool" else (name.target) r
                     , attOptional = (not.isTot) r
                     }
   isPropty r = null([Sym,Asy]>-properties r)
   topLevelDcls = vrels fSpec \\
                  (concatMap relsDefdIn (vpatterns fSpec))
   allDcls = topLevelDcls `uni`
             [ d -- restricted to those themes that must be printed.
             | d@Sgn{} <- nub . concat $
                            [relsDefdIn p ++ relsMentionedIn p  | p <- pattsInScope fSpec ] 
             ]
   assocsAndAggrs = [ decl2assocOrAggr d
                    | d <- allDcls
                    , not.isPropty $ d
               {- SJ 20150416: the following restriction prevents printing attribute-relations to empty boxes.
               -}
                    , d `notElem` attribDcls  ||
                      ( source d `elem` nodeConcepts && target d `elem` nodeConcepts && source d/= target d )
                    ] where family c = [c] ++ specializationsOf fSpec c ++ generalizationsOf fSpec c
                            nodeConcepts = concatMap family roots
                            

   -- Aggregates are disabled for now, as the conditions we use to regard a relation as an aggregate still seem to be too weak
--   decl2assocOrAggr :: Declaration -> Either Association Aggregation
   --decl2assocOrAggr d | isUni d && isTot d = Right $ OOAggr {aggDel = Close, aggChild = source d, aggParent = target d}
   --decl2assocOrAggr d | isInj d && isSur d = Right $ OOAggr {aggDel = Close, aggChild = target d, aggParent = source d}
   decl2assocOrAggr d | otherwise          = Left $
     OOAssoc { assSrc = name $ source d
             , assSrcPort = name d
             , asslhm = mults . flp $ EDcD d
             , asslhr = ""
             , assTgt = name $ target d
             , assrhm = mults d
             , assrhr = name d
             , assmdcl = Just d
             }
   attribDcls = [ d | d <- allDcls, Aut `notElem` properties d, isUni d || isInj d ]
   attribs = [ if isInj d then flp (EDcD d) else EDcD d | d<-attribDcls ]
   ooClasses = eqCl source attribs      -- an equivalence class wrt source yields the attributes that constitute an OO-class.
   roots = map (source.head) ooClasses

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
                            TblSQL{attributes=attribs, cLkpTbl=kernelLookupTbl, mLkpTbl=t} -> 
                              let kernelAtts = map snd $ kernelLookupTbl -- extract kernel attributes from kernel lookup table
                              in  map (ooAttr kernelAtts . lookInFor t . attExpr) attribs
                            BinSQL{columns=(a,b)}      ->
                              [ OOAttr { attNm       = attName a
                                       , attTyp      = (name.target.attExpr) a
                                       , attOptional = False
                                       }
                              , OOAttr { attNm       = attName b
                                       , attTyp      = (name.target.attExpr) b
                                       , attOptional = False
                                       }
                              ]
                            _     -> fatal 166 "Unexpected type of table!"
               , clMths = []
               }
      | table <- tables
      , length (plugAttributes table) > 1
      ]

   lookInFor [] _ = fatal 191 "Expression not found!"
   lookInFor ((expr,_,t):xs) a
              | expr == a = t
              | otherwise = lookInFor xs a
   tables = [ pSql | InternalPlug pSql <- plugInfos fSpec, not (isScalar pSql)]
      where isScalar ScalarSQL{} = True
            isScalar _           = False
   roots :: [A_Concept]
   roots = (catMaybes.map primKey) tables
   primKey :: PlugSQL -> Maybe A_Concept
   primKey TblSQL{attributes=(f:_)} = Just (source (attExpr f))
   primKey _                    = Nothing
   ooAttr :: [SqlAttribute] -> SqlAttribute -> CdAttribute
   ooAttr kernelAtts f =
     OOAttr { attNm = attName f
            , attTyp = if null([Sym,Asy]>-properties (attExpr f)) && (f `notElem` kernelAtts)
                       then "Bool"
                       else (name.target.attExpr) f
            , attOptional = attNull f
            }
   allAssocs = filter isAssocBetweenClasses $ concatMap relsOf tables
     where
       isAssocBetweenClasses a = let allClassNames = map clName allClasses in assSrc a `elem` allClassNames && assTgt a `elem` allClassNames
       
       kernelConcepts = map fst (concatMap cLkpTbl tables)

       relsOf t =
         case t of
           TblSQL{} -> map (mkRel t) (catMaybes (map relOf (attributes t)))
           BinSQL{columns=(a,b)} ->
                     [ OOAssoc { assSrc = sqlname t
                               , assSrcPort = attName a
                               , asslhm = Mult MinZero MaxMany
                               , asslhr = ""
                               , assTgt = getConceptTableFor fSpec . target . attExpr $ a
                               , assrhm = Mult MinOne MaxOne
                               , assrhr = ""
                               , assmdcl = Nothing
                               }
                     , OOAssoc { assSrc = sqlname t
                               , assSrcPort = attName b
                               , asslhm = Mult MinZero MaxMany
                               , asslhr = ""
                               , assTgt = getConceptTableFor fSpec . target . attExpr $ b
                               , assrhm = Mult MinOne MaxOne
                               , assrhr = ""
                               , assmdcl = Nothing
                               }
                     ]
           _  -> fatal 195 "Unexpected type of table"
       relOf f =
         let expr = attExpr f in
         case expr of
           EDcI{} -> Nothing
           EDcD d -> if target d `elem` kernelConcepts then Just (expr,f) else Nothing
           EFlp (EDcD d) -> if source d `elem` kernelConcepts then Just (expr,f) else Nothing
           _ -> fatal 200 ("Unexpected expression: "++show expr)
       mkRel :: PlugSQL -> (Expression,SqlAttribute) -> Database.Design.Ampersand.Graphic.ClassDiagram.Association
       mkRel t (expr,f) =
            OOAssoc { assSrc = sqlname t
                    , assSrcPort = attName f
                    , asslhm = (mults.flp) expr
                    , asslhr = attName f
                    , assTgt = getConceptTableFor fSpec (target expr)
                    , assrhm = mults expr
                    , assrhr = case [name d | d@Sgn{}<-relsMentionedIn expr] of h:_ -> h ; _ -> fatal 229 "no relations used in expr"
                    , assmdcl = Nothing
                    }

----             
mults :: Relational r => r -> Multiplicities
mults r = let minVal = if isTot r then MinOne else MinZero
              maxVal = if isUni r then MaxOne else MaxMany
          in  Mult minVal maxVal
             