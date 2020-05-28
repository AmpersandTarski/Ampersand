{-# LANGUAGE OverloadedStrings #-}
module Ampersand.FSpec.Crud (CrudInfo(..), getCrudObjectsForInterface, mkCrudInfo) where

import           Ampersand.Basics
import           Ampersand.Classes
import           Ampersand.ADL1
import qualified RIO.List as L
import qualified RIO.Map as Map 
import qualified RIO.Set as Set

-- For a description of the algorithms in this module, see https://github.com/AmpersandTarski/ampersand/issues/45 

-- NOTE: The definitions of the various CRUD aspects are still a bit quirky and will most-likely need refinement. 
--      (see notes/todo's here and in ampersand-models/Tests/NoSentinel/Crud.adl)

data CrudInfo = CrudInfo { allCrudObjects :: [(A_Concept,[A_Concept])] -- crud concept together with its target concept in the surjective/total transitive closure of relations 
                         , crudObjsPerInterface :: [ (Interface, [(A_Concept,Bool,Bool,Bool,Bool)]) ]
                         , crudObjsPerConcept :: [(A_Concept, ([Interface], [Interface], [Interface], [Interface]))]
                         -- TODO: think about representation of these matrices
                         } deriving Show


getCrudObjectsForInterface :: CrudInfo -> Interface -> [(A_Concept,Bool,Bool,Bool,Bool)]
getCrudObjectsForInterface crudInfo ifc = 
  fromMaybe (fatal $ "NO CRUD objects for interface " <> tshow (name ifc))
            (lookup ifc $ crudObjsPerInterface crudInfo) 
  
mkCrudInfo :: A_Concepts -> Relations -> [Interface] -> CrudInfo
mkCrudInfo  allConceptsPrim decls allIfcs =
  CrudInfo crudObjs crudObjsPerIfc (getCrudObjsPerConcept crudObjsPerIfc)
  where allConcs = [ c | c <- Set.elems allConceptsPrim, not $ isONE c || isSESSION c ]
        nonCrudConcpts = (map source . filter isUni . filter isSur . map EDcD . Set.elems $ decls) <>
                         (map target . filter isInj . filter isTot . map EDcD . Set.elems $ decls)
        crudCncpts = allConcs L.\\ nonCrudConcpts
        
        transSurjClosureMap :: Map.Map A_Concept [A_Concept]
        transSurjClosureMap = transClosureMap' . Map.fromListWith L.union $
          (map (mkMapItem . flp) . filter isSur . map EDcD $ Set.elems decls) <> -- TODO: no isUni?
          (map (mkMapItem      ) . filter isTot . map EDcD $ Set.elems decls)    -- TODO: no isInj?
          -- TODO: use transClosureMap instead of transClosureMap', it's faster, and this is transClosureMap's last occurrence
           where
             mkMapItem :: Expression -> (A_Concept,[A_Concept])
             mkMapItem expr = (source expr,[target expr])
        
        -- crud concept together with its target concept in the surjective/total transitive closure of relations
        crudObjs :: [(A_Concept, [A_Concept])]
        crudObjs = [ (crudCncpt, Map.findWithDefault [] crudCncpt transSurjClosureMap) -- TODO: should [] be a fatal? 
                   | crudCncpt <- crudCncpts ]
        
        getCrudUpdateConcpts :: Expression -> [A_Concept]
        getCrudUpdateConcpts decl = 
          if  isSur decl || isTot decl  -- TODO: no isUni?  -- TODO: no isInj?
          then [ cObj | (cObj, cCncpts) <- crudObjs, source decl `elem` cCncpts && target decl `elem` cCncpts ]    
          else []
          
        crudObjsPerIfc = [ (ifc, getCrudObjsPerIfc ifc) | ifc <- allIfcs ]
        
        -- Not the most efficient implementation, but it is easy to read, and the total number of concepts will not be enormous.
        getCrudObjsPerIfc :: Interface -> [(A_Concept,Bool,Bool,Bool,Bool)]
        getCrudObjsPerIfc ifc = [ (cObj, isC, isR, isU, isD)
                                | cObj <- crudCncpts
                                , let isC = cObj `elem` crudCreateCncpts
                                , let isR = cObj `elem` crudReadCncpts
                                , let isU = cObj `elem` crudUpdateCncpts
                                , let isD = cObj `elem` crudDeleteCncpts
                                , or [isC, isR, isU, isD]
                                ]                            
          where crudCreateCncpts = editableTgts
                crudReadCncpts   = concs (bindedRelationsIn ifc) -- NOTE: this includes interface params, even if they do not appear in any of the field expressions
                crudDeleteCncpts = crudCreateCncpts -- We can't currently distinguish between these two.
                crudUpdateCncpts = concatMap (getCrudUpdateConcpts .EDcD) editableDecls
                (editableDecls, editableTgts) = L.unzip $ getEditableDeclsAndTargets allIfcs ifc
                                             
-- NOTE: editable target is not necessarily the target of decl, as it may have been flipped (in which case it's the source)
getEditableDeclsAndTargets :: [Interface] -> Interface -> [(Relation, A_Concept)]
getEditableDeclsAndTargets allIfcs ifc = concatMap editableTarget $ getAllInterfaceExprs allIfcs ifc
  where editableTarget expr = 
          case getExpressionRelation expr of
            Nothing                                                              -> []
            Just (declSrc, decl, declTgt, isFlipped') | decl `elem` [] -> [(decl, if isFlipped' then declSrc else declTgt)]
                                                      | otherwise                 -> []

getAllInterfaceExprs :: [Interface] -> Interface -> [Expression]
getAllInterfaceExprs allIfcs ifc = getExprs $ ifcObj ifc
  where 
    getExprs :: ObjectDef -> [Expression]
    getExprs objExpr =
      objExpression objExpr : 
          case objmsub objExpr of 
                   Nothing                -> []
                   Just si -> case si of
                               InterfaceRef{siIsLink = True} -> []
                               InterfaceRef{siIsLink = False} ->
                                  case filter (\rIfc -> name rIfc == siIfcId si) allIfcs of -- Follow interface ref
                                    []      -> fatal ("Referenced interface " <> siIfcId si <> " missing")
                                    (_:_:_) -> fatal ("Multiple relations of referenced interface " <> siIfcId si)
                                    [i]     -> getAllInterfaceExprs allIfcs i
                               Box{} -> concatMap getExprs' (siObjs si)
                        where getExprs' (BxExpr e) = getExprs e
                              getExprs' (BxTxt _) = []

getCrudObjsPerConcept :: [(Interface, [(A_Concept,Bool,Bool,Bool,Bool)])] ->
                         [(A_Concept, ([Interface], [Interface], [Interface], [Interface]))]
getCrudObjsPerConcept crudsPerIfc = L.sortBy (compare `on` fst)  conceptsAndInterfaces
  where conceptsAndInterfaces :: [(A_Concept, ([Interface], [Interface], [Interface], [Interface]))]
        conceptsAndInterfaces = concatMap toIfcPerConcept crudsPerIfc
        
        toIfcPerConcept :: (Interface, [(A_Concept,Bool,Bool,Bool,Bool)]) -> 
                           [(A_Concept, ([Interface], [Interface], [Interface], [Interface]))]
        toIfcPerConcept (ifc, ifcCrudObjs) = [ (cncpt, ( [ifc | isC]
                                                       , [ifc | isR]
                                                       , [ifc | isU]
                                                       , [ifc | isD]
                                                       )
                                               )
                                             | (cncpt, isC, isR, isU, isD) <- ifcCrudObjs
                                             ]
