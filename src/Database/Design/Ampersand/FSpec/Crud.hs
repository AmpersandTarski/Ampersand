module Database.Design.Ampersand.FSpec.Crud (CrudInfo(..), showCrudInfo, getCrudObjectsForInterface, mkCrudInfo) where

import Data.Function
import Data.List
import Data.Map (Map) 
import qualified Data.Map as Map 
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Classes.ConceptStructure
import Database.Design.Ampersand.Classes.Relational
import Database.Design.Ampersand.Core.AbstractSyntaxTree

-- For a description of the algorithms in this module, see https://github.com/AmpersandTarski/ampersand/issues/45 

-- NOTE: The definitions of the various CRUD aspects are still a bit quirky and will most-likely need refinement. 
--      (see notes/todo's here and in ampersand-models/Tests/NoSentinel/Crud.adl)

data CrudInfo = CrudInfo { allCrudObjects :: [(A_Concept,[A_Concept])] -- crud concept together with its target concept in the surjective/total transitive closure of declarations 
                         , crudObjsPerInterface :: [ (Interface, [(A_Concept,Bool,Bool,Bool,Bool)]) ]
                         , crudObjsPerConcept :: [(A_Concept, ([Interface], [Interface], [Interface], [Interface]))]
                         -- TODO: think about representation of these matrices
                         } deriving Show

showCrudInfo :: CrudInfo -> String
showCrudInfo (CrudInfo crudObjs ifcCrudObjs _) =
  "CRUD info\nObjects:\n" ++ unlines [ name crudCncpt ++" : " ++ show (map name crudDecls) | (crudCncpt, crudDecls) <- crudObjs] ++
  "\nMatrices\n" ++ concat
    [ "Interface " ++ name ifc ++
      "\nC R U D Object\n" ++
      (unlines $ map showCrud cObjs)
    | (ifc, cObjs) <- ifcCrudObjs
    ] ++ "\n"
  where showCrud (cncpt, isC, isR, isU, isD) = concat [ showX isX ++ " " | isX <- [isC, isR, isU, isD] ] ++ show (name cncpt)
        showX isX = if isX then "X" else " "

getCrudObjectsForInterface :: CrudInfo -> Interface -> [(A_Concept,Bool,Bool,Bool,Bool)]
getCrudObjectsForInterface crudInfo ifc = 
  case lookup ifc $ crudObjsPerInterface crudInfo of
    Nothing       -> fatal 33 $ "NO CRUD objects for interface " ++ show (name ifc)
    Just crudObjs -> crudObjs
  
mkCrudInfo :: [A_Concept] -> [Declaration] -> [Interface] -> CrudInfo
mkCrudInfo  allConceptsPrim decls allIfcs =
  CrudInfo crudObjs crudObjsPerIfc (getCrudObjsPerConcept crudObjsPerIfc)
  where allConcs = [ c | c <- allConceptsPrim, not $ c == ONE || name c == "SESSION" ]
        nonCrudConcpts = [ source d | d <- decls, isUni d && isSur d ] ++
                         [ target d | d <- decls, isInj d && isTot d ]
        crudCncpts = allConcs \\ nonCrudConcpts
        
        transSurjClosureMap :: Map A_Concept [A_Concept]
        transSurjClosureMap = transClosureMap . Map.fromListWith union $
          [ (target d, [source d]) | d <- decls, isSur d ] ++ -- TODO: no isUni?
          [ (source d, [target d]) | d <- decls, isTot d ]    -- TODO: no isInj?
        
        
        -- crud concept together with its target concept in the surjective/total transitive closure of declarations
        crudObjs :: [(A_Concept, [A_Concept])]
        crudObjs = [ (crudCncpt, Map.findWithDefault [] crudCncpt transSurjClosureMap) -- TODO: should [] be a fatal? 
                   | crudCncpt <- crudCncpts ]
        
        getCrudUpdateConcpts :: Declaration -> [A_Concept]
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
                crudReadCncpts   = concs (relsUsedIn ifc) -- NOTE: this includes interface params, even if they do not appear in any of the field expressions
                crudDeleteCncpts = crudCreateCncpts -- We can't currently distinguish between these two.
                crudUpdateCncpts = concatMap getCrudUpdateConcpts editableDecls
                (editableDecls, editableTgts) = unzip $ getEditableDeclsAndTargets allIfcs ifc
                                             
-- NOTE: editable target is not necessarily the target of decl, as it may have been flipped (in which case it's the source)
getEditableDeclsAndTargets :: [Interface] -> Interface -> [(Declaration, A_Concept)]
getEditableDeclsAndTargets allIfcs ifc = concatMap editableTarget $ getAllInterfaceExprs allIfcs ifc
  where editableTarget expr = 
          case getExpressionRelation expr of
            Nothing                                                              -> []
            Just (declSrc, decl, declTgt, isFlipped) | decl `elem` ifcParams ifc -> [(decl, if isFlipped then declSrc else declTgt)]
                                                     | otherwise                 -> []

getAllInterfaceExprs :: [Interface] -> Interface -> [Expression]
getAllInterfaceExprs allIfcs ifc = getExprs $ ifcObj ifc
  where getExprs Obj{objctx=expr, objmsub=subObj} = 
          expr : case subObj of Nothing                -> []
                                Just (InterfaceRef _ nm _) ->
                                  case filter (\rIfc -> name rIfc == nm) $ allIfcs of -- Follow interface ref
                                    []      -> fatal 65 $ "Referenced interface " ++ nm ++ " missing"
                                    (_:_:_) -> fatal 66 $ "Multiple declarations of referenced interface " ++ nm
                                    [i]     -> getAllInterfaceExprs allIfcs i
                                Just (Box _ _ objs)    -> concatMap getExprs objs

getCrudObjsPerConcept :: [(Interface, [(A_Concept,Bool,Bool,Bool,Bool)])] ->
                         [(A_Concept, ([Interface], [Interface], [Interface], [Interface]))]
getCrudObjsPerConcept crudsPerIfc = sortBy (compare `on` fst)  conceptsAndInterfaces
  where conceptsAndInterfaces :: [(A_Concept, ([Interface], [Interface], [Interface], [Interface]))]
        conceptsAndInterfaces = concatMap toIfcPerConcept crudsPerIfc
        
        toIfcPerConcept :: (Interface, [(A_Concept,Bool,Bool,Bool,Bool)]) -> 
                           [(A_Concept, ([Interface], [Interface], [Interface], [Interface]))]
        toIfcPerConcept (ifc, ifcCrudObjs) = [ (cncpt, ( if isC then [ifc] else []
                                                       , if isR then [ifc] else []
                                                       , if isU then [ifc] else []
                                                       , if isD then [ifc] else []
                                                       )
                                               )
                                             | (cncpt, isC, isR, isU, isD) <- ifcCrudObjs
                                             ]
