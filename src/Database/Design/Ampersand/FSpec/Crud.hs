module Database.Design.Ampersand.FSpec.Crud (CrudInfo(..), showCrudInfo, getCrudObjectsForInterface, mkCrudInfo) where

import Data.Function
import Data.List
import Data.Map (Map) 
import qualified Data.Map as Map 
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Classes.ConceptStructure
import Database.Design.Ampersand.Classes.Relational
import Database.Design.Ampersand.Core.AbstractSyntaxTree

fatal :: Int -> String -> a
fatal = fatalMsg "Crud"

data CrudInfo = CrudInfo { allCrudObjects :: [A_Concept]
                         , crudObjsPerInterface :: [ (Interface, [(A_Concept,Bool,Bool,Bool,Bool)]) ]
                         , crudObjsPerConcept :: [(A_Concept, ([Interface], [Interface], [Interface], [Interface]))]
                         -- TODO: name (crudPerInterface,crudPerConcept?) (also in code below)
                         -- TODO: think about representation of these matrices
                         } deriving Show

showCrudInfo :: CrudInfo -> String
showCrudInfo (CrudInfo crudObjects ifcCrudObjs _) =
  "CRUD info\nObjects: " ++ showNames crudObjects ++
  "\nMatrices\n" ++ concat
    [ "Interface " ++ name ifc ++
      "\nC R U D Object\n" ++
      (unlines $ map showCrud cObjs)
    | (ifc, cObjs) <- ifcCrudObjs
    ] ++ "\n"
  where showNames xs = intercalate ", " $ map name xs
        showCrud (cncpt, isC, isR, isU, isD) = concat [ showX isX ++ " " | isX <- [isC, isR, isU, isD] ] ++ show (name cncpt)
        showX isX = if isX then "X" else " "

getCrudObjectsForInterface :: CrudInfo -> Interface -> [(A_Concept,Bool,Bool,Bool,Bool)]
getCrudObjectsForInterface crudInfo ifc = 
  case lookup ifc $ crudObjsPerInterface crudInfo of
    Nothing       -> fatal 33 $ "NO CRUD objects for interface " ++ show (name ifc)
    Just crudObjs -> crudObjs
  
mkCrudInfo :: [A_Concept] -> [Declaration] -> [Interface] -> CrudInfo
mkCrudInfo  allConceptsPrim allDecls allIfcs =
  CrudInfo crudCncpts crudObjsPerIfc (getCrudObjsPerConcept crudObjsPerIfc)
  where allConcepts = [ c | c <- allConceptsPrim, not $ c == ONE || name c == "SESSION" ]
        nonCrudConcpts = [ source d | d <- allDecls, isUni d && isSur d ] ++
                         [ target d | d <- allDecls, isInj d && isTot d ]
        crudCncpts = allConcepts \\ nonCrudConcpts
        
        transSurjClosureMap :: [(A_Concept, [(A_Concept, Declaration)])]
        transSurjClosureMap = Map.toList . transClosureMapEx . Map.fromList $
          [ (source d, [(target d,d)]) | d <- allDecls, isSur d ] ++ -- TODO: no isUni?
          [ (target d, [(source d,d)]) | d <- allDecls, isTot d ]    -- TODO: no isInj?
        
        -- crud concept together with declarations that comprise the object
        crudObjs :: [(A_Concept, [Declaration])]
        crudObjs = [ (crudCncpt, [ d | (crcpt, tgtsDecls) <- transSurjClosureMap, crcpt == crudCncpt, (_,d) <- tgtsDecls ]) 
                   | crudCncpt <- crudCncpts ]
        
        getCrudUpdateConcpts :: Declaration -> [A_Concept]
        getCrudUpdateConcpts decl = [ cObj | (cObj, cDecls) <- crudObjs, decl `elem` cDecls ]

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
                                Just (InterfaceRef nm) ->
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

                                             
-- |  Yet another Warshall's transitive closure algorithm. This one uses labeled relations, and keeps track of the last label of each path.
transClosureMapEx :: (Eq a, Ord a, Eq lbl) =>Map a [(a,lbl)] -> Map a [(a,lbl)]
transClosureMapEx xs
  = foldl f xs (Map.keys xs `intersect` nub (map fst $ concat (Map.elems xs)))
    where
     f :: (Eq a, Ord a, Eq x) => Map a [(a,x)] -> a -> Map a [(a,x)]
     f q x = Map.unionWith union q (Map.fromListWith union [(a, q Map.! x) | (a, bs) <- Map.assocs q, x `elem` map fst bs])
