module Database.Design.Ampersand.FSpec.Crud (CrudInfo(..), showCrudInfo, getCrudObjectsForInterface, mkCrudInfo) where

import Data.List
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Classes.ConceptStructure
import Database.Design.Ampersand.Classes.Relational
import Database.Design.Ampersand.Core.AbstractSyntaxTree

fatal :: Int -> String -> a
fatal = fatalMsg "Crud"

type CrudObject = (A_Concept,Bool,Bool,Bool,Bool)

data CrudInfo = CrudInfo { getCrudObjects     :: [A_Concept]
                         , getIfcCrudObjs :: [ (Interface, [CrudObject]) ]
                         -- TODO: think about representation of these matrices
                         } deriving Show

showCrudInfo :: CrudInfo -> String
showCrudInfo (CrudInfo crudObjects ifcCrudObjs) =
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

getCrudObjectsForInterface :: CrudInfo -> Interface -> [CrudObject]
getCrudObjectsForInterface crudInfo ifc = 
  case lookup ifc $ getIfcCrudObjs crudInfo of
    Nothing       -> fatal 33 $ "NO CRUD matrix for interface " ++ show (name ifc)
    Just crudObjs -> crudObjs
  
mkCrudInfo :: [A_Concept] -> [Declaration] -> [Interface] -> CrudInfo
mkCrudInfo  allConceptsPrim allDecls allIfcs =
  CrudInfo allCrudObjects [ (ifc, getCrudMatrix ifc) | ifc <- allIfcs ]
  where allConcepts = [ c | c <- allConceptsPrim, not $ c == ONE || name c == "SESSION" ]
        nonCrudObjects = [ source d | d <- allDecls, isUni d && isSur d ] ++
                         [ target d | d <- allDecls, isInj d && isTot d ]
        allCrudObjects = allConcepts \\ nonCrudObjects

        -- Not the most efficient implementation, but it is easy to read, and the total number of concepts will not be enormous.
        getCrudMatrix :: Interface -> [(A_Concept,Bool,Bool,Bool,Bool)]
        getCrudMatrix ifc = [ (cObj, isC, isR, isU, isD)
                            | cObj <- allCrudObjects
                            , let isC = cObj `elem` crudCreateObjs
                            , let isR = cObj `elem` crudReadObjs
                            , let isU = cObj `elem` crudUpdateObjs
                            , let isD = cObj `elem` crudDeleteObjs
                            , or [isC, isR, isU, isD] -- TODO: is this right?
                            ]

          where crudCreateObjs = getEditableTargets allIfcs ifc
                crudReadObjs   = concs (relsUsedIn ifc) -- NOTE: this includes interface params, even if they do not appear in any of the field expressions
                crudDeleteObjs = crudCreateObjs -- TODO: check if these are the same in Ampersand
                crudUpdateObjs = []

getEditableTargets :: [Interface] -> Interface -> [A_Concept]
getEditableTargets allIfcs ifc = concatMap editableTarget $ getAllInterfaceExprs allIfcs ifc
  where editableTarget expr = 
          case getExpressionRelation expr of
            Nothing                                                              -> []
            Just (declSrc, decl, declTgt, isFlipped) | decl `elem` ifcParams ifc -> [if isFlipped then declSrc else declTgt]
                                                     | otherwise                 -> []
-- TODO: Handle specs? And do we use source/target expr or declSrc/Tgt?

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

                                
                                