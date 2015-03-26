module Database.Design.Ampersand.FSpec.Crud where

import Data.List
import Database.Design.Ampersand.Basics.Unique
import Database.Design.Ampersand.Classes.ConceptStructure
import Database.Design.Ampersand.Classes.Relational
import Database.Design.Ampersand.Core.AbstractSyntaxTree

data CrudInfo = CrudInfo { getCrudObjects :: [A_Concept]
                         , getIfcCrudMatrices :: [ (Interface, [(A_Concept,Bool,Bool,Bool,Bool)]) ] 
                         -- TODO: think about representation of these matrices
                         } deriving Show

showCrudInfo :: CrudInfo -> String
showCrudInfo (CrudInfo crudObjects ifcCrudMatrices) =
  "CRUD info\nObjects: " ++ showNames crudObjects ++
  "\nMatrices\n" ++ concat
    [ "Interface " ++ name ifc ++
      "\nC R U D Object\n" ++
      (unlines $ map showCrud cObjs) 
    | (ifc, cObjs) <- ifcCrudMatrices
    ] ++ "\n"
  where showNames xs = intercalate ", " $ map name xs
        showCrud (cncpt, isC, isR, isU, isD) = concat [showX isX ++ " " | isX <- [isC, isR, isU, isD] ] ++ show (name cncpt)
        showX isX = if isX then "X" else " "
  
mkCrudInfo :: [A_Concept] -> [Declaration] -> [Interface] -> CrudInfo
mkCrudInfo  allConceptsPrim allDecls allInterfaces =
  CrudInfo allCrudObjects [ (ifc, getCrudMatrix ifc) | ifc <- allInterfaces ]
  where allConcepts = [ c | c <- allConceptsPrim, not $ c == ONE || name c == "SESSION" ]
        nonCrudObjects = [ source d | d <- allDecls, isUni d && isSur d ] ++
                         [ target d | d <- allDecls, isInj d && isTot d ]
        allCrudObjects = allConcepts \\ nonCrudObjects

        -- Not the most efficient implementation, but it is easy to read, and the total number of concepts will not be enormous.
        getCrudMatrix :: Interface -> [(A_Concept,Bool,Bool,Bool,Bool)]
        getCrudMatrix ifc = [ (cObj, cObj `elem` crudCreateObjs, cObj `elem` crudReadObjs, cObj `elem` crudUpdateObjs, cObj `elem` crudDeleteObjs)
                            | cObj <- allCrudObjects
                            ]
        
          where crudCreateObjs = []
                crudReadObjs   = concs (relsUsedIn ifc)
                crudDeleteObjs = []
                crudUpdateObjs = []