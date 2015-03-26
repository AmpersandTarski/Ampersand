module Database.Design.Ampersand.FSpec.Crud where

import Database.Design.Ampersand.Classes.ConceptStructure
import Database.Design.Ampersand.Core.AbstractSyntaxTree

data CrudInfo = CrudInfo { crudObjects :: [A_Concept] } deriving Show

mkCrudInfo :: [A_Concept] -> [Declaration] -> [Interface] -> CrudInfo
mkCrudInfo  allConcepts allDecls allInterfaces =
  CrudInfo allConcepts
