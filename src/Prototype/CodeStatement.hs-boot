{-# OPTIONS_GHC -Wall #-}
module Prototype.CodeStatement (UseVar(..),PHPconcept(..)) where
 import Ampersand (Concept,SpecHierarchy,Expression,Relation,Identified)
 import Prototype.CodeAuxiliaries (Named)

 data PHPconcept
     = PHPC Concept      -- ^Usual concept
     | PHPexp (Expression (Relation Concept))-- ^A concept containing pairs representing the population in the expression. The letter D stands for derived
     | PHPI1  { cpvar :: Named UseVar }
 
 data UseVar = UseVar {uvList::[Either String (Named UseVar)]}
 instance Show UseVar
 instance Eq UseVar
 instance Show PHPconcept
 instance Eq PHPconcept
 instance Identified PHPconcept
 instance SpecHierarchy PHPconcept
