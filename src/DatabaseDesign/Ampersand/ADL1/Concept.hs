{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module DatabaseDesign.Ampersand.ADL1.Concept ( newAcpt,atomsOf) 
where
  import DatabaseDesign.Ampersand.Basics (fatalMsg)
  import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
  import DatabaseDesign.Ampersand.Core.Poset(Ordering(..))
  import Prelude hiding (Ordering(..))
  fatal :: Int -> String -> a
  fatal = fatalMsg "ADL1.Concept"



  -- | This function returns the atoms of a concept
  atomsOf :: A_Concept -> [String]
  atomsOf C{cptnm="SESSION"} = [] -- TODO: HACK to prevent populating SESSION
  atomsOf C{cptos=x} = x
  atomsOf ONE = fatal 126 "Asking for the value of the universal singleton"
  

  -- | Constructor of a concept with the given name and population
  newAcpt :: String -> [String] -> A_Concept
  newAcpt nm atoms = C { cptnm = nm
                       , cptgE = ((\x y -> if x==y then EQ else NC),[])
                       , cptos = atoms
                       , cpttp = []
                       , cptdf = []
                       }
  

