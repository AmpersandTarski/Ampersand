{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module DatabaseDesign.Ampersand.ADL1.Concept ( newAcpt) 
where
  import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
  import DatabaseDesign.Ampersand.Core.Poset(Ordering(..))
  import Prelude hiding (Ordering(..))


  -- | Constructor of a concept with the given name and population
  newAcpt :: String -> [String] -> A_Concept
  newAcpt nm atoms = C { cptnm = nm
                       , cptgE = (\x y -> if x==y then EQ else NC,[])
                       , cptos = atoms
                       , cpttp = []
                       , cptdf = []
                       }
  

