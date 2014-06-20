{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Classes
   ( module DatabaseDesign.Ampersand.Classes.Populated
   , module DatabaseDesign.Ampersand.Classes.ConceptStructure
   , module DatabaseDesign.Ampersand.Classes.Relational
   , module DatabaseDesign.Ampersand.Classes.ViewPoint
   ) where
import DatabaseDesign.Ampersand.Classes.Populated 
       (fullContents,atomsOf)
import DatabaseDesign.Ampersand.Classes.ConceptStructure 
       (ConceptStructure(..),prim2rel)
import DatabaseDesign.Ampersand.Classes.Relational
       (Relational(..))
import DatabaseDesign.Ampersand.Classes.ViewPoint
       (Language(..), ProcessStructure(..))