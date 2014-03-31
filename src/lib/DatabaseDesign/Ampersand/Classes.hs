{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Classes (module X) where
import DatabaseDesign.Ampersand.Classes.Populated as X
       (atomsOf,fullContents)
import DatabaseDesign.Ampersand.Classes.ConceptStructure as X
       (ConceptStructure(..),prim2rel)
import DatabaseDesign.Ampersand.Classes.Relational as X
       (Relational(..))
import DatabaseDesign.Ampersand.Classes.ViewPoint as X
       (Language(..), ProcessStructure(..))