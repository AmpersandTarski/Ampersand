{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Classes (module X) where
import DatabaseDesign.Ampersand.Classes.Populated as X
       (Populated(..),atomsOf)
import DatabaseDesign.Ampersand.Classes.ConceptStructure as X
       (ConceptStructure(..))
import DatabaseDesign.Ampersand.Classes.Relational as X
       (Relational(..))
import DatabaseDesign.Ampersand.Classes.ViewPoint as X
       (Language(..), ProcessStructure(..))