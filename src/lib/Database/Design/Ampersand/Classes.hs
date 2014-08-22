{-# OPTIONS_GHC -Wall #-}
module Database.Design.Ampersand.Classes
   ( module Database.Design.Ampersand.Classes.Populated
   , module Database.Design.Ampersand.Classes.ConceptStructure
   , module Database.Design.Ampersand.Classes.Relational
   , module Database.Design.Ampersand.Classes.ViewPoint
   ) where
import Database.Design.Ampersand.Classes.Populated
       (fullContents,atomsOf)
import Database.Design.Ampersand.Classes.ConceptStructure
       (ConceptStructure(..),prim2rel)
import Database.Design.Ampersand.Classes.Relational
       (Relational(..))
import Database.Design.Ampersand.Classes.ViewPoint
       (Language(..), ProcessStructure(..))