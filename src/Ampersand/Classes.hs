module Ampersand.Classes 
   ( module Ampersand.Classes.ConceptStructure
   , module Ampersand.Classes.Relational
   , module Ampersand.Classes.ViewPoint
   ) where
import Ampersand.Classes.ConceptStructure (ConceptStructure(..))
import Ampersand.Classes.Relational       (Relational(..),HasProps(..),isONE,isSESSION)
import Ampersand.Classes.ViewPoint        (Language(..),enforce2Rules)