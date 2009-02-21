{-# OPTIONS_GHC -Wall #-}
module Adl.ConceptDef 
where
   import Adl.FilePos
   import CommonClasses(Identified(name,typ)) 
   type ConceptDefs = [ConceptDef]
   data ConceptDef 
      = Cd  { cdpos :: FilePos  -- ^ The position of this definition in the text of the ADL source (filename, line number and column number).
            , cdnm  :: String   -- ^ The name of this concept. If there is no such concept, the conceptdefinition is ignored.
            , cddef :: String   -- ^ The textual definition of this concept.
            , cdref :: String   -- ^ A label meant to identify the source of the definition. (useful as LaTeX' symbolic reference)
            } 

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ConceptDef                    ***
-- \***********************************************************************
   instance Eq ConceptDef where
    cd == cd' = cdnm cd == cdnm cd'
   instance Show ConceptDef    
   instance Identified ConceptDef where
    name cd = cdnm cd
    typ _ = "ConceptDef_"
   
