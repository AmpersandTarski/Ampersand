{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.ADL1.ConceptDef    (ConceptDef(..),ConceptDefs)
where
   import DatabaseDesign.Ampersand.Input.ADL1.FilePos    (FilePos)
   import DatabaseDesign.Ampersand.Basics  (Identified(..)) 

   type ConceptDefs = [ConceptDef]
   data ConceptDef 
      = Cd  { cdpos :: FilePos  -- ^ The position of this definition in the text of the Ampersand source (filename, line number and column number).
            , cdnm  :: String   -- ^ The name of this concept. If there is no such concept, the conceptdefinition is ignored.
            , cdplug:: Bool     -- ^ Whether the user specifically told Ampersand n—t to store this concept in the database
            , cddef :: String   -- ^ The textual definition of this concept.
            , cdref :: String   -- ^ A label meant to identify the source of the definition. (useful as LaTeX' symbolic reference)
            }   deriving (Show)

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ConceptDef                    ***
-- \***********************************************************************
   instance Eq ConceptDef where
    cd == cd' = cdnm cd == cdnm cd'
 --  instance Show ConceptDef
   instance Identified ConceptDef where
    name cd = cdnm cd
   
