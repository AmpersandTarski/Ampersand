  {-# OPTIONS -XTypeSynonymInstances #-}
  module ADLdataDefOud ()
                 where
   import Adl             
   import UU_Scanner (Pos(Pos))
   import Strings(chain)
   import CommonClasses(Identified(name,typ)
                        , ABoolAlg(glb,lub,order)
                        , Explained(explain)
                        , Conceptual(conts)
                        , Morphics(anything)
                        )
   import Collection (Collection (rd))

   
-- In deze module worden aan taalconstuctors van ADL de eigenschappen
-- toegekend voor de volgende classen:
--   Eq
--   Show
--   Identified
--   Association
--   Numbered
--   Ord
--   ABoolAlg





-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Architecture                  ***
{- \**** (Eq dient alleen diagnostische doeleinden)    ********************

   instance Eq Architecture where
    a==a' = archContexts a==archContexts a'
-}

    
   
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Rule                          ***
-- \***********************************************************************

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: RuleType                      ***
-- \***********************************************************************
--   instance Eq RuleType
   
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Population                    ***
-- \***********************************************************************
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: AutType                       ***
-- \***********************************************************************
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FilePos                       ***
-- \***********************************************************************



--  /---------------------Hieronder moet nog verder worden bekeken of het hier wel thuishoort.
--
   -- HJO, Wellicht mag i.p.v. bovenstaande ook gewoon het volgende worden gezegd, maar dat kan ik momenteel niet bevestigen:
   -- uncomp r = r{r_cpu = []}   (Dat zou wel elegant zijn, maar het moet nog worden getest of dit goed gaat...)


   


