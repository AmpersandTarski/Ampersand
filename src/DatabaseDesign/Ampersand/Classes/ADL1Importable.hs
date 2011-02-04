{-# OPTIONS_GHC -Wall -XFlexibleInstances -XTypeSynonymInstances -XFlexibleContexts -XUndecidableInstances #-}
module DatabaseDesign.Ampersand.Classes.ADL1Importable                 (ADL1Importable(makeADL1Populations))
where
import DatabaseDesign.Ampersand.ADL1.Concept                    (Concept(..))
import DatabaseDesign.Ampersand.ADL1.Population                 (Populations,Population(..))
import DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration     (Declaration(..),makeRelation)
import DatabaseDesign.Ampersand.Basics

--implement makeADL1Populations to transform a certain --importformat of some --importfile to Populations Concept for a ADL1.Context
--for example, importing an ADL1 file into the Atlas application as defined in DatabaseDesign\Ampersand_Prototype\Apps\atlas.adl
--             see instance ADL1Importable Context (and instance ADL1Importable Fspc) in DatabaseDesign\Ampersand_Prototype\Apps\atlas.hs
--             USE -> cmd: adl --importfile=some.adl --importformat=adl atlas.adl
--                    the (map popm (makeADL1Populations someadlcontext))::[Relation Concept] should of course exist in atlas.adl
class ADL1Importable a where
 makeADL1Populations  :: [Declaration Concept] -> a -> Populations Concept


--instance (ADL1Importable a) => ADL1Importable [a] where
-- makeADL1Populations xs = concat(map makeADL1Populations xs)


--TODO -> fill the atlas context
--        a dummy implementation for populating atlasds example::Relation->PragmaExample has been given
instance ADL1Importable [Declaration Concept] where
 makeADL1Populations atlasds ds 
   = let example = [makeRelation d|d<-atlasds,name d=="example"]
     in [Popu{ popm=if length example==1 then head example else error "no or multiple declarations for relvar"
             , popps=[(name d,"pragma")|d<-ds]
             }]



--ADL1 POPULATIONs can be defined in a separate import file (no transformation required)
--instance ADL1Importable (Population Concept) where
-- makeADL1Populations p = [p]

             
