{-# OPTIONS_GHC -Wall -XFlexibleInstances -XTypeSynonymInstances -XFlexibleContexts -XUndecidableInstances #-}
module DatabaseDesign.Ampersand.Classes.ADL1Importable                 (ADL1Importable(makeADL1Populations))
where
import DatabaseDesign.Ampersand.ADL1.Concept                    (Concept(..))
import DatabaseDesign.Ampersand.ADL1.Population                 (Populations)

--implement makeADL1Populations to transform a certain --importformat of some --importfile to Populations Concept for a ADL1.Context
--for example, importing an ADL1 file into the Atlas application as defined in DatabaseDesign\Ampersand_Prototype\Apps\atlas.adl
--             see instance ADL1Importable Context (and instance ADL1Importable Fspc) in DatabaseDesign\Ampersand_Prototype\Apps\atlas.hs
--             USE -> cmd: adl --importfile=some.adl --importformat=adl atlas.adl
--                    the (map popm (makeADL1Populations someadlcontext))::[Relation Concept] should of course exist in atlas.adl
class ADL1Importable a where
 makeADL1Populations  :: a -> Populations Concept

--ADL1 POPULATIONs can be defined in a separate import file (no transformation required)
instance ADL1Importable (Populations Concept) where
 makeADL1Populations pops = pops



             
