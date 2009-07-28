
module Statistics where

   import Adl
   import FspecDef
   import FPA
 -- TODO Deze module moet nog geheel worden ingekleurd...
 -- TODO Is there a need for the number of Views or is FViewDef obsolete or is it a special kind of ServiceSpec?
 
   class Statistics a where
    nServices :: a -> Int      -- ^ The number of services in a
    nPatterns :: a -> Int      -- ^ The number of patterns in a
    nFpoints  :: a -> Int      -- ^ The number of function points in a
    
    
   instance Statistics a => Statistics [a] where
    nServices xs = sum (map nServices xs)
    nPatterns xs = sum (map nPatterns xs)
    nFpoints  xs = sum (map nFpoints xs)


-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fspc                          ***
-- \***********************************************************************
   instance Statistics Fspc where
    nServices fSpec = length (services fSpec) --TODO -> check correctness
    nPatterns fSpec = nPatterns (vpatterns fSpec)
    nFpoints  fSpec = nFpoints (services fSpec) --TODO -> check correctness
 -- TODO Deze module moet nog geheel worden ingekleurd...

--   instance Statistics Fspc where
--    nServices fspc = nServices (serviceS fspc `uni` serviceG fspc)  -- TODO: Stef, is het reeel om deze services op deze manier te tellen?
--    nPatterns (Fctx context themes datasets objects vrules) = nPatterns themes+nPatterns objects
--    nFpoints  (Fctx context themes datasets objects vrules) = nFpoints datasets + nFpoints objects
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Funit                         ***
-- \***********************************************************************

   instance Statistics Pattern where
    nServices u = 0 --TODO -> check correctness
    nPatterns _ = 1
    nFpoints u =  0

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fservice                         ***
-- \***********************************************************************
   instance Statistics Fservice where
    nServices fSvc = nServices (objectdef fSvc)
    nPatterns _ = 0
    nFpoints  fSvc = nFpoints (objectdef fSvc) --TODO -> implement correct FPA qualification

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Dataset                       ***
-- \*** TODO: zowel datasets als services worden weergegeven middels een ObjectDef. Dit maakt voor de functiepuntentelling natuurlijk wel wat uit, dus dat kan zo niet....
   instance Statistics ObjectDef where
    nServices (Obj{objats=[]}) = 2 -- dit is een associatie, en dus een binaire relatie --TODO -> check correctness
    nServices _ = 4 -- dit is een entiteit met ��n of meer attributen. --TODO -> check correctness
    nPatterns _ = 0
    nFpoints  _ = fPoints (ILGV Eenvoudig) --TODO -> implement correct FPA qualification

-- \*** TODO: de functiepuntentelling voor Services zou er als volgt uit moeten zien....
--   instance Statistics ObjectDef where
--    nServices o = 4+sum [nServices a| a<-attributes o]
--    nPatterns o = 0
--    nFpoints  o = error ("(Module Fspec) Function points TBD")

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ParamSpec                     ***
-- \***********************************************************************

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FSid                          ***
-- \***********************************************************************




--   instance Statistics Fobj where
--    nServices (Fobj dset o svcs rs) = length svcs
--    nPatterns (Fobj dset o svcs rs) = 1
--    nFpoints  (Fobj dset o svcs rs) = nFpoints o

