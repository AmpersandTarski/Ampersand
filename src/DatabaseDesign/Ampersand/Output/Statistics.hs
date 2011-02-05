{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Output.Statistics (Statistics(..)) where

   import DatabaseDesign.Ampersand.ADL1
   import DatabaseDesign.Ampersand.Fspec
   import DatabaseDesign.Ampersand.Fspec.Fspec(Fservice(..)) --TODO import structure niet doordacht genoeg.
 -- TODO Deze module moet nog verder worden ingekleurd...
 
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
    nServices fSpec = length (fServices fSpec) --TODO -> check correctness
    nPatterns fSpec = nPatterns (patterns fSpec)
    nFpoints  fSpec = sum [fPoints (fsv_fpa svc)| svc<-fServices fSpec] +
                      sum [fPoints (fpa plug)   | InternalPlug plug <- plugInfos fSpec]
-- TODO Deze module moet nog verder worden ingekleurd...
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Pattern                       ***
-- \***********************************************************************

   instance Statistics Pattern where
    nServices _ = 0 --TODO -> check correctness
    nPatterns _ = 1
    nFpoints _  = error "!Fatal (module Statistics 38): function points are not defined for patterns at all."

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fservice                         ***
-- \***********************************************************************
   instance Statistics Fservice where
    nServices fSvc = nServices (objectdef fSvc)
    nPatterns _ = 0
    nFpoints fSvc = fPoints (fsv_fpa fSvc) --TODO -> implement correct FPA qualification

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Dataset                       ***
-- \*** TODO: zowel datasets als services worden weergegeven middels een ObjectDef. Dit maakt voor de functiepuntentelling natuurlijk wel wat uit, dus dat kan zo niet....
   instance Statistics ObjectDef where
    nServices (Obj{objats=[]}) = 2 -- dit is een associatie, en dus een binaire relatie --TODO -> check correctness
    nServices _ = 4 -- dit is een entiteit met ��n of meer attributen. --TODO -> check correctness
    nPatterns _ = 0
    nFpoints  _ = error "!Fatal (module Statistics 55): function points are not defined for ObjectDefs at all."

