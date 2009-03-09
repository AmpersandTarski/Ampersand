
module Statistics where

   import Adl
   import FspecDef
   import FPA
 -- TODO Deze module moet nog geheel worden ingekleurd...
 
   class Statistics a where
    nServices :: a -> Int      -- ^ The number of services in a
    nPatterns :: a -> Int      -- ^ The number of services in a
    nFpoints  :: a -> Int      -- ^ The number of function points in a
    
    
   instance Statistics a => Statistics [a] where
    nServices xs = sum (map nServices xs)
    nPatterns xs = sum (map nPatterns xs)
    nFpoints  xs = sum (map nFpoints xs)


-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fspc                          ***
-- \***********************************************************************
   instance Statistics Fspc where
    nServices s = -1
    nPatterns s = -2
    nFpoints  s = -3
 -- TODO Deze module moet nog geheel worden ingekleurd...

--   instance Statistics Fspc where
--    nServices fspc = nServices (serviceS fspc `uni` serviceG fspc)  -- TODO: Stef, is het reeel om deze services op deze manier te tellen?
--    nPatterns (Fctx context themes datasets objects vrules) = nPatterns themes+nPatterns objects
--    nFpoints  (Fctx context themes datasets objects vrules) = nFpoints datasets + nFpoints objects

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Ftheme                        ***
-- \***********************************************************************

--   instance Statistics Ftheme where
--    nServices (Tspc p us) = nServices us
--    nPatterns (Tspc p us) = 1
--    nFpoints  (Tspc p us) = sum (map nFpoints us)
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Funit                         ***
-- \***********************************************************************

--   instance Statistics Funit where
--    nServices (Uspc nm pat ents svs) = length svs
--    nPatterns x = 0
--    nFpoints (Uspc unm pat car specs) = sum[fPoints fpa| (_,fpa,_,_)<-car]+sum [fPoints fpa| Sspc _ _ _ fpa _ _ _ _ _<-specs]

     
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fservice                         ***
-- \***********************************************************************

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FViewDef                      ***
-- \***********************************************************************

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ServiceSpec                   ***
-- \***********************************************************************

--   instance Statistics ServiceSpec where
--    nServices x = 1
--    nPatterns x = 0
--    nFpoints (Sspc nm sees changes fpa input output rs pre post) = fPoints fpa

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Dataset                       ***
-- \*** TODO: zowel datasets als services worden weergegeven middels een ObjectDef. Dit maakt voor de functiepuntentelling natuurlijk wel wat uit, dus dat kan zo niet....
   instance Statistics ObjectDef where
    nServices (Obj nm _ _ []  _) = 2 -- dit is een associatie, en dus een binaire relatie
    nServices (Obj nm _ _ ats _) = 4 -- dit is een entiteit met ��n of meer attributen.
    nPatterns (Obj nm _ _ []  _) = 0
    nPatterns (Obj nm _ _ ats _) = 0
    nFpoints  (Obj nm _ _ []  _) = fPoints (ILGV Eenvoudig)
    nFpoints  (Obj nm _ _ ats _) = fPoints (ILGV Eenvoudig)
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

