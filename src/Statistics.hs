
module Statistics where

   import Data.Fspec
   import FPA
 -- TODO Deze module moet nog geheel worden ingekleurd...
 
   class Statistics a where
    nServices :: a -> Int
    nPatterns :: a -> Int
    nFpoints  :: a -> Int
    
    
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
    nFpoints s = -3
 -- TODO Deze module moet nog geheel worden ingekleurd...

--   instance Statistics Fspc where
--    nServices fspc = nServices themes+nServices objects
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
-- \*** Eigenschappen met betrekking tot: Fview                         ***
-- \***********************************************************************

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Frule                         ***
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
-- \***********************************************************************
   instance Statistics Dataset where
    nServices (DS c pths) = 4
    nServices (BR m)      = 2
    nPatterns (DS c pths) = 0
    nPatterns (BR m)      = 0
    nFpoints  (DS c pths) = fPoints (ILGV Eenvoudig)
    nFpoints  (BR m)      = fPoints (ILGV Eenvoudig)

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ParamSpec                     ***
-- \***********************************************************************

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FSid                          ***
-- \***********************************************************************



--   instance Statistics ObjectDef where
--    nServices o = 4+sum [nServices a| a<-attributes o]
--    nPatterns o = 0
--    nFpoints  o = error ("(Module Fspec) Function points TBD")

--   instance Statistics Fobj where
--    nServices (Fobj dset o svcs rs) = length svcs
--    nPatterns (Fobj dset o svcs rs) = 1
--    nFpoints  (Fobj dset o svcs rs) = nFpoints o

