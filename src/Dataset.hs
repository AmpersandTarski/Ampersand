  module Dataset (--Identified(..)
                 makeDataset
                 ,makeDatasets
                 ,datasetMor
                 ,declarationsDS
                 ,morsDS
                 ,concsDS)
  where
   import CommonClasses (Identified(..))
   import Auxiliaries (eqClass)
   import ADLdef
   import Data.Fspec

  -- (SJ:) is het volgende commentaar nog geldig? Zo nee, moeten we Dataset dan maar Morphical maken?
  -- Dataset should not be made Morphical, because it would make it too dependent on ADL. For this reason the functions are defined as follows: 

  -- WAAROM? Stef, hier lijk het er sterk op dat er niet goed is nagedacht over de datastructuur van Dataset. 
  --         Dat leidt er toe dat je op veel plaatsen functies dubbel moet uitvoeren, terwijl dat niet nodig is. 
  --         kijk ook in Data.Fspec 
  --         Is er niet iets eenvoudigers te regelen met een class Dataset?
  --   data DStype1 = DS { dscpt :: Concept     -- the root of the dataset
  --                     , dsmors:: [Morphism]  -- the functions from the root
  --   data DStype2 = BR { dsmors:: [Morphism]  -- for every m that is not (isFunction m || isFunction (flp m))
  --                                    ^- Maak hier ook een lijst van. Scheelt veel geneuzel.
  --  en dan iets van 
  --  instance Dataset DStype1 where  etc.
  

   declarationsDS :: Dataset -> Declarations
   declarationsDS ds = declarations (morsDS ds)
   morsDS :: Dataset -> Morphisms 
   morsDS (DS c pths) = pths
   morsDS (BR m     ) = [m]
   concsDS :: Dataset ->  Concepts
   concsDS ds = concs (morsDS ds)

{- Datasets zijn bedoeld voor functiepuntentellingen en voor mogelijke efficiency-redenen in SQL-implementaties.
   Ze brengen een aantal relaties bijeen die zich als één SQL-tabel laten implementeren.
   De volgende drie functies, makeDataset, makeDatasets en datasetMor, horen bij elkaar
   en moeten onderling consistent blijven.
-}


   makeDataset :: Context -> Concept -> Dataset
   makeDataset context c
    = DS (minimum [g|g<-concs context,g<=head cl]) dss
      where
       cl   = head ([cl| cl<-eqClass bi (concs context), c `elem` cl]++error ("!Fatal (module Fspec>dataset): cannot determine dataset for concept "++name c))
       c `bi` c' = not (null [m| m<-declarations context, isFunction m, isFunction (flp m)
                               , source m<=c && target m<=c'  ||  source m<=c' && target m<=c])
       dss = [     makeMph d | d<-declarations context, isFunction      d , source d `elem` cl]++
             [flp (makeMph d)| d<-declarations context, isFunction (flp d), target d `elem` cl]

   makeDatasets :: Context -> [Dataset]
   makeDatasets context
    = [ DS (minimum [g|g<-concs context,g<=head cl]) (dss cl)
      | cl<-eqClass bi (concs context) ]
      where
       c `bi` c' = not (null [m| m<-declarations context, isFunction m, isFunction (flp m)
                               , source m<=c && target m<=c'  ||  source m<=c' && target m<=c])
       dss cl = [     makeMph d | d<-declarations context, isFunction      d , source d `elem` cl]++
                [flp (makeMph d)| d<-declarations context, isFunction (flp d), target d `elem` cl]

   datasetMor :: Context -> Morphism -> Dataset
   datasetMor context m | isFunction      m  = makeDataset context (source m)
                        | isFunction (flp m) = makeDataset context (target m)
                        | otherwise          = BR m

