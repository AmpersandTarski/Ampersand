  module Dataset (makeDataset
                 ,makeDatasets
                 ,datasetMor)
  where
   import CommonClasses (Identified(..))
   import Auxiliaries (eqClass)
   import ADLdef

{- Datasets zijn bedoeld voor functiepuntentellingen en voor mogelijke efficiency-redenen in SQL-implementaties.
   Ze brengen een aantal relaties bijeen die zich als één SQL-tabel laten implementeren.
   Om praktische redenen krijgt een dataset geen eigen zelfstandige type, maar wordt het weergegeven als een ObjectDef.
   De volgende drie functies, makeDataset, makeDatasets en datasetMor, horen bij elkaar
   en moeten onderling consistent blijven.
-}

   makeDataset :: Context -> Concept -> ObjectDef
   makeDataset context c
    = Obj (name c) posNone (v (one,c)) [Obj (name m++name(target m)) posNone (Tm m) [] []| m<-dss] []
      where
       c  = minimum [g|g<-concs context,g<=head cl]
       cl = head ([cl| cl<-eqClass bi (concs context), c `elem` cl]++error ("!Fatal (module Fspec>dataset): cannot determine dataset for concept "++name c))
       c `bi` c' = not (null [m| m<-declarations context, isFunction m, isFunction (flp m)
                               , source m<=c && target m<=c'  ||  source m<=c' && target m<=c])
       dss = [     makeMph d | d<-declarations context, isFunction      d , source d `elem` cl]++
             [flp (makeMph d)| d<-declarations context, isFunction (flp d), target d `elem` cl]

   makeDatasets :: Context -> [ObjectDef]
   makeDatasets context
    = [ Obj (name c) posNone (v (one,c)) [Obj (name m++name(target m)) posNone (Tm m) [] []| m<-dss cl] []
      | cl<-eqClass bi (concs context), c<-[minimum [g|g<-concs context,g<=head cl]] ]
      where
       c `bi` c' = not (null [m| m<-declarations context, isFunction m, isFunction (flp m)
                               , source m<=c && target m<=c'  ||  source m<=c' && target m<=c])
       dss cl = [     makeMph d | d<-declarations context, isFunction      d , source d `elem` cl]++
                [flp (makeMph d)| d<-declarations context, isFunction (flp d), target d `elem` cl]

   datasetMor :: Context -> Morphism -> ObjectDef
   datasetMor context m | isFunction      m  = makeDataset context (source m)
                        | isFunction (flp m) = makeDataset context (target m)
                        | otherwise          = Obj (name m) posNone (Tm m) [] []

