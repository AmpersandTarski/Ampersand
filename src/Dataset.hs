  {-# OPTIONS_GHC -Wall #-}
  module Dataset (makeDataset
                 ,makeDatasets
                 ,datasetMor)
  where
   import Auxiliaries (eqClass)
   import Adl
   import NormalForms (disjNF)

{- The type Dataset is designed to implement the populations of ADL in data sets in a relational database.
   (They can also be used for counting function points, because function point theory is founded on the same data sets.)
   In a relational database, we prefer to implement a model in tables that are as few and as wide as possible, without minimal data duplication.
   ADL knows the concept of SQL-plugs, which can be used to implement an ADL model on existing data sets.
   The idea is to generate SQL-plugs from an ADL-model, unless the user defines his own SQL-plugs to overrule that.

-}


 {- Bericht aan Stef van Han:
    In onderstaande codes wordt momenteel erg veel variabelen geshadowd. Dit maakt de code ondoorzichtig, en
    het toepassen er van zorgt ook voor fouten, zoals onderstaand voorbeeld volgens mij aantoont. 
    Het voorkomen van shadowing is eenvoudig: Geef binnen de scope van een variabele v elke andere variabele een naam die afwijkt van de naam van v. 
    Het pragma bovenaan deze module kan je hiermee helpen. (even de XXX weghalen en de pragma werkt...) Succes!
 -}

{-
   De volgende drie functies, makeDataset, makeDatasets en datasetMor, horen bij elkaar
   en moeten onderling consistent blijven.
-}
   makeDataset :: Context -> Concept -> ObjectDef
   makeDataset context c'    -- TODO FOUT?? WAAROM Stef, de c wordt niet gebruikt. Ik kan me niet voorstellen dat dat goed is.....
   --TODO -> c2 was c', c was c, c1 was c, c' was c. => dat was fout door een loop tussen cl=..c `elem`.. en c=minimum..head cl.. Nu is het ook niet goed. Sowieso een objectdef met objctx I[ONE] resulteert in error.
    = Obj { objnm   = name c
          , objpos  = Nowhere
          , objctx  = v (S,c)
          , objats  = [Obj { objnm   = name mph++name(target mph)
                           , objpos  = Nowhere
                           , objctx  = Tm mph
                           , objats  = []
                           , objstrs = []
                           }| mph<-dss]
          , objstrs = []
          } 
--was:    = Obj (name c) Nowhere (v (S,c)) [Obj (name m++name(target m)) Nowhere (Tm m) [] []| m<-dss] []
      where
       c  = minimum [g|g<-concs context,g<=head cl]
       cl = head ([cl'| cl'<-eqClass bi (concs context), c' `elem` cl']
              ++error ("!Fatal (module Fspec>dataset 48): cannot determine dataset for concept "++name c'))
       c1 `bi` c2 = not (null [mph| mph<-declarations context, isFunction mph, isFunction (flp mph)
                               , source mph<=c1 && target mph<=c2  ||  source mph<=c2 && target mph<=c1])
       dss = [     makeMph d | d<-declarations context, isFunction      d , source d `elem` cl]++
             [flp (makeMph d)| d<-declarations context, isFunction (flp d), target d `elem` cl]

   makeDatasets :: Context -> [ObjectDef]
   makeDatasets context
    = [ Obj (name c) Nowhere (v (S,c)) [Obj (name mph++name(target mph)) Nowhere (Tm mph) [] []| mph<-dss cl] []
      | cl<-eqClass bi (concs context), c<-[minimum [g|g<-concs context,g<=head cl]] ]
      where
       c `bi` c' = not (null [mph| mph<-declarations context, isFunction mph, isFunction (flp mph)
                               , source mph<=c && target mph<=c'  ||  source mph<=c' && target mph<=c])
       dss cl = [     makeMph d | d<-declarations context, isFunction      d , source d `elem` cl]++
                [flp (makeMph d)| d<-declarations context, isFunction (flp d), target d `elem` cl]

   datasetMor :: Context -> Morphism -> ObjectDef
   datasetMor context mph | isFunction      mph  = makeDataset context (source mph)
                          | isFunction (flp mph) = makeDataset context (target mph)
                          | otherwise            = Obj (name mph) Nowhere (Tm mph) [] []

