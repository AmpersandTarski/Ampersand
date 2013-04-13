{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.ShowMeatGrinder 
  (meatGrinder)
where

import Data.List
import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec.ShowADL
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import Data.Hashable
   
meatGrinder :: Options -> Fspc -> (FilePath, String)
meatGrinder flags fSpec = ("TemporaryPopulationsFileOfRap" ,content)
 where 
  content = unlines
     ([ "{-Generated code by "++ampersandVersionStr++" at "++show (genTime flags)++"-}"
      , "CONTEXT RapPopulations"]
      ++ (concat.intersperse  []) (map (lines.showADL) (metaPopsOf flags fSpec)) 
      ++
      [ "ENDCONTEXT"
      ])
data Pop = Pop { popName ::String
               , popSource :: String
               , popTarget :: String
               , popPairs :: [(String,String)]
               }
instance ShowADL Pop where
 showADL pop = 
   ("POPULATION "++ popName pop++
       " ["++popSource pop++" * "++popTarget pop++"] CONTAINS"
   ++
   if null (popPairs pop)
   then "[]" 
   else "\n"++indent++"[ "++intercalate ("\n"++indent++"; ") showContent++indent++"]"
   )
    where indent = "   "
          showContent = map showPaire (popPairs pop)
          showPaire (s,t) = "( "++show s++" , "++show t++" )"

techId :: Identified a => a -> String
techId = show.hash.name
class AdlId a where
 uri :: a -> String

instance AdlId Fspc where 
 uri a= "Ctx"++techId a
instance AdlId Pattern where 
 uri a= "Pat"++techId a
instance AdlId A_Concept where 
 uri a= "Cpt"++techId a
instance AdlId Rule where 
 uri a= "Rul"++techId a
instance AdlId A_Gen where 
 uri a= "Gen"++(show.hash) ((name.gengen) a++(name.genspc) a)
instance AdlId Declaration where 
 uri a= "Dcl"++techId a
instance AdlId Purpose where 
 uri a= "Prp"++(show.hash)((show.origin) a)
 
metaPopsOf :: Options -> Fspc -> [Pop]
metaPopsOf _ fSpec = 
  [ Pop "ctxnm"   "Context" "Conid"
         [(uri fSpec,(show.name) fSpec)]
  , Pop "ctxpats" "Context" "Pattern"
         [(uri fSpec,uri x) | x <- vpatterns fSpec]
  , Pop "ctxcs"   "Context" "Concept"
         [(uri fSpec,uri x) | x <- allConcepts fSpec]
  ]
 ++ concat [metaPopsOfPatternPattern pat | pat <- vpatterns fSpec]
 ++ concat [metaPopsOfPatternGen     gen | gen <- vgens     fSpec]

 where
  metaPopsOfPatternPattern pat =  
   [ Pop "ptnm"    "Pattern" "Conid"
          [(uri pat, ptnm pat)]
   , Pop "ptrls"   "Pattern" "Rule"
          [(uri pat,uri x) | x <- ptrls pat]
   , Pop "ptgns"   "Pattern" "Gen"
          [(uri pat,uri x) | x <- ptgns pat]
   , Pop "ptdcs"   "Pattern" "Declaration"
          [(uri pat,uri x) | x <- ptdcs pat]
   , Pop "ptxps"   "Pattern" "Blob"
          [(uri pat,uri x) | x <- ptxps pat]
   ]
  metaPopsOfPatternGen gen =  
   [ Pop "gengen"  "Gen" "Concept"
          [(uri gen,uri(gengen gen))]
   , Pop "genspc"  "Gen" "Concept"
          [(uri gen,uri(genspc gen))]
   ]
--  metaPopsOfPatternConcept cpt =  
--   [ Pop "cptnm"    "Concept" "Conid"
--          [(uri cpt, cptnm cpt)]
--   , Pop "cptos"   "Concept" "AtomID"
--          [(uri cpt,uri x) | x <- cptos cpt]
--   , Pop "cptdf"   "Concept" "AtomID"
--          [(uri cpt,uri x) | x <- cptdf cpt]
--   , Pop "cptpurpose"   "Concept" "Blob"
--          [(uri cpt,uri x) | x <- cptpurpose cpt]
--   ]
   
   
   