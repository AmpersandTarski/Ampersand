{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-} 
module DatabaseDesign.Ampersand.Fspec.ShowMeatGrinder 
  (meatGrinder)
where

import Data.List
import Data.Ord
import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Fspec.Motivations
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec.ShowADL
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import Data.Hashable

fatal :: Int -> String -> a
fatal = fatalMsg "ShowMeatGrinder.hs"

meatGrinder :: Options -> Fspc -> (FilePath, String)
meatGrinder flags fSpec = ("TemporaryPopulationsFileOfRap" ,content)
 where 
  content = unlines
     ([ "{- Do not edit manually. This code has been generated!!!"
      , "    Generated with "++ampersandVersionStr
      , "    Generated at "++show (genTime flags)
      , "-}"
      , ""
      , "CONTEXT RapPopulations"]
      ++ (concat.intersperse  []) (map (lines.showADL) (metaPops flags fSpec fSpec)) 
      ++
      [ ""
      , "ENDCONTEXT"
      ])
data Pop = Pop { popName ::String
               , popSource :: String
               , popTarget :: String
               , popPairs :: [(String,String)]
               } 
         | Comment { comment :: String  -- Not-so-nice way to get comments in a list of populations. Since it is local to this module, it is not so bad, I guess...
                   } 
instance ShowADL Pop where
 showADL pop = 
  case pop of
      Pop{} -> "POPULATION "++ popName pop++
                  " ["++popSource pop++" * "++popTarget pop++"] CONTAINS"
              ++
              if null (popPairs pop)
              then "[]" 
              else "\n"++indent++"[ "++intercalate ("\n"++indent++"; ") showContent++indent++"]"
      Comment{} -> "-- "++comment pop        
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
instance AdlId ConceptDef where 
 uri a= "CDf"++techId a
instance AdlId Rule where 
 uri a= "Rul"++techId a
instance AdlId A_Gen where 
 uri a= "Gen"++(show.hash) ((name.gengen) a++(name.genspc) a)
instance AdlId Declaration where 
 uri a= "Dcl"++techId a
instance AdlId Purpose where 
 uri a= "Prp"++(show.hash)((show.origin) a)
instance AdlId Sign where 
 uri (Sign s t) = "Sgn"++(show.hash) (uri s++uri t)
 
data RelPopuType = InitPop | CurrPop deriving Show
mkUriRelPopu :: Declaration -> RelPopuType  -> String
mkUriRelPopu d t = show t++"Of"++uri d

data Atom = Atom { atmRoot :: A_Concept -- The root concept of the atom. (this implies that there can only be a single root for
                 , atmVal :: String
                 } deriving Eq
instance AdlId Atom where
 uri a="Atm"++atmVal a++"Of"++(uri.atmRoot) a  


mkAtom :: A_Concept -> String -> Atom
mkAtom cpt value = Atom { atmRoot = root cpt
                        , atmVal  = value
                        }

class MetaPopulations a where
 metaPops :: Options -> Fspc -> a -> [Pop]

instance MetaPopulations Fspc where
 metaPops flags _ fSpec = 
   filter (not.nullContent)
    (
    [ Comment " "
    , Comment "The following declarations are all known declarations. This list should be"
    , Comment "helpfull during the developement of the meatgrinder."
    , Comment "NOTE:"
    , Comment "  The order of the declarations is determined in a special way, based on Concepts."
    ]
  ++[Comment ("  "++show i++") "++"Pop "++(show.name) dcl++" "++(show.name.source) dcl++" "++(show.name.target) dcl) | (i,dcl) <- (declOrder.allDecls)       fSpec]
  ++[ Pop "ctxnm"   "Context" "Conid"
           [(uri fSpec,name fSpec)]
    ]
  ++[ Comment " ", Comment $ "*** Patterns: (count="++(show.length.vpatterns) fSpec++") ***"]
  ++   concat [metaPops flags fSpec pat | pat <- (sortBy (comparing name).vpatterns  )    fSpec]
  ++[ Comment " ", Comment $ "*** Rules: (count="++(show.length.allRules) fSpec++")***"]
  ++   concat [metaPops flags fSpec rul | rul <- (sortBy (comparing name).allRules)    fSpec]


  ++[ Comment " ", Comment $ "*** Concepts: (count="++(show.length.allConcepts) fSpec++")***"]
  ++   concat [metaPops flags fSpec cpt | cpt <- (sortBy (comparing name).allConcepts)    fSpec]
  ++[ Comment " ", Comment $ "*** Generalisations: (count="++(show.length.vgens) fSpec++") ***"]
  ++   concat [metaPops flags fSpec gen | gen <- vgens          fSpec]
  ++[ Comment " ", Comment $ "*** Declarations: (count="++(show.length.allDecls) fSpec++") ***"]
  ++   concat [metaPops flags fSpec dcl | (_,dcl) <- (declOrder.allDecls)       fSpec]
  ++[ Comment " ", Comment $ "*** Atoms: ***"]
  ++   concat [metaPops flags fSpec atm | atm <- allAtoms]
  )
   where
    allAtoms :: [Atom]
    allAtoms = nub (concatMap atoms (userDefPops fSpec))
      where 
        atoms :: UserDefPop -> [Atom]
        atoms udp = case udp of
          PRelPopu{} ->  map (mkAtom ((source.popdcl) udp).fst) (popps udp) 
                      ++ map (mkAtom ((target.popdcl) udp).snd) (popps udp) 
          PCptPopu{} ->  map (mkAtom (        popcpt  udp)    ) (popas udp)
    nullContent :: Pop -> Bool
    nullContent (Pop _ _ _ []) = True
    nullContent _ = False
    -- | the order of declarations is done by an order of the concepts, which is a hardcoded list 
    declOrder ::[Declaration] -> [(Int,Declaration)]
    declOrder decls = zip [1..] (concat (map (sortBy f) (declGroups conceptOrder decls))) 
      where 
        conceptOrder = ["Pattern"
                       ,"Rule"
                       ,"Declaration"
                       ,"RelPopu"
                       ,"A_Concept"
                       ,"Concept"
                       ]
        f a b =
          case comparing (name.source) a b of
            EQ -> case comparing (name.target) a b of
                    EQ -> comparing (name) a b 
                    x  -> x
            x -> x
        declGroups :: [String] -> [Declaration] -> [[Declaration]]
        declGroups [] ds = [ds]
        declGroups (nm:nms) ds = [x] ++ declGroups nms y
           where
             (x,y) = partition crit ds
             crit dcl = or [(name.source) dcl == nm, (name.target) dcl == nm]
instance MetaPopulations Pattern where
 metaPops _ fSpec pat = 
   [ Comment " "
   , Comment $ "*** Pattern `"++name pat++"` ***"
   , Pop "ctxpats" "Context" "Pattern"
          [(uri fSpec,uri pat)]
   , Pop "ptxps"   "Pattern" "Blob"
          [(uri pat,uri x) | x <- ptxps pat]
   , Pop "ptnm"    "Pattern" "Conid"
          [(uri pat, ptnm pat)]
   , Pop "ptctx" "Pattern" "Context"
          [(uri pat,uri fSpec)]
   , Pop "ptdcs"   "Pattern" "Declaration"
          [(uri pat,uri x) | x <- ptdcs pat]
   , Pop "ptgns"   "Pattern" "Gen"
          [(uri pat,uri x) | x <- ptgns pat]
--HJO, 20130728: TODO: De Image (Picture) van het pattern moet worden gegenereerd op een of andere manier:
--   , Pop "ptpic"   "Pattern" "Image"
--          [(uri pat,uri x) | x <- ptpic pat]
   , Pop "ptrls"   "Pattern" "Rule"
          [(uri pat,uri x) | x <- ptrls pat]
   ]
instance MetaPopulations Rule where
 metaPops _ fSpec rul =
   [ Comment " "
   , Comment $ "*** Rule `"++name rul++"` ***"
   , Pop "rrnm"  "Rule" "ADLid"
          [(uri rul,rrnm rul)]
   , Pop "rrmean"  "Rule" "Blob"
          [(uri rul,show(rrmean rul))]
   , Pop "rrpurpose"  "Rule" "Blob"
          [(uri rul,showADL x) | x <- explanations rul]
   , Pop "rrexp"  "Rule" "ExpressionID"
          [(uri rul,showADL (rrexp rul))]
--HJO, 20130728: TODO: De Image (Picture) van de rule moet worden gegenereerd op een of andere manier:
--   , Pop "rrpic"   "Rule" "Image"
--          [(uri rul,uri x) | x <- rrpic pat]
   , Pop "rrviols"  "Rule" "Violation"
          [(uri rul,show v) | (r,v) <- allViolations fSpec, r == rul]

   ]
instance MetaPopulations Declaration where
 metaPops flags fSpec dcl = 
   case dcl of 
     Sgn{} ->
      [ Comment " "
      , Comment $ "*** Declaration `"++name dcl++" ["++(name.source.decsgn) dcl++" * "++(name.target.decsgn) dcl++"]"++"` ***"
      , Pop "decmean"    "Declaration" "Blob"
             [(uri dcl, show(decMean dcl))]
      , Pop "decpurpose" "Declaration" "Blob"
             [(uri dcl, showADL x) | x <- explanations dcl]
      , Pop "decexample"    "Declaration" "PragmaSentence"
             [(uri dcl, unwords ["PRAGMA",show (decprL dcl),show (decprM dcl),show (decprR dcl)])]
      , Pop "decprps" "Declaration" "PropertyRule"
             [(uri dcl, uri rul) | rul <- filter ofDecl (allRules fSpec)]
      , Pop "decpopu" "Declaration" "RelPopu"
             [(uri dcl,mkUriRelPopu dcl CurrPop)] 
      , Pop "inipopu" "Declaration" "RelPopu"
             [(uri dcl,mkUriRelPopu dcl InitPop)] 
      , Pop "decsgn" "Declaration" "Sign"
             [(uri dcl,uri (decsgn dcl))]
      , Pop "decprL" "Declaration" "String"
             [(uri dcl,decprL dcl)]
      , Pop "decprM" "Declaration" "String"
             [(uri dcl,decprM dcl)]
      , Pop "decprR" "Declaration" "String"
             [(uri dcl,decprR dcl)]
      , Pop "decnm" "Declaration" "Varid"
             [(uri dcl, name dcl)]
 --TODO HIER GEBLEVEN. (HJO, 20130802)

--      , Pop "rels" "ExpressionID" "Declaration"
--      , Pop "popdcl" "RelPopu" "Declaration"


 
      ] 
             
     Isn{} -> fatal 157 "Isn is not implemented yet"
     Vs{}  -> fatal 158 "Vs is not implemented yet"
    where
      ofDecl :: Rule -> Bool
      ofDecl rul = case rrdcl rul of
                     Nothing -> False
                     Just (_,d) -> d == dcl   
instance MetaPopulations Atom where
 metaPops _ _ atm = 
   [ Pop "root"  "Atom" "Concept"
          [(uri atm,uri(atmRoot atm))]
   , Pop "atomvalue"  "Atom" "AtomValue"
          [(uri atm,atmVal atm)]
   ]
   
instance MetaPopulations A_Gen where
 metaPops _ _ gen = 
   [ Pop "gengen"  "Gen" "Concept"
          [(uri gen,uri(gengen gen))]
   , Pop "genspc"  "Gen" "Concept"
          [(uri gen,uri(genspc gen))]
   ]
instance MetaPopulations A_Concept where
 metaPops _ fSpec cpt = 
   case cpt of
     PlainConcept{} ->
      [ Comment " "
      , Comment $ "*** Concept `"++name cpt++"` ***"
      , Pop "ctxcs"   "Context" "Concept"
           [(uri fSpec,uri cpt)]
      , Pop "cptnm"      "PlainConcept" "Conid"
             [(uri cpt, name cpt)]
      , Pop "context"    "PlainConcept" "Context"
             [(uri cpt,uri fSpec)]
      , Pop "cpttp"      "PlainConcept" "Blob"
             [(uri cpt,cpttp cpt) ]
      , Pop "cptdf"      "PlainConcept" "Blob"
             [(uri cpt,showADL x) | x <- cptdf cpt]
--      , Pop "cptpurpose" "PlainConcept" "Blob"
--             [(uri cpt,showADL x) | x <- purposeOf fSpec cpt ]
      ]
     ONE -> [
            ]
instance MetaPopulations Sign where
 metaPops _ _ sgn = 
      [ Pop "src"    "Sign" "Concept"
             [(uri sgn, uri (source sgn))]
      , Pop "trg"   "Sign" "Concept"
             [(uri sgn, uri (target sgn))]
      ]

   