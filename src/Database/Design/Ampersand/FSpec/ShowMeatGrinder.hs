{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Database.Design.Ampersand.FSpec.ShowMeatGrinder
  (meatGrinder)
where

import Data.List
import Data.Char
import Data.Ord
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.FSpec.Motivations
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.FSpec.ShowADL
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.Core.ParseTree
--import Data.Hashable
import Data.Maybe

fatal :: Int -> String -> a
fatal = fatalMsg "FSpec.ShowMeatGrinder"

meatGrinder :: FSpec -> (FilePath, String)
meatGrinder fSpec = ("TemporaryPopulationsFileOfRap" ,content)
 where
  content = unlines
     ([ "{- Do not edit manually. This code has been generated!!!"
      , "    Generated with "++ampersandVersionStr
      , "    Generated at "++show (genTime (getOpts fSpec))
      , "-}"
      , ""
      , "CONTEXT RapPopulations IN ENGLISH -- (the language is chosen arbitrary, for it is mandatory but irrelevant."]
      ++ (concat.intersperse  []) (map (lines.showADL) (metaPops fSpec fSpec))
      ++
      [ ""
      , "ENDCONTEXT"
      ])

instance MetaPopulations FSpec where
 metaPops _ fSpec =
   filter (not.nullContent)
    (
    [ Comment " "
    , Comment "The populations defined in this file are the populations from the user's"
    , Comment $ "model named '"++name fSpec++"'."
    , Comment ""
    , Comment "The order in which these populations are defined correspond with the order "
    , Comment "in which Ampersand is defined in itself. Currently (Feb. 2015), this is hard-"
    , Comment "coded. This means, that whenever Formal Ampersand changes, it might have "
    , Comment "impact on the generator of this file. "
    , Comment ""
    ]
  ++[Comment  " ", Comment $ "PATTERN Context: ('"++name fSpec++"')"]
  ++[ Pop "ctxnm"   "Context" "Conid"
           [(uri fSpec,name fSpec)]]
  ++[ Pop "ctxpats" "Context" "Pattern"
           [(uri fSpec,uri pat) | pat <- vpatterns fSpec]]
  ++[ Pop "ctxcs" "Context" "PlainConcept"
           [(uri fSpec,uri cpt) | cpt <- allConcepts fSpec]]
  ++[ Comment " ", Comment $ "PATTERN Patterns: (count="++(show.length.vpatterns) fSpec++")"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).vpatterns)    fSpec)
  ++[ Comment " ", Comment $ "PATTERN Specialization: (count="++(show.length.vgens) fSpec++")"]
  ++   concatMap (metaPops fSpec) (vgens          fSpec)
  ++[ Comment " ", Comment $ "PATTERN Concept: (count="++(show.length.allConcepts) fSpec++")***"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).allConcepts)    fSpec)
  ++[ Comment " ", Comment $ "PATTERN Atoms: (count="++(show.length) allAtoms++")"]
  ++   concatMap (metaPops fSpec) allAtoms
  ++[ Comment " ", Comment $ "PATTERN Sign: (count="++(show.length) allSigns++")"]
  ++   concatMap (metaPops fSpec) allSigns
  ++[ Comment " ", Comment $ "PATTERN Declaration: (count="++(show.length.allDecls) fSpec++")"]
  ++   concatMap (metaPops fSpec) (allDecls  fSpec)
  ++[ Comment " ", Comment $ "PATTERN Expression: (count="++(show.length.allExprs) fSpec++")"]
  ++   concatMap (metaPops fSpec) (allExprs  fSpec)
  ++[ Comment " ", Comment $ "PATTERN Rules: (count="++(show.length.allRules) fSpec++")***"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).allRules)    fSpec)
  ++[ Comment " ", Comment $ "PATTERN Plugs: (count="++(show.length.allRules) fSpec++")***"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).plugInfos)    fSpec)




  )
   where
    allAtoms :: [AtomID]
    allAtoms = nub (concatMap atoms (initialPops fSpec))
      where
        atoms :: Population -> [AtomID]
        atoms udp = case udp of
          PRelPopu{} ->  map (mkAtom fSpec ((source.popdcl) udp).srcPaire) (popps udp)
                      ++ map (mkAtom fSpec ((target.popdcl) udp).trgPaire) (popps udp)
          PCptPopu{} ->  map (mkAtom fSpec (        popcpt  udp)         ) (popas udp)
    allSigns :: [Sign]
    allSigns = [] --TODO. 
    nullContent :: Pop -> Bool
    nullContent (Pop _ _ _ []) = True
    nullContent _ = False

instance MetaPopulations Pattern where
 metaPops _ pat =
   [ Comment " "
   , Comment $ "*** Pattern `"++name pat++"` ***"
   , Pop "ptnm"    "Pattern" "Conid"
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
instance MetaPopulations A_Gen where
 metaPops _ gen =
  [ Pop "genspc"  "Gen" "PlainConcept"
          [(uri gen,uri(genspc gen))]
  , Pop "gengen"  "Gen" "PlainConcept"
          [(uri gen,uri c) | c<- case gen of
                                   Isa{} -> [gengen gen]
                                   IsE{} -> genrhs gen
          ]
  ]

instance MetaPopulations A_Concept where
 metaPops fSpec cpt =
   case cpt of
     PlainConcept{} ->
      [ Comment " "
      , Comment $ "*** Concept `"++name cpt++"` ***"
      , Pop "cptnm" "PlainConcept" "Conid"
             [(uri cpt, name cpt)]
      , Pop "cptdf" "PlainConcept" "Blob"
             [(uri cpt,showADL cdef) | cdef <- conceptDefs  fSpec, name cdef == name cpt]
      , Pop "cptpurpose" "PlainConcept" "Blob"
             [(uri cpt,showADL x) | lang <- allLangs, x <- fromMaybe [] (purposeOf fSpec lang cpt) ]
      , Pop "cpttp" "PlainConcept" "Blob"
             [(uri cpt,cpttp cpt)  | not.null.cpttp $ cpt
             ]
      ]
     ONE -> [
            ]

instance MetaPopulations AtomID where
 metaPops _ atm = 
   [ Pop "atmRoot" "AtomID" "PlainConcept" 
          [(uri atm, uri cpt) | cpt <- atmRoots atm]
   , Pop "in" "AtomID" "PlainConcept" 
          [(uri atm, uri cpt) | cpt <- atmIn atm]
   , Pop "atomvalue"  "AtomID" "AtomValue"
          [(uri atm,(show.atmVal) atm)]
   ]
instance MetaPopulations Sign where
 metaPops _ sgn =
      [ Pop "src"    "Sign" "Concept"
             [(uri sgn, uri (source sgn))]
      , Pop "trg"   "Sign" "Concept"
             [(uri sgn, uri (target sgn))]
      ]

instance MetaPopulations Declaration where
 metaPops fSpec dcl =
   case dcl of
     Sgn{} ->
      [ Comment " "
      , Comment $ "*** Declaration `"++name dcl++" ["++(name.source.decsgn) dcl++" * "++(name.target.decsgn) dcl++"]"++"` ***"
      , Pop "decnm" "Declaration" "Varid"
             [(uri dcl, name dcl)]
      , Pop "decsgn" "Declaration" "Sign"
             [(uri dcl,uri (decsgn dcl))]
      , Comment $ " PropertyRules of "++name dcl++":"
      , Pop "decprps" "Declaration" "PropertyRule"
             [(uri dcl, uri rul) | rul <- filter ofDecl (allRules fSpec)]
      , Pop "declaredthrough" "PropertyRule" "Property"
             [(uri rul,show prp) | rul <- filter ofDecl (grules fSpec), Just (prp,d) <- [rrdcl rul], d == dcl]
      , Pop "decprL" "Declaration" "String"
             [(uri dcl,decprL dcl)]
      , Pop "decprM" "Declaration" "String"
             [(uri dcl,decprM dcl)]
      , Pop "decprR" "Declaration" "String"
             [(uri dcl,decprR dcl)]
      , Pop "decmean" "Declaration" "Blob"
             [(uri dcl, show(decMean dcl))]
      , Pop "decpurpose" "Declaration" "Blob"
             [(uri dcl, showADL x) | x <- explanations dcl]
      , Comment $ "The population of "++name dcl++":"
      , Pop "decpopu" "Declaration" "PairID"
             [(uri dcl,uri p) | p <- mkLinks fSpec (sign dcl) (pairsOf dcl)]
      ]++ metaPops fSpec ( mkLinks fSpec (sign dcl) (pairsOf dcl))

     Isn{} -> fatal 157 "Isn is not implemented yet"
     Vs{}  -> fatal 158 "Vs is not implemented yet"
    where
      ofDecl :: Rule -> Bool
      ofDecl rul = case rrdcl rul of
                     Nothing -> False
                     Just (_,d) -> d == dcl
      pairsOf :: Declaration -> Pairs
      pairsOf d = case filter theDecl (initialPops fSpec) of
                    []    -> []
                    [pop] -> popps pop
                    _     -> fatal 273 "Multiple entries found in populationTable"
        where
          theDecl :: Population -> Bool
          theDecl p = case p of
                        PRelPopu{} -> popdcl p == d
                        PCptPopu{} -> False

instance MetaPopulations Expression where
 metaPops _ e =
      [ Pop "exprvalue" "ExpressionID" "Expression"
             [(uri e, showADL e)]
      ]

instance MetaPopulations Rule where
 metaPops _ rul =
   [ Comment " "
   , Comment $ "*** Rule `"++name rul++"` ***"
   , Pop "rrnm"  "Rule" "ADLid"
          [(uri rul,rrnm rul)]
   , Pop "rrexp"  "Rule" "ExpressionID"
          [(uri rul,uri (rrexp rul))]
   , Pop "rrmean"  "Rule" "Blob"
          [(uri rul,show(rrmean rul))]
   , Pop "rrpurpose"  "Rule" "Blob"
          [(uri rul,showADL x) | x <- explanations rul]
   ]

instance MetaPopulations PlugInfo where
 metaPops _ _ = 
      [ Comment $ "TODO:  PlugInfo"
--      , Pop "in" "PlainConcept" "Plug"
--             [(uri cpt, uri plug) | plug <- nub $ map fst (lookupCpt fSpec cpt)]
      ]      

instance MetaPopulations a => MetaPopulations [a] where
 metaPops fSpec = concatMap $ metaPops fSpec
 
instance MetaPopulations PairID where
 metaPops _ p =
  [ Pop "sign" "PairID" "Sign"
         [(uri p, uri(lnkSgn p))]
  , Pop "left" "PairID" "AtomID"
         [(uri p, uri(lnkLeft p))]
  , Pop "right" "PairID" "AtomID"
         [(uri p, uri(lnkRight p))]
  ]


-----------------------------------------------------
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
              else "\n"++indentA++"[ "++intercalate ("\n"++indentA++"; ") showContent++indentA++"]"
      Comment{} -> "-- "++comment pop
    where indentA = "   "
          showContent = map showPaire (popPairs pop)
          showPaire (s,t) = "( "++show s++" , "++show t++" )"

class Unique a => AdlId a where
 uri :: a -> String
 uri = camelCase . uniqueShow True
-- All 'things' that are relevant in the meta-environment (RAP),
-- must be an instance of AdlId:
instance AdlId A_Concept
instance AdlId A_Gen
instance AdlId AtomID
instance AdlId ConceptDef
instance AdlId Declaration
instance AdlId Expression
instance AdlId FSpec
instance AdlId PairID
instance AdlId Pattern
instance AdlId PlugSQL
instance AdlId Purpose
instance AdlId Rule
instance AdlId Sign
instance AdlId a => AdlId [a] where

mkAtom :: FSpec  -> A_Concept -> String -> AtomID
mkAtom fSpec cpt value = 
   AtomID { atmRoots = rootConcepts gens [cpt]
          , atmIn   = largerConcepts gens cpt `uni` [cpt]
          , atmVal  = value
          }
  where
    gens = vgens fSpec
mkLink :: FSpec -> Sign -> Paire -> PairID
mkLink fSpec sgn p 
  = PairID { lnkSgn = sgn
         , lnkLeft  = mkAtom fSpec (source sgn) (srcPaire p) 
         , lnkRight = mkAtom fSpec (target sgn) (trgPaire p)
         }
mkLinks :: FSpec -> Sign -> [Paire] -> [PairID]
mkLinks fSpec sgn = map $ mkLink fSpec sgn


-- | remove spaces and make camelCase
camelCase :: String -> String
camelCase str = concatMap capitalize (words str)
  where
    capitalize [] = []
    capitalize (s:ss) = toUpper s : ss
    
class MetaPopulations a where
 metaPops :: FSpec -> a -> [Pop]
   