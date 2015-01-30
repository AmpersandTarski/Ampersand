{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Database.Design.Ampersand.FSpec.ShowMeatGrinder
  (meatGrinder)
where

import Data.List
import Data.Ord
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.FSpec.Motivations
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.FSpec.ShowADL
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.ADL1.Pair
import Data.Hashable
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
      , "CONTEXT RapPopulations IN ENGLISH"]
      ++ (concat.intersperse  []) (map (lines.showADL) (metaPops fSpec fSpec))
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
              else "\n"++indentA++"[ "++intercalate ("\n"++indentA++"; ") showContent++indentA++"]"
      Comment{} -> "-- "++comment pop
    where indentA = "   "
          showContent = map showPaire (popPairs pop)
          showPaire (s,t) = "( "++show s++" , "++show t++" )"

techId :: Identified a => a -> String
techId = show.hash.name
class AdlId a where
 uri :: a -> String

instance AdlId FSpec where
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
 uri a= "Gen"++(show.hash) g
        where g = case a of
                    Isa{} -> ((name.gengen) a++(name.genspc) a)
                    IsE{} -> ((concat.map name.genrhs) a++(name.genspc) a)
instance AdlId Declaration where
 uri a= "Dcl"++techId a
instance AdlId Purpose where
 uri a= "Prp"++(show.hash)((show.origin) a)
instance AdlId Sign where
 uri (Sign s t) = "Sgn"++(show.hash) (uri s++uri t)
instance AdlId Paire where
 uri p = "Paire"++(show.hash) (srcPaire p++"_"++trgPaire p)

instance AdlId Atom where
 uri a="Atm"++atmVal a++"Of"++(uri.atmRoot) a

mkAtom :: A_Concept -> String -> Atom
mkAtom cpt value = Atom { atmRoot = cpt
                        , atmVal  = value
                        }

class MetaPopulations a where
 metaPops :: FSpec -> a -> [Pop]

instance MetaPopulations FSpec where
 metaPops _ fSpec =
   filter (not.nullContent)
    (
    [ Comment " "
    , Comment "The following relations are all known to be declared. This list"
    , Comment "should be helpful during the developement of the meatgrinder."
    , Comment "NOTE:"
    , Comment "  The order of the relations is determined in a special way, based on Concepts."
    ]
  ++[Comment ("  "++show i++") "++"Pop "++(show.name) dcl++" "++(show.name.source) dcl++" "++(show.name.target) dcl) | (i,dcl) <- (declOrder.allDecls)       fSpec]
  ++[ Pop "ctxnm"   "Context" "Conid"
           [(uri fSpec,name fSpec)]
    ]
  ++[ Comment " ", Comment $ "*** Patterns: (count="++(show.length.vpatterns) fSpec++") ***"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).vpatterns)    fSpec)
  ++[ Comment " ", Comment $ "*** Rules: (count="++(show.length.allRules) fSpec++")***"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).allRules)    fSpec)

  ++[ Comment " ", Comment $ "*** Concepts: (count="++(show.length.allConcepts) fSpec++")***"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).allConcepts)    fSpec)
  ++[ Comment " ", Comment $ "*** Generalisations: (count="++(show.length.vgens) fSpec++") ***"]
  ++   concatMap (metaPops fSpec) (vgens          fSpec)
  ++[ Comment " ", Comment $ "*** Declarations: (count="++(show.length.allDecls) fSpec++") ***"]
  ++   concatMap (metaPops fSpec) ((map snd.declOrder.allDecls)       fSpec)
  ++[ Comment " ", Comment $ "*** Expressions: (count="++(show.length.allExprs) fSpec++") ***"]
  ++   concatMap (metaPops fSpec) (allExprs  fSpec)
  ++[ Comment " ", Comment $ "*** Atoms: (count="++(show.length) allAtoms++") ***"]
  ++   concatMap (metaPops fSpec) allAtoms
  ++[ Comment " ", Comment $ "*** Pairs: (count="++(show.length) allPairs++") ***"]
  ++   concatMap (metaPops fSpec) allPairs
  )
   where
    allAtoms :: [Atom]
    allAtoms = nub (concatMap atoms (initialPops fSpec))
      where
        atoms :: Population -> [Atom]
        atoms udp = case udp of
          PRelPopu{} ->  map (mkAtom ((source.popdcl) udp).srcPaire) (popps udp)
                      ++ map (mkAtom ((target.popdcl) udp).trgPaire) (popps udp)
          PCptPopu{} ->  map (mkAtom (        popcpt  udp)         ) (popas udp)
    allPairs :: [Paire]
    allPairs= nub (concatMap pairs (initialPops fSpec))
      where
        pairs :: Population -> [Paire]
        pairs udp = case udp of
          PRelPopu{} -> popps udp
          PCptPopu{} -> []
    nullContent :: Pop -> Bool
    nullContent (Pop _ _ _ []) = True
    nullContent _ = False
    -- | the order of relations is done by an order of the concepts, which is a hardcoded list
    declOrder ::[Declaration] -> [(Int,Declaration)]
    declOrder decls = zip [1..] (concatMap (sortBy f) (declGroups conceptOrder decls))
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
 metaPops fSpec pat =
   [ Comment " "
   , Comment $ "*** Pattern `"++name pat++"` ***"
   , Pop "ctxpats" "Context" "Pattern"
          [(uri fSpec,uri pat)]
   , Pop "ptxps"   "Pattern" "Blob"
          [(uri pat,uri x) | x <- ptxps pat]
   , Pop "ptnm"    "Pattern" "Conid"
          [(uri pat, ptnm pat)]
-- following fiedls would be double (ptctx = ~ctxpats)
--   , Pop "ptctx" "Pattern" "Context"
--          [(uri pat,uri fSpec)]
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
 metaPops fSpec rul =
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
 metaPops fSpec dcl =
   case dcl of
     Sgn{} ->
      [ Comment " "
      , Comment $ "*** Declaration `"++name dcl++" ["++(name.source.decsgn) dcl++" * "++(name.target.decsgn) dcl++"]"++"` ***"
      , Pop "decmean"    "Declaration" "Blob"
             [(uri dcl, show(decMean dcl))]
      , Pop "decpurpose" "Declaration" "Blob"
             [(uri dcl, showADL x) | x <- explanations dcl]
--      , Pop "decexample"    "Declaration" "PragmaSentence"
--             [(uri dcl, unwords ["PRAGMA",show (decprL dcl),show (decprM dcl),show (decprR dcl)])]
      , Pop "decprps" "Declaration" "PropertyRule"
             [(uri dcl, uri rul) | rul <- filter ofDecl (allRules fSpec)]
      , Pop "declaredthrough" "PropertyRule" "Property"
             [(uri rul,show prp) | rul <- filter ofDecl (allRules fSpec), Just (prp,d) <- [rrdcl rul], d == dcl]
      , Pop "decpopu" "Declaration" "PairID"
             [(uri dcl, uri p) | p <- pairsOf dcl]
--      , Pop "inipopu" "Declaration" "PairID"
--             [(uri dcl,mkUriRelPopu dcl InitPop)]
      , Pop "decsgn" "Declaration" "Sign"
             [(uri dcl,uri (decsgn dcl))]
      , Pop "src" "Sign" "Concept"
             [((uri.decsgn) dcl,(uri.source.decsgn) dcl)]
      , Pop "trg" "Sign" "Concept"
             [((uri.decsgn) dcl,(uri.target.decsgn) dcl)]
      , Pop "decprL" "Declaration" "String"
             [(uri dcl,decprL dcl)]
      , Pop "decprM" "Declaration" "String"
             [(uri dcl,decprM dcl)]
      , Pop "decprR" "Declaration" "String"
             [(uri dcl,decprR dcl)]
      , Pop "decnm" "Declaration" "Varid"
             [(uri dcl, name dcl)]
 --TODO HIER GEBLEVEN. (HJO, 20130802)
--      , Pop "cptos" "PlainConcept" "AtomID"
--      , Pop "rels" "ExpressionID" "Declaration"
--relnm : Relation × Varid The name of a relation used as a relation token.
--relsgn : Relation × Sign The signature of a relation.
--reldcl : Relation × Declaration A relation token refers to a relation.
--rrnm : Rule × ADLid The name of a rule.
--rrexp : Rule × ExpressionID The rule expressed in relation algebra.
--rrmean : Rule × Blob The meanings of a rule.
--rrpurpose : Rule × Blob The purposes of a rule.
      ]

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

instance MetaPopulations Atom where
 metaPops _ _ = []
--   [ Pop "root"  "AtomID" "Concept"
--          [(uri atm,uri(atmRoot atm))]
--   , Pop "atomvalue"  "AtomID" "AtomValue"
--          [(uri atm,atmVal atm)]
--   ]
instance MetaPopulations Paire where
 metaPops _ p =
  [ Pop "left" "PairID" "AtomID"
         [(uri p, srcPaire p)]
  , Pop "right" "PairID" "AtomID"
         [(uri p, trgPaire p)]
  ]
instance MetaPopulations A_Gen where
 metaPops _ gen =
  [ Pop "genspc"  "Gen" "PlainConcept"
            [(uri gen,uri(genspc gen))]
  ]++
  case gen of
   Isa{} ->
     [ Pop "gengen"  "Gen" "PlainConcept"
            [(uri gen,uri(gengen gen))]
     ]
   IsE{} ->
     [ Pop "genrhs"  "Gen" "PlainConcept"
          [(uri gen,uri c) | c<-genrhs gen]
     ]
instance MetaPopulations A_Concept where
 metaPops fSpec cpt =
   case cpt of
     PlainConcept{} ->
      [ Comment " "
      , Comment $ "*** Concept `"++name cpt++"` ***"
      , Pop "ctxcs"   "Context" "PlainConcept"
           [(uri fSpec,uri cpt)]
      , Pop "cptnm"      "Concept" "Conid"
             [(uri cpt, name cpt)]
-- removed: equals ctxcs~
--     , Pop "context"    "PlainConcept" "Context"
--             [(uri cpt,uri fSpec)]
-- removed:
--    , Pop "cpttp"      "PlainConcept" "Blob"
--           [(uri cpt,cpttp cpt) ]
-- removed:
--    , Pop "cptdf"      "PlainConcept" "Blob"
--           [(uri cpt,showADL x) | x <- cptdf cpt]
      , Pop "cptpurpose" "PlainConcept" "Blob"
             [(uri cpt,showADL x) | lang <- [English,Dutch], x <- fromMaybe [] (purposeOf fSpec lang cpt) ]
      ]
     ONE -> [
            ]
instance MetaPopulations Sign where
 metaPops _ sgn =
      [ Pop "src"    "Sign" "Concept"
             [(uri sgn, uri (source sgn))]
      , Pop "trg"   "Sign" "Concept"
             [(uri sgn, uri (target sgn))]
      ]

instance MetaPopulations Expression where
 metaPops _ e =
      [ Pop "exprvalue" "ExpressionID" "Expression"
             [(showADL e, showADL e)]
      ]
  