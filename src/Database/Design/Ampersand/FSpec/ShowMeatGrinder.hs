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

class Unique a => AdlId a where
 uri :: a -> String
 uri = camelCase . uniqueShow True
-- All 'things' that are relevant in the meta-environment (RAP),
-- must be an instance of AdlId:
instance AdlId A_Concept
instance AdlId A_Gen
instance AdlId AtomID
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
   AtomID { atmRoot = rootConcepts gens [cpt]
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
 
instance MetaPopulations a => MetaPopulations [a] where
 metaPops fSpec = concatMap $ metaPops fSpec
 
instance MetaPopulations FSpec where
 metaPops _ fSpec =
   filter (not.nullContent)
    (
    [ Comment " "
    , Comment "The following relations are all known to be declared. This list"
    , Comment "should be helpful during the developement of the meatgrinder."
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
   , Pop "rrexp"  "Rule" "ExpressionID"
          [(uri rul,uri (rrexp rul))]
   , Pop "rrmean"  "Rule" "Blob"
          [(uri rul,show(rrmean rul))]
   , Pop "rrtyp"  "Rule" "Sign"
          [(uri rul,uri(rrtyp rul))]
   , Pop "rrpurpose"  "Rule" "Blob"
          [(uri rul,showADL x) | x <- explanations rul]
--HJO, 20130728: TODO: De Image (Picture) van de rule moet worden gegenereerd op een of andere manier:
--   , Pop "rrpic"   "Rule" "Image"
--          [(uri rul,uri x) | x <- rrpic pat]
   , Pop "rrviols"  "Rule" "Violation"
          [(uri rul,show v) | (r,v) <- allViolations fSpec, r == rul]
   , Pop "sign"  "Rule" "Sign"
          [(uri rul,(uri.sign) rul)]

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

instance MetaPopulations AtomID where
 metaPops _ atm = 
   [ Pop "atmRoot" "AtomId" "Concept" 
          [(uri atm,uri (atmRoot atm))]
   , Pop "atmIn" "AtomId" "Concept" 
          [(uri atm,uri cpt)|cpt<-atmIn atm]
   , Pop "atomvalue"  "AtomID" "Atom"
          [(uri atm,(show.atmVal) atm)]
   ]
instance MetaPopulations PairID where
 metaPops _ p =
  [ Pop "lnkSgn" "PairID" "Sign"
         [(uri p, uri(lnkSgn p))]
  , Pop "left" "Link" "AtomID"
         [(uri p, uri(lnkLeft p))]
  , Pop "right" "Link" "AtomID"
         [(uri p, uri(lnkRight p))]
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
      , Pop "in" "PlainConcept" "Plug"
             [(uri cpt, uri plug) | plug <- nub $ map fst (lookupCpt fSpec cpt)]
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
  