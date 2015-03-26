{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Database.Design.Ampersand.FSpec.ShowMeatGrinder
  (meatGrinder,makeGenerics)
where

import Data.List
import Data.Char
import Data.Ord
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.FSpec.FSpecAux
import Database.Design.Ampersand.FSpec.Motivations
import Database.Design.Ampersand.FSpec.SQL
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.FSpec.ShowADL
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.Classes.ConceptStructure
import Database.Design.Ampersand.Core.ParseTree

--import Data.Hashable
import Data.Maybe

fatal :: Int -> String -> a
fatal = fatalMsg "ShowMeatGrinder"

makeGenerics :: FSpec -> (FilePath,String)
makeGenerics fSpec = ("TemporaryPopulationsFileOfGenerics" ,content (generics fSpec) "Generics" fSpec )
meatGrinder :: FSpec -> (FilePath, String)
meatGrinder fSpec = ("TemporaryPopulationsFileOfRap" ,content (metaPops fSpec) "AST" fSpec)

content :: (FSpec -> [Pop]) -> String -> FSpec -> String
content popKind cName fSpec = unlines
   ([ "{- Do not edit manually. This code has been generated!!!"
    , "    Generated with "++ampersandVersionStr
    , "    Generated at "++show (genTime (getOpts fSpec))
    , " "
    , "The populations defined in this file are the populations from the user's"
    , "model named '"++name fSpec++"'."
    , ""
    , "The order in which these populations are defined correspond with the order "
    , "in which Ampersand is defined in itself. Currently (Feb. 2015), this is hard-"
    , "coded. This means, that whenever Formal Ampersand changes, it might have "
    , "impact on the generator of this file. "
    , ""
    , "-}"
    , ""
    , "CONTEXT "++cName++" IN ENGLISH -- (the language is chosen arbitrary, for it is mandatory but irrelevant."]
    ++ (concat.intersperse  []) (map (lines.showADL) (popKind fSpec))
    ++
    [ ""
    , "ENDCONTEXT"
    ])
instance GenericPopulations FSpec where
 generics _ fSpec =
   filter (not.nullContent)
    (
    [ Pop "versionInfo" "Context"  "AmpersandVersion"
           [(uri fSpec, ampersandVersionStr)]
    , Pop "contextName" "Context" "ContextName"
           [(uri fSpec, name fSpec)]
    , Pop "dbName" "Context" "DatabaseName"
           [(uri fSpec, dbName (getOpts fSpec))]
    ]
  ++[ Comment " ", Comment $ "[Relations]--: (count="++(show.length.allDecls) fSpec++")"]
  ++   concatMap (generics fSpec) (allDecls fSpec)
  ++[ Comment " ", Comment $ "[Concepts]--: (count="++(show.length) [c | c <- allConcepts fSpec, c /= ONE]++")"]
  ++   concatMap (generics fSpec) [c | c <- allConcepts fSpec, c /= ONE]
  ++[ Comment " ", Comment $ "[TableColumnInfo]--: (count="++(show.length) allSqlPlugs++")"]
  ++   concatMap (generics fSpec) allSqlPlugs
  ++[ Comment " ", Comment $ "[Rules]--: (count="++(show.length.allRules) fSpec++")"]
  ++   concatMap (generics fSpec) (allRules fSpec)
  ++[ Comment " ", Comment $ "[Conjuncts]--: (count="++(show.length.vconjs) fSpec++")"]
  ++   concatMap (generics fSpec) (vconjs fSpec)
  ++[ Comment " ", Comment $ "[Roles]--: (count="++(show.length.fRoles) fSpec++")"]
  ++   concatMap (generics fSpec) (fRoles fSpec)
    )
  where
    allSqlPlugs = [plug | InternalPlug plug <- plugInfos fSpec]

instance MetaPopulations FSpec where
 metaPops _ fSpec =
   filter (not.nullContent)
    (
    [Comment  " ", Comment $ "PATTERN Context: ('"++name fSpec++"')"]
  ++[ Pop "ctxnm"   "Context" "Conid"
           [(uri fSpec,name fSpec)]]
  ++[ Pop "ctxpats" "Context" "Pattern"
           [(uri fSpec,uri pat) | pat <- vpatterns fSpec]]
  ++[ Pop "ctxcs" "Context" "PlainConcept"
           [(uri fSpec,uri cpt) | cpt <- allConcepts fSpec, cpt /= ONE]]
  ++[ Comment " ", Comment $ "PATTERN Patterns: (count="++(show.length.vpatterns) fSpec++")"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).vpatterns)    fSpec)
  ++[ Comment " ", Comment $ "PATTERN Specialization: (count="++(show.length.vgens) fSpec++")"]
  ++   concatMap (metaPops fSpec) (vgens          fSpec)
  ++[ Comment " ", Comment $ "PATTERN Concept: (count="++(show.length.allConcepts) fSpec++")"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).allConcepts)    fSpec)
  ++[ Comment " ", Comment $ "PATTERN Atoms: (count="++(show.length) allAtoms++")"]
  ++   concatMap (metaPops fSpec) allAtoms
  ++[ Comment " ", Comment $ "PATTERN Sign: (count="++(show.length) allSigns++")"]
  ++   concatMap (metaPops fSpec) allSigns
  ++[ Comment " ", Comment $ "PATTERN Declaration: (count="++(show.length.allDecls) fSpec++")"]
  ++   concatMap (metaPops fSpec) (allDecls  fSpec)
  ++[ Comment " ", Comment $ "PATTERN Expression: (count="++(show.length.allExprs) fSpec++")"]
  ++   concatMap (metaPops fSpec) (allExprs  fSpec)
  ++[ Comment " ", Comment $ "PATTERN Rules: (count="++(show.length.allRules) fSpec++")"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).allRules)    fSpec)
  ++[ Comment " ", Comment $ "PATTERN Plugs: (count="++(show.length.plugInfos) fSpec++")"]
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

instance MetaPopulations Pattern where
 metaPops _ pat =
   [ Comment " "
   , Comment $ " Pattern `"++name pat++"` "
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

instance GenericPopulations A_Concept where
 generics fSpec cpt =
   case cpt of
     PlainConcept{} ->
      [ Comment " "
      , Comment $ " Concept `"++name cpt++"` "
      , Pop "allConcepts" "Context" "Concept"
             [(uri fSpec,uri cpt)]
      , Pop "cptnm" "Concept" "ConceptName"
             [(uri cpt, name cpt)]
      , Pop "affectedInvConjunctIds" "Concept" "ConjunctID"
             [(uri cpt, uri conj) | conj <- filterFrontEndInvConjuncts affConjs]
      , Pop "affectedSigConjunctIds" "Concept" "ConjunctID"
             [(uri cpt, uri conj) | conj <- filterFrontEndSigConjuncts affConjs]
      , Pop "conceptTableFields" "Concept" "TableColumn"
             [(uri cpt, uri fld) | fld <- tablesAndFields]
      ]
     ONE -> [
            ]
  where
   affConjs = fromMaybe [] (lookup cpt $ allConjsPerConcept fSpec)
   tablesAndFields = nub . concatMap (lookupCpt fSpec) $ cpt : largerConcepts (gens fSpec) cpt
instance MetaPopulations A_Concept where
 metaPops fSpec cpt =
   case cpt of
     PlainConcept{} ->
      [ Comment " "
      , Comment $ " Concept `"++name cpt++"` "
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
instance GenericPopulations PlugSQL where
  generics fSpec plug =
      [ Comment " "
      , Comment $ "Plug: '"++name plug++"'"
      , Pop "tableInfo" "Context" "DBTable"
               [(uri fSpec, uri plug)]
      ] ++ concatMap (generics fSpec) [(plug,fld) | fld <- plugFields plug]

instance GenericPopulations (PlugSQL,SqlField) where
  generics _ (plug,fld) =
      [ Pop "columninfo" "DBTable" "TableColumn"
                 [(uri plug, uri (plug,fld)) ]
      , Pop "concept" "TableColumn" "Concept"
                 [(uri (plug,fld), uri.target.fldexpr $ fld)]
      , Pop "unique" "TableColumn" "BOOLEAN"
                 [(uri (plug,fld), uri.flduniq $ fld)]
      , Pop "null" "TableColumn" "BOOLEAN"
                 [(uri (plug,fld), uri.fldnull $ fld)]
      ]
     
instance GenericPopulations Role where
  generics fSpec rol =
      [ Comment $ "Role: '"++name rol++"'"
      , Pop "allRoles" "Context" "Role"
                 [(uri fSpec, uri rol) ]
      , Pop "name" "Role" "RoleName"
                 [(uri rol, name rol) ]
      , Pop "ruleNames" "Role" "Rule"
                 [(uri rol, uri rul) | (rol',rul) <-  fRoleRuls fSpec, rol==rol' ]
      ]

instance MetaPopulations AtomID where
 metaPops _ atm = 
   [ Pop "atmRoot" "AtomID" "PlainConcept" 
          [(uri atm, uri cpt) | cpt <- atmRoots atm]
   , Pop "instanceOf" "AtomID" "PlainConcept" 
          [(uri atm, uri cpt) | cpt <- atmIn atm]
   , Pop "atomvalue"  "AtomID" "AtomValue"
          [(uri atm,(show.atmVal) atm)]
   ]
instance MetaPopulations Sign where
 metaPops _ sgn =
--      [ Pop "sign" "Declaration" "Sign"
--             [(aap,uri sgn)]
--      , Pop "sign" "PairID" "Sign"
--             [(noot,uri sgn)]
      [ Pop "in" "PairID" "Declaration"
             [(uri sgn, uri (source sgn))]
      , Pop "src" "Sign" "Concept"
             [(uri sgn, uri (source sgn))]
      , Pop "trg" "Sign" "Concept"
             [(uri sgn, uri (target sgn))]
      ]

instance GenericPopulations Declaration where
 generics fSpec dcl =
   case dcl of 
     Sgn{} ->
      [ Comment " "
      , Comment $ " Relation '"++name dcl++showSign (sign dcl)++"'"
      , Pop "allRelations" "Context" "Relation"
             [(uri fSpec, uri dcl)]
      , Pop "name" "Relation" "RelationName"
             [(uri dcl,name dcl)]
      , Pop "srcConcept" "Relation" "Concept"
             [(uri dcl,uri (source dcl))]
      , Pop "tgtConcept" "Relation" "Concept"
             [(uri dcl,uri (target dcl))]
      , Pop "table" "Relation" "DBTable"
             [(uri dcl,uri table)]
      , Pop "srcCol" "Relation" "DBTableColumn"
             [(uri dcl,uri (table,srcCol))]
      , Pop "tgtCol" "Relation" "DBTableColumn"
             [(uri dcl,uri (table,tgtCol))]
      , Pop "affectedInvConjunctIds" "Relation" "ConjunctID"
             [(uri dcl,uri conj) | conj <- filterFrontEndInvConjuncts affConjs ]
      , Pop "affectedSigConjunctIds" "Relation" "ConjunctID"
             [(uri dcl,uri conj) | conj <- filterFrontEndSigConjuncts affConjs ]
      ]
     Isn{} -> fatal 157 "Isn is not implemented yet"
     Vs{}  -> fatal 158 "Vs is not implemented yet"
   where
     (table,srcCol,tgtCol) = getDeclarationTableInfo fSpec dcl
     affConjs = fromMaybe [] (lookup dcl $ allConjsPerDecl fSpec)
instance MetaPopulations Declaration where
 metaPops fSpec dcl =
   case dcl of
     Sgn{} ->
      [ Comment " "
      , Comment $ " Declaration `"++name dcl++" ["++(name.source.decsgn) dcl++" * "++(name.target.decsgn) dcl++"]"++"` "
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

instance GenericPopulations Rule where
 generics fSpec rul =
      [ Comment " "
      , Comment $ " Rule `"++name rul++"` "
      , Pop "allRules" "Context" "Rule"
             [(uri fSpec, uri rul)]
      , Pop "name"  "Rule" "RuleID"
             [(uri rul,name rul)]
      , Pop "ruleAdl"  "Rule" "Adl"
             [(uri rul,(showADL.rrexp) rul)]
      , Pop "origin"  "Rule" "Origin"
             [(uri rul,(show.origin) rul)]
      , Pop "meaning"  "Rule" "Meaning"
             [(uri rul,aMarkup2String m) | m <- (maybeToList . meaning (fsLang fSpec)) rul ]
      , Pop "message"  "Rule" "Message"
             [(uri rul,aMarkup2String m) | m <- rrmsg rul, amLang m == fsLang fSpec ]
      , Pop "srcConcept"  "Rule" "Concept"
             [(uri rul,(uri.source.rrexp) rul)]
      , Pop "tgtConcept"  "Rule" "Concept"
             [(uri rul,(uri.target.rrexp) rul)]
      , Pop "conjunctIds"  "Rule" "ConjunctID"
             [(uri rul,uri conj) | (rule,conjs)<-allConjsPerRule fSpec, rule==rul,conj <- conjs]
      ]++case rrviol rul of
        Nothing -> []
        Just pve ->
         [ Pop "pairView"  "Rule" "PairView"
              [(uri rul, uri pve)]
         ]++generics fSpec pve
      
      
instance GenericPopulations (PairView Expression) where
 generics fSpec pve = 
      [ Comment " "
      ]++concatMap makeSegment (zip [0..] (ppv_segs pve))
  where
    makeSegment :: (Int,PairViewSegment Expression) -> [Pop]
    makeSegment (i,pvs) =
      [ Pop "segment" "PairView" "PairViewSegment"
             [(uri pve,uri pvs)]
      , Pop "sequenceNr" "PairViewSegment" "Int"
             [(uri pvs,show i)]
      , Pop "segmentType" "PairViewSegment" "PairViewSegmentType"
             [(uri pvs, case pvs of 
                         PairViewText{} -> "Text"
                         PairViewExp{}  -> "Exp")
            ]
      ]++
      case pvs of
        PairViewText str -> 
          [Pop "text" "PairViewSegment" "String"
               [(uri pvs, show str)] 
          ]
        PairViewExp sot expr -> 
          [Pop "srcOrTgt" "PairViewSegment" "SourceOrTarget"
               [(uri pvs, show sot)] 
          ,Pop "expTgt" "PairViewSegment" "Concept"
               [(uri pvs, uri (case sot of
                                Src -> source expr
                                Tgt -> target expr
                              ))] 
          ,Pop "expSQL" "PairViewSegment" "MySQLQuery"
               [(uri pvs, show (prettySQLQuery fSpec 0 expr))] 
          ]
         
      
instance MetaPopulations Rule where
 metaPops _ rul =
      [ Comment " "
      , Comment $ " Rule `"++name rul++"` "
      , Pop "rrnm"  "Rule" "ADLid"
             [(uri rul,rrnm rul)]
      , Pop "rrexp"  "Rule" "ExpressionID"
             [(uri rul,uri (rrexp rul))]
      , Pop "rrmean"  "Rule" "Blob"
             [(uri rul,show(rrmean rul))]
      , Pop "rrpurpose"  "Rule" "Blob"
             [(uri rul,showADL x) | x <- explanations rul]
      , -- The next population is from the adl pattern 'Plugs':
        Pop "sign" "Rule" "Sign"
             [(uri rul,uri (rrtyp rul))]
      ]



instance MetaPopulations PlugInfo where
 metaPops _ plug = 
      [ Comment $ " Plug `"++name plug++"` "
      , Pop "maintains" "Plug" "Rule" [{-STILL TODO. -}] --HJO, 20150205: Waar halen we deze info vandaan??
      , Pop "in" "PlainConcept" "Plug"                 
             [(uri cpt,uri plug)| cpt <- concs plug, isRelevant plug] -- TODO @Stef: Dit levert mogelijk meerdere plugs op, omdat we hier naar plugs kijken, en niet filteren op alleen brede tabellen. Waardoor weten we dat we niet naar BinSQL moeten kijken, maar alleen naar TblSQL? En hoe zit dat voor andere typen plugs (PHP plugs?)  
      , Pop "in" "Declaration" "Plug"
             [(uri dcl,uri plug)| dcl <- relsMentionedIn plug, isRelevant plug]  --Idem
      ]      
-- TIJDELIJKE FUNCTIE: Moet worden overlegd met Stef:
isRelevant :: PlugInfo -> Bool
isRelevant (InternalPlug TblSQL{}) = True
isRelevant _ = False
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

instance GenericPopulations Conjunct where
 generics fSpec conj =
  [ Comment $ "Conjunct: '"++rc_id conj++"'."
  , Pop "allConjuncts" "Context" "Conjunct" 
         [(uri fSpec, uri conj)]
  , Pop "signalRuleNames" "Conjunct" "Rule" 
         [(uri conj,uri r) | r <- rc_orgRules conj, isFrontEndSignal r]
  , Pop "invariantRuleNames" "Conjunct" "Rule" 
         [(uri conj,uri r) | r <- rc_orgRules conj, isFrontEndInvariant  r]
--TODO: ViolationsSQL
--  , Pop "violationsSQL" "Conjunct" "MySQLQuery" 
--         [(uri conj
--             ,selectExpr fSpec 0 "src" "tgt" (conjNF (getOpts fSpec) (notCpl (rc_conjunct conj)))
--          )]
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
      Comment{} -> intercalate "\n" (map prepend (lines (comment pop)))
    where indentA = "   "
          showContent = map showPaire (popPairs pop)
          showPaire (s,t) = "( "++show s++" , "++show t++" )"
          prepend str = "-- " ++ str

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
instance AdlId PlugInfo
instance AdlId PlugSQL
instance AdlId (PlugSQL,SqlField)
instance AdlId Purpose
instance AdlId Rule
instance AdlId Role
instance AdlId Sign
instance AdlId Conjunct
instance AdlId (PairView Expression)
instance AdlId (PairViewSegment Expression)

instance AdlId Bool where
 uri = showUnique
instance AdlId a => AdlId [a] where

mkAtom :: FSpec  -> A_Concept -> String -> AtomID
mkAtom fSpec cpt value = 
   AtomID { atmRoots = rootConcepts gs [cpt]
          , atmIn   = largerConcepts gs cpt `uni` [cpt]
          , atmVal  = value
          }
  where
    gs = vgens fSpec
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

nullContent :: Pop -> Bool
nullContent (Pop _ _ _ []) = True
nullContent _ = False
    
class MetaPopulations a where
 metaPops :: FSpec -> a -> [Pop]
class GenericPopulations a where
 generics :: FSpec -> a -> [Pop]   


--------- Below here are some functions copied from Generate.hs TODO: Clean up.
-- Because the signal/invariant condition appears both in generateConjuncts and generateInterface, we use
-- two abstractions to guarantee the same implementation.
isFrontEndInvariant :: Rule -> Bool
isFrontEndInvariant r = not (isSignal r) && not (ruleIsInvariantUniOrInj r)

isFrontEndSignal :: Rule -> Bool
isFrontEndSignal r = isSignal r

-- NOTE that results from filterFrontEndInvConjuncts and filterFrontEndSigConjuncts may overlap (conjunct appearing in both invariants and signals)
-- and that because of extra condition in isFrontEndInvariant (not (ruleIsInvariantUniOrInj r)), some parameter conjuncts may not be returned
-- as either inv or sig conjuncts (i.e. conjuncts that appear only in uni or inj rules) 
filterFrontEndInvConjuncts :: [Conjunct] -> [Conjunct]
filterFrontEndInvConjuncts conjs = filter (\c -> any isFrontEndInvariant $ rc_orgRules c) conjs

filterFrontEndSigConjuncts :: [Conjunct] -> [Conjunct]
filterFrontEndSigConjuncts conjs = filter (\c -> any isFrontEndSignal $ rc_orgRules c) conjs
