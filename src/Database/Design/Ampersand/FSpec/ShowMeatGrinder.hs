{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Database.Design.Ampersand.FSpec.ShowMeatGrinder
  (makeMetaPopulationFile,MetaType(..))
where

import Data.List
import Data.Char
import Data.Ord
import Data.Hashable (hash) -- a not good enouqh function, but used for the time being. 
import Data.Typeable
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.FSpec.FSpecAux
import Database.Design.Ampersand.FSpec.Motivations
import Database.Design.Ampersand.FSpec.SQL
import Database.Design.Ampersand.FSpec.ToFSpec.NormalForms (conjNF)
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.FSpec.ShowADL
import Database.Design.Ampersand.Core.AbstractSyntaxTree hiding (RuleType(..))
import Database.Design.Ampersand.Classes.ConceptStructure

--import Data.Hashable
import Data.Maybe

fatal :: Int -> String -> a
fatal = fatalMsg "ShowMeatGrinder"


makeMetaPopulationFile :: MetaType -> FSpec -> (FilePath,String)
makeMetaPopulationFile mType fSpec
  = ("MetaPopulationFile"++show mType++".adl", content popKind mType fSpec)
    where popKind = case mType of
                      Generics -> generics fSpec
                      AST      -> metaPops fSpec 

content :: (FSpec -> [Pop]) -> MetaType -> FSpec -> String
content popKind mType fSpec = unlines
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
    , "CONTEXT "++show mType++" IN ENGLISH -- (the language is chosen arbitrary, for it is mandatory but irrelevant."]
    ++ (concat.intersperse  []) (map (lines . showADL ) (popKind fSpec))
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
  ++[ Comment " ", Comment $ "[Rules]--: (count="++(show.length.fallRules) fSpec++")"]
  ++   concatMap (generics fSpec) (fallRules fSpec)
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
  ++[ Pop "name"   "Context" "ContextIdentifier"
           [(uri fSpec,name fSpec)]]
  ++[ Comment " ", Comment $ "PATTERN Patterns: (count="++(show.length.vpatterns) fSpec++")"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).vpatterns)    fSpec)
  ++[ Comment " ", Comment $ "PATTERN Specialization: (count="++(show.length.vgens) fSpec++")"]
  ++   concatMap (metaPops fSpec) (vgens          fSpec)
  ++[ Comment " ", Comment $ "PATTERN Concept: (count="++(show.length.allConcepts) fSpec++")"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).allConcepts)    fSpec)
--  ++[ Comment " ", Comment $ "PATTERN Atoms: (count="++(show.length) (allAtoms fSpec)++")"]
--  ++   concatMap (metaPops fSpec) (allAtoms fSpec)
  ++[ Comment " ", Comment $ "PATTERN Signature: (count="++(show.length.allSigns) fSpec++")"]
  ++   concatMap (metaPops fSpec) (allSigns fSpec)
  ++[ Comment " ", Comment $ "PATTERN Declaration: (count="++(show.length.allDecls) fSpec++")"]
  ++   concatMap (metaPops fSpec) (allDecls  fSpec)
  ++[ Comment " ", Comment $ "PATTERN Expression: (count="++(show.length.allExprs) fSpec++")"]
  ++   concatMap (metaPops fSpec) (allExprs  fSpec)
  ++[ Comment " ", Comment $ "PATTERN Rules: (count="++(show.length.fallRules) fSpec++")"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).fallRules)    fSpec)
  ++[ Comment " ", Comment $ "PATTERN Plugs: (count="++(show.length.plugInfos) fSpec++")"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).plugInfos)    fSpec)
--  ++[ Comment " ", Comment $ "[Initial pairs]--: (count="++(show.length.allLinks) fSpec++")"]
--  ++   concatMap (metaPops fSpec) (allLinks fSpec)
  )
   where

instance MetaPopulations Pattern where
 metaPops fSpec pat =
   [ Comment " "
   , Comment $ " Pattern `"++name pat++"` "
   , Pop "patterns" "Context" "Pattern"
          [(uri fSpec,uri pat)]
   , Pop "name"    "Pattern" "PatternIdentifier"
          [(uri pat, name pat)]
   , Pop "rules"   "Pattern" "Rule"
          [(uri pat,uri x) | x <- ptrls pat]
   , Pop "declarations"   "Pattern" "Declaration"
          [(uri pat,uri x) | x <- ptdcs pat]
   , Pop "purpose"   "Pattern" "Purpose"
          [(uri pat,uri x) | x <- ptxps pat]
   ]
instance MetaPopulations A_Gen where
 metaPops fSpec gen =
  [ Pop "gens" "Context" "Gen"
          [(uri fSpec,uri gen)]
  , Pop "genspc"  "Gen" "Concept"
          [(uri gen,uri(genspc gen))]
  , Pop "gengen"  "Gen" "Concept"
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
      , Pop "name" "Concept" "Identifier"
             [(uri cpt, name cpt)]
      , Pop "affectedInvConjunctIds" "Concept" "ConjunctID"
             [(uri cpt, uri conj) | conj <- filterFrontEndInvConjuncts affConjs]
      , Pop "affectedSigConjunctIds" "Concept" "ConjunctID"
             [(uri cpt, uri conj) | conj <- filterFrontEndSigConjuncts affConjs]
      , Pop "conceptTableFields" "Concept" "TableColumn"
             [(uri cpt, uri fld) | fld <- tablesAndFields]
      ]
     ONE -> 
      [ Comment " "
      , Comment $ " Concept ONE "
      , Pop "allConcepts" "Context" "Concept"
             [(uri fSpec,uri cpt)]
      , Pop "name" "Concept" "Identifier"
             [(uri cpt, name cpt)]
      , Pop "conceptTableFields" "Concept" "TableColumn"
             [(uri cpt, uri fld) | fld <- tablesAndFields]
      ]
  where
    affConjs = nub [ conj  
                   | Just conjs<-[lookup cpt (allConjsPerConcept fSpec)]
                   , conj<-conjs
                   ]
    largerConcs = largerConcepts (vgens fSpec) cpt++[cpt]
    tablesAndFields = nub . concatMap (lookupCpt fSpec) $ largerConcs

instance MetaPopulations A_Concept where
 metaPops fSpec cpt =
   case cpt of
     PlainConcept{} ->
      [ Comment " "
      , Comment $ " Concept `"++name cpt++"` "
      , Pop "concs" "Context" "Concept"
           [(uri fSpec,uri cpt)]
      , Pop "name" "Concept" "Identifier"
             [(uri cpt, name cpt)]
--      , Pop "cptdf" "Concept" "ConceptDefinition"
--             [(uri cpt,showADL cdef) | cdef <- conceptDefs  fSpec, name cdef == name cpt]
--      , Pop "cptpurpose" "Concept" "Purpose"
--             [(uri cpt,showADL x) | lang <- allLangs, x <- fromMaybe [] (purposeOf fSpec lang cpt) ]
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
      , Pop "name" "Role" "TEXT"
                 [(uri rol, name rol) ]
      , Pop "maintains" "Role" "Rule"
                 [(uri rol, uri rul) | (rol',rul) <-  fRoleRuls fSpec, rol==rol' ]
      ]

instance MetaPopulations Atom where
 metaPops _ atm =
   [ Pop "pop" "Atom" "Concept" 
          [(uri atm, uri cpt)
          |cpt <- atmRoots atm]
   , Pop "repr"  "Atom" "Representation"
          [(uri atm, (showValADL.atmVal) atm)]
   ]
instance MetaPopulations Signature where
 metaPops _ sgn =
      [ Pop "src" "Signature" "Concept"
             [(uri sgn, uri (source sgn))]
      , Pop "tgt" "Signature" "Concept"
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
      , Pop "allDeclarations" "Context" "Relation"
             [(uri fSpec,uri dcl)] 
      , Pop "name" "Relation" "Identifier"
             [(uri dcl, name dcl)]
--      , Pop "sign" "Declaration" "Signature"
--             [(uri dcl,uri (sign dcl))]
      , Pop "source" "Relation" "Concept"
             [(uri dcl,uri (source dcl))]
      , Pop "target" "Relation" "Concept"
             [(uri dcl,uri (target dcl))]
--      , Pop "decprL" "Declaration" "String"
--             [(uri dcl,decprL dcl)]
--      , Pop "decprM" "Declaration" "String"
--             [(uri dcl,decprM dcl)]
--      , Pop "decprR" "Declaration" "String"
--             [(uri dcl,decprR dcl)]
--      , Pop "decmean" "Declaration" "Meaning"
--             [(uri dcl, show(decMean dcl))]
--      , Pop "decpurpose" "Declaration" "Purpose"
--             [(uri dcl, showADL x) | x <- explanations dcl]
      ]
     Isn{} -> 
      [ Comment " "
      , Comment $ " Declaration `I["++name (source dcl)++"]`"
--      , Pop "sign" "Declaration" "Signature"
--             [(uri dcl,uri (sign dcl))]
      , Pop "source" "Relation" "Concept"
             [(uri dcl,uri (source dcl))]
      , Pop "target" "Relation" "Concept"
             [(uri dcl,uri (target dcl))]
      ]
     Vs{}  -> fatal 158 "Vs is not implemented yet"

instance MetaPopulations A_Pair where
 metaPops _ pair =
      [ Pop "in" "Pair" "Relation"
             [(uri pair, uri (lnkDcl pair))]
      , Pop "l" "Pair" "Atom"
             [(uri pair, uri(lnkLeft pair))]
      , Pop "r" "Pair" "Atom"
             [(uri pair, uri(lnkRight pair))]
      ]

instance MetaPopulations Expression where
 metaPops fSpec expr =
  case expr of 
    EBrk e -> metaPops fSpec e
    _      ->
      [ Pop "src" "Term" "Concept"
             [(uri expr, uri (source expr))]
      , Pop "tgt" "Term" "Concept"
             [(uri expr, uri (target expr))]
      ]++
      ( case expr of
            (EEqu (l,r)) -> makeBinaryTerm Equivalence l r
            (EInc (l,r)) -> makeBinaryTerm Inclusion l r
            (EIsc (l,r)) -> makeBinaryTerm Intersection l r
            (EUni (l,r)) -> makeBinaryTerm Union l r
            (EDif (l,r)) -> makeBinaryTerm Difference l r
            (ELrs (l,r)) -> makeBinaryTerm LeftResidu l r   
            (ERrs (l,r)) -> makeBinaryTerm RightResidu l r
            (EDia (l,r)) -> makeBinaryTerm Diamond l r
            (ECps (l,r)) -> makeBinaryTerm Composition l r
            (ERad (l,r)) -> makeBinaryTerm RelativeAddition l r
            (EPrd (l,r)) -> makeBinaryTerm CartesionProduct l r
--            (EKl0 e)     -> 
--            (EKl1 e)     -> 
--            (EFlp e)     -> 
--            (ECpl e)     -> 
            (EBrk _)     -> fatal 348 "This should not happen, because EBrk has been handled before"
            (EDcD dcl)   -> [Pop "bind" "Term" "Relation" [(uri expr,uri dcl)]
                            ]
--            EDcI{}       -> 
--            EEps{}       -> 
--            EDcV{}       -> 
--            EMp1{}       -> 
            _            -> [Comment $ "TODO: "++showADL expr]
-- TODO: Work on the rest of the expressions (get rid of the statement above) 
       ) 
  where
    makeBinaryTerm :: BinOp -> Expression -> Expression -> [Pop]
    makeBinaryTerm bop lhs rhs = 
      [ Pop "lhs"  "BinaryTerm" "Term"
             [(uri expr,uri lhs)]
      , Pop "rhs"  "BinaryTerm" "Term"
             [(uri expr,uri rhs)]
      , Pop "operator"  "BinaryTerm" "Operator"
             [(uri expr,uri bop)]
      ]++metaPops fSpec lhs
       ++metaPops fSpec rhs
       
data BinOp = CartesionProduct
           | Composition
           | Diamond
           | Difference
           | Equivalence 
           | Inclusion 
           | Intersection 
           | LeftResidu
           | RightResidu
           | RelativeAddition 
           | Union deriving (Eq, Show, Typeable)
instance Unique BinOp where
  showUnique = show


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
             [(uri rul,aMarkup2String ReST m) | m <- (maybeToList . meaning (fsLang fSpec)) rul ]
      , Pop "message"  "Rule" "Message"
             [(uri rul,aMarkup2String ReST m) | m <- rrmsg rul, amLang m == fsLang fSpec ]
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
        PairViewText{} -> 
          [Pop "text" "PairViewSegment" "String"
               [(uri pvs, pvsStr pvs)] 
          ]
        PairViewExp{} -> 
          [Pop "srcOrTgt" "PairViewSegment" "SourceOrTarget"
               [(uri pvs, show (pvsSoT pvs))] 
          ,Pop "expTgt" "PairViewSegment" "Concept"
               [(uri pvs, uri (case pvsSoT pvs of
                                Src -> source (pvsExp pvs)
                                Tgt -> target (pvsExp pvs)
                              ))] 
          ,Pop "expSQL" "PairViewSegment" "MySQLQuery"
               [(uri pvs, prettySQLQuery fSpec 0 (pvsExp pvs))] 
          ]
         
      
instance MetaPopulations Rule where
 metaPops _ rul =
      [ Comment " "
      , Comment $ " Rule `"++name rul++"` "
      , Pop "name"  "Rule" "RuleName"
             [(uri rul,name rul)]
      , Pop "term"  "Rule" "Term"
             [(uri rul,uri (rrexp rul))]
      , Pop "rrexp"  "Rule" "ExpressionID"
             [(uri rul,uri (rrexp rul))]
      , Pop "rrmean"  "Rule" "Meaning"
             [(uri rul,show(rrmean rul))]
      , Pop "rrpurpose"  "Rule" "Purpose"
             [(uri rul,showADL x) | x <- explanations rul]
      , -- The next population is from the adl pattern 'Plugs':
        Pop "sign" "Rule" "Signature"
             [(uri rul,uri (sign rul))]
      , Pop "declaredthrough" "PropertyRule" "Property"
             [(uri rul,show prp) | Just(prp,_) <- [rrdcl rul]]
      , Pop "decprps" "Declaration" "PropertyRule"
             [(uri dcl, uri rul) | Just(_,dcl) <- [rrdcl rul]]
      ]



instance MetaPopulations PlugInfo where
 metaPops _ plug = 
      [ Comment $ " Plug `"++name plug++"` "
      , Pop "maintains" "Plug" "Rule" [{-STILL TODO. -}] --HJO, 20150205: Waar halen we deze info vandaan??
      , Pop "in" "Concept" "Plug"                 
             [(uri cpt,uri plug)| cpt <- concs plug]  
      , Pop "in" "Declaration" "Plug"
             [(uri dcl,uri plug)| dcl <- relsMentionedIn plug]
      ]      

instance MetaPopulations a => MetaPopulations [a] where
 metaPops fSpec = concatMap $ metaPops fSpec
 
instance GenericPopulations Conjunct where
 generics fSpec conj =
  [ Comment $ "Conjunct: '"++rc_id conj++"'."
  , Pop "allConjuncts" "Context" "Conjunct" 
         [(uri fSpec, uri conj)]
  , Pop "signalRuleNames" "Conjunct" "Rule" 
         [(uri conj,uri r) | r <- rc_orgRules conj, isFrontEndSignal r]
  , Pop "invariantRuleNames" "Conjunct" "Rule" 
         [(uri conj,uri r) | r <- rc_orgRules conj, isFrontEndInvariant  r]
  , Pop "violationsSQL" "Conjunct" "MySQLQuery" 
         [(uri conj , prettySQLQuery fSpec 0 (conjNF (getOpts fSpec) (notCpl (rc_conjunct conj)))
          )]
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
 uri = camelCase . uniqueShow False
-- All 'things' that are relevant in the meta-environment (RAP),
-- must be an instance of AdlId:
instance AdlId A_Concept
instance AdlId A_Gen
instance AdlId Atom
instance AdlId ConceptDef
instance AdlId Declaration
instance AdlId Expression
instance AdlId BinOp
instance AdlId FSpec
instance AdlId A_Pair
instance AdlId Pattern
instance AdlId PlugInfo
instance AdlId PlugSQL
instance AdlId (PlugSQL,SqlField)
instance AdlId Purpose
instance AdlId Rule
instance AdlId Role
instance AdlId Signature
instance AdlId Conjunct
instance AdlId (PairView Expression)
  where uri x = show (typeOf x)++show (hash x)
instance AdlId (PairViewSegment Expression)
  where uri x = show (typeOf x)++show (hash (show (hash x) ++ show (origin x)))
instance AdlId Bool where
 uri = showUnique
instance AdlId a => AdlId [a] where
--instance AdlId (Declaration,Paire)



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
