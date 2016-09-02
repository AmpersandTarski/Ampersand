{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Ampersand.FSpec.ShowMeatGrinder
  (makeMetaPopulationFile)
where

import Data.List
import Data.Char
import Data.Ord
import Data.Hashable (hash) -- a not good enouqh function, but used for the time being. 
import Data.Maybe
import Data.Typeable
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.FSpecAux
import Ampersand.FSpec.Motivations
import Ampersand.Basics
import Ampersand.Misc
import Ampersand.FSpec.ShowADL
import Ampersand.Core.AbstractSyntaxTree


makeMetaPopulationFile :: FSpec -> (FilePath,String)
makeMetaPopulationFile fSpec
  = ("MetaPopulationFile.adl", content fSpec)

{-SJ 2015-11-06 Strange that the function 'content' generates text.
I would have expected a P-structure (of even an A-structure) instead.
Is there a reason? 
Answer: HJO: By directly generate a string, the resulting file can contain comment, which is 
             usefull for debugging. However, the idea is good. In future, we might change
             it to create a P_Context in stead of a String.
-} 
content :: FSpec -> String
content fSpec = unlines
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
    , "CONTEXT FormalAmpersand IN ENGLISH -- (the language is chosen arbitrary, for it is mandatory but irrelevant."]
    ++ intercalate [] (map (lines . showADL ) (metaPops fSpec fSpec))
    ++
    [ ""
    , "ENDCONTEXT"
    ])
instance MetaPopulations FSpec where
 metaPops _ fSpec =
   filter (not.nullContent)
    (
    [Comment  " ", Comment $ "PATTERN Context: ('"++name fSpec++"')"]
  ++[ Pop "versionInfo" "Context"  "AmpersandVersion"
           [(dirtyId fSpec, show ampersandVersionStr)]
    , Pop "name" "Context" "ContextIdentifier"
           [(dirtyId fSpec, (show.name) fSpec)]
    , Pop "dbName" "Context" "DatabaseName"
           [(dirtyId fSpec, (show.dbName.getOpts) fSpec)]
    , Pop "allRoles" "Context" "Role"
           [(dirtyId fSpec, show "SystemAdmin")]
    , Pop "name"   "Role" "RoleName"
           [(show "SystemAdmin", show "SystemAdmin")]
    ]
  ++[ Comment " ", Comment $ "PATTERN Patterns: (count="++(show.length.vpatterns) fSpec++")"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).vpatterns)    fSpec)
  ++[ Comment " ", Comment $ "PATTERN Specialization: (count="++(show.length.vgens) fSpec++")"]
  ++   concatMap (metaPops fSpec) (vgens          fSpec)
  ++[ Comment " ", Comment $ "PATTERN Concept: (count="++(show.length.concs) fSpec++")"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).concs)    fSpec)
  ++[ Comment " ", Comment $ "PATTERN Signature: (count="++(show.length.allSigns) fSpec++")"]
  ++   concatMap (metaPops fSpec) (allSigns fSpec)
  ++[ Comment " ", Comment $ "PATTERN Relation: (count="++(show.length.vrels) fSpec++")"]
  ++   concatMap (metaPops fSpec) (vrels fSpec ++ [ Isn c | c<-concs fSpec])
  ++[ Comment " ", Comment $ "PATTERN Expression: (count="++(show.length.allExprs) fSpec++")"]
  ++   concatMap (metaPops fSpec) (allExprs  fSpec)
  ++[ Comment " ", Comment $ "PATTERN Rules: (count="++(show.length.fallRules) fSpec++")"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).fallRules)    fSpec)
  ++[ Comment " ", Comment $ "PATTERN Conjuncts: (count="++(show.length.allConjuncts) fSpec++")"]
  ++   concatMap (metaPops fSpec) (allConjuncts fSpec)
  ++[ Comment " ", Comment $ "PATTERN Plugs: (count="++(show.length.plugInfos) fSpec++")"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).plugInfos)    fSpec)
  )

instance MetaPopulations Pattern where
 metaPops fSpec pat =
   [ Comment " "
   , Comment $ " Pattern `"++name pat++"` "
   , Pop "patterns" "Context" "Pattern"
          [(dirtyId fSpec,dirtyId pat)]
   , Pop "name"    "Pattern" "PatternIdentifier"
          [(dirtyId pat, (show.name) pat)]
   , Pop "rules"   "Pattern" "Rule"
          [(dirtyId pat,dirtyId x) | x <- ptrls pat]
   , Pop "declarations"   "Pattern" "Relation"
          [(dirtyId pat,dirtyId x) | x <- ptdcs pat]
   , Pop "purpose"   "Pattern" "Purpose"
          [(dirtyId pat,dirtyId x) | x <- ptxps pat]
   ]
instance MetaPopulations A_Gen where
 metaPops fSpec gen =
  [ Pop "gens" "Context" "Gen"
          [(dirtyId fSpec,dirtyId gen)]
  , Pop "genspc"  "Gen" "Concept"
          [(dirtyId gen,dirtyId(genspc gen))]
  , Pop "gengen"  "Gen" "Concept"
          [(dirtyId gen,dirtyId c) | c<- case gen of
                                   Isa{} -> [gengen gen]
                                   IsE{} -> genrhs gen
          ]
  ]

instance MetaPopulations A_Concept where
 metaPops fSpec cpt =
   [ Comment " "
   , Comment $ " Concept `"++name cpt++"` "
   , Pop "ttype" "Concept" "TType"
             [(dirtyId cpt, dirtyId (cptTType fSpec cpt))] 
   , Pop "name" "Concept" "Identifier"
             [(dirtyId cpt, dirtyId cpt)]
   ]++
   case cpt of
     PlainConcept{} ->
      [ Comment $ " Concept `"++name cpt++"` "
      , Pop "concs" "Context" "Concept"
             [(dirtyId fSpec,dirtyId cpt)]
--      , Pop "conceptColumn" "Concept" "SqlAttribute"
--             [(dirtyId cpt, dirtyId att) | att <- tablesAndAttributes]
--      , Pop "cptdf" "Concept" "ConceptDefinition"
--             [(dirtyId cpt,(show.showADL) cdef) | cdef <- conceptDefs  fSpec, name cdef == name cpt]
--      , Pop "cptpurpose" "Concept" "Purpose"
--             [(dirtyId cpt,(show.showADL) x) | lang <- allLangs, x <- fromMaybe [] (purposeOf fSpec lang cpt) ]
      ]
     ONE -> 
      [ ]
  where
    largerConcs = largerConcepts (vgens fSpec) cpt++[cpt]
    tablesAndAttributes = nub . concatMap (lookupCpt fSpec) $ largerConcs

instance MetaPopulations Conjunct where
  metaPops fSpec conj =
    [ Comment $ " Conjunct `"++rc_id conj++"` "
    , Pop "allConjuncts" "Context" "Conjunct"
             [(dirtyId fSpec,dirtyId conj)]
    , Pop "originatesFrom" "Conjunct" "Rule"
             [(dirtyId conj, dirtyId rul) | rul <- rc_orgRules conj]
    , Pop "conjunct" "Conjunct" "Expression"
             [(dirtyId conj, dirtyId (rc_conjunct conj))]
    ] 

instance MetaPopulations PlugInfo where
 metaPops fSpec plug = 
      [ Comment $ " Plug `"++name plug++"` "
      , Pop "maintains" "Plug" "Rule" [{-STILL TODO. -}] --HJO, 20150205: Waar halen we deze info vandaan??
      , Pop "in" "Concept" "Plug"                 
             [(dirtyId cpt,dirtyId plug)| cpt <- concs plug]  
      , Pop "relsInPlug" "Plug" "Relation"
             [(dirtyId plug,dirtyId dcl)| dcl <- relsMentionedIn plug]
      ]++
      (case plug of
         InternalPlug plugSQL   -> metaPops fSpec plugSQL
         ExternalPlug _ -> fatal 167 "ExternalPlug is not implemented in the meatgrinder. "
      )      

instance MetaPopulations PlugSQL where
  metaPops fSpec plug =
    case plug of 
       TblSQL{} ->
         [ Pop "rootConcept" "TblSQL" "Concept"
               [(dirtyId plug, dirtyId . target . attExpr . head . plugAttributes $ plug)]
         , Pop "key" "TblSQL" "SqlAttribute"
               [(dirtyId plug, dirtyId(plug,head . plugAttributes $ plug))]
         ] ++ 
         concatMap (metaPops fSpec) [(plug,att) | att <- plugAttributes plug]
       BinSQL{} -> []  

instance MetaPopulations (PlugSQL,SqlAttribute) where
  metaPops _ (plug,att) =
      [ Pop "table" "SqlAttribute" "SQLPlug"
                 [(dirtyId (plug,att), dirtyId plug) ]
      , Pop "concept" "SqlAttribute" "Concept"
                 [(dirtyId (plug,att), dirtyId.target.attExpr $ att)]
      , Pop "relsInPlug" "Plug" "Relation"
                 [(dirtyId plug, dirtyId rel) | Just rel <- [primRel.attExpr $ att]]
--      , Pop "null" "SqlAttribute" "SqlAttribute"
--                 [(a,a) | attNull att, let a=dirtyId (plug,att)]
      ]
    where primRel :: Expression -> Maybe Declaration
          primRel expr =
            case expr of
              EDcD dcl -> Just dcl
              EFlp (EDcD dcl) -> Just dcl
              EDcI cpt -> Just (Isn cpt)
              _  -> Nothing
instance MetaPopulations Role where
  metaPops fSpec rol =
      [ Pop "allRoles" "Context" "Role"
                 [(dirtyId fSpec, dirtyId rol) ]
      , Pop "name" "Role" "RoleName"
                 [(dirtyId rol, dirtyId rol) ]
      , Pop "maintains" "Role" "Rule"
                 [(dirtyId rol, dirtyId rul) | (rol',rul) <-  fRoleRuls fSpec, rol==rol' ]
      ]

instance MetaPopulations Atom where
 metaPops _ atm =
   [ Pop "pop" "Atom" "Concept" 
          [(dirtyId atm, dirtyId cpt)
          |cpt <- atmRoots atm]
   , Pop "repr"  "Atom" "Representation"
          [(dirtyId atm, (showValADL.atmVal) atm)]
   ]
instance MetaPopulations Signature where
 metaPops _ sgn =
      [ Pop "src" "Signature" "Concept"
             [(dirtyId sgn, dirtyId (source sgn))]
      , Pop "tgt" "Signature" "Concept"
             [(dirtyId sgn, dirtyId (target sgn))]
      ]

instance MetaPopulations Declaration where
 metaPops fSpec dcl =
   (case dcl of
     Sgn{} ->
      [ Comment " "
      , Comment $ " Relation `"++name dcl++" ["++(name.source.decsgn) dcl++" * "++(name.target.decsgn) dcl++"]"++"` "
      , Pop "context" "Relation" "Context"
             [(dirtyId dcl,dirtyId fSpec)] 
      , Pop "name" "Relation" "Identifier"
             [(dirtyId dcl, (show.name) dcl)]
--      , Pop "srcCol" "Relation" "SqlAttribute"
--             [(dirtyId dcl,dirtyId (table,srcCol))]
--      , Pop "tgtCol" "Relation" "SqlAttribute"
--             [(dirtyId dcl,dirtyId (table,tgtCol))]
      , Pop "sign" "Relation" "Signature"
             [(dirtyId dcl,dirtyId (sign dcl))]
      , Pop "source" "Relation" "Concept"
             [(dirtyId dcl,dirtyId (source dcl))]
      , Pop "target" "Relation" "Concept"
             [(dirtyId dcl,dirtyId (target dcl))]
      , Pop "prop" "Relation" "Property"
             [(dirtyId dcl, dirtyId x) | x <- decprps dcl]  -- decprps gives the user defined properties; not the derived properties.
      , Pop "decprL" "Relation" "String"
             [(dirtyId dcl,(show.decprL) dcl)]
      , Pop "decprM" "Relation" "String"
             [(dirtyId dcl,(show.decprM) dcl)]
      , Pop "decprR" "Relation" "String"
             [(dirtyId dcl,(show.decprR) dcl)]
      , Pop "decmean" "Relation" "Meaning"
             [(dirtyId dcl, (show.concatMap showADL.ameaMrk.decMean) dcl)]
      , Pop "decpurpose" "Relation" "Purpose"
             [(dirtyId dcl, (show.showADL) x) | x <- explanations dcl]
      ]
     Isn ONE -> 
      [ Comment " "
      , Comment " Relation `I[ONE]` "
      , Pop "context" "Relation" "Context"
             [(dirtyId dcl,dirtyId fSpec)]
      , Pop "name" "Relation" "Identifier"
             [(dirtyId dcl, (show.name) dcl)]
      , Pop "source" "Relation" "Concept"
             [(dirtyId dcl,dirtyId (source dcl))]
      , Pop "target" "Relation" "Concept"
             [(dirtyId dcl,dirtyId (target dcl))]
      ]
     Isn{} -> 
      [ Comment " "
      , Comment $ " Relation `I["++name (source dcl)++"]`"
      , Pop "sign" "Relation" "Signature"
             [(dirtyId dcl,dirtyId (sign dcl))]
      , Pop "context" "Relation" "Context"
             [(dirtyId dcl,dirtyId fSpec)]
      , Pop "name" "Relation" "Identifier"
             [(dirtyId dcl, (show.name) dcl)]
--      , Pop "srcCol" "Relation" "SqlAttribute"
--             [(dirtyId dcl,dirtyId (table,srcCol))]
--      , Pop "tgtCol" "Relation" "SqlAttribute"
--             [(dirtyId dcl,dirtyId (table,tgtCol))]
      , Pop "source" "Relation" "Concept"
             [(dirtyId dcl,dirtyId (source dcl))]
      , Pop "target" "Relation" "Concept"
             [(dirtyId dcl,dirtyId (target dcl))]
      ]
     Vs{}  -> fatal 158 "Vs is not implemented yet"
   )++
   metaPops fSpec (sign dcl)
   where
     (table,srcCol,tgtCol) = getDeclarationTableInfo fSpec dcl  -- type: (PlugSQL,SqlAttribute,SqlAttribute)

instance MetaPopulations A_Pair where
 metaPops _ pair =
      [ Pop "in" "Pair" "Relation"
             [(dirtyId pair, dirtyId (lnkDcl pair))]
      , Pop "l" "Pair" "Atom"
             [(dirtyId pair, dirtyId(lnkLeft pair))]
      , Pop "r" "Pair" "Atom"
             [(dirtyId pair, dirtyId(lnkRight pair))]
      ]

instance MetaPopulations Expression where
 metaPops fSpec expr =
  case expr of 
    EBrk e -> metaPops fSpec e
    _      ->
      [ Pop "src" "Expression" "Concept"
             [(dirtyId expr, dirtyId (source expr))]
      , Pop "tgt" "Expression" "Concept"
             [(dirtyId expr, dirtyId (target expr))]
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
            (EKl0 e)     -> makeUnaryTerm  KleeneStar e
            (EKl1 e)     -> makeUnaryTerm  KleenePlus e
            (EFlp e)     -> makeUnaryTerm  Converse   e
            (ECpl e)     -> makeUnaryTerm  UnaryMinus e
            (EBrk _)     -> fatal 348 "This should not happen, because EBrk has been handled before"
            (EDcD dcl)   -> [Pop "bind" "Expression" "Relation" 
                              [(dirtyId expr,dirtyId dcl)]
                            ]
            (EDcI cpt)   -> [Pop "bind" "Expression" "Relation" 
                              [(dirtyId expr,dirtyId (Isn cpt))]
                            ]
            EEps{}       -> []
            (EDcV sgn)   -> [Pop "userSrc"  (show "V") "Concept" 
                              [(dirtyId expr,dirtyId (source sgn))]
                            ,Pop "userTrg"  (show "V") "Concept" 
                              [(dirtyId expr,dirtyId (target sgn))]
                            ]
            (EMp1 v c)   -> [ --TODO! 
                              --  Pop "singleton" "Atom" "Expression" 
                              --    [(dirtyId expr,dirtyId (c,v))]
                              -- ,Pop "pop" "Atom" "Concept"
                              --    [(dirtyId (c,v),dirtyId c)]
                            ]
       ) 
  where
    makeBinaryTerm :: BinOp -> Expression -> Expression -> [Pop]
    makeBinaryTerm op lhs rhs = 
      [ Pop "first"  "BinaryTerm" "Expression"
             [(dirtyId expr,dirtyId lhs)]
      , Pop "second" "BinaryTerm" "Expression"
             [(dirtyId expr,dirtyId rhs)]
      , Pop "operator"  "BinaryTerm" "Operator"
             [(dirtyId expr,dirtyId op)]
      ]++metaPops fSpec lhs
       ++metaPops fSpec rhs
    makeUnaryTerm :: UnaryOp -> Expression -> [Pop]
    makeUnaryTerm op arg =
      [ Pop "arg" "UnaryTerm" "Expression"
             [(dirtyId expr,dirtyId arg)]
      , Pop "operator"  "BinaryTerm" "Operator"
             [(dirtyId expr,dirtyId op)]
      ]++metaPops fSpec arg

data UnaryOp = 
             KleeneStar
           | KleenePlus
           | Converse
           | UnaryMinus deriving (Eq, Show, Typeable)
instance Unique UnaryOp where
  showUnique = show

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


instance MetaPopulations Rule where
 metaPops fSpec rul =
      [ Comment " "
      , Comment $ " Rule `"++name rul++"` "

      , Pop "allRules" "Context" "Rule"
             [(dirtyId fSpec, dirtyId rul)]
      , Pop "name"  "Rule" "RuleID"
             [(dirtyId rul, (show.name) rul)]
      , Pop "ruleAdl"  "Rule" "Adl"
             [(dirtyId rul, (show.showADL.rrexp) rul)]
      , Pop "origin"  "Rule" "Origin"
             [(dirtyId rul, (show.show.origin) rul)]
      , Pop "message"  "Rule" "Message"
             [(dirtyId rul, show (aMarkup2String ReST m)) | m <- rrmsg rul, amLang m == fsLang fSpec ]
      , Pop "srcConcept"  "Rule" "Concept"
             [(dirtyId rul, (dirtyId.source.rrexp) rul)]
      , Pop "tgtConcept"  "Rule" "Concept"
             [(dirtyId rul, (dirtyId.target.rrexp) rul)]
      , Pop "conjunctIds"  "Rule" "Conjunct"
             [(dirtyId rul, dirtyId conj) | (rule,conjs)<-allConjsPerRule fSpec, rule==rul,conj <- conjs]
      , Pop "originatesFrom" "Conjunct" "Rule"
             [(dirtyId conj,dirtyId rul) | (rule,conjs)<-allConjsPerRule fSpec, rule==rul,conj <- conjs]
      , Pop "term"  "Rule" "Expression"
             [(dirtyId rul, dirtyId (rrexp rul))]
      , Pop "rrmean"  "Rule" "Meaning"
             [(dirtyId rul, show (aMarkup2String ReST m)) | m <- (maybeToList . meaning (fsLang fSpec)) rul ]
      , Pop "rrpurpose"  "Rule" "Purpose"
             [(dirtyId rul, (show.showADL) x) | x <- explanations rul]
      , -- The next population is from the adl pattern 'Plugs':
        Pop "sign" "Rule" "Signature"
             [(dirtyId rul, dirtyId (sign rul))]
      , Pop "declaredthrough" "PropertyRule" "Property"
             [(dirtyId rul, dirtyId prp) | Just(prp,_) <- [rrdcl rul]]
      , Pop "decprps" "Relation" "PropertyRule"
             [(dirtyId dcl, dirtyId rul) | Just(_,dcl) <- [rrdcl rul]]
      ]


instance MetaPopulations a => MetaPopulations [a] where
 metaPops fSpec = concatMap $ metaPops fSpec
 


-----------------------------------------------------
data Pop = Pop { popName ::   String
               , popSource :: String
               , popTarget :: String
               , popPairs ::  [(String,String)]
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
          showPaire (s,t) = "( "++s++" , "++t++" )"
          prepend str = "-- " ++ str

class Unique a => AdlId a where
 dirtyId :: a -> String
 dirtyId = show . camelCase . uniqueShow False
-- All 'things' that are relevant in the meta-environment (RAP),
-- must be an instance of AdlId:
instance AdlId A_Concept
instance AdlId A_Gen
instance AdlId Atom
instance AdlId ConceptDef
instance AdlId Declaration
instance AdlId Prop
instance AdlId Expression
  where dirtyId = show . show . hash . camelCase . uniqueShow False  -- Need to hash, because otherwise too long (>255)
instance AdlId BinOp
instance AdlId UnaryOp
instance AdlId FSpec
instance AdlId A_Pair
instance AdlId Pattern
instance AdlId PlugInfo
instance AdlId PlugSQL
instance AdlId (PlugSQL,SqlAttribute)
  where dirtyId (plug,att) = concatDirtyIdStrings [dirtyId plug, (show.camelCase.attName) att]
instance AdlId Purpose
instance AdlId (A_Concept,PSingleton)
instance AdlId Rule
instance AdlId Role
instance AdlId Signature
instance AdlId TType
instance AdlId Conjunct
instance AdlId (PairView Expression)
  where dirtyId x = show (typeOf x)++show (hash x)
instance AdlId (PairViewSegment Expression)
  where dirtyId x = show (typeOf x)++show (hash (show (hash x) ++ show (origin x)))
instance AdlId Bool
  where dirtyId = map toUpper . show
instance AdlId a => AdlId [a] where
--instance AdlId (Declaration,Paire)



-- | remove spaces and make camelCase
camelCase :: String -> String
camelCase str = concatMap capitalize (words str)
  where
    capitalize [] = []
    capitalize (s:ss) = toUpper s : ss

-- | utility function to concat dirtyId's, knowing that the individual strings are doublequoted
concatDirtyIdStrings :: [String] -> String
concatDirtyIdStrings [] = []
concatDirtyIdStrings [s] = s
concatDirtyIdStrings (s0:s1:ss)   
  | length s0 < 2 = fatal 645 "String too short to have quotes: "++s0
  | length s1 < 2 = fatal 646 "String too short to have quotes: "++s1
  | otherwise = concatDirtyIdStrings (concatFirstTwo:ss)
  where
   concatFirstTwo = show (unquoted s0 ++ separator ++ unquoted s1)
   separator = "."
   unquoted = reverse . unqfst . reverse . unqfst
   unqfst ('"':tl) = tl
   unqfst _ = fatal 653 "expected quote, but it is not there!"
nullContent :: Pop -> Bool
nullContent (Pop _ _ _ []) = True
nullContent _ = False
    
class MetaPopulations a where
 metaPops :: FSpec -> a -> [Pop]


