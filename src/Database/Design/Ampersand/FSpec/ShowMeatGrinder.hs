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
import Data.Maybe
import Data.Typeable
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.FSpec.FSpecAux
import Database.Design.Ampersand.FSpec.Motivations
import Database.Design.Ampersand.FSpec.SQL
import Database.Design.Ampersand.FSpec.ToFSpec.NormalForms (conjNF)
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.FSpec.ShowADL
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.Classes.ConceptStructure


makeMetaPopulationFile :: MetaType -> FSpec -> (FilePath,String)
makeMetaPopulationFile mType fSpec
  = ("MetaPopulationFile"++show mType++".adl", content popKind mType fSpec)
    where popKind = case mType of
                      Generics -> generics fSpec
                      AST      -> metaPops fSpec 

{-SJ 2015-11-06 Strange that the function 'content' generates text.
I would have expected a P-structure (of even an A-structure) instead.
Is there a reason? -}
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
           [(dirtyId fSpec, show ampersandVersionStr)]
    , Pop "contextName" "Context" "ContextName"
           [(dirtyId fSpec, (show.name) fSpec)]
    , Pop "dbName" "Context" "DatabaseName"
           [(dirtyId fSpec, (show.dbName.getOpts) fSpec)]
    , Comment " ", Comment $ "[Relations]--: (count="++(show.length.allDecls) fSpec++")" ]
  ++   concatMap (generics fSpec) (allDecls fSpec)
  ++[ Comment " ", Comment $ "[Concepts]--: (count="++(show.length) [c | c <- concs fSpec]++")"]
  ++   concatMap (generics fSpec) [c | c <- concs fSpec]
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
  ++[ Pop "versionInfo" "Context"  "AmpersandVersion"
           [(dirtyId fSpec, show ampersandVersionStr)]
    , Pop "name" "Context" "ContextIdentifier"
           [(dirtyId fSpec, (show.name) fSpec)]
    , Pop "dbName" "Context" "DatabaseName"
           [(dirtyId fSpec, (show.dbName.getOpts) fSpec)]
--    , Pop "concs" "Context" "Concept"
--           [(dirtyId fSpec, show "SESSION"), (dirtyId fSpec, show "ONE")]
--    , Pop "name"   "Context" "ContextIdentifier"
--           [(dirtyId fSpec, (show.name) fSpec)]
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
--  ++[ Comment " ", Comment $ "PATTERN Atoms: (count="++(show.length) (allAtoms fSpec)++")"]
--  ++   concatMap (metaPops fSpec) (allAtoms fSpec)
  ++[ Comment " ", Comment $ "PATTERN Signature: (count="++(show.length.allSigns) fSpec++")"]
  ++   concatMap (metaPops fSpec) (allSigns fSpec)
  ++[ Comment " ", Comment $ "PATTERN Relation: (count="++(show.length.allDecls) fSpec++")"]
  ++   concatMap (metaPops fSpec) (allDecls fSpec ++ [ Isn c | c<-concs fSpec])
  ++[ Comment " ", Comment $ "PATTERN Expression: (count="++(show.length.allExprs) fSpec++")"]
  ++   concatMap (metaPops fSpec) (allExprs  fSpec)
  ++[ Comment " ", Comment $ "PATTERN Rules: (count="++(show.length.fallRules) fSpec++")"]
  ++   concatMap (metaPops fSpec) ((sortBy (comparing name).fallRules)    fSpec)
  ++[ Comment " ", Comment $ "PATTERN Plugs: (count="++(show.length) allSqlPlugs++")"]
  ++   concatMap (metaPops fSpec) allSqlPlugs
--  ++[ Comment " ", Comment $ "[Initial pairs]--: (count="++(show.length.allLinks) fSpec++")"]
--  ++   concatMap (metaPops fSpec) (allLinks fSpec)
  )
  where
    allSqlPlugs = sortBy (comparing name) [plug | InternalPlug plug <- plugInfos fSpec]

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

instance GenericPopulations A_Concept where
 generics fSpec cpt =
   case cpt of
     PlainConcept{} ->
      [ Comment " "
      , Comment $ " Concept `"++name cpt++"` "
      , Pop "allConcepts" "Context" "Concept"
             [(dirtyId fSpec,dirtyId cpt)]
      , Pop "name" "Concept" "Identifier"
             [(dirtyId cpt, (show.name) cpt)]
      , Pop "affectedInvConjunctIds" "Concept" "ConjunctID"
             [(dirtyId cpt, dirtyId conj) | conj <- filterFrontEndInvConjuncts affConjs]
      , Pop "affectedSigConjunctIds" "Concept" "ConjunctID"
             [(dirtyId cpt, dirtyId conj) | conj <- filterFrontEndSigConjuncts affConjs]
      , Pop "conceptTableFields" "Concept" "TableColumn"
             [(dirtyId cpt, dirtyId att) | att <- tablesAndAttributes]
      ]
     ONE -> 
      [ Comment " "
      , Comment $ " Concept ONE "
      , Pop "allConcepts" "Context" "Concept"
             [(dirtyId fSpec,dirtyId cpt)]
      , Pop "name" "Concept" "Identifier"
             [(dirtyId cpt, (show.name) cpt)]
      , Pop "conceptTableFields" "Concept" "TableColumn"
             [(dirtyId cpt, dirtyId att) | att <- tablesAndAttributes]
      ]
  where
    affConjs = nub [ conj  
                   | Just conjs<-[lookup cpt (allConjsPerConcept fSpec)]
                   , conj<-conjs
                   ]
    largerConcs = largerConcepts (vgens fSpec) cpt++[cpt]
    tablesAndAttributes = nub . concatMap (lookupCpt fSpec) $ largerConcs

instance MetaPopulations A_Concept where
 metaPops fSpec cpt =
   [ Comment " "
   , Comment $ " Concept `"++name cpt++"` "
   , Pop "ttype" "Concept" "TType"
             [(dirtyId cpt,dirtyId ((cptTType fSpec) cpt))] 
   ]++
   case cpt of
     PlainConcept{} ->
      [ Comment $ " Concept `"++name cpt++"` "
      , Pop "concs" "Context" "Concept"
             [(dirtyId fSpec,dirtyId cpt)]
      , Pop "name" "Concept" "Identifier"
             [(dirtyId cpt, dirtyId cpt)]
--      , Pop "conceptColumn" "Concept" "SqlAttribute"
--             [(dirtyId cpt, dirtyId att) | att <- tablesAndAttributes]
--      , Pop "cptdf" "Concept" "ConceptDefinition"
--             [(dirtyId cpt,(show.showADL) cdef) | cdef <- conceptDefs  fSpec, name cdef == name cpt]
      , Pop "cptpurpose" "Concept" "Purpose"
             [(dirtyId cpt,(show.showADL) x) | lang <- allLangs, x <- fromMaybe [] (purposeOf fSpec lang cpt) ]
      ]
     ONE -> []
  where
    largerConcs = largerConcepts (vgens fSpec) cpt++[cpt]
    tablesAndAttributes = nub . concatMap (lookupCpt fSpec) $ largerConcs

instance GenericPopulations PlugSQL where
  generics fSpec plug =
      [ Comment " "
      , Comment $ "Plug: '"++name plug++"'"
      , Pop "tableInfo" "Context" "DBTable"
               [(dirtyId fSpec, dirtyId plug)]
      ] ++ concatMap (generics fSpec) [(plug,att) | att <- plugAttributes plug]

instance MetaPopulations PlugSQL where
  metaPops fSpec plug =
  --    [ Pop "context" "PlugInfo" "Context"
  --             [(dirtyId plug, dirtyId fSpec)]
  --    , Pop "key" "PlugInfo" "SqlAttribute"
  --             [(dirtyId plug, dirtyId (plug, head . plugAttributes $ plug))]
  --    ] ++ 
      concatMap (metaPops fSpec) [(plug,att) | att <- plugAttributes plug]

instance GenericPopulations (PlugSQL,SqlAttribute) where
  generics _ (plug,att) =
      [ Pop "columninfo" "DBTable" "TableColumn"
                 [(dirtyId plug, dirtyId (plug,att)) ]
      , Pop "concept" "TableColumn" "Concept"
                 [(dirtyId (plug,att), dirtyId.target.attExpr $ att)]
      , Pop "unique" "TableColumn" "Boolean"
                 [(dirtyId (plug,att), (dirtyId.attUniq) att)]
      , Pop "null" "TableColumn" "Boolean"
                 [(dirtyId (plug,att), (dirtyId.attNull) att)]
      ]

instance MetaPopulations (PlugSQL,SqlAttribute) where
  metaPops _ (plug,att) =
      [ Pop "table" "SqlAttribute" "PlugInfo"
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
instance GenericPopulations Role where
  generics fSpec rol =
      [ Comment $ "Role: '"++name rol++"'"
      , Pop "allRoles" "Context" "Role"
                 [(dirtyId fSpec, dirtyId rol) ]
      , Pop "name" "Role" "TEXT"
                 [(dirtyId rol, dirtyId rol) ]
      , Pop "maintains" "Role" "Rule"
                 [(dirtyId rol, dirtyId rul) | (rol',rul) <-  fRoleRuls fSpec, rol==rol' ]
      ]

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

instance GenericPopulations Declaration where
 generics fSpec dcl =
   case dcl of 
     Sgn{} ->
      [ Comment " "
      , Comment $ " Relation '"++name dcl++showSign (sign dcl)++"'"
      , Pop "allRelations" "Context" "Relation"
             [(dirtyId fSpec, dirtyId dcl)]
      , Pop "name" "Relation" "RelationName"
             [(dirtyId dcl, (show.name) dcl)]
      , Pop "srcConcept" "Relation" "Concept"
             [(dirtyId dcl,dirtyId (source dcl))]
      , Pop "tgtConcept" "Relation" "Concept"
             [(dirtyId dcl,dirtyId (target dcl))]
      , Pop "table" "Relation" "DBTable"
             [(dirtyId dcl,dirtyId table)]
      , Pop "srcCol" "Relation" "DBTableColumn"
             [(dirtyId dcl,dirtyId (table,srcCol))]
      , Pop "tgtCol" "Relation" "DBTableColumn"
             [(dirtyId dcl,dirtyId (table,tgtCol))]
      , Pop "affectedInvConjunctIds" "Relation" "ConjunctID"
             [(dirtyId dcl,dirtyId conj) | conj <- filterFrontEndInvConjuncts affConjs ]
      , Pop "affectedSigConjunctIds" "Relation" "ConjunctID"
             [(dirtyId dcl,dirtyId conj) | conj <- filterFrontEndSigConjuncts affConjs ]
      ]
     Isn{} -> fatal 157 "Isn is not implemented yet"
     Vs{}  -> fatal 158 "Vs is not implemented yet"
   where
     (table,srcCol,tgtCol) = getDeclarationTableInfo fSpec dcl  -- type: (PlugSQL,SqlAttribute,SqlAttribute)
     affConjs = fromMaybe [] (lookup dcl $ allConjsPerDecl fSpec)

instance MetaPopulations Declaration where
 metaPops fSpec dcl =
   case dcl of
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
     Isn ONE -> [Comment "No population for the declaration ONE" ]
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
      [ Pop "src" "Term" "Concept"
             [(dirtyId expr, dirtyId (source expr))]
      , Pop "tgt" "Term" "Concept"
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
            (EDcD dcl)   -> [Pop "bind" "Term" "Relation" [(dirtyId expr,dirtyId dcl)]
                            ]
--            EDcI{}       -> 
--            EEps{}       -> 
--            EDcV{}       -> 
--            EMp1{}       -> 
            _            -> [Comment $ "TODO(in instance MetaPopulations Expression): "++showADL expr]
-- TODO: Work on the rest of the expressions (get rid of the statement above) 
       ) 
  where
    makeBinaryTerm :: BinOp -> Expression -> Expression -> [Pop]
    makeBinaryTerm op lhs rhs = 
      [ Pop "lhs"  "BinaryTerm" "Term"
             [(dirtyId expr,dirtyId lhs)]
      , Pop "rhs"  "BinaryTerm" "Term"
             [(dirtyId expr,dirtyId rhs)]
      , Pop "first"  "BinaryTerm" "Expression"
             [(dirtyId expr,dirtyId lhs)]
      , Pop "second" "BinaryTerm" "Expreseeion"
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


instance GenericPopulations Rule where
 generics fSpec rul =
      [ Comment " "
      , Comment $ " Rule `"++name rul++"` "
      , Pop "allRules" "Context" "Rule"
             [(dirtyId fSpec, dirtyId rul)]
      , Pop "name"  "Rule" "RuleID"
             [(dirtyId rul, (show.name) rul)]
      , Pop "ruleAdl"  "Rule" "Adl"
             [(dirtyId rul,(show.showADL.rrexp) rul)]
      , Pop "origin"  "Rule" "Origin"
             [(dirtyId rul,(show.show.origin) rul)]
      , Pop "meaning"  "Rule" "Meaning"
             [(dirtyId rul,aMarkup2String ReST m) | m <- (maybeToList . meaning (fsLang fSpec)) rul ]
      , Pop "message"  "Rule" "Message"
             [(dirtyId rul,aMarkup2String ReST m) | m <- rrmsg rul, amLang m == fsLang fSpec ]
      , Pop "srcConcept"  "Rule" "Concept"
             [(dirtyId rul,(dirtyId.source.rrexp) rul)]
      , Pop "tgtConcept"  "Rule" "Concept"
             [(dirtyId rul,(dirtyId.target.rrexp) rul)]
      , Pop "conjunctIds"  "Rule" "ConjunctID"
             [(dirtyId rul,dirtyId conj) | (rule,conjs)<-allConjsPerRule fSpec, rule==rul,conj <- conjs]
      ]++case rrviol rul of
        Nothing -> []
        Just pve ->
         [ Pop "pairView"  "Rule" "PairView"
              [(dirtyId rul, dirtyId pve)]
         ]++generics fSpec pve
      
      
instance GenericPopulations (PairView Expression) where
 generics fSpec pve = 
      [ Comment " "
      ]++concatMap makeSegment (zip [0..] (ppv_segs pve))
  where
    makeSegment :: (Int,PairViewSegment Expression) -> [Pop]
    makeSegment (i,pvs) =
      [ Pop "segment" "PairView" "PairViewSegment"
             [(dirtyId pve,dirtyId pvs)]
      , Pop "sequenceNr" "PairViewSegment" "Int"
             [(dirtyId pvs,show i)]
      , Pop "segmentType" "PairViewSegment" "PairViewSegmentType"
             [(dirtyId pvs, case pvs of 
                         PairViewText{} -> "Text"
                         PairViewExp{}  -> "Exp")
            ]
      ]++
      case pvs of
        PairViewText{} -> 
          [Pop "text" "PairViewSegment" "String"
               [(dirtyId pvs, pvsStr pvs)] 
          ]
        PairViewExp{} -> 
          [Pop "srcOrTgt" "PairViewSegment" "SourceOrTarget"
               [(dirtyId pvs, show (pvsSoT pvs))] 
          ,Pop "expTgt" "PairViewSegment" "Concept"
               [(dirtyId pvs, dirtyId (case pvsSoT pvs of
                                Src -> source (pvsExp pvs)
                                Tgt -> target (pvsExp pvs)
                              ))] 
          ,Pop "expSQL" "PairViewSegment" "MySQLQuery"
               [(dirtyId pvs, prettySQLQuery fSpec 0 (pvsExp pvs))] 
          ]

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
      , Pop "conjunctIds"  "Rule" "ConjunctID"
             [(dirtyId rul, dirtyId conj) | (rule,conjs)<-allConjsPerRule fSpec, rule==rul,conj <- conjs]
      , Pop "rrexp"  "Rule" "Expression"
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



instance MetaPopulations PlugInfo where
 metaPops _ plug = 
      [ Comment $ " Plug `"++name plug++"` "
      , Pop "maintains" "Plug" "Rule" [{-STILL TODO. -}] --HJO, 20150205: Waar halen we deze info vandaan??
      , Pop "in" "Concept" "Plug"                 
             [(dirtyId cpt,dirtyId plug)| cpt <- concs plug]  
--      , Pop "relsMentionedIn" "Plug" "Relation"
--             [(dirtyId plug,dirtyId dcl)| dcl <- relsMentionedIn plug]
      ]      

instance MetaPopulations a => MetaPopulations [a] where
 metaPops fSpec = concatMap $ metaPops fSpec
 
instance GenericPopulations Conjunct where
 generics fSpec conj =
  [ Comment $ "Conjunct: '"++rc_id conj++"'."
  , Pop "allConjuncts" "Context" "Conjunct" 
         [(dirtyId fSpec, dirtyId conj)]
  , Pop "signalRuleNames" "Conjunct" "Rule" 
         [(dirtyId conj,dirtyId r) | r <- rc_orgRules conj, isFrontEndSignal r]
  , Pop "invariantRuleNames" "Conjunct" "Rule" 
         [(dirtyId conj,dirtyId r) | r <- rc_orgRules conj, isFrontEndInvariant  r]
  , Pop "violationsSQL" "Conjunct" "MySQLQuery" 
         [(dirtyId conj , prettySQLQuery fSpec 0 (conjNF (getOpts fSpec) (notCpl (rc_conjunct conj)))
          )]
  ] 


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
instance AdlId BinOp
instance AdlId UnaryOp
instance AdlId FSpec
instance AdlId A_Pair
instance AdlId Pattern
instance AdlId PlugInfo
instance AdlId PlugSQL
instance AdlId (PlugSQL,SqlAttribute)
  where dirtyId (plug,att) = concatDirtyIdStrings $ [dirtyId plug, (show.camelCase.attName) att]
instance AdlId Purpose
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
