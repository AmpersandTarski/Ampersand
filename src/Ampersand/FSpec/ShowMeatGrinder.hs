{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Ampersand.FSpec.ShowMeatGrinder
  (makeMetaPopulationFile)
where

import Data.List
import Data.Char
import Data.Ord
import qualified Data.Map.Strict as Map
import Data.Hashable (hash) -- a not good enouqh function, but used for the time being. 
import Data.Maybe
import Data.Typeable
import Ampersand.FSpec.FSpec
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
             useful for debugging. However, the idea is good. In future, we might change
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
    , "CONTEXT FormalAmpersand IN ENGLISH -- (the language is chosen arbitrary, for it is mandatory but irrelevant."
    , showRelsFromPops pops
    , "" ]
    ++ intercalate [] (map (lines . showADL ) pops)  ++
    [ ""
    , "ENDCONTEXT"
    ])
    where pops = metaPops fSpec fSpec

{-SJ 2016-07-24 In generating the metapopulation of a script, we need to maintain a close relation
with the A-structure. But why?
-} 
instance MetaPopulations FSpec where
 metaPops _ fSpec =
   filter (not.nullContent)
    (
    [Comment  " ", Comment $ "PATTERN Context: ('"++name fSpec++"')"]
  ++[ Pop "versionInfo" "Context"  "AmpersandVersion" [Uni,Tot]
           [(dirtyId ctx ctx, show ampersandVersionStr)]
    , Pop "dbName" "Context" "DatabaseName" [Uni,Tot]
           [(dirtyId ctx ctx, (show.dbName.getOpts) fSpec)]
    , Pop "name" "Context" "Identifier" [Uni,Tot]
           [(dirtyId ctx ctx, (show.ctxnm) ctx)]
    , Pop "location" "Context" "Location" [Uni,Tot]
           [(dirtyId ctx ctx, (show.showUnique.ctxpos) ctx)]
    , Pop "language" "Context" "Language" [Uni,Tot]
           [(dirtyId ctx ctx, (show.show.ctxlang) ctx)]
    , Pop "markup" "Context" "Markup" [Uni,Tot]
           [(dirtyId ctx ctx, (show.show.ctxmarkup) ctx)]
    , Pop "context" "Pattern" "Context" [Uni]                      -- The context in which a pattern is defined.
           [(dirtyId ctx p, dirtyId ctx ctx) | p<-ctxpats ctx]
    , Pop "context" "Rule" "Context" [Uni]                         -- The context in which a rule is defined.
           [(dirtyId ctx r, dirtyId ctx ctx) | r<-ctxrs ctx]
    , Pop "context" "Relation" "Context" [Uni]                         -- The context in which a rule is defined.
           [(dirtyId ctx r, dirtyId ctx ctx) | r<-ctxds ctx]
    , Pop "context" "Population" "Context" [Uni]                         -- The context in which a rule is defined.
           [(dirtyId ctx pop, dirtyId ctx ctx) | pop<-ctxpopus ctx]
    , Pop "context" "Concept" "Context" [Uni]                         -- The context in which a rule is defined.
           [(dirtyId ctx c, dirtyId ctx ctx) | c<-ctxcds ctx]
    , Pop "context" "IdentityDef" "Context" [Uni]                         -- The context in which a rule is defined.
           [(dirtyId ctx c, dirtyId ctx ctx) | c<-ctxks ctx]
    , Pop "allRoleRules" "Context" "Role" [Tot]
           [(dirtyId ctx ctx, show "SystemAdmin")]
    , Pop "name"   "Role" "RoleName" [Uni,Tot]
           [(show "SystemAdmin", show "SystemAdmin")]
    ]
  ++[ Comment " ", Comment $ "PATTERN Patterns: (count="++(show.length.vpatterns) fSpec++")"]
  ++   concatMap extract (sortByName (vpatterns fSpec))
  ++[ Comment " ", Comment $ "PATTERN Specialization: (count="++(show.length.vgens) fSpec++")"]
  ++   concatMap extract (vgens fSpec)
  ++[ Comment " ", Comment $ "PATTERN Concept: (count="++(show.length.concs) fSpec++")"]
  ++   concatMap extract (sortByName (concs fSpec))
  ++[ Comment " ", Comment $ "PATTERN Signature: (count="++(show.length.allSigns) fSpec++")"]
  ++   concatMap extract (allSigns fSpec)
  ++[ Comment " ", Comment $ "PATTERN Relation: (count="++(show.length.vrels) fSpec++")"]
  ++   concatMap extract (vrels fSpec ++ [ Isn c | c<-concs fSpec])
  ++[ Comment " ", Comment $ "PATTERN Expression: (count="++(show.length.allExprs) fSpec++")"]
  ++   concatMap extract (allExprs  fSpec)
  ++[ Comment " ", Comment $ "PATTERN Rules: (count="++(show.length.fallRules) fSpec++")"]
  ++   concatMap extract (sortByName (fallRules fSpec))
  ++[ Comment " ", Comment $ "PATTERN Conjuncts: (count="++(show.length.allConjuncts) fSpec++")"]
  ++   concatMap extract (allConjuncts fSpec)
  ++[ Comment " ", Comment $ "PATTERN Plugs: (count="++(show.length.plugInfos) fSpec++")"]
  ++   concatMap extract (sortByName (plugInfos fSpec))
  ++[ Comment " ", Comment $ "PATTERN Interfaces: (count="++(show.length.interfaceS) fSpec++")"]
  ++   concatMap extract (sortByName (interfaceS fSpec))
  ++[ Comment " ", Comment $ "PATTERN Roles: (count="++(show.length.fRoles) fSpec++")"]
  ++   concatMap (extract . fst) (fRoles fSpec)
  )
  where 
    ctx = originalContext fSpec
    extract :: MetaPopulations a => a -> [Pop]
    extract = metaPops fSpec
    sortByName :: Named a => [a] -> [a]
    sortByName = sortBy (comparing name)

instance MetaPopulations Pattern where
 metaPops fSpec pat =
   [ Comment " "
   , Comment $ " Pattern `"++name pat++"` "
   , Pop "name"    "Pattern" "PatternIdentifier" [Uni,Tot]
          [(dirtyId ctx pat, (show.name) pat)]
--  Activate this code when concept definitions are allowed inside a pattern
--   , Pop "concepts"   "Pattern" "Concept" []
--          [(dirtyId pat,dirtyId x) | x <- ptcds pat]
   , Pop "rules"   "Pattern" "Rule" []
          [(dirtyId ctx pat,dirtyId ctx x) | x <- ptrls pat]
   , Pop "relsDefdIn"   "Context" "Relation" [Sur,Inj]
          [(dirtyId ctx ctx,dirtyId ctx x) | x <- (relsDefdIn.originalContext) fSpec]
   , Pop "relsDefdIn"   "Pattern" "Relation" [Sur,Inj]
          [(dirtyId ctx pat,dirtyId ctx x) | x <- ptdcs pat]
   , Pop "purpose"   "Pattern" "Purpose" [Uni,Tot]
          [(dirtyId ctx pat,dirtyId ctx x) | x <- ptxps pat]
   ]
  where 
    ctx = originalContext fSpec

instance MetaPopulations A_Gen where
 metaPops fSpec gen =
  [ Pop "gens" "Context" "Gen" [Sur,Inj]
          [(dirtyId ctx ctx,dirtyId ctx gen)]
  , Pop "genspc"  "Gen" "Concept" []
          [(dirtyId ctx gen,dirtyId ctx (genspc gen))]
  , Pop "gengen"  "Gen" "Concept" []
          [ (dirtyId ctx gen,dirtyId ctx c)
          | c<- case gen of
                     Isa{} -> [gengen gen]
                     IsE{} -> genrhs gen
          ]
  ]
  where 
    ctx = originalContext fSpec

instance MetaPopulations A_Concept where
 metaPops fSpec cpt =
   [ Comment " "
   , Comment $ " Concept `"++name cpt++"` "
   , Pop "ttype" "Concept" "TType" [Uni,Tot]
             [(dirtyId ctx cpt, dirtyId ctx (cptTType fSpec cpt))] 
   , Pop "name" "Concept" "Identifier" [Uni,Tot]
             [(dirtyId ctx cpt, show . name $ cpt)]
   ]++
   case cpt of
     PlainConcept{} ->
      [ Pop "concs" "Context" "Concept" [Sur,Inj]
             [(dirtyId ctx ctx, dirtyId ctx cpt)]
      ]
     ONE -> 
      [ ]
  where
    ctx = originalContext fSpec

instance MetaPopulations Conjunct where
  metaPops fSpec conj =
    [ Comment $ " Conjunct `"++rc_id conj++"` "
    , Pop "allConjuncts" "Context" "Conjunct" [Sur,Inj]
             [(dirtyId ctx ctx, dirtyId ctx conj)]
    , Pop "originatesFrom" "Conjunct" "Rule" [Uni,Tot]
             [(dirtyId ctx conj, dirtyId ctx rul) | rul <- rc_orgRules conj]
    , Pop "conjunct" "Conjunct" "Expression" [Uni,Tot]
             [(dirtyId ctx conj, dirtyId ctx (rc_conjunct conj))]
    ] 
   where
    ctx = originalContext fSpec

instance MetaPopulations PlugInfo where
  metaPops fSpec plug = 
      [ Comment $ " Plug `"++name plug++"` "
      , Pop "maintains" "Plug" "Rule" []
             [{-STILL TODO. -}] --HJO, 20150205: Waar halen we deze info vandaan??
      , Pop "in" "Concept" "Plug" []
             [(dirtyId ctx cpt,dirtyId ctx plug)| cpt <- concs plug]  
--      , Pop "relsInPlug" "Plug" "Relation" []
--             [(dirtyId ctx plug,dirtyId ctx dcl)| dcl <- relsMentionedIn plug]
      ]++
      (case plug of
         InternalPlug plugSQL   -> metaPops fSpec plugSQL
         ExternalPlug _ -> fatal 167 "ExternalPlug is not implemented in the meatgrinder. "
      )      
   where
    ctx = originalContext fSpec

instance MetaPopulations PlugSQL where
  metaPops _ _ = []
{-    case plug of 
       TblSQL{} ->
         [ Pop "rootConcept" "TblSQL" "Concept" []
               [(dirtyId ctx plug, dirtyId ctx . target . attExpr . head . plugAttributes $ plug)]
         , Pop "key" "TblSQL" "SqlAttribute" []
               [(dirtyId ctx plug, dirtyId ctx (plug,head . plugAttributes $ plug))]
         ] ++ 
         concatMap extract [(plug,att) | att <- plugAttributes plug]
       BinSQL{} -> []  
  where
    ctx = originalContext fSpec
-}

instance MetaPopulations (PlugSQL,SqlAttribute) where
  metaPops _ (_,_) = []
{-      [ Pop "table" "SqlAttribute" "SQLPlug" []
                 [(dirtyId ctx (plug,att), dirtyId ctx plug) ]
      , Pop "concept" "SqlAttribute" "Concept" []
                 [(dirtyId ctx (plug,att), dirtyId ctx.target.attExpr $ att)]
      , Pop "relsInPlug" "Plug" "Relation" []
                 [(dirtyId ctx plug, dirtyId ctx rel) | Just rel <- [primRel.attExpr $ att]]
--      , Pop "null" "SqlAttribute" "SqlAttribute" []
--                 [(a,a) | attNull att, let a=dirtyId ctx (plug,att)]
      ]
    where primRel :: Expression -> Maybe Declaration
          primRel expr =
            case expr of
              EDcD dcl -> Just dcl
              EFlp (EDcD dcl) -> Just dcl
              EDcI cpt -> Just (Isn cpt)
              _  -> Nothing
          ctx = originalContext fSpec
-}

instance MetaPopulations Role where
  metaPops fSpec rol =
      [ Pop "allRoles" "Context" "Role" [Sur,Inj]
                 [(dirtyId ctx ctx, dirtyId ctx rol) ]
      , Pop "name" "Role" "RoleName" [Uni,Tot]
                 [(dirtyId ctx rol, dirtyId ctx rol) ]
      , Pop "maintains" "Role" "Rule" []
                 [(dirtyId ctx rol, dirtyId ctx rul) | (rol',rul) <-  fRoleRuls fSpec, rol==rol' ]
      , Pop "interfaces" "Role" "Interface" []
                 [(dirtyId ctx rol, dirtyId ctx ifc) | ifc <- roleInterfaces fSpec rol]
      ]
   where
    ctx = originalContext fSpec

instance MetaPopulations Interface where
  metaPops fSpec ifc =
      [ Pop "interfaces" "Context" "Interface" [Sur,Inj]
                 [(dirtyId ctx ctx, dirtyId ctx ifc) ]
      ]
   where
    ctx = originalContext fSpec

instance MetaPopulations Atom where
  metaPops fSpec atm =
   [ Pop "pop" "Atom" "Concept" []
          [(dirtyId ctx atm, dirtyId ctx cpt)
          |cpt <- atmRoots atm]
   , Pop "repr"  "Atom" "Representation" [Uni,Tot]
          [(dirtyId ctx atm, (showValADL.atmVal) atm)]
   ]
   where
    ctx = originalContext fSpec

instance MetaPopulations Signature where
 metaPops fSpec sgn =
      [ Pop "src" "Signature" "Concept" [Uni,Tot]
             [(dirtyId ctx sgn, dirtyId ctx (source sgn))]
      , Pop "tgt" "Signature" "Concept" [Uni,Tot]
             [(dirtyId ctx sgn, dirtyId ctx (target sgn))]
      ]
  where
    ctx = originalContext fSpec

instance MetaPopulations Declaration where
 metaPops fSpec dcl =
   (case dcl of
     Sgn{} ->
      [ Comment " "
      , Comment $ " Relation `"++name dcl++" ["++(name.source.decsgn) dcl++" * "++(name.target.decsgn) dcl++"]"++"` "
      , Pop "context" "Relation" "Context" [Uni,Tot]
             [(dirtyId ctx dcl,dirtyId ctx ctx)] 
      , Pop "name" "Relation" "Name" [Uni,Tot]
             [(dirtyId ctx dcl, (show.name) dcl)]
--      , Pop "srcCol" "Relation" "SqlAttribute" []
--             [(dirtyId ctx dcl,dirtyId ctx (table,srcCol))]
--      , Pop "tgtCol" "Relation" "SqlAttribute" []
--             [(dirtyId ctx dcl,dirtyId ctx (table,tgtCol))]
      , Pop "sign" "Relation" "Signature" [Uni,Tot]
             [(dirtyId ctx dcl,dirtyId ctx (sign dcl))]
      , Pop "source" "Relation" "Concept" [Uni,Tot]
             [(dirtyId ctx dcl,dirtyId ctx (source dcl))]
      , Pop "target" "Relation" "Concept" [Uni,Tot]
             [(dirtyId ctx dcl,dirtyId ctx (target dcl))]
      , Pop "prop" "Relation" "Property" []
             [(dirtyId ctx dcl, dirtyId ctx x) | x <- decprps dcl]  -- decprps gives the user defined properties; not the derived properties.
      , Pop "decprL" "Relation" "String" [Uni,Tot]
             [(dirtyId ctx dcl,(show.decprL) dcl)]
      , Pop "decprM" "Relation" "String" [Uni,Tot]
             [(dirtyId ctx dcl,(show.decprM) dcl)]
      , Pop "decprR" "Relation" "String" [Uni,Tot]
             [(dirtyId ctx dcl,(show.decprR) dcl)]
      , Pop "decmean" "Relation" "Meaning" [Uni,Tot]
             [(dirtyId ctx dcl, (show.concatMap showADL.ameaMrk.decMean) dcl)]
      , Pop "decpurpose" "Relation" "Purpose" []
             [(dirtyId ctx dcl, (show.showADL) x) | x <- explanations dcl]
      ]
     Isn{} -> 
      [ Comment " "
      , Comment $ " Relation `I["++name (source dcl)++"]`"
      , Pop "sign" "Relation" "Signature" [Uni,Tot]
             [(dirtyId ctx dcl,dirtyId ctx (sign dcl))]
      , Pop "context" "Relation" "Context" [Uni,Tot]
             [(dirtyId ctx dcl,dirtyId ctx ctx)]
      , Pop "name" "Relation" "Name" [Uni,Tot]
             [(dirtyId ctx dcl, (show.name) dcl)]
      , Pop "source" "Relation" "Concept" [Uni,Tot]
             [(dirtyId ctx dcl,dirtyId ctx (source dcl))]
      , Pop "target" "Relation" "Concept" [Uni,Tot]
             [(dirtyId ctx dcl,dirtyId ctx (target dcl))]
      ]
     Vs{}  -> fatal 158 "Vs is not implemented yet"
   )++
   metaPops fSpec (sign dcl)
   where
    ctx = originalContext fSpec

instance MetaPopulations A_Pair where
 metaPops fSpec pair =
      [ Pop "in" "Pair" "Relation" []
             [(dirtyId ctx pair, dirtyId ctx (lnkDcl pair))]
      , Pop "l" "Pair" "Atom" [Uni,Tot]
             [(dirtyId ctx pair, dirtyId ctx (lnkLeft pair))]
      , Pop "r" "Pair" "Atom" [Uni,Tot]
             [(dirtyId ctx pair, dirtyId ctx (lnkRight pair))]
      ]
  where
    ctx = originalContext fSpec

instance MetaPopulations Expression where
 metaPops fSpec expr =
  case expr of 
    EBrk e -> metaPops fSpec e
    _      ->
      [ Pop "src" "Expression" "Concept" [Uni,Tot]
             [(dirtyId ctx expr, dirtyId ctx (source expr))]
      , Pop "tgt" "Expression" "Concept" [Uni,Tot]
             [(dirtyId ctx expr, dirtyId ctx (target expr))]
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
            (EDcD dcl)   -> [Pop "bind" "BindedRelation" "Relation" [Uni,Tot]
                              [(dirtyId ctx expr,dirtyId ctx dcl)]
                            ]
            (EDcI cpt)   -> [Pop "bind" "BindedRelation" "Relation" [Uni,Tot]  -- SJ 2016-07-24 TODO: Here is something fishy going on...
                              [(dirtyId ctx expr,dirtyId ctx (Isn cpt))]
                            ]
            EEps{}       -> []
            (EDcV sgn)   -> [Pop "userSrc"  (show "V") "Concept"  [Uni,Tot]
                              [(dirtyId ctx expr,dirtyId ctx (source sgn))]
                            ,Pop "userTrg"  (show "V") "Concept"  [Uni,Tot]
                              [(dirtyId ctx expr,dirtyId ctx (target sgn))]
                            ]
            (EMp1 v _)   -> [ Pop "singleton" "Singleton" "AtomValue" [Uni,Tot]
                              [(dirtyId ctx expr,showADL v)]
                            ]
       ) 
  where
    ctx = originalContext fSpec
    makeBinaryTerm :: BinOp -> Expression -> Expression -> [Pop]
    makeBinaryTerm op lhs rhs = 
      [ Pop "first"  "BinaryTerm" "Expression" [Uni,Tot]
             [(dirtyId ctx expr,dirtyId ctx lhs)]
      , Pop "second" "BinaryTerm" "Expression" [Uni,Tot]
             [(dirtyId ctx expr,dirtyId ctx rhs)]
      , Pop "operator"  "BinaryTerm" "Operator" [Uni,Tot]
             [(dirtyId ctx expr,dirtyId ctx op)]
      ]++metaPops fSpec lhs
       ++metaPops fSpec rhs
    makeUnaryTerm :: UnaryOp -> Expression -> [Pop]
    makeUnaryTerm op arg =
      [ Pop "arg" "UnaryTerm" "Expression" [Uni,Tot]
             [(dirtyId ctx expr,dirtyId ctx arg)]
      , Pop "operator"  "BinaryTerm" "Operator" [Uni,Tot]
             [(dirtyId ctx expr,dirtyId ctx op)]
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
      , Pop "name"  "Rule" "RuleID" [Uni,Tot]
             [(dirtyId ctx rul, (show.name) rul)]
      , Pop "ruleAdl"  "Rule" "Adl" [Uni,Tot]
             [(dirtyId ctx rul, (show.showADL.rrexp) rul)]
      , Pop "origin"  "Rule" "Origin" [Uni,Tot]
             [(dirtyId ctx rul, (show.show.origin) rul)]
      , Pop "message"  "Rule" "Message" []
             [(dirtyId ctx rul, show (aMarkup2String ReST m)) | m <- rrmsg rul, amLang m == fsLang fSpec ]
      , Pop "srcConcept"  "Rule" "Concept" [Uni,Tot]
             [(dirtyId ctx rul, (dirtyId ctx.source.rrexp) rul)]
      , Pop "tgtConcept"  "Rule" "Concept" [Uni,Tot]
             [(dirtyId ctx rul, (dirtyId ctx.target.rrexp) rul)]
      , Pop "conjunctIds"  "Rule" "Conjunct" [Tot,Sur,Inj]
             [(dirtyId ctx rul, dirtyId ctx conj) | (rule,conjs)<-allConjsPerRule fSpec, rule==rul,conj <- conjs]
      , Pop "originatesFrom" "Conjunct" "Rule" [Uni,Tot]
             [(dirtyId ctx conj,dirtyId ctx rul) | (rule,conjs)<-allConjsPerRule fSpec, rule==rul,conj <- conjs]
      , Pop "formalExpression"  "Rule" "Expression" [Uni,Tot]
             [(dirtyId ctx rul, dirtyId ctx (rrexp rul))]
      , Pop "rrmean"  "Rule" "Meaning" []
             [(dirtyId ctx rul, show (aMarkup2String ReST m)) | m <- (maybeToList . meaning (fsLang fSpec)) rul ]
      , Pop "rrpurpose"  "Rule" "Purpose" []
             [(dirtyId ctx rul, (show.showADL) x) | x <- explanations rul]
      , -- The next population is from the adl pattern 'Plugs':
        Pop "sign" "Rule" "Signature" [Uni,Tot]
             [(dirtyId ctx rul, dirtyId ctx (sign rul))]
      , Pop "declaredthrough" "PropertyRule" "Property" []
             [(dirtyId ctx rul, dirtyId ctx prp) | Just(prp,_) <- [rrdcl rul]]
      , Pop "decprps" "Relation" "PropertyRule" []
             [(dirtyId ctx dcl, dirtyId ctx rul) | Just(_,dcl) <- [rrdcl rul]]
      ]
  where
    ctx = originalContext fSpec


instance MetaPopulations a => MetaPopulations [a] where
 metaPops fSpec = concatMap $ metaPops fSpec
 


-----------------------------------------------------
data Pop = Pop { popName ::   String
               , popSource :: String
               , popTarget :: String
               , popMult ::   [Prop]
               , popPairs ::  [(String,String)]
               }
         | Comment { comment :: String  -- Not-so-nice way to get comments in a list of populations. Since it is local to this module, it is not so bad, I guess...
                   }

instance ShowADL Pop where
 showADL pop =
  case pop of
      Pop{} -> "POPULATION "++ popNameSignature pop++" CONTAINS"
              ++
              if null (popPairs pop)
              then "[]"
              else "\n"++indentA++"[ "++intercalate ("\n"++indentA++"; ") showContent++indentA++"]"
      Comment{} -> intercalate "\n" (map prepend (lines (comment pop)))
    where indentA = "   "
          showContent = map showPaire (popPairs pop)
          showPaire (s,t) = "( "++s++" , "++t++" )"
          prepend str = "-- " ++ str

popNameSignature :: Pop -> String
popNameSignature pop =
   case pop of
     Pop{}     -> popName pop++" ["++popSource pop++" * "++popTarget pop++"]"
     Comment{} -> fatal 503 "Must not call popName on a Comment-combinator."

showRelsFromPops :: [Pop] -> String
showRelsFromPops pops
  = intercalate "\n" [ "RELATION "++popNameSignature (head cl)++show (props cl)
                     | cl<-eqCl popNameSignature [p | p@Pop{} <- pops] ]
    where props cl = (foldr1 uni . map popMult) cl

class Unique a => AdlId a where
 dirtyId :: A_Context -> a -> String
 dirtyId _ = show . camelCase . uniqueShow False
-- All 'things' that are relevant in the meta-environment (RAP),
-- must be an instance of AdlId:
instance AdlId A_Concept
instance AdlId A_Gen
instance AdlId Atom
instance AdlId ConceptDef
instance AdlId Declaration
  where dirtyId ctx r
         = case Map.lookup r (declMap) of
            Nothing -> fatal 546 ("no relation known as: "++showUnique r)
            Just i  -> show (show i)
          where
           declMap :: Map.Map Declaration Int
           declMap = Map.fromList (zip (relsDefdIn ctx++[ Isn c | c<-concs ctx]) [1..])
instance AdlId Prop
instance AdlId Expression
  where dirtyId _ = show . show . hash . camelCase . uniqueShow False  -- Need to hash, because otherwise too long (>255)
instance AdlId BinOp
instance AdlId UnaryOp
instance AdlId A_Context
instance AdlId A_Pair
instance AdlId Pattern
instance AdlId PlugInfo
instance AdlId PlugSQL
instance AdlId (PlugSQL,SqlAttribute)
  where dirtyId ctx (plug,att) = concatDirtyIdStrings [dirtyId ctx plug, (show.camelCase.attName) att]
instance AdlId Purpose
instance AdlId Rule
instance AdlId Role
instance AdlId Population
instance AdlId IdentityDef
instance AdlId Interface
instance AdlId Signature
instance AdlId TType
instance AdlId Conjunct
instance AdlId (PairView Expression)
  where dirtyId _ x = show (typeOf x)++show (hash x)
instance AdlId (PairViewSegment Expression)
  where dirtyId _ x = show (typeOf x)++show (hash (show (hash x) ++ show (origin x)))
instance AdlId Bool
  where dirtyId _ = map toUpper . show
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
nullContent (Pop _ _ _ _ []) = True
nullContent _ = False
    
class MetaPopulations a where
 metaPops :: FSpec -> a -> [Pop]



