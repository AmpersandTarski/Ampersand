{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields#-}
{-# LANGUAGE OverloadedLabels #-}
module Ampersand.FSpec.ShowMeatGrinder
  ( dumpGrindFile
  , grind 
  )
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
import Ampersand.Core.ShowPStruct
import Ampersand.Core.ShowAStruct
import Ampersand.Core.ParseTree
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Classes
import Ampersand.Input
import Ampersand.Input.ADL1.CtxError
import Ampersand.Input.ADL1.Parser
import Ampersand.Input.Parsing

-- ^ Create a P_Context that contains meta-information from 
--   an FSpec.
grind :: FSpec -> FSpec -> P_Context
grind fromFormalAmpersand fSpec =
  (mkContextOfPopsOnly []) {ctx_ds = mapMaybe (extractFromPop fromFormalAmpersand) . metaPops fromFormalAmpersand fSpec $ fSpec }

  -- ^ Write the meta-information of an FSpec to a file. This is usefull for debugging.
--   The comments that are in Pop are preserved. 
dumpGrindFile :: FSpec -> FSpec -> (FilePath,String)
dumpGrindFile fromFormalAmpersand fSpec
  = ("MetaPopulationFile.adl", adl)
  where
    adl :: String
    adl = unlines
      ([ "{- Do not edit manually. This code has been generated!!!"
        , "    Generated with "++ampersandVersionStr
        , "    Generated at "++show (genTime (getOpts fSpec))
        , " "
        , "The populations defined in this file are the populations from the user's"
        , "model named '"++name fSpec++"'."
        , ""
        , "-}"
        , "CONTEXT "++name fSpec
        , showRelsFromPops pops
        , "" ]
        ++ intercalate [] (map (lines . showPop ) pops)  ++
        [ ""
        , "ENDCONTEXT"
        ])
        where pops = metaPops fromFormalAmpersand fSpec fSpec

{-SJ 2016-07-24 In generating the metapopulation of a script, we need to maintain a close relation
with the A-structure. But why?
-} 
instance MetaPopulations FSpec where
 metaPops fromFormalAmpersand _ fSpec =
   filter (not.nullContent)
    ( metaPops fromFormalAmpersand fSpec ctx
    ++[ --Pop "dbName" "Context" "DatabaseName" [(dirtyId ctx ctx, (show.dbName.getOpts) fSpec)]
        Pop "maintains" "Role" "Rule" 
                 [(dirtyId ctx rol, dirtyId ctx rul) | (rol,rul) <-  fRoleRuls fSpec ]
      , Pop "interfaces" "Role" "Interface"
                 [(dirtyId ctx rol, dirtyId ctx ifc) | ifc <- ctxifcs ctx, rol<-ifcRoles ifc]
      ]
--    ++[ Comment " ", Comment $ "PATTERN Conjuncts: (count="++(show.length.allConjuncts) fSpec++")"]
--    ++   concatMap extract (allConjuncts fSpec)
--    ++[ Comment " ", Comment $ "PATTERN Plugs: (count="++(show.length.plugInfos) fSpec++")"]
--    ++   concatMap extract (sortByName (plugInfos fSpec))
    ++[ Comment " ", Comment $ "PATTERN Roles: (count="++(show.length.fRoles) fSpec++")"]
    ++   concatMap (extract . fst) (fRoles fSpec)
    )
  where 
    ctx = originalContext fSpec
    extract :: MetaPopulations a => a -> [Pop]
    extract = metaPops fromFormalAmpersand fSpec

instance MetaPopulations A_Context where
 metaPops fromFormalAmpersand fSpec ctx =
   filter (not.nullContent)
    (
    [Comment  " ", Comment $ "PATTERN Context: ('"++name ctx++"')"]
  ++[ Pop "versionInfo" "Context"  "AmpersandVersion"
           [(dirtyId ctx ctx, PopAlphaNumeric ampersandVersionStr)]
    , Pop "name" "Context" "ContextName"
           [(dirtyId ctx ctx, (PopAlphaNumeric . name) ctx)]
  --  , Pop "location" "Context" "Location"
  --         [(dirtyId ctx ctx, (show.showUnique.ctxpos) ctx)]
    , Pop "language" "Context" "Language"
           [(dirtyId ctx ctx, (PopAlphaNumeric . show . ctxlang) ctx)]
  --  , Pop "markup" "Context" "Markup"
  --         [(dirtyId ctx ctx, (show.show.ctxmarkup) ctx)]
    , Pop "context" "Pattern" "Context"
           [(dirtyId ctx p, dirtyId ctx ctx) | p<-ctxpats ctx]
    , Pop "ctxrs" "Rule" "Context"
           [(dirtyId ctx r, dirtyId ctx ctx) | r<-ctxrs ctx]
    , Pop "udefrules" "Rule" "Context"
           [(dirtyId ctx r, dirtyId ctx ctx) | r<-udefrules ctx]
    , Pop "multrules" "Rule" "Context"
           [(dirtyId ctx r, dirtyId ctx ctx) | r<-multrules ctx]
    , Pop "identityRules" "Rule" "Context"
           [(dirtyId ctx r, dirtyId ctx ctx) | r<-identityRules ctx]
    , Pop "allRules" "Context" "Rule"
           [(dirtyId ctx ctx, dirtyId ctx r) | r<-allRules ctx]
    , Pop "ctxds" "Relation" "Context"
           [(dirtyId ctx r, dirtyId ctx ctx) | r<-ctxds ctx]
    , Pop "declaredIn" "Relation" "Context"
           [(dirtyId ctx r, dirtyId ctx ctx) | r<-relsDefdIn ctx]
    , Pop "context" "Population" "Context"
           [(dirtyId ctx pop, dirtyId ctx ctx) | pop<-ctxpopus ctx]
--    , Pop "context" "Concept" "Context"
--           [(dirtyId ctx c, dirtyId ctx ctx) | c<-ctxcds ctx]
    , Pop "context" "IdentityDef" "Context"
           [(dirtyId ctx c, dirtyId ctx ctx) | c<-ctxks ctx]
    , Pop "allRoles" "Context" "Role"
           [(dirtyId ctx ctx, PopAlphaNumeric "SystemAdmin")]
    , Pop "name"   "Role" "RoleName"
           [(dirtyId ctx (Role "SystemAdmin"), PopAlphaNumeric "SystemAdmin")]
    ]
  ++[ Comment " ", Comment $ "PATTERN Patterns: (count="++(show.length.patterns) ctx++")"]
  ++   (concatMap extract . sortByName . patterns) ctx
  ++[ Comment " ", Comment $ "PATTERN Specialization: (count="++(show.length.gens) ctx++")"]
  ++   concatMap extract (gens ctx)
  ++[ Comment " ", Comment $ "PATTERN Concept: (count="++(show.length.concs) ctx++")"]
  ++[ Pop "context" "Concept" "Context"
           [(dirtyId ctx c, dirtyId ctx ctx) | c<-concs ctx] ]
  ++   (concatMap extract . sortByName . concs) ctx
  ++[ Comment " ", Comment $ "PATTERN Relation: (count="++(show.length.relsDefdIn) ctx++")"]
  ++   concatMap extract (relsDefdIn ctx)  -- SJ 2 sept 2016: I don't think we should populate I-relations and V-relations. But why?
                                           -- HJO 4 sept 2016: I agree. This is, because I and V are not Relations, but Expressions. See the current version of FormalAmpersand. We have to fix this some day in the Haskell source too. 
  ++[ Comment " ", Comment $ "PATTERN Rules: (count="++(show.length.allRules) ctx++")"]
  ++   (concatMap extract . sortByName . allRules) ctx
  ++[ Comment " ", Comment $ "PATTERN Interfaces: (count="++(show.length.ctxifcs) ctx++")"]
  ++   (concatMap extract . sortByName . ctxifcs) ctx
  ++ metaPops fromFormalAmpersand fSpec (ctxps ctx)
  )
  where 
    extract :: MetaPopulations a => a -> [Pop]
    extract = metaPops fromFormalAmpersand fSpec
    sortByName :: Named a => [a] -> [a]
    sortByName = sortBy (comparing name)

instance MetaPopulations Pattern where
 metaPops fromFormalAmpersand fSpec pat =
    [ Comment " "
    , Comment $ " Pattern `"++name pat++"` "
    , Pop "name"    "Pattern" "PatternName"
           [(dirtyId ctx pat, (PopAlphaNumeric . name) pat)]
    , Pop "urlEncodedName" "Pattern" "EncodedName"
             [(dirtyId ctx pat, (PopAlphaNumeric . escapeNonAlphaNum . name) pat)]
    , Pop "udefrules" "Rule" "Pattern"
           [(dirtyId ctx r, dirtyId ctx pat) | r<-udefrules pat]
    , Pop "multrules" "Rule" "Pattern"
           [(dirtyId ctx r, dirtyId ctx pat) | r<-multrules pat]
    , Pop "identityRules" "Rule" "Pattern"
           [(dirtyId ctx r, dirtyId ctx pat) | r<-identityRules pat]
    , Pop "allRules" "Pattern" "Rule"
           [(dirtyId ctx pat, dirtyId ctx r) | r<-allRules pat]
    , Pop "relsDefdIn"   "Pattern" "Relation"
           [(dirtyId ctx pat,dirtyId ctx x) | x <- ptdcs pat]
    ]++ metaPops fromFormalAmpersand fSpec (ptxps pat)
  where 
    ctx = originalContext fSpec

instance MetaPopulations Purpose where
  metaPops _ fSpec purp = 
    case mMotivatedThing of
       Nothing -> []
       Just motivatedThing ->
         if explUserdefd purp -- Only supply userdefined purposes for now
         then [ Pop "purpose"  metaType "Purpose"
                [(motivatedThing, dirtyId ctx purp)]   
              , Pop "markupText" "Purpose" "MarkupText"
                [(dirtyId ctx purp, PopAlphaNumeric . aMarkup2String Markdown . explMarkup $ purp)]
              ]
         else []
   where 
     ctx = originalContext fSpec
     metaType :: String
     mMotivatedThing :: Maybe PopAtom  
     (metaType, mMotivatedThing) = 
       case explObj purp of
          ExplConceptDef x 
            -> ( "Concept"  , case filter (\cpt -> name cpt == name x) (concs ctx) of
                                [cpt]  -> Just $ dirtyId ctx cpt
                                ys -> fatal 192 $ show (length ys)++" concepts found that match `"++name x++"`")
          ExplDeclaration x
            -> ( "Relation" , case filter (x == ) (relsDefdIn ctx) of
                                [rel]  -> Just $ dirtyId ctx rel
                                ys -> fatal 196 $ show (length ys)++" relations found that match `"++show x++"`")
          ExplRule x
            -> ( "Rule"     , case filter (\rul -> name rul == x) (allRules ctx) of
                                [rul]  -> Just $ dirtyId ctx rul
                                ys -> fatal 200 $ show (length ys)++" rules found that match `"++show x++"`")
          ExplIdentityDef x
            -> ( "Identity" , case filter (\idn -> name idn == x) (identities ctx) of
                                [idn]  -> Just $ dirtyId ctx idn
                                ys -> fatal 200 $ show (length ys)++" identities found that match `"++show x++"`")
          ExplViewDef x
            -> ( "View"     , case filter (\view -> name view == x) (viewDefs ctx) of
                                [idn]  -> Just $ dirtyId ctx idn
                                ys -> fatal 200 $ show (length ys)++" views found that match `"++show x++"`")
          ExplPattern x
            -> ( "Pattern"  , case filter (\pat -> name pat == x) (patterns ctx) of
                                [pat]  -> Just $ dirtyId ctx pat
                                ys -> fatal 200 $ show (length ys)++" views found that match `"++show x++"`")
          ExplInterface x
            -> ( "Interface", case filter (\ifc -> name ifc == x) (ctxifcs ctx) of
                                [ifc]  -> Just $ dirtyId ctx ifc
                                ys -> fatal 200 $ show (length ys)++" views found that match `"++show x++"`")
          ExplContext x 
            -> ( "Context"  , case filter (\y -> name y == x) [ctx] of
                                []  -> Nothing
                                [y] -> Just $ dirtyId ctx y
                                ys  -> fatal 200 $ show (length ys)++" contexts found that match `"++show x++"`")


instance MetaPopulations A_Gen where
 metaPops _ fSpec gen@Isa{} =
  [ Pop "gens" "Context" "Isa"
          [(dirtyId ctx ctx,dirtyId ctx gen)]
  , Pop "genspc"  "Isa" "Concept"
          [(dirtyId ctx gen,dirtyId ctx (genspc gen))]
  , Pop "gengen"  "Isa" "Concept"
          [(dirtyId ctx gen,dirtyId ctx (gengen gen))]
  ]
  where 
    ctx = originalContext fSpec
 metaPops _ fSpec gen@IsE{} =
  [ Pop "gens" "Context" "IsE"
          [(dirtyId ctx ctx,dirtyId ctx gen)]
  , Pop "genspc"  "IsE" "Concept"
          [(dirtyId ctx gen,dirtyId ctx (genspc gen))]
  , Pop "gengen"  "IsE" "Concept"
          [ (dirtyId ctx gen,dirtyId ctx c) | c<-genrhs gen ]
  ]
  where 
    ctx = originalContext fSpec

instance MetaPopulations A_Concept where
 metaPops _ fSpec cpt =
   [ Comment " "
   , Comment $ " Concept `"++name cpt++"` "
   , Pop "ttype" "Concept" "TType"
             [(dirtyId ctx cpt, dirtyId ctx (cptTType fSpec cpt))] 
   , Pop "name" "Concept" "ConceptName"
             [(dirtyId ctx cpt, (PopAlphaNumeric . name) cpt)]
   , Pop "urlEncodedName" "Concept" "EncodedName"
             [(dirtyId ctx cpt, (PopAlphaNumeric . escapeNonAlphaNum . name) cpt)]
   ]++
   case cpt of
     PlainConcept{} ->
      [ Pop "context" "Concept" "Context"
             [(dirtyId ctx cpt, dirtyId ctx ctx)]
      ]
     ONE -> 
      [ ]
  where
    ctx = originalContext fSpec

instance MetaPopulations Role where
  metaPops _ fSpec rol =
      [ Pop "allRoles" "Context" "Role"
                 [(dirtyId ctx ctx, dirtyId ctx rol) ]
      , Pop "name" "Role" "RoleName"
                 [(dirtyId ctx rol, (PopAlphaNumeric . name) rol) ]
      ]
   where
    ctx = originalContext fSpec

instance MetaPopulations Interface where
  metaPops _ fSpec ifc =
      [ Pop "interfaces" "Context" "Interface"
                 [(dirtyId ctx ctx, dirtyId ctx ifc) ]
      , Pop "ifcname" "Interface" "String"
                 [(dirtyId ctx ifc, PopAlphaNumeric . ifcname $ ifc) ]
--      , Pop "ifcPos" "Interface" "Origin"
--                 [(dirtyId ctx ifc, dirtyId ctx (ifcPos ifc))]
      , Pop "allRoles" "Context" "Role"
                 [(dirtyId ctx ctx, dirtyId ctx rol) | rol <- ifcRoles ifc]
      , Pop "ifcObj" "Interface" "ObjectDef"
                 [(dirtyId ctx ifc, dirtyId ctx (ifcObj ifc)) ]
      ]
   where
    ctx = originalContext fSpec

instance MetaPopulations Atom where  --Why is this required??
  metaPops _ fSpec atm =
   [ Pop "pop" "Atom" "Concept"
          [(dirtyId ctx atm, dirtyId ctx cpt)
          |cpt <- atmRoots atm]
  -- , Pop "repr"  "Atom" "Representation"
  --        [(dirtyId ctx atm, (showValADL.atmVal) atm)]
   ]
   where
    ctx = originalContext fSpec

instance MetaPopulations Signature where
 metaPops _ fSpec sgn =
      [ Pop "src" "Signature" "Concept"
             [(dirtyId ctx sgn, dirtyId ctx (source sgn))]
      , Pop "tgt" "Signature" "Concept"
             [(dirtyId ctx sgn, dirtyId ctx (target sgn))]
      ]
  where
    ctx = originalContext fSpec

instance MetaPopulations Declaration where
 metaPops fromFormalAmpersand fSpec dcl =
   (case dcl of
     Sgn{} ->
      [ Comment " "
      , Comment $ " Relation `"++name dcl++" ["++(name.source.decsgn) dcl++" * "++(name.target.decsgn) dcl++"]"++"` "
      , Pop "context" "Relation" "Context"
             [(dirtyId ctx dcl,dirtyId ctx ctx)] 
      , Pop "name" "Relation" "RelationName"
             [(dirtyId ctx dcl, (PopAlphaNumeric . name) dcl)]
      , Pop "sign" "Relation" "Signature"
             [(dirtyId ctx dcl,dirtyId ctx (sign dcl))]
      , Pop "source" "Relation" "Concept"
             [(dirtyId ctx dcl,dirtyId ctx (source dcl))]
      , Pop "target" "Relation" "Concept"
             [(dirtyId ctx dcl,dirtyId ctx (target dcl))]
      , Pop "prop" "Relation" "Property"
             [(dirtyId ctx dcl, dirtyId ctx x) | x <- decprps dcl]  -- decprps gives the user defined properties; not the derived properties.
      , Pop "decprL" "Relation" "String"
             [(dirtyId ctx dcl,(PopAlphaNumeric . decprL) dcl)]
      , Pop "decprM" "Relation" "String"
             [(dirtyId ctx dcl,(PopAlphaNumeric . decprM) dcl)]
      , Pop "decprR" "Relation" "String"
             [(dirtyId ctx dcl,(PopAlphaNumeric . decprR) dcl)]
 --     , Pop "decmean" "Relation" "Meaning"
 --            [(dirtyId ctx dcl, (show.concatMap showP.ameaMrk.decMean) dcl)]
      ]
     Isn{} -> fatal 335 "Isn should not be populated by the meatgrinder."
{- SJ sept 2nd, 2016: I don't think we should populate the I-relation from the meatgrinder,
but I'm not sure why. -}
{- HJ july 22, 2017: This is because Isn{} must be removed from relation. It is an expression, not a relation.
      [ Comment " "
      , Comment $ " Relation `I["++name (source dcl)++"]`"
      , Pop "sign" "Relation" "Signature"
             [(dirtyId ctx dcl,dirtyId ctx (sign dcl))]
      , Pop "context" "Relation" "Context"
             [(dirtyId ctx dcl,dirtyId ctx ctx)]
      , Pop "name" "Relation" "RelationName"
             [(dirtyId ctx dcl, (PopAlphaNumeric . name) dcl)]
      , Pop "source" "Relation" "Concept"
             [(dirtyId ctx dcl,dirtyId ctx (source dcl))]
      , Pop "target" "Relation" "Concept"
             [(dirtyId ctx dcl,dirtyId ctx (target dcl))]
      ] -}

     Vs{}  -> fatal 158 "Vs should not be populated by the meatgrinder."
   )++
   metaPops fromFormalAmpersand fSpec (sign dcl)
   where
    ctx = originalContext fSpec

instance MetaPopulations A_Pair where
 metaPops _ fSpec pair =
      [ Pop "in" "Pair" "Relation"
             [(dirtyId ctx pair, dirtyId ctx (lnkDcl pair))]
      , Pop "lAtom" "Pair" "Atom"
             [(dirtyId ctx pair, dirtyId ctx (lnkLeft pair))]
      , Pop "rAtom" "Pair" "Atom"
             [(dirtyId ctx pair, dirtyId ctx (lnkRight pair))]
      ]
  where
    ctx = originalContext fSpec

instance MetaPopulations Expression where
 metaPops fromFormalAmpersand fSpec expr =
  case expr of 
    EBrk e -> metaPops fromFormalAmpersand fSpec e
    _      ->
      [ Comment $ "Expression: "++showA expr++" ("++show (sign expr)++")"
      , Pop "sign" "Expression" "Signature"
             [(dirtyId ctx expr, dirtyId ctx (sign expr))]
-- SJ20170721: The following two are redundant. They must not be populated, so I have commented them away.
--      , Pop "src" "Expression" "Concept" [Uni,Tot]
--             [(dirtyId ctx expr, dirtyId ctx (source expr))]
--      , Pop "tgt" "Expression" "Concept" [Uni,Tot]
--             [(dirtyId ctx expr, dirtyId ctx (target expr))]
      , Pop "showADL" "Expression" "ShowADL"
             [(dirtyId ctx expr, PopAlphaNumeric (showA expr))]
      ]++
      ( case skipEpsilon expr of
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
            (EPrd (l,r)) -> makeBinaryTerm CartesianProduct l r
            (EKl0 e)     -> makeUnaryTerm  KleeneStar e
            (EKl1 e)     -> makeUnaryTerm  KleenePlus e
            (EFlp e)     -> makeUnaryTerm  Converse   e
            (ECpl e)     -> makeUnaryTerm  UnaryMinus e
            (EBrk _)     -> fatal 348 "This should not happen, because EBrk has been handled before"
            (EDcD dcl)   -> [Pop "bind" "BindedRelation" "Relation"
                              [(dirtyId ctx expr,dirtyId ctx dcl)]
                            ]
            EDcI{}       -> []
            EEps{}       -> fatal 430 $ "EEps is not an expression in FormalAmpersand.\n"++
                                  "  Expression: "++showA expr++" ("++show (sign expr)++")" 
            EDcV{}       -> []
            (EMp1 v _)   -> [ Pop "singleton" "Singleton" "AtomValue"
                              [(dirtyId ctx expr,(PopAlphaNumeric . showP) v)]
                            ]
       ) 
  where
    ctx = originalContext fSpec
    makeBinaryTerm :: BinOp -> Expression -> Expression -> [Pop]
    makeBinaryTerm op lhs rhs = 
      [ Comment $ "BinOperator: "++show op
      , Comment $ "  First : "++showA lhs
      , Comment $ "  Second: "++showA rhs
      , Pop "first"  "BinaryTerm" "Expression"
             [(dirtyId ctx expr,dirtyId ctx lhs)]
      , Pop "second" "BinaryTerm" "Expression"
             [(dirtyId ctx expr,dirtyId ctx rhs)]
      , Pop "operator"  "BinaryTerm" "Operator"
             [(dirtyId ctx expr,dirtyId ctx op)]
      ]++metaPops fromFormalAmpersand fSpec lhs
       ++metaPops fromFormalAmpersand fSpec rhs
    makeUnaryTerm :: UnaryOp -> Expression -> [Pop]
    makeUnaryTerm op arg =
      [ Comment $ "UnaOperator: "++show op
      , Comment $ "  Arg : "++showA arg
      , Pop "arg" "UnaryTerm" "Expression"
             [(dirtyId ctx expr,dirtyId ctx arg)]
      , Pop "operator"  "UnaryTerm" "Operator"
             [(dirtyId ctx expr,dirtyId ctx op)]
      ]++metaPops fromFormalAmpersand fSpec arg

    -- | As long as FormalAmpersand doesn't need/know about Epsilons, 
    --   we will not inject epsilon expressions into it.
    --   So the epsilon expression must be skipped over.
    --   This goes for brackets as well.   
    skipEpsilon :: Expression -> Expression
    skipEpsilon e =
      case e of
        (ECps (EEps{}, e') ) -> skipEpsilon e'
        (ECps (e', EEps{}) ) -> skipEpsilon e'
        (EBrk e'           ) -> skipEpsilon e'
        _                    -> e

data UnaryOp = 
             KleeneStar
           | KleenePlus
           | Converse
           | UnaryMinus deriving (Eq, Show, Typeable)
instance Unique UnaryOp where
  showUnique = show

data BinOp = CartesianProduct
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
 metaPops fromFormalAmpersand fSpec rul =
      [ Comment " "
      , Comment $ " Rule `"++name rul++"` "
      , Pop "name"  "Rule" "RuleName"
             [(dirtyId ctx rul, (PopAlphaNumeric . name) rul)]
      , Pop "urlEncodedName" "Rule" "EncodedName"
             [(dirtyId ctx rul, (PopAlphaNumeric . escapeNonAlphaNum . name) rul) 
             | rul `elem` vrules fSpec --Rule must be user defined to show graphic 
             ]
      , Pop "origin"  "Rule" "Origin"
             [(dirtyId ctx rul, (PopAlphaNumeric . show . origin) rul)]
      , Pop "message"  "Rule" "Message"
             [(dirtyId ctx rul, PopAlphaNumeric (aMarkup2String ReST m)) | m <- rrmsg rul, amLang m == fsLang fSpec ]
      , Pop "formalExpression"  "Rule" "Expression"
             [(dirtyId ctx rul, dirtyId ctx (rrexp rul))]
      , Pop "meaning"  "Rule" "Meaning"
             [(dirtyId ctx rul, PopAlphaNumeric (aMarkup2String ReST m)) | m <- (maybeToList . meaning (fsLang fSpec)) rul ]
      , Pop "sign" "Rule" "Signature"
             [(dirtyId ctx rul, dirtyId ctx (sign rul))]
      , Pop "declaredthrough" "PropertyRule" "Property"
             [(dirtyId ctx rul, dirtyId ctx prp) | Just(prp,_) <- [rrdcl rul]]
      , Pop "propertyRule" "Relation" "PropertyRule"
             [(dirtyId ctx dcl, dirtyId ctx rul) | Just(_,dcl) <- [rrdcl rul]]
      ] ++ 
      metaPops fromFormalAmpersand fSpec (sign rul) ++
      metaPops fromFormalAmpersand fSpec (rrexp rul)
  where
    ctx = originalContext fSpec


instance MetaPopulations a => MetaPopulations [a] where
 metaPops fromFormalAmpersand fSpec = concatMap $ metaPops fromFormalAmpersand fSpec
 

extractFromPop :: MetaFSpec -> Pop -> Maybe P_Declaration
extractFromPop fromFormalAmpersand pop =
  case pop of 
    Comment{}                -> Nothing
    (Pop rel src tgt tuples) -> 
      Just P_Sgn { dec_nm = popName pop
                 , dec_sign = P_Sign { pSrc = PCpt src
                                     , pTgt = PCpt tgt
                                     }
                 , dec_prps   = []
                 , dec_pragma = []
                 , dec_Mean   = []
                 , dec_popu   = dclLookup
                 , pos        = Origin "Extracted by the meatgrinder of Ampersand"
                 , dec_plug   = False
                 }
     where
      dclLookup :: [PAtomPair]
      dclLookup =
         case string2AValue . unwords . words . show $ tuples of
            Checked x -> case checkAtomValues aRel x of
                          Checked _ -> x
                          Errors err -> fatal 664 $
                              "ERROR in tupels that are generated in the meatgrinder for relation\n"
                            ++"  "++rel++"["++src++"*"++tgt++"]"
                            ++intercalate (replicate 30 '=') (map showErr err)
            Errors err -> fatal 668 $ 
                              "ERROR in tupels that are generated in the meatgrinder for relation\n"
                            ++"  "++rel++"["++src++"*"++tgt++"]"
                            ++intercalate (replicate 30 '=') (map showErr err)
      checkAtomValues :: Declaration -> [PAtomPair] -> Guarded [AAtomPair]
      checkAtomValues dcl pps = sequence $ map fun pps
            where
              fun pp = mkAtomPair 
                <$> pAtomValue2aAtomValue (source dcl) (ppLeft  pp)
                <*> pAtomValue2aAtomValue (target dcl) (ppRight pp)
            
              pAtomValue2aAtomValue :: A_Concept -> PAtomValue -> Guarded AAtomValue
              pAtomValue2aAtomValue cpt pav =
                case unsafePAtomVal2AtomValue typ (Just cpt) pav of
                  Left msg -> Errors [mkIncompatibleAtomValueError pav msg]
                  Right av -> pure av
                where typ = cptTType fromFormalAmpersand cpt
            
      aRel = case [r | r <- vrels fromFormalAmpersand
                     , name r == rel
                     , name (source r) == src
                     , name (target r) == tgt
                  ] of
         []  -> fatal 673 $ "A relation populated by the meatgrinder must be defined in Formalampersand adl files.\n"
                          ++"   Violation: `"++rel++"["++src++"*"++tgt++"]`"
         [r] -> r
         rs  -> fatal 675 $ "Multiple relations that match?? Impossible!"++
                               concatMap (\r -> "\n  "++show r) rs
      string2AValue :: String -> Guarded [PAtomPair]
      string2AValue = runParser pContent "Somewhere in formalAmpersand files"
 
data Pop = Pop { popName   :: String
               , popSource :: String
               , popTarget :: String
               , popPairs  :: [(PopAtom,PopAtom)]
               }
         | Comment { comment :: String  -- Not-so-nice way to get comments in a list of populations. Since it is local to this module, it is not so bad, I guess...
                   }
data PopAtom = 
    DirtyId String  -- Any String. Quotes will be applied later
  -- from the following constructors, show will be used to show it in an ADL script
  | PopAlphaNumeric String 
  | PopInt Integer
instance Show PopAtom where
 showsPrec _ x
   = showString $ 
      case x of
        DirtyId str         -> show str
        PopAlphaNumeric str -> show str
        PopInt i            -> show i
showPop :: Pop -> String
showPop pop =
  case pop of
      Pop{} -> "POPULATION "++ popNameSignature pop++" CONTAINS"
              ++
              if null (popPairs pop)
              then "[]"
              else "\n"++indentA++"[ "++intercalate ("\n"++indentA++"; ") showContent++indentA++"]"
      Comment{} -> intercalate "\n" . map ("-- " ++) . lines . comment $ pop
    where indentA = "   "
          showContent = map showPaire (popPairs pop)
          showPaire :: Show a => (a,a) -> String
          showPaire (s,t) = "( "++show s++" , "++show t++" )"

popNameSignature :: Pop -> String
popNameSignature pop =
   case pop of
     Pop{}     -> popName pop++" ["++popSource pop++" * "++popTarget pop++"]"
     Comment{} -> fatal 503 "Must not call popName on a Comment-combinator."

showRelsFromPops :: [Pop] -> String
showRelsFromPops pops
  = intercalate "\n" [ "RELATION "++popNameSignature (head cl) 
                     | cl<-eqCl popNameSignature . filter isPop $ pops]
    where isPop Pop{}     = True
          isPop _ = False
class Unique a => HasDirtyId a where
 dirtyId :: A_Context -> a -> PopAtom
 dirtyId _ = DirtyId . camelCase . uniqueShow False
     
-- All 'things' that are relevant in the meta-environment (RAP),
-- must be an instance of HasDirtyId:
instance HasDirtyId A_Concept
instance HasDirtyId A_Gen
instance HasDirtyId Atom
instance HasDirtyId ConceptDef
instance HasDirtyId Declaration
  where dirtyId ctx r
         = case Map.lookup r declMap of
            Nothing -> fatal 546 ("no relation known as: "++showUnique r)
            Just i  -> DirtyId $ show ("Relation_"++show i)
          where
           declMap :: Map.Map Declaration Int
           declMap = Map.fromList (zip (relsDefdIn ctx++[ Isn c | c<-concs ctx]) [1..])
instance HasDirtyId Prop
instance HasDirtyId Expression
  where dirtyId ctx (EEps _ e') = dirtyId ctx e'
        dirtyId ctx (EBrk e') = dirtyId ctx e'
        dirtyId _ e = DirtyId . show $ take 150 (showA e) ++"#"++ (show . abs . hash . camelCase . uniqueShow True $ e)
instance HasDirtyId BinOp
instance HasDirtyId UnaryOp
instance HasDirtyId A_Context
instance HasDirtyId A_Pair
instance HasDirtyId Pattern
instance HasDirtyId PlugInfo
instance HasDirtyId PlugSQL
instance HasDirtyId Purpose
instance HasDirtyId Rule
instance HasDirtyId Role
instance HasDirtyId Population
instance HasDirtyId IdentityDef
instance HasDirtyId ViewDef
instance HasDirtyId Interface
instance HasDirtyId ObjectDef
instance HasDirtyId Signature
instance HasDirtyId TType
instance HasDirtyId Conjunct
instance HasDirtyId (PairView Expression)
  where dirtyId _ x = DirtyId $ show (typeOf x)++show (hash x)
instance HasDirtyId (PairViewSegment Expression)
  where dirtyId _ x = DirtyId $ show (typeOf x)++show (hash (show (hash x) ++ show (origin x)))
instance HasDirtyId Bool
  where dirtyId _ = DirtyId . map toUpper . show
--instance HasDirtyId a => HasDirtyId [a] where



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
nullContent Pop{popPairs = ps} = null ps
nullContent Comment{}          = False

type MetaFSpec = FSpec
class MetaPopulations a where
 metaPops :: MetaFSpec -> FSpec -> a -> [Pop]



