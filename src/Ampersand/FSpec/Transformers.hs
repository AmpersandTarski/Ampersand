{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.FSpec.Transformers 
  ( transformers
  , Transformer(..)
  , PopAtom(..)
  , instances
  ) where

import Ampersand.Basics hiding (singleton)
import Ampersand.Classes
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ParseTree
import Ampersand.Core.ShowAStruct
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.Motivations
import Ampersand.Misc
import Data.Hashable
import Data.List
import Data.Typeable


-- | The function that retrieves the population of
--   some relation of Formal Ampersand of a given
--   ampersand script.
data Transformer = Transformer 
      { tRel :: String  -- name of relation
      , tSrc :: String  -- name of source
      , tTrg :: String  -- name of target
      , tPairs :: [(PopAtom,PopAtom)] -- the population of this relation from the user's script.
      }

-- | This datatype reflects the nature of an atom. It is use to construct
--   the atom. 
data PopAtom = 
    DirtyId String  -- ^ Any String. must be unique of course. (TType = Object)
  | PopAlphaNumeric String -- ^ Intended to be observable by users. Not a 'dirty id'.
  | PopInt Integer 
  deriving (Eq,Ord)
instance Show PopAtom where
 showsPrec _ x
   = showString $ 
      case x of
        DirtyId str         -> show str
        PopAlphaNumeric str -> show str
        PopInt i            -> show i


toTransformer :: (String, String, String, [(PopAtom,PopAtom)]) -> Transformer 
toTransformer (rel, sCpt, tCpt, fun) = Transformer rel sCpt tCpt fun                   

-- | The list of all transformers, one for each and every relation in Formal Ampersand.
transformers :: FSpec -> [Transformer]
transformers fSpec = map toTransformer [
      ("allConjuncts"          , "Context"               , "Conjunct"
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId ctx, dirtyId conj ) 
        | ctx::A_Context <- instances fSpec
        , conj::Conjunct <- instances fSpec
        ]
      )
     ,("allRoles"              , "Context"               , "Role"    
      , [(dirtyId ctx, dirtyId rol ) 
        | ctx::A_Context <- instances fSpec
        , rol::Role <- instances fSpec
        ]
      )
     ,("allRules"              , "Context"               , "Rule"    
      , [(dirtyId ctx, dirtyId rul) 
        | ctx::A_Context <- instances fSpec
        , rul::Rule      <- allRules ctx
        ]
      )
     ,("allRules"              , "Pattern"               , "Rule"    
      , [(dirtyId pat, dirtyId rul) 
        | pat::Pattern <- instances fSpec
        , rul::Rule      <- allRules pat
        ]
      )
     ,("arg"                   , "UnaryTerm"             , "Expression"
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId expr, dirtyId x)
        | expr::Expression <- instances fSpec
        , Just x <- [arg expr]
        ]
      )
     ,("attIn"                 , "Attribute"             , "ObjectDef"
      , []  --TODO
      )
     ,("attObj"                , "Attribute"             , "ObjectDef"
      , []  --TODO
      )
     ,("bind"                  , "BindedRelation"        , "Relation"
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId expr, dirtyId x)
        | expr::Expression <- instances fSpec
        , Just x <- [bindedRel expr]
        ]
      )
     ,("changes"               , "Act"                   , "Relation"
      , []  --TODO
      )
     ,("concepts"              , "Pattern"               , "Concept" 
      , [(dirtyId pat, dirtyId cpt)
        | pat::Pattern <- instances fSpec
        , cpt <- elems (concs pat)
        ]
      )
     ,("conjunct"              , "Conjunct"              , "Expression"
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId conj, dirtyId (rc_conjunct conj))
        | conj::Conjunct <- instances fSpec
        ]
      )
     ,("context"               , "Concept"               , "Context" 
      , [(dirtyId cpt, dirtyId ctx) 
        | ctx::A_Context <- instances fSpec 
        , cpt::A_Concept <- instances fSpec
        ]
      )
     ,("context"               , "IdentityDef"           , "Context" 
      , [(dirtyId idf, dirtyId ctx) 
        | ctx::A_Context <- instances fSpec
        , idf::IdentityDef <- instances fSpec
        ]
      )
     ,("context"               , "Pattern"               , "Context" 
      , [(dirtyId pat, dirtyId ctx) 
        | ctx::A_Context <- instances fSpec
        , pat::Pattern <- instances fSpec
        ]
      )
     ,("context"               , "Population"            , "Context" 
      , [(dirtyId pop, dirtyId ctx) 
        | ctx::A_Context <- instances fSpec
        , pop::Population <- instances fSpec
        ]
      )
     ,("context"               , "Relation"              , "Context" 
      , [(dirtyId rel, dirtyId ctx) 
        | ctx::A_Context <- instances fSpec
        , rel::Relation <- instances fSpec
        ]
      )
     ,("ctxds"                 , "Relation"              , "Context" 
      , [(dirtyId rel, dirtyId ctx) 
        | ctx::A_Context <- instances fSpec
        , rel::Relation <- elems $ ctxds ctx
        ]
      )
     ,("ctxrs"                 , "Rule"                  , "Context" 
      , [(dirtyId rul, dirtyId ctx) 
        | ctx::A_Context <- instances fSpec
        , rul::Rule <- instances fSpec
        ]
      )
     ,("dbName"                , "Context"               , "DatabaseName"
      , []  --TODO
      )
     ,("declaredIn"            , "Relation"              , "Context" 
      , [(dirtyId rel, dirtyId ctx) 
        | ctx::A_Context <- instances fSpec
        , rel::Relation <- elems $ relsDefdIn ctx
        ]
      )
     ,("declaredIn"            , "Relation"              , "Pattern" 
      , [(dirtyId rel, dirtyId pat) 
        | pat::Pattern <- instances fSpec
        , rel::Relation <- elems $ relsDefdIn pat
        ]
      )
     ,("declaredthrough"       , "PropertyRule"          , "Property"
      , [(dirtyId rul, PopAlphaNumeric . show $ prop) 
        | rul::Rule <- instances fSpec
        , Just(prop,_) <- [rrdcl rul]
        ]
      )
     ,("decmean"               , "Relation"              , "Meaning" 
      , []  --TODO
      )
     ,("decprL"                , "Relation"              , "String"  
      , [(dirtyId rel, (PopAlphaNumeric . decprL) rel) 
        | rel::Relation <- instances fSpec
        ]
      )
     ,("decprM"                , "Relation"              , "String"  
      , [(dirtyId rel, (PopAlphaNumeric . decprM) rel) 
        | rel::Relation <- instances fSpec
        ]
      )
     ,("decprR"                , "Relation"              , "String"  
      , [(dirtyId rel, (PopAlphaNumeric . decprR) rel) 
        | rel::Relation <- instances fSpec
        ]
      )
     ,("default"               , "View"                  , "Concept" 
      , []  --TODO
      )
     ,("delta"                 , "Act"                   , "Pair"    
      , []  --TODO
      )
     ,("expSQL"                , "PairViewSegment"       , "MySQLQuery"
      , []  --TODO
      )
     ,("expTgt"                , "PairViewSegment"       , "Concept" 
      , []  --TODO
      )
     ,("first"                 , "BinaryTerm"            , "Expression"
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId expr, dirtyId x)
        | expr::Expression <- instances fSpec
        , Just x <- [first expr]
        ]
      )
     ,("formalExpression"      , "Rule"                  , "Expression"
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId rul, dirtyId (formalExpression rul))
        | rul::Rule <- instances fSpec
        ]
      )
     ,("gengen"                , "IsE"                   , "Concept" 
      , [ ( dirtyId ise, dirtyId cpt) 
        | ise@IsE{} <- instances fSpec
        , cpt <- genrhs ise]
      )
     ,("gengen"                , "Isa"                   , "Concept" 
      , [ ( dirtyId isa, dirtyId (gengen isa)) 
        | isa@Isa{} <- instances fSpec
        ]
      )
     ,("gens"                  , "Context"               , "IsE"     
      , [ ( dirtyId ctx, dirtyId ise) 
        | ctx::A_Context <- instances fSpec
        , ise@IsE{} <- instances fSpec
        ] 
      )
     ,("gens"                  , "Context"               , "Isa"     
      , [(dirtyId ctx, dirtyId isa) 
        | ctx::A_Context <- instances fSpec
        , isa@Isa{} <- instances fSpec
        ]
      )
     ,("genspc"                , "IsE"                   , "Concept" 
      , [ ( dirtyId ise, dirtyId (genspc ise)) 
        | ise@IsE{} <- instances fSpec
        ]
      )
     ,("genspc"                , "Isa"                   , "Concept" 
      , [ ( dirtyId isa, dirtyId (genspc isa)) 
        | isa@Isa{} <- instances fSpec
        ]
      )
     ,("getExpressionRelation" , "Expression"            , "Relation"
      , []  --TODO
      )
     ,("hasView"               , "Concept"               , "Concept" 
      , []  --TODO
      )
     ,("identityRules"         , "Rule"                  , "Context" 
      , [(dirtyId rul, dirtyId ctx) 
        | ctx::A_Context <- instances fSpec
        , rul            <- identityRules ctx
        ]
      )
     ,("identityRules"         , "Rule"                  , "Pattern" 
      , [(dirtyId rul, dirtyId pat) 
        | pat::Pattern <- instances fSpec
        , rul          <- identityRules pat
        ]
      )
     ,("ifcClass"              , "Interface"             , "String"  
      , []  --TODO
      )
     ,("ifcControls"           , "Interface"             , "Conjunct"
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId ifc, dirtyId conj) 
        | ifc::Interface <- instances fSpec
        , conj <- ifcControls ifc
        ]
      )
     ,("ifcInputs"             , "Interface"             , "Relation"
      , []  --TODO
      )
     ,("ifcObj"                , "Interface"             , "ObjectDef"
      , [(dirtyId ifc, dirtyId (ifcObj ifc)) 
        | ifc::Interface <- instances fSpec
        ]
      )
     ,("ifcOutputs"            , "Interface"             , "Relation"
      , []  --TODO
      )
     ,("ifcPos"                , "Interface"             , "Origin"  
      , [(dirtyId ifc, PopAlphaNumeric . show . ifcPos $ ifc) 
        | ifc::Interface <- instances fSpec
        ]
      )
     ,("ifcPrp"                , "Interface"             , "String"  
      , []  --TODO
      )
     ,("ifcQuads"              , "Interface"             , "Quad"    
      , []  --TODO
      )
     ,("ifcRoles"              , "Interface"             , "Role"    
      , []  --TODO
      )
     ,("in"                    , "Pair"                  , "Expression"
      , []  --TODO
      )
     ,("inQ"                   , "Quad"                  , "Act"     
      , []  --TODO
      )
     ,("inst"                  , "Object"                , "ObjectDef"
      , []  --TODO
      )
     ,("inst"                  , "Transaction"           , "Interface"
      , []  --TODO
      )
     ,("interfaces"            , "Context"               , "Interface"
      , [(dirtyId ctx,dirtyId ifc)
        | ctx::A_Context <- instances fSpec
        , ifc::Interface <- instances fSpec
        ]
      )
     ,("interfaces"            , "Role"                  , "Interface"
      , [(dirtyId rol,dirtyId ifc)
        | ifc <- instances fSpec
        , rol <- ifcRoles ifc
        ]      
      )
     ,("isa"                   , "Concept"               , "Concept" 
      , [ ( dirtyId gCpt, dirtyId (genspc ise)) 
        | ise@IsE{} <- instances fSpec
        , gCpt <- genrhs ise
        ]++
        [ ( dirtyId (genspc isa), dirtyId (genspc isa)) 
        | isa@Isa{} <- instances fSpec
        ]
      )
     ,("isaCopy"               , "Concept"               , "Concept" 
      , []  --TODO
      )
     ,("isaPlus"               , "Concept"               , "Concept" 
      , []  --TODO
      )
     ,("isaRfx"                , "Concept"               , "Concept" 
      , []  --TODO
      )
     ,("isaRfxCopy"            , "Concept"               , "Concept" 
      , []  --TODO
      )
     ,("isaRfxPlus"            , "Concept"               , "Concept" 
      , []  --TODO
      )
     ,("isaRfxStar"            , "Concept"               , "Concept" 
      , []  --TODO
      )
     ,("isaStar"               , "Concept"               , "Concept" 
      , []  --TODO
      )
     ,("language"              , "Context"               , "Language"
      , [(dirtyId ctx,(PopAlphaNumeric . show . ctxlang) ctx)
        | ctx::A_Context <- instances fSpec
        ]
      )
     ,("left"                  , "Pair"                  , "Atom"    
      , []  --TODO
      )
     ,("maintains"             , "Role"                  , "Rule"    
      , [(dirtyId rol, dirtyId rul) 
        | (rol,rul) <-  fRoleRuls fSpec 
        ]
      )
     ,("markupText"            , "Purpose"               , "MarkupText"
      , [(dirtyId purp
         ,PopAlphaNumeric . aMarkup2String Markdown . explMarkup $ purp
         ) 
        | purp::Purpose <-  instances fSpec
        ]
      )
     ,("meaning"               , "Rule"                  , "Meaning" 
      , []  --TODO
      )
     ,("message"               , "Rule"                  , "Message" 
      , []  --TODO
      )
     ,("multrules"             , "Rule"                  , "Context" 
      , [(dirtyId rul, dirtyId ctx) 
        | ctx::A_Context <- instances fSpec
        , rul            <- multrules ctx
        ]
      )
     ,("multrules"             , "Rule"                  , "Pattern" 
      , [(dirtyId rul, dirtyId pat) 
        | pat::Pattern <- instances fSpec
        , rul            <- multrules pat
        ]
      )
     ,("name"                  , "Concept"               , "ConceptName"
      , [(dirtyId cpt,(PopAlphaNumeric . name) cpt)
        | cpt::A_Concept <- instances fSpec
        ]
      )
     ,("name"                  , "Context"               , "ContextName"
      , [(dirtyId ctx,(PopAlphaNumeric . name) ctx)
        | ctx::A_Context <- instances fSpec
        ]
      )
     ,("name"                  , "Interface"             , "InterfaceName"  
      , [(dirtyId ifc,(PopAlphaNumeric . name) ifc)
        | ifc::Interface <- instances fSpec
        ]
      )
     ,("name"                 , "ObjectDef"             , "ObjectName"  
      , [(dirtyId obj, (PopAlphaNumeric . name) obj)
        | obj::ObjectDef <- instances fSpec
        ]
      )
     ,("name"                  , "Pattern"               , "PatternName"
      , [(dirtyId pat,(PopAlphaNumeric . name) pat)
        | pat::Pattern <- instances fSpec
        ]
      )
     ,("name"                  , "Relation"              , "RelationName"
      , [(dirtyId rel,(PopAlphaNumeric . name) rel)
        | rel::Relation <- instances fSpec
        ]
      )
     ,("name"                  , "Role"                  , "RoleName"
      , [(dirtyId rol,(PopAlphaNumeric . name) rol)
        | rol::Role <- instances fSpec
        ]
      )
     ,("name"                  , "Rule"                  , "RuleName"
      , [(dirtyId rul,(PopAlphaNumeric . name) rul)
        | rul::Rule <- instances fSpec
        ]
      )
     ,("objExpression"         , "ObjectDef"             , "Expression"
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId obj, dirtyId (objExpression obj))
        | obj::ObjectDef <- instances fSpec
        ]
      )
     ,("objmView"              , "ObjectDef"             , "View"    
      , []  --TODO
      )
     ,("objpos"                , "ObjectDef"             , "Origin"  
      , [(dirtyId obj, PopAlphaNumeric . show . objpos $ obj) 
        | obj::ObjectDef <- instances fSpec
        ]
      )
     ,("operator"              , "BinaryTerm"            , "Operator"
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId expr, PopAlphaNumeric . show $ op) 
        | expr::Expression <- instances fSpec
        , Just op <- [binOp expr]
        ]
      )
     ,("operator"              , "UnaryTerm"             , "Operator"
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId expr, PopAlphaNumeric . show $ op) 
        | expr::Expression <- instances fSpec
        , Just op <- [unaryOp expr]
        ]
      )
     ,("origin"                , "Rule"                  , "Origin"  
      , [(dirtyId rul, (PopAlphaNumeric . show . origin) rul)
        | rul::Rule <- instances fSpec
        ]
      )
     ,("originatesFrom"        , "Conjunct"              , "Rule"    
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId conj, dirtyId rul)
        | conj::Conjunct <- instances fSpec
        , rul <- rc_orgRules conj
        ]
      )
     ,("outQ"                  , "Quad"                  , "Act"     
      , []  --TODO
      )
     ,("pairView"              , "Rule"                  , "PairView"
      , []  --TODO
      )
     ,("prop"                  , "Relation"              , "Property"
      , [(dirtyId rel, PopAlphaNumeric . show $ prop) 
        | rel::Relation <- instances fSpec
        , prop <- elems (decprps rel)
        ]
      )
     ,("propertyRule"          , "Relation"              , "PropertyRule"
      , [(dirtyId rel, dirtyId rul) 
        | rul::Rule <- instances fSpec
        , Just(_,rel) <- [rrdcl rul]
        ]
      )
     ,("purpose"               , "Concept"               , "Purpose" 
      , [(dirtyId cpt, dirtyId purp) 
        | cpt::A_Concept <- instances fSpec
        , purp           <- purposes fSpec cpt
        ]
      )
     ,("purpose"               , "Context"               , "Purpose" 
      , [(dirtyId ctx, dirtyId purp) 
        | ctx::A_Context <- instances fSpec
        , purp           <- purposes fSpec ctx
        ]
      )
     ,("purpose"               , "Identity"              , "Purpose" 
      , [(dirtyId idn, dirtyId purp) 
        | idn::IdentityDef <- instances fSpec
        , purp           <- purposes fSpec idn
        ]
      )
     ,("purpose"               , "Interface"             , "Purpose" 
      , [(dirtyId ifc, dirtyId purp) 
        | ifc::Interface <- instances fSpec
        , purp           <- purposes fSpec ifc
        ]
      )
     ,("purpose"               , "Pattern"               , "Purpose" 
      , [(dirtyId pat, dirtyId purp) 
        | pat::Pattern <- instances fSpec
        , purp           <- purposes fSpec pat
        ]
      )
     ,("purpose"               , "Relation"              , "Purpose" 
      , [(dirtyId rel, dirtyId purp) 
        | rel::Relation <- instances fSpec
        , purp             <- purposes fSpec rel
        ]
      )
     ,("purpose"               , "Rule"                  , "Purpose" 
      , [(dirtyId rul, dirtyId purp) 
        | rul::Rule <- instances fSpec
        , purp           <- purposes fSpec rul
        ]
      )
     ,("purpose"               , "View"                  , "Purpose" 
      , [(dirtyId vw, dirtyId purp) 
        | vw::ViewDef  <- instances fSpec
        , purp         <- purposes fSpec vw
        ]
      )
     ,("relsDefdIn"            , "Pattern"               , "Relation"
      , [(dirtyId pat, dirtyId rel) 
        | pat::Pattern <- instances fSpec
        , rel            <- elems $ ptdcs pat
        ]
      )
     ,("right"                 , "Pair"                  , "Atom"    
      , []  --TODO
      )
     ,("formalExpression"      , "Rule"                  , "Expression"
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId rul, dirtyId (formalExpression rul))
        | rul::Rule <- instances fSpec
        ]
      )
     ,("second"                , "BinaryTerm"            , "Expression"
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId expr, dirtyId x)
        | expr::Expression <- instances fSpec
        , Just x <- [second expr]
        ]
      )
     ,("segment"               , "PairView"              , "PairViewSegment" 
      , []  --TODO
      )
     ,("segmentType"           , "PairViewSegment"       , "PairViewSegmentType"
      , []  --TODO
      )
     ,("sequenceNr"            , "PairViewSegment"       , "Int"     
      , []  --TODO
      )
     ,("sessAtom"              , "SESSION"               , "Atom"    
      , []  --TODO
      )
     ,("sessIfc"               , "SESSION"               , "Interface"
      , []  --TODO
      )
     ,("sessionRole"           , "SESSION"               , "Role"    
      , []  --TODO
      )
     ,("showADL"               , "Expression"            , "ShowADL" 
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId expr, PopAlphaNumeric (showA expr)) 
        | expr::Expression <- instances fSpec
        ]
      )
     ,("sign"                  , "Expression"            , "Signature"
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId expr, dirtyId (sign expr)) 
        | expr::Expression <- instances fSpec
        ]
      )
     ,("sign"                  , "Relation"              , "Signature"
      , [(dirtyId rel, dirtyId (sign rel)) 
        | rel::Relation <- instances fSpec
        ]
      )
     ,("sign"                  , "Rule"                  , "Signature"
      , [(dirtyId rul, dirtyId (sign rul)) 
        | rul::Rule <- instances fSpec
        ]
      )
     ,("singleton"             , "Singleton"             , "AtomValue"
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId expr, dirtyId x)
        | expr::Expression <- instances fSpec
        , Just x <- [singleton expr]
        ]
      )
     ,("source"                , "Relation"              , "Concept" 
      , [(dirtyId rel, dirtyId (source rel)) 
        | rel::Relation <- instances fSpec
        ]
      )
     ,("src"                   , "Signature"             , "Concept" 
      , [(dirtyId sgn, dirtyId (source sgn)) 
        | sgn::Signature <- instances fSpec
        ]
      )
     ,("srcOrTgt"              , "PairViewSegment"       , "SourceOrTarget"
      , []  --TODO
      )
     ,("target"                , "Relation"              , "Concept" 
      , [(dirtyId rel, dirtyId (target rel)) 
        | rel::Relation <- instances fSpec
        ]
      )
     ,("text"                  , "PairViewSegment"       , "String"  
      , []  --TODO
      )
     ,("tgt"                   , "Signature"             , "Concept" 
      , [(dirtyId sgn, dirtyId (target sgn)) 
        | sgn::Signature <- instances fSpec
        ]
      )
     ,("transactionObject"     , "Transaction"           , "Object"  
      , []  --TODO
      )
     ,("ttype"                 , "Concept"               , "TType"   
      , [(dirtyId cpt, (PopAlphaNumeric . show . cptTType fSpec) cpt) 
        | cpt::A_Concept <- instances fSpec
        ]
      )
     ,("udefrules"             , "Rule"                  , "Context" 
      , [(dirtyId rul, dirtyId ctx) 
        | ctx::A_Context <- instances fSpec
        , rul            <- udefrules ctx
        ]
      )
     ,("udefrules"             , "Rule"                  , "Pattern" 
      , [(dirtyId rul, dirtyId pat) 
        | pat::Pattern <- instances fSpec
        , rul            <- udefrules pat
        ]
      )
     ,("urlEncodedName"        , "Concept"               , "EncodedName"
      , [(dirtyId cpt,(PopAlphaNumeric . escapeNonAlphaNum . name) cpt)
        | cpt::A_Concept <- instances fSpec
        ]
      )
     ,("urlEncodedName"        , "Pattern"               , "EncodedName"
      , [(dirtyId pat,(PopAlphaNumeric . escapeNonAlphaNum . name) pat)
        | pat::Pattern <- instances fSpec
        ]
      )
     ,("urlEncodedName"        , "Rule"                  , "EncodedName"
      , [(dirtyId rul,(PopAlphaNumeric . escapeNonAlphaNum . name) rul)
        | rul::Rule <- instances fSpec
        ]
      )
     ,("usedIn"                , "Relation"              , "Expression"
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId rel, dirtyId expr)
        | expr::Expression <- instances fSpec
        , rel::Relation <- elems $ bindedRelationsIn expr
        ]
      )
     ,("userCpt"               , "Epsilon"                     , "Concept" 
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId expr, dirtyId x)
        | expr::Expression <- instances fSpec
        , Just (x::A_Concept) <- [userCpt expr]
        ]
      )
     ,("userSrc"               , "V"                     , "Concept" 
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId expr, dirtyId x)
        | expr::Expression <- instances fSpec
        , Just x <- [userSrc expr]
        ]
      )
     ,("userTrg"               , "V"                     , "Concept" 
      , if atlasWithoutExpressions opts then [] else
        [(dirtyId expr, dirtyId x)
        | expr::Expression <- instances fSpec
        , Just x <- [userTrg expr]
        ]
      )
     ,("uses"                  , "Context"               , "Pattern" 
      , []  --TODO
      )
     ,("valid"                 , "Concept"               , "Context" 
      , []  --TODO
      )
     ,("valid"                 , "Relation"              , "Context" 
      , []  --TODO
      )
     ,("valid"                 , "Rule"                  , "Context" 
      , []  --TODO
      )
     ,("versionInfo"           , "Context"               , "AmpersandVersion"
      , [(dirtyId ctx,PopAlphaNumeric ampersandVersionStr)
        | ctx::A_Context <- instances fSpec
        ]
      )
     ,("viewBy"                , "Concept"               , "Concept" 
      , []  --TODO
      )
     ,("viol"                  , "Interface"             , "Rule"    
      , []  --TODO
      )
     ]
   where
     opts = getOpts fSpec
-- | Within a specific context there are all kinds of things.
--   These 'things' are instances (elements / atoms) of some
--   Concept. They are the atoms of the concepts, as looked
--   upon from the Formal Ampersand viewpoint.
class Typeable a => Instances a where
--TODO: This should eventially be replaced by Set a
  instances ::  FSpec -> [a]

instance Instances A_Context where
  instances fSpec = [originalContext fSpec]
instance Instances A_Gen where
  instances fSpec = gens (originalContext fSpec)
instance Instances A_Concept where
  instances fSpec = elems . concs . originalContext $ fSpec
instance Instances Conjunct where
  instances fSpec = allConjuncts fSpec
instance Instances Relation where
  instances fSpec = elems $ relsDefdIn (originalContext fSpec)
instance Instances Expression where
  instances fSpec = elems $ allExprs fSpec
instance Instances IdentityDef where
  instances fSpec = ctxks (originalContext fSpec)
instance Instances Interface where
  instances fSpec = ctxifcs (originalContext fSpec)
instance Instances ObjectDef where
  instances fSpec = nub $
       ctxsql (originalContext fSpec)
    ++ ctxphp (originalContext fSpec)
    ++ (concatMap (objects . ifcObj) . instances $ fSpec)
    where
      objects :: ObjectDef -> [ObjectDef]
      objects obj = obj :
          case objmsub obj of
              Nothing       -> []
              Just InterfaceRef{} -> []
              Just b@Box{}  -> concatMap objects . siObjs $ b  
instance Instances Pattern where
  instances fSpec = ctxpats (originalContext fSpec)  
instance Instances Population where
  instances fSpec = ctxpopus (originalContext fSpec)
instance Instances Purpose where
  instances fSpec = explanations fSpec
instance Instances Role where
  instances fSpec = nub $ [Role "SystemAdmin"] ++ map fst (fRoles fSpec)
instance Instances Rule where
  instances fSpec = allRules (originalContext fSpec)  
instance Instances Signature where
  instances fSpec = nub $
       [sign dcl  | dcl::Relation <- instances fSpec]
    ++ [sign rul  | rul::Rule <- instances fSpec]
    ++ [sign expr | expr::Expression <- instances fSpec]
instance Instances ViewDef where
  instances fSpec = viewDefs (originalContext fSpec)  
  

-- All Concepts that are relevant in Formal Ampersand (RAP),
-- must be an instance of HasDirtyId:
class HasDirtyId a where
  dirtyId :: a -> PopAtom
  dirtyId = DirtyId . uniqueButNotTooLong . rawId
   where
    uniqueButNotTooLong :: String -> String
    uniqueButNotTooLong str =
      case splitAt safeLength str of
        (_ , []) -> str
        (prfx,_) -> prfx++"#"++show (hash str)++"#"
      where safeLength = 50 -- HJO, 20170812: Subjective value. This is based on the 
                             -- limitation that DirtyId's are stored in an sql database
                             -- in a field that is normally 255 long. We store the
                             -- prefix of the string but make sure we still have space
                             -- left over for the hash. While theoretically this is a 
                             -- crappy solution, in practice this will prove to be well 
                             -- enough. 
  rawId :: a -> String
            
instance Unique a => HasDirtyId a where
  rawId = uniqueShow True
class Instances a => HasPurpose a where 
  purposes :: FSpec -> a -> [Purpose]
  purposes fSpec a = 
    filter (isFor a) (instances fSpec)
  isFor :: a -> Purpose -> Bool 
instance HasPurpose A_Concept where
  isFor cpt purp =
    case explObj purp of
        ExplConceptDef x  -> name cpt == name x
        _                 -> False
instance HasPurpose A_Context where
  isFor ctx purp =
    case explObj purp of
        ExplContext x     -> name ctx == x
        _                 -> False
instance HasPurpose Relation where
  isFor rel purp =
    case explObj purp of
        ExplRelation x -> rel == x
        _                 -> False
instance HasPurpose IdentityDef where
  isFor idf purp =
    case explObj purp of
        ExplInterface x  -> name idf == x
        _                -> False
instance HasPurpose Interface where
  isFor ifc purp =
    case explObj purp of
        ExplInterface x  -> name ifc == x
        _                -> False
instance HasPurpose Pattern where
  isFor pat purp =
    case explObj purp of
        ExplPattern x    -> name pat == x
        _                -> False
instance HasPurpose Rule where
  isFor rul purp =
    case explObj purp of
        ExplRule x        -> name rul == x
        _                 -> False
instance HasPurpose ViewDef where
  isFor vw purp =
    case explObj purp of
        ExplViewDef x    -> name vw == x
        _                -> False

data ExprInfo = ExprInfo
   { binOp' :: Maybe BinOp
   , unaryOp' :: Maybe UnaryOp
   , bindedRel' :: Maybe Relation
   , first' :: Maybe Expression
   , second' :: Maybe Expression
   , arg' :: Maybe Expression
   , userCpt' :: Maybe A_Concept -- the concept of an Epsilon (and thus I too) expression
   , userSrc' :: Maybe A_Concept -- the source concept of a V expression
   , userTrg' :: Maybe A_Concept -- the target concept of a V expression
   , singleton' :: Maybe PAtomValue -- the value of a singleton expression
   }  
binOp :: Expression -> Maybe BinOp
binOp = binOp' . exprInfo
unaryOp :: Expression -> Maybe UnaryOp
unaryOp = unaryOp' . exprInfo
bindedRel :: Expression -> Maybe Relation
bindedRel = bindedRel' . exprInfo
first :: Expression -> Maybe Expression
first = first' . exprInfo
second :: Expression -> Maybe Expression
second = second' . exprInfo
arg :: Expression -> Maybe Expression
arg = arg' . exprInfo
userCpt :: Expression -> Maybe A_Concept
userCpt = userCpt' . exprInfo
userSrc :: Expression -> Maybe A_Concept
userSrc = userSrc' . exprInfo
userTrg :: Expression -> Maybe A_Concept
userTrg = userTrg' . exprInfo
singleton :: Expression -> Maybe PAtomValue
singleton = singleton' . exprInfo

exprInfo :: Expression -> ExprInfo
exprInfo expr =
  case expr of
    (EEqu (l,r)) -> ExprInfo
        { binOp'     = Just Equivalence
        , unaryOp'   = Nothing
        , bindedRel' = Nothing
        , first'     = Just l
        , second'    = Just r
        , arg'       = Nothing
        , userCpt'   = Nothing
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Nothing
        }
    (EInc (l,r)) -> ExprInfo
        { binOp'     = Just Inclusion
        , unaryOp'   = Nothing
        , bindedRel' = Nothing
        , first'     = Just l
        , second'    = Just r
        , arg'       = Nothing
        , userCpt'   = Nothing
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Nothing
        }
    (EIsc (l,r)) -> ExprInfo
        { binOp'     = Just Equivalence
        , unaryOp'   = Nothing
        , bindedRel' = Nothing
        , first'     = Just l
        , second'    = Just r
        , arg'       = Nothing
        , userCpt'   = Nothing
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Nothing
        }
    (EUni (l,r)) -> ExprInfo
        { binOp'     = Just Union
        , unaryOp'   = Nothing
        , bindedRel' = Nothing
        , first'     = Just l
        , second'    = Just r
        , arg'       = Nothing
        , userCpt'   = Nothing
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Nothing
        }
    (EDif (l,r)) -> ExprInfo
        { binOp'     = Just Difference
        , unaryOp'   = Nothing
        , bindedRel' = Nothing
        , first'     = Just l
        , second'    = Just r
        , arg'       = Nothing
        , userCpt'   = Nothing
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Nothing
        }
    (ELrs (l,r)) -> ExprInfo
        { binOp'     = Just LeftResidu
        , unaryOp'   = Nothing
        , bindedRel' = Nothing
        , first'     = Just l
        , second'    = Just r
        , arg'       = Nothing
        , userCpt'   = Nothing
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Nothing
        }
    (ERrs (l,r)) -> ExprInfo
        { binOp'     = Just RightResidu
        , unaryOp'   = Nothing
        , bindedRel' = Nothing
        , first'     = Just l
        , second'    = Just r
        , arg'       = Nothing
        , userCpt'   = Nothing
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Nothing
        }
    (EDia (l,r)) -> ExprInfo
        { binOp'     = Just Diamond
        , unaryOp'   = Nothing
        , bindedRel' = Nothing
        , first'     = Just l
        , second'    = Just r
        , arg'       = Nothing
        , userCpt'   = Nothing
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Nothing
        }
    (ECps (l,r)) -> ExprInfo
        { binOp'     = Just Composition
        , unaryOp'   = Nothing
        , bindedRel' = Nothing
        , first'     = Just l
        , second'    = Just r
        , arg'       = Nothing
        , userCpt'   = Nothing
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Nothing
        }
    (ERad (l,r)) -> ExprInfo
        { binOp'     = Just RelativeAddition
        , unaryOp'   = Nothing
        , bindedRel' = Nothing
        , first'     = Just l
        , second'    = Just r
        , arg'       = Nothing
        , userCpt'   = Nothing
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Nothing
        }
    (EPrd (l,r)) -> ExprInfo
        { binOp'     = Just CartesianProduct
        , unaryOp'   = Nothing
        , bindedRel' = Nothing
        , first'     = Just l
        , second'    = Just r
        , arg'       = Nothing
        , userCpt'   = Nothing
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Nothing
        }
    (EKl0 e)     -> ExprInfo
        { binOp'     = Nothing
        , unaryOp'   = Just KleeneStar
        , bindedRel' = Nothing
        , first'     = Nothing
        , second'    = Nothing
        , arg'       = Just e
        , userCpt'   = Nothing
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Nothing
        }
    (EKl1 e)     -> ExprInfo
        { binOp'     = Nothing
        , unaryOp'   = Just KleenePlus
        , bindedRel' = Nothing
        , first'     = Nothing
        , second'    = Nothing
        , arg'       = Just e
        , userCpt'   = Nothing
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Nothing
        }
    (EFlp e)     -> ExprInfo
        { binOp'     = Nothing
        , unaryOp'   = Just Converse
        , bindedRel' = Nothing
        , first'     = Nothing
        , second'    = Nothing
        , arg'       = Just e
        , userCpt'   = Nothing
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Nothing
        }
    (ECpl e)     -> ExprInfo
        { binOp'     = Nothing
        , unaryOp'   = Just UnaryMinus
        , bindedRel' = Nothing
        , first'     = Nothing
        , second'    = Nothing
        , arg'       = Just e
        , userCpt'   = Nothing
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Nothing
        }
    (EBrk e)     -> ExprInfo
        { binOp'     = binOp e
        , unaryOp'   = unaryOp e
        , bindedRel' = bindedRel e
        , first'     = first e
        , second'    = second e
        , arg'       = arg e
        , userCpt'   = userCpt e
        , userSrc'   = userSrc e
        , userTrg'   = userTrg e
        , singleton' = singleton e
        }
    (EDcD r)     -> ExprInfo
        { binOp'     = Nothing
        , unaryOp'   = Nothing
        , bindedRel' = Just r
        , first'     = Nothing
        , second'    = Nothing
        , arg'       = Nothing
        , userCpt'   = Nothing
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Nothing
        }
    (EDcI cpt)       -> ExprInfo
        { binOp'     = Nothing
        , unaryOp'   = Nothing
        , bindedRel' = Nothing
        , first'     = Nothing
        , second'    = Nothing
        , arg'       = Nothing
        , userCpt'   = Just cpt
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Nothing
        }
    (EEps cpt _)      -> ExprInfo
        { binOp'     = Nothing
        , unaryOp'   = Nothing
        , bindedRel' = Nothing
        , first'     = Nothing
        , second'    = Nothing
        , arg'       = Nothing
        , userCpt'   = Just cpt
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Nothing
        }
    (EDcV sgn)      -> ExprInfo
        { binOp'     = Nothing
        , unaryOp'   = Nothing
        , bindedRel' = Nothing
        , first'     = Nothing
        , second'    = Nothing
        , arg'       = Nothing
        , userCpt'   = Nothing
        , userSrc'   = Just (source sgn)
        , userTrg'   = Just (target sgn)
        , singleton' = Nothing
        }
    (EMp1 val _) -> ExprInfo
        { binOp'     = Nothing
        , unaryOp'   = Nothing
        , bindedRel' = Nothing
        , first'     = Nothing
        , second'    = Nothing
        , arg'       = Nothing
        , userCpt'   = Nothing
        , userSrc'   = Nothing
        , userTrg'   = Nothing
        , singleton' = Just val
        }
data UnaryOp = 
             KleeneStar
           | KleenePlus
           | Converse
           | UnaryMinus
           | Bracket deriving (Eq, Show, Typeable)
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
instance Unique (Either BinOp UnaryOp) where
  showUnique (Left  a) = showUnique a
  showUnique (Right b) = showUnique b