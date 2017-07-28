{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.FSpec.Transformers 
  ( transformers
  , Transformer(..)
  , PopAtom(..)
  ) where

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
instance Show PopAtom where
 showsPrec _ x
   = showString $ 
      case x of
        DirtyId str         -> show str
        PopAlphaNumeric str -> show str
        PopInt i            -> show i


toTransformer :: (String, String, String, [(PopAtom,PopAtom)]) -> Transformer 
toTransformer (rel, sCpt, tCpt, fun) = Transformer rel sCpt tCpt fun                   

-- | The list of all transformers, one for each and every declaration in Formal Ampersand.
transformers :: FSpec -> [Transformer]
transformers x = map toTransformer [
      ("allConjuncts"          , "Context"               , "Conjunct"
      , []  --TODO
      )
     ,("allRoles"              , "Context"               , "Role"    
      , []  --TODO
      )
     ,("allRules"              , "Context"               , "Rule"    
      , []  --TODO
      )
     ,("allRules"              , "Pattern"               , "Rule"    
      , []  --TODO
      )
     ,("arg"                   , "UnaryTerm"             , "Expression"
      , []  --TODO
      )
     ,("attIn"                 , "Attribute"             , "ObjectDef"
      , []  --TODO
      )
     ,("attObj"                , "Attribute"             , "ObjectDef"
      , []  --TODO
      )
     ,("bind"                  , "BindedRelation"        , "Relation"
      , []  --TODO
      )
     ,("changes"               , "Act"                   , "Relation"
      , []  --TODO
      )
     ,("concepts"              , "Pattern"               , "Concept" 
      , []  --TODO
      )
     ,("conjunct"              , "Conjunct"              , "Expression"
      , []  --TODO
      )
     ,("context"               , "Concept"               , "Context" 
      , []  --TODO
      )
     ,("context"               , "IdentityDef"           , "Context" 
      , []  --TODO
      )
     ,("context"               , "Pattern"               , "Context" 
      , [(dirtyId pat, dirtyId ctx) 
        | ctx::A_Context <- instances x
        , pat::Pattern   <- instances x
        ]
      )
     ,("context"               , "Population"            , "Context" 
      , []  --TODO
      )
     ,("context"               , "Relation"              , "Context" 
      , []  --TODO
      )
     ,("ctxds"                 , "Relation"              , "Context" 
      , []  --TODO
      )
     ,("ctxrs"                 , "Rule"                  , "Context" 
      , [(dirtyId rul, dirtyId ctx) 
        | ctx::A_Context <- instances x
        , rul::Rule      <- instances x
        ]
      )
     ,("dbName"                , "Context"               , "DatabaseName"
      , []  --TODO
      )
     ,("declaredIn"            , "Relation"              , "Context" 
      , []  --TODO
      )
     ,("declaredIn"            , "Relation"              , "Pattern" 
      , []  --TODO
      )
     ,("declaredthrough"       , "PropertyRule"          , "Property"
      , []  --TODO
      )
     ,("decmean"               , "Relation"              , "Meaning" 
      , []  --TODO
      )
     ,("decprL"                , "Relation"              , "String"  
      , []  --TODO
      )
     ,("decprM"                , "Relation"              , "String"  
      , []  --TODO
      )
     ,("decprR"                , "Relation"              , "String"  
      , []  --TODO
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
      , []  --TODO
      )
     ,("formalExpression"      , "Rule"                  , "Expression"
      , []  --TODO
      )
     ,("gengen"                , "IsE"                   , "Concept" 
      , []  --TODO
      )
     ,("gengen"                , "Isa"                   , "Concept" 
      , []  --TODO
      )
     ,("gens"                  , "Context"               , "IsE"     
      , []  --TODO
      )
     ,("gens"                  , "Context"               , "Isa"     
      , []  --TODO
      )
     ,("genspc"                , "IsE"                   , "Concept" 
      , []  --TODO
      )
     ,("genspc"                , "Isa"                   , "Concept" 
      , []  --TODO
      )
     ,("getExpressionRelation" , "Expression"            , "Relation"
      , []  --TODO
      )
     ,("hasView"               , "Concept"               , "Concept" 
      , []  --TODO
      )
     ,("identityRules"         , "Rule"                  , "Context" 
      , []  --TODO
      )
     ,("identityRules"         , "Rule"                  , "Pattern" 
      , []  --TODO
      )
     ,("ifcClass"              , "Interface"             , "String"  
      , []  --TODO
      )
     ,("ifcControls"           , "Interface"             , "Conjunct"
      , []  --TODO
      )
     ,("ifcInputs"             , "Interface"             , "Relation"
      , []  --TODO
      )
     ,("ifcObj"                , "Interface"             , "ObjectDef"
      , []  --TODO
      )
     ,("ifcOutputs"            , "Interface"             , "Relation"
      , []  --TODO
      )
     ,("ifcPos"                , "Interface"             , "Origin"  
      , []  --TODO
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
     ,("ifcname"               , "Interface"             , "String"  
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
      , []  --TODO
      )
     ,("interfaces"            , "Role"                  , "Interface"
      , [(dirtyId rol,dirtyId ifc)
        | ifc <- instances x
        , rol <- ifcRoles ifc
        ]      
      )
     ,("isa"                   , "Concept"               , "Concept" 
      , []  --TODO
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
        | ctx::A_Context <- instances x
        ]
      )
     ,("left"                  , "Pair"                  , "Atom"    
      , []  --TODO
      )
     ,("maintains"             , "Role"                  , "Rule"    
      , [(dirtyId rol, dirtyId rul) 
        | (rol,rul) <-  fRoleRuls x 
        ]
      )
     ,("markupText"            , "Purpose"               , "MarkupText"
      , []  --TODO
      )
     ,("meaning"               , "Rule"                  , "Meaning" 
      , []  --TODO
      )
     ,("message"               , "Rule"                  , "Message" 
      , []  --TODO
      )
     ,("multrules"             , "Rule"                  , "Context" 
      , []  --TODO
      )
     ,("multrules"             , "Rule"                  , "Pattern" 
      , []  --TODO
      )
     ,("name"                  , "Concept"               , "ConceptName"
      , []  --TODO
      )
     ,("name"                  , "Context"               , "ContextName"
      , [(dirtyId ctx,(PopAlphaNumeric . name) ctx)
        | ctx::A_Context <- instances x
        ]
      )
     ,("name"                  , "Pattern"               , "PatternName"
      , []  --TODO
      )
     ,("name"                  , "Relation"              , "RelationName"
      , []  --TODO
      )
     ,("name"                  , "Role"                  , "RoleName"
      , []  --TODO
      )
     ,("name"                  , "Rule"                  , "RuleName"
      , []  --TODO
      )
     ,("objctx"                , "ObjectDef"             , "Expression"
      , []  --TODO
      )
     ,("objmView"              , "ObjectDef"             , "View"    
      , []  --TODO
      )
     ,("objnm"                 , "ObjectDef"             , "String"  
      , []  --TODO
      )
     ,("objpos"                , "ObjectDef"             , "Origin"  
      , []  --TODO
      )
     ,("operator"              , "BinaryTerm"            , "Operator"
      , []  --TODO
      )
     ,("operator"              , "UnaryTerm"             , "Operator"
      , []  --TODO
      )
     ,("origin"                , "Rule"                  , "Origin"  
      , []  --TODO
      )
     ,("originatesFrom"        , "Conjunct"              , "Rule"    
      , []  --TODO
      )
     ,("outQ"                  , "Quad"                  , "Act"     
      , []  --TODO
      )
     ,("pairView"              , "Rule"                  , "PairView"
      , []  --TODO
      )
     ,("prop"                  , "Relation"              , "Property"
      , []  --TODO
      )
     ,("propertyRule"          , "Relation"              , "PropertyRule"
      , []  --TODO
      )
     ,("purpose"               , "Concept"               , "Purpose" 
      , []  --TODO
      )
     ,("purpose"               , "Context"               , "Purpose" 
      , []  --TODO
      )
     ,("purpose"               , "Identity"              , "Purpose" 
      , []  --TODO
      )
     ,("purpose"               , "Interface"             , "Purpose" 
      , []  --TODO
      )
     ,("purpose"               , "Pattern"               , "Purpose" 
      , []  --TODO
      )
     ,("purpose"               , "Relation"              , "Purpose" 
      , []  --TODO
      )
     ,("purpose"               , "Rule"                  , "Purpose" 
      , []  --TODO
      )
     ,("purpose"               , "View"                  , "Purpose" 
      , []  --TODO
      )
     ,("relsDefdIn"            , "Pattern"               , "Relation"
      , []  --TODO
      )
     ,("right"                 , "Pair"                  , "Atom"    
      , []  --TODO
      )
     ,("rrexp"                 , "Rule"                  , "Expression"
      , []  --TODO
      )
     ,("second"                , "BinaryTerm"            , "Expression"
      , []  --TODO
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
      , []  --TODO
      )
     ,("sign"                  , "Expression"            , "Signature"
      , []  --TODO
      )
     ,("sign"                  , "Relation"              , "Signature"
      , []  --TODO
      )
     ,("sign"                  , "Rule"                  , "Signature"
      , []  --TODO
      )
     ,("singleton"             , "Singleton"             , "AtomValue"
      , []  --TODO
      )
     ,("source"                , "Relation"              , "Concept" 
      , []  --TODO
      )
     ,("src"                   , "Signature"             , "Concept" 
      , []  --TODO
      )
     ,("srcOrTgt"              , "PairViewSegment"       , "SourceOrTarget"
      , []  --TODO
      )
     ,("target"                , "Relation"              , "Concept" 
      , []  --TODO
      )
     ,("text"                  , "PairViewSegment"       , "String"  
      , []  --TODO
      )
     ,("tgt"                   , "Signature"             , "Concept" 
      , []  --TODO
      )
     ,("transactionObject"     , "Transaction"           , "Object"  
      , []  --TODO
      )
     ,("ttype"                 , "Concept"               , "TType"   
      , []  --TODO
      )
     ,("udefrules"             , "Rule"                  , "Context" 
      , [(dirtyId rul, dirtyId ctx) 
        | ctx::A_Context <- instances x
        , rul            <- udefrules ctx
        ]
      )
     ,("udefrules"             , "Rule"                  , "Pattern" 
      , []  --TODO
      )
     ,("urlEncodedName"        , "Concept"               , "EncodedName"
      , []  --TODO
      )
     ,("urlEncodedName"        , "Pattern"               , "EncodedName"
      , []  --TODO
      )
     ,("urlEncodedName"        , "Rule"                  , "EncodedName"
      , []  --TODO
      )
     ,("usedIn"                , "Relation"              , "Expression"
      , []  --TODO
      )
     ,("userCpt"               , "I"                     , "Concept" 
      , []  --TODO
      )
     ,("userSrc"               , "V"                     , "Concept" 
      , []  --TODO
      )
     ,("userTrg"               , "V"                     , "Concept" 
      , []  --TODO
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
        | ctx::A_Context <- instances x
        ]
      )
     ,("viewBy"                , "Concept"               , "Concept" 
      , []  --TODO
      )
     ,("viol"                  , "Interface"             , "Rule"    
      , []  --TODO
      )
     ]


class Typeable a => Instances a where
  instances ::  FSpec -> [a]

instance Instances A_Context where
  instances x = [originalContext x]
instance Instances Interface where
  instances x = nub (interfaceS x ++ interfaceG x)
instance Instances Pattern where
  instances x = ctxpats (originalContext x)  
instance Instances Rule where
  instances x = ctxrs (originalContext x)  




-- All Concepts that are relevant in Formal Ampersand (RAP),
-- must be an instance of HasDirtyId:
class HasDirtyId a where
 dirtyId :: a -> PopAtom
 dirtyId = DirtyId . rawId
 rawId :: a -> String
 
instance Unique a => HasDirtyId a where
    rawId = uniqueShow True