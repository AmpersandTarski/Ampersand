{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Ampersand.FSpec.Transformers 
  ( transformersFormalAmpersand
  , transformersPrototypeContext
  , Transformer(..)
  , PopAtom(..)
  , instances
  ) where

import           Ampersand.Basics hiding (first,second)
import           Ampersand.Classes
import           Ampersand.ADL1
import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.Motivations
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set


-- | The function that retrieves the population of
--   some relation of Formal Ampersand of a given
--   ampersand script.
data Transformer = Transformer 
      { tRel :: String  -- name of relation
      , tSrc :: String  -- name of source
      , tTrg :: String  -- name of target
      , tPairs :: Set.Set (PopAtom,PopAtom)-- the population of this relation from the user's script.
      }

-- | This datatype reflects the nature of an atom. It is use to construct
--   the atom. 
data PopAtom = 
    DirtyId String         -- ^ Any String. must be:
                           --      * unique in the scope of the entire fspec
                           --      * storable in a 255 database field
  | PopAlphaNumeric String -- ^ Intended to be observable by users. Not a 'dirty id'.
  | PopInt Integer 
  deriving (Eq,Ord)
instance Show PopAtom where
 show x
   = case x of
        DirtyId str         -> show str
        PopAlphaNumeric str -> show str
        PopInt i            -> show i

dirtyId :: Unique a => a -> PopAtom
dirtyId = DirtyId . idWithType

-- Function for PrototypeContext transformers. These atoms don't need to have a type prefix
dirtyIdWithoutType :: Unique a => a -> PopAtom
dirtyIdWithoutType = DirtyId . idWithoutType

toTransformer :: (String, String, String, Set.Set (PopAtom,PopAtom) ) -> Transformer 
toTransformer (a,b,c,d) = Transformer a b c d

-- | The list of all transformers, one for each and every relation in Formal Ampersand.
transformersFormalAmpersand :: FSpec -> [Transformer]
transformersFormalAmpersand fSpec = map toTransformer [
      ("allConjuncts"          , "Context"               , "Conjunct"
      , Set.fromList $
        [(dirtyId ctx, dirtyId conj ) 
        | ctx::A_Context <- instanceList fSpec
        , conj::Conjunct <- instanceList fSpec
        ]
      )
     ,("allRoles"              , "Context"               , "Role"    
      , Set.fromList $
        [(dirtyId ctx, dirtyId rol ) 
        | ctx::A_Context <- instanceList fSpec
        , rol::Role <- instanceList fSpec
        ]
      )
     ,("allRules"              , "Context"               , "Rule"    
      , Set.fromList $
        [(dirtyId ctx, dirtyId rul) 
        | ctx::A_Context <- instanceList fSpec
        , rul::Rule      <- Set.elems $ allRules ctx
        ]
      )
     ,("allRules"              , "Pattern"               , "Rule"    
      , Set.fromList $
        [(dirtyId pat, dirtyId rul) 
        | pat::Pattern <- instanceList fSpec
        , rul::Rule      <- Set.elems $ allRules pat
        ]
      )
     ,("arg"                   , "UnaryTerm"             , "Expression"
      , Set.fromList $
        [(dirtyId expr, dirtyId x)
        | expr::Expression <- instanceList fSpec
        , Just x <- [arg expr]
        ]
      )
     ,("asMarkdown"            , "Markup"                , "Text"
      , Set.empty  --TODO
      )
     ,("attIn"                 , "Attribute"             , "ObjectDef"
      , Set.empty  --TODO
      )
     ,("attObj"                , "Attribute"             , "ObjectDef"
      , Set.empty  --TODO
      )
     ,("bind"                  , "BindedRelation"        , "Relation"
      , Set.fromList $
        [(dirtyId expr, dirtyId x)
        | expr::Expression <- instanceList fSpec
        , Just x <- [bindedRel expr]
        ]
      )
     ,("changes"               , "Act"                   , "Relation"
      , Set.empty  --TODO
      )
     ,("concepts"              , "Pattern"               , "Concept" 
      , Set.fromList $
        [(dirtyId pat, dirtyId cpt)
        | pat::Pattern <- instanceList fSpec
        , cpt <- Set.elems $ concs pat
        ]
      )
     ,("conjunct"              , "Conjunct"              , "Expression"
      , Set.fromList $
        [(dirtyId conj, dirtyId (rc_conjunct conj))
        | conj::Conjunct <- instanceList fSpec
        ]
      )
     ,("context"               , "Concept"               , "Context" 
      , Set.fromList $
        [(dirtyId cpt, dirtyId ctx) 
        | ctx::A_Context <- instanceList fSpec 
        , cpt::A_Concept <- instanceList fSpec
        ]
      )
     ,("context"               , "IdentityDef"           , "Context" 
      , Set.fromList $
        [(dirtyId idf, dirtyId ctx) 
        | ctx::A_Context <- instanceList fSpec
        , idf::IdentityDef <- instanceList fSpec
        ]
      )
     ,("context"               , "Pattern"               , "Context" 
      , Set.fromList $
        [(dirtyId pat, dirtyId ctx) 
        | ctx::A_Context <- instanceList fSpec
        , pat::Pattern <- instanceList fSpec
        ]
      )
     ,("context"               , "Population"            , "Context" 
      , Set.fromList $
        [(dirtyId pop, dirtyId ctx) 
        | ctx::A_Context <- instanceList fSpec
        , pop::Population <- instanceList fSpec
        ]
      )
     ,("context"               , "Relation"              , "Context" 
      , Set.fromList $
        [(dirtyId rel, dirtyId ctx) 
        | ctx::A_Context <- instanceList fSpec
        , rel::Relation <- instanceList fSpec
        ]
      )
     ,("ctxds"                 , "Relation"              , "Context" 
      , Set.fromList $
        [(dirtyId rel, dirtyId ctx) 
        | ctx::A_Context <- instanceList fSpec
        , rel::Relation <- Set.elems $ ctxds ctx
        ]
      )
     ,("ctxrs"                 , "Rule"                  , "Context" 
      , Set.fromList $
        [(dirtyId rul, dirtyId ctx) 
        | ctx::A_Context <- instanceList fSpec
        , rul::Rule <- instanceList fSpec
        ]
      )
     ,("dbName"                , "Context"               , "DatabaseName"
      , Set.empty  --TODO
      )
     ,("declaredIn"            , "Relation"              , "Context" 
      , Set.fromList $
        [(dirtyId rel, dirtyId ctx) 
        | ctx::A_Context <- instanceList fSpec
        , rel::Relation <- Set.elems $ relsDefdIn ctx
        ]
      )
     ,("declaredIn"            , "Relation"              , "Pattern" 
      , Set.fromList $
        [(dirtyId rel, dirtyId pat) 
        | pat::Pattern <- instanceList fSpec
        , rel::Relation <- Set.elems $ relsDefdIn pat
        ]
      )
     ,("declaredthrough"       , "PropertyRule"          , "Property"
      , Set.fromList $
        [(dirtyId rul, PopAlphaNumeric . show $ prop) 
        | rul::Rule <- instanceList fSpec
        , Just(prop,_) <- [rrdcl rul]
        ]
      )
     ,("decMean"               , "Relation"              , "Meaning" 
      , Set.empty  --TODO
      )
     ,("decprL"                , "Relation"              , "String"  
      , Set.fromList $
        [(dirtyId rel, (PopAlphaNumeric . decprL) rel) 
        | rel::Relation <- instanceList fSpec
        ]
      )
     ,("decprM"                , "Relation"              , "String"  
      , Set.fromList $
        [(dirtyId rel, (PopAlphaNumeric . decprM) rel) 
        | rel::Relation <- instanceList fSpec
        ]
      )
     ,("decprR"                , "Relation"              , "String"  
      , Set.fromList $
        [(dirtyId rel, (PopAlphaNumeric . decprR) rel) 
        | rel::Relation <- instanceList fSpec
        ]
      )
     ,("default"               , "View"                  , "Concept" 
      , Set.empty  --TODO
      )
     ,("delta"                 , "Act"                   , "Pair"    
      , Set.empty  --TODO
      )
     ,("expSQL"                , "PairViewSegment"       , "MySQLQuery"
      , Set.empty  --TODO
      )
     ,("expTgt"                , "PairViewSegment"       , "Concept" 
      , Set.empty  --TODO
      )
     ,("first"                 , "BinaryTerm"            , "Expression"
      , Set.fromList $
        [(dirtyId expr, dirtyId x)
        | expr::Expression <- instanceList fSpec
        , Just x <- [first expr]
        ]
      )
     ,("formalExpression"      , "Rule"                  , "Expression"
      , Set.fromList $
        [(dirtyId rul, dirtyId (formalExpression rul))
        | rul::Rule <- instanceList fSpec
        ]
      )
     ,("gengen"                , "IsE"                   , "Concept" 
      , Set.fromList $
        [ ( dirtyId ise, dirtyId cpt) 
        | ise@IsE{} <- instanceList fSpec
        , cpt <- genrhs ise]
      )
     ,("gengen"                , "Isa"                   , "Concept" 
      , Set.fromList $
        [ ( dirtyId isa, dirtyId (gengen isa)) 
        | isa@Isa{} <- instanceList fSpec
        ]
      )
     ,("gens"                  , "Context"               , "IsE"     
      , Set.fromList $
        [ ( dirtyId ctx, dirtyId ise) 
        | ctx::A_Context <- instanceList fSpec
        , ise@IsE{} <- instanceList fSpec
        ] 
      )
     ,("gens"                  , "Context"               , "Isa"     
      , Set.fromList $
        [(dirtyId ctx, dirtyId isa) 
        | ctx::A_Context <- instanceList fSpec
        , isa@Isa{} <- instanceList fSpec
        ]
      )
     ,("genspc"                , "IsE"                   , "Concept" 
      , Set.fromList $
        [ ( dirtyId ise, dirtyId (genspc ise)) 
        | ise@IsE{} <- instanceList fSpec
        ]
      )
     ,("genspc"                , "Isa"                   , "Concept" 
      , Set.fromList $
        [ ( dirtyId isa, dirtyId (genspc isa)) 
        | isa@Isa{} <- instanceList fSpec
        ]
      )
     ,("getExpressionRelation" , "Expression"            , "Relation"
      , Set.empty  --TODO
      )
     ,("hasView"               , "Concept"               , "Concept" 
      , Set.empty  --TODO
      )
     ,("identityRules"         , "Rule"                  , "Context" 
      , Set.fromList $
        [(dirtyId rul, dirtyId ctx) 
        | ctx::A_Context <- instanceList fSpec
        , rul            <- Set.elems $ identityRules ctx
        ]
      )
     ,("identityRules"         , "Rule"                  , "Pattern" 
      , Set.fromList $
        [(dirtyId rul, dirtyId pat) 
        | pat::Pattern <- instanceList fSpec
        , rul          <- Set.elems $ identityRules pat
        ]
      )
     ,("ifcClass"              , "Interface"             , "String"  
      , Set.empty  --TODO
      )
     ,("ifcControls"           , "Interface"             , "Conjunct"
      , Set.fromList $
        [(dirtyId ifc, dirtyId conj) 
        | ifc::Interface <- instanceList fSpec
        , conj <- ifcControls ifc
        ]
      )
     ,("ifcInputs"             , "Interface"             , "Relation"
      , Set.empty  --TODO
      )
     ,("ifcObj"                , "Interface"             , "ObjectDef"
      , Set.fromList $
        [(dirtyId ifc, dirtyId (ifcObj ifc)) 
        | ifc::Interface <- instanceList fSpec
        ]
      )
     ,("ifcOutputs"            , "Interface"             , "Relation"
      , Set.empty  --TODO
      )
     ,("ifcPos"                , "Interface"             , "Origin"  
      , Set.fromList $
        [(dirtyId ifc, PopAlphaNumeric . show . ifcPos $ ifc) 
        | ifc::Interface <- instanceList fSpec
        ]
      )
     ,("ifcPrp"                , "Interface"             , "String"  
      , Set.empty  --TODO
      )
     ,("ifcQuads"              , "Interface"             , "Quad"    
      , Set.empty  --TODO
      )
     ,("ifcRoles"              , "Interface"             , "Role"    
      , Set.empty  --TODO
      )
     ,("in"                    , "Pair"                  , "Expression"
      , Set.empty  --TODO
      )
     ,("inQ"                   , "Quad"                  , "Act"     
      , Set.empty  --TODO
      )
     ,("inst"                  , "Object"                , "ObjectDef"
      , Set.empty  --TODO
      )
     ,("inst"                  , "Transaction"           , "Interface"
      , Set.empty  --TODO
      )
     ,("interfaces"            , "Context"               , "Interface"
      , Set.fromList $
        [(dirtyId ctx,dirtyId ifc)
        | ctx::A_Context <- instanceList fSpec
        , ifc::Interface <- instanceList fSpec
        ]
      )
     ,("interfaces"            , "Role"                  , "Interface"
      , Set.fromList $
        [(dirtyId rol,dirtyId ifc)
        | ifc <- instanceList fSpec
        , rol <- ifcRoles ifc
        ]      
      )
     ,("isa"                   , "Concept"               , "Concept" 
      , Set.fromList 
        [ ( dirtyId gCpt, dirtyId (genspc ise)) 
        | ise@IsE{} <- instanceList fSpec
        , gCpt <- genrhs ise
        ] `Set.union`
        Set.fromList
        [ ( dirtyId (genspc isa), dirtyId (genspc isa)) 
        | isa@Isa{} <- instanceList fSpec
        ]
      )
     ,("isaCopy"               , "Concept"               , "Concept" 
      , Set.empty  --TODO
      )
     ,("isaPlus"               , "Concept"               , "Concept" 
      , Set.empty  --TODO
      )
     ,("isaRfx"                , "Concept"               , "Concept" 
      , Set.empty  --TODO
      )
     ,("isaRfxCopy"            , "Concept"               , "Concept" 
      , Set.empty  --TODO
      )
     ,("isaRfxPlus"            , "Concept"               , "Concept" 
      , Set.empty  --TODO
      )
     ,("isaRfxStar"            , "Concept"               , "Concept" 
      , Set.empty  --TODO
      )
     ,("isaStar"               , "Concept"               , "Concept" 
      , Set.empty  --TODO
      )
     ,("language"              , "Context"               , "Language"
      , Set.fromList
        [(dirtyId ctx,(PopAlphaNumeric . show . ctxlang) ctx)
        | ctx::A_Context <- instanceList fSpec
        ]
      )
     ,("language"              , "Markup"               , "Language"
      , Set.fromList
        [(dirtyId mrk,(PopAlphaNumeric . show . amLang) mrk)
        | mrk::Markup <- instanceList fSpec
        ]
      )
     ,("left"                  , "Pair"                  , "Atom"    
      , Set.empty  --TODO
      )
     ,("maintains"             , "Role"                  , "Rule"    
      , Set.fromList
        [(dirtyId rol, dirtyId rul) 
        | (rol,rul) <-  fRoleRuls fSpec 
        ]
      )
     ,("markup"            , "Meaning"               , "Markup"
      , Set.fromList
        [ (dirtyId mean, dirtyId . ameaMrk $ mean) 
        | mean::Meaning <- Set.toList . meaningInstances $ fSpec
        ]
      )
     ,("markup"            , "Purpose"               , "Markup"
      , Set.fromList
        [(dirtyId purp, dirtyId . explMarkup $ purp) 
        | purp::Purpose <- Set.toList . purposeInstances $ fSpec
        ]
      )
     ,("meaning"               , "Rule"                  , "Meaning" 
      , Set.empty  --TODO
      )
     ,("message"               , "Rule"                  , "Message" 
      , Set.empty  --TODO
      )
     ,("multrules"             , "Rule"                  , "Context" 
      , Set.fromList
        [(dirtyId rul, dirtyId ctx) 
        | ctx::A_Context <- instanceList fSpec
        , rul            <- Set.elems $ multrules ctx
        ]
      )
     ,("multrules"             , "Rule"                  , "Pattern" 
      , Set.fromList
        [(dirtyId rul, dirtyId pat) 
        | pat::Pattern <- instanceList fSpec
        , rul            <- Set.elems $ multrules pat
        ]
      )
     ,("name"                  , "Concept"               , "ConceptName"
      , Set.fromList
        [(dirtyId cpt,(PopAlphaNumeric . name) cpt)
        | cpt::A_Concept <- instanceList fSpec
        ]
      )
     ,("name"                  , "Context"               , "ContextName"
      , Set.fromList
        [(dirtyId ctx,(PopAlphaNumeric . name) ctx)
        | ctx::A_Context <- instanceList fSpec
        ]
      )
     ,("name"                  , "Interface"             , "InterfaceName"  
      , Set.fromList
        [(dirtyId ifc,(PopAlphaNumeric . name) ifc)
        | ifc::Interface <- instanceList fSpec
        ]
      )
     ,("name"                 , "ObjectDef"             , "ObjectName"  
      , Set.fromList
        [(dirtyId obj, (PopAlphaNumeric . name) obj)
        | obj::ObjectDef <- instanceList fSpec
        ]
      )
     ,("name"                  , "Pattern"               , "PatternName"
      , Set.fromList
        [(dirtyId pat,(PopAlphaNumeric . name) pat)
        | pat::Pattern <- instanceList fSpec
        ]
      )
     ,("name"                  , "Relation"              , "RelationName"
      , Set.fromList
        [(dirtyId rel,(PopAlphaNumeric . name) rel)
        | rel::Relation <- instanceList fSpec
        ]
      )
     ,("name"                  , "Role"                  , "RoleName"
      , Set.fromList
        [(dirtyId rol,(PopAlphaNumeric . name) rol)
        | rol::Role <- instanceList fSpec
        ]
      )
     ,("name"                  , "Rule"                  , "RuleName"
      , Set.fromList
        [(dirtyId rul,(PopAlphaNumeric . name) rul)
        | rul::Rule <- instanceList fSpec
        ]
      )
     ,("objExpression"         , "ObjectDef"             , "Expression"
      , Set.fromList
        [(dirtyId obj, dirtyId (objExpression obj))
        | obj::ObjectDef <- instanceList fSpec
        ]
      )
     ,("objmView"              , "ObjectDef"             , "View"    
      , Set.empty  --TODO
      )
     ,("objpos"                , "ObjectDef"             , "Origin"  
      , Set.fromList
        [(dirtyId obj, PopAlphaNumeric . show . origin $ obj) 
        | obj::ObjectDef <- instanceList fSpec
        ]
      )
     ,("operator"              , "BinaryTerm"            , "Operator"
      , Set.fromList
        [(dirtyId expr, PopAlphaNumeric . show $ op) 
        | expr::Expression <- instanceList fSpec
        , Just op <- [binOp expr]
        ]
      )
     ,("operator"              , "UnaryTerm"             , "Operator"
      , Set.fromList
        [(dirtyId expr, PopAlphaNumeric . show $ op) 
        | expr::Expression <- instanceList fSpec
        , Just op <- [unaryOp expr]
        ]
      )
     ,("origin"                , "Rule"                  , "Origin"  
      , Set.fromList
        [(dirtyId rul, (PopAlphaNumeric . show . origin) rul)
        | rul::Rule <- instanceList fSpec
        ]
      )
     ,("originatesFrom"        , "Conjunct"              , "Rule"    
      , Set.fromList
        [(dirtyId conj, dirtyId rul)
        | conj::Conjunct <- instanceList fSpec
        , rul <- NE.toList $ rc_orgRules conj
        ]
      )
     ,("outQ"                  , "Quad"                  , "Act"     
      , Set.empty  --TODO
      )
     ,("pairView"              , "Rule"                  , "PairView"
      , Set.empty  --TODO
      )
     ,("prop"                  , "Relation"              , "Property"
      , Set.fromList
        [(dirtyId rel, PopAlphaNumeric . show $ prop) 
        | rel::Relation <- instanceList fSpec
        , prop <- Set.elems $ decprps rel
        ]
      )
     ,("propertyRule"          , "Relation"              , "PropertyRule"
      , Set.fromList
        [(dirtyId rel, dirtyId rul) 
        | rul::Rule <- instanceList fSpec
        , Just(_,rel) <- [rrdcl rul]
        ]
      )
     ,("purpose"               , "Concept"               , "Purpose" 
      , Set.fromList
        [(dirtyId cpt, dirtyId purp) 
        | cpt::A_Concept <- instanceList fSpec
        , purp           <- purposes fSpec cpt
        ]
      )
     ,("purpose"               , "Context"               , "Purpose" 
      , Set.fromList
        [(dirtyId ctx, dirtyId purp) 
        | ctx::A_Context <- instanceList fSpec
        , purp           <- purposes fSpec ctx
        ]
      )
     ,("purpose"               , "Identity"              , "Purpose" 
      , Set.fromList
        [(dirtyId idn, dirtyId purp) 
        | idn::IdentityDef <- instanceList fSpec
        , purp           <- purposes fSpec idn
        ]
      )
     ,("purpose"               , "Interface"             , "Purpose" 
      , Set.fromList
        [(dirtyId ifc, dirtyId purp) 
        | ifc::Interface <- instanceList fSpec
        , purp           <- purposes fSpec ifc
        ]
      )
     ,("purpose"               , "Pattern"               , "Purpose" 
      , Set.fromList
        [(dirtyId pat, dirtyId purp) 
        | pat::Pattern <- instanceList fSpec
        , purp           <- purposes fSpec pat
        ]
      )
     ,("purpose"               , "Relation"              , "Purpose" 
      , Set.fromList
        [(dirtyId rel, dirtyId purp) 
        | rel::Relation <- instanceList fSpec
        , purp             <- purposes fSpec rel
        ]
      )
     ,("purpose"               , "Rule"                  , "Purpose" 
      , Set.fromList
        [(dirtyId rul, dirtyId purp) 
        | rul::Rule <- instanceList fSpec
        , purp           <- purposes fSpec rul
        ]
      )
     ,("purpose"               , "View"                  , "Purpose" 
      , Set.fromList
        [(dirtyId vw, dirtyId purp) 
        | vw::ViewDef  <- instanceList fSpec
        , purp         <- purposes fSpec vw
        ]
      )
     ,("relsDefdIn"            , "Pattern"               , "Relation"
      , Set.fromList
        [(dirtyId pat, dirtyId rel) 
        | pat::Pattern <- instanceList fSpec
        , rel            <- Set.elems $ relsDefdIn pat
        ]
      )
     ,("right"                 , "Pair"                  , "Atom"    
      , Set.empty  --TODO
      )
     ,("formalExpression"      , "Rule"                  , "Expression"
      , Set.fromList
        [(dirtyId rul, dirtyId (formalExpression rul))
        | rul::Rule <- instanceList fSpec
        ]
      )
     ,("second"                , "BinaryTerm"            , "Expression"
      , Set.fromList
        [(dirtyId expr, dirtyId x)
        | expr::Expression <- instanceList fSpec
        , Just x <- [second expr]
        ]
      )
     ,("segment"               , "PairView"              , "PairViewSegment" 
      , Set.empty  --TODO
      )
     ,("segmentType"           , "PairViewSegment"       , "PairViewSegmentType"
      , Set.empty  --TODO
      )
     ,("sequenceNr"            , "PairViewSegment"       , "Int"     
      , Set.empty  --TODO
      )
     ,("sessAtom"              , "SESSION"               , "Atom"    
      , Set.empty  --TODO
      )
     ,("sessIfc"               , "SESSION"               , "Interface"
      , Set.empty  --TODO
      )
     ,("sessionRole"           , "SESSION"               , "Role"    
      , Set.empty  --TODO
      )
     ,("showADL"               , "Expression"            , "ShowADL" 
      , Set.fromList
        [(dirtyId expr, PopAlphaNumeric (showA expr)) 
        | expr::Expression <- instanceList fSpec
        ]
      )
     ,("sign"                  , "Expression"            , "Signature"
      , Set.fromList
        [(dirtyId expr, dirtyId (sign expr)) 
        | expr::Expression <- instanceList fSpec
        ]
      )
     ,("sign"                  , "Relation"              , "Signature"
      , Set.fromList
        [(dirtyId rel, dirtyId (sign rel)) 
        | rel::Relation <- instanceList fSpec
        ]
      )
     ,("singleton"             , "Singleton"             , "AtomValue"
      , Set.fromList
        [(dirtyId expr, dirtyId x)
        | expr::Expression <- instanceList fSpec
        , Just x <- [singleton expr]
        ]
      )
     ,("source"                , "Relation"              , "Concept" 
      , Set.fromList
        [(dirtyId rel, dirtyId (source rel)) 
        | rel::Relation <- instanceList fSpec
        ]
      )
     ,("src"                   , "Signature"             , "Concept" 
      , Set.fromList
        [(dirtyId sgn, dirtyId (source sgn)) 
        | sgn::Signature <- instanceList fSpec
        ]
      )
     ,("srcOrTgt"              , "PairViewSegment"       , "SourceOrTarget"
      , Set.empty  --TODO
      )
     ,("target"                , "Relation"              , "Concept" 
      , Set.fromList
        [(dirtyId rel, dirtyId (target rel)) 
        | rel::Relation <- instanceList fSpec
        ]
      )
     ,("text"                  , "PairViewSegment"       , "String"  
      , Set.empty  --TODO
      )
     ,("tgt"                   , "Signature"             , "Concept" 
      , Set.fromList
        [(dirtyId sgn, dirtyId (target sgn)) 
        | sgn::Signature <- instanceList fSpec
        ]
      )
     ,("transactionObject"     , "Transaction"           , "Object"  
      , Set.empty  --TODO
      )
     ,("ttype"                 , "Concept"               , "TType"   
      , Set.fromList
        [(dirtyId cpt, (PopAlphaNumeric . show . cptTType fSpec) cpt) 
        | cpt::A_Concept <- instanceList fSpec
        ]
      )
     ,("udefrules"             , "Rule"                  , "Context" 
      , Set.fromList
        [(dirtyId rul, dirtyId ctx) 
        | ctx::A_Context <- instanceList fSpec
        , rul            <- Set.elems $ udefrules ctx
        ]
      )
     ,("udefrules"             , "Rule"                  , "Pattern" 
      , Set.fromList
        [(dirtyId rul, dirtyId pat) 
        | pat::Pattern <- instanceList fSpec
        , rul            <- Set.elems $ udefrules pat
        ]
      )
     ,("urlEncodedName"        , "Concept"               , "EncodedName"
      , Set.fromList
        [(dirtyId cpt,(PopAlphaNumeric . escapeNonAlphaNum . name) cpt)
        | cpt::A_Concept <- instanceList fSpec
        ]
      )
     ,("urlEncodedName"        , "Pattern"               , "EncodedName"
      , Set.fromList
        [(dirtyId pat,(PopAlphaNumeric . escapeNonAlphaNum . name) pat)
        | pat::Pattern <- instanceList fSpec
        ]
      )
     ,("urlEncodedName"        , "Rule"                  , "EncodedName"
      , Set.fromList
        [(dirtyId rul,(PopAlphaNumeric . escapeNonAlphaNum . name) rul)
        | rul::Rule <- instanceList fSpec
        ]
      )
     ,("usedIn"                , "Relation"              , "Expression"
      , Set.fromList
        [(dirtyId rel, dirtyId expr)
        | expr::Expression <- instanceList fSpec
        , rel::Relation <- Set.elems $ bindedRelationsIn expr
        ]
      )
     ,("userCpt"               , "Epsilon"                     , "Concept" 
      , Set.fromList
        [(dirtyId expr, dirtyId x)
        | expr::Expression <- instanceList fSpec
        , Just (x::A_Concept) <- [userCpt expr]
        ]
      )
     ,("userSrc"               , "V"                     , "Concept" 
      , Set.fromList
        [(dirtyId expr, dirtyId x)
        | expr::Expression <- instanceList fSpec
        , Just x <- [userSrc expr]
        ]
      )
     ,("userTrg"               , "V"                     , "Concept" 
      , Set.fromList
        [(dirtyId expr, dirtyId x)
        | expr::Expression <- instanceList fSpec
        , Just x <- [userTrg expr]
        ]
      )
     ,("uses"                  , "Context"               , "Pattern" 
      , Set.empty  --TODO
      )
     ,("valid"                 , "Concept"               , "Context" 
      , Set.empty  --TODO
      )
     ,("valid"                 , "Relation"              , "Context" 
      , Set.empty  --TODO
      )
     ,("valid"                 , "Rule"                  , "Context" 
      , Set.empty  --TODO
      )
     ,("versionInfo"           , "Context"               , "AmpersandVersion"
      , Set.fromList
        [(dirtyId ctx,PopAlphaNumeric ampersandVersionStr)
        | ctx::A_Context <- instanceList fSpec
        ]
      )
     ,("viewBy"                , "Concept"               , "Concept" 
      , Set.empty  --TODO
      )
     ,("viol"                  , "Interface"             , "Rule"    
      , Set.empty  --TODO
      )
     ]
   


 
-- | The list of all transformers, one for each and every relation in PrototypeContext.
transformersPrototypeContext :: FSpec -> [Transformer]
transformersPrototypeContext fSpec = map toTransformer [
      ("ifc"                   , "PF_NavMenuItem"        , "PF_Interface"
      , Set.empty
      )
    , ("isAPI"                 , "PF_Interface"          , "PF_Interface"
      , Set.fromList $
        [(dirtyIdWithoutType ifc, dirtyIdWithoutType ifc)
        | ifc::Interface <- instanceList fSpec
        , ifcIsAPI ifc
        ]
      )
    , ("isPartOf"              , "PF_NavMenuItem"        , "PF_NavMenu"
      , Set.empty
      )
    , ("isPublic"              , "PF_Interface"          , "PF_Interface"
      , Set.fromList $
        [(dirtyIdWithoutType ifc, dirtyIdWithoutType ifc)
        | ifc::Interface <- instanceList fSpec
        , null (ifcRoles ifc)
        ]
      )
    , ("isSubItemOf"           , "PF_NavMenuItem"        , "PF_NavMenuItem"
      , Set.empty
      )
    , ("isVisible"             , "PF_NavMenuItem"        , "PF_NavMenuItem"
      , Set.empty
      )
    , ("label"                 , "PF_Interface"          , "PF_Label"    
      , Set.fromList $
        [(dirtyIdWithoutType ifc, PopAlphaNumeric . name $ ifc)
        | ifc::Interface <- instanceList fSpec
        ]
      )
    , ("label"                 , "PF_NavMenuItem"        , "PF_Label"
      , Set.empty
      )
    , ("label"                 , "Role"               , "PF_Label"
      , Set.fromList $
        [ (dirtyIdWithoutType role, PopAlphaNumeric . name $ role)
        | role::Role <- instanceList fSpec
        ]
      )
    , ("lastAccess"            , "SESSION"               , "DateTime"
      , Set.empty
      )
    , ("pf_ifcRoles"           , "PF_Interface"          , "Role"
      , Set.fromList $
        [(dirtyIdWithoutType ifc , dirtyIdWithoutType role)
        | ifc::Interface <- instanceList fSpec
        , role <- ifcRoles ifc
        ]
      )
    , ("pf_navItemRoles"       , "PF_NavMenuItem"        , "Role"
      , Set.empty
      )
    , ("seqNr"                 , "PF_NavMenuItem"        , "PF_SeqNr"
      , Set.empty
      )
    , ("sessionActiveRoles"    , "SESSION"               , "Role"
      , Set.empty
      )
    , ("sessionAllowedRoles"   , "SESSION"               , "Role"
      , Set.empty
      )
    , ("url"                   , "PF_NavMenuItem"        , "PF_URL"
      , Set.empty
      )
    ]



     -- | Within a specific context there are all kinds of things.
--   These 'things' are instances (elements / atoms) of some
--   Concept. They are the atoms of the concepts, as looked
--   upon from the Formal Ampersand viewpoint.
class Typeable a => Instances a where
  instances ::  FSpec -> Set.Set a
  instanceList :: FSpec -> [a]
  instanceList = Set.toList . instances
  {-# MINIMAL instances #-} 

-- --WARNING: Beware of loops!
-- To prevent loops in the definition of instances, it is considered bad
-- to use the `instances` function while defining it. 
-- For this reason, some helper functions are defined here:
expressionInstances :: FSpec -> Set.Set Expression
expressionInstances = allExprs
interfaceInstances :: FSpec -> Set.Set Interface
interfaceInstances = Set.fromList . ctxifcs . originalContext
meaningInstances :: FSpec -> Set.Set Meaning
meaningInstances fSpec = (Set.fromList . concat . map meanings . Set.toList . relationInstances $ fSpec)
                          `Set.union`
                         (Set.fromList . concat . map meanings . Set.toList . ruleInstances $ fSpec)
purposeInstances :: FSpec -> Set.Set Purpose
purposeInstances fSpec = Set.fromList . fSexpls $ fSpec
relationInstances :: FSpec -> Set.Set Relation
relationInstances = relsDefdIn . originalContext
ruleInstances :: FSpec -> Set.Set Rule
ruleInstances = allRules . originalContext

instance Instances A_Context where
  instances = Set.singleton . originalContext
instance Instances AClassify where
  instances = Set.fromList . gens . originalContext
instance Instances A_Concept where
  instances = concs . originalContext
instance Instances ConceptDef where
  instances = Set.fromList . ctxcds . originalContext
instance Instances Conjunct where
  instances = Set.fromList . allConjuncts
instance Instances Expression where
  instances = expressionInstances
instance Instances IdentityDef where
  instances = Set.fromList . ctxks . originalContext
instance Instances Interface where
  instances = interfaceInstances
--instance Instances Meaning where
--  instances = meaningInstances
instance Instances Markup where
  instances fSpec = (Set.fromList . map explMarkup . Set.toList . purposeInstances $ fSpec) 
                    `Set.union`
                    (Set.fromList . map ameaMrk . Set.toList . meaningInstances $ fSpec)
instance Instances ObjectDef where
  instances fSpec = Set.fromList . concatMap (objects . ifcObj) 
                  . interfaceInstances $ fSpec
    where
      objects :: ObjectDef -> [ObjectDef]
      objects obj = obj : fields obj
instance Instances Pattern where
  instances = Set.fromList . ctxpats . originalContext
instance Instances Population where
  instances = Set.fromList . ctxpopus . originalContext
instance Instances Purpose where
  instances = purposeInstances
instance Instances Relation where
  instances = relationInstances
instance Instances Role where
  instances = Set.fromList . map fst . fRoles
instance Instances A_RoleRule where
  instances = Set.fromList . ctxrrules . originalContext
instance Instances Rule where
  instances = ruleInstances
instance Instances Signature where
  instances fSpec = 
       (Set.fromList . map sign . Set.toList . relationInstances $ fSpec)
       `Set.union`
       (Set.fromList . map sign . Set.toList . expressionInstances $ fSpec)
instance Instances ViewDef where
  instances = Set.fromList . viewDefs . originalContext

class Instances a => HasPurpose a where 
  purposes :: FSpec -> a -> [Purpose]
  purposes fSpec a = 
    Set.toList . Set.filter (isFor a) . instances $ fSpec
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

