module Ampersand.FSpec.Transformers 
  ( transformers
  , Transformer(..)
  , PopAtom(..)
  ) where
import Ampersand.FSpec.FSpec

-- | The function that retrieves the population of
--   some relation of Formal Ampersand of a given
--   ampersand script.
data Transformer = Transformer 
      { tRel :: String  -- name of relation
      , tSrc :: String  -- name of source
      , tTrg :: String  -- name of target
      , tPairFunction :: (FSpec -> [(PopAtom,PopAtom)]) -- the function that retrieves the population of this relation from the user's script.
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


toTransformer :: (String, String, String, (FSpec -> [(PopAtom,PopAtom)])) -> Transformer 
toTransformer (rel, sCpt, tCpt, fun) = Transformer rel sCpt tCpt fun                   

-- | The list of all transformers, one for each and every declaration in Formal Ampersand.
transformers :: [Transformer]
transformers = map toTransformer [
      ("allConjuncts"          , "Context"               , "Conjunct"
      , (\x -> [])
      )
     ,("allRoles"              , "Context"               , "Role"    
      , (\x -> [])
      )
     ,("allRules"              , "Context"               , "Rule"    
      , (\x -> [])
      )
     ,("allRules"              , "Pattern"               , "Rule"    
      , (\x -> [])
      )
     ,("arg"                   , "UnaryTerm"             , "Expression"
      , (\x -> [])
      )
     ,("attIn"                 , "Attribute"             , "ObjectDef"
      , (\x -> [])
      )
     ,("attObj"                , "Attribute"             , "ObjectDef"
      , (\x -> [])
      )
     ,("bind"                  , "BindedRelation"        , "Relation"
      , (\x -> [])
      )
     ,("changes"               , "Act"                   , "Relation"
      , (\x -> [])
      )
     ,("concepts"              , "Pattern"               , "Concept" 
      , (\x -> [])
      )
     ,("conjunct"              , "Conjunct"              , "Expression"
      , (\x -> [])
      )
     ,("context"               , "Concept"               , "Context" 
      , (\x -> [])
      )
     ,("context"               , "IdentityDef"           , "Context" 
      , (\x -> [])
      )
     ,("context"               , "Pattern"               , "Context" 
      , (\x -> [])
      )
     ,("context"               , "Population"            , "Context" 
      , (\x -> [])
      )
     ,("context"               , "Relation"              , "Context" 
      , (\x -> [])
      )
     ,("ctxds"                 , "Relation"              , "Context" 
      , (\x -> [])
      )
     ,("ctxrs"                 , "Rule"                  , "Context" 
      , (\x -> [])
      )
     ,("dbName"                , "Context"               , "DatabaseName"
      , (\x -> [])
      )
     ,("declaredIn"            , "Relation"              , "Context" 
      , (\x -> [])
      )
     ,("declaredIn"            , "Relation"              , "Pattern" 
      , (\x -> [])
      )
     ,("declaredthrough"       , "PropertyRule"          , "Property"
      , (\x -> [])
      )
     ,("decmean"               , "Relation"              , "Meaning" 
      , (\x -> [])
      )
     ,("decprL"                , "Relation"              , "String"  
      , (\x -> [])
      )
     ,("decprM"                , "Relation"              , "String"  
      , (\x -> [])
      )
     ,("decprR"                , "Relation"              , "String"  
      , (\x -> [])
      )
     ,("default"               , "View"                  , "Concept" 
      , (\x -> [])
      )
     ,("delta"                 , "Act"                   , "Pair"    
      , (\x -> [])
      )
     ,("expSQL"                , "PairViewSegment"       , "MySQLQuery"
      , (\x -> [])
      )
     ,("expTgt"                , "PairViewSegment"       , "Concept" 
      , (\x -> [])
      )
     ,("first"                 , "BinaryTerm"            , "Expression"
      , (\x -> [])
      )
     ,("formalExpression"      , "Rule"                  , "Expression"
      , (\x -> [])
      )
     ,("gengen"                , "IsE"                   , "Concept" 
      , (\x -> [])
      )
     ,("gengen"                , "Isa"                   , "Concept" 
      , (\x -> [])
      )
     ,("gens"                  , "Context"               , "IsE"     
      , (\x -> [])
      )
     ,("gens"                  , "Context"               , "Isa"     
      , (\x -> [])
      )
     ,("genspc"                , "IsE"                   , "Concept" 
      , (\x -> [])
      )
     ,("genspc"                , "Isa"                   , "Concept" 
      , (\x -> [])
      )
     ,("getExpressionRelation" , "Expression"            , "Relation"
      , (\x -> [])
      )
     ,("hasView"               , "Concept"               , "Concept" 
      , (\x -> [])
      )
     ,("identityRules"         , "Rule"                  , "Context" 
      , (\x -> [])
      )
     ,("identityRules"         , "Rule"                  , "Pattern" 
      , (\x -> [])
      )
     ,("ifcClass"              , "Interface"             , "String"  
      , (\x -> [])
      )
     ,("ifcControls"           , "Interface"             , "Conjunct"
      , (\x -> [])
      )
     ,("ifcInputs"             , "Interface"             , "Relation"
      , (\x -> [])
      )
     ,("ifcObj"                , "Interface"             , "ObjectDef"
      , (\x -> [])
      )
     ,("ifcOutputs"            , "Interface"             , "Relation"
      , (\x -> [])
      )
     ,("ifcPos"                , "Interface"             , "Origin"  
      , (\x -> [])
      )
     ,("ifcPrp"                , "Interface"             , "String"  
      , (\x -> [])
      )
     ,("ifcQuads"              , "Interface"             , "Quad"    
      , (\x -> [])
      )
     ,("ifcRoles"              , "Interface"             , "Role"    
      , (\x -> [])
      )
     ,("ifcname"               , "Interface"             , "String"  
      , (\x -> [])
      )
     ,("in"                    , "Pair"                  , "Expression"
      , (\x -> [])
      )
     ,("inQ"                   , "Quad"                  , "Act"     
      , (\x -> [])
      )
     ,("inst"                  , "Object"                , "ObjectDef"
      , (\x -> [])
      )
     ,("inst"                  , "Transaction"           , "Interface"
      , (\x -> [])
      )
     ,("interfaces"            , "Context"               , "Interface"
      , (\x -> [])
      )
     ,("interfaces"            , "Role"                  , "Interface"
      , (\x -> [])
      )
     ,("isa"                   , "Concept"               , "Concept" 
      , (\x -> [])
      )
     ,("isaCopy"               , "Concept"               , "Concept" 
      , (\x -> [])
      )
     ,("isaPlus"               , "Concept"               , "Concept" 
      , (\x -> [])
      )
     ,("isaRfx"                , "Concept"               , "Concept" 
      , (\x -> [])
      )
     ,("isaRfxCopy"            , "Concept"               , "Concept" 
      , (\x -> [])
      )
     ,("isaRfxPlus"            , "Concept"               , "Concept" 
      , (\x -> [])
      )
     ,("isaRfxStar"            , "Concept"               , "Concept" 
      , (\x -> [])
      )
     ,("isaStar"               , "Concept"               , "Concept" 
      , (\x -> [])
      )
     ,("language"              , "Context"               , "Language"
      , (\x -> [])
      )
     ,("left"                  , "Pair"                  , "Atom"    
      , (\x -> [])
      )
     ,("maintains"             , "Role"                  , "Rule"    
      , (\x -> [])
      )
     ,("markupText"            , "Purpose"               , "MarkupText"
      , (\x -> [])
      )
     ,("meaning"               , "Rule"                  , "Meaning" 
      , (\x -> [])
      )
     ,("message"               , "Rule"                  , "Message" 
      , (\x -> [])
      )
     ,("multrules"             , "Rule"                  , "Context" 
      , (\x -> [])
      )
     ,("multrules"             , "Rule"                  , "Pattern" 
      , (\x -> [])
      )
     ,("name"                  , "Concept"               , "ConceptName"
      , (\x -> [])
      )
     ,("name"                  , "Context"               , "ContextName"
      , (\x -> [])
      )
     ,("name"                  , "Pattern"               , "PatternName"
      , (\x -> [])
      )
     ,("name"                  , "Relation"              , "RelationName"
      , (\x -> [])
      )
     ,("name"                  , "Role"                  , "RoleName"
      , (\x -> [])
      )
     ,("name"                  , "Rule"                  , "RuleName"
      , (\x -> [])
      )
     ,("objctx"                , "ObjectDef"             , "Expression"
      , (\x -> [])
      )
     ,("objmView"              , "ObjectDef"             , "View"    
      , (\x -> [])
      )
     ,("objnm"                 , "ObjectDef"             , "String"  
      , (\x -> [])
      )
     ,("objpos"                , "ObjectDef"             , "Origin"  
      , (\x -> [])
      )
     ,("operator"              , "BinaryTerm"            , "Operator"
      , (\x -> [])
      )
     ,("operator"              , "UnaryTerm"             , "Operator"
      , (\x -> [])
      )
     ,("origin"                , "Rule"                  , "Origin"  
      , (\x -> [])
      )
     ,("originatesFrom"        , "Conjunct"              , "Rule"    
      , (\x -> [])
      )
     ,("outQ"                  , "Quad"                  , "Act"     
      , (\x -> [])
      )
     ,("pairView"              , "Rule"                  , "PairView"
      , (\x -> [])
      )
     ,("prop"                  , "Relation"              , "Property"
      , (\x -> [])
      )
     ,("propertyRule"          , "Relation"              , "PropertyRule"
      , (\x -> [])
      )
     ,("purpose"               , "Concept"               , "Purpose" 
      , (\x -> [])
      )
     ,("purpose"               , "Context"               , "Purpose" 
      , (\x -> [])
      )
     ,("purpose"               , "Identity"              , "Purpose" 
      , (\x -> [])
      )
     ,("purpose"               , "Interface"             , "Purpose" 
      , (\x -> [])
      )
     ,("purpose"               , "Pattern"               , "Purpose" 
      , (\x -> [])
      )
     ,("purpose"               , "Relation"              , "Purpose" 
      , (\x -> [])
      )
     ,("purpose"               , "Rule"                  , "Purpose" 
      , (\x -> [])
      )
     ,("purpose"               , "View"                  , "Purpose" 
      , (\x -> [])
      )
     ,("relsDefdIn"            , "Pattern"               , "Relation"
      , (\x -> [])
      )
     ,("right"                 , "Pair"                  , "Atom"    
      , (\x -> [])
      )
     ,("rrexp"                 , "Rule"                  , "Expression"
      , (\x -> [])
      )
     ,("second"                , "BinaryTerm"            , "Expression"
      , (\x -> [])
      )
     ,("segment"               , "PairView"              , "PairViewSegment" 
      , (\x -> [])
      )
     ,("segmentType"           , "PairViewSegment"       , "PairViewSegmentType"
      , (\x -> [])
      )
     ,("sequenceNr"            , "PairViewSegment"       , "Int"     
      , (\x -> [])
      )
     ,("sessAtom"              , "SESSION"               , "Atom"    
      , (\x -> [])
      )
     ,("sessIfc"               , "SESSION"               , "Interface"
      , (\x -> [])
      )
     ,("sessionRole"           , "SESSION"               , "Role"    
      , (\x -> [])
      )
     ,("showADL"               , "Expression"            , "ShowADL" 
      , (\x -> [])
      )
     ,("sign"                  , "Expression"            , "Signature"
      , (\x -> [])
      )
     ,("sign"                  , "Relation"              , "Signature"
      , (\x -> [])
      )
     ,("sign"                  , "Rule"                  , "Signature"
      , (\x -> [])
      )
     ,("singleton"             , "Singleton"             , "AtomValue"
      , (\x -> [])
      )
     ,("source"                , "Relation"              , "Concept" 
      , (\x -> [])
      )
     ,("src"                   , "Signature"             , "Concept" 
      , (\x -> [])
      )
     ,("srcOrTgt"              , "PairViewSegment"       , "SourceOrTarget"
      , (\x -> [])
      )
     ,("target"                , "Relation"              , "Concept" 
      , (\x -> [])
      )
     ,("text"                  , "PairViewSegment"       , "String"  
      , (\x -> [])
      )
     ,("tgt"                   , "Signature"             , "Concept" 
      , (\x -> [])
      )
     ,("transactionObject"     , "Transaction"           , "Object"  
      , (\x -> [])
      )
     ,("ttype"                 , "Concept"               , "TType"   
      , (\x -> [])
      )
     ,("udefrules"             , "Rule"                  , "Context" 
      , (\x -> [])
      )
     ,("udefrules"             , "Rule"                  , "Pattern" 
      , (\x -> [])
      )
     ,("urlEncodedName"        , "Concept"               , "EncodedName"
      , (\x -> [])
      )
     ,("urlEncodedName"        , "Pattern"               , "EncodedName"
      , (\x -> [])
      )
     ,("urlEncodedName"        , "Rule"                  , "EncodedName"
      , (\x -> [])
      )
     ,("usedIn"                , "Relation"              , "Expression"
      , (\x -> [])
      )
     ,("userCpt"               , "I"                     , "Concept" 
      , (\x -> [])
      )
     ,("userSrc"               , "V"                     , "Concept" 
      , (\x -> [])
      )
     ,("userTrg"               , "V"                     , "Concept" 
      , (\x -> [])
      )
     ,("uses"                  , "Context"               , "Pattern" 
      , (\x -> [])
      )
     ,("valid"                 , "Concept"               , "Context" 
      , (\x -> [])
      )
     ,("valid"                 , "Relation"              , "Context" 
      , (\x -> [])
      )
     ,("valid"                 , "Rule"                  , "Context" 
      , (\x -> [])
      )
     ,("versionInfo"           , "Context"               , "AmpersandVersion"
      , (\x -> [])
      )
     ,("viewBy"                , "Concept"               , "Concept" 
      , (\x -> [])
      )
     ,("viol"                  , "Interface"             , "Rule"    
      , (\x -> [])
      )
     ]
    