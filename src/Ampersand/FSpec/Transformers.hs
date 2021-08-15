{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
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
import qualified RIO.Text as T
import qualified Text.Pandoc.Shared as P

-- | The function that retrieves the population of
--   some relation of Formal Ampersand of a given
--   ampersand script.
data Transformer = Transformer
      { tRel :: Text  -- name of relation
      , tSrc :: Text  -- name of source
      , tTrg :: Text  -- name of target
      , mults :: Props -- property constraints
      , tPairs :: [PAtomPair]-- the population of this relation from the user's script.
      }

-- | This datatype reflects the nature of an atom. It is use to construct
--   the atom.
data PopAtom =
    DirtyId Text
    -- ^ Any Text. must be:
    --      * unique in the scope of the entire fspec
    --      * storable in a 255 database field
  | PopAlphaNumeric Text -- ^ Intended to be observable by users. Not a 'dirty id'.
  | PopInt Integer
  deriving (Eq,Ord)
instance Show PopAtom where
 show x
   = case x of
        DirtyId str         -> show str
        PopAlphaNumeric str -> show str
        PopInt i            -> show i

dirtyId :: Unique a => a -> PopAtom
dirtyId = DirtyId . idWithoutType

-- Function for PrototypeContext transformers. These atoms don't need to have a type prefix
toTransformer :: (Text, Text, Text, Props, [ (PopAtom,PopAtom)] ) -> Transformer
toTransformer (rel,src,tgt,props,tuples)
 = Transformer rel src tgt props tuples'
   where
     tuples' :: [PAtomPair]
     tuples' = map popAtomPair2PAtomPair tuples
     popAtomPair2PAtomPair (a,b)
      = PPair MeatGrinder (pAtom2AtomValue a) (pAtom2AtomValue b)
     pAtom2AtomValue :: PopAtom -> PAtomValue
     pAtom2AtomValue atm = 
       case atm of 
         DirtyId str         -> ScriptString MeatGrinder str
         PopAlphaNumeric str -> ScriptString MeatGrinder str
         PopInt i            -> ScriptInt MeatGrinder i

-- | The list of all transformers, one for each and every relation in Formal Ampersand.
transformersFormalAmpersand :: FSpec -> [Transformer]
transformersFormalAmpersand fSpec = map toTransformer [
{-
-}
--    RELATION acdcpt[ConceptDef*Text] [UNI]      -- ^ The name of the concept for which this is the definition. If there is no such concept, the conceptdefinition is ignored.
      ("acdcpt"                , "ConceptDef"            , "ConceptName"
      , Set.fromList [Uni]
      , [ (dirtyId cdf, PopAlphaNumeric . tshow . acdcpt $ cdf)
        | cdf::AConceptDef <- instanceList fSpec
        ]
      )
--    RELATION acddef2[ConceptDef*Meaning] [UNI]  -- ^ The textual definition of this concept.
     ,("acddef2"                , "ConceptDef"           , "Meaning"
      , Set.fromList [Uni]
      , [ (dirtyId cdf, dirtyId mean)
        | cdf::AConceptDef <- instanceList fSpec
        , mean::Meaning <- acdmean cdf
        ]
      )
--    RELATION acdfrom[ConceptDef*Pattern] [UNI]  -- ^ The name of the pattern or context in which this concept definition was made
     ,("acdfrom"                , "ConceptDef"           , "Pattern"
      , Set.fromList [Uni]
      , [ (dirtyId cdf, dirtyId pat)
        | pat::Pattern <- instanceList fSpec
        , cdf::AConceptDef <- ptcds pat
        ]
      )
--    RELATION acdmean[ConceptDef*Meaning] [UNI]  -- ^ User-specified meanings, possibly more than one, for multiple languages.
     ,("acdmean"                , "ConceptDef"           , "Meaning"
      , Set.empty
      , [ (dirtyId cdf, dirtyId mean)
        | cdf::AConceptDef <- instanceList fSpec
        , mean::Meaning <- acdmean cdf
        ]
      )
--    RELATION acdpos[ConceptDef*Origin] [UNI]      -- ^ The position of this definition in the text of the Ampersand source (filename, line number and column number).
    ,("acdpos"                , "ConceptDef"            , "Origin"
      , Set.fromList [Uni]
      , [ (dirtyId cdf, PopAlphaNumeric . tshow . origin $ cdf)
        | cdf::AConceptDef <- instanceList fSpec
        ]
      )
     ,("allConjuncts"          , "Context"               , "Conjunct"
      , Set.fromList [Inj]
      , [ (dirtyId ctx, dirtyId conj)
        | ctx::A_Context <- instanceList fSpec
        , conj::Conjunct <- instanceList fSpec
        ]
      )
     ,("allRoles"              , "Context"               , "Role"
      , Set.fromList [Inj]
      , [ (dirtyId ctx, dirtyId rol)
        | ctx::A_Context <- instanceList fSpec
        , rol::Role      <- instanceList fSpec
        ]
      )
     ,("allRules"              , "Pattern"               , "Rule"
      , Set.fromList [{-Inj-}]
      , [ (dirtyId pat, dirtyId rul)
        | pat::Pattern <- instanceList fSpec
        , rul::Rule    <- Set.elems $ allRules pat
        ]
      )
     ,("allRules"              , "Rule"                  , "Context"
      , Set.fromList [Uni {-,Sur-}]
      , [ (dirtyId rul, dirtyId ctx)
        | ctx::A_Context <- instanceList fSpec
        , rul::Rule      <- Set.elems $ allRules ctx
        ]
      )
     ,("arg"                   , "UnaryTerm"             , "Term"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId expr, dirtyId x)
        | expr::Expression <- instanceList fSpec
        , Just x <- [arg expr]
        ]
      )
     ,("asMarkdown"            , "Markup"                , "Text"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId mrk,(PopAlphaNumeric . P.stringify . amPandoc) mrk)
        | mrk::Markup <- instanceList fSpec
        ]
      )
     ,("bind"                  , "BindedRelation"        , "Relation"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId expr, dirtyId x)
        | expr::Expression <- instanceList fSpec
        , Just x <- [bindedRel expr]
        ]
      )
     ,("concepts"              , "Pattern"               , "Concept"
      , Set.empty
      , [ (dirtyId pat, dirtyId cpt)
        | pat::Pattern   <- instanceList fSpec
        , cpt::A_Concept <- Set.elems $ concs pat
        ]
      )
     ,("rc_conjunct"           , "Conjunct"              , "Term"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId conj, dirtyId (rc_conjunct conj))
        | conj::Conjunct <- instanceList fSpec
        ]
      )
     ,("context"               , "Concept"               , "Context"
      , Set.fromList [Uni]
      , [ (dirtyId cpt, dirtyId ctx)
        | ctx::A_Context <- instanceList fSpec
        , cpt::A_Concept <- Set.toList . concs $ ctx
        ]
      )
     ,("context"               , "Interface"             , "Context"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId ifc,dirtyId ctx)
        | ctx::A_Context <- instanceList fSpec
        , ifc::Interface <- ctxifcs ctx
        ]
      )
     ,("context"               , "Isa"                   , "Context"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId isa, dirtyId ctx)
        | ctx::A_Context <- instanceList fSpec
        , isa@Isa{} <- instanceList fSpec
        ]
      )
     ,("context"               , "IsE"                   , "Context"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId ise, dirtyId ctx)
        | ctx::A_Context <- instanceList fSpec
        , ise@IsE{} <- instanceList fSpec
        ]
      )
     ,("context"               , "Pattern"               , "Context"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId pat, dirtyId ctx)
        | ctx::A_Context <- instanceList fSpec
        , pat::Pattern   <- instanceList fSpec
        ]
      )
     ,("context"               , "Population"            , "Context"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId pop, dirtyId ctx)
        | ctx::A_Context  <- instanceList fSpec
        , pop::Population <- instanceList fSpec
        ]
      )
     ,("ctxcds"                , "ConceptDef"            , "Context"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId cdf, dirtyId ctx)
        | ctx::A_Context <- instanceList fSpec
        , cdf::AConceptDef <- instanceList fSpec
        ]
      )
     ,("relsDefdIn"            , "Relation"              , "Context"   ---contains ALL relations defined in this context
      , Set.fromList [Uni,Tot]
      , [ (dirtyId rel, dirtyId ctx)
        | ctx::A_Context <- instanceList fSpec
        , rel::Relation  <- Set.elems $ relsDefdIn ctx
        ]
      )
     ,("ctxds"                  , "Relation"              , "Context"
      , Set.fromList [Uni]
      , [ (dirtyId rel, dirtyId ctx)
        | ctx::A_Context <- instanceList fSpec
        , rel::Relation  <- Set.elems $ ctxds ctx
        ]
      )
     ,("ctxrs"                 , "Rule"                  , "Context"
      , Set.fromList [Uni]
      , [ (dirtyId rul, dirtyId ctx)
        | ctx::A_Context <- instanceList fSpec
        , rul::Rule      <- Set.elems . ctxrs  $ ctx
        ]
      )
     ,("declaredIn"            , "Relation"              , "Pattern"
      , Set.empty
      , [ (dirtyId rel, dirtyId pat)
        | pat::Pattern  <- instanceList fSpec
        , rel::Relation <- Set.elems $ relsDefdIn pat
        ]
      )
     ,("decMean"               , "Relation"              , "Meaning"
      , Set.empty
      , [ (dirtyId rel, dirtyId mean)
        | rel::Relation <- instanceList fSpec
        , mean::Meaning <- decMean rel
        ]
      )
     ,("decprL"                , "Relation"              , "String"
      , Set.fromList [Uni]
      , [ (dirtyId rel, (PopAlphaNumeric . decprL) rel)
        | rel::Relation <- instanceList fSpec
        , (not . T.null . decprL) rel
        ]
      )
     ,("decprM"                , "Relation"              , "String"
      , Set.fromList [Uni]
      , [ (dirtyId rel, (PopAlphaNumeric . decprM) rel)
        | rel::Relation <- instanceList fSpec
        , (not . T.null . decprM) rel
        ]
      )
     ,("decprR"                , "Relation"              , "String"
      , Set.fromList [Uni]
      , [ (dirtyId rel, (PopAlphaNumeric . decprR) rel)
        | rel::Relation <- instanceList fSpec
        , (not . T.null . decprR) rel
        ]
      )
     ,("expSQL"                , "PairViewSegment"       , "MySQLQuery"
      , Set.empty
      , []  --TODO
      )
     ,("expTgt"                , "PairViewSegment"       , "Concept"
      , Set.empty
      , []  --TODO
      )
     ,("fieldIn"               , "FieldDef"             , "ObjectDef"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId fld, dirtyId obj)
        | obj::ObjectDef <- instanceList fSpec
        , fld <- fields obj
        ]
     )
     ,("first"                 , "BinaryTerm"            , "Term"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId expr, dirtyId x)
        | expr::Expression <- instanceList fSpec
        , Just x <- [first expr]
        ]
      )
     ,("formalTerm"            , "Rule"                  , "Term"
      , Set.fromList [Uni]
      , [ (dirtyId rul, dirtyId (formalExpression rul))
        | rul::Rule <- instanceList fSpec
        ]
      )
     ,("gengen"                , "Isa"                   , "Concept"
      , Set.fromList [Uni,Tot]
      , [ ( dirtyId isa, dirtyId (gengen isa))
        | isa@Isa{} <- instanceList fSpec
        ]
      )
     ,("gengen"                , "IsE"                   , "Concept"
      , Set.fromList [Tot]
      , [ ( dirtyId ise, dirtyId cpt)
        | ise@IsE{} <- instanceList fSpec
        , cpt <- NE.toList $ genrhs ise]
      )
     ,("genspc"                , "IsE"                   , "Concept"
      , Set.fromList [Uni,Tot]
      , [ ( dirtyId ise, dirtyId (genspc ise))
        | ise@IsE{} <- instanceList fSpec
        ]
      )
     ,("genspc"                , "Isa"                   , "Concept"
      , Set.fromList [Uni,Tot]
      , [ ( dirtyId isa, dirtyId (genspc isa))
        | isa@Isa{} <- instanceList fSpec
        ]
      )
     ,("identityRules"         , "Rule"                  , "Context"
      , Set.fromList [Uni]
      , [ (dirtyId rul, dirtyId ctx)
        | ctx::A_Context <- instanceList fSpec
        , rul            <- Set.elems $ identityRules ctx
        ]
      )
     ,("identityRules"         , "Rule"                  , "Pattern"
      , Set.fromList [Uni]
      , [ (dirtyId rul, dirtyId pat)
        | pat::Pattern <- instanceList fSpec
        , rul          <- Set.elems $ identityRules pat
        ]
      )
     ,("ifcConjuncts"           , "Interface"             , "Conjunct"
      , Set.empty
      , [ (dirtyId ifc, dirtyId conj)
        | ifc::Interface <- instanceList fSpec
        , conj <- ifcConjuncts ifc
        ]
      )
     ,("ifcInputs"             , "Interface"             , "Relation"
      , Set.empty
      , []  --TODO
      )
     ,("ifcObj"                , "Interface"             , "ObjectDef"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId ifc, dirtyId (ifcObj ifc))
        | ifc::Interface <- instanceList fSpec
        ]
      )
     ,("ifcOutputs"            , "Interface"             , "Relation"
      , Set.empty
      , []  --TODO
      )
     ,("ifcPos"                , "Interface"             , "Origin"
      , Set.fromList [Uni]
      , [ (dirtyId ifc, PopAlphaNumeric . tshow . origin $ ifc)
        | ifc::Interface <- instanceList fSpec
        , origin ifc `notElem` [OriginUnknown, MeatGrinder]
        ]
      )
     ,("ifcPurpose"            , "Interface"             , "Purpose"
      , Set.empty
      , [ (dirtyId ifc, dirtyId purp)
        | ifc::Interface <- instanceList fSpec
        , purp           <- purposes fSpec ifc
        ]
      )
     ,("ifcRoles"              , "Interface"             , "Role"
      , Set.empty
      , [ (dirtyId ifc,dirtyId rol)
        | ifc <- instanceList fSpec
        , rol <- ifcRoles ifc
        ]
      )
    , ("isAPI"                 , "Interface"          , "Interface"
      , Set.fromList [Asy,Sym]
      , [ (dirtyId ifc, dirtyId ifc)
        | ifc::Interface <- instanceList fSpec
        , ifcIsAPI ifc
        ]
      )
-- the following transformer can be calculated by the Exec Engine. So it can be removed here if so desired.
    , ("isPublic"              , "Interface"          , "Interface"
      , Set.fromList [Asy,Sym]
      , [ (dirtyId ifc, dirtyId ifc)
        | ifc::Interface <- instanceList fSpec
        , null (ifcRoles ifc)
        ]
      )
     ,("isa"                   , "Concept"               , "Concept"
      , Set.empty
      , [ ( dirtyId gCpt, dirtyId (genspc ise))
        | ise@IsE{} <- instanceList fSpec
        , gCpt <- NE.toList $ genrhs ise
        ] ++
        [ ( dirtyId (genspc isa), dirtyId (genspc isa))
        | isa@Isa{} <- instanceList fSpec
        ]
      )
     ,("label"                 , "FieldDef"              , "FieldName"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId fld, PopAlphaNumeric (name obj))
        | obj::ObjectDef <- instanceList fSpec
        , fld <- fields obj
        ]
      )
     ,("language"              , "Context"               , "Language"
      , Set.empty
      , [ (dirtyId ctx,(PopAlphaNumeric . tshow . ctxlang) ctx)
        | ctx::A_Context <- instanceList fSpec
        ]
      )
     ,("language"              , "Markup"               , "Language"
      , Set.empty
      , [ (dirtyId mrk,(PopAlphaNumeric . tshow . amLang) mrk)
        | mrk::Markup <- instanceList fSpec
        ]
      )
     ,("maintains"             , "Role"                  , "Rule"
      , Set.empty
      , [ (dirtyId rol, dirtyId rul)
        | (rol,rul) <-  fRoleRuls fSpec
        ]
      )
     ,("markup"            , "Meaning"               , "Markup"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId mean, dirtyId . ameaMrk $ mean)
        | mean::Meaning <- Set.toList . meaningInstances $ fSpec
        ]
      )
     ,("markup"            , "Purpose"               , "Markup"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId purp, dirtyId . explMarkup $ purp)
        | purp::Purpose <- Set.toList . purposeInstances $ fSpec
        ]
      )
     ,("meaning"               , "Rule"                  , "Meaning"
      , Set.empty
      , [ (dirtyId rul, dirtyId mean)
        | rul::Rule <- instanceList fSpec
        , mean::Meaning <- rrmean rul
        ]
      )
     ,("message"               , "Rule"                  , "Message"
      , Set.empty
      , []  --TODO
      )
     ,("proprules"             , "PropertyRule"          , "Context"
      , Set.empty
      , [ (dirtyId rul, dirtyId ctx)
        | ctx::A_Context <- instanceList fSpec
        , rul            <- Set.elems $ proprules ctx
        ]
      )
     ,("proprules"             , "PropertyRule"          , "Pattern"
      , Set.empty
      , [ (dirtyId rul, dirtyId pat)
        | pat::Pattern <- instanceList fSpec
        , rul          <- Set.elems $ proprules pat
        ]
      )
     ,("propertyRule"          , "Relation"              , "PropertyRule"
      , Set.fromList [Sur]
      , [ (dirtyId rel, dirtyId rul)
        | ctx::A_Context <- instanceList fSpec
        , rul            <- Set.elems $ proprules ctx
        , Just (_,rel)   <- [rrdcl rul]
        ]
      )
     ,("declaredthrough"       , "PropertyRule"          , "Property"
      , Set.fromList [Tot]
      , [ (dirtyId rul, (PopAlphaNumeric . tshow) prop)
        | ctx::A_Context <- instanceList fSpec
        , rul            <- Set.elems $ proprules ctx
        , Just (prop,_)  <- [rrdcl rul]
        ]
      )
     ,("name"                  , "Concept"               , "ConceptName"
      , Set.fromList [Uni]
      , [ (dirtyId cpt, (PopAlphaNumeric . name) cpt)
        | cpt::A_Concept <- instanceList fSpec
        ]
      )
     ,("name"                  , "Context"               , "ContextName"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId ctx, (PopAlphaNumeric . name) ctx)
        | ctx::A_Context <- instanceList fSpec
        ]
      )
     ,("name"                  , "Interface"             , "InterfaceName"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId ifc, (PopAlphaNumeric . name) ifc)
        | ifc::Interface <- instanceList fSpec
        ]
      )
     ,("name"                  , "ObjectDef"             , "ObjectName"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId obj, (PopAlphaNumeric . name) obj)
        | obj::ObjectDef <- instanceList fSpec
        ]
      )
     ,("name"                  , "Pattern"               , "PatternName"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId pat,(PopAlphaNumeric . name) pat)
        | pat::Pattern <- instanceList fSpec
        ]
      )
     ,("name"                  , "Relation"              , "RelationName"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId rel,(PopAlphaNumeric . name) rel)
        | rel::Relation <- instanceList fSpec
        ]
      )
     ,("name"                  , "Role"                  , "RoleName"
      , Set.fromList [Uni]
      , [ (dirtyId rol,(PopAlphaNumeric . name) rol)
        | rol::Role <- instanceList fSpec
        ]
      )
     ,("name"                  , "Rule"                  , "RuleName"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId rul,(PopAlphaNumeric . name) rul)
        | rul::Rule <- instanceList fSpec
        ]
      )
     ,("name"                  , "ViewDef"              , "ViewDefName"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId vd, PopAlphaNumeric . tshow . name $ vd)
        | vd::ViewDef <- instanceList fSpec
        ]
      )
     ,("objView"               , "ObjectDef"             , "View"
      , Set.empty
      , [ (dirtyId obj, PopAlphaNumeric vw)
        | obj::ObjectDef <- instanceList fSpec
        , Just vw <- [objmView obj]
        ]
      )
     ,("objpos"                , "ObjectDef"             , "Origin"
      , Set.fromList [Uni]
      , [ (dirtyId obj, PopAlphaNumeric . tshow . origin $ obj)
        | obj::ObjectDef <- instanceList fSpec
        , origin obj `notElem` [OriginUnknown, MeatGrinder]
        ]
      )
     ,("operator"              , "BinaryTerm"            , "Operator"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId expr, PopAlphaNumeric . tshow $ op)
        | expr::Expression <- instanceList fSpec
        , Just op <- [binOp expr]
        ]
      )
     ,("operator"              , "UnaryTerm"             , "Operator"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId expr, PopAlphaNumeric . tshow $ op)
        | expr::Expression <- instanceList fSpec
        , Just op <- [unaryOp expr]
        ]
      )
     ,("origin"                , "Rule"                  , "Origin"
      , Set.fromList [Uni]
      , [ (dirtyId rul, PopAlphaNumeric . tshow . origin $ rul)
        | rul::Rule <- instanceList fSpec
        , origin rul `notElem` [OriginUnknown, MeatGrinder]
        ]
      )
     ,("pairView"              , "Rule"                  , "PairView"
      , Set.empty
      , []  --TODO
      )
     ,("prop"                  , "Relation"              , "Property"
      , Set.empty
      , [ (dirtyId rel, PopAlphaNumeric . tshow $ prop)
        | rel::Relation <- instanceList fSpec
        , prop <- Set.elems $ decprps rel
        ]
      )
     ,("purpose"               , "Concept"               , "Purpose"
      , Set.empty
      , [ (dirtyId cpt, dirtyId purp)
        | cpt::A_Concept <- instanceList fSpec
        , purp           <- purposes fSpec cpt
        ]
      )
     ,("purpose"               , "Context"               , "Purpose"
      , Set.empty
      , [ (dirtyId ctx, dirtyId purp)
        | ctx::A_Context <- instanceList fSpec
        , purp           <- purposes fSpec ctx
        ]
      )
     ,("purpose"               , "IdentityRule"                  , "Purpose"
      , Set.empty
      , [ (dirtyId idn, dirtyId purp)
        | idn::IdentityRule <- instanceList fSpec
        , purp           <- purposes fSpec idn
        ]
      )
     ,("purpose"               , "Interface"             , "Purpose"
      , Set.empty
      , [ (dirtyId ifc, dirtyId purp)
        | ifc::Interface <- instanceList fSpec
        , purp           <- purposes fSpec ifc
        ]
      )
     ,("purpose"               , "Pattern"               , "Purpose"
      , Set.empty
      , [ (dirtyId pat, dirtyId purp)
        | pat::Pattern <- instanceList fSpec
        , purp           <- purposes fSpec pat
        ]
      )
     ,("purpose"               , "Relation"              , "Purpose"
      , Set.empty
      , [ (dirtyId rel, dirtyId purp)
        | rel::Relation <- instanceList fSpec
        , purp             <- purposes fSpec rel
        ]
      )
     ,("purpose"               , "Rule"                  , "Purpose"
      , Set.empty
      , [ (dirtyId rul, dirtyId purp)
        | rul::Rule <- instanceList fSpec
        , purp           <- purposes fSpec rul
        ]
      )
     ,("purpose"               , "View"                  , "Purpose"
      , Set.empty
      , [ (dirtyId vw, dirtyId purp)
        | vw::ViewDef  <- instanceList fSpec
        , purp         <- purposes fSpec vw
        ]
      )
     ,("qConjuncts"            , "Quad"             , "Conjunct"
      , Set.empty
      , [ (dirtyId quad, dirtyId conj)
        | quad <- vquads fSpec
        , conj <- NE.toList (qConjuncts quad)
        ]  --TODO
      )
     ,("qDcl"                  , "Quad"             , "Relation"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId quad, dirtyId (qDcl quad))
        | quad <- vquads fSpec
        ]  --TODO
      )
     ,("qRule"                 , "Quad"             , "Rule"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId quad, dirtyId (qRule quad))
        | quad <- vquads fSpec
        ]  --TODO
      )
     ,("rc_orgRules"        , "Conjunct"              , "Rule"
      , Set.empty
      , [ (dirtyId conj, dirtyId rul)
        | conj::Conjunct <- instanceList fSpec
        , rul <- NE.toList $ rc_orgRules conj
        ]
      )
     ,("relsDefdIn"            , "Pattern"               , "Relation"
      , Set.empty
      , [ (dirtyId pat, dirtyId rel)
        | pat::Pattern <- instanceList fSpec
        , rel            <- Set.elems $ relsDefdIn pat
        ]
      )
     ,("second"                , "BinaryTerm"            , "Term"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId expr, dirtyId x)
        | expr::Expression <- instanceList fSpec
        , Just x <- [second expr]
        ]
      )
     ,("segment"               , "PairView"              , "PairViewSegment"
      , Set.empty
      , []  --TODO
      )
     ,("segmentType"           , "PairViewSegment"       , "PairViewSegmentType"
      , Set.empty
      , []  --TODO
      )
     ,("sequenceNr"            , "PairViewSegment"       , "Int"
      , Set.empty
      , []  --TODO
      )
     ,("sessAtom"              , "SESSION"               , "Atom"
      , Set.empty
      , []  -- This goes too deep. Keep it empty.
      )
     ,("sessIfc"               , "SESSION"               , "Interface"
      , Set.empty
      , []  --TODO
      )
     ,("sessionRole"           , "SESSION"               , "Role"
      , Set.empty
      , []  --TODO
      )
     ,("showADL"               , "Term"                  , "ShowADL"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId expr, PopAlphaNumeric (showA expr))
        | expr::Expression <- instanceList fSpec
        ]
      )
     ,("sign"                  , "Term"                  , "Signature"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId expr, dirtyId (sign expr))
        | expr::Expression <- instanceList fSpec
        ]
      )
     ,("sign"                  , "Relation"              , "Signature"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId rel, dirtyId (sign rel))
        | rel::Relation <- instanceList fSpec
        ]
      )
     ,("singleton"             , "Singleton"             , "AtomValue"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId expr, dirtyId x)
        | expr::Expression <- instanceList fSpec
        , Just x <- [singleton expr]
        ]
      )
     ,("source"                , "Relation"              , "Concept"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId rel, dirtyId (source rel))
        | rel::Relation <- instanceList fSpec
        ]
      )
     ,("src"                   , "Signature"             , "Concept"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId sgn, dirtyId (source sgn))
        | sgn::Signature <- instanceList fSpec
        ]
      )
     ,("srcOrTgt"              , "PairViewSegment"       , "SourceOrTarget"
      , Set.fromList [Uni,Tot]
      , []  --TODO
      )
     ,("target"                , "Relation"              , "Concept"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId rel, dirtyId (target rel))
        | rel::Relation <- instanceList fSpec
        ]
      )
     ,("text"                  , "PairViewSegment"       , "String"
      , Set.fromList [Uni,Tot]
      , []  --TODO
      )
     ,("tgt"                   , "Signature"             , "Concept"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId sgn, dirtyId (target sgn))
        | sgn::Signature <- instanceList fSpec
        ]
      )
    ,("ttype"                 , "Concept"               , "TType"
      , Set.fromList [Uni]
      , [ (dirtyId cpt, PopAlphaNumeric . tshow . cptTType fSpec $ cpt)
        | cpt::A_Concept <- instanceList fSpec
        ]
      )
     ,("udefrules"             , "Rule"                  , "Context"
      , Set.fromList [Uni]
      , [ (dirtyId rul, dirtyId ctx)
        | ctx::A_Context <- instanceList fSpec
        , rul            <- Set.elems $ udefrules ctx
        ]
      )
     ,("udefrules"             , "Rule"                  , "Pattern"
      , Set.fromList [Uni]
      , [ (dirtyId rul, dirtyId pat)
        | pat::Pattern <- instanceList fSpec
        , rul            <- Set.elems $ udefrules pat
        ]
      )
     ,("urlEncodedName"        , "Concept"               , "EncodedName"
      , Set.fromList [Uni]
      , [ (dirtyId cpt, PopAlphaNumeric . urlEncodedName . name $ cpt)
        | cpt::A_Concept <- instanceList fSpec
        ]
      )
     ,("urlEncodedName"        , "Pattern"               , "EncodedName"
      , Set.fromList [Uni]
      , [ (dirtyId pat, PopAlphaNumeric . urlEncodedName . name $ pat)
        | pat::Pattern <- instanceList fSpec
        ]
      )
     ,("urlEncodedName"        , "Rule"                  , "EncodedName"
      , Set.fromList [Uni]
      , [ (dirtyId rul, PopAlphaNumeric . urlEncodedName . name $ rul)
        | rul::Rule <- instanceList fSpec
        ]
      )
     ,("usedIn"                , "Relation"              , "Term"
      , Set.empty
      , [ (dirtyId rel, dirtyId expr)
        | expr::Expression <- instanceList fSpec
        , rel::Relation <- Set.elems $ bindedRelationsIn expr
        ]
      )
     ,("userCpt"               , "Epsilon"                     , "Concept"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId expr, dirtyId x)
        | expr::Expression <- instanceList fSpec
        , Just (x::A_Concept) <- [userCpt expr]
        ]
      )
     ,("userSrc"               , "V"                     , "Concept"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId expr, dirtyId x)
        | expr::Expression <- instanceList fSpec
        , Just x <- [userSrc expr]
        ]
      )
     ,("userTgt"               , "V"                     , "Concept"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId expr, dirtyId x)
        | expr::Expression <- instanceList fSpec
        , Just x <- [userTgt expr]
        ]
      )
     ,("vdats"                 , "ViewDef"              , "ViewSegment"
      , Set.fromList [Inj,Sur]
      , [ (dirtyId vd, PopAlphaNumeric . tshow $ vs)
        | vd::ViewDef <- instanceList fSpec
        , vs <- vdats vd
        ]
      )
     ,("vdcpt"                 , "ViewDef"              , "Concept"
      , Set.fromList [Uni]
      , [ (dirtyId vd, PopAlphaNumeric . tshow . vdcpt $ vd)
        | vd::ViewDef <- instanceList fSpec, vdIsDefault vd
        ]
      )
     ,("vdhtml"                , "ViewDef"              , "Concept"
      , Set.fromList [Uni]
      , [ (dirtyId vd, PopAlphaNumeric . tshow $ html)
        | vd::ViewDef <- instanceList fSpec
        , Just html <- [vdhtml vd]
        ]
      )
     ,("vdIsDefault"           , "ViewDef"              , "Concept"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId vd, PopAlphaNumeric . tshow . vdcpt $ vd)
        | vd::ViewDef <- instanceList fSpec
        ]
      )
     ,("vdpos"                 , "ViewDef"              , "Origin"
      , Set.fromList [Uni]
      , [ (dirtyId vd, PopAlphaNumeric . tshow . origin $ vd)
        | vd::ViewDef <- instanceList fSpec
        , origin vd `notElem` [OriginUnknown, MeatGrinder]
        ]
      )
     ,("versionInfo"           , "Context"               , "AmpersandVersion"
      , Set.fromList [Uni,Tot]
      , [ (dirtyId ctx,PopAlphaNumeric (longVersion appVersion))
        | ctx::A_Context <- instanceList fSpec
        ]
      )
     ,("viewBy"                , "Concept"               , "Concept"
      , Set.empty
      , []  --TODO
      )
     ,("violatable"            , "Interface"             , "Rule"
      , Set.empty
      , []  --TODO
      )
     ]


dirtyIdWithoutType :: Unique a => a -> PopAtom
dirtyIdWithoutType = DirtyId . idWithoutType

-- | The following transformers provide the metamodel needed to run a prototype.
--   Note: The information in transformersPrototypeContext is fully contained in FormalAmpersand.
--   You might do this by dropping all prefixes "PF_" and "pf_" and doing
--   the following transformation:
--     label[Role*PF_Label]                -> name[Role*RoleName]
--   Then you will see that the transformers defined here are a subset of the FormalAmpersand transformers.
transformersPrototypeContext :: FSpec -> [Transformer]
transformersPrototypeContext fSpec = map toTransformer
-- the following transformer is also contained in FormalAmpersand.
    [ ("isAPI"                 , "PF_Interface"          , "PF_Interface"
      , Set.fromList []
      , [ (dirtyIdWithoutType ifc, dirtyIdWithoutType ifc)
        | ifc::Interface <- instanceList fSpec
        , ifcIsAPI ifc
        ]
      )
-- the following transformer can be calculated by the Exec Engine.
-- it is also contained in FormalAmpersand.
    , ("isPublic"              , "PF_Interface"          , "PF_Interface"
      , Set.fromList []
      , [ (dirtyIdWithoutType ifc, dirtyIdWithoutType ifc)
        | ifc::Interface <- instanceList fSpec
        , null (ifcRoles ifc)
        ]
      )
-- the following transformer is also contained in FormalAmpersand.
    , ("label"                 , "PF_Interface"          , "PF_Label"
      , Set.fromList []
      , [ (dirtyIdWithoutType ifc, PopAlphaNumeric . name $ ifc)
        | ifc::Interface <- instanceList fSpec
        ]
      )
-- the following transformer is called name[Role*RoleName] in FormalAmpersand
    , ("label"                 , "Role"               , "PF_Label"
      , Set.fromList [Uni]
      , [ (dirtyIdWithoutType role, PopAlphaNumeric . name $ role)
        | role::Role <- instanceList fSpec
        ]
      )
-- the following transformer is called ifcRoles[Interface*Role] in FormalAmpersand
    , ("pf_ifcRoles"           , "PF_Interface"          , "Role"
      , Set.fromList []
      , [ (dirtyIdWithoutType ifc , dirtyIdWithoutType role)
        | ifc::Interface <- instanceList fSpec
        , role <- ifcRoles ifc
        ]
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
meaningInstances fSpec = (Set.fromList . concatMap meanings . Set.toList . relationInstances $ fSpec)
                          `Set.union`
                         (Set.fromList . concatMap meanings . Set.toList . ruleInstances $ fSpec)
purposeInstances :: FSpec -> Set.Set Purpose
purposeInstances = fSexpls
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
instance Instances AConceptDef where
  instances = Set.fromList . ctxcds . originalContext
instance Instances Conjunct where
  instances = Set.fromList . allConjuncts
instance Instances Expression where
  instances = expressionInstances
instance Instances IdentityRule where
  instances = Set.fromList . ctxks . originalContext
instance Instances Rule where
  instances = allRules . originalContext   -- This contains all rules declared inside a context but outside the patterns it contains.
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
  instances = Set.fromList . vpatterns
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
        ExplConcept x  -> cpt == x
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
instance HasPurpose IdentityRule where
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
   , userCpt' :: Maybe A_Concept -- the concept of an Epsilon (and thus I too) Expression
   , userSrc' :: Maybe A_Concept -- the source concept of a V Expression
   , userTgt' :: Maybe A_Concept -- the target concept of a V Expression
   , singleton' :: Maybe PAtomValue -- the value of a singleton Expression
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
userTgt :: Expression -> Maybe A_Concept
userTgt = userTgt' . exprInfo
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
        , userTgt'   = Nothing
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
        , userTgt'   = Nothing
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
        , userTgt'   = Nothing
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
        , userTgt'   = Nothing
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
        , userTgt'   = Nothing
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
        , userTgt'   = Nothing
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
        , userTgt'   = Nothing
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
        , userTgt'   = Nothing
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
        , userTgt'   = Nothing
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
        , userTgt'   = Nothing
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
        , userTgt'   = Nothing
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
        , userTgt'   = Nothing
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
        , userTgt'   = Nothing
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
        , userTgt'   = Nothing
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
        , userTgt'   = Nothing
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
        , userTgt'   = userTgt e
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
        , userTgt'   = Nothing
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
        , userTgt'   = Nothing
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
        , userTgt'   = Nothing
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
        , userTgt'   = Just (target sgn)
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
        , userTgt'   = Nothing
        , singleton' = Just val
        }
data UnaryOp =
             KleeneStar
           | KleenePlus
           | Converse
           | UnaryMinus
           | Bracket deriving (Eq, Show, Typeable)
instance Unique UnaryOp where
  showUnique = tshow

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
  showUnique = tshow
instance Unique (Either BinOp UnaryOp) where
  showUnique (Left  a) = showUnique a
  showUnique (Right b) = showUnique b

