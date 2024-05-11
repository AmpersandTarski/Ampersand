{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Ampersand.FSpec.Transformers
  ( transformersFormalAmpersand,
    transformersPrototypeContext,
    Transformer (..),
    PopAtom (..),
    instances,
  )
where

import Ampersand.ADL1
import Ampersand.Basics hiding (first, second)
import Ampersand.Classes
import Ampersand.Core.ShowAStruct
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.Instances
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T

-- import qualified Text.Pandoc.Shared as P

-- | The function that retrieves the population of
--   some relation of Formal Ampersand of a given
--   ampersand script.
data Transformer = Transformer
  { tRel :: Text, -- name of relation
    tSrc :: Text, -- name of source
    tTrg :: Text, -- name of target
    tPairs :: [PAtomPair] -- the population of this relation from the user's script.
  }

-- | This datatype reflects the nature of an atom. It is use to construct
--   the atom.
data PopAtom
  = -- | Any Text. must be:
    --      * unique in the scope of the entire fspec
    --      * storable in a 255 database field
    DirtyId Text
  | -- | Intended to be observable by users. Not a 'dirty id'.
    PopAlphaNumeric Text
  | PopInt Integer
  deriving (Eq, Ord)

instance Show PopAtom where
  show x =
    case x of
      DirtyId str -> show str
      PopAlphaNumeric str -> show str
      PopInt i -> show i

dirtyId :: Unique a => a -> PopAtom
dirtyId = DirtyId . idWithoutType

-- Function for PrototypeContext transformers. These atoms don't need to have a type prefix
toTransformer :: (Text, Text, Text, [(PopAtom, PopAtom)]) -> Transformer
toTransformer (rel, src, tgt, tuples) =
  Transformer rel src tgt tuples'
  where
    tuples' :: [PAtomPair]
    tuples' = map popAtomPair2PAtomPair tuples
    popAtomPair2PAtomPair (a, b) =
      PPair MeatGrinder (pAtom2AtomValue a) (pAtom2AtomValue b)
    pAtom2AtomValue :: PopAtom -> PAtomValue
    pAtom2AtomValue atm =
      case atm of
        DirtyId str -> ScriptString MeatGrinder str
        PopAlphaNumeric str -> ScriptString MeatGrinder str
        PopInt i -> ScriptInt MeatGrinder i

-- | The list of all transformers, one for each and every relation in Formal Ampersand.
transformersFormalAmpersand :: FSpec -> [Transformer]
transformersFormalAmpersand fSpec =
  map
    toTransformer
    [ {-
      -}
      -- --    RELATION acdcpt[ConceptDef*Text] [UNI]      -- ^ The name of the concept for which this is the definition. If there is no such concept, the conceptdefinition is ignored.
      -- ( "acdcpt",
      --   "ConceptDef",
      --   "ConceptName",
      --   [ (dirtyId cdf, PopAlphaNumeric . acdcpt $ cdf)
      --     | cdf :: AConceptDef <- instanceList fSpec
      --   ]
      -- ),
      -- --    RELATION acddef2[ConceptDef*Meaning] [UNI]  -- ^ The textual definition of this concept.
      -- ( "acddef2",
      --   "ConceptDef",
      --   "Meaning",
      --   [ (dirtyId cdf, dirtyId mean)
      --     | cdf :: AConceptDef <- instanceList fSpec,
      --       mean :: Meaning <- acdmean cdf
      --   ]
      -- ),
      -- --    RELATION acdfrom[ConceptDef*Pattern] [UNI]  -- ^ The name of the pattern or context in which this concept definition was made
      -- ( "acdfrom",
      --   "ConceptDef",
      --   "Pattern",
      --   [ (dirtyId cdf, dirtyId pat)
      --     | pat :: Pattern <- instanceList fSpec,
      --       cdf :: AConceptDef <- ptcds pat
      --   ]
      -- ),
      -- --    RELATION acdmean[ConceptDef*Meaning] [UNI]  -- ^ User-specified meanings, possibly more than one, for multiple languages.
      -- ( "acdmean",
      --   "ConceptDef",
      --   "Meaning",
      --   [ (dirtyId cdf, dirtyId mean)
      --     | cdf :: AConceptDef <- instanceList fSpec,
      --       mean :: Meaning <- acdmean cdf
      --   ]
      -- ),
      -- --    RELATION acdpos[ConceptDef*Origin] [UNI]      -- ^ The position of this definition in the text of the Ampersand source (filename, line number and column number).
      -- ( "acdpos",
      --   "ConceptDef",
      --   "Origin",
      --   [ (dirtyId cdf, PopAlphaNumeric . tshow . origin $ cdf)
      --     | cdf :: AConceptDef <- instanceList fSpec
      --   ]
      -- ),
      ( "allConjuncts",
        "Context",
        "Conjunct",
        [ (dirtyId ctx, dirtyId conj)
          | ctx :: A_Context <- instanceList fSpec,
            conj :: Conjunct <- instanceList fSpec
        ]
      ),
      ( "allRoles",
        "Context",
        "Role",
        [ (dirtyId ctx, dirtyId rol)
          | ctx :: A_Context <- instanceList fSpec,
            rol :: Role <- instanceList fSpec
        ]
      ),
      -- ( "allRules",
      --   "Pattern",
      --   "Rule",
      --   {-Inj-}
      --   [ (dirtyId pat, dirtyId rul)
      --     | pat :: Pattern <- instanceList fSpec,
      --       rul :: Rule <- Set.elems $ allRules pat
      --   ]
      -- ),
      ( "allRules",
        "Rule",
        "Context",
        [ (dirtyId rul, dirtyId ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rul :: Rule <- Set.elems $ allRules ctx
        ]
      ),
      ( "arg",
        "UnaryTerm",
        "Term",
        [ (dirtyId expr, dirtyId x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [arg expr]
        ]
      ),
      -- ( "asMarkdown",
      --   "Markup",
      --   "Text",
      --   [ (dirtyId mrk, (PopAlphaNumeric . P.stringify . amPandoc) mrk)
      --     | mrk :: Markup <- instanceList fSpec
      --   ]
      -- ),
      ( "bind",
        "BindedRelation",
        "Relation",
        [ (dirtyId expr, dirtyId x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [bindedRel expr]
        ]
      ),
      ( "concepts",
        "Pattern",
        "Concept",
        [ (dirtyId pat, dirtyId cpt)
          | pat :: Pattern <- instanceList fSpec,
            cpt :: A_Concept <- Set.elems $ concs pat
        ]
      ),
      ( "rc_conjunct",
        "Conjunct",
        "Term",
        [ (dirtyId conj, dirtyId (rc_conjunct conj))
          | conj :: Conjunct <- instanceList fSpec
        ]
      ),
      ( "context",
        "Concept",
        "Context",
        [ (dirtyId cpt, dirtyId ctx)
          | ctx :: A_Context <- instanceList fSpec,
            cpt :: A_Concept <- Set.toList . concs $ ctx
        ]
      ),
      ( "context",
        "Interface",
        "Context",
        [ (dirtyId ifc, dirtyId ctx)
          | ctx :: A_Context <- instanceList fSpec,
            ifc :: Interface <- ctxifcs ctx
        ]
      ),
      ( "context",
        "Isa",
        "Context",
        [ (dirtyId isa, dirtyId ctx)
          | ctx :: A_Context <- instanceList fSpec,
            isa@Isa {} <- instanceList fSpec
        ]
      ),
      ( "context",
        "IsE",
        "Context",
        [ (dirtyId ise, dirtyId ctx)
          | ctx :: A_Context <- instanceList fSpec,
            ise@IsE {} <- instanceList fSpec
        ]
      ),
      ( "context",
        "Pattern",
        "Context",
        [ (dirtyId pat, dirtyId ctx)
          | ctx :: A_Context <- instanceList fSpec,
            pat :: Pattern <- instanceList fSpec
        ]
      ),
      -- ( "context",
      --   "Population",
      --   "Context",
      --
      --   [ (dirtyId pop, dirtyId ctx)
      --     | ctx :: A_Context <- instanceList fSpec,
      --       pop :: Population <- instanceList fSpec
      --   ]
      -- ),
      -- ( "ctxcds",
      --   "ConceptDef",
      --   "Context",
      --
      --   [ (dirtyId cdf, dirtyId ctx)
      --     | ctx :: A_Context <- instanceList fSpec,
      --       cdf :: AConceptDef <- instanceList fSpec
      --   ]
      -- ),
      ( "relsDefdIn",
        "Relation",
        "Context", ---contains ALL relations defined in this context
        [ (dirtyId rel, dirtyId ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rel :: Relation <- Set.elems $ relsDefdIn ctx
        ]
      ),
      ( "ctxds",
        "Relation",
        "Context",
        [ (dirtyId rel, dirtyId ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rel :: Relation <- Set.elems $ ctxds ctx
        ]
      ),
      ( "ctxrs",
        "Rule",
        "Context",
        [ (dirtyId rul, dirtyId ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rul :: Rule <- Set.elems . ctxrs $ ctx
        ]
      ),
      ( "declaredIn",
        "Relation",
        "Pattern",
        [ (dirtyId rel, dirtyId pat)
          | pat :: Pattern <- instanceList fSpec,
            rel :: Relation <- Set.elems $ relsDefdIn pat
        ]
      ),
      ( "decMean",
        "Relation",
        "Meaning",
        [ (dirtyId rel, dirtyId mean)
          | rel :: Relation <- instanceList fSpec,
            mean :: Meaning <- decMean rel
        ]
      ),
      ( "decprL",
        "Relation",
        "String",
        [ (dirtyId rel, (PopAlphaNumeric . decprL) rel)
          | rel :: Relation <- instanceList fSpec,
            (not . T.null . decprL) rel
        ]
      ),
      ( "decprM",
        "Relation",
        "String",
        [ (dirtyId rel, (PopAlphaNumeric . decprM) rel)
          | rel :: Relation <- instanceList fSpec,
            (not . T.null . decprM) rel
        ]
      ),
      ( "decprR",
        "Relation",
        "String",
        [ (dirtyId rel, (PopAlphaNumeric . decprR) rel)
          | rel :: Relation <- instanceList fSpec,
            (not . T.null . decprR) rel
        ]
      ),
      ( "pvsExp",
        "PairViewSegment",
        "Term",
        [ (dirtyId pvs, dirtyId (pvsExp pvs))
          | pvs@PairViewExp {} :: PairViewSegment Expression <- instanceList fSpec
        ]
      ),
      -- ( "fieldIn",
      --   "FieldDef",
      --   "ObjectDef",
      --
      --   [ (dirtyId fld, dirtyId obj)
      --     | obj :: ObjectDef <- instanceList fSpec,
      --       fld <- fields obj
      --   ]
      -- ),
      ( "first",
        "BinaryTerm",
        "Term",
        [ (dirtyId expr, dirtyId x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [first expr]
        ]
      ),
      ( "formalTerm",
        "Rule",
        "Term",
        [ (dirtyId rul, dirtyId (formalExpression rul))
          | rul :: Rule <- instanceList fSpec
        ]
      ),
      ( "gengen",
        "Isa",
        "Concept",
        [ (dirtyId isa, dirtyId (gengen isa))
          | isa@Isa {} <- instanceList fSpec
        ]
      ),
      ( "gengen",
        "IsE",
        "Concept",
        [ (dirtyId ise, dirtyId cpt)
          | ise@IsE {} <- instanceList fSpec,
            cpt <- NE.toList $ genrhs ise
        ]
      ),
      ( "genspc",
        "IsE",
        "Concept",
        [ (dirtyId ise, dirtyId (genspc ise))
          | ise@IsE {} <- instanceList fSpec
        ]
      ),
      ( "genspc",
        "Isa",
        "Concept",
        [ (dirtyId isa, dirtyId (genspc isa))
          | isa@Isa {} <- instanceList fSpec
        ]
      ),
      ( "identityRules",
        "Rule",
        "Context",
        [ (dirtyId rul, dirtyId ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rul <- Set.elems $ identityRules ctx
        ]
      ),
      ( "identityRules",
        "Rule",
        "Pattern",
        [ (dirtyId rul, dirtyId pat)
          | pat :: Pattern <- instanceList fSpec,
            rul <- Set.elems $ identityRules pat
        ]
      ),
      ( "ifcConjuncts",
        "Interface",
        "Conjunct",
        [ (dirtyId ifc, dirtyId conj)
          | ifc :: Interface <- instanceList fSpec,
            conj <- ifcConjuncts ifc
        ]
      ),
      ( "ifcInputs",
        "Interface",
        "Relation",
        [] --TODO future work
      ),
      ( "ifcObj",
        "Interface",
        "ObjectDef",
        [ (dirtyId ifc, dirtyId (ifcObj ifc))
          | ifc :: Interface <- instanceList fSpec
        ]
      ),
      ( "ifcOutputs",
        "Interface",
        "Relation",
        [] --TODO future work
      ),
      ( "ifcPos",
        "Interface",
        "Origin",
        [ (dirtyId ifc, PopAlphaNumeric . tshow . origin $ ifc)
          | ifc :: Interface <- instanceList fSpec,
            origin ifc `notElem` [OriginUnknown, MeatGrinder]
        ]
      ),
      ( "ifcPurpose",
        "Interface",
        "Purpose",
        [ (dirtyId ifc, dirtyId purp)
          | ifc :: Interface <- instanceList fSpec,
            purp <- purposes fSpec ifc
        ]
      ),
      ( "ifcRoles",
        "Interface",
        "Role",
        [ (dirtyId ifc, dirtyId rol)
          | ifc <- instanceList fSpec,
            rol <- ifcRoles ifc
        ]
      ),
      ( "isAPI",
        "Interface",
        "Interface",
        [ (dirtyId ifc, dirtyId ifc)
          | ifc :: Interface <- instanceList fSpec,
            ifcIsAPI ifc
        ]
      ),
      ( "isPublic",
        "Interface",
        "Interface",
        [ (dirtyId ifc, dirtyId ifc)
          | ifc :: Interface <- instanceList fSpec,
            null (ifcRoles ifc)
        ]
      ),
      ( "isa",
        "Concept",
        "Concept",
        [ (dirtyId (genspc ise), dirtyId gCpt)
          | ise@IsE {} <- instanceList fSpec,
            gCpt <- NE.toList $ genrhs ise
        ]
          ++ [ (dirtyId (genspc isa), dirtyId (gengen isa))
               | isa@Isa {} <- instanceList fSpec
             ]
      ),
      ( "label",
        "FieldDef",
        "FieldName",
        [ (dirtyId fld, PopAlphaNumeric (name obj))
          | obj :: ObjectDef <- instanceList fSpec,
            fld <- fields obj
        ]
      ),
      -- ( "language",
      --   "Context",
      --   "Language",
      --
      --   [ (dirtyId ctx, (PopAlphaNumeric . tshow . ctxlang) ctx)
      --     | ctx :: A_Context <- instanceList fSpec
      --   ]
      -- ),
      -- ( "language",
      --   "Markup",
      --   "Language",
      --
      --   [ (dirtyId mrk, (PopAlphaNumeric . tshow . amLang) mrk)
      --     | mrk :: Markup <- instanceList fSpec
      --   ]
      -- ),
      ( "maintains",
        "Role",
        "Rule",
        [ (dirtyId rol, dirtyId rul)
          | (rol, rul) <- fRoleRuls fSpec
        ]
      ),
      -- ( "markup",
      --   "Meaning",
      --   "Markup",
      --
      --   [ (dirtyId mean, dirtyId . ameaMrk $ mean)
      --     | mean :: Meaning <- instanceList fSpec
      --   ]
      -- ),
      -- ( "markup",
      --   "Purpose",
      --   "Markup",
      --
      --   [ (dirtyId purp, dirtyId . explMarkup $ purp)
      --     | purp :: Purpose <- instanceList fSpec
      --   ]
      -- ),
      ( "meaning",
        "Rule",
        "Meaning",
        [ (dirtyId rul, dirtyId mean)
          | rul :: Rule <- instanceList fSpec,
            mean :: Meaning <- rrmean rul
        ]
      ),
      ( "message",
        "Rule",
        "Message",
        [] --TODO
      ),
      ( "propertyRule",
        "Relation",
        "PropertyRule",
        [ (dirtyId rel, dirtyId rul)
          | ctx :: A_Context <- instanceList fSpec,
            rul <- Set.elems $ proprules ctx,
            Propty _ rel <- [rrkind rul]
        ]
      ),
      ( "declaredthrough",
        "PropertyRule",
        "Property",
        [ (dirtyId rul, (PopAlphaNumeric . tshow) prop)
          | ctx :: A_Context <- instanceList fSpec,
            rul <- Set.elems $ proprules ctx,
            Propty prop _ <- [rrkind rul]
        ]
      ),
      ( "name",
        "Concept",
        "ConceptName",
        [ (dirtyId cpt, (PopAlphaNumeric . name) cpt)
          | cpt :: A_Concept <- instanceList fSpec
        ]
      ),
      ( "name",
        "Context",
        "ContextName",
        [ (dirtyId ctx, (PopAlphaNumeric . name) ctx)
          | ctx :: A_Context <- instanceList fSpec
        ]
      ),
      ( "name",
        "Interface",
        "InterfaceName",
        [ (dirtyId ifc, (PopAlphaNumeric . name) ifc)
          | ifc :: Interface <- instanceList fSpec
        ]
      ),
      -- ( "name",
      --   "ObjectDef",
      --   "ObjectName",
      --
      --   [ (dirtyId obj, (PopAlphaNumeric . name) obj)
      --     | obj :: ObjectDef <- instanceList fSpec
      --   ]
      -- ),
      ( "name",
        "Pattern",
        "PatternName",
        [ (dirtyId pat, (PopAlphaNumeric . name) pat)
          | pat :: Pattern <- instanceList fSpec
        ]
      ),
      ( "name",
        "Relation",
        "RelationName",
        [ (dirtyId rel, (PopAlphaNumeric . name) rel)
          | rel :: Relation <- instanceList fSpec
        ]
      ),
      ( "name",
        "Role",
        "RoleName",
        [ (dirtyId rol, (PopAlphaNumeric . name) rol)
          | rol :: Role <- instanceList fSpec
        ]
      ),
      ( "name",
        "Rule",
        "RuleName",
        [ (dirtyId rul, (PopAlphaNumeric . name) rul)
          | rul :: Rule <- instanceList fSpec
        ]
      ),
      -- ( "name",
      --   "View",
      --   "ViewDefName",
      --
      --   [ (dirtyId vd, PopAlphaNumeric . tshow . name $ vd)
      --     | vd :: ViewDef <- instanceList fSpec
      --   ]
      -- ),
      ( "objView",
        "ObjectDef",
        "View",
        [ (dirtyId obj, PopAlphaNumeric vw)
          | obj :: ObjectDef <- instanceList fSpec,
            Just vw <- [objmView obj]
        ]
      ),
      ( "origin",
        "ObjectDef",
        "Origin",
        [ (dirtyId obj, PopAlphaNumeric . tshow . origin $ obj)
          | obj :: ObjectDef <- instanceList fSpec,
            origin obj `notElem` [OriginUnknown, MeatGrinder]
        ]
      ),
      ( "operator",
        "BinaryTerm",
        "Operator",
        [ (dirtyId expr, PopAlphaNumeric . tshow $ op)
          | expr :: Expression <- instanceList fSpec,
            Just op <- [binOp expr]
        ]
      ),
      ( "operator",
        "UnaryTerm",
        "Operator",
        [ (dirtyId expr, PopAlphaNumeric . tshow $ op)
          | expr :: Expression <- instanceList fSpec,
            Just op <- [unaryOp expr]
        ]
      ),
      ( "origin",
        "Rule",
        "Origin",
        [ (dirtyId rul, PopAlphaNumeric . tshow . origin $ rul)
          | rul :: Rule <- instanceList fSpec,
            origin rul `notElem` [OriginUnknown, MeatGrinder]
        ]
      ),
      ( "rrviol",
        "Rule",
        "PairView",
        [ (dirtyId rul, dirtyId pv)
          | rul@Ru {rrviol = Just pv} :: Rule <- instanceList fSpec
        ]
      ),
      ( "prop",
        "Relation",
        "Property",
        [ (dirtyId rel, PopAlphaNumeric . tshow $ prop)
          | rel :: Relation <- instanceList fSpec,
            prop <- Set.elems $ decprps rel
        ]
      ),
      ( "purpose",
        "Concept",
        "Purpose",
        [ (dirtyId cpt, dirtyId purp)
          | cpt :: A_Concept <- instanceList fSpec,
            purp <- purposes fSpec cpt
        ]
      ),
      ( "purpose",
        "Context",
        "Purpose",
        [ (dirtyId ctx, dirtyId purp)
          | ctx :: A_Context <- instanceList fSpec,
            purp <- purposes fSpec ctx
        ]
      ),
      -- ( "purpose",
      --   "IdentityRule",
      --   "Purpose",
      --
      --   [ (dirtyId idn, dirtyId purp)
      --     | idn :: IdentityRule <- instanceList fSpec,
      --       purp <- purposes fSpec idn
      --   ]
      -- ),
      ( "purpose",
        "Interface",
        "Purpose",
        [ (dirtyId ifc, dirtyId purp)
          | ifc :: Interface <- instanceList fSpec,
            purp <- purposes fSpec ifc
        ]
      ),
      ( "purpose",
        "Pattern",
        "Purpose",
        [ (dirtyId pat, dirtyId purp)
          | pat :: Pattern <- instanceList fSpec,
            purp <- purposes fSpec pat
        ]
      ),
      ( "purpose",
        "Relation",
        "Purpose",
        [ (dirtyId rel, dirtyId purp)
          | rel :: Relation <- instanceList fSpec,
            purp <- purposes fSpec rel
        ]
      ),
      ( "purpose",
        "Rule",
        "Purpose",
        [ (dirtyId rul, dirtyId purp)
          | rul :: Rule <- instanceList fSpec,
            purp <- purposes fSpec rul
        ]
      ),
      ( "purpose",
        "View",
        "Purpose",
        [ (dirtyId vw, dirtyId purp)
          | vw :: ViewDef <- instanceList fSpec,
            purp <- purposes fSpec vw
        ]
      ),
      -- -- ( "qConjuncts",
      -- --   "Quad",
      -- --   "Conjunct",
      -- --
      -- --   [ (dirtyId quad, dirtyId conj)
      -- --     | quad <- vquads fSpec,
      -- --       conj <- NE.toList (qConjuncts quad)
      -- --   ] --TODO
      -- -- ),
      -- ( "qDcl",
      --   "Quad",
      --   "Relation",
      --
      --   [ (dirtyId quad, dirtyId (qDcl quad))
      --     | quad <- vquads fSpec
      --   ] --TODO
      -- ),
      -- ( "qRule",
      --   "Quad",
      --   "Rule",
      --
      --   [ (dirtyId quad, dirtyId (qRule quad))
      --     | quad <- vquads fSpec
      --   ] --TODO
      -- ),
      -- ( "rc_orgRules",
      --   "Conjunct",
      --   "Rule",
      --
      --   [ (dirtyId conj, dirtyId rul)
      --     | conj :: Conjunct <- instanceList fSpec,
      --       rul <- NE.toList $ rc_orgRules conj
      --   ]
      -- ),
      ( "second",
        "BinaryTerm",
        "Term",
        [ (dirtyId expr, dirtyId x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [second expr]
        ]
      ),
      ( "segment",
        "PairView",
        "PairViewSegment",
        [ (dirtyId pv, dirtyId pvs)
          | pv :: PairView Expression <- instanceList fSpec,
            pvs <- NE.toList (ppv_segs pv)
        ]
      ),
      ( "sequenceNr",
        "PairViewSegment",
        "SequenceNumber",
        [ (dirtyId pvs, PopInt nr)
          | pv :: PairView Expression <- instanceList fSpec,
            (pvs, nr) <- zip (NE.toList . ppv_segs $ pv) [0 ..]
        ]
      ),
      -- ( "sessAtom",
      --   "SESSION",
      --   "Atom",
      --
      --   [] -- This goes too deep. Keep it empty.
      -- ),
      -- ( "sessIfc",
      --   "SESSION",
      --   "Interface",
      --
      --   [] --TODO
      -- ),
      -- ( "sessionRole",
      --   "SESSION",
      --   "Role",
      --
      --   [] --TODO
      -- ),
      ( "showADL",
        "Term",
        "ShowADL",
        [ (dirtyId expr, PopAlphaNumeric (showA expr))
          | expr :: Expression <- instanceList fSpec
        ]
      ),
      ( "sign",
        "Term",
        "Signature",
        [ (dirtyId expr, dirtyId (sign expr))
          | expr :: Expression <- instanceList fSpec
        ]
      ),
      ( "sign",
        "Relation",
        "Signature",
        [ (dirtyId rel, dirtyId (sign rel))
          | rel :: Relation <- instanceList fSpec
        ]
      ),
      ( "singleton",
        "Singleton",
        "AtomValue",
        [ (dirtyId expr, dirtyId x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [singleton expr]
        ]
      ),
      ( "source",
        "Relation",
        "Concept",
        [ (dirtyId rel, dirtyId (source rel))
          | rel :: Relation <- instanceList fSpec
        ]
      ),
      ( "src",
        "Signature",
        "Concept",
        [ (dirtyId sgn, dirtyId (source sgn))
          | sgn :: Signature <- instanceList fSpec
        ]
      ),
      ( "srcOrTgt",
        "PairViewSegment",
        "SourceOrTarget",
        [ (dirtyId pvs, PopAlphaNumeric . tshow . pvsSoT $ pvs)
          | pvs@PairViewExp {} :: PairViewSegment Expression <- instanceList fSpec
        ]
      ),
      ( "target",
        "Relation",
        "Concept",
        [ (dirtyId rel, dirtyId (target rel))
          | rel :: Relation <- instanceList fSpec
        ]
      ),
      ( "text",
        "PairViewSegment",
        "String",
        [ (dirtyId pvs, PopAlphaNumeric . pvsStr $ pvs)
          | pvs@PairViewText {} :: PairViewSegment Expression <- instanceList fSpec
        ]
      ),
      ( "tgt",
        "Signature",
        "Concept",
        [ (dirtyId sgn, dirtyId (target sgn))
          | sgn :: Signature <- instanceList fSpec
        ]
      ),
      -- ( "ttype",
      --   "Concept",
      --   "TType",
      --
      --   [ (dirtyId cpt, PopAlphaNumeric . tshow . cptTType fSpec $ cpt)
      --     | cpt :: A_Concept <- instanceList fSpec
      --   ]
      -- ),
      ( "udefrules",
        "Rule",
        "Context",
        [ (dirtyId rul, dirtyId ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rul <- Set.elems $ udefrules ctx
        ]
      ),
      ( "udefrules",
        "Rule",
        "Pattern",
        [ (dirtyId rul, dirtyId pat)
          | pat :: Pattern <- instanceList fSpec,
            rul <- Set.elems $ udefrules pat
        ]
      ),
      ( "urlEncodedName",
        "Concept",
        "EncodedName",
        [ (dirtyId cpt, PopAlphaNumeric . urlEncodedName . name $ cpt)
          | cpt :: A_Concept <- instanceList fSpec
        ]
      ),
      ( "urlEncodedName",
        "Pattern",
        "EncodedName",
        [ (dirtyId pat, PopAlphaNumeric . urlEncodedName . name $ pat)
          | pat :: Pattern <- instanceList fSpec
        ]
      ),
      ( "urlEncodedName",
        "Rule",
        "EncodedName",
        [ (dirtyId rul, PopAlphaNumeric . urlEncodedName . name $ rul)
          | rul :: Rule <- instanceList fSpec
        ]
      ),
      ( "usedIn",
        "Relation",
        "Term",
        [ (dirtyId rel, dirtyId expr)
          | expr :: Expression <- instanceList fSpec,
            rel :: Relation <- Set.elems $ bindedRelationsIn expr
        ]
      ),
      ( "userCpt",
        "Epsilon",
        "Concept",
        [ (dirtyId expr, dirtyId x)
          | expr :: Expression <- instanceList fSpec,
            Just (x :: A_Concept) <- [userCpt expr]
        ]
      ),
      ( "userSrc",
        "V",
        "Concept",
        [ (dirtyId expr, dirtyId x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [userSrc expr]
        ]
      ),
      ( "userTgt",
        "V",
        "Concept",
        [ (dirtyId expr, dirtyId x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [userTgt expr]
        ]
      ),
      -- ( "vdats",
      --   "View",
      --   "ViewSegment",
      --
      --   [ (dirtyId vd, PopAlphaNumeric . tshow $ vs)
      --     | vd :: ViewDef <- instanceList fSpec,
      --       vs <- vdats vd
      --   ]
      -- ),
      -- ( "vdcpt",
      --   "View",
      --   "Concept",
      --
      --   [ (dirtyId vd, PopAlphaNumeric . tshow . vdcpt $ vd)
      --     | vd :: ViewDef <- instanceList fSpec,
      --       vdIsDefault vd
      --   ]
      -- ),
      -- ( "vdhtml",
      --   "View",
      --   "Concept",
      --
      --   [ (dirtyId vd, PopAlphaNumeric . tshow $ html)
      --     | vd :: ViewDef <- instanceList fSpec,
      --       Just html <- [vdhtml vd]
      --   ]
      -- ),
      -- ( "vdIsDefault",
      --   "View",
      --   "Concept",
      --
      --   [ (dirtyId vd, PopAlphaNumeric . tshow . vdcpt $ vd)
      --     | vd :: ViewDef <- instanceList fSpec
      --   ]
      -- ),
      -- ( "vdpos",
      --   "View",
      --   "Origin",
      --
      --   [ (dirtyId vd, PopAlphaNumeric . tshow . origin $ vd)
      --     | vd :: ViewDef <- instanceList fSpec,
      --       origin vd `notElem` [OriginUnknown, MeatGrinder]
      --   ]
      -- ),
      ( "versionInfo",
        "Context",
        "AmpersandVersion",
        [ (dirtyId ctx, PopAlphaNumeric (longVersion appVersion))
          | ctx :: A_Context <- instanceList fSpec
        ]
      ),
      --      ( "viewBy",
      --        "Concept",
      --        "Concept",
      --
      --        [] --TODO
      --      ),
      ( "violatable",
        "Interface",
        "Rule",
        [] --TODO future work
      )
    ]
    <> tmpNewTransformerDefsFA fSpec

dirtyIdWithoutType :: Unique a => a -> PopAtom
dirtyIdWithoutType = DirtyId . idWithoutType

-- | The following transformers provide the metamodel needed to run a prototype.
--   Note: The information in transformersPrototypeContext is fully contained in FormalAmpersand.
--   You might do this by dropping all prefixes "PF_" and "pf_" and doing
--   the following transformation:
--     label[Role*PF_Label]                -> name[Role*RoleName]
--   Then you will see that the transformers defined here are a subset of the FormalAmpersand transformers.
transformersPrototypeContext :: FSpec -> [Transformer]
transformersPrototypeContext fSpec =
  map
    toTransformer
    -- the following transformer is also contained in FormalAmpersand.
    [ ( "isAPI",
        "PF_Interface",
        "PF_Interface",
        [ (dirtyIdWithoutType ifc, dirtyIdWithoutType ifc)
          | ifc :: Interface <- instanceList fSpec,
            ifcIsAPI ifc
        ]
      ),
      -- the following transformer can be calculated by the Exec Engine.
      -- it is also contained in FormalAmpersand.
      ( "isPublic",
        "PF_Interface",
        "PF_Interface",
        [ (dirtyIdWithoutType ifc, dirtyIdWithoutType ifc)
          | ifc :: Interface <- instanceList fSpec,
            null (ifcRoles ifc)
        ]
      ),
      -- the following transformer is also contained in FormalAmpersand.
      ( "label",
        "PF_Interface",
        "PF_Label",
        [ (dirtyIdWithoutType ifc, PopAlphaNumeric . name $ ifc)
          | ifc :: Interface <- instanceList fSpec
        ]
      ),
      -- the following transformer is called name[Role*RoleName] in FormalAmpersand
      ( "label",
        "Role",
        "PF_Label",
        [ (dirtyIdWithoutType role, PopAlphaNumeric . name $ role)
          | role :: Role <- instanceList fSpec
        ]
      ),
      -- the following transformer is called ifcRoles[Interface*Role] in FormalAmpersand
      ( "pf_ifcRoles",
        "PF_Interface",
        "Role",
        [ (dirtyIdWithoutType ifc, dirtyIdWithoutType role)
          | ifc :: Interface <- instanceList fSpec,
            role <- ifcRoles ifc
        ]
      ),
      ( "ifc",
        "PF_NavMenuItem",
        "PF_Interface",
        [] --TODO
      ),
      ( "label",
        "PF_NavMenuItem",
        "PF_Label",
        [] --TODO
      ),
      ( "isSubItemOf",
        "PF_NavMenuItem",
        "PF_NavMenuItem",
        [] --TODO
      ),
      ( "isVisible",
        "PF_NavMenuItem",
        "PF_NavMenuItem",
        [] --TODO
      ),
      ( "isPartOf",
        "PF_NavMenuItem",
        "PF_NavMenu",
        [] --TODO
      ),
      ( "seqNr",
        "PF_NavMenuItem",
        "PF_SeqNr",
        [] --TODO
      ),
      ( "url",
        "PF_NavMenuItem",
        "PF_URL",
        [] --TODO
      ),
      ( "pf_navItemRoles",
        "PF_NavMenuItem",
        "Role",
        [] --TODO
      ),
      ( "lastAccess",
        "SESSION",
        "DateTime",
        [] --TODO
      ),
      ( "sessionActiveRoles",
        "SESSION",
        "Role",
        [] --TODO
      ),
      ( "sessionAllowedRoles",
        "SESSION",
        "Role",
        [] --TODO
      )
    ]

class Instances a => HasPurpose a where
  purposes :: FSpec -> a -> [Purpose]
  purposes fSpec a =
    Set.toList . Set.filter (isFor a) . instances $ fSpec
  isFor :: a -> Purpose -> Bool

instance HasPurpose A_Concept where
  isFor cpt purp =
    case explObj purp of
      ExplConcept x -> cpt == x
      _ -> False

instance HasPurpose A_Context where
  isFor ctx purp =
    case explObj purp of
      ExplContext x -> name ctx == x
      _ -> False

instance HasPurpose Relation where
  isFor rel purp =
    case explObj purp of
      ExplRelation x -> rel == x
      _ -> False

instance HasPurpose IdentityRule where
  isFor idf purp =
    case explObj purp of
      ExplInterface x -> name idf == x
      _ -> False

instance HasPurpose Interface where
  isFor ifc purp =
    case explObj purp of
      ExplInterface x -> name ifc == x
      _ -> False

instance HasPurpose Pattern where
  isFor pat purp =
    case explObj purp of
      ExplPattern x -> name pat == x
      _ -> False

instance HasPurpose Rule where
  isFor rul purp =
    case explObj purp of
      ExplRule x -> name rul == x
      _ -> False

instance HasPurpose ViewDef where
  isFor vw purp =
    case explObj purp of
      ExplViewDef x -> name vw == x
      _ -> False

data ExprInfo = ExprInfo
  { binOp' :: Maybe BinOp,
    unaryOp' :: Maybe UnaryOp,
    bindedRel' :: Maybe Relation,
    first' :: Maybe Expression,
    second' :: Maybe Expression,
    arg' :: Maybe Expression,
    userCpt' :: Maybe A_Concept, -- the concept of an Epsilon (and thus I too) Expression
    userSrc' :: Maybe A_Concept, -- the source concept of a V Expression
    userTgt' :: Maybe A_Concept, -- the target concept of a V Expression
    singleton' :: Maybe PAtomValue -- the value of a singleton Expression
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
    (EEqu (l, r)) ->
      ExprInfo
        { binOp' = Just Equivalence,
          unaryOp' = Nothing,
          bindedRel' = Nothing,
          first' = Just l,
          second' = Just r,
          arg' = Nothing,
          userCpt' = Nothing,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (EInc (l, r)) ->
      ExprInfo
        { binOp' = Just Inclusion,
          unaryOp' = Nothing,
          bindedRel' = Nothing,
          first' = Just l,
          second' = Just r,
          arg' = Nothing,
          userCpt' = Nothing,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (EIsc (l, r)) ->
      ExprInfo
        { binOp' = Just Equivalence,
          unaryOp' = Nothing,
          bindedRel' = Nothing,
          first' = Just l,
          second' = Just r,
          arg' = Nothing,
          userCpt' = Nothing,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (EUni (l, r)) ->
      ExprInfo
        { binOp' = Just Union,
          unaryOp' = Nothing,
          bindedRel' = Nothing,
          first' = Just l,
          second' = Just r,
          arg' = Nothing,
          userCpt' = Nothing,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (EDif (l, r)) ->
      ExprInfo
        { binOp' = Just Difference,
          unaryOp' = Nothing,
          bindedRel' = Nothing,
          first' = Just l,
          second' = Just r,
          arg' = Nothing,
          userCpt' = Nothing,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (ELrs (l, r)) ->
      ExprInfo
        { binOp' = Just LeftResidu,
          unaryOp' = Nothing,
          bindedRel' = Nothing,
          first' = Just l,
          second' = Just r,
          arg' = Nothing,
          userCpt' = Nothing,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (ERrs (l, r)) ->
      ExprInfo
        { binOp' = Just RightResidu,
          unaryOp' = Nothing,
          bindedRel' = Nothing,
          first' = Just l,
          second' = Just r,
          arg' = Nothing,
          userCpt' = Nothing,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (EDia (l, r)) ->
      ExprInfo
        { binOp' = Just Diamond,
          unaryOp' = Nothing,
          bindedRel' = Nothing,
          first' = Just l,
          second' = Just r,
          arg' = Nothing,
          userCpt' = Nothing,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (ECps (l, r)) ->
      ExprInfo
        { binOp' = Just Composition,
          unaryOp' = Nothing,
          bindedRel' = Nothing,
          first' = Just l,
          second' = Just r,
          arg' = Nothing,
          userCpt' = Nothing,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (ERad (l, r)) ->
      ExprInfo
        { binOp' = Just RelativeAddition,
          unaryOp' = Nothing,
          bindedRel' = Nothing,
          first' = Just l,
          second' = Just r,
          arg' = Nothing,
          userCpt' = Nothing,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (EPrd (l, r)) ->
      ExprInfo
        { binOp' = Just CartesianProduct,
          unaryOp' = Nothing,
          bindedRel' = Nothing,
          first' = Just l,
          second' = Just r,
          arg' = Nothing,
          userCpt' = Nothing,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (EKl0 e) ->
      ExprInfo
        { binOp' = Nothing,
          unaryOp' = Just KleeneStar,
          bindedRel' = Nothing,
          first' = Nothing,
          second' = Nothing,
          arg' = Just e,
          userCpt' = Nothing,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (EKl1 e) ->
      ExprInfo
        { binOp' = Nothing,
          unaryOp' = Just KleenePlus,
          bindedRel' = Nothing,
          first' = Nothing,
          second' = Nothing,
          arg' = Just e,
          userCpt' = Nothing,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (EFlp e) ->
      ExprInfo
        { binOp' = Nothing,
          unaryOp' = Just Converse,
          bindedRel' = Nothing,
          first' = Nothing,
          second' = Nothing,
          arg' = Just e,
          userCpt' = Nothing,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (ECpl e) ->
      ExprInfo
        { binOp' = Nothing,
          unaryOp' = Just UnaryMinus,
          bindedRel' = Nothing,
          first' = Nothing,
          second' = Nothing,
          arg' = Just e,
          userCpt' = Nothing,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (EBrk e) ->
      ExprInfo
        { binOp' = binOp e,
          unaryOp' = unaryOp e,
          bindedRel' = bindedRel e,
          first' = first e,
          second' = second e,
          arg' = arg e,
          userCpt' = userCpt e,
          userSrc' = userSrc e,
          userTgt' = userTgt e,
          singleton' = singleton e
        }
    (EDcD r) ->
      ExprInfo
        { binOp' = Nothing,
          unaryOp' = Nothing,
          bindedRel' = Just r,
          first' = Nothing,
          second' = Nothing,
          arg' = Nothing,
          userCpt' = Nothing,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (EDcI cpt) ->
      ExprInfo
        { binOp' = Nothing,
          unaryOp' = Nothing,
          bindedRel' = Nothing,
          first' = Nothing,
          second' = Nothing,
          arg' = Nothing,
          userCpt' = Just cpt,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (EEps cpt _) ->
      ExprInfo
        { binOp' = Nothing,
          unaryOp' = Nothing,
          bindedRel' = Nothing,
          first' = Nothing,
          second' = Nothing,
          arg' = Nothing,
          userCpt' = Just cpt,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (EDcV sgn) ->
      ExprInfo
        { binOp' = Nothing,
          unaryOp' = Nothing,
          bindedRel' = Nothing,
          first' = Nothing,
          second' = Nothing,
          arg' = Nothing,
          userCpt' = Nothing,
          userSrc' = Just (source sgn),
          userTgt' = Just (target sgn),
          singleton' = Nothing
        }
    (EMp1 val _) ->
      ExprInfo
        { binOp' = Nothing,
          unaryOp' = Nothing,
          bindedRel' = Nothing,
          first' = Nothing,
          second' = Nothing,
          arg' = Nothing,
          userCpt' = Nothing,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Just val
        }

data UnaryOp
  = KleeneStar
  | KleenePlus
  | Converse
  | UnaryMinus
  | Bracket
  deriving (Eq, Show, Typeable)

instance Unique UnaryOp where
  showUnique = tshow

data BinOp
  = CartesianProduct
  | Composition
  | Diamond
  | Difference
  | Equivalence
  | Inclusion
  | Intersection
  | LeftResidu
  | RightResidu
  | RelativeAddition
  | Union
  deriving (Eq, Show, Typeable)

instance Unique BinOp where
  showUnique = tshow

instance Unique (Either BinOp UnaryOp) where
  showUnique (Left a) = showUnique a
  showUnique (Right b) = showUnique b

-- TODO: fix FormalAmpersand to get Pragma inside it in a proper way.
decprL, decprM, decprR :: Relation -> Text
decprL = maybe "" praLeft . decpr
decprM = maybe "" praMid . decpr
decprR = maybe "" praRight . decpr

tmpNewTransformerDefsFA :: FSpec -> [Transformer]
tmpNewTransformerDefsFA fSpec =
  map
    toTransformer
    [ ( "cddef2",
        "ConceptDef",
        "Meaning",
        [] --TODO
      ),
      ( "context",
        "Rule",
        "Context",
        [] --TODO
      ),
      ( "edit",
        "FieldDef",
        "Relation",
        [] --TODO Future work
      ),
      ( "editFlp",
        "FieldDef",
        "Relation",
        [] --TODO
      ),
      ( "fst",
        "CombineStrings",
        "String",
        [] --TODO
      ),
      ( "ifcQuads",
        "Interface",
        "Quad",
        [] --TODO future work
      ),
      ( "interfaces",
        "Context",
        "Interface",
        [ (dirtyId ctx, dirtyId ifc)
          | ctx :: A_Context <- instanceList fSpec,
            ifc :: Interface <- instanceList fSpec
        ]
      ),
      ( "interfaces",
        "Role",
        "Interface",
        [ (dirtyId rol, dirtyId ifc)
          | ifc :: Interface <- instanceList fSpec,
            rol <- ifcRoles ifc
        ]
      ),
      ( "isaCopy",
        "Concept",
        "Concept",
        [] --TODO
      ),
      ( "isaPlus",
        "Concept",
        "Concept",
        [] --TODO
      ),
      ( "isaRfx",
        "Concept",
        "Concept",
        [] --TODO
      ),
      ( "isaRfxCopy",
        "Concept",
        "Concept",
        [] --TODO
      ),
      ( "isaRfxPlus",
        "Concept",
        "Concept",
        [] --TODO
      ),
      ( "isaRfxStar",
        "Concept",
        "Concept",
        [] --TODO
      ),
      ( "isaStar",
        "Concept",
        "Concept",
        [] --TODO
      ),
      ( "originatesFrom",
        "Conjunct",
        "Rule",
        [] --TODO
      ),
      ( "patRules",
        "Pattern",
        "Rule",
        [ (dirtyId pat, dirtyId rul)
          | pat :: Pattern <- instanceList fSpec,
            rul <- Set.toList (ptrls pat)
        ]
      ),
      ( "result",
        "CombineStrings",
        "String",
        [] --TODO
      ),
      ( "snd",
        "CombineStrings",
        "String",
        [] --TODO
      ),
      ( "uses",
        "Context",
        "Pattern",
        [] --TODO Future work
      ),
      ( "valid",
        "Concept",
        "Context",
        [] --TODO Future work
      ),
      ( "valid",
        "Relation",
        "Context",
        [] --TODO Future work
      ),
      ( "valid",
        "Rule",
        "Context",
        [] --TODO Future work
      ),
      ( "siHeader",
        "Box",
        "BoxHeader",
        [ (dirtyId box, dirtyId (siHeader box))
          | box@Box {} <- instanceList fSpec
        ]
      ),
      ( "siConcept",
        "Box",
        "Concept",
        [ (dirtyId box, dirtyId (siConcept box))
          | box@Box {} <- instanceList fSpec
        ]
      ),
      ( "objcrud",
        "ObjectDef",
        "Crud",
        [] --TODO HAN invuloefening
      ),
      ( "objSub",
        "ObjectDef",
        "SubInterface",
        [ (dirtyId si, dirtyId od)
          | od :: ObjectDef <- instanceList fSpec,
            Just si <- [objmsub od]
        ]
      ),
      ( "objTerm",
        "ObjectDef",
        "Term",
        [ (dirtyId od, dirtyId (objExpression od))
          | od :: ObjectDef <- instanceList fSpec
        ]
      ),
      ( "origin",
        "SubInterface",
        "Origin",
        [ (dirtyId si, PopAlphaNumeric . tshow . origin $ si)
          | si :: SubInterface <- instanceList fSpec
        ]
      ),
      ( "siObjs",
        "Box",
        "BoxItem",
        [] --TODO HAN invuloefening
      ),
      ( "isLink",
        "InterfaceRef",
        "InterfaceRef",
        [ (dirtyId ref, dirtyId ref)
          | ref@InterfaceRef {} <- instanceList fSpec,
            siIsLink ref
        ]
      ),
      ( "references",
        "InterfaceRef",
        "Interface",
        [ (dirtyId ref, dirtyId ifc)
          | ref@InterfaceRef {} <- instanceList fSpec,
            ifc :: Interface <- instanceList fSpec,
            name ifc == siIfcId ref
        ]
      )
    ]
