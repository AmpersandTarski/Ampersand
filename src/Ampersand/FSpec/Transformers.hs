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
import Text.Pandoc.Shared (stringify)

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

dirtyId :: (Unique a) => a -> PopAtom
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
    [ ( "allConjuncts",
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
      ( "bind",
        "BindedRelation",
        "Relation",
        [ (dirtyId expr, dirtyId x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [bindedRel expr]
        ]
      ),
      ( "concept",
        "ConceptDef",
        "Concept",
        [ (dirtyId cd, dirtyId cpt)
          | cd :: AConceptDef <- instanceList fSpec,
            cpt :: A_Concept <- instanceList fSpec,
            name cpt == name cd
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
      ( "context",
        "Rule",
        "Context",
        [ (dirtyId rul, dirtyId ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rul :: Rule <- instanceList fSpec
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
      ( "declaredthrough",
        "PropertyRule",
        "Property",
        [ (dirtyId rul, (PopAlphaNumeric . tshow) prop)
          | ctx :: A_Context <- instanceList fSpec,
            rul <- Set.elems $ proprules ctx,
            Propty prop _ <- [rrkind rul]
        ]
      ),
      ( "decMean",
        "Relation",
        "Meaning",
        [ (dirtyId rel, dirtyId mean)
          | rel :: Relation <- instanceList fSpec,
            mean <- decMean rel
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
      ( "edit",
        "FieldDef",
        "Relation",
        [] -- TODO Future work
      ),
      ( "editFlp",
        "FieldDef",
        "Relation",
        [] -- TODO
      ),
      ( "explRefIds",
        "Purpose",
        "Text",
        [ (dirtyId prp, PopAlphaNumeric txt)
          | prp :: Purpose <- instanceList fSpec,
            txt <- explRefIds prp
        ]
      ),
      ( "explMarkup",
        "Purpose",
        "Markup",
        [ (dirtyId prp, dirtyId (explMarkup prp))
          | prp :: Purpose <- instanceList fSpec
        ]
      ),
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
      ( "fst",
        "CombineStrings",
        "String",
        [] -- TODO
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
        [] -- TODO future work
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
        [] -- TODO future work
      ),
      ( "ifcPos",
        "Interface",
        "Origin",
        [ (dirtyId ifc, popatom)
          | ifc :: Interface <- instanceList fSpec,
            Just popatom <- [originToPopAtom ifc]
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
      ( "ifcQuads",
        "Interface",
        "Quad",
        [] -- TODO future work
      ),
      ( "ifcRoles",
        "Interface",
        "Role",
        [ (dirtyId ifc, dirtyId rol)
          | ifc <- instanceList fSpec,
            rol <- ifcRoles ifc
        ]
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
      ( "isaCopy",
        "Concept",
        "Concept",
        [] -- TODO
      ),
      ( "isAPI",
        "Interface",
        "Interface",
        [ (dirtyId ifc, dirtyId ifc)
          | ifc :: Interface <- instanceList fSpec,
            ifcIsAPI ifc
        ]
      ),
      ( "isaPlus",
        "Concept",
        "Concept",
        [] -- TODO
      ),
      ( "isaRfx",
        "Concept",
        "Concept",
        [] -- TODO
      ),
      ( "isaRfxCopy",
        "Concept",
        "Concept",
        [] -- TODO
      ),
      ( "isaRfxPlus",
        "Concept",
        "Concept",
        [] -- TODO
      ),
      ( "isaRfxStar",
        "Concept",
        "Concept",
        [] -- TODO
      ),
      ( "isaStar",
        "Concept",
        "Concept",
        [] -- TODO
      ),
      ( "isLink",
        "InterfaceRef",
        "InterfaceRef",
        [ (dirtyId ref, dirtyId ref)
          | ref@InterfaceRef {} <- instanceList fSpec,
            siIsLink ref
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
      ( "label",
        "FieldDef",
        "FieldName",
        [ (dirtyId fld, PopAlphaNumeric (objnmOD obj))
          | obj :: ObjectDef <- instanceList fSpec,
            objnmOD obj /= "",
            fld <- fields obj
        ]
      ),
      ( "language",
        "Markup",
        "Language",
        [ (dirtyId mkp, PopAlphaNumeric . tshow . amLang $ mkp)
          | mkp :: Markup <- instanceList fSpec
        ]
      ),
      ( "maintains",
        "Role",
        "Rule",
        [ (dirtyId rol, dirtyId rul)
          | (rol, rul) <- fRoleRuls fSpec
        ]
      ),
      ( "markup",
        "Meaning",
        "Markup",
        [ (dirtyId mean, dirtyId . ameaMrk $ mean)
          | mean :: Meaning <- instanceList fSpec
        ]
      ),
      ( "meaning",
        "ConceptDef",
        "Meaning",
        [ (dirtyId cd, dirtyId mean)
          | cd :: AConceptDef <- instanceList fSpec,
            mean <- acddef2 cd : acdmean cd
        ]
      ),
      ( "meaning",
        "Rule",
        "Meaning",
        [ (dirtyId rul, dirtyId mean)
          | rul :: Rule <- instanceList fSpec,
            mean <- rrmean rul
        ]
      ),
      ( "message",
        "Rule",
        "Markup",
        [ (dirtyId rul, dirtyId mrkup)
          | rul :: Rule <- instanceList fSpec,
            mrkup <- rrmsg rul
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
      ( "objcruds",
        "ObjectDef",
        "Cruds",
        [ (dirtyId od, PopAlphaNumeric . tshow . objcrud $ od)
          | od :: ObjectDef <- instanceList fSpec
        ]
      ),
      ( "objDef",
        "BxExpr",
        "ObjectDef",
        [ (dirtyId item, dirtyId obj)
          | item@BxExpr {objE = obj} <- instanceList fSpec
        ]
      ),
      ( "objSub",
        "ObjectDef",
        "SubInterface",
        [ (dirtyId od, dirtyId si)
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
      ( "objView",
        "ObjectDef",
        "View",
        [ (dirtyId obj, PopAlphaNumeric vw)
          | obj :: ObjectDef <- instanceList fSpec,
            Just vw <- [objmView obj]
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
        "BoxItem",
        "Origin",
        [ (dirtyId item, popatom)
          | item :: BoxItem <- instanceList fSpec,
            Just popatom <- [originToPopAtom item]
        ]
      ),
      ( "origin",
        "ObjectDef",
        "Origin",
        [ (dirtyId obj, popatom)
          | obj :: ObjectDef <- instanceList fSpec,
            Just popatom <- [originToPopAtom obj]
        ]
      ),
      ( "origin",
        "Purpose",
        "Origin",
        [ (dirtyId prp, popatom)
          | prp :: Purpose <- instanceList fSpec,
            Just popatom <- [originToPopAtom prp]
        ]
      ),
      ( "origin",
        "Rule",
        "Origin",
        [ (dirtyId rul, popatom)
          | rul :: Rule <- instanceList fSpec,
            Just popatom <- [originToPopAtom rul]
        ]
      ),
      ( "origin",
        "SubInterface",
        "Origin",
        [ (dirtyId si, popatom)
          | si :: SubInterface <- instanceList fSpec,
            Just popatom <- [originToPopAtom si]
        ]
      ),
      ( "originatesFrom",
        "Conjunct",
        "Rule",
        [] -- TODO
      ),
      ( "patRules",
        "Pattern",
        "Rule",
        [ (dirtyId pat, dirtyId rul)
          | pat :: Pattern <- instanceList fSpec,
            rul <- Set.toList (ptrls pat)
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
      ( "propertyRule",
        "Relation",
        "PropertyRule",
        [ (dirtyId rel, dirtyId rul)
          | ctx :: A_Context <- instanceList fSpec,
            rul <- Set.elems $ proprules ctx,
            Propty _ rel <- [rrkind rul]
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
      ( "pvsExp",
        "PairViewSegment",
        "Term",
        [ (dirtyId pvs, dirtyId (pvsExp pvs))
          | pvs@PairViewExp {} :: PairViewSegment Expression <- instanceList fSpec
        ]
      ),
      ( "rc_conjunct",
        "Conjunct",
        "Term",
        [ (dirtyId conj, dirtyId (rc_conjunct conj))
          | conj :: Conjunct <- instanceList fSpec
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
      ),
      ( "relsDefdIn",
        "Relation",
        "Context", ---contains ALL relations defined in this context
        [ (dirtyId rel, dirtyId ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rel :: Relation <- Set.elems $ relsDefdIn ctx
        ]
      ),
      ( "result",
        "CombineStrings",
        "String",
        [] -- TODO
      ),
      ( "rrviol",
        "Rule",
        "PairView",
        [ (dirtyId rul, dirtyId pv)
          | rul@Ru {rrviol = Just pv} :: Rule <- instanceList fSpec
        ]
      ),
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
      ( "showADL",
        "PairView",
        "ShowADL",
        [ (dirtyId pv, PopAlphaNumeric (showA pv))
          | pv :: PairView Expression <- instanceList fSpec
        ]
      ),
      ( "showADL",
        "Term",
        "ShowADL",
        [ (dirtyId expr, PopAlphaNumeric (showA expr))
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
      ( "sign",
        "Term",
        "Signature",
        [ (dirtyId expr, dirtyId (sign expr))
          | expr :: Expression <- instanceList fSpec
        ]
      ),
      ( "singleton",
        "Singleton",
        "AtomValue",
        [ (dirtyId expr, PopAlphaNumeric (tshow x))
          | expr :: Expression <- instanceList fSpec,
            Just x <- [singleton expr]
        ]
      ),
      ( "siConcept",
        "Box",
        "Concept",
        [ (dirtyId box, dirtyId (siConcept box))
          | box@Box {} <- instanceList fSpec
        ]
      ),
      ( "siHeader",
        "Box",
        "BoxHeader",
        [ (dirtyId box, dirtyId (siHeader box))
          | box@Box {} <- instanceList fSpec
        ]
      ),
      ( "siObjs",
        "Box",
        "BoxItem",
        [ (dirtyId bx, dirtyId item)
          | bx@Box {} <- instanceList fSpec,
            item <- siObjs bx
        ]
      ),
      ( "snd",
        "CombineStrings",
        "String",
        [] -- TODO
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
        "BxTxt",
        "Text",
        [ (dirtyId item, PopAlphaNumeric (objtxt x))
          | item@BxTxt {objT = x} <- instanceList fSpec
        ]
      ),
      ( "text",
        "Markup",
        "MarkupText",
        [ (dirtyId mkp, PopAlphaNumeric . stringify . amPandoc $ mkp)
          | mkp :: Markup <- instanceList fSpec
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
      ( "versionInfo",
        "Context",
        "AmpersandVersion",
        [ (dirtyId ctx, PopAlphaNumeric (longVersion appVersion))
          | ctx :: A_Context <- instanceList fSpec
        ]
      ),
      ( "violatable",
        "Interface",
        "Rule",
        [] -- TODO future work
      ),
      ( "uses",
        "Context",
        "Pattern",
        [ (dirtyId ctx, dirtyId pat)
          | ctx :: A_Context <- instanceList fSpec,
            pat :: Pattern <- instanceList fSpec
        ]
      ),
      ( "valid",
        "Concept",
        "Context",
        [] -- TODO Future work
      ),
      ( "valid",
        "Relation",
        "Context",
        [] -- TODO Future work
      ),
      ( "valid",
        "Rule",
        "Context",
        [] -- TODO Future work
      )
    ]

dirtyIdWithoutType :: (Unique a) => a -> PopAtom
dirtyIdWithoutType = DirtyId . idWithoutType

-- | The following transformers provide the metamodel needed to run a prototype.
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
        [] -- TODO
      ),
      ( "label",
        "PF_NavMenuItem",
        "PF_Label",
        [] -- TODO
      ),
      ( "isSubItemOf",
        "PF_NavMenuItem",
        "PF_NavMenuItem",
        [] -- TODO
      ),
      ( "isVisible",
        "PF_NavMenuItem",
        "PF_NavMenuItem",
        [] -- TODO
      ),
      ( "isPartOf",
        "PF_NavMenuItem",
        "PF_NavMenu",
        [] -- TODO
      ),
      ( "seqNr",
        "PF_NavMenuItem",
        "PF_SeqNr",
        [] -- TODO
      ),
      ( "url",
        "PF_NavMenuItem",
        "PF_URL",
        [] -- TODO
      ),
      ( "pf_navItemRoles",
        "PF_NavMenuItem",
        "Role",
        [] -- TODO
      ),
      ( "lastAccess",
        "SESSION",
        "DateTime",
        [] -- TODO
      ),
      ( "sessionActiveRoles",
        "SESSION",
        "Role",
        [] -- TODO
      ),
      ( "sessionAllowedRoles",
        "SESSION",
        "Role",
        [] -- TODO
      )
    ]

class (Instances a) => HasPurpose a where
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

originToPopAtom :: (Traced a) => a -> Maybe PopAtom
originToPopAtom x = case origin x of
  OriginUnknown -> Nothing
  OriginAtlas -> Nothing
  Origin txt -> Just (PopAlphaNumeric txt)
  PropertyRule {} -> standard
  FileLoc {} -> standard
  XLSXLoc {} -> standard
  MeatGrinder -> Nothing
  where
    standard = Just . PopAlphaNumeric . tshow . origin $ x
