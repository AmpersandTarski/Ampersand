{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Ampersand.FSpec.Transformers
  ( nameSpaceFormalAmpersand,
    transformersFormalAmpersand,
    transformersPrototypeContext,
    Transformer (..),
    PopAtom (..),
    instances,
  )
where

import Ampersand.ADL1
import Ampersand.Basics hiding (first, second)
import Ampersand.Classes
import Ampersand.Core.ShowAStruct (AStruct (showA))
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.Instances
import Data.Typeable (typeOf)
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import Text.Pandoc.Shared (stringify)

-- import qualified Text.Pandoc.Shared as P

-- | The function that retrieves the population of
--   some relation of Formal Ampersand of a given
--   ampersand script.
data Transformer = Transformer
  { tRel :: Name, -- name of relation
    tSrc :: Name, -- name of source
    tTrg :: Name, -- name of target
    tPairs :: [PAtomPair] -- the population of this relation from the user's script.
  }

-- | This datatype reflects the nature of an atom. It is use to construct
--   the atom.
data PopAtom
  = -- | Any Text. must be:
    --      * unique in the scope of the entire fspec
    --      * storable in a 255 database field
    DirtyId !Text1
  | -- | Intended to be observable by users. Not a 'dirty id'.
    PopAlphaNumeric !Text
  | PopInt !Integer
  deriving (Eq, Ord)

instance Show PopAtom where
  show x =
    case x of
      DirtyId str -> show str
      PopAlphaNumeric str -> show str
      PopInt i -> show i

dirtyId :: (Unique a) => a -> Maybe PopAtom
dirtyId x = DirtyId <$> idWithoutType x

dirtyId' :: (Unique e) => e -> PopAtom
dirtyId' x = case dirtyId x of
  Nothing -> fatal $ "Not a valid dirtyId could be generated: " <> tshow (typeOf x) <> ": " <> text1ToText (showUnique x)
  Just pa -> pa

dirtyIdWithoutType :: (Unique a) => a -> Maybe PopAtom
dirtyIdWithoutType x = DirtyId <$> idWithoutType x

dirtyIdWithoutType' :: (Unique e) => e -> PopAtom
dirtyIdWithoutType' x = case dirtyIdWithoutType x of
  Nothing -> fatal $ "Not a valid dirtyIdWithoutType could be generated: " <> tshow (typeOf x) <> ": " <> text1ToText (showUnique x)
  Just pa -> pa

-- Function for PrototypeContext transformers. These atoms don't need to have a type prefix
toTransformer :: (Text, Text, Text, [(PopAtom, PopAtom)]) -> Transformer
toTransformer (rel, src, tgt, tuples) =
  Transformer rel' src' tgt' tuples'
  where
    rel' = mkName RelationName $ toNameParts rel
    src' = mkName ConceptName $ toNameParts src
    tgt' = mkName ConceptName $ toNameParts tgt
    toNameParts :: Text -> NonEmpty NamePart
    toNameParts x = case T.uncons x of
      Nothing -> fatal $ "Not a valid NamePart: `" <> x <> "`"
      Just (h, tl) -> case catMaybes . toList $ toNamePart1 <$> splitOnDots (Text1 h tl) of
        [] -> fatal $ "Not a valid NamePart: `" <> x <> "`"
        h' : tl' -> h' :| tl'
    tuples' :: [PAtomPair]
    tuples' = map popAtomPair2PAtomPair tuples
    popAtomPair2PAtomPair (a, b) =
      PPair MeatGrinder (pAtom2AtomValue a) (pAtom2AtomValue b)
    pAtom2AtomValue :: PopAtom -> PAtomValue
    pAtom2AtomValue atm =
      case atm of
        DirtyId str -> ScriptString MeatGrinder (text1ToText str)
        PopAlphaNumeric str -> ScriptString MeatGrinder str
        PopInt i -> ScriptInt MeatGrinder i

nameSpaceFormalAmpersand :: NameSpace
nameSpaceFormalAmpersand =
  [ case toNamePart "FormalAmpersand" of
      Nothing -> fatal "Not a valid NamePart."
      Just np -> np
  ]

-- | The list of all transformers, one for each and every relation in Formal Ampersand.
transformersFormalAmpersand :: FSpec -> [Transformer]
transformersFormalAmpersand fSpec =
  map
    toTransformer
    [ {-
      -}
      ( "FormalAmpersand.allConjuncts",
        "FormalAmpersand.Context",
        "FormalAmpersand.Conjunct",
        [ (dirtyId' ctx, dirtyId' conj)
          | ctx :: A_Context <- instanceList fSpec,
            conj :: Conjunct <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.allRoles",
        "FormalAmpersand.Context",
        "FormalAmpersand.Role",
        [ (dirtyId' ctx, dirtyId' rol)
          | ctx :: A_Context <- instanceList fSpec,
            rol :: Role <- instanceList fSpec
        ]
      ),
      -- ( "FormalAmpersand.allRules",
      --   "FormalAmpersand.Pattern",
      --   "FormalAmpersand.Rule",
      --   {-Inj-}
      --   [ (dirtyId' pat, dirtyId' rul)
      --     | pat :: Pattern <- instanceList fSpec,
      --       rul :: Rule <- Set.elems $ allRules pat
      --   ]
      -- ),
      ( "FormalAmpersand.allRules",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Context",
        [ (dirtyId' rul, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rul :: Rule <- Set.elems $ allRules ctx
        ]
      ),
      ( "FormalAmpersand.arg",
        "FormalAmpersand.UnaryTerm",
        "FormalAmpersand.Term",
        [ (dirtyId' expr, dirtyId' x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [arg expr]
        ]
      ),
      -- ( "FormalAmpersand.asMarkdown",
      --   "FormalAmpersand.Markup",
      --   "FormalAmpersand.Text",
      --   [ (dirtyId' mrk, (PopAlphaNumeric . P.stringify . amPandoc) mrk)
      --     | mrk :: Markup <- instanceList fSpec
      --   ]
      -- ),
      ( "FormalAmpersand.bind",
        "FormalAmpersand.BindedRelation",
        "FormalAmpersand.Relation",
        [ (dirtyId' expr, dirtyId' x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [bindedRel expr]
        ]
      ),
      ( "FormalAmpersand.concepts",
        "FormalAmpersand.Pattern",
        "FormalAmpersand.Concept",
        [ (dirtyId' pat, dirtyId' cpt)
          | pat :: Pattern <- instanceList fSpec,
            cpt :: A_Concept <- Set.elems $ concs pat
        ]
      ),
      ( "FormalAmpersand.rcConjunct",
        "FormalAmpersand.Conjunct",
        "FormalAmpersand.Term",
        [ (dirtyId' conj, dirtyId' (rcConjunct conj))
          | conj :: Conjunct <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.context",
        "FormalAmpersand.Concept",
        "FormalAmpersand.Context",
        [ (dirtyId' cpt, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            cpt :: A_Concept <- Set.toList . concs $ ctx
        ]
      ),
      ( "FormalAmpersand.context",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Context",
        [ (dirtyId' ifc, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            ifc :: Interface <- ctxifcs ctx
        ]
      ),
      ( "FormalAmpersand.context",
        "FormalAmpersand.Isa",
        "FormalAmpersand.Context",
        [ (dirtyId' isa, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            isa@Isa {} <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.context",
        "FormalAmpersand.IsE",
        "FormalAmpersand.Context",
        [ (dirtyId' ise, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            ise@IsE {} <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.context",
        "FormalAmpersand.Pattern",
        "FormalAmpersand.Context",
        [ (dirtyId' pat, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            pat :: Pattern <- instanceList fSpec
        ]
      ),
      -- ( "FormalAmpersand.context",
      --   "FormalAmpersand.Population",
      --   "FormalAmpersand.Context",
      --   [ (dirtyId' pop, dirtyId' ctx)
      --     | ctx :: A_Context <- instanceList fSpec,
      --       pop :: Population <- instanceList fSpec
      --   ]
      -- ),
      -- ( "FormalAmpersand.ctxcds",
      --   "FormalAmpersand.ConceptDef",
      --   "FormalAmpersand.Context",
      --   [ (dirtyId' cdf, dirtyId' ctx)
      --     | ctx :: A_Context <- instanceList fSpec,
      --       cdf :: AConceptDef <- instanceList fSpec
      --   ]
      -- ),
      ( "FormalAmpersand.relsDefdIn",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Context", ---contains ALL relations defined in this context
        [ (dirtyId' rel, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rel :: Relation <- Set.elems $ relsDefdIn ctx
        ]
      ),
      ( "FormalAmpersand.ctxds",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Context",
        [ (dirtyId' rel, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rel :: Relation <- Set.elems $ ctxds ctx
        ]
      ),
      ( "FormalAmpersand.ctxrs",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Context",
        [ (dirtyId' rul, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rul :: Rule <- Set.elems . ctxrs $ ctx
        ]
      ),
      ( "FormalAmpersand.declaredIn",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Pattern",
        [ (dirtyId' rel, dirtyId' pat)
          | pat :: Pattern <- instanceList fSpec,
            rel :: Relation <- Set.elems $ relsDefdIn pat
        ]
      ),
      ( "FormalAmpersand.decMean",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Meaning",
        [ (dirtyId' rel, dirtyId' mean)
          | rel :: Relation <- instanceList fSpec,
            mean :: Meaning <- decMean rel
        ]
      ),
      ( "FormalAmpersand.decprL",
        "FormalAmpersand.Relation",
        "FormalAmpersand.String",
        [ (dirtyId' rel, (PopAlphaNumeric . decprL) rel)
          | rel :: Relation <- instanceList fSpec,
            (not . T.null . decprL) rel
        ]
      ),
      ( "FormalAmpersand.decprM",
        "FormalAmpersand.Relation",
        "FormalAmpersand.String",
        [ (dirtyId' rel, (PopAlphaNumeric . decprM) rel)
          | rel :: Relation <- instanceList fSpec,
            (not . T.null . decprM) rel
        ]
      ),
      ( "FormalAmpersand.decprR",
        "FormalAmpersand.Relation",
        "FormalAmpersand.String",
        [ (dirtyId' rel, (PopAlphaNumeric . decprR) rel)
          | rel :: Relation <- instanceList fSpec,
            (not . T.null . decprR) rel
        ]
      ),
      ( "FormalAmpersand.first",
        "FormalAmpersand.BinaryTerm",
        "FormalAmpersand.Term",
        [ (dirtyId' expr, dirtyId' x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [first expr]
        ]
      ),
      ( "FormalAmpersand.formalTerm",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Term",
        [ (dirtyId' rul, dirtyId' (formalExpression rul))
          | rul :: Rule <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.gengen",
        "FormalAmpersand.Isa",
        "FormalAmpersand.Concept",
        [ (dirtyId' isa, dirtyId' (gengen isa))
          | isa@Isa {} <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.gengen",
        "FormalAmpersand.IsE",
        "FormalAmpersand.Concept",
        [ (dirtyId' ise, dirtyId' cpt)
          | ise@IsE {} <- instanceList fSpec,
            cpt <- NE.toList $ genrhs ise
        ]
      ),
      ( "FormalAmpersand.genspc",
        "FormalAmpersand.IsE",
        "FormalAmpersand.Concept",
        [ (dirtyId' ise, dirtyId' (genspc ise))
          | ise@IsE {} <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.genspc",
        "FormalAmpersand.Isa",
        "FormalAmpersand.Concept",
        [ (dirtyId' isa, dirtyId' (genspc isa))
          | isa@Isa {} <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.identityRules",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Context",
        [ (dirtyId' rul, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rul <- Set.elems $ identityRules ctx
        ]
      ),
      ( "FormalAmpersand.identityRules",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Pattern",
        [ (dirtyId' rul, dirtyId' pat)
          | pat :: Pattern <- instanceList fSpec,
            rul <- Set.elems $ identityRules pat
        ]
      ),
      ( "FormalAmpersand.ifcConjuncts",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Conjunct",
        [ (dirtyId' ifc, dirtyId' conj)
          | ifc :: Interface <- instanceList fSpec,
            conj <- ifcConjuncts ifc
        ]
      ),
      ( "FormalAmpersand.ifcInputs",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Relation",
        [] -- TODO future work
      ),
      ( "FormalAmpersand.ifcObj",
        "FormalAmpersand.Interface",
        "FormalAmpersand.ObjectDef",
        [ (dirtyId' ifc, dirtyId' (ifcObj ifc))
          | ifc :: Interface <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.ifcOutputs",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Relation",
        [] -- TODO future work
      ),
      ( "FormalAmpersand.ifcPos",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Origin",
        [ (dirtyId' ifc, popatom)
          | ifc :: Interface <- instanceList fSpec,
            Just popatom <- [originToPopAtom ifc]
        ]
      ),
      ( "FormalAmpersand.ifcPurpose",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Purpose",
        [ (dirtyId' ifc, dirtyId' purp)
          | ifc :: Interface <- instanceList fSpec,
            purp <- purposes fSpec ifc
        ]
      ),
      ( "FormalAmpersand.ifcRoles",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Role",
        [ (dirtyId' ifc, dirtyId' rol)
          | ifc <- instanceList fSpec,
            rol <- ifcRoles ifc
        ]
      ),
      ( "FormalAmpersand.isAPI",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Interface",
        [ (dirtyId' ifc, dirtyId' ifc)
          | ifc :: Interface <- instanceList fSpec,
            ifcIsAPI ifc
        ]
      ),
      -- the following transformer can be calculated by the Exec Engine. So it can be removed here if so desired.
      ( "FormalAmpersand.isPublic",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Interface",
        [ (dirtyId' ifc, dirtyId' ifc)
          | ifc :: Interface <- instanceList fSpec,
            null (ifcRoles ifc)
        ]
      ),
      ( "FormalAmpersand.isa",
        "FormalAmpersand.Concept",
        "FormalAmpersand.Concept",
        [ (dirtyId' (genspc ise), dirtyId' gCpt)
          | ise@IsE {} <- instanceList fSpec,
            gCpt <- NE.toList $ genrhs ise
        ]
          ++ [ (dirtyId' (genspc isa), dirtyId' (gengen isa))
               | isa@Isa {} <- instanceList fSpec
             ]
      ),
      ( "FormalAmpersand.label",
        "FormalAmpersand.FieldDef",
        "FormalAmpersand.FieldName",
        [ ( dirtyId' fld,
            PopAlphaNumeric
              ( case objPlainName fld of
                  Nothing -> fatal "This should not happen, because only fields with a label are filtered."
                  Just lbl -> text1ToText lbl
              )
          )
          | obj :: ObjectDef <- instanceList fSpec,
            fld <- fields obj,
            isJust (objPlainName fld)
        ]
      ),
      -- ( "FormalAmpersand.language",
      --   "FormalAmpersand.Context",
      --   "FormalAmpersand.Language",
      --   [ (dirtyId' ctx, (PopAlphaNumeric . tshow . ctxlang) ctx)
      --     | ctx :: A_Context <- instanceList fSpec
      --   ]
      -- ),
      -- ( "FormalAmpersand.language",
      --   "FormalAmpersand.Markup",
      --   "FormalAmpersand.Language",
      --   [ (dirtyId' mrk, (PopAlphaNumeric . tshow . amLang) mrk)
      --     | mrk :: Markup <- instanceList fSpec
      --   ]
      -- ),
      ( "FormalAmpersand.maintains",
        "FormalAmpersand.Role",
        "FormalAmpersand.Rule",
        [ (dirtyId' rol, dirtyId' rul)
          | (rol, rul) <- fRoleRuls fSpec
        ]
      ),
      ( "FormalAmpersand.markup",
        "FormalAmpersand.Meaning",
        "FormalAmpersand.Markup",
        [ (dirtyId' mean, dirtyId' . ameaMrk $ mean)
          | mean :: Meaning <- instanceList fSpec
        ]
      ),
      -- ( "FormalAmpersand.markup",
      --   "FormalAmpersand.Purpose",
      --   "FormalAmpersand.Markup",
      --   [ (dirtyId' purp, dirtyId' . explMarkup $ purp)
      --     | purp :: Purpose <- instanceList fSpec
      --   ]
      -- ),
      ( "FormalAmpersand.meaning",
        "FormalAmpersand.ConceptDef",
        "FormalAmpersand.Meaning",
        [ (dirtyId' cd, dirtyId' mean)
          | cd :: AConceptDef <- instanceList fSpec,
            mean <- acddef2 cd : acdmean cd
        ]
      ),
      ( "FormalAmpersand.meaning",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Meaning",
        [ (dirtyId' rul, dirtyId' mean)
          | rul :: Rule <- instanceList fSpec,
            mean :: Meaning <- rrmean rul
        ]
      ),
      ( "FormalAmpersand.message",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Message",
        [ (dirtyId' rul, dirtyId' msg)
          | rul :: Rule <- instanceList fSpec,
            msg <- rrmsg rul
        ]
      ),
      ( "FormalAmpersand.propertyRule",
        "FormalAmpersand.Relation",
        "FormalAmpersand.PropertyRule",
        [ (dirtyId' rel, dirtyId' rul)
          | ctx :: A_Context <- instanceList fSpec,
            rul <- Set.elems $ proprules ctx,
            Propty _ rel <- [rrkind rul]
        ]
      ),
      ( "FormalAmpersand.declaredthrough",
        "FormalAmpersand.PropertyRule",
        "FormalAmpersand.Property",
        [ (dirtyId' rul, (PopAlphaNumeric . tshow) prop)
          | ctx :: A_Context <- instanceList fSpec,
            rul <- Set.elems $ proprules ctx,
            Propty prop _ <- [rrkind rul]
        ]
      ),
      ( "FormalAmpersand.name",
        "FormalAmpersand.Concept",
        "FormalAmpersand.ConceptName",
        [ (dirtyId' cpt, (PopAlphaNumeric . fullName) cpt)
          | cpt :: A_Concept <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.name",
        "FormalAmpersand.Context",
        "FormalAmpersand.ContextName",
        [ (dirtyId' ctx, (PopAlphaNumeric . fullName) ctx)
          | ctx :: A_Context <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.name",
        "FormalAmpersand.Interface",
        "FormalAmpersand.InterfaceName",
        [ (dirtyId' ifc, (PopAlphaNumeric . fullName) ifc)
          | ifc :: Interface <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.name",
        "FormalAmpersand.Pattern",
        "FormalAmpersand.PatternName",
        [ (dirtyId' pat, (PopAlphaNumeric . fullName) pat)
          | pat :: Pattern <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.name",
        "FormalAmpersand.Relation",
        "FormalAmpersand.RelationName",
        [ (dirtyId' rel, (PopAlphaNumeric . fullName) rel)
          | rel :: Relation <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.name",
        "FormalAmpersand.Role",
        "FormalAmpersand.RoleName",
        [ (dirtyId' rol, (PopAlphaNumeric . fullName) rol)
          | rol :: Role <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.name",
        "FormalAmpersand.Rule",
        "FormalAmpersand.RuleName",
        [ (dirtyId' rul, (PopAlphaNumeric . fullName) rul)
          | rul :: Rule <- instanceList fSpec
        ]
      ),
      -- ( "FormalAmpersand.name",
      --   "FormalAmpersand.View",
      --   "FormalAmpersand.ViewDefName",
      --   [ (dirtyId' vd, PopAlphaNumeric . fullName $ vd)
      --     | vd :: ViewDef <- instanceList fSpec
      --   ]
      -- ),
      ( "FormalAmpersand.objDef",
        "FormalAmpersand.BxExpr",
        "FormalAmpersand.ObjectDef",
        [ (dirtyId' item, dirtyId' obj)
          | item@BxExpr {objE = obj} <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.objView",
        "FormalAmpersand.ObjectDef",
        "FormalAmpersand.View",
        [ (dirtyId' obj, PopAlphaNumeric . fullName $ vw)
          | obj :: ObjectDef <- instanceList fSpec,
            Just vw <- [objmView obj]
        ]
      ),
      ( "FormalAmpersand.origin",
        "FormalAmpersand.ObjectDef",
        "FormalAmpersand.Origin",
        [ (dirtyId' obj, popatom)
          | obj :: ObjectDef <- instanceList fSpec,
            Just popatom <- [originToPopAtom obj]
        ]
      ),
      ( "FormalAmpersand.operator",
        "FormalAmpersand.BinaryTerm",
        "FormalAmpersand.Operator",
        [ (dirtyId' expr, PopAlphaNumeric . tshow $ op)
          | expr :: Expression <- instanceList fSpec,
            Just op <- [binOp expr]
        ]
      ),
      ( "FormalAmpersand.operator",
        "FormalAmpersand.UnaryTerm",
        "FormalAmpersand.Operator",
        [ (dirtyId' expr, PopAlphaNumeric . tshow $ op)
          | expr :: Expression <- instanceList fSpec,
            Just op <- [unaryOp expr]
        ]
      ),
      ( "FormalAmpersand.origin",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Origin",
        [ (dirtyId' rul, popatom)
          | rul :: Rule <- instanceList fSpec,
            Just popatom <- [originToPopAtom rul]
        ]
      ),
      ( "FormalAmpersand.rrviol",
        "FormalAmpersand.Rule",
        "FormalAmpersand.PairView",
        [ (dirtyId' rul, dirtyId' pv)
          | rul@Rule {rrviol = Just pv} :: Rule <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.prop",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Property",
        [ (dirtyId' rel, PopAlphaNumeric . tshow $ prop)
          | rel :: Relation <- instanceList fSpec,
            prop <- Set.elems $ decprps rel
        ]
      ),
      ( "FormalAmpersand.purpose",
        "FormalAmpersand.Concept",
        "FormalAmpersand.Purpose",
        [ (dirtyId' cpt, dirtyId' purp)
          | cpt :: A_Concept <- instanceList fSpec,
            purp <- purposes fSpec cpt
        ]
      ),
      ( "FormalAmpersand.purpose",
        "FormalAmpersand.Context",
        "FormalAmpersand.Purpose",
        [ (dirtyId' ctx, dirtyId' purp)
          | ctx :: A_Context <- instanceList fSpec,
            purp <- purposes fSpec ctx
        ]
      ),
      -- ( "FormalAmpersand.purpose",
      --   "FormalAmpersand.IdentityRule",
      --   "FormalAmpersand.Purpose",
      --   [ (dirtyId' idn, dirtyId' purp)
      --     | idn :: IdentityRule <- instanceList fSpec,
      --       purp <- purposes fSpec idn
      --   ]
      -- ),
      ( "FormalAmpersand.purpose",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Purpose",
        [ (dirtyId' ifc, dirtyId' purp)
          | ifc :: Interface <- instanceList fSpec,
            purp <- purposes fSpec ifc
        ]
      ),
      ( "FormalAmpersand.purpose",
        "FormalAmpersand.Pattern",
        "FormalAmpersand.Purpose",
        [ (dirtyId' pat, dirtyId' purp)
          | pat :: Pattern <- instanceList fSpec,
            purp <- purposes fSpec pat
        ]
      ),
      ( "FormalAmpersand.purpose",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Purpose",
        [ (dirtyId' rel, dirtyId' purp)
          | rel :: Relation <- instanceList fSpec,
            purp <- purposes fSpec rel
        ]
      ),
      ( "FormalAmpersand.purpose",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Purpose",
        [ (dirtyId' rul, dirtyId' purp)
          | rul :: Rule <- instanceList fSpec,
            purp <- purposes fSpec rul
        ]
      ),
      ( "FormalAmpersand.purpose",
        "FormalAmpersand.View",
        "FormalAmpersand.Purpose",
        [ (dirtyId' vw, dirtyId' purp)
          | vw :: ViewDef <- instanceList fSpec,
            purp <- purposes fSpec vw
        ]
      ),
      ( "FormalAmpersand.pvsExp",
        "FormalAmpersand.PairViewSegment",
        "FormalAmpersand.Term",
        [ (dirtyId' pvs, dirtyId' (pvsExp pvs))
          | pvs@PairViewExp {} :: PairViewSegment Expression <- instanceList fSpec
        ]
      ),
      -- ( "FormalAmpersand.relsDefdIn",
      --   "FormalAmpersand.Pattern",
      --   "FormalAmpersand.Relation",
      --   [ (dirtyId' pat, dirtyId' rel)
      --     | pat :: Pattern <- instanceList fSpec,
      --       rel <- Set.elems $ relsDefdIn pat
      --   ]
      -- ),
      ( "FormalAmpersand.second",
        "FormalAmpersand.BinaryTerm",
        "FormalAmpersand.Term",
        [ (dirtyId' expr, dirtyId' x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [second expr]
        ]
      ),
      ( "FormalAmpersand.segment",
        "FormalAmpersand.PairView",
        "FormalAmpersand.PairViewSegment",
        [ (dirtyId' pv, dirtyId' pvs)
          | pv :: PairView Expression <- instanceList fSpec,
            pvs <- NE.toList . ppv_segs $ pv
        ]
      ),
      ( "FormalAmpersand.sequenceNr",
        "FormalAmpersand.PairViewSegment",
        "FormalAmpersand.SequenceNumber",
        [ (dirtyId' pvs, PopInt nr)
          | pv :: PairView Expression <- instanceList fSpec,
            (pvs, nr) <- zip (NE.toList . ppv_segs $ pv) [0 ..]
        ]
      ),
      ( "FormalAmpersand.showADL",
        "FormalAmpersand.PairView",
        "FormalAmpersand.ShowADL",
        [ (dirtyId' pv, PopAlphaNumeric (showA pv))
          | pv :: PairView Expression <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.showADL",
        "FormalAmpersand.Term",
        "FormalAmpersand.ShowADL",
        [ (dirtyId' expr, PopAlphaNumeric (showA expr))
          | expr :: Expression <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.sign",
        "FormalAmpersand.Term",
        "FormalAmpersand.Signature",
        [ (dirtyId' expr, dirtyId' (sign expr))
          | expr :: Expression <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.sign",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Signature",
        [ (dirtyId' rel, dirtyId' (sign rel))
          | rel :: Relation <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.singleton",
        "FormalAmpersand.Singleton",
        "FormalAmpersand.AtomValue",
        [ (dirtyId' expr, PopAlphaNumeric (tshow x))
          | expr :: Expression <- instanceList fSpec,
            Just x <- [singleton expr]
        ]
      ),
      ( "FormalAmpersand.source",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Concept",
        [ (dirtyId' rel, dirtyId' (source rel))
          | rel :: Relation <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.src",
        "FormalAmpersand.Signature",
        "FormalAmpersand.Concept",
        [ (dirtyId' sgn, dirtyId' (source sgn))
          | sgn :: Signature <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.srcOrTgt",
        "FormalAmpersand.PairViewSegment",
        "FormalAmpersand.SourceOrTarget",
        [ (dirtyId' pvs, PopAlphaNumeric . tshow . pvsSoT $ pvs)
          | pvs@PairViewExp {} :: PairViewSegment Expression <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.target",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Concept",
        [ (dirtyId' rel, dirtyId' (target rel))
          | rel :: Relation <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.text",
        "FormalAmpersand.BxTxt",
        "FormalAmpersand.Text",
        [ (dirtyId' item, PopAlphaNumeric (boxtxt item))
          | item@BxText {} <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.text",
        "FormalAmpersand.PairViewSegment",
        "FormalAmpersand.String",
        [ (dirtyId' pvs, PopAlphaNumeric . pvsStr $ pvs)
          | pvs@PairViewText {} :: PairViewSegment Expression <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.tgt",
        "FormalAmpersand.Signature",
        "FormalAmpersand.Concept",
        [ (dirtyId' sgn, dirtyId' (target sgn))
          | sgn :: Signature <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.ttype",
        "FormalAmpersand.Concept",
        "FormalAmpersand.TType",
        [ (dirtyId' cpt, PopAlphaNumeric . tshow . cptTType fSpec $ cpt)
          | cpt :: A_Concept <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.udefrules",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Context",
        [ (dirtyId' rul, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rul <- Set.elems $ udefrules ctx
        ]
      ),
      ( "FormalAmpersand.udefrules",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Pattern",
        [ (dirtyId' rul, dirtyId' pat)
          | pat :: Pattern <- instanceList fSpec,
            rul <- Set.elems $ udefrules pat
        ]
      ),
      ( "FormalAmpersand.urlEncodedName",
        "FormalAmpersand.Concept",
        "FormalAmpersand.EncodedName",
        [ (dirtyId' cpt, PopAlphaNumeric . text1ToText . urlEncodedName . name $ cpt)
          | cpt :: A_Concept <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.urlEncodedName",
        "FormalAmpersand.Pattern",
        "FormalAmpersand.EncodedName",
        [ (dirtyId' pat, PopAlphaNumeric . text1ToText . urlEncodedName . name $ pat)
          | pat :: Pattern <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.urlEncodedName",
        "FormalAmpersand.Rule",
        "FormalAmpersand.EncodedName",
        [ (dirtyId' rul, PopAlphaNumeric . text1ToText . urlEncodedName . name $ rul)
          | rul :: Rule <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.usedIn",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Term",
        [ (dirtyId' rel, dirtyId' expr)
          | expr :: Expression <- instanceList fSpec,
            rel :: Relation <- Set.elems $ bindedRelationsIn expr
        ]
      ),
      ( "FormalAmpersand.userCpt",
        "FormalAmpersand.Epsilon",
        "FormalAmpersand.Concept",
        [ (dirtyId' expr, dirtyId' x)
          | expr :: Expression <- instanceList fSpec,
            Just (x :: A_Concept) <- [userCpt expr]
        ]
      ),
      ( "FormalAmpersand.userSrc",
        "FormalAmpersand.V",
        "FormalAmpersand.Concept",
        [ (dirtyId' expr, dirtyId' x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [userSrc expr]
        ]
      ),
      ( "FormalAmpersand.userTgt",
        "FormalAmpersand.V",
        "FormalAmpersand.Concept",
        [ (dirtyId' expr, dirtyId' x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [userTgt expr]
        ]
      ),
      ( "FormalAmpersand.versionInfo",
        "FormalAmpersand.Context",
        "FormalAmpersand.AmpersandVersion",
        [ (dirtyId' ctx, PopAlphaNumeric (longVersion appVersion))
          | ctx :: A_Context <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.violatable",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Rule",
        [] -- TODO future work
      )
    ]
    <> tmpNewTransformerDefsFA fSpec

-- | The following transformers provide the metamodel needed to run a prototype.
transformersPrototypeContext :: FSpec -> [Transformer]
transformersPrototypeContext fSpec =
  map
    toTransformer
    -- the following transformer is also contained in FormalAmpersand.
    [ ( "PrototypeContext.isAPI",
        "PrototypeContext.Interface",
        "PrototypeContext.Interface",
        [ (dirtyIdWithoutType' ifc, dirtyIdWithoutType' ifc)
          | ifc :: Interface <- instanceList fSpec,
            ifcIsAPI ifc
        ]
      ),
      -- the following transformer can be calculated by the Exec Engine.
      -- it is also contained in FormalAmpersand.
      ( "PrototypeContext.isPublic",
        "PrototypeContext.Interface",
        "PrototypeContext.Interface",
        [ (dirtyIdWithoutType' ifc, dirtyIdWithoutType' ifc)
          | ifc :: Interface <- instanceList fSpec,
            null (ifcRoles ifc)
        ]
      ),
      -- the following transformer is also contained in FormalAmpersand.
      ( "PrototypeContext.label",
        "PrototypeContext.Interface",
        "PrototypeContext.Label",
        [ (dirtyIdWithoutType' ifc, PopAlphaNumeric . label $ ifc)
          | ifc :: Interface <- instanceList fSpec
        ]
      ),
      -- the following transformer is called name[Role*RoleName] in FormalAmpersand
      ( "PrototypeContext.label",
        "PrototypeContext.Role",
        "PrototypeContext.Label",
        [ (dirtyIdWithoutType' role, PopAlphaNumeric . label $ role)
          | role :: Role <- instanceList fSpec,
            isJust (rlLbl role)
        ]
      ),
      -- the following transformer is called ifcRoles[Interface*Role] in FormalAmpersand
      ( "PrototypeContext.ifcRoles",
        "PrototypeContext.Interface",
        "PrototypeContext.Role",
        [ (dirtyIdWithoutType' ifc, dirtyIdWithoutType' role)
          | ifc :: Interface <- instanceList fSpec,
            role <- ifcRoles ifc
        ]
      ),
      ( "PrototypeContext.ifc",
        "PrototypeContext.NavMenuItem",
        "PrototypeContext.Interface",
        [] -- TODO
      ),
      ( "PrototypeContext.label",
        "PrototypeContext.NavMenuItem",
        "PrototypeContext.Label",
        [] -- TODO
      ),
      ( "PrototypeContext.isSubItemOf",
        "PrototypeContext.NavMenuItem",
        "PrototypeContext.NavMenuItem",
        [] -- TODO
      ),
      ( "PrototypeContext.isVisible",
        "PrototypeContext.NavMenuItem",
        "PrototypeContext.NavMenuItem",
        [] -- TODO
      ),
      ( "PrototypeContext.isPartOf",
        "PrototypeContext.NavMenuItem",
        "PrototypeContext.NavMenu",
        [] -- TODO
      ),
      ( "PrototypeContext.seqNr",
        "PrototypeContext.NavMenuItem",
        "PrototypeContext.SeqNr",
        [] -- TODO
      ),
      ( "PrototypeContext.url",
        "PrototypeContext.NavMenuItem",
        "PrototypeContext.URL",
        [] -- TODO
      ),
      ( "PrototypeContext.navItemRoles",
        "PrototypeContext.NavMenuItem",
        "PrototypeContext.Role",
        [] -- TODO
      ),
      ( "PrototypeContext.lastAccess",
        "SESSION",
        "PrototypeContext.DateTime",
        [] -- TODO
      ),
      ( "PrototypeContext.sessionActiveRoles",
        "SESSION",
        "PrototypeContext.Role",
        [] -- TODO
      ),
      ( "PrototypeContext.sessionAllowedRoles",
        "SESSION",
        "PrototypeContext.Role",
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
    arg' :: Maybe Expression, -- argument of a unary expression
    param' :: Maybe PBinOp, -- a relational operator of type (source(Expression) -> target(Expression) -> Bool)
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
          param' = Nothing,
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
          param' = Nothing,
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
          param' = Nothing,
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
          param' = Nothing,
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
          param' = Nothing,
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
          param' = Nothing,
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
          param' = Nothing,
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
          param' = Nothing,
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
          param' = Nothing,
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
          param' = Nothing,
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
          param' = Nothing,
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
          param' = Nothing,
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
          param' = Nothing,
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
          param' = Nothing,
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
          param' = Nothing,
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
          param' = Nothing,
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
          param' = Nothing,
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
          param' = Nothing,
          userCpt' = Just cpt,
          userSrc' = Nothing,
          userTgt' = Nothing,
          singleton' = Nothing
        }
    (EBin oper cpt) ->
      ExprInfo
        { binOp' = Nothing,
          unaryOp' = Nothing,
          bindedRel' = Nothing,
          first' = Nothing,
          second' = Nothing,
          arg' = Nothing,
          param' = Just oper,
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
          param' = Nothing,
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
          param' = Nothing,
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
          param' = Nothing,
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
  showUnique = toText1Unsafe . tshow

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
  showUnique = toText1Unsafe . tshow

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
    [ ( "FormalAmpersand.context",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Context",
        [ (dirtyId' rul, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rul :: Rule <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.edit",
        "FormalAmpersand.FieldDef",
        "FormalAmpersand.Relation",
        [] -- TODO Future work
      ),
      ( "FormalAmpersand.editFlp",
        "FormalAmpersand.FieldDef",
        "FormalAmpersand.Relation",
        [] -- TODO
      ),
      ( "FormalAmpersand.fst",
        "FormalAmpersand.CombineStrings",
        "FormalAmpersand.String",
        [] -- TODO
      ),
      ( "FormalAmpersand.ifcQuads",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Quad",
        [] -- TODO future work
      ),
      ( "FormalAmpersand.interfaces",
        "FormalAmpersand.Context",
        "FormalAmpersand.Interface",
        [ (dirtyId' ctx, dirtyId' ifc)
          | ctx :: A_Context <- instanceList fSpec,
            ifc :: Interface <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.interfaces",
        "FormalAmpersand.Role",
        "FormalAmpersand.Interface",
        [ (dirtyId' rol, dirtyId' ifc)
          | ifc :: Interface <- instanceList fSpec,
            rol <- ifcRoles ifc
        ]
      ),
      ( "FormalAmpersand.isaCopy",
        "FormalAmpersand.Concept",
        "FormalAmpersand.Concept",
        [] -- TODO
      ),
      ( "FormalAmpersand.isaPlus",
        "FormalAmpersand.Concept",
        "FormalAmpersand.Concept",
        [] -- TODO
      ),
      ( "FormalAmpersand.isaRfx",
        "FormalAmpersand.Concept",
        "FormalAmpersand.Concept",
        [] -- TODO
      ),
      ( "FormalAmpersand.isaRfxCopy",
        "FormalAmpersand.Concept",
        "FormalAmpersand.Concept",
        [] -- TODO
      ),
      ( "FormalAmpersand.isaRfxPlus",
        "FormalAmpersand.Concept",
        "FormalAmpersand.Concept",
        [] -- TODO
      ),
      ( "FormalAmpersand.isaRfxStar",
        "FormalAmpersand.Concept",
        "FormalAmpersand.Concept",
        [] -- TODO
      ),
      ( "FormalAmpersand.isaStar",
        "FormalAmpersand.Concept",
        "FormalAmpersand.Concept",
        [] -- TODO
      ),
      ( "FormalAmpersand.isUni",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Relation",
        [ (dirtyId' rel, dirtyId' rel)
          | rel :: Relation <- instanceList fSpec,
            isUni rel
        ]
      ),
      ( "FormalAmpersand.isTot",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Relation",
        [ (dirtyId' rel, dirtyId' rel)
          | rel :: Relation <- instanceList fSpec,
            isTot rel
        ]
      ),
      ( "FormalAmpersand.isMapping",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Relation",
        [ (dirtyId' rel, dirtyId' rel)
          | rel :: Relation <- instanceList fSpec,
            isMapping rel
        ]
      ),
      ( "FormalAmpersand.isInj",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Relation",
        [ (dirtyId' rel, dirtyId' rel)
          | rel :: Relation <- instanceList fSpec,
            isInj rel
        ]
      ),
      ( "FormalAmpersand.isSur",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Relation",
        [ (dirtyId' rel, dirtyId' rel)
          | rel :: Relation <- instanceList fSpec,
            isSur rel
        ]
      ),
      ( "FormalAmpersand.isBijective",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Relation",
        [ (dirtyId' rel, dirtyId' rel)
          | rel :: Relation <- instanceList fSpec,
            isBijective rel
        ]
      ),
      ( "FormalAmpersand.isAsy",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Relation",
        [ (dirtyId' rel, dirtyId' rel)
          | rel :: Relation <- instanceList fSpec,
            isAsy rel
        ]
      ),
      ( "FormalAmpersand.isSym",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Relation",
        [ (dirtyId' rel, dirtyId' rel)
          | rel :: Relation <- instanceList fSpec,
            isSym rel
        ]
      ),
      ( "FormalAmpersand.isProp",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Relation",
        [ (dirtyId' rel, dirtyId' rel)
          | rel :: Relation <- instanceList fSpec,
            isProp rel
        ]
      ),
      ( "FormalAmpersand.isTrn",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Relation",
        [ (dirtyId' rel, dirtyId' rel)
          | rel :: Relation <- instanceList fSpec,
            isTrn rel
        ]
      ),
      ( "FormalAmpersand.isRfx",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Relation",
        [ (dirtyId' rel, dirtyId' rel)
          | rel :: Relation <- instanceList fSpec,
            isRfx rel
        ]
      ),
      ( "FormalAmpersand.isIrf",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Relation",
        [ (dirtyId' rel, dirtyId' rel)
          | rel :: Relation <- instanceList fSpec,
            isIrf rel
        ]
      ),
      ( "FormalAmpersand.hasAttributes",
        "FormalAmpersand.Concept",
        "FormalAmpersand.Concept",
        [] -- The Atlas will maintain this population, so we don't need to do it here.
      ),
      ( "FormalAmpersand.isAttribute",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Relation",
        [] -- The Atlas will maintain this population, so we don't need to do it here.
      ),
      ( "FormalAmpersand.originatesFrom",
        "FormalAmpersand.Conjunct",
        "FormalAmpersand.Rule",
        [] -- TODO
      ),
      ( "FormalAmpersand.patRules",
        "FormalAmpersand.Pattern",
        "FormalAmpersand.Rule",
        [ (dirtyId' pat, dirtyId' rul)
          | pat :: Pattern <- instanceList fSpec,
            rul <- Set.toList (ptrls pat)
        ]
      ),
      ( "FormalAmpersand.result",
        "FormalAmpersand.CombineStrings",
        "FormalAmpersand.String",
        [] -- TODO
      ),
      ( "FormalAmpersand.snd",
        "FormalAmpersand.CombineStrings",
        "FormalAmpersand.String",
        [] -- TODO
      ),
      ( "FormalAmpersand.uses",
        "FormalAmpersand.Context",
        "FormalAmpersand.Pattern",
        [ (dirtyId' ctx, dirtyId' pat)
          | ctx :: A_Context <- instanceList fSpec,
            pat :: Pattern <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.valid",
        "FormalAmpersand.Concept",
        "FormalAmpersand.Context",
        [] -- TODO Future work
      ),
      ( "FormalAmpersand.valid",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Context",
        [] -- TODO Future work
      ),
      ( "FormalAmpersand.valid",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Context",
        [] -- TODO Future work
      ),
      ( "FormalAmpersand.siHeader",
        "FormalAmpersand.Box",
        "FormalAmpersand.HTMLtemplateCall",
        [ (dirtyId' box, dirtyId' (siHeader box))
          | box@Box {} <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.siConcept",
        "FormalAmpersand.Box",
        "FormalAmpersand.Concept",
        [ (dirtyId' box, dirtyId' (siConcept box))
          | box@Box {} <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.objcruds",
        "FormalAmpersand.ObjectDef",
        "FormalAmpersand.Cruds",
        [ (dirtyId' od, PopAlphaNumeric . tshow . objcrud $ od)
          | od :: ObjectDef <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.objSub",
        "FormalAmpersand.ObjectDef",
        "FormalAmpersand.SubInterface",
        [ (dirtyId' od, dirtyId' si)
          | od :: ObjectDef <- instanceList fSpec,
            Just si <- [objmsub od]
        ]
      ),
      ( "FormalAmpersand.objTerm",
        "FormalAmpersand.ObjectDef",
        "FormalAmpersand.Term",
        [ (dirtyId' od, dirtyId' (objExpression od))
          | od :: ObjectDef <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.origin",
        "FormalAmpersand.SubInterface",
        "FormalAmpersand.Origin",
        [ (dirtyId' si, popatom)
          | si :: SubInterface <- instanceList fSpec,
            Just popatom <- [originToPopAtom si]
        ]
      ),
      ( "FormalAmpersand.siObjs",
        "FormalAmpersand.Box",
        "FormalAmpersand.BoxItem",
        [ (dirtyId' bx, dirtyId' item)
          | bx@Box {} <- instanceList fSpec,
            item <- siObjs bx
        ]
      ),
      ( "FormalAmpersand.isLink",
        "FormalAmpersand.InterfaceRef",
        "FormalAmpersand.InterfaceRef",
        [ (dirtyId' ref, dirtyId' ref)
          | ref@InterfaceRef {} <- instanceList fSpec,
            siIsLink ref
        ]
      ),
      ( "FormalAmpersand.references",
        "FormalAmpersand.InterfaceRef",
        "FormalAmpersand.Interface",
        [ (dirtyId' ref, dirtyId' ifc)
          | ref@InterfaceRef {} <- instanceList fSpec,
            ifc :: Interface <- instanceList fSpec,
            name ifc == siIfcId ref
        ]
      ),
      ( "FormalAmpersand.origin",
        "FormalAmpersand.BoxItem",
        "FormalAmpersand.Origin",
        [ (dirtyId' item, popatom)
          | item :: BoxItem <- instanceList fSpec,
            Just popatom <- [originToPopAtom item]
        ]
      ),
      ( "FormalAmpersand.concept",
        "FormalAmpersand.ConceptDef",
        "FormalAmpersand.Concept",
        [ (dirtyId' cd, dirtyId' cpt)
          | cd :: AConceptDef <- instanceList fSpec,
            cpt :: A_Concept <- instanceList fSpec,
            name cpt == name cd
        ]
      ),
      ( "FormalAmpersand.patRules",
        "FormalAmpersand.Pattern",
        "FormalAmpersand.Rule",
        [] -- TODO
      ),
      ( "FormalAmpersand.language",
        "FormalAmpersand.Markup",
        "FormalAmpersand.Language",
        [ (dirtyId' mkp, PopAlphaNumeric . tshow . amLang $ mkp)
          | mkp :: Markup <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.text",
        "FormalAmpersand.Markup",
        "FormalAmpersand.MarkupText",
        [ (dirtyId' mkp, PopAlphaNumeric . stringify . amPandoc $ mkp)
          | mkp :: Markup <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.explRefIds",
        "FormalAmpersand.Purpose",
        "FormalAmpersand.Text",
        [ (dirtyId' prp, PopAlphaNumeric txt)
          | prp :: Purpose <- instanceList fSpec,
            txt <- explRefIds prp
        ]
      ),
      ( "FormalAmpersand.explMarkup",
        "FormalAmpersand.Purpose",
        "FormalAmpersand.Markup",
        [ (dirtyId' prp, dirtyId' (explMarkup prp))
          | prp :: Purpose <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.origin",
        "FormalAmpersand.Purpose",
        "FormalAmpersand.Origin",
        [ (dirtyId' prp, popatom)
          | prp :: Purpose <- instanceList fSpec,
            Just popatom <- [originToPopAtom prp]
        ]
      ),
      ( "FormalAmpersand.decMean",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Meaning",
        [ (dirtyId' rel, dirtyId' mean)
          | rel :: Relation <- instanceList fSpec,
            mean <- decMean rel
        ]
      )
    ]

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
