{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Ampersand.FSpec.Transformers
  ( nameSpaceFormalAmpersand,
    transformersFormalAmpersand,
    nameSpacePrototypeContext,
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
import Data.Typeable (typeOf)
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified Text.Pandoc.Shared as P

-- | The function that retrieves the population of
--   some relation of Formal Ampersand of a given
--   ampersand script.
data Transformer = Transformer
  { tRel :: Name, -- name of relation
    tSrc :: Name, -- name of source
    tTrg :: Name, -- name of target
    mults :: AProps, -- property constraints
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

dirtyId :: Unique a => a -> Maybe PopAtom
dirtyId x = DirtyId <$> idWithoutType x

dirtyId' :: Unique e => e -> PopAtom
dirtyId' x = case dirtyId x of
  Nothing -> fatal $ "Not a valid dirtyId could be generated: " <> tshow (typeOf x) <> ": " <> text1ToText (showUnique x)
  Just pa -> pa

dirtyIdWithoutType :: Unique a => a -> Maybe PopAtom
dirtyIdWithoutType x = DirtyId <$> idWithoutType x

dirtyIdWithoutType' :: Unique e => e -> PopAtom
dirtyIdWithoutType' x = case dirtyIdWithoutType x of
  Nothing -> fatal $ "Not a valid dirtyIdWithoutType could be generated: " <> tshow (typeOf x) <> ": " <> text1ToText (showUnique x)
  Just pa -> pa

-- Function for PrototypeContext transformers. These atoms don't need to have a type prefix
toTransformer :: NameSpace -> (Text, Text, Text, AProps, [(PopAtom, PopAtom)]) -> Transformer
toTransformer namespace (rel, src, tgt, props, tuples) =
  Transformer rel' src' tgt' props tuples'
  where
    rel' = withNameSpace namespace . mkName RelationName $ toNamePart' rel NE.:| []
    src' = withNameSpace namespace . mkName ConceptName $ toNamePart' src NE.:| []
    tgt' = withNameSpace namespace . mkName ConceptName $ toNamePart' tgt NE.:| []
    toNamePart' :: Text -> NamePart
    toNamePart' x = case toNamePart x of
      Nothing -> fatal "Not a valid NamePart."
      Just np -> np
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
    (toTransformer nameSpaceFormalAmpersand)
    [ {-
      -}
      --    RELATION acdcpt[ConceptDef*Text] [UNI]      -- ^ The name of the concept for which this is the definition. If there is no such concept, the conceptdefinition is ignored.
      ( "FormalAmpersand.acdcpt",
        "FormalAmpersand.ConceptDef",
        "FormalAmpersand.ConceptName",
        Set.fromList [Uni],
        [ (dirtyId' cdf, PopAlphaNumeric . fullName $ cdf)
          | cdf :: AConceptDef <- instanceList fSpec
        ]
      ),
      --    RELATION acddef2[ConceptDef*Meaning] [UNI]  -- ^ The textual definition of this concept.
      ( "FormalAmpersand.acddef2",
        "FormalAmpersand.ConceptDef",
        "FormalAmpersand.Meaning",
        Set.fromList [Uni],
        [ (dirtyId' cdf, dirtyId' mean)
          | cdf :: AConceptDef <- instanceList fSpec,
            mean :: Meaning <- acdmean cdf
        ]
      ),
      --    RELATION acdfrom[ConceptDef*Pattern] [UNI]  -- ^ The name of the pattern or context in which this concept definition was made
      ( "FormalAmpersand.acdfrom",
        "FormalAmpersand.ConceptDef",
        "FormalAmpersand.Pattern",
        Set.fromList [Uni],
        [ (dirtyId' cdf, dirtyId' pat)
          | pat :: Pattern <- instanceList fSpec,
            cdf :: AConceptDef <- ptcds pat
        ]
      ),
      --    RELATION acdmean[ConceptDef*Meaning] [UNI]  -- ^ User-specified meanings, possibly more than one, for multiple languages.
      ( "FormalAmpersand.acdmean",
        "FormalAmpersand.ConceptDef",
        "FormalAmpersand.Meaning",
        Set.empty,
        [ (dirtyId' cdf, dirtyId' mean)
          | cdf :: AConceptDef <- instanceList fSpec,
            mean :: Meaning <- acdmean cdf
        ]
      ),
      --    RELATION acdpos[ConceptDef*Origin] [UNI]      -- ^ The position of this definition in the text of the Ampersand source (filename, line number and column number).
      ( "FormalAmpersand.acdpos",
        "FormalAmpersand.ConceptDef",
        "FormalAmpersand.Origin",
        Set.fromList [Uni],
        [ (dirtyId' cdf, PopAlphaNumeric . tshow . origin $ cdf)
          | cdf :: AConceptDef <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.allConjuncts",
        "FormalAmpersand.Context",
        "FormalAmpersand.Conjunct",
        Set.fromList [Inj],
        [ (dirtyId' ctx, dirtyId' conj)
          | ctx :: A_Context <- instanceList fSpec,
            conj :: Conjunct <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.allRoles",
        "FormalAmpersand.Context",
        "FormalAmpersand.Role",
        Set.fromList [Inj],
        [ (dirtyId' ctx, dirtyId' rol)
          | ctx :: A_Context <- instanceList fSpec,
            rol :: Role <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.allRules",
        "FormalAmpersand.Pattern",
        "FormalAmpersand.Rule",
        Set.fromList [],
        {-Inj-}
        [ (dirtyId' pat, dirtyId' rul)
          | pat :: Pattern <- instanceList fSpec,
            rul :: Rule <- toList $ allRules pat
        ]
      ),
      ( "FormalAmpersand.allRules",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Context",
        Set.fromList [Uni {-,Sur-}],
        [ (dirtyId' rul, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rul :: Rule <- toList $ allRules ctx
        ]
      ),
      ( "FormalAmpersand.arg",
        "FormalAmpersand.UnaryTerm",
        "FormalAmpersand.Term",
        Set.fromList [Uni, Tot],
        [ (dirtyId' expr, dirtyId' x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [arg expr]
        ]
      ),
      ( "FormalAmpersand.asMarkdown",
        "FormalAmpersand.Markup",
        "FormalAmpersand.Text",
        Set.fromList [Uni, Tot],
        [ (dirtyId' mrk, (PopAlphaNumeric . P.stringify . amPandoc) mrk)
          | mrk :: Markup <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.bind",
        "FormalAmpersand.BindedRelation",
        "FormalAmpersand.Relation",
        Set.fromList [Uni, Tot],
        [ (dirtyId' expr, dirtyId' x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [bindedRel expr]
        ]
      ),
      ( "FormalAmpersand.concepts",
        "FormalAmpersand.Pattern",
        "FormalAmpersand.Concept",
        Set.empty,
        [ (dirtyId' pat, dirtyId' cpt)
          | pat :: Pattern <- instanceList fSpec,
            cpt :: A_Concept <- toList $ concs pat
        ]
      ),
      ( "FormalAmpersand.rcConjunct",
        "FormalAmpersand.Conjunct",
        "FormalAmpersand.Term",
        Set.fromList [Uni, Tot],
        [ (dirtyId' conj, dirtyId' (rcConjunct conj))
          | conj :: Conjunct <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.context",
        "FormalAmpersand.Concept",
        "FormalAmpersand.Context",
        Set.fromList [Uni],
        [ (dirtyId' cpt, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            cpt :: A_Concept <- Set.toList . concs $ ctx
        ]
      ),
      ( "FormalAmpersand.context",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Context",
        Set.fromList [Uni, Tot],
        [ (dirtyId' ifc, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            ifc :: Interface <- ctxifcs ctx
        ]
      ),
      ( "FormalAmpersand.context",
        "FormalAmpersand.Isa",
        "FormalAmpersand.Context",
        Set.fromList [Uni, Tot],
        [ (dirtyId' isa, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            isa@Isa {} <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.context",
        "FormalAmpersand.IsE",
        "FormalAmpersand.Context",
        Set.fromList [Uni, Tot],
        [ (dirtyId' ise, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            ise@IsE {} <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.context",
        "FormalAmpersand.Pattern",
        "FormalAmpersand.Context",
        Set.fromList [Uni, Tot],
        [ (dirtyId' pat, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            pat :: Pattern <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.context",
        "FormalAmpersand.Population",
        "FormalAmpersand.Context",
        Set.fromList [Uni, Tot],
        [ (dirtyId' pop, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            pop :: Population <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.ctxcds",
        "FormalAmpersand.ConceptDef",
        "FormalAmpersand.Context",
        Set.fromList [Uni, Tot],
        [ (dirtyId' cdf, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            cdf :: AConceptDef <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.relsDefdIn",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Context", ---contains ALL relations defined in this context
        Set.fromList [Uni, Tot],
        [ (dirtyId' rel, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rel :: Relation <- toList $ relsDefdIn ctx
        ]
      ),
      ( "FormalAmpersand.ctxds",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Context",
        Set.fromList [Uni],
        [ (dirtyId' rel, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rel :: Relation <- toList $ ctxds ctx
        ]
      ),
      ( "FormalAmpersand.ctxrs",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Context",
        Set.fromList [Uni],
        [ (dirtyId' rul, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rul :: Rule <- toList . ctxrs $ ctx
        ]
      ),
      ( "FormalAmpersand.declaredIn",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Pattern",
        Set.empty,
        [ (dirtyId' rel, dirtyId' pat)
          | pat :: Pattern <- instanceList fSpec,
            rel :: Relation <- toList $ relsDefdIn pat
        ]
      ),
      ( "FormalAmpersand.decMean",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Meaning",
        Set.empty,
        [ (dirtyId' rel, dirtyId' mean)
          | rel :: Relation <- instanceList fSpec,
            mean :: Meaning <- decMean rel
        ]
      ),
      ( "FormalAmpersand.decprL",
        "FormalAmpersand.Relation",
        "FormalAmpersand.String",
        Set.fromList [Uni],
        [ (dirtyId' rel, (PopAlphaNumeric . decprL) rel)
          | rel :: Relation <- instanceList fSpec,
            (not . T.null . decprL) rel
        ]
      ),
      ( "FormalAmpersand.decprM",
        "FormalAmpersand.Relation",
        "FormalAmpersand.String",
        Set.fromList [Uni],
        [ (dirtyId' rel, (PopAlphaNumeric . decprM) rel)
          | rel :: Relation <- instanceList fSpec,
            (not . T.null . decprM) rel
        ]
      ),
      ( "FormalAmpersand.decprR",
        "FormalAmpersand.Relation",
        "FormalAmpersand.String",
        Set.fromList [Uni],
        [ (dirtyId' rel, (PopAlphaNumeric . decprR) rel)
          | rel :: Relation <- instanceList fSpec,
            (not . T.null . decprR) rel
        ]
      ),
      ( "FormalAmpersand.expSQL",
        "FormalAmpersand.PairViewSegment",
        "FormalAmpersand.MySQLQuery",
        Set.empty,
        [] --TODO
      ),
      ( "FormalAmpersand.expTgt",
        "FormalAmpersand.PairViewSegment",
        "FormalAmpersand.Concept",
        Set.empty,
        [] --TODO
      ),
      ( "FormalAmpersand.fieldIn",
        "FormalAmpersand.FieldDef",
        "FormalAmpersand.ObjectDef",
        Set.fromList [Uni, Tot],
        [ (dirtyId' fld, dirtyId' obj)
          | obj :: ObjectDef <- instanceList fSpec,
            fld <- fields obj
        ]
      ),
      ( "FormalAmpersand.first",
        "FormalAmpersand.BinaryTerm",
        "FormalAmpersand.Term",
        Set.fromList [Uni, Tot],
        [ (dirtyId' expr, dirtyId' x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [first expr]
        ]
      ),
      ( "FormalAmpersand.formalTerm",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Term",
        Set.fromList [Uni],
        [ (dirtyId' rul, dirtyId' (formalExpression rul))
          | rul :: Rule <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.gengen",
        "FormalAmpersand.Isa",
        "FormalAmpersand.Concept",
        Set.fromList [Uni, Tot],
        [ (dirtyId' isa, dirtyId' (gengen isa))
          | isa@Isa {} <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.gengen",
        "FormalAmpersand.IsE",
        "FormalAmpersand.Concept",
        Set.fromList [Tot], -- it is Tot by definition, because genrhs is a NonEmpty.
        [ (dirtyId' ise, dirtyId' cpt)
          | ise@IsE {} <- instanceList fSpec,
            cpt <- NE.toList $ genrhs ise
        ]
      ),
      ( "FormalAmpersand.genspc",
        "FormalAmpersand.IsE",
        "FormalAmpersand.Concept",
        Set.fromList [Uni, Tot],
        [ (dirtyId' ise, dirtyId' (genspc ise))
          | ise@IsE {} <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.genspc",
        "FormalAmpersand.Isa",
        "FormalAmpersand.Concept",
        Set.fromList [Uni, Tot],
        [ (dirtyId' isa, dirtyId' (genspc isa))
          | isa@Isa {} <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.identityRules",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Context",
        Set.fromList [Uni],
        [ (dirtyId' rul, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rul <- toList $ identityRules ctx
        ]
      ),
      ( "FormalAmpersand.identityRules",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Pattern",
        Set.fromList [Uni],
        [ (dirtyId' rul, dirtyId' pat)
          | pat :: Pattern <- instanceList fSpec,
            rul <- toList $ identityRules pat
        ]
      ),
      ( "FormalAmpersand.ifcConjuncts",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Conjunct",
        Set.empty,
        [ (dirtyId' ifc, dirtyId' conj)
          | ifc :: Interface <- instanceList fSpec,
            conj <- ifcConjuncts ifc
        ]
      ),
      ( "FormalAmpersand.ifcInputs",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Relation",
        Set.empty,
        [] --TODO
      ),
      ( "FormalAmpersand.ifcObj",
        "FormalAmpersand.Interface",
        "FormalAmpersand.ObjectDef",
        Set.fromList [Uni, Tot],
        [ (dirtyId' ifc, dirtyId' (ifcObj ifc))
          | ifc :: Interface <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.ifcOutputs",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Relation",
        Set.empty,
        [] --TODO
      ),
      ( "FormalAmpersand.ifcPos",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Origin",
        Set.fromList [Uni],
        [ (dirtyId' ifc, PopAlphaNumeric . tshow . origin $ ifc)
          | ifc :: Interface <- instanceList fSpec,
            origin ifc `notElem` [OriginUnknown, MeatGrinder]
        ]
      ),
      ( "FormalAmpersand.ifcPurpose",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Purpose",
        Set.empty,
        [ (dirtyId' ifc, dirtyId' purp)
          | ifc :: Interface <- instanceList fSpec,
            purp <- purposes fSpec ifc
        ]
      ),
      ( "FormalAmpersand.ifcRoles",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Role",
        Set.empty,
        [ (dirtyId' ifc, dirtyId' rol)
          | ifc <- instanceList fSpec,
            rol <- ifcRoles ifc
        ]
      ),
      ( "FormalAmpersand.isAPI",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Interface",
        Set.fromList [Asy, Sym],
        [ (dirtyId' ifc, dirtyId' ifc)
          | ifc :: Interface <- instanceList fSpec,
            ifcIsAPI ifc
        ]
      ),
      -- the following transformer can be calculated by the Exec Engine. So it can be removed here if so desired.
      ( "FormalAmpersand.isPublic",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Interface",
        Set.fromList [Asy, Sym],
        [ (dirtyId' ifc, dirtyId' ifc)
          | ifc :: Interface <- instanceList fSpec,
            null (ifcRoles ifc)
        ]
      ),
      ( "FormalAmpersand.isa",
        "FormalAmpersand.Concept",
        "FormalAmpersand.Concept",
        Set.empty,
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
        Set.fromList [Uni, Tot],
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
      ( "FormalAmpersand.language",
        "FormalAmpersand.Context",
        "FormalAmpersand.Language",
        Set.empty,
        [ (dirtyId' ctx, (PopAlphaNumeric . tshow . ctxlang) ctx)
          | ctx :: A_Context <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.language",
        "FormalAmpersand.Markup",
        "FormalAmpersand.Language",
        Set.empty,
        [ (dirtyId' mrk, (PopAlphaNumeric . tshow . amLang) mrk)
          | mrk :: Markup <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.maintains",
        "FormalAmpersand.Role",
        "FormalAmpersand.Rule",
        Set.empty,
        [ (dirtyId' rol, dirtyId' rul)
          | (rol, rul) <- fRoleRuls fSpec
        ]
      ),
      ( "FormalAmpersand.markup",
        "FormalAmpersand.Meaning",
        "FormalAmpersand.Markup",
        Set.fromList [Uni, Tot],
        [ (dirtyId' mean, dirtyId' . ameaMrk $ mean)
          | mean :: Meaning <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.markup",
        "FormalAmpersand.Purpose",
        "FormalAmpersand.Markup",
        Set.fromList [Uni, Tot],
        [ (dirtyId' purp, dirtyId' . explMarkup $ purp)
          | purp :: Purpose <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.meaning",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Meaning",
        Set.empty,
        [ (dirtyId' rul, dirtyId' mean)
          | rul :: Rule <- instanceList fSpec,
            mean :: Meaning <- rrmean rul
        ]
      ),
      ( "FormalAmpersand.message",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Message",
        Set.empty,
        [] --TODO
      ),
      ( "FormalAmpersand.proprules",
        "FormalAmpersand.PropertyRule",
        "FormalAmpersand.Context",
        Set.empty,
        [ (dirtyId' rul, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rul <- toList $ proprules ctx
        ]
      ),
      ( "FormalAmpersand.proprules",
        "FormalAmpersand.PropertyRule",
        "FormalAmpersand.Pattern",
        Set.empty,
        [ (dirtyId' rul, dirtyId' pat)
          | pat :: Pattern <- instanceList fSpec,
            rul <- toList $ proprules pat
        ]
      ),
      ( "FormalAmpersand.propertyRule",
        "FormalAmpersand.Relation",
        "FormalAmpersand.PropertyRule",
        Set.fromList [Sur],
        [ (dirtyId' rel, dirtyId' rul)
          | ctx :: A_Context <- instanceList fSpec,
            rul <- toList $ proprules ctx,
            Propty _ rel <- [rrkind rul]
        ]
      ),
      ( "FormalAmpersand.declaredthrough",
        "FormalAmpersand.PropertyRule",
        "FormalAmpersand.Property",
        Set.fromList [Tot],
        [ (dirtyId' rul, (PopAlphaNumeric . tshow) prop)
          | ctx :: A_Context <- instanceList fSpec,
            rul <- toList $ proprules ctx,
            Propty prop _ <- [rrkind rul]
        ]
      ),
      ( "FormalAmpersand.name",
        "FormalAmpersand.Concept",
        "FormalAmpersand.ConceptName",
        Set.fromList [Uni],
        [ (dirtyId' cpt, (PopAlphaNumeric . fullName) cpt)
          | cpt :: A_Concept <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.name",
        "FormalAmpersand.Context",
        "FormalAmpersand.ContextName",
        Set.fromList [Uni, Tot],
        [ (dirtyId' ctx, (PopAlphaNumeric . fullName) ctx)
          | ctx :: A_Context <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.name",
        "FormalAmpersand.Interface",
        "FormalAmpersand.InterfaceName",
        Set.fromList [Uni, Tot],
        [ (dirtyId' ifc, (PopAlphaNumeric . fullName) ifc)
          | ifc :: Interface <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.name",
        "FormalAmpersand.Pattern",
        "FormalAmpersand.PatternName",
        Set.fromList [Uni, Tot],
        [ (dirtyId' pat, (PopAlphaNumeric . fullName) pat)
          | pat :: Pattern <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.name",
        "FormalAmpersand.Relation",
        "FormalAmpersand.RelationName",
        Set.fromList [Uni, Tot],
        [ (dirtyId' rel, (PopAlphaNumeric . fullName) rel)
          | rel :: Relation <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.name",
        "FormalAmpersand.Role",
        "FormalAmpersand.RoleName",
        Set.fromList [Uni],
        [ (dirtyId' rol, (PopAlphaNumeric . fullName) rol)
          | rol :: Role <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.name",
        "FormalAmpersand.Rule",
        "FormalAmpersand.RuleName",
        Set.fromList [Uni, Tot],
        [ (dirtyId' rul, (PopAlphaNumeric . fullName) rul)
          | rul :: Rule <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.name",
        "FormalAmpersand.View",
        "FormalAmpersand.ViewDefName",
        Set.fromList [Uni, Tot],
        [ (dirtyId' vd, PopAlphaNumeric . fullName $ vd)
          | vd :: ViewDef <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.objView",
        "FormalAmpersand.ObjectDef",
        "FormalAmpersand.View",
        Set.empty,
        [ (dirtyId' obj, PopAlphaNumeric . fullName $ vw)
          | obj :: ObjectDef <- instanceList fSpec,
            Just vw <- [objmView obj]
        ]
      ),
      ( "FormalAmpersand.boxpos",
        "FormalAmpersand.ObjectDef",
        "FormalAmpersand.Origin",
        Set.fromList [Uni],
        [ (dirtyId' obj, PopAlphaNumeric . tshow . origin $ obj)
          | obj :: ObjectDef <- instanceList fSpec,
            origin obj `notElem` [OriginUnknown, MeatGrinder]
        ]
      ),
      ( "FormalAmpersand.operator",
        "FormalAmpersand.BinaryTerm",
        "FormalAmpersand.Operator",
        Set.fromList [Uni, Tot],
        [ (dirtyId' expr, PopAlphaNumeric . tshow $ op)
          | expr :: Expression <- instanceList fSpec,
            Just op <- [binOp expr]
        ]
      ),
      ( "FormalAmpersand.operator",
        "FormalAmpersand.UnaryTerm",
        "FormalAmpersand.Operator",
        Set.fromList [Uni, Tot],
        [ (dirtyId' expr, PopAlphaNumeric . tshow $ op)
          | expr :: Expression <- instanceList fSpec,
            Just op <- [unaryOp expr]
        ]
      ),
      ( "FormalAmpersand.origin",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Origin",
        Set.fromList [Uni],
        [ (dirtyId' rul, PopAlphaNumeric . tshow . origin $ rul)
          | rul :: Rule <- instanceList fSpec,
            origin rul `notElem` [OriginUnknown, MeatGrinder]
        ]
      ),
      ( "FormalAmpersand.pairView",
        "FormalAmpersand.Rule",
        "FormalAmpersand.PairView",
        Set.empty,
        [] --TODO
      ),
      ( "FormalAmpersand.prop",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Property",
        Set.empty,
        [ (dirtyId' rel, PopAlphaNumeric . tshow $ prop)
          | rel :: Relation <- instanceList fSpec,
            prop <- toList $ decprps rel
        ]
      ),
      ( "FormalAmpersand.purpose",
        "FormalAmpersand.Concept",
        "FormalAmpersand.Purpose",
        Set.empty,
        [ (dirtyId' cpt, dirtyId' purp)
          | cpt :: A_Concept <- instanceList fSpec,
            purp <- purposes fSpec cpt
        ]
      ),
      ( "FormalAmpersand.purpose",
        "FormalAmpersand.Context",
        "FormalAmpersand.Purpose",
        Set.empty,
        [ (dirtyId' ctx, dirtyId' purp)
          | ctx :: A_Context <- instanceList fSpec,
            purp <- purposes fSpec ctx
        ]
      ),
      ( "FormalAmpersand.purpose",
        "FormalAmpersand.IdentityRule",
        "FormalAmpersand.Purpose",
        Set.empty,
        [ (dirtyId' idn, dirtyId' purp)
          | idn :: IdentityRule <- instanceList fSpec,
            purp <- purposes fSpec idn
        ]
      ),
      ( "FormalAmpersand.purpose",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Purpose",
        Set.empty,
        [ (dirtyId' ifc, dirtyId' purp)
          | ifc :: Interface <- instanceList fSpec,
            purp <- purposes fSpec ifc
        ]
      ),
      ( "FormalAmpersand.purpose",
        "FormalAmpersand.Pattern",
        "FormalAmpersand.Purpose",
        Set.empty,
        [ (dirtyId' pat, dirtyId' purp)
          | pat :: Pattern <- instanceList fSpec,
            purp <- purposes fSpec pat
        ]
      ),
      ( "FormalAmpersand.purpose",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Purpose",
        Set.empty,
        [ (dirtyId' rel, dirtyId' purp)
          | rel :: Relation <- instanceList fSpec,
            purp <- purposes fSpec rel
        ]
      ),
      ( "FormalAmpersand.purpose",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Purpose",
        Set.empty,
        [ (dirtyId' rul, dirtyId' purp)
          | rul :: Rule <- instanceList fSpec,
            purp <- purposes fSpec rul
        ]
      ),
      ( "FormalAmpersand.purpose",
        "FormalAmpersand.View",
        "FormalAmpersand.Purpose",
        Set.empty,
        [ (dirtyId' vw, dirtyId' purp)
          | vw :: ViewDef <- instanceList fSpec,
            purp <- purposes fSpec vw
        ]
      ),
      ( "FormalAmpersand.qConjuncts",
        "FormalAmpersand.Quad",
        "FormalAmpersand.Conjunct",
        Set.empty,
        [ (dirtyId' quad, dirtyId' conj)
          | quad <- vquads fSpec,
            conj <- NE.toList (qConjuncts quad)
        ] --TODO
      ),
      ( "FormalAmpersand.qDcl",
        "FormalAmpersand.Quad",
        "FormalAmpersand.Relation",
        Set.fromList [Uni, Tot],
        [ (dirtyId' quad, dirtyId' (qDcl quad))
          | quad <- vquads fSpec
        ] --TODO
      ),
      ( "FormalAmpersand.qRule",
        "FormalAmpersand.Quad",
        "FormalAmpersand.Rule",
        Set.fromList [Uni, Tot],
        [ (dirtyId' quad, dirtyId' (qRule quad))
          | quad <- vquads fSpec
        ] --TODO
      ),
      ( "FormalAmpersand.rc_orgRules",
        "FormalAmpersand.Conjunct",
        "FormalAmpersand.Rule",
        Set.empty,
        [ (dirtyId' conj, dirtyId' rul)
          | conj :: Conjunct <- instanceList fSpec,
            rul <- NE.toList $ rc_orgRules conj
        ]
      ),
      ( "FormalAmpersand.relsDefdIn",
        "FormalAmpersand.Pattern",
        "FormalAmpersand.Relation",
        Set.empty,
        [ (dirtyId' pat, dirtyId' rel)
          | pat :: Pattern <- instanceList fSpec,
            rel <- toList $ relsDefdIn pat
        ]
      ),
      ( "FormalAmpersand.second",
        "FormalAmpersand.BinaryTerm",
        "FormalAmpersand.Term",
        Set.fromList [Uni, Tot],
        [ (dirtyId' expr, dirtyId' x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [second expr]
        ]
      ),
      ( "FormalAmpersand.segment",
        "FormalAmpersand.PairView",
        "FormalAmpersand.PairViewSegment",
        Set.empty,
        [] --TODO
      ),
      ( "FormalAmpersand.segmentType",
        "FormalAmpersand.PairViewSegment",
        "FormalAmpersand.PairViewSegmentType",
        Set.empty,
        [] --TODO
      ),
      ( "FormalAmpersand.sequenceNr",
        "FormalAmpersand.PairViewSegment",
        "FormalAmpersand.Int",
        Set.empty,
        [] --TODO
      ),
      ( "FormalAmpersand.sessAtom",
        "SESSION",
        "FormalAmpersand.Atom",
        Set.empty,
        [] -- This goes too deep. Keep it empty.
      ),
      ( "FormalAmpersand.sessIfc",
        "SESSION",
        "FormalAmpersand.Interface",
        Set.empty,
        [] --TODO
      ),
      ( "FormalAmpersand.sessionRole",
        "SESSION",
        "FormalAmpersand.Role",
        Set.empty,
        [] --TODO
      ),
      ( "FormalAmpersand.showADL",
        "FormalAmpersand.Term",
        "FormalAmpersand.ShowADL",
        Set.fromList [Uni, Tot],
        [ (dirtyId' expr, PopAlphaNumeric (showA expr))
          | expr :: Expression <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.sign",
        "FormalAmpersand.Term",
        "FormalAmpersand.Signature",
        Set.fromList [Uni, Tot],
        [ (dirtyId' expr, dirtyId' (sign expr))
          | expr :: Expression <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.sign",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Signature",
        Set.fromList [Uni, Tot],
        [ (dirtyId' rel, dirtyId' (sign rel))
          | rel :: Relation <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.singleton",
        "FormalAmpersand.Singleton",
        "FormalAmpersand.AtomValue",
        Set.fromList [Uni, Tot],
        [ (dirtyId' expr, dirtyId' x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [singleton expr]
        ]
      ),
      ( "FormalAmpersand.source",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Concept",
        Set.fromList [Uni, Tot],
        [ (dirtyId' rel, dirtyId' (source rel))
          | rel :: Relation <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.src",
        "FormalAmpersand.Signature",
        "FormalAmpersand.Concept",
        Set.fromList [Uni, Tot],
        [ (dirtyId' sgn, dirtyId' (source sgn))
          | sgn :: Signature <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.srcOrTgt",
        "FormalAmpersand.PairViewSegment",
        "FormalAmpersand.SourceOrTarget",
        Set.fromList [Uni, Tot],
        [] --TODO
      ),
      ( "FormalAmpersand.target",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Concept",
        Set.fromList [Uni, Tot],
        [ (dirtyId' rel, dirtyId' (target rel))
          | rel :: Relation <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.text",
        "FormalAmpersand.PairViewSegment",
        "FormalAmpersand.String",
        Set.fromList [Uni, Tot],
        [] --TODO
      ),
      ( "FormalAmpersand.tgt",
        "FormalAmpersand.Signature",
        "FormalAmpersand.Concept",
        Set.fromList [Uni, Tot],
        [ (dirtyId' sgn, dirtyId' (target sgn))
          | sgn :: Signature <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.ttype",
        "FormalAmpersand.Concept",
        "FormalAmpersand.TType",
        Set.fromList [Uni],
        [ (dirtyId' cpt, PopAlphaNumeric . tshow . cptTType fSpec $ cpt)
          | cpt :: A_Concept <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.udefrules",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Context",
        Set.fromList [Uni],
        [ (dirtyId' rul, dirtyId' ctx)
          | ctx :: A_Context <- instanceList fSpec,
            rul <- toList $ udefrules ctx
        ]
      ),
      ( "FormalAmpersand.udefrules",
        "FormalAmpersand.Rule",
        "FormalAmpersand.Pattern",
        Set.fromList [Uni],
        [ (dirtyId' rul, dirtyId' pat)
          | pat :: Pattern <- instanceList fSpec,
            rul <- toList $ udefrules pat
        ]
      ),
      ( "FormalAmpersand.urlEncodedName",
        "FormalAmpersand.Concept",
        "FormalAmpersand.EncodedName",
        Set.fromList [Uni],
        [ (dirtyId' cpt, PopAlphaNumeric . text1ToText . urlEncodedName . name $ cpt)
          | cpt :: A_Concept <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.urlEncodedName",
        "FormalAmpersand.Pattern",
        "FormalAmpersand.EncodedName",
        Set.fromList [Uni],
        [ (dirtyId' pat, PopAlphaNumeric . text1ToText . urlEncodedName . name $ pat)
          | pat :: Pattern <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.urlEncodedName",
        "FormalAmpersand.Rule",
        "FormalAmpersand.EncodedName",
        Set.fromList [Uni],
        [ (dirtyId' rul, PopAlphaNumeric . text1ToText . urlEncodedName . name $ rul)
          | rul :: Rule <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.usedIn",
        "FormalAmpersand.Relation",
        "FormalAmpersand.Term",
        Set.empty,
        [ (dirtyId' rel, dirtyId' expr)
          | expr :: Expression <- instanceList fSpec,
            rel :: Relation <- toList $ bindedRelationsIn expr
        ]
      ),
      ( "FormalAmpersand.userCpt",
        "FormalAmpersand.Epsilon",
        "FormalAmpersand.Concept",
        Set.fromList [Uni, Tot],
        [ (dirtyId' expr, dirtyId' x)
          | expr :: Expression <- instanceList fSpec,
            Just (x :: A_Concept) <- [userCpt expr]
        ]
      ),
      ( "FormalAmpersand.userSrc",
        "FormalAmpersand.V",
        "FormalAmpersand.Concept",
        Set.fromList [Uni, Tot],
        [ (dirtyId' expr, dirtyId' x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [userSrc expr]
        ]
      ),
      ( "FormalAmpersand.userTgt",
        "FormalAmpersand.V",
        "FormalAmpersand.Concept",
        Set.fromList [Uni, Tot],
        [ (dirtyId' expr, dirtyId' x)
          | expr :: Expression <- instanceList fSpec,
            Just x <- [userTgt expr]
        ]
      ),
      ( "FormalAmpersand.vdats",
        "FormalAmpersand.View",
        "FormalAmpersand.ViewSegment",
        Set.fromList [Inj, Sur],
        [ (dirtyId' vd, PopAlphaNumeric . tshow $ vs)
          | vd :: ViewDef <- instanceList fSpec,
            vs <- vdats vd
        ]
      ),
      ( "FormalAmpersand.vdcpt",
        "FormalAmpersand.View",
        "FormalAmpersand.Concept",
        Set.fromList [Uni],
        [ (dirtyId' vd, PopAlphaNumeric . tshow . vdcpt $ vd)
          | vd :: ViewDef <- instanceList fSpec,
            vdIsDefault vd
        ]
      ),
      ( "FormalAmpersand.vdhtml",
        "FormalAmpersand.View",
        "FormalAmpersand.Concept",
        Set.fromList [Uni],
        [ (dirtyId' vd, PopAlphaNumeric . tshow $ html)
          | vd :: ViewDef <- instanceList fSpec,
            Just html <- [vdhtml vd]
        ]
      ),
      ( "FormalAmpersand.vdIsDefault",
        "FormalAmpersand.View",
        "FormalAmpersand.Concept",
        Set.fromList [Uni, Tot],
        [ (dirtyId' vd, PopAlphaNumeric . tshow . vdcpt $ vd)
          | vd :: ViewDef <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.vdpos",
        "FormalAmpersand.View",
        "FormalAmpersand.Origin",
        Set.fromList [Uni],
        [ (dirtyId' vd, PopAlphaNumeric . tshow . origin $ vd)
          | vd :: ViewDef <- instanceList fSpec,
            origin vd `notElem` [OriginUnknown, MeatGrinder]
        ]
      ),
      ( "FormalAmpersand.versionInfo",
        "FormalAmpersand.Context",
        "FormalAmpersand.AmpersandVersion",
        Set.fromList [Uni, Tot],
        [ (dirtyId' ctx, PopAlphaNumeric (longVersion appVersion))
          | ctx :: A_Context <- instanceList fSpec
        ]
      ),
      ( "FormalAmpersand.viewBy",
        "FormalAmpersand.Concept",
        "FormalAmpersand.Concept",
        Set.empty,
        [] --TODO
      ),
      ( "FormalAmpersand.violatable",
        "FormalAmpersand.Interface",
        "FormalAmpersand.Rule",
        Set.empty,
        [] --TODO
      )
    ]

nameSpacePrototypeContext :: NameSpace
nameSpacePrototypeContext =
  [ case toNamePart "PrototypeContext" of
      Nothing -> fatal "Not a valid NamePart."
      Just np -> np
  ]

-- | The following transformers provide the metamodel needed to run a prototype.
--   Note: The information in transformersPrototypeContext is fully contained in FormalAmpersand.
--   You might do this by dropping all prefixes "" and "" and doing
--   the following transformation:
--     label[Role*Label]                -> name[Role*RoleName]
--   Then you will see that the transformers defined here are a subset of the FormalAmpersand transformers.
transformersPrototypeContext :: FSpec -> [Transformer]
transformersPrototypeContext fSpec =
  map
    (toTransformer nameSpacePrototypeContext)
    -- the following transformer is also contained in FormalAmpersand.
    [ ( "isAPI",
        "Interface",
        "Interface",
        Set.fromList [],
        [ (dirtyIdWithoutType' ifc, dirtyIdWithoutType' ifc)
          | ifc :: Interface <- instanceList fSpec,
            ifcIsAPI ifc
        ]
      ),
      -- the following transformer can be calculated by the Exec Engine.
      -- it is also contained in FormalAmpersand.
      ( "isPublic",
        "Interface",
        "Interface",
        Set.fromList [],
        [ (dirtyIdWithoutType' ifc, dirtyIdWithoutType' ifc)
          | ifc :: Interface <- instanceList fSpec,
            null (ifcRoles ifc)
        ]
      ),
      -- the following transformer is also contained in FormalAmpersand.
      ( "label",
        "Interface",
        "Label",
        Set.fromList [],
        [ (dirtyIdWithoutType' ifc, PopAlphaNumeric . label $ ifc)
          | ifc :: Interface <- instanceList fSpec
        ]
      ),
      -- the following transformer is called name[Role*RoleName] in FormalAmpersand
      ( "label",
        "Role",
        "Label",
        Set.fromList [Uni],
        [ (dirtyIdWithoutType' role, PopAlphaNumeric . label $ role)
          | role :: Role <- instanceList fSpec,
            isJust (rlLbl role)
        ]
      ),
      -- the following transformer is called ifcRoles[Interface*Role] in FormalAmpersand
      ( "ifcRoles",
        "Interface",
        "Role",
        Set.fromList [],
        [ (dirtyIdWithoutType' ifc, dirtyIdWithoutType' role)
          | ifc :: Interface <- instanceList fSpec,
            role <- ifcRoles ifc
        ]
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
