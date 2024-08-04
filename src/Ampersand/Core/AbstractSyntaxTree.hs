{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Ampersand.Core.AbstractSyntaxTree
  ( A_Context (..),
    Typology (..),
    MetaData (..),
    Origin (..),
    Pattern (..),
    PairView (..),
    PairViewSegment (..),
    Rule (..),
    Rules,
    RuleKind (..),
    AEnforce (..),
    Relation (..),
    Relations,
    ARelDefault (..),
    ARelDefaults,
    AProp (..),
    AProps,
    IdentityRule (..),
    IdentitySegment (..),
    ViewDef (..),
    ViewSegment (..),
    ViewSegmentPayLoad (..),
    AClassify (..),
    Interface (..),
    getInterfaceByName,
    SubInterface (..),
    BoxItem (..),
    ObjectDef (..),
    BoxTxt (..),
    Object (..),
    Cruds (..),
    Default (..),
    Purpose (..),
    ExplObj (..),
    Expression (..),
    getExpressionRelation,
    A_Concept (..),
    A_Concepts,
    AConceptDef (..),
    ShowWithAliases (..),
    Meaning (..),
    A_RoleRule (..),
    Representation (..),
    TType (..),
    unsafePAtomVal2AtomValue,
    safePSingleton2AAtomVal,
    Signature (..),
    Population (..),
    HasSignature (..),
    Traced (..),
    Conjunct (..),
    DnfClause (..),
    AAtomPair (..),
    AAtomPairs,
    AAtomValue (..),
    AAtomValues,
    mkAtomPair,
    PAtomValue (..),
    ContextInfo (..),
    showValADL,
    showValSQL,
    showSign,
    SignOrd (..),
    Type (..),
    typeOrConcept,
    -- , module Ampersand.Core.ParseTree  -- export all used constructors of the parsetree, because they have actually become part of the Abstract Syntax Tree.
    (.==.),
    (.|-.),
    (./\.),
    (.\/.),
    (.-.),
    (./.),
    (.\.),
    (.<>.),
    (.:.),
    (.!.),
    (.*.),
    makeConceptMap,
    ConceptMap,
  )
where

import Ampersand.ADL1.Lattices (Op1EqualitySystem)
import Ampersand.Basics
import Ampersand.Core.ParseTree
  ( BoxHeader (..),
    DefinitionContainer (..),
    EnforceOperator,
    MetaData (..),
    Origin (..),
    PAtomValue (..),
    PClassify (generics, specific),
    P_Concept (..),
    PairView (..),
    PairViewSegment (..),
    Pragma,
    Representation (..),
    Role (..),
    SrcOrTgt (..),
    TType (..),
    Traced (..),
    ViewHtmlTemplate (..),
    maybeOrdering,
    mkPConcept,
  )
import Data.Default (Default (..))
import qualified Data.Text1 as T1
import Data.Typeable (typeOf)
import RIO.Char (toLower, toUpper)
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import RIO.Time

data A_Context = ACtx
  { -- | The name of this context
    ctxnm :: !Name,
    ctxlbl :: !(Maybe Label),
    -- | The origin of the context. A context can be a merge of a file including other files c.q. a list of Origin.
    ctxpos :: ![Origin],
    -- | The default language used in this context.
    ctxlang :: !Lang,
    -- | The default markup format for free text in this context.
    ctxmarkup :: !PandocFormat,
    -- | The patterns defined in this context
    ctxpats :: ![Pattern],
    -- | All user defined rules in this context, but outside patterns
    ctxrs :: !Rules,
    -- | The relations that are declared in this context, outside the scope of patterns
    ctxds :: !Relations,
    -- | The user defined populations of relations defined in this context, including those from patterns
    ctxpopus :: ![Population],
    -- | The concept definitions defined outside the patterns of this context.
    ctxcdsOutPats :: ![AConceptDef],
    -- | The concept definitions defined in this context, including those from patterns
    ctxcds :: ![AConceptDef],
    -- | The identity definitions defined in this context, outside the scope of patterns
    ctxks :: ![IdentityRule],
    ctxrrules :: ![A_RoleRule],
    ctxreprs :: !(A_Concept -> TType),
    -- | The view definitions defined in this context, outside the scope of patterns
    ctxvs :: ![ViewDef],
    -- | The specialization statements defined in this context, outside the scope of patterns
    ctxgs :: ![AClassify],
    -- | A partitioning of all concepts: the union of all these concepts contains all atoms, and the concept-lists are mutually distinct in terms of atoms in one of the mentioned concepts
    ctxgenconcs :: ![[A_Concept]],
    -- | The interfaces defined in this context
    ctxifcs :: ![Interface],
    -- | The purposes of objects defined in this context, outside the scope of patterns
    ctxps :: ![Purpose],
    -- | used for Pandoc authors (and possibly other things)
    ctxmetas :: ![MetaData],
    ctxInfo :: !ContextInfo,
    -- | All user defined enforcement rules in this context, but outside patterns.
    ctxEnforces :: ![AEnforce]
  }
  deriving (Typeable)

instance Show A_Context where
  show = T.unpack . fullName

instance Eq A_Context where
  c1 == c2 = name c1 == name c2

instance Ord A_Context where
  a `compare` b = name a `compare` name b

instance Unique A_Context where
  showUnique = fullName1

instance Named A_Context where
  name = ctxnm

instance Labeled A_Context where
  mLabel = ctxlbl

data Pattern = A_Pat
  { -- | Name of this pattern
    ptnm :: !Name,
    ptlbl :: !(Maybe Label),
    -- | the position in the file in which this pattern was declared.
    ptpos :: !Origin,
    -- | the end position in the file, elements with a position between pos and end are elements of this pattern.
    ptend :: !Origin,
    -- | The user defined rules in this pattern
    ptrls :: !Rules,
    -- | The generalizations defined in this pattern
    ptgns :: ![AClassify],
    -- | The relations that are declared in this pattern
    ptdcs :: !Relations,
    -- | The role-rule assignments that are declared in this pattern
    ptrrs :: ![A_RoleRule],
    -- | The concept definitions that are declared in this pattern
    ptcds :: ![AConceptDef],
    -- | The concept definitions that are declared in this pattern
    ptrps :: ![Representation],
    -- | The user defined populations in this pattern
    ptups :: ![Population],
    -- | The identity definitions defined in this pattern
    ptids :: ![IdentityRule],
    -- | The view definitions defined in this pattern
    ptvds :: ![ViewDef],
    -- | The purposes of elements defined in this pattern
    ptxps :: ![Purpose],
    ptenfs :: ![AEnforce]
  }
  deriving (Typeable) -- Show for debugging purposes

instance Eq Pattern where
  a == b = compare a b == EQ

instance Unique Pattern where
  showUnique = fullName1

instance Ord Pattern where
  a `compare` b = name a `compare` name b

instance Named Pattern where
  name = ptnm

instance Labeled Pattern where
  mLabel = ptlbl

instance Traced Pattern where
  origin = ptpos

data AEnforce = AEnforce
  { pos :: !Origin,
    enfRel :: !Relation,
    enfOp :: !EnforceOperator,
    enfExpr :: !Expression,
    -- | If the enforcement rule is defined in the context of a pattern, the name of that pattern.
    enfPatName :: !(Maybe Text),
    enfRules :: ![Rule]
  }
  deriving (Eq)

instance Traced AEnforce where
  origin AEnforce {pos = orig} = orig

data AConceptDef = AConceptDef
  { -- | The position of this definition in the text of the Ampersand source (filename, line number and column number).
    pos :: !Origin,
    -- | The name of the concept for which this is the definition. If there is no such concept, the conceptdefinition is ignored.
    acdcpt :: !A_Concept,
    acdname :: !Name,
    acdlabel :: !(Maybe Label),
    -- | The textual definition of this concept.
    acddef2 :: !Meaning,
    -- | User-specified meanings, possibly more than one, for multiple languages.
    acdmean :: ![Meaning],
    -- | The name of the pattern or context in which this concept definition was made --TODO: Refactor to Maybe Pattern.
    acdfrom :: !DefinitionContainer
  }
  deriving (Show, Typeable)

instance Named AConceptDef where
  name = acdname

instance Traced AConceptDef where
  origin AConceptDef {pos = orig} = orig

instance Ord AConceptDef where
  compare a b = case compare (name a) (name b) of
    EQ ->
      fromMaybe
        ( fatal
            . T.intercalate "\n"
            $ [ "ConceptDef should have a non-fuzzy Origin.",
                tshow (origin a),
                tshow (origin b)
              ]
        )
        (maybeOrdering (origin a) (origin b))
    x -> x

instance Eq AConceptDef where
  a == b = compare a b == EQ

data A_RoleRule = A_RoleRule
  { arPos :: !Origin,
    arRoles :: !(NE.NonEmpty Role),
    arRules :: !(NE.NonEmpty Name) -- the names of the rules
  }
  deriving (Show)

instance Ord A_RoleRule where
  compare a b =
    fromMaybe
      ( fatal
          . T.intercalate "\n"
          $ [ "PPurpose a should have a non-fuzzy Origin.",
              tshow (origin a),
              tshow (origin b)
            ]
      )
      (maybeOrdering (origin a) (origin b))

instance Eq A_RoleRule where
  p1 == p2 = compare p1 p2 == EQ

instance Traced A_RoleRule where
  origin = arPos

data RuleKind
  = UserDefined -- This rule was specified explicitly as a rule in the Ampersand script
  | Propty !AProp !Relation
  | -- This rule follows implicitly from the Ampersand script (Because of a property) and generated by a computer
    Identity A_Concept -- This rule follows implicitly from the Ampersand script (Because of a identity) and generated by a computer
  | Enforce -- This rule follows implicitly from the Ampersand script (Because of an Enforce statement) and generated by a computer
  deriving (Show, Eq)

type Rules = Set.Set Rule

data Rule = Rule
  { -- | Name of this rule
    rrnm :: !Name,
    -- | Optional label
    rrlbl :: !(Maybe Label),
    -- | The term that should be True
    formalExpression :: !Expression,
    -- | Position in the Ampersand file
    rrfps :: !Origin,
    -- | Ampersand generated meaning (for all known languages)
    rrmean :: ![Meaning],
    -- | User-specified violation messages, possibly more than one, for multiple languages.
    rrmsg :: ![Markup],
    -- | Custom presentation for violations, currently only in a single language
    rrviol :: !(Maybe (PairView Expression)),
    -- | If the rule is defined in the context of a pattern, the label of that pattern for documentation purposes.
    rrpat :: !(Maybe Text),
    -- | Where does this rule come from?
    rrkind :: !RuleKind
  }
  deriving (Typeable)

instance Eq Rule where
  a == b = compare a b == EQ

instance Unique Rule where
  showUnique = fullName1

instance Ord Rule where
  compare = compare `on` name -- Origin should not be here: A check that they all have unique names is done before typechecking.

instance Show Rule where
  show x =
    T.unpack
      $ "RULE "
      <> text1ToText (fullName1 x)
      <> ": "
      <> tshow (formalExpression x)

instance Traced Rule where
  origin = rrfps

instance Named Rule where
  name = rrnm

instance Labeled Rule where
  mLabel = rrlbl

instance Hashable Rule where
  hashWithSalt s rul =
    s
      `hashWithSalt` name rul
      `hashWithSalt` formalExpression rul

data Conjunct = Cjct
  { rc_id :: !Text1, -- string that identifies this conjunct ('id' rather than 'name', because
  -- this is an internal id that has no counterpart at the ADL level)
    rc_orgRules :: !(NE.NonEmpty Rule), -- All rules this conjunct originates from
    rcConjunct :: !Expression,
    rc_dnfClauses :: ![DnfClause]
  }
  deriving (Show, Typeable)

data DnfClause = Dnf
  { antcs :: ![Expression],
    conss :: ![Expression]
  }
  deriving (Show, Eq) -- Show is for debugging purposes only.

{- The intended semantics of |Dnf ns ps| is the disjunction |foldr1 ( .\/. ) (map notCpl ns <> ps)|.
   The list |ns| and |ps| are not guaranteed to be sorted or duplicate-free.
-}

instance Eq Conjunct where
  a == b = compare a b == EQ

instance Unique Conjunct where
  showUnique = rc_id

instance Ord Conjunct where
  compare = compare `on` rc_id

type AProps = Set.Set AProp

data AProp
  = -- | univalent
    Uni
  | -- | injective
    Inj
  | -- | surjective
    Sur
  | -- | total
    Tot
  | -- | symmetric
    Sym
  | -- | antisymmetric
    Asy
  | -- | transitive
    Trn
  | -- | reflexive
    Rfx
  | -- | irreflexive
    Irf
  deriving (Eq, Ord, Data, Typeable)

instance Show AProp where
  show Uni = "UNI"
  show Inj = "INJ"
  show Sur = "SUR"
  show Tot = "TOT"
  show Sym = "SYM"
  show Asy = "ASY"
  show Trn = "TRN"
  show Rfx = "RFX"
  show Irf = "IRF"

instance Unique AProp where
  showUnique = toText1Unsafe . tshow

instance Flippable AProp where
  flp Uni = Inj
  flp Tot = Sur
  flp Sur = Tot
  flp Inj = Uni
  flp x = x

type ARelDefaults = Set ARelDefault

data ARelDefault
  = ARelDefaultAtom !SrcOrTgt !(NE.NonEmpty AAtomValue)
  | ARelDefaultEvalPHP !SrcOrTgt !Text
  deriving (Eq, Ord, Show, Data)

type Relations = Set.Set Relation

data Relation = Relation
  { -- | the name of the relation
    decnm :: !Name,
    -- | the source and target concepts of the relation
    decsgn :: !Signature,
    -- | a friendly user-readable alternative for the name
    declabel :: !(Maybe Label),
    -- | the user defined properties (Uni, Tot, Sur, Inj, Sym, Asy, Trn, Rfx, Irf)
    decprps :: !AProps,
    -- | the defaults for atoms in pairs in the population of this relation, used when populating relations at runtime
    decDefaults :: !ARelDefaults,
    -- | the pragma is a way to make the meaning of a relation explicit by examples.
    decpr :: !(Maybe Pragma),
    -- | the meaning of a relation, for each language supported by Ampersand.
    decMean :: ![Meaning],
    -- | the position in the Ampersand source file where this declaration is declared. Not all declarations come from the ampersand souce file.
    decfpos :: !Origin,
    -- | if true, this relation is declared by an author in the Ampersand script; otherwise it was generated by Ampersand.
    decusr :: !Bool,
    -- | If the relation is declared inside a pattern, the label of that pattern, just for documentation purposes.
    decpat :: !(Maybe Text),
    dechash :: !Int
  }
  deriving (Typeable, Data)

instance Eq Relation where
  a == b = compare a b == EQ

instance Ord Relation where
  compare a b = compare (name a, sign a) (name b, sign b)

instance Unique Relation where
  showUnique :: Relation -> Text1
  showUnique x = toText1Unsafe $ "Relation_" <> (tshow . abs . hash . text1ToText $ readable)
    where
      readable = showWithSign x

instance Hashable Relation where
  hashWithSalt s Relation {dechash = v} = s `hashWithSalt` v

instance Show Relation where
  show = T.unpack . text1ToText . showWithSign

showWithSign :: Relation -> Text1
showWithSign rel = fullName1 rel <> showSign rel

newtype Meaning = Meaning {ameaMrk :: Markup} deriving (Show, Eq, Ord, Typeable, Data)

instance Unique Meaning where
  showUnique x = toText1Unsafe $ "Meaning_" <> (tshow . abs . hash $ readable)
    where
      readable = tshow x

instance Named Relation where
  name = decnm

instance Labeled Relation where
  mLabel = declabel

instance HasSignature Relation where
  sign = decsgn

instance Traced Relation where
  origin = decfpos

data IdentityRule = Id
  { -- | The position of this definition in the text of the Ampersand source file (filename, line number and column number).
    idPos :: !Origin,
    -- | the name of this Identity. The name has no meaning in the Compliant Service Layer, but is used in the generated user interface.
    idName :: !Name,
    -- | a friendly, user readable alternative for the name
    idlabel :: !(Maybe Label),
    -- | this term describes the instances of this object, related to their context
    idCpt :: !A_Concept,
    -- | if defined within a pattern, then the label of that pattern.
    idPat :: !(Maybe Text),
    -- | the constituent attributes (i.e. name/term pairs) of this identity.
    identityAts :: NE.NonEmpty IdentitySegment
  }
  deriving (Show)

instance Named IdentityRule where
  name = idName

instance Traced IdentityRule where
  origin = idPos

instance Unique IdentityRule where
  showUnique = fullName1

instance Ord IdentityRule where
  compare a b = name a `compare` name b

instance Eq IdentityRule where
  a == b = compare a b == EQ

newtype IdentitySegment = IdentityExp
  { segment :: ObjectDef
  }
  deriving (Eq, Show) -- TODO: refactor to a list of terms

data ViewDef = Vd
  { -- | position of this definition in the text of the Ampersand source file (filename, line number and column number).
    vdpos :: !Origin,
    -- | the name of this View. The name has no meaning in the Compliant Service Layer, but is used in the generated user interface. It is not an empty string.
    vdname :: !Name,
    vdlabel :: !(Maybe Label),
    -- | the concept for which this view is applicable
    vdcpt :: !A_Concept,
    -- | whether or not this is the default view for the concept
    vdIsDefault :: !Bool,
    -- | the html template for this view (not required since we may have other kinds of views as well in the future)
    --                  , vdtext :: Maybe ViewText -- Future extension
    vdhtml :: !(Maybe ViewHtmlTemplate),
    -- | the constituent attributes (i.e. name/term pairs) of this view.
    vdats :: ![ViewSegment]
  }
  deriving (Show, Data)

instance Named ViewDef where
  name = vdname

instance Labeled ViewDef where
  mLabel = vdlabel

instance Traced ViewDef where
  origin = vdpos

instance Unique ViewDef where
  showUnique vd = toText1Unsafe "ViewDef_" <> fullName1 vd <> toText1Unsafe ("_" <> text1ToText (fullName1 (vdcpt vd)))

instance Eq ViewDef where
  a == b = compare a b == EQ

instance Ord ViewDef where
  a `compare` b = (name a, vdcpt a) `compare` (name b, vdcpt b)

data ViewSegment = ViewSegment
  { vsmpos :: !Origin,
    vsmlabel :: !(Maybe Text1),
    vsmSeqNr :: !Integer,
    vsmLoad :: !ViewSegmentPayLoad
  }
  deriving (Show, Data)

instance Traced ViewSegment where
  origin = vsmpos

data ViewSegmentPayLoad
  = ViewExp
      { vsgmExpr :: !Expression
      }
  | ViewText
      { vsgmTxt :: !Text
      }
  deriving (Eq, Show, Data)

-- | data structure AClassify contains the CLASSIFY statements from an Ampersand script
--   CLASSIFY Employee ISA Person   translates to Isa (C "Person") (C "Employee")
--   CLASSIFY Workingstudent IS Employee/\Student   translates to IsE orig (C "Workingstudent") [C "Employee",C "Student"]
data AClassify
  = Isa
      { genpos :: !Origin,
        -- | specific concept
        genspc :: !A_Concept,
        -- | generic concept
        gengen :: !A_Concept
      }
  | IsE
      { genpos :: !Origin,
        -- | specific concept
        genspc :: !A_Concept,
        -- | concepts of which the conjunction is equivalent to the specific concept
        genrhs :: !(NE.NonEmpty A_Concept)
      }
  deriving (Typeable)

instance Ord AClassify where
  -- subjective choice: Isa > IsE
  compare a b = case (a, b) of
    (Isa {}, Isa {}) -> compare (genspc a, gengen a) (genspc b, gengen b)
    (Isa {}, IsE {}) -> GT
    (IsE {}, IsE {}) ->
      let fun = NE.nub . NE.sort . genrhs
       in compare (genspc a, fun a) (genspc b, fun b)
    (IsE {}, Isa {}) -> LT

instance Eq AClassify where
  a == b = compare a b == EQ

instance Traced AClassify where
  origin = genpos

instance Unique AClassify where
  showUnique a = toText1Unsafe $ "Classify_" <> (tshow . abs . hash . text1ToText $ readable)
    where
      readable = case a of
        Isa {} -> showUnique (genspc a) <> toText1Unsafe " ISA " <> showUnique (gengen a)
        IsE {} -> showUnique (genspc a) <> toText1Unsafe " IS " <> toText1Unsafe (T.intercalate " /\\ " (NE.toList . fmap (text1ToText . showUnique) $ genrhs a))

instance Show AClassify where
  -- This show is used in error messages. It should therefore not display the term's type
  show g =
    case g of
      Isa {} -> "CLASSIFY " <> show (genspc g) <> " ISA " <> show (gengen g)
      IsE {} -> "CLASSIFY " <> show (genspc g) <> " IS " <> L.intercalate " /\\ " (NE.toList . fmap show $ genrhs g)

instance Hashable AClassify where
  hashWithSalt s g =
    s
      `hashWithSalt` genspc g
      `hashWithSalt` ( case g of
                         Isa {} -> [genspc g]
                         IsE {} -> NE.toList . NE.sort $ genrhs g
                     )

data Interface = Ifc
  { -- | is this interface of type API?
    ifcIsAPI :: !Bool,
    -- | The name of the interface
    ifcname :: !Name,
    ifclbl :: !(Maybe Label),
    -- | All roles for which an interface is available (empty means: available for all roles)
    ifcRoles :: ![Role],
    -- | NOTE: this top-level ObjectDef contains the interface itself (ie. name and expression)
    ifcObj :: !ObjectDef,
    -- | All conjuncts that must be evaluated after a transaction
    ifcConjuncts :: ![Conjunct],
    -- | The position in the file (filename, line- and column number)
    ifcPos :: !Origin,
    -- | The purpose of the interface
    ifcPurpose :: !Text
  }
  deriving (Show)

instance Eq Interface where
  a == b = compare a b == EQ

instance Ord Interface where
  compare a b = compare (name a) (name b)

instance Named Interface where
  name = ifcname

instance Labeled Interface where
  mLabel = ifclbl

instance Traced Interface where
  origin = ifcPos

instance Unique Interface where
  showUnique = fullName1

-- Utility function for looking up interface refs
getInterfaceByName :: [Interface] -> Name -> Interface
getInterfaceByName interfaces' nm = case [ifc | ifc <- interfaces', name ifc == nm] of
  [] -> fatal $ "getInterface by name: no interfaces named " <> tshow nm
  [ifc] -> ifc
  _ -> fatal $ "getInterface by name: multiple interfaces named " <> tshow nm

class Object a where
  concept :: a -> A_Concept -- the type of the object
  fields :: a -> [ObjectDef] -- the objects directly defined within the object
  contextOf :: a -> Expression -- the context term
  fieldsRecursive :: a -> [ObjectDef] -- the objects defined within the object and its subinterfaces

instance Object ObjectDef where
  concept = target . objExpression
  fields obj = case objmsub obj of
    Nothing -> []
    Just InterfaceRef {} -> []
    Just b@Box {} -> map objE . filter isObjExp $ siObjs b
  contextOf = objExpression
  fieldsRecursive obj = fields obj <> subFields obj
    where
      subFields :: ObjectDef -> [ObjectDef]
      subFields x = case objmsub x of
        Nothing -> []
        Just si@Box {} -> concatMap (fieldsRecursive . objE) (filter isObjExp . siObjs $ si)
        Just InterfaceRef {} -> []

data BoxItem
  = BxExpr {objE :: !ObjectDef}
  | BxTxt {objT :: !BoxTxt}
  deriving (Eq, Ord, Show)

isObjExp :: BoxItem -> Bool
isObjExp BxExpr {} = True
isObjExp BxTxt {} = False

instance Unique BoxItem where
  showUnique = showUniqueAsHash

instance Traced BoxItem where
  origin o =
    case o of
      BxExpr {} -> origin . objE $ o
      BxTxt {} -> origin . objT $ o

data BoxTxt = BoxTxt
  { -- | view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
    boxPlainName :: !(Maybe Text1),
    boxpos :: !Origin,
    boxtxt :: !Text
  }
  deriving (Show)

instance Ord BoxTxt where
  compare a b = case compare (boxPlainName a, boxtxt a) (boxPlainName b, boxtxt b) of
    EQ ->
      fromMaybe
        ( fatal
            . T.intercalate "\n"
            $ [ "BoxTxt should have a non-fuzzy Origin.",
                tshow (origin a),
                tshow (origin b)
              ]
        )
        (maybeOrdering (origin a) (origin b))
    x -> x

instance Eq BoxTxt where
  a == b = compare a b == EQ

data ObjectDef = ObjectDef
  { -- | view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
    objPlainName :: !(Maybe Text1),
    objlbl :: !(Maybe Label),
    -- | position of this definition in the text of the Ampersand source file (filename, line number and column number)
    objPos :: !Origin,
    -- | this term describes the instances of this object, related to their context.
    objExpression :: !Expression,
    -- | CRUD as defined by the user
    objcrud :: !Cruds,
    -- | The view that should be used for this object
    objmView :: !(Maybe Name),
    -- | the fields, which are object definitions themselves.
    objmsub :: !(Maybe SubInterface)
  }
  deriving (Show) -- just for debugging (zie ook instance Show BoxItem)

instance Traced ObjectDef where
  origin = objPos

instance Unique ObjectDef where
  showUnique x = toText1Unsafe ("ObjectDef_" <> (tshow . abs . hash . tshow) x)

instance Ord ObjectDef where
  compare a b = case compare (objPlainName a) (objPlainName b) of
    EQ ->
      fromMaybe
        ( fatal
            . T.intercalate "\n"
            $ [ "ObjectDef should have a non-fuzzy Origin.",
                tshow (origin a),
                tshow (origin b)
              ]
        )
        (maybeOrdering (origin a) (origin b))
    x -> x

instance Eq ObjectDef where
  a == b = compare a b == EQ

instance Traced BoxTxt where
  origin = boxpos

data Cruds = Cruds
  { crudOrig :: !Origin,
    crudC :: !Bool,
    crudR :: !Bool,
    crudU :: !Bool,
    crudD :: !Bool
  }
  deriving (Data)

instance Show Cruds where
  show x =
    uncurry (\upper -> if upper then toUpper else toLower)
      <$> [ (crudC x, 'C'),
            (crudR x, 'R'),
            (crudU x, 'U'),
            (crudD x, 'D')
          ]

data SubInterface
  = Box
      { pos :: !Origin,
        siConcept :: !A_Concept,
        siHeader :: !BoxHeader,
        siObjs :: ![BoxItem]
      }
  | InterfaceRef
      { pos :: !Origin,
        siIsLink :: !Bool,
        siIfcId :: !Name -- id of the interface that is referenced to
      }
  deriving (Show)

instance Traced SubInterface where
  origin Box {pos = orig} = orig
  origin InterfaceRef {pos = orig} = orig

instance Ord SubInterface where
  compare a b = case (a, b) of
    (Box {}, Box {}) -> compare (siConcept a, siHeader a, siObjs a) (siConcept b, siHeader b, siObjs b)
    (Box {}, InterfaceRef {}) -> GT
    (InterfaceRef {}, InterfaceRef {}) -> compare (siIsLink a, siIfcId a) (siIsLink b, siIfcId b)
    (InterfaceRef {}, Box {}) -> LT

instance Eq SubInterface where
  a == b = compare a b == EQ

instance Unique SubInterface where
  showUnique si@Box {} = "BOX_" T1..<> (showUnique . siHeader) si T1.<>. (T.concat . fmap (text1ToText . showUnique) . siObjs) si
  showUnique si@InterfaceRef {} = "InterfaceRef_" T1..<> (showUnique . siIsLink) si <> fullName1 (siIfcId si)

-- | Explanation is the intended constructor. It explains the purpose of the object it references.
--   The enrichment process of the parser must map the names (from PPurpose) to the actual objects
data Purpose = Expl
  { -- | The position in the Ampersand script of this purpose definition
    explPos :: !Origin,
    -- | The object that is explained.
    explObj :: !ExplObj,
    -- | This field contains the text of the explanation including language and markup info.
    explMarkup :: !Markup,
    -- | Is this purpose defined in the script?
    explUserdefd :: !Bool,
    -- | The references of the explaination
    explRefIds :: ![Text]
  }
  deriving (Show, Typeable)

-- instance Eq Purpose where
--  a == b = compare a b == EQ

instance Ord Purpose where
  compare a b = case compare (explObj a) (explObj b) of
    EQ ->
      fromMaybe
        ( fatal
            . T.intercalate "\n"
            $ [ "Purpose should have a non-fuzzy Origin.",
                tshow (origin a),
                tshow (origin b)
              ]
        )
        (maybeOrdering (origin a) (origin b))
    x -> x

instance Eq Purpose where
  a == b = compare a b == EQ

instance Unique Purpose where
  showUnique p = toText1Unsafe $ "Purpose_" <> (tshow . abs . hash $ readable)
    where
      readable = text1ToText $ uniqueShowWithType (explMarkup p) <> toText1Unsafe (tshow (typeOf orig) <> "_" <> tshow orig)
      orig = origin p

instance Traced Purpose where
  origin = explPos

data Population -- The user defined populations
  = ARelPopu
      { popdcl :: !Relation,
        popps :: !AAtomPairs, -- The user-defined pairs that populate the relation
        popsrc :: !A_Concept, -- potentially more specific types than the type of Relation
        poptgt :: !A_Concept
      }
  | ACptPopu
      { popcpt :: !A_Concept,
        popas :: ![AAtomValue] -- The user-defined atoms that populate the concept
      }
  deriving (Eq, Ord, Show)

instance Unique Population where
  showUnique pop = toText1Unsafe $ "Population_" <> (tshow . abs . hash $ readable)
    where
      readable = text1ToText $ case pop of
        ARelPopu {} -> (uniqueShowWithType . popdcl) pop <> (showUnique . popps) pop
        ACptPopu {} -> (uniqueShowWithType . popcpt) pop <> (showUnique . popas) pop

type AAtomPairs = Set.Set AAtomPair

data AAtomPair = APair
  { apLeft :: !AAtomValue,
    apRight :: !AAtomValue
  }
  deriving (Eq, Ord, Show)

mkAtomPair :: AAtomValue -> AAtomValue -> AAtomPair
mkAtomPair = APair

instance Unique AAtomPair where
  showUnique apair = toText1Unsafe $ "AAtomPair_" <> (tshow . abs . hash . text1ToText $ readable)
    where
      readable = toText1Unsafe "(" <> (showUnique . apLeft) apair <> (toText1Unsafe "," <> ((showUnique . apRight) apair <> toText1Unsafe ")"))

type AAtomValues = Set.Set AAtomValue

data AAtomValue
  = AAVString
      { aavhash :: !Int,
        aavtyp :: !TType,
        aavtxt :: !Text
      }
  | AAVInteger
      { aavtyp :: !TType,
        aavint :: !Integer
      }
  | AAVFloat
      { aavtyp :: !TType,
        aavflt :: !Double
      }
  | AAVBoolean
      { aavtyp :: !TType,
        aavbool :: !Bool
      }
  | AAVDate
      { aavtyp :: !TType,
        aadateDay :: !Day
      }
  | AAVDateTime
      { aavtyp :: !TType,
        aadatetime :: !UTCTime
      }
  | AtomValueOfONE
  deriving (Eq, Ord, Show, Data)

instance Unique AAtomValue where -- FIXME:  this in incorrect! (AAtomValue should probably not be in Unique at all. We need to look into where this is used for.)
  showUnique value = toText1Unsafe $ "AtomValue_" <> (tshow . abs . hash $ readable)
    where
      readable = case value of
        AAVString {} -> tshow (aavhash value)
        AAVInteger {} -> tshow (aavint value)
        AAVFloat {} -> tshow (aavflt value)
        AAVBoolean {} -> tshow (aavbool value)
        AAVDate {} -> tshow (aadateDay value)
        AAVDateTime {} -> tshow (aadatetime value)
        AtomValueOfONE -> text1ToText (fullName1 nameOfONE)

showValSQL :: AAtomValue -> Text
showValSQL val =
  case val of
    AAVString {} -> singleQuote . f . aavtxt $ val
      where
        f :: Text -> Text
        f txt = case T.uncons txt of
          Nothing -> mempty
          Just (h, tl)
            | h `elem` ['\'', '\\'] ->
                T.cons h (T.cons h (f tl))
            | otherwise -> T.cons h (f tl)
    AAVInteger {} -> tshow (aavint val)
    AAVBoolean {} -> tshow (aavbool val)
    AAVDate {} -> singleQuote . T.pack $ showGregorian (aadateDay val)
    AAVDateTime {} -> singleQuote . T.pack $ formatTime defaultTimeLocale "%F %T" (aadatetime val) -- NOTE: MySQL 5.5 does not comply to ISO standard. This format is MySQL specific
    -- formatTime SL.defaultTimeLocale "%FT%T%QZ" (aadatetime val)
    AAVFloat {} -> tshow (aavflt val)
    AtomValueOfONE {} -> "1"

singleQuote :: Text -> Text
singleQuote str = "'" <> str <> "'"

showValADL :: AAtomValue -> Text
showValADL val =
  case val of
    AAVString {} -> aavtxt val
    AAVInteger {} -> tshow (aavint val)
    AAVBoolean {} -> tshow (aavbool val)
    AAVDate {} -> T.pack $ showGregorian (aadateDay val)
    AAVDateTime {} -> T.pack $ formatTime defaultTimeLocale "%FT%T%QZ" (aadatetime val)
    AAVFloat {} -> tshow (aavflt val)
    AtomValueOfONE {} -> "1"

data ExplObj
  = ExplConcept !A_Concept
  | ExplRelation !Relation
  | ExplRule !Name
  | ExplIdentityDef !Name
  | ExplViewDef !Name
  | ExplPattern !Name
  | ExplInterface !Name
  | ExplContext !Name
  deriving (Show, Eq, Typeable, Ord)

instance Unique ExplObj where
  showUnique e = toText1Unsafe $ "Explanation_" <> (tshow . abs . hash $ readable)
    where
      readable =
        "Explanation of "
          <> text1ToText
            ( case e of
                (ExplConcept cpt) -> uniqueShowWithType cpt
                (ExplRelation rel) -> uniqueShowWithType rel
                (ExplRule s) -> toText1Unsafe "a Rule named " <> fullName1 s
                (ExplIdentityDef s) -> toText1Unsafe "an Ident named " <> fullName1 s
                (ExplViewDef s) -> toText1Unsafe "a View named " <> fullName1 s
                (ExplPattern s) -> toText1Unsafe "a Pattern named " <> fullName1 s
                (ExplInterface s) -> toText1Unsafe "an Interface named " <> fullName1 s
                (ExplContext s) -> toText1Unsafe "a Context named " <> fullName1 s
            )

data Expression
  = -- | equivalence             =
    EEqu !(Expression, Expression)
  | -- | inclusion               |-
    EInc !(Expression, Expression)
  | -- | intersection            /\
    EIsc !(Expression, Expression)
  | -- | union                   \/
    EUni !(Expression, Expression)
  | -- | difference              -
    EDif !(Expression, Expression)
  | -- | left residual           /
    ELrs !(Expression, Expression)
  | -- | right residual          \
    ERrs !(Expression, Expression)
  | -- | diamond                 <>
    EDia !(Expression, Expression)
  | -- | composition             ;
    ECps !(Expression, Expression)
  | -- | relative addition       !
    ERad !(Expression, Expression)
  | -- | cartesian product       *
    EPrd !(Expression, Expression)
  | -- | Rfx.Trn closure         *  (Kleene star)
    EKl0 !Expression
  | -- | Transitive closure      +  (Kleene plus)
    EKl1 !Expression
  | -- | conversion (flip, wok)  ~
    EFlp !Expression
  | -- | Complement
    ECpl !Expression
  | -- | bracketed expression ( ... )
    EBrk !Expression
  | -- | simple relation
    EDcD !Relation
  | -- | Identity relation
    EDcI !A_Concept
  | -- | Epsilon relation (introduced by the system to ensure we compare concepts by equality only.
    EEps !A_Concept !Signature
  | -- | Cartesian product relation
    EDcV !Signature
  | -- | constant PAtomValue, because when building the Expression, the TType of the concept isn't known yet.
    EMp1 !PAtomValue !A_Concept
  deriving (Eq, Ord, Show, Typeable, Generic, Data)

instance Hashable Expression where
  hashWithSalt s expr =
    s
      `hashWithSalt` case expr of
        EEqu (a, b) -> (0 :: Int) `hashWithSalt` a `hashWithSalt` b
        EInc (a, b) -> (1 :: Int) `hashWithSalt` a `hashWithSalt` b
        EIsc (a, b) -> (2 :: Int) `hashWithSalt` a `hashWithSalt` b
        EUni (a, b) -> (3 :: Int) `hashWithSalt` a `hashWithSalt` b
        EDif (a, b) -> (4 :: Int) `hashWithSalt` a `hashWithSalt` b
        ELrs (a, b) -> (5 :: Int) `hashWithSalt` a `hashWithSalt` b
        ERrs (a, b) -> (6 :: Int) `hashWithSalt` a `hashWithSalt` b
        EDia (a, b) -> (7 :: Int) `hashWithSalt` a `hashWithSalt` b
        ECps (a, b) -> (8 :: Int) `hashWithSalt` a `hashWithSalt` b
        ERad (a, b) -> (9 :: Int) `hashWithSalt` a `hashWithSalt` b
        EPrd (a, b) -> (10 :: Int) `hashWithSalt` a `hashWithSalt` b
        EKl0 e -> (11 :: Int) `hashWithSalt` e
        EKl1 e -> (12 :: Int) `hashWithSalt` e
        EFlp e -> (13 :: Int) `hashWithSalt` e
        ECpl e -> (14 :: Int) `hashWithSalt` e
        EBrk e -> (15 :: Int) `hashWithSalt` e
        EDcD d -> (16 :: Int) `hashWithSalt` d
        EDcI c -> (17 :: Int) `hashWithSalt` c
        EEps c sgn -> (18 :: Int) `hashWithSalt` c `hashWithSalt` sgn
        EDcV sgn -> (19 :: Int) `hashWithSalt` sgn
        EMp1 val c -> (21 :: Int) `hashWithSalt` show val `hashWithSalt` c

instance Unique Expression where
  showUnique x = toText1Unsafe $ "Expression_" <> (tshow . abs . hash $ readable)
    where
      readable = tshow x

instance Unique (PairView Expression) where
  showUnique x = toText1Unsafe $ "PairView_" <> (tshow . abs . hash $ readable)
    where
      readable = tshow x

instance Unique (PairViewSegment Expression) where
  showUnique x = toText1Unsafe $ "PairViewSegment_Expression_" <> (tshow . abs . hash $ readable)
    where
      readable = tshow x

(.==.), (.|-.), (./\.), (.\/.), (.-.), (./.), (.\.), (.<>.), (.:.), (.!.), (.*.) :: Expression -> Expression -> Expression

infixl 1 .==. -- equivalence

infixl 1 .|-. -- inclusion

infixl 2 ./\. -- intersection

infixl 2 .\/. -- union

infixl 4 .-. -- difference

infixl 6 ./. -- left residual

infixl 6 .\. -- right residual

infixl 6 .<>. -- diamond

infixl 8 .:. -- composition    -- .;. was unavailable, because Haskell's scanner does not recognize it as an operator.

infixl 8 .!. -- relative addition

infixl 8 .*. -- cartesian product

-- SJ 20130118: The fatals are superfluous, but only if the type checker works correctly. For that reason, they are not being removed. Not even for performance reasons.
l .==. r =
  if source l /= source r || target l /= target r
    then fatal ("Cannot equate (with operator \"==\") term l of type " <> tshow (sign l) <> ":\n    " <> tshow l <> "\n  with term r of type " <> tshow (sign r) <> ":\n    " <> tshow r <> ".")
    else EEqu (l, r)

l .|-. r =
  if source l /= source r || target l /= target r
    then fatal ("Cannot include (with operator \"|-\") term l of type " <> tshow (sign l) <> ":\n    " <> tshow l <> "\n  with term r of type " <> tshow (sign r) <> ":\n    " <> tshow r <> ".")
    else EInc (l, r)

l ./\. r =
  if source l /= source r || target l /= target r
    then fatal ("Cannot intersect (with operator \"/\\\") term l of type " <> tshow (sign l) <> ":\n    " <> tshow l <> "\n  with term r of type " <> tshow (sign r) <> ":\n    " <> tshow r <> ".")
    else EIsc (l, r)

l .\/. r =
  if source l /= source r || target l /= target r
    then fatal ("Cannot unite (with operator \"\\/\") term l of type " <> tshow (sign l) <> ":\n    " <> tshow l <> "\n  with term r of type " <> tshow (sign r) <> ":\n    " <> tshow r <> ".")
    else EUni (l, r)

l .-. r =
  if source l /= source r || target l /= target r
    then fatal ("Cannot subtract (with operator \"-\") term l of type " <> tshow (sign l) <> ":\n    " <> tshow l <> "\n  with term r of type " <> tshow (sign r) <> ":\n    " <> tshow r <> ".")
    else EDif (l, r)

l ./. r =
  if target l /= target r
    then fatal ("Cannot residuate (with operator \"/\") term l of type " <> tshow (sign l) <> ":\n    " <> tshow l <> "\n  with term r of type " <> tshow (sign r) <> ":\n    " <> tshow r <> ".")
    else ELrs (l, r)

l .\. r =
  if source l /= source r
    then fatal ("Cannot residuate (with operator \"\\\") term l of type " <> tshow (sign l) <> ":\n    " <> tshow l <> "\n  with term r of type " <> tshow (sign r) <> ":\n    " <> tshow r <> ".")
    else ERrs (l, r)

l .<>. r =
  if source r /= target l
    then fatal ("Cannot use diamond operator \"<>\") term l of type " <> tshow (sign l) <> ":\n    " <> tshow l <> "\n  with term r of type " <> tshow (sign r) <> ":\n    " <> tshow r <> ".")
    else EDia (l, r)

l .:. r =
  if source r /= target l
    then fatal ("Cannot compose (with operator \";\") term l of type " <> tshow (sign l) <> ":\n    " <> tshow l <> "\n  with term r of type " <> tshow (sign r) <> ":\n    " <> tshow r <> ".")
    else ECps (l, r)

l .!. r =
  if source r /= target l
    then fatal ("Cannot add (with operator \"!\") term l of type " <> tshow (sign l) <> ":\n    " <> tshow l <> "\n  with term r of type " <> tshow (sign r) <> ":\n    " <> tshow r <> ".")
    else ERad (l, r)

l .*. r =
  -- SJC: always fits! No fatal here..
  EPrd (l, r)

{- For the operators /, \, ;, ! and * we must not check whether the intermediate types exist.
   Suppose the user says GEN Student ISA Person and GEN Employee ISA Person, then Student `join` Employee has a name (i.e. Person), but Student `meet` Employee
   does not. In that case, -(r!s) (with target r=Student and source s=Employee) is defined, but -r;-s is not.
   So in order to let -(r!s) be equal to -r;-s we must not check for the existence of these types, for the Rotterdam paper already shows that this is fine.
-}

instance Flippable Expression where
  flp expr = case expr of
    EEqu (l, r) -> EEqu (flp l, flp r)
    EInc (l, r) -> EInc (flp l, flp r)
    EIsc (l, r) -> EIsc (flp l, flp r)
    EUni (l, r) -> EUni (flp l, flp r)
    EDif (l, r) -> EDif (flp l, flp r)
    ELrs (l, r) -> ERrs (flp r, flp l)
    ERrs (l, r) -> ELrs (flp r, flp l)
    EDia (l, r) -> EDia (flp r, flp l)
    ECps (l, r) -> ECps (flp r, flp l)
    ERad (l, r) -> ERad (flp r, flp l)
    EPrd (l, r) -> EPrd (flp r, flp l)
    EFlp e -> e
    ECpl e -> ECpl (flp e)
    EKl0 e -> EKl0 (flp e)
    EKl1 e -> EKl1 (flp e)
    EBrk f -> EBrk (flp f)
    EDcD {} -> EFlp expr
    EDcI {} -> expr
    EEps i sgn -> EEps i (flp sgn)
    EDcV sgn -> EDcV (flp sgn)
    EMp1 {} -> expr

instance HasSignature Expression where
  sign (EEqu (l, r)) = Sign (source l) (target r)
  sign (EInc (l, r)) = Sign (source l) (target r)
  sign (EIsc (l, r)) = Sign (source l) (target r)
  sign (EUni (l, r)) = Sign (source l) (target r)
  sign (EDif (l, r)) = Sign (source l) (target r)
  sign (ELrs (l, r)) = Sign (source l) (source r)
  sign (ERrs (l, r)) = Sign (target l) (target r)
  sign (EDia (l, r)) = Sign (source l) (target r)
  sign (ECps (l, r)) = Sign (source l) (target r)
  sign (ERad (l, r)) = Sign (source l) (target r)
  sign (EPrd (l, r)) = Sign (source l) (target r)
  sign (EKl0 e) = sign e
  sign (EKl1 e) = sign e
  sign (EFlp e) = flp (sign e)
  sign (ECpl e) = sign e
  sign (EBrk e) = sign e
  sign (EDcD d) = sign d
  sign (EDcI c) = Sign c c
  sign (EEps _ sgn) = sgn
  sign (EDcV sgn) = sgn
  sign (EMp1 _ c) = Sign c c

showSign :: (HasSignature a) => a -> Text1
showSign x = Text1 '[' $ fullName s <> "*" <> fullName t <> "]"
  where
    Sign s t = sign x

-- We allow editing on basic relations (Relations) that may have been flipped, or narrowed/widened by composing with I.
-- Basically, we have a relation that may have several epsilons to its left and its right, and the source/target concepts
-- we use are the concepts in the innermost epsilon, or the source/target concept of the relation, in absence of epsilons.
-- This is used to determine the type of the atoms provided by the outside world through interfaces.
getExpressionRelation :: Expression -> Maybe (A_Concept, Relation, A_Concept, Bool)
getExpressionRelation expr = case getRelation expr of
  Just (s, Just d, t, isFlipped) -> Just (s, d, t, isFlipped)
  _ -> Nothing
  where
    -- If the expression represents an editable relation, the relation is returned together with the narrowest possible source and target
    -- concepts, as well as a boolean that states whether the relation is flipped.
    getRelation :: Expression -> Maybe (A_Concept, Maybe Relation, A_Concept, Bool)
    getRelation (ECps (e, EDcI {})) = getRelation e
    getRelation (ECps (EDcI {}, e)) = getRelation e
    getRelation (ECps (e1, e2)) =
      case (getRelation e1, getRelation e2) of -- note: target e1==source e2
        (Just (_, Nothing, i1, _), Just (i2, Nothing, _, _))
          | i1 == target e1 && i2 == source e2 -> Just (i1, Nothing, i2, False)
          | i1 == target e1 && i2 /= source e2 -> Just (i2, Nothing, i2, False)
          | i1 /= target e1 && i2 == source e2 -> Just (i1, Nothing, i1, False)
          | otherwise -> Nothing
        (Just (_, Nothing, i, _), Just (s, d, t, isFlipped))
          | i == target e1 -> Just (s, d, t, isFlipped)
          | i /= target e1 && s == target e1 -> Just (i, d, t, isFlipped)
          | otherwise -> Nothing
        (Just (s, d, t, isFlipped), Just (i, Nothing, _, _))
          | i == source e2 -> Just (s, d, t, isFlipped)
          | i /= source e2 && t == source e2 -> Just (s, d, i, isFlipped)
          | otherwise -> Nothing
        _ -> Nothing
    getRelation (EFlp e) =
      case getRelation e of
        Just (s, d, t, isFlipped) -> Just (t, d, s, not isFlipped)
        Nothing -> Nothing
    getRelation (EDcD d) = Just (source d, Just d, target d, False)
    getRelation (EEps i _) = Just (i, Nothing, i, False)
    getRelation _ = Nothing

-- The following definition of concept is used in the type checker only.
-- It is called Concept, meaning "type checking concept"

data A_Concept
  = PlainConcept
      { -- | List of names that the concept is refered to, in random order
        aliases :: !(NE.NonEmpty (Name, Maybe Label))
      }
  | -- | The universal Singleton: 'I'['Anything'] = 'V'['Anything'*'Anything']
    ONE
  deriving (Typeable, Data, Ord, Eq)

type A_Concepts = Set.Set A_Concept

{- -- this is faster, so if you think Eq on concepts is taking a long time, try this..
instance Ord A_Concept where
  compare (PlainConcept{cpthash=v1}) (PlainConcept{cpthash=v2}) = compare v1 v2
  compare ONE ONE = EQ
  compare ONE (PlainConcept{}) = LT
  compare (PlainConcept{}) ONE = GT

instance Eq A_Concept where
  a == b = compare a b == EQ

-}

instance Unique AConceptDef where
  showUnique = toText1Unsafe . fullName

instance Unique A_Concept where
  showUnique = toText1Unsafe . tshow

instance Hashable A_Concept where
  hashWithSalt s cpt =
    s
      `hashWithSalt` ( case cpt of
                         PlainConcept {} -> (0 :: Int) `hashWithSalt` (fst . NE.head . NE.sort $ aliases cpt)
                         ONE -> 1 :: Int
                     )

instance Named A_Concept where
  name PlainConcept {aliases = names} = fst . NE.head $ names
  name ONE = nameOfONE

instance Labeled A_Concept where
  mLabel cpt = case cpt of
    PlainConcept {} -> snd . NE.head . aliases $ cpt
    ONE -> Nothing

instance Show A_Concept where
  show = T.unpack . fullName

-- | special type of Show, for types that can have aliases. Its purpose is
--   to use when giving feedback to the ampersand modeler, in cases aliases
--   are used.
class (Show a) => ShowWithAliases a where
  showWithAliases :: a -> Text1

instance ShowWithAliases A_Concept where
  showWithAliases ONE = fullName1 ONE
  showWithAliases cpt@PlainConcept {} =
    case NE.tail (fst <$> aliases cpt) of
      [] -> fullName1 cpt
      xs -> fullName1 cpt <> toText1Unsafe ("(" <> T.intercalate ", " (fullName <$> xs) <> ")")

instance Unique (A_Concept, PAtomValue) where
  showUnique (c, val) = toText1Unsafe $ "ConceptAtomValue_" <> (tshow . abs . hash $ readable)
    where
      readable = tshow val <> "[" <> text1ToText (showUnique c) <> "]"

data Signature = Sign !A_Concept !A_Concept deriving (Eq, Ord, Typeable, Generic, Data)

instance Hashable Signature

instance Show Signature where
  show (Sign s t) =
    "[" <> show s <> "*" <> show t <> "]"

instance ShowWithAliases Signature where
  showWithAliases (Sign s t) =
    toText1Unsafe "[" <> showWithAliases s <> toText1Unsafe "*" <> showWithAliases t <> toText1Unsafe "]"

instance Unique Signature where
  showUnique (Sign s t) = toText1Unsafe $ "Signature" <> (tshow . abs . hash $ readable)
    where
      readable = "[" <> text1ToText (showUnique s) <> "*" <> text1ToText (showUnique t) <> "]"

instance HasSignature Signature where
  source (Sign s _) = s
  target (Sign _ t) = t
  sign sgn = sgn

instance Flippable Signature where
  flp (Sign s t) = Sign t s

class HasSignature a where
  source, target :: a -> A_Concept
  source = source . sign
  target = target . sign
  sign :: a -> Signature
  isEndo :: a -> Bool
  isEndo s = source s == target s

--  {-# MINIMAL sign #-}

-- Convenient data structure to hold information about concepts and their representations
--  in a context.
data ContextInfo = CI
  { -- | The generalisation relations in the context
    ctxiGens :: ![AClassify],
    -- | a list containing all user defined Representations in the context
    representationOf :: !(A_Concept -> TType),
    -- | a list of typologies, based only on the CLASSIFY statements. Single-concept typologies are not included
    multiKernels :: ![Typology],
    -- | a list of all Representations
    reprList :: ![Representation],
    -- | a map of declarations and the corresponding types
    declDisambMap :: !(Map.Map Name (Map.Map SignOrd Expression)),
    -- | types not used in any declaration
    soloConcs :: !(Set.Set Type),
    -- | generalisation relations again, as a type system (including phantom types)
    gens_efficient :: !(Op1EqualitySystem Type),
    -- | a map that must be used to convert P_Concept to A_Concept
    conceptMap :: !ConceptMap,
    -- | the default language used to interpret markup texts in this context
    defaultLang :: !Lang,
    -- | the default format used to interpret markup texts in this context
    defaultFormat :: !PandocFormat
  }

typeOrConcept :: ConceptMap -> Type -> Either A_Concept (Maybe TType)
typeOrConcept fun (BuiltIn TypeOfOne) = Left . fun $ mkPConcept nameOfONE Nothing
typeOrConcept fun (UserConcept (nm, lbl)) = Left . fun $ mkPConcept nm lbl
typeOrConcept _ (BuiltIn x) = Right (Just x)
typeOrConcept _ RepresentSeparator = Right Nothing

data Type
  = UserConcept !(Name, Maybe Label)
  | BuiltIn !TType
  | RepresentSeparator
  deriving (Eq, Ord)

instance Named Type where
  name t = case t of
    UserConcept (nm, _) -> nm
    BuiltIn tt -> mkName ConceptName . fmap toNamePart' $ ("AmpersandBuiltIn" NE.:| [tshow tt])
    RepresentSeparator -> mkName ConceptName . fmap toNamePart' $ "AmpersandBuiltIn" NE.:| ["RepresentSeparator"]
    where
      toNamePart' :: Text -> NamePart
      toNamePart' x = case toNamePart x of
        Nothing -> fatal $ "Not a proper namepart: " <> x
        Just np -> np

instance Show Type where
  show a = T.unpack $ case a of
    UserConcept (nm, _) -> fullName nm
    BuiltIn tt -> "BuiltIn " <> tshow tt
    RepresentSeparator -> "RepresentSeparator"

-- for faster comparison
newtype SignOrd = SignOrd Signature

instance Ord SignOrd where
  compare (SignOrd (Sign a b)) (SignOrd (Sign c d)) = compare (a, b) (c, d)

instance Eq SignOrd where
  a == b = compare a b == EQ

-- | This function is meant to convert the PSingleton inside EMp1 to an AAtomValue,
--   after the expression has been built inside an A_Context. Only at that time
--   the TType is known, enabling the correct transformation.
--   To ensure that this function is not used too early, ContextInfo is required,
--   which only exsists after disambiguation.
safePSingleton2AAtomVal :: ContextInfo -> A_Concept -> PAtomValue -> AAtomValue
safePSingleton2AAtomVal ci c val =
  case unsafePAtomVal2AtomValue typ (Just c) val of
    Left _ ->
      fatal
        . T.intercalate "\n  "
        $ [ "This should be impossible: after checking everything an unhandled singleton value found!",
            "Concept: " <> tshow c,
            "TType: " <> tshow typ,
            "Origin: " <> tshow (origin val),
            "PAtomValue: " <> case val of
              (PSingleton _ _ v) -> "PSingleton (" <> tshow v <> ")"
              (ScriptString _ v) -> "ScriptString (" <> tshow v <> ")"
              (XlsxString _ v) -> "XlsxString (" <> tshow v <> ")"
              (ScriptInt _ v) -> "ScriptInt (" <> tshow v <> ")"
              (ScriptFloat _ v) -> "ScriptFloat (" <> tshow v <> ")"
              (XlsxDouble _ v) -> "XlsxDouble (" <> tshow v <> ")"
              (ComnBool _ v) -> "ComnBool (" <> tshow v <> ")"
              (ScriptDate _ v) -> "ScriptDate (" <> tshow v <> ")"
              (ScriptDateTime _ v) -> "ScriptDateTime (" <> tshow v <> ")"
          ]
    Right x -> x
  where
    typ = representationOf ci c

-- SJC: Note about this code:
-- error messages are written here, and later turned into error messages via mkIncompatibleAtomValueError
-- Ideally, this module would import Ampersand.Input.ADL1.CtxError
-- that way, unsafePAtomVal2AtomValue could create a 'Origin -> Guarded AAtomValue' instead.
unsafePAtomVal2AtomValue :: TType -> Maybe A_Concept -> PAtomValue -> Either Text AAtomValue
unsafePAtomVal2AtomValue typ mCpt pav =
  case unsafePAtomVal2AtomValue' of
    Left err -> Left err
    Right rawVal -> Right roundedVal
      where
        roundedVal =
          case rawVal of
            AAVDateTime t x ->
              -- Rounding is needed, to maximize the number of databases
              -- on wich this runs. (MySQL 5.5 only knows seconds)
              AAVDateTime t roundBySeconds
              where
                picosecondsInASecond = 1000000000000
                roundBySeconds :: UTCTime
                roundBySeconds = x {utctDayTime = rounded (utctDayTime x)}
                  where
                    rounded :: DiffTime -> DiffTime
                    rounded = picosecondsToDiffTime . quot picosecondsInASecond . diffTimeToPicoseconds
            _ -> rawVal
  where
    unsafePAtomVal2AtomValue' :: Either Text AAtomValue
    unsafePAtomVal2AtomValue' =
      case pav of
        PSingleton o str mval ->
          case typ of
            Alphanumeric -> Right (AAVString (abs . hash $ str) typ str)
            BigAlphanumeric -> Right (AAVString (abs . hash $ str) typ str)
            HugeAlphanumeric -> Right (AAVString (abs . hash $ str) typ str)
            Password -> Right (AAVString (abs . hash $ str) typ str)
            Object -> Right (AAVString (abs . hash $ str) typ str)
            _ -> case mval of
              Nothing -> Left (message o str)
              Just x -> unsafePAtomVal2AtomValue typ mCpt x
        ScriptString o str ->
          case typ of
            Alphanumeric -> Right (AAVString (abs . hash $ str) typ str)
            BigAlphanumeric -> Right (AAVString (abs . hash $ str) typ str)
            HugeAlphanumeric -> Right (AAVString (abs . hash $ str) typ str)
            Password -> Right (AAVString (abs . hash $ str) typ str)
            Binary -> Left "Binary cannot be populated in an ADL script"
            BigBinary -> Left "Binary cannot be populated in an ADL script"
            HugeBinary -> Left "Binary cannot be populated in an ADL script"
            Date -> Left (message o str)
            DateTime -> Left (message o str)
            Boolean -> Left (message o str)
            Integer -> Left (message o str)
            Float -> Left (message o str)
            TypeOfOne -> Left "ONE has a population of it's own, that cannot be modified"
            Object -> Right (AAVString (abs . hash $ str) typ str)
        XlsxString o str ->
          case typ of
            Alphanumeric -> Right (AAVString (abs . hash $ str) typ str)
            BigAlphanumeric -> Right (AAVString (abs . hash $ str) typ str)
            HugeAlphanumeric -> Right (AAVString (abs . hash $ str) typ str)
            Password -> Right (AAVString (abs . hash $ str) typ str)
            Binary -> Left "Binary cannot be populated in an ADL script"
            BigBinary -> Left "Binary cannot be populated in an ADL script"
            HugeBinary -> Left "Binary cannot be populated in an ADL script"
            Date -> Left (message o str)
            DateTime -> Left (message o str)
            Boolean ->
              let table =
                    [ ("TRUE", True),
                      ("FALSE", False),
                      ("YES", True),
                      ("NO", False),
                      ("WAAR", True),
                      ("ONWAAR", False),
                      ("JA", True),
                      ("NEE", False),
                      ("WEL", True),
                      ("NIET", False)
                    ]
               in case lookup (T.toUpper str) table of
                    Just b -> Right (AAVBoolean typ b)
                    Nothing -> Left $ "permitted Booleans: " <> (tshow . fmap (camelCase . fst) $ table)
              where
                camelCase :: Text -> Text
                camelCase txt = case T.uncons txt of
                  Nothing -> mempty
                  Just (h, tl) -> T.cons (toUpper h) (T.toLower tl)
            Integer -> case readMaybe . T.unpack $ str of
              Just i -> Right (AAVInteger typ i)
              Nothing -> Left (message o str)
            Float -> case readMaybe . T.unpack $ str of
              Just r -> Right (AAVFloat typ r)
              Nothing -> Left (message o str)
            TypeOfOne -> Left "ONE has a population of it's own, that cannot be modified"
            Object -> Right (AAVString (abs . hash $ str) typ str)
        ScriptInt o i ->
          case typ of
            Alphanumeric -> Left (message o i)
            BigAlphanumeric -> Left (message o i)
            HugeAlphanumeric -> Left (message o i)
            Password -> Left (message o i)
            Binary -> Left "Binary ca)not be populated in an ADL script"
            BigBinary -> Left "Binary cannot be populated in an ADL script"
            HugeBinary -> Left "Binary cannot be populated in an ADL script"
            Date -> Left (message o i)
            DateTime -> Left (message o i)
            Boolean -> Left (message o i)
            Integer -> Right (AAVInteger typ i)
            Float -> Right (AAVFloat typ (fromInteger i)) -- must convert, because `34.000` is lexed as Integer
            TypeOfOne -> Left "ONE has a population of it's own, that cannot be modified"
            Object -> Left (message o i)
        ScriptFloat o x ->
          case typ of
            Alphanumeric -> Left (message o x)
            BigAlphanumeric -> Left (message o x)
            HugeAlphanumeric -> Left (message o x)
            Password -> Left (message o x)
            Binary -> Left "Binary cannot be populated in an ADL script"
            BigBinary -> Left "Binary cannot be populated in an ADL script"
            HugeBinary -> Left "Binary cannot be populated in an ADL script"
            Date -> Left (message o x)
            DateTime -> Left (message o x)
            Boolean -> Left (message o x)
            Integer -> Left (message o x)
            Float -> Right (AAVFloat typ x)
            TypeOfOne -> Left "ONE has a population of it's own, that cannot be modified"
            Object -> Left (message o x)
        XlsxDouble o d ->
          case typ of
            Alphanumeric -> relaxXLSXInput d
            BigAlphanumeric -> relaxXLSXInput d
            HugeAlphanumeric -> relaxXLSXInput d
            Password -> relaxXLSXInput d
            Binary -> Left "Binary cannot be populated in an ADL script"
            BigBinary -> Left "Binary cannot be populated in an ADL script"
            HugeBinary -> Left "Binary cannot be populated in an ADL script"
            Date ->
              Right
                AAVDate
                  { aavtyp = typ,
                    aadateDay = addDays (floor d) dayZeroExcel
                  }
            DateTime ->
              Right
                AAVDateTime
                  { aavtyp = typ,
                    aadatetime =
                      UTCTime
                        (addDays daysSinceZero dayZeroExcel)
                        (picosecondsToDiffTime . floor $ fractionOfDay * picosecondsPerDay)
                  }
              where
                picosecondsPerDay = 24 * 60 * 60 * 1000000000000
                (daysSinceZero, fractionOfDay) = properFraction d
            Boolean -> Left (message o d)
            Integer ->
              if frac == 0
                then Right (AAVInteger typ int)
                else Left (message o d)
              where
                (int, frac) = properFraction d
            Float -> Right (AAVFloat typ d)
            TypeOfOne -> Left "ONE has a population of it's own, that cannot be modified"
            Object -> relaxXLSXInput d
        ComnBool o b ->
          if typ == Boolean
            then Right (AAVBoolean typ b)
            else Left (message o b)
        ScriptDate o x ->
          if typ == Date
            then Right (AAVDate typ x)
            else Left (message o x)
        ScriptDateTime o x ->
          if typ == DateTime
            then Right (AAVDateTime typ x)
            else Left (message o x)
      where
        relaxXLSXInput :: Double -> Either Text AAtomValue
        relaxXLSXInput v = Right . AAVString (hash v) typ . neat . tshow $ v
          where
            neat :: Text -> Text
            neat s
              | onlyZeroes dotAndAfter = beforeDot
              | otherwise = s
              where
                (beforeDot, dotAndAfter) = T.span (/= '.') s
                onlyZeroes s' = case T.uncons s' of
                  Nothing -> True
                  Just ('.', afterDot) -> T.all (== '0') afterDot
                  _ -> False
        message :: (Show x) => Origin -> x -> Text
        message orig x =
          T.intercalate "\n    "
            $ [ "Representation mismatch",
                "Found: `" <> tshow x <> "` (" <> tshow orig <> "),",
                "as representation of an atom in concept `" <> text1ToText (fullName1 c) <> "`.",
                "However, the representation-type of that concept is " <> implicitly,
                "defined as " <> tshow typ <> ". The found value does not match that type."
              ]
            <> example
          where
            c = fromMaybe (fatal "Representation mismatch without concept known should not happen.") mCpt
            implicitly = if typ == Object then "(implicitly) " else ""
            example :: [Text]
            example = case typ of
              Alphanumeric -> ["ALPHANUMERIC types are texts (max 255 chars) surrounded with double quotes (\"-characters)."]
              BigAlphanumeric -> ["BIGALPHANUMERIC types are texts (max 64k chars) surrounded with double quotes (\"-characters)."]
              Boolean -> ["BOOLEAN types can have the value TRUE or FALSE (without surrounding quotes)."]
              Date -> ["DATE types are defined by ISO8601, e.g. 2013-07-04 (without surrounding quotes)."]
              DateTime -> ["DATETIME types follow ISO 8601 format, e.g. 2013-07-04T11:11:11+00:00 or 2015-06-03T13:21:58Z (without surrounding quotes)."]
              Float -> ["FLOAT type are floating point numbers. There should be a dot character (.) in it."]
              HugeAlphanumeric -> ["HUGEALPHANUMERIC types are texts (max 16M chars) surrounded with double quotes (\"-characters)."]
              Integer -> ["INTEGER types are decimal numbers (max 20 positions), e.g. 4711 or -4711 (without surrounding quotes)"]
              Password -> ["PASSWORD types are texts (max 255 chars) surrounded with double quotes (\"-characters)."]
              Object -> ["OBJECT types are non-scalar atoms represented by an identifier (max 255 chars) surrounded with double quotes (\"-characters)."]
              _ -> fatal $ "There is no example denotational syntax for a value of type `" <> tshow typ <> "`."
        dayZeroExcel = addDays (-2) (fromGregorian 1900 1 1) -- Excel documentation tells that counting starts a jan 1st, however, that isn't totally true.

-- | The typology of a context is the partioning of the concepts in that context into
--   sets such that (isa\/isa~)*;typology |- typology
--   Note, that with isa we only refer to the relations defined by CLASSIFY statements,
--   not named relations with the same properties ( {UNI,INJ,TOT} or {UNI,INJ,SUR} )
data Typology = Typology
  { tyroot :: !A_Concept, -- the most generic concept in the typology
    tyCpts :: ![A_Concept] -- all concepts, from generic to specific
  }
  deriving (Show)

-- | Since we can have concepts with several aliasses, we need to have a
--   way to resolve these aliasses. In the A-structure, we do not want to
--   bother: if `foo` is an alias of `bar`, there should only be one A_Concept
--   that represents both `foo` and `bar`. We should be able to use a map
--   whenever we need to know the A_Concept for a P_Concept.
type ConceptMap = P_Concept -> A_Concept

makeConceptMap :: [PClassify] -> ConceptMap
makeConceptMap gs = mapFunction
  where
    mapFunction :: P_Concept -> A_Concept
    mapFunction pCpt = case L.nub . concat . filter inCycle $ getCycles edges of
      xs -> mkConcept pCpt xs
      where
        inCycle xs = pCpt `elem` xs
    mkConcept :: P_Concept -> [P_Concept] -> A_Concept
    mkConcept pCpt aliasses =
      case pCpt of
        P_ONE -> ONE
        PCpt {} ->
          PlainConcept
            { aliases = fmap toTuple . NE.nub . NE.sort $ (pCpt NE.:| aliasses)
            }
      where
        toTuple :: P_Concept -> (Name, Maybe Label)
        toTuple cpt = (name cpt, p_cptlabel cpt)
    edges :: [(P_Concept, [P_Concept])]
    edges = L.nub . map mkEdge . eqCl specific $ gs
    mkEdge :: NonEmpty PClassify -> (P_Concept, [P_Concept])
    mkEdge x = (from, to's)
      where
        from = specific . NE.head $ x
        to's = L.nub . concatMap (toList . generics) $ x
