{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Ampersand.Core.AbstractSyntaxTree
  ( A_Context (..),
    Typology (..),
    MetaData (..),
    Origin (..),
    Pattern (..),
    -- Error handling types (moved here to break circular dependency)
    Guarded (..),
    CtxError (..),
    Warning (..),
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
    Object (..),
    Cruds (..),
    Default (..),
    Purpose (..),
    ExplObj (..),
    Expression (..),
    getExpressionRelation,
    A_Concept (..),
    sessionConcept,
    A_Concepts,
    AConceptDef (..),
    A_Representation (..),
    ShowWithAliases (..),
    Meaning (..),
    A_RoleRule (..),
    RoleRules,
    P_Representation (..),
    Flippable (..),
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
    AliasGraph, makeAliasGraph,
    topCpt, botCpt, -- oneCpt,
    geq, join, meet, meetIsect, MeetOrJoin (..),
    conceptLabel,
    smallerConcepts, largerConcepts,
    joinSig, meetSig, geqSig, isConcreteSignature,
    makeTypologies,
    singletonTypology, oneTypology,
    sortGeneric2Specific,
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
    -- makeConceptMap,
    ConceptMap,
    makeGraph, makePGraph, synonym
  )
where
import Algebra.Graph.AdjacencyMap
import Ampersand.Basics hiding (join)
import Ampersand.Core.ParseTree
  ( DefinitionContainer (..),
    EnforceOperator,
    HTMLtemplateCall (..),
    MetaData (..),
    Origin (..),
    PAtomValue (..),
    PBinOp,
    PClassify (generics, specific),
    P_Concept (..),
    P_Representation (..),
    PairView (..),
    PairViewSegment (..),
    Pragma,
    Role (..),
    SrcOrTgt (..),
    TType (..),
    Traced (..),
    ViewHtmlTemplate (..),
    Flippable (..),
    maybeOrdering,
  )
import Data.Data (Constr, DataType, mkConstr, mkDataType, constrIndex, Fixity(Prefix))
import Data.Default (Default (..))
import qualified Data.Tree
import qualified Data.Text1 as T1
import Data.Typeable (typeOf)
import RIO.Char (toLower, toUpper)
import RIO.Time
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Alga
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import Text.Parsec (ParseError, errorPos, sourceName, sourceLine, sourceColumn)
import Ampersand.Input.ADL1.LexerMessage (LexerError(..))
import Ampersand.Input.ADL1.FilePos (FilePos(..))

-- =====================================================================
-- Guarded type and error handling (defined here to break circular dependency)
-- =====================================================================

data CtxError
  = CTXE Origin Text
  | PE ParseError
  | LE LexerError
  | RoundTripError Text (Either (NonEmpty CtxError) Text)

instance Show CtxError where
  -- If for whatever reason this function is changed, please verify
  -- the proper working of the Ampersand language extension
  show err =
    T.unpack
      . T.intercalate "\n  "
      $ [tshow (origin err) <> " error:"]
      <> ( case err of
             CTXE _ s -> T.lines s
             PE e ->
               -- The first line of a parse error allways contains the filename and position of the error.
               -- However, these are in a wrong format.
               -- So we strip the first line of the error:
               case T.lines (tshow e) of
                 [] -> fatal "Whoh! the impossible just happened! (triggered by a parse error somewhere in your script)"
                 _ : xs -> xs
             LE (LexerError _ info) -> T.lines (tshow info)
             RoundTripError script err' ->
               ["Roundtrip test failed. Script that was tried:"]
                 ++ map ("  " <>) (T.lines script)
                 ++ ["Yields the following error:"]
                 ++ map ("  " <>) (T.lines $ tshow err')
         )

instance Traced CtxError where
  origin (CTXE o _) = o
  origin (PE perr) =
    let sourcePos = errorPos perr
     in FileLoc (FilePos (sourceName sourcePos) (sourceLine sourcePos) (sourceColumn sourcePos)) ""
  origin (LE (LexerError fp info)) = FileLoc fp (tshow info)
  origin (RoundTripError _ _) = Origin "File generated by QuickCheck"

data Warning = Warning Origin Text

instance Show Warning where
  show (Warning o msg) =
    T.unpack
      . T.intercalate "\n  "
      $ [tshow o <> " warning: "]
      <> T.lines msg

instance Traced Warning where
  origin (Warning o _) = o

data Guarded a
  = Errors (NE.NonEmpty CtxError)
  | Checked a [Warning]

instance (Eq a) => Eq (Guarded a) where
  Checked a1 _ == Checked a2 _ = a1 == a2
  _ == _ = False

instance Functor Guarded where
  fmap _ (Errors a) = Errors a
  fmap f (Checked a ws) = Checked (f a) ws

instance Applicative Guarded where
  pure x = Checked x []
  (<*>) (Checked f ws) (Checked a ws') = Checked (f a) (ws <> ws')
  (<*>) (Errors a) (Checked _ _) = Errors a
  (<*>) (Checked _ _) (Errors b) = Errors b
  (<*>) (Errors a) (Errors b) = Errors (a <> b)

instance Monad Guarded where
  (>>=) (Checked a ws) f = addWarnings ws (f a)
    where
      addWarnings :: [Warning] -> Guarded b -> Guarded b
      addWarnings ws' (Checked b ws'') = Checked b (ws' <> ws'')
      addWarnings _ (Errors b) = Errors b
  (>>=) (Errors x) _ = Errors x

-- =====================================================================
-- Main data structures
-- =====================================================================

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
    -- | The concept definitions defined outside the patterns of this context.
    ctxcdsOutPats :: ![AConceptDef],
    -- | All concepts that are declared in this context, either implicitly or explicitly, and either inside or outside the scope of patterns
    ctxcs :: ![A_Concept],
    -- | The relations that are declared in this context, outside the scope of patterns
    ctxds :: !Relations,
    -- | The user defined populations of relations defined in this context, including those from patterns
    ctxpopus :: ![Population],
    -- | All user defined rules in this context, but outside patterns
    ctxrs :: !Rules,
    -- | The concept definitions defined in this context, including those from patterns
    ctxcds :: ![AConceptDef],
    -- | The identity definitions defined in this context, outside the scope of patterns
    ctxks :: ![IdentityRule],
    ctxrrules :: !RoleRules,
    -- | The view definitions defined in this context, outside the scope of patterns
    ctxvs :: ![ViewDef],
    -- | The specialization statements defined in this context, outside the scope of patterns
    ctxgs :: ![AClassify],
    -- | The representation types of all concepts in this context
    ctxReprType :: A_Concept -> TType,
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
  deriving (Typeable, Show)

-- instance Show A_Context where
--   show = T.unpack . fullName
instance Show (A_Concept -> TType) where
  show _ = "A function that maps concepts to types"

instance Eq A_Context where
  a == b = compare a b == EQ

instance Ord A_Context where
  compare = compare `on` name

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
    ptrrs :: !RoleRules,
    -- | The concept definitions that are declared in this pattern
    ptcds :: ![AConceptDef],
    -- | The concept definitions that are declared in this pattern
    ptrps :: ![P_Representation],
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
  deriving (Typeable, Show) -- Show for debugging purposes

instance Eq Pattern where
  a == b = compare a b == EQ

instance Unique Pattern where
  showUnique = fullName1

instance Ord Pattern where
  compare = compare `on` name

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
  deriving (Eq, Show)

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

instance Labeled AConceptDef where
  mLabel = acdlabel

instance Traced AConceptDef where
  origin AConceptDef {pos = orig} = orig

instance Eq AConceptDef where
  a == b = compare a b == EQ

instance Ord AConceptDef where
  -- We compare on the name, origin and the string representation of the definition because:
  -- 1. The name is the most important part of a concept definition.
  -- 2. The origin is important to distinguish between concept definitions with the same name.
  -- 3. The definitionContainer is important to distinguish between concept definitions with the same name and origin.
  --    This is especially important for places where Origin isn't properly fit for
  --    traceability, like the Turtle and Atlas importers, Meatgrinder stuf. There whe have no exact and unique Origins.
  compare a b =
    compare
      ( name a,
        origin a,
        acdfrom a
      )
      ( name b,
        origin b,
        acdfrom b
      )

data A_Representation = Arepr
  { -- | the concepts
    aReprFrom :: !(NE.NonEmpty A_Concept),
    -- | the type of the concept the atom is in
    aReprTo :: !TType
  }
  deriving (Eq, Show)

type RoleRules = Set.Set A_RoleRule

data A_RoleRule = A_RoleRule
  { arPos :: !Origin,
    arRole :: !Role,
    arRule :: !Name -- the name of the rule
  }
  deriving (Show, Ord, Eq)

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
  = -- Map, Bij, and Prop are merely syntactic sugar in the Ampersand language. So, they show up in the P-structure but not in the A-structure.

    -- | univalent
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
  -- Relations are ordered first by name, then by signature using SignOrd for structural comparison.
  -- This ensures relations with incomparable signatures (different typologies) can coexist in Set.Set.
  -- See "Note on Signature Ordering" near geqSig for detailed explanation.
  compare a b = compare (name a, SignOrd (sign a)) (name b, SignOrd (sign b))

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
    identityAts :: NE.NonEmpty Expression
  }
  deriving (Show)

instance Named IdentityRule where
  name = idName

instance Traced IdentityRule where
  origin = idPos

instance Unique IdentityRule where
  showUnique = fullName1

instance Ord IdentityRule where
  compare = compare `on` name

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
-- In the new thinking, we have:
--   CLASSIFY Workingstudent ISA Employee/\Student   translates to Isa (C "Workingstudent") (C "Employee") and Isa (C "Workingstudent") (C "Student"), which means that a Workingstudent can be in two different tables: that of students and that of employees.
--   CLASSIFY Employee\/Student ISA Person  translates to Isa (C "Employee") (C "Person") and Isa (C "Student") (C "Person"), which means that Employees and Persons can (but don't have to) be in one table: that of persons.
--   CLASSIFY Employee\/Student ISA Person/\Consumer  translates to Isa (C "Employee") (C "Person") and Isa (C "Student") (C "Person") and Isa (C "Employee") (C "Consumer") and Isa (C "Student") (C "Consumer"), which means that a Workingstudent can be in two different tables: that of students and that of employees.
--   CLASSIFY Employee/\Student ISA Person\/Consumer  translates to Isa (C "Employee") (C "Person") and Isa (C "Student") (C "Person") and Isa (C "Employee") (C "Consumer") and Isa (C "Student") (C "Consumer"), which means that a Workingstudent can be in two different tables: that of students and that of employees.
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
    -- | The position in the file (filename, line- and column number)
    ifcPos :: !Origin,
    -- | The purpose of the interface
    ifcPurpose :: !Text
  }
  deriving (Show)

instance Eq Interface where
  a == b = compare a b == EQ

instance Ord Interface where
  compare = compare `on` name

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
  | -- | view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
    BxText
      { boxPlainName :: !(Maybe Text1),
        boxpos :: !Origin,
        boxtxt :: !Text
      }
  deriving (Show)

isObjExp :: BoxItem -> Bool
isObjExp BxExpr {} = True
isObjExp BxText {} = False

instance Unique BoxItem where
  showUnique = showUniqueAsHash

instance Traced BoxItem where
  origin o =
    case o of
      BxExpr {} -> origin . objE $ o
      BxText {} -> boxpos o

instance Ord BoxItem where
  compare a b =
    case (a, b) of
      (BxExpr {}, BxExpr {}) -> compare (objE a) (objE b)
      (BxExpr {}, BxText {}) -> GT
      (BxText {}, BxExpr {}) -> LT
      (BxText {}, BxText {}) -> compare (boxPlainName a, boxtxt a) (boxPlainName b, boxtxt b)

instance Eq BoxItem where
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
    -- | The name of the view to be used for this object
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
        siHeader :: !HTMLtemplateCall,
        siObjs :: ![BoxItem]
      }
  | InterfaceRef
      { pos :: !Origin,
        siConcept :: !A_Concept,
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
    (InterfaceRef {}, InterfaceRef {}) -> compare (siConcept a, siIsLink a, siIfcId a) (siConcept b, siIsLink b, siIfcId b)
    (InterfaceRef {}, Box {}) -> LT

instance Eq SubInterface where
  a == b = compare a b == EQ

instance Unique SubInterface where
  showUnique si@Box {} = (showUnique . siHeader) si T1.<>. (tshow . abs . hash . tshow . siObjs) si
  showUnique si@InterfaceRef {} = (showUnique . siIsLink) si T1.<>. (fullName . siIfcId $ si)

-- | Explanation is the intended constructor. It explains the purpose of the object it references.
--   The enrichment process of the parser must map the names (from PPurpose) to the actual objects
data Purpose = Expl
  { -- | The position in the Ampersand script of this purpose definition
    explPos :: !Origin,
    -- | The object that is explained.
    explObj :: !ExplObj,
    -- | This field contains the text of the explanation including language and markup info.
    explMarkup :: !Markup,
    -- | The references of the explaination
    explRefIds :: ![Text]
  }
  deriving (Show, Typeable)

instance Ord Purpose where
  compare a b = compare (explObj a, explMarkup a, explRefIds a) (explObj b, explMarkup b, explRefIds b)

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
  | AAVInteger { aavtyp :: !TType, aavint :: !Integer }
  | AAVFloat { aavtyp :: !TType, aavflt :: !Double }
  | AAVBoolean { aavtyp :: !TType, aavbool :: !Bool }
  | AAVDate { aavtyp :: !TType, aadateDay :: !Day }
  | AAVDateTime { aavtyp :: !TType, aadatetime :: !UTCTime }
  | AtomValueOfONE
  deriving (Show, Data)

-- Custom Eq instance that ignores aavtyp
instance Eq AAtomValue where
  AAVString _ _ t1 == AAVString _ _ t2 = t1 == t2
  AAVInteger _ i1 == AAVInteger _ i2 = i1 == i2
  AAVFloat _ f1 == AAVFloat _ f2 = f1 == f2
  AAVBoolean _ b1 == AAVBoolean _ b2 = b1 == b2
  AAVDate _ d1 == AAVDate _ d2 = d1 == d2
  AAVDateTime _ dt1 == AAVDateTime _ dt2 = dt1 == dt2
  AtomValueOfONE == AtomValueOfONE = True
  _ == _ = False

-- Custom Ord instance consistent with Eq (also ignoring aavtyp)
instance Ord AAtomValue where
  compare (AAVString _ _ t1) (AAVString _ _ t2) = compare t1 t2
  compare (AAVInteger _ i1) (AAVInteger _ i2) = compare i1 i2
  compare (AAVFloat _ f1) (AAVFloat _ f2) = compare f1 f2
  compare (AAVBoolean _ b1) (AAVBoolean _ b2) = compare b1 b2
  compare (AAVDate _ d1) (AAVDate _ d2) = compare d1 d2
  compare (AAVDateTime _ dt1) (AAVDateTime _ dt2) = compare dt1 dt2
  compare AtomValueOfONE AtomValueOfONE = EQ
  -- Define ordering between different constructors
  compare (AAVString _ _ _) _ = LT
  compare _ (AAVString _ _ _) = GT
  compare (AAVInteger _ _) _ = LT
  compare _ (AAVInteger _ _) = GT
  compare (AAVFloat _ _) _ = LT
  compare _ (AAVFloat _ _) = GT
  compare (AAVBoolean _ _) _ = LT
  compare _ (AAVBoolean _ _) = GT
  compare (AAVDate _ _) _ = LT
  compare _ (AAVDate _ _) = GT
  compare (AAVDateTime _ _) AtomValueOfONE = LT
  compare AtomValueOfONE (AAVDateTime _ _) = GT

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
  | -- | relation based on a simple binary operator  (e.g. x > y)
    EBin !PBinOp !Signature
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
        EDcV sgn -> (19 :: Int) `hashWithSalt` sgn
        EBin oper sgn -> (20 :: Int) `hashWithSalt` oper `hashWithSalt` sgn
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

-- Useful shorthands:
l .==. r = EEqu (l, r)
l .|-. r = EInc (l, r)
l ./\. r = EIsc (l, r)
l .\/. r = EUni (l, r)
l .-. r  = EDif (l, r)
l ./. r  = ELrs (l, r)
l .\. r  = ERrs (l, r)
l .<>. r = EDia (l, r)
l .:. r  = ECps (l, r)
l .!. r  = ERad (l, r)
l .*. r  = EPrd (l, r)

{- For the operators /, \, ;, ! and * we must not check whether the intermediate types exist.
   Suppose the user says CLASSIFY Student ISA Person and CLASSIFY Employee ISA Person, then Student `join` Employee has a name (i.e. Person), but Student `meet` Employee
   does not. In that case, -(r!s) (with target r=Student and source s=Employee) is defined, but -r;-s is not.
   So in order to let -(r!s) be equal to -r;-s we must not check for the existence of these types, for the Rotterdam paper already shows that this is fine.
-}

instance Flippable A_Concept where
  flp = id

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
    EBin oper sgn -> EBin (flp oper) sgn
    EDcV sgn -> EDcV (flp sgn)
    EMp1 {} -> expr

instance HasSignature Expression where
  -- sign :: Expression -> Signature gives fatal errors because it is used after type checking.
  -- Since the type system is closed, all signatures are correct unless there is an error in this compiler.
  sign (EEqu (l, _)) = sign l
  sign (EInc (l, _)) = sign l
  -- You might expect "sign l `meetSig` sign r" in the subexpression sign (EIsc (l, r)), but that would inhibit manipulations such as De Morgan. To keep type checking closed, EIs, EUni, and EDif all require a joinSig.
  sign (EIsc (l, r)) = fromMaybe (fatal ("Incompatible signatures in intersection: "<> tshow (sign l) <> " /\\ " <> tshow (sign r)))
                                 (sign l `joinSig` sign r)
  sign (EUni (l, r)) = fromMaybe (fatal ("Incompatible signatures in union: "<> tshow (sign l) <> " \\/ " <> tshow (sign r)))
                                 (sign l `joinSig` sign r)
  sign (EDif (l, r)) = fromMaybe (fatal ("Incompatible signatures in difference: "<> tshow (sign l) <> " - " <> tshow (sign r)))
                                 (sign l `joinSig` sign r)
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
  sign (EDcI c) = ISgn c
  sign (EBin _ sgn) = sgn
  sign (EDcV sgn) = sgn
  sign (EMp1 _ c) = ISgn c

-- Compute meet of two signatures
meetSig :: Signature -> Signature -> Maybe Signature
meetSig (ISgn c1) (ISgn c2)               = ISgn <$> meet c1 c2
meetSig (ISgn c) (Sign src tgt)           = ISgn <$> (meet c =<< meet src tgt)
meetSig (Sign src tgt) (ISgn c)           = ISgn <$> (meet c =<< meet src tgt)
meetSig (Sign src1 tgt1) (Sign src2 tgt2) = Sign <$> meet src1 src2 <*> meet tgt1 tgt2

-- Compute join of two signatures
joinSig :: Signature -> Signature -> Maybe Signature
joinSig (ISgn c1) (ISgn c2)               = ISgn <$> join c1 c2
joinSig (ISgn c) (Sign src tgt)           = Sign <$> join c src <*> join c tgt
joinSig (Sign src tgt) (ISgn c)           = Sign <$> join src c <*> join tgt c
joinSig (Sign src1 tgt1) (Sign src2 tgt2) = Sign <$> join src1 src2 <*> join tgt1 tgt2

{- Note on Signature Ordering: Ord vs SignOrd vs geqSig
   ======================================================
   
   There are THREE different ways to compare signatures in Ampersand:
   
   1. SignOrd - Structural comparison (total order)
      Purpose: Used as Map keys in DeclMap = Map.Map Name (Map.Map SignOrd Relation)
      Properties: 
        - Simple lexicographic comparison of concepts
        - Always comparable (total order)
        - No semantic meaning regarding concept hierarchy
        - Fast and predictable
      Example: SignOrd (Sign A B) `compare` SignOrd (Sign C D) compares (A,B) with (C,D)
   
   2. Ord Signature - Semantic comparison (total order with fallback)
      Purpose: Used when Relations are stored in Set.Set Relation
      Properties:
        - Respects concept hierarchy when signatures are comparable (same typology)
        - Uses geqSig to determine ordering based on concept generalization
        - Falls back to lexicographic ordering for incomparable signatures
        - This fallback is necessary because Ord must provide a total order
      Example: When comparing relations with same name but different signatures
   
   3. geqSig - Partial order based on concept hierarchy
      Purpose: Type-checking and semantic analysis
      Properties:
        - Returns Maybe Bool (Nothing for incomparable signatures)
        - Forms a partial order (not all pairs comparable)
        - Just True means first signature is more generic or equal
        - Nothing means signatures belong to different typologies
      Example: geqSig [Person*Project] [Student*Project] might be Just True if Student ISA Person
   
   Relationship between Ord and geqSig:
   - When signatures ARE comparable (same typology), Ord MUST respect geqSig's ordering
   - When signatures are NOT comparable (different typologies), Ord uses lexicographic fallback
   - This design allows Relations with incomparable signatures to coexist in Set.Set
   
   Example from testIssue1183.adl:
     RELATION foo[A123 * A123L]   -- typology 1
     RELATION foo[A133 * A13L]    -- typology 2 (A133 undeclared, separate typology)
   These relations have the same name but incomparable signatures. The Ord instance
   allows them to be stored together in a Set by falling back to lexicographic comparison.
-}

-- Compute geq of two signatures (applicative style)
geqSig :: Signature -> Signature -> Maybe Bool
geqSig (ISgn c1) (ISgn c2)               = geq c1 c2
geqSig (ISgn c) (Sign src tgt)           = (&&) <$> geq c src <*> geq c tgt
geqSig (Sign src tgt) (ISgn c)           = (&&) <$> geq src c <*> geq tgt c
geqSig (Sign src1 tgt1) (Sign src2 tgt2) = (&&) <$> geq src1 src2 <*> geq tgt1 tgt2
-- Compute geq of two signatures (monadic style), just for the fun of it:
-- geqSig (ISgn c1) (ISgn c2)               = geq c1 c2
-- geqSig (ISgn c) (Sign src tgt)           = do
--   g1 <- geq c src
--   g2 <- geq c tgt
--   Just (g1 && g2)
-- geqSig (Sign src tgt) (ISgn c)           = do
--   g1 <- geq src c
--   g2 <- geq tgt c
--   Just (g1 && g2)
-- geqSig (Sign src1 tgt1) (Sign src2 tgt2) = do
--   g1 <- geq src1 src2
--   g2 <- geq tgt1 tgt2
--   Just (g1 && g2)

isConcreteSignature :: Signature -> Bool
isConcreteSignature (Sign src tgt) = src/=topCpt && src/=botCpt && tgt/=topCpt && tgt/=botCpt
isConcreteSignature (ISgn cpt)     = cpt/=topCpt && cpt/=botCpt

showSign :: (HasSignature a) => a -> Text1
showSign x = case sign x of
               Sign s t -> Text1 '[' $ fullName s <> "*" <> fullName t <> "]"
               ISgn c   -> Text1 '[' $ fullName c <> "]"

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
    getRelation (EFlp e) = do
      (s, d, t, isFlipped) <- getRelation e
      Just (t, d, s, not isFlipped)
    getRelation (EDcD d) = Just (source d, Just d, target d, False)
    getRelation _ = Nothing

data A_Concept
  = PlainConcept
      { -- | List of names that the concept is refered to, in random order
        aliases :: !(Set Name),
        -- | The typology (concept graph) this concept belongs to
        typology :: !Typology
      }
  | -- | The disjunction type of concepts:
    DISJT !(Set.Set A_Concept)
  | -- | The union type of concepts:
    UNION !(Set.Set A_Concept)
  | -- | The intersection type of concepts:
    ISECT !(Set.Set A_Concept)
  | -- | The universal Singleton: 'I'['Anything'] = 'V'['Anything'*'Anything']
    ONE
  deriving (Typeable)

-- | Equality for A_Concept ignores the typology field
instance Eq A_Concept where
  PlainConcept {aliases = a1} == PlainConcept {aliases = a2} = (not.null) (a1 `Set.intersection` a2)
  DISJT c1 == DISJT c2 = c1 == c2
  UNION c1 == UNION c2 = c1 == c2
  ISECT c1 == ISECT c2 = c1 == c2
  ONE == ONE = True
  _ == _ = False

instance Ord A_Concept where
  compare (PlainConcept als1 _) (PlainConcept als2 _) = compare als1 als2
  compare (PlainConcept _ _) _ = LT
  compare _ (PlainConcept _ _) = GT
  compare (DISJT cpts1) (DISJT cpts2) = compare cpts1 cpts2
  compare (DISJT _) _ = LT
  compare _ (DISJT _) = GT
  compare (UNION cpts1) (UNION cpts2) = compare cpts1 cpts2
  compare (UNION _) _ = LT
  compare _ (UNION _) = GT
  compare (ISECT cpts1) (ISECT cpts2) = compare cpts1 cpts2
  compare (ISECT _) ONE = LT
  compare ONE (ISECT _) = GT
  compare ONE ONE = EQ


-- | Data instance for A_Concept that skips the typology field
instance Data A_Concept where
  gfoldl k z (als `PlainConcept` _typo) = z (`PlainConcept` emptyTypology) `k` als
  gfoldl k z (DISJT cpts) = z DISJT `k` cpts
  gfoldl k z (UNION cpts) = z UNION `k` cpts
  gfoldl k z (ISECT cpts) = z ISECT `k` cpts
  gfoldl _ z ONE = z ONE

  gunfold k z c = case constrIndex c of
    1 -> k (z (`PlainConcept` emptyTypology))
    2 -> k (z DISJT)
    3 -> k (z UNION)
    4 -> k (z ISECT)
    5 -> z ONE
    _ -> fatal "gunfold: A_Concept: invalid constructor index"

  toConstr (PlainConcept {}) = plainConceptConstr
  toConstr (DISJT {}) = disjunctConstr
  toConstr (UNION {}) = unionConstr
  toConstr (ISECT {}) = intersectConstr
  toConstr ONE = oneConstr

  dataTypeOf _ = aConceptDataType

plainConceptConstr, disjunctConstr, unionConstr, intersectConstr, oneConstr :: Constr
plainConceptConstr = mkConstr aConceptDataType "PlainConcept" [] Prefix
disjunctConstr = mkConstr aConceptDataType "DISJT" [] Prefix
unionConstr = mkConstr aConceptDataType "UNION" [] Prefix
intersectConstr = mkConstr aConceptDataType "ISECT" [] Prefix
oneConstr = mkConstr aConceptDataType "ONE" [] Prefix

aConceptDataType :: DataType
aConceptDataType = mkDataType "Ampersand.Core.AbstractSyntaxTree.A_Concept"
  [plainConceptConstr, disjunctConstr, unionConstr, intersectConstr, oneConstr]

-- | The reason that SESSION is a plain concept (so not added as a data type variant SESSION, next to ONE)
--   is that we want it to be treated as any other plain concept, for instance when generating code.
sessionConcept :: A_Concept
sessionConcept = PlainConcept {aliases = Set.singleton nameOfSESSION, typology = singletonTypology nameOfSESSION}

type A_Concepts = Set.Set A_Concept

makeGraph :: [AClassify] -> Set.Set A_Concept -> AdjacencyMap A_Concept
makeGraph conceptPairs concepts
 = overlays ([vertex spc `connect` vertex gen | (spc,gen) <- pairs]<>
             [vertex cpt | cpt <- Set.toList concepts])
  where
    pairs = [ (genspc isa, gengen isa) | isa@(Isa{})<-conceptPairs]<>
            [ (genspc ise, c) | ise@(IsE{})<-conceptPairs, c<-toList (genrhs ise)]

makePGraph :: [PClassify] -> Set.Set P_Concept -> AdjacencyMap P_Concept
makePGraph conceptPairs concepts
 = overlays ([vertex spc `connect` vertex gen | (spc,gen) <- pairs]<>
             [vertex cpt | cpt <- Set.toList concepts])
  where
    pairs = [ (specific classify, c) | classify <- conceptPairs, c <- toList (generics classify)]

synonym :: Ord a => AdjacencyMap a -> a -> a -> Bool
synonym g a b = a==b || (hasEdge a b g && hasEdge b a g)

-- | Test function that validates the meet property of concept graphs.
-- If edges (a,j) and (b,j) both exist in the graph, then a `meet` b should exist.
-- When the meet does NOT exist, this function creates a concept with name "a/\\b" 
-- and returns the missing edges (m,a) and (m,b) that should connect this meet to a and b.
-- Returns an AdjacencyMap containing the missing edges that violate this property.

instance Unique AConceptDef where
  showUnique = toText1Unsafe . fullName

instance Unique A_Concept where
  showUnique = toText1Unsafe . tshow

instance Hashable A_Concept where
  hashWithSalt s cpt =
    s
      `hashWithSalt` ( case cpt of
                         PlainConcept {} -> (0 :: Int) `hashWithSalt` (fmap (text1ToText . fullName1) . Set.toList . aliases) cpt
                         DISJT cpts -> (1 :: Int) `hashWithSalt` (show . L.sort . Set.toList $ cpts)
                         UNION cpts -> (2 :: Int) `hashWithSalt` (show . L.sort . Set.toList $ cpts)
                         ISECT cpts -> (3 :: Int) `hashWithSalt` (show . L.sort . Set.toList $ cpts)
                         ONE -> (4 :: Int) `hashWithSalt` ("ONE" :: String)
                     )

instance Named A_Concept where
  name PlainConcept {aliases = names} = case Set.toList names of nm : _ -> nm; _ -> fatal "This A_Concept has no name"
  name (DISJT cpts) = toConceptName "><"  cpts
  name (UNION cpts) = toConceptName "\\/" cpts
  name (ISECT cpts) = toConceptName "/\\" cpts
  name ONE = nameOfONE

toConceptName :: Text -> Set.Set A_Concept -> Name
toConceptName opString cpts = case (try2Name ConceptName . T.intercalate opString . map tshow . Set.toList) cpts of
  Left err -> fatal $ "Not a proper concept name: " <> tshow cpts <> ". " <> err
  Right (nm, _) -> nm

instance Show A_Concept where
  show = T.unpack . showWithAliases

-- | special type of Show, for types that can have aliases. Its purpose is
--   to use when giving feedback to the ampersand modeler, in cases aliases
--   are used.
class (Show a) => ShowWithAliases a where
  showWithAliases :: a -> Text

instance ShowWithAliases A_Concept where
  showWithAliases cpt@PlainConcept {aliases=names} =
    case Set.toList names of
      []  -> fatal "2This A_Concept has no name"
      [nm] -> fullName nm  -- Use name if no label
      xs -> fullName cpt <> "(alias: " <> T.intercalate ", " (fmap fullName xs) <> ")"
  showWithAliases _ = fullName ONE

instance Unique (A_Concept, PAtomValue) where
  showUnique (c, val) = toText1Unsafe $ "ConceptAtomValue_" <> (tshow . abs . hash $ readable)
    where
      readable = tshow val <> "[" <> text1ToText (showUnique c) <> "]"

data Signature = Sign !A_Concept  !A_Concept
               | ISgn !A_Concept deriving (Typeable, Generic, Data)

-- | Custom Eq instance that treats ISgn c and Sign c c as equivalent
instance Eq Signature where
  (Sign s1 t1) == (Sign s2 t2) = s1 == s2 && t1 == t2
  (ISgn c1) == (ISgn c2) = c1 == c2
  (ISgn c) == (Sign s t) = c == s && c == t  -- ISgn c is equivalent to Sign c c
  (Sign s t) == (ISgn c) = s == c && t == c  -- Symmetric

-- | Simple Ord instance for Signature using lexicographic comparison.
-- This is purely structural - it does NOT consider concept hierarchies.
-- For semantic ordering based on concept hierarchy, use geqSig.
-- See "Note on Signature Ordering" below for detailed explanation.
instance Ord Signature where
  compare (Sign a b) (Sign c d) = compare (a, b) (c, d)
  compare (ISgn _)   (Sign _ _) = GT
  compare (Sign _ _) (ISgn _)   = LT
  compare (ISgn a)   (ISgn b)   = compare a b

instance Hashable Signature

instance Show Signature where
  show (Sign s t) = "[" <> show s <> "*" <> show t <> "]"
  show (ISgn c) = "[" <> show c <> "]"

instance ShowWithAliases Signature where
  showWithAliases (Sign s t) = "[" <> showWithAliases s <> "*" <> showWithAliases t <> "]"
  showWithAliases (ISgn c)   = "[" <> showWithAliases c <>  "]"

instance Unique Signature where
  showUnique (Sign s t) = toText1Unsafe $ "Signature" <> (tshow . abs . hash $ "[" <> text1ToText (showUnique s) <> "*" <> text1ToText (showUnique t) <> "]")
  showUnique (ISgn c)   = toText1Unsafe $ "Signature" <> (tshow . abs . hash $ "[" <> text1ToText (showUnique c) <> "]")

instance HasSignature Signature where
  source (Sign s _) = s
  source (ISgn c)   = c
  target (Sign _ t) = t
  target (ISgn c)   = c
  sign sgn = sgn

instance Flippable Signature where
  flp (Sign s t) = Sign t s
  flp sgn        = sgn

class HasSignature a where
  source, target :: a -> A_Concept
  source = source . sign
  target = target . sign
  sign :: a -> Signature
  -- | Compute the signature using the concept graph for proper join/meet calculations.
  --   This is needed for inter-type operations (EEqu, EInc, EIsc, EUni, EDif) where
  --   the signature depends on the concept hierarchy.
  signWithGraph :: a -> Signature
  signWithGraph a = sign a  -- default implementation: ignore graph, use simple sign
  isEndo :: a -> Bool
  isEndo s = source s == target s

--  {-# MINIMAL sign #-}

-- Convenient data structure to hold information about concepts and their representations
--  in a context.
data ContextInfo = CI
  { -- | Provisional representation function (only from REPRESENT statements, before type-checking interfaces)
    reprType :: !(A_Concept -> TType),
    -- | a list of typologies, based only on the CLASSIFY statements. Single-concept typologies are not included
    multiKernels :: ![Typology],
    -- | a list of all Representations in the context, including the implicit Object definitions.
    reprList :: ![P_Representation],
    -- | a map of declarations and the corresponding types. We need this to look up declarations during type checking.
    declarationsMap :: !(Map.Map Name (Map.Map SignOrd Relation)),
    -- | types not used in any declaration
    allPConcepts :: !(Set.Set P_Concept),
    -- | a map that must be used to convert P_Concept to A_Concept, i.e. pCpt2aCpt
    conceptMap :: !ConceptMap,
    -- | the default language used to interpret markup texts in this context
    defaultLang :: !Lang,
    -- | the default format used to interpret markup texts in this context
    defaultFormat :: !PandocFormat
  }
  deriving (Show)

-- for faster comparison
newtype SignOrd = SignOrd Signature
  deriving (Show)

instance Ord SignOrd where
  compare (SignOrd (Sign a b)) (SignOrd (Sign c d)) = compare (a, b) (c, d)
  compare (SignOrd (ISgn _))   (SignOrd (Sign _ _)) = GT
  compare (SignOrd (Sign _ _)) (SignOrd (ISgn _))   = LT
  compare (SignOrd (ISgn a))   (SignOrd (ISgn b))   = compare a b

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
        $ [ "Manufacturing error in the compiler: after checking everything I still found an unhandled singleton value!",
            "Concept: " <> tshow c,
            "TType: " <> tshow typ <> (if origin val==OriginUnknown then "" else "\n  Origin: " <> tshow (origin val)),
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
    typ = reprType ci c

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
            AAVDateTime _ (UTCTime _ 0) -> rawVal -- prevent devision by zero
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
                    Nothing -> Left $ "permitted Booleans: " <> (tshow . fmap (camel . fst) $ table)
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

type AliasGraph = AdjacencyMap (Set.Set Name)

-- | We need an alias graph to compute geq, join, and meet correctly because concepts can have several names (aliases).
-- This function transforms the concept graph to a directed acyclic graph (DAG) producing AliasGraph
makeAliasGraph :: AdjacencyMap P_Concept -> AliasGraph
makeAliasGraph graph = overlay (edges dagEdges) (vertices sccReps)
  where
    -- The graph of strongly connected components cannot contain cycles because each cycle is condensed into a single vertex:
    sccGraph :: AdjacencyMap (NonEmpty.AdjacencyMap P_Concept)
    sccGraph = Alga.scc graph
    -- Extract singleton SCCs and create representative concepts:
    sccs :: [NonEmpty.AdjacencyMap P_Concept]
    sccs = vertexList sccGraph
    -- Each SCC becomes a set of names (aliases)
    sccReps :: [Set.Set Name]
    sccReps = map (Set.map name . Set.fromList . NE.toList . NonEmpty.vertexList1) sccs
    -- Map each SCC to its representative alias set
    sccToRep :: Map.Map (NonEmpty.AdjacencyMap P_Concept) (Set.Set Name)
    sccToRep = Map.fromList (zip sccs sccReps)
    -- Edges between alias sets based on original SCC graph edges
    dagEdges :: [(Set.Set Name, Set.Set Name)]
    dagEdges = [ (Map.findWithDefault (fatal "SCC not found") fromScc sccToRep,
                  Map.findWithDefault (fatal "SCC not found") toScc sccToRep)
               | (fromScc, toScc) <- edgeList sccGraph ]

-- | The typology of a context is the partioning of the concepts in that context into
--   sets such that (isa\/isa~)*;typology |- typology
--   Note, that with isa we only refer to the relations defined by CLASSIFY statements,
--   not named relations with the same properties ( {UNI,INJ,TOT} or {UNI,INJ,SUR} )
data Typology = Typology
  { tyroot :: !P_Concept              -- This is the P_Concept because an A_Concept has a typology and we don't want to have a circular definition.
  , tyCpts :: ![Set.Set Name]              -- all concepts from the alias graph, from generic to specific, used in the database table for this typology.
  , tyGrph :: AliasGraph  -- the subgraph of the alias graph that represents this typology, used for join/meet calculations.
  }
  deriving (Show)
-- Invariants. For any Typology t:
--   - t is a subgraph of the concept graph: tyGrph t `isSubgraphOf` aliasGraph
--   - tyGrph and tyCpts contain the same set of concepts: Set.fromList (tyCpts t) == concs (tyGrph t)
--   - tyroot is the most generic concept in tyGrph: For every c `Set.member` tyCpts t: (tyroot t, c) `Set.member` (edges . transitiveClosure . reflexiveClosure . tyGrph) t

-- | Empty typology used as a placeholder when typology is not relevant
emptyTypology :: Typology
emptyTypology = Typology
  { tyroot = P_ONE
  , tyCpts = []
  , tyGrph = vertex (Set.singleton nameOfONE)
  }

oneTypology :: Typology
oneTypology = Typology
  { tyroot = P_ONE
  , tyCpts = [Set.singleton nameOfONE]
  , tyGrph = vertex (Set.singleton nameOfONE)
  }

singletonTypology :: Name -> Typology
singletonTypology nm = Typology
  { tyroot = P_ONE
  , tyCpts = [Set.singleton nm]
  , tyGrph = vertex (Set.singleton nm)
  }

instance Eq Typology where
  (==) :: Typology -> Typology -> Bool
  t == t'  =  tyroot t == tyroot t'


-- Helper: for sorting alias sets from generic to specific
sortGeneric2Specific :: (Ord a) =>AdjacencyMap a -> [a] -> [a]
sortGeneric2Specific graph xs =
  L.sortBy (compare `on` specificityLevel) xs
  where
    tc = transitiveClosure graph
    specificityLevel x = Set.size (postSet x tc)
-- alternative, with a different algorithm:
        -- sortSpecific2Generic :: [A_Concept] -> [A_Concept]
        -- sortSpecific2Generic = go []
        --   where
        --     go xs [] = xs
        --     go xs (y : ys) = case [y' | y' <- L.nub ys, y' `elem` Set.fromList (smallerConcepts y)] of
        --       [] -> go (xs ++ [y]) ys
        --       _ : _ -> go xs (ys ++ [y])



-- | Create typologies from alias graph
--   Pre: the alias graph is acyclic
makeTypologies :: AliasGraph -> Guarded [Typology]
makeTypologies aliasGraph = traverse createTypology wcComponents
  where
    createTypology :: AliasGraph -> Guarded Typology
    createTypology subGraph =
      let aliasSets = vertexList subGraph
          -- Find roots: nodes with no outgoing edges (most generic)
          roots = [as | as <- aliasSets, null (postSet as subGraph)]
      in case roots of
           [r] -> pure Typology
                    { tyroot = if r == Set.singleton nameOfONE then P_ONE else case Set.toList r of nm:_ -> PCpt nm; _ -> fatal "makeTypologies: This should not happen: alias set without names"
                    , tyCpts = sortGeneric2Specific subGraph aliasSets
                    , tyGrph = subGraph
                    }
           []  -> fatal ("No root found in typology " <> tshow aliasSets) -- cannot occur if subGraph is acyclic
           _   -> Errors (CTXE (Origin "Internal")
                    ("Multiple roots found in typology: " <> T.intercalate ", " (map tshow roots)) NE.:| [])

    -- | To compute typologies, we need the weakly connected components (WCCs) of the alias graph, which is a directed acyclic graph (DAG).
    -- A weakly connected component is a maximal set of vertices such that for every pair
    -- of vertices u and v, there is an undirected path between u and v in the underlying 
    -- undirected graph (ignoring edge directions). This path is found in the symmetric closure of the graph.
    --
    -- This function converts the directed graph to an undirected graph by overlaying it
    -- with its transpose, then finds connected components using depth-first search.
    -- Each returned AdjacencyMap P_Concept is a subgraph of the input containing exactly
    -- the vertices and edges for that WCC.
    wcComponents :: [AliasGraph]
    wcComponents = map (createSubgraph aliasGraph) (Alga.dfsForest symmetricGraph)
      where
        symmetricGraph = overlay aliasGraph (transpose aliasGraph)
        createSubgraph :: AliasGraph -> Data.Tree.Tree (Set.Set Name) -> AliasGraph
        createSubgraph originalGraph conceptTree =
          let verticesInComponent = Set.fromList (flattenTree conceptTree)
              componentEdges = filter (\(from, tgt) -> from `Set.member` verticesInComponent &&
                                                         tgt `Set.member` verticesInComponent)
                                      (edgeList originalGraph)
          in overlay (edges componentEdges) (vertices (Set.toList verticesInComponent))
        flattenTree :: Data.Tree.Tree a -> [a]
        flattenTree (Data.Tree.Node x children) = x : concatMap flattenTree children

-- | The TOP concept - universal upper bound for all concepts. It is more generic than every other concept.
topCpt :: A_Concept
topCpt = PlainConcept
  { aliases = Set.fromList
      [case try2Name ConceptName "_TOP" of
          Left err -> fatal $ "Not a proper concept name: _TOP. " <> err
          Right (nm, _) -> nm
      ]
  , typology = emptyTypology
  }

-- | The BOT concept - universal lower bound for all concepts. It is more specific than every other concept.
botCpt :: A_Concept
botCpt = PlainConcept
  { aliases = Set.fromList
      [case try2Name ConceptName "_BOT" of
          Left err -> fatal $ "Not a proper concept name: _BOT. " <> err
          Right (nm, _) -> nm
      ]
  , typology = emptyTypology
  }

-- | The ONE concept - universal lower bound for all concepts. It is more specific than every other concept.
-- oneCpt :: A_Concept
-- oneCpt = PlainConcept
--   { aliases = Set.fromList
--       [case try2Name ConceptName "ONE" of
--           Left err -> fatal $ "Not a proper concept name: ONE. " <> err
--           Right (nm, _) -> nm
--       ]
--   , typology = emptyTypology
--   }

{- Design decision:
topCpt and botCpt are universal bottom and top elements.
-}
-- | smallerConcepts delivers a list of all concepts that are more specific than the given concept.
--   If there are no cycles in the generalization graph,  cpt  cannot be an element of  smallerConcepts gens cpt.
smallerConcepts :: A_Concept -> [A_Concept]
smallerConcepts ONE = []
smallerConcepts cpt@PlainConcept{} =
  let rtc = (transitiveClosure . tyGrph . typology) cpt
      -- preSet gives all predecessors (more specific concepts)
      moreSpecific = Set.toList $ preSet (aliases cpt) rtc
      -- Convert back to A_Concepts, excluding self
  in [PlainConcept als (typology cpt) | als <- moreSpecific, als /= aliases cpt]
smallerConcepts _ = []  -- DISJT, UNION, ISECT

-- | this function delivers a list of all concepts that are more generic than the given concept.
largerConcepts :: A_Concept -> [A_Concept]
largerConcepts ONE = []
largerConcepts cpt@PlainConcept{} =
  let rtc = (transitiveClosure . tyGrph . typology) cpt
      -- postSet gives all successors (more generic concepts)
      moreGeneric = Set.toList $ postSet (aliases cpt) rtc
      -- Convert back to A_Concepts, excluding self
  in [PlainConcept als (typology cpt) | als <- moreGeneric, als /= aliases cpt]
largerConcepts _ = []  -- DISJT, UNION, ISECT

-- | geq for A_Concepts using embedded typology (no external graph needed!)
geq :: A_Concept -> A_Concept -> Maybe Bool
geq a@PlainConcept{} b@PlainConcept{}
  | a == b                   = Just True
  | a == topCpt              = Just True
  | b == topCpt              = Just False
  | b == botCpt              = Just True
  | a == botCpt              = Just False
  | typology a /= typology b = Nothing  -- Different typologies, no geq
  | otherwise = (Just . hasEdge (aliases b) (aliases a) . transitiveClosure . tyGrph . typology) a
geq a b | a==b = Just True
geq _ _ = Nothing  -- DISJT, UNION, ISECT    TODO: handle these cases?

data MeetOrJoin = Meet | Join deriving (Show) -- for preventing code duplication in the type checker

-- | join for A_Concepts using embedded typology (no external graph needed!)
join :: A_Concept -> A_Concept -> Maybe A_Concept
join a@PlainConcept{} b@PlainConcept{}
  | a == b                   = Just a
  | a == topCpt              = Just topCpt
  | b == topCpt              = Just topCpt
  | a == botCpt              = Just b
  | b == botCpt              = Just a
  | typology a /= typology b = Nothing  -- Different typologies, no join
  | otherwise = case joinX (tyGrph (typology a)) a b of
                  Just result | Set.null result || nameOfONE `Set.member` result -> 
                    fatal $ tshow a <> " `join` " <> tshow b <> " produced alias set: " <> tshow result
                  Just result -> Just (PlainConcept result (typology a))
                  Nothing -> Nothing
join a b | a==b = Just a
join _ _ = Nothing  -- DISJT, UNION, ISECT    TODO: handle these cases?

-- | meet for A_Concepts using embedded typology
meet :: A_Concept -> A_Concept -> Maybe A_Concept
meet a@PlainConcept{} b@PlainConcept{}
  | a == b                   = Just a
  | a == topCpt              = Just b
  | b == topCpt              = Just a
  | a == botCpt              = Just botCpt
  | b == botCpt              = Just botCpt
  | typology a /= typology b = Nothing
  | otherwise = case meetX (tyGrph (typology a)) a b of
                  Just result | Set.null result || nameOfONE `Set.member` result -> 
                    fatal $ tshow a <> " `meet` " <> tshow b <> " produced alias set: " <> tshow result
                  Just result -> Just (PlainConcept result (typology a))
                  Nothing -> Nothing
meet a b | a==b = Just a
meet _ _ = Nothing  -- DISJT, UNION, ISECT    TODO: handle these cases?

meetIsect :: A_Concept -> A_Concept -> Maybe A_Concept
meetIsect a@PlainConcept{} b@PlainConcept{}
  | a == b = Just a
  | a == topCpt = Just b
  | b == topCpt = Just a
  | a == botCpt = Just botCpt
  | b == botCpt = Just botCpt
  | typology a /= typology b = Nothing
  | otherwise = case meetX (tyGrph (typology a)) a b of
                  Just result | Set.null result || nameOfONE `Set.member` result -> 
                    fatal $ tshow a <> " `meet` " <> tshow b <> " produced alias set: " <> tshow result
                  Just result -> Just (PlainConcept result (typology a))
                  Nothing -> Just (ISECT (Set.fromList [a, b]))  -- Fallback to disjunction
meetIsect a b | a==b = Just a
meetIsect _ _ = Nothing  -- DISJT, UNION, ISECT    TODO: handle these cases?

findNode :: (Named a) => AliasGraph -> a -> Maybe (Set.Set Name)
findNode aliasGraph x = L.find (Set.member (name x)) (vertexList aliasGraph)

-- Compute the least upper bound (join) of a list of pairs
joinX :: (Named a) => AliasGraph -> a -> a -> Maybe (Set.Set Name)
joinX aliasGraph a b =
  case (a', b') of
    (Just aSet, Just bSet)
      | aSet == bSet -> a'  -- Same alias set, mathematical identity: join(x, x) = x
      | hasEdge aSet bSet rtc -> b'  -- b is more generic
      | hasEdge bSet aSet rtc -> a'  -- a is more generic
      | otherwise ->
          case Set.toList (postSet aSet rtc `Set.intersection` postSet bSet rtc) L.\\ [aSet, bSet] of
            [] -> Nothing
            x:xs -> Just (L.foldr minimum x xs) -- find the minimum of the upper bounds
    _ -> Nothing  -- One or both not found in graph
    where
      rtc = reflexiveClosure (transitiveClosure aliasGraph)
      a' = findNode aliasGraph (name a)
      b' = findNode aliasGraph (name b)
      minimum x y = if hasEdge x y rtc then x else y

-- | This meet function includes some Ampersand-specific features, that deviate from the mathematical definition of meet.
--   The purpose is to enable disjunctions.
meetX :: (Named a) => AliasGraph -> a -> a -> Maybe (Set.Set Name)
meetX aliasGraph a b =
  case (a', b') of
    (Just aSet, Just bSet)
      | aSet == bSet -> a'  -- Same alias set, mathematical identity: meet(x, x) = x
      | hasEdge aSet bSet rtc -> a'  -- a is more specific
      | hasEdge bSet aSet rtc -> b'  -- b is more specific
      | otherwise ->
          case Set.toList (preSet aSet rtc `Set.intersection` preSet bSet rtc) L.\\ [aSet, bSet] of
            [] -> Nothing
            x:xs -> Just (L.foldr maximum x xs) -- find the maximum of the lower bounds
    _ -> Nothing  -- One or both not found in graph
    where
      rtc = reflexiveClosure (transitiveClosure aliasGraph)
      a' = findNode aliasGraph (name a)
      b' = findNode aliasGraph (name b)
      maximum x y = if hasEdge x y rtc then y else x

{- Should we distinguish between ISgn and Sign here?
  geq aliasGraph a b =
   case (geq aliasGraph (source a) (source b), geq aliasGraph (target a) (target b)) of
     (Just True, Just True) -> case (a,b) of
                                 (ISgn{}, Sign{}) -> Just False
                                 _                -> Just True
     (Just _,    Just _)    -> Just False
     _                      -> Nothing
-}

{- Here is some test output for the join and meet functions, applied on the following graph:
edges [("even","int"),("float","num"),("int","num"),("integer","even"),("integer","oneven"),("num","gegeven"),("oneven","int")]

"even" `join` "even" = Just "even"   and   "even" `meet` "even" = Just "even"
The join intersection set for "even" and "float" is: ["gegeven","num"]
"even" `join` "float" = Just "num"   and   "even" `meet` "float" = Nothing
"even" `join` "gegeven" = Just "gegeven"   and   "even" `meet` "gegeven" = Just "even"
"even" `join` "int" = Just "int"   and   "even" `meet` "int" = Just "even"
"even" `join` "integer" = Just "even"   and   "even" `meet` "integer" = Just "integer"
"even" `join` "num" = Just "num"   and   "even" `meet` "num" = Just "even"
The join intersection set for "even" and "oneven" is: ["gegeven","int","num"]
"even" `join` "oneven" = Just "int"   and   "even" `meet` "oneven" = Just "integer"
The join intersection set for "float" and "even" is: ["gegeven","num"]
"float" `join` "even" = Just "num"   and   "float" `meet` "even" = Nothing
The join intersection set for "float" and "int" is: ["gegeven","num"]
"float" `join` "int" = Just "num"   and   "float" `meet` "int" = Nothing
The join intersection set for "float" and "integer" is: ["gegeven","num"]
"float" `join` "integer" = Just "num"   and   "float" `meet` "integer" = Nothing
"float" `join` "num" = Just "num"   and   "float" `meet` "num" = Just "float"
The join intersection set for "float" and "oneven" is: ["gegeven","num"]
"float" `join` "oneven" = Just "num"   and   "float" `meet` "oneven" = Nothing
"gegeven" `join` "even" = Just "gegeven"   and   "gegeven" `meet` "even" = Just "even"
"int" `join` "even" = Just "int"   and   "int" `meet` "even" = Just "even"
The join intersection set for "int" and "float" is: ["gegeven","num"]
"int" `join` "float" = Just "num"   and   "int" `meet` "float" = Nothing
"integer" `join` "even" = Just "even"   and   "integer" `meet` "even" = Just "integer"
The join intersection set for "integer" and "float" is: ["gegeven","num"]
"integer" `join` "float" = Just "num"   and   "integer" `meet` "float" = Nothing
"num" `join` "oneven" = Just "num"   and   "num" `meet` "oneven" = Just "oneven"
The join intersection set for "oneven" and "even" is: ["gegeven","int","num"]
"oneven" `join` "even" = Just "int"   and   "oneven" `meet` "even" = Just "integer"
The join intersection set for "oneven" and "float" is: ["gegeven","num"]
"oneven" `join` "float" = Just "num"   and   "oneven" `meet` "float" = Nothing
"oneven" `join` "gegeven" = Just "gegeven"   and   "oneven" `meet` "gegeven" = Just "oneven"
"oneven" `join` "oneven" = Just "oneven"   and   "oneven" `meet` "oneven" = Just "oneven"
-}

-- | Get the label for a concept from its AConceptDef in a context.
--   Returns the label if defined in any AConceptDef with the same name.
conceptLabel :: A_Context -> A_Concept -> Maybe Label
conceptLabel ctx cpt =
  case [acdlabel cd | cd <- ctxcds ctx, acdcpt cd == cpt] of
    [] -> Nothing
    lbl:_ -> lbl

-- | Since we can have concepts with several aliases, we need to have a
--   way to resolve these aliases. In the A-structure, we do not want to
--   bother: if `foo` is an alias of `bar`, there should only be one A_Concept
--   that represents both `foo` and `bar`. We should be able to use a map
--   whenever we need to know the A_Concept for a P_Concept.
--   Formally: A_Concepts have a classification relation, which is reflexive, transitive, and antisymmetric.
--   So, it is a directed acyclic graph.
--   P_Concepts are mapped to A_Concepts, and all cycles in the classification of P_Concepts are condensed into one
--   A_Concept. This is done by the function `makeConceptMap`.
type ConceptMap = Origin -> P_Concept -> Guarded A_Concept

instance Show ConceptMap where
  show _ = "The (guarded) function that maps P_Concepts to A_Concepts"

{-
makeConceptMap :: [PConceptDef] -> [PClassify] -> Map.Map Name (AdjacencyMap A_Concept) -> ConceptMap
makeConceptMap cds gs nameToGraph = mapFunction
  where
    mapFunction :: P_Concept -> A_Concept
    mapFunction pCpt = case L.nub . concat . filter inCycle . getCycles $ edgesList of
      xs -> mkConcept pCpt xs
      where
        inCycle xs = pCpt `elem` xs
    mkConcept :: P_Concept -> [P_Concept] -> A_Concept
    mkConcept pCpt aliass =
      case pCpt of
        P_ONE  -> ONE
        PCpt{} -> 
          let als = Set.fromList . fmap toTuple . (pCpt:) $ aliass
              typoGraph = Map.findWithDefault empty (name pCpt) nameToGraph
          in PlainConcept { aliases = als, typology = typoGraph }
      where
        toTuple :: P_Concept -> (Name, Maybe Label)
        toTuple cpt =
          ( name cpt,
            case mapMaybe mLabel . filter (\cd -> name cd == name cpt) $ cds of
              [] -> Nothing
              [h] -> Just h
              _ -> Just . Label . T.intercalate ", " . L.nub . mapMaybe labelToText . filter (\cd -> name cd == name cpt) $ cds
          )
          where
            labelToText :: PConceptDef -> Maybe Text
            labelToText cd = case mLabel cd of
              Nothing -> Nothing
              Just (Label txt) -> Just txt
    edgesList :: [(P_Concept, [P_Concept])]
    edgesList = L.nub . map mkEdge . eqCl specific $ gs
    mkEdge :: NonEmpty PClassify -> (P_Concept, [P_Concept])
    mkEdge x = (from, to's)
      where
        from = specific . NE.head $ x
        to's = L.nub . concatMap (toList . generics) $ x
-}
