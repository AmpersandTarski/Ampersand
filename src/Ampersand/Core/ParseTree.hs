{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Ampersand.Core.ParseTree
  ( P_Context (..),
    mergeContexts,
    MetaData (..),
    P_RoleRule (..),
    Role (..),
    P_Enforce (..),
    EnforceOperator (..),
    P_Pattern (..),
    P_Relation (..),
    Pragma (..),
    mergeRels,
    Term (..),
    TermPrim (..),
    P_NamedRel (..),
    PBinOp (..),
    binaryFunction,
    PairView (..),
    PairViewSegment (..),
    PairViewTerm (..),
    PairViewSegmentTerm (..),
    HTMLtemplateCall (..),
    TemplateKeyValue (..),
    SrcOrTgt (..),
    DefinitionContainer (..),
    P_Rule (..),
    PConceptDef (..),
    PCDDef (..),
    P_Representation (..),
    TType (..),
    P_Population (..),
    PAtomPair (..),
    PAtomValue (..),
    mkPair,
    makePSingleton,
    ObjectKind (..),
    P_BoxBodyElement,
    P_SubInterface,
    P_Interface (..),
    P_BoxItem (..),
    P_SubIfc (..),
    P_Cruds (..),
    P_IdentDef,
    P_IdentDf (..),
    P_IdentSegment,
    P_IdentSegmnt (..),
    P_ViewDef,
    P_ViewSegment (..),
    ViewHtmlTemplate (..),
    P_ViewD (..),
    P_ViewSegmtPayLoad (..),
    PPurpose (..),
    PRef2Obj (..),
    PMeaning (..),
    PMessage (..),
    P_Concept (..),
    P_Sign (..),
    mkPConcept,
    PClassify (..),
    P_Markup (..),
    PProp (..),
    PProps,
    PRelationDefault (..),
    -- Inherited stuff:
    module Ampersand.Input.ADL1.FilePos,
  )
where

import Ampersand.Basics hiding (concatMap, foldr, orElse, sequence)
import Ampersand.Input.ADL1.FilePos
import Data.Foldable (concatMap)
import qualified Data.Text1 as T1
import Data.Traversable
import Data.Typeable (typeOf)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import RIO.Time

data P_Context = PCtx
  { -- | The name of this context
    ctx_nm :: !Name,
    -- | The label of this context, if it exits
    ctx_lbl :: !(Maybe Label),
    -- | The origins of the context. A context can be a merge of a file including other files c.q. a list of Origin.
    ctx_pos :: ![Origin],
    -- | The language specified on the top-level context. If omitted, English will be the default.
    ctx_lang :: !(Maybe Lang),
    -- | The default markup format for free text in this context
    ctx_markup :: !(Maybe PandocFormat),
    -- | The patterns defined in this context
    ctx_pats :: ![P_Pattern],
    -- | All user defined rules in this context, but outside patterns and outside processes
    ctx_rs :: ![P_Rule TermPrim],
    -- | The relations defined in this context, outside the scope of patterns
    ctx_ds :: ![P_Relation],
    -- | The concept definitions defined in this context, outside the scope of patterns
    ctx_cs :: ![PConceptDef],
    -- | The identity definitions defined in this context, outside the scope of patterns
    ctx_ks :: ![P_IdentDef],
    -- | The MAINTAIN definitions defined in this context, outside the scope of patterns
    ctx_rrules :: ![P_RoleRule],
    ctx_reprs :: ![P_Representation],
    -- | The view definitions defined in this context, outside the scope of patterns
    ctx_vs :: ![P_ViewDef],
    -- | The gen definitions defined in this context, outside the scope of patterns
    ctx_gs :: ![PClassify],
    -- | The interfaces defined in this context
    ctx_ifcs :: ![P_Interface],
    -- | The purposes defined in this context, outside the scope of patterns and processes
    ctx_ps :: ![PPurpose],
    -- | The populations defined in this context (without patterns), from POPULATION statements as well as from Relation declarations
    ctx_pops :: ![P_Population],
    -- | generic meta information (name/value pairs) that can be used for experimenting without having to modify the adl syntax
    ctx_metas :: ![MetaData],
    -- | The Enforce statements defined in this context, outside the scope of patterns
    ctx_enfs :: ![P_Enforce TermPrim]
  }
  deriving (Show) -- for QuickCheck

instance Eq P_Context where
  c1 == c2 = name c1 == name c2

instance Named P_Context where
  name = ctx_nm

instance Labeled P_Context where
  mLabel = ctx_lbl

-- for declaring name/value pairs with information that is built in to the adl syntax yet
data MetaData = MetaData
  { pos :: !Origin,
    mtName :: !Text1,
    mtVal :: !Text
  }
  deriving (Show)

instance Traced MetaData where
  origin (MetaData p _ _) = p

data EnforceOperator
  = IsSuperSet !Origin
  | IsSubSet !Origin
  | IsSameSet !Origin
  deriving (Show, Eq)

data P_Enforce a = P_Enforce
  { pos :: !Origin,
    penfRel :: !a,
    penfOp :: !EnforceOperator,
    penfExpr :: !(Term a)
  }
  deriving (Show)

instance Functor P_Enforce where fmap = fmapDefault

instance Foldable P_Enforce where foldMap = foldMapDefault

instance Traversable P_Enforce where
  traverse f (P_Enforce orig rel op expr) =
    (\r e -> P_Enforce orig r op e)
      <$> f rel
      <*> traverse f expr

-- | A RoleRule r means that a role called 'mRoles r' must maintain the process rule called 'mRules r'
data P_RoleRule = Maintain
  { -- | position in the Ampersand script
    pos :: !Origin,
    -- | names of a role
    mRoles :: !(NE.NonEmpty Role),
    -- | names of a Rule
    mRules :: !(NE.NonEmpty Name)
  }
  deriving (Show) -- deriving (Show) is just for debugging

instance Traced P_RoleRule where
  origin Maintain {pos = orig} = orig

data Role = Role
  { pos :: !Origin,
    rlName :: !Name,
    rlLbl :: !(Maybe Label),
    rlIsService :: !Bool
  }
  deriving (Show, Typeable, Data) -- deriving (Show) is just for debugging

instance Ord Role where
  compare a b = compare (name a) (name b)

instance Eq Role where
  a == b = compare a b == EQ

instance Named Role where
  name = rlName

instance Labeled Role where
  mLabel = rlLbl

instance Unique Role where
  showUnique = fullName1

data P_Pattern = P_Pat
  { -- | the starting position in the file in which this pattern was declared.
    pos :: !Origin,
    -- | Name of this pattern
    pt_nm :: !Name,
    -- | The user defined rules in this pattern
    pt_lbl :: !(Maybe Label),
    pt_rls :: ![P_Rule TermPrim],
    -- | The generalizations defined in this pattern
    pt_gns :: ![PClassify],
    -- | The relations that are declared in this pattern
    pt_dcs :: ![P_Relation],
    -- | The assignment of roles to rules.
    pt_RRuls :: ![P_RoleRule],
    -- | The concept definitions defined in this pattern
    pt_cds :: ![PConceptDef],
    -- | The type into which concepts is represented
    pt_Reprs :: ![P_Representation],
    -- | The identity definitions defined in this pattern
    pt_ids :: ![P_IdentDef],
    -- | The view definitions defined in this pattern
    pt_vds :: ![P_ViewDef],
    -- | The purposes of elements defined in this pattern
    pt_xps :: ![PPurpose],
    -- | The populations that are local to this pattern
    pt_pop :: ![P_Population],
    -- | the end position in the file in which this pattern was declared.
    pt_end :: !Origin,
    -- | The Enforce statements defined in this pattern
    pt_enfs :: ![P_Enforce TermPrim]
  }
  deriving (Show) -- for QuickCheck

instance Ord P_Pattern where
  compare a b = case compare (name a) (name b) of
    EQ ->
      fromMaybe
        ( fatal
            . T.intercalate "\n"
            $ [ "P_Pattern should have a non-fuzzy Origin.",
                tshow (origin a),
                tshow (origin b)
              ]
        )
        (maybeOrdering (origin a) (origin b))
    x -> x

instance Eq P_Pattern where
  a == b = compare a b == EQ

instance Named P_Pattern where
  name = pt_nm

instance Labeled P_Pattern where
  mLabel = pt_lbl

instance Traced P_Pattern where
  origin P_Pat {pos = p} = p

data PConceptDef = PConceptDef
  { -- | The position of this definition in the text of the Ampersand source (filename, line number and column number).
    pos :: !Origin,
    -- | The name of the concept for which this is the definition. If there is no such concept, the conceptdefinition is ignored.
    cdname :: !Name,
    cdlbl :: !(Maybe Label),
    -- | The textual definition of this concept.
    cddef2 :: !PCDDef,
    -- | A label meant to identify the source of the definition. (useful as LaTeX' symbolic reference)
    cdmean :: ![PMeaning],
    -- | The name of the pattern or context in which this concept definition was made
    cdfrom :: !DefinitionContainer
  }
  deriving (Show, Typeable)

data DefinitionContainer
  = CONTEXT !Name
  | PATTERN !Name
  | Module !Name

instance Show DefinitionContainer where
  show x = case x of
    CONTEXT nm -> show nm
    PATTERN nm -> show nm
    Module nm -> show nm

instance Ord DefinitionContainer where
  compare a b = case (a, b) of
    (CONTEXT x, CONTEXT y) -> compare x y
    (PATTERN x, PATTERN y) -> compare x y
    (Module x, Module y) -> compare x y
    (CONTEXT _, _) -> LT
    (_, CONTEXT _) -> GT
    (PATTERN _, _) -> LT
    (_, PATTERN _) -> GT

instance Eq DefinitionContainer where
  a == b = compare a b == EQ

instance Ord PConceptDef where
  compare a b =
    -- We compare on the name, origin and the string representation of the definition because:
    -- 1. The name is the most important part of a concept definition.
    -- 2. The origin is important to distinguish between concept definitions with the same name.
    -- 3. The definitionContainer is important to distinguish between concept definitions with the same name and origin.
    --    This is especially important for places where Origin isn't properly fit for
    --    traceability, like the Turtle and Atlas importers, Meatgrinder stuf. There whe have no exact and unique Origins.
    compare
      ( name a,
        origin a,
        cdfrom a
      )
      ( name b,
        origin b,
        cdfrom b
      )

instance Eq PConceptDef where
  a == b = compare a b == EQ

instance Unique PConceptDef where
  showUnique cd = fullName1 cd <> toText1Unsafe ("At" <> tshow (typeOf x) <> "_" <> tshow x)
    where
      x = origin cd

instance Traced PConceptDef where
  origin PConceptDef {pos = p} = p

instance Named PConceptDef where
  name = cdname

instance Labeled PConceptDef where
  mLabel = cdlbl

-- | Data structure to implement the change to the new way to specify
--   the definition part of a concept. By using this structure, we can
--   implement the change in a fully backwards compatible way.
data PCDDef
  = PCDDefLegacy
      { -- | The textual definition of this concept.
        pcddef :: !Text,
        -- | A label meant to identify the source of the definition. (useful as LaTeX' symbolic reference)
        pcdref :: !Text
      }
  | PCDDefNew
      { pcdmean :: !PMeaning
      }
  deriving (Show, Typeable)

data P_Representation
  = Repr
      { pos :: !Origin,
        -- | the concepts
        reprcpts :: !(NE.NonEmpty P_Concept),
        -- | the type of the concept the atom is in
        reprdom :: !TType
      }
  | ImplicitRepr
      { -- | the type of the concept the atom is in
        reprTerm :: !(Term TermPrim)
      }
  deriving (Show)

instance Traced P_Representation where
  origin Repr {pos = orig} = orig
  origin r@ImplicitRepr {} = origin (reprTerm r)

data TType
  = Alphanumeric
  | BigAlphanumeric
  | HugeAlphanumeric
  | Password
  | Binary
  | BigBinary
  | HugeBinary
  | Date
  | DateTime
  | Boolean
  | Integer
  | Float
  | Object
  | TypeOfOne -- special type for the special concept ONE.
  deriving (Eq, Ord, Data, Typeable, Enum, Bounded)

instance Unique TType where
  showUnique = toText1Unsafe . tshow

instance Show TType where
  show tt = case tt of
    Alphanumeric -> "ALPHANUMERIC"
    BigAlphanumeric -> "BIGALPHANUMERIC"
    HugeAlphanumeric -> "HUGEALPHANUMERIC"
    Password -> "PASSWORD"
    Binary -> "BINARY"
    BigBinary -> "BIGBINARY"
    HugeBinary -> "HUGEBINARY"
    Date -> "DATE"
    DateTime -> "DATETIME"
    Boolean -> "BOOLEAN"
    Integer -> "INTEGER"
    Float -> "FLOAT"
    Object -> "OBJECT"
    TypeOfOne -> "TYPEOFONE"

instance Hashable TType where
  hashWithSalt s tt =
    s `hashWithSalt` show tt

data P_Relation = P_Relation
  { -- | the name of the relation
    dec_nm :: !Name,
    -- | the type. Parser must guarantee it is not empty.
    dec_sign :: !P_Sign,
    -- | an optional label, which can be a friendly, human readable alternative for the name
    dec_label :: !(Maybe Label),
    -- | the user defined properties (Uni, Tot, Sur, Inj, Sym, Asy, Trn, Rfx, Irf, Prop)
    dec_prps :: !PProps,
    -- | Three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
    -- ^    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
    dec_defaults :: ![PRelationDefault],
    -- | a list of default values for tuples in the relation
    dec_pragma :: !(Maybe Pragma),
    -- | the optional meaning of a relation, possibly more than one for different languages.
    dec_Mean :: ![PMeaning],
    -- | the position in the Ampersand source file where this relation is declared. Not all relations come from the ampersand souce file.
    dec_pos :: !Origin
  }
  deriving (Show)

instance Eq P_Relation where
  a == b = compare a b == EQ

instance Ord P_Relation where
  compare a b = compare (dec_pos a, dec_nm a, dec_sign a) (dec_pos b, dec_nm b, dec_sign b)

-- | Pragma, used in relations. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
--  then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
data Pragma = Pragma
  { pos :: !Origin,
    praLeft :: !Text,
    praMid :: !Text,
    praRight :: !Text
  }
  deriving (Show, Data, Eq)

instance Traced Pragma where
  origin Pragma {pos = orig} = orig

-- | Equality on P_Relation
--   Normally, equality on relations means equality of both name (dec_nm) and signature (dec_sign).
--   However, in the parser, we need to distinguish between two relations with the same name and signature when they are in different locations.
--   That occurs for example if we need to locate a copy of a relation for generating an error message.
--   For this reason, equality in the P-structure is defined on origin.
--   It is easy to see that if the locations are the same, then the relations must be the same.
--   But is that true all the time? ... No. If one or both origins are unknown, we revert to comparing name and signature.
--   This is still not true for MEATGRINDER stuff!
--   So,
-- DO NOT USE ORD and EQ on P_Relation!
instance Named P_Relation where
  name = dec_nm

instance Traced P_Relation where
  origin P_Relation {dec_pos = orig} = orig

-- | The union of relations requires the conservation of properties of relations, so it is called 'merge' rather than 'union'.
--   Relations with the same signature are merged. Relations with different signatures are left alone.
mergeRels :: [P_Relation] -> [P_Relation]
mergeRels rs = map fun (eqCl signat rs) -- each equiv. class contains at least 1 element, so foldr1 is just right!
  where
    fun :: NonEmpty P_Relation -> P_Relation
    fun rels =
      P_Relation
        { dec_nm = name r0,
          dec_sign = dec_sign r0,
          dec_label = dec_label r0,
          dec_prps = Set.unions (dec_prps <$> NE.toList rels),
          dec_defaults = concatMap dec_defaults rels,
          dec_pragma = case mapMaybe dec_pragma (NE.toList rels) of
            [] -> Nothing
            h : _ -> Just h,
          dec_Mean = L.nub $ concatMap dec_Mean rels,
          dec_pos =
            case NE.filter (not . isFuzzyOrigin . origin) rels of
              [] -> origin r0
              h : _ -> origin h
        }
      where
        (r0 :| _) = rels
    signat rel = (name rel, pSrc (dec_sign rel), pTgt (dec_sign rel))

data PAtomPair = PPair
  { pos :: Origin,
    ppLeft :: PAtomValue,
    ppRight :: PAtomValue
  }
  deriving (Show) -- Show is for QuickCheck error messages and/or input redundancy removal only!

instance Ord PAtomPair where
  compare a b = compare (ppLeft a, ppRight a) (ppLeft b, ppRight b)

instance Eq PAtomPair where
  a == b = compare a b == EQ

instance Traced PAtomPair where
  origin PPair {pos = orig} = orig

instance Flippable PAtomPair where
  flp pr =
    pr
      { ppLeft = ppRight pr,
        ppRight = ppLeft pr
      }

makePSingleton :: Text -> PAtomValue
makePSingleton s = PSingleton (Origin "ParseTree.hs") s Nothing

data PAtomValue
  = PSingleton !Origin !Text !(Maybe PAtomValue)
  | ScriptString !Origin !Text -- string from script char to enquote with when printed
  | XlsxString !Origin !Text
  | ScriptInt !Origin !Integer
  | ScriptFloat !Origin !Double
  | XlsxDouble !Origin !Double
  | ComnBool !Origin !Bool
  | ScriptDate !Origin !Day
  | ScriptDateTime !Origin !UTCTime
  deriving (Typeable, Data)

instance Show PAtomValue where -- Used for showing in Expressions as PSingleton
  show pav =
    case pav of
      PSingleton _ s _ -> show s
      ScriptString _ s -> show s
      XlsxString _ s -> show s
      ScriptInt _ i -> show i
      ScriptFloat _ d -> show d
      XlsxDouble o d -> fatal ("We got a value " <> tshow d <> " from " <> tshow o <> ", which has to be shown in an expression, however the technicaltype is not known.")
      ComnBool _ b -> show b
      ScriptDate _ x -> show x
      ScriptDateTime _ x -> show x

instance Eq PAtomValue where
  a == b = compare a b == EQ

instance Ord PAtomValue where
  compare a b =
    case (a, b) of
      (PSingleton _ x _, PSingleton _ x' _) -> compare x x'
      (PSingleton {}, _) -> GT
      (ScriptString _ x, ScriptString _ x') -> compare x x'
      (ScriptString {}, _) -> GT
      (XlsxString _ x, XlsxString _ x') -> compare x x'
      (XlsxString {}, _) -> GT
      (ScriptInt _ x, ScriptInt _ x') -> compare x x'
      (ScriptInt {}, _) -> GT
      (ScriptFloat _ x, ScriptFloat _ x') -> compare x x'
      (ScriptFloat {}, _) -> GT
      (XlsxDouble _ x, XlsxDouble _ x') -> compare x x'
      (XlsxDouble {}, _) -> GT
      (ScriptDate _ x, ScriptDate _ x') -> compare x x'
      (ScriptDate {}, _) -> GT
      (ScriptDateTime _ x, ScriptDateTime _ x') -> compare x x'
      (ScriptDateTime {}, _) -> GT
      (ComnBool _ x, ComnBool _ x') -> compare x x'
      (ComnBool {}, _) -> GT

instance Traced PAtomValue where
  origin pav =
    case pav of
      PSingleton o _ _ -> o
      ScriptString o _ -> o
      XlsxString o _ -> o
      ScriptInt o _ -> o
      ScriptFloat o _ -> o
      XlsxDouble o _ -> o
      ComnBool o _ -> o
      ScriptDate o _ -> o
      ScriptDateTime o _ -> o

instance Unique PAtomValue where
  showUnique x = toText1Unsafe ("AtomValue_" <> (tshow . abs . hash . tshow) x)

mkPair :: Origin -> PAtomValue -> PAtomValue -> PAtomPair
mkPair o l r =
  PPair
    { pos = o,
      ppLeft = l,
      ppRight = r
    }

data TermPrim
  = -- | identity element without a type
    --   At parse time, there may be zero or one element in the list of concepts.
    --   Reason: when making eqClasses, the least element of that class is used as a witness of that class
    --   to know whether an eqClass represents a concept, we only look at its witness
    --   By making Pid the first in the data decleration, it becomes the least element for "deriving Ord".
    PI !Origin
  | -- | identity element restricted to a type
    Pid !Origin !P_Concept
  | -- | a singleton atom, possibly with a type. The list contains denotational equivalent values
    --   eg, when `123` is found by the parser, the list will contain both interpretations as
    --   the Text "123" or as Integer 123.
    --   Since everything between the single quotes can allways be interpretated as a Text,
    --   it is quaranteed that the list contains the interpretation as Text, and thus cannot
    --   be empty.
    Patm !Origin !PAtomValue !(Maybe P_Concept)
  | -- | the complete relation, of which the type is yet to be derived by the type checker.
    PVee !Origin
  | -- | the complete relation, restricted to a type.
    --   At parse time, there may be zero, one or two elements in the list of concepts.
    Pfull !Origin !P_Concept !P_Concept
  | -- | a binary operator on two terms
    PBin !Origin !PBinOp
  | -- | a binary operator on two terms, restricted to a type
    PBind !Origin !PBinOp !P_Concept
  | -- | a named relation
    PNamedR !P_NamedRel
  deriving (Show) -- For QuickCheck error messages only!

data P_NamedRel = PNamedRel
  { pos :: !Origin,
    p_nrnm :: !Name,
    p_mbSign :: !(Maybe P_Sign)
  }
  deriving (Show)

instance Ord P_NamedRel where
  compare a b = compare (name a, p_mbSign a) (name b, p_mbSign b)

instance Eq P_NamedRel where
  a == b = compare a b == EQ

data Term a
  = Prim !a
  | -- | equivalence             =
    PEqu !Origin !(Term a) !(Term a)
  | -- | inclusion               |-
    PInc !Origin !(Term a) !(Term a)
  | -- | intersection            /\
    PIsc !Origin !(Term a) !(Term a)
  | -- | union                   \/
    PUni !Origin !(Term a) !(Term a)
  | -- | difference              -
    PDif !Origin !(Term a) !(Term a)
  | -- | left residual           /
    PLrs !Origin !(Term a) !(Term a)
  | -- | right residual          \
    PRrs !Origin !(Term a) !(Term a)
  | -- | diamond                 <>
    PDia !Origin !(Term a) !(Term a)
  | -- | composition             ;
    PCps !Origin !(Term a) !(Term a)
  | -- | relative addition       !
    PRad !Origin !(Term a) !(Term a)
  | -- | cartesian product       #
    PPrd !Origin !(Term a) !(Term a)
  | -- | Rfx.Trn closure         *  (Kleene star)
    PKl0 !Origin !(Term a)
  | -- | Transitive closure      +  (Kleene plus)
    PKl1 !Origin !(Term a)
  | -- | conversion (flip, wok)  ~
    PFlp !Origin !(Term a)
  | -- | Complement
    PCpl !Origin !(Term a)
  | -- | bracketed term ( ... )
    PBrk !Origin !(Term a)
  deriving (Show) -- deriving Show for debugging purposes

instance Functor Term where fmap = fmapDefault

instance Foldable Term where foldMap = foldMapDefault

instance Traversable Term where
  traverse f' x =
    case x of
      Prim a -> Prim <$> f' a
      PEqu o a b -> PEqu o <$> f a <*> f b
      PInc o a b -> PInc o <$> f a <*> f b
      PIsc o a b -> PIsc o <$> f a <*> f b
      PUni o a b -> PUni o <$> f a <*> f b
      PDif o a b -> PDif o <$> f a <*> f b
      PLrs o a b -> PLrs o <$> f a <*> f b
      PRrs o a b -> PRrs o <$> f a <*> f b
      PDia o a b -> PDia o <$> f a <*> f b
      PCps o a b -> PCps o <$> f a <*> f b
      PRad o a b -> PRad o <$> f a <*> f b
      PPrd o a b -> PPrd o <$> f a <*> f b
      PKl0 o a -> PKl0 o <$> f a
      PKl1 o a -> PKl1 o <$> f a
      PFlp o a -> PFlp o <$> f a
      PCpl o a -> PCpl o <$> f a
      PBrk o a -> PBrk o <$> f a
    where
      f = traverse f'

instance Functor P_SubIfc where fmap = fmapDefault

instance Foldable P_SubIfc where foldMap = foldMapDefault

instance Traversable P_SubIfc where
  traverse _ (P_InterfaceRef o a b) = pure (P_InterfaceRef o a b)
  traverse f (P_Box o c lst) = P_Box o c <$> traverse (traverse f) lst

instance Traced (P_SubIfc a) where
  origin P_Box {pos = orig} = orig
  origin P_InterfaceRef {pos = orig} = orig

instance Functor P_BoxItem where fmap = fmapDefault

instance Foldable P_BoxItem where foldMap = foldMapDefault

instance Traversable P_BoxItem where
  traverse f (P_BoxItemTerm nm lbl orig ctx mCrud mView msub) =
    (\ctx' msub' -> P_BoxItemTerm nm lbl orig ctx' mCrud mView msub')
      <$> traverse f ctx
      <*> traverse (traverse f) msub
  traverse _ (P_BxTxt nm pos' str) = pure (P_BxTxt nm pos' str)

instance Traced TermPrim where
  origin e = case e of
    PI orig -> orig
    Pid orig _ -> orig
    Patm orig _ _ -> orig
    PVee orig -> orig
    Pfull orig _ _ -> orig
    PNamedR r -> origin r
    PBin orig _ -> orig
    PBind orig _ _ -> orig

instance Traced P_NamedRel where
  origin (PNamedRel o _ _) = o

instance Named P_NamedRel where
  name (PNamedRel _ nm _) = nm

instance (Traced a) => Traced (Term a) where
  origin e = case e of
    Prim a -> origin a
    PEqu orig _ _ -> orig
    PInc orig _ _ -> orig
    PIsc orig _ _ -> orig
    PUni orig _ _ -> orig
    PDif orig _ _ -> orig
    PLrs orig _ _ -> orig
    PRrs orig _ _ -> orig
    PDia orig _ _ -> orig
    PCps orig _ _ -> orig
    PRad orig _ _ -> orig
    PPrd orig _ _ -> orig
    PKl0 orig _ -> orig
    PKl1 orig _ -> orig
    PFlp orig _ -> orig
    PCpl orig _ -> orig
    PBrk orig _ -> orig

data SrcOrTgt = Src | Tgt deriving (Show, Eq, Ord, Generic, Enum, Bounded, Data)

instance Hashable SrcOrTgt

instance Flippable SrcOrTgt where
  flp Src = Tgt
  flp Tgt = Src

newtype PairView a = PairView {ppv_segs :: NE.NonEmpty (PairViewSegment a)} deriving (Show, Typeable, Eq, Ord, Generic)

instance (Hashable a) => Hashable (PairView a)

instance (Traced a) => Traced (PairView a) where
  origin = origin . NE.head . ppv_segs

data PairViewSegment a
  = PairViewText
      { pos :: Origin,
        pvsStr :: Text
      }
  | PairViewExp
      { pos :: Origin,
        pvsSoT :: SrcOrTgt,
        pvsExp :: a
      }
  deriving (Show, Typeable, Generic)

instance Eq (PairViewSegment a) where
  a == b = compare a b == EQ

instance Ord (PairViewSegment a) where
  compare a b =
    fromMaybe
      ( fatal
          . T.intercalate "\n"
          $ [ "P_Rule a should have a non-fuzzy Origin.",
              tshow (origin a),
              tshow (origin b)
            ]
      )
      (maybeOrdering (origin a) (origin b))

instance (Hashable a) => Hashable (PairViewSegment a)

instance Traced (PairViewSegment a) where
  origin PairViewText {pos = orig} = orig
  origin PairViewExp {pos = orig} = orig

-- | the newtype to make it possible for a PairView to be disambiguatable: it must be of the form "d a" instead of "d (Term a)"
newtype PairViewTerm a = PairViewTerm (PairView (Term a))

newtype PairViewSegmentTerm a = PairViewSegmentTerm (PairViewSegment (Term a))

instance Traversable PairViewSegmentTerm where
  traverse f (PairViewSegmentTerm x) = PairViewSegmentTerm <$> traverse (traverse f) x

instance Functor PairViewSegmentTerm where fmap = fmapDefault

instance Foldable PairViewSegmentTerm where foldMap = foldMapDefault

instance Traversable PairViewTerm where
  traverse f (PairViewTerm x) = PairViewTerm <$> traverse (traverse f) x

instance Functor PairViewTerm where fmap = fmapDefault

instance Foldable PairViewTerm where foldMap = foldMapDefault

instance Traversable PairViewSegment where
  traverse _ (PairViewText ori s) = pure (PairViewText ori s)
  traverse f (PairViewExp ori st x) = PairViewExp ori st <$> f x

instance Functor PairViewSegment where fmap = fmapDefault

instance Foldable PairViewSegment where foldMap = foldMapDefault

instance Traversable PairView where
  traverse f (PairView s) = PairView <$> traverse (traverse f) s

instance Functor PairView where fmap = fmapDefault

instance Foldable PairView where foldMap = foldMapDefault

data P_Rule a = P_Rule
  { -- | Position in the Ampersand file
    pos :: !Origin,
    -- | Name of this rule
    rr_nm :: !Name,
    -- | Label, if present
    rr_lbl :: !(Maybe Label),
    -- | The rule term
    rr_exp :: !(Term a),
    -- | User-specified meanings, possibly more than one, for multiple languages.
    rr_mean :: ![PMeaning],
    -- | User-specified violation messages, possibly more than one, for multiple languages.
    rr_msg :: ![PMessage],
    -- | Custom presentation for violations, currently only in a single language
    rr_viol :: !(Maybe (PairView (Term a)))
  }
  deriving (Show)

instance Ord (P_Rule a) where
  compare a b = case compare (name a) (name b) of
    EQ ->
      fromMaybe
        ( fatal
            . T.intercalate "\n"
            $ [ "P_Rule a should have a non-fuzzy Origin.",
                tshow (origin a),
                tshow (origin b)
              ]
        )
        (maybeOrdering (origin a) (origin b))
    x -> x

instance Eq (P_Rule a) where -- Required for merge of P_Contexts
  a == b = compare a b == EQ

instance Traced (P_Rule a) where
  origin P_Rule {pos = orig} = orig

instance Functor P_Rule where fmap = fmapDefault

instance Foldable P_Rule where foldMap = foldMapDefault

instance Traversable P_Rule where
  traverse f (P_Rule fps nm lbl expr mean msg viol) =
    (\e v -> P_Rule fps nm lbl e mean msg v) <$> traverse f expr <*> traverse (traverse (traverse f)) viol

instance Named (P_Rule a) where
  name = rr_nm

newtype PMeaning = PMeaning P_Markup
  deriving (Show, Eq)

newtype PMessage = PMessage P_Markup
  deriving (Show)

data P_Markup = P_Markup
  { mLang :: Maybe Lang,
    mFormat :: Maybe PandocFormat,
    mString :: Text
  }
  deriving (Show, Eq) -- for debugging only

data P_Population
  = P_RelPopu
      { p_src :: Maybe P_Concept, -- a separate src and tgt instead of "Maybe Sign", such that it is possible to specify only one of these.
        p_tgt :: Maybe P_Concept, -- these src and tgt must be more specific than the P_NamedRel
        pos :: Origin, -- the origin
        p_nmdr :: P_NamedRel, -- the named relation that corresponds with the table which the pairs (p_popps) are stored.
        p_popps :: [PAtomPair] -- the contents
      }
  | P_CptPopu
      { pos :: Origin, -- the origin
        p_cpt :: P_Concept, -- the concept the population belongs to
        p_popas :: [PAtomValue] -- atoms in the initial population of that concept
      }
  deriving (Show) -- For QuickCheck error messages only!
  -- NOTE :: Do NOT make instance Eq P_Population, for this is causing problems with merging.

instance Named P_Population where
  name P_RelPopu {p_nmdr = rel} = name rel
  name P_CptPopu {p_cpt = cpt} = name cpt

instance Traced P_Population where
  origin P_RelPopu {pos = orig} = orig
  origin P_CptPopu {pos = orig} = orig

data P_Interface = P_Ifc
  { -- | The interface is of type API
    ifc_IsAPI :: !Bool,
    -- | the name of the interface
    ifc_Name :: !Name,
    ifc_lbl :: !(Maybe Label),
    -- | a list of roles that may use this interface
    ifc_Roles :: ![Role],
    -- | the context term (mostly: I[c])
    ifc_Obj :: !P_BoxBodyElement,
    pos :: !Origin,
    ifc_Prp :: !Text
  }
  deriving (Show) -- For QuickCheck error messages only!

instance Ord P_Interface where -- Required for merge of P_Contexts
  compare a b = case compare (name a) (name b) of
    EQ ->
      fromMaybe
        ( fatal
            . T.intercalate "\n"
            $ [ "P_Interface should have a non-fuzzy Origin.",
                tshow (origin a),
                tshow (origin b)
              ]
        )
        (maybeOrdering (origin a) (origin b))
    x -> x

instance Eq P_Interface where
  a == b = compare a b == EQ

instance Named P_Interface where
  name = ifc_Name

instance Labeled P_Interface where
  mLabel = ifc_lbl

instance Traced P_Interface where
  origin P_Ifc {pos = orig} = orig

type P_SubInterface = P_SubIfc TermPrim

data P_SubIfc a
  = P_Box
      { pos :: !Origin,
        si_header :: !HTMLtemplateCall,
        si_box :: [P_BoxItem a]
      }
  | P_InterfaceRef
      { pos :: !Origin,
        si_isLink :: !Bool, -- True iff LINKTO is used. (will display as hyperlink)
        si_str :: !Name -- Name of the interface that is reffered to
      }
  deriving (Show)

-- | Key-value pairs used to supply attributes into an HTML template that is used to render a subinterface
data HTMLtemplateCall = HTMLtemplateCall
  { pos :: !Origin,
    -- | Type of the HTML template that is used for rendering
    btType :: !Text1,
    -- | Key-value pairs
    btKeys :: [TemplateKeyValue]
  }
  deriving (Show, Data)

instance Ord HTMLtemplateCall where
  compare a b = compare (btType a, L.sort (btKeys a)) (btType b, L.sort (btKeys b))

instance Eq HTMLtemplateCall where
  a == b = compare a b == EQ

instance Unique HTMLtemplateCall where
  showUnique x = btType x T1.<>. (T.concat . fmap (text1ToText . showUnique) . L.sort . btKeys $ x)

instance Traced HTMLtemplateCall where
  origin HTMLtemplateCall {pos = orig} = orig

data TemplateKeyValue = TemplateKeyValue
  { pos :: !Origin,
    -- | Name of the attribute
    tkkey :: !Text1,
    -- | value of the attribute. (when no value, the attribute is handled like a switch)
    tkval :: !(Maybe Text)
  }
  deriving (Show, Data)

instance Ord TemplateKeyValue where
  compare a b = compare (tkkey a, tkval a) (tkkey b, tkval b)

instance Eq TemplateKeyValue where
  a == b = compare a b == EQ

instance Unique TemplateKeyValue where
  showUnique x = toText1Unsafe $ tshow (tkkey x) <> tshow (tkval x)

instance Traced TemplateKeyValue where
  origin TemplateKeyValue {pos = orig} = orig

type P_BoxBodyElement = P_BoxItem TermPrim

data ObjectKind = InterfaceKind | SubInterfaceKind {siMaxDepth :: !Int} | IdentSegmentKind | ViewSegmentKind
  deriving (Show)

data P_BoxItem a
  = P_BoxItemTerm
      { -- | view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
        obj_PlainName :: !(Maybe Text1),
        obj_lbl :: !(Maybe Label),
        -- | position of this definition in the text of the Ampersand source file (filename, line number and column number)
        pos :: !Origin,
        -- | this term describes the instances of this object, related to their context.
        obj_term :: !(Term a),
        -- | the CRUD actions as required by the user
        obj_crud :: !(Maybe P_Cruds),
        -- | The view that should be used for this object
        obj_mView :: !(Maybe Name),
        -- | the attributes, which are object definitions themselves.
        obj_msub :: !(Maybe (P_SubIfc a))
      }
  | P_BxTxt
      { -- | view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
        obj_PlainName :: !(Maybe Text1),
        pos :: !Origin,
        box_txt :: !Text
      }
  deriving (Show) -- just for debugging (zie ook instance Show BoxItem)

instance Ord (P_BoxItem a) where
  compare a b =
    fromMaybe
      ( fatal
          . T.intercalate "\n"
          $ [ "P_BoxItem a should have a non-fuzzy Origin.",
              tshow (origin a),
              tshow (origin b)
            ]
      )
      (maybeOrdering (origin a) (origin b))

instance Eq (P_BoxItem a) where
  a == b = compare a b == EQ

instance Traced (P_BoxItem a) where
  origin P_BoxItemTerm {pos = orig} = orig
  origin P_BxTxt {pos = orig} = orig

data P_Cruds = P_Cruds Origin Text1 deriving (Show)

type P_IdentDef = P_IdentDf TermPrim -- this is what is returned by the parser, but we need to change the "TermPrim" for disambiguation

data P_IdentDf a -- so this is the parametric data-structure
  = P_Id
  { -- | position of this definition in the text of the Ampersand source file (filename, line number and column number).
    pos :: !Origin,
    -- | the name of this Identity. The name has no meaning in the Compliant Service Layer, but is used in the generated user interface.
    ix_name :: !Name,
    -- | a friendly, user readable alternative for the name
    ix_label :: !(Maybe Label),
    -- | this term describes the instances of this object, related to their context
    ix_cpt :: !P_Concept,
    -- | the constituent segments of this identity. TODO: refactor to a list of terms
    ix_ats :: !(NE.NonEmpty (P_IdentSegmnt a))
  }
  deriving (Show)

instance Named (P_IdentDf a) where
  name = ix_name

instance Ord (P_IdentDf a) where
  compare a b =
    fromMaybe
      ( fatal
          . T.intercalate "\n"
          $ [ "P_IdentDf a should have a non-fuzzy Origin.",
              tshow (origin a),
              tshow (origin b)
            ]
      )
      (maybeOrdering (origin a) (origin b))

instance Eq (P_IdentDf a) where
  a == b = compare a b == EQ

instance Traced (P_IdentDf a) where
  origin P_Id {pos = orig} = orig

instance Functor P_IdentDf where fmap = fmapDefault

instance Foldable P_IdentDf where foldMap = foldMapDefault

instance Traversable P_IdentDf where
  traverse f (P_Id orig nm lbl cpt lst) = P_Id orig nm lbl cpt <$> traverse (traverse f) lst

instance Functor P_IdentSegmnt where fmap = fmapDefault

instance Foldable P_IdentSegmnt where foldMap = foldMapDefault

instance Traversable P_IdentSegmnt where
  traverse f (P_IdentExp x) = P_IdentExp <$> traverse f x

type P_IdentSegment = P_IdentSegmnt TermPrim

newtype P_IdentSegmnt a = P_IdentExp {ks_obj :: P_BoxItem a}
  deriving (Eq, Ord, Show)

type P_ViewDef = P_ViewD TermPrim

data P_ViewD a = P_Vd
  { -- | position of this definition in the text of the Ampersand source file (filename, line number and column number).
    pos :: !Origin,
    -- | the name of this View. The name has no meaning in the Compliant Service Layer, but is used in the generated user interface. It is not an empty string.
    vd_nm :: !Name,
    -- | the label of this View, only if provided
    vd_label :: !(Maybe Label),
    -- | the concept for which this view is applicable
    vd_cpt :: !P_Concept,
    -- | whether or not this is the default view for the concept
    vd_isDefault :: !Bool,
    -- | the html template for this view (not required since we may have other kinds of views as well in the future)
    --              , vd_text :: Maybe P_ViewText -- Future extension
    vd_html :: !(Maybe ViewHtmlTemplate),
    -- | the constituent segments of this view.
    vd_ats :: ![P_ViewSegment a]
  }
  deriving (Show)

instance Ord (P_ViewD a) where
  compare a b = case compare (name a) (name b) of
    EQ ->
      fromMaybe
        ( fatal
            . T.intercalate "\n"
            $ [ "P_ViewD a should have a non-fuzzy Origin.",
                tshow (origin a),
                tshow (origin b)
              ]
        )
        (maybeOrdering (origin a) (origin b))
    x -> x

instance Eq (P_ViewD a) where -- Required for merge of P_Contexts
  a == b = compare a b == EQ

instance Traced (P_ViewD a) where
  origin P_Vd {pos = orig} = orig

instance Named (P_ViewD a) where
  name = vd_nm

instance Functor P_ViewD where fmap = fmapDefault

instance Foldable P_ViewD where foldMap = foldMapDefault

instance Traversable P_ViewD where
  traverse fn (P_Vd orig nm lbl cpt isDef html segs) = P_Vd orig nm lbl cpt isDef html <$> traverse (traverse fn) segs

data P_ViewSegment a = P_ViewSegment
  { vsm_labl :: !(Maybe Text1),
    pos :: !Origin,
    vsm_load :: !(P_ViewSegmtPayLoad a)
  }
  deriving (Show)

instance Traced (P_ViewSegment a) where
  origin P_ViewSegment {pos = orig} = orig

instance Functor P_ViewSegment where fmap = fmapDefault

instance Foldable P_ViewSegment where foldMap = foldMapDefault

instance Traversable P_ViewSegment where
  traverse fn (P_ViewSegment a b c) = P_ViewSegment a b <$> traverse fn c

data P_ViewSegmtPayLoad a
  = P_ViewExp {vs_expr :: !(Term a)}
  | P_ViewText {vs_txt :: !Text}
  deriving (Show)

newtype ViewHtmlTemplate = ViewHtmlTemplateFile FilePath
  --              | ViewHtmlTemplateInline Text -- Future extension
  deriving (Eq, Ord, Show, Data)

instance Functor P_ViewSegmtPayLoad where
  fmap = fmapDefault

instance Foldable P_ViewSegmtPayLoad where
  foldMap = foldMapDefault

instance Traversable P_ViewSegmtPayLoad where
  traverse f (P_ViewExp a) = P_ViewExp <$> traverse f a
  traverse _ (P_ViewText a) = pure (P_ViewText a)

data PRef2Obj
  = PRef2ConceptDef !Name
  | PRef2Relation !P_NamedRel
  | PRef2Rule !Name
  | PRef2IdentityDef !Name
  | PRef2ViewDef !Name
  | PRef2Pattern !Name
  | PRef2Interface !Name
  | PRef2Context !Name
  deriving (Show, Eq, Ord) -- only for fatal error messages

-- instance Named PRef2Obj where
--   name pe = case pe of
--     PRef2ConceptDef str -> str
--     PRef2Relation (PNamedRel _ nm mSgn) -> nm <> maybe "" tshow mSgn
--     PRef2Rule str -> str
--     PRef2IdentityDef str -> str
--     PRef2ViewDef str -> str
--     PRef2Pattern str -> str
--     PRef2Interface str -> str
--     PRef2Context str -> str

data PPurpose = PPurpose
  { pos :: Origin, -- the position in the Ampersand script of this purpose definition
    pexObj :: PRef2Obj, -- the reference to the object whose purpose is explained
    pexMarkup :: P_Markup, -- the piece of text, including markup and language info
    pexRefIDs :: [Text] -- the references (for traceability)
  }
  deriving (Show)

instance Ord PPurpose where -- Required for merge of P_Contexts
  compare a b = case compare (pexObj a) (pexObj b) of
    EQ -> case (origin a, origin b) of
      (OriginUnknown, OriginUnknown) -> compare (pexRefIDs a) (pexRefIDs b)
      (OriginUnknown, _) -> LT
      (_, OriginUnknown) -> GT
      (_, _) ->
        fromMaybe
          ( fatal
              . T.intercalate "\n"
              $ [ "PPurpose a should have a non-fuzzy Origin.",
                  tshow (origin a),
                  tshow (origin b)
                ]
          )
          (maybeOrdering (origin a) (origin b))
    x -> x

instance Eq PPurpose where -- Required for merge of P_Contexts
  a == b = compare a b == EQ

instance Traced PPurpose where
  origin PPurpose {pos = orig} = orig

data P_Concept
  = -- | The name of this Concept
    PCpt
      { p_cptnm :: !Name
      -- Note: HJO, 20240901: NO LABEL HERE, because this is only a reference to a (maybe implicilty defined) ConceptDef.
      }
  | -- | The universal Singleton: 'I'['Anything'] = 'V'['Anything'*'Anything']
    P_ONE

instance Eq P_Concept where
  -- (Stef June 17th, 2016)   P_Concept is defined Eq, because P_Relation must be Eq on name and signature.
  a == b = compare a b == EQ

instance Ord P_Concept where
  -- (Sebastiaan 16 jul 2016) P_Concept has been defined Ord, only because we want to maintain sets of concepts in the type checker for quicker lookups.
  compare a b = compare (name a) (name b)

mkPConcept :: Name -> P_Concept
mkPConcept nm =
  if nm == nameOfONE
    then P_ONE
    else PCpt {p_cptnm = nm}

instance Named P_Concept where
  name PCpt {p_cptnm = nm} = nm
  name P_ONE = nameOfONE

instance Show P_Concept where
  show = T.unpack . fullName

data PBinOp
  = LessThan
  | GreaterThan
  | -- | Equal -- NOTE: There is no need for the Equal operator. It doesn't add anything, so we leave it out.
    LessThanOrEqual
  | GreaterThanOrEqual
  deriving (Ord, Eq, Enum, Bounded, Data, Typeable)

instance Hashable PBinOp where
  hashWithSalt s oper =
    s `hashWithSalt` case oper of
      LessThan -> (0 :: Int)
      GreaterThan -> (1 :: Int)
      LessThanOrEqual -> (2 :: Int)
      GreaterThanOrEqual -> (3 :: Int)

binaryFunction :: (Ord a) => PBinOp -> (a -> a -> Bool)
binaryFunction LessThan = (<)
binaryFunction GreaterThan = (>)
binaryFunction LessThanOrEqual = (<=)
binaryFunction GreaterThanOrEqual = (>=)

instance Flippable PBinOp where
  flp LessThan = GreaterThan
  flp GreaterThan = LessThan
  flp LessThanOrEqual = GreaterThanOrEqual
  flp GreaterThanOrEqual = LessThanOrEqual

instance Show PBinOp where
  show x = case x of
    LessThan -> "<"
    GreaterThan -> ">"
    LessThanOrEqual -> "<="
    GreaterThanOrEqual -> ">="

data P_Sign = P_Sign
  { pSrc :: P_Concept,
    pTgt :: P_Concept
  }
  -- (Stef June 17th, 2016)   P_Sign is defined Ord,Eq, because P_Relation must be Ord,Eq on name and signature.
  deriving (Ord, Eq)

instance Show P_Sign where
  show sgn = "[" <> show (pSrc sgn) <> "*" <> show (pTgt sgn) <> "]"

instance Flippable P_Sign where
  flp sgn =
    P_Sign
      { pSrc = pTgt sgn,
        pTgt = pSrc sgn
      }

data PClassify = PClassify
  { pos :: Origin,
    -- | Left hand side concept
    specific :: P_Concept,
    -- | Right hand side concept
    generics :: NE.NonEmpty P_Concept
  }
  deriving (Show)

-- (Stef April 29th, 2020) Eq PClassify is used to generate P-contexts without duplicate PClassify's in it.
instance Eq PClassify where
  p == q = specific p == specific q && generics p == generics q

instance Traced PClassify where
  origin PClassify {pos = orig} = orig

type PProps = Set PProp

data PProp
  = -- | univalent
    P_Uni
  | -- | total
    P_Tot
  | -- | MAP keyword, Ampersand replaces this by [Uni, Tot].
    P_Map
  | -- | injective
    P_Inj
  | -- | surjective
    P_Sur
  | -- | BIJ keyword, Ampersand replaces this by [Inj, Sur].
    P_Bij
  | -- | symmetric
    P_Sym
  | -- | antisymmetric
    P_Asy
  | -- | PROP keyword, Ampersand replaces this by [Sym, Asy].
    P_Prop
  | -- | transitive
    P_Trn
  | -- | reflexive
    P_Rfx
  | -- | irreflexive
    P_Irf
  deriving (Eq, Ord, Typeable, Data, Enum, Bounded)

instance Show PProp where
  show P_Uni = "UNI"
  show P_Inj = "INJ"
  show P_Map = "MAP"
  show P_Sur = "SUR"
  show P_Tot = "TOT"
  show P_Bij = "BIJ"
  show P_Sym = "SYM"
  show P_Asy = "ASY"
  show P_Prop = "PROP"
  show P_Trn = "TRN"
  show P_Rfx = "RFX"
  show P_Irf = "IRF"

instance Unique PProp where
  showUnique = toText1Unsafe . tshow

instance Flippable PProp where
  flp P_Uni = P_Inj
  flp P_Tot = P_Sur
  flp P_Map = P_Bij
  flp P_Sur = P_Tot
  flp P_Inj = P_Uni
  flp P_Bij = P_Map
  flp x = x

data PRelationDefault
  = PDefAtom SrcOrTgt (NE.NonEmpty PAtomValue)
  | PDefEvalPHP SrcOrTgt Text
  deriving (Eq, Ord, Data, Show)

mergeContexts :: P_Context -> P_Context -> P_Context
mergeContexts ctx1 ctx2 =
  PCtx
    { ctx_nm = name ctx1, -- This is an arbitrary choice. For all univalent fields of a context, we take the first one.
      ctx_lbl = ctx_lbl ctx1,
      ctx_pos = fromContextsKeepDoubles ctx_pos,
      ctx_lang = ctx_lang ctx1, -- By taking the first, we end up with the language of the top-level context
      ctx_markup = foldl' (<|>) Nothing $ map ctx_markup contexts,
      ctx_pats = fromContextsKeepDoubles ctx_pats,
      ctx_rs = fromContextsRemoveDoubles ctx_rs,
      ctx_ds = mergeRels (ctx_ds ctx1 <> ctx_ds ctx2),
      ctx_cs = fromContextsKeepDoubles ctx_cs,
      ctx_ks = fromContextsKeepDoubles ctx_ks,
      ctx_rrules = fromContextsKeepDoubles ctx_rrules,
      ctx_reprs = fromContextsKeepDoubles ctx_reprs,
      ctx_vs = fromContextsRemoveDoubles ctx_vs,
      ctx_gs = fromContextsKeepDoubles ctx_gs,
      ctx_ifcs = fromContextsRemoveDoubles ctx_ifcs,
      ctx_ps = fromContextsKeepDoubles ctx_ps,
      ctx_pops = mergePops (ctx_pops ctx1 <> ctx_pops ctx2),
      ctx_metas = fromContextsKeepDoubles ctx_metas,
      ctx_enfs = fromContextsKeepDoubles ctx_enfs
    }
  where
    -- NOTE:
    -- In the P_Structure we want to limit nub as much as possible.
    -- this is to ensure that no information is lost because we do
    -- not know a proper origin of some element. Sometimes the origin
    -- is used to distinquish between two elements. That is not
    -- usefull here, and might lead to information lost.
    fromContextsKeepDoubles :: (P_Context -> [a]) -> [a]
    fromContextsKeepDoubles fun = concatMap fun contexts
    contexts = [ctx1, ctx2]
    fromContextsRemoveDoubles :: (Ord b) => (P_Context -> [b]) -> [b]
    fromContextsRemoveDoubles f =
      Set.toList . Set.unions . map (Set.fromList . f) $ contexts
    mergePops :: [P_Population] -> [P_Population]
    mergePops = map mergePopsSameType . NE.groupBy groupCondition
      where
        groupCondition :: P_Population -> P_Population -> Bool
        groupCondition a b =
          case (a, b) of
            (P_RelPopu {}, P_RelPopu {}) ->
              p_src a
                == p_src b
                && p_tgt a
                == p_tgt b
                && sameNamedRels (p_nmdr a) (p_nmdr b)
            (P_CptPopu {}, P_CptPopu {}) -> p_cpt a == p_cpt b
            _ -> False
          where
            sameNamedRels :: P_NamedRel -> P_NamedRel -> Bool
            sameNamedRels x y =
              p_nrnm x
                == p_nrnm y
                && p_mbSign x
                == p_mbSign y
        mergePopsSameType :: NE.NonEmpty P_Population -> P_Population
        mergePopsSameType (h :| tl) = case h of
          P_RelPopu {} -> h {p_popps = Set.toList . Set.unions . map (Set.fromList . p_popps) $ (h : tl)}
          P_CptPopu {} -> h {p_popas = Set.toList . Set.unions . map (Set.fromList . p_popas) $ (h : tl)}
