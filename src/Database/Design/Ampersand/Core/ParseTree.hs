{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric  #-}
module Database.Design.Ampersand.Core.ParseTree (
     P_Context(..), mergeContexts, mkContextOfPopsOnly
   , Meta(..)
   , MetaObj(..)
   , P_RoleRelation(..)
   , P_RoleRule(..)
   , Role(..)
   , P_Pattern(..)
   , P_Declaration(..)
   , Term(..), TermPrim(..), P_NamedRel(..)
   , PairView(..), PairViewSegment(..), PairViewTerm(..), PairViewSegmentTerm(..)
   , SrcOrTgt(..), isSrc
   , P_Rule(..)
   , ConceptDef(..)
   , Representation(..), TType(..)
   , P_Population(..)
   , PAtomPair(..), PAtomValue(..), mkPair, PSingleton, makePSingleton
   , P_ObjectDef, P_SubInterface, P_Interface(..), P_IClass(..), P_ObjDef(..), P_SubIfc(..)
   , P_Cruds(..)
   , P_IdentDef, P_IdentDf(..) , P_IdentSegment, P_IdentSegmnt(..)
   , P_ViewDef , P_ViewSegment, ViewHtmlTemplate(..) {-, ViewTextTemplate-}
   , P_ViewD(..) , P_ViewSegmt(..)

   , PPurpose(..),PRef2Obj(..),PMeaning(..),PMessage(..)

   , P_Concept(..), P_Sign(..)

   , P_Gen(..)

   , Lang(..)
   , P_Markup(..)

   , PandocFormat(..)

   , Prop(..), Props, normalizeProps
   -- Inherited stuff:
   , module Database.Design.Ampersand.Input.ADL1.FilePos
   , gen_concs
  ) where
import Database.Design.Ampersand.Input.ADL1.FilePos
import Database.Design.Ampersand.Basics
import Data.Traversable
import Data.Foldable hiding (concat)
import Data.List (nub)
import Prelude hiding (foldr, sequence, foldl, concatMap)
import Data.Typeable
import Data.Data
import GHC.Generics (Generic)
import Data.Hashable
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime() -- for instance Show UTCTime

data P_Context
   = PCtx{ ctx_nm ::     String           -- ^ The name of this context
         , ctx_pos ::    [Origin]         -- ^ The origin of the context. A context can be a merge of a file including other files c.q. a list of Origin.
         , ctx_lang ::   Lang             -- ^ The default language specified on the top-level context
         , ctx_markup :: Maybe PandocFormat  -- ^ The default markup format for free text in this context
         , ctx_thms ::   [String]         -- ^ Names of patterns/processes to be printed in the functional specification. (For partial documents.)
         , ctx_pats ::   [P_Pattern]      -- ^ The patterns defined in this context
         , ctx_rs ::     [P_Rule TermPrim] -- ^ All user defined rules in this context, but outside patterns and outside processes
         , ctx_ds ::     [P_Declaration]  -- ^ The relations defined in this context, outside the scope of patterns
         , ctx_cs ::     [ConceptDef]     -- ^ The concept definitions defined in this context, outside the scope of patterns
         , ctx_ks ::     [P_IdentDef]     -- ^ The identity definitions defined in this context, outside the scope of patterns
         , ctx_rrules :: [P_RoleRule]     -- ^ The MAINTAIN definitions defined in this context, outside the scope of patterns
         , ctx_rrels ::  [P_RoleRelation] -- ^ The assignment of roles to Relations. (EDITS statements)
         , ctx_reprs ::  [Representation]
         , ctx_vs ::     [P_ViewDef]      -- ^ The view definitions defined in this context, outside the scope of patterns
         , ctx_gs ::     [P_Gen]          -- ^ The gen definitions defined in this context, outside the scope of patterns
         , ctx_ifcs ::   [P_Interface]    -- ^ The interfaces defined in this context
         , ctx_ps ::     [PPurpose]       -- ^ The purposes defined in this context, outside the scope of patterns and processes
         , ctx_pops ::   [P_Population]   -- ^ The populations defined in this context
         , ctx_sql ::    [P_ObjectDef]    -- ^ user defined sqlplugs, taken from the Ampersand script
         , ctx_php ::    [P_ObjectDef]    -- ^ user defined phpplugs, taken from the Ampersand script
         , ctx_metas ::  [Meta]         -- ^ generic meta information (name/value pairs) that can be used for experimenting without having to modify the adl syntax
         } deriving (Show) --For QuickCheck error messages only!

instance Eq P_Context where
  c1 == c2  =  name c1 == name c2

instance Named P_Context where
  name = ctx_nm

-- for declaring name/value pairs with information that is built in to the adl syntax yet
data Meta = Meta { mtPos :: Origin
              , mtObj :: MetaObj
              , mtName :: String
              , mtVal :: String
              } deriving (Show)
instance Eq Meta where --Required for merge of P_Contexts
 p1 == p2 = show p1 == show p2 && origin p1 ==origin p2 
instance Traced Meta where
  origin = mtPos
data MetaObj = ContextMeta deriving Show -- for now, we just have meta data for the entire context

-- | A RoleRelation rs means that any role in 'rrRoles rs' may edit any Relation  in  'rrInterfaces rs'
data P_RoleRelation
   = P_RR { rr_Pos :: Origin      -- ^ position in the Ampersand script
          , rr_Roles :: [Role]      -- ^ list of roles
          , rr_Rels :: [P_NamedRel] -- ^ list of named relations
          } deriving (Show)       -- deriving Show is just for debugging
instance Eq P_RoleRelation where rr==rr' = origin rr==origin rr'
instance Traced P_RoleRelation where
 origin = rr_Pos

 -- | A RoleRule r means that a role called 'mRoles r' must maintain the process rule called 'mRules r'
data P_RoleRule
   = Maintain
     { mPos :: Origin      -- ^ position in the Ampersand script
     , mRoles :: [Role]    -- ^ name of a role
     , mRules :: [String]  -- ^ name of a Rule
     } deriving (Eq, Show) -- deriving (Eq, Show) is just for debugging

data Role = Role String
          | Service String
           deriving (Show, Typeable, Data )   -- deriving (Eq, Show) is just for debugging
instance Eq Role where
 r == r' = name r == name r'
instance Named Role where
 name (Role nm) = nm
 name (Service nm) = nm
instance Unique Role where
 showUnique = name
instance Traced P_RoleRule where
 origin = mPos

data P_Pattern
   = P_Pat { pt_pos :: Origin           -- ^ the starting position in the file in which this pattern was declared.
           , pt_nm :: String            -- ^ Name of this pattern
           , pt_rls :: [P_Rule TermPrim]         -- ^ The user defined rules in this pattern
           , pt_gns :: [P_Gen]          -- ^ The generalizations defined in this pattern
           , pt_dcs :: [P_Declaration]  -- ^ The relations that are declared in this pattern
           , pt_RRuls :: [P_RoleRule]   -- ^ The assignment of roles to rules.
           , pt_RRels :: [P_RoleRelation] -- ^ The assignment of roles to Relations.
           , pt_cds :: [ConceptDef]     -- ^ The concept definitions defined in this pattern
           , pt_Reprs :: [Representation] -- ^ The type into which concepts is represented
           , pt_ids :: [P_IdentDef]     -- ^ The identity definitions defined in this pattern
           , pt_vds :: [P_ViewDef]      -- ^ The view definitions defined in this pattern
           , pt_xps :: [PPurpose]       -- ^ The purposes of elements defined in this pattern
           , pt_pop :: [P_Population]   -- ^ The populations that are local to this pattern
           , pt_end :: Origin           -- ^ the end position in the file in which this pattern was declared.
           } deriving (Show) --For QuickCheck error messages only!

instance Eq P_Pattern where --Required for merge of P_Contexts
 p1 == p2 = name p1 == name p2 && origin p1 ==origin p2 
instance Named P_Pattern where
 name = pt_nm

instance Traced P_Pattern where
 origin = pt_pos

data ConceptDef
   = Cd  { cdpos :: Origin   -- ^ The position of this definition in the text of the Ampersand source (filename, line number and column number).
         , cdcpt :: String   -- ^ The name of the concept for which this is the definition. If there is no such concept, the conceptdefinition is ignored.
         , cdplug:: Bool     -- ^ Whether the user specifically told Ampersand not to store this concept in the database
         , cddef :: String   -- ^ The textual definition of this concept.
         , cdref :: String   -- ^ A label meant to identify the source of the definition. (useful as LaTeX' symbolic reference)
         , cdfrom:: String   -- ^ The name of the pattern or context in which this concept definition was made
         }   deriving (Show,Eq,Typeable)

instance Unique ConceptDef where
  showUnique cd = cdcpt cd++"At"++uniqueShow True (cdpos cd)
instance Traced ConceptDef where
 origin = cdpos
instance Named ConceptDef where
 name = cdcpt

data Representation
  = Repr { reprpos  :: Origin
         , reprcpts  :: [String]  -- ^ the concepts
         , reprdom :: TType     -- the type of the concept the atom is in
         } deriving (Show)
instance Eq Representation where --Required for merge of P_Contexts
 p1 == p2 = show p1 == show p2 && origin p1 ==origin p2 
instance Traced Representation where
 origin = reprpos

data TType
  = Alphanumeric | BigAlphanumeric | HugeAlphanumeric | Password
  | Binary | BigBinary | HugeBinary
  | Date | DateTime
  | Boolean | Integer | Float | Object
  | TypeOfOne --special type for the special concept ONE.
     deriving (Eq, Ord, Typeable)
instance Unique TType where
 showUnique = show
instance Show TType where
  show tt = case tt of
    Alphanumeric      ->   "ALPHANUMERIC"
    BigAlphanumeric   ->   "BIGALPHANUMERIC"
    HugeAlphanumeric  ->   "HUGEALPHANUMERIC"
    Password          ->   "PASSWORD"
    Binary            ->   "BINARY"
    BigBinary         ->   "BIGBINARY"
    HugeBinary        ->   "HUGEBINARY"
    Date              ->   "DATE"
    DateTime          ->   "DATETIME"
    Boolean           ->   "BOOLEAN"
    Integer           ->   "INTEGER"
    Float             ->   "FLOAT"
    Object            ->   "OBJECT"
    TypeOfOne         ->   "TYPEOFONE"
data P_Declaration =
      P_Sgn { dec_nm :: String    -- ^ the name of the declaration
            , dec_sign :: P_Sign    -- ^ the type. Parser must guarantee it is not empty.
            , dec_prps :: Props     -- ^ the user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
            , dec_pragma :: [String]  -- ^ Three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
                                      -- ^    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
            , dec_Mean :: [PMeaning]  -- ^ the optional meaning of a declaration, possibly more than one for different languages.
            , dec_popu :: [PAtomPair]     -- ^ the list of tuples, of which the relation consists.
            , dec_fpos :: Origin    -- ^ the position in the Ampersand source file where this declaration is declared. Not all decalartions come from the ampersand souce file.
            , dec_plug :: Bool      -- ^ if true, this relation may not be stored in or retrieved from the standard database (it should be gotten from a Plug of some sort instead)
            } deriving (Show) --For QuickCheck error messages only!
instance Eq P_Declaration where
 decl==decl' = origin decl==origin decl'
instance Prelude.Ord P_Declaration where
 decl `compare` decl' = origin decl `compare` origin decl'
instance Named P_Declaration where
 name = dec_nm
instance Traced P_Declaration where
 origin = dec_fpos

data PAtomPair
  = PPair { pppos :: Origin
          , ppLeft  :: PAtomValue
          , ppRight :: PAtomValue
          } deriving (Show) --For QuickCheck error messages only!
instance Traced PAtomPair where
  origin = pppos
instance Flippable PAtomPair where
  flp pr = pr{ppLeft = ppRight pr
             ,ppRight = ppLeft pr}
--data PSingleton
--  = PSingleton { psOrig :: Origin
--               , psRaw  :: String
--               , psInterprets :: [PAtomValue]
--               }
--instance Show PSingleton where
-- show = psRaw
--instance Eq PSingleton where
-- a == b = psRaw a == psRaw b
--instance Ord PSingleton where
-- compare a b = compare (psRaw a) (psRaw b)
--instance Traced PSingleton where
-- origin = psOrig
type PSingleton = PAtomValue
makePSingleton :: String -> PSingleton
makePSingleton s = PSingleton (Origin "ParseTree.hs") s Nothing
--   PSingleton { psOrig =Origin "ParseTree.hs"
--              , psRaw = s
--              , psInterprets = fatal 241 "Probably no need to make something up..."
--              }
data PAtomValue
  = PSingleton Origin String (Maybe PAtomValue)
  | ScriptString Origin String -- string from script char to enquote with when printed
  | XlsxString Origin String
  | ScriptInt Origin Integer
  | ScriptFloat Origin Double
  | XlsxDouble Origin Double
  | ComnBool Origin Bool
  | ScriptDate Origin Day
  | ScriptDateTime Origin UTCTime
   deriving (Typeable, Data)
instance Show PAtomValue where -- Used for showing in Expressions as PSingleton
 show pav =
  case pav of
    PSingleton   _ s _ -> singleQuote s
    ScriptString   _ s -> singleQuote s
    XlsxString     _ s -> singleQuote s
    ScriptInt      _ i -> singleQuote (show i)
    ScriptFloat    _ d -> singleQuote (show d)
    XlsxDouble     _ _ -> fatal 267 $ "We got a value from an .xlsx file, which has to be shown in an expression, however the technicaltype is not known"
    ComnBool       _ b -> singleQuote (show b)
    ScriptDate     _ x -> singleQuote (show x)
    ScriptDateTime _ x -> singleQuote (show x)
   where
     singleQuote :: String -> String
     singleQuote str = "\'" ++concatMap f str++"\'"
     f :: Char -> String
     f '\'' = "\\'"
     f c    = [c]
instance Eq PAtomValue where
  PSingleton _ s _ == PSingleton _ s' _ = s == s'
  PSingleton _ _ _ == _                 = False
  ScriptString _ s == ScriptString _ s' = s == s'
  ScriptString _ _ == _                 = False
  XlsxString   _ s == XlsxString   _ s' = s == s'
  XlsxString   _ _ == _                 = False
  ScriptInt    _ i == ScriptInt    _ i' = i == i'
  ScriptInt    _ _ == _                 = False
  ScriptFloat  _ x == ScriptFloat  _ x' = x == x'
  ScriptFloat  _ _ == _                 = False
  XlsxDouble   _ d == XlsxDouble   _ d' = d == d'
  XlsxDouble   _ _ == _                 = False
  ScriptDate   _ d == ScriptDate   _ d' = d == d'
  ScriptDate   _ _ == _                 = False
  ScriptDateTime _ d == ScriptDateTime _ d' = d == d'
  ScriptDateTime _ _ == _               = False
  ComnBool     _ b == ComnBool     _ b' = b == b'
  ComnBool     _ _ == _                 = False

instance Ord PAtomValue where
  compare a b =
   case (a,b) of
    (PSingleton  _ x _ , PSingleton   _ x' _) -> compare x x'
    (PSingleton  _ _ _ , _                  ) -> GT
    (ScriptString   _ x, ScriptString   _ x') -> compare x x'
    (ScriptString   _ _, _                  ) -> GT
    (XlsxString     _ x, XlsxString     _ x') -> compare x x'
    (XlsxString     _ _, _                  ) -> GT
    (ScriptInt      _ x, ScriptInt      _ x') -> compare x x'
    (ScriptInt      _ _, _                  ) -> GT
    (ScriptFloat    _ x, ScriptFloat    _ x') -> compare x x'
    (ScriptFloat    _ _, _                  ) -> GT
    (XlsxDouble     _ x, XlsxDouble     _ x') -> compare x x'
    (XlsxDouble     _ _, _                  ) -> GT
    (ScriptDate     _ x, ScriptDate     _ x') -> compare x x'
    (ScriptDate     _ _, _                  ) -> GT
    (ScriptDateTime _ x, ScriptDateTime _ x') -> compare x x'
    (ScriptDateTime _ _, _                  ) -> GT
    (ComnBool       _ x, ComnBool       _ x') -> compare x x'
    (ComnBool       _ _, _                  ) -> GT
instance Traced PAtomValue where
  origin pav =
   case pav of
    PSingleton   o _ _ -> o
    ScriptString   o _ -> o
    XlsxString     o _ -> o
    ScriptInt      o _ -> o
    ScriptFloat    o _ -> o
    XlsxDouble     o _ -> o
    ComnBool       o _ -> o
    ScriptDate     o _ -> o
    ScriptDateTime o _ -> o

mkPair :: Origin -> PAtomValue -> PAtomValue -> PAtomPair
mkPair o l r
   = PPair { pppos   = o
           , ppLeft  = l
           , ppRight = r}

data TermPrim
   = PI Origin                              -- ^ identity element without a type
                                            --   At parse time, there may be zero or one element in the list of concepts.
                                            --   Reason: when making eqClasses, the least element of that class is used as a witness of that class
                                            --   to know whether an eqClass represents a concept, we only look at its witness
                                            --   By making Pid the first in the data decleration, it becomes the least element for "deriving Ord".
   | Pid Origin P_Concept                   -- ^ identity element restricted to a type
   | Patm Origin PSingleton (Maybe P_Concept)   -- ^ a singleton atom, possibly with a type. The list contains denotational equivalent values
                                                  --   eg, when `123` is found by the parser, the list will contain both interpretations as
                                                  --   the String "123" or as Integer 123.
                                                  --   Since everything between the single quotes can allways be interpretated as a String,
                                                  --   it is quaranteed that the list contains the interpretation as String, and thus cannot
                                                  --   be empty.
   | PVee Origin                            -- ^ the complete relation, of which the type is yet to be derived by the type checker.
   | Pfull Origin P_Concept P_Concept       -- ^ the complete relation, restricted to a type.
                                            --   At parse time, there may be zero, one or two elements in the list of concepts.
   | PNamedR P_NamedRel
   deriving (Show) --For QuickCheck error messages only!

data P_NamedRel = PNamedRel { p_nrpos :: Origin, p_nrnm :: String, p_mbSign :: Maybe P_Sign }
   deriving Show

{- For whenever it may turn out to be useful
instance Eq TermPrim where
  PI _           == PI _            = True
  Pid _ (Just c) == Pid _ (Just c') = p_cptnm c==p_cptnm c'
  Pid _ Nothing  == Pid _ Nothing   = True
  Patm _ x c     == Patm _ x' c'    = x==x' && p_cptnm c==p_cptnm c'
  PVee _         == PVee _          = True
  Pfull _ c d    == Pfull _ c' d'   = p_cptnm c==p_cptnm c' && d==d'
  Prel _ x       == Prel _ x'       = x==x'
  PTrel _ x s    == PTrel _ x' s'   = x==x' && pSrc s==pSrc s' && pTgt s==pTgt s'
  _ == _ = False
-}

data Term a
   = Prim a
   | PEqu Origin (Term a) (Term a)  -- ^ equivalence             =
   | PInc Origin (Term a) (Term a)  -- ^ inclusion               |-
   | PIsc Origin (Term a) (Term a)  -- ^ intersection            /\
   | PUni Origin (Term a) (Term a)  -- ^ union                   \/
   | PDif Origin (Term a) (Term a)  -- ^ difference              -
   | PLrs Origin (Term a) (Term a)  -- ^ left residual           /
   | PRrs Origin (Term a) (Term a)  -- ^ right residual          \
   | PDia Origin (Term a) (Term a)  -- ^ diamond                 <>
   | PCps Origin (Term a) (Term a)  -- ^ composition             ;
   | PRad Origin (Term a) (Term a)  -- ^ relative addition       !
   | PPrd Origin (Term a) (Term a)  -- ^ cartesian product       #
   | PKl0 Origin (Term a)           -- ^ Rfx.Trn closure         *  (Kleene star)
   | PKl1 Origin (Term a)           -- ^ Transitive closure      +  (Kleene plus)
   | PFlp Origin (Term a)           -- ^ conversion (flip, wok)  ~
   | PCpl Origin (Term a)           -- ^ Complement
   | PBrk Origin (Term a)           -- ^ bracketed expression ( ... )
   deriving (Show) -- deriving Show for debugging purposes
instance Functor Term where fmap = fmapDefault
instance Foldable Term where foldMap = foldMapDefault
instance Traversable Term where
 traverse f' x
  = case x of
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
    PKl0 o a   -> PKl0 o <$> f a
    PKl1 o a   -> PKl1 o <$> f a
    PFlp o a   -> PFlp o <$> f a
    PCpl o a   -> PCpl o <$> f a
    PBrk o a   -> PBrk o <$> f a
  where f = traverse f'

instance Functor P_SubIfc where fmap = fmapDefault
instance Foldable P_SubIfc where foldMap = foldMapDefault
instance Traversable P_SubIfc where
  traverse _ (P_InterfaceRef o a b cs) = pure (P_InterfaceRef o a b cs)
  traverse f (P_Box o c lst) = P_Box o c <$> traverse (traverse f) lst

instance Traced (P_SubIfc a) where
 origin = si_ori

instance Functor P_ObjDef where fmap = fmapDefault
instance Foldable P_ObjDef where foldMap = foldMapDefault
instance Traversable P_ObjDef where
 traverse f (P_Obj nm pos ctx mCrud mView msub strs)
  = (\ctx' msub'->(P_Obj nm pos ctx' mCrud mView msub' strs)) <$>
     traverse f ctx <*> traverse (traverse f) msub

instance Traced TermPrim where
 origin e = case e of
   PI orig        -> orig
   Pid orig _     -> orig
   Patm orig _ _  -> orig
   PVee orig      -> orig
   Pfull orig _ _ -> orig
   PNamedR r      -> origin r

--instance Named TermPrim where
-- name e = case e of
--   PI _        -> "I"
--   Pid _ _     -> "I"
--   Patm _ s _  -> s
--   PVee _      -> "V"
--   Pfull _ _ _ -> "V"
--   PNamedR r   -> name r
--
instance Traced P_NamedRel where
  origin (PNamedRel o _ _) = o

instance Named P_NamedRel where
  name (PNamedRel _ nm _) = nm

instance Traced a => Traced (Term a) where
 origin e = case e of
   Prim a         -> origin a
   PEqu orig _ _  -> orig
   PInc orig _ _  -> orig
   PIsc orig _ _  -> orig
   PUni orig _ _  -> orig
   PDif orig _ _  -> orig
   PLrs orig _ _  -> orig
   PRrs orig _ _  -> orig
   PDia orig _ _  -> orig
   PCps orig _ _  -> orig
   PRad orig _ _  -> orig
   PPrd orig _ _  -> orig
   PKl0 orig _    -> orig
   PKl1 orig _    -> orig
   PFlp orig _    -> orig
   PCpl orig _    -> orig
   PBrk orig _    -> orig

data SrcOrTgt = Src | Tgt deriving (Show, Eq, Ord, Generic)
instance Hashable SrcOrTgt
instance Flippable SrcOrTgt where
  flp Src = Tgt
  flp Tgt = Src

isSrc :: SrcOrTgt -> Bool
isSrc Src = True
isSrc Tgt = False

data PairView a = PairView { ppv_segs :: [PairViewSegment a] } deriving (Show, Typeable, Eq, Generic)
instance Hashable a => Hashable (PairView a)
instance Traced a => Traced (PairView a) where
  origin pv =
    case ppv_segs pv of
       [] -> fatal 342 "An empty PairView must not occur"
       xs -> origin (head xs)
data PairViewSegment a =
    PairViewText{ pvsOrg :: Origin
                , pvsStr :: String
                }
  | PairViewExp { pvsOrg :: Origin
                , pvsSoT :: SrcOrTgt
                , pvsExp :: a
                } deriving (Show, Typeable, Eq, Generic)
instance Hashable a => Hashable (PairViewSegment a)
instance Traced (PairViewSegment a) where
  origin = pvsOrg

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

data P_Rule a  =
   P_Ru { rr_fps ::  Origin            -- ^ Position in the Ampersand file
        , rr_nm ::   String            -- ^ Name of this rule
        , rr_exp ::  Term a            -- ^ The rule expression
        , rr_mean :: [PMeaning]        -- ^ User-specified meanings, possibly more than one, for multiple languages.
        , rr_msg ::  [PMessage]        -- ^ User-specified violation messages, possibly more than one, for multiple languages.
        , rr_viol :: Maybe (PairView (Term a))  -- ^ Custom presentation for violations, currently only in a single language
        } deriving Show
instance Eq (P_Rule a) where --Required for merge of P_Contexts
 p1 == p2 = name p1 == name p2 && origin p1 ==origin p2 

instance Traced (P_Rule a) where
 origin = rr_fps
instance Functor P_Rule where fmap = fmapDefault
instance Foldable P_Rule where foldMap = foldMapDefault
instance Traversable P_Rule where
 traverse f (P_Ru fps nm expr mean msg viol)
  = (\e v -> P_Ru fps nm e mean msg v) <$> traverse f expr <*> traverse (traverse (traverse f)) viol

instance Named (P_Rule a) where
 name = rr_nm

newtype PMeaning = PMeaning P_Markup
         deriving Show
newtype PMessage = PMessage P_Markup
         deriving Show
data P_Markup =
    P_Markup  { mLang   ::   Maybe Lang
              , mFormat :: Maybe PandocFormat
              , mString :: String
              } deriving Show -- for debugging only

data P_Population
  = P_RelPopu { p_src   :: Maybe String -- a separate src and tgt instead of "Maybe Sign", such that it is possible to specify only one of these.
              , p_tgt   :: Maybe String -- these src and tgt must be more specific than the P_NamedRel
              , p_orig  :: Origin  -- the origin
              , p_nmdr  :: P_NamedRel  -- the named relation
              , p_popps :: [PAtomPair]   -- the contents
              }
  | P_CptPopu { p_orig  :: Origin  -- the origin
              , p_cnme  :: String  -- the name of a concept
              , p_popas :: [PAtomValue]  -- atoms in the initial population of that concept
              }
   deriving (Show) --For QuickCheck error messages only!

instance Eq P_Population where --Required for merge of P_Contexts
 p1 == p2 = name p1 == name p2 && origin p1 ==origin p2 

instance Named P_Population where
 name P_RelPopu{p_nmdr = nr} = name nr
 name P_CptPopu{p_cnme = nm} = nm

instance Traced P_Population where
 origin = p_orig

data P_Interface =
     P_Ifc { ifc_Name :: String           -- ^ the name of the interface
           , ifc_Class :: Maybe String    -- ^ the class of the interface
           , ifc_Params :: [P_NamedRel]     -- ^ a list of named relations that are editable within this interface.
           , ifc_Args :: [[String]]       -- ^ a list of arguments for code generation.
           , ifc_Roles :: [Role]        -- ^ a list of roles that may use this interface
           , ifc_Obj :: P_ObjectDef       -- ^ the context expression (mostly: I[c])
           , ifc_Pos :: Origin
           , ifc_Prp :: String
           } deriving (Show) --For QuickCheck error messages only!

instance Eq P_Interface where --Required for merge of P_Contexts
 p1 == p2 = name p1 == name p2 && origin p1 ==origin p2 
instance Named P_Interface where
 name = ifc_Name

instance Traced P_Interface where
 origin = ifc_Pos

data P_IClass = P_IClass { iclass_name :: String } deriving (Eq, Show)

type P_SubInterface = P_SubIfc TermPrim
data P_SubIfc a
              = P_Box          { si_ori :: Origin
                               , si_class :: Maybe String
                               , si_box :: [P_ObjDef a] }
              | P_InterfaceRef { si_ori :: Origin
                               , si_isLink :: Bool --True iff LINKTO is used. (will display as hyperlink)
                               , si_str :: String  -- Name of the interface that is reffered to
                               , si_crud :: Maybe P_Cruds -- ^ string containing the CRUD actions as required by the user
                               } 
                deriving (Show)

type P_ObjectDef = P_ObjDef TermPrim
data P_ObjDef a =
     P_Obj { obj_nm :: String          -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
           , obj_pos :: Origin         -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number)
           , obj_ctx :: Term a         -- ^ this expression describes the instances of this object, related to their context.
           , obj_crud :: Maybe P_Cruds  -- ^ string containing the CRUD actions as required by the user  
           , obj_mView :: Maybe String -- ^ The view that should be used for this object
           , obj_msub :: Maybe (P_SubIfc a)  -- ^ the attributes, which are object definitions themselves.
           , obj_strs :: [[String]]    -- ^ directives that specify the interface.
           }  deriving (Show)       -- just for debugging (zie ook instance Show ObjectDef)
instance Eq (P_ObjDef a) where od==od' = origin od==origin od'
instance Named (P_ObjDef a) where
 name = obj_nm
instance Traced (P_ObjDef a) where
 origin = obj_pos
data P_Cruds = P_Cruds Origin String deriving Show
type P_IdentDef = P_IdentDf TermPrim -- this is what is returned by the parser, but we need to change the "TermPrim" for disambiguation
data P_IdentDf a = -- so this is the parametric data-structure
         P_Id { ix_pos :: Origin         -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number).
              , ix_lbl :: String         -- ^ the name (or label) of this Identity. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface. It is not an empty string.
              , ix_cpt :: P_Concept      -- ^ this expression describes the instances of this object, related to their context
              , ix_ats :: [P_IdentSegmnt a] -- ^ the constituent segments of this identity. TODO: refactor to a list of terms
              } deriving (Show)
instance Named (P_IdentDf a) where
 name = ix_lbl
instance Eq (P_IdentDf a) where identity==identity' = origin identity==origin identity'
instance Traced (P_IdentDf a) where
 origin = ix_pos
instance Functor P_IdentDf where fmap = fmapDefault
instance Foldable P_IdentDf where foldMap = foldMapDefault
instance Traversable P_IdentDf where
  traverse f (P_Id a b c lst) = P_Id a b c <$> traverse (traverse f) lst
instance Functor P_IdentSegmnt where fmap = fmapDefault
instance Foldable P_IdentSegmnt where foldMap = foldMapDefault
instance Traversable P_IdentSegmnt where
  traverse f (P_IdentExp x) = P_IdentExp <$> traverse f x

type P_IdentSegment = P_IdentSegmnt TermPrim
data P_IdentSegmnt a
              = P_IdentExp  { ks_obj :: P_ObjDef a}
                deriving (Eq, Show)

type P_ViewDef = P_ViewD TermPrim
data P_ViewD a =
         P_Vd { vd_pos :: Origin            -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number).
              , vd_lbl :: String            -- ^ the name (or label) of this View. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface. It is not an empty string.
              , vd_cpt :: P_Concept         -- ^ the concept for which this view is applicable
              , vd_isDefault :: Bool        -- ^ whether or not this is the default view for the concept
              , vd_html :: Maybe ViewHtmlTemplate -- ^ the html template for this view (not required since we may have other kinds of views as well in the future)
--              , vd_text :: Maybe P_ViewText -- Future extension
              , vd_ats :: [P_ViewSegmt a]   -- ^ the constituent segments of this view.
              } deriving (Show)
instance Eq (P_ViewD a) where --Required for merge of P_Contexts
 p1 == p2 = name p1 == name p2 && origin p1 ==origin p2 

instance Named (P_ViewD a) where
 name = vd_lbl
instance Functor P_ViewD where fmap = fmapDefault
instance Foldable P_ViewD where foldMap = foldMapDefault
instance Traversable P_ViewD where
 traverse fn (P_Vd a b c d e f) = P_Vd a b c d e <$> traverse (traverse fn) f

type P_ViewSegment = P_ViewSegmt TermPrim
data P_ViewSegmt a  = P_ViewExp  { vs_nr ::Integer, vs_obj :: P_ObjDef a }
                    | P_ViewText { vs_nr ::Integer, vs_txt :: String }
                    | P_ViewHtml { vs_nr ::Integer, vs_htm :: String }
                      deriving (Eq, Show)

data ViewHtmlTemplate = ViewHtmlTemplateFile String
--              | ViewHtmlTemplateInline String -- Future extension
                  deriving (Eq, Show)

{- Future extension:
data ViewText = ViewTextTemplateFile String
              | ViewTextTemplateInline String
                  deriving (Eq, Show)
-}

instance Functor P_ViewSegmt where fmap = fmapDefault
instance Foldable P_ViewSegmt where foldMap = foldMapDefault
instance Traversable P_ViewSegmt where
 traverse f (P_ViewExp i a) = P_ViewExp i <$> traverse f a
 traverse _ (P_ViewText i a) = pure (P_ViewText i a)
 traverse _ (P_ViewHtml i a) = pure (P_ViewHtml i a)

instance Traced (P_ViewD a) where
 origin = vd_pos


-- PPurpose is a parse-time constructor. It contains the name of the object it explains.
-- It is a pre-explanation in the sense that it contains a reference to something that is not yet built by the compiler.
--                       Constructor      name          RefID  Explanation
data PRef2Obj = PRef2ConceptDef String
              | PRef2Declaration P_NamedRel
              | PRef2Rule String
              | PRef2IdentityDef String
              | PRef2ViewDef String
              | PRef2Pattern String
              | PRef2Interface String
              | PRef2Context String
              deriving Show -- only for fatal error messages

instance Named PRef2Obj where
  name pe = case pe of
     PRef2ConceptDef str -> str
     PRef2Declaration (PNamedRel _ nm mSgn) -> nm++maybe "" show mSgn
     PRef2Rule str -> str
     PRef2IdentityDef str -> str
     PRef2ViewDef str -> str
     PRef2Pattern str -> str
     PRef2Interface str -> str
     PRef2Context str -> str

data PPurpose = PRef2 { pexPos :: Origin      -- the position in the Ampersand script of this purpose definition
                      , pexObj :: PRef2Obj    -- the reference to the object whose purpose is explained
                      , pexMarkup:: P_Markup  -- the piece of text, including markup and language info
                      , pexRefIDs :: [String] -- the references (for traceability)
                      } deriving Show
instance Eq PPurpose where --Required for merge of P_Contexts
 p1 == p2 = name p1 == name p2 && origin p1 ==origin p2 

instance Named PPurpose where
 name pe = name (pexObj pe)

instance Traced PPurpose where
 origin = pexPos

data P_Concept
   = PCpt{ p_cptnm :: String }  -- ^The name of this Concept
   | P_Singleton
--      deriving (Eq, Ord)
-- (Sebastiaan 12 feb 2012) P_Concept has been defined Ord, only because we want to maintain sets of concepts in the type checker for quicker lookups.
-- (Sebastiaan 11 okt 2013) Removed this again, I thought it would be more clean to use newtype for this instead

instance Named P_Concept where
 name (PCpt {p_cptnm = nm}) = nm
 name P_Singleton = "ONE"

instance Show P_Concept where
 showsPrec _ c = showString (name c)

data P_Sign = P_Sign {pSrc :: P_Concept, pTgt :: P_Concept }

instance Show P_Sign where
  showsPrec _ sgn =
      showString (   "[" ++ show (pSrc sgn)++"*"++show (pTgt sgn) ++ "]" )
instance Flippable P_Sign where
  flp sgn = P_Sign { pSrc = pTgt sgn
                   , pTgt = pSrc sgn
                   }

data P_Gen =  P_Cy{ gen_fp ::  Origin            -- ^ Position in the Ampersand file
                  , gen_spc :: P_Concept         -- ^ Left hand side concept expression
                  , gen_rhs :: [P_Concept]       -- ^ Right hand side concept expression
                  }
            | PGen{ gen_fp  :: Origin         -- ^ the position of the GEN-rule
                  , gen_spc :: P_Concept      -- ^ specific concept
                  , gen_gen :: P_Concept      -- ^ generic concept
                  }
instance Eq P_Gen where --Required for merge of P_Contexts
 p1 == p2 = origin p1 ==origin p2 
gen_concs :: P_Gen -> [P_Concept]
gen_concs (P_Cy {gen_rhs=x}) = x
gen_concs (PGen {gen_gen=x,gen_spc=y}) = [x,y]

instance Show P_Gen where
 -- This show is used in error messages.
 showsPrec _ g = showString ("CLASSIFY "++show (gen_spc g)++" IS "++show (gen_concs g))

instance Traced P_Gen where
 origin = gen_fp

data Lang = Dutch | English deriving (Show, Eq, Ord,Typeable, Data)

data PandocFormat = HTML | ReST | LaTeX | Markdown deriving (Eq, Show, Ord)

type Props = [Prop]

data Prop      = Uni          -- ^ univalent
               | Inj          -- ^ injective
               | Sur          -- ^ surjective
               | Tot          -- ^ total
               | Sym          -- ^ symmetric
               | Asy          -- ^ antisymmetric
               | Trn          -- ^ transitive
               | Rfx          -- ^ reflexive
               | Irf          -- ^ irreflexive
               | Aut          -- ^ automatically computed (NOTE: this is a hacky way to denote these until we have appropriate syntax)
               | Prop         -- ^ PROP keyword, later replaced by [Sym, Asy]
                 deriving (Eq, Ord, Enum, Bounded,Typeable, Data)

instance Show Prop where
 showsPrec _ Uni = showString "UNI"
 showsPrec _ Inj = showString "INJ"
 showsPrec _ Sur = showString "SUR"
 showsPrec _ Tot = showString "TOT"
 showsPrec _ Sym = showString "SYM"
 showsPrec _ Asy = showString "ASY"
 showsPrec _ Trn = showString "TRN"
 showsPrec _ Rfx = showString "RFX"
 showsPrec _ Irf = showString "IRF"
 showsPrec _ Aut = showString "AUT"
 showsPrec _ Prop = showString "PROP"

instance Unique Prop where
 showUnique = show

instance Flippable Prop where
 flp Uni = Inj
 flp Tot = Sur
 flp Sur = Tot
 flp Inj = Uni
 flp x = x

normalizeProps :: [Prop] -> [Prop]
normalizeProps = nub.conv.rep
    where -- replace PROP by SYM, ASY
          rep (Prop:ps) = [Sym, Asy] ++ rep ps
          rep (p:ps) = (p:rep ps)
          rep [] = []
          -- add Uni and Inj if ps has neither Sym nor Asy
          conv ps = ps ++ concat [[Uni, Inj] | null ([Sym, Asy]>-ps)]

mergeContexts :: P_Context -> P_Context -> P_Context
mergeContexts ctx1 ctx2 =
  PCtx{ ctx_nm     = case (filter (not.null) . map ctx_nm) contexts of
                        []    -> ""
                        (x:_) -> x
      , ctx_pos    = nub . concatMap ctx_pos $ contexts
      , ctx_lang   = ctx_lang ctx1 -- By taking the first, we end up with the language of the top-level context
      , ctx_markup = foldl orElse Nothing $ map ctx_markup contexts
      , ctx_thms   = nub . concatMap ctx_thms $ contexts
      , ctx_pats   = nub . concatMap ctx_pats $ contexts
      , ctx_rs     = nub . concatMap ctx_rs $ contexts
      , ctx_ds     = nub . concatMap ctx_ds $ contexts
      , ctx_cs     = nub . concatMap ctx_cs $ contexts
      , ctx_ks     = nub . concatMap ctx_ks $ contexts
      , ctx_rrules = nub . concatMap ctx_rrules $ contexts
      , ctx_rrels  = nub . concatMap ctx_rrels $ contexts
      , ctx_reprs  = nub . concatMap ctx_reprs $ contexts
      , ctx_vs     = nub . concatMap ctx_vs $ contexts
      , ctx_gs     = nub . concatMap ctx_gs $ contexts
      , ctx_ifcs   = nub . concatMap ctx_ifcs $ contexts
      , ctx_ps     = nub . concatMap ctx_ps $ contexts
      , ctx_pops   = nub . concatMap ctx_pops $ contexts
      , ctx_sql    = nub . concatMap ctx_sql $ contexts
      , ctx_php    = nub . concatMap ctx_php $ contexts
      , ctx_metas  = nub . concatMap ctx_metas $ contexts
      }
    where contexts = [ctx1,ctx2]

mkContextOfPopsOnly :: [P_Population] -> P_Context
mkContextOfPopsOnly pops =
  PCtx{ ctx_nm     = ""
      , ctx_pos    = []
      , ctx_lang   = fatal 686 "No language because of excel import hack. Please report this as a bug"
      , ctx_markup = Nothing
      , ctx_thms   = []
      , ctx_pats   = []
      , ctx_rs     = []
      , ctx_ds     = []
      , ctx_cs     = []
      , ctx_ks     = []
      , ctx_rrules = []
      , ctx_rrels  = []
      , ctx_reprs  = []
      , ctx_vs     = []
      , ctx_gs     = []
      , ctx_ifcs   = []
      , ctx_ps     = []
      , ctx_pops   = pops
      , ctx_sql    = []
      , ctx_php    = []
      , ctx_metas  = []
      }
-- | Left-biased choice on maybes
orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = case x of
                 Just _  -> x
                 Nothing -> y
