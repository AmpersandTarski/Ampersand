{-# LANGUAGE DuplicateRecordFields #-}

module Ampersand.ADL1.TypeCheckTypes
  ( TExpression (..),
    TInterface (..),
    TObjectDef (..),
    TSubInterface (..),
    TBoxItem (..),
    isFitForCrudC,
    isFitForCrudR,
    isFitForCrudU,
    isFitForCrudD,
    tObjectDef2pObjectDef,
    tExpression2pTermPrim,
  )
where

-- used for type-checking
import Ampersand.Basics hiding (conc, set)
import Ampersand.Core.A2P_Converters
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ParseTree

-- a data definition for the type of an expression, but without knowledge of the TTypes
data TExpression
  = -- | equivalence             =
    TEEqu !(TExpression, TExpression)
  | -- | inclusion               |-
    TEInc !(TExpression, TExpression)
  | -- | intersection            /\
    TEIsc !(TExpression, TExpression)
  | -- | union                   \/
    TEUni !(TExpression, TExpression)
  | -- | difference              -
    TEDif !(TExpression, TExpression)
  | -- | left residual           /
    TELrs !(TExpression, TExpression)
  | -- | right residual          \
    TERrs !(TExpression, TExpression)
  | -- | diamond                 <>
    TEDia !(TExpression, TExpression)
  | -- | composition             ;
    TECps !(TExpression, TExpression)
  | -- | relative addition       !
    TERad !(TExpression, TExpression)
  | -- | cartesian product       *
    TEPrd !(TExpression, TExpression)
  | -- | Rfx.Trn closure         *  (Kleene star)
    TEKl0 !TExpression
  | -- | Transitive closure      +  (Kleene plus)
    TEKl1 !TExpression
  | -- | conversion (flip, wok)  ~
    TEFlp !TExpression
  | -- | Complement
    TECpl !TExpression
  | -- | bracketed expression ( ... )
    TEBrk !TExpression
  | -- | simple relation
    TEDcD !Relation
  | -- | Identity relation
    TEDcI !A_Concept
  | -- | Epsilon relation (introduced by the system to ensure we compare concepts by equality only.
    TEEps !A_Concept !Signature
  | -- | relation based on a simple binary operator  (e.g. x > y)
    TEBin !PBinOp !A_Concept
  | -- | Cartesian product relation
    TEDcV !Signature
  | -- | constant PAtomValue, because when building the Expression, the TType of the concept isn't known yet.
    TEMp1 !PAtomValue !A_Concept
  deriving (Eq, Ord, Show, Typeable)

data TInterface a = TInterface
  { -- | is this interface of type API?
    tifcIsAPI :: !Bool,
    -- | The name of the interface
    tifcname :: !Name,
    tifclbl :: !(Maybe Label),
    -- | All roles for which an interface is available (empty means: available for all roles)
    tifcRoles :: ![Role],
    -- | NOTE: this top-level ObjectDef contains the interface itself (ie. name and expression)
    tifcObj :: !(TObjectDef a),
    -- | All conjuncts that must be evaluated after a transaction
    tifcConjuncts :: ![Conjunct],
    -- | The position in the file (filename, line- and column number)
    tifcPos :: !Origin,
    -- | The purpose of the interface
    tifcPurpose :: !Text
  }
  deriving (Show)

data TObjectDef a = TObjectDef
  { -- | view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
    tobjPlainName :: !(Maybe Text1),
    tobjlbl :: !(Maybe Label),
    -- | position of this definition in the text of the Ampersand source file (filename, line number and column number)
    tobjPos :: !Origin,
    -- | this term describes the instances of this object, related to their context.
    tobjExpression :: !TExpression,
    -- | CRUD as defined by the user
    tobjcrud :: !Cruds,
    -- | The view that should be used for this object
    tobjmView :: !(Maybe Name),
    -- | the fields, which are object definitions themselves.
    tobjmsub :: !(Maybe (TSubInterface a))
  }
  deriving (Show) -- just for debugging (zie ook instance Show BoxItem)

instance Traced (TObjectDef a) where
  origin = tobjPos

data TSubInterface a
  = TBox
      { pos :: !Origin,
        tsiConcept :: !A_Concept,
        tsiHeader :: !HTMLtemplateCall,
        tsiObjs :: ![TBoxItem a]
      }
  | TInterfaceRef
      { pos :: !Origin,
        tsiConcept :: !A_Concept,
        tsiIsLink :: !Bool,
        tsiIfcId :: !Name -- id of the interface that is referenced to
      }
  deriving (Show)

data TBoxItem a
  = TBxExpr
      { tobjE :: !(TObjectDef a)
      }
  | -- | view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
    TBxText
      { tboxPlainName :: !(Maybe Text1),
        tboxpos :: !Origin,
        tboxtxt :: !Text
      }
  deriving (Show)

instance Traced (TBoxItem a) where
  origin x = case x of
    TBxExpr o -> origin o
    TBxText _ p _ -> p

-- | Function to determine that the term
--   could be used to create a new atom in its target concept
isFitForCrudC :: TExpression -> Bool
isFitForCrudC expr =
  case expr of
    TEFlp e -> isFitForCrudC e
    TEBrk e -> isFitForCrudC e
    TECps (TEEps _ _, e) -> isFitForCrudC e
    TECps (e, TEEps _ _) -> isFitForCrudC e
    TECps (_, _) -> True
    TEEps _ _ -> False
    TEMp1 {} -> False
    _ -> True

-- EDcI{} -> True -- TODO: set to False when functionality of +menu is adapted from I[Cpt] to V[SESSION*Cpt] expressions (see Issue #884)

-- | Function to determine that the term
--   could be used to read the population of its target concept
isFitForCrudR :: TExpression -> Bool
isFitForCrudR _ = True

-- | Function to determine that the term
--   could be used to insert or delete a pair in the population of a relation
isFitForCrudU :: TExpression -> Bool
isFitForCrudU expr =
  case expr of
    TEDcD {} -> True
    TEFlp e -> isFitForCrudU e
    TEBrk e -> isFitForCrudU e
    TECps (TEEps _ _, e) -> isFitForCrudU e
    TECps (e, TEEps _ _) -> isFitForCrudU e
    TECps (e, TEDcI {}) -> isFitForCrudU e
    TECps (_, _) -> False
    _ -> False

-- | Function to determine that the term is simple, that it
--   could be used to update the population of a relation
isFitForCrudD :: TExpression -> Bool
isFitForCrudD _ = True

tObjectDef2pObjectDef :: TBoxItem TermPrim -> P_BoxBodyElement
tObjectDef2pObjectDef x =
  case x of
    TBxExpr oDef ->
      P_BoxItemTerm
        { pos = origin oDef,
          obj_PlainName = tobjPlainName oDef,
          obj_lbl = tobjlbl oDef,
          obj_term = pterm,
          obj_crud = case tobjmsub oDef of
            Just (TInterfaceRef _ _ False _) -> Nothing -- Crud specification is not allowed in combination with a reference to an interface.
            _ -> Just $ aCruds2pCruds (tobjcrud oDef),
          obj_mView = tobjmView oDef,
          obj_msub = pSubs
        }
    TBxText {} ->
      P_BxTxt
        { obj_PlainName = tboxPlainName x,
          pos = origin x,
          box_txt = tboxtxt x
        }

tExpression2pTermPrim :: TExpression -> Term TermPrim
tExpression2pTermPrim expr =
  case expr of
    TEEqu (l, r) -> PEqu o (tExpression2pTermPrim l) (tExpression2pTermPrim r)
    TEInc (l, r) -> PInc o (tExpression2pTermPrim l) (tExpression2pTermPrim r)
    TEIsc (l, r) -> PIsc o (tExpression2pTermPrim l) (tExpression2pTermPrim r)
    TEUni (l, r) -> PUni o (tExpression2pTermPrim l) (tExpression2pTermPrim r)
    TEDif (l, r) -> PDif o (tExpression2pTermPrim l) (tExpression2pTermPrim r)
    TELrs (l, r) -> PLrs o (tExpression2pTermPrim l) (tExpression2pTermPrim r)
    TERrs (l, r) -> PRrs o (tExpression2pTermPrim l) (tExpression2pTermPrim r)
    TEDia (l, r) -> PDia o (tExpression2pTermPrim l) (tExpression2pTermPrim r)
    TECps (TEEps {}, r) -> tExpression2pTermPrim r
    TECps (l, TEEps {}) -> tExpression2pTermPrim l
    TECps (l, r) -> PCps o (tExpression2pTermPrim l) (tExpression2pTermPrim r)
    TERad (l, r) -> PRad o (tExpression2pTermPrim l) (tExpression2pTermPrim r)
    TEPrd (l, r) -> PPrd o (tExpression2pTermPrim l) (tExpression2pTermPrim r)
    TEEps a _ -> tExpression2pTermPrim $ TEDcI a
    TEKl0 e -> PKl0 o (tExpression2pTermPrim e)
    TEKl1 e -> PKl1 o (tExpression2pTermPrim e)
    TEFlp e -> PFlp o (tExpression2pTermPrim e)
    TECpl e -> PCpl o (tExpression2pTermPrim e)
    TEBrk e -> PBrk o (tExpression2pTermPrim e)
    TEDcD dcl -> Prim . PNamedR . PNamedRel (origin dcl) (name dcl) . Just . aSign2pSign . sign $ dcl
    TEDcI cpt -> Prim . Pid o . aConcept2pConcept $ cpt
    TEBin oper cpt -> Prim . PBind o oper . aConcept2pConcept $ cpt
    TEDcV sgn -> Prim . Pfull o (aConcept2pConcept . source $ sgn) . aConcept2pConcept . target $ sgn
    TEMp1 val cpt -> Prim . Patm o val . Just . aConcept2pConcept $ cpt
  where
    o = Origin $ "Origin is not present in Expression: " <> tshow expr

instance HasSignature TExpression where
  sign (TEEqu (l, r)) = Sign (source l) (target r)
  sign (TEInc (l, r)) = Sign (source l) (target r)
  sign (TEIsc (l, r)) = Sign (source l) (target r)
  sign (TEUni (l, r)) = Sign (source l) (target r)
  sign (TEDif (l, r)) = Sign (source l) (target r)
  sign (TELrs (l, r)) = Sign (source l) (source r)
  sign (TERrs (l, r)) = Sign (target l) (target r)
  sign (TEDia (l, r)) = Sign (source l) (target r)
  sign (TECps (l, r)) = Sign (source l) (target r)
  sign (TERad (l, r)) = Sign (source l) (target r)
  sign (TEPrd (l, r)) = Sign (source l) (target r)
  sign (TEKl0 e) = sign e
  sign (TEKl1 e) = sign e
  sign (TEFlp e) = flp (sign e)
  sign (TECpl e) = sign e
  sign (TEBrk e) = sign e
  sign (TEDcD d) = sign d
  sign (TEDcI c) = Sign c c
  sign (TEBin _ c) = Sign c c
  sign (TEEps _ sgn) = sgn
  sign (TEDcV sgn) = sgn
  sign (TEMp1 _ c) = Sign c c