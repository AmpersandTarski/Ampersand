{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS -XMultiParamTypeClasses  #-} -- to allow multi-parameter classes
{-# OPTIONS -XFunctionalDependencies #-} -- to allow fundeps
{-# OPTIONS -XFlexibleInstances      #-} -- to allow instance types other than 
                                         -- those of the form (T a1 ... an)
                                         -- where a1 ... an are type *variables*
-- | Definition of 'Ampersand' data structures and some common classes
module Data.Ampersand.Definition (
   -- * Class definitions
   Identified(name),
   Association(source,target,sign),
   -- * Data structures 
--   Concept(..),
--   Relation(..),
--   Declaration(..),
--   Expression(..),
--   Morphism(..),
--   Prop(..),Props,
--   Pairs,flipPair,
--   Population(..)    
--   )
   Architecture(..)
  ,Concept(..)
  ,ConceptDef(..)
  ,Context(..)
  ,Declaration(..)
  ,Expression(..)
  ,Gen(..)
  ,KeyDef(..)
  ,Label(..)
  ,Morphism(..)
  ,Service(..),ObjectDef(..),ObjectDefs
  ,RoleRelation(..),RoleService(..)
  ,Pairs,Paire,mkPair
  ,Pattern(..)
  ,PExplanation(..)
  ,Population(..)
  ,Prop(..)
  ,Rule(..),RuleType(..)
  ,PExplObj(..)
  ,PExpression(..)
  ,FilePos(..)
  ,Sign
  ,UnOp(..),MulOp(..)
                         
   
   
   
   )where
import Data.List  (intercalate)
import DatabaseDesign.Ampersand.Core.Basics (rd) -- TODO: must be removed from here. 
import Adl.Pair (Pairs,flipPair)-- TODO: Must be fully moved to here
import Adl.Prop        -- TODO: Must be fully moved to here
import Adl.FilePos     -- TODO: Must be fully moved to here
import DatabaseDesign.Ampersand.Core.Basics  (Classification)
import Text.Pandoc
------------------------- *Classes* ------------------------
-- | Naming things makes referencing to it easy (in an informal way, of course)
class Identified a where
  name   :: a->String

-- | Association. 
--  This class resides over here, because it is required in some instances of Show.
-- 'source' and 'target' are the minimal functions to define for an instance.
-- ***class (Eq c,Identified c) => Association a c | a -> c  where
class Association a where
    source, target :: a -> Concept
    sign           :: a -> (Concept,Concept)
    sign x = (source x,target x) 
    swap           :: a -> (Concept,Concept)
    swap x = (target x,source x)
    homogeneous :: a -> Bool
    homogeneous s = source s == target s


-------------------- *Data contstructors*-------------------
-- | Architecture of ADL consists of a set of contexts
data Architecture = Arch { archContexts :: [Context]}

---------------------------------------------------------------- 
data Context
   = Ctx { ctxnm    :: String                    -- ^ The name of this context
         , ctxon    :: [String]                  -- ^ The list of extends (= context names of contexts) whose rules are imported
         , ctxpats  :: Patterns                  -- ^ The patterns defined in this context
         , ctxrs    :: Rules                     -- ^ All user defined rules in this context, but outside patterns
         , ctxds    :: Declarations              -- ^ The declarations defined in this context, outside the scope of patterns
         , ctxcs    :: ConceptDefs               -- ^ The concept definitions defined in this context, outside the scope of patterns
         , ctxks    :: KeyDefs                   -- ^ The key definitions defined in this context, outside the scope of patterns
         , ctxsvcs  :: [Service]                 -- ^ The services defined in this context, outside the scope of patterns
         , ctxps    :: PExplanations             -- ^ The pre-explanations defined in this context, outside the scope of patterns
 --        , ctxros   :: [RoleService]             -- ^ The assignment of roles to ObjectDefs (also called role service assignments).
 --                                                --   If r is an element of rsRole (ctxros ctx) and s is an element of rsServ (ctxros ctx), then role r may use service s.
 --        , ctxmed   :: [RoleRelation]            -- ^ The assignment of roles to ObjectDefs.
 --                                                --   If r is an element of rrRole (ctxmed ctx) and p is an element of rrRel (ctxmed ctx), then role r may edit relation p.
         , ctxpops  :: Populations               -- ^ The populations defined in this context
         , ctxsql   :: ObjectDefs  --a list of sqlplugs
         , ctxphp   :: ObjectDefs  --a list of phpplugs
         , ctxenv   :: (Expression,[(Declaration,String)]) --an expression on the context with unbound morphisms, to be bound in this environment
         }

instance Show Context where
  showsPrec _ ctx = showString (ctxnm ctx)

instance Eq Context where
  (==) c1 c2 = name c1 == name c2

instance Identified Context where
  name ctx = ctxnm ctx


---------------------------------------------------------------- 
-- | Patterns are a container for rules that fit a specific theme.
data Pattern
   = Pat { ptnm  :: String        -- ^ Name of this pattern
         , ptrls :: Rules         -- ^ The rules declared in this pattern
         , ptgns :: Gens          -- ^ The generalizations defined in this pattern
         , ptdcs :: Declarations  -- ^ The declarations declared in this pattern
         , ptcds :: ConceptDefs   -- ^ The concept definitions defined in this pattern
         , ptkds :: KeyDefs       -- ^ The key definitions defined in this pattern
         , ptxps :: PExplanations -- ^ The explanations of elements defined in this pattern
--         , testexpr :: [PExpression (Morphism Concept) (Maybe Sign)]
--         , inftestexpr :: [PExpression Declaration Sign]
         }   --deriving (Show) -- voor debugging
type Patterns = [Pattern]

instance Identified Pattern where
  name pat = ptnm pat

---------------------------------------------------------------- 
data ConceptDef 
   = Cd  { cdpos :: FilePos  -- ^ The position of this definition in the text of the ADL source (filename, line number and column number).
         , cdnm  :: String   -- ^ The name of this concept. If there is no such concept, the conceptdefinition is ignored.
         , cdplug:: Bool     -- ^ Whether the user specifically told ADL n—t to store this concept in the database
         , cddef :: String   -- ^ The textual definition of this concept.
         , cdref :: String   -- ^ A label meant to identify the source of the definition. (useful as LaTeX' symbolic reference)
         }   deriving (Show)

type ConceptDefs = [ConceptDef]
instance Eq ConceptDef where
  cd == cd' = cdnm cd == cdnm cd'

instance Identified ConceptDef where
  name cd = cdnm cd
   

---------------------------------------------------------------- 
-- | The basic Concept.
data Concept
   = C   { cptnm :: a          -- ^The name of this Concept
         , cptgE :: GenR a     -- ^The generalization relation
         , cptos :: Maybe [a]  -- ^Atoms
         }

type GenR a = Concept->Concept->Bool  
          
instance Eq Concept where
 C a _ _ == C b _ _ = a==b

instance Show Concept where
 showsPrec _ c = showString (name c)
 
instance Identified Concept where
 name = cptnm

instance Ord Concept where
 a@(C _ gE _) <= b = a `gE` b


---------------------------------------------------------------- 
-- | The parse-time construction for a relation between concepts
-- TODO (WAAROM?) @Bas & Stef: Moet decprps_calc hier wel in staan? Dat is geen parse-time ding, dus naar Morphism?
data Declaration 
   = Sgn { decnm   :: String  -- ^ the name of the declaration
         , desrc   :: Concept -- ^ the source concept of the declaration
         , detrg   :: Concept -- ^ the target concept of the declaration
           --multiplicities returns decprps_calc so if you only need the user defined properties do not use multiplicities but decprps
         , decprps :: Props   -- ^ the user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
         , decprps_calc :: Props   -- ^ the calculated and user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
         , decprL  :: String  -- ^ three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
         , decprM  :: String  -- ^    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
         , decprR  :: String
         , decpopu :: Pairs   -- ^ the list of tuples, of which the relation consists.
         , decfpos :: FilePos -- ^ the position in the ADL source file where this declaration is declared.
         , decid   :: Int     -- ^ a unique number that can be used to identify the relation
         , deciss  :: Bool    -- ^ if true, this is a signal relation; otherwise it is an ordinary relation.
         , decusr  :: Bool    -- ^ if true, this relation is declared in the ADL-script; otherwise it was generated by ADL.
         , decpat  :: String  -- ^ the pattern where this declaration has been declared.
         , decplug :: Bool    -- ^ if true, this relation may not be stored in or retrieved from the standard database (it should be gotten from a Plug of some sort instead)
         }
    | Isn 
         { degen :: Concept  -- ^ The generic concept
         , despc :: Concept  -- ^ The specific concept
         }
    | Iscompl 
         { degen :: Concept
         , despc :: Concept
         }
    | Vs 
         { degen :: Concept
         , despc :: Concept
         }

type Declarations = [Declaration]
instance Eq Declaration where
   d == d' = name d==name d' && source d==source d' && target d==target d'

instance Show Declaration where
 showsPrec _ d =
   case d of
    Sgn{}   -> showString 
                (intercalate " " [decnm d,"::",name (desrc d),"*",name (detrg d)
                                 ,show (decprps_calc d)
                                 ,"PRAGMA",show (decprL d)
                                          ,show (decprM d)
                                          ,show (decprR d)])
    _       -> error ("TODO")

instance Identified Declaration where
  name d@Sgn{}   = decnm d
  name Isn{}     = "I"
  name Iscompl{} = "-I"
  name Vs{}      = "V"

instance Association Declaration where
   source d = case d of
                Sgn {}    -> desrc d
                Isn {}    -> despc d
                Iscompl{} -> despc d
                Vs {}     -> degen d
   target d = case d of
                Sgn {}    -> detrg d
                Isn {}    -> degen d
                Iscompl{} -> degen d
                Vs {}     -> despc d


---------------------------------------------------------------- 
-- ^A relation for during parsing
data Morphism                     
   = MpRel { mphrel :: Relation    -- ^The actual relation
           , mphdcl :: Declaration -- ^Where the morphism was declared
           , mphpos :: FilePos     -- ^File and position of this relation
           , mphats :: [Concept]   -- ^The specified concepts
           } deriving Show

instance Eq Morphism where
  m == m' = mphrel m == mphrel m'
 
instance Ord Morphism where
  a <= b = source a <= source b && target a <= target b

instance Identified Morphism where
  name m = name (mphdcl m)

instance Association Morphism where  
  source m = source (mphdcl m)
  target m = target (mphdcl m)

---------------------------------------------------------------- 
-- | The basic Relation
data Relation 
   = Rel { relnm  :: String -- ^The name of the relation
         , relsrc :: Concept     -- ^Source concept
         , reltrg :: Concept     -- ^Target concept
         , relflp :: Bool  -- ^Whether this relation is flipped
         }
    | I  { reltyp :: Concept }   -- ^identity relation
    | V  { reltyp :: Concept }   -- ^full relation
      deriving Eq

instance Identified (Relation ) where
  name r = case r of
             Rel{} -> relnm r
             I{}   -> "I"
             V{}   -> "V"
               
instance Association Relation where
  source r = case r of
       Rel{} -> relsrc r
       _     -> reltyp r
  target r = case r of
       Rel{} -> reltrg r
       _     -> reltyp r
    
instance Show Relation where
  showsPrec _ r = showString (name r++
     (case r of
      Rel{relflp = False} -> showSign [target r,source r]++"~"
      _                   -> showSign [source r,target r]
     )                        )
                               
---------------------------------------------------------------- 
-- | The pairs of related atoms in source and target of a relation.
data Population  
   = Popu { popr  :: Relation 
          , popps :: Pairs
          }
type Populations  = [Population ]
instance Association Population where
  source pop = source (popr pop)
  target pop = target (popr pop)

 

---------------------------------------------------------------- 
-- | The basic Expression
data Expression
   = Finter     [Expression]   -- ^Intersect of expressions           /\  (Fi)
   | Funion     [Expression]   -- ^Union of expressions               \/  (Fu)
   | Complement  Expression    -- ^Complement of an expression        -   (Cp)
   | Fcomp      [Expression]   -- ^Composition of expressions         ;   (Fc)
   | FrAdd      [Expression]   -- ^relative addition of expressions   !   (Fd)
   | Brackets    Expression    -- ^Bracketed expression              ( ... ) 
   | Mph         Relation      -- ^The basic relation                      
instance Show Expression where

data UnOp
  = K0 -- ^ Reflexive and transitive closure *
  | K1 -- ^ Transitive closure +
  | Cp -- ^ Complement -
  | Co -- ^ Converse ~
    deriving (Show,Eq)

data MulOp
  = Fc -- ^ composition ;
  | Fd -- ^ relative addition !
  | Fi -- ^ intersection
  | Fu -- ^ union \/
  | Ri -- ^ Rule implication |-  => (r |- s |- t <=> (-r\/s) /\ (-s\/t) )
  | Re -- ^ Rule equivalence =   => (r = s = t   <=> (r |- s |- t) /\ (t |- s |- r)
    deriving (Show,Eq)

data Op = Op1 UnOp | Opn MulOp
  
  
---------------------------------------------------------------- 
data RuleType = Implication | Equivalence | Truth {- | Generalization (obsolete?)-} deriving (Eq,Show)
data Rule
   = Ru { rrsrt    :: RuleType          -- ^ One of the following:
                                        --    | Implication if this is an implication;
                                        --    | Equivalence if this is an equivalence;
                                        --    | Truth  if this is an ALWAYS expression.
        , rrant    :: Expression        -- ^ Antecedent
        , rrfps    :: FilePos           -- ^ Position in the ADL file
        , rrcon    :: Expression        -- ^ Consequent
        , rrxpl    :: [AutoExplain]     -- ^ ADL-generated explanations (for all known languages)
        , rrtyp    :: (Concept,Concept) -- ^ Sign of this rule
        , rrdcl    :: Maybe (Prop,Declaration)  -- ^ The property, if this rule originates from a property on a Declaration
        , runum    :: Int               -- ^ Rule number
        , r_pat    :: String            -- ^ Name of pattern in which it was defined.
        , r_usr    :: Bool              -- ^ True if this rule was specified explicitly as a rule in the ADL-script; False if it follows implicitly from the ADL-script and generated by a computer
        , r_sgl    :: Bool              -- ^ True if this is a signal; False if it is an ALWAYS rule
        , srrel    :: Declaration       -- ^ the signal relation
        } 
type Rules = [Rule]

instance Ord Rule where
  compare r r' = compare (runum r) (runum r')

instance Show Rule where
  showsPrec _ x =
     case x of
        Ru{rrsrt = Implication   } -> showString$ show(rrant x) ++ " |- " ++ (show$rrcon x)
        Ru{rrsrt = Equivalence   } -> showString$ show(rrant x) ++ " = "  ++ (show$rrcon x)
        Ru{rrsrt = Truth         } -> showString$ show(rrcon x)
        
instance Identified Rule where
  name r = if null (name (srrel r)) then "Rule"++show (runum r) else name (srrel r)
    
instance Association Rule where
  source r  = fst (rrtyp r)
  target r  = snd (rrtyp r)


---------------------------------------------------------------- 
data KeyDef 
   = Kd { kdpos :: FilePos      -- ^ position of this definition in the text of the ADL source file (filename, line number and column number).
        , kdlbl :: String       -- ^ the name (or label) of this Key. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
        , kdcpt :: Concept      -- ^ this expression describes the instances of this object, related to their context
        , kdats :: ObjectDefs   -- ^ the constituent attributes (i.e. name/expression pairs) of this key.
        } 
type KeyDefs = [KeyDef]

instance Identified KeyDef where
  name kd = kdlbl kd

---------------------------------------------------------------- 
data Service 
   = Serv { svName   :: String
          , svParams :: [Morphism]
          , svObj    :: ObjectDef
          , svPos    :: FilePos
          }

---------------------------------------------------------------- 
data ObjectDef 
   = Obj { objnm   :: String         -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
         , objpos  :: FilePos        -- ^ position of this definition in the text of the ADL source file (filename, line number and column number)
         , objctx  :: Expression     -- ^ this expression describes the instances of this object, related to their context. 
         , objats  :: ObjectDefs     -- ^ the attributes, which are object definitions themselves.
         , objstrs :: [[String]]     -- ^ directives that specify the interface.
         } 
type ObjectDefs = [ObjectDef]

instance Identified ObjectDef where
  name obj = objnm obj

---------------------------------------------------------------- 
-- | PExplanation is a parse-time constructor. It contains the name of the object it explains.
-- It is a pre-explanation in the sense that it contains a reference to something that is not yet built by the compiler.
--                       Constructor      name          RefID  Explanation
data PExplanation 
   = PExpl {pexObj  :: PExplObj
           ,pexLang :: Lang
           ,pexRefID:: String
           ,pexExpl :: String
           }
type PExplanations = [PExplanation]

instance Identified PExplanation where
  name pe = name (pexObj pe)

data PExplObj 
   = PExplConceptDef String
   | PExplDeclaration Morphism
   | PExplRule String
   | PExplKeyDef String
   | PExplObjectDef String
   | PExplPattern String
   | PExplContext String

instance Identified PExplObj where
  name pe = case pe of 
     PExplConceptDef str -> str
     PExplDeclaration rel -> name rel
     PExplRule str -> str
     PExplKeyDef str -> str
     PExplObjectDef str -> str
     PExplPattern str -> str
     PExplContext str -> str
        
---------------------------------------------------------------- 
-- | The languages that currently are available in Ampersand
data Lang = Dutch | English deriving (Show, Eq)


---------------------------------------------------------------- 
-- | A RoleService rs means that a role called 'rsRole rs' may use the ObjectDef called 'rsServ rs'
data RoleService
   = RS { rsRole :: [String]       -- ^ name of a role
        , rsServ :: [String]       -- ^ name of an ObjectDef
        , rsPos  :: FilePos        -- ^ position in the Ampersand script
        } deriving (Eq, Show)      -- ^ just for debugging

-- | A RoleRelation rs means that a role called 'rsRole rs' may use the ObjectDef called 'rsServ rs'
data RoleRelation
   = RR { rrRole :: [String]       -- ^ name of a role
        , rrRel  :: [Morphism]     -- ^ name of a Relation
        , rrPos  :: FilePos        -- ^ position in the Ampersand script
        } deriving (Eq, Show)      -- ^ just for debugging

---------------------------------------------------------------- 
data Gen
   = G { genfp  :: FilePos         -- ^ the position of the GEN-rule
       , gengen :: Concept         -- ^ generic concept
       , genspc :: Concept         -- ^ specific concept
       , genpat :: String          -- ^ pattern of declaration
       }
type Gens = [Gen]

instance Eq Gen where
   g == g' = gengen g == gengen g' &&
             genspc g == genspc g'

instance Show Gen where
-- This show is used in error messages. It should therefore not display the term's type
  showsPrec _ g = showString ("GEN "++show (genspc g)++" ISA "++show (gengen g))
   

---------------------------------------------------------------- 
data AutoExplain = Because Lang ExplainContent deriving (Eq,Show)
 
type ExplainContent = [Block]











-------------------- *Auxilliary functions* ---------------------
showSign :: Identified a => [a] -> String
showSign cs = "["++(intercalate "*".rd.map name) cs++"]"

           