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
   Concept(..),
   Relation(..),
   Declaration(..),
   Expression(..),
   Morphism(..),
   Prop(..),Props,
   Pairs,flipPair,
   Population(..)    
   )where
import Data.List  (intercalate)
import Collection (rd) -- TODO: must be removed from here. 
import Adl.Pair (Pairs,flipPair)-- TODO: Must be fully moved to here
import Adl.Prop        -- TODO: Must be fully moved to here
import Adl.FilePos     -- TODO: Must be fully moved to here
import Classification  (Classification)

------------------------- *Classes* ------------------------
-- | Naming things makes referencing to it easy (in an informal way, of course)
class Identified a where
  name   :: a->String

-- | Association. 
--  This class resides over here, because it is required in some instances of Show.
-- 'source' and 'target' are the minimal functions to define for an instance.
class (Eq c,Identified c) => Association a c | a -> c  where
    source, target :: a -> c
    sign           :: a -> (c,c)
    sign x = (source x,target x) 
    swap           :: a -> (c,c)
    swap x = (target x,source x)
    homogeneous :: a -> Bool
    homogeneous s = source s == target s


-------------------- *Data contstructors*-------------------
-- | Architecture of ADL consists of a set of contexts
data Architecture = Arch { archContexts :: [Context]}

---------------------------------------------------------------- 
data Context
-- TODO: ctxisa, ctxwrld en ctxrs zijn na parsen leeg. Ze horen waarschijnlijk niet in Context thuis, maar in Fspec
   = Ctx { ctxnm    :: String                    -- ^ The name of this context
         , ctxon    :: [String]                  -- ^ The list of extends (= context names of contexts) whose rules are imported
--         , ctxisa   :: Inheritance Concept       -- ^ A data structure containing the generalization structure of concepts
--         , ctxwrld  :: [Classification Context]  -- ^ A tree, being the transitive closure of the 'extends' (see formal definition) relation.
         , ctxpats  :: Patterns                  -- ^ The patterns defined in this context
--         , ctxrs    :: Rules                     -- ^ All user defined rules in this context, but outside patterns
         , ctxds    :: Declarations              -- ^ The declarations defined in this context, outside the scope of patterns
         , ctxcs    :: ConceptDefs               -- ^ The concept definitions defined in this context, outside the scope of patterns
         , ctxks    :: KeyDefs                   -- ^ The key definitions defined in this context, outside the scope of patterns
         , ctxos    :: ObjectDefs                -- ^ The attributes defined in this context, outside the scope of patterns
         , ctxps    :: PExplanations             -- ^ The pre-explanations defined in this context, outside the scope of patterns
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


-- | The basic Concept.
data Concept
   = C   { cptnm :: String          -- ^The name of this Concept
         , cptgE :: GenR            -- ^The generalization relation
         , cptos :: Maybe [String]  -- ^Atoms
         }

type GenR  = Concept->Concept->Bool  
          
instance Eq Concept where
 C a _ _ == C b _ _ = a==b

instance Show Concept where
 showsPrec _ c = showString (name c)
 
instance Identified Concept where
 name = cptnm

instance Ord Concept where
 a@(C _ gE _) <= b = a `gE` b


---------------------------------------------------------------- 
-- | The basic Relation
data Relation c
   = Rel { relnm :: String -- ^The name of the relation
         , relsrc :: c     -- ^Source concept
         , reltrg :: c     -- ^Target concept
         , relflp :: Bool  -- ^Whether this relation is flipped
         }
    | I  { reltyp :: c }   -- ^identity relation
    | V  { reltyp :: c }   -- ^full relation
      deriving Eq

instance Identified (Relation c) where
  name r = case r of
             Rel{} -> relnm r
             I{}   -> "I"
             V{}   -> "V"
               
instance  (Identified c, Eq c) => Association (Relation c) c where
  source r = case r of
       Rel{} -> relsrc r
       _     -> reltyp r
  target r = case r of
       Rel{} -> reltrg r
       _     -> reltyp r
    
instance (Identified c, Eq c) => Show (Relation c) where
  showsPrec _ r = showString (name r++
     (case r of
      Rel{relflp = False} -> showSign [target r,source r]++"~"
      _                   -> showSign [source r,target r]
     )                        )
                               
---------------------------------------------------------------- 
data Population r 
   = Popu { popr  :: Relation r
          , popps :: Pairs
          }

instance (Identified r, Eq r) => Association (Population r) r where
  source pop = source (popr pop)
  target pop = target (popr pop)

 

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

instance Association Declaration Concept where
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
data Morphism c                    
   = MpRel { mphrel :: Relation c  -- ^The actual relation
           , mphdcl :: Declaration -- ^Where the morphism was declared
           , mphpos :: FilePos     -- ^File and position of this relation
           , mphats :: [c]         -- ^The specified concepts
           } deriving Show

instance Eq c => Eq (Morphism c) where
  m == m' = mphrel m == mphrel m'
 
instance (Identified c, Eq c, Ord c) => Ord (Morphism c) where
  a <= b = source a <= source b && target a <= target b

instance Identified (Morphism c) where
  name m = name (mphdcl m)

instance (Identified c, Eq c) => Association (Morphism c) Concept where  
  source m = source (mphdcl m)
  target m = target (mphdcl m)

---------------------------------------------------------------- 
-- | The basic Expression
data Expression r
   = Fi [(Expression r)] -- ^Intersect of expressions
   | Fu [(Expression r)] -- ^Union of expressions
   | Cp (Expression r)   -- ^Complement of an expression
   | Fj [(Expression r)] -- ^Join of expressions
   | Fd [(Expression r)] -- ^Dagger of expressions
   | Mph r               -- ^The basic relation

---------------------------------------------------------------- 

---------------------------------------------------------------- 

---------------------------------------------------------------- 

---------------------------------------------------------------- 











-------------------- *Auxilliary functions* ---------------------
showSign :: Identified a => [a] -> String
showSign cs = "["++(intercalate "*".rd.map name) cs++"]"
           