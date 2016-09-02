{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImplicitParams #-}{-# OPTIONS_GHC -Wall -Werror #-}
module Ampersand.Core.AbstractSyntaxTree (
   A_Context(..)
 , Typology(..)
 , Meta(..)
 , Pattern(..)
 , PairView(..)
 , PairViewSegment(..)
 , Rule(..)
 , RuleOrigin(..)
 , Declaration(..)
 , IdentityDef(..)
 , IdentitySegment(..)
 , ViewDef(..)
 , ViewSegment(..)
 , ViewSegmentPayLoad(..)
 , A_Gen(..)
 , Interface(..)
 , getInterfaceByName
 , SubInterface(..)
 , ObjectDef(..)
 , Object(..)
 , Cruds(..)
 , Default(..)
 , objAts
 , Purpose(..)
 , ExplObj(..)
 , Expression(..)
 , getExpressionRelation
 , A_Concept(..)
 , A_Markup(..)
 , AMeaning(..)
 , A_RoleRule(..)
 , A_RoleRelation(..)
 , Representation(..), TType(..)
 , unsafePAtomVal2AtomValue, safePSingleton2AAtomVal
 , Signature(..)
 , Population(..)
 , Association(..)
 , PAclause(..), Event(..), ECArule(..), InsDel(..), Conjunct(..), DnfClause(..)
 , AAtomPair(..), AAtomValue(..), mkAtomPair, PAtomValue(..)
 , ContextInfo(..)
 , showValADL,showValPHP,showValSQL
 , showSign
 , aMarkup2String
 , module Ampersand.Core.ParseTree  -- export all used constructors of the parsetree, because they have actually become part of the Abstract Syntax Tree.
 , (.==.), (.|-.), (./\.), (.\/.), (.-.), (./.), (.\.), (.<>.), (.:.), (.!.), (.*.)
 , makeConcept
 , aavstr
 ) where
import Ampersand.Basics
import Ampersand.Core.ParseTree ( MetaObj(..),Meta(..),Role(..),ConceptDef,Origin(..),Traced(..), ViewHtmlTemplate(..){-, ViewTextTemplate(..)-}
                                                , PairView(..),PairViewSegment(..),Prop(..),Lang, PandocFormat, P_Markup(..), PMeaning(..)
                                                , SrcOrTgt(..), isSrc , Representation(..), TType(..), PAtomValue(..), PSingleton, makePSingleton
                                                )
import Ampersand.Misc
import Text.Pandoc hiding (Meta)
import Data.Function
import Data.Typeable
import GHC.Generics (Generic)
import Data.Data
import Data.Char (toUpper,toLower)
import Data.List (nub,intercalate)
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Default
import GHC.Stack
import Data.Hashable
import Data.Text (Text,unpack,pack)
import qualified Data.Time.Format as DTF (formatTime,parseTimeOrError,defaultTimeLocale,iso8601DateFormat)

data A_Context
   = ACtx{ ctxnm :: String           -- ^ The name of this context
         , ctxpos :: [Origin]        -- ^ The origin of the context. A context can be a merge of a file including other files c.q. a list of Origin.
         , ctxlang :: Lang           -- ^ The default language used in this context.
         , ctxmarkup :: PandocFormat -- ^ The default markup format for free text in this context (currently: LaTeX, ...)
         , ctxthms :: [String]       -- ^ Names of patterns/processes to be printed in the functional specification. (For partial documents.)
         , ctxpats :: [Pattern]      -- ^ The patterns defined in this context
         , ctxrs :: [Rule]           -- ^ All user defined rules in this context, but outside patterns and outside processes
         , ctxds :: [Declaration]    -- ^ The relations that are declared in this context, outside the scope of patterns
         , ctxpopus :: [Population]  -- ^ The user defined populations of relations defined in this context, including those from patterns and processes
         , ctxcds :: [ConceptDef]    -- ^ The concept definitions defined in this context, including those from patterns and processes
         , ctxks :: [IdentityDef]    -- ^ The identity definitions defined in this context, outside the scope of patterns
         , ctxrrules :: [A_RoleRule]
         , ctxRRels :: [A_RoleRelation] -- ^ The assignment of roles to Relations (which role mayEdit what relations)
         , ctxreprs :: A_Concept -> TType
         , ctxvs :: [ViewDef]        -- ^ The view definitions defined in this context, outside the scope of patterns
         , ctxgs :: [A_Gen]          -- ^ The specialization statements defined in this context, outside the scope of patterns
         , ctxgenconcs :: [[A_Concept]] -- ^ A partitioning of all concepts: the union of all these concepts contains all atoms, and the concept-lists are mutually distinct in terms of atoms in one of the mentioned concepts
         , ctxifcs :: [Interface]    -- ^ The interfaces defined in this context
         , ctxps :: [Purpose]        -- ^ The purposes of objects defined in this context, outside the scope of patterns and processes
         , ctxsql :: [ObjectDef]     -- ^ user defined sqlplugs, taken from the Ampersand script
         , ctxphp :: [ObjectDef]     -- ^ user defined phpplugs, taken from the Ampersand script
         , ctxmetas :: [Meta]        -- ^ used for Pandoc authors (and possibly other things)
         , ctxInfo :: ContextInfo
         } deriving (Typeable)              --deriving (Show) -- voor debugging
instance Show A_Context where
  showsPrec _ c = showString (ctxnm c)
instance Eq A_Context where
  c1 == c2  =  name c1 == name c2
instance Unique A_Context where
  showUnique = name
instance Named A_Context where
  name  = ctxnm

data A_RoleRelation
   = RR { rrRoles :: [Role]     -- ^ name of a role
        , rrRels :: [Declaration]   -- ^ name of a Relation
        , rrPos :: Origin       -- ^ position in the Ampersand script
        } deriving Show
instance Traced A_RoleRelation where
   origin = rrPos

data Pattern
   = A_Pat { ptnm :: String         -- ^ Name of this pattern
           , ptpos :: Origin        -- ^ the position in the file in which this pattern was declared.
           , ptend :: Origin        -- ^ the end position in the file, elements with a position between pos and end are elements of this pattern.
           , ptrls :: [Rule]        -- ^ The user defined rules in this pattern
           , ptgns :: [A_Gen]       -- ^ The generalizations defined in this pattern
           , ptdcs :: [Declaration] -- ^ The relations that are declared in this pattern
           , ptups :: [Population]  -- ^ The user defined populations in this pattern
           , ptids :: [IdentityDef] -- ^ The identity definitions defined in this pattern
           , ptvds :: [ViewDef]     -- ^ The view definitions defined in this pattern
           , ptxps :: [Purpose]     -- ^ The purposes of elements defined in this pattern
           }   deriving (Typeable)    -- Show for debugging purposes
instance Eq Pattern where
  p==p' = ptnm p==ptnm p'
instance Unique Pattern where
  showUnique = name

instance Named Pattern where
 name = ptnm
instance Traced Pattern where
 origin = ptpos


data A_RoleRule = A_RoleRule { arRoles :: [Role]
                             , arRules ::  [String] -- the names of the rules
                             , arPos ::   Origin
                             } deriving (Show)
data A_Markup =
    A_Markup { amLang :: Lang -- No Maybe here!  In the A-structure, it will be defined by the default if the P-structure does not define it. In the P-structure, the language is optional.
             , amPandoc :: [Block]
             } deriving (Show, Eq, Prelude.Ord, Typeable, Data)

data RuleOrigin = UserDefined     -- This rule was specified explicitly as a rule in the Ampersand script
                | Multiplicity    -- This rule follows implicitly from the Ampersand script (Because of a property) and generated by a computer
                | Identity        -- This rule follows implicitly from the Ampersand script (Because of a identity) and generated by a computer
                deriving (Show, Eq)
data Rule =
     Ru { rrnm ::     String                      -- ^ Name of this rule
        , rrexp ::    Expression                  -- ^ The rule expression
        , rrfps ::    Origin                      -- ^ Position in the Ampersand file
        , rrmean ::   AMeaning                    -- ^ Ampersand generated meaning (for all known languages)
        , rrmsg ::    [A_Markup]                  -- ^ User-specified violation messages, possibly more than one, for multiple languages.
        , rrviol ::   Maybe (PairView Expression) -- ^ Custom presentation for violations, currently only in a single language
        , rrtyp ::    Signature                   -- ^ Allocated signature
        , rrdcl ::    Maybe (Prop,Declaration)    -- ^ The property, if this rule originates from a property on a Declaration
        , r_env ::    String                      -- ^ Name of pattern in which it was defined.
        , r_usr ::    RuleOrigin                  -- ^ Where does this rule come from?
        , isSignal :: Bool                        -- ^ True if this is a signal; False if it is an invariant
        } deriving Typeable
instance Eq Rule where
  r==r' = rrnm r==rrnm r'
instance Unique Rule where
  showUnique = rrnm
instance Prelude.Ord Rule where
  compare = Prelude.compare `on` rrnm
instance Show Rule where
  showsPrec _ x
   = showString $ "RULE "++ (if null (name x) then "" else name x++": ")++ show (rrexp x)
instance Traced Rule where
  origin = rrfps
instance Named Rule where
  name   = rrnm
instance Association Rule where
  sign   = rrtyp

data Conjunct = Cjct { rc_id ::         String -- string that identifies this conjunct ('id' rather than 'name', because
                                               -- this is an internal id that has no counterpart at the ADL level)
                     , rc_orgRules ::   [Rule] -- All rules this conjunct originates from
                     , rc_conjunct ::   Expression
                     , rc_dnfClauses :: [DnfClause]
                     } deriving (Show,Typeable)

data DnfClause = Dnf { antcs :: [Expression]
                     , conss :: [Expression]
                     }  deriving (Show, Eq) -- Show is for debugging purposes only.

{- The intended semantics of |Dnf ns ps| is the disjunction |foldr1 ( .\/. ) (map notCpl ns ++ ps)|.
   The list |ns| and |ps| are not guaranteed to be sorted or duplicate-free.
-}

instance Eq Conjunct where
  rc==rc' = rc_id rc==rc_id rc'
instance Unique Conjunct where
  showUnique = rc_id
instance Prelude.Ord Conjunct where
  compare = Prelude.compare `on` rc_id

data Declaration =
  Sgn { decnm :: Text              -- ^ the name of the declaration
      , decsgn :: Signature          -- ^ the source and target concepts of the declaration
       --properties returns decprps_calc, when it has been calculated. So if you only need the user defined properties do not use 'properties' but 'decprps'.
      , decprps :: [Prop]            -- ^ the user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
      , decprps_calc :: Maybe [Prop] -- ^ the calculated and user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx, Irf). Note that calculated properties are made by adl2fspec, so in the A-structure decprps and decprps_calc yield exactly the same answer.
      , decprL :: String             -- ^ three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
      , decprM :: String             -- ^    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
      , decprR :: String
      , decMean :: AMeaning          -- ^ the meaning of a declaration, for each language supported by Ampersand.
      , decfpos :: Origin            -- ^ the position in the Ampersand source file where this declaration is declared. Not all decalartions come from the ampersand souce file.
      , decusr ::  Bool              -- ^ if true, this relation is declared by an author in the Ampersand script; otherwise it was generated by Ampersand.
      , decpat ::  String            -- ^ the pattern where this declaration has been declared.
      , decplug :: Bool              -- ^ if true, this relation may not be stored in or retrieved from the standard database (it should be gotten from a Plug of some sort instead)
      , dech :: Int
      } |
  Isn
      { detyp :: A_Concept       -- ^ The type
      } |
  Vs
      { decsgn :: Signature
      } deriving (Typeable, Data)

instance Eq Declaration where
  d@Sgn{}     == d'@Sgn{}     = dech d == dech d' && decnm d == decnm d' && decsgn d==decsgn d'
  d@Isn{}     == d'@Isn{}     = detyp d==detyp d'
  d@Vs{}      == d'@Vs{}      = decsgn d==decsgn d'
  _           == _            = False
instance Ord Declaration where
  compare a b =
    case a of
      Sgn{} -> case b of
                Sgn{} -> if name a == name b
                         then Prelude.compare (sign a) (sign b)
                         else Prelude.compare (name a) (name b)
                Isn{} -> GT
                Vs{}  -> GT
      Isn{} -> case b of
                Sgn{} -> LT
                Isn{} -> Prelude.compare (sign a) (sign b)
                Vs{}  -> GT
      Vs{}  -> case b of
                Sgn{} -> LT
                Isn{} -> LT
                Vs{}  -> Prelude.compare (sign a) (sign b)
instance Unique Declaration where
  showUnique d =
    case d of
      Sgn{} -> name d++uniqueShow False (decsgn d)
      Isn{} -> "I["++uniqueShow False (detyp d)++"]"
      Vs{}  -> "V"++uniqueShow False (decsgn d)
instance Hashable Declaration where
   hashWithSalt s (Sgn{dech = v}) = s `hashWithSalt` v
   hashWithSalt s (Isn{detyp = v}) = hashWithSalt s v
   hashWithSalt s (Vs{decsgn = v})  = hashWithSalt s v
instance Show Declaration where  -- For debugging purposes only (and fatal messages)
  showsPrec _ decl@Sgn{}
   = showString (case decl of
                  Sgn{} -> name decl++showSign (sign decl)
                  Isn{} -> "I["++show (detyp decl)++"]" -- Isn{} is of type Declaration and it is implicitly defined
                  Vs{}  -> "V"++show (decsgn decl) )

  showsPrec _ d@Isn{}     = showString $ "Isn{detyp="++show(detyp d)++"}"
  showsPrec _ d@Vs{}      = showString $ "V"++showSign(decsgn d)

aMarkup2String :: PandocFormat -> A_Markup -> String
aMarkup2String fmt a = blocks2String fmt False (amPandoc a)

data AMeaning = AMeaning { ameaMrk ::[A_Markup]} deriving (Show, Eq, Prelude.Ord, Typeable, Data)

instance Named Declaration where
  name d@Sgn{}   = unpack (decnm d)
  name Isn{}     = "I"
  name Vs{}      = "V"
instance Association Declaration where
  sign d = case d of
              Sgn {}    -> decsgn d
              Isn {}    -> Sign (detyp d) (detyp d)
              Vs {}     -> decsgn d
instance Traced Declaration where
  origin d = case d of
              Sgn{}     -> decfpos d
              _         -> OriginUnknown

data IdentityDef = Id { idPos :: Origin        -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number).
                      , idLbl :: String        -- ^ the name (or label) of this Identity. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface. It is not an empty string.
                      , idCpt :: A_Concept     -- ^ this expression describes the instances of this object, related to their context
                      , identityAts :: [IdentitySegment]  -- ^ the constituent attributes (i.e. name/expression pairs) of this identity.
                      } deriving (Eq,Show)
instance Named IdentityDef where
  name = idLbl
instance Traced IdentityDef where
  origin = idPos

data IdentitySegment = IdentityExp ObjectDef deriving (Eq, Show)  -- TODO: refactor to a list of terms

data ViewDef = Vd { vdpos :: Origin          -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number).
                  , vdlbl :: String          -- ^ the name (or label) of this View. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface. It is not an empty string.
                  , vdcpt :: A_Concept       -- ^ the concept for which this view is applicable
                  , vdIsDefault :: Bool      -- ^ whether or not this is the default view for the concept
                  , vdhtml :: Maybe ViewHtmlTemplate -- ^ the html template for this view (not required since we may have other kinds of views as well in the future)
--                  , vdtext :: Maybe ViewText -- Future extension
                  , vdats :: [ViewSegment]   -- ^ the constituent attributes (i.e. name/expression pairs) of this view.
                  } deriving (Show)
instance Named ViewDef where
  name = vdlbl
instance Traced ViewDef where
  origin = vdpos
data ViewSegment = ViewSegment
     { vsmpos :: Origin
     , vsmlabel :: Maybe String
     , vsmSeqNr :: Integer
     , vsmLoad  :: ViewSegmentPayLoad
     } deriving Show
instance Traced ViewSegment where
  origin = vsmpos
data ViewSegmentPayLoad
                 = ViewExp { vsgmExpr :: Expression
                           }
                 | ViewText{ vsgmTxt  :: String
                           }deriving (Eq, Show)


-- | data structure A_Gen contains the CLASSIFY statements from an Ampersand script
--   CLASSIFY Employee ISA Person   translates to Isa (C "Person") (C "Employee")
--   CLASSIFY Workingstudent IS Employee/\Student   translates to IsE orig (C "Workingstudent") [C "Employee",C "Student"]
data A_Gen = Isa { genpos :: Origin
                 , genspc :: A_Concept      -- ^ specific concept
                 , gengen :: A_Concept      -- ^ generic concept
                 }
           | IsE { genpos :: Origin
                 , genspc :: A_Concept      -- ^ specific concept
                 , genrhs :: [A_Concept]    -- ^ concepts of which the conjunction is equivalent to the specific concept
                 } deriving (Typeable, Eq)
instance Traced A_Gen where
  origin = genpos
instance Unique A_Gen where
  showUnique a =
    case a of
      Isa{} -> uniqueShow False (genspc a)++" ISA "++uniqueShow False (gengen a)
      IsE{} -> uniqueShow False (genspc a)++" IS "++intercalate " /\\ " (map (uniqueShow False) (genrhs a))
instance Show A_Gen where
  -- This show is used in error messages. It should therefore not display the term's type
  showsPrec _ g =
    case g of
     Isa{} -> showString ("CLASSIFY "++show (genspc g)++" ISA "++show (gengen g))
     IsE{} -> showString ("CLASSIFY "++show (genspc g)++" IS "++intercalate " /\\ " (map show (genrhs g)))

data Interface = Ifc { ifcRoles ::    [Role]        -- all roles for which an interface is available (empty means: available for all roles)
                     , ifcObj ::      ObjectDef     -- NOTE: this top-level ObjectDef is contains the interface itself (ie. name and expression)
                     , ifcEcas ::     [ECArule]     -- All ECArules that are needed to perform computations for maintaining rules
                     , ifcControls :: [Conjunct]    -- All conjuncts that must be evaluated after a transaction
                     , ifcPos ::      Origin        -- The position in the file (filename, line- and column number)
                     , ifcPrp ::      String        -- The purpose of the interface
                     } deriving Show

instance Eq Interface where
  s==s' = name s==name s'
instance Named Interface where
  name = name . ifcObj
instance Traced Interface where
  origin = ifcPos

-- Utility function for looking up interface refs
getInterfaceByName :: [Interface] -> String -> Interface
getInterfaceByName interfaces' nm = case [ ifc | ifc <- interfaces', name ifc == nm ] of
                                []    -> fatal 327 $ "getInterface by name: no interfaces named "++show nm
                                [ifc] -> ifc
                                _     -> fatal 330 $ "getInterface by name: multiple interfaces named "++show nm

objAts :: ObjectDef -> [ObjectDef]
objAts obj
  = case objmsub obj of
     Nothing       -> []
     Just InterfaceRef{} -> []
     Just b@Box{}    -> siObjs b

class Object a where
 concept ::   a -> A_Concept        -- the type of the object
 fields ::    a -> [ObjectDef]   -- the objects defined within the object
 contextOf :: a -> Expression     -- the context expression

instance Object ObjectDef where
 concept obj = target (objctx obj)
 fields      = objAts
 contextOf   = objctx

data ObjectDef = Obj { objnm ::    String         -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
                     , objpos ::   Origin         -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number)
                     , objctx ::   Expression     -- ^ this expression describes the instances of this object, related to their context.
                     , objcrud ::  Cruds -- ^ CRUD as defined by the user 
                     , objmView :: Maybe String   -- ^ The view that should be used for this object
                     , objmsub ::  Maybe SubInterface    -- ^ the fields, which are object definitions themselves.
                     } deriving (Eq, Show)        -- just for debugging (zie ook instance Show ObjectDef)
instance Named ObjectDef where
  name   = objnm
instance Traced ObjectDef where
  origin = objpos
data Cruds = Cruds { crudOrig :: Origin
                   , crudC :: Bool
                   , crudR :: Bool
                   , crudU :: Bool
                   , crudD :: Bool
                   } deriving (Eq, Show)
data SubInterface = Box { siConcept :: A_Concept
                        , siMClass  :: Maybe String
                        , siObjs    :: [ObjectDef] 
                        }
                  | InterfaceRef 
                        { siIsLink :: Bool
                        , siIfcId  :: String  --id of the interface that is referenced to
                        , siCruds  :: Cruds
                        } deriving (Eq, Show)

data InsDel   = Ins | Del
                 deriving (Show,Eq,Ord)
data ECArule= ECA { ecaTriggr :: Event       -- The event on which this rule is activated
                  , ecaDelta ::  Declaration -- The delta to be inserted or deleted from this rule. It actually serves very much like a formal parameter.
                  , ecaAction :: PAclause    -- The action to be taken when triggered.
                  , ecaNum ::    Int         -- A unique number that identifies the ECArule within its scope.
                  }

instance Show ECArule where
  showsPrec _ r = showString ("ON "++show (ecaTriggr r)++" "++show (ecaDelta r)++" do something.")

instance Eq (ECArule) where
   e==e' = ecaNum e==ecaNum e'

data Event = On { eSrt :: InsDel
                , eDcl :: Declaration
                } deriving (Show,Eq)

data PAclause
              = CHC { paCls :: [PAclause]                 -- precisely one clause is executed.
                    , paMotiv :: [(Expression,[Rule] )]   -- tells which conjunct from which rule is being maintained
                    }
              | GCH { paGCls :: [(InsDel,Expression,PAclause)]    -- guarded choice; The rule is maintained if one of the clauses of which the expression is populated is executed.
                    , paMotiv :: [(Expression,[Rule] )]   -- tells which conjunct from which rule is being maintained
                    }
              | ALL { paCls :: [PAclause]                 -- all clauses are executed.
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | Do  { paSrt :: InsDel                     -- do Insert or Delete
                    , paTo :: Declaration                 -- into toExpr    or from toExpr
                    , paDelta :: Expression               -- delta
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | New { paCpt :: A_Concept                  -- make a new instance of type c
                    , paCl :: PSingleton ->PAclause            -- to be done after creating the concept
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | Rmv { paCpt :: A_Concept                  -- Remove an instance of type c
                    , paCl :: PSingleton->PAclause            -- to be done afteremoving the concept
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | Nop { paMotiv :: [(Expression,[Rule] )]   -- tells which conjunct from whichule is being maintained
                    }
              | Blk { paMotiv :: [(Expression,[Rule] )]   -- tells which expression from whichule has caused the blockage
                    }
              | Let { paExpr :: PAclause                  -- the expression that represents a condition to be tested.
                    , paBody :: PAclause -> PAclause
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | Ref { paVar :: String
                    }

-- SJC: adding motivation separately may help:
-- data Motivated a = Motivated a [(Expression,[Rule])] | Ref String
--   then make an instance for Ord Motivated (any way you like)
--   let PAclause be deriving (Ord,Eq)
instance Ord PAclause where
   CHC ds _ `compare` CHC ds' _ = ds `compare` ds'
   CHC{} `compare` _ = LT
   _ `compare` CHC{} = GT
   GCH ds _ `compare` GCH ds' _ = ds `compare` ds'
   GCH{} `compare` _ = LT
   _ `compare` GCH{} = GT
   ALL ds _ `compare` ALL ds' _ = ds `compare` ds'
   ALL{} `compare` _ = LT
   _ `compare` ALL{} = GT
   p@Do{}   `compare`   p'@Do{} = (paSrt p,paTo p,paDelta p) `compare` (paSrt p',paTo p',paDelta p')
   Do{} `compare` _ = LT
   _ `compare` Do{} = GT
   Nop _    `compare`     Nop _ = EQ
   Nop{} `compare` _ = LT
   _ `compare` Nop{} = GT
   p@New{}  `compare`  p'@New{} = paCpt p `compare` paCpt p'
   New{} `compare` _ = LT
   _ `compare` New{} = GT
   p@Rmv{}  `compare`  p'@Rmv{} = paCpt p `compare` paCpt p'
   Rmv{} `compare` _ = LT
   _ `compare` Rmv{} = GT
   Blk{} `compare` Blk{} = EQ
   Blk{} `compare` _ = LT
   _ `compare` Blk{} = GT
   Ref{} `compare` Ref{} = EQ
   Ref{} `compare` _ = LT
   _ `compare` Ref{} = GT
   p@Let{} `compare` p'@Let{} = paExpr p `compare` paExpr p' -- TODO SJC: this cannot be right, right? It was "False" in the old EQ class, but that was wrong too I think
instance Eq PAclause where a == b = (compare a b == EQ)

-- | Explanation is the intended constructor. It explains the purpose of the object it references.
--   The enrichment process of the parser must map the names (from PPurpose) to the actual objects
data Purpose  = Expl { explPos :: Origin     -- ^ The position in the Ampersand script of this purpose definition
                     , explObj :: ExplObj    -- ^ The object that is explained.
                     , explMarkup :: A_Markup   -- ^ This field contains the text of the explanation including language and markup info.
                     , explUserdefd :: Bool       -- ^ Is this purpose defined in the script?
                     , explRefIds :: [String]     -- ^ The references of the explaination
                     } deriving (Show, Typeable)
instance Eq Purpose where
  x0 == x1  =  explObj x0 == explObj x1 &&  -- TODO: check if this definition is right.
                                            -- I(Han) suspect that the Origin should be part of it.
               (amLang . explMarkup) x0 == (amLang . explMarkup) x1
instance Unique Purpose where
  showUnique p = uniqueShow True (explObj p)++" in "++(show.amLang.explMarkup) p
                   ++ uniqueShow True (explPos p)
instance Traced Purpose where
  origin = explPos

data Population -- The user defined populations
  = ARelPopu { popdcl :: Declaration
             , popps ::  [AAtomPair]     -- The user-defined pairs that populate the relation
             , popsrc :: A_Concept -- potentially more specific types than the type of Declaration
             , poptgt :: A_Concept
             }
  | ACptPopu { popcpt :: A_Concept
             , popas ::  [AAtomValue]  -- The user-defined atoms that populate the concept
             } deriving (Eq,Ord)

data AAtomPair
  = APair { apLeft  :: AAtomValue
          , apRight :: AAtomValue
          }deriving(Eq,Prelude.Ord)
mkAtomPair :: AAtomValue -> AAtomValue -> AAtomPair
mkAtomPair = APair
data AAtomValue
  = AAVString  { aavhash :: Int
               , aavtyp :: TType
               , aavtxt :: Text
               }
  | AAVInteger { aavtyp :: TType
               , aavint :: Integer
               }
  | AAVFloat   { aavtyp :: TType
               , aavflt :: Double
               }
  | AAVBoolean { aavtyp :: TType
               , aavbool :: Bool
               }
  | AAVDate { aavtyp :: TType
            , aadateDay ::  Day
            }
  | AAVDateTime { aavtyp :: TType
                , aadatetime ::  UTCTime
                }
  | AtomValueOfONE deriving (Eq,Prelude.Ord, Show)

aavstr :: AAtomValue -> String
aavstr = unpack.aavtxt
showValPHP :: AAtomValue -> Text
showValPHP val = pack$
  case val of
   AAVString{}  -> "'"++f (aavstr val)++"'"
     where
        f str'=
          case str' of
            []        -> []
            ('\'':cs) -> "\\\'"++ f cs  --This is required to ensure that the result of showValue will be a proper singlequoted string.
            ('\\':s') -> "\\\\" ++ f s'
            (c:cs)    -> c : f cs
   AAVInteger{} -> show (aavint val)
   AAVBoolean{} -> show (aavbool val)
   AAVDate{}    -> "'"++showGregorian (aadateDay val)++"'"
   AAVDateTime {} -> "'"++DTF.formatTime DTF.defaultTimeLocale "%F %T" (aadatetime val)++"'" --NOTE: MySQL 5.5 does not comply to ISO standard. This format is MySQL specific
     --formatTime SL.defaultTimeLocale "%FT%T%QZ" (aadatetime val)
   AAVFloat{}   -> show (aavflt val)
   AtomValueOfONE{} -> "1"
showValSQL :: AAtomValue -> String
showValSQL val =
  case val of
   AAVString{}  -> aavstr val
   AAVInteger{} -> show (aavint val)
   AAVBoolean{} -> show (aavbool val)
   AAVDate{}    -> showGregorian (aadateDay val)
   AAVDateTime {} -> "'"++DTF.formatTime DTF.defaultTimeLocale "%F %T" (aadatetime val)++"'" --NOTE: MySQL 5.5 does not comply to ISO standard. This format is MySQL specific
     --formatTime SL.defaultTimeLocale "%FT%T%QZ" (aadatetime val)
   AAVFloat{}   -> show (aavflt val)
   AtomValueOfONE{} -> "1"
showValADL :: AAtomValue -> String
showValADL val =
  case val of
   AAVString{}  ->       aavstr val
   AAVInteger{} -> show (aavint val)
   AAVBoolean{} -> show (aavbool val)
   AAVDate{}    -> showGregorian (aadateDay val)
   AAVDateTime {} -> DTF.formatTime DTF.defaultTimeLocale "%FT%T%QZ" (aadatetime val)
   AAVFloat{}   -> show (aavflt val)
   AtomValueOfONE{} -> "1"

data ExplObj = ExplConceptDef ConceptDef
             | ExplDeclaration Declaration
             | ExplRule String
             | ExplIdentityDef String
             | ExplViewDef String
             | ExplPattern String
             | ExplInterface String
             | ExplContext String
          deriving (Show ,Eq, Typeable)
instance Unique ExplObj where
  showUnique e = "Explanation of "++
    case e of
     (ExplConceptDef cd) -> uniqueShow True cd
     (ExplDeclaration d) -> uniqueShow True d
     (ExplRule s)        -> "a Rule named "++s
     (ExplIdentityDef s) -> "an Ident named "++s
     (ExplViewDef s)     -> "a View named "++s
     (ExplPattern s)     -> "a Pattern named "++s
     (ExplInterface s)   -> "an Interface named "++s
     (ExplContext s)     -> "a Context named "++s

data Expression
      = EEqu (Expression,Expression)   -- ^ equivalence             =
      | EInc (Expression,Expression)   -- ^ inclusion               |-
      | EIsc (Expression,Expression)   -- ^ intersection            /\
      | EUni (Expression,Expression)   -- ^ union                   \/
      | EDif (Expression,Expression)   -- ^ difference              -
      | ELrs (Expression,Expression)   -- ^ left residual           /
      | ERrs (Expression,Expression)   -- ^ right residual          \
      | EDia (Expression,Expression)   -- ^ diamond                 <>
      | ECps (Expression,Expression)   -- ^ composition             ;
      | ERad (Expression,Expression)   -- ^ relative addition       !
      | EPrd (Expression,Expression)   -- ^ cartesian product       *
      | EKl0 Expression                -- ^ Rfx.Trn closure         *  (Kleene star)
      | EKl1 Expression                -- ^ Transitive closure      +  (Kleene plus)
      | EFlp Expression                -- ^ conversion (flip, wok)  ~
      | ECpl Expression                -- ^ Complement
      | EBrk Expression                -- ^ bracketed expression ( ... )
      | EDcD Declaration               -- ^ simple declaration
      | EDcI A_Concept                 -- ^ Identity relation
      | EEps A_Concept Signature       -- ^ Epsilon relation (introduced by the system to ensure we compare concepts by equality only.
      | EDcV Signature                 -- ^ Cartesian product relation
      | EMp1 PSingleton A_Concept      -- ^ constant PAtomValue, because when building the Expression, the TType of the concept isn't known yet.
      deriving (Eq, Prelude.Ord, Show, Typeable, Generic, Data)
instance Unique Expression where
  showUnique = show
instance Hashable Expression where
   hashWithSalt s expr =
     s `hashWithSalt`
       case expr of
        EEqu (a,b) -> ( 0::Int) `hashWithSalt` a `hashWithSalt` b
        EInc (a,b) -> ( 1::Int) `hashWithSalt` a `hashWithSalt` b
        EIsc (a,b) -> ( 2::Int) `hashWithSalt` a `hashWithSalt` b
        EUni (a,b) -> ( 3::Int) `hashWithSalt` a `hashWithSalt` b
        EDif (a,b) -> ( 4::Int) `hashWithSalt` a `hashWithSalt` b
        ELrs (a,b) -> ( 5::Int) `hashWithSalt` a `hashWithSalt` b
        ERrs (a,b) -> ( 6::Int) `hashWithSalt` a `hashWithSalt` b
        EDia (a,b) -> ( 7::Int) `hashWithSalt` a `hashWithSalt` b
        ECps (a,b) -> ( 8::Int) `hashWithSalt` a `hashWithSalt` b
        ERad (a,b) -> ( 9::Int) `hashWithSalt` a `hashWithSalt` b
        EPrd (a,b) -> (10::Int) `hashWithSalt` a `hashWithSalt` b
        EKl0 e     -> (11::Int) `hashWithSalt` e
        EKl1 e     -> (12::Int) `hashWithSalt` e
        EFlp e     -> (13::Int) `hashWithSalt` e
        ECpl e     -> (14::Int) `hashWithSalt` e
        EBrk e     -> (15::Int) `hashWithSalt` e
        EDcD d     -> (16::Int) `hashWithSalt` d
        EDcI c     -> (17::Int) `hashWithSalt` c
        EEps c sgn -> (18::Int) `hashWithSalt` c `hashWithSalt` sgn
        EDcV sgn   -> (19::Int) `hashWithSalt` sgn
        EMp1 val c -> (21::Int) `hashWithSalt` show val `hashWithSalt` c

instance Unique (PairView Expression) where
  showUnique = show
instance Unique (PairViewSegment Expression) where
  showUnique = show


(.==.), (.|-.), (./\.), (.\/.), (.-.), (./.), (.\.), (.<>.), (.:.), (.!.), (.*.) :: (?loc :: CallStack) => Expression -> Expression -> Expression
infixl 1 .==.   -- equivalence
infixl 1 .|-.   -- inclusion
infixl 2 ./\.   -- intersection
infixl 2 .\/.   -- union
infixl 4 .-.    -- difference
infixl 6 ./.    -- left residual
infixl 6 .\.    -- right residual
infixl 6 .<>.   -- diamond
infixl 8 .:.    -- composition    -- .;. was unavailable, because Haskell's scanner does not recognize it as an operator.
infixl 8 .!.    -- relative addition
infixl 8 .*.    -- cartesian product

-- SJ 20130118: The fatals are superfluous, but only if the type checker works correctly. For that reason, they are not being removed. Not even for performance reasons.
l .==. r = if source l/=source r ||  target l/=target r then fatal 424 ("Cannot equate (with operator \"==\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           EEqu (l,r)
l .|-. r = if source l/=source r ||  target l/=target r then fatal 426 ("Cannot include (with operator \"|-\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           EInc (l,r)
l ./\. r = if source l/=source r ||  target l/=target r then fatal 428 ("Cannot intersect (with operator \"/\\\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           EIsc (l,r)
l .\/. r = if source l/=source r ||  target l/=target r then fatal 430 ("Cannot unite (with operator \"\\/\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           EUni (l,r)
l .-. r  = if source l/=source r ||  target l/=target r then fatal 432 ("Cannot subtract (with operator \"-\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           EDif (l,r)
l ./. r  = if target l/=target r then fatal 434 ("Cannot residuate (with operator \"/\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           ELrs (l,r)
l .\. r  = if source l/=source r then fatal 436 ("Cannot residuate (with operator \"\\\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           ERrs (l,r)
l .<>. r = if source l/=target r then fatal 438 ("Cannot use diamond operator \"<>\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           EDia (l,r)
l .:. r  = if source r/=target l then fatal 440 ("Cannot compose (with operator \";\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           ECps (l,r)
l .!. r  = if source r/=target l then fatal 442 ("Cannot add (with operator \"!\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           ERad (l,r)
l .*. r  = -- SJC: always fits! No fatal here..
           EPrd (l,r)
{- For the operators /, \, ;, ! and * we must not check whether the intermediate types exist.
   Suppose the user says GEN Student ISA Person and GEN Employee ISA Person, then Student `join` Employee has a name (i.e. Person), but Student `meet` Employee
   does not. In that case, -(r!s) (with target r=Student and source s=Employee) is defined, but -r;-s is not.
   So in order to let -(r!s) be equal to -r;-s we must not check for the existence of these types, for the Rotterdam paper already shows that this is fine.
-}

instance Flippable Expression where
  flp expr = case expr of
               EEqu (l,r) -> EEqu (flp l, flp r)
               EInc (l,r) -> EInc (flp l, flp r)
               EIsc (l,r) -> EIsc (flp l, flp r)
               EUni (l,r) -> EUni (flp l, flp r)
               EDif (l,r) -> EDif (flp l, flp r)
               ELrs (l,r) -> ERrs (flp r, flp l)
               ERrs (l,r) -> ELrs (flp r, flp l)
               EDia (l,r) -> EDia (flp r, flp l)
               ECps (l,r) -> ECps (flp r, flp l)
               ERad (l,r) -> ERad (flp r, flp l)
               EPrd (l,r) -> EPrd (flp r, flp l)
               EFlp e     -> e
               ECpl e     -> ECpl (flp e)
               EKl0 e     -> EKl0 (flp e)
               EKl1 e     -> EKl1 (flp e)
               EBrk f     -> EBrk (flp f)
               EDcD{}     -> EFlp expr
               EDcI{}     -> expr
               EEps i sgn -> EEps i (flp sgn)
               EDcV sgn   -> EDcV (flp sgn)
               EMp1{}     -> expr

instance Association Expression where
 sign (EEqu (l,r)) = Sign (source l) (target r)
 sign (EInc (l,r)) = Sign (source l) (target r)
 sign (EIsc (l,r)) = Sign (source l) (target r)
 sign (EUni (l,r)) = Sign (source l) (target r)
 sign (EDif (l,r)) = Sign (source l) (target r)
 sign (ELrs (l,r)) = Sign (source l) (source r)
 sign (ERrs (l,r)) = Sign (target l) (target r)
 sign (EDia (l,r)) = Sign (source l) (target r)
 sign (ECps (l,r)) = Sign (source l) (target r)
 sign (ERad (l,r)) = Sign (source l) (target r)
 sign (EPrd (l,r)) = Sign (source l) (target r)
 sign (EKl0 e)     = sign e
 sign (EKl1 e)     = sign e
 sign (EFlp e)     = flp (sign e)
 sign (ECpl e)     = sign e
 sign (EBrk e)     = sign e
 sign (EDcD d)     = sign d
 sign (EDcI c)     = Sign c c
 sign (EEps _ sgn) = sgn
 sign (EDcV sgn)   = sgn
 sign (EMp1 _ c)   = Sign c c

showSign :: Association a => a -> String
showSign x = let Sign s t = sign x in "["++name s++"*"++name t++"]"

-- We allow editing on basic relations (Declarations) that may have been flipped, or narrowed/widened by composing with I.
-- Basically, we have a relation that may have several epsilons to its left and its right, and the source/target concepts
-- we use are the concepts in the innermost epsilon, or the source/target concept of the relation, in absence of epsilons.
-- This is used to determine the type of the atoms provided by the outside world through interfaces.
getExpressionRelation :: Expression -> Maybe (A_Concept, Declaration, A_Concept, Bool)
getExpressionRelation expr = case getRelation expr of
   Just (s,Just d,t,isFlipped)  -> Just (s,d,t,isFlipped)
   _                            -> Nothing
 where
    -- If the expression represents an editable relation, the relation is returned together with the narrowest possible source and target
    -- concepts, as well as a boolean that states whether the relation is flipped.
    getRelation :: Expression -> Maybe (A_Concept, Maybe Declaration, A_Concept, Bool)
    getRelation (ECps (e, EDcI{})) = getRelation e
    getRelation (ECps (EDcI{}, e)) = getRelation e
    getRelation (ECps (e1, e2))
      = case (getRelation e1, getRelation e2) of --note: target e1==source e2
         (Just (_,Nothing,i1,_), Just (i2,Nothing,_,_)) -> if i1==target e1 && i2==source e2 then Just (i1, Nothing, i2, False) else -- i1==i2
                                                           if i1==target e1 && i2/=source e2 then Just (i2, Nothing, i2, False) else
                                                           if i1/=target e1 && i2==source e2 then Just (i1, Nothing, i1, False) else
                                                           Nothing
         (Just (_,Nothing,i,_), Just (s,d,t,isFlipped)) -> if i==target e1                 then Just (s,d,t,isFlipped) else
                                                           if i/=target e1 && s==target e1 then Just (i,d,t,isFlipped) else
                                                           Nothing
         (Just (s,d,t,isFlipped), Just (i,Nothing,_,_)) -> if i==source e2                 then Just (s,d,t,isFlipped) else
                                                           if i/=source e2 && t==source e2 then Just (s,d,i,isFlipped) else
                                                           Nothing
         _                                              -> Nothing
    getRelation (EFlp e)
     = case getRelation e of
         Just (s,d,t,isFlipped) -> Just (t,d,s,not isFlipped)
         Nothing                -> Nothing
    getRelation (EDcD d)   = Just (source d, Just d, target d, False)
    getRelation (EEps i _) = Just (i, Nothing, i, False)
    getRelation _ = Nothing


-- The following definition of concept is used in the type checker only.
-- It is called Concept, meaning "type checking concept"

data A_Concept
   = PlainConcept { cpthash :: Int
                  , cptnm :: Text  -- ^PlainConcept nm represents the set of instances cs by name nm.
                  }
   | ONE  -- ^The universal Singleton: 'I'['Anything'] = 'V'['Anything'*'Anything']
    deriving (Typeable,Data,Prelude.Ord,Eq)

{- -- this is faster, so if you think Eq on concepts is taking a long time, try this..
instance Prelude.Ord A_Concept where
  compare (PlainConcept{cpthash=v1}) (PlainConcept{cpthash=v2}) = compare v1 v2
  compare ONE ONE = EQ
  compare ONE (PlainConcept{}) = LT
  compare (PlainConcept{}) ONE = GT

instance Prelude.Eq A_Concept where
  (==) a b = compare a b == EQ

-- SJC TODO: put "makeConcept" in a monad or something, and number them consecutively to avoid hash collisions
-}
  
makeConcept :: String -> A_Concept
makeConcept "ONE" = ONE
makeConcept v = PlainConcept (hash v) (pack v)

instance Unique A_Concept where
  showUnique = name
instance Hashable A_Concept where
  hashWithSalt s cpt =
     s `hashWithSalt` (case cpt of
                        PlainConcept{} -> (0::Int) `hashWithSalt` cpthash cpt
                        ONE            -> (1::Int)
                      )
instance Named A_Concept where
  name PlainConcept{cptnm = nm} = unpack nm
  name ONE = "ONE"

instance Show A_Concept where
  showsPrec _ c = showString (name c)

instance Unique (A_Concept, PSingleton) where
  showUnique (c,val) = show val++"["++showUnique c++"]"

data Signature = Sign A_Concept A_Concept deriving (Eq, Prelude.Ord, Typeable, Generic, Data)
instance Hashable Signature
instance Show Signature where
  showsPrec _ (Sign s t) =
     showString (   "[" ++ show s ++ "*" ++ show t ++ "]" )
instance Unique Signature where
  showUnique (Sign s t) = "[" ++ uniqueShow False s ++ "*" ++ uniqueShow False t ++ "]"
instance Association Signature where
  source (Sign s _) = s
  target (Sign _ t) = t
  sign sgn = sgn

instance Flippable Signature where
 flp (Sign s t) = Sign t s

class Association rel where
  source, target :: rel -> A_Concept      -- e.g. Declaration -> Concept
  source x        = source (sign x)
  target x        = target (sign x)
  sign :: rel -> Signature
  isEndo :: rel  -> Bool
  isEndo s        = source s == target s


-- Convenient data structure to hold information about concepts and their representations
--  in a context.
data ContextInfo =
  CI { ctxiGens         :: [A_Gen]      -- The generalisation relations in the context
     , representationOf :: A_Concept -> TType -- a list containing all user defined Representations in the context
     , multiKernels     :: [Typology] -- a list of typologies, based only on the CLASSIFY statements. Single-concept typologies are not included
     }
                       
   

-- | This function is meant to convert the PSingleton inside EMp1 to an AAtomValue,
--   after the expression has been built inside an A_Context. Only at that time
--   the TType is known, enabling the correct transformation.
--   To ensure that this function is not used too early, ContextInfo is required,
--   which only exsists after disambiguation.
safePSingleton2AAtomVal :: ContextInfo -> A_Concept -> PSingleton -> AAtomValue
safePSingleton2AAtomVal ci c val =
   case unsafePAtomVal2AtomValue typ (Just c) val of
     Left _ -> fatal 855 $ "This should be impossible: after checking everything an unhandled singleton value found!\n  "
                     ++ show val
     Right x -> x
  where typ = representationOf ci c

-- SJC: Note about this code:
-- error messages are written here, and later turned into error messages via mkIncompatibleAtomValueError
-- Ideally, this module would import Ampersand.Input.ADL1.CtxError
-- that way, unsafePAtomVal2AtomValue could create a 'Origin -> Guarded AAtomValue' instead.
unsafePAtomVal2AtomValue :: TType -> Maybe A_Concept -> PAtomValue -> Either String AAtomValue
unsafePAtomVal2AtomValue typ mCpt pav =
  case unsafePAtomVal2AtomValue' typ mCpt pav of
    Left err -> Left err
    Right rawVal -> Right roundedVal
      where roundedVal =
             case rawVal of
              AAVDateTime t x -> -- Rounding is needed, to maximize the number of databases
                                 -- on wich this runs. (MySQL 5.5 only knows seconds)
                                 AAVDateTime t (truncateByFormat x)
                                  where
                                    truncateByFormat :: UTCTime  -> UTCTime
                                    truncateByFormat = f (DTF.parseTimeOrError True) . f DTF.formatTime
                                      where
                                        format = DTF.iso8601DateFormat (Just "%H:%M:%S")
                                    --    f:: DTF.TimeLocale -> String -> typ
                                        f fun = fun DTF.defaultTimeLocale format
              _          -> rawVal

unsafePAtomVal2AtomValue' :: TType -> Maybe A_Concept -> PAtomValue -> Either String AAtomValue
unsafePAtomVal2AtomValue' typ mCpt pav
  = case pav of
      PSingleton _ str mval
         -> case typ of
             Alphanumeric     -> Right (AAVString (hash str) typ (pack str))
             BigAlphanumeric  -> Right (AAVString (hash str) typ (pack str))
             HugeAlphanumeric -> Right (AAVString (hash str) typ (pack str))
             Password         -> Right (AAVString (hash str) typ (pack str))
             Object           -> Right (AAVString (hash str) typ (pack str))
             _                -> case mval of
                                   Nothing -> message str
                                   Just x -> unsafePAtomVal2AtomValue typ mCpt x
      ScriptString _ str
         -> case typ of
             Alphanumeric     -> Right (AAVString (hash str) typ (pack str))
             BigAlphanumeric  -> Right (AAVString (hash str) typ (pack str))
             HugeAlphanumeric -> Right (AAVString (hash str) typ (pack str))
             Password         -> Right (AAVString (hash str) typ (pack str))
             Binary           -> Left "Binary cannot be populated in an ADL script"
             BigBinary        -> Left "Binary cannot be populated in an ADL script"
             HugeBinary       -> Left "Binary cannot be populated in an ADL script"
             Date             -> message str
             DateTime         -> message str
             Boolean          -> message str
             Integer          -> message str
             Float            -> message str
             TypeOfOne        -> Left "ONE has a population of it's own, that cannot be modified"
             Object           -> Right (AAVString (hash str) typ (pack str))
      XlsxString _ str
         -> case typ of
             Alphanumeric     -> Right (AAVString (hash str) typ (pack str))
             BigAlphanumeric  -> Right (AAVString (hash str) typ (pack str))
             HugeAlphanumeric -> Right (AAVString (hash str) typ (pack str))
             Password         -> Right (AAVString (hash str) typ (pack str))
             Binary           -> Left "Binary cannot be populated in an ADL script"
             BigBinary        -> Left "Binary cannot be populated in an ADL script"
             HugeBinary       -> Left "Binary cannot be populated in an ADL script"
             Date             -> message str
             DateTime         -> message str
             Boolean          -> let table =
                                        [("TRUE", True), ("FALSE" , False)
                                        ,("YES" , True), ("NO"    , False)
                                        ,("WAAR", True), ("ONWAAR", False)
                                        ,("JA"  , True), ("NEE"   , False)
                                        ,("WEL" , True), ("NIET"  , False)
                                        ]
                                 in case lookup (map toUpper str) table of
                                    Just b -> Right (AAVBoolean typ b)
                                    Nothing -> Left $ "permitted Booleans: "++(show . map (camelCase . fst)) table
                                   where camelCase []     = []
                                         camelCase (c:xs) = toUpper c: map toLower xs

             Integer          -> case maybeRead str  of
                                   Just i  -> Right (AAVInteger typ i)
                                   Nothing -> message str
             Float         -> case maybeRead str of
                                   Just r  -> Right (AAVFloat typ r)
                                   Nothing -> message str
             TypeOfOne        -> Left "ONE has a population of it's own, that cannot be modified"
             Object           -> Right (AAVString (hash str) typ (pack str))
      ScriptInt _ i
         -> case typ of
             Alphanumeric     -> message i
             BigAlphanumeric  -> message i
             HugeAlphanumeric -> message i
             Password         -> message i
             Binary           -> Left "Binary cannot be populated in an ADL script"
             BigBinary        -> Left "Binary cannot be populated in an ADL script"
             HugeBinary       -> Left "Binary cannot be populated in an ADL script"
             Date             -> message i
             DateTime         -> message i
             Boolean          -> message i
             Integer          -> Right (AAVInteger typ i)
             Float            -> Right (AAVFloat typ (fromInteger i)) -- must convert, because `34.000` is lexed as Integer
             TypeOfOne        -> Left "ONE has a population of it's own, that cannot be modified"
             Object           -> message i
      ScriptFloat _ x
         -> case typ of
             Alphanumeric     -> message x
             BigAlphanumeric  -> message x
             HugeAlphanumeric -> message x
             Password         -> message x
             Binary           -> Left "Binary cannot be populated in an ADL script"
             BigBinary        -> Left "Binary cannot be populated in an ADL script"
             HugeBinary       -> Left "Binary cannot be populated in an ADL script"
             Date             -> message x
             DateTime         -> message x
             Boolean          -> message x
             Integer          -> message x
             Float            -> Right (AAVFloat typ x)
             TypeOfOne        -> Left "ONE has a population of it's own, that cannot be modified"
             Object           -> message x
      XlsxDouble _ d
         -> case typ of
             Alphanumeric     -> relaxXLSXInput d    
             BigAlphanumeric  -> relaxXLSXInput d
             HugeAlphanumeric -> relaxXLSXInput d
             Password         -> relaxXLSXInput d
             Binary           -> Left "Binary cannot be populated in an ADL script"
             BigBinary        -> Left "Binary cannot be populated in an ADL script"
             HugeBinary       -> Left "Binary cannot be populated in an ADL script"
             Date             -> Right (AAVDate {aavtyp = typ
                                                ,aadateDay = addDays (floor d) dayZeroExcel
                                                })
             DateTime         -> Right (AAVDateTime {aavtyp = typ
                                                    ,aadatetime = UTCTime (addDays daysSinceZero dayZeroExcel)
                                                                          (picosecondsToDiffTime.floor $ fractionOfDay*picosecondsPerDay)

                                                })
                                     where picosecondsPerDay = 24*60*60*1000000000000
                                           (daysSinceZero, fractionOfDay) = properFraction d
             Boolean          -> message d
             Integer          -> if frac == 0
                                 then Right (AAVInteger typ int)
                                 else message d
                                  where
                                    (int,frac) = properFraction d
             Float            -> Right (AAVFloat typ d)
             TypeOfOne        -> Left "ONE has a population of it's own, that cannot be modified"
             Object           -> relaxXLSXInput d
      ComnBool _ b
         -> if typ == Boolean
            then Right (AAVBoolean typ b)
            else message b
      ScriptDate _ x
         -> if typ == Date
            then Right (AAVDate typ x)
            else message x
      ScriptDateTime _ x
         -> if typ == DateTime
            then Right (AAVDateTime typ x)
            else message x

   where
     relaxXLSXInput :: Double -> Either String AAtomValue
     relaxXLSXInput v = Right (AAVString (hash v) typ (pack (neat (show v))))
       where neat :: String -> String
             neat s 
               | onlyZeroes dotAndAfter = beforeDot
               | otherwise = s
               where (beforeDot, dotAndAfter) = span (/= '.') s
                     onlyZeroes s' =
                      case s' of 
                       [] -> True
                       '.':zeros ->  nub zeros == "0"
                       _ -> False
     message :: Show x => x -> Either String a
     message x = Left . intercalate "\n    " $
                 ["Representation mismatch"
                 , "Found: `"++show x++"`,"
                 , "as representation of an atom in concept `"++name c++"`."
                 , "However, the representation-type of that concept is "++implicitly
                 , "defined as "++show expected++". The found value does not match that type."
                 ]
        where
          c = fromMaybe (fatal 1004 "Representation mismatch without concept known should not happen.") mCpt
          expected = if typ == Object then Alphanumeric else typ
          implicitly = if typ == Object then "(implicitly) " else ""
     dayZeroExcel = addDays (-2) (fromGregorian 1900 1 1) -- Excel documentation tells that counting starts a jan 1st, however, that isn't totally true.
     maybeRead :: Read a => String -> Maybe a
     maybeRead = fmap fst . listToMaybe . reads


-- | The typology of a context is the partioning of the concepts in that context into 
--   sets such that (isa\/isa~)*;typology |- typology
--   Note, that with isa we only refer to the relations defined by CLASSIFY statements, 
--   not named relations with the same properties ( {UNI,INJ,TOT} or {UNI,INJ,SUR} )
data Typology = Typology { tyroot :: A_Concept -- the most generic concept in the typology 
                         , tyCpts :: [A_Concept] -- all concepts, from generic to specific
                         } deriving Show
