{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Database.Design.Ampersand.Core.AbstractSyntaxTree (
   A_Context(..)
 , Meta(..)
 , Theme(..)
 , Process(..)
 , Pattern(..)
 , PairView(..)
 , PairViewSegment(..)
 , Rule(..)
 , ruleIsInvariantUniOrInj
 , RuleType(..)
 , RuleOrigin(..)
 , Declaration(..)
 , IdentityDef(..)
 , IdentitySegment(..)
 , ViewDef(..)
 , ViewSegment(..)
 , A_Gen(..)
 , Interface(..)
 , getInterfaceByName
 , SubInterface(..)
 , ObjectDef(..)
 , Object(..)
 , objAts
 , Purpose(..)
 , ExplObj(..)
 , Expression(..)
 , A_Concept(..)
 , A_Markup(..)
 , AMeaning(..)
 , RoleRelation(..)
 , Sign(..)
 , Population(..)
 , GenR
 , Association(..)
 , PAclause(..), Event(..), ECArule(..), InsDel(..), Conjunct(..), DnfClause(..)
  -- (Poset.<=) is not exported because it requires hiding/qualifying the Prelude.<= or Poset.<= too much
  -- import directly from Database.Design.Ampersand.Core.Poset when needed
 , (<==>),join,meet,greatest,least,maxima,minima,sortWith
 , smallerConcepts, largerConcepts, rootConcepts
 , showSign
 , aMarkup2String
 , module Database.Design.Ampersand.Core.ParseTree  -- export all used constructors of the parsetree, because they have actually become part of the Abstract Syntax Tree.
 , (.==.), (.|-.), (./\.), (.\/.), (.-.), (./.), (.\.), (.<>.), (.:.), (.!.), (.*.)
)where
import qualified Prelude
import Prelude hiding (Ord(..), Ordering(..))
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Core.ParseTree   (MetaObj(..),Meta(..),ConceptDef,Origin(..),Traced(..),PairView(..),PairViewSegment(..),Prop(..),Lang,Pairs, PandocFormat, P_Markup(..), PMeaning(..), SrcOrTgt(..), isSrc)
import Database.Design.Ampersand.Core.Poset (Poset(..), Sortable(..),Ordering(..),greatest,least,maxima,minima,sortWith)
import Database.Design.Ampersand.Misc
import Text.Pandoc hiding (Meta)
import Data.Function
import Data.List (intercalate,nub,delete)
import Data.Typeable

fatal :: Int -> String -> a
fatal = fatalMsg "Core.AbstractSyntaxTree"

data A_Context
   = ACtx{ ctxnm :: String           -- ^ The name of this context
         , ctxpos :: [Origin]        -- ^ The origin of the context. A context can be a merge of a file including other files c.q. a list of Origin.
         , ctxlang :: Lang           -- ^ The default language used in this context.
         , ctxmarkup :: PandocFormat -- ^ The default markup format for free text in this context (currently: LaTeX, ...)
         , ctxthms :: [String]       -- ^ Names of patterns/processes to be printed in the functional specification. (For partial documents.)
         , ctxpats :: [Pattern]      -- ^ The patterns defined in this context
         , ctxprocs :: [Process]     -- ^ The processes defined in this context
         , ctxrs :: [Rule]           -- ^ All user defined rules in this context, but outside patterns and outside processes
         , ctxds :: [Declaration]    -- ^ The relations that are declared in this context, outside the scope of patterns
         , ctxpopus :: [Population]  -- ^ The user defined populations of relations defined in this context, including those from patterns and processes
         , ctxcds :: [ConceptDef]    -- ^ The concept definitions defined in this context, including those from patterns and processes
         , ctxks :: [IdentityDef]    -- ^ The identity definitions defined in this context, outside the scope of patterns
         , ctxvs :: [ViewDef]        -- ^ The view definitions defined in this context, outside the scope of patterns
         , ctxgs :: [A_Gen]          -- ^ The specialization statements defined in this context, outside the scope of patterns
         , ctxgenconcs :: [[A_Concept]] -- ^ A partitioning of all concepts: the union of all these concepts contains all atoms, and the concept-lists are mutually distinct in terms of atoms in one of the mentioned concepts
         , ctxifcs :: [Interface]    -- ^ The interfaces defined in this context
         , ctxps :: [Purpose]        -- ^ The purposes of objects defined in this context, outside the scope of patterns and processes
         , ctxsql :: [ObjectDef]     -- ^ user defined sqlplugs, taken from the Ampersand script
         , ctxphp :: [ObjectDef]     -- ^ user defined phpplugs, taken from the Ampersand script
         , ctxmetas :: [Meta]        -- ^ used for Pandoc authors (and possibly other things)
         } deriving (Typeable)              --deriving (Show) -- voor debugging
instance Show A_Context where
  showsPrec _ c = showString (ctxnm c)
instance Eq A_Context where
  c1 == c2  =  name c1 == name c2
instance Unique A_Context where
  showUnique = name
instance Named A_Context where
  name  = ctxnm

data Theme = PatternTheme Pattern | ProcessTheme Process

instance Named Theme where
  name (PatternTheme pat) = name pat
  name (ProcessTheme prc) = name prc

instance Traced Theme where
  origin (PatternTheme pat) = origin pat
  origin (ProcessTheme prc) = origin prc

data Process = Proc { prcNm :: String
                    , prcPos :: Origin
                    , prcEnd :: Origin      -- ^ the end position in the file, elements with a position between pos and end are elements of this process.
                    , prcRules :: [Rule]
                    , prcGens :: [A_Gen]
                    , prcDcls :: [Declaration]
                    , prcUps :: [Population]  -- ^ The user defined populations in this process
                    , prcRRuls :: [(String,Rule)]    -- ^ The assignment of roles to rules.
                    , prcRRels :: [(String,Declaration)] -- ^ The assignment of roles to Relations.
                    , prcIds :: [IdentityDef]            -- ^ The identity definitions defined in this process
                    , prcVds :: [ViewDef]            -- ^ The view definitions defined in this process
                    , prcXps :: [Purpose]           -- ^ The motivations of elements defined in this process
                    }
instance Named Process where
  name = prcNm

instance Traced Process where
  origin = prcPos

data RoleRelation
   = RR { rrRoles :: [String]     -- ^ name of a role
        , rrRels :: [Declaration]   -- ^ name of a Relation
        , rrPos :: Origin       -- ^ position in the Ampersand script
        } deriving Show
instance Traced RoleRelation where
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
           } deriving (Typeable, Show)    -- Show for debugging purposes
instance Eq Pattern where
  p==p' = ptnm p==ptnm p'
instance Unique Pattern where
  showUnique = name

instance Named Pattern where
 name = ptnm
instance Traced Pattern where
 origin = ptpos

data A_Markup =
    A_Markup { amLang :: Lang -- No Maybe here!  In the A-structure, it will be defined by the default if the P-structure does not define it. In the P-structure, the language is optional.
             , amFormat :: PandocFormat -- Idem: no Maybe in the A-structure.
             , amPandoc :: [Block]
             } deriving (Show, Eq, Prelude.Ord)

data RuleOrigin = UserDefined     -- This rule was specified explicitly as a rule in the Ampersand script
                | Multiplicity    -- This rule follows implicitly from the Ampersand script (Because of a property) and generated by a computer
                | Identity             -- This rule follows implicitly from the Ampersand script (Because of a identity) and generated by a computer
                deriving (Show, Eq)
data Rule =
     Ru { rrnm :: String                  -- ^ Name of this rule
        , rrexp :: Expression              -- ^ The rule expression
        , rrfps :: Origin                  -- ^ Position in the Ampersand file
        , rrmean :: AMeaning                -- ^ Ampersand generated meaning (for all known languages)
        , rrmsg :: [A_Markup]              -- ^ User-specified violation messages, possibly more than one, for multiple languages.
        , rrviol :: Maybe (PairView Expression) -- ^ Custom presentation for violations, currently only in a single language
        , rrtyp :: Sign                    -- ^ Allocated type
        , rrdcl :: Maybe (Prop,Declaration)  -- ^ The property, if this rule originates from a property on a Declaration
        , r_env :: String                  -- ^ Name of pattern in which it was defined.
        , r_usr :: RuleOrigin              -- ^ Where does this rule come from?
        , isSignal :: Bool                    -- ^ True if this is a signal; False if it is an invariant
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

-- When an invariant rule is univalent or injective, the way it is stored in a table does not allow the univalence or injectivity
-- to be broken. Hence, we need not check these rules in the prototype. (preventing breakage is the responsibility of the front-end)
ruleIsInvariantUniOrInj :: Rule -> Bool
ruleIsInvariantUniOrInj rule | not (isSignal rule), Just (p,_) <- rrdcl rule = p `elem` [Uni, Inj]
                             | otherwise                                     = False
                             -- NOTE: currently all rules coming from properties are invariants, so the not isSignal
                             -- condition is unnecessary, but this will change in the future.    
    
data RuleType = Implication | Equivalence | Truth  deriving (Eq,Show)

data Conjunct = Cjct { rc_id         :: String -- string that identifies this conjunct ('id' rather than 'name', because 
                                               -- this is an internal id that has no counterpart at the ADL level)
                     , rc_orgRules   :: [Rule] -- All rules this conjunct originates from
                     , rc_conjunct   :: Expression
                     , rc_dnfClauses :: [DnfClause]
                     } deriving (Show,Typeable)

data DnfClause = Dnf [Expression] [Expression] deriving (Show, Eq) -- Show is for debugging purposes only.

instance Eq Conjunct where
  rc==rc' = rc_id rc==rc_id rc'
instance Unique Conjunct where
  showUnique = rc_id
instance Prelude.Ord Conjunct where
  compare = Prelude.compare `on` rc_id

data Declaration =
  Sgn { decnm :: String     -- ^ the name of the declaration
      , decsgn :: Sign       -- ^ the source and target concepts of the declaration
       --multiplicities returns decprps_calc, when it has been calculated. So if you only need the user defined properties do not use multiplicities but decprps
      , decprps :: [Prop]     -- ^ the user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
      , decprps_calc :: Maybe [Prop] -- ^ the calculated and user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx, Irf). Note that calculated properties are made by adl2fspec, so in the A-structure decprps and decprps_calc yield exactly the same answer.
      , decprL :: String     -- ^ three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
      , decprM :: String     -- ^    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
      , decprR :: String
      , decMean :: AMeaning   -- ^ the meaning of a declaration, for each language supported by Ampersand.
      , decfpos :: Origin     -- ^ the position in the Ampersand source file where this declaration is declared. Not all decalartions come from the ampersand souce file.
      , deciss  :: Bool       -- ^ if true, this is a signal relation; otherwise it is an ordinary relation.
      , decusr  :: Bool       -- ^ if true, this relation is declared by an author in the Ampersand script; otherwise it was generated by Ampersand.
      , decpat  :: String     -- ^ the pattern where this declaration has been declared.
      , decplug :: Bool       -- ^ if true, this relation may not be stored in or retrieved from the standard database (it should be gotten from a Plug of some sort instead)
      } |
  Isn
      { detyp :: A_Concept       -- ^ The type
      } |
  Vs
      { decsgn :: Sign
      } deriving (Prelude.Ord, Typeable)

instance Eq Declaration where
  d@Sgn{}     == d'@Sgn{}     = decnm d==decnm d' && decsgn d==decsgn d'
  d@Isn{}     == d'@Isn{}     = detyp d==detyp d'
  d@Vs{}      == d'@Vs{}      = decsgn d==decsgn d'
  _           == _            = False
instance Unique Declaration where
  showUnique d = 
    case d of
      Sgn{} -> name d++uniqueShow False (decsgn d)
      Isn{} -> "I["++uniqueShow False (detyp d)++"]"
      Vs{}  -> "V"++uniqueShow False (decsgn d)
instance Show Declaration where  -- For debugging purposes only (and fatal messages)
  showsPrec _ decl@Sgn{}
   = showString (case decl of
                  Sgn{} -> name decl++showSign (sign decl)
                  Isn{} -> "I["++show (detyp decl)++"]" -- Isn{} is of type Declaration and it is implicitly defined
                  Vs{}  -> "V"++show (decsgn decl) )
-- was:
--  = showString (unwords (["RELATION",decnm decl,show (decsgn decl),show (decprps_calc decl)
--                         ,"PRAGMA",show (decprL decl),show (decprM decl),show (decprR decl)]
--                          ++concatMap showMeaning (ameaMrk (decMean decl))
--               )        )
--         where
--            showMeaning m = "MEANING"
--                           : ["IN", show (amLang m)]
--                          ++ [show (amFormat m)]
--                          ++ ["{+",aMarkup2String m,"-}"]
--                          -- then [] else ["MEANING",show (decMean decl)] ))

  showsPrec _ d@Isn{}     = showString $ "Isn{detyp="++show(detyp d)++"}"
  showsPrec _ d@Vs{}      = showString $ "V"++showSign(decsgn d)

aMarkup2String :: A_Markup -> String
aMarkup2String a = blocks2String (amFormat a) False (amPandoc a)

data AMeaning = AMeaning { ameaMrk ::[A_Markup]} deriving (Show, Eq, Prelude.Ord)

instance Named Declaration where
  name d@Sgn{}   = decnm d
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

data ViewDef = Vd { vdpos :: Origin         -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number).
                  , vdlbl :: String         -- ^ the name (or label) of this View. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface. It is not an empty string.
                  , vdcpt :: A_Concept      -- ^ this expression describes the instances of this object, related to their context
                  , vdats :: [ViewSegment]  -- ^ the constituent attributes (i.e. name/expression pairs) of this view.
                  } deriving (Eq,Show)
instance Named ViewDef where
  name = vdlbl
instance Traced ViewDef where
  origin = vdpos

data ViewSegment = ViewExp ObjectDef | ViewText String | ViewHtml String deriving (Eq, Show)

-- | data structure A_Gen contains the CLASSIFY statements from an Ampersand script
--   CLASSIFY Employee ISA Person   translates to Isa (C "Person") (C "Employee")
--   CLASSIFY Workingstudent IS Employee/\Student   translates to IsE orig (C "Workingstudent") [C "Employee",C "Student"]
data A_Gen = Isa { genspc :: A_Concept      -- ^ specific concept
                 , gengen :: A_Concept      -- ^ generic concept
                 }
           | IsE { genspc :: A_Concept      -- ^ specific concept
                 , genrhs :: [A_Concept]    -- ^ concepts of which the conjunction is equivalent to the specific concept
                 } deriving (Typeable, Eq)
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

-- | this function takes all generalisation relations from the context and a concept and delivers a list of all concepts that are more specific than the given concept.
--   If there are no cycles in the generalization graph,  cpt  cannot be an element of  smallerConcepts gens cpt.
smallerConcepts :: [A_Gen] -> A_Concept -> [A_Concept]
smallerConcepts gens cpt
  = nub$ oneSmaller ++ concatMap (smallerConcepts gens) oneSmaller
  where oneSmaller = delete cpt. nub $ [ genspc g | g@Isa{}<-gens, gengen g==cpt ]++[ genspc g | g@IsE{}<-gens, cpt `elem` genrhs g ]
-- | this function takes all generalisation relations from the context and a concept and delivers a list of all concepts that are more generic than the given concept.
largerConcepts :: [A_Gen] -> A_Concept -> [A_Concept]
largerConcepts gens cpt
 = nub$ oneLarger ++ concatMap (largerConcepts gens) oneLarger
  where oneLarger  = delete cpt. nub $[ gengen g | g@Isa{}<-gens, genspc g==cpt ]++[ c | g@IsE{}<-gens, genspc g==cpt, c<-genrhs g ]

-- | this function returns the most generic concepts in the class of a given concept
rootConcepts :: [A_Gen]  -> [A_Concept] -> [A_Concept]
rootConcepts gens cpts = [ root | root<-nub $ [ c | cpt<-cpts, c<-largerConcepts gens cpt ] `uni` cpts
                                , root `notElem` [ genspc g | g@Isa{}<-gens]++[c | g@IsE{}<-gens, c<-genrhs g ]
                                ]

data Interface = Ifc { ifcParams ::   [Expression] -- Only primitive expressions are allowed!
                     , ifcClass ::    Maybe String
                     , ifcArgs ::     [[String]]
                     , ifcRoles ::    [String]
                     , ifcObj ::      ObjectDef -- NOTE: this top-level ObjectDef is contains the interface itself (ie. name and expression)
                     , ifcEcas ::     [ECArule]
                     , ifcControls :: [Conjunct]
                     , ifcPos ::      Origin
                     , ifcPrp ::      String
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
     Just (InterfaceRef _) -> []
     Just (Box _ _ objs)     -> objs

class Object a where
 concept :: a -> A_Concept        -- the type of the object
 attributes :: a -> [ObjectDef]   -- the objects defined within the object
 contextOf :: a -> Expression     -- the context expression

instance Object ObjectDef where
 concept obj = target (objctx obj)
 attributes  = objAts
 contextOf   = objctx

data ObjectDef = Obj { objnm ::   String         -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
                     , objpos ::  Origin         -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number)
                     , objctx ::  Expression     -- ^ this expression describes the instances of this object, related to their context.
                     , objmsub :: Maybe SubInterface    -- ^ the attributes, which are object definitions themselves.
                     , objstrs :: [[String]]     -- ^ directives that specify the interface.
                     } deriving (Eq, Show)       -- just for debugging (zie ook instance Show ObjectDef)
instance Named ObjectDef where
  name   = objnm
instance Traced ObjectDef where
  origin = objpos

data SubInterface = Box A_Concept (Maybe String)[ObjectDef] | InterfaceRef String deriving (Eq, Show)

data InsDel   = Ins | Del
                 deriving (Show,Eq)
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
                    , paCl :: String->PAclause            -- to be done after creating the concept
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | Rmv { paCpt :: A_Concept                  -- Remove an instance of type c
                    , paCl :: String->PAclause            -- to be done afteremoving the concept
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

instance Eq PAclause where
   CHC ds _ == CHC ds' _ = ds==ds'
   GCH ds _ == GCH ds' _ = ds==ds'
   ALL ds _ == ALL ds' _ = ds==ds'
   p@Do{}   ==   p'@Do{} = paSrt p==paSrt p' && paTo p==paTo p' && paDelta p==paDelta p'
   Nop _    ==     Nop _ = True
   p@New{}  ==  p'@New{} = paCpt p==paCpt p'
   p@Rmv{}  ==  p'@Rmv{} = paCpt p==paCpt p'
   _ == _ = False

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
  = PRelPopu { popdcl :: Declaration
             , popps ::  Pairs     -- The user-defined pairs that populate the relation
             }
  | PCptPopu { popcpt :: A_Concept
             , popas ::  [String]  -- The user-defined atoms that populate the concept
             } deriving (Show, Eq)

data ExplObj = ExplConceptDef ConceptDef
             | ExplDeclaration Declaration
             | ExplRule String
             | ExplIdentityDef String
             | ExplViewDef String
             | ExplPattern String
             | ExplProcess String
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
     (ExplProcess s)     -> "a Process named "++s
     (ExplInterface s)   -> "an Interface named "++s
     (ExplContext s)     -> "a Context named "++s
     
data Expression
      = EEqu (Expression,Expression)   -- ^ equivalence             =
      | EImp (Expression,Expression)   -- ^ implication             |-
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
      | EEps A_Concept Sign            -- ^ Epsilon relation (introduced by the system to ensure we compare concepts by equality only.
      | EDcV Sign                      -- ^ Cartesian product relation
      | EMp1 String A_Concept          -- ^ constant (string between single quotes)
      deriving (Eq, Prelude.Ord, Show, Typeable)
instance Unique Expression where
  showUnique = show
(.==.), (.|-.), (./\.), (.\/.), (.-.), (./.), (.\.), (.<>.), (.:.), (.!.), (.*.) :: Expression -> Expression -> Expression

infixl 1 .==.   -- equivalence
infixl 1 .|-.   -- implication
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
           EImp (l,r)
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
               EImp (l,r) -> EImp (flp l, flp r)
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
 sign (EImp (l,r)) = Sign (source l) (target r)
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

-- The following definition of concept is used in the type checker only.
-- It is called Concept, meaning "type checking concept"

data A_Concept
   = PlainConcept { cptnm :: String  -- ^PlainConcept nm represents the set of instances cs by name nm.
                  , cpttp :: String  -- HJO, 20150204 TODO: add sqlTypeOf. It is required for the meatgrinder. (zit nu ver weg gestopt bij het maken van plugs. 
                  }
   | ONE  -- ^The universal Singleton: 'I'['Anything'] = 'V'['Anything'*'Anything']
    deriving (Prelude.Ord,Typeable)

instance Eq A_Concept where
   PlainConcept{cptnm=a} == PlainConcept{cptnm=b} = a==b
   ONE == ONE = True
   _ == _ = False
instance Unique A_Concept where
  showUnique = name

instance Named A_Concept where
  name PlainConcept{cptnm = nm} = nm
  name ONE = "ONE"

instance Show A_Concept where
  showsPrec _ c = showString (name c)

data Sign = Sign A_Concept A_Concept deriving (Eq, Prelude.Ord, Typeable)

instance Show Sign where
  showsPrec _ (Sign s t) =
     showString (   "[" ++ show s ++ "*" ++ show t ++ "]" )
instance Unique Sign where
  showUnique (Sign s t) = "[" ++ uniqueShow False s ++ "*" ++ uniqueShow False t ++ "]"
instance Association Sign where
  source (Sign s _) = s
  target (Sign _ t) = t
  sign sgn = sgn

instance Flippable Sign where
 flp (Sign s t) = Sign t s

class Association rel where
  source, target :: rel -> A_Concept      -- e.g. Declaration -> Concept
  source x        = source (sign x)
  target x        = target (sign x)
  sign :: rel -> Sign
  isEndo :: rel  -> Bool
  isEndo s        = source s == target s

{-
  --  a <= b means that concept a is more specific than b and b is more generic than a. For instance 'Elephant' <= 'Animal'
  --  The generalization relation <= between concepts is a partial order.
  --  Partiality reflects the fact that not every pair of concepts of a specification need be related.
  --  Although meets, joins and sorting of all concepts may be meaningless, within classes of comparable concepts it is meaningfull.
  --  See Core.Poset to see how these functions are defined for the meaningfull cases only.
  --  Core.Poset is based and partly copied from http://hackage.haskell.org/package/altfloat-0.3.1 intended to sort floats and more
  --  A partial order is by definition reflexive, antisymmetric, and transitive
  --  For every concept a and b in Ampersand, the following rule holds: a<b || b<a || a==b || a <==> b || a<\=> b
  --  Every concept drags around the same partial order represented by
  --   + a compare function (A_Concept->A_Concept->Ordering)
  --   + and a list of comparable classes [[A_Concept]]
-}
type GenR = ( A_Concept -> A_Concept -> Ordering      --  gE: the ordering relation, which yields EQ, LT, GT, CP, or NC
            , [[A_Concept]]                           --  join classes. Each class corresponds to a (scalar, binary or wide) entity later on in the database generator.
            , [(A_Concept,A_Concept)]                 --  the smallest set of pairs that produces the ordering relation gE
            , A_Concept -> A_Concept -> [A_Concept]   --  c `elem` (a `meets` b) means that c<=q and c<=b
            , A_Concept -> A_Concept -> [A_Concept]   --  c `elem` (a `joins` b) means that c>=q and c>=b
            )

{- not used, but may be handy for debugging
showOrder :: A_Concept -> String
showOrder x = "\nComparability classes:"++ind++intercalate ind (map show classes)
 where (_,classes,_,_,_) = cptgE x; ind = "\n   "
-}

  
