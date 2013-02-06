{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
module DatabaseDesign.Ampersand.Core.AbstractSyntaxTree (
   A_Context(..)
 , Meta(..)
 , Theme(..)
 , Process(..)
 , Pattern(..)
 , PairView(..)
 , PairViewSegment(..)
 , Rule(..)
 , RuleType(..)
 , RuleOrigin(..)
-- , RelConceptDef(..)
 , Declaration(..)
 , KeyDef(..)
 , KeySegment(..)
 , A_Gen(..)
 , Interface(..)
 , SubInterface(..)
 , ObjectDef(..)
 , objAts
 , objatsLegacy -- for use in legacy code only
 , Purpose(..)
 , ExplObj(..)
 , Expression(..), iExpr, vExpr
 , Relation(..)
 , A_Concept(..)
 , A_Markup(..)
 , AMeaning(..)
 , RoleRelation(..)
 , Sign(..)
 , UserDefPop(..)
 , GenR
 , Signaling(..)
 , Association(..)
  -- (Poset.<=) is not exported because it requires hiding/qualifying the Prelude.<= or Poset.<= too much
  -- import directly from DatabaseDesign.Ampersand.Core.Poset when needed
 , (<==>),join,meet,greatest,least,maxima,minima,sortWith 
 , makeDeclaration
 , showSign
 , aMarkup2String
 , insParentheses
 , module DatabaseDesign.Ampersand.Core.ParseTree  -- export all used contstructors of the parsetree, because they have actually become part of the Abstract Syntax Tree.
 , (.==.), (.|-.), (./\.), (.\/.), (.-.), (./.), (.\.), (.:.), (.!.), (.*.)
)where
import qualified Prelude
import Prelude hiding (Ord(..), Ordering(..))
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Core.ParseTree   (MetaObj(..),ConceptDef,Origin(..),Traced(..),Prop,Lang,Pairs, PandocFormat, P_Markup(..), PMeaning(..), SrcOrTgt(..), isSrc, RelConceptDef(..))
import DatabaseDesign.Ampersand.Core.Poset (Poset(..), Sortable(..),Ordering(..),comparableClass,greatest,least,maxima,minima,sortWith)
import DatabaseDesign.Ampersand.Misc
import Text.Pandoc hiding (Meta)
import Data.List

fatal :: Int -> String -> a
fatal = fatalMsg "AbstractSyntaxTree.hs"

data A_Context
   = ACtx{ ctxnm :: String           -- ^ The name of this context
         , ctxpos :: [Origin]        -- ^ The origin of the context. A context can be a merge of a file including other files c.q. a list of Origin.
         , ctxlang :: Lang           -- ^ The default language used in this context.
         , ctxmarkup :: PandocFormat -- ^ The default markup format for free text in this context (currently: LaTeX, ...)
         , ctxthms :: [String]       -- ^ Names of patterns/processes to be printed in the functional specification. (For partial documents.)
         , ctxpo :: GenR             -- ^ A tuple representing the partial order of concepts (see makePartialOrder)
         , ctxpats :: [Pattern]      -- ^ The patterns defined in this context
         , ctxprocs :: [Process]     -- ^ The processes defined in this context
         , ctxrs :: [Rule]           -- ^ All user defined rules in this context, but outside patterns and outside processes
         , ctxds :: [Declaration]    -- ^ The declarations defined in this context, outside the scope of patterns
         , ctxpopus :: [UserDefPop]  -- ^ The user defined populations of relations defined in this context, including those from patterns and processes
         , ctxcds :: [ConceptDef]    -- ^ The concept definitions defined in this context, including those from patterns and processes
         , ctxks :: [KeyDef]         -- ^ The key definitions defined in this context, outside the scope of patterns
         , ctxgs :: [A_Gen]          -- ^ The specialization statements defined in this context, outside the scope of patterns
         , ctxifcs :: [Interface]    -- ^ The interfaces defined in this context, outside the scope of patterns
         , ctxps :: [Purpose]        -- ^ The purposes of objects defined in this context, outside the scope of patterns
         , ctxsql :: [ObjectDef]     -- ^ user defined sqlplugs, taken from the Ampersand script
         , ctxphp :: [ObjectDef]     -- ^ user defined phpplugs, taken from the Ampersand script
         , ctxenv :: (Expression,[(Declaration,String)]) -- ^ an expression on the context with unbound relations, to be bound in this environment
         , ctxmetas :: [Meta]
         }               --deriving (Show) -- voor debugging
instance Show A_Context where
  showsPrec _ c = showString (ctxnm c)
instance Eq A_Context where
  c1 == c2  =  name c1 == name c2
instance Identified A_Context where
  name  = ctxnm 

-- for declaring name/value pairs with information that is built in to the adl syntax yet
data Meta = Meta { mtPos :: Origin
                 , mtObj :: MetaObj
                 , mtName :: String
                 , mtVal :: String
                 } deriving (Show)

data Theme = PatternTheme Pattern | ProcessTheme Process

instance Identified Theme where
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
                    , prcUps :: [UserDefPop]  -- ^ The user defined populations in this process
                    , prcRRuls :: [(String,Rule)]    -- ^ The assignment of roles to rules.
                    , prcRRels :: [(String,Relation)] -- ^ The assignment of roles to Relations.
                    , prcKds :: [KeyDef]            -- ^ The key definitions defined in this process
                    , prcXps :: [Purpose]           -- ^ The motivations of elements defined in this process
                    }
instance Identified Process where
  name = prcNm

instance Traced Process where
  origin = prcPos

data RoleRelation
   = RR { rrRoles :: [String]     -- ^ name of a role
        , rrRels :: [Relation]   -- ^ name of a Relation
        , rrPos :: Origin       -- ^ position in the Ampersand script
        } deriving (Eq, Show)     -- just for debugging
instance Traced RoleRelation where
   origin = rrPos
    


data Pattern
   = A_Pat { ptnm :: String        -- ^ Name of this pattern
           , ptpos :: Origin        -- ^ the position in the file in which this pattern was declared.
           , ptend :: Origin        -- ^ the end position in the file, elements with a position between pos and end are elements of this pattern.
           , ptrls :: [Rule]        -- ^ The user defined rules in this pattern
           , ptgns :: [A_Gen]       -- ^ The generalizations defined in this pattern
           , ptdcs :: [Declaration] -- ^ The declarations declared in this pattern
           , ptups :: [UserDefPop]  -- ^ The user defined populations in this pattern
           , ptkds :: [KeyDef]      -- ^ The key definitions defined in this pattern
           , ptxps :: [Purpose]     -- ^ The purposes of elements defined in this pattern
           }   --deriving (Show)    -- for debugging purposes
instance Identified Pattern where
 name = ptnm
instance Traced Pattern where
 origin = ptpos

data A_Markup =
    A_Markup { amLang :: Lang
             , amFormat :: PandocFormat
             , amPandoc :: [Block]
             } deriving Show

data PairView = PairView [PairViewSegment] deriving Show

data PairViewSegment = PairViewText String
                     | PairViewExp SrcOrTgt Expression deriving Show

data RuleOrigin = UserDefined     -- This rule was specified explicitly as a rule in the Ampersand script
                | Multiplicity    -- This rule follows implicitly from the Ampersand script (Because of a property) and generated by a computer
                | Key             -- This rule follows implicitly from the Ampersand script (Because of a key) and generated by a computer
                deriving (Show, Eq)
data Rule =
     Ru { rrnm :: String                  -- ^ Name of this rule
        , rrexp :: Expression              -- ^ The rule expression
        , rrfps :: Origin                  -- ^ Position in the Ampersand file
        , rrmean :: AMeaning                -- ^ Ampersand generated meaning (for all known languages)
        , rrmsg :: [A_Markup]              -- ^ User-specified violation messages, possibly more than one, for multiple languages.
        , rrviol :: Maybe PairView          -- ^ Custom presentation for violations, currently only in a single language
        , rrtyp :: Sign                    -- ^ Allocated type
        , rrdcl :: Maybe (Prop,Declaration)  -- ^ The property, if this rule originates from a property on a Declaration
        , r_env :: String                  -- ^ Name of pattern in which it was defined.
        , r_usr :: RuleOrigin              -- ^ Where does this rule come from?
        , r_sgl :: Bool                    -- ^ True if this is a signal; False if it is an invariant
        , srrel :: Declaration             -- ^ the signal relation
        }
instance Eq Rule where
  r==r' = rrnm r==rrnm r'
instance Show Rule where
  showsPrec _ x
   = showString $ "RULE "++ (if null (name x) then "" else name x++": ")++ show (rrexp x)
instance Traced Rule where
  origin = rrfps
instance Identified Rule where
  name   = rrnm
instance Association Rule where
  sign   = rrtyp
instance Signaling Rule where
  isSignal = r_sgl

data RuleType = Implication | Equivalence | Truth  deriving (Eq,Show)

data Declaration = 
  Sgn { decnm :: String     -- ^ the name of the declaration
      , decsgn :: Sign       -- ^ the source concept of the declaration
       --multiplicities returns decprps_calc so if you only need the user defined properties do not use multiplicities but decprps
      , decprps :: [Prop]     -- ^ the user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
      , decprps_calc :: [Prop]-- ^ the calculated and user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx, Irf). Note that calculated properties are made by adl2fspec, so in the A-structure decprps and decprps_calc yield exactly the same answer.
      , decprL :: String     -- ^ three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
      , decprM :: String     -- ^    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
      , decprR :: String
      , decMean :: AMeaning   -- ^ the meaning of a declaration, for each language supported by Ampersand.
      , decConceptDef :: Maybe RelConceptDef -- ^ alternative definition for the source or target concept in the context of this relation
 --     , decpopu :: Pairs      -- ^ the list of tuples, of which the relation consists.
      , decfpos :: Origin     -- ^ the position in the Ampersand source file where this declaration is declared. Not all decalartions come from the ampersand souce file. 
      , deciss :: Bool       -- ^ if true, this is a signal relation; otherwise it is an ordinary relation.
      , decusr :: Bool       -- ^ if true, this relation is declared in the Ampersand script; otherwise it was generated by Ampersand.
      , decpat :: String     -- ^ the pattern where this declaration has been declared.
      , decplug :: Bool       -- ^ if true, this relation may not be stored in or retrieved from the standard database (it should be gotten from a Plug of some sort instead)
      } | 
 Isn 
      { detyp :: A_Concept       -- ^ The type
      } |
 Iscompl 
      { detyp :: A_Concept
      } |
 Vs 
      { decsgn :: Sign
      }
instance Eq Declaration where
  d@Sgn{}     == d'@Sgn{}     = decnm d==decnm d' && decsgn d==decsgn d'
  d@Isn{}     == d'@Isn{}     = detyp d==detyp d'
  d@Iscompl{} == d'@Iscompl{} = detyp d==detyp d'
  d@Vs{}      == d'@Vs{}      = decsgn d==decsgn d'
  _           == _            = False
instance Show Declaration where
  showsPrec _ d
    = showString (unwords (["RELATION",decnm d,show (decsgn d),show (decprps_calc d)
                           ,"PRAGMA",show (decprL d),show (decprM d),show (decprR d)]
                            ++concatMap showMeaning (ameaMrk (decMean d))
                 )        )
           where 
              showMeaning m = "MEANING"
                             : ["IN", show (amLang m)]
                            ++ [show (amFormat m)]
                            ++ ["{+",aMarkup2String m,"-}"]                
                            -- then [] else ["MEANING",show (decMean d)] ))

instance Flippable Declaration where
  flp d
    = case d of
           Sgn {} -> d{ decsgn  = flp (decsgn d)
                      , decprps = map flp (decprps d)
                      , decprps_calc = map flp (decprps_calc d)
                      , decprL  = ""
                      , decprM  = ""
                      , decprR  = ""
                --      , decpopu = map swap (decpopu d)
                      }
           Vs {}  -> d{ decsgn  = flp (decsgn d) }
           _      -> d



aMarkup2String :: A_Markup -> String
aMarkup2String a = blocks2String (amFormat a) False (amPandoc a)

data AMeaning = AMeaning { ameaMrk ::[A_Markup]} deriving Show

instance Identified Declaration where
  name d@Sgn{}   = decnm d
  name Isn{}     = "I"
  name Iscompl{} = "-I"
  name Vs{}      = "V"
instance Association Declaration where
  sign d = case d of
              Sgn {}    -> decsgn d
              Isn {}    -> Sign (detyp d) (detyp d)
              Iscompl{} -> Sign (detyp d) (detyp d)
              Vs {}     -> decsgn d
instance Traced Declaration where
  origin d = case d of
              Sgn{}     -> decfpos d
              _         -> OriginUnknown

data KeyDef = Kd { kdpos :: Origin       -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number).
                 , kdlbl :: String       -- ^ the name (or label) of this Key. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface. It is not an empty string.
                 , kdcpt :: A_Concept    -- ^ this expression describes the instances of this object, related to their context
                 , kdats :: [KeySegment]  -- ^ the constituent attributes (i.e. name/expression pairs) of this key.
                 } deriving (Eq,Show)
instance Identified KeyDef where
  name = kdlbl
instance Traced KeyDef where
  origin = kdpos

data KeySegment = KeyExp ObjectDef | KeyText String | KeyHtml String deriving (Eq, Show)

data A_Gen = Gen { genfp :: Origin         -- ^ the position of the GEN-rule
                 , gengen :: A_Concept      -- ^ generic concept
                 , genspc :: A_Concept      -- ^ specific concept
                 , genpat :: String         -- ^ pattern of declaration
                 }
instance Eq A_Gen where
  g == g' = gengen g == gengen g' && genspc g == genspc g'
instance Show A_Gen where
  -- This show is used in error messages. It should therefore not display the term's type
  showsPrec _ g = showString ("SPEC "++show (genspc g)++" ISA "++show (gengen g))
instance Traced A_Gen where
  origin = genfp
instance Association A_Gen where
  sign r = Sign (genspc r) (gengen r)


data Interface = Ifc { ifcParams :: [Relation]
                     , ifcViols ::  [Rule]
                     , ifcArgs ::   [[String]]
                     , ifcRoles ::  [String]
                     , ifcObj ::    ObjectDef -- NOTE: this top-level ObjectDef is contains the interface itself (ie. name and expression)
                     , ifcPos ::    Origin
                     , ifcPrp ::    String
                     } deriving Show
instance Eq Interface where
  s==s' = name s==name s'
instance Identified Interface where
  name = name . ifcObj
instance Traced Interface where
  origin = ifcPos

objAts :: ObjectDef -> [ObjectDef]
objAts Obj{ objmsub=Nothing } = []
objAts Obj{ objmsub=Just (InterfaceRef _) } = []
objAts Obj{ objmsub=Just (Box objs) } = objs

objatsLegacy :: ObjectDef -> [ObjectDef]
objatsLegacy Obj{ objmsub=Nothing } = []
objatsLegacy Obj{ objmsub=Just (Box objs) } = objs
objatsLegacy Obj{ objmsub=Just (InterfaceRef _) } = fatal 301 $ "Using functionality that has not been extended to InterfaceRefs"

data ObjectDef = Obj { objnm ::   String         -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
                     , objpos ::  Origin         -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number)
                     , objctx ::  Expression     -- ^ this expression describes the instances of this object, related to their context. 
                     , objmsub :: Maybe SubInterface    -- ^ the attributes, which are object definitions themselves.
                     , objstrs :: [[String]]     -- ^ directives that specify the interface.
                     } deriving (Eq, Show)       -- just for debugging (zie ook instance Show ObjectDef)
instance Identified ObjectDef where
  name   = objnm
instance Traced ObjectDef where
  origin = objpos

data SubInterface = Box [ObjectDef] | InterfaceRef String deriving (Eq, Show) 

-- | Explanation is the intended constructor. It explains the purpose of the object it references.
--   The enrichment process of the parser must map the names (from PPurpose) to the actual objects
data Purpose  = Expl { explPos :: Origin     -- ^ The position in the Ampersand script of this purpose definition
                     , explObj :: ExplObj    -- ^ The object that is explained.
                     , explMarkup :: A_Markup   -- ^ This field contains the text of the explanation including language and markup info.
                     , explUserdefd :: Bool       -- ^ Is this purpose defined in the script?
                     , explRefId :: String     -- ^ The reference of the explaination
                     } 
instance Eq Purpose where
  x0 == x1  =  explObj x0 == explObj x1 && 
               (amLang . explMarkup) x0 == (amLang . explMarkup) x1
instance Traced Purpose where
  origin = explPos

data UserDefPop -- The user defined populations
  = PRelPopu { popdcl :: Declaration
             , popps ::  Pairs     -- The user-defined pairs that populate the relation
             }
  | PCptPopu { popcpt :: A_Concept
             , popas ::  [String]  -- The user-defined atoms that populate the concept
             }

data ExplObj = ExplConceptDef ConceptDef
             | ExplDeclaration Declaration
             | ExplRule String
             | ExplKeyDef String
             | ExplPattern String
             | ExplProcess String
             | ExplInterface String
             | ExplContext String
             | ExplFspc String
          deriving (Show ,Eq)

data Expression
      = EEqu (Expression,Expression) Sign  -- ^ equivalence             =
      | EImp (Expression,Expression) Sign  -- ^ implication             |-
      | EIsc (Expression,Expression) Sign  -- ^ intersection            /\
      | EUni (Expression,Expression) Sign  -- ^ union                   \/
      | EDif (Expression,Expression) Sign  -- ^ difference              -
      | ELrs (Expression,Expression) Sign  -- ^ left residual           /
      | ERrs (Expression,Expression) Sign  -- ^ right residual          \
      | ECps (Expression,Expression) Sign  -- ^ composition             ; 
      | ERad (Expression,Expression) Sign  -- ^ relative addition       ! 
      | EPrd (Expression,Expression) Sign  -- ^ cartesian product       * 
      | EKl0 Expression              Sign  -- ^ Rfx.Trn closure         *  (Kleene star)
      | EKl1 Expression              Sign  -- ^ Transitive closure      +  (Kleene plus)
      | EFlp Expression              Sign  -- ^ conversion (flip, wok)  ~
      | ECpl Expression              Sign  -- ^ Complement
      | EBrk Expression                    -- ^ bracketed expression ( ... )
      | ETyp Expression              Sign  -- ^ type cast expression ... [c] (defined tuple instead of list because ETyp only exists for actual casts)
      | ERel Relation                Sign  -- ^ simple relation
      deriving (Eq, Show)

iExpr :: A_Concept -> Expression
iExpr  c = ERel (I c) (Sign c c)
vExpr :: Sign -> Expression
vExpr sgn = ERel (V sgn) sgn

(.==.), (.|-.), (./\.), (.\/.), (.-.), (./.), (.\.), (.:.), (.!.), (.*.) :: Expression -> Expression -> Expression

infixl 1 .==.   -- equivalence
infixl 1 .|-.   -- implication
infixl 2 ./\.   -- intersection
infixl 2 .\/.   -- union
infixl 4 .-.    -- difference
infixl 6 ./.    -- left residual
infixl 6 .\.    -- right residual
infixl 8 .:.    -- composition    -- .;. was unavailable, because Haskells scanner does not recognize it as an operator.
infixl 8 .!.    -- relative addition
infixl 8 .*.    -- cartesian product

l .==. r = EEqu (l,r)
           (case sign l `compare` sign r of
              EQ -> sign l
              _  -> fatal 417 ("Equality between two terms of different types.\n  l: "++show l++"\t  signature: "++show (sign l)++"\n  r: "++show r++"\t  signature: "++show (sign r)++"\n  "++show (sign l)++" `compare` "++show (sign r)++": "++show (sign l `compare` sign r))
           )
l .|-. r = EImp (l,r)
           (case sign l `compare` sign (r./\.l) of
              EQ -> sign (l.==.r./\.l)
              _  -> fatal 422 ("Implication between two terms is type incompatible.\n  l: "++show l++"\t  signature: "++show (sign l)++"\n  r: "++show r++"\t  signature: "++show (sign r)++"\n  "++show (sign l)++" `compare` "++show (sign r)++": "++show (sign l `compare` sign r))
           )
l ./\. r = EIsc (l,r)
           (case sign l `compare` sign r of        -- meet yields the more specific of two concepts
              NC -> fatal 426 ("Intersection of two incompatible terms.\n  l: "++show l++"\t  signature: "++show (sign l)++"\n  r: "++show r++"\t  signature: "++show (sign r)++"\n  "++show (sign l)++" `compare` "++show (sign r)++": "++show (sign l `compare` sign r))
              _  -> sign l `meet` sign r
           )
l .\/. r = EUni (l,r)
           (case sign l `compare` sign r of        -- join yields the more generic of two concepts
              NC -> fatal 432 ("Union of two incompatible terms.\n  l: "++show l++"\t  signature: "++show (sign l)++"\n  r: "++show r++"\t  signature: "++show (sign r)++"\n  "++show (sign l)++" `compare` "++show (sign r)++": "++show (sign l `compare` sign r))
              _  -> sign l `join` sign r
           )
l .-. r  = EDif (l,r) (sign l)
l ./. r  = ELrs (l,r) (Sign (source l) (source r))
l .\. r  = ERrs (l,r) (Sign (target l) (target r))
l .:. r  = ECps (l,r) (Sign (source l) (target r))
l .!. r  = ERad (l,r) (Sign (source l) (target r))
l .*. r  = EPrd (l,r) (Sign (source l) (target r))
{- For the operators /, \, ;, ! and * we must not check whether the intermediate types exist.
   Suppose the user says GEN Student ISA Person and GEN Employee ISA Person, then Student `join` Employee has a name (i.e. Person), but Student `meet` Employee
   does not. In that case, -(r!s) (with target r=Student and source s=Employee) is defined, but -r;-s is not.
   So in order to let -(r!s) be equal to -r;-s we must not check for the existence of these types, for the Rotterdam paper already shows that this is fine.
-}

instance Flippable Expression where
  flp expr = case expr of
               EEqu (l,r) sgn -> EEqu (flp l, flp r) (flp sgn)
               EImp (l,r) sgn -> EImp (flp l, flp r) (flp sgn)
               EIsc (l,r) sgn -> EIsc (flp l, flp r) (flp sgn)
               EUni (l,r) sgn -> EUni (flp l, flp r) (flp sgn)
               EDif (l,r) sgn -> EDif (flp l, flp r) (flp sgn)
               ELrs (l,r) sgn -> ERrs (flp r, flp l) (flp sgn)
               ERrs (l,r) sgn -> ELrs (flp r, flp l) (flp sgn)
               ECps (l,r) sgn -> ECps (flp r, flp l) (flp sgn)
               ERad (l,r) sgn -> ERad (flp r, flp l) (flp sgn)
               EPrd (l,r) sgn -> EPrd (flp r, flp l) (flp sgn)
               EFlp e     _   -> e
               ECpl e     sgn -> ECpl (flp e) (flp sgn)
               EKl0 e     sgn -> EKl0 (flp e) (flp sgn)
               EKl1 e     sgn -> EKl1 (flp e) (flp sgn)
               EBrk f         -> EBrk (flp f)
               ETyp e     sgn -> ETyp (flp e) (flp sgn)
               e@(ERel _ sgn) -> EFlp e (flp sgn)

insParentheses :: Expression -> Expression
insParentheses = insPar 0
      where
       wrap :: Integer -> Integer -> Expression -> Expression
       wrap i j e' = if i<=j then e' else EBrk e'
       insPar :: Integer -> Expression -> Expression
       insPar i (EEqu (l,r) sgn) = wrap i     0 (EEqu (insPar 1 l, insPar 1 r) sgn)
       insPar i (EImp (l,r) sgn) = wrap i     0 (EImp (insPar 1 l, insPar 1 r) sgn)
       insPar i (EUni (l,r) sgn) = wrap (i+1) 2 (EUni (insPar 2 l, insPar 2 r) sgn)
       insPar i (EIsc (l,r) sgn) = wrap (i+1) 2 (EIsc (insPar 2 l, insPar 2 r) sgn)
       insPar i (EDif (l,r) sgn) = wrap i     4 (EDif (insPar 5 l, insPar 5 r) sgn)
       insPar i (ELrs (l,r) sgn) = wrap i     6 (ELrs (insPar 7 l, insPar 7 r) sgn)
       insPar i (ERrs (l,r) sgn) = wrap i     6 (ERrs (insPar 7 l, insPar 7 r) sgn)
       insPar i (ECps (l,r) sgn) = wrap (i+1) 8 (ECps (insPar 8 l, insPar 8 r) sgn)
       insPar i (ERad (l,r) sgn) = wrap (i+1) 8 (ERad (insPar 8 l, insPar 8 r) sgn)
       insPar i (EPrd (l,r) sgn) = wrap (i+1) 8 (EPrd (insPar 8 l, insPar 8 r) sgn)
       insPar _ (EKl0 e     sgn) = EKl0 (insPar 10 e) sgn
       insPar _ (EKl1 e     sgn) = EKl1 (insPar 10 e) sgn
       insPar _ (EFlp e     sgn) = EFlp (insPar 10 e) sgn
       insPar _ (ECpl e     sgn) = ECpl (insPar 10 e) sgn
       insPar i (EBrk f)         = insPar i f
       insPar _ (ETyp e sgn)     = ETyp (insPar 10 e) sgn
       insPar _ e@ERel{}         = e

instance Association Expression where
 sign (EEqu _ sgn) = sgn
 sign (EImp _ sgn) = sgn
 sign (EIsc _ sgn) = sgn
 sign (EUni _ sgn) = sgn
 sign (EDif _ sgn) = sgn
 sign (ELrs _ sgn) = sgn
 sign (ERrs _ sgn) = sgn
 sign (ECps _ sgn) = sgn
 sign (ERad _ sgn) = sgn
 sign (EPrd _ sgn) = sgn
 sign (EKl0 _ sgn) = sgn
 sign (EKl1 _ sgn) = sgn
 sign (EFlp _ sgn) = sgn
 sign (ECpl _ sgn) = sgn
 sign (EBrk e)     = sign e
 sign (ETyp _ sgn) = sgn
 sign (ERel _ sgn) = sgn


data Relation = 
  Rel { relnm ::   String           -- ^ the name of the relation. This is the same name as the name of reldcl.
                                    --    WHY: Why is relnm needed? After all, its name is always equal to the name of the Declaration reldcl ....
                                    --    BECAUSE: When parsing, until the declaration has been assigned to the relation, the name must be known. Only after binding, the name is equal to the name of reldcl.
      , relpos ::  Origin           -- ^ the position in the Ampersand source file. Let rel_pos be Nowhere if not applicable e.g. relations in generated rules
      , reldcl ::  Declaration      -- ^ the declaration bound to this relation.
      } |
 I    { rel1typ :: A_Concept        -- ^ the allocated type.
      } |
 V    { reltyp ::  Sign             -- ^ the allocated type.
      } |
 --   An Mp1 is a subset of I. Shouldn't we replace it by an I?
 Mp1  { relval ::  String           -- ^ the value of the singleton relation
--      , rel1typ :: A_Concept               -- ^ the allocated type.
      } 
instance Eq Relation where
 rel == rel' 
   = case (rel,rel') of
       (Rel{},Rel{}) -> relnm   rel==relnm   rel'
       (I{}  ,I{}  ) -> rel1typ rel==rel1typ rel'
       (V{}  ,V{}  ) -> reltyp  rel==reltyp  rel'
       (Mp1{},Mp1{}) -> relval  rel==relval  rel'
       (_    ,_    ) -> False
       
instance Show Relation where
 showsPrec _ r = case r of
   Rel{} -> showString (name r++showSign [source r,target r])
   I{}   -> showString (name r++"["++show (rel1typ r)++"]")
   V{}   -> showString (name r++show (sign r))
   Mp1{} -> showString ("'"++relval r++"'")
instance Identified Relation where
  name r = name (makeDeclaration r)
instance Association Relation where
  sign r =
    case r of 
      Rel{}   -> sign (reldcl r)
      I{}     -> Sign (rel1typ r) (rel1typ r)
      V{}     -> reltyp r
      Mp1{}   -> fatal 556 "Mp1 has no signature..."
makeDeclaration :: Relation -> Declaration
makeDeclaration r = case r of
      Rel{} -> reldcl r
      I{}   -> Isn{ detyp = rel1typ r}
      V{}   -> Vs { decsgn = sign r}
      Mp1{} -> fatal 556 "Mp1 has no declaration..."
showSign :: Identified a => [a] -> String
showSign cs = "["++(intercalate "*".nub.map name) cs++"]"
instance Traced Relation where
 origin = relpos

-- The following definition of concept is used in the type checker only.
-- It is called Concept, meaning "type checking concept"

data A_Concept
   = C   { cptnm :: String         -- ^The name of this Concept
         , cptgE :: GenR           -- ^This contains the generalization relation between concepts.
                                   --  It is included in every concept, for the purpose of comparing concepts in the Ord class.
                                   --  As a result, you may write  c<=d  in your Haskell code for any two A_Concepts c and d that are in the same context.
         , cpttp :: String         -- ^The type of this Concept
         , cptdf :: [ConceptDef]   -- ^Concept definitions of this concept.
         }  -- ^C nm gE cs represents the set of instances cs by name nm.
   | ONE  -- ^The universal Singleton: 'I'['Anything'] = 'V'['Anything'*'Anything']


instance Eq A_Concept where
   a@C{} == b@C{} = name a==name b
   ONE == ONE = True
   _ == _ = False

instance Identified A_Concept where
  name (C {cptnm = nm}) = nm
  name ONE = "ONE"

instance Show A_Concept where
  showsPrec _ c = showString (name c)
   
  
data Sign = Sign A_Concept A_Concept deriving Eq
  
instance Poset Sign where
  Sign s t `compare` Sign s' t'
   = case (s `compare` s', t `compare` t') of
      (EQ, EQ)  -> EQ
      (LT, EQ)  -> LT
      (GT, EQ)  -> GT
      (EQ, LT)  -> LT
      (EQ, GT)  -> GT
      (LT, LT)  -> LT
      (GT, GT)  -> GT
      (NC, _ )  -> NC
      (_ , NC)  -> NC
      (CP, _ )  -> CP
      (_ , CP)  -> CP
      _         -> NC
{- This has the following effect:
   | s==s' && t==t' = EQ
   | s<=s' && t<=t' = LT
   | s'<=s && t'<=t = GT
   | s<==>s' && t<==>t' = CP
   | otherwise = NC
-}
instance Sortable Sign where
  meet (Sign a b) (Sign a' b') = Sign (a `meet` a') (b `meet` b')
  join (Sign a b) (Sign a' b') = Sign (a `join` a') (b `join` b')
  sortBy f = Data.List.sortBy ((comparableClass .) . f)  -- is this implementation correct?

instance Show Sign where
  showsPrec _ (Sign s t) = 
     showString (   "[" ++ show s ++ "*" ++ show t ++ "]" )

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

class Signaling a where
  isSignal :: a -> Bool  -- > tells whether the argument refers to a signal
    

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
type GenR = ( A_Concept -> A_Concept -> Ordering      -- ^ gE: the ordering relation, which yields EQ, LT, GT, CP, or NC
            , [[A_Concept]]                           -- ^ join classes. Each class corresponds to a (scalar, binary or wide) entity later on in the database generator.
            , [(A_Concept,A_Concept)]                 -- ^ the smallest set of pairs that produces the ordering relation gE
            , A_Concept -> A_Concept -> [A_Concept]   -- ^ c `elem` (a `meets` b) means that c<=q and c<=b 
            , A_Concept -> A_Concept -> [A_Concept]   -- ^ c `elem` (a `joins` b) means that c>=q and c>=b 
            )

instance Poset A_Concept where
  ONE `compare` ONE = EQ
  _ `compare` ONE   = NC
  ONE `compare` _   = NC
  a `compare` b     = a `comp` b
                      where (comp,_,_,_,_) = cptgE a -- the second element contains sets of concepts, each of which represents a class of comparable concepts.

instance Sortable A_Concept where
  meet a b = case a `meets` b of        -- meet yields the more specific of two concepts
              [z] -> z
              []  -> fatal 671 ("meet may not be applied to " ++ show a ++ " and "++show b++", because they have no atoms in common.")
              cs  -> greatest cs
             where (_,_,_,meets,_) = cptgE a
  join a b = case a `joins` b of        -- join yields the more generic of two concepts
              [z] -> z
              []  -> fatal 675 ("join may not be applied to " ++ show a ++ " and "++show b++", because they have no atoms in common.")
              cs  -> least cs
             where (_,_,_,_,joins) = cptgE a
  sortBy f = Data.List.sortBy ((comparableClass .) . f)

{- not used, but may be handy for debugging
showOrder :: A_Concept -> String
showOrder x = "\nComparability classes:"++ind++intercalate ind (map show classes)
 where (_,classes,_,_,_) = cptgE x; ind = "\n   "
-}
