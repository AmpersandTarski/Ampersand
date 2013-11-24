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
 , Declaration(..), decusr, deciss
 , IdentityDef(..)
 , IdentitySegment(..)
 , ViewDef(..)
 , ViewSegment(..)
 , A_Gen(..)
 , Interface(..)
 , SubInterface(..)
 , ObjectDef(..)
 , objAts
 , objatsLegacy -- for use in legacy code only
 , Purpose(..)
 , ExplObj(..)
 , Expression(..)
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
 , smallerConcepts, largerConcepts
 , showSign
 , aMarkup2String
 , insParentheses
 , module DatabaseDesign.Ampersand.Core.ParseTree  -- export all used contstructors of the parsetree, because they have actually become part of the Abstract Syntax Tree.
 , (.==.), (.|-.), (./\.), (.\/.), (.-.), (./.), (.\.), (.:.), (.!.), (.*.)
)where
import qualified Prelude
import Prelude hiding (Ord(..), Ordering(..))
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Core.ParseTree   (MetaObj(..),Meta(..),ConceptDef,Origin(..),Traced(..),PairView(..),PairViewSegment(..),Prop,Lang,Pairs, PandocFormat, P_Markup(..), PMeaning(..), SrcOrTgt(..), isSrc, RelConceptDef(..))
import DatabaseDesign.Ampersand.Core.Poset (Poset(..), Sortable(..),Ordering(..),greatest,least,maxima,minima,sortWith)
import DatabaseDesign.Ampersand.Misc
import Text.Pandoc hiding (Meta)
import Data.List (intercalate,nub)
fatal :: Int -> String -> a
fatal = fatalMsg "AbstractSyntaxTree.hs"

data A_Context
   = ACtx{ ctxnm :: String           -- ^ The name of this context
         , ctxpos :: [Origin]        -- ^ The origin of the context. A context can be a merge of a file including other files c.q. a list of Origin.
         , ctxlang :: Lang           -- ^ The default language used in this context.
         , ctxmarkup :: PandocFormat -- ^ The default markup format for free text in this context (currently: LaTeX, ...)
         , ctxthms :: [String]       -- ^ Names of patterns/processes to be printed in the functional specification. (For partial documents.)
         , ctxpats :: [Pattern]      -- ^ The patterns defined in this context
         , ctxprocs :: [Process]     -- ^ The processes defined in this context
         , ctxrs :: [Rule]           -- ^ All user defined rules in this context, but outside patterns and outside processes
         , ctxds :: [Declaration]    -- ^ The declarations defined in this context, outside the scope of patterns
         , ctxpopus :: [UserDefPop]  -- ^ The user defined populations of relations defined in this context, including those from patterns and processes
         , ctxcds :: [ConceptDef]    -- ^ The concept definitions defined in this context, including those from patterns and processes
         , ctxks :: [IdentityDef]    -- ^ The identity definitions defined in this context, outside the scope of patterns
         , ctxvs :: [ViewDef]        -- ^ The view definitions defined in this context, outside the scope of patterns
         , ctxgs :: [A_Gen]          -- ^ The specialization statements defined in this context, outside the scope of patterns
         , ctxgenconcs :: [[A_Concept]] -- ^ A partitioning of all concepts: the union of all these concepts contains all atoms, and the concept-lists are mutually distinct in terms of atoms in one of the mentioned concepts
         , ctxifcs :: [Interface]    -- ^ The interfaces defined in this context, outside the scope of patterns
         , ctxps :: [Purpose]        -- ^ The purposes of objects defined in this context, outside the scope of patterns
         , ctxsql :: [ObjectDef]     -- ^ user defined sqlplugs, taken from the Ampersand script
         , ctxphp :: [ObjectDef]     -- ^ user defined phpplugs, taken from the Ampersand script
         , ctxmetas :: [Meta]        -- ^ used for Pandoc authors (and possibly other things)
         }               --deriving (Show) -- voor debugging
instance Show A_Context where
  showsPrec _ c = showString (ctxnm c)
instance Eq A_Context where
  c1 == c2  =  name c1 == name c2
instance Identified A_Context where
  name  = ctxnm 


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
                    , prcRRels :: [(String,Declaration)] -- ^ The assignment of roles to Relations.
                    , prcIds :: [IdentityDef]            -- ^ The identity definitions defined in this process
                    , prcVds :: [ViewDef]            -- ^ The view definitions defined in this process
                    , prcXps :: [Purpose]           -- ^ The motivations of elements defined in this process
                    }
instance Identified Process where
  name = prcNm

instance Traced Process where
  origin = prcPos

data RoleRelation
   = RR { rrRoles :: [String]     -- ^ name of a role
        , rrRels :: [Declaration]   -- ^ name of a Relation
        , rrPos :: Origin       -- ^ position in the Ampersand script
        } --deriving (Eq, Show)     -- just for debugging
instance Traced RoleRelation where
   origin = rrPos
    


data Pattern
   = A_Pat { ptnm :: String         -- ^ Name of this pattern
           , ptpos :: Origin        -- ^ the position in the file in which this pattern was declared.
           , ptend :: Origin        -- ^ the end position in the file, elements with a position between pos and end are elements of this pattern.
           , ptrls :: [Rule]        -- ^ The user defined rules in this pattern
           , ptgns :: [A_Gen]       -- ^ The generalizations defined in this pattern
           , ptdcs :: [Declaration] -- ^ The declarations declared in this pattern
           , ptups :: [UserDefPop]  -- ^ The user defined populations in this pattern
           , ptrruls :: [(String,Rule)]         -- ^ The assignment of roles to rules.
           , ptrrels :: [(String,Declaration)]  -- ^ The assignment of roles to Relations.
           , ptids :: [IdentityDef] -- ^ The identity definitions defined in this pattern
           , ptvds :: [ViewDef]     -- ^ The view definitions defined in this pattern
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
      , decsgn :: Sign       -- ^ the source and target concepts of the declaration
       --multiplicities returns decprps_calc, when it has been calculated. So if you only need the user defined properties do not use multiplicities but decprps
      , decprps :: [Prop]     -- ^ the user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
      , decprps_calc :: Maybe [Prop] -- ^ the calculated and user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx, Irf). Note that calculated properties are made by adl2fspec, so in the A-structure decprps and decprps_calc yield exactly the same answer.
      , decprL :: String     -- ^ three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
      , decprM :: String     -- ^    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
      , decprR :: String
      , decMean :: AMeaning   -- ^ the meaning of a declaration, for each language supported by Ampersand.
      , decConceptDef :: Maybe RelConceptDef -- ^ alternative definition for the source or target concept in the context of this relation
 --     , decpopu :: Pairs      -- ^ the list of tuples, of which the relation consists.
      , decfpos :: Origin     -- ^ the position in the Ampersand source file where this declaration is declared. Not all decalartions come from the ampersand souce file. 
      , decissX :: Bool       -- ^ if true, this is a signal relation; otherwise it is an ordinary relation.
      , decusrX :: Bool       -- ^ if true, this relation is declared by an author in the Ampersand script; otherwise it was generated by Ampersand.
      , decISA ::  Bool      -- ^ if true, this relation is the result of an ISA declaration.
      , decpat :: String     -- ^ the pattern where this declaration has been declared.
      , decplug :: Bool       -- ^ if true, this relation may not be stored in or retrieved from the standard database (it should be gotten from a Plug of some sort instead)
      } | 
 Isn 
      { detyp :: A_Concept       -- ^ The type
      } |
 Vs 
      { decsgn :: Sign
      }
decusr :: Declaration -> Bool
decusr Sgn{decusrX=b}=b
decusr _ = False
deciss :: Declaration -> Bool
deciss Sgn{decissX=b}=b
deciss _ = False

instance Eq Declaration where
  d@Sgn{}     == d'@Sgn{}     = decnm d==decnm d' && decsgn d==decsgn d'
  d@Isn{}     == d'@Isn{}     = detyp d==detyp d'
  d@Vs{}      == d'@Vs{}      = decsgn d==decsgn d'
  _           == _            = False
instance Show Declaration where  -- For debugging purposes only (and fatal messages)
  showsPrec _ d@Sgn{}
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
  showsPrec _ d@Isn{}     = showString $ "Isn{detyp="++show(detyp d)++"}"
  showsPrec _ d@Vs{}      = showString $ "V"++showSign(decsgn d)


aMarkup2String :: A_Markup -> String
aMarkup2String a = blocks2String (amFormat a) False (amPandoc a)

data AMeaning = AMeaning { ameaMrk ::[A_Markup]} deriving Show

instance Identified Declaration where
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
instance Identified IdentityDef where
  name = idLbl
instance Traced IdentityDef where
  origin = idPos

data IdentitySegment = IdentityExp ObjectDef deriving (Eq, Show)  -- TODO: refactor to a list of terms

data ViewDef = Vd { vdpos :: Origin         -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number).
                  , vdlbl :: String         -- ^ the name (or label) of this View. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface. It is not an empty string.
                  , vdcpt :: A_Concept      -- ^ this expression describes the instances of this object, related to their context
                  , vdats :: [ViewSegment]  -- ^ the constituent attributes (i.e. name/expression pairs) of this view.
                  } deriving (Eq,Show)
instance Identified ViewDef where
  name = vdlbl
instance Traced ViewDef where
  origin = vdpos

data ViewSegment = ViewExp ObjectDef | ViewText String | ViewHtml String deriving (Eq, Show)

-- | data structure A_Gen contains the CLASSIFY statements from an Ampersand script
--   CLASSIFY Employee ISA Person   translates to Isa orig (C "Person") (C "Employee")
--   CLASSIFY Workingstudent IS Employee/\Student   translates to IsE orig [C "Employee",C "Student"] (C "Workingstudent")
data A_Gen = Isa { genfp :: Origin          -- ^ the position of the GEN-rule
                 , gengen :: A_Concept      -- ^ generic concept
                 , genspc :: A_Concept      -- ^ specific concept
                 }
           | IsE { genfp :: Origin          -- ^ the position of the GEN-rule
                 , genrhs :: [A_Concept]    -- ^ concepts of which the conjunction is equivalent to the specific concept
                 , genspc :: A_Concept      -- ^ specific concept
                 }
instance Show A_Gen where
  -- This show is used in error messages. It should therefore not display the term's type
  showsPrec _ g@(Isa{}) = showString ("CLASSIFY "++show (genspc g)++" ISA "++show (gengen g))
  showsPrec _ g@(IsE{}) = showString ("CLASSIFY "++show (genspc g)++" IS "++intercalate " /\\ " (map show $ genrhs g))
instance Traced A_Gen where
  origin = genfp
      
-- | this function takes all generalisation relations from the context and a concept and delivers a list of all concepts that are more specific than the given concept.
smallerConcepts :: [A_Gen] -> A_Concept -> [A_Concept]
smallerConcepts gens cpt 
  = nub$ oneSmaller ++ concatMap (smallerConcepts gens) oneSmaller 
  where oneSmaller = nub$[s | Isa _ g   s <- gens , g == cpt]
                       ++[s | IsE _ rhs s <- gens , cpt `elem` rhs]
-- | this function takes all generalisation relations from the context and a concept and delivers a list of all concepts that are more generic than the given concept.
largerConcepts :: [A_Gen] -> A_Concept -> [A_Concept]
largerConcepts gens cpt 
 = nub$ oneLarger ++ concatMap (largerConcepts gens) oneLarger
  where oneLarger  = nub$[g | Isa _ g   s <- gens , s == cpt]
                       ++concat[rhs | IsE _ rhs s <- gens , s == cpt] 
data Interface = Ifc { ifcParams :: [Expression] -- Only primitive expressions are allowed!
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
objAts Obj{ objmsub=Just (Box _ objs) } = objs

objatsLegacy :: ObjectDef -> [ObjectDef]
objatsLegacy Obj{ objmsub=Nothing } = []
objatsLegacy Obj{ objmsub=Just (Box _ objs) } = objs
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

data SubInterface = Box A_Concept [ObjectDef] | InterfaceRef String deriving (Eq, Show) 

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
             | ExplIdentityDef String
             | ExplViewDef String
             | ExplPattern String
             | ExplProcess String
             | ExplInterface String
             | ExplContext String
          deriving (Show ,Eq)

data Expression
      = EEqu (Expression,Expression)   -- ^ equivalence             =
      | EImp (Expression,Expression)   -- ^ implication             |-
      | EIsc (Expression,Expression)   -- ^ intersection            /\
      | EUni (Expression,Expression)   -- ^ union                   \/
      | EDif (Expression,Expression)   -- ^ difference              -
      | ELrs (Expression,Expression)   -- ^ left residual           /
      | ERrs (Expression,Expression)   -- ^ right residual          \
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
      deriving (Eq,Show)

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

-- SJ 2013118: The fatals are superfluous, but only if the type checker works correctly. Once we have sufficient confidence, they can be removed for performance reasons.
l .==. r = if source l/=source r ||  target l/=target r then fatal 432 ("Cannot equate (with operator \"==\") expression\n   "++show l++"\n   with "++show r++".") else
           EEqu (l,r)
l .|-. r = if source l/=source r ||  target l/=target r then fatal 432 ("Cannot include (with operator \"|-\") expression\n   "++show l++"\n   with "++show r++".") else
           EImp (l,r)
l ./\. r = if source l/=source r ||  target l/=target r then fatal 432 ("Cannot intersect (with operator \"/\\\") expression\n   "++show l++"\n   with "++show r++".") else
           EIsc (l,r)
l .\/. r = if source l/=source r ||  target l/=target r then fatal 432 ("Cannot unite (with operator \"\\/\") expression\n   "++show l++"\n   with "++show r++".") else
           EUni (l,r)
l .-. r  = if source l/=source r ||  target l/=target r then fatal 432 ("Cannot subtract (with operator \"-\") expression\n   "++show l++"\n   with "++show r++".") else
           EDif (l,r)
l ./. r  = if target l/=target r then fatal 432 ("Cannot residuate (with operator \"/\") expression\n   "++show l++"\n   with "++show r++".") else
           ELrs (l,r)
l .\. r  = if source l/=source r then fatal 432 ("Cannot residuate (with operator \"\\\") expression\n   "++show l++"\n   with "++show r++".") else
           ERrs (l,r)
l .:. r  = if source r/=target l then fatal 432 ("Cannot compose (with operator \";\") expression\n   "++show l++"\n   with "++show r++".") else
           ECps (l,r)
l .!. r  = if source r/=target l then fatal 432 ("Cannot add (with operator \"!\") expression\n   "++show l++"\n   with "++show r++".") else
           ERad (l,r)
l .*. r  = -- SJC: should always fit! No fatal here..
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

insParentheses :: Expression -> Expression
insParentheses = insPar 0
      where
       wrap :: Integer -> Integer -> Expression -> Expression
       wrap i j e' = if i<=j then e' else EBrk e'
       insPar :: Integer -> Expression -> Expression
       insPar i (EEqu (l,r)) = wrap i     0 (EEqu (insPar 1 l, insPar 1 r))
       insPar i (EImp (l,r)) = wrap i     0 (EImp (insPar 1 l, insPar 1 r))
       insPar i (EUni (l,r)) = wrap (i+1) 2 (EUni (insPar 2 l, insPar 2 r))
       insPar i (EIsc (l,r)) = wrap (i+1) 2 (EIsc (insPar 2 l, insPar 2 r))
       insPar i (EDif (l,r)) = wrap i     4 (EDif (insPar 5 l, insPar 5 r))
       insPar i (ELrs (l,r)) = wrap i     6 (ELrs (insPar 7 l, insPar 7 r))
       insPar i (ERrs (l,r)) = wrap i     6 (ERrs (insPar 7 l, insPar 7 r))
       insPar i (ECps (l,r)) = wrap (i+1) 8 (ECps (insPar 8 l, insPar 8 r))
       insPar i (ERad (l,r)) = wrap (i+1) 8 (ERad (insPar 8 l, insPar 8 r))
       insPar i (EPrd (l,r)) = wrap (i+1) 8 (EPrd (insPar 8 l, insPar 8 r))
       insPar _ (EKl0 e)     = EKl0 (insPar 10 e)
       insPar _ (EKl1 e)     = EKl1 (insPar 10 e)
       insPar _ (EFlp e)     = EFlp (insPar 10 e)
       insPar _ (ECpl e)     = ECpl (insPar 10 e)
       insPar i (EBrk f)     = insPar i f
       insPar _ e@EDcD{}     = e
       insPar _ e@EDcI{}     = e
       insPar _ e@EEps{}     = e
       insPar _ e@EDcV{}     = e
       insPar _ e@EMp1{}     = e

instance Association Expression where
 sign (EEqu (l,r)) = Sign (source l) (target r)
 sign (EImp (l,r)) = Sign (source l) (target r)
 sign (EIsc (l,r)) = Sign (source l) (target r)
 sign (EUni (l,r)) = Sign (source l) (target r)
 sign (EDif (l,r)) = Sign (source l) (target r)
 sign (ELrs (l,r)) = Sign (source l) (source r)
 sign (ERrs (l,r)) = Sign (target l) (target r)
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
   = PlainConcept   
         { cptnm :: String         -- ^The name of this Concept
         , cpttp :: String         -- ^The (SQL) type of this Concept
         , cptdf :: [ConceptDef]   -- ^Concept definitions of this concept.
         }  -- ^PlainConcept nm gE cs represents the set of instances cs by name nm.
   | ONE  -- ^The universal Singleton: 'I'['Anything'] = 'V'['Anything'*'Anything']

instance Eq A_Concept where
   PlainConcept{cptnm=a} == PlainConcept{cptnm=b} = a==b
   ONE == ONE = True
   _ == _ = False

instance Identified A_Concept where
  name PlainConcept{cptnm = nm} = nm
  name ONE = "ONE"

instance Show A_Concept where
  showsPrec _ c = showString (name c)
   
  
data Sign = Sign A_Concept A_Concept deriving Eq
  

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
