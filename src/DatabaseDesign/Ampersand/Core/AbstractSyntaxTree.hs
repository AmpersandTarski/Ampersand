{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
module DatabaseDesign.Ampersand.Core.AbstractSyntaxTree (
   Architecture(..)
 , A_Context(..)
 , Process(..)
 , Pattern(..)
 , PairView(..)
 , PairViewSegment(..)
 , SrcOrTgt(..)
 , Rule(..)
 , RuleType(..)
 , Declaration(..)
 , KeyDef(..)
 , KeySegment(..)
 , A_Gen(..)
 , Interface(..)
 , ObjectDef(..)
 , Purpose(..)
 , ExplObj(..)
 , Expression(..)
 , Relation(..)
 , A_Concept(..)
 , A_Markup(..)
 , AMeaning(..)
 , RoleRelation(..)
 , Sign(..)
 , Population(..)
 , GenR
 , Signaling(..)
 , Association(..)
  -- (Poset.<=) is not exported because it requires hiding/qualifying the Prelude.<= or Poset.<= too much
  -- import directly from DatabaseDesign.Ampersand.Core.Poset when needed
 , (<==>),join,order,meet,greatest,least,maxima,minima 
 , makeDeclaration
 , showExpr
 , aMarkup2String
 , concatMarkup
 , insParentheses
 , module DatabaseDesign.Ampersand.Core.ParseTree  -- export all used contstructors of the parsetree, because they have acutally become part of the Abstract Syntax Tree.
 -- TODO: Remove the next constructors from here: (start with removing [Activity]  in Process! This should be moved to the Fspec.
)where
import qualified Prelude
import Prelude hiding (Ord(..), Ordering(..))
import DatabaseDesign.Ampersand.Basics.Auxiliaries (eqCl)
import DatabaseDesign.Ampersand.Basics           (fatalMsg,Identified(..))
import DatabaseDesign.Ampersand.Core.ParseTree   (ConceptDef,ConceptDefs,Origin(..),Traced(..),Prop,Lang,Pairs, PandocFormat, P_Markup(..), PMeaning(..), SrcOrTgt)
import DatabaseDesign.Ampersand.Core.Poset (Poset(..), Sortable(..),Ordering(..),comparableClass,greatest,least,maxima,minima)
import DatabaseDesign.Ampersand.Misc
import Text.Pandoc
import Data.List

fatal :: Int -> String -> a
fatal = fatalMsg "AbstractSyntaxTree.hs"


data Architecture = A_Arch { arch_Contexts :: [A_Context]}

data A_Context
   = ACtx{ ctxnm     :: String        -- ^ The name of this context
         , ctxlang   :: Lang          -- ^ The default language used in this context.
         , ctxmarkup :: PandocFormat  -- ^ The default markup format for free text in this context (currently: LaTeX, ...)
         , ctxthms   :: [String]      -- ^ Names of patterns/processes to be printed in the functional specification. (For partial documents.)
         , ctxpo     :: GenR          -- ^ A tuple representing the partial order of concepts (see makePartialOrder)
         , ctxpats   :: [Pattern]     -- ^ The patterns defined in this context
         , ctxprocs  :: [Process]     -- ^ The processes defined in this context
         , ctxrs     :: [Rule]        -- ^ All user defined rules in this context, but outside patterns and outside processes
         , ctxds     :: [Declaration] -- ^ The declarations defined in this context, outside the scope of patterns
         , ctxcds    :: [ConceptDef]  -- ^ The concept definitions defined in this context, including those from patterns and processes
         , ctxks     :: [KeyDef]      -- ^ The key definitions defined in this context, outside the scope of patterns
         , ctxgs     :: [A_Gen]       -- ^ The key definitions defined in this context, outside the scope of patterns
         , ctxifcs   :: [Interface]   -- ^ The interfaces defined in this context, outside the scope of patterns
         , ctxps     :: [Purpose]     -- ^ The purposes of objects defined in this context, outside the scope of patterns
         , ctxsql    :: [ObjectDef]   -- ^ user defined sqlplugs, taken from the Ampersand script
         , ctxphp    :: [ObjectDef]   -- ^ user defined phpplugs, taken from the Ampersand script
         , ctxenv    :: (Expression,[(Declaration,String)]) -- ^ an expression on the context with unbound relations, to be bound in this environment
         }               --deriving (Show) -- voor debugging
instance Show A_Context where
  showsPrec _ c = showString (ctxnm c)
instance Eq A_Context where
  c1 == c2  =  name c1 == name c2
instance Identified A_Context where
  name  = ctxnm 



data Process = Proc { prcNm    :: String
                    , prcPos   :: Origin
                    , prcEnd   :: Origin      -- ^ the end position in the file, elements with a position between pos and end are elements of this process.
                    , prcRules :: [Rule]
                    , prcGens  :: [A_Gen]
                    , prcDcls  :: [Declaration]
                    , prcRRuls :: [(String,Rule)]    -- ^ The assignment of roles to rules.
                    , prcRRels :: [(String,Relation)] -- ^ The assignment of roles to Relations.
                    , prcKds   :: [KeyDef]            -- ^ The key definitions defined in this process
                    , prcXps   :: [Purpose]           -- ^ The pre-explanations of elements defined in this process
                    }
instance Identified Process where
  name = prcNm

instance Traced Process where
  origin = prcPos

data RoleRelation
   = RR { rrRoles :: [String]     -- ^ name of a role
        , rrRels  :: [Relation]   -- ^ name of a Relation
        , rrPos   :: Origin       -- ^ position in the Ampersand script
        } deriving (Eq, Show)     -- just for debugging
instance Traced RoleRelation where
   origin = rrPos
    


data Pattern
   = A_Pat { ptnm  :: String        -- ^ Name of this pattern
           , ptpos :: Origin        -- ^ the position in the file in which this pattern was declared.
           , ptend :: Origin        -- ^ the end position in the file, elements with a position between pos and end are elements of this pattern.
           , ptrls :: [Rule]        -- ^ The user defined rules in this pattern
           , ptgns :: [A_Gen]       -- ^ The generalizations defined in this pattern
           , ptdcs :: [Declaration] -- ^ The declarations declared in this pattern
           , ptkds :: [KeyDef]      -- ^ The key definitions defined in this pattern
           , ptxps :: [Purpose]     -- ^ The purposes of elements defined in this pattern
           }   --deriving (Show)    -- for debugging purposes
instance Identified Pattern where
 name = ptnm
instance Traced Pattern where
 origin = ptpos

data A_Markup =
    A_Markup { amLang   :: Lang
             , amFormat :: PandocFormat
             , amPandoc :: [Block]
             } deriving Show
concatMarkup :: [A_Markup] -> Maybe A_Markup
concatMarkup es
 = case eqCl f es of
    []   -> Nothing
    [cl] -> Just (A_Markup { amLang   = amLang (head cl)
                           , amFormat = amFormat (head cl)
                           , amPandoc = concat (map amPandoc es)
                           })
    cls  -> fatal 136 ("don't call concatMarkup with different languages and formats\n   "++
                      intercalate "\n   " [(show.f.head) cl | cl<-cls])
   where f e = (amLang e, amFormat e)


data PairView = PairView [PairViewSegment] deriving Show

data PairViewSegment = PairViewText String
                     | PairViewExp SrcOrTgt Expression deriving Show

data Rule =
     Ru { rrnm      :: String                  -- ^ Name of this rule
        , rrexp     :: Expression              -- ^ The rule expression
        , rrfps     :: Origin                  -- ^ Position in the Ampersand file
        , rrmean    :: AMeaning                -- ^ Ampersand generated explanations (MEANINGs) (for all known languages)
        , rrmsg     :: [A_Markup]              -- ^ User-specified violation messages, possibly more than one, for multiple languages.
        , rrviol    :: Maybe PairView          -- ^ Custom presentation for violations, currently only in a single language
        , rrtyp     :: Sign                    -- ^ Allocated type
        , rrdcl     :: Maybe (Prop,Declaration)  -- ^ The property, if this rule originates from a property on a Declaration
        , r_env     :: String                  -- ^ Name of pattern in which it was defined.
        , r_usr     :: Bool                    -- ^ True if this rule was specified explicitly as a rule in the Ampersand script; False if it follows implicitly from the Ampersand script and generated by a computer
        , r_sgl     :: Bool                    -- ^ True if this is a signal; False if it is an ALWAYS rule
        , srrel     :: Declaration             -- ^ the signal relation
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
  Sgn { decnm   :: String     -- ^ the name of the declaration
      , decsgn  :: Sign       -- ^ the source concept of the declaration
       --multiplicities returns decprps_calc so if you only need the user defined properties do not use multiplicities but decprps
      , decprps :: [Prop]     -- ^ the user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
      , decprps_calc :: [Prop]-- ^ the calculated and user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx, Irf). Note that calculated properties are made by adl2fspec, so in the A-structure decprps and decprps_calc yield exactly the same answer.
      , decprL  :: String     -- ^ three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
      , decprM  :: String     -- ^    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
      , decprR  :: String
      , decMean :: AMeaning   -- ^ the meaning of a declaration, for each language supported by Ampersand.
      , decpopu :: Pairs      -- ^ the list of tuples, of which the relation consists.
      , decfpos :: Origin     -- ^ the position in the Ampersand source file where this declaration is declared. Not all decalartions come from the ampersand souce file. 
      , deciss  :: Bool       -- ^ if true, this is a signal relation; otherwise it is an ordinary relation.
      , decusr  :: Bool       -- ^ if true, this relation is declared in the Ampersand script; otherwise it was generated by Ampersand.
      , decpat  :: String     -- ^ the pattern where this declaration has been declared.
      , decplug :: Bool       -- ^ if true, this relation may not be stored in or retrieved from the standard database (it should be gotten from a Plug of some sort instead)
      } | 
 Isn 
      { detyp   :: A_Concept       -- ^ The type
      } |
 Iscompl 
      { detyp   :: A_Concept
      } |
 Vs 
      { decsgn  :: Sign
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
                            ++ ["TEXTMARKUP",show (amFormat m)]
                            ++ ["{+",aMarkup2String m,"-}"]                
                            -- then [] else ["MEANING",show (decMean d)] ))

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
instance Signaling Declaration where
  isSignal d = case d of
              Sgn {}    -> deciss d
              _         -> False


data KeyDef = Kd { kdpos :: Origin       -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number).
                 , kdlbl :: String       -- ^ the name (or label) of this Key. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
                 , kdcpt :: A_Concept    -- ^ this expression describes the instances of this object, related to their context
                 , kdats :: [KeySegment]  -- ^ the constituent attributes (i.e. name/expression pairs) of this key.
                 } deriving (Eq,Show)
instance Identified KeyDef where
  name = kdlbl
instance Traced KeyDef where
  origin = kdpos

data KeySegment = KeyExp ObjectDef | KeyText String deriving (Eq, Show)

data A_Gen = Gen { genfp  :: Origin         -- ^ the position of the GEN-rule
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


data Interface = Ifc { ifcName   :: String
                     , ifcParams :: [Relation]
                     , ifcViols  :: [Rule]
                     , ifcArgs   :: [[String]]
                     , ifcRoles  :: [String]
                     , ifcObj    :: ObjectDef
                     , ifcPos    :: Origin
                     , ifcExpl   :: String
                     } deriving Show
instance Eq Interface where
  s==s' = ifcName s==ifcName s'
instance Identified Interface where
  name = ifcName
instance Traced Interface where
  origin = ifcPos

data ObjectDef = Obj { objnm   :: String         -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
                     , objpos  :: Origin         -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number)
                     , objctx  :: Expression     -- ^ this expression describes the instances of this object, related to their context. 
                     , objats  :: [ObjectDef]    -- ^ the attributes, which are object definitions themselves.
                     , objstrs :: [[String]]     -- ^ directives that specify the interface.
                     } deriving (Eq, Show)       -- just for debugging (zie ook instance Show ObjectDef)
instance Identified ObjectDef where
  name   = objnm
instance Traced ObjectDef where
  origin = objpos


-- | Explanation is the intended constructor. It explains the purpose of the object it references.
--   The enrichment process of the parser must map the names (from PPurpose) to the actual objects
data Purpose  = Expl { explPos      :: Origin     -- ^ The position in the Ampersand script of this purpose definition
                     , explObj      :: ExplObj    -- ^ The object that is explained.
                     , explMarkup   :: A_Markup   -- ^ This field contains the text of the explanation including language and markup info.
                     , explUserdefd :: Bool       -- ^ Is this purpose defined in the script?
                     , explRefId    :: String     -- ^ The reference of the explaination
                     } deriving Show  --handy for XML creation
instance Eq Purpose where
  x0 == x1  =  explObj x0 == explObj x1 && 
               (amLang . explMarkup) x0 == (amLang . explMarkup) x1
instance Traced Purpose where
  origin = explPos

data Population
  = Popu { popm  :: Relation
         , popps :: Pairs
         }

{- To avoid a compile time loop, names were used in the old data structure. Since we have a distinction between P-structure and A-structure,
the reason for doing this has evaporated. So we should be able to fix this without trouble. TODO -}
data ExplObj = ExplConceptDef ConceptDef
             | ExplDeclaration Declaration
             | ExplRule Rule
             | ExplKeyDef KeyDef
             | ExplPattern String   -- SJ: (now obsolete...) To avoid a compile time loop, the name of the pattern is used rather than the entire pattern. Hence, for patterns the PRef2Pattern is identical to the ExplPattern
             | ExplProcess String   -- SJ: (now obsolete...) To avoid a compile time loop, the name of the process is used rather than the entire process. Hence, for patterns the PRef2Process is identical to the ExplProcess
             | ExplInterface String -- SJ: (now obsolete...) To avoid a compile time loop, the name of the interface is used rather than the entire interface. Hence, for interfaces the PRef2Interface is identical to the ExplInterface
             | ExplContext String   -- SJ: (now obsolete...) To avoid a compile time loop, the name of the context is used rather than the entire context. Hence, for contexts the PRef2Context is identical to the ExplContext
             | ExplFspc String      -- SJ: (now obsolete...) To avoid a compile time loop, the name of the fSpec is used rather than the entire fSpec. Hence, for contexts the PRef2Fspc is identical to the ExplFspc
          deriving (Show ,Eq)
                  

data Expression
      = EEqu (Expression,Expression)       -- ^ equivalence             =
      | EImp (Expression,Expression)       -- ^ implication             |-
      | EIsc [Expression]                  -- ^ intersection            /\
      | EUni [Expression]                  -- ^ union                   \/
      | EDif (Expression,Expression)       -- ^ difference              -
      | ELrs (Expression,Expression)       -- ^ left residual           /
      | ERrs (Expression,Expression)       -- ^ right residual          \
      | ECps [Expression]                  -- ^ composition             ;
      | ERad [Expression]                  -- ^ relative addition       !
      | EPrd [Expression]                  -- ^ cartesian product       *  -- The argument is a list of Expressions rather than a tuple (l,r), only because * is associative.
      | EKl0 Expression                    -- ^ Rfx.Trn closure         *  (Kleene star)
      | EKl1 Expression                    -- ^ Transitive closure      +  (Kleene plus)
      | EFlp Expression                    -- ^ conversion (flip, wok)  ~
      | ECpl Expression                    -- ^ Complement
      | EBrk Expression                    -- ^ bracketed expression ( ... )
      | ETyp Expression Sign               -- ^ type cast expression ... [c] (defined tuple instead of list because ETyp only exists for actual casts)
      | ERel Relation                      -- ^ simple relation
      deriving Eq
instance Show Expression where
 showsPrec _ = showString . showExpr (" = ", " |- ", "/\\", " \\/ ", " - ", " / ", " \\ ", ";", "!", "*", "*", "+", "~", ("-"++), "(", ")", "[", "*", "]") . insParentheses
showExpr :: (String,String,String,String,String,String,String,String,String,String,String,String,String,String -> String,String,String,String,String,String)
            -> Expression -> String
showExpr    (equi,  impl,  inter, union',diff,  lresi, rresi, rMul  , rAdd , rPrd ,closK0,closK1,flp',  compl,           lpar,  rpar,  lbr,   star,  rbr) expr
 = showchar expr
   where
     showchar (EEqu (l,r)) = showchar l++equi++showchar r
     showchar (EImp (l,r)) = showchar l++impl++showchar r
     showchar (EIsc [])    = "V"
     showchar (EIsc es)    = intercalate inter  [showchar e | e<-es]
     showchar (EUni [])    = "-V"
     showchar (EUni es)    = intercalate union' [showchar e | e<-es]
     showchar (EDif (l,r)) = showchar l++diff ++showchar r
     showchar (ELrs (l,r)) = showchar l++lresi++showchar r
     showchar (ERrs (l,r)) = showchar l++rresi++showchar r
     showchar (ECps [])    = "I"
     showchar (ECps es)    = intercalate rMul [showchar e | e<-es]
     showchar (ERad [])    = "-I"
     showchar (ERad es)    = intercalate rAdd [showchar e | e<-es]
     showchar (EPrd [])    = "ONE"
     showchar (EPrd es)    = intercalate rPrd [showchar e | e<-es]
     showchar (EKl0 e)     = showchar e++closK0
     showchar (EKl1 e)     = showchar e++closK1
     showchar (EFlp e)     = showchar e++flp'
     showchar (ECpl e)     = compl (showchar e)
     showchar (EBrk e)     = lpar++showchar e++rpar
     showchar (ETyp e sgn) 
      | source sgn==target sgn = showchar e++lbr++show (source sgn)++rbr
      | otherwise              = showchar e++lbr++show (source sgn)++star++show (target sgn)++rbr
     -- relations in expressions are printed without type signature, use ETyp to print signatures
     showchar (ERel rel@(Rel{})) = name rel
     showchar (ERel      I{})    = "I"
     showchar (ERel      V{})    = "V"
     showchar (ERel rel@(Mp1{})) = "'"++relval rel++"'"

insParentheses :: Expression -> Expression
insParentheses expr = insPar 0 expr
      where
       wrap :: Integer -> Integer -> Expression -> Expression
       wrap i j e' = if i<=j then e' else EBrk e'
       insPar :: Integer -> Expression -> Expression
       insPar i (EEqu (l,r)) = wrap i     0 (EEqu (insPar 1 l, insPar 1 r))
       insPar i (EImp (l,r)) = wrap i     0 (EImp (insPar 1 l, insPar 1 r))
       insPar i (EUni fs)    = wrap (i+1) 2 (EUni [insPar 2 f | f<-fs])
       insPar i (EIsc fs)    = wrap (i+1) 2 (EIsc [insPar 2 f | f<-fs])
       insPar i (EDif (l,r)) = wrap i     4 (EDif (insPar 5 l, insPar 5 r))
       insPar i (ELrs (l,r)) = wrap i     6 (ELrs (insPar 7 l, insPar 7 r))
       insPar i (ERrs (l,r)) = wrap i     6 (ERrs (insPar 7 l, insPar 7 r))
       insPar i (ECps ts)    = wrap (i+1) 8 (ECps [insPar 8 t | t<-ts])
       insPar i (ERad ts)    = wrap (i+1) 8 (ERad [insPar 8 t | t<-ts])
       insPar i (EPrd ts)    = wrap (i+1) 8 (EPrd [insPar 8 t | t<-ts])
       insPar _ (EKl0 e)     = EKl0 (insPar 10 e)
       insPar _ (EKl1 e)     = EKl1 (insPar 10 e)
       insPar _ (EFlp e)     = EFlp (insPar 10 e)
       insPar _ (ECpl e)     = ECpl (insPar 10 e)
       insPar i (EBrk f)     = insPar i f
       insPar _ (ETyp e t)   = ETyp (insPar 10 e) t
       insPar _ (ERel rel)   = ERel rel

-- The following code has been reviewed by Gerard and Stef on nov 1st, 2011 (revision 290)
instance Association Expression where
 sign (EEqu (l,r))   = if sign l <==> sign r
                     then sign l `join` sign r
                     else fatal 233 $ "type checker failed to verify "++show (EEqu (l,r))++"."
 sign (EImp (l,r))   = if sign l <==> sign r
                     then sign l `join` sign r
                     else fatal 236 $ "type checker failed to verify "++show (EImp (l,r))++"."
 sign (EIsc [])      = fatal 237 $ "Ampersand failed to eliminate "++show (EIsc [])++"."
 sign (EIsc es)      = let ss=map sign es in
                     if and [l <==> r | (l,r)<-zip (init ss) (tail ss)] -- The alternative [head ss <==> s | s<-tail ss] may be wrong, since comparable is not transitive.
                     then Sign (greatest$map source ss)(greatest$map target ss) -- do not use  foldr1 join ss, because <==> is not transitive.
                     else fatal 241 $ "type checker failed to verify "++show (EIsc es)++"."
 sign (EUni [])      = fatal 242 $ "Ampersand failed to eliminate "++show (EUni [])++"."
 sign (EUni es)      = let ss=map sign es in
                     if and [l <==> r | (l,r)<-zip (init ss) (tail ss)] -- The alternative [head ss <==> s | s<-tail ss] may be wrong, since comparable is not transitive.
                     then Sign (greatest$map source ss)(greatest$map target ss) -- do not use  foldr1 join ss, because <==> is not transitive.
                     else fatal 246 $ "type checker failed to verify "++show (EUni es)++"."
 sign (EDif (l,r))   = if sign l <==> sign r
                     then sign l
                     else sign l -- fatal 249 $ "type checker failed to verify "++show (EDif (l,r))++"."
 sign (ELrs (l,r))   = if target l <==> target r
                     then Sign (source l) (source r)
                     else fatal 252 $ "type checker failed to verify "++show (ELrs (l,r))++"."
 sign (ERrs (l,r))   = if source l <==> source r
                     then Sign (target l) (target r)
                     else fatal 255 $ "type checker failed to verify "++show (ERrs (l,r))++"."
 sign (ECps [])      = fatal 256 $ "Ampersand failed to eliminate "++show (ECps [])++"."
 sign (ECps es)      = let ss=map sign es in
                     if and [r <==> l | (r,l)<-zip [target sgn |sgn<-init ss] [source sgn |sgn<-tail ss]]
                     then Sign (source (head ss)) (target (last ss))
                     else fatal 260 $ "type checker failed to verify "++show (ECps es)++"."
 sign (ERad [])      = fatal 261 $ "Ampersand failed to eliminate "++show (ERad [])++"."
 sign (ERad es)      = let ss=map sign es in
                     if and [r <==> l | (r,l)<-zip [target sgn |sgn<-init ss] [source sgn |sgn<-tail ss]]
                     then Sign (source (head ss)) (target (last ss))
                     else fatal 265 $ "type checker failed to verify "++show (ERad es)++"."
 sign (EPrd [])      = fatal 261 $ "Ampersand failed to eliminate "++show (EPrd [])++"."
 sign (EPrd es)      = Sign (source (head es)) (target (last es))
 sign (EKl0 e)       = --see #166 
                     if source e <==> target e
                     then Sign (source e `join` target e)(source e `join` target e)
                     else fatal 409 $ "type checker failed to verify "++show (EKl0 e)++"."
 sign (EKl1 e)       = sign e
 sign (EFlp e)       = Sign t s where Sign s t=sign e
 sign (ECpl e)       = sign e
 sign (EBrk e)       = sign e
 sign (ETyp e sgn)   = if sign e <==> sgn
                     then sgn
                     else fatal 417 $ "type checker failed to verify "++show (ETyp e sgn)++"."
 sign (ERel rel)     = sign rel


{- The atoms in a relation are accessible as follows:
   Atoms in a Rel{} are found through the declaration (via decpopu.reldcl).
   Atoms in a I{} and V{} are found in the concept (via rel1typ and reltyp).
   Mp1{} has precisely one atom, which must be an element of its type, i.e. relatom m `elem` atoms (rel1typ c)
-}
data Relation = 
  Rel { relnm   :: String           -- ^ the name of the relation. This is the same name as the name of reldcl.
                                    --    VRAAG: Waarom zou je dit attribuut opnemen? De naam van het morphisme is immers altijd gelijk aan de naam van de Declaration reldcl ....
                                    --    ANTWOORD: Tijdens het parsen, tot het moment dat de declaration aan het morphism is gekoppeld, moet de naam van het morphism bekend zijn. Nadat het morphisme gebonden is aan een declaration moet de naam van het morphisme gelijk zijn aan de naam van zijn reldcl.
      , relpos  :: Origin           -- ^ the position in the Ampersand source file. Let rel_pos be Nowhere if not applicable e.g. relations in generated rules
      , relsgn  :: Sign             -- ^ the allocated signature. May differ from the signature in the reldcl.
      , reldcl  :: Declaration      -- ^ the declaration bound to this relation.
      } |
 I    { rel1typ :: A_Concept        -- ^ the allocated type.
      } |
 V    { reltyp  :: Sign             -- ^ the allocated type.
      } |
 --   An Mp1 is a subset of I. Shouldn't we replace it by an I?
 Mp1  { relval  :: String          -- ^ the value of the singleton morphism
      , rel1typ :: A_Concept               -- ^ the allocated type.
      } 
instance Eq Relation where
 Rel nm _ sgn _ == Rel nm' _ sgn' _ = nm==nm' && sgn==sgn'
 I c            == I c'             = c==c'
 V sgn          == V sgn'           = sgn==sgn'
 Mp1 s c        == Mp1 s' c'        = s==s' && c==c'
 _ == _ = False
instance Show Relation where
 showsPrec _ r = case r of
   Rel{} -> showString (name r++showSign [source r,target r])
   I{}   -> showString (name r++"["++show (rel1typ r)++"]")
   V{}   -> showString (name r++show (sign r))
   Mp1{} -> showString ("'"++relval r++"'["++ show (rel1typ r)++"]")
instance Identified Relation where
  name r = name (makeDeclaration r)
instance Association Relation where
  sign r =
    case r of 
      Rel{}   -> relsgn r
      I{}     -> Sign (rel1typ r) (rel1typ r)
      V{}     -> reltyp r
      Mp1{}   -> Sign (rel1typ r) (rel1typ r)
makeDeclaration :: Relation -> Declaration
makeDeclaration r = case r of
      Rel{} -> reldcl r
      I{}   -> Isn{ detyp = rel1typ r}
      V{}   -> Vs { decsgn = sign r}
      Mp1{} -> Isn{ detyp = rel1typ r}
showSign :: Identified a => [a] -> String
showSign cs = "["++(intercalate "*".nub.map name) cs++"]"
instance Traced Relation where
 origin = relpos
instance Signaling Relation where
 isSignal r = isSignal (makeDeclaration r)









-- The following definition of concept is used in the type checker only.
-- It is called Concept, meaning "type checking concept"

data A_Concept
   = C   { cptnm :: String         -- ^The name of this Concept
         , cptgE :: GenR           -- ^This is the generalization relation between concepts.
                                   --  It is included in every concept, for the purpose of comparing concepts in the Ord class.
                                   --  As a result, you may write  c<=d  in your Haskell code for any two A_Concepts c and d that are in the same context.
         , cptos :: [String]       -- ^Atoms
         , cpttp :: String         -- ^The type of this Concept
         , cptdf :: [ConceptDef]   -- ^Concept definitions of this concept.
         }  -- ^C nm gE cs represents the set of instances cs by name nm.
   | ONE  -- ^The universal Singleton: 'I'['Anything'] = 'V'['Anything'*'Anything']


instance Eq A_Concept where
   a@(C{}) == b@(C{}) = name a==name b
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
   | s==s' && t==t' = EQ
   | s<=s' && t<=t' = LT
   | s'<=s && t'<=t = GT
   | s<==>s' && t<==>t' = CP
   | otherwise = NC
instance Sortable Sign where
  meet (Sign a b) (Sign a' b') = Sign (a `meet` a') (b `meet` b')
  join (Sign a b) (Sign a' b') = Sign (a `join` a') (b `join` b')
  sortBy = fatal 534 "sortBy of Sign has not been implemented (yet)."
  
   
instance Show Sign where
  showsPrec _ (Sign s t) = 
     showString (   "[" ++ show s ++ "*" ++ show t ++ "]" )
instance Association Sign where
  source (Sign s _) = s
  target (Sign _ t) = t
  sign sgn = sgn



class Association rel where
  source, target :: rel -> A_Concept      -- e.g. Declaration -> Concept
  source x        = source (sign x)
  target x        = target (sign x)
  sign           :: rel -> Sign
  isEndo         :: rel  -> Bool
  isEndo s        = source s == target s
  
instance (Eq a,Association a) => Poset a where
  a <= b = sign a <= sign b

class Signaling a where
  isSignal       :: a -> Bool  -- > tells whether the argument refers to a signal
    

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
type GenR = (A_Concept -> A_Concept -> Ordering,[[A_Concept]])
order      :: A_Concept -> GenR
order c@(C{}) = cptgE c
order _ = ((\x y -> if x==y then EQ else NC),[])
instance Poset A_Concept where
  a `compare` b = (fst$order a) a b
instance Sortable A_Concept where
  meet a b | b <= a = b
           | a <= b = a
           | a `compare` b == CP = fatal 561 "implement ISA" 
           | otherwise = fatal 568 $ "meet undefined: a="++show a++", b="++show b
  join a b | a <= b = b
           | b <= a = a
           | a `compare` b == CP = fatal 565 ("implement ISA" ++ show(a,b))
           | otherwise = fatal 571 $ "join undefined: a="++show a++", b="++show b
  sortBy f = Data.List.sortBy ((comparableClass .) . f)

