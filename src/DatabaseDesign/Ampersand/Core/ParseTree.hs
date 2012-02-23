{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Core.ParseTree (
     P_Architecture(..)
   , P_Context(..)
   , P_Process(..)
   , P_RoleRelation(..)
   , RoleRule(..)
   
   , P_Pattern(..)
   
   , P_Relation(..),P_Declaration(..)
   
   , P_Expression(..)
   
   , P_PairView(..), P_PairViewSegment(..), SrcOrTgt(..)
   
   , P_Rule(..)
   
   , ConceptDef(..)
   
   , P_Population(..)
   
   , P_Interface(..), P_ObjectDef(..), P_SubInterface(..)
   
   , P_KeyDef(..)
   , P_KeySegment(..)
   
   , PPurpose(..),PRef2Obj(..),PMeaning(..)
   
   , P_Concept(..), P_Sign(..)
   
   , P_Gen(..),P_Gens
   
   , Lang(..)
   , P_Markup(..)
   
   , PandocFormat(..)
   
   , Label(..)
   
   , Prop(..), Props
   -- Inherited stuff: 
   , module DatabaseDesign.Ampersand.Input.ADL1.FilePos
   , module DatabaseDesign.Ampersand.ADL1.Pair
  )
where
   import DatabaseDesign.Ampersand.Input.ADL1.FilePos           
   import DatabaseDesign.Ampersand.Basics                       (fatalMsg,Identified(..))
   import DatabaseDesign.Ampersand.ADL1.Pair (Pairs,Paire,mkPair ,srcPaire, trgPaire)
   import Data.List   (intercalate)
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "ParseTree"
   
   -- | Architecture of Ampersand consists of a set of contexts
   data P_Architecture = P_Arch { p_arch_Contexts :: [P_Context]}
   data P_Context
      = PCtx{ ctx_nm     :: String          -- ^ The name of this context
            , ctx_lang   :: Maybe Lang      -- ^ The default language specified by this context, if specified at all.
            , ctx_markup :: Maybe PandocFormat  -- ^ The default markup format for free text in this context
            , ctx_thms   :: [String]        -- ^ Names of patterns/processes to be printed in the functional specification. (For partial documents.)
            , ctx_pats   :: [P_Pattern]     -- ^ The patterns defined in this context
            , ctx_PPrcs  :: [P_Process]     -- ^ The processes as defined by the parser
            , ctx_rs     :: [P_Rule]        -- ^ All user defined rules in this context, but outside patterns and outside processes
            , ctx_ds     :: [P_Declaration] -- ^ The declarations defined in this context, outside the scope of patterns
            , ctx_cs     :: [ConceptDef]    -- ^ The concept definitions defined in this context, outside the scope of patterns
            , ctx_ks     :: [P_KeyDef]      -- ^ The key definitions defined in this context, outside the scope of patterns
            , ctx_gs     :: [P_Gen]         -- ^ The gen definitions defined in this context, outside the scope of patterns
            , ctx_ifcs   :: [P_Interface]   -- ^ The interfaces defined in this context, outside the scope of patterns
            , ctx_ps     :: [PPurpose]      -- ^ The pre-explanations defined in this context, outside the scope of patterns
            , ctx_pops   :: [P_Population]  -- ^ The populations defined in this context
            , ctx_sql    :: [P_ObjectDef]   -- ^ user defined sqlplugs, taken from the Ampersand script
            , ctx_php    :: [P_ObjectDef]   -- ^ user defined phpplugs, taken from the Ampersand script
            , ctx_experimental :: Bool      -- flag that specifies whether Ampersand was executed with --exp (not techniqually part of the context, but prevents giant refactorings of type checker)
            }

   instance Show P_Context where
     showsPrec _ = showString . ctx_nm

   instance Eq P_Context where
     c1 == c2  =  name c1 == name c2

   instance Identified P_Context where
     name = ctx_nm
   -- | A RoleRelation rs means that any role in 'rrRoles rs' may edit any Relation  in  'rrInterfaces rs'
   data P_RoleRelation
      = P_RR { rr_Roles :: [String]                -- ^ name of a role
             , rr_Rels  :: [(P_Relation,P_Sign)]   -- ^ Relation with type information
             , rr_Pos   :: Origin                  -- ^ position in the Ampersand script
             } -- deriving (Eq, Show)              -- just for debugging
   instance Traced P_RoleRelation where
    origin = rr_Pos

   data P_Process = P_Prc { procNm    :: String
                          , procPos   :: Origin           -- ^ the start position in the file
                          , procEnd   :: Origin           -- ^ the end position in the file
                          , procRules :: [P_Rule]
                          , procGens  :: [P_Gen]
                          , procDcls  :: [P_Declaration]
                          , procRRuls :: [RoleRule]       -- ^ The assignment of roles to rules.
                          , procRRels :: [P_RoleRelation] -- ^ The assignment of roles to Relations.
                          , procCds   :: [ConceptDef]     -- ^ The concept definitions defined in this process
                          , procKds   :: [P_KeyDef]       -- ^ The key definitions defined in this process
                          , procXps   :: [PPurpose]       -- ^ The pre-explanations of elements defined in this process
                          , procPop   :: [P_Population]   -- ^ The populations that are local to this process
                          } 

   instance Identified P_Process where
    name = procNm 

   instance Traced P_Process where
    origin = procPos

    -- | A RoleRule r means that a role called 'mRoles r' must maintain the process rule called 'mRules r'
   data RoleRule
      = Maintain
        { mRoles :: [String]    -- ^ name of a role
        , mRules :: [String]    -- ^ name of a Rule
        , mPos   :: Origin      -- ^ position in the Ampersand script
        } deriving (Eq, Show)   -- just for debugging

   instance Traced RoleRule where
    origin = mPos

   data P_Pattern
      = P_Pat { pt_nm  :: String          -- ^ Name of this pattern
              , pt_pos :: Origin          -- ^ the starting position in the file in which this pattern was declared.
              , pt_end :: Origin          -- ^ the end position in the file in which this pattern was declared.
              , pt_rls :: [P_Rule]        -- ^ The user defined rules in this pattern
              , pt_gns :: [P_Gen]         -- ^ The generalizations defined in this pattern
              , pt_dcs :: [P_Declaration] -- ^ The declarations declared in this pattern
              , pt_cds :: [ConceptDef]    -- ^ The concept definitions defined in this pattern
              , pt_kds :: [P_KeyDef]      -- ^ The key definitions defined in this pattern
              , pt_xps :: [PPurpose]      -- ^ The purposes of elements defined in this pattern
              , pt_pop :: [P_Population]  -- ^ The populations that are local to this pattern
              }   --deriving (Show)       -- for debugging purposes

   instance Identified P_Pattern where
    name = pt_nm

   instance Traced P_Pattern where
    origin = pt_pos

   data ConceptDef 
      = Cd  { cdpos :: Origin   -- ^ The position of this definition in the text of the Ampersand source (filename, line number and column number).
            , cdcpt :: String   -- ^ The name of the concept for which this is the definition. If there is no such concept, the conceptdefinition is ignored.
            , cdplug:: Bool     -- ^ Whether the user specifically told Ampersand not to store this concept in the database
            , cddef :: String   -- ^ The textual definition of this concept.
            , cdtyp :: String   -- ^ The type of this concept.
            , cdref :: String   -- ^ A label meant to identify the source of the definition. (useful as LaTeX' symbolic reference)
            }   deriving (Show,Eq)

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ConceptDef                    ***
-- \***********************************************************************
 --  instance Show ConceptDef
   instance Traced ConceptDef where
    origin = cdpos
   instance Identified ConceptDef where
    name = cdcpt
       
   data P_Declaration = 
         P_Sgn { dec_nm   :: String    -- ^ the name of the declaration
               , dec_sign :: P_Sign    -- ^ the type. Parser must guarantee it is not empty.
               , dec_prps :: Props     -- ^ the user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
               , dec_prL  :: String    -- ^ three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
               , dec_prM  :: String    -- ^    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
               , dec_prR  :: String
               , dec_Mean :: [PMeaning]  -- ^ the optional meaning of a declaration, possibly more than one for different languages.
               , dec_popu :: Pairs     -- ^ the list of tuples, of which the relation consists.
               , dec_fpos :: Origin    -- ^ the position in the Ampersand source file where this declaration is declared. Not all decalartions come from the ampersand souce file. 
               , dec_plug :: Bool      -- ^ if true, this relation may not be stored in or retrieved from the standard database (it should be gotten from a Plug of some sort instead)
               } deriving Show -- for debugging and testing only
   instance Identified P_Declaration where
    name = dec_nm
   instance Traced P_Declaration where
    origin = dec_fpos

   data P_Relation = 
        P_Rel   { rel_nm   :: String            -- ^ the name of the relation.
                , rel_pos  :: Origin            -- ^ the position in the Ampersand source file. Let rel_pos be Nowhere if not applicable.
                }
        | P_I
        | P_V
        | P_Mp1 { rel_1val :: String }          -- ^ the value of the one morphism
        deriving Eq
        
   instance Show P_Relation where
    showsPrec _ r = case r of
      P_Rel{} -> showString (rel_nm r)
      P_I{}   -> showString "I"
      P_V{}   -> showString "V"
      P_Mp1{} -> showString ("'"++rel_1val r++"'")

   instance Identified P_Relation where
    name r = case r of
      P_Rel{} -> rel_nm r
      P_I{}   -> "I"
      P_V{}   -> "V"
      P_Mp1{} -> "Mp1"
      
   instance Traced P_Relation where
    origin r = case r of
      P_Rel{} -> rel_pos r
      _       -> OriginUnknown
   
   data P_Expression 
      = Pequ (P_Expression,P_Expression)   -- ^ equivalence             =
      | Pimp (P_Expression,P_Expression)   -- ^ implication             |-
      | Pisc [P_Expression]                -- ^ intersection            /\
      | PUni [P_Expression]                -- ^ union                   \/
      | PDif (P_Expression,P_Expression)   -- ^ difference              -
      | PLrs (P_Expression,P_Expression)   -- ^ left residual           /
      | PRrs (P_Expression,P_Expression)   -- ^ right residual          \
      | PCps [P_Expression]                -- ^ composition             ;
      | PRad [P_Expression]                -- ^ relative addition       !
      | PPrd [P_Expression]                -- ^ cartesian product       *
      | PKl0 P_Expression                  -- ^ Rfx.Trn closure         *  (Kleene star)
      | PKl1 P_Expression                  -- ^ Transitive closure      +  (Kleene plus)
      | PFlp P_Expression                  -- ^ conversion (flip, wok)  ~
      | PCpl P_Expression                  -- ^ Complement
      | PBrk P_Expression                  -- ^ bracketed expression ( ... )
      | PTyp P_Expression P_Sign           -- ^ type cast expression ... [c] (defined tuple instead of list because ETyp only exists for actual casts)
      | Prel P_Relation                    -- ^ simple relation
      deriving (Eq, Show) -- deriving only for debugging purposes

   data SrcOrTgt = Src | Tgt deriving Show

   data P_PairView = P_PairView [P_PairViewSegment]

   data P_PairViewSegment = P_PairViewText String
                          | P_PairViewExp SrcOrTgt P_Expression

   data P_Rule  =
      P_Ru { rr_nm    :: String             -- ^ Name of this rule
           , rr_exp   :: P_Expression       -- ^ The rule expression 
           , rr_fps   :: Origin             -- ^ Position in the Ampersand file
           , rr_mean  :: [PMeaning]         -- ^ User-specified meanings, possibly more than one, for multiple languages.
           , rr_msg   :: [P_Markup]         -- ^ User-specified violation messages, possibly more than one, for multiple languages.
           , rr_viol  :: Maybe P_PairView   -- ^ Custom presentation for violations, currently only in a single language
           }
   instance Traced P_Rule where
    origin = rr_fps

   data PMeaning = PMeaning P_Markup 
            deriving Show
            
   data P_Markup = 
       P_Markup  { mLang   :: Maybe Lang
                 , mFormat :: Maybe PandocFormat
                 , mString :: String
                 } deriving Show -- for debugging only     
               
   data P_Population
     = P_Popu { p_popm   :: P_Relation
              , p_type   :: P_Sign
              , p_popps  :: Pairs
              }

   data P_Interface = 
        P_Ifc { ifc_Name   :: String                 -- ^ the name of the interface
              , ifc_Params :: [(P_Relation,P_Sign)]  -- ^ a list of relations, which are editable within this interface.
              , ifc_Args   :: [[String]]             -- ^ a list of arguments for code generation.
              , ifc_Roles  :: [String]               -- ^ a list of roles that may use this interface
              , ifc_Obj    :: P_ObjectDef            -- ^ the context expression (mostly: I[c])
              , ifc_Pos    :: Origin
              , ifc_Expl   :: String
              } --deriving Show

   instance Identified P_Interface where
    name = ifc_Name

   instance Traced P_Interface where
    origin = ifc_Pos

   data P_SubInterface = P_Box [P_ObjectDef] | P_InterfaceRef Origin String deriving (Eq, Show) 

   data P_ObjectDef = 
        P_Obj { obj_nm   :: String         -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
              , obj_pos  :: Origin         -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number)
              , obj_ctx  :: P_Expression   -- ^ this expression describes the instances of this object, related to their context. 
              , obj_msub :: Maybe P_SubInterface  -- ^ the attributes, which are object definitions themselves.
              , obj_strs :: [[String]]     -- ^ directives that specify the interface.
              }  deriving (Eq, Show)       -- just for debugging (zie ook instance Show ObjectDef)
   instance Identified P_ObjectDef where
    name = obj_nm
   instance Traced P_ObjectDef where
    origin = obj_pos

   data P_KeyDef = 
            P_Kd { kd_pos :: Origin         -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number).
                 , kd_lbl :: String         -- ^ the name (or label) of this Key. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
                 , kd_cpt :: P_Concept      -- ^ this expression describes the instances of this object, related to their context
                 , kd_ats :: [P_KeySegment] -- ^ the constituent segments of this key.
                 } --deriving (Eq,Show)
   instance Identified P_KeyDef where
    name = kd_lbl

   instance Traced P_KeyDef where
    origin = kd_pos
   
   data P_KeySegment = P_KeyExp P_ObjectDef | P_KeyText String deriving (Eq, Show) -- A name/expression pairs or a piece of text
   
-- PPurpose is a parse-time constructor. It contains the name of the object it explains.
-- It is a pre-explanation in the sense that it contains a reference to something that is not yet built by the compiler.
--                       Constructor      name          RefID  Explanation
   data PRef2Obj = PRef2ConceptDef String
                 | PRef2Declaration (P_Relation, P_Sign)
                 | PRef2Rule String
                 | PRef2KeyDef String
                 | PRef2Pattern String
                 | PRef2Process String
                 | PRef2Interface String
                 | PRef2Context String
                 | PRef2Fspc String
                 deriving Show -- only for fatal error messages
   
   instance Identified PRef2Obj where
     name pe = case pe of 
        PRef2ConceptDef str -> str
        PRef2Declaration (rel, _) -> name rel
        PRef2Rule str -> str
        PRef2KeyDef str -> str
        PRef2Pattern str -> str
        PRef2Process str -> str
        PRef2Interface str -> str
        PRef2Context str -> str
        PRef2Fspc str -> str

   data PPurpose = PRef2 { pexPos   :: Origin     -- the position in the Ampersand script of this purpose definition
                         , pexObj   :: PRef2Obj   -- the reference to the object whose purpose is explained
                         , pexMarkup:: P_Markup   -- the piece of text, including markup and language info
                         , pexRefID :: String     -- the reference (for traceability)
                         }

   instance Identified PPurpose where
    name pe = name (pexObj pe)

   instance Traced PPurpose where
    origin = pexPos

   data P_Concept
      = PCpt{ p_cptnm :: String }  -- ^The name of this Concept
      | P_Singleton 
      deriving (Eq, Ord)
-- (Sebastiaan 12 feb 2012) P_Concept has been defined Ord, only because we want to maintain sets of concepts in the type checker for quicker lookups.

   instance Identified P_Concept where
    name (PCpt {p_cptnm = nm}) = nm
    name P_Singleton = "ONE"
   
   instance Show P_Concept where
    showsPrec _ c = showString (name c)


   data P_Sign = P_Sign {psign :: [P_Concept] }  -- will contain no more than two elements

   instance Eq P_Sign  where
     P_Sign [] == P_Sign _  = fatal 122 "Equality on P_Sign requires defined types."
     P_Sign _  == P_Sign [] = fatal 123 "Equality on P_Sign requires defined types."
     P_Sign s  == P_Sign s' = head s==head s'  &&  last s==last s'

   instance Show P_Sign where
     showsPrec _ s = 
         showString (   "[" ++ intercalate "*" (map show (psign s)) ++ "]" )

   type P_Gens = [P_Gen]
   data P_Gen = PGen{ gen_fp  :: Origin         -- ^ the position of the GEN-rule
                    , gen_gen :: P_Concept      -- ^ generic concept
                    , gen_spc :: P_Concept      -- ^ specific concept
                    }
   instance Eq P_Gen where
       g == g' = gen_gen g == gen_gen g' &&
                 gen_spc g == gen_spc g'

   instance Show P_Gen where
    -- This show is used in error messages. It should therefore not display the term's type
    showsPrec _ g = showString ("SPEC "++show (gen_spc g)++" ISA "++show (gen_gen g))

   instance Traced P_Gen where
    origin = gen_fp

   data Lang = Dutch | English deriving (Show, Eq)

   data PandocFormat = HTML | ReST | LaTeX | Markdown deriving (Eq, Show)

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
                    deriving (Eq,Ord)
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

   data Label = Lbl { lblnm   :: String
                    , lblpos  :: Origin
                    , lblstrs :: [[String]]
                    }
   instance Eq Label where
    l==l' = lblnm l==lblnm l'

