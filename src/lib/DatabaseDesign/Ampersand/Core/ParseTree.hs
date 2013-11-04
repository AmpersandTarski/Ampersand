{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Core.ParseTree (
     P_Context(..)
   , Meta(..)
   , MetaObj(..)
   , P_Process(..)
   , P_RoleRelation(..)
   , RoleRule(..)
   , P_Pattern(..)
   , RelConceptDef(..), P_Declaration(..)
   , Term(..), TermPrim(..)
   , P_PairView(..), P_PairViewSegment(..), SrcOrTgt(..), isSrc
   , P_Rule(..)
   , ConceptDef(..)
   , P_Population(..)
   
   , P_ObjectDef, P_SubInterface, P_Interface(..), P_ObjDef(..), P_SubIfc(..)
   
   , P_IdentDef(..) , P_IdentSegment(..)
   , P_ViewDef(..) , P_ViewSegment(..)
   
   , PPurpose(..),PRef2Obj(..),PMeaning(..)
   
   , P_Concept(..), P_Sign(..)
   
   , P_Gen(..)
   
   , Lang(..)
   , P_Markup(..)
   
   , PandocFormat(..)
   
   , Label(..)
   
   , Prop(..), Props
   -- Inherited stuff: 
   , module DatabaseDesign.Ampersand.Input.ADL1.FilePos
   , module DatabaseDesign.Ampersand.ADL1.Pair
   , gen_concs
  )
where
   import DatabaseDesign.Ampersand.Input.ADL1.FilePos           
   import DatabaseDesign.Ampersand.Basics
   import DatabaseDesign.Ampersand.ADL1.Pair (Pairs,Paire,mkPair ,srcPaire, trgPaire)
   import Data.Traversable
   import Data.Foldable
   import Prelude hiding (foldr)
   import Control.Applicative

   fatal :: Int -> String -> a
   fatal = fatalMsg "ParseTree"
   
   data P_Context
      = PCtx{ ctx_nm ::     String           -- ^ The name of this context
            , ctx_pos ::    [Origin]         -- ^ The origin of the context. A context can be a merge of a file including other files c.q. a list of Origin.
            , ctx_lang ::   Maybe Lang       -- ^ The default language specified by this context, if specified at all.
            , ctx_markup :: Maybe PandocFormat  -- ^ The default markup format for free text in this context
            , ctx_thms ::   [String]         -- ^ Names of patterns/processes to be printed in the functional specification. (For partial documents.)
            , ctx_pats ::   [P_Pattern]      -- ^ The patterns defined in this context
            , ctx_PPrcs ::  [P_Process]      -- ^ The processes as defined by the parser
            , ctx_rs ::     [P_Rule]         -- ^ All user defined rules in this context, but outside patterns and outside processes
            , ctx_ds ::     [P_Declaration]  -- ^ The declarations defined in this context, outside the scope of patterns
            , ctx_cs ::     [ConceptDef]     -- ^ The concept definitions defined in this context, outside the scope of patterns
            , ctx_ks ::     [P_IdentDef]     -- ^ The identity definitions defined in this context, outside the scope of patterns
            , ctx_vs ::     [P_ViewDef]      -- ^ The view definitions defined in this context, outside the scope of patterns
            , ctx_gs ::     [P_Gen]          -- ^ The gen definitions defined in this context, outside the scope of patterns
            , ctx_ifcs ::   [P_Interface]    -- ^ The interfaces defined in this context, outside the scope of patterns
            , ctx_ps ::     [PPurpose]       -- ^ The purposes defined in this context, outside the scope of patterns
            , ctx_pops ::   [P_Population]   -- ^ The populations defined in this context
            , ctx_sql ::    [P_ObjectDef]    -- ^ user defined sqlplugs, taken from the Ampersand script
            , ctx_php ::    [P_ObjectDef]    -- ^ user defined phpplugs, taken from the Ampersand script
            , ctx_metas ::  [Meta]         -- ^ generic meta information (name/value pairs) that can be used for experimenting without having to modify the adl syntax
            } deriving Show

--   instance Show P_Context where
--     showsPrec _ = showString . ctx_nm

   instance Eq P_Context where
     c1 == c2  =  name c1 == name c2

   instance Identified P_Context where
     name = ctx_nm
   
   -- for declaring name/value pairs with information that is built in to the adl syntax yet
   data Meta = Meta { mtPos :: Origin
                 , mtObj :: MetaObj
                 , mtName :: String
                 , mtVal :: String
                 } deriving (Show)
   data MetaObj = ContextMeta deriving Show -- for now, we just have meta data for the entire context  
   
   -- | A RoleRelation rs means that any role in 'rrRoles rs' may edit any Relation  in  'rrInterfaces rs'
   data P_RoleRelation
      = P_RR { rr_Roles :: [String]  -- ^ name of a role
             , rr_Rels :: [TermPrim] -- ^ Typically: PTrel nm sgn, with nm::String and sgn::P_Sign representing a Relation with type information
             , rr_Pos :: Origin      -- ^ position in the Ampersand script
             } deriving (Show)       -- deriving Show is just for debugging
   instance Eq P_RoleRelation where rr==rr' = origin rr==origin rr'
   instance Traced P_RoleRelation where
    origin = rr_Pos

   data P_Process
      = P_Prc { procNm :: String
              , procPos :: Origin             -- ^ the start position in the file
              , procEnd :: Origin             -- ^ the end position in the file
              , procRules :: [P_Rule]         -- ^ the rules in this process
              , procGens :: [P_Gen]           -- ^ the generalizations in this process
              , procDcls :: [P_Declaration]   -- ^ the relation declarations in this process
              , procRRuls :: [RoleRule]       -- ^ The assignment of roles to rules.
              , procRRels :: [P_RoleRelation] -- ^ The assignment of roles to Relations.
              , procCds :: [ConceptDef]       -- ^ The concept definitions defined in this process
              , procIds :: [P_IdentDef]       -- ^ The identity definitions defined in this process
              , procVds :: [P_ViewDef]        -- ^ The view definitions defined in this process
              , procXps :: [PPurpose]         -- ^ The purposes of elements defined in this process
              , procPop :: [P_Population]     -- ^ The populations that are local to this process
              } deriving Show

   instance Identified P_Process where
    name = procNm 

   instance Traced P_Process where
    origin = procPos

    -- | A RoleRule r means that a role called 'mRoles r' must maintain the process rule called 'mRules r'
   data RoleRule
      = Maintain
        { mRoles :: [String]    -- ^ name of a role
        , mRules :: [String]    -- ^ name of a Rule
        , mPos :: Origin      -- ^ position in the Ampersand script
        } deriving (Eq, Show)   -- deriving (Eq, Show) is just for debugging

   instance Traced RoleRule where
    origin = mPos

   data P_Pattern
      = P_Pat { pt_nm :: String            -- ^ Name of this pattern
              , pt_pos :: Origin           -- ^ the starting position in the file in which this pattern was declared.
              , pt_end :: Origin           -- ^ the end position in the file in which this pattern was declared.
              , pt_rls :: [P_Rule]         -- ^ The user defined rules in this pattern
              , pt_gns :: [P_Gen]          -- ^ The generalizations defined in this pattern
              , pt_dcs :: [P_Declaration]  -- ^ The declarations declared in this pattern
              , pt_rus :: [RoleRule]       -- ^ The assignment of roles to rules.
              , pt_res :: [P_RoleRelation] -- ^ The assignment of roles to Relations.
              , pt_cds :: [ConceptDef]     -- ^ The concept definitions defined in this pattern
              , pt_ids :: [P_IdentDef]     -- ^ The identity definitions defined in this pattern
              , pt_vds :: [P_ViewDef]      -- ^ The view definitions defined in this pattern
              , pt_xps :: [PPurpose]       -- ^ The purposes of elements defined in this pattern
              , pt_pop :: [P_Population]   -- ^ The populations that are local to this pattern
              }   deriving (Show)       -- for debugging purposes

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
   
   data RelConceptDef = RelConceptDef SrcOrTgt String deriving (Eq, Show)
   
   data P_Declaration = 
         P_Sgn { dec_nm :: String    -- ^ the name of the declaration
               , dec_sign :: P_Sign    -- ^ the type. Parser must guarantee it is not empty.
               , dec_prps :: Props     -- ^ the user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
               , dec_prL :: String    -- ^ three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
               , dec_prM :: String    -- ^    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
               , dec_prR :: String
               , dec_Mean :: [PMeaning]  -- ^ the optional meaning of a declaration, possibly more than one for different languages.
               , dec_conceptDef :: Maybe RelConceptDef -- ^ alternative definition for the source or target concept in the context of this relation
               , dec_popu :: Pairs     -- ^ the list of tuples, of which the relation consists.
               , dec_fpos :: Origin    -- ^ the position in the Ampersand source file where this declaration is declared. Not all decalartions come from the ampersand souce file. 
               , dec_plug :: Bool      -- ^ if true, this relation may not be stored in or retrieved from the standard database (it should be gotten from a Plug of some sort instead)
               } deriving Show -- for debugging and testing only
   instance Eq P_Declaration where
    decl==decl' = origin decl==origin decl'
   instance Prelude.Ord P_Declaration where
    decl `compare` decl' = origin decl `compare` origin decl'
   instance Identified P_Declaration where
    name = dec_nm
   instance Traced P_Declaration where
    origin = dec_fpos
   
   data TermPrim
      = PI Origin                              -- ^ identity element without a type
                                               --   At parse time, there may be zero or one element in the list of concepts.
                                               --   Reason: when making eqClasses, the least element of that class is used as a witness of that class
                                               --   to know whether an eqClass represents a concept, we only look at its witness
                                               --   By making Pid the first in the data decleration, it becomes the least element for "deriving Ord".
      | Pid Origin P_Concept                   -- ^ identity element restricted to a type
      | Patm Origin String (Maybe P_Concept)   -- ^ an atom, possibly with a type
      | PVee Origin                            -- ^ the complete relation, of which the type is yet to be derived by the type checker.
      | Pfull Origin P_Concept P_Concept       -- ^ the complete relation, restricted to a type.
                                               --   At parse time, there may be zero, one or two elements in the list of concepts.
      | Prel Origin String                     -- ^ we expect expressions in flip-normal form
      | PTrel Origin String P_Sign             -- ^ type cast expression ... [c] (defined tuple instead of list because ETyp only exists for actual casts)
      deriving Show
   data Term a
      = Prim a
      | Pequ Origin (Term a) (Term a)  -- ^ equivalence             =
      | Pimp Origin (Term a) (Term a)  -- ^ implication             |-
      | PIsc Origin (Term a) (Term a)  -- ^ intersection            /\
      | PUni Origin (Term a) (Term a)  -- ^ union                   \/
      | PDif Origin (Term a) (Term a)  -- ^ difference              -
      | PLrs Origin (Term a) (Term a)  -- ^ left residual           /
      | PRrs Origin (Term a) (Term a)  -- ^ right residual          \
      | PCps Origin (Term a) (Term a)  -- ^ composition             ;
      | PRad Origin (Term a) (Term a)  -- ^ relative addition       !
      | PPrd Origin (Term a) (Term a)  -- ^ cartesian product       *
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
       Pequ o a b -> Pequ o <$> (f a) <*> (f b)
       Pimp o a b -> Pimp o <$> (f a) <*> (f b)
       PIsc o a b -> PIsc o <$> (f a) <*> (f b)
       PUni o a b -> PUni o <$> (f a) <*> (f b)
       PDif o a b -> PDif o <$> (f a) <*> (f b)
       PLrs o a b -> PLrs o <$> (f a) <*> (f b)
       PRrs o a b -> PRrs o <$> (f a) <*> (f b)
       PCps o a b -> PCps o <$> (f a) <*> (f b)
       PRad o a b -> PRad o <$> (f a) <*> (f b)
       PPrd o a b -> PPrd o <$> (f a) <*> (f b)
       PKl0 o a   -> PKl0 o <$> (f a)
       PKl1 o a   -> PKl1 o <$> (f a)
       PFlp o a   -> PFlp o <$> (f a)
       PCpl o a   -> PCpl o <$> (f a)
       PBrk o a   -> PBrk o <$> (f a)
     where f = traverse f'
   
   
   instance Functor P_SubIfc where fmap = fmapDefault
   instance Foldable P_SubIfc where foldMap = foldMapDefault
   instance Traversable P_SubIfc where
    traverse _ (P_InterfaceRef a b) = pure (P_InterfaceRef a b)
    traverse f (P_Box b lst) = P_Box b <$> (traverse (traverse f) lst)
   
   instance Traced (P_SubIfc a) where
    origin = si_ori
   
   instance Functor P_ObjDef where fmap = fmapDefault
   instance Foldable P_ObjDef where foldMap = foldMapDefault
   instance Traversable P_ObjDef where
    traverse f (P_Obj nm pos ctx msub strs)
     = (\ctx' msub'->(P_Obj nm pos ctx' msub' strs)) <$>
        traverse f ctx <*> traverse (traverse f) msub
   
   instance Traced TermPrim where
    origin e = case e of
      PI orig        -> orig
      Pid orig _     -> orig
      Patm orig _ _  -> orig
      PVee orig      -> orig
      Pfull orig _ _ -> orig
      Prel orig _    -> orig
      PTrel orig _ _ -> orig
      
   instance Traced a => Traced (Term a) where
    origin e = case e of
      Prim a         -> origin a
      Pequ orig _ _  -> orig
      Pimp orig _ _  -> orig
      PIsc orig _ _  -> orig
      PUni orig _ _  -> orig
      PDif orig _ _  -> orig
      PLrs orig _ _  -> orig
      PRrs orig _ _  -> orig
      PCps orig _ _  -> orig
      PRad orig _ _  -> orig
      PPrd orig _ _  -> orig
      PKl0 orig _    -> orig
      PKl1 orig _    -> orig
      PFlp orig _    -> orig
      PCpl orig _    -> orig
      PBrk orig _    -> orig

   data SrcOrTgt = Src | Tgt deriving (Show, Eq, Ord)
   instance Flippable SrcOrTgt where
     flp Src = Tgt
     flp Tgt = Src
   
   isSrc :: SrcOrTgt -> Bool
   isSrc Src = True
   isSrc Tgt = False
   
   data P_PairView = P_PairView { ppv_segs :: [P_PairViewSegment] } deriving Show

   data P_PairViewSegment = P_PairViewText String
                          | P_PairViewExp SrcOrTgt (Term TermPrim)
            deriving Show

   data P_Rule  =
      P_Ru { rr_nm ::   String            -- ^ Name of this rule
           , rr_exp ::  (Term TermPrim)   -- ^ The rule expression 
           , rr_fps ::  Origin            -- ^ Position in the Ampersand file
           , rr_mean :: [PMeaning]        -- ^ User-specified meanings, possibly more than one, for multiple languages.
           , rr_msg ::  [P_Markup]        -- ^ User-specified violation messages, possibly more than one, for multiple languages.
           , rr_viol :: Maybe P_PairView  -- ^ Custom presentation for violations, currently only in a single language
           } deriving Show

   instance Traced P_Rule where
    origin = rr_fps

   instance Identified P_Rule where
    name = rr_nm

   data PMeaning = PMeaning P_Markup 
            deriving Show

   data P_Markup = 
       P_Markup  { mLang ::   Maybe Lang
                 , mFormat :: Maybe PandocFormat
                 , mString :: String
                 } deriving Show -- for debugging only     
               
   data P_Population
     = P_RelPopu { p_rnme ::  String  -- the name of a relation
                 , p_orig ::  Origin  -- the origin 
                 , p_popps :: Pairs   -- the contents
                 }
     | P_TRelPop { p_rnme ::  String  -- the name of a relation
                 , p_type ::  P_Sign  -- the sign of the relation
                 , p_orig ::  Origin  -- the origin 
                 , p_popps :: Pairs   -- the contents
                 }
     | P_CptPopu { p_cnme ::  String  -- the name of a concept
                 , p_orig ::  Origin  -- the origin
                 , p_popas :: [String]   -- atoms in the initial population of that concept
                 }
       deriving Show
       
   instance Identified P_Population where
    name P_RelPopu{p_rnme = nm} = nm
    name P_TRelPop{p_rnme = nm} = nm
    name P_CptPopu{p_cnme = nm} = nm
    
   instance Traced P_Population where
    origin = p_orig

   data P_Interface = 
        P_Ifc { ifc_Name :: String           -- ^ the name of the interface
              , ifc_Params :: [TermPrim]         -- ^ a list of relations, which are editable within this interface.
                                             --   either   Prel o nm
                                             --       or   PTrel o nm sgn
              , ifc_Args :: [[String]]       -- ^ a list of arguments for code generation.
              , ifc_Roles :: [String]        -- ^ a list of roles that may use this interface
              , ifc_Obj :: P_ObjectDef       -- ^ the context expression (mostly: I[c])
              , ifc_Pos :: Origin
              , ifc_Prp :: String
              } deriving Show

   instance Identified P_Interface where
    name = ifc_Name

   instance Traced P_Interface where
    origin = ifc_Pos
   
   type P_SubInterface = P_SubIfc TermPrim
   data P_SubIfc a
                 = P_Box          { si_ori :: Origin
                                  , si_box :: [P_ObjDef a] }
                 | P_InterfaceRef { si_ori :: Origin
                                  , si_str :: String }
                   deriving (Eq, Show)

   type P_ObjectDef = P_ObjDef TermPrim
   data P_ObjDef a = 
        P_Obj { obj_nm :: String         -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
              , obj_pos :: Origin         -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number)
              , obj_ctx :: Term a        -- ^ this expression describes the instances of this object, related to their context. 
              , obj_msub :: Maybe (P_SubIfc a)  -- ^ the attributes, which are object definitions themselves.
              , obj_strs :: [[String]]     -- ^ directives that specify the interface.
              }  deriving (Show)       -- just for debugging (zie ook instance Show ObjectDef)
   instance Eq (P_ObjDef a) where od==od' = origin od==origin od'
   instance Identified (P_ObjDef a) where
    name = obj_nm
   instance Traced (P_ObjDef a) where
    origin = obj_pos

   data P_IdentDef =
            P_Id { ix_pos :: Origin         -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number).
                 , ix_lbl :: String         -- ^ the name (or label) of this Identity. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface. It is not an empty string.
                 , ix_cpt :: P_Concept      -- ^ this expression describes the instances of this object, related to their context
                 , ix_ats :: [P_IdentSegment] -- ^ the constituent segments of this identity. TODO: refactor to a list of terms
                 } deriving (Show)
   instance Identified P_IdentDef where
    name = ix_lbl
   instance Eq P_IdentDef where identity==identity' = origin identity==origin identity'

   instance Traced P_IdentDef where
    origin = ix_pos
   
   data P_IdentSegment 
                 = P_IdentExp  { ks_obj :: P_ObjectDef }
                   deriving (Eq, Show)

   data P_ViewDef = 
            P_Vd { vd_pos :: Origin         -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number).
                 , vd_lbl :: String         -- ^ the name (or label) of this View. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface. It is not an empty string.
                 , vd_cpt :: P_Concept      -- ^ this expression describes the instances of this object, related to their context
                 , vd_ats :: [P_ViewSegment] -- ^ the constituent segments of this view.
                 } deriving (Show)
   instance Identified P_ViewDef where
    name = vd_lbl
   instance Eq P_ViewDef where vd==vd' = origin vd==origin vd'

   instance Traced P_ViewDef where
    origin = vd_pos
   
   data P_ViewSegment 
                 = P_ViewExp  { vs_obj :: P_ObjectDef }
                 | P_ViewText { vs_txt :: String }
                 | P_ViewHtml { vs_htm :: String }
                   deriving (Eq, Show)
                  
-- PPurpose is a parse-time constructor. It contains the name of the object it explains.
-- It is a pre-explanation in the sense that it contains a reference to something that is not yet built by the compiler.
--                       Constructor      name          RefID  Explanation
   data PRef2Obj = PRef2ConceptDef String
                 | PRef2Declaration TermPrim -- typically PTrel o nm sgn,   with nm::String and sgn::P_Sign
                                         -- or        Prel o nm; Other terms become fatals
                 | PRef2Rule String
                 | PRef2IdentityDef String
                 | PRef2ViewDef String
                 | PRef2Pattern String
                 | PRef2Process String
                 | PRef2Interface String
                 | PRef2Context String
                 | PRef2Fspc String
                 deriving Show -- only for fatal error messages
   
   instance Identified PRef2Obj where
     name pe = case pe of 
        PRef2ConceptDef str -> str
        PRef2Declaration (PTrel _ nm sgn) -> nm++show sgn
        PRef2Declaration (Prel _ nm) -> nm
        PRef2Declaration expr -> fatal 362 ("Expression "++show expr++" should never occur in PRef2Declaration")
        PRef2Rule str -> str
        PRef2IdentityDef str -> str
        PRef2ViewDef str -> str
        PRef2Pattern str -> str
        PRef2Process str -> str
        PRef2Interface str -> str
        PRef2Context str -> str
        PRef2Fspc str -> str

   data PPurpose = PRef2 { pexPos :: Origin     -- the position in the Ampersand script of this purpose definition
                         , pexObj :: PRef2Obj   -- the reference to the object whose purpose is explained
                         , pexMarkup:: P_Markup   -- the piece of text, including markup and language info
                         , pexRefID :: String     -- the reference (for traceability)
                         } deriving Show

   instance Identified PPurpose where
    name pe = name (pexObj pe)

   instance Traced PPurpose where
    origin = pexPos

   data P_Concept
      = PCpt{ p_cptnm :: String }  -- ^The name of this Concept
      | P_Singleton 
--      deriving (Eq, Ord)
-- (Sebastiaan 12 feb 2012) P_Concept has been defined Ord, only because we want to maintain sets of concepts in the type checker for quicker lookups.
-- (Sebastiaan 11 okt 2013) Removed this again, I thought it would be more clean to use newtype for this instead

   instance Identified P_Concept where
    name (PCpt {p_cptnm = nm}) = nm
    name P_Singleton = "ONE"
   
   instance Show P_Concept where
    showsPrec _ c = showString (name c)


   data P_Sign = P_Sign {pSrc :: P_Concept, pTrg :: P_Concept }

   instance Show P_Sign where
     showsPrec _ sgn = 
         showString (   "[" ++ show (pSrc sgn)++"*"++show (pTrg sgn) ++ "]" )

   data P_Gen =  P_Cy{ gen_spc :: P_Concept         -- ^ Left hand side concept expression 
                     , gen_rhs :: [P_Concept]       -- ^ Right hand side concept expression
                     , gen_fp  :: Origin            -- ^ Position in the Ampersand file
                     }
               | PGen{ gen_spc :: P_Concept      -- ^ specific concept
                     , gen_gen :: P_Concept      -- ^ generic concept
                     , gen_fp  :: Origin         -- ^ the position of the GEN-rule
                     }
   gen_concs :: P_Gen -> [P_Concept]
   gen_concs (P_Cy {gen_rhs=x}) = x
   gen_concs (PGen {gen_gen=x,gen_spc=y}) = [x,y]
      
   instance Show P_Gen where
    -- This show is used in error messages.
    showsPrec _ g = showString ("CLASSIFY "++show (gen_spc g)++" IS "++show (gen_concs g))

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

   instance Flippable Prop where
    flp Uni = Inj
    flp Tot = Sur
    flp Sur = Tot
    flp Inj = Uni
    flp x = x

   data Label = Lbl { lblnm :: String
                    , lblpos :: Origin
                    , lblstrs :: [[String]]
                    }
   instance Eq Label where
    l==l' = lblnm l==lblnm l'

