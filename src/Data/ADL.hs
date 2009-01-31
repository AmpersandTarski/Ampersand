-- | This module contains the internal data structure of ADL. 
module Data.ADL where

   import Typology ( Inheritance())
   import Classification ( Classification())
   import UU_Scanner (Pos(Pos),noPos)

   -- | Architecture of ADL consists of a set of contexts
   data Architecture = Arch { archContexts :: Contexts} 

   type Contexts  = [Context]
   data Context   
      = Ctx { ctxnm    :: String                    -- ^ The name of this context
            , ctxon    :: [String]                  -- ^ The list of extends (= context names of contexts) whose rules are imported
            , ctxisa   :: (Inheritance Concept)     -- ^ A data structure containing the generalization structure of concepts
            , ctxwrld  :: [Classification Context]  -- ^ A tree, being the transitive closure of the 'extends' (see formal definition) relation.
            , ctxpats  :: Patterns                  -- ^ A list of patterns defined in this context
            , ctxrs    :: Rules                     -- ^ A list of all rules that are valid within this context
            , ctxds    :: Declarations              -- ^ A list of declarations defined in this context, outside the scope of patterns
            , ctxcs    :: ConceptDefs               -- ^ A list of concept definitions defined in this context, outside the scope of patterns
            , ctxks    :: KeyDefs                   -- ^ A list of key definitions defined in this context, outside the scope of patterns
            , ctxos    :: ObjectDefs                -- ^ A list of attributes defined in this context, outside the scope of patterns
            , ctxpops  :: Populations               -- ^ A list of populations defined in this context
            } 

   type Patterns  = [Pattern]
   data Pattern 
      = Pat { ptnm  :: String       -- ^ Name of this pattern
            , ptrls :: Rules        -- ^ List of rules declared in this pattern
            , ptgns :: Gens         -- ^ List of generalizations defined in this pattern
            , ptdcs :: Declarations -- ^ List of declarations defined in this pattern
            , ptcds :: ConceptDefs  -- ^ list of concept definitions defined in this pattern
            , ptkds :: KeyDefs      -- ^ list of key definitions defined in this pattern
            } 

   type Rules = [Rule]
   data Rule =
  -- Ru c antc p cons cpu expla sgn nr pn
        Ru { rrsrt :: RuleType          -- ^ One of the following:
                                        --    | Implication if this is an implication;
                                        --    | Equivalence if this is an equivalence;
                                        --    | AlwaysExpr  if this is an ALWAYS expression.
           , rrant :: Expression        -- ^ Antecedent
           , rrfps :: FilePos           -- ^ Position in the ADL file
           , rrcon :: Expression        -- ^ Consequent
           , r_cpu :: Expressions       -- ^ This is a list of subexpressions, which must be computed.
           , rrxpl :: String            -- ^ Explanation
           , rrtyp :: (Concept,Concept) -- ^ Sign of this rule
           , runum :: Int               -- ^ Rule number
           , r_pat :: String            -- ^ Name of pattern in which it was defined.
           }
  -- Sg p rule expla sgn nr pn signal
      | Sg { srfps :: FilePos           -- ^ position in the ADL file
           , srsig :: Rule              -- ^ the rule to be signalled
           , srxpl :: String            -- ^ explanation
           , srtyp :: (Concept,Concept) -- ^ type
           , runum :: Int               -- ^ rule number
           , r_pat :: String            -- ^ name of pattern in which it was defined.
           , srrel :: Declaration       -- ^ the signal relation
           }
  -- Gc p antc cons cpu _ _ _
      | Gc { grfps :: FilePos           -- ^ position in the ADL file
           , grspe :: Morphism          -- ^ specific
           , grgen :: Expression        -- ^ generic
           , r_cpu :: Expressions       -- ^ This is a list of subexpressions, which must be computed.
           , grtyp :: (Concept,Concept) -- ^ declaration
           , runum :: Int               -- ^ rule number
           , r_pat :: String            -- ^ name of pattern in which it was defined.
           }
  -- Fr t d expr pn  -- represents an automatic computation, such as * or +.
      | Fr { fraut :: AutType           -- ^ the type of automatic computation
           , frdec :: Declaration       -- ^ where the result is to be stored
           , frcmp :: Expression        -- ^ expression to be computed
           , frpat :: String            -- ^ name of pattern in which it was defined.
           } 
   data RuleType = Implication | Equivalence | AlwaysExpr | Generalization | Automatic deriving (Eq,Show)

   -- | WAAROM? Dit mag hier wel even expliciet worden uitgelegd. Hier zit vast een heel verhaal achter... Stef?
   data AutType = Clos0 | Clos1 deriving (Eq,Show)
        
   data Label = Lbl { lblnm   :: String
                    , lblpos  :: FilePos
                    , lblstrs :: [[String]]
                    }
   instance Eq Label where
    l==l' = lblnm l==lblnm l'

   type KeyDefs = [KeyDef]
   data KeyDef = Kd { kdpos :: FilePos      -- ^ position of this definition in the text of the ADL source file (filename, line number and column number).
                    , kdlbl :: String       -- ^ the name (or label) of this Key. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
                    , kdctx :: Expression   -- ^ this expression describes the instances of this object, related to their context
                    , kdats :: ObjectDefs   -- ^ the constituent attributes (i.e. name/expression pairs) of this key.
                    } 

   type Populations = [Population]
   data Population = Popu 
              { popm  :: Morphism
              , popps :: Pairs
              }

   type ObjectDefs = [ObjectDef]
   data ObjectDef = Obj { objnm   :: String         -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
                        , objpos  :: FilePos        -- ^ position of this definition in the text of the ADL source file (filename, line number and column number)
                        , objctx  :: Expression     -- ^ this expression describes the instances of this object, related to their context. 
                        , objats  :: ObjectDefs     -- ^ the attributes, which are object definitions themselves.
                        , objstrs :: [[String]]     -- ^ directives that specify the interface.
                        } 

   type Expressions = [Expression]
   data Expression  = Tm Morphism          -- ^ simple morphism, possibly conversed     ~
                    | Tc Expression        -- ^ bracketed expression                 ( ... )
                    | F  Expressions       -- ^ composition                             ;
                    | Fd Expressions       -- ^ relative addition                       !
                    | Fi Expressions       -- ^ intersection                            /\
                    | Fu Expressions       -- ^ union                                   \/
                    | K0 Expression        -- ^ Reflexive and transitive closure        *
                    | K1 Expression        -- ^ Transitive closure                      +
                    | Cp Expression        -- ^ Complement                              -

   type Gens      = [Gen]
   data Gen       = G { genfp  :: FilePos          -- ^ the position of the GEN-rule
                      , gengen :: Concept          -- ^ generic concept
                      , genspc :: Concept          -- ^ specific concept
                      }
                      
   type Morphisms = [Morphism]
   data Morphism  = 
                   Mph { mphnm :: String             -- ^ the name of the morphism. This is the same name as
                                                     --   the declaration that is bound to the morphism.
                                                     --    WAAROM Waarom zou je dit attribuut opnemen? De naam van het morphisme is immers altijd gelijk aan de naam van de Declaration mphdcl ....
                                                     --    ANTWOORD Tijdens het parsen, tot het moment dat de declaration aan het Morphism is gekoppeld, moet de naam van het Morphism bekend zijn. Nadat het morphisme gebonden is aan een declaration moet de naam van het morphisme gelijk zijn aan de naam van zijn mphdcl.
                       , mphpos :: FilePos           -- ^ the position of the rule in which the morphism occurs
                       , mphats :: [Concept]         -- ^ the attributes specified inline
                       , mphtyp :: Sign              -- ^ the allocated type. Together with the name, this forms the declaration.
                       , mphyin :: Bool              -- ^ the 'yin' factor. If true, a declaration is bound in the same direction as the morphism. If false, binding occurs in the opposite direction.
                       , mphdcl :: Declaration       -- ^ the declaration bound to this morphism.
                       }
                  | I  { mphats :: [Concept]         -- ^ the (optional) attribute specified inline. ADL syntax allows at most one concept in this list.
                       , mphgen ::  Concept          -- ^ the generic concept  
                       , mphspc ::  Concept          -- ^ the specific concept
                       , mphyin ::  Bool             -- ^ the 'yin' factor. If true, the specific concept is source and the generic concept is target. If false, the other way around.
                       } 
                  | V  { mphats :: [Concept]         -- ^ the (optional) attributes specified inline.
                       , mphtyp :: Sign              -- ^ the allocated type.
                       }
                  | Mp1 { mph1val :: String          -- ^ the value of the one morphism
                        , mph1typ :: Concept         -- ^ the allocated type.
                        }  


   type Declarations = [Declaration]
   data Declaration = 
           Sgn { decnm   :: String  -- ^ the name of the declaration
               , desrc   :: Concept -- ^ the source concept of the declaration
               , detgt   :: Concept -- ^ the target concept of the declaration
               , decprps :: Props   -- ^ the multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
               , decp1   :: String  -- ^ three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
               , decp2   :: String  -- ^    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
               , decp3   :: String
               , decpr   :: Pairs   -- ^ the list of tuples, of which the relation consists.
               , decexpl :: String  -- ^ the explanation
               , decfpos :: FilePos -- ^ the position in the ADL source file where this declaration is declared.
               , decid   :: Int     -- ^ a unique number that can be used to identify the relation
               , deciss  :: Bool    -- ^ if true, this is a signal relation; otherwise it is an ordinary relation.
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

   type ConceptDefs = [ConceptDef]
   data ConceptDef 
      = Cd  { cdpos :: FilePos  -- ^ The position of this definition in the text of the ADL source (filename, line number and column number).
            , cdnm  :: String   -- ^ The name of this concept. If there is no such concept, the conceptdefinition is ignored.
            , cddef :: String   -- ^ The textual definition of this concept.
            , cdref :: String   -- ^ A label meant to identify the source of the definition. (useful as LaTeX' symbolic reference)
            } 


   type Concepts = [Concept]
   data Concept
      = C   { cptnm :: String    -- ^The name of this Concept
            , cptgE :: GenR 
            , cptos :: [String]  -- ^Atoms
            }  -- ^C nm gE cs represents the set of instances cs by name nm.
      | S  -- ^The universal Singleton: 'I'['Anything'] = 'V'['Anything']
      | Anything -- ^Really Anything!
      | NOthing  -- ^Nothing at all
            
   type Props = [Prop]
   data Prop      = Uni          -- ^ univalent
                  | Inj          -- ^ injective
                  | Sur          -- ^ surjective
                  | Tot          -- ^ total
                  | Sym          -- ^ symmetric
                  | Asy          -- ^ antisymmetric
                  | Trn          -- ^ transitive
                  | Rfx          -- ^ reflexive
                  | Aut          -- ^ calculate contents automatically if possible
                    deriving (Eq,Ord)


   type Sign = (Concept,Concept) 
   

   type Pairs     = [Paire]  -- WAAROM? Zouden dit niet tweetallen moeten zijn? In dit geval mogen Paire ook uit meer dan twee bestaan...
   type Paire     = [String]
   
   type GenR = Concept->Concept->Bool

   newtype FilePos = FilePos (String, Pos, String) 
                          deriving (Eq,Ord)
   posNone :: FilePos
   posNone = FilePos ("",noPos,"")
   instance Ord Pos where
     a >= b = (show a) >= (show b)
     a <= b = (show a) <= (show b)
     
   instance Show Pos where
     show (Pos l c)
       = "line " ++ show l
         ++ ", column " ++ show c

   instance Show FilePos where
     show (FilePos (fn,Pos l c,sym))
       = "line " ++ show l
         ++ ", file " ++ show fn
     
            