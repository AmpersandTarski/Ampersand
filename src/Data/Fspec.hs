{- | The intentions behind Fspc (SJ 30 dec 2008):
Generation of functional specifications is the core functionality of ADL.
All items in a specification are generated into the following data structure, Fspc.
It is built by compiling an ADL-script and translating that to Fspc.
In the future, other ways of 'filling' Fspc are foreseen.
All generators (such as the code generator, the proof generator, the atlas generator, etc.)
are merely different ways to show Fspc.
-}
module Data.Fspec ( 
               Pattern(..)
             , Rule(..)
             , ObjectDef(..)
             , Expression(..)
             , Morphism(..)
             , Declaration(..)
             , Concept(..)
             , Fspc(..)
             , Ftheme(..)
             , Funit(..)
             , Fservice(..)
             , FViewDef(..)
             , ServiceSpec(..)
             , ParamSpec(..)
             , FSid(..)
             , InsDel(..), ECArule(..), Event(..), PAclause(..)
             )
where
   import Adl.Pattern
   import Adl.Rule
   import Adl.ObjectDef
   import Adl.Expression
   import Adl.MorphismAndDeclaration
   import Adl.Concept
   import Typology(Inheritance)
   data Fspc = Fspc  { fsfsid   :: FSid          -- ^ The name of the specification
                     , themes   :: [Ftheme]      -- ^ One for every pattern
                     , datasets :: [ObjectDef]   -- ^ This list contains the data sets that are computed from the basic ontology.
                     , serviceS :: [ObjectDef]   -- ^ all services defined in the ADL-script
                     , serviceG :: [ObjectDef]   -- ^ all services derived from the basic ontology
                     , services :: [Fservice]    -- ^ One for every service 
                     , vrules   :: [Rule]        -- ^ One for every rule
                     , vrels    :: [Declaration] -- ^ One for every declaration
                     , fsisa    :: (Inheritance Concept) -- ^ The data structure containing the generalization structure of concepts
                     }
   data Ftheme = Tspc
                     { ftsid    :: FSid     -- ^ The name of the theme (aka pattern)
                     , units    :: [Funit]  -- ^ The units of the theme
                     , ftpat    :: Pattern  -- ^ Het pattern van de unit -- Obsolete
                     }

   data Funit = Uspc 
                  { fusid    :: FSid
                  , pattern  :: Pattern
                  , viewDefs :: [FViewDef]
                  , servDefs :: [ServiceSpec] -- services
                  }
              
   data Fservice = Fservice 
                       { objectdef  :: ObjectDef
                       , trBoundary :: [Expression]
                       , ecaRules   :: [ECArule]
                       , dataset    :: ObjectDef
                       , methods    :: [ServiceSpec]
                       , frules     :: [Rule]
                       }

   data FViewDef = Vdef
                  { vdobjdef :: ObjectDef
                  , vdmorphs :: [Morphism]
                  , vdExprRules :: [(Expression,Rule)]
                  }

   data ServiceSpec = Sspc 
                       { ssid    :: FSid         -- name of the service
                       , sees    :: [Morphism]   -- the list of relations this service may see
                       , changes :: [Morphism]   -- the list of relations this service may change
                       , input   :: [ParamSpec]  -- parameters
                       , output  :: [ParamSpec]  -- results
                       , rs      :: [Rule]       -- Invariants
                       , pre     :: [String]     -- Preconditions
                       , post    :: [String]     -- Postconditions
                       }

   data ParamSpec   = Aspc 
                      {pname :: FSid         -- name of the parameter
                      ,ptype :: String }     -- type of the parameter
                    --   WAAROM stond hieronder Pbool ?? HJO: Wat moet die Pbool hier?? Zat hier nog een gedachte achter? Zo niet, gooi deze regels dan maar weg, Stef.
                    --   Pbool

   data FSid = FS_id String     -- Identifiers in the Functional Specification Language contain strings that do not contain any spaces.
           --  | NoName           -- some identified objects have no name...

   -- | The following datatypes form a process algebra. ADL derives the process logic from the static logic by interpreting an expression in relation algebra as an invariant.
   --   An example: suppose you have large shoes, which means that there is no way you can fit you shoes through your trousers. What does this mean for the process of dressing in the morning? Well, if the shoes won't fit through your trousers, you must first put on your trousers, and then put on your shoes. So the order of putting on trousers and putting on shoes is dictated by the (static) fact that your shoes are too big to fit through your trousers. When undressing, the order is reversed: you must take off your shoes before taking off your trousers. This example ilustrates how the order of activities is restricted by an invariant property. So it is possible to derive some dynamic behaviour from static properties.

   data InsDel   = Ins | Del
                   deriving (Eq,Show)
   data ECArule  = ECA Event PAclause
   data Event    = On InsDel Morphism
   data PAclause = Choice [PAclause]
                 | All [PAclause]
                 | Do  InsDel         -- do Insert or Delete
                       Expression     -- into toExpr    or from toExpr
                       Expression     -- delta
                 | New Concept        -- makes a new instance of type c
 --                  deriving Show

--   instance Show ECArule where
--    showsPrec p (ECA event pa) = showString (show event++" "++show pa)
--   instance Show Event where
--    showsPrec p (On Ins m) = showString ("ON INSERT Delta IN "++show m)
--    showsPrec p (On Del m) = showString ("ON DELETE Delta FROM "++show m)

--   instance Show PAclause where
--    showsPrec p fragm = showString ("ON "++show "\n  " fragm)
