{- | The intentions behind Fspc (SJ 30 dec 2008):
Generation of functional specifications is the core functionality of ADL.
All items in a specification are generated into the following data structure, Fspc.
It is built by compiling an ADL-script and translating that to Fspc.
In the future, other ways of 'filling' Fspc are foreseen.
All generators (such as the code generator, the proof generator, the atlas generator, etc.)
are merely different ways to show Fspc.
-}
module Data.Fspec 
             (module Data.ADL
             , Fspc(..)
             , Ftheme(..)
             , Funit(..)
             , Fview(..)
             , Frule(..)
             , FViewDef(..)
             , ServiceSpec(..)
             , Dataset(..)
             , ParamSpec(..)
             , FSid(..)
             )
where
   import Data.ADL  
               ( Pattern
               , Rule
               , ObjectDef
               , Expression
               , Morphism
               , Declaration
               , Concept
               )

   import Typology(Inheritance)
   data Fspc = Fspc -- Fctx 
              { fsfsid   :: FSid          -- ^The name of the specification
              , themes   :: [Ftheme]      -- ^ One for every pattern
              , datasets :: [Dataset]     -- ^ This list contains the data sets that are computed from the basic ontology.
              , serviceS :: [ObjectDef]   -- ^ all services defined in the ADL-script
              , serviceG :: [ObjectDef]   -- ^ all services derived from the basic ontology
              , views    :: [Fview]       -- ^ One for every service 
              , vrules   :: [Frule]       -- ^ One for every rule
              , vrels    :: [Declaration] -- ^ One for every declaration
              , isa      :: (Inheritance Concept) -- ^ The data structure containing the generalization structure of concepts
              }
   data Ftheme  = Tspc                    --  The constructor
                 { ftsid :: FSid          -- ^ The name of the theme (aka pattern)
                 , units :: [Funit]       -- ^ The units of the theme
                 , ftpat :: Pattern       -- ^ Het pattern van de unit 
                 }

   data Funit = Uspc 
                  { fusid    :: FSid
                  , pattern  :: Pattern
                  , viewDefs :: [FViewDef]
                  , servDefs :: [ServiceSpec] -- services
                  }
              
   data Fview  = Fview 
               { dataset   :: Dataset
               , objectdef :: ObjectDef
               , services  :: [ServiceSpec]
               , frules    :: [Frule]
               }

   data Frule = Frul Rule

   data FViewDef = Vdef
                  { vdobjdef :: ObjectDef
                  , vdmorphs :: [Morphism]
                  , vdExprRules :: [(Expression,Rule)]
                  }

   data ServiceSpec = Sspc 
                       { ssid    :: FSid         -- name of the service
                       , sees    :: [Morphism]   -- the list of relations this service may see
                       , changes :: [Morphism]   -- the list of relations this service may change
             -- Hoort hier niet meer thuis             FPA          -- function point analysis information
                       , input   :: [ParamSpec]  -- parameters
                       , output  :: [ParamSpec]  -- results
                       , rs      :: [Rule]       -- Invariants
                       , pre     :: [String]     -- Preconditions
                       , post    :: [String]     -- Postconditions
                       }

   data Dataset = DS Concept     -- the root of the dataset
                     [Morphism]  -- the functions from the root
                | BR Morphism    -- for every m that is not (isFunction m || isFunction (flp m))

   data ParamSpec   = Aspc 
                      {pname :: FSid         -- name of the parameter
                      ,ptype :: String }     -- type of the parameter
                    --   WAAROM stond hieronder Pbool ?? HJO: Wat moet die Pbool hier?? Zat hier nog een gedachte achter? Zo niet, gooi deze regels dan maar weg, Stef.
                    --   Pbool

   data FSid = FS_id String     -- Identifiers in the Functional Specification Language contain strings that do not contain any spaces.
             | NoName           -- some identified objects have no name...
              