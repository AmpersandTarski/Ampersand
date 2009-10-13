{-# OPTIONS_GHC -Wall #-}
{- | The intentions behind Fspc (SJ 30 dec 2008):
Generation of functional specifications is the core functionality of ADL.
All items in a specification are generated into the following data structure, Fspc.
It is built by compiling an ADL-script and translating that to Fspc.
In the future, other ways of 'filling' Fspc are foreseen.
All generators (such as the code generator, the proof generator, the atlas generator, etc.)
are merely different ways to show Fspc.
-}
module Data.Fspec ( 
               Fspc(..)
             , Fservice(..)
             , FSid(..)
             , FTheme(..)
             , WSOperation(..), WSAction(..)
             , InsDel(..), ECArule(..), Event(..), PAclause(..)
             )
where
import Adl.Pattern                   (Pattern)
import Adl.Rule                      (Rule(..))
import Adl.ObjectDef                 (ObjectDef)
import Adl.Expression                (Expression)
import Adl.MorphismAndDeclaration    (Morphism,Declaration)
import Adl.Concept                   (Concept)
import Adl.Pair
import Typology                      (Inheritance)
import Data.Plug
import Rendering.ClassDiagram 
data Fspc = Fspc  { fsfsid   :: FSid          -- ^ The name of the specification
                  , datasets :: [ObjectDef]   -- ^ This list contains the data sets that are computed from the basic ontology.
                  , vplugs   :: [Plug]        -- ^ all plugs defined in the ADL-script
                  , plugs    :: [Plug]        -- ^ all plugs (defined and derived)
                  , serviceS :: [ObjectDef]   -- ^ all services defined in the ADL-script
                  , serviceG :: [ObjectDef]   -- ^ all services derived from the basic ontology
                  , services :: [Fservice]    -- ^ One for every service 
                  , vrules   :: [Rule]        -- ^ One for every rule
                  , vrels    :: [Declaration] -- ^ One for every declaration
                  , fsisa    :: (Inheritance Concept) -- ^ The data structure containing the generalization structure of concepts
                  , vpatterns:: [Pattern]
                  , classdiagrams :: [ClassDiag]
                  , themes :: [FTheme]
                  , violations :: [(Rule,Paire)]
                  }
           
--DESCR -> Fservice is like Fspc only within the scope of one ObjectDef
--         It contains the ObjectDef and precalculated structures
--EXTEND ->, trBoundary :: [Expression]
--         , ecaRules   :: [ECArule] -> see revision around 340
data Fservice = Fservice 
                    { objectdef  :: ObjectDef
                    }

--DESCR -> a theme is dataset service with functions under certain rules
--         the dataset is identified by one root concept
data FTheme = FTheme {tconcept :: Concept, tfunctions :: [WSOperation], trules :: [Rule]}
{- from http://www.w3.org/TR/wsdl20/#InterfaceOperation
 - "The properties of the Interface Operation component are as follows:
 - ...
 - * {interface message references} OPTIONAL. A set of Interface Message Reference components for the ordinary messages the operation accepts or sends.
 - ..."
 -}
data WSOperation = WSOper {wsaction::WSAction, wsmsgin::[ObjectDef], wsmsgout::[ObjectDef]}
data WSAction = WSCreate | WSRead | WSUpdate |WSDelete

data FSid = FS_id String     -- Identifiers in the Functional Specification Language contain strings that do not contain any spaces.
        --  | NoName           -- some identified objects have no name...

-- | The following datatypes form a process algebra. ADL derives the process logic from the static logic by interpreting an expression in relation algebra as an invariant.
--   An example: suppose you have large shoes, which means that there is no way you can fit you shoes through your trousers. What does this mean for the process of dressing in the morning? Well, if the shoes won't fit through your trousers, you must first put on your trousers, and then put on your shoes. So the order of putting on trousers and putting on shoes is dictated by the (static) fact that your shoes are too big to fit through your trousers. When undressing, the order is reversed: you must take off your shoes before taking off your trousers. This example ilustrates how the order of activities is restricted by an invariant property. So it is possible to derive some dynamic behaviour from static properties.

data InsDel   = Ins | Del
                deriving (Eq,Show)
data ECArule  = ECA { ecaTriggr :: Event
                    , ecaAction :: PAclause
                    }
data Event    = On { eSrt :: InsDel
                   , eMhp :: Morphism
                   }
data PAclause = Choice { paCls:: [PAclause]
                       }
              | All { paCls   :: [PAclause]}
              | Do  { paSrt   :: InsDel         -- do Insert or Delete
                    , paTo    :: Expression     -- into toExpr    or from toExpr
                    , paDelta ::Expression     -- delta
                    }
              | New { paNew :: Concept }        -- makes a new instance of type c
 --                  deriving Show

--   instance Show ECArule where
--    showsPrec p (ECA event pa) = showString (show event++" "++show pa)
--   instance Show Event where
--    showsPrec p (On Ins m) = showString ("ON INSERT Delta IN "++show m)
--    showsPrec p (On Del m) = showString ("ON DELETE Delta FROM "++show m)

--   instance Show PAclause where
--    showsPrec p fragm = showString ("ON "++show "\n  " fragm)
