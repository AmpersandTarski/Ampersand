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
             )
where
import Adl.Pattern                   (Pattern)
import Adl.Rule                      (Rule(..))
import Adl.ECArule                   (ECArule(..))
import Adl.ObjectDef                 (ObjectDef)
import Adl.Expression                (Expression)
import Adl.MorphismAndDeclaration    (Morphism,Declaration)
import Adl.Concept                   (Concept)
import Adl.Pair
import Typology                      (Inheritance)
import Data.Plug
import Rendering.ClassDiagram 
data Fspc = Fspc  { fsfsid   :: FSid                  -- ^ The name of the specification
                  , datasets :: [ObjectDef]           -- ^ This list contains the data sets that are computed from the basic ontology.
                  , vplugs   :: [Plug]                -- ^ all plugs defined in the ADL-script
                  , plugs    :: [Plug]                -- ^ all plugs (defined and derived)
                  , serviceS :: [ObjectDef]           -- ^ all services defined in the ADL-script
                  , serviceG :: [ObjectDef]           -- ^ all services derived from the basic ontology
                  , services :: [Fservice]            -- ^ One for every service 
                  , vrules   :: [Rule]                -- ^ One for every rule
                  , ecaRules :: [ECArule]             -- ^ event-condition-action rules derived from the context
                  , vrels    :: [Declaration]         -- ^ One for every declaration
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

