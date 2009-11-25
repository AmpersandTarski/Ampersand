{-# OPTIONS_GHC -Wall #-}
{- | The intentions behind Fspc (SJ 30 dec 2008):
Generation of functional specifications is the core functionality of ADL.
All items in a specification are generated into the following data structure, Fspc.
It is built by compiling an ADL-script and translating that to Fspc.
In the future, other ways of 'filling' Fspc are foreseen.
All generators (such as the code generator, the proof generator, the atlas generator, etc.)
are merely different ways to show Fspc.
-}
module Data.Fspec ( Fspc(..)
                  , Fservice(..), Field(..), Clauses(..)
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
           
--DESCR -> Fservice contains everything needed to render the specification, the code, and the documentation including proofs of a single service.
--         All "intelligence" is put in assembling an Fservice.
--         The coding process that uses an Fservice takes care of language specific issues, and renders it to the final product.
data Fservice = Fservice 
                  { fsv_objectdef :: ObjectDef              -- The service declaration that was specified by the programmer,
                                                            -- and which has been type checked by the compiler.
                  , fsv_rels      :: [Morphism]             -- The declarations that may be changed by the user of this service
                  , fsv_rules     :: [Rule]                 -- The rules that may be affected by this service
                  , fsv_ecaRules  :: [Declaration->ECArule] -- The ECA-rules that may be used by this service to restore invariants.
                  , fsv_signals   :: [Rule]                 -- All signals that are visible in this service
                  , fsv_fields    :: [Field]                -- All fields/parameters of this service
                  }                                         

data Field    = Att
                  { fld_name      :: String                 -- The name of this field
                  , fld_expr      :: Expression             -- The expression by which this field is attached to the service
                  , fld_mph       :: Morphism               -- The morphism to which the database table is attached.
                  , fld_editable  :: Bool                   -- can this field be changed by the user of this service?
                  , fld_list      :: Bool                   -- can there be multiple values in this field?
                  , fld_must      :: Bool                   -- is this field obligatory?
                  , fld_new       :: Bool                   -- can new elements be filled in? (if no, only existing elements can be selected)
                  , fld_fields    :: [Field]                -- All fields/parameters of this service
                  , fld_insAble   :: Bool                   -- can the user insert in this field?
                  , fld_onIns     :: Declaration->ECArule   -- the PAclause to be executed after an insert on this field
                  , fld_delAble   :: Bool                   -- can the user delete this field?
                  , fld_onDel     :: Declaration->ECArule   -- the PAclause to be executed after a delete on this field
                  } 

-- The data structure Clauses is meant for calculation purposes.
-- It must always satisfy for every i<length (cl_rule cl): cl_rule cl is equivalent to Fi [Fu disj| conj<-cl_conjNF cl, disj<-[conj!!i]]
-- Every rule is transformed to this form, as a step to derive eca-rules
data Clauses  = Clauses
                    {cl_conjNF :: [[Expression]]  -- The conjunctive normal form of the clause
                    ,cl_rule   :: Rule            -- The rule that is restored by this clause (for traceability purposes)
                    }

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

