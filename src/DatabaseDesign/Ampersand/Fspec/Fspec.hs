{-# OPTIONS_GHC -Wall -XMultiParamTypeClasses #-}
{- | The intentions behind Fspc (SJ 30 dec 2008):
Generation of functional specifications is the core functionality of Ampersand.
All items in a specification are generated into the following data structure, Fspc.
It is built by compiling an Ampersand script and translating that to Fspc.
In the future, other ways of 'filling' Fspc are foreseen.
All generators (such as the code generator, the proof generator, the atlas generator, etc.)
are merely different ways to show Fspc.
-}
module DatabaseDesign.Ampersand.Fspec.Fspec 
          ( Fspc(..)
                  , Fservice(..), Field(..), Clauses(..), Quad(..)
                  , FSid(..)
                  , FTheme(..)
                  , toService
                  )
 where
   import DatabaseDesign.Ampersand.ADL1            hiding (Association)
   import DatabaseDesign.Ampersand.Basics                    ((>-),Identified(..))
   import DatabaseDesign.Ampersand.Basics                      (Inheritance(..))
   import DatabaseDesign.Ampersand.Fspec.Plug                     
   import DatabaseDesign.Ampersand.Fspec.Graphic.Picture  (Pictures)
   import DatabaseDesign.Ampersand.Fspec.FPA              (FPA)

   data Fspc = Fspc { fsName       :: String                 -- ^ The name of the specification, taken from the Ampersand script
                    , vplugInfos   :: PlugInfos              -- ^ All plugs defined in the Ampersand script
                    , plugInfos    :: PlugInfos              -- ^ All plugs (defined and derived)
                    , serviceS     :: [Service]              -- ^ All services defined in the Ampersand script
                    , serviceG     :: [Service]              -- ^ All services derived from the basic ontology
                    , services     :: Fservices              -- ^ generated: One Fservice for every ObjectDef in serviceG and serviceS 
                    , roleServices :: [(String,Fservice)]    -- ^ the relation saying which roles may use which service
                    , mayEdit      :: [(String,Relation Concept)] -- ^ the relation saying which roles may change the population of which relation.
                    , vrules       :: Rules (Relation Concept) -- ^ All rules that apply in the entire Fspc, including all signals
                    , grules       :: Rules (Relation Concept) -- ^ All rules that are generated: multiplicity rules and key rules
                    , vkeys        :: KeyDefs                -- ^ All keys that apply in the entire Fspc
                    , vgens        :: Gens Concept           -- ^ All gens that apply in the entire Fspc
                    , vconjs       :: Expressions (Relation Concept) -- ^ All conjuncts generated (by ADL2Fspec) from non-signal rules
                    , vquads       :: Quads                  -- ^ All quads generated (by ADL2Fspec) from non-signal rules
                    , vrels        :: [Declaration Concept]   -- ^ All user defined and generated declarations.
                                                             -- ^ The generated declarations are all generalizations and
                                                             -- ^ one declaration for each signal.
                    , fsisa        :: Inheritance Concept    -- ^ generated: The data structure containing the generalization structure of concepts
                    , vpatterns    :: Patterns               -- ^ All patterns taken from the Ampersand script
                    , pictPatts    :: Pictures               -- ^ List of pictures containing pattern pictures (in same order as patterns)
                    , vConceptDefs :: ConceptDefs            -- ^ All conceptDefs defined in the Ampersand script
                    , fSexpls      :: Explanations           -- ^ All explanations that have been declared within the current specification.
                    , vctxenv :: ( Expression (Relation Concept)
                                 , [(Declaration Concept,String)]) --an expression on the context with unbound relations, to be bound in this environment
                    }

   instance ConceptStructure Fspc Concept where
    concs     fSpec = concs (vrels fSpec)                          -- The set of all concepts used in this Fspc
    morlist   fSpec = morlist (serviceS fSpec) ++ morlist (vrules fSpec)
    genE      fSpec = genE (vrels fSpec)  
--  closExprs fSpec = closExprs (rules fSpec++signals fSpec)
   
   instance ViewPoint Fspc where
    objectdef    fSpec = Obj { objnm   = name fSpec
                             , objpos  = Nowhere
                             , objctx  = Tm (mIs S) (-1)
                             , objats  = map svObj (serviceS fSpec ++ serviceG fSpec)
                             , objstrs = []
                             , objctx_proof = Nothing
                             }
    conceptDefs  fSpec = vConceptDefs fSpec
    --REMARK: in the fspec we do not distinguish between the disjoint relation declarations and rule declarations (yet?). 
    declarations fSpec = vrels fSpec
    rules        fSpec = [r| r<-vrules fSpec, not (isSignal r)]
    signals      fSpec = [r| r<-vrules fSpec,      isSignal r ]
    objDefs      fSpec = map svObj (serviceS fSpec ++ serviceG fSpec)
    keyDefs      fSpec = vkeys fSpec
    gens         fSpec = vgens fSpec
    patterns     fSpec = vpatterns fSpec
    isa          fSpec = fsisa  fSpec
   
 
   --DESCR -> Fservice contains everything needed to render the specification, the code, and the documentation including proofs of a single service.
   --         All "intelligence" is put in assembling an Fservice.
   --         The coding process that uses an Fservice takes care of language specific issues, and renders it to the final product.
   type Fservices = [Fservice]
   data Fservice = Fservice 
                     { fsv_svcdef    :: Service                -- The service declaration that was specified by the programmer,
                                                               -- and which has been type checked by the compiler.
                     , fsv_insrels   :: [Relation Concept]     -- The relations into which a user of this service may insert elements
                     , fsv_delrels   :: [Relation Concept]     -- The relations from which a user of this service may remove elements
                     , fsv_rules     :: Rules (Relation Concept) -- The rules that may be affected by this service (provided by the parser)
                     , fsv_quads     :: Quads                  -- The Quads that are used to make a switchboard. (generated by ADL2Fspec)
                     , fsv_ecaRules  :: [Declaration Concept->ECArule Concept] -- The ECA-rules that may be used by this service to restore invariants. (generated by ADL2Fspec)
                     , fsv_signals   :: Rules (Relation Concept) -- All signals that are visible in this service
                     , fsv_fields    :: Fields                 -- All fields/parameters of this service
                     , fsv_creating  :: [Concept]              -- All concepts of which this service can create new instances
                     , fsv_deleting  :: [Concept]              -- All concepts of which this service can delete instances
                     , fsv_fpa       :: FPA                    -- function point assessment of this service
                     , fsv_expls     :: Explanations           -- The explanations of everything that is used in this service.
                     }
   toService :: Fservice -> Service
   toService  = fsv_svcdef 
   

   instance Eq Fservice where -- service names must be unique throughout the entire scope. The compiler must check this.
    f == f'  =  name f == name f'

   instance Show Fservice where
    showsPrec _ svc@(Fservice{})
     = showString (show (fsv_svcdef    svc)++"\n"++
                   show (fsv_insrels   svc)++"\n"++
                   show (fsv_delrels   svc)++"\n"++
                   show (fsv_rules     svc)++"\n"++
--                 show [e delt| e<-fsv_ecaRules svc]++"\n"++  -- levert een lastige infinite loop op
                   show (fsv_signals   svc)++"\n"++
--                 show (fsv_fields    svc)++"\n"++
                   show (fsv_creating  svc)++"\n"++
                   show (fsv_deleting  svc)++"\n"
                  ) -- where delt::Declaration Concept; delt = error "!Fatal (module Fspec 107): Undef declaration"


   instance ConceptStructure Fservice Concept where
    concs     svc = concs (rules svc++signals svc)         -- The set of all concepts used in this Fservice
    mors      svc = mors (rules svc++signals svc)          -- The set of all relations in this Fservice
    morlist   svc = morlist (rules svc++signals svc)       -- The list of all relations in this Fservice
    genE      svc = genE (rules svc++signals svc)          -- The genE relation
--  closExprs svc = closExprs (rules svc++signals svc)     -- The closure expressions of this Fservice

   instance ViewPoint Fservice where
    objectdef    svc = svObj (fsv_svcdef svc)
    conceptDefs   _  = []                                  -- The set of all concept definitions in this Fservice
    declarations  _  = []                                  -- Currently, no declarations are made within a service.
    --REMARK: in the fspec we do not distinguish between the disjoint relation declarations and rule declarations (yet?).
    rules        svc = [r| r<-fsv_rules svc]
    signals      svc = [r| r<-fsv_signals svc]
    objDefs      svc = [svObj (fsv_svcdef svc)]
    keyDefs       _  = []
    gens          _  = []
    patterns      _  = []
    isa          svc = Isa ts (concs svc>-[c| (g,s)<-ts,c<-[g,s]])
                       where ts = [(g,s)| g<-concs svc, s<-concs svc, g<s, null [c|c<-concs svc, g<c, c<s]]
--   instance Explainable Fservice where
---- Once Ampersand allows explanations to be given from with a service declaration, these must be made visible by <explanations>
---- Until that time, the list of explanations is (predictably) empty.
--     explanations fServ = fsv_expls fServ

   type Fields = [Field]
   data Field  = Att { fld_name      :: String                 -- The name of this field
                     , fld_sub       :: Fields                 -- all sub-fields
                     , fld_expr      :: Expression (Relation Concept)  -- The expression by which this field is attached to the service
                     , fld_rel       :: Relation Concept               -- The relation to which the database table is attached.
                     , fld_editable  :: Bool                   -- can this field be changed by the user of this service?
                     , fld_list      :: Bool                   -- can there be multiple values in this field?
                     , fld_must      :: Bool                   -- is this field obligatory?
                     , fld_new       :: Bool                   -- can new elements be filled in? (if no, only existing elements can be selected)
                     , fld_sLevel    :: Int                    -- The (recursive) depth of the current servlet wrt the entire service. This is used for documentation.
                     , fld_insAble   :: Bool                   -- can the user insert in this field?
                     , fld_onIns     :: Declaration Concept->ECArule Concept   -- the PAclause to be executed after an insert on this field
                     , fld_delAble   :: Bool                   -- can the user delete this field?
                     , fld_onDel     :: Declaration Concept->ECArule Concept   -- the PAclause to be executed after a delete on this field
                     } 
   
   -- The data structure Clauses is meant for calculation purposes.
   -- It must always satisfy for every i<length (cl_rule cl): cl_rule cl is equivalent to Fi [Fu disj| (conj, hcs)<-cl_conjNF cl, disj<-[conj!!i]]
   -- Every rule is transformed to this form, as a step to derive eca-rules
   data Clauses  = Clauses
                     { cl_conjNF :: [(Expression (Relation Concept)
                                     ,Expressions (Relation Concept))]   -- The list of pairs (conj, hcs) in which conj is a conjunct of the rule
                                                                 -- and hcs contains all derived expressions to be used for eca-rule construction.
                                                                 -- hcs contains only disjunctive normal forms.
                     , cl_rule   :: Rule (Relation Concept) -- The rule that is restored by this clause (for traceability purposes)
                     }
   -- A Quad is used in the "switchboard" of rules. It represents a "proto-rule" with the following meaning:
   -- whenever qMorph is affected (i.e. tuples in qMorph are inserted or deleted), qRule may have to be restored using functionality from qClauses.
   -- The rule is taken along for traceability.
   type Quads = [Quad]
   data Quad     = Quad
                     { qMorph        :: Relation Concept        -- The relation that, when affected, triggers a restore action.
                     , qClauses      :: Clauses         -- The clauses
                     }

   data FTheme = FTheme { tconcept   :: Concept
                        , trules     :: Rules (Relation Concept)
                        }
   {- from http://www.w3.org/TR/wsdl20/#InterfaceOperation
    - "The properties of the Interface Operation component are as follows:
    - ...
    - * {interface message references} OPTIONAL. A set of Interface Message Reference components for the ordinary messages the operation accepts or sends.
    - ..."
    -}
   
   data FSid = FS_id String     -- Identifiers in the Functional Specification Language contain strings that do not contain any spaces.
           --  | NoName           -- some identified objects have no name...

   instance Identified Fspc where
     name fspc = fsName fspc

   instance Identified Fservice where
     name fservice = name (fsv_svcdef fservice)

   instance Identified FSid where
    name (FS_id nm) = nm
