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
                  , Fservice(..), Field(..), Clauses(..), Quad(..)
                  , FSid(..)
                  , FTheme(..)
                  , WSOperation(..), WSAction(..)
                  , ClassDiag(..), Class(..), Attribute(..), Association(..), Aggregation(..), Generalization(..), Deleting(..), Method(..)
                  , datasets
                  )
 where
   import Adl            hiding (Association)
   import Collection                    (uni,(>-))
   import Strings                       (chain)
   import Typology                      (Inheritance(..))
   import Data.Plug                     
   import Picture                       (Pictures)
   import FPA
   data Fspc = Fspc { fsName       :: String               -- ^ The name of the specification, taken from the ADL-script
                    , vplugs       :: Plugs                 -- ^ all plugs defined in the ADL-script
                    , plugs        :: Plugs                 -- ^ all plugs (defined and derived)
                    , serviceS     :: ObjectDefs            -- ^ all services defined in the ADL-script
                    , serviceG     :: ObjectDefs            -- ^ all services derived from the basic ontology
                    , services     :: Fservices             -- ^ generated: One Fservice for every ObjectDef in serviceG and serviceS 
                    , vrules       :: Rules                 -- ^ All rules that apply in the entire Fspc, including all signals
                    , vkeys        :: KeyDefs               -- ^ All keys that apply in the entire Fspc
                    , vgens        :: Gens                  -- ^ All keys that apply in the entire Fspc
                    , vconjs       :: Expressions           -- ^ All conjuncts generated (by ADL2Fspec) from non-signal rules
                    , vquads       :: Quads                 -- ^ All quads generated (by ADL2Fspec) from non-signal rules
                    , vrels        :: Declarations          -- ^ All declarations declared in this specification
                    , fsisa        :: Inheritance Concept   -- ^ generated: The data structure containing the generalization structure of concepts
                    , vpatterns    :: Patterns              -- ^ all patterns taken from the ADL-script
                    , pictPatts    :: Pictures              -- ^ List of pictures containing pattern pictures (in same order as patterns)
                    , vConceptDefs :: ConceptDefs          -- ^ all conceptDefs defined in the ADL-script
                    , themes       :: [FTheme]             -- ^ generated: one FTheme for every pattern
                    , fSexpls      :: [Explanation]        -- ^ all explanations that are valid within the current specification
                    , vctxenv :: (Expression,[(Declaration,String)]) --an expression on the context with unbound morphisms, to be bound in this environment
                    }

   instance Morphical Fspc where
    concs     fSpec = concs (vrels fSpec)                          -- The set of all concepts used in this Fspc
    mors      fSpec = mors (vplugs fSpec) `uni` mors (serviceS fSpec) `uni` mors (vrules fSpec)
    morlist   fSpec = morlist (vplugs fSpec) ++ morlist (serviceS fSpec) ++ morlist (vrules fSpec)
    genE      fSpec = genE (vrels fSpec)  
    closExprs fSpec = closExprs (rules fSpec++signals fSpec)

   instance ViewPoint Fspc where
    objectdef    fSpec = Obj { objnm   = name fSpec
                             , objpos  = Nowhere
                             , objctx  = Tm (mIs S) (-1)
                             , objats  = serviceS fSpec ++ serviceG fSpec
                             , objstrs = []
                             , objctx_proof = Nothing
                             }
    conceptDefs  fSpec = vConceptDefs fSpec
    --REMARK: in the fspec we do not distinguish between the disjoint relation declarations and rule declarations (yet?). 
    declarations fSpec = vrels fSpec
    rel_declarations _ = error "error (module Fspec 63): in the fspec we do not distinguish between the disjoint relation declarations and rule declarations (yet?)"
    rule_declarations _ = error "error (module Fspec 64): in the fspec we do not distinguish between the disjoint relation declarations and rule declarations (yet?)"
    rules        fSpec = [r| r<-vrules fSpec, not (isSignal r)]
    signals      fSpec = [r| r<-vrules fSpec,      isSignal r ]
    objDefs      fSpec = serviceS fSpec ++ serviceG fSpec
    keyDefs      fSpec = vkeys fSpec
    gens         fSpec = vgens fSpec
    patterns     fSpec = vpatterns fSpec
    isa          fSpec = fsisa  fSpec

--   instance UserExplainable Fspc where
---- Once ADL allows explanations to be given from with a service declaration, these must be made visible by <explanations>
---- Until that time, the list of explanations is (predictably) empty.
--     explanations fSpec = fSexpls fSpec

   --DESCR -> Fservice contains everything needed to render the specification, the code, and the documentation including proofs of a single service.
   --         All "intelligence" is put in assembling an Fservice.
   --         The coding process that uses an Fservice takes care of language specific issues, and renders it to the final product.
   type Fservices = [Fservice]
   data Fservice = Fservice 
                     { fsv_objectdef :: ObjectDef              -- The service declaration that was specified by the programmer,
                                                               -- and which has been type checked by the compiler.
                     , fsv_insrels   :: Morphisms              -- The relations into which a user of this service may insert elements
                     , fsv_delrels   :: Morphisms              -- The relations from which a user of this service may remove elements
                     , fsv_rules     :: Rules                  -- The rules that may be affected by this service (provided by the parser)
                     , fsv_quads     :: Quads                  -- The Quads that are used to make a switchboard. (generated by ADL2Fspec)
                     , fsv_ecaRules  :: [Declaration->ECArule] -- The ECA-rules that may be used by this service to restore invariants. (generated by ADL2Fspec)
                     , fsv_signals   :: Rules                  -- All signals that are visible in this service
                     , fsv_fields    :: Fields                 -- All fields/parameters of this service
                     , fsv_creating  :: Concepts               -- All concepts of which this service can create new instances
                     , fsv_deleting  :: Concepts               -- All concepts of which this service can delete instances
                     , fsv_fpa       :: FPA                    -- function point assessment of this service
                     , fsv_expls     :: [Explanation]          -- The explanations of everything that is used in this service.
                     }

   instance Show Fservice where
    showsPrec _ svc@(Fservice{})
     = showString ("\n!Diagnosis error (module Fspec 102): empty show(Fservice)"
                   ++show (fsv_objectdef svc)++"\n"
                   ++show (fsv_insrels   svc)++"\n"
                   ++show (fsv_delrels   svc)++"\n"
                   ++show (fsv_rules     svc)++"\n"
--                   ++show [e delt| e<-fsv_ecaRules svc]++"\n"  -- levert een lastige infinite loop op
                   ++show (fsv_signals   svc)++"\n"
--                   ++show (fsv_fields    svc)++"\n"
                   ++show (fsv_creating  svc)++"\n"
                   ++show (fsv_deleting  svc)++"\n"
                  ) -- where delt::Declaration; delt = error "!Fatal (module Fspec 111): Undef declaration"


   instance Morphical Fservice where
    concs     svc = concs (rules svc++signals svc)         -- The set of all concepts used in this Fservice
    mors      svc = mors (rules svc++signals svc)          -- The set of all morphisms in this Fservice
    morlist   svc = morlist (rules svc++signals svc)       -- The list of all morphisms in this Fservice
    decls     svc = decls (rules svc++signals svc)         -- The set of all relations used in this Fservice
    genE      svc = genE (rules svc++signals svc)          -- The genE relation
    closExprs svc = closExprs (rules svc++signals svc)     -- The closure expressions of this Fservice

   instance ViewPoint Fservice where
    objectdef    svc = fsv_objectdef svc
    conceptDefs   _  = []                                  -- The set of all concept definitions in this Fservice
    declarations  _  = []                                  -- Currently, no declarations are made within a service.
    --REMARK: in the fspec we do not distinguish between the disjoint relation declarations and rule declarations (yet?).
    rel_declarations _ = error "error (module Fspec 118): in the fspec we do not distinguish between the disjoint relation declarations and rule declarations (yet?)"
    rule_declarations _ = error "error (module Fspec 119): in the fspec we do not distinguish between the disjoint relation declarations and rule declarations (yet?)"
    rules        svc = [r| r<-fsv_rules svc]
    signals      svc = [r| r<-fsv_signals svc]
    objDefs      svc = [fsv_objectdef svc]
    keyDefs       _  = []
    gens          _  = []
    patterns      _  = []
    isa          svc = Isa ts (concs svc>-[c| (g,s)<-ts,c<-[g,s]])
                       where ts = [(g,s)| g<-concs svc, s<-concs svc, g<s, null [c|c<-concs svc, g<c, c<s]]
--   instance Explainable Fservice where
---- Once ADL allows explanations to be given from with a service declaration, these must be made visible by <explanations>
---- Until that time, the list of explanations is (predictably) empty.
--     explanations fServ = fsv_expls fServ

   type Fields = [Field]
   data Field  = Att { fld_name      :: String                 -- The name of this field
                     , fld_sub       :: Fields                 -- all sub-fields
                     , fld_expr      :: Expression             -- The expression by which this field is attached to the service
                     , fld_mph       :: Morphism               -- The morphism to which the database table is attached.
                     , fld_editable  :: Bool                   -- can this field be changed by the user of this service?
                     , fld_list      :: Bool                   -- can there be multiple values in this field?
                     , fld_must      :: Bool                   -- is this field obligatory?
                     , fld_new       :: Bool                   -- can new elements be filled in? (if no, only existing elements can be selected)
                     , fld_sLevel    :: Int                    -- The (recursive) depth of the current servlet wrt the entire service. This is used for documentation.
                     , fld_insAble   :: Bool                   -- can the user insert in this field?
                     , fld_onIns     :: Declaration->ECArule   -- the PAclause to be executed after an insert on this field
                     , fld_delAble   :: Bool                   -- can the user delete this field?
                     , fld_onDel     :: Declaration->ECArule   -- the PAclause to be executed after a delete on this field
                     } 
   
   -- The data structure Clauses is meant for calculation purposes.
   -- It must always satisfy for every i<length (cl_rule cl): cl_rule cl is equivalent to Fi [Fu disj| (conj, hcs)<-cl_conjNF cl, disj<-[conj!!i]]
   -- Every rule is transformed to this form, as a step to derive eca-rules
   data Clauses  = Clauses
                     { cl_conjNF     :: [(Expression,Expressions)]   -- The list of pairs (conj, hcs) in which conj is a conjunct of the rule
                                                                     -- and hcs contains all derived expressions to be used for eca-rule construction.
                                                                     -- hcs contains only disjunctive normal forms.
                     , cl_rule       :: Rule            -- The rule that is restored by this clause (for traceability purposes)
                     }
   -- A Quad is used in the "switchboard" of rules. It represents a "proto-rule" with the following meaning:
   -- whenever qMorph is affected (i.e. tuples in qMorph are inserted or deleted), qRule may have to be restored using functionality from qClauses.
   -- The rule is taken along for traceability.
   type Quads = [Quad]
   data Quad     = Quad
                     { qMorph        :: Morphism        -- The morphism that, when affected, triggers a restore action.
                     , qClauses      :: Clauses         -- The clauses
                     }

   data FTheme = FTheme { tconcept   :: Concept
                        , tfunctions :: WSOperations
                        , trules     :: Rules 
                        }
   {- from http://www.w3.org/TR/wsdl20/#InterfaceOperation
    - "The properties of the Interface Operation component are as follows:
    - ...
    - * {interface message references} OPTIONAL. A set of Interface Message Reference components for the ordinary messages the operation accepts or sends.
    - ..."
    -}
   type WSOperations = [WSOperation] 
   data WSOperation = WSOper { wsaction ::WSAction
                             , wsmsgin  ::ObjectDefs
                             , wsmsgout ::ObjectDefs
                             }
   data WSAction = WSCreate | WSRead | WSUpdate |WSDelete deriving Show
   
   data FSid = FS_id String     -- Identifiers in the Functional Specification Language contain strings that do not contain any spaces.
           --  | NoName           -- some identified objects have no name...

   instance Identified Fspc where
     name fspc = fsName fspc

   instance Identified Fservice where
     name fservice = name (fsv_objectdef fservice)

   instance Identified FSid where
    name (FS_id nm) = nm

   datasets :: Fspc -> [Plug]
   datasets fSpec = [p| p<-plugs fSpec
                      , fld<-take 1 (fields p), flduniq fld  -- this excludes associations, because they are not flduniq
                      , length (fields p)>1]                 -- this excludes scalar plugs

-------------- Class Diagrams ------------------
   data ClassDiag = OOclassdiagram {classes     :: [Class]            --
                                   ,assocs      :: [Association]      --
                                   ,aggrs       :: [Aggregation]      --
                                   ,geners      :: [Generalization]   --
                                   ,nameandcpts :: (String,Concepts)}
                            deriving Show
   instance Identified ClassDiag where
      name cd = n
        where (n,_) = nameandcpts cd
        
   data Class          = OOClass        String             --
                                        [Attribute]        --
                                        [Method]           --
                                    deriving Show
   data Attribute      = OOAttr         String             -- name of the attribute
                                        String             -- type of the attribute (Concept name or built-in type)
                                        Bool               -- fNull:  says whether the attribute may be left open
                                    deriving Show
   data Association    = OOAssoc        String             -- source: the left hand side class
                                        String             -- left hand side multiplicities
                                        String             -- left hand side role
                                        String             -- target: the right hand side class
                                        String             -- right hand side multiplicities
                                        String             -- right hand side role
                                    deriving Show
   data Aggregation    = OOAggr         Deleting           --
                                        String             --
                                        String             --
                                    deriving (Show, Eq)
   data Generalization = OOGener        String             --
                                        [String]           --
                                    deriving (Show, Eq)
   data Deleting       = Open | Close                      --
                                    deriving (Show, Eq)
   data Method         = OOMethodC      String             -- name of this method, which creates a new object (producing a handle)
                                        [Attribute]        -- list of parameters: attribute names and types
                       | OOMethodR      String             -- name of this method, which yields the attribute values of an object (using a handle).
                                        [Attribute]        -- list of parameters: attribute names and types
                       | OOMethodS      String             -- name of this method, which selects an object using key attributes (producing a handle).
                                        [Attribute]        -- list of parameters: attribute names and types
                       | OOMethodU      String             -- name of this method, which updates an object (using a handle).
                                        [Attribute]        -- list of parameters: attribute names and types
                       | OOMethodD      String             -- name of this method, which deletes an object (using nothing but a handle).
                       | OOMethod       String             -- name of this method, which deletes an object (using nothing but a handle).
                                        [Attribute]        -- list of parameters: attribute names and types
                                        String             -- result: a type

   instance Show Method where
    showsPrec _ (OOMethodC nm cs)  = showString (nm++"("++chain "," [ n | OOAttr n _ _<-cs]++"):handle")
    showsPrec _ (OOMethodR nm as)  = showString (nm++"(handle):["++chain "," [ n | OOAttr n _ _<-as]++"]")
    showsPrec _ (OOMethodS nm ks)  = showString (nm++"("++chain "," [ n | OOAttr n _ _<-ks]++"):handle")
    showsPrec _ (OOMethodD nm)     = showString (nm++"(handle)")
    showsPrec _ (OOMethodU nm cs)  = showString (nm++"(handle,"++chain "," [ n | OOAttr n _ _<-cs]++")")
    showsPrec _ (OOMethod nm cs r) = showString (nm++"("++chain "," [ n | OOAttr n _ _<-cs]++"): "++r)
