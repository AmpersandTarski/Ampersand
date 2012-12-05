{-# OPTIONS_GHC -Wall #-}
{- | The intentions behind Fspc (SJ 30 dec 2008):
Generation of functional specifications is the core functionality of Ampersand.
All items in a specification are generated into the following data structure, Fspc.
It is built by compiling an Ampersand script and translating that to Fspc.
In the future, other ways of 'filling' Fspc are foreseen.
All generators (such as the code generator, the proof generator, the atlas generator, etc.)
are merely different ways to show Fspc.
-}
module DatabaseDesign.Ampersand.Fspec.Fspec 
          ( Fspc(..), Fswitchboard(..), Field(..), Clauses(..), Quad(..)
          , FSid(..), FProcess(..)
          , FTheme(..)
          , InsDel(..)
          , ECArule(..)
          , Event(..)
          , PAclause(..)
          , Activity(..)
          , PlugSQL(..)
          , lookupCpt
          , SqlField(..)
          , FPA(..)
          , FPcompl(..)
          , PlugInfo(..)
          , SqlType(..)
          , getGeneralizations, getSpecializations
          )
where
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Basics
import Data.List(nub)
import DatabaseDesign.Ampersand.ADL1.Pair
import qualified DatabaseDesign.Ampersand.Core.Poset as Poset ((<),(>)) -- unfortunately this also imports some nasty classes which make type errors incomprehensible (as they default to the Poset classes, not the standard ones)

fatal :: Int -> String -> a
fatal = fatalMsg "Fspec.Fspec"

data Fspc = Fspc { fsName ::       String                   -- ^ The name of the specification, taken from the Ampersand script
                 , fspos ::        [Origin]                 -- ^ The origin of the Fspc. An Fspc can be a merge of a file including other files c.q. a list of Origin.
                 , themes ::       [String]                 -- ^ The names of patterns/processes to be printed in the functional specification. (for making partial documentation)
                 , fsLang ::       Lang                     -- ^ The default language for this specification, if specified at all.
                 , vprocesses ::   [FProcess]               -- ^ All processes defined in the Ampersand script
                 , vplugInfos ::   [PlugInfo]               -- ^ All plugs defined in the Ampersand script
                 , plugInfos ::    [PlugInfo]               -- ^ All plugs (defined and derived)
                 , interfaceS ::   [Interface]              -- ^ All interfaces defined in the Ampersand script
                 , interfaceG ::   [Interface]              -- ^ All interfaces derived from the basic ontology
                 , fSwitchboard :: Fswitchboard             -- ^ The code to be executed to maintain the truth of invariants
                 , fActivities ::  [Activity]               -- ^ generated: One Activity for every ObjectDef in interfaceG and interfaceS 
                 , fRoleRels ::    [(String,Relation)]      -- ^ the relation saying which roles may change the population of which relation.
                 , fRoleRuls ::    [(String,Rule)]          -- ^ the relation saying which roles may change the population of which relation.
                 , vrules ::       [Rule]                   -- ^ All user defined rules that apply in the entire Fspc
                 , grules ::       [Rule]                   -- ^ All rules that are generated: multiplicity rules and key rules
                 , allRules::      [Rule]                   -- ^ All rules, both generated (from multiplicity and keys) as well as user defined ones.
                 , vkeys ::        [KeyDef]                 -- ^ All keys that apply in the entire Fspc
                 , vgens ::        [A_Gen]                  -- ^ All gens that apply in the entire Fspc
                 , vconjs ::       [Expression]             -- ^ All conjuncts generated (by ADL2Fspec)
                 , vquads ::       [Quad]                   -- ^ All quads generated (by ADL2Fspec)
                 , vEcas ::        [ECArule]                -- ^ All ECA rules generated (by ADL2Fspec)
                 , vrels ::        [Declaration]            -- ^ All user defined and generated declarations plus all defined and computed totals.
                                                            --   The generated declarations are all generalizations and
                                                            --   one declaration for each signal.
                 , fsisa ::        [(A_Concept, A_Concept)] -- ^ generated: The data structure containing the generalization structure of concepts
                 , vpatterns ::    [Pattern]                -- ^ All patterns taken from the Ampersand script
                 , vConceptDefs :: [ConceptDef]             -- ^ All conceptDefs defined in the Ampersand script including those of concepts not in concs fSpec
                 , fSexpls ::      [Purpose]                -- ^ All purposes that have been declared at the top level of the current specification, but not in the processes, patterns and interfaces.
                 , metas ::        [Meta]                   -- ^ All meta declarations from the entire context      
                 , vctxenv ::      ( Expression
                                   , [(Declaration,String)])-- an expression on the context with unbound relations, to be bound in this environment
               --  , fPopulations :: [UserDefPop]             -- ^ a lookup table for the contents of all relations and concepts
                 , hasPopulations :: Bool
                 , userDefPops    :: [UserDefPop]           -- all user defined populations of relations and concepts
                 , allViolations  :: [(Rule,[Paire])]       -- all rules with violations.
                 }
instance ConceptStructure Fspc where
  concs     fSpec = concs (vrels fSpec)                     -- The set of all concepts used in this Fspc
  morlist   fSpec = morlist (interfaceS fSpec) ++ morlist (vrules fSpec)
--  closExprs fSpec = closExprs (rules fSpec)

instance Language Fspc where
  objectdef    fSpec = Obj { objnm   = name fSpec
                           , objpos  = Origin "generated object by objectdef (Language Fspc)"
                           , objctx  = ERel (I ONE) 
                           , objmsub = Just . Box $ map ifcObj (interfaceS fSpec ++ interfaceG fSpec)
                           , objstrs = []
                           }
  conceptDefs fSpec = nub (concatMap cptdf (concs fSpec)) --use vConceptDefs to get CDs of concepts not in concs too
   --REMARK: in the fspec we do not distinguish between the disjoint relation declarations and rule declarations (yet?). 
  declarations = vrels
  udefrules    = vrules -- only user defined rules
  invariants fSpec = [r | r<-vrules fSpec, not (isSignal r)]  -- HJO, 20121202: @Stef: Is dit goed? waarom worden de multipliciteiten en keys niet als invariant gerekend?
  keyDefs      = vkeys
  gens         = vgens
  patterns     = vpatterns
--  violations udp fSpec = [(r,viol) |r<- invariants fSpec ++ grules fSpec, viol<-ruleviolations udp r]
                     -- default violations computes al rules again from declarations

data FProcess
  = FProc { fpProc :: Process
          , fpActivities :: [Activity]
          }  
instance Identified FProcess where
  name = name . fpProc 

instance Language FProcess where
  objectdef    = objectdef.fpProc
  conceptDefs  = conceptDefs.fpProc
  declarations = declarations.fpProc
  udefrules    = udefrules.fpProc
  invariants   = invariants.fpProc
  keyDefs      = keyDefs.fpProc
  gens         = gens.fpProc
  patterns     = patterns.fpProc
--  violations pt= (violations pt).fpProc

-- | A list of ECA rules, which is used for automated functionality.
data Fswitchboard
  = Fswtch { fsbEvIn :: [Event]
           , fsbEvOut :: [Event]
           , fsbConjs :: [(Rule, Expression)]
           , fsbECAs :: [ECArule]
           }

--DESCR -> Finterface contains everything needed to render the specification, the code, and the documentation including proofs of a single interface.
--         All "intelligence" is put in assembling an Finterface.
--         The coding process that uses an Finterface takes care of language specific issues, and renders it to the final product.
--TODO: Task of ticket #107
data Finterface = Fifc
                  { fsv_ifcdef :: Interface                -- The interface declaration that was specified by the programmer,
                                                            -- and which has been type checked by the compiler.
                  , fsv_insrels :: [Relation]     -- The relations into which a user of this interface may insert elements
                  , fsv_delrels :: [Relation]     -- The relations from which a user of this interface may remove elements
                  , fsv_rules :: [Rule] -- The rules that may be affected by this interface (provided by the parser)
--                  , fsv_quads :: [Quad]                 -- The Quads that are used to make a switchboard. (generated by ADL2Fspec)
--                  , fsv_ecaRules :: [ECArule]      -- The ECA-rules that may be used by this interface to restore invariants. (generated by ADL2Fspec)
                  , fsv_procRules :: [Rule] -- All process rules that are visible in this interface
--                  , fsv_fields :: Fields                 -- All fields/parameters of this interface
                  , fsv_creating :: [A_Concept]              -- All concepts of which this interface can create new instances
                  , fsv_deleting :: [A_Concept]              -- All concepts of which this interface can delete instances
--                  , fsv_fpa :: FPA                    -- function point assessment of this interface
                  }
   

instance Eq Finterface where -- interface names must be unique throughout the entire scope. The compiler must check this.
  f == f'  =  name f == name f'

instance Show Finterface where
  showsPrec _ ifc@(Fifc{})
    = showString (show (fsv_ifcdef    ifc)++"\n"++
                  show (fsv_insrels   ifc)++"\n"++
                  show (fsv_delrels   ifc)++"\n"++
                  show (fsv_rules     ifc)++"\n"++
--                  show [e delt | e<-fsv_ecaRules ifc]++"\n"++  -- levert een lastige infinite loop op
                  show (fsv_procRules ifc)++"\n"++
--                  show (fsv_fields    ifc)++"\n"++
                  show (fsv_creating  ifc)++"\n"++
                  show (fsv_deleting  ifc)++"\n"
                 ) -- where delt::Declaration; delt = fatal 129 "Undef declaration"


instance ConceptStructure Finterface where
  concs     ifc = concs (fsv_ifcdef ifc)         -- The set of all concepts used in this Fifc
  mors      ifc = mors (fsv_ifcdef ifc)          -- The set of all relations used in this Fifc
  morlist   ifc = morlist (fsv_ifcdef ifc)       -- The list of all relations in this Fifc
--  closExprs ifc = closExprs (fsv_ifcdef ifc)     -- The closure expressions of this Fifc


--   instance Explainable Finterface where
---- Once Ampersand allows explanations to be given from with a interface declaration, these must be made visible by <explanations>
---- Until that time, the list of explanations is (predictably) empty.
--     explanations fServ = fsv_expls fServ
    
type Fields = [Field]
data Field  = Att { fld_name :: String        -- The name of this field
                  , fld_sub :: Fields        -- all sub-fields
                  , fld_expr :: Expression    -- The expression by which this field is attached to the interface
                  , fld_rel :: Relation      -- The relation to which the database table is attached.
                  , fld_editable :: Bool          -- can this field be changed by the user of this interface?
                  , fld_list :: Bool          -- can there be multiple values in this field?
                  , fld_must :: Bool          -- is this field obligatory?
                  , fld_new :: Bool          -- can new elements be filled in? (if no, only existing elements can be selected)
                  , fld_sLevel :: Int           -- The (recursive) depth of the current servlet wrt the entire interface. This is used for documentation.
                  , fld_insAble :: Bool          -- can the user insert in this field?
                  , fld_onIns :: ECArule       -- the PAclause to be executed after an insert on this field
                  , fld_delAble :: Bool          -- can the user delete this field?
                  , fld_onDel :: ECArule       -- the PAclause to be executed after a delete on this field
                  } 
   

data FTheme = FTheme { tconcept :: A_Concept
                     , trules :: [Rule]
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
  name = fsName
  rename fspc nm = fspc{fsName=nm}

instance Identified Finterface where
  name ifc | length prs==1  = name (head prs)
           | otherwise      = fatal 208 "length of prs must be 1"
    where prs = fsv_procRules ifc

instance Identified FSid where
  name (FS_id nm) = nm


data Activity = Act { actRule :: Rule
                    , actTrig :: [Relation]
                    , actAffect :: [Relation]
                    , actQuads :: [Quad]
                    , actEcas :: [ECArule]
                    , actFPA :: FPA
                    , actPurp :: [Purpose]
                    }
instance Identified Activity where
  name act = name (actRule act)
-- | A Quad is used in the "switchboard" of rules. It represents a "proto-rule" with the following meaning:
--   whenever qRel is affected (i.e. tuples in qRel are inserted or deleted), qRule may have to be restored using functionality from qClauses.
--   The rule is taken along for traceability.
       
instance ConceptStructure Activity where
 concs     act = concs (actRule act) `uni` concs (actAffect act)  -- The set of all concepts used in this Activity
 mors      act = mors (actRule act) `uni` actAffect act           -- The set of all relations used in this Activity
 morlist   act = morlist (actRule act) ++ actAffect act           -- The list of all relations in this Activity
--  closExprs act = closExprs (actRule act)                          -- The closure expressions of this Activity

data Quad     = Quad
          { qRel :: Relation        -- The relation that, when affected, triggers a restore action.
          , qClauses :: Clauses         -- The clauses
          } deriving Eq

instance Eq Activity where
  a == a'  = actRule a == actRule a'

data InsDel   = Ins | Del
                 deriving (Show,Eq)
data ECArule= ECA { ecaTriggr :: Event     -- The event on which this rule is activated
                  , ecaDelta :: Relation  -- The delta to be inserted or deleted from this rule. It actually serves very much like a formal parameter.
                  , ecaAction :: PAclause  -- The action to be taken when triggered.
                  , ecaNum :: Int       -- A unique number that identifies the ECArule within its scope.
                  }
instance Eq (ECArule) where
   e==e' = ecaNum e==ecaNum e'
   
data Event = On { eSrt :: InsDel
                  , eRel :: Relation
                  } deriving (Show,Eq)
data PAclause
              = Chc { paCls :: [PAclause]
                    , paMotiv :: [(Expression,[Rule] )] -- tells which conjunct from whichule is being maintained
                    }
              | All { paCls :: [PAclause]
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | Do  { paSrt :: InsDel                   -- do Insert or Delete
                    , paTo :: Expression               -- into toExpr    or from toExpr
                    , paDelta :: Expression               -- delta
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | Sel { paCpt :: A_Concept                -- pick an existing instance of type c
                    , paExp :: Expression               -- the expression to pick from
                    , paCl :: String->PAclause         -- the completion of the clause
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | New { paCpt :: A_Concept                -- make a new instance of type c
                    , paCl :: String->PAclause         -- to be done after creating the concept
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | Rmv { paCpt :: A_Concept                -- Remove an instance of type c
                    , paCl :: String->PAclause         -- to be done afteremoving the concept
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | Nop { paMotiv :: [(Expression,[Rule] )]   -- tells which conjunct from whichule is being maintained
                    }
              | Blk { paMotiv :: [(Expression,[Rule] )]   -- tells which expression from whichule has caused the blockage
                    }
              | Let { paExpr :: PAclause               -- the expression that represents a condition to be tested.
                    , paBody :: PAclause -> PAclause
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | Ref { paVar :: String
                    }
   -- The data structure Clauses is meant for calculation purposes.
   -- It must always satisfy for every i<length (cl_rule cl): cl_rule cl is equivalent to EIsc [EUni disj | (conj, hcs)<-cl_conjNF cl, disj<-[conj!!i]]
   -- Every rule is transformed to this form, as a step to derive eca-rules
instance Eq PAclause where
   Chc ds _ == Chc ds' _ = ds==ds'
   All ds _ == All ds' _ = ds==ds'
   p@Do{}   ==   p'@Do{} = paSrt p==paSrt p' && paTo p==paTo p' && paDelta p==paDelta p'
   Nop _    ==     Nop _ = True
   p@New{}  ==  p'@New{} = paCpt p==paCpt p'
   p@Rmv{}  ==  p'@Rmv{} = paCpt p==paCpt p'
   _ == _ = False


data Clauses  = Clauses
                  { cl_conjNF :: [(Expression
                                  ,[Expression])]   -- The list of pairs (conj, hcs) in which conj is a conjunct of the rule
                                                    -- and hcs contains all derived expressions to be used for eca-rule construction.
                                                    -- hcs contains only disjunctive normal forms.
                  , cl_rule :: Rule -- The rule that is restored by this clause (for traceability purposes)
                  }
instance Eq Clauses where
  cl==cl' = cl_rule cl==cl_rule cl'
    
data FPA = ILGV FPcompl -- bevat permanente, voor de gebruiker relevante gegevens. De gegevens worden door het systeem gebruikt en onderhouden. Onder "onderhouden" verstaat FPA het toevoegen, wijzigen of verwijderen van gegevens.
         | KGV  FPcompl -- bevat permanente, voor de gebruiker relevante gegevens. Deze gegevens worden door het systeem gebruikt, maar worden door een ander systeem onderhouden (voor dat andere systeem is het dus een ILGV).
         | IF   FPcompl -- verwerkt gegevens in een ILGV van het systeem. (dus create, update en delete functies)
         | UF   FPcompl -- presenteert gegevens uit het systeem. Voorbeelden: het afdrukken van alle debiteuren; het aanmaken van facturen; het aanmaken van een diskette met betalingsopdrachten; het medium is hierbij niet van belang: papier, scherm, magneetband, datacom, enzovoorts.
         | OF   FPcompl -- is een speciaal (eenvoudig) soort uitvoerfunctie. Een opvraagfunctie presenteert gegevens uit het systeem op basis van een uniek identificerend zoekgegeven, waarbij geen aanvullende bewerkingen (zoals berekeningen of het bijwerken van een gegevensverzameling) plaats hebben. Voorbeeld: Het tonen van de gegevens van de klant met klantnummer 123456789.
         | NO           -- een onderdeel waaraan geen functiepunten worden toegekend.
           deriving (Eq, Show)

data FPcompl = Eenvoudig | Gemiddeld | Moeilijk deriving (Eq, Show)




data PlugInfo = InternalPlug PlugSQL 
              | ExternalPlug ObjectDef
                deriving (Show, Eq)
instance Identified PlugInfo where
  name (InternalPlug psql) = name psql
  name (ExternalPlug obj)  = name obj

data PlugSQL
 = TblSQL  { sqlname :: String
           , fields :: [SqlField]
           , cLkpTbl :: [(A_Concept,SqlField)]           -- lookup table that links all kernel concepts to fields in the plug
           , mLkpTbl :: [(Expression,SqlField,SqlField)]   -- lookup table that links concepts to column names in the plug (kernel+attRels)
           , sqlfpa :: FPA -- ^ function point analysis
           }
 | BinSQL  { --see rel2plug in ADL2Fspec.hs
             sqlname :: String
           , columns :: (SqlField,SqlField)
           , cLkpTbl :: [(A_Concept,SqlField)] --given that mLkp cannot be (UNI or INJ) (because then r would be in a TblSQL plug)
                                                --if mLkp is TOT, then the concept (source mLkp) is stored in this plug
                                                --if mLkp is SUR, then the concept (target mLkp) is stored in this plug
           , mLkp :: Expression -- the relation links concepts implemented by this plug
           , sqlfpa :: FPA -- ^ function point analysis
           }
 | ScalarSQL
           { sqlname :: String
           , sqlColumn :: SqlField
           , cLkp :: A_Concept -- the concept implemented by this plug
           , sqlfpa :: FPA -- ^ function point analysis
           }
   deriving (Show) 
instance Identified PlugSQL where
  name = sqlname
  rename p x = p{sqlname=x}
instance Eq PlugSQL where
  x==y = name x==name y
instance Ord PlugSQL where
  compare x y = compare (name x) (name y)

-- | This returns all column/table pairs that serve as a concept table for cpt. When adding/removing atoms, all of these
-- columns need to be updated 
lookupCpt :: Fspc -> A_Concept -> [(PlugSQL,SqlField)]
lookupCpt fSpec cpt = [(plug,fld) |InternalPlug plug@(TblSQL{})<-plugInfos fSpec, (c,fld)<-cLkpTbl plug,c==cpt]++
                 [(plug,fld) |InternalPlug plug@(BinSQL{})<-plugInfos fSpec, (c,fld)<-cLkpTbl plug,c==cpt]++
                 [(plug,sqlColumn plug) |InternalPlug plug@(ScalarSQL{})<-plugInfos fSpec, cLkp plug==cpt]

data SqlField = Fld { fldname :: String
                    , fldexpr :: Expression
                    , fldtype :: SqlType
                    , fldnull :: Bool -- can there be empty field-values?
                    , flduniq :: Bool -- are all field-values unique?
                    } deriving (Eq, Show)

instance Ord SqlField where
  compare x y = compare (fldname x) (fldname y)
                    
                    
data SqlType = SQLChar    Int
             | SQLBlob              -- cannot compare, but can show (as a file)
             | SQLPass              -- password, encrypted: cannot show, but can compare
             | SQLSingle  
             | SQLDouble  
             | SQLText              -- cannot compare, but can show (as a text)
             | SQLuInt    Int
             | SQLsInt    Int
             | SQLId                -- autoincrement integer
             | SQLVarchar Int
             | SQLBool              -- exists y/n
             deriving (Eq,Show)

getGeneralizations :: Fspc -> A_Concept -> [A_Concept]
getGeneralizations fSpec c = filter (c Poset.<) $ concs fSpec

getSpecializations :: Fspc -> A_Concept -> [A_Concept]
getSpecializations fSpec c = filter (c Poset.>) $ concs fSpec

  
