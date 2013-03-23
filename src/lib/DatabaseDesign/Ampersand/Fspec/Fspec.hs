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
          , InsDel(..)
          , ECArule(..)
          , Event(..)
          , PAclause(..)
          , Activity(..)
          , PlugSQL(..)
          , lookupCpt
          , metaValues
          , SqlField(..)
          , FPA(..)
          , FPtype(..)
          , FPcompl(..)
          , PlugInfo(..)
          , SqlType(..)
          , SqlFieldUsage(..)
          , getGeneralizations, getSpecializations
          , HornClause(..), horn2expr, events
          )
where
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Basics
import Data.List(nub)
import DatabaseDesign.Ampersand.ADL1.Pair
import DatabaseDesign.Ampersand.ADL1.Expression (notCpl)
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
                 , fRoleRels ::    [(String,Relation)]   -- ^ the relation saying which roles may change the population of which relation.
                 , fRoleRuls ::    [(String,Rule)]          -- ^ the relation saying which roles may change the population of which relation.
                 , vrules ::       [Rule]                   -- ^ All user defined rules that apply in the entire Fspc
                 , grules ::       [Rule]                   -- ^ All rules that are generated: multiplicity rules and key rules
                 , invars ::       [Rule]                   -- ^ All invariant rules
                 , allRules::      [Rule]                   -- ^ All rules, both generated (from multiplicity and keys) as well as user defined ones.
                 , allDeclarations :: [Declaration]               -- ^ All declarations in the fspec
                 , allRelations    :: [Relation]            -- ^ All relations in the fspec
                 , allConcepts ::  [A_Concept]              -- ^ All concepts in the fspec
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
                 , userDefPops ::    [UserDefPop]           -- all user defined populations of relations and concepts
                 , allViolations ::  [(Rule,[Paire])]       -- all rules with violations.
                 }
metaValues :: String -> Fspc -> [String]
metaValues key fSpec = [mtVal m | m <-metas fSpec, mtName m == key]
  

instance ConceptStructure Fspc where
  concs     fSpec = concs (vrels fSpec)                     -- The set of all concepts used in this Fspc
  relationsIn fSpec = foldr (uni) []
                        [ (relationsIn.interfaceS) fSpec
                        , (relationsIn.vrules) fSpec
                        ]
  mp1Exprs  _ = fatal 77 "do not use mp1Exprs from an Fspc"
  
instance Language Fspc where
  objectdef    fSpec = Obj { objnm   = name fSpec
                           , objpos  = Origin "generated object by objectdef (Language Fspc)"
                           , objctx  = iExpr ONE
                           , objmsub = Just . Box $ map ifcObj (interfaceS fSpec ++ interfaceG fSpec)
                           , objstrs = []
                           }
  conceptDefs fSpec = nub (concatMap cptdf (concs fSpec)) --use vConceptDefs to get CDs of concepts not in concs too
   --REMARK: in the fspec we do not distinguish between the disjoint relation declarations and rule declarations (yet?). 
  declarations = vrels
  udefrules    = vrules -- only user defined rules
  invariants    = invars
  keyDefs      = vkeys
  gens         = vgens
  patterns     = vpatterns

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
  showsPrec _ ifc@Fifc{}
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
  concs     ifc = concs (fsv_ifcdef ifc)
  relationsIn   ifc = relationsIn (fsv_ifcdef ifc)
  mp1Exprs  _ = fatal 160 "do not use mp1Exprs from an Finterface"


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

instance Identified Finterface where
  name ifc | length prs==1  = name (head prs)
           | otherwise      = fatal 208 "length of prs must be 1"
    where prs = fsv_procRules ifc

instance Identified FSid where
  name (FS_id nm) = nm


data Activity = Act { actRule ::   Rule
                    , actTrig ::   [Relation]
                    , actAffect :: [Relation]
                    , actQuads ::  [Quad]
                    , actEcas ::   [ECArule]
                    , actPurp ::   [Purpose]
                    }
instance Identified Activity where
  name act = name (actRule act)
-- | A Quad is used in the "switchboard" of rules. It represents a "proto-rule" with the following meaning:
--   whenever qRel is affected (i.e. tuples in qRel are inserted or deleted), qRule may have to be restored using functionality from qClauses.
--   The rule is taken along for traceability.
       
instance ConceptStructure Activity where
 concs     act = concs (actRule act) `uni` concs (actAffect act)
 relationsIn   act = relationsIn (actRule act) `uni` actAffect act
 mp1Exprs  act = mp1Exprs (actRule act)

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
                  } deriving (Show)
instance Eq Event where
 q == q' = fatal 260 "Eq moet worden teruggezet voor Event"

data PAclause
              = CHC { paCls :: [PAclause]
                    , paMotiv :: [(Expression,[Rule] )]   -- tells which conjunct from whichule is being maintained
                    }
              | ALL { paCls :: [PAclause]
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | Do  { paSrt :: InsDel                     -- do Insert or Delete
                    , paTo :: Relation                    -- into toExpr    or from toExpr
                    , paDelta :: Expression               -- delta
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | Sel { paCpt :: A_Concept                  -- pick an existing instance of type c
                    , paExp :: Expression                 -- the expression to pick from
                    , paCl :: String->PAclause            -- the completion of the clause
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | New { paCpt :: A_Concept                  -- make a new instance of type c
                    , paCl :: String->PAclause            -- to be done after creating the concept
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | Rmv { paCpt :: A_Concept                  -- Remove an instance of type c
                    , paCl :: String->PAclause            -- to be done afteremoving the concept
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | Nop { paMotiv :: [(Expression,[Rule] )]   -- tells which conjunct from whichule is being maintained
                    }
              | Blk { paMotiv :: [(Expression,[Rule] )]   -- tells which expression from whichule has caused the blockage
                    }
              | Let { paExpr :: PAclause                  -- the expression that represents a condition to be tested.
                    , paBody :: PAclause -> PAclause
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | Ref { paVar :: String
                    }

events :: PAclause -> [(InsDel,Relation)]
events paClause = fatal 299 $ "terugzetten" -- nub (evs paClause)
 where evs clause
        = case clause of
           CHC{} -> (concat.map evs) (paCls clause)
           ALL{} -> (concat.map evs) (paCls clause)
           Do{}  -> [(paSrt paClause, paTo clause)]
           Sel{} -> evs (paCl clause "")
           New{} -> evs (paCl clause "")
           Rmv{} -> evs (paCl clause "")
           Nop{} -> []
           Blk{} -> []
           Let{} -> fatal 305 "events for let undetermined"
           Ref{} -> fatal 306 "events for Ref undetermined"

   -- The data structure Clauses is meant for calculation purposes.
   -- It must always satisfy for every i<length (cl_rule cl): cl_rule cl is equivalent to EIsc [EUni disj | (conj, hornClauses)<-cl_conjNF cl, disj<-[conj!!i]]
   -- Every rule is transformed to this form, as a step to derive eca-rules
instance Eq PAclause where
   CHC ds _ == CHC ds' _ = ds==ds'
   ALL ds _ == ALL ds' _ = ds==ds'
   p@Do{}   ==   p'@Do{} = paSrt p==paSrt p' && sameDecl (paTo p) (paTo p') && paDelta p==paDelta p'
   Nop _    ==     Nop _ = True
   p@New{}  ==  p'@New{} = paCpt p==paCpt p'
   p@Rmv{}  ==  p'@Rmv{} = paCpt p==paCpt p'
   _ == _ = False


data HornClause = Hc [Expression] [Expression] deriving (Show, Eq) -- Show is for debugging purposes only.

--
horn2expr :: HornClause -> Expression
horn2expr hc@(Hc antcs conss)
 = case (antcs, conss) of
    ([],[]) -> fatal 327 "empty Horn clause"
    ([],_ ) -> cons
    (_ ,[]) -> notCpl sgna antc
    (_ ,_ ) -> notCpl sgna antc .\/. cons
   where
       antc = foldr1 (./\.) antcs
       cons = foldr1 (.\/.) conss
       sgna = case sign antc of
               sgn@(Sign C{} C{}) -> sgn
               sgn -> fatal 336 ("\nThe signature of the antecedent of "++show hc++"\n is "++show sgn)

data Clauses  = Clauses
                  { cl_conjNF :: [(Expression
                                  ,[HornClause])]   -- The list of pairs (conj, hornClauses) in which conj is a conjunct of the rule
                                                    -- and hornClauses contains all derived expressions to be used for eca-rule construction.
                  , cl_rule :: Rule -- The rule that is restored by this clause (for traceability purposes)
                  }
instance Eq Clauses where
  cl==cl' = cl_rule cl==cl_rule cl'
    
data FPA = FPA { fpType :: FPtype
               , complexity :: FPcompl
               } deriving (Show)

-- | These types are defined bij Nesma. See http://www.nesma.nl/sectie/fpa/hoefpa.asp
data FPtype 
 = ILGV -- ^ bevat permanente, voor de gebruiker relevante gegevens. De gegevens worden door het systeem gebruikt en onderhouden. Onder "onderhouden" verstaat FPA het toevoegen, wijzigen of verwijderen van gegevens.
 | KGV  -- ^ bevat permanente, voor de gebruiker relevante gegevens. Deze gegevens worden door het systeem gebruikt, maar worden door een ander systeem onderhouden (voor dat andere systeem is het dus een ILGV).
 | IF   -- ^ verwerkt gegevens in een ILGV van het systeem. (dus create, update en delete functies)
 | UF   -- ^ presenteert gegevens uit het systeem. Voorbeelden: het afdrukken van alle debiteuren; het aanmaken van facturen; het aanmaken van een diskette met betalingsopdrachten; het medium is hierbij niet van belang: papier, scherm, magneetband, datacom, enzovoorts.
 | OF   -- ^ is een speciaal (eenvoudig) soort uitvoerfunctie. Een opvraagfunctie presenteert gegevens uit het systeem op basis van een uniek identificerend zoekgegeven, waarbij geen aanvullende bewerkingen (zoals berekeningen of het bijwerken van een gegevensverzameling) plaats hebben. Voorbeeld: Het tonen van de gegevens van de klant met klantnummer 123456789.
          deriving (Eq, Show)

data FPcompl = Eenvoudig | Gemiddeld | Moeilijk deriving (Eq, Show)




data PlugInfo = InternalPlug PlugSQL 
              | ExternalPlug ObjectDef
                deriving (Show, Eq)
instance Identified PlugInfo where
  name (InternalPlug psql) = name psql
  name (ExternalPlug obj)  = name obj

data PlugSQL
   -- | stores a related collection of relations: a kernel of concepts and attribute relations of this kernel
   --   i.e. a list of SqlField given some A -> [target r | r::A*B,isUni r,isTot r, isInj r] 
   --                                        ++ [target r | r::A*B,isUni r, not(isTot r), not(isSur r)]
   --     kernel = A closure of concepts A,B for which there exists a r::A->B[INJ] 
   --              (r=fldexpr of kernel field holding instances of B, in practice r is I or a makeRelation(flipped declaration))
   --      attribute relations = All concepts B, A in kernel for which there exists a r::A*B[UNI] and r not TOT and SUR
   --              (r=fldexpr of attMor field, in practice r is a makeRelation(declaration))
 = TblSQL  { sqlname :: String
           , fields :: [SqlField]
           , cLkpTbl :: [(A_Concept,SqlField)]             -- ^ lookup table that links all kernel concepts to fields in the plug
                                                           -- cLkpTbl is een lijst concepten die in deze plug opgeslagen zitten, en hoe je ze eruit kunt halen
           , mLkpTbl :: [(Expression,SqlField,SqlField)]   -- ^ lookup table that links concepts to column names in the plug (kernel+attRels)
                                                           -- mLkpTbl is een lijst met relaties die in deze plug opgeslagen zitten, en hoe je ze eruit kunt halen
           }
   -- | stores one relation r in two ordered columns
   --   i.e. a tuple of SqlField -> (source r,target r) with (fldexpr=I/\r;r~, fldexpr=r) 
   --   (note: if r TOT then (I/\r;r~ = I). Thus, the concept (source r) is stored in this plug too)
   --   with tblcontents = [[x,y] |(x,y)<-contents r]. 
   --   Typical for BinSQL is that it has exactly two columns that are not unique and may not contain NULL values
 | BinSQL  { sqlname :: String
           , columns :: (SqlField,SqlField)
           , cLkpTbl :: [(A_Concept,SqlField)] --given that mLkp cannot be (UNI or INJ) (because then r would be in a TblSQL plug)
                                                --if mLkp is TOT, then the concept (source mLkp) is stored in this plug
                                                --if mLkp is SUR, then the concept (target mLkp) is stored in this plug
           , mLkp :: Expression -- the relation links concepts implemented by this plug
           }
 -- |stores one concept c in one column
 --  i.e. a SqlField -> c
 --  with tblcontents = [[x] |(x,_)<-contents c].
 --  Typical for ScalarSQL is that it has exactly one column that is unique and may not contain NULL values i.e. fldexpr=I[c]
 | ScalarSQL
           { sqlname :: String
           , sqlColumn :: SqlField
           , cLkp :: A_Concept -- the concept implemented by this plug
           }
   deriving (Show) 
instance Identified PlugSQL where
  name = sqlname
instance Eq PlugSQL where
  x==y = name x==name y
instance Ord PlugSQL where
  compare x y = compare (name x) (name y)

-- | This returns all column/table pairs that serve as a concept table for cpt. When adding/removing atoms, all of these
-- columns need to be updated 
lookupCpt :: Fspc -> A_Concept -> [(PlugSQL,SqlField)]
lookupCpt fSpec cpt = [(plug,fld) |InternalPlug plug@TblSQL{}<-plugInfos fSpec, (c,fld)<-cLkpTbl plug,c==cpt]++
                 [(plug,fld) |InternalPlug plug@BinSQL{}<-plugInfos fSpec, (c,fld)<-cLkpTbl plug,c==cpt]++
                 [(plug,sqlColumn plug) |InternalPlug plug@ScalarSQL{}<-plugInfos fSpec, cLkp plug==cpt]

data SqlFieldUsage = PrimKey A_Concept     -- The field is the primary key of the table
                   | ForeignKey A_Concept  -- The field is a reference (containing the primary key value of) a TblSQL
                   | PlainAttr             -- None of the above
                   | NonMainKey            -- Key value of an Specialization of the Primary key. (field could be null)
                   | UserDefinedUsage
                   | FillInLater          -- Must be filled in later....
                   
                   deriving (Eq, Show)
data SqlField = Fld { fldname :: String
                    , fldexpr :: Expression     -- ^ De target van de expressie geeft de waarden weer in de SQL-tabel-kolom.
                    , fldtype :: SqlType
                    , flduse  :: SqlFieldUsage
                    , fldnull :: Bool           -- ^ can there be empty field-values? (intended for data dictionary of DB-implementation)
                    , flduniq :: Bool           -- ^ are all field-values unique? (intended for data dictionary of DB-implementation)
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

  
