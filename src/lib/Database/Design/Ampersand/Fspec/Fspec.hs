{-# OPTIONS_GHC -Wall #-}
{- | The intentions behind Fspc (SJ 30 dec 2008):
Generation of functional specifications is the core functionality of Ampersand.
All items in a specification are generated into the following data structure, Fspc.
It is built by compiling an Ampersand script and translating that to Fspc.
In the future, other ways of 'filling' Fspc are foreseen.
All generators (such as the code generator, the proof generator, the atlas generator, etc.)
are merely different ways to show Fspc.
-}
module Database.Design.Ampersand.Fspec.Fspec
          ( Fspc(..), concDefs, Atom(..)
          , Fswitchboard(..), Quad(..)
          , FSid(..), FProcess(..)
--        , InsDel(..)
          , ECArule(..)
--        , Event(..)
--        , PAclause(..)
          , Activity(..)
          , PlugSQL(..),plugFields
          , lookupCpt
          , metaValues
          , SqlField(..)
          , Object(..)
          , FPA(..)
          , FPtype(..)
          , FPcompl(..)
          , PlugInfo(..)
          , SqlType(..)
          , SqlFieldUsage(..)
          , getGeneralizations, getSpecializations
          , Conjunct(..),DnfClause(..), dnf2expr
          )
where
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc.Options (Options)
import Database.Design.Ampersand.ADL1.Pair
import Database.Design.Ampersand.ADL1.Expression (notCpl)
--import Debug.Trace

fatal :: Int -> String -> a
fatal = fatalMsg "Fspec.Fspec"

data Fspc = Fspc { fsName ::       String                   -- ^ The name of the specification, taken from the Ampersand script
                 , flags ::        Options                  -- ^ The command line options that were used when this Fspc was compiled  by Ampersand.
                 , fspos ::        [Origin]                 -- ^ The origin of the Fspc. An Fspc can be a merge of a file including other files c.q. a list of Origin.
                 , themes ::       [String]                 -- ^ The names of patterns/processes to be printed in the functional specification. (for making partial documentation)
                   , pattsInScope :: [Pattern]
                   , procsInScope :: [Process]
                   , rulesInScope :: [Rule]
                   , declsInScope :: [Declaration]
                   , concsInScope :: [A_Concept]
                   , cDefsInScope :: [ConceptDef]
                   , gensInScope  :: [A_Gen]
                 , fsLang ::       Lang                     -- ^ The default language for this specification (always specified, so no Maybe here!).
                 , vprocesses ::   [FProcess]               -- ^ All processes defined in the Ampersand script
                 , vplugInfos ::   [PlugInfo]               -- ^ All plugs defined in the Ampersand script
                 , plugInfos ::    [PlugInfo]               -- ^ All plugs (defined and derived)
                 , interfaceS ::   [Interface]              -- ^ All interfaces defined in the Ampersand script
                 , interfaceG ::   [Interface]              -- ^ All interfaces derived from the basic ontology (the Lonneker interface)
                 , fSwitchboard :: Fswitchboard             -- ^ The code to be executed to maintain the truth of invariants
                 , fActivities ::  [Activity]               -- ^ generated: One Activity for every ObjectDef in interfaceG and interfaceS
                 , fRoleRels ::    [(String,Declaration)]   -- ^ the relation saying which roles may change the population of which relation.
                 , fRoleRuls ::    [(String,Rule)]          -- ^ the relation saying which roles may change the population of which relation.
                 , fRoles ::       [String]                 -- ^ All roles mentioned in this context.
                 , vrules ::       [Rule]                   -- ^ All user defined rules that apply in the entire Fspc
                 , grules ::       [Rule]                   -- ^ All rules that are generated: multiplicity rules and identity rules
                 , invars ::       [Rule]                   -- ^ All invariant rules
                 , allRules::      [Rule]                   -- ^ All rules, both generated (from multiplicity and keys) as well as user defined ones.
                 , allUsedDecls :: [Declaration]            -- ^ All relations that are used in the fSpec
                 , allDecls ::     [Declaration]            -- ^ All relations that are declared in the fSpec
                 , vrels ::        [Declaration]            -- ^ All user defined and generated relations plus all defined and computed totals.
                                                            --   The generated relations are all generalizations and
                                                            --   one declaration for each signal.
                 , allConcepts ::  [A_Concept]              -- ^ All concepts in the fSpec
                 , kernels ::      [[A_Concept]]            -- ^ All concepts, grouped by their classifications
                 , vIndices ::     [IdentityDef]            -- ^ All keys that apply in the entire Fspc
                 , vviews ::       [ViewDef]                -- ^ All views that apply in the entire Fspc
                 , vgens ::        [A_Gen]                  -- ^ All gens that apply in the entire Fspc
                 , vconjs ::       [Conjunct]               -- ^ All conjuncts generated (by ADL2Fspec)
                 , vquads ::       [Quad]                   -- ^ All quads generated (by ADL2Fspec)
                 , vEcas ::        [ECArule]                -- ^ All ECA rules generated (by ADL2Fspec)
                 , fsisa ::        [(A_Concept, A_Concept)] -- ^ generated: The data structure containing the generalization structure of concepts
                 , vpatterns ::    [Pattern]                -- ^ All patterns taken from the Ampersand script
                 , conceptDefs ::  [ConceptDef]             -- ^ All concept definitions defined throughout a context, including those inside patterns and processes
                 , fSexpls ::      [Purpose]                -- ^ All purposes that have been declared at the top level of the current specification, but not in the processes, patterns and interfaces.
                 , metas ::        [Meta]                   -- ^ All meta relations from the entire context
                 , initialPops ::  [Population]             -- all user defined populations of relations and concepts
                 , allViolations :: [(Rule,[Paire])]        -- all rules with violations.
                 }
metaValues :: String -> Fspc -> [String]
metaValues key fSpec = [mtVal m | m <-metas fSpec, mtName m == key]

data Atom = Atom { atmRoot :: A_Concept -- The root concept of the atom. (this implies that there can only be a single root for
                 , atmVal :: String
                 } deriving Eq

concDefs :: Fspc -> A_Concept -> [ConceptDef]
concDefs fSpec c = [ cdef | cdef<-conceptDefs fSpec, name cdef==name c ]

instance ConceptStructure Fspc where
  concs     fSpec = allConcepts fSpec                     -- The set of all concepts used in this Fspc
  expressionsIn fSpec = foldr (uni) []
                        [ (expressionsIn.interfaceS) fSpec
                        , (expressionsIn.vrules) fSpec
                        , (expressionsIn.vviews) fSpec
                        , (expressionsIn.vIndices) fSpec
                        ]
  mp1Exprs  _ = fatal 77 "do not use mp1Exprs from an Fspc"

instance Language Fspc where
  objectdef    fSpec = Obj { objnm   = name fSpec
                           , objpos  = Origin "generated object by objectdef (Language Fspc)"
                           , objctx  = EDcI ONE
                           , objmsub = Just . Box ONE $ map ifcObj (interfaceS fSpec ++ interfaceG fSpec)
                           , objstrs = []
                           }
   --REMARK: in the fSpec we do not distinguish between the disjoint relation declarations and rule declarations (yet?).
  relsDefdIn = vrels
  udefrules  = vrules -- only user defined rules
  invariants = invars
  identities = vIndices
  viewDefs   = vviews
  gens       = vgens
  patterns   = vpatterns

data FProcess
  = FProc { fpProc :: Process
          , fpActivities :: [Activity]
          }
instance Identified FProcess where
  name = name . fpProc

instance Language FProcess where
  objectdef  = objectdef.fpProc
  relsDefdIn = relsDefdIn.fpProc
  udefrules  = udefrules.fpProc
  invariants = invariants.fpProc
  identities = identities.fpProc
  viewDefs   = viewDefs.fpProc
  gens       = gens.fpProc
  patterns   = patterns.fpProc

-- | A list of ECA rules, which is used for automated functionality.
data Fswitchboard
  = Fswtch { fsbEvIn :: [Event]
           , fsbEvOut :: [Event]
           , fsbConjs :: [(Rule, Expression)]
           , fsbECAs :: [ECArule]
           }

--type Fields = [Field]
--data Field  = Att { fld_name :: String        -- The name of this field
--                  , fld_sub :: Fields        -- all sub-fields
--                  , fld_expr :: Expression    -- The expression by which this field is attached to the interface
--                  , fld_rel :: Relation      -- The relation to which the database table is attached.
--                  , fld_editable :: Bool          -- can this field be changed by the user of this interface?
--                  , fld_list :: Bool          -- can there be multiple values in this field?
--                  , fld_must :: Bool          -- is this field obligatory?
--                  , fld_new :: Bool          -- can new elements be filled in? (if no, only existing elements can be selected)
--                  , fld_sLevel :: Int           -- The (recursive) depth of the current servlet wrt the entire interface. This is used for documentation.
--                  , fld_insAble :: Bool          -- can the user insert in this field?
--                  , fld_onIns :: ECArule       -- the PAclause to be executed after an insert on this field
--                  , fld_delAble :: Bool          -- can the user delete this field?
--                  , fld_onDel :: ECArule       -- the PAclause to be executed after a delete on this field
--                  }

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

instance Identified FSid where
  name (FS_id nm) = nm

data Activity = Act { actRule ::   Rule
                    , actTrig ::   [Declaration]
                    , actAffect :: [Declaration]
                    , actQuads ::  [Quad]
                    , actEcas ::   [ECArule]
                    , actPurp ::   [Purpose]
                    }
instance Identified Activity where
  name act = name (actRule act)
-- | A Quad is used in the "switchboard" of rules. It represents a "proto-rule" with the following meaning:
--   whenever qDcl is affected (i.e. tuples in qDcl are inserted or deleted), qRule may have to be restored using functionality from qConjuncts.
--   The rule is taken along for traceability.

instance ConceptStructure Activity where
 concs         act = concs (actRule act) `uni` concs (actAffect act)
 expressionsIn act = expressionsIn (actRule act)

data Quad = Quad { qDcl ::       Declaration   -- The relation that, when affected, triggers a restore action.
                 , qRule ::      Rule          -- The rule from which qConjuncts is derived.
                 , qConjuncts :: [Conjunct]    -- The conjuncts, with clauses included
                 } deriving Show

instance Eq Quad where
  q == q'  = qDcl q == qDcl q' && qRule q == qRule q'

instance Eq Activity where
  a == a'  = actRule a == actRule a'

--
dnf2expr :: DnfClause -> Expression
dnf2expr (Dnf antcs conss)
 = case (antcs, conss) of
    ([],[]) -> fatal 327 "empty dnf clause"
    ([],_ ) -> foldr1 (.\/.) conss
    (_ ,[]) -> notCpl (foldr1 (./\.) antcs)
    (_ ,_ ) -> notCpl (foldr1 (./\.) antcs) .\/. (foldr1 (.\/.) conss)

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
instance ConceptStructure PlugInfo where
  concs   (InternalPlug psql) = concs   psql
  concs   (ExternalPlug obj)  = concs   obj
  expressionsIn (InternalPlug psql) = expressionsIn psql
  expressionsIn (ExternalPlug obj)  = expressionsIn obj
instance ConceptStructure PlugSQL where
  concs     p = concs   (plugFields p)
  expressionsIn   p = expressionsIn (plugFields p)
  mp1Exprs = fatal 458 "mp1Exprs is not meant to be for a plug."

data PlugSQL
   -- | stores a related collection of relations: a kernel of concepts and attribute relations of this kernel
   --   i.e. a list of SqlField given some A -> [target r | r::A*B,isUni r,isTot r, isInj r]
   --                                        ++ [target r | r::A*B,isUni r, not(isTot r), not(isSur r)]
   --     kernel = A closure of concepts A,B for which there exists a r::A->B[INJ]
   --              (r=fldexpr of kernel field holding instances of B, in practice r is I or a makeRelation(flipped declaration))
   --      attribute relations = All concepts B, A in kernel for which there exists a r::A*B[UNI] and r not TOT and SUR
   --              (r=fldexpr of attMor field, in practice r is a makeRelation(declaration))
 = TblSQL  { sqlname :: String
           , fields :: [SqlField]                          -- ^ the first field is the concept table of the most general concept (e.g. Person)
                                                           --   then follow concept tables of specializations. Together with the first field this is called the "kernel"
                                                           --   the remaining fields represent attributes.
           , cLkpTbl :: [(A_Concept,SqlField)]             -- ^ lookup table that links all kernel concepts to fields in the plug
                                                           -- cLkpTbl is een lijst concepten die in deze plug opgeslagen zitten, en hoe je ze eruit kunt halen
           , mLkpTbl :: [(Expression,SqlField,SqlField)]   -- ^ lookup table that links concepts to column names in the plug (kernel+attRels)
                                                           -- mLkpTbl is een lijst met relaties die in deze plug opgeslagen zitten, en hoe je ze eruit kunt halen
           }
   -- | stores one relation r in two ordered columns
   --   i.e. a tuple of SqlField -> (source r,target r) with (fldexpr=I/\r;r~, fldexpr=r)
   --   (note: if r TOT then (I/\r;r~ = I). Thus, the concept (source r) is stored in this plug too)
   --   with tblcontents = [[Just x,Just y] |(x,y)<-contents r].
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
 --  with tblcontents = [[Just x] |(x,_)<-contents c].
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

plugFields :: PlugSQL->[SqlField]
plugFields plug = case plug of
    TblSQL{}    -> fields plug
    BinSQL{}    -> [fst(columns plug),snd(columns plug)]
    ScalarSQL{} -> [sqlColumn plug]

-- | This returns all column/table pairs that serve as a concept table for cpt. When adding/removing atoms, all of these
-- columns need to be updated
lookupCpt :: Fspc -> A_Concept -> [(PlugSQL,SqlField)]
lookupCpt fSpec cpt = [(plug,fld) |InternalPlug plug@TblSQL{}<-plugInfos fSpec, (c,fld)<-cLkpTbl plug,c==cpt]++
                 [(plug,fld) |InternalPlug plug@BinSQL{}<-plugInfos fSpec, (c,fld)<-cLkpTbl plug,c==cpt]++
                 [(plug,sqlColumn plug) |InternalPlug plug@ScalarSQL{}<-plugInfos fSpec, cLkp plug==cpt]

data SqlFieldUsage = TableKey Bool A_Concept  -- The field is the (primary) key of the table. (The boolean tells whether or not it is primary)
                   | ForeignKey A_Concept  -- The field is a reference (containing the primary key value of) a TblSQL
                   | PlainAttr             -- None of the above
                   deriving (Eq, Show)

data SqlField = Fld { fldname :: String
                    , fldexpr :: Expression     -- ^ De target van de expressie geeft de waarden weer in de SQL-tabel-kolom.
                    , fldtype :: SqlType
                    , flduse ::  SqlFieldUsage
                    , fldnull :: Bool           -- ^ True if there can be empty field-values (intended for data dictionary of DB-implementation)
                    , flduniq :: Bool           -- ^ True if all field-values are unique? (intended for data dictionary of DB-implementation)
                    } deriving (Eq, Show)

instance Ord SqlField where
  compare x y = compare (fldname x) (fldname y)
instance ConceptStructure SqlField where
  concs     f = [target e' |let e'=fldexpr f,isSur e']
  expressionsIn   f = expressionsIn   (fldexpr f)
  mp1Exprs = fatal 452 "mp1Exprs is not meant to be for a plug."

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
getGeneralizations fSpec = largerConcepts (gens fSpec)

getSpecializations :: Fspc -> A_Concept -> [A_Concept]
getSpecializations fSpec = smallerConcepts (gens fSpec)

