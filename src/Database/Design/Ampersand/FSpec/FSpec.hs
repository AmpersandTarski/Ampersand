{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{- | The intentions behind FSpec (SJ 30 dec 2008):
Generation of functional specifications is the core functionality of Ampersand.
All items in a specification are generated into the following data structure, FSpec.
It is built by compiling an Ampersand script and translating that to FSpec.
In the future, other ways of 'filling' FSpec are foreseen.
All generators (such as the code generator, the proof generator, the atlas generator, etc.)
are merely different ways to show FSpec.
-}
module Database.Design.Ampersand.FSpec.FSpec
          ( FSpec(..), concDefs, Atom(..), A_Pair(..)
          , Fswitchboard(..), Quad(..)
          , A_Concept, Declaration, A_Gen
          , FSid(..)
--        , InsDel(..)
          , ECArule(..)
--        , Event(..)
--        , PAclause(..)
          , Activity(..)
          , PlugSQL(..),plugFields
          , lookupCpt, getConceptTableFor
          , metaValues
          , SqlField(..)
          , Object(..)
          , PlugInfo(..)
          , SqlTType(..)
          , SqlFieldUsage(..)
          , Conjunct(..),DnfClause(..), dnf2expr, notCpl
          , Language(..),AAtomValue
          , showValADL,showValPHP,showValSQL
          , module Database.Design.Ampersand.FSpec.ToFSpec.Populated 
          ) where
-- TODO: Export module Database.Design.Ampersand.Core.AbstractSyntaxTree in the same way as is done
--       for module Database.Design.Ampersand.Core.ParseTree in that module. Then build to a better
--       hyrarchie to reflect the Architecture. 
import Data.List
import Data.Typeable
import Database.Design.Ampersand.ADL1.Expression (notCpl)
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.FSpec.Crud
import Database.Design.Ampersand.Misc.Options (Options)
import Text.Pandoc.Builder (Blocks)
import Database.Design.Ampersand.FSpec.ToFSpec.Populated

fatal :: Int -> String -> a
fatal = fatalMsg "FSpec.FSpec"

data FSpec = FSpec { fsName ::       String                   -- ^ The name of the specification, taken from the Ampersand script
                   , originalContext :: A_Context             -- ^ the original context. (for showADL)  
                   , getOpts ::      Options                  -- ^ The command line options that were used when this FSpec was compiled  by Ampersand.
                   , fspos ::        [Origin]                 -- ^ The origin of the FSpec. An FSpec can be a merge of a file including other files c.q. a list of Origin.
                   , themes ::       [String]                 -- ^ The names of patterns/processes to be printed in the functional specification. (for making partial documentation)
                     , pattsInScope :: [Pattern]
                     , rulesInScope :: [Rule]
                     , declsInScope :: [Declaration]
                     , concsInScope :: [A_Concept]
                     , cDefsInScope :: [ConceptDef]
                     , gensInScope ::  [A_Gen]
                   , fsLang ::       Lang                     -- ^ The default language for this specification (always specified, so no Maybe here!).
                   , vplugInfos ::   [PlugInfo]               -- ^ All plugs defined in the Ampersand script
                   , plugInfos ::    [PlugInfo]               -- ^ All plugs (defined and derived)
                   , interfaceS ::   [Interface]              -- ^ All interfaces defined in the Ampersand script
                   , interfaceG ::   [Interface]              -- ^ All interfaces derived from the basic ontology (the Lonneker interface)
                   , roleInterfaces  :: Role -> [Interface]   -- ^ All interfaces defined in the Ampersand script, for use by a specific Role
                   , fSwitchboard :: Fswitchboard             -- ^ The code to be executed to maintain the truth of invariants
                   , fDeriveProofs :: Blocks                  -- ^ The proofs in Pandoc format
                   , fActivities ::  [Activity]               -- ^ generated: One Activity for every ObjectDef in interfaceG and interfaceS
                   , fRoleRels ::    [(Role,Declaration)]     -- ^ the relation saying which roles may change the population of which relation.
                   , fRoleRuls ::    [(Role,Rule)]            -- ^ the relation saying which roles may change the population of which relation.
                   , fRoles ::       [Role]                   -- ^ All roles mentioned in this context.
                   , fallRules ::    [Rule]
                   , vrules ::       [Rule]                   -- ^ All user defined rules that apply in the entire FSpec
                   , grules ::       [Rule]                   -- ^ All rules that are generated: multiplicity rules and identity rules
                   , invariants ::   [Rule]                   -- ^ All invariant rules
                   , allUsedDecls :: [Declaration]            -- ^ All relations that are used in the fSpec
                   , allDecls ::     [Declaration]            -- ^ All relations that are declared in the fSpec
                   , vrels ::        [Declaration]            -- ^ All user defined and generated relations plus all defined and computed totals.
                                                              --   The generated relations are all generalizations and
                                                              --   one declaration for each signal.
                   , allConcepts ::  [A_Concept]              -- ^ All concepts in the fSpec
                   , kernels ::      [[A_Concept]]            -- ^ All concepts, grouped by their classifications
                   , cptTType :: A_Concept -> TType 
                   , vIndices ::     [IdentityDef]            -- ^ All keys that apply in the entire FSpec
                   , vviews ::       [ViewDef]                -- ^ All views that apply in the entire FSpec
                   , getDefaultViewForConcept :: A_Concept -> Maybe ViewDef
                   , lookupView :: String -> ViewDef          -- ^ Lookup view by id in fSpec.
                   , vgens ::        [A_Gen]                  -- ^ All gens that apply in the entire FSpec
                   , vconjs ::       [Conjunct]               -- ^ All conjuncts generated (by ADL2FSpec)
                   , allConjsPerRule :: [(Rule,[Conjunct])]   -- ^ Maps each rule onto the conjuncts it consists of (note that a single conjunct may be part of several rules) 
                   , allConjsPerDecl :: [(Declaration, [Conjunct])]   -- ^ Maps each declaration to the conjuncts it appears in   
                   , allConjsPerConcept :: [(A_Concept, [Conjunct])]  -- ^ Maps each concept to the conjuncts it appears in (as source or target of a constituent relation)
                   , vquads ::       [Quad]                   -- ^ All quads generated (by ADL2FSpec)
                   , vEcas ::        [ECArule]                -- ^ All ECA rules generated (by ADL2FSpec)
                   , fsisa ::        [(A_Concept, A_Concept)] -- ^ generated: The data structure containing the generalization structure of concepts
                   , vpatterns ::    [Pattern]                -- ^ All patterns taken from the Ampersand script
                   , conceptDefs ::  [ConceptDef]             -- ^ All concept definitions defined throughout a context, including those inside patterns and processes
                   , fSexpls ::      [Purpose]                -- ^ All purposes that have been declared at the top level of the current specification, but not in the processes, patterns and interfaces.
                   , metas ::        [Meta]                   -- ^ All meta relations from the entire context
                   , crudInfo ::     CrudInfo                 -- ^ Information for CRUD matrices 
               --    , popsOfCptWithoutSmaller :: A_Concept -> [Population]  -- ^ All user defined populations of an A_concept, WITHOUT the populations of smaller A_Concepts
                   , atomsInCptIncludingSmaller :: A_Concept -> [AAtomValue] -- ^ All user defined populations of an A_concept, INCLUDING the populations of smaller A_Concepts
                   , tableContents :: PlugSQL -> [[Maybe AAtomValue]] -- ^ tableContents is meant to compute the contents of an entity table.
                                                                      --   It yields a list of records. Values in the records may be absent, which is why Maybe is used rather than String.
                   
                   , pairsInExpr :: Expression -> [AAtomPair]   
                   , initialConjunctSignals :: [(Conjunct,[AAtomPair])] -- ^ All conjuncts that have process-rule violations.
                   , allViolations ::  [(Rule,[AAtomPair])]   -- ^ All invariant rules with violations.
                   , allExprs ::     [Expression]             -- ^ All expressions in the fSpec
                   , allSigns ::     [Signature]              -- ^ All Signs in the fSpec
                   , contextInfo   :: ContextInfo 
                   , specializationsOf :: A_Concept -> [A_Concept]    
                   , generalizationsOf :: A_Concept -> [A_Concept]
                   , editableConcepts :: Role -> [A_Concept]  -- ^ All editable concepts per role. (See https://github.com/AmpersandTarski/ampersand/issues/211 )
                   } deriving Typeable
instance Eq FSpec where
 f == f' = name f == name f'
instance Unique FSpec where
 showUnique = name
metaValues :: String -> FSpec -> [String]
metaValues key fSpec = [mtVal m | m <-metas fSpec, mtName m == key]

data Atom = Atom { atmRoots :: [A_Concept] -- The root concept(s) of the atom.
                 , atmIn ::    [A_Concept] -- all concepts the atom is in. (Based on generalizations)
                 , atmVal   :: AAtomValue
                 } deriving (Typeable,Eq)
instance Unique Atom where
  showUnique a = showValADL (atmVal a)++" in "
         ++case atmRoots a of
             []  -> fatal 110 "an atom must have at least one root concept"
             [x] -> uniqueShow True x
             xs  -> "["++intercalate ", " (map (uniqueShow True) xs)++"]"

data A_Pair = Pair { lnkDcl :: Declaration
                   , lnkLeft :: Atom
                   , lnkRight :: Atom
                   } deriving (Typeable,Eq)
instance Association A_Pair where
  sign = sign . lnkDcl
instance Unique A_Pair where
  showUnique x = uniqueShow False (lnkDcl x)
              ++ uniqueShow False (lnkLeft x)
              ++ uniqueShow False (lnkRight x)
concDefs :: FSpec -> A_Concept -> [ConceptDef]
concDefs fSpec c = [ cdef | cdef<-conceptDefs fSpec, name cdef==name c ]

instance ConceptStructure FSpec where
  concs     fSpec = allConcepts fSpec                     -- The set of all concepts used in this FSpec
  expressionsIn fSpec = allExprs fSpec

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
instance Named FSpec where
  name = fsName

instance Named FSid where
  name (FS_id nm) = nm

data Activity = Act { actRule ::   Rule
                    , actTrig ::   [Declaration]
                    , actAffect :: [Declaration]
                    , actQuads ::  [Quad]
                    , actEcas ::   [ECArule]
                    , actPurp ::   [Purpose]
                    } deriving Show

instance Named Activity where
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
dnf2expr dnf
 = case (antcs dnf, conss dnf) of
    ([],[]) -> fatal 327 "empty dnf clause"
    ([],cs ) -> foldr1 (.\/.) cs
    (as,[]) -> notCpl (foldr1 (./\.) as)
    (as,cs) -> notCpl (foldr1 (./\.) as) .\/. (foldr1 (.\/.) cs)

data PlugInfo = InternalPlug PlugSQL
              | ExternalPlug ObjectDef
                deriving (Show, Eq,Typeable)
instance Named PlugInfo where
  name (InternalPlug psql) = name psql
  name (ExternalPlug obj)  = name obj
instance Unique PlugInfo where
  showUnique (InternalPlug psql) = "SQLTable "++name psql
  showUnique (ExternalPlug obj ) = "Object "++name obj++show (origin obj)
instance ConceptStructure PlugInfo where
  concs   (InternalPlug psql) = concs   psql
  concs   (ExternalPlug obj)  = concs   obj
  expressionsIn (InternalPlug psql) = expressionsIn psql
  expressionsIn (ExternalPlug obj)  = expressionsIn obj
instance ConceptStructure PlugSQL where
  concs     p = concs   (plugFields p)
  expressionsIn   p = expressionsIn (plugFields p)

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
   deriving (Show, Typeable)
instance Named PlugSQL where
  name = sqlname
instance Eq PlugSQL where
  x==y = name x==name y
instance Unique PlugSQL where
  showUnique = name
instance Ord PlugSQL where
  compare x y = compare (name x) (name y)

plugFields :: PlugSQL->[SqlField]
plugFields plug = case plug of
    TblSQL{}    -> fields plug
    BinSQL{}    -> [fst(columns plug),snd(columns plug)]
    ScalarSQL{} -> [sqlColumn plug]

-- | This returns all column/table pairs that serve as a concept table for cpt. When adding/removing atoms, all of these
-- columns need to be updated
lookupCpt :: FSpec -> A_Concept -> [(PlugSQL,SqlField)]
lookupCpt fSpec cpt = [(plug,fld) |InternalPlug plug@TblSQL{}<-plugInfos fSpec, (c,fld)<-cLkpTbl plug,c==cpt]++
                 [(plug,fld) |InternalPlug plug@BinSQL{}<-plugInfos fSpec, (c,fld)<-cLkpTbl plug,c==cpt]++
                 [(plug,sqlColumn plug) |InternalPlug plug@ScalarSQL{}<-plugInfos fSpec, cLkp plug==cpt]

-- Convenience function that returns the name of the table that contains the concept table (or more accurately concept column) for c
getConceptTableFor :: FSpec -> A_Concept -> String
getConceptTableFor fSpec c = case lookupCpt fSpec c of
                               []      -> fatal 297 $ "tableFor: No concept table for " ++ name c
                               (t,_):_ -> name t -- in case there are more, we use the first one

data SqlFieldUsage = TableKey Bool A_Concept  -- The field is the (primary) key of the table. (The boolean tells whether or not it is primary)
                   | ForeignKey A_Concept  -- The field is a reference (containing the primary key value of) a TblSQL
                   | PlainAttr             -- None of the above
                   deriving (Eq, Show)

data SqlField = Fld { fldname :: String
                    , fldexpr :: Expression     -- ^ De target van de expressie geeft de waarden weer in de SQL-tabel-kolom.
                    , fldtype :: SqlTType
                    , flduse ::  SqlFieldUsage
                    , fldnull :: Bool           -- ^ True if there can be empty field-values (intended for data dictionary of DB-implementation)
                    , flduniq :: Bool           -- ^ True if all field-values are unique? (intended for data dictionary of DB-implementation)
                    } deriving (Eq, Show,Typeable)
instance Named SqlField where
  name = fldname
instance Unique (PlugSQL,SqlField) where
  showUnique (p,f) = showUnique p++"."++fldname f
instance Ord SqlField where
  compare x y = compare (fldname x) (fldname y)
instance ConceptStructure SqlField where
  concs     f = [target e' |let e'=fldexpr f,isSur e']
  expressionsIn   f = expressionsIn   (fldexpr f)

data SqlTType = SQLFloat   -- See http://dev.mysql.com/doc/refman/5.7/en/data-types.html
             | SQLVarchar Int
             | SQLText
             | SQLMediumText
             | SQLBlob
             | SQLMediumBlob
             | SQLLongBlob
             | SQLDate     -- MySQL retrieves and displays DATE values in 'YYYY-MM-DD' format
             | SQLDateTime -- MySQL retrieves and displays DATETIME values in 'YYYY-MM-DD HH:MM:SS' format
             | SQLBool
             | SQLBigInt
             | SQLSerial   -- SERIAL is an alias for BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE
             
             deriving (Eq,Show)


