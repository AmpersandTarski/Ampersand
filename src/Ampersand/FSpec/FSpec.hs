{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{- | The intentions behind FSpec (SJ 30 dec 2008):
Generation of functional designs is a core functionality of Ampersand.
All items in a specification are generated into the following data structure, FSpec.
It is built by compiling an Ampersand script and translating that to FSpec.
In the future, other ways of 'filling' FSpec are foreseen.
All generators (such as the code generator, the proof generator, the atlas generator, etc.)
are merely different ways to show FSpec.
-}
module Ampersand.FSpec.FSpec
          ( MultiFSpecs(..)
          , FSpec(..), concDefs, Atom(..), A_Pair(..)
          , Quad(..)
          , FSid(..)
          , PlugSQL(..),plugAttributes
          , lookupCpt, getConceptTableFor
          , RelStore(..)
          , metaValues
          , SqlAttribute(..),isPrimaryKey,isForeignKey
          , Typology(..)
          , Interface(..)
          , Object(..)
          , ObjectDef(..)
          , SubInterface(..)
          , PlugInfo(..)
          , SqlAttributeUsage(..)
          , Conjunct(..),DnfClause(..), dnf2expr, notCpl
          , Language(..)
          , showSQL
      --    , module Ampersand.Classes
          ) where
import Data.List
import Data.Text (Text,unpack)
import Data.Typeable
import Ampersand.ADL1.Expression (notCpl)
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.Core.ParseTree
        ( Traced(..), Origin
        , Role
        , ConceptDef
        )
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.FSpec.Crud
import Ampersand.Misc
import Text.Pandoc.Builder (Blocks)

data MultiFSpecs = MultiFSpecs
                   { userFSpec :: FSpec        -- ^ The FSpec based on the user's script only.
                   , metaFSpec :: Maybe FSpec  -- ^ The FormalAmpersand metamodel, populated with the items from the user's script 
                   }
data FSpec = FSpec { fsName ::       Text                   -- ^ The name of the specification, taken from the Ampersand script
                   , originalContext :: A_Context             -- ^ the original context. (for showA)  
                   , getOpts ::      Options                  -- ^ The command line options that were used when this FSpec was compiled  by Ampersand.
                   , fspos ::        [Origin]                 -- ^ The origin of the FSpec. An FSpec can be a merge of a file including other files c.q. a list of Origin.
                   , themes ::       [String]                 -- ^ The names of patterns/processes to be printed in the functional design document. (for making partial documentation)
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
                   , fDeriveProofs :: Blocks                  -- ^ The proofs in Pandoc format
                   , fRoleRels ::    [(Role,Declaration)]     -- ^ the relation saying which roles may change the population of which relation.
                   , fRoleRuls ::    [(Role,Rule)]            -- ^ the relation saying which roles maintain which rules.
                   , fMaintains ::   Role -> [Rule]
                   , fRoles ::       [(Role,Int)]             -- ^ All roles mentioned in this context, numbered.
                   , fallRules ::    [Rule]
                   , vrules ::       [Rule]                   -- ^ All user defined rules that apply in the entire FSpec
                   , grules ::       [Rule]                   -- ^ All rules that are generated: multiplicity rules and identity rules
                   , invariants ::   [Rule]                   -- ^ All invariant rules
                   , signals ::      [Rule]                   -- ^ All signal rules
                   , allUsedDecls :: [Declaration]            -- ^ All relations that are used in the fSpec
                   , vrels ::        [Declaration]            -- ^ All user defined and generated relations plus all defined and computed totals.
                                                              --   The generated relations are all generalizations and
                                                              --   one declaration for each signal.
                   , allConcepts ::  [A_Concept]              -- ^ All concepts in the fSpec
                   , cptTType :: A_Concept -> TType 
                   , vIndices ::     [IdentityDef]            -- ^ All keys that apply in the entire FSpec
                   , vviews ::       [ViewDef]                -- ^ All views that apply in the entire FSpec
                   , getDefaultViewForConcept :: A_Concept -> Maybe ViewDef
                   , getAllViewsForConcept :: A_Concept -> [ViewDef]
                   , lookupView :: String -> ViewDef          -- ^ Lookup view by id in fSpec.
                   , vgens ::        [A_Gen]                  -- ^ All gens that apply in the entire FSpec
                   , allConjuncts :: [Conjunct]               -- ^ All conjuncts generated (by ADL2FSpec)
                   , allConjsPerRule :: [(Rule,[Conjunct])]   -- ^ Maps each rule onto the conjuncts it consists of (note that a single conjunct may be part of several rules) 
                   , allConjsPerDecl :: [(Declaration, [Conjunct])]   -- ^ Maps each declaration to the conjuncts it appears in   
                   , allConjsPerConcept :: [(A_Concept, [Conjunct])]  -- ^ Maps each concept to the conjuncts it appears in (as source or target of a constituent relation)
                   , vquads ::       [Quad]                   -- ^ All quads generated (by ADL2FSpec)
                   , fsisa ::        [(A_Concept, A_Concept)] -- ^ generated: The data structure containing the generalization structure of concepts
                   , vpatterns ::    [Pattern]                -- ^ All patterns taken from the Ampersand script
                   , conceptDefs ::  [ConceptDef]             -- ^ All concept definitions defined throughout a context, including those inside patterns and processes
                   , fSexpls ::      [Purpose]                -- ^ All purposes that have been declared at the top level of the current specification, but not in the processes, patterns and interfaces.
                   , metas ::        [Meta]                   -- ^ All meta relations from the entire context
                   , crudInfo ::     CrudInfo                 -- ^ Information for CRUD matrices 
               --    , popsOfCptWithoutSmaller :: A_Concept -> [Population]  -- ^ All user defined populations of an A_concept, WITHOUT the populations of smaller A_Concepts
                   , atomsInCptIncludingSmaller :: A_Concept -> [AAtomValue] -- ^ All user defined populations of an A_concept, INCLUDING the populations of smaller A_Concepts
                   , atomsBySmallestConcept :: A_Concept -> [AAtomValue] -- ^ All user defined populations of an A_Concept, where a population is NOT listed iff it also is in a smaller A_Concept.
                   , tableContents :: PlugSQL -> [[Maybe AAtomValue]] -- ^ tableContents is meant to compute the contents of an entity table.
                                                                      --   It yields a list of records. Values in the records may be absent, which is why Maybe is used rather than String.
                                                                      -- SJ 2016-05-06: Why is that? `tableContents` should represent a set of atoms, so `Maybe` should have no part in this. Why is Maybe necessary?
                                                                      -- HJO 2016-09-05: Answer: Broad tables may contain rows where some of the attributes implement a relation that is UNI, but not TOT. In such case,
                                                                      --                         we may see empty attributes. (NULL values in database terminology)
                   , pairsInExpr :: Expression -> [AAtomPair]   
                   , initialConjunctSignals :: [(Conjunct,[AAtomPair])] -- ^ All conjuncts that have process-rule violations.
                   , allViolations ::  [(Rule,[AAtomPair])]   -- ^ All invariant rules with violations.
                   , allExprs ::     [Expression]             -- ^ All expressions in the fSpec
                   , fcontextInfo   :: ContextInfo 
                   , ftypologies   :: [Typology]
                   , typologyOf :: A_Concept -> Typology
                   , largestConcept :: A_Concept -> A_Concept
                   , specializationsOf :: A_Concept -> [A_Concept]    
                   , generalizationsOf :: A_Concept -> [A_Concept]
                   } deriving Typeable
instance Eq FSpec where
 f == f' = name f == name f'
instance Unique FSpec where
 showUnique = name
metaValues :: String -> FSpec -> [String]
metaValues key fSpec = [mtVal m | m <-metas fSpec, mtName m == key]

instance Language FSpec where
  relsDefdIn = relsDefdIn.originalContext
  udefrules  = udefrules.originalContext
  identities = identities.originalContext
  viewDefs   = viewDefs.originalContext
  gens       = gens.originalContext
  patterns   = patterns.originalContext

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
  concs         = allConcepts
  expressionsIn = allExprs 


--type Attributes = [Attribute]
--data Attribute  = Attr { fld_name :: String        -- The name of this attribute
--                       , fld_sub :: Attributes        -- all sub-attributes
--                       , fld_expr :: Expression    -- The expression by which this attribute is attached to the interface
--                       , fld_rel :: Relation      -- The relation to which the database table is attached.
--                       , fld_editable :: Bool          -- can this attribute be changed by the user of this interface?
--                       , fld_list :: Bool          -- can there be multiple values in this attribute?
--                       , fld_must :: Bool          -- is this attribute obligatory?
--                       , fld_new :: Bool          -- can new elements be filled in? (if no, only existing elements can be selected)
--                       , fld_sLevel :: Int           -- The (recursive) depth of the current servlet wrt the entire interface. This is used for documentation.
--                       , fld_insAble :: Bool          -- can the user insert in this attribute?
--                       , fld_onIns :: ECArule       -- the PAclause to be executed after an insert on this attribute
--                       , fld_delAble :: Bool          -- can the user delete this attribute?
--                       , fld_onDel :: ECArule       -- the PAclause to be executed after a delete on this attribute
--                       }

{- from http://www.w3.org/TR/wsdl20/#InterfaceOperation
 - "The properties of the Interface Operation component are as follows:
 - ...
 - * {interface message references} OPTIONAL. A set of Interface Message Reference components for the ordinary messages the operation accepts or sends.
 - ..."
-}

data FSid = FS_id String     -- Identifiers in Ampersand contain strings that do not contain any spaces.
        --  | NoName           -- some identified objects have no name...
instance Named FSpec where
  name = unpack . fsName

instance Named FSid where
  name (FS_id nm) = nm



data Quad = Quad { qDcl ::       Declaration   -- The relation that, when affected, triggers a restore action.
                 , qRule ::      Rule          -- The rule from which qConjuncts is derived.
                 , qConjuncts :: [Conjunct]    -- The conjuncts, with clauses included
                 } deriving Show

instance Ord Quad where
  q `compare` q'  = (qDcl q,qRule q) `compare` (qDcl q',qRule q')
instance Eq Quad where q == q' = compare q q' == EQ

--
dnf2expr :: DnfClause -> Expression
dnf2expr dnf
 = case (antcs dnf, conss dnf) of
    ([],[]) -> fatal 327 "empty dnf clause"
    ([],cs ) -> foldr1 (.\/.) cs
    (as,[]) -> notCpl (foldr1 (./\.) as)
    (as,cs) -> notCpl (foldr1 (./\.) as) .\/. foldr1 (.\/.) cs

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
  concs     p = concs   (plugAttributes p)
  expressionsIn   p = expressionsIn (plugAttributes p)

data PlugSQL
   -- | stores a related collection of relations: a kernel of concepts and attribute relations of this kernel
   --   i.e. a list of SqlAttribute given some A -> [target r | r::A*B,isUni r,isTot r, isInj r]
   --                                            ++ [target r | r::A*B,isUni r, not(isTot r), not(isSur r)]
   --     kernel = A closure of concepts A,B for which there exists a r::A->B[INJ]
   --              (r=attExpr of kernel attribute holding instances of B, in practice r is I or a makeRelation(flipped declaration))
   --      attribute relations = All concepts B, A in kernel for which there exists a r::A*B[UNI] and r not TOT and SUR
   --              (r=attExpr of attMor attribute, in practice r is a makeRelation(declaration))
 = TblSQL  { sqlname ::    String
           , attributes :: [SqlAttribute]                           -- ^ the first attribute is the concept table of the most general concept (e.g. Person)
                                                                    --   then follow concept tables of specializations. Together with the first attribute this is called the "kernel"
                                                                    --   the remaining attributes represent attributes.
           , cLkpTbl ::    [(A_Concept,SqlAttribute)]               -- ^ lookup table that links all typology concepts to attributes in the plug
                                                                    -- cLkpTbl is een lijst concepten die in deze plug opgeslagen zitten, en hoe je ze eruit kunt halen
           , dLkpTbl ::   [RelStore]
           }
   -- | stores one relation r in two ordered columns
   --   i.e. a tuple of SqlAttribute -> (source r,target r) with (attExpr=I/\r;r~, attExpr=r)
   --   (note: if r TOT then (I/\r;r~ = I). Thus, the concept (source r) is stored in this plug too)
   --   with tblcontents = [[Just x,Just y] |(x,y)<-contents r].
   --   Typical for BinSQL is that it has exactly two columns that are not unique and may not contain NULL values
 | BinSQL  { sqlname :: String
           , cLkpTbl :: [(A_Concept,SqlAttribute)] 
           , dLkpTbl :: [RelStore]
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

plugAttributes :: PlugSQL->[SqlAttribute]
plugAttributes plug = case plug of
    TblSQL{}    -> attributes plug
    BinSQL{}    -> let store = case dLkpTbl plug of
                         [x] -> x
                         _   -> fatal 292 $ "Declaration lookup table of a binary table should contain exactly one element:\n" ++
                                            show (dLkpTbl plug)
                   in [rsSrcAtt store,rsTrgAtt store]

-- | This returns all column/table pairs that serve as a concept table for cpt. When adding/removing atoms, all of these
-- columns need to be updated
lookupCpt :: FSpec -> A_Concept -> [(PlugSQL,SqlAttribute)]
lookupCpt fSpec cpt = [(plug,att) |InternalPlug plug@TblSQL{}<-plugInfos fSpec, (c,att)<-cLkpTbl plug,c==cpt]++
                      [(plug,att) |InternalPlug plug@BinSQL{}<-plugInfos fSpec, (c,att)<-cLkpTbl plug,c==cpt]

-- Convenience function that returns the name of the table that contains the concept table (or more accurately concept column) for c
getConceptTableFor :: FSpec -> A_Concept -> PlugSQL
getConceptTableFor fSpec c = case lookupCpt fSpec c of
                               []      -> fatal 297 $ "tableFor: No concept table for " ++ name c
                               (t,_):_ -> t -- in case there are more, we use the first one

-- | Information about the source and target attributes of a relation in an sqlTable. The relation could be stored either flipped or not.  
data RelStore 
  = RelStore
     { rsDcl       :: Declaration
     , rsSrcAtt    :: SqlAttribute
     , rsTrgAtt    :: SqlAttribute
     } deriving (Show, Typeable)
data SqlAttributeUsage = PrimaryKey A_Concept
                       | ForeignKey A_Concept  -- The SQL-attribute is a reference (containing the primary key value of) a TblSQL
                       | PlainAttr             -- None of the above
                       deriving (Eq, Show)

data SqlAttribute = Att { attName :: String
                        , attExpr :: Expression     -- ^ De target van de expressie geeft de waarden weer in de SQL-tabel-kolom.
                        , attType :: TType
                        , attUse ::  SqlAttributeUsage
                        , attNull :: Bool           -- ^ True if there can be NULL-values in the SQL-attribute (intended for data dictionary of DB-implementation)
                        , attDBNull :: Bool       -- True for all fields, to disable strict checking by the database itself. 
                        , attUniq :: Bool           -- ^ True if all values in the SQL-attribute are unique? (intended for data dictionary of DB-implementation)
                        , attFlipped :: Bool
                        } deriving (Eq, Show,Typeable)
instance Named SqlAttribute where
  name = attName
instance Unique (PlugSQL,SqlAttribute) where
  showUnique (p,f) = showUnique p++"."++attName f
instance Ord SqlAttribute where
  compare x y = compare (attName x) (attName y)
instance ConceptStructure SqlAttribute where
  concs     f = [target e' |let e'=attExpr f,isSur e']
  expressionsIn   f = expressionsIn   (attExpr f)

isPrimaryKey :: SqlAttribute -> Bool
isPrimaryKey att = case attUse att of
                    PrimaryKey _ -> True
                    _ -> False
isForeignKey :: SqlAttribute -> Bool
isForeignKey att = case attUse att of
                    ForeignKey _ -> True
                    _ -> False

showSQL :: TType -> String
showSQL tt =
  case tt of 
     Alphanumeric     -> "VARCHAR(255)"
     BigAlphanumeric  -> "TEXT"
     HugeAlphanumeric -> "MEDIUMTEXT"
     Password         -> "VARCHAR(255)"
     Binary           -> "BLOB"
     BigBinary        -> "MEDIUMBLOB"
     HugeBinary       -> "LONGBLOB"
     Date             -> "DATE"
     DateTime         -> "DATETIME"
     Boolean          -> "BOOLEAN"
     Integer          -> "BIGINT"
     Float            -> "FLOAT"
     Object           -> "VARCHAR(255)"
     TypeOfOne        -> fatal 461 "ONE is not represented in SQL" 

