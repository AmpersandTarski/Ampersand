{- | The intentions behind FSpec (SJ 30 dec 2008):
Generation of functional specifications is the core functionality of Ampersand.
All items in a specification are generated into the following data structure, FSpec.
It is built by compiling an Ampersand script and translating that to FSpec.
In the future, other ways of 'filling' FSpec are foreseen.
All generators (such as the code generator, the proof generator, the atlas generator, etc.)
are merely different ways to show FSpec.
-}
module Database.Design.Ampersand.FSpec.FSpec
          ( FSpec(..), concDefs, Atom(..)
          , Fswitchboard(..), Quad(..)
          , FSid(..), FProcess(..)
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
          , SqlType(..)
          , SqlFieldUsage(..)
          , getGeneralizations, getSpecializations, getEditableRelation
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
fatal = fatalMsg "FSpec.FSpec"

data FSpec = FSpec { fsName ::       String                   -- ^ The name of the specification, taken from the Ampersand script
                   , getOpts ::      Options                  -- ^ The command line options that were used when this FSpec was compiled  by Ampersand.
                   , fspos ::        [Origin]                 -- ^ The origin of the FSpec. An FSpec can be a merge of a file including other files c.q. a list of Origin.
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
                   , vrules ::       [Rule]                   -- ^ All user defined rules that apply in the entire FSpec
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
                   , vIndices ::     [IdentityDef]            -- ^ All keys that apply in the entire FSpec
                   , vviews ::       [ViewDef]                -- ^ All views that apply in the entire FSpec
                   , vgens ::        [A_Gen]                  -- ^ All gens that apply in the entire FSpec
                   , vconjs ::       [Conjunct]               -- ^ All conjuncts generated (by ADL2FSpec)
                   , vquads ::       [Quad]                   -- ^ All quads generated (by ADL2FSpec)
                   , vEcas ::        [ECArule]                -- ^ All ECA rules generated (by ADL2FSpec)
                   , fsisa ::        [(A_Concept, A_Concept)] -- ^ generated: The data structure containing the generalization structure of concepts
                   , vpatterns ::    [Pattern]                -- ^ All patterns taken from the Ampersand script
                   , conceptDefs ::  [ConceptDef]             -- ^ All concept definitions defined throughout a context, including those inside patterns and processes
                   , fSexpls ::      [Purpose]                -- ^ All purposes that have been declared at the top level of the current specification, but not in the processes, patterns and interfaces.
                   , metas ::        [Meta]                   -- ^ All meta relations from the entire context
                   , initialPops ::  [Population]             -- all user defined populations of relations and concepts
                   , initialSignals :: [(Rule,[(Conjunct,[Paire])])] -- all process rules with violations per conjunct.
                   , allViolations ::  [(Rule,[Paire])]        -- all invariant rules with violations.
                   }
metaValues :: String -> FSpec -> [String]
metaValues key fSpec = [mtVal m | m <-metas fSpec, mtName m == key]

data Atom = Atom { atmRoot :: A_Concept -- The root concept of the atom. (this implies that there can only be a single root for
                 , atmVal :: String
                 } deriving Eq

concDefs :: FSpec -> A_Concept -> [ConceptDef]
concDefs fSpec c = [ cdef | cdef<-conceptDefs fSpec, name cdef==name c ]

instance ConceptStructure FSpec where
  concs     fSpec = allConcepts fSpec                     -- The set of all concepts used in this FSpec
  expressionsIn fSpec = foldr (uni) []
                        [ (expressionsIn.interfaceS) fSpec
                        , (expressionsIn.vrules) fSpec
                        , (expressionsIn.vviews) fSpec
                        , (expressionsIn.vIndices) fSpec
                        ]
  mp1Exprs  _ = fatal 77 "do not use mp1Exprs from an FSpec"

instance Language FSpec where
  objectdef    fSpec = Obj { objnm   = name fSpec
                           , objpos  = Origin "generated object by objectdef (Language FSpec)"
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
instance Identified FSpec where
  name = fsName

instance Identified FSid where
  name (FS_id nm) = nm

data Activity = Act { actRule ::   Rule
                    , actTrig ::   [Declaration]
                    , actAffect :: [Declaration]
                    , actQuads ::  [Quad]
                    , actEcas ::   [ECArule]
                    , actPurp ::   [Purpose]
                    } deriving Show

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

getGeneralizations :: FSpec -> A_Concept -> [A_Concept]
getGeneralizations fSpec = largerConcepts (gens fSpec)

getSpecializations :: FSpec -> A_Concept -> [A_Concept]
getSpecializations fSpec = smallerConcepts (gens fSpec)

-- We allow editing on basic relations (Declarations) that may have been flipped, or narrowed/widened by composing with I.
-- Basically, we have a relation that may have several epsilons to its left and its right, and the source/target concepts
-- we use are the concepts in the outermost epsilon, or the source/target concept of the relation, in absence of epsilons.
-- This is used to determine the type of the atoms provided by the outside world through interfaces.
getEditableRelation :: [Expression] -> Expression -> Maybe (A_Concept, Declaration, A_Concept, Bool)
getEditableRelation editableRels expr = case getRelation expr of
   Just (s,Just d,t,isFlipped)  -> if EDcD d `elem` editableRels then Just (s,d,t,isFlipped) else Nothing
   _                            -> Nothing
 where
    -- If the expression represents an editable relation, the relation is returned together with the narrowest possible source and target 
    -- concepts, as well as a boolean that states whether the relation is flipped. 
    getRelation :: Expression -> Maybe (A_Concept, Maybe Declaration, A_Concept, Bool)
    getRelation (ECps (e, EDcI{})) = getRelation e
    getRelation (ECps (EDcI{}, e)) = getRelation e
    getRelation (ECps (e1, e2))
      = case (getRelation e1, getRelation e2) of --note: target e1==source e2
         (Just (_,Nothing,i1,_), Just (i2,Nothing,_,_)) -> if i1==target e1 && i2==source e2 then Just (i1, Nothing, i2, False) else -- i1==i2
                                                           if i1==target e1 && i2/=source e2 then Just (i2, Nothing, i2, False) else
                                                           if i1/=target e1 && i2==source e2 then Just (i1, Nothing, i1, False) else
                                                           Nothing
         (Just (_,Nothing,i,_), Just (s,d,t,isFlipped)) -> if i==target e1                 then Just (s,d,t,isFlipped) else                       
                                                           if i/=target e1 && s==target e1 then Just (i,d,t,isFlipped) else                       
                                                           Nothing                                                     
         (Just (s,d,t,isFlipped), Just (i,Nothing,_,_)) -> if i==source e2                 then Just (s,d,t,isFlipped) else
                                                           if i/=source e2 && t==source e2 then Just (s,d,i,isFlipped) else        
                                                           Nothing                                                                 
         _                                              -> Nothing
    getRelation (EFlp e)
     = case getRelation e of
         Just (s,d,t,isFlipped) -> Just (t,d,s,not isFlipped)
         Nothing                -> Nothing
    getRelation (EDcD d)   = Just (source d, Just d, target d, False)
    getRelation (EEps i _) = Just (i, Nothing, i, False)
    getRelation _ = Nothing

