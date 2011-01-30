{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.ADL1.Context (Context(..),Contexts
                   ,RoleService(..),RoleRelation(..)
                   ,Architecture(..))
-- This module exports datatype 'Context', which represents a single context as it emerges after type checking.
-- This data structure is as accurate a representation of the Ampersand script as possible, with minimal redundancy.
-- It is type correct, because it is not generated unless the type checker is fully satisfied with the user's script.
where
   import DatabaseDesign.Ampersand.ADL1.Concept                 (Concept)
   import DatabaseDesign.Ampersand.ADL1.Pattern                 (Patterns)
   import DatabaseDesign.Ampersand.ADL1.Rule                    (Rules)
   import DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration  (Relation,Declaration,Identified(..))
   import DatabaseDesign.Ampersand.ADL1.KeyDef                  (KeyDefs)
   import DatabaseDesign.Ampersand.ADL1.ObjectDef               (ObjectDefs,Service)
   import DatabaseDesign.Ampersand.ADL1.Explanation             (PExplanations)
   import DatabaseDesign.Ampersand.ADL1.FilePos                 (FilePos(..))
   import DatabaseDesign.Ampersand.ADL1.Population              (Populations)
   import DatabaseDesign.Ampersand.ADL1.ConceptDef              (ConceptDefs)
   import DatabaseDesign.Ampersand.ADL1.Expression              (Expression)
   import DatabaseDesign.Ampersand.Core.Basics                 (Inheritance)
   import DatabaseDesign.Ampersand.Core.Basics                 (Classification)


   -- | Architecture of Ampersand consists of a set of contexts
   data Architecture = Arch { archContexts :: Contexts}

   type Contexts  = [Context]
   data Context
      = Ctx { ctxnm    :: String                    -- ^ The name of this context
            , ctxon    :: [String]                  -- ^ The list of extends (= context names of contexts) whose rules are imported
            , ctxisa   :: Inheritance Concept   -- ^ A data structure containing the generalization structure of concepts
            , ctxwrld  :: [Classification Context]  -- ^ A tree, being the transitive closure of the 'extends' (see formal definition) relation.
            , ctxpats  :: Patterns                  -- ^ The patterns defined in this context
            , ctxrs    :: Rules (Relation Concept) -- ^ All user defined rules in this context, but outside patterns
            , ctxds    :: [Declaration Concept]    -- ^ The declarations defined in this context, outside the scope of patterns
            , ctxcs    :: ConceptDefs               -- ^ The concept definitions defined in this context, outside the scope of patterns
            , ctxks    :: KeyDefs                   -- ^ The key definitions defined in this context, outside the scope of patterns
            , ctxsvcs  :: [Service]                 -- ^ The services defined in this context, outside the scope of patterns
            , ctxps    :: PExplanations             -- ^ The pre-explanations defined in this context, outside the scope of patterns
            , ctxros   :: [RoleService]             -- ^ The assignment of roles to ObjectDefs (also called role service assignments).
                                                    -- ^ If r is an element of rsRoles (ctxros ctx) and s is an element of rsServices (ctxros ctx), then role r may use service s.
            , ctxmed   :: [RoleRelation]            -- ^ The assignment of roles to ObjectDefs.
                                                    -- ^ If r is an element of rrRoles (ctxmed ctx) and p is an element of rrRels (ctxmed ctx), then role r may edit relation p.
            , ctxpops  :: Populations Concept       -- ^ The populations defined in this context
            , ctxsql   :: ObjectDefs                -- ^ user defined sqlplugs, taken from the Ampersand script
            , ctxphp   :: ObjectDefs                -- ^ user defined phpplugs, taken from the Ampersand script
            , ctxenv   :: (Expression (Relation Concept),[(Declaration Concept,String)]) -- ^ an expression on the context with unbound relations, to be bound in this environment
            }               --deriving (Show) -- voor debugging

-- A RoleService rs means that any role in 'rsRoles rs' may use any ObjectDef mentioned in 'rsServices rs'
   data RoleService     = RS { rsRoles    :: [String]       -- ^ name of a role
                             , rsServices :: [String]       -- ^ name of an ObjectDef
                             , rsPos      :: FilePos        -- ^ position in the Ampersand script
                             } deriving (Eq, Show)      -- ^ just for debugging

-- A RoleRelation rs means that any role in 'rrRoles rs' may edit any Relation  in  'rrServices rs'
   data RoleRelation    = RR { rrRoles :: [String]       -- ^ name of a role
                             , rrRels  :: [Relation Concept]     -- ^ name of a Relation
                             , rrPos   :: FilePos        -- ^ position in the Ampersand script
                             } deriving (Eq, Show)      -- ^ just for debugging

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Context                       ***
{- \**** (Eq dient alleen diagnostische doeleinden)    ********************

   instance Eq Context where
    c==c' = ctxnm      c == ctxnm   c'
            && ctxon   c == ctxon   c'
            && ctxisa  c == ctxisa  c'
            && ctxwrld c == ctxwrld c'
            && ctxpats c == ctxpats c'
            && ctxrs   c == ctxrs   c'
            && ctxds   c == ctxds   c'
            && ctxcs   c == ctxcs   c'
            && ctxks   c == ctxks   c'
            && ctxsvcs c == ctxsvcs c'
            && ctxros  c == ctxros  c'
            && ctxmed  c == ctxmed  c'
            && ctxser  c == ctxser  c'
      --    && ctxpops c == ctxpops c'
-}


   instance Show Context where
     showsPrec _ ctx = showString (ctxnm ctx)

   instance Eq Context where
     (==) c1 c2 = name c1 == name c2

   instance Identified Context where
    name ctx = ctxnm ctx


