{-# OPTIONS_GHC -Wall #-}
module Adl.Context (Context(..),Contexts
                   ,Architecture(..))
-- This module exports datatype 'Context', which represents a single context as it emerges after type checking.
-- This data structure is as accurate a representation of the Ampersand script as possible, with minimal redundancy.
-- It is type correct, because it is not generated unless the type checker is fully satisfied with the user's script.
where
   import Adl.Concept                 (Concept)
   import Adl.Pattern                 (Patterns)
   import Adl.Rule                    (Rules)
   import Adl.MorphismAndDeclaration  (Declarations,Declaration)
   import Adl.KeyDef                  (KeyDefs)
   import Adl.ObjectDef               (ObjectDefs)
   import Adl.Explanation             (PExplanations)
   import Adl.Population              (Populations)
   import Adl.ConceptDef              (ConceptDefs)
   import Adl.Expression              (Expression)
   import Typology                    (Inheritance)
   import Classification              (Classification)
   import CommonClasses               (Identified(..))


   -- | Architecture of ADL consists of a set of contexts
   data Architecture = Arch { archContexts :: Contexts}

   type Contexts  = [Context]
   data Context
      = Ctx { ctxnm    :: String                    -- ^ The name of this context
            , ctxon    :: [String]                  -- ^ The list of extends (= context names of contexts) whose rules are imported
            , ctxisa   :: Inheritance Concept       -- ^ A data structure containing the generalization structure of concepts
            , ctxwrld  :: [Classification Context]  -- ^ A tree, being the transitive closure of the 'extends' (see formal definition) relation.
            , ctxpats  :: Patterns                  -- ^ The patterns defined in this context
            , ctxrs    :: Rules                     -- ^ All user defined rules in this context, but outside patterns
            , ctxds    :: Declarations              -- ^ The declarations defined in this context, outside the scope of patterns
            , ctxcs    :: ConceptDefs               -- ^ The concept definitions defined in this context, outside the scope of patterns
            , ctxks    :: KeyDefs                   -- ^ The key definitions defined in this context, outside the scope of patterns
            , ctxos    :: ObjectDefs                -- ^ The attributes defined in this context, outside the scope of patterns
            , ctxps    :: PExplanations             -- ^ The pre-explanations defined in this context, outside the scope of patterns
            , ctxpops  :: Populations               -- ^ The populations defined in this context
            , ctxsql   :: ObjectDefs  --a list of sqlplugs
            , ctxphp   :: ObjectDefs  --a list of phpplugs
            , ctxenv   :: (Expression,[(Declaration,String)]) --an expression on the context with unbound morphisms, to be bound in this environment
            }               --deriving (Show) -- voor debugging
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
            && ctxos   c == ctxos   c'
      --    && ctxpops c == ctxpops c'
-}


   instance Show Context where
     showsPrec _ ctx = showString (ctxnm ctx)

   instance Eq Context where
     (==) c1 c2 = name c1 == name c2

   instance Identified Context where
    name ctx = ctxnm ctx


