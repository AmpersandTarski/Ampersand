{-# OPTIONS_GHC -Wall #-}
module Adl.Context (Context(..),Contexts
                   ,Architecture(..))
where
   import Adl.Concept                 (Concept)
   import Adl.Pattern                 (Patterns)
   import Adl.Rule                    (Rules)
   import Adl.MorphismAndDeclaration  (Declarations)
   import Adl.KeyDef                  (KeyDefs)
   import Adl.ObjectDef               (ObjectDefs)
   import Adl.Population              (Populations)
   import Adl.ConceptDef              (ConceptDefs)
   import Typology                    (Inheritance)
   import Classification              (Classification)
   import CommonClasses               (Identified(..))


   -- | Architecture of ADL consists of a set of contexts
   data Architecture = Arch { archContexts :: Contexts}

   type Contexts  = [Context]
   data Context
      = Ctx { ctxnm    :: String                    -- ^ The name of this context
            , ctxon    :: [String]                  -- ^ The list of extends (= context names of contexts) whose rules are imported
            , ctxisa   :: (Inheritance Concept)     -- ^ A data structure containing the generalization structure of concepts
            , ctxwrld  :: [Classification Context]  -- ^ A tree, being the transitive closure of the 'extends' (see formal definition) relation.
            , ctxpats  :: Patterns                  -- ^ A list of patterns defined in this context
            , ctxrs    :: Rules                     -- ^ A list of all rules that are valid within this context
            , ctxds    :: Declarations              -- ^ A list of declarations defined in this context, outside the scope of patterns
            , ctxcs    :: ConceptDefs               -- ^ A list of concept definitions defined in this context, outside the scope of patterns
            , ctxks    :: KeyDefs                   -- ^ A list of key definitions defined in this context, outside the scope of patterns
            , ctxos    :: ObjectDefs                -- ^ A list of attributes defined in this context, outside the scope of patterns
            , ctxpops  :: Populations               -- ^ A list of populations defined in this context
            , ctxsql   :: ObjectDefs  --a list of sqlplugs
            , ctxphp   :: ObjectDefs  --a list of phpplugs
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
    typ _ = "Context_"


