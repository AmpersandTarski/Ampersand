
module Adl.Context where
   import Adl.Concept
   import Adl.Pattern
   import Adl.Rule
   import Adl.MorphismAndDeclaration
   import Adl.KeyDef
   import Adl.ObjectDef
   import Adl.Population
   import Adl.ConceptDef
   import Typology ( Inheritance())
   import Classification ( Classification())
   import CommonClasses(Identified(name,typ))
   
   
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

   instance Identified Context where
    name ctx = ctxnm ctx
    typ ctx = "Context_"
            
            