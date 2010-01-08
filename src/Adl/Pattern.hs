{-# OPTIONS_GHC -Wall #-}
module Adl.Pattern (Pattern(..),Patterns
                   ,union)
where
   import Adl.KeyDef                   (KeyDefs)
   import Adl.Rule                     (Rules)
   import Adl.Gen                      (Gens)
   import Adl.MorphismAndDeclaration   (Declarations)
   import Adl.ConceptDef               (ConceptDefs)
   import CommonClasses                (Identified(..))
   import Collection                   (Collection(..))
   
   type Patterns  = [Pattern]
   data Pattern
      = Pat { ptnm  :: String       -- ^ Name of this pattern
            , ptrls :: Rules        -- ^ List of rules declared in this pattern
            , ptgns :: Gens         -- ^ List of generalizations defined in this pattern
            , ptdcs :: Declarations -- ^ List of declarations declared in this pattern
            , ptcds :: ConceptDefs  -- ^ list of concept definitions defined in this pattern
            , ptkds :: KeyDefs      -- ^ list of key definitions defined in this pattern
            }   --deriving (Show) -- voor debugging

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Pattern                       ***
{- \**** (Eq dient alleen diagnostische doeleinden)    ********************

   instance Eq Pattern where
    p==p' = ptnm     p == ptnm  p'
       --   && ptrls p == ptrls p'
            && ptgns p == ptgns p'
       --   && ptdcs p == ptdcs p'
            && ptcds p == ptcds p'
            && ptkds p == ptkds p'
-}

--   instance Show Pattern
   instance Identified Pattern where
    name pat = ptnm pat

   union :: Pattern -> Pattern -> Pattern
   union pat pat'
     = Pat (ptnm pat')
           (ptrls pat `uni` ptrls pat')
           (ptgns pat `uni` ptgns pat')
           (ptdcs pat `uni` ptdcs pat')
           (ptcds pat `uni` ptcds pat')
           (ptkds pat `uni` ptkds pat')


            