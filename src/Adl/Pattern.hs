{-# OPTIONS_GHC -Wall #-}
module Adl.Pattern (Pattern(..),Patterns
                   ,union)
where
   import Adl.KeyDef                   (KeyDefs)
   import Adl.Rule                     (Rules)
   import Adl.Gen                      (Gens)
   import Adl.Concept                  (Sign)
   import Adl.MorphismAndDeclaration   (Declarations,Declaration,Morphism)
   import Adl.ConceptDef               (ConceptDefs)
   import Adl.Explanation              (PExplanations)
   import CommonClasses                (Identified(..))
   import Collection                   (Collection(..))
   import Adl.Expression               (PExpression,Expressionx)
   
   type Patterns  = [Pattern]
   data Pattern
      = Pat { ptnm  :: String        -- ^ Name of this pattern
            , ptrls :: Rules         -- ^ The rules declared in this pattern
            , ptgns :: Gens          -- ^ The generalizations defined in this pattern
            , ptdcs :: Declarations  -- ^ The declarations declared in this pattern
            , ptcds :: ConceptDefs   -- ^ The concept definitions defined in this pattern
            , ptkds :: KeyDefs       -- ^ The key definitions defined in this pattern
            , ptxps :: PExplanations -- ^ The explanations of elements defined in this pattern
            , testexpr :: [PExpression Morphism (Maybe Sign)]
            , inftestexpr :: [PExpression Declaration Sign]
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
           (ptxps pat  ++   ptxps pat')
           []
           []


            
