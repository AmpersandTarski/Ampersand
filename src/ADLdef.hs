  module ADLdefOud
  where
 
   import Adl
   import CommonClasses ( Identified(name)
                        , ABoolAlg(glb,lub,order)
                        , Explained(explain)
                        , Conceptual(conts)
                        , Morphics(anything)
                        )
   import Typology ( Inheritance(Isa), Typologic(typology), genEq)
   import Classification ( Classification(),preCl)
   import Collection (Collection (uni,isc,(>-),empty,rd))
   import Auxiliaries (eqClass, enumerate, sort', clos1,diag,eqCl) 





   instance Explained Concept where
    explain c = name c   
    --explain (C expla _ _) = expla
    --explain (S)           = "ONE"
    --explain NOthing       = "Nothing"
    --explain Anything      = "Anything"



--   isNothing, isAnything :: Concept -> Bool
--   isNothing  c | c == cptNothing  = True
--                | otherwise        = False
   isAnything c | c == cptAnything = True
                | otherwise        = False
   isC C{} = True
   isC c   = False



   sIs c = Isn c c


   instance Ord Expression where
    a <= b = source a <= source b && target a <= target b


   normExpr :: Rule -> Expression
   normExpr rule
    | isSignal rule      = v (sign rule)
    | ruleType rule==AlwaysExpr = consequent rule
    | ruleType rule==Implication = Fu [Cp (antecedent rule), consequent rule]
    | ruleType rule==Equivalence = Fi [ Fu [antecedent rule, Cp (consequent rule)]
                              , Fu [Cp (antecedent rule), consequent rule]]
    | otherwise          = error("Fatal (module CC_aux): Cannot make an expression of "++misbruiktShowHS "" rule)


   --Onderstaande functies zijn bedoeld voor foutmeldingen:
   misbruiktShowHS indent e = "showHS \""++indent++show e
