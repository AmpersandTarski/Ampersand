--AGtry doet verschillende dingen
--hij bouwt bijvoorbeeld de ISA op en de wereld en leidt regels af uit de hierarchy
--als ik de typechecker eerst uitvoer en als er geen fouten zijn de AGtry, dan gaat die functionaliteit nooit verloren
--de typechecker zal alleen bepaalde fouten eerder afvangen.
module TypeChecker (typecheck) where

   import Adl
   import Classification

   typecheck :: Architecture -> [String]
   --typecheck _arch = iwantastring _arch  -- voor debugging
   --typecheck (Arch ctxs) = iwantastring (srchContext ctxs "Test")
   typecheck arch@(Arch ctxs) = checkCtx arch ctxs   --this list of errors is not distinct
   typecheck _ = []

   {-
   --te gebruiken om de context of een deel daarvan als string op het scherm te krijgen
   --datatypes moeten wel een implementatie voor show hebben, dat is standaard niet
   iwantastring :: Architecture -> [String]
   iwantastring (Arch ((Ctx _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11):_))
       -- = (show _x1):(show _x2):(show _x3):(show _x4):(show _x5):(show _x6):(show _x7):(show _x8):(show _x9):(show _x11):[]
       = (show _x10):[]
   -}
   {-
   iwantastring :: ContextFound -> [String]
   iwantastring (NotFound str) = ("Context " ++ str ++ " could not be found"):[]
   iwantastring (Found (Ctx _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11))
       -- = (show _x1):(show _x2):(show _x3):(show _x4):(show _x5):(show _x6):(show _x7):(show _x8):(show _x9):(show _x11):[]
       = (show _x3):[]
   -}

   type ContextCheckResult = ((Classification Concept, Classification Concept), [String])
   checkCtx :: Architecture -> Contexts -> [String]
   checkCtx _ [] = []
   --Take the errors found when checking this context as root context and concat it with the errors of the other contexts
   checkCtx arch@(Arch ctxs) (cx@(Ctx nm _ _ _ _ _ _ _ _ _ _):tl_ctxs) = snd (check (Found cx)) ++ (checkCtx arch tl_ctxs)
      where
         check :: ContextFound -> ContextCheckResult
         check (Found cx@(Ctx _ xts _ _ _ _ _ _ _ _ _))
                     -- = ("CHECK " ++ nm):[]
                     = checkExtCtx  --TODO
                     where
                         --get the list of extended Context
                         --check all extended Context in the list
                         --merge the results (classification, relations, errors)
                         checkExtCtx :: ContextCheckResult
                         checkExtCtx = foldr mergeRes ((Bottom, Bottom),[]) (map check (map (srchContext ctxs) xts))
         check (NotFound str) = ((Bottom,Bottom),("Extended context " ++ str ++ " of context " ++ nm ++ " could not be found"):[])

   mergeRes :: ContextCheckResult -> ContextCheckResult -> ContextCheckResult
   mergeRes ((_,_),errs1) ((_,_),errs2) | length errs1 > 0
                                          || length errs2 > 0 = ((Bottom,Bottom),errs1 ++ errs2)
                                        | otherwise = ((Bottom,Bottom),[])  --TODO

   --construct the classification of concepts according to extend and ISA decls and attach the
   --environment (like patterns in scope) to the concepts
   --Ik heb nog geen compleet gevoel over hoe ik dit moet implementeren
   classification :: Architecture -> Context -> Classification Concept
   classification _ _ = Bottom

   relations :: Architecture -> Context -> Classification Concept
   relations _ _ = Bottom

   data ContextFound = Found Context | NotFound String
   srchContext :: Contexts -> String -> ContextFound
   srchContext [] srchstr = NotFound srchstr   --Maybe this is impossible as a consequence of the parse method
   srchContext (cx@(Ctx nm _ _ _ _ _ _ _ _ _ _):ctxs) srchstr
            | nm==srchstr = Found cx
            | otherwise = srchContext ctxs srchstr

   --lookup the concept in the classification tree of concepts, and return the patterns in scope
   --Ik heb nog geen compleet gevoel over hoe ik dit moet implementeren
   --patterns :: Concept -> Classification Concept -> Patterns
   --patterns _ _ = []

   type ADLType = String

   --needs the rules, relations, and concepts from the patterns in scope
   typeof :: Expression -> Classification Concept -> [ADLType]
   typeof _ _ = []





{-
1)  Doe voor iedere context in Architecture. De contexten in architecture hebben geen join, misschien
    is er een efficientere manier.
2)  Beschouw recursief de componenten van de extended contexten als onderdeel van de context.
    Dit zijn alle componenten in scope.
2a) Bouw de classification van concepten op. Door het mergen van lager gelegen contexten en het 
    toevoegen van de componenten in de huidige. Nodig patterns: gens, concs, rels decls
2b) Bouw de relatieboom op. Nodig patterns: concs, rels decls
3a) Check de types van de rules uit de huidige context
3b) Check de types van de expressies uit de ObjectDefs
-}


