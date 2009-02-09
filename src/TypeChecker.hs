--AGtry doet verschillende dingen
--hij bouwt bijvoorbeeld de ISA op en de wereld en leidt regels af uit de hierarchy
--als ik de typechecker eerst uitvoer en als er geen fouten zijn de AGtry, dan gaat die functionaliteit nooit verloren
--de typechecker zal alleen bepaalde fouten eerder afvangen.
module TypeChecker (typecheck) where

   import Adl
   import Classification

   type Errors = [String]
   type Hierarchy = Classification Concept
   type RelationTree = Classification Concept
   type Environment = (Hierarchy, RelationTree)

   typecheck :: Architecture -> Errors
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

   type ContextCheckResult = (Environment, Errors)
   checkCtx :: Architecture -> Contexts -> Errors
   checkCtx _ [] = []
   --Take the errors found when checking this context as root context and concat it with the errors of the other contexts taken as root
   checkCtx arch@(Arch ctxs) (cx@(Ctx nm _ _ _ _ _ _ _ _ _ _):tl_ctxs) = errors (check (Found cx)) ++ (checkCtx arch tl_ctxs)
      where
         check :: ContextFound -> ContextCheckResult
         check (Found cx@(Ctx _ xts _ _ _ _ _ _ _ _ _))
                     = constructEnv checkExtCtx cx
                     where
                         --get the list of extended Context
                         --check all extended Context in the list
                         --merge the results (classification, relations, errors)
                         checkExtCtx :: ContextCheckResult
                         checkExtCtx = foldr mergeRes ((Bottom, Bottom),[]) (map check (map (srchContext ctxs) xts))
                         --Enrich the Environment of the extended contexts with patterns from the current context
                         constructEnv :: ContextCheckResult -> Context -> ContextCheckResult
                         constructEnv extRes cx = extRes --TODO
         check (NotFound str) = ((Bottom,Bottom),("Extended context " ++ str ++ " of context " ++ nm ++ " could not be found"):[]) --this case will not have been caught by the parser yet
         errors :: ContextCheckResult -> Errors
         errors ((_,_),e) = e

   mergeRes :: ContextCheckResult -> ContextCheckResult -> ContextCheckResult
   mergeRes ((cl1,rel1),errs1) ((cl2,rel2),errs2) | errs1==[] && errs2==[]
                                                              = ((mergeCl cl1 cl2, mergeRel rel1 rel2),[])
                                                  | otherwise = ((Bottom,Bottom),errs1 ++ errs2)   
   --merge the classification of concepts trees
   mergeCl :: Hierarchy -> Hierarchy -> Hierarchy
   mergeCl _ _ = Bottom       --TODO
   --merge the relations trees
   mergeRel :: RelationTree -> RelationTree -> RelationTree
   mergeRel _ _ = Bottom      --TODO

     {-
   hierarchy :: ContextCheckResult -> Hierarchy
   hierarchy    ((h,_),_) = h
   relationTree :: ContextCheckResult -> RelationTree
   relationTree ((_,r),_) = r
   -}

   data ContextFound = Found Context | NotFound String
   srchContext :: Contexts -> String -> ContextFound
   srchContext [] srchstr = NotFound srchstr   --The UU_Parser has already caught this case
   srchContext (cx@(Ctx nm _ _ _ _ _ _ _ _ _ _):ctxs) srchstr
            | nm==srchstr = Found cx
            | otherwise = srchContext ctxs srchstr
            
            
            


   type ADLType = String

   --needs the rules, relations, and concepts from the patterns in scope
   typeof :: Environment -> Expression -> [ADLType]
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


