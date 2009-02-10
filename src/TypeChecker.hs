--AGtry doet verschillende dingen
--hij bouwt bijvoorbeeld de ISA op en de wereld en leidt regels af uit de hierarchy
--als ik de typechecker eerst uitvoer en als er geen fouten zijn de AGtry, dan gaat die functionaliteit nooit verloren
--de typechecker zal alleen bepaalde fouten eerder afvangen.
module TypeChecker (typecheck) where

   import Adl
   import Classification

   type Errors = [String]
   type RelationTree = Classification Concept
   type Environment = (Ancestors, RelationTree)

   ---------------------------------------------------------------------------------------------
   --MAIN function
   ---------------------------------------------------------------------------------------------

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

   ---------------------------------------------------------------------------------------------
   --Context part: later in separate module
   ---------------------------------------------------------------------------------------------

   type ContextCheckResult = (Environment, Errors)
   data ContextFound = Found Context | NotFound String

   checkCtx :: Architecture -> Contexts -> Errors
   checkCtx _ [] = []
   --Take the errors found when checking this context as root context and concat it with the errors of the other contexts taken as root
   checkCtx arch@(Arch ctxs) (cx@(Ctx nm _ _ _ _ _ _ _ _ _ _):tl_ctxs) = errors (check (Found cx)) ++ (checkCtx arch tl_ctxs)
      where
         check :: ContextFound -> ContextCheckResult
         check (Found cx@(Ctx _ xts _ _ ((Pat _ _ gens@((G _ g _):_) _ _ _):_) _ _ _ _ _ _))
                     = constructEnv checkExtCtx cx
                     where
                         --get the list of extended Context
                         --check all extended Context in the list
                         --merge the results (classification, relations, errors)
                         checkExtCtx :: ContextCheckResult
                         checkExtCtx = foldr mergeRes (([], Bottom),[]) (map check (map (srchContext ctxs) xts))
                         --Enrich the Environment of the extended contexts with patterns from the current context
                         constructEnv :: ContextCheckResult -> Context -> ContextCheckResult
                         constructEnv extRes cx =
                                   extRes --TODO
                                   --((Bottom, Bottom),[show (flatAncList gens)]) --for debugging to show flatAncList
                                   --(([], Bottom),[show (ancestors (oneAnc (flatAncList gens) g) )])
         check (NotFound str) = (([],Bottom),("Extended context " ++ str ++ " of context " ++ nm ++ " could not be found"):[]) --this case will not have been caught by the parser yet
         errors :: ContextCheckResult -> Errors
         errors ((_,_),e) = e

   mergeRes :: ContextCheckResult -> ContextCheckResult -> ContextCheckResult
   mergeRes ((cl1,rel1),errs1) ((cl2,rel2),errs2) | errs1==[] && errs2==[]
                                                              = ((mergeCl cl1 cl2, mergeRel rel1 rel2),[])
                                                  | otherwise = (([],Bottom),errs1 ++ errs2)
   --merge the classification of concepts trees
   mergeCl :: Ancestors -> Ancestors -> Ancestors
   mergeCl _ _ = []       --TODO
   --merge the relations trees
   mergeRel :: RelationTree -> RelationTree -> RelationTree
   mergeRel _ _ = Bottom      --TODO

     {-
   hierarchy :: ContextCheckResult -> Hierarchy
   hierarchy    ((h,_),_) = h
   relationTree :: ContextCheckResult -> RelationTree
   relationTree ((_,r),_) = r
   -}

   --search for a context by name and return the first one found
   srchContext :: Contexts -> String -> ContextFound
   srchContext [] srchstr = NotFound srchstr   --The UU_Parser has already caught this case
   srchContext (cx@(Ctx nm _ _ _ _ _ _ _ _ _ _):ctxs) srchstr
            | nm==srchstr = Found cx
            | otherwise = srchContext ctxs srchstr
            
            
            
   ---------------------------------------------------------------------------------------------
   --Expression part: later in separate module
   ---------------------------------------------------------------------------------------------

   type ADLType = String

   --needs the rules, relations, and concepts from the patterns in scope
   typeof :: Environment -> Expression -> [ADLType]
   typeof _ _ = []

   ---------------------------------------------------------------------------------------------
   --Ancestors part: later in separate module
   --flatAncList is the interesting function which returns the model and the tracklist to
   --correlate to the ADL code. flatAncList is build in two phases. First the actual code
   --declarations are enumerated (declancs). Then the ancestors are resolved recursively, and folded.
   --Each concept is resolved once to prevent recursive loops. Therefore a list is passed around
   --to keep track of the resolved concepts.
   --
   --oneAnc can be used to get the Ancestor entry for a Concept by Concept.
   --ancestors can be used to get the ancestors from an Ancestor entry
   --
   --Maybe I can arrange and label the functions more clearly. 
   --target = ancestor,
   --source = child,
   --both target and source are Concepts.
   --
   --Ancestor is a kind of nonlinear intuitionistic implication.
   --if Boss -> Person. If Boss is true then Person is true.
   --Both Boss and Person are free resources.
   --Ancestor is also an Identity, If Boss then Boss
   ---------------------------------------------------------------------------------------------

   type Ancestors = [Ancestor]

   -- (target, tracklist of gen declarations in ADL code)
   type AncTarget = (Concept,[Gen])

   data Ancestor = Anc (Concept       , [AncTarget]) deriving (Show)
                    --(child / source , list of AncTarget / ancestors)

   --triple to pass around progress ([Concept]), intermediate result ([AncTarget]), 
   --and progress original input (Ancestors)
   --to get all ancestors of a Concept and prevent looping
   type AllAncResult = ([Concept],[AncTarget],Ancestors)

   --equality of Ancestor = equality of its source Concept = equality of the name of the source Concept
   instance Eq Ancestor where
     (Anc (src,_))==(Anc (src',_)) = src==src'

   ------------------
   --PUBLIC FUNCTIONS
   ------------------

   --returns the model and the tracklist to correlate to the ADL code.
   --flatAncList is build in two phases. First the actual code
   --declarations are enumerated (declAncs). Then the ancestors are resolved recursively, and folded (foldTrgs).
   flatAncList :: Gens -> Ancestors
   flatAncList gens = foldTrgs (declAncs gens) (declAncs gens)
         where    
         --foreach Ancestor entry in declAncs, fold distinct the targets of its targets
         --provide the Ancestors declared in the code, and the list of Ancestors to resolve
         foldTrgs :: Ancestors -> Ancestors -> Ancestors
         foldTrgs _ [] = []
         foldTrgs declancs ((Anc (src,trgs)):das) = (Anc(src,allAncsTrgs declancs trgs)):(foldTrgs declancs das)
         --Ancestor relations which are declared in ADL
         declAncs :: Gens -> Ancestors
         declAncs gens = foldr insertGen [] gens

   --get the Ancestor entry from an Ancestor model by Concept 
   --(equality by concept name)
   oneAnc :: Ancestors -> Concept -> Ancestor
   oneAnc [] src       =
                         --apparantly this Concept has no ancestors, return the concept without ancestors
                         Anc (src,[])
   oneAnc (a@(Anc(src',_)):ancs) src
         | src' == src = 
                         --found -> return
                         a
         | otherwise   = 
                         --try next
                         oneAnc ancs src

   --get the ancestors of a Ancestor entry
   ancestors :: Ancestor -> [AncTarget]
   ancestors (Anc (_,trgs)) = trgs

   -------------------
   --PRIVATE FUNCTIONS
   -------------------

   --return all AncTargets of an AncTarget, 
   --respecting the already resolved Concepts and results,
   --given the explicit declarations from the ADL code
   allAncsTrg :: AncTarget -> AllAncResult -> AllAncResult
   allAncsTrg  at@(trg,_) (lst,ancs,declancs)
         | elem trg lst = 
                          --skip, already resolved: so just forward result so far
                          (lst,ancs,declancs)

         | otherwise    =
                          --add this target (at:ancs),
                          --and all its ancestors (foldr ... oneAnc declancs trg), as ancestor,
                          --register this target as resolved (trg:lst)
                          foldr allAncsTrg (trg:lst,at:ancs,declancs) (ancestors (oneAnc declancs trg))

   --return all the AncTarget for a list of AncTarget, 
   --given the explicit declaration from the ADL code
   allAncsTrgs :: Ancestors -> [AncTarget] -> [AncTarget]
   allAncsTrgs declancs trgs = allAncRes (foldr allAncsTrg ([],[],declancs) trgs)
       where   allAncRes :: AllAncResult -> [AncTarget]
               allAncRes (_,ancs,_) = ancs

   --insert an explicit ADL code declaration (Gen) to the list
   insertGen :: Gen -> Ancestors -> Ancestors
   --if there is no Ancestor entry yet, add a new entry
   insertGen gen@(G _ src trg) [] = ( Anc (src, [(trg,[gen])] ) ):[]
   --if there are entries, search for an entry of the child / source of the Gen
   insertGen gen@(G _ src _) (a@(Anc(src',trgs)):as)
          | src' == src =
                          --if the child is located, add the ancestor to the list of ancestors of this child
                          (Anc(src, insertGenTrg trgs gen)):as --update  (insert target)
          | otherwise   = 
                          --child not located yet, try next and preserve all entries (a:)
                          a:(insertGen gen as)

   --insert a new target/ancestor to the ancestor list.
   --if the ancestor already exists, add track information to the ancestor (declared twice)
   insertGenTrg :: [AncTarget] -> Gen -> [AncTarget]
   insertGenTrg [] gen@(G _ _ trg) = (trg,[gen]):[] --insert
   insertGenTrg (t@(trg',gens):trgs) gen@(G _ _ trg)
          | trg' == trg = (trg,gen:gens):trgs --update (tracklist)
          | otherwise   = t:(insertGenTrg trgs gen) --try next



   ---------------------------------------------------------------------------------------------
   --MORE COMMENTS
   ---------------------------------------------------------------------------------------------

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





