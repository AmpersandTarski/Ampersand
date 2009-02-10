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
         check (Found cx@(Ctx _ xts _ _ ((Pat _ _ gens _ _ _):_) _ _ _ _ _ _))
                     = constructEnv checkExtCtx cx
                     where
                         --get the list of extended Context
                         --check all extended Context in the list
                         --merge the results (classification, relations, errors)
                         checkExtCtx :: ContextCheckResult
                         checkExtCtx = foldr mergeRes ((Bottom, Bottom),[]) (map check (map (srchContext ctxs) xts))
                         --Enrich the Environment of the extended contexts with patterns from the current context
                         constructEnv :: ContextCheckResult -> Context -> ContextCheckResult
                         constructEnv extRes cx = 
                                   extRes --TODO
                                   --((Bottom, Bottom),[show (flatAncList gens)]) --for debugging to show flatAncList
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

   --search for a context by name and return the first one found
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

   ---------------------------------------------------------------------------------------------
   --Ancestors part: later in separate module
   --flatAncList is the interesting function which returns the Hierarchy and the tracklist to
   --correlate to the ADL code
   ---------------------------------------------------------------------------------------------

   type Ancestors = [Ancestor]
   type AncTarget = (Concept,[Gen])   -- (target, tracklist of gen declarations in ADL code)
   data Ancestor = Anc (Concept, [AncTarget]) deriving (Show)
                    --(source , list of AncTarget)
                    --source is child, targets all its ancestors

   instance Eq Ancestor where
     (Anc (src,_))==(Anc (src',_)) = src==src'

   type AllAncResult = ([Concept],[AncTarget],Ancestors)

   allAncsTrg :: AncTarget -> AllAncResult -> AllAncResult
   allAncsTrg  at@(trg,_) (lst,ancs,declancs)
         | elem trg lst = (lst,ancs,declancs)           --skip
         | otherwise    = foldr allAncsTrg (trg:lst,at:ancs,declancs) (ancestors (oneAnc declancs trg))    --add AncTarget and the allancs of this target

   --get the entry of a child from declAncs
   oneAnc :: Ancestors -> Concept -> Ancestor
   oneAnc [] src = Anc (src,[]) --should not be possible to find no entry
   oneAnc (a@(Anc(src',_)):ancs) src
         | src' == src = a            --found -> return
         | otherwise = oneAnc ancs src --try next

   --get the ancestors of a child
   ancestors :: Ancestor -> [AncTarget]
   ancestors (Anc (_,trgs)) = trgs


   allAncsTrgs :: Ancestors -> [AncTarget] -> [AncTarget]
   allAncsTrgs declancs trgs = allAncRes (foldr allAncsTrg ([],[],declancs) trgs)
       where   allAncRes :: AllAncResult -> [AncTarget]
               allAncRes (_,ancs,_) = ancs

   flatAncList :: Gens -> Ancestors
   flatAncList gens = foldTrgs (declAncs gens) (declAncs gens)
         where    --foreach Ancestor in declAncs, fold distinct the targets of its targets
         foldTrgs _ [] = []
         foldTrgs declancs ((Anc (src,trgs)):das) = (Anc(src,allAncsTrgs declancs trgs)):(foldTrgs declancs das)
         --Ancestor relations which are declared in ADL
         declAncs :: Gens -> Ancestors
         declAncs gens = foldr insertGen [] gens

   insertGen :: Gen -> Ancestors -> Ancestors
   insertGen gen@(G _ src trg) [] = ( Anc (src, [(trg,[gen])] ) ):[] --insert new
   insertGen gen@(G _ src _) (a@(Anc(src',trgs)):as)
          | src' == src = (Anc(src, insertGenTrg trgs gen)):as --update  (insert target)
          | otherwise   = a:(insertGen gen as) --try next

   insertGenTrg :: [AncTarget] -> Gen -> [AncTarget]
   insertGenTrg [] gen@(G _ _ trg) = (trg,[gen]):[] --insert
   insertGenTrg (t@(trg',gens):trgs) gen@(G _ _ trg)
          | trg' == trg = (trg,gen:gens):trgs --update (tracklist)
          | otherwise   = t:(insertGenTrg trgs gen) --try next




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





