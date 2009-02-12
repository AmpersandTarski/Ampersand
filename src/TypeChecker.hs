--AGtry doet verschillende dingen
--hij bouwt bijvoorbeeld de ISA op en de wereld en leidt regels af uit de hierarchy
--als ik de typechecker eerst uitvoer en als er geen fouten zijn de AGtry, dan gaat die functionaliteit nooit verloren
--de typechecker zal alleen bepaalde fouten eerder afvangen.
module TypeChecker (typecheck) where

   import Adl
   import Data.List   --intersect

   ---------------------------------------------------------------------------------------------
   --MAIN function
   ---------------------------------------------------------------------------------------------

   type Errors = [String]
   type Environment = (Children, DeclRels, Contexts)

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
   checkCtx arch@(Arch ctxs) (cx:tl_ctxs) = errors (check (Found cx)) ++ (checkCtx arch tl_ctxs)
      where
         check :: ContextFound -> ContextCheckResult
         check (Found cx) -- @(Ctx _ xts _ _ ((Pat _ _ gens@((G _ g _):_) _ _ _):_) _ _ _ _ _ _))
                     = checkObjDefExprs (constructEnv checkExtCtx cx)
                     where
                         --get the list of extended Context
                         --check all extended Context in the list (all siblings)
                         --merge the results (classification, relations, errors)
                         checkExtCtx :: ContextCheckResult
                         checkExtCtx = case cx of Ctx{} -> foldr concatRes (([], [], []),[]) (map check (map (srchContext ctxs) ( ctxon cx) ))
                         --Enrich the Environment of the extended contexts with patterns from the current context
                         constructEnv :: ContextCheckResult -> Context -> ContextCheckResult
                         constructEnv ((_,_,extCtxs),errs) cx -- @(Ctx nm _ _ _ _ _ _ _ _ _ _)
                                   --{-
                                    = (
                                         ( flatChdList (allCtxGens (cx:extCtxs)),
                                           declRels (allCtxPats (cx:extCtxs)),
                                           cx:extCtxs
                                         ),
                                         errs
                                       ) --TODO
                                   ---}
                                   -- =(([], [],[]),[show (flatChdList (allCtxGens (cx:extCtxs)) )]) --for debugging to show flatChdList
                                   -- =(([], [],[]),[show (declRels    (allCtxPats (cx:extCtxs)) )]) --for debugging to show declRels
                                   -- | nm=="Test2" = ((flatChdList (allCtxGens (cx:extCtxs)),declRels (allCtxPats (cx:extCtxs)),cx:extCtxs),[])            --for debugging
                                   -- | otherwise   = (([], [], []),[show (flatChdList (allCtxGens (cx:extCtxs)))])  --of extends Test2
                                   -- | otherwise   = (([], [], []),[show (declRels    (allCtxPats (cx:extCtxs)))])  --of extends Test2
         check (NotFound str) = (([],[],[]),("Extended context " ++ str ++ " of context " ++ (case cx of Ctx{} -> ctxnm cx) ++ " could not be found"):[]) --this case will not have been caught by the parser yet
         errors :: ContextCheckResult -> Errors
         errors ((_,_,_),e) = e

   --search for a context by name and return the first one found
   srchContext :: Contexts -> String -> ContextFound
   srchContext [] srchstr = NotFound srchstr
   srchContext (cx:ctxs) srchstr
            | case cx of Ctx{} -> (ctxnm cx==srchstr)
                                  = Found cx
            | otherwise = srchContext ctxs srchstr

   checkObjDefExprs :: ContextCheckResult -> ContextCheckResult
   --abort when there are errors from previous steps
   checkObjDefExprs ccr@(_,err:errs)       = ccr
   --resolve the type and check if the arguments are of such a type
   checkObjDefExprs ccr@(env@(_,_,ctxs),_) = 
                            ccr
                            --play env (allCtxObjDefs ctxs)  --TODO probably some foldr
             where play :: Environment -> ObjectDefs -> ContextCheckResult
                   play env (objdef1@(Obj  _ _ _ (objdef2:_) _):_)
                        = (env, go env objdef2)
                   play _ _ = (env, ["not good"])
                   go :: Environment -> ObjectDef -> [String]
                   go env obj = processResult (infer (castObjectDefToAdlExpr env obj))
                   processResult :: InferedType -> [String]
                   processResult (TypeError err) = [err]
                   processResult (Type _) = []

   ------------------------------
   --cumulative context functions
   --assumption, names are unqualified and unique within the context and its extended contexts
   --            if names must be qualified, then change the names of the components in
   --            the patterns to qualified names (p.e. TestContext.concept1 instead of concept1)
   ------------------------------
   --combine sibling contexts
   concatRes :: ContextCheckResult -> ContextCheckResult -> ContextCheckResult
   concatRes ((chd1,rel1,cxs1),errs1) ((chd2,rel2,cxs2),errs2) | errs1==[] && errs2==[]
                                                              = ((chd1 ++ chd2, rel1 ++ rel2, cxs1 ++ cxs2),[])
                                                  | otherwise = (([],[],[]),errs1 ++ errs2)

   --all the Gens of Contexts
   allCtxGens :: Contexts -> Gens
   allCtxGens [] = []
   allCtxGens (cx:ctxs) = case cx of Ctx{} -> allPatGens (ctxpats cx) ++ allCtxGens ctxs

   --all the Gens of patterns
   allPatGens :: Patterns -> Gens
   allPatGens [] = []
   allPatGens (p:ps)  = case p of Pat{} -> ptgns p ++ allPatGens ps

   --all the patterns of contexts
   allCtxPats :: Contexts -> Patterns
   allCtxPats [] = []
   allCtxPats (cx:ctxs) = case cx of Ctx{} -> ctxpats cx ++ allCtxPats ctxs

   --all the ObjectDefs of Contexts
   allCtxObjDefs :: Contexts -> ObjectDefs
   allCtxObjDefs [] = []
   allCtxObjDefs (cx:ctxs) = case cx of Ctx{} -> ctxos cx ++ allCtxObjDefs ctxs

   ---------------------------------------------------------------------------------------------
   --Expression part: later in separate module
   --This module connects the ADL code to the type inference module by means of the InferExpr data type
   -- e1::(a,b), e2::(b,c) |- e3::e1;e2   -> (a,c)
   -- e1::(a,b), e2::(b,c) |- e3::e1!e2   -> (a,c)
   -- e1::(a,b), e2::(a,b) |- e3::e1\/e2  -> (a,b)
   -- e1::(a,b), e2::(a,b) |- e3::e1/\e2  -> (a,b)
   -- e1::(a,b)            |- e2::e1~     -> (b,a)
   -- c1::a, c2::b         |- e::V[c1,c2] -> (a,b)
   -- c::a                 |- e::I[c]     -> (a,a)
   -- e1::(a,b)            |- e2::-e1     -> (a,b)
   -- e1::(a,b)            |- e2::e1*     -> (a,b)
   -- e1::(a,b)            |- e2::e1+     -> (a,b)
   -- c1::a, c2::b, a>=b   |- c1::a -> b           --need to define >=, it is the lowerbound
   ---------------------------------------------------------------------------------------------

   --Expression for which the type can be infered by function infer
   data InferExpr = Inf AdlExpr FilePos

   --Relation will be the only expression already infered possibly containing a TypeError
   data AdlExpr =   Relation    InferedType --([Concept],DeclRelFound)              --The type of a Relation is declared locally in the expression or as a declaration line
                                                                      --use typeofRel to cast to InferedType (The AdlExpr must be part of an InferExpr containing the file position)
                  | Semicolon   {source::AdlExpr, target::AdlExpr}
                  | Dagger      {source::AdlExpr, target::AdlExpr}
                  | Flip        AdlExpr
                  | TrsClose    AdlExpr
                  | TrsRefClose AdlExpr
                  | Complement  AdlExpr
                  | Union       {source::AdlExpr, target::AdlExpr}
                  | Intersect   {source::AdlExpr, target::AdlExpr}
                  | Identity    AdlExpr
                  | Universe    AdlExpr
                  | ImplRule    {premise::AdlExpr, conclusion::AdlExpr}
                  --TODO

   ------------------
   --common functions
   ------------------

   --An AdlExpr of type ([Anything],[Anything])
   -- define as morphism attributes to infer the type like all other types by using typeofRel
   anythingExpr :: AdlExpr
   anythingExpr =  Relation (typeofRel
                               []
                               (Just [Anything,Anything])
                               Nothing
                             )
                             
   lowerbound :: Children -> Concept -> [Concept]
   lowerbound _ Anything = [Anything]
   lowerbound _ NOthing = [NOthing]
   lowerbound _ S = [S] --TODO check if this is correct
   lowerbound chds c = c:(children (chdTargets (oneChd chds c)))

   ------------------------
   --Cast functions 
   --for casting ADL module 
   --data type to InferExpr
   ------------------------

   castObjectDefToAdlExpr :: Environment -> ObjectDef -> InferExpr
   castObjectDefToAdlExpr env obj = case obj of
                                   Obj{} -> Inf (castExpressionToAdlExpr env (objctx obj)) (objpos obj)

   --cast the rule to an AdlExpr
   --TODO more guards for different rules
   castRuleToAdlExpr :: Environment -> Rule -> InferExpr
   castRuleToAdlExpr env rule
                     | case rule of Ru{} -> (rrsrt rule == Implication)
                                            = Inf (ImplRule                                         --rule of type implication
                                                    (castExpressionToAdlExpr env (rrant rule)) --left expr of rule
                                                    (castExpressionToAdlExpr env (rrcon rule)) --right expr of rule
                                                    )
                                              (rrfps rule)                                          --file position of rule


   --TODO more guards for different expressions
   castExpressionToAdlExpr :: Environment -> Expression -> AdlExpr
   castExpressionToAdlExpr (chds,declrels,_) (Tm morph)
                         | case morph of Mph{} -> mphats morph==[]
                                                  =  Relation (typeofRel
                                                          chds
                                                          Nothing
                                                          (Just (srchDeclRel declrels (mphnm morph)))
                                                       )
                         | otherwise              = Relation (typeofRel
                                                          chds
                                                          (Just (mphats morph))
                                                          Nothing
                                                       )
   castExpressionToAdlExpr env (F (expr1:expr2:exprs))
                                | exprs==[]    = Semicolon
                                                      (castExpressionToAdlExpr env expr1)
                                                      (castExpressionToAdlExpr env expr2)
                                | otherwise    = Semicolon
                                                      (castExpressionToAdlExpr env expr1)
                                                      (castExpressionToAdlExpr env (F (expr2:exprs)))
   castExpressionToAdlExpr _ _ = anythingExpr --unimplemented patterns


   --morphism attributes -> Declaration -> the type given mphatts or declaration, when both specified mphatts will be used
   typeofRel :: Children -> Maybe [Concept] -> Maybe DeclRelFound -> InferedType
   typeofRel chds (Just mphats@(src:trg:_)) _ = Type(lowerbound chds src,lowerbound chds trg)
   typeofRel chds _ (Just (FoundDr d)) = case d of
                    Sgn{} -> Type (lowerbound chds (desrc d), lowerbound chds (detgt d))
   typeofRel _ _ (Just (NotFoundDr srchstr)) = TypeError ("Relation " ++ srchstr ++ " has not been declared. " )



   ---------------------------------------------------------------------------------------------
   --Type inference part: later in separate module
   ---------------------------------------------------------------------------------------------

   --The type infered
   data InferedType = Type AdlType | TypeError String

   --a Concept is identified by its name of type String. A type is always a binary relation of Concepts
   --The Concept is the list of all types it can be as a result of ISA relations
   type AdlType = ([Concept],[Concept])
   
   errorpos :: String -> FilePos -> String
   errorpos err pos = err ++ " in expression at " ++ show pos

   --TODO more guards for AdlExpr
   --I'll need a function intersection::[Concept]->[Concept]->[Concept] to get the intersection of to concept lists as a list
   --there is a function intersect::[a]->[a]->[a] taken the intersect based on equality (==) of a.
   infer :: InferExpr -> InferedType
   infer (Inf (Relation (TypeError err)) pos) = TypeError (errorpos err pos)
   infer (Inf (Relation rel) _) = rel   --Relation is already an InferedType
   infer (Inf (Semicolon expr1 expr2) pos) = typeofSemiColon (infer (Inf expr1 pos)) (infer (Inf expr2 pos)) pos
   infer (Inf (Dagger expr1 expr2) pos) = typeofDagger expr1 expr2 pos
   infer (Inf (Union expr1 expr2) pos) = typeofUnion expr1 expr2 pos
   infer (Inf (Intersect expr1 expr2) pos) = typeofIntersect expr1 expr2 pos
   infer (Inf (ImplRule expr1 expr2) pos) = typeofImplRule expr1 expr2 pos
   infer (Inf (Flip expr) pos) = typeofFlip expr pos
   infer (Inf (Identity expr) pos) = typeofIdentity expr pos
   infer (Inf (Universe expr) pos) = typeofUniverse expr pos
   infer (Inf (Complement expr) pos) = typeofComplement expr pos
   infer (Inf (TrsClose expr) pos) = typeofTrsClose expr pos
   infer (Inf (TrsRefClose expr) pos) = typeofTrsRefClose expr pos
   infer (Inf _ pos) = TypeError ("Unknown expression: " ++ show pos) --TODO

   typeofSemiColon :: InferedType -> InferedType -> FilePos -> InferedType
   typeofSemiColon (TypeError err) _ pos = TypeError (errorpos err pos)
   typeofSemiColon _ (TypeError err) pos = TypeError (errorpos err pos)
   typeofSemiColon (Type (src1,trg1)) (Type (src2,trg2)) _
                   | intersect trg1 src2==[] = TypeError ("Type inference rule Composition: Possible types of target1 " ++ show trg1 ++ " do not match the possible types of source2 " ++ show src2)
                   | otherwise = Type (src1,trg2)
   typeofSemiColon _ _ _ = TypeError ("typeofSemiColon: not implemented. ")

   typeofDagger :: AdlExpr -> AdlExpr -> FilePos -> InferedType
   typeofDagger _ _ _ = TypeError ("typeofDagger: not implemented. ")
   
   typeofUnion :: AdlExpr -> AdlExpr -> FilePos -> InferedType
   typeofUnion _ _ _ = TypeError ("typeofUnion: not implemented. ")

   typeofIntersect :: AdlExpr -> AdlExpr -> FilePos -> InferedType
   typeofIntersect _ _ _ = TypeError ("typeofIntersect: not implemented. ")

   typeofImplRule :: AdlExpr -> AdlExpr -> FilePos -> InferedType
   typeofImplRule _ _ _ = TypeError ("typeofImplRule: not implemented. ")
   
   typeofFlip :: AdlExpr -> FilePos -> InferedType
   typeofFlip _ _ = TypeError ("typeofFlip: not implemented. ")

   typeofIdentity :: AdlExpr -> FilePos -> InferedType
   typeofIdentity _ _ = TypeError ("typeofIdentity: not implemented. ")
   
   typeofUniverse :: AdlExpr -> FilePos -> InferedType
   typeofUniverse _ _ = TypeError ("typeofUniverse: not implemented. ")
   
   typeofComplement :: AdlExpr -> FilePos -> InferedType
   typeofComplement _ _ = TypeError ("typeofComplement: not implemented. ")
   
   typeofTrsClose :: AdlExpr -> FilePos -> InferedType
   typeofTrsClose _ _ = TypeError ("typeofTrsClose: not implemented. ")
   
   typeofTrsRefClose :: AdlExpr -> FilePos -> InferedType
   typeofTrsRefClose _ _ = TypeError ("typeofTrsRefClose: not implemented. ")

   ---------------------------------------------------------------------------------------------
   --Children part: later in separate module
   --flatChdList is the interesting function which returns the model and the tracklist to
   --correlate to the ADL code. flatChdList is build in two phases. First the actual code
   --declarations are enumerated (declchds). Then the children are resolved recursively, and folded.
   --Each concept is resolved once to prevent recursive loops. Therefore a list is passed around
   --to keep track of the resolved concepts.
   --
   --oneChd can be used to get the Child entry for a Concept by Concept.
   --children can be used to get the children from an Child entry
   --
   --Maybe I can arrange and label the functions more clearly. 
   --target = parent,
   --source = child,
   --both target and source are Concepts.
   --
   --Child is a kind of nonlinear intuitionistic implication.
   --if Boss -> Person. If Boss is true then Person is true.
   --Both Boss and Person are free resources.
   --Child is also an Identity, If Boss then Boss
   -- House -> Building, (Building,Door) |- (House,Door)
   -- Villa -> House, House -> Building |- Villa -> Building
   -- isa::a -> b, rel::(b,c) |- rel::(a,c)
   ---------------------------------------------------------------------------------------------

   type Children = [Child]

   -- (target, tracklist of gen declarations in ADL code)
   type ChdTarget = (Concept,[Gen])

   data Child = Chd (Concept       , [ChdTarget]) deriving (Show)
                    --(parent / source , list of ChdTarget / children)

   --triple to pass around progress ([Concept]), intermediate result ([ChdTarget]), 
   --and progress original input (Children)
   --to get all children of a Concept (lowerbound) and prevent looping
   type AllChdResult = ([Concept],[ChdTarget],Children)

   --equality of Child = equality of its source Concept = equality of the name of the source Concept
   instance Eq Child where
     (Chd (src,_))==(Chd (src',_)) = src==src'

   ------------------
   --PUBLIC FUNCTIONS
   ------------------

   --returns the model and the tracklist to correlate to the ADL code.
   --flatChdList is build in two phases. First the actual code
   --declarations are enumerated (declChds). Then the children are resolved recursively, and folded (foldTrgs).
   flatChdList :: Gens -> Children
   flatChdList gens = foldTrgs (declChds gens) (declChds gens)
         where
         --foreach Child entry in declChds, fold distinct the targets of its targets
         --provide the Children declared in the code, and the list of Children to resolve
         foldTrgs :: Children -> Children -> Children
         foldTrgs _ [] = []
         foldTrgs declchds ((Chd (src,trgs)):das) = (Chd(src,allChdsTrgs declchds trgs)):(foldTrgs declchds das)
         --Child relations which are declared in ADL
         declChds :: Gens -> Children
         declChds gens = foldr insertGen [] gens

   --get the Child entry from an Child model by Concept 
   --(equality by concept name)
   oneChd :: Children -> Concept -> Child
   oneChd [] src       =
                         --apparantly this Concept has no children, return the concept without children
                         Chd (src,[])
   oneChd (a@(Chd(src',_)):chds) src
         | src' == src = 
                         --found -> return
                         a
         | otherwise   = 
                         --try next
                         oneChd chds src

   --get the children of a Child entry
   chdTargets :: Child -> [ChdTarget]
   chdTargets (Chd (_,trgs)) = trgs
   
   children :: [ChdTarget] -> [Concept]
   children [] = []
   children ((c,_):ats) = c:(children ats)

   -------------------
   --PRIVATE FUNCTIONS
   -------------------

   --return all ChdTargets of an ChdTarget, 
   --respecting the already resolved Concepts and results,
   --given the explicit declarations from the ADL code
   allChdsTrg :: ChdTarget -> AllChdResult -> AllChdResult
   allChdsTrg  at@(trg,_) (lst,chds,declchds)
         | elem trg lst = 
                          --skip, already resolved: so just forward result so far
                          (lst,chds,declchds)

         | otherwise    =
                          --add this target (at:chds),
                          --and all its children (foldr ... oneChd declchds trg), as child,
                          --register this target as resolved (trg:lst)
                          foldr allChdsTrg (trg:lst,at:chds,declchds) (chdTargets (oneChd declchds trg))

   --return all the ChdTarget for a list of ChdTarget,
   --given the explicit declaration from the ADL code
   allChdsTrgs :: Children -> [ChdTarget] -> [ChdTarget]
   allChdsTrgs declchds trgs = allChdRes (foldr allChdsTrg ([],[],declchds) trgs)
       where   allChdRes :: AllChdResult -> [ChdTarget]
               allChdRes (_,chds,_) = chds

   --insert an explicit ADL code declaration (Gen) to the list
   insertGen :: Gen -> Children -> Children
   --if there is no Child entry yet, add a new entry
   insertGen gen [] = case gen of
                           G{} -> ( Chd (genspc gen, [(gengen gen,[gen])] ) ):[]
   --if there are entries, search for an entry of the child / source of the Gen
   insertGen gen (a@(Chd(src',trgs)):as)
          | case gen of G{} -> (genspc gen == src')
                               =
                               --if the child is located, add the child to the list of children of this child
                               (Chd(genspc gen, insertGenTrg trgs gen)):as --update  (insert target)
          | otherwise   =
                          --child not located yet, try next and preserve all entries (a:)
                          a:(insertGen gen as)

   --insert a new target/child to the child list.
   --if the child already exists, add track information to the child (declared twice)
   insertGenTrg :: [ChdTarget] -> Gen -> [ChdTarget]
   insertGenTrg [] gen = case gen of
                              G{} -> (gengen gen,[gen]):[] --insert child
   insertGenTrg (t@(trg',gens):trgs) gen
          | case gen of G{} -> (gengen gen == trg')
                               = (gengen gen, gen:gens):trgs --update tracklist
          | otherwise   = t:(insertGenTrg trgs gen) --try next

   ---------------------------------------------------------------------------------------------
   --Relations part: later in separate module
   -- relations have nonlinear resources
   -- (Building, Door) |- (Building,Door)
   --
   --BADLY formulated reasoning by Gerard for Gerard only, because I know what I mean, if you know what I mean. :)
   --Will be removed...
   --The difference with the Child model is that a relation declaration is not an
   --intuitionistic nonlinear implication, but more a labeled equality.
   --The label has a direction, the flip reverses the direction
   --Thus, the Child model needed to support only a query returning children for a given child
   --      because the relations in this model are not equal.
   --      the relations in the relation model ARE equal. Only in the definition there is a
   --      source and a target, and also in the application of the relation in expressions
   --      and rules the concepts relate as source and target.
   --On the other hand because the concepts in a relation are equal, there is no inheritchde.
   --The relation model can be just a list of declarations.
   --New relations can be defined as expressions consisting of relations from the model.
   ---------------------------------------------------------------------------------------------

   --just Declarations?
   type DeclRels = [DeclRel]
   type DeclRel = Declaration
   data DeclRelFound = FoundDr DeclRel | NotFoundDr String

   --concatenate the declarations of relation from the patterns
   declRels :: Patterns -> DeclRels
   declRels [] = []
   declRels (p:ps) = case p of
                          Pat{} -> ptdcs p ++ declRels ps

   srchDeclRel :: DeclRels -> String -> DeclRelFound
   srchDeclRel [] srchstr = NotFoundDr srchstr
   srchDeclRel (drl:drls) srchstr
             | case drl of Sgn{} -> (decnm drl == srchstr)
                                    = FoundDr drl
             | otherwise = srchDeclRel drls srchstr

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

{- Opmerking van Han aan Gerard:
Beste Gerard,
Ik zag dat je hard aan het werk bent aan de typechecker! Welkom aan boord!
Ik kon mijn nieuwsgierigheid niet bedwingen, en heb even in je code zitten gluren.
Hopelijk vind je dat niet erg. Het ziet er goed uit, overzichtelijk m.b.t. structuur,
en lekker veel commentaar. Houden zo. 
Daarnaast zie ik ook veel gebruik van de streepjes notatie. Dat is een kwestie
van smaak natuurlijk, maar weet dat je te kiezen hebt:

een voorbeeld:

   declRels :: Patterns -> DeclRels
   declRels [] = []
   declRels (p@(Pat _ _ _ decls _ _):ps) = decls ++ declRels ps

is identiek aan

   declRels :: Patterns -> DeclRels
   declRels [] = []
   declRels (p:ps) = case p of
                     Pat{} -> ptdcs p ++ declRels ps

Beide notaties doen hetzelfde, maar de onderste is minder gevoelig voor wijzigingen
in de datastructuur. Als er een 7de attribuut aan Pat wordt toegevoegd, dan
moet je de eerste variant aanpassen. De tweede variant is ongevoelig. 
In sommige gevallen wil je juist wél getriggerd worden als de datastructuur
wijzigt. (bijvoorbeeld in ShowXML). Dan is de eerste variant verstandiger. 
Maar het is natuurlijk allemaal een kwestie van smaak. 
Succes met typechecken! 
NB mocht je me willen bellen voor vragen, doe dat gerust: 06-10930606
-}


