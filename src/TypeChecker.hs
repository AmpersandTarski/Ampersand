--AGtry doet verschillende dingen
--hij bouwt bijvoorbeeld de ISA op en de wereld en leidt regels af uit de hierarchy
--als ik de typechecker eerst uitvoer en als er geen fouten zijn de AGtry, dan gaat die functionaliteit nooit verloren
--de typechecker zal alleen bepaalde fouten eerder afvangen.
module TypeChecker (typecheck) where

   import Adl

   ---------------------------------------------------------------------------------------------
   --MAIN function
   ---------------------------------------------------------------------------------------------

   type Errors = [String]
   type Environment = (Ancestors, DeclRels, Contexts)

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
                         checkExtCtx = foldr concatRes (([], [], []),[]) (map check (map (srchContext ctxs) (case cx of Ctx{} -> ctxon cx) ))
                         --Enrich the Environment of the extended contexts with patterns from the current context
                         constructEnv :: ContextCheckResult -> Context -> ContextCheckResult
                         constructEnv ((_,_,extCtxs),_) cx -- @(Ctx nm _ _ _ _ _ _ _ _ _ _)
                                   --{-
                                    = (
                                         ( flatAncList (allCtxGens (cx:extCtxs)),
                                           declRels (allCtxPats (cx:extCtxs)),
                                           cx:extCtxs
                                         ),
                                         []
                                       ) --TODO
                                   ---}
                                   -- =(([], [],[]),[show (flatAncList (allCtxGens (cx:extCtxs)) )]) --for debugging to show flatAncList
                                   -- =(([], [],[]),[show (declRels    (allCtxPats (cx:extCtxs)) )]) --for debugging to show declRels
                                   -- | nm=="Test2" = ((flatAncList (allCtxGens (cx:extCtxs)),declRels (allCtxPats (cx:extCtxs)),cx:extCtxs),[])            --for debugging
                                   -- | otherwise   = (([], [], []),[show (flatAncList (allCtxGens (cx:extCtxs)))])  --of extends Test2
                                   -- | otherwise   = (([], [], []),[show (declRels    (allCtxPats (cx:extCtxs)))])  --of extends Test2
         check (NotFound str) = (([],[],[]),("Extended context " ++ str ++ " of context " ++ (case cx of Ctx{} -> ctxnm cx) ++ " could not be found"):[]) --this case will not have been caught by the parser yet
         errors :: ContextCheckResult -> Errors
         errors ((_,_,_),e) = e

   --search for a context by name and return the first one found
   srchContext :: Contexts -> String -> ContextFound
   srchContext [] srchstr = NotFound srchstr   --The UU_Parser has already caught this case
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
                   go (_,rels,_) obj = processResult (infer env (castObjectDefToAdlExpr rels obj))
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
   concatRes ((anc1,rel1,cxs1),errs1) ((anc2,rel2,cxs2),errs2) | errs1==[] && errs2==[]
                                                              = ((anc1 ++ anc2, rel1 ++ rel2, cxs1 ++ cxs2),[])
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
   --the expr are definitions?
   -- x::(a,b), y::(b,c), expr::(a,b);(b,c)  -> (a,c) |- expr::(a,c) (I have to proof x and y to proof x;y)
   -- x::(a,b), y::(b,c), expr::(a,b)!(b,c)  -> (a,c) |- expr::(a,c)
   -- x::(a,b), y::(a,b), expr::(a,b)\/(a,b) -> (a,b) |- expr::(a,b)
   -- x::(a,b), y::(a,b), expr::(a,b)/\(a,b) -> (a,b) |- expr::(a,b)
   -- x::(a,b), expr::(a,b)~ -> (b,a)                 |- expr::(b,a)
   -- c1::a, c2::b, expr::V[a,b] -> (a,b)             |- expr::(a,b)
   -- c::a, expr::I[a] -> (a,a)                       |- expr::(a,a)
   -- x::(a,b), expr::-(a,b) -> (a,b)                 |- expr::(a,b)
   -- x::(a,b), expr::(a,b)* -> (a,b)                 |- expr::(a,b)
   -- x::(a,b), expr::(a,b)+ -> (a,b)                 |- expr::(a,b)
   ---------------------------------------------------------------------------------------------

   anythingExpr :: AdlExpr
   anythingExpr = Relation ([Anything,Anything],NotFoundDr "anythingExpr")

   --TODO more guards for different expressions
   castExpressionToAdlExpr :: DeclRels -> Expression -> AdlExpr
   castExpressionToAdlExpr declrels (Tm morph) = case morph of
                                    Mph{} -> Relation ([],srchDeclRel declrels (mphnm morph))
   castExpressionToAdlExpr _ _ = anythingExpr --unimplemented patterns

   castObjectDefToAdlExpr :: DeclRels -> ObjectDef -> InferExpr
   --castObjectDefToAdlExpr _ _ = Skip --comment to enable this function
   castObjectDefToAdlExpr declrels obj = case obj of
                                   Obj{} -> Inf (castExpressionToAdlExpr declrels (objctx obj)) (objpos obj)

   --cast the rule to an AdlExpr
   --TODO more guards for different rules
   castRuleToAdlExpr :: DeclRels -> Rule -> InferExpr
   --castRuleToAdlExpr _ _ = Skip --comment to enable this function
   castRuleToAdlExpr declrels rule
                     | case rule of Ru{} -> (rrsrt rule == Implication)
                                            = Inf (ImplRule                                         --rule of type implication
                                                    (castExpressionToAdlExpr declrels (rrant rule)) --left expr of rule
                                                    (castExpressionToAdlExpr declrels (rrcon rule)) --right expr of rule
                                                    )
                                              (rrfps rule)                                          --file position of rule


   --The type infered
   data InferedType = Type AdlType | TypeError String

   --Expression for which the type can be infered by function infer
   data InferExpr = Inf AdlExpr FilePos | Skip

   --a Concept is identified by its name of type String. A type is always a binary relation of Concepts
   type AdlType = (Concept,Concept)
   data AdlExpr =   Relation    ([Concept],DeclRelFound)              --The type of a Relation is declared locally in the expression or as a declaration line
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

   --TODO more guards for AdlExpr
   infer :: Environment -> InferExpr -> InferedType
   infer _ Skip = Type (Anything, Anything)
   infer _ (Inf (Relation rel) pos) = typeofRel rel pos
   infer _ (Inf _ pos) = TypeError ("Unknown type: " ++ show pos) --TODO

   --TODO
   --commonUpperBound returns a list of all common Ancestors.
   --If the expression is used in other expressions then there should be a common AdlType that fits the main expression.
   commonUpperBound :: Environment -> AdlType -> AdlType -> [AdlType]
   commonUpperBound _ _ _ = []

   typeofRel :: ([Concept],DeclRelFound) -> FilePos -> InferedType
   --morphism attributes first
   typeofRel (mphats@(src:trg:_),_) _ = Type(src,trg)
   typeofRel (_,(FoundDr d)) _ = case d of
                         Sgn{} -> Type (desrc d, detgt d)
   typeofRel (_,(NotFoundDr srchstr)) pos = TypeError ("Relation " ++ srchstr ++ " in expression at " ++ show pos ++ " has not been declared. ")

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
   -- House -> Building, (Building,Door) |- (House,Door)
   -- Villa -> House, House -> Building |- Villa -> Building
   -- isa::a -> b, rel::(b,c) |- rel::(a,c)
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
   insertGen gen [] = case gen of
                           G{} -> ( Anc (gengen gen, [(genspc gen,[gen])] ) ):[]
   --if there are entries, search for an entry of the child / source of the Gen
   insertGen gen@(G _ src _) (a@(Anc(src',trgs)):as)
          | case gen of G{} -> (gengen gen == src')
                               =
                               --if the child is located, add the ancestor to the list of ancestors of this child
                               (Anc(gengen gen, insertGenTrg trgs gen)):as --update  (insert target)
          | otherwise   =
                          --child not located yet, try next and preserve all entries (a:)
                          a:(insertGen gen as)

   --insert a new target/ancestor to the ancestor list.
   --if the ancestor already exists, add track information to the ancestor (declared twice)
   insertGenTrg :: [AncTarget] -> Gen -> [AncTarget]
   insertGenTrg [] gen = case gen of
                              G{} -> (genspc gen,[gen]):[] --insert ancestor
   insertGenTrg (t@(trg',gens):trgs) gen
          | case gen of G{} -> (genspc gen == trg')
                               = (genspc gen, gen:gens):trgs --update tracklist
          | otherwise   = t:(insertGenTrg trgs gen) --try next

   ---------------------------------------------------------------------------------------------
   --Relations part: later in separate module
   -- relations have nonlinear resources
   -- (Building, Door) |- (Building,Door)
   --
   --BADLY formulated reasoning by Gerard for Gerard only, because I know what I mean, if you know what I mean. :)
   --Will be removed...
   --The difference with the Ancestor model is that a relation declaration is not an
   --intuitionistic nonlinear implication, but more a labeled equality.
   --The label has a direction, the flip reverses the direction
   --Thus, the Ancestor model needed to support only a query returning ancestors for a given child
   --      because the relations in this model are not equal.
   --      the relations in the relation model ARE equal. Only in the definition there is a
   --      source and a target, and also in the application of the relation in expressions
   --      and rules the concepts relate as source and target.
   --On the other hand because the concepts in a relation are equal, there is no inheritance.
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


