--AGtry doet verschillende dingen
--hij bouwt bijvoorbeeld de ISA op en de wereld en leidt regels af uit de hierarchy
--als ik de typechecker eerst uitvoer en als er geen fouten zijn de AGtry, dan gaat die functionaliteit nooit verloren
--de typechecker zal alleen bepaalde fouten eerder afvangen.
module TypeChecker (typecheck) where

   import Adl         -- .MorphismAndDeclaration.makeDeclaration
   import Data.List   --intersect, union, delete

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
                     = checkThisCtx (constructEnv checkExtCtx cx)
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

   checkThisCtx :: ContextCheckResult -> ContextCheckResult
   --abort when there are errors from previous steps
   checkThisCtx ccr@(_,err:errs)       = ccr
   --resolve the type and check if the arguments are of such a type
   checkThisCtx ccr@(env@(_,_,ctxs),_) =
                            --combine all errors of things to check like objectdefs and rules
                            --(env, checkObjDefs env (allCtxObjDefs ctxs)) --return this to enable the checking of expressions in ADL code
                            ccr  --return this to disable the checking of expressions in ADL code

   --abstract expressions from all objectdefs (castObjectDefsToAdlExprs) and infer their types (infer)
   --Then check the result (processResult)
   --Return the list of error strings
   checkObjDefs :: Environment -> ObjectDefs -> [String]
   checkObjDefs _ [] = []
   --checkObjDefs env (obj:objs) = case obj of Obj{} -> [show (castObjectDefsToAdlExprs env [obj] 0)]
   checkObjDefs env (obj:objs) =
            case obj of Obj{} -> (processResult
                                       (map inferWithInfo (castObjectDefsToAdlExprs env [obj] 0))
                                  )
                                 ++ checkObjDefs env objs


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
   --Meta information part: later in separate module
   ---------------------------------------------------------------------------------------------

   ----------------------------------------------------
   --generic meta information structures and functions
   ----------------------------------------------------

   --generic type to communicate a meta information structure with an object a
   data MetaInfo info a = Info info a  deriving (Show)    

   --infer the type of an AdlExpr maintaining the link to the meta information
   inferWithInfo :: MetaInfo a AdlExpr -> MetaInfo a InferedType
   inferWithInfo (Info info expr) = Info info (infer expr)

   --removeInfo :: MetaInfo info a -> a
   --removeInfo (Info _ x) = x

   --Combine code position information and an error string
   errorpos :: String -> FilePos -> String
   errorpos err pos = err ++ " in expression at " ++ show pos
   
   ----------------------------------------------------
   --specific meta information structures and functions
   ----------------------------------------------------   

   --MetaInfo of this type is used for abstracting and checking expressions from ObjectDefs
   type MetaInfo1 a = MetaInfo (FilePos, Depth) a
   type Depth = Int

   --Process type inference results of ObjectDefs
   --If a type is infered, then it's ok. In case of a TypeError return the error.
   processResult :: [MetaInfo1 InferedType] -> [String]
   processResult [] = []
   processResult ((Info (pos,_) (TypeError err)):ts) = (errorpos err pos):(processResult ts)
   processResult ((Info _ (Type _)):ts) = processResult ts


   ---------------------------------------------------------------------------------------------
   --Expression part: later in separate module
   --This module connects the ADL tool to the type inference module by means of the InferExpr data type
   --If something changes in the ADL tool this is the module to adapt to changes
   --In other words: the inference module should be decoupled by this module
   ---------------------------------------------------------------------------------------------

   ------------------------
   --Cast functions
   --for casting ADL module 
   --data type to InferExpr
   ------------------------

   --TODO casting of ObjectDef and Rule should probably result in a list of (AdlExpr, FilePos)
   --The type of the AdlExprs should be inferred, and the types should be checked in their
   --context for example in a rule both sides must be of the same type
   --                    in an ObjectDef the expression should be applied to a conceptof a valid type
   castObjectDefsToAdlExprs :: Environment -> ObjectDefs -> Depth ->  [MetaInfo1 AdlExpr] -- [(AdlExpr,MetaInfo)]
   castObjectDefsToAdlExprs _ [] _ = []
   castObjectDefsToAdlExprs env@(x,xx,_) (obj:objs) currdepth = case obj of
                                   Obj{} -> if null (objats obj)
                                            then (Info (objpos obj,currdepth) (castExpressionToAdlExpr env (objctx obj)))--add this objdef as AdlExpr for evaluation
                                                  :(castObjectDefsToAdlExprs env objs currdepth) --add the sibling objdefs as AdlExpr for evaluation
                                            --compose a semicolon expr
                                            --by combining expr objctx with all objats as expr
                                            --TODO objats worden aan elkaar gehangen, dat is niet goed
                                            else --[UnknownExpr ((show xx)++(show

                                                   (castObjectDefsToAdlExprs env (objats obj) (currdepth+1)) --add the nested objdefs as AdlExpr for evaluation
                                                   ++ [(Info (objpos obj,currdepth) (castExpressionToAdlExpr env (objctx obj)))] --add this objdef as AdlExpr for evaluation
                                                   ++ (map
                                                        (combineObjDefs (castExpressionToAdlExpr env (objctx obj)) currdepth)
                                                        (castObjectDefsToAdlExprs env (objats obj) (currdepth+1))
                                                   ) --add the nested objDefs combined with this objdef as AdlExpr for evaluation
                                                   ++ (castObjectDefsToAdlExprs env objs currdepth) --add the sibling objdefs as AdlExpr for evaluation
                                                   --))]

   combineObjDefs :: AdlExpr -> Depth -> MetaInfo1 AdlExpr -> MetaInfo1 AdlExpr
   combineObjDefs obj currdepth expr@(Info (pos,depth) nestedobj)
                         --only combine subexpression one depth lower then the current depth
                         --put the combined expr on the current depth
                         --TODO filter to prevent duplicates not complete yet
                       | (currdepth+1)==depth = if (isError (infer obj)) || (isError (infer nestedobj))
                                                then Info (pos,currdepth) (Relation (TypeError "Parent or nested SERVICE contains type error."))
                                                else Info (pos,currdepth) (Semicolon obj nestedobj )
                       | otherwise            = expr


   --cast the rule to an AdlExpr
   --TODO more guards for different rules
   castRuleToAdlExpr :: Environment -> Rule -> AdlExpr
   castRuleToAdlExpr env rule
                     | case rule of Ru{} -> (rrsrt rule == Implication)
                                            = ImplRule                                        --rule of type implication
                                                    (castExpressionToAdlExpr env (rrant rule)) --left expr of rule
                                                    (castExpressionToAdlExpr env (rrcon rule)) --right expr of rule
                                             -- , rrfps rule)                                    --file position of rule


   --The parser translates expressions with a flip on subexpressions to an expressions
   --with only flips on morphisms of type Mph for example (r;s)~ is parsed as s~;r~
   --flips on Universe V[A*B] will be returned by the parser as V[B*A]
   --SubExpressions must be of type MayContainInfo AdlExpr so construct them with Plain
   castExpressionToAdlExpr :: Environment -> Expression -> AdlExpr
   castExpressionToAdlExpr (chds,declrels,_) (Tm morph@(Mph{}))
                                               = doNotFlip (mphyin morph)
                                                            (Relation (typeofRel
                                                                               chds
                                                                               (srchDeclRelByMorphism declrels morph)
                                                                      )
                                                            )

                         where
                            doNotFlip:: Bool -> AdlExpr -> AdlExpr
                            doNotFlip False expr@(Relation (Type _)) = Flip expr
                            doNotFlip _     expr                     = expr
   castExpressionToAdlExpr (chds,declrels,_) (Tm morph) --other Morphisms (I and V etc do not need to be flipped)
                                               = Relation (typeofRel
                                                          chds
                                                          (srchDeclRelByMorphism declrels morph)
                                                          )
   castExpressionToAdlExpr env (Tc expr)       = castExpressionToAdlExpr env expr
   castExpressionToAdlExpr env (F (expr1:expr2:exprs))
                                | exprs==[]    = Semicolon
                                                      (castExpressionToAdlExpr env expr1)
                                                      (castExpressionToAdlExpr env expr2)
                                | otherwise    = Semicolon
                                                      (castExpressionToAdlExpr env expr1)
                                                      (castExpressionToAdlExpr env (F (expr2:exprs)))
   castExpressionToAdlExpr env (Fd (expr1:expr2:exprs))
                                | exprs==[]    = Dagger
                                                      (castExpressionToAdlExpr env expr1)
                                                      (castExpressionToAdlExpr env expr2)
                                | otherwise    = Dagger
                                                      (castExpressionToAdlExpr env expr1)
                                                      (castExpressionToAdlExpr env (Fd (expr2:exprs)))
   castExpressionToAdlExpr env (Fi (expr1:expr2:exprs))
                                | exprs==[]    = Intersect
                                                      (castExpressionToAdlExpr env expr1)
                                                      (castExpressionToAdlExpr env expr2)
                                | otherwise    = Intersect
                                                      (castExpressionToAdlExpr env expr1)
                                                      (castExpressionToAdlExpr env (Fi (expr2:exprs)))
   castExpressionToAdlExpr env (Fu (expr1:expr2:exprs))
                                | exprs==[]    = Union
                                                      (castExpressionToAdlExpr env expr1)
                                                      (castExpressionToAdlExpr env expr2)
                                | otherwise    = Union
                                                       (castExpressionToAdlExpr env expr1)
                                                       (castExpressionToAdlExpr env (Fu (expr2:exprs)))
   castExpressionToAdlExpr env (K0 expr)       = TrsRefClose (castExpressionToAdlExpr env expr)
   castExpressionToAdlExpr env (K1 expr)       = TrsClose (castExpressionToAdlExpr env expr)
   castExpressionToAdlExpr env (Cp expr)       = Complement (castExpressionToAdlExpr env expr)
   castExpressionToAdlExpr env expr = UnknownExpr "Fatal: Cannot cast to AdlExpr. "


   --morphism attributes -> Declaration -> the type given mphatts or declaration, when both specified mphatts will be used
   --Loose the declarations of ISA and relations, and other ADL tool specifics, by already infering a type
   typeofRel :: Children -> DeclRelFound -> InferedType
   --typeofRel chds (Just mphats@(src:trg:_)) _ = Type (lowerbound chds src , lowerbound chds trg)
   typeofRel chds (FoundDr d) = case d of
                    Sgn{} -> Type (lowerbound chds (desrc d), lowerbound chds (detgt d));
                    --TODO why is there a despc and degen?
                    Isn{} -> Type (lowerbound chds (despc d), lowerbound chds (despc d));
                    --Vs degen is the source Concept, Vs despc the target Concept
                    Vs {} -> Type (lowerbound chds (degen d), lowerbound chds (despc d));
                    --IsCompl{} will never be the result of ADL.MorphismAndDeclaration.makeDeclaration
                    --makeDeclaration is used in srchDeclRelByMorphism
                    _ -> TypeError ("Fatal: Unknown Declaration constructor. ")
   typeofRel _ (NotFoundDr d) = TypeError ("Relation " ++ d ++ " has not been declared. " )



   ---------------------------------------------------------------------------------------------
   --Type inference part: later in separate module
   --need to define a >= b
   -- Anything = top, Nothing = bottom
   -- c1::a, c2::b, a>=b   |- c1::a -> b
   -- e1::(a,b1) , e2::(b2,c)  b1>=b b2>=b             |- e3::e1;e2 -> (a,c)
   -- e1::(a,b1) , e2::(b2,c)  b1>=b b2>=b             |- e3::e1!e2 -> (a,c)
   -- e1::(a1,b1), e2::(a2,b2) a1>=a a2>=a b1>=b b2>=b |- e3::e1\/e2 -> (a,b)
   -- e1::(a1,b1), e2::(a2,b2) a1>=a a2>=a b1>=b b2>=b |- e3::e1/\e2 -> (a,b)
   -- e1::(a,b)            |- e2::e1~     -> (b,a)
   -- c1::a, c2::b         |- e::V[c1,c2] -> (a,b)
   -- c::a                 |- e::I[c]     -> (a,a)
   -- e1::(a,b)            |- e2::-e1     -> (a,b)
   -- e1::(a,b)            |- e2::e1*     -> (a,b)
   -- e1::(a,b)            |- e2::e1+     -> (a,b)
   ---------------------------------------------------------------------------------------------

   --The type inferred
   data InferedType = Type AdlType | TypeError String deriving (Show)

   isError :: InferedType -> Bool
   isError (TypeError _) = True
   isError _             = False

   --a Concept is identified by its name of type String. A type is always a binary relation of Concepts
   --The Concept is the list of all types it can be as a result of ISA relations
   type AdlType = ([Concept],[Concept])
   

   --Relation will be the only expression already inferred possibly containing a TypeError
   --TODO make an AdlExpr trackable by storing the original Expression (AdlExpr, Expression) and use this in errors
   data AdlExpr =   Relation    InferedType --([Concept],DeclRelFound)              --The type of a Relation is declared locally in the expression or as a declaration line
                                                                      --use typeofRel to cast to InferedType (The AdlExpr must be part of an InferExpr containing the file position)
                  | Semicolon   {source::AdlExpr, target::AdlExpr}
                  | Dagger      {source::AdlExpr, target::AdlExpr}
                  | Flip        {expr::AdlExpr}
                  | TrsClose    {expr::AdlExpr}
                  | TrsRefClose {expr::AdlExpr}
                  | Complement  {expr::AdlExpr}
                  | Union       {source::AdlExpr, target::AdlExpr}
                  | Intersect   {source::AdlExpr, target::AdlExpr}
   --why can't I specify an I or V for a Relation? Why are I and V morphisms and not expressions?
   --I and V are cast as Relation InferedType and thus supported as morphisms
        --TODO          | Identity    AdlExpr
        --TODO          | Universe    InferedType
                  | ImplRule    {premise::AdlExpr, conclusion::AdlExpr} --TODO is a rule an expression, and thus has a type?
                  | UnknownExpr String   deriving (Show)
        --SJ: Ja, een regel is een expressie. De regel a|-c is hetzelfde als de expressie -a\/c.
        --SJ: Het type van een regel is het type van de equivalente expressie, namelijk  typeOf a `lub` typeOf c (aannemende dat typeOf het type van een expressie bepaalt)
                  
                  --TODO


   --An AdlExpr of type ([Anything],[Anything])
   -- define as morphism attributes to infer the type like all other types by using typeofRel
   --anythingExpr :: AdlExpr
   --anythingExpr =  Relation (typeofRel
    --                           []
     --                          (Just [Anything,Anything])
       ---                        Nothing
          --                   )

   lowerbound :: Children -> Concept -> [Concept]
   lowerbound _ Anything = [Anything]
   lowerbound _ NOthing = [NOthing]
   lowerbound _ S = [S] --TODO check if this is correct
   lowerbound chds c = c:(children (chdTargets (oneChd chds c)))

   infer :: AdlExpr -> InferedType
   infer (Relation rel) = rel   --Relation is already an InferedType
   infer (Semicolon expr1 expr2) = inferAbBcAc (infer expr1) (infer expr2)
   infer (Dagger expr1 expr2) = inferAbBcAc (infer expr1) (infer expr2)
   infer (Union expr1 expr2) = inferAbAbAb (infer expr1) (infer expr2)
   infer (Intersect expr1 expr2) = inferAbAbAb (infer expr1) (infer expr2)
   infer (ImplRule expr1 expr2) = inferAbAbAb (infer expr1) (infer expr2)
   infer (Flip expr) = inferAbBa (infer expr)
   --If I and V ever become expressions:
   --TODO infer (Identity expr) = inferIdentity expr
   --TODO infer (Universe expr) = inferUniverse expr
   infer (Complement expr) = inferAbAb (infer expr)
   infer (TrsClose expr) = inferAbAb (infer expr)
   infer (TrsRefClose expr) = inferAbAb (infer expr)
   infer (UnknownExpr err)= TypeError err --The expression is already known to be unknown
   infer _ = TypeError ("Fatal: No inference algorithm implemented for certain AdlExpr. ")


   --infer  e1::(a,b1), e2::(b2,c) b1>=b b2>=b |- e3::e1 -> e2 -> (a,c)
   inferAbBcAc :: InferedType -> InferedType -> InferedType
   inferAbBcAc err@(TypeError str) _ = err   --pass errors up
   inferAbBcAc _ err@(TypeError str) = err
   inferAbBcAc (Type (src1,trg1)) (Type (src2,trg2))
                   | elem Anything trg1 || elem Anything src2 = returnType
                   | intersect trg1 src2==[]                  = TypeError ("Type inference (a,b) -> (b,c) -> (a,c): Possible types of target1::b " ++ show trg1 ++ " do not match the possible types of source2::b " ++ show src2)
                   | otherwise                                = returnType
                   where returnType = Type (src1,trg2)

   --infer  e1::(a1,b1), e2::(a2,b2) a1>=a a2>=a b1>=b b2>=b |- e3::e1 -> e2 -> (a,b)
   --TODO guards kunnen vast mooier
   inferAbAbAb :: InferedType -> InferedType -> InferedType
   inferAbAbAb err@(TypeError str) _ = err   --pass errors up
   inferAbAbAb _ err@(TypeError str) = err
   inferAbAbAb (Type (src1,trg1)) (Type (src2,trg2))
                     --one of the targets and one of the sources is Anything
                   |     elem Anything src1 || elem Anything src2
                     &&  elem Anything trg1 || elem Anything trg2  = Type (intersectAnything src1 src2, intersectAnything trg1 trg2)
                     --only one of the sources isAnything
                   | elem Anything src1 || elem Anything src2  = Type (intersectAnything src1 src2, intersect trg1 trg2)
                     --only one of the targets is Anything
                   | elem Anything trg1 || elem Anything trg2  = Type (intersect src1 src2, intersectAnything trg1 trg2)
                   | intersect trg1 trg2==[]
                               = TypeError ("Type inference (a,b) -> (a,b) -> (a,b): Possible types of target1::b " ++ show trg1 ++ " do not match the possible types of target2::b " ++ show trg2)
                   | intersect src1 src2==[]
                               = TypeError ("Type inference (a,b) -> (a,b) -> (a,b): Possible types of source1::a " ++ show src1 ++ " do not match the possible types of source2::a " ++ show src2)
                     --the type is the intersections of sources and targets
                   | otherwise = Type (intersect src1 src2, intersect trg1 trg2)

   --union removes duplicates except when the duplicates are already in c1
   --TODO duplicates must be removed for predicatable type inference
   intersectAnything :: [Concept] -> [Concept] -> [Concept]
   intersectAnything c1 c2 = delete Anything (Data.List.union c1 c2)

   --infer  e1::(a,b) |- e2::e1 -> (b,a)                   
   inferAbBa :: InferedType -> InferedType
   inferAbBa err@(TypeError str) = err
   inferAbBa (Type (src,trg)) = Type (trg,src)

   --infer  e1::(a,b) |- e2::e1 -> (a,b)
   inferAbAb :: InferedType -> InferedType
   inferAbAb t = t

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

   {-   not used
   --search by declaration name and return the first one found
   srchDeclRel :: DeclRels -> String -> DeclRelFound
   srchDeclRel [] srchstr = NotFoundDr srchstr
   srchDeclRel (drl:drls) srchstr
             | case drl of Sgn{} -> (decnm drl == srchstr)
                                    = FoundDr drl
             | otherwise = srchDeclRel drls srchstr
   -}

   --search by Morphism (use mphats if specified, otherwise the mphnm)
   --TODO the show of Morphism does not display mphats, wouldn't that be more convenient?
   --TODO detect duplicate names searched without mphats
   srchDeclRelByMorphism :: DeclRels -> Morphism -> DeclRelFound
   srchDeclRelByMorphism [] morph = case morph of Mph{} -> NotFoundDr (show morph)
   srchDeclRelByMorphism (drl:drls) morph
             | case drl of 
                   Sgn{} -> case morph of
                      Mph{} -> if null (mphats morph)
                               then decnm drl == mphnm morph
                               else decnm drl == mphnm morph && desrc drl == head (mphats morph) &&  detgt drl == head (tail (mphats morph));
                      _ -> False
                                    = if null (mphats morph) && (containsDecl (decnm drl) drls)
                                      then NotFoundDr ("Ambiguous relation " ++ (mphnm morph) ++ " specify type explicitely (relation[a*b])")
                                      else FoundDr drl
             | case drl of
                   Sgn{} -> case morph of
                      Mph{} -> False    --This morphism is not this declaration, so go to otherwise to try next declaration
                      _ -> True
                                    = FoundDr (makeDeclaration morph)
             --TODO what about other declaration types and Mp1 morphisms?
             | otherwise            = srchDeclRelByMorphism drls morph
   
   containsDecl :: String -> DeclRels -> Bool
   containsDecl _ [] = False
   containsDecl name (d:ds) | case d of Sgn{} -> name == (decnm d) = True
                            | otherwise = containsDecl name ds
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


