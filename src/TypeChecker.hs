--Words in comments in written in capitals only provide a certain of information to programmers:
--     -> TODO                Describes some kind of improvement needed or things to be reexamined
--                            If reexamination results in the conclusion that its correct, then remove the comment
--     -> EXTEND              A place indicator defining the properties of potential functionality to be put there
--                            or properties to be persisted when adjusting functionality
--     -> DESCR               The description of the code block
--     -> USE                 Description of the usage of a function or (data) type
--     -> REMARK              Explicit comment for example to describe why some implementation choice has been made
--                            to prevent unnecessary reexamination and discussion
--     -> DEBUG               Code block which can be useful during development for example for debugging
--                            Single lines should be marked: -- code block --DEBUG, when inactive and:
--                            code block --DEBUG, when active
--                            Multiple line code blocks should be marked: --{-DEBUG \r\n code block \r\n ---} \r\n\,
--                            when active and: {-DEBUG \r\n code block \r\n }, when inactive
--                            All debug code blocks must be inactive when compiling for a release.
--     -> RULE                The implementation is correct whenever this rule holds. If the rule does not hold anymore
--                            then reexamine the implementation.

--TODO -> AGtry doet verschillende dingen
--        hij bouwt bijvoorbeeld de ISA op en de wereld en leidt regels af uit de hierarchy
--        als ik de typechecker eerst uitvoer en als er geen fouten zijn de AGtry, dan gaat die functionaliteit nooit verloren
--        de typechecker zal alleen bepaalde fouten eerder afvangen.
--TODO -> Put information in the trace to be able to present the user the reason why a type error has occurred
--        The phd thesis (book) of Bastiaan talks about this as an Explanation System (p.24)
--TODO -> meerdere fouten in expressie, dan binnenste fout, 1 per expressie
--
--DESCR ->
--         types are inferred bottom up. First the type of the morphisms is inferred, then the types of the expressions using them are inferred
--         subexpressions are evaluated from left to right if applicable (thus only for the union, intersection, semicolon, and dagger)
--TODO -> Checking sick.adl results in a lot of ambiguous relations in expressions, because
--        this type checker just puts all patterns of this context and the extended contexts of this context on a heap
--        apparantly there is another definition for the objects in scope than the definition implemented.
--        Check the correctness of handling context extension and patterns.
module TypeChecker (typecheck, Error, Errors) where

   import Adl         -- USE -> .MorphismAndDeclaration.makeDeclaration
                      --        and of course many data types
   import Data.List   -- USE -> unionBy

   ---------------
   --MAIN function
   ---------------

   --USE -> The error if is of type String and contains a complete error message
   --       This is the only type needed outside of the TypeChecker.
   type Errors = [Error]
   type Error = String

   --DESCR -> The parser composes an Architecture object. This function typechecks this object.
   --USE   -> This is the only function needed outside of the TypeChecker
   typecheck :: Architecture -> (Contexts, Errors)
   --typecheck _ = [] --DEBUG -> uncomment to disable typechecker
   --typecheck (Arch ctxs) = iwantastring (srchContext ctxs "Test")  --DEBUG
   typecheck arch@(Arch ctxs) =
                                (enrich ctxs,
                                --EXTEND -> put extra checking rules of the Architecture object here
                                --DESCR  -> check ctx name uniqueness, if that's ok then check the contexts
                                --TODO -> check circularity
                                checkCtxNameUniqueness ctxs ++||
                                checkCtxs arch ctxs   --TODO -> this list of errors is not distinct for SERVICES
                                )

   --TODO -> put extra information, derived from the patterns (and ???), in the contexts, like :
   --        Isa [] [] -> representing isa relations
   --        Rules -> active rules
   --        Declarations -> active declarations
   --        ObjectDefs   p.e. types of expressions
   enrich :: Contexts -> Contexts
   enrich ctxs = ctxs

   --DESCR -> check rule: Every context must have a unique name
   checkCtxNameUniqueness :: Contexts -> Errors
   checkCtxNameUniqueness [] = []
   checkCtxNameUniqueness (cx:ctxs) | elemBy eqCtx cx ctxs = (notUniqError cx):checkCtxNameUniqueness ctxs
                                    | otherwise    = checkCtxNameUniqueness ctxs
                                    where
                                    --DESCR -> return True if the names of the Contexts are equal
                                    eqCtx :: Context -> Context -> Bool
                                    eqCtx cx1 cx2 =
                                                case cx1 of Ctx{} -> (ctxnm cx1)
                                                ==
                                                case cx2 of Ctx{} -> (ctxnm cx2)
                                    --REMARK -> Context objects do not carry FilePos information
                                    notUniqError :: Context -> Error
                                    notUniqError cx' = case cx' of
                                                Ctx{} ->  "Context name " ++ (ctxnm cx')++ " is not unique"

   ------------------
   --Common functions
   ------------------

   infixl 6 ++&&
   infixl 6 ++||

   --DESCR -> same as ++
   --USE   -> use ++&& and ++|| to combine multiple checks
   (++&&) :: Errors -> Errors -> Errors
   (++&&) e1 e2 = e1 ++ e2

   --DESCR -> only return errors of the right check if left check did not have errors
   --USE   -> use ++&& and ++|| to combine multiple checks
   (++||) :: Errors -> Errors -> Errors
   (++||) [] e2 = e2 -- if left contains no Errors then return the errors of the right
   (++||) e1 _ = e1  -- if left contains Errors then return them and ignore the right

   --DESCR -> function elem provided with own equality function
   --USE   -> use when not instance Eq a or if another predicate is needed then implemented by instance Eq a
   elemBy :: (a->a->Bool)->a->[a]->Bool
   elemBy _ _ [] = False --not in list
   elemBy eq el (el':els) = (eq el el') || (elemBy eq el els)

   ------------------
   {- DEBUG
   --te gebruiken om de context of een deel daarvan als string op het scherm te krijgen
   --datatypes moeten wel een implementatie voor show hebben, dat hebben ze niet allemaal
   iwantastring :: ContextFound -> [String]
   iwantastring (NotFound str) = ("Context " ++ str ++ " could not be found"):[]
   iwantastring (Found (Ctx _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11))
       -- = (show _x1):(show _x2):(show _x3):(show _x4):(show _x5):(show _x6):(show _x7):(show _x8):(show _x9):(show _x11):[]
        = (show _x5):(show _x6):[]
   -}

------------------------------------------------------------------------------------------------
--Context part: later in separate module
------------------------------------------------------------------------------------------------

   --USE   -> The Environment is used to communicate ready to use input information for type checking
   --         The Environment is needed to transform to AdlExpr objects
   --DESCR -> The environment consists of:
   --          a dictionary containing the lowerbounds of the Concepts from the contexts in scope, the Concept is the key
   --          a list of all declared (direct) relations between two Concepts from the contexts in scope
   --          the contexts in scope, which will be the context under evaluation and its extended contexts (recursively)
   type Environment = (RelSet Concept, DeclRels, Contexts)

   --USE    -> The ContextCheckResult is needed to communicate the environment from a context and potential errors
   --REMARK -> From the environment only the Contexts containing the contexts in scope is used
   type ContextCheckResult = (Environment, Errors)
   data ContextFound = Found Context | NotFound Error

   --DESCR -> check all the Contexts. The Architecture is communicated to be able to search for (extended) contexts.
   checkCtxs :: Architecture -> Contexts -> Errors
   checkCtxs _ [] = []
   --DESCR -> Take the errors found when checking this context as root context and concat it with the errors of the other contexts taken as root
   --TODO -> context in 1x checken
   checkCtxs arch@(Arch ctxs) (ctx':tl_ctxs) = errors (check (Found ctx')) ++ (checkCtxs arch tl_ctxs)
      where
         errors :: ContextCheckResult -> Errors
         errors ((_,_,_),err) = err
         check :: ContextFound -> ContextCheckResult
         check (NotFound str) = ((RelSet [],[],[]),("Extended context " ++ str ++ " of context " ++ (case ctx' of Ctx{} -> ctxnm ctx') ++ " could not be found"):[]) --this case will not have been caught by the parser yet
         check (Found cx)
                     = checkThisCtx (constructEnv checkExtCtx cx)
                     where
                         --DESCR -> get the list of extended Context
                         --         check all extended Context in the list (all siblings)
                         --         merge the results (classification, relations, errors)
                         checkExtCtx :: ContextCheckResult
                         checkExtCtx = case cx of Ctx{} -> foldr concatRes ((RelSet [], [], []),[]) (map check (map (srchContext ctxs) ( ctxon cx) ))
                         --DESCR -> Enrich the Environment of the extended contexts with patterns from the current context
                         constructEnv :: ContextCheckResult -> Context -> ContextCheckResult
                         constructEnv ((_,_,extCtxs),errs) cx' -- @(Ctx nm _ _ _ _ _ _ _ _ _ _) --DEBUG
                                    = (
                                         ( isaRel (allCtxConcepts (cx':extCtxs)) (allCtxGens (cx':extCtxs)),
                                           declRels (allCtxPats (cx':extCtxs)),
                                           cx':extCtxs
                                         ),
                                         errs
                                       )
                                --   = ((RelSet [],[],[]),[show ( isaRel
                                  --                         (allCtxConcepts (cx':extCtxs))
                                    --                       (allCtxGens (cx':extCtxs))
                                      --                  )])
                                   -- =((RelSet [], [],[]),[show (lowerboundsOfs (allCtxGens (cx:extCtxs)) )]) --DEBUG to show lowerboundsOfs
                                   -- =((RelSet [], [],[]),[show (declRels    (allCtxPats (cx:extCtxs)) )]) --DEBUG to show declRels
                                   -- \| nm=="Test2" = ((lowerboundsOfs (allCtxGens (cx:extCtxs)),declRels (allCtxPats (cx:extCtxs)),cx:extCtxs),[])  --DEBUG
                                   -- \| otherwise   = ((RelSet [], [], []),[show (lowerboundsOfs (allCtxGens (cx:extCtxs)))])  --DEBUG of extends Test2
                                   -- \| otherwise   = ((RelSet [], [], []),[show (declRels    (allCtxPats (cx:extCtxs)))])  --DEBUG of extends Test2

   --DESCR -> search for a context by name and return the first one found
   srchContext :: Contexts -> String -> ContextFound
   srchContext [] srchstr = NotFound srchstr
   srchContext (cx:ctxs) srchstr
            | case cx of Ctx{} -> (ctxnm cx==srchstr)
                                  = Found cx
            | otherwise = srchContext ctxs srchstr

   --DESCR -> Check what needs to be checked on a context
   checkThisCtx :: ContextCheckResult -> ContextCheckResult
   --DESCR -> abort when there are errors from previous steps
   checkThisCtx ccr@(_,_:_)       = ccr
   --DESCR -> resolve the type and check if the arguments are of such a type
   checkThisCtx (env@(_,_,ctxs),_) =
                            --DESCR -> combine all errors of things to check like objectdefs and rules
                            (env,        --TODO -> is this redundant
                                  checkObjDefs env (allCtxObjDefs ctxs) ++&&
                                  checkRules env (allCtxRules ctxs)
                            )

   --TODO -> replace by function checkTypes :: Checkable a => Environment -> a -> Errors
   --DESCR -> abstract expressions from all objectdefs (castObjectDefsToAdlExprs) and infer their types (inferWithInfo)
   --         Then check the result (processResult)
   --         Return the list of error strings
   checkObjDefs :: Environment -> ObjectDefs -> Errors
   --checkObjDefs env (obj:objs) = case obj of Obj{} -> [show (castObjectDefsToAdlExprs env [obj] 0)] --DEBUG
   checkObjDefs env@(universe,_,_) objs =
                               (processResult1
                                       (map (inferWithInfo universe) (castObjectDefsToAdlExprs env objs 0))
                                  )

   --DESCR -> abstract expressions from all objectdefs (castObjectDefsToAdlExprs) and infer their types (inferWithInfo)
   --         Then check the result (processResult)
   --         Return the list of error strings
   checkRules :: Environment -> Rules -> Errors
   --checkRules env rules =  [(show (castRulesToAdlExprs env rules))] --DEBUG
   checkRules env@(universe,_,_) ruls = (processResult2 (map (inferWithInfo universe) (castRulesToAdlExprs env ruls)))


   ------------------------------
   --cumulative context functions
   --TODO -> assumption, names are unqualified and unique within the context and its extended contexts
   --             if names must be qualified, then change the names of the components in
   --             the patterns to qualified names (p.e. TestContext.concept1 instead of concept1)
   ------------------------------

   --DESCR -> Merge two ContextCheckResult objects
   --USE   -> This function is only used to combine two sibling, extended contexts
   --RULE  -> The LowerboundsOfs and DeclRels from the Environment are always recomputed
   --        based on the Contexts in the Environment resulting from this function
   concatRes :: ContextCheckResult -> ContextCheckResult -> ContextCheckResult
   concatRes ((_,_,cxs1),errs1) ((_,_,cxs2),errs2) | errs1==[] && errs2==[]
                                                              = ((RelSet [], [], cxs1 ++ cxs2),[])
                                                  | otherwise = ((RelSet [],[],[]),errs1 ++ errs2)

   --DESCR -> all the Gens of Contexts
   allCtxGens :: Contexts -> Gens
   allCtxGens [] = []
   allCtxGens (cx:ctxs) = case cx of Ctx{} -> allPatGens (ctxpats cx) ++ allCtxGens ctxs

   --DESCR -> all the Gens of patterns
   allPatGens :: Patterns -> Gens
   allPatGens [] = []
   allPatGens (p:ps)  = case p of Pat{} -> ptgns p ++ allPatGens ps

   --DESCR -> all the patterns of contexts
   allCtxPats :: Contexts -> Patterns
   allCtxPats [] = []
   allCtxPats (cx:ctxs) = case cx of Ctx{} -> ctxpats cx ++ allCtxPats ctxs

   --DESCR -> all the ObjectDefs of Contexts
   allCtxObjDefs :: Contexts -> ObjectDefs
   allCtxObjDefs [] = []
   allCtxObjDefs (cx:ctxs) = case cx of Ctx{} -> ctxos cx ++ allCtxObjDefs ctxs

   --DESCR -> all the Rules of Contexts
   allCtxRules :: Contexts -> Rules
   allCtxRules [] = []
   allCtxRules (cx:ctxs) = case cx of Ctx{} -> ctxrs cx ++ allPatRules (ctxpats cx) ++ allCtxRules ctxs

   --DESCR -> all the Gens of patterns
   allPatRules :: Patterns -> Rules
   allPatRules [] = []
   allPatRules (p:ps)  = case p of Pat{} -> ptrls p ++ allPatRules ps
   
   allCtxConcepts :: Contexts -> Concepts
   allCtxConcepts ctxs = foldr cptinsert []
                            (allDeclConcepts (declRels (allCtxPats ctxs)) ++
                             allGenConcepts (allCtxGens ctxs)
                             )
                         where
                         cptinsert :: Concept -> Concepts -> Concepts
                         cptinsert cp cpts | elem cp cpts = cpts
                                           | otherwise    = cp:cpts
   
   allDeclConcepts :: DeclRels -> Concepts
   --allDeclConcepts [] = []
   allDeclConcepts dls = [case d of Sgn{} -> desrc d | d<-dls] ++
                         [case d of Sgn{} -> detgt d | d<-dls]

   allGenConcepts :: Gens -> Concepts
   --allGenConcepts [] = []
   allGenConcepts gens = [case g of G{} -> gengen g | g<-gens] ++
                         [case g of G{} -> genspc g | g<-gens]

---------------------------------------------------------------------------------------------
--Meta information part: later in separate module
---------------------------------------------------------------------------------------------

   ----------------------------------------------------
   --generic meta information structures and functions
   ----------------------------------------------------

   --USE -> generic type to communicate a meta information structure with an object a
   data MetaInfo info a = Info info (Trace a)  deriving (Show)
   data Trace a = Trace [String] a
   
   instance Show (Trace a) where
       showsPrec _ (Trace [] _)     = showString ""
       showsPrec _ (Trace (x:xs) subj) = showString (x ++ show (Trace xs subj))

   --DESCR -> infer the type of an AdlExpr maintaining the link to the meta information
   --TODO -> put the trace down to the type inferer?
   inferWithInfo :: RelSet Concept -> MetaInfo a AdlExpr -> MetaInfo a RelationType
   inferWithInfo universe (Info info (Trace trc expr1)) = Info info (Trace trc (checkInferred (infer universe expr1)))
          where 
          checkInferred (TypeError t err) = TypeError t (errInExpr expr1 err)
          checkInferred t = t

   --DESCR -> function to write trace lines
   --EXTEND -> always use this function for writing trace lines to be able to easily change the implementation
   --          for writing trace lines, for example reversing the order of trace lines in a trace
   writeTrcLn :: String -> [String] -> a -> Trace a
   writeTrcLn ln trc x = Trace (trc ++ [ln,"\n--------\n"]) x -- Trace (ln:trc) x

   errInExpr :: AdlExpr -> Error -> Error
   errInExpr ex err = "\nError in expression: " ++ show ex
                           ++"\n"
                           ++err

   --removeInfo :: MetaInfo info a -> a
   --removeInfo (Info _ x) = x

   ----------------------------------------------------
   --specific meta information structures and functions
   ----------------------------------------------------

   --USE -> MetaInfo of this type is used for abstracting and checking expressions from ObjectDefs
   type MetaInfo1 a = MetaInfo (FilePos, Depth) a
   type Depth = Int

   --DESCR -> Process type inference results of ObjectDefs
   --         If a type is inferred, then it's ok. In case of a TypeError return the error.
   processResult1 :: [MetaInfo1 RelationType] -> Errors
   processResult1 [] = []
   processResult1 ((Info (posi,_) trc@(Trace _ (TypeError t err)):ts)) = ((compose t err posi)++"\nTRACE\n"++(show trc)++"\nENDTRACE\n\n"):(processResult1 ts)
   processResult1 ((Info _ (Trace _ (RelationType _)):ts)) = processResult1 ts

   --USE -> MetaInfo of this type is used for checking Rules
   type MetaInfo2 a = MetaInfo (FilePos) a

   --DESCR -> Process type inference results of Rules
   --         If a type is inferred, then it's ok. In case of a TypeError return the error.
   processResult2 :: [MetaInfo2 RelationType] -> Errors
   processResult2 [] = []
   processResult2 ((Info (posi) trc@(Trace _ (TypeError t err)):ts)) = ((compose t err posi)++"\nTRACE\n"++(show trc)++"\nENDTRACE\n\n"):(processResult2 ts)
   processResult2 ((Info _ (Trace _(RelationType _)):ts))  = processResult2 ts
   
   --DESCR -> Combine code position information and an error string
   compose :: TypeErrorType ->Error -> FilePos -> Error
   compose RTE_Fatal         err posi = "\nError at " ++ show posi ++ "\nFATAL ERROR while infering types: " ++ err ++ "\n"
   compose (RTE_DeclError t) err posi = "\nError at " ++ show posi ++ "\nThere is a problem finding the declaration of a relation:\n\t" ++ composeNFD t err ++ "\n"
   compose (RTE_ExprError t) err posi = "\nError at " ++ show posi ++ "\nThere is a problem interpreting an expression:\n\t" ++ composeEE t err ++ "\n"
   compose RTE_AbAbAb        err posi = "\nError at " ++ show posi ++ "\nType inference (a,b) -> (a,b) -> (a,b): " ++ err ++ "\n"
   compose RTE_AbBcAc        err posi = "\nError at " ++ show posi ++ "\nType inference (a,b) -> (b,c) -> (a,c): " ++ err ++ "\n"
   compose RTE_AaAa          err posi = "\nError at " ++ show posi ++ "\nType inference (a,a) -> (a,a): " ++ err ++ "\n"
   --compose _                err posi = "\n Error at " ++ show posi ++ "\n" ++ err ++ "\n"

   composeNFD :: NotFoundDrType -> Error -> Error
   composeNFD NFD_Fatal err = "FATAL ERROR: " ++ err
   composeNFD _         err = err

   composeEE :: ExprErrorType -> Error -> Error
   composeEE EE_Fatal   err = "FATAL ERROR: " ++ err
   composeEE EE_SubExpr err = "Error in subexpression: " ++ err

---------------------------------------------------------------------------------------------
--Expression part: later in separate module
--DESCR  -> This module connects the ADL tool to the type inference module by means of the InferExpr data type
--EXTEND -> If something changes in the ADL tool this is the module to adapt to changes
--          In other words: the inference module should be decoupled by this module
---------------------------------------------------------------------------------------------

   ------------------------
   --Cast functions
   --for casting ADL module
   --data type to InferExpr
   ------------------------

   --DESCR -> Cast all objectdefs based on the environment to a list of AdlExprs
   --         The AdlExprs are linked to meta data
   --         The depth is used to be able to compose AdlExprs from nested objectdefs
   --         The AdlExprs abstracted are:
   --                1) all the isolated expression, one from each objectdef
   --                2) all the nested objectdefs cast as AdlExprs
   --                3) the cartesian product of an isolated expression of an objectdef
   --                   combined with the nested objectdefs of that objectdef cast as AdlExprs
   --                   each combination is a Semicolon AdlExpr
   castObjectDefsToAdlExprs :: Environment -> ObjectDefs -> Depth ->  [MetaInfo1 AdlExpr] -- [(AdlExpr,MetaInfo)]
   castObjectDefsToAdlExprs _ [] _ = []
   castObjectDefsToAdlExprs env@(universe,_,_) (obj:objs) currdepth = case obj of
                                   Obj{} -> if null (objats obj)
                                            then
                                                  --DESCR -> add this objdef as AdlExpr for evaluation
                                                  (Info (objpos obj,currdepth) (writeTrcLn composetrace [] thisAsAdlExpr) )
                                                  --DESCR -> add the sibling objdefs as AdlExpr for evaluation
                                                  :(castObjectDefsToAdlExprs env objs currdepth)
                                            else
                                                  --DESCR -> add the nested objdefs as AdlExpr for evaluation
                                                  (castObjectDefsToAdlExprs env (objats obj) (currdepth+1))
                                                  --DESCR -> add this objdef as AdlExpr for evaluation
                                                  ++ [Info (objpos obj,currdepth) (writeTrcLn composetrace [] thisAsAdlExpr)]
                                                  --DESCR -> add the nested objDefs combined with this objdef as AdlExpr for evaluation
                                                  ++ (map
                                                       (combineObjDefs universe thisAsAdlExpr currdepth)
                                                       (castObjectDefsToAdlExprs env (objats obj) (currdepth+1))
                                                  )
                                                  --DESCR -> add the sibling objdefs as AdlExpr for evaluation
                                                  ++ (castObjectDefsToAdlExprs env objs currdepth)
                                            where
                                                  thisAsAdlExpr = castExpressionToAdlExpr env (objctx obj)
                                                  composetrace =
                                                       ("Validating type of subexpression (expr1) in a SERVICE on depth " ++
                                                       show currdepth ++ " :\n" ++
                                                       "=> expr1 -> " ++ show thisAsAdlExpr ++ " of type " ++ show (infer universe thisAsAdlExpr)
                                                       )

   --DESCR -> given the current depth, combine the subexpression from the current objectdef
   --         with a nested object def as AdlExpr with MetaInfo1 to a new AdlExpr with MetaInfo1
   combineObjDefs :: RelSet Concept -> AdlExpr -> Depth -> MetaInfo1 AdlExpr -> MetaInfo1 AdlExpr
   combineObjDefs universe obj currdepth expr1@(Info (posi,depth) (Trace trc nestedobj))
                         --DESCR -> only combine if the nested expr is one depth lower then the current depth
                         --         put the combined expr on the current depth
                         --         link the combined expr to the file position of the nested expr
                         --TODO  -> filter to prevent duplicates not complete yet
                         --         I think this is the place to filter, because this is the place where
                         --         expressions are copied, and thus the place where potential errors are duplicated.
                         --         MetaInfo1 contains the Depth of the AdlExpr in an objectdef
                         --         Because subexpressions will be evaluated more then once, incorrect exprs will
                         --         result in multiple reporting of the same error. The error from the deepest AdlExpr
                         --         has the most precise MetaInfo (at the time of writing only the FilePos)
                         --         I can also just display pointing at the fact that this error is a result of another error (the more the better?)
                       | (currdepth+1)==depth = --if (isError (infer obj)) || (isError (infer nestedobj))
                                                --then Info (posi,currdepth) (Trace [] (ExprError EE_SubExpr "Parent or nested SERVICE contains type error."))
                                                --else
                                                Info (posi,currdepth) ((writeTrcLn composetrace trc) (Semicolon obj nestedobj))
                       | otherwise            = expr1
                                                where composetrace =
                                                       ("Combining subexpression (expr1) in a SERVICE on depth " ++ show depth ++
                                                       " with a nested subexpression (expr2) to a new expression expr1;expr2 (expr3) for type validation:\n" ++
                                                       "=> expr1 -> " ++ show obj       ++ " of type " ++ show (infer universe obj)       ++ "\n" ++
                                                       "=> expr2 -> " ++ show nestedobj ++ " of type " ++ show (infer universe nestedobj) ++ "\n" ++
                                                       "=> expr3 has type " ++ show (infer universe (Semicolon obj nestedobj))
                                                       )


   --DESCR  -> cast the rule to an AdlExpr
   --TODO   -> more guards for different rules
   --RULE   -> SJ: Ja, een regel is een expressie. De regel a|-c is hetzelfde als de expressie -a\/c.
   --          a b c  1 2 3      V=a1a2a3b1b2b3c1c2c3    a=a1a2a3 c=a1a3b1c3  => b1b3 are the rule violating instances
   --          SJ: Het type van een regel is het type van de equivalente expressie, namelijk  typeOf a `lub` typeOf c (aannemende dat typeOf het type van een expressie bepaalt)
   castRulesToAdlExprs :: Environment -> Rules -> [MetaInfo2 AdlExpr]
   castRulesToAdlExprs _ [] = []
   castRulesToAdlExprs env@(universe,_,_) (rul:ruls)
                     | case rul of
                              Ru{} -> (rrsrt rul == Implication);
                              _ -> False
                                            = (Info
                                                 --DESCR -> file position of rule
                                                 (rrfps rul)
                                                 (writeTrcLn composetrace1 [] castimplication)
                                               ):(castRulesToAdlExprs env ruls)
                     | case rul of
                              Ru{} -> (rrsrt rul == Equivalence);
                              _ -> False
                                            = (Info
                                                 --DESCR -> file position of rule
                                                 (rrfps rul)
                                                 (writeTrcLn composetrace2 [] castequivalence)
                                               ):(castRulesToAdlExprs env ruls)
                 {-
                     | case rul of
                              Sg{} -> True
                              _ -> False
                                            = (Info posNone (Trace [] (ExprError EE_Fatal "Rule type Sg not implemented.")):(castRulesToAdlExprs env ruls))
                     | case rul of
                              Gc{} -> True
                              _ -> False
                                            = (Info posNone (Trace [] (ExprError EE_Fatal "Rule type Gc not implemented.")):(castRulesToAdlExprs env ruls))
                     | case rul of
                              Fr{} -> True
                              _ -> False
                                            = (Info posNone (Trace [] (ExprError EE_Fatal "Rule type Fr not implemented.")):(castRulesToAdlExprs env ruls))
                 -}
                     | otherwise            = (Info posNone (Trace [] (ExprError EE_Fatal "Unknown rule type.")):(castRulesToAdlExprs env ruls))
                        where
                            leftsubexpr = castExpressionToAdlExpr env (rrant rul)
                            rightsubexpr = castExpressionToAdlExpr env (rrcon rul)
                                                   -- left|-right => -left\/right
                            castimplication = Implicate leftsubexpr rightsubexpr
                            composetrace1 =
                               ("ERROR IN RULE ->\n" ++
                                show  castimplication ++ "\n" ++
                                --TODO -> show rul is ugly "Translating rule " ++ show rul ++ " resulting in expression -" ++ show (rrant rul) ++ "\\/" ++ show (rrcon rul) ++ "\n" ++
                                "=> subexpr1 -> " ++ show leftsubexpr  ++ " has type " ++ show (infer universe leftsubexpr) ++ "\n" ++
                                "=> subexpr2 -> " ++ show rightsubexpr ++ " has type " ++ show (infer universe rightsubexpr) ++ "\n" ++
                                traceruleerror (infer universe castimplication)
                               )
                            castequivalence = Equality leftsubexpr rightsubexpr
                            composetrace2 =
                               ("ERROR IN RULE ->\n" ++
                                show castequivalence ++ "\n" ++
                                --TODO -> show rul is ugly "Translating rule " ++ show rul ++ " resulting in expression -" ++ show (rrant rul) ++ "\\/" ++ show (rrcon rul) ++ "\n" ++
                                "=> subexpr1 -> " ++ show leftsubexpr  ++ " has type " ++ show (infer universe leftsubexpr) ++ "\n" ++
                                "=> subexpr2 -> " ++ show rightsubexpr ++ " has type " ++ show (infer universe rightsubexpr) ++ "\n" ++
                                traceruleerror (infer universe castequivalence)
                               )
                            traceruleerror (TypeError RTE_AbAbAb err)
                                            = "ERROR DESCRIPTION -> subexpr1 does not match type of subexpr2:\n" ++ err
                            traceruleerror _ = ""


   --RULE -> The parser translates expressions with a flip on subexpressions to an expressions
   --        with only flips on morphisms of type Mph for example (r;s)~ is parsed as s~;r~
   --RULE -> flips on Universe V[A*B] will be returned by the parser as V[B*A]
   castExpressionToAdlExpr :: Environment -> Expression -> AdlExpr
   castExpressionToAdlExpr (_,declrels,_) (Tm morph@(Mph{}))
                                               = doNotFlip (mphyin morph)
                                                            (typeofRel
                                                                  (srchDeclRelByMorphism declrels morph)
                                                            )

                         where
                            doNotFlip:: Bool -> AdlExpr -> AdlExpr
                            doNotFlip False expr1@(Relation (RelationType _)) = Flip expr1
                            doNotFlip _     expr1                     = expr1
   castExpressionToAdlExpr (_,declrels,_) (Tm morph) --RULE -> other Morphisms (I and V etc do not need to be flipped)
                                               = typeofRel
                                                      (srchDeclRelByMorphism declrels morph)
   castExpressionToAdlExpr env (Tc expr1)       = castExpressionToAdlExpr env expr1
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
   castExpressionToAdlExpr env (K0 expr1)       = TrsRefClose (castExpressionToAdlExpr env expr1)
   castExpressionToAdlExpr env (K1 expr1)       = TrsClose (castExpressionToAdlExpr env expr1)
   castExpressionToAdlExpr env (Cp expr1)       = Complement (castExpressionToAdlExpr env expr1)
   castExpressionToAdlExpr _ _ = ExprError EE_Fatal "Cannot cast to AdlExpr. "

   --EXTEND -> Loose the declarations of ISA and relations, and other ADL tool specifics, by already infering a type
   typeofRel :: DeclRelFound -> AdlExpr
   typeofRel (NotFoundDr t err) = Relation (TypeError (RTE_DeclError t) err)
   typeofRel (FoundDr d) = case d of
                    -- _ -> TypeError (show lbos) ; --DEBUG
                    Sgn{} -> Relation (RelationType ( desrc d, detgt d ));
                    --TODO -> why is there a despc and degen?
                    Isn{} -> Identity (RelationType ( despc d, despc d ));
                    --REMARK -> Vs degen is the source Concept, Vs despc the target Concept
                    Vs {} -> Universe (RelationType ( degen d, despc d ));
                    --TODO   -> when will there be an IsCompl?
                    --REMARK -> IsCompl{} will never be the result of ADL.MorphismAndDeclaration.makeDeclaration
                    --          makeDeclaration is used in srchDeclRelByMorphism
                    _ -> Relation( TypeError RTE_Fatal ("Unknown Declaration constructor. "))

---------------------------------------------------------------------------------------------
--Type inference part: later in separate module
--PATTERN Type16Error
--GEN XX ISA Medewerker
--GEN XX ISA Document
--van         :: Toegangsrecht * Medewerker.
--op          :: Toegangsrecht * Document.
-- van~;op |- I[XX]            ---> AGtry CORRECT
-- van~;op |- I                ---> AGtry INCORRECT, maar zou moeten betekenen van~;op is een subset van I[Anything]
--                                  deze rule zou heel veel violations moeten retourneren
--                                  het type van de rule zou (Anything,Anything) moeten zijn.
-- -(van~;op) \/ I |- V        ---> AGtry CORRECT (even if van~;op contains a type error)
--ENDPATTERN
--
--DESCR -> a (concept)type is a set of concepttypes
--         the set consists of all concepttypes lower than or equal to the concepttype
--         a>=b indicates that concepttype b is a subset of concepttype a
--         thus if a concepttype is part of type b then it is also part of concepttype a
--         Anything = top = the set of all concepttypes, 
--         Nothing = bottom = the set with no concepttypes
--      1) Id of a concepttype
--                              |- c::a
--      2) if concepttype b is a subset of concepttype a then concept c1 of type a is also a concept of type b
--         c1::a, c2::b, a>=b   |- c1::a -> b
--      3) a relation expression can be defined given concepttypes a and b, the type is (a,b)
--         c1::a, c2::b         |- e::def[c1,c2] -> (a,b)
--      4) the universal relation expression can be defined given concepttypes a and b, the type is (a,b)
--         c1::a, c2::b         |- e::V[c1,c2]   -> (a,b)
--      5) the identity relation expression can be defined given concepttype a, the type is (a,a)
--         c::a                 |- e::I[c]       -> (a,a)
--      6) the composition expression can be defined given expression e1 and e2
--         if there is a concepttype b which is a subset of concepttype b1 and b2, the type is (a,c)
--         e1::(a,b1), e2::(b2,c), b1>=b, b2>=b, not b=bottom |- e3::e1;e2 -> (a,c)
--      7) the relative addition expression can be defined given expression e1 and e2
--         if there is a concepttype b which is a subset of concepttype b1 and b2, the type is (a,c)
--         e1::(a,b1) , e2::(b2,c)  b1>=b b2>=b, not b=bottom |- e3::e1!e2 -> (a,c)
--      8) the union expression can be defined given expression e1 and e2
--         if concepttype a1 and a2 are subsets of concepttype a AND
--         if concepttype b1 and b2 are subsets of concepttype b, the type is (a,b)
--         e1::(a1,b1), e2::(a2,b2) a>=a1 a>=a2 b>=b1 b>=b2 |- e3::e1\/e2 -> (a,b)
--      9) the intersection expression can be defined given expression e1 and e2
--         if concepttype a1 and a2 are subsets of concepttype a AND
--         if concepttype b1 and b2 are subsets of concepttype b, the type is (a,b)
--         e1::(a1,b1), e2::(a2,b2) a1>=a a2>=a b1>=b b2>=b |- e3::e1/\e2 -> (a,b)
--     10) the flip expression can be defined given expression e1, the type is (b,a)
--         e1::(a,b)            |- e2::e1~     -> (b,a)
--     11) the complement expression can be defined given expression e1, the type is (a,b)
--         e1::(a,b)            |- e2::-e1     -> (a,b)
--     12) the reflexive, transitive closure expression can be defined given expression e1, the type is (a,b)
--         e1::(a,b)            |- e2::e1*     -> (a,b)
--     13) the transitive closure expression can be defined given expression e1, the type is (a,b)
--         e1::(a,b)            |- e2::e1+     -> (a,b)
---------------------------------------------------------------------------------------------

   --USE ->  Store the type of an expression or a type error
   data RelationType = RelationType (Concept, Concept) | TypeError TypeErrorType Error

   instance Show RelationType where
       showsPrec _ (RelationType t)     = showString (show t)
       showsPrec _ (TypeError t err)    = showString ("TypeError " ++ show t ++ err)

   --USE -> A type error can result from:
   --          - type inference rules ab->ab->ab or ab->bc->ac
   --          - a fatal error during type inference
   --          - an expression which cannot be interpreted
   --          - a problem with finding a declaration of a relation
   data TypeErrorType = RTE_AaAa | RTE_AbAbAb | RTE_AbBcAc | RTE_ExprError ExprErrorType | RTE_DeclError NotFoundDrType | RTE_Fatal deriving (Show)

   --USE -> Relation will be the only expression already inferred possibly containing a TypeError
   data AdlExpr =   Relation    RelationType  --USE -> use typeofRel to get the RelationType
                  | Implicate   {ex1::AdlExpr, ex2::AdlExpr}
                  | Equality    {ex1::AdlExpr, ex2::AdlExpr}
                  | Complement  {ex1::AdlExpr}
                  | Flip        {ex1::AdlExpr}
                  | Union       {ex1::AdlExpr, ex2::AdlExpr}
                  | Intersect   {ex1::AdlExpr, ex2::AdlExpr}
                  | Semicolon   {ex1::AdlExpr, ex2::AdlExpr}
                  | Dagger      {ex1::AdlExpr, ex2::AdlExpr}
   --TODO -> why can't I specify an I or V for a Relation? Why are I and V morphisms and not expressions?
                  | Identity    RelationType
                  | Universe    RelationType        --TODO -> this is not in table in article
                  | TrsClose    {ex1::AdlExpr}      --TODO -> this is not in table in article
                  | TrsRefClose {ex1::AdlExpr}      --TODO -> this is not in table in article
                  | ExprError ExprErrorType Error  -- deriving (Show)

   data ExprErrorType = EE_SubExpr | EE_Fatal deriving (Show)
   
   instance Show AdlExpr where
       showsPrec _ (Relation (TypeError _ _)) = showString "<error>" --DESCR -> override show of TypeError
       showsPrec _ (Identity (TypeError _ _)) = showString "<error>" --DESCR -> override show of TypeError
       showsPrec _ (Universe (TypeError _ _)) = showString "<error>" --DESCR -> override show of TypeError
       --showsPrec _ t@(Relation _)    = showString (show t)
       showsPrec _ (Relation (RelationType t))     = showString (show t)
       showsPrec _ (Identity (RelationType t))     = showString ("I[" ++ show t ++ "]")
       showsPrec _ (Universe (RelationType t))     = showString ("V[" ++ show t ++ "]")
       showsPrec _ (Semicolon e1 e2) = showString (show e1 ++ ";" ++ show e2)
       showsPrec _ (Dagger e1 e2)    = showString (show e1 ++ "!" ++ show e2)
       showsPrec _ (Union e1 e2)     = showString (show e1 ++ "\\/" ++ show e2)
       showsPrec _ (Intersect e1 e2) = showString (show e1 ++ "/\\" ++ show e2)
       showsPrec _ (Flip e1)         = showString (show e1 ++ "~")
       showsPrec _ (TrsClose e1)     = showString (show e1 ++ "+")
       showsPrec _ (TrsRefClose e1)  = showString (show e1 ++ "*")
       showsPrec _ (Complement e1)   = showString ("-" ++ show e1)
       showsPrec _ (Implicate e1 e2) = showString (show e1 ++ "|-" ++ show e2)
       showsPrec _ (Equality e1 e2)   = showString (show e1 ++ "=" ++ show e2)
       showsPrec _ (ExprError _ err) = showString err

   --TODO -> check the expressions as a whole and not just the subexpressions
   --        I have to change to infer all possible types, and then check if only one type is inferred
   infer :: RelSet Concept -> AdlExpr -> RelationType
   infer _ (Relation rel) = inferAbAb rel
   infer _ (Universe rel) = inferAbAb rel
   infer _ (Identity rel) = inferAaAa rel
                                --TODO -> check for equality of source and target
   infer isarel (Semicolon expr1 expr2) = inferAbBcAc isarel (infer isarel expr1) (infer isarel expr2)
   infer isarel (Dagger expr1 expr2) = inferAbBcAc isarel (infer isarel expr1) (infer isarel expr2)
   infer isarel (Union expr1 expr2) = extInferAbAbAb isarel expr1 expr2
   infer isarel (Intersect expr1 expr2) = extInferAbAbAb isarel expr1 expr2
   infer isarel (Implicate expr1 expr2) = extInferAbAbAb isarel expr1 expr2
   infer isarel (Equality expr1 expr2)  = extInferAbAbAb isarel expr1 expr2
   infer isarel (Flip expr1) = inferAbBa (infer isarel expr1)
   infer isarel (Complement expr1) = inferAbAb (infer isarel expr1)
   infer isarel (TrsClose expr1) = inferAbAb (infer isarel expr1)
   infer isarel (TrsRefClose expr1) = inferAbAb (infer isarel expr1)
   infer _ (ExprError t err)= TypeError (RTE_ExprError t) err --The expression is already known to be unknown

   extInferAbAbAb isarel expr1@(Identity rel) expr2 = inferAaAaAa isarel (infer isarel expr1) (infer isarel expr2)
   extInferAbAbAb isarel expr1 expr2@(Identity rel) = inferAaAaAa isarel (infer isarel expr1) (infer isarel expr2)
   extInferAbAbAb isarel expr1 expr2                = inferAbAbAb isarel (infer isarel expr1) (infer isarel expr2)

   --DESCR -> infer  e1::(a,b1), e2::(b2,c) b1>=b b2>=b |- e3::e1 -> e2 -> (a,c)
   inferAbBcAc :: RelSet Concept -> RelationType -> RelationType -> RelationType
   inferAbBcAc _ err@(TypeError _ _) _ = err   --pass errors up
   inferAbBcAc _ _ err@(TypeError _ _) = err
   inferAbBcAc isarel (RelationType (a,b1)) (RelationType (b2,c))
             | diamond isarel b1 b2 = (RelationType (a,c))
             | otherwise              = TypeError RTE_AbBcAc ("The target of the left expression ("++ show b1 ++") does not match the source of the right expression ("++ show b2 ++")\n")

   --DESCR -> infer  e1::(a1,b1), e2::(a2,b2) a1>=a a2>=a b1>=b b2>=b |- e3::e1 -> e2 -> (a,b)
   inferAbAbAb :: RelSet Concept -> RelationType -> RelationType -> RelationType
   inferAbAbAb _ err@(TypeError _ _) _                = err   --DESCR -> pass errors up
   inferAbAbAb _ _ err@(TypeError _ _)                = err
   inferAbAbAb isarel (RelationType (a,b)) (RelationType (p,q))
             | diamond isarel a p
               && diamond isarel b q    = (RelationType (lubcpt isarel a p,lubcpt isarel b q))
             | not (diamond isarel a p) = TypeError RTE_AbAbAb ("The source of the left expression (" ++ show a ++ ") does not match the source of the right expression (" ++ show p ++ ")\n")
             | otherwise                = TypeError RTE_AbAbAb ("The source of the left expression (" ++ show b ++ ") does not match the source of the right expression (" ++ show q ++ ")\n")

   --DESCR -> infer  e1::(a,b) |- e2::e1 -> (b,a)
   inferAbBa :: RelationType -> RelationType
   inferAbBa err@(TypeError _ _) = err
   inferAbBa (RelationType (src,trg)) = RelationType (trg,src)

   --DESCR -> infer  e1::(a,b) |- e2::e1 -> (a,b)
   inferAbAb :: RelationType -> RelationType
   inferAbAb t = t

   inferAaAaAa :: RelSet Concept -> RelationType -> RelationType -> RelationType
   inferAaAaAa _ err@(TypeError _ _) _                = err   --DESCR -> pass errors up
   inferAaAaAa _ _ err@(TypeError _ _)                = err
   inferAaAaAa isarel t1@(RelationType (a,b)) t2@(RelationType (p,q))
                                        | a==b &&
                                          p==q      = (RelationType (lubcpt isarel a p,lubcpt isarel a p))
                                        | not(a==b) = TypeError RTE_AaAa ("Left expression: Source does not equal target -> source: " ++ show a ++ " target: " ++ show b ++ "\n")
                                        | otherwise = TypeError RTE_AaAa ("Right expression: Source does not equal target -> source: " ++ show p ++ " target: " ++ show q ++ "\n")

   --DESCR -> infer  e1::(a,a) |- e2::e1 -> (a,a)
   --REMARK -> this function is only used for Identities, Identities are constructed in such a way that always src==trg
   --          so the otherwise has never been hit in a test
   inferAaAa :: RelationType -> RelationType
   inferAaAa err@(TypeError _ _) = err
   inferAaAa t@(RelationType (src,trg)) | src==trg  = t
                                        | otherwise = TypeError RTE_AaAa ("Source does not equal target -> source: " ++ show src ++ " target: " ++ show trg ++ "\n")

---------------------------------------------------------------------------------------------

   data RelSet a = RelSet [(a,a)] deriving (Show)

   --DESCR -> if is in isaRel then predicate isa is true. reflects axiom 15-19
   --         reflexive transitive closure (R0 \/ transclose) of the declared GEN relations
   --         including that every concept has a top (NOthing) and bottom (Anything)
   --TODO  -> does not ensure axiom 18, antisymmetry, does AGtry ensure it?
   --REMARK -> Anything and NOthing must not be in Concepts
   --          "Ampersand is restricted to concepts that are not bottom or top, but the two are needed to signal type errors"
   isaRel :: Concepts -> Gens -> RelSet Concept
   isaRel cpts gens = foldr unite (RelSet [])
                        (RelSet [(a,NOthing) | a<-cpts      ]:
                         RelSet [(Anything,b) |  b<-cpts     ]:
                         expon (NOthing:Anything:cpts) 0 (RelSet []):    --Iu
                         transitiveclosure cpts gens2rels:
                         []
                         )
                      where
                         gens2rels = RelSet [(a,b) | (a,b)<-(map gen2rel gens)]
                         gen2rel gen =  case gen of
                                        G{} -> (gengen gen, genspc gen) --TODO -> check if gengen actually contains gen and not spc

   --REMARK -> I could construct a list (set) with all top-to-bottom paths in isaRel,
   --          p.e. isaLists :: Relset Concept -> [[Concept]], but I won't
   --          If A 'lub' B results in something not NOthing, A, or B, then I can conclude that A and B are
   --          not (A 'diamond' B) thus NOthing (axiom 21,22,23)
   --DESCR -> returns c1 'lub' c2
   --lub :: RelSet a -> a -> a -> a --TODO -> make a class for a to define top and bottom
   lubcpt :: RelSet Concept -> Concept -> Concept -> Concept
   lubcpt isarel a b | isA isarel a b = b
                     | isA isarel b a = a
                     | not (diamond isarel a b) = NOthing
                      -- | otherwise = should not be possible by definition of diamond

   diamond :: Eq a => RelSet a -> a -> a -> Bool
   diamond isarel a b = isA isarel a b || isA isarel b a

   --DESCR -> check if (c1,c2) exists in isaRel
   isA :: Eq a => RelSet a -> a -> a -> Bool
   isA (RelSet r) c1 c2 = elem (c1,c2) r
                                        
   --DESCR -> R+ = R \/ R^2 \/ R^3 \/ ...
   --REMARK -> transitiveclosure must be evaluated completely. We don't know the number of loops up front
   --          we know that the function is evaluated if
   transitiveclosure :: Eq a => [a] -> RelSet a -> RelSet a
   transitiveclosure universe r = geteval $ allCumUnion universe r
            where
            --DESCR  -> if a cumUnion n matches a cumUnion n+1 then cumUnion n and higher are
            --          all equal and the transitiveclosure
            --REMARK -> transitiveclosure must get a matching set1 and set2 at some point
            --TODO -> I could implement == for RelSet
            geteval (set1:set2:sets) | stop set1 set2 = set1
                                     | otherwise  = geteval (set2:sets)
            stop (RelSet set1) (RelSet set2) = foldr (&&) True ([elem s2 set1 | s2<-set2]++[elem s1 set2 | s1<-set1])

   --DESCR -> a list of cumUnion n for all n>0 indexed by n where n=1 is the head of he list
   --REMARK -> 1 million should be enough
   allCumUnion :: Eq a => [a] -> RelSet a -> [RelSet a]
   allCumUnion universe r = [cumUnion universe n r | n<-[1..999999]]
   
   --DESCR -> R \/ R^2 \/ .. \/ R^n
   --USE -> n>0
   cumUnion :: Eq a => [a] -> Int -> RelSet a -> RelSet a
   cumUnion universe 1 r = expon universe 1 r --REMARK -> should be the same as just r
   cumUnion universe n r = unite (cumUnion universe (n-1) r) (expon universe n r)

   --DESCR -> R \/ S
   unite :: Eq a => RelSet a -> RelSet a -> RelSet a
   unite (RelSet r) (RelSet s) = RelSet (unionBy equalelem r s)
            where
            --DESCR -> a binary value equals a binary value if sources and targets are equal
            equalelem :: Eq a => (a,a) -> (a,a) -> Bool
            equalelem (a,b) (c,d) = a==c && b==d

   --DESCR -> given the universe set => R^n
   --REMARK -> not tested with infinite universes and lazy evaluation, because no need for infinite universes yet
   --USE -> Use with positive exponents only (>=0)
   expon :: Eq a => [a] -> Int -> RelSet a -> RelSet a
   expon universe 0   _ =  RelSet [(a,a) | a<-universe] --DESCR -> Iu = R0
   expon universe exp r =  composition r (expon universe (exp-1) r)

   --DESCR -> R;S
   composition :: Eq a => RelSet a -> RelSet a -> RelSet a
   composition (RelSet r) (RelSet s) = RelSet [(a,c) | (a,b1)<-r,(b2,c)<-s, b1==b2]

---------------------------------------------------------------------------------------------
--Relations part: later in separate module
-- relations have nonlinear resources
--  {assumptions} |- (Building,Door)
--
---------------------------------------------------------------------------------------------

   --TODO -> just Declarations?
   type DeclRels = [DeclRel]
   type DeclRel = Declaration
   data DeclRelFound = FoundDr DeclRel | NotFoundDr NotFoundDrType Error
   data NotFoundDrType = NFD_NotFound | NFD_Fatal deriving (Show)

   --DESCR -> concatenate the declarations of relations from the patterns
   declRels :: Patterns -> DeclRels
   declRels [] = []
   declRels (p:ps) = case p of
                          Pat{} -> ptdcs p ++ declRels ps

   --DESCR -> search by Morphism (use mphats if specified)
   srchDeclRelByMorphism :: DeclRels -> Morphism -> DeclRelFound
   srchDeclRelByMorphism [] morph = case morph of Mph{} -> NotFoundDr NFD_NotFound ("Relation '" ++ (show morph) ++ "' has not been declared. " );
                                                      _ -> NotFoundDr NFD_Fatal ("Morphism type is not supported.")
   srchDeclRelByMorphism (drl:drls) morph
             | case drl of
                   Sgn{} -> case morph of
                         Mph{} -> if null (mphats morph)
                               then decnm drl == mphnm morph
                               else decnm drl == mphnm morph && desrc drl == head (mphats morph) &&  detgt drl == head (tail (mphats morph));
                         _ -> False;
                   _ -> False
                                    = if null (mphats morph) && not ((filter (isDeclWithName (mphnm morph)) drls)==[])
                                      then NotFoundDr NFD_NotFound (
                                            "Ambiguous relation '" ++ (mphnm morph) ++
                                            "', specify type explicitly (relation[a*b])\nPossible types are:\n"
                                            ++ show (filter (isDeclWithName (mphnm morph)) (drl:drls)) )
                                      else FoundDr drl
             | case drl of
                   Sgn{} -> case morph of
                         Mph{} -> False    --DESCR -> This morphism is not this declaration, so go to otherwise to try next declaration
                         _ -> True;
                   _ -> False
                                    = FoundDr (makeDeclaration morph)
             --TODO -> what about other declaration types and Mp1 morphisms?
             | otherwise            = srchDeclRelByMorphism drls morph

   --DESCR -> Check if a declared relation has been given a certain name
   --USE   -> combine with filter to get all declarations with a certain name
   isDeclWithName :: String -> DeclRel -> Bool
   isDeclWithName str decl = case decl of Sgn{} -> str == (decnm decl);
                                          _     -> False


---------------------------------------------------------------------------------------------
--MORE COMMENTS
---------------------------------------------------------------------------------------------



