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
   import Data.List   -- USE -> intersect, union, delete

   ---------------
   --MAIN function
   ---------------

   --USE -> The error if is of type String and contains a complete error message
   --       This is the only type needed outside of the TypeChecker.
   type Errors = [Error]
   type Error = String

   --DESCR -> The parser composes an Architecture object. This function typechecks this object.
   --USE   -> This is the only function needed outside of the TypeChecker
   typecheck :: Architecture -> Errors
   typecheck _ = [] --DEBUG -> uncomment to disable typechecker
   --typecheck (Arch ctxs) = iwantastring (srchContext ctxs "Test")  --DEBUG
   typecheck arch@(Arch ctxs) =
                                --EXTEND -> put extra checking rules of the Architecture object here
                                --DESCR  -> check ctx name uniqueness, if that's ok then check the contexts
                                checkCtxNameUniqueness ctxs ++||
                                checkCtxs arch ctxs   --TODO -> this list of errors is not distinct

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
   type Environment = (LowerboundsOfs, DeclRels, Contexts)

   --USE    -> The ContextCheckResult is needed to communicate the environment from a context and potential errors
   --REMARK -> From the environment only the Contexts containing the contexts in scope is used
   type ContextCheckResult = (Environment, Errors)
   data ContextFound = Found Context | NotFound Error

   --DESCR -> check all the Contexts. The Architecture is communicated to be able to search for (extended) contexts.
   checkCtxs :: Architecture -> Contexts -> Errors
   checkCtxs _ [] = []
   --DESCR -> Take the errors found when checking this context as root context and concat it with the errors of the other contexts taken as root
   checkCtxs arch@(Arch ctxs) (ctx':tl_ctxs) = errors (check (Found ctx')) ++ (checkCtxs arch tl_ctxs)
      where
         errors :: ContextCheckResult -> Errors
         errors ((_,_,_),err) = err
         check :: ContextFound -> ContextCheckResult
         check (NotFound str) = (([],[],[]),("Extended context " ++ str ++ " of context " ++ (case ctx' of Ctx{} -> ctxnm ctx') ++ " could not be found"):[]) --this case will not have been caught by the parser yet
         check (Found cx)
                     = checkThisCtx (constructEnv checkExtCtx cx)
                     where
                         --DESCR -> get the list of extended Context
                         --         check all extended Context in the list (all siblings)
                         --         merge the results (classification, relations, errors)
                         checkExtCtx :: ContextCheckResult
                         checkExtCtx = case cx of Ctx{} -> foldr concatRes (([], [], []),[]) (map check (map (srchContext ctxs) ( ctxon cx) ))
                         --DESCR -> Enrich the Environment of the extended contexts with patterns from the current context
                         constructEnv :: ContextCheckResult -> Context -> ContextCheckResult
                         constructEnv ((_,_,extCtxs),errs) cx' -- @(Ctx nm _ _ _ _ _ _ _ _ _ _) --DEBUG
                                    = (
                                         ( lowerboundsOfs (allCtxGens (cx':extCtxs)),
                                           declRels (allCtxPats (cx':extCtxs)),
                                           cx':extCtxs
                                         ),
                                         errs
                                       )
                                   -- =(([], [],[]),[show (lowerboundsOfs (allCtxGens (cx:extCtxs)) )]) --DEBUG to show lowerboundsOfs
                                   -- =(([], [],[]),[show (declRels    (allCtxPats (cx:extCtxs)) )]) --DEBUG to show declRels
                                   -- \| nm=="Test2" = ((lowerboundsOfs (allCtxGens (cx:extCtxs)),declRels (allCtxPats (cx:extCtxs)),cx:extCtxs),[])  --DEBUG
                                   -- \| otherwise   = (([], [], []),[show (lowerboundsOfs (allCtxGens (cx:extCtxs)))])  --DEBUG of extends Test2
                                   -- \| otherwise   = (([], [], []),[show (declRels    (allCtxPats (cx:extCtxs)))])  --DEBUG of extends Test2

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
                            (env,
                                  checkObjDefs env (allCtxObjDefs ctxs) ++&&
                                  checkRules env (allCtxRules ctxs)
                            )

   --DESCR -> abstract expressions from all objectdefs (castObjectDefsToAdlExprs) and infer their types (inferWithInfo)
   --         Then check the result (processResult)
   --         Return the list of error strings
   checkObjDefs :: Environment -> ObjectDefs -> Errors
   --checkObjDefs env (obj:objs) = case obj of Obj{} -> [show (castObjectDefsToAdlExprs env [obj] 0)] --DEBUG
   checkObjDefs env objs =
                               (processResult1
                                       (map inferWithInfo (castObjectDefsToAdlExprs env objs 0))
                                  )

   --DESCR -> abstract expressions from all objectdefs (castObjectDefsToAdlExprs) and infer their types (inferWithInfo)
   --         Then check the result (processResult)
   --         Return the list of error strings
   checkRules :: Environment -> Rules -> Errors
   --checkRules env rules =  [(show (castRulesToAdlExprs env rules))] --DEBUG
   checkRules env ruls = (processResult2 (map inferWithInfo (castRulesToAdlExprs env ruls)))


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
                                                              = (([], [], cxs1 ++ cxs2),[])
                                                  | otherwise = (([],[],[]),errs1 ++ errs2)

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
   --TODO -> put the trace down to the type inferer
   inferWithInfo :: MetaInfo a AdlExpr -> MetaInfo a RelationType
   inferWithInfo (Info info (Trace trc expr1)) = Info info (Trace trc (checkInferred (infer expr1)))
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
   castObjectDefsToAdlExprs env (obj:objs) currdepth = case obj of
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
                                                       (combineObjDefs thisAsAdlExpr currdepth)
                                                       (castObjectDefsToAdlExprs env (objats obj) (currdepth+1))
                                                  )
                                                  --DESCR -> add the sibling objdefs as AdlExpr for evaluation
                                                  ++ (castObjectDefsToAdlExprs env objs currdepth)
                                            where
                                                  thisAsAdlExpr = castExpressionToAdlExpr env (objctx obj)
                                                  composetrace =
                                                       ("Validating type of subexpression (expr1) in a SERVICE on depth " ++
                                                       show currdepth ++ " :\n" ++
                                                       "=> expr1 -> " ++ show thisAsAdlExpr ++ " of type " ++ show (infer thisAsAdlExpr)
                                                       )

   --DESCR -> given the current depth, combine the subexpression from the current objectdef
   --         with a nested object def as AdlExpr with MetaInfo1 to a new AdlExpr with MetaInfo1
   combineObjDefs :: AdlExpr -> Depth -> MetaInfo1 AdlExpr -> MetaInfo1 AdlExpr
   combineObjDefs obj currdepth expr1@(Info (posi,depth) (Trace trc nestedobj))
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
                                                       "=> expr1 -> " ++ show obj       ++ " of type " ++ show (infer obj)       ++ "\n" ++
                                                       "=> expr2 -> " ++ show nestedobj ++ " of type " ++ show (infer nestedobj) ++ "\n" ++
                                                       "=> expr3 has type " ++ show (infer (Semicolon obj nestedobj))
                                                       )


   --DESCR  -> cast the rule to an AdlExpr
   --TODO   -> more guards for different rules
   --RULE   -> SJ: Ja, een regel is een expressie. De regel a|-c is hetzelfde als de expressie -a\/c.
   --          a b c  1 2 3      V=a1a2a3b1b2b3c1c2c3    a=a1a2a3 c=a1a3b1c3  => b1b3 are the rule violating instances
   --          SJ: Het type van een regel is het type van de equivalente expressie, namelijk  typeOf a `lub` typeOf c (aannemende dat typeOf het type van een expressie bepaalt)
   castRulesToAdlExprs :: Environment -> Rules -> [MetaInfo2 AdlExpr]
   castRulesToAdlExprs _ [] = []
   castRulesToAdlExprs env (rul:ruls)
                     | case rul of
                              Ru{} -> (rrsrt rul == Implication);
                              _ -> False
                                            = (Info
                                                 --DESCR -> file position of rule
                                                 (rrfps rul)
                                                 (writeTrcLn composetrace1 [] translateimplication)
                                               ):(castRulesToAdlExprs env ruls)
                     | case rul of
                              Ru{} -> (rrsrt rul == Equivalence);
                              _ -> False
                                            = (Info
                                                 --DESCR -> file position of rule
                                                 (rrfps rul)
                                                 (writeTrcLn composetrace2 [] translateequivalence)
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
                            translateimplication = (Intersect (Complement (leftsubexpr)) (rightsubexpr))
                            composetrace1 =
                               ("ERROR IN RULE ->\n" ++
                                "Translating implication rule (subexpr1 |- subexpr2) to expression ( -subexpr1\\/subexpr2 ) for type validation:\n" ++
                                --TODO -> show rul is ugly "Translating rule " ++ show rul ++ " resulting in expression -" ++ show (rrant rul) ++ "\\/" ++ show (rrcon rul) ++ "\n" ++
                                "=> subexpr1 -> " ++ show leftsubexpr  ++ " has type " ++ show (infer leftsubexpr) ++ "\n" ++
                                "=> subexpr2 -> " ++ show rightsubexpr ++ " has type " ++ show (infer rightsubexpr) ++ "\n" ++
                                traceruleerror (infer translateimplication)
                               )
                                                   -- left=right => (-left\/right)/\(left\/-right)
                            translateequivalence = Union
                                                         (Intersect (Complement (leftsubexpr)) (rightsubexpr))
                                                         (Intersect (Complement (rightsubexpr)) (leftsubexpr))
                            composetrace2 =
                               ("ERROR IN RULE ->\n" ++
                                "Translating equivalence rule (subexpr1 = subexpr2) to expression ( -subexpr1\\/subexpr2/\\-subexpr2\\/subexpr1) for type validation:\n" ++
                                --TODO -> show rul is ugly "Translating rule " ++ show rul ++ " resulting in expression -" ++ show (rrant rul) ++ "\\/" ++ show (rrcon rul) ++ "\n" ++
                                "=> subexpr1 -> " ++ show leftsubexpr  ++ " has type " ++ show (infer leftsubexpr) ++ "\n" ++
                                "=> subexpr2 -> " ++ show rightsubexpr ++ " has type " ++ show (infer rightsubexpr) ++ "\n" ++
                                traceruleerror (infer translateequivalence)
                               )
                            traceruleerror (TypeError RTE_AbAbAb err)
                                            = "ERROR DESCRIPTION -> subexpr1 does not match type of subexpr2:\n" ++ err
                            traceruleerror _ = ""


   --RULE -> The parser translates expressions with a flip on subexpressions to an expressions
   --        with only flips on morphisms of type Mph for example (r;s)~ is parsed as s~;r~
   --RULE -> flips on Universe V[A*B] will be returned by the parser as V[B*A]
   castExpressionToAdlExpr :: Environment -> Expression -> AdlExpr
   castExpressionToAdlExpr (lbos,declrels,_) (Tm morph@(Mph{}))
                                               = doNotFlip (mphyin morph)
                                                            (Relation (typeofRel
                                                                               lbos
                                                                               (srchDeclRelByMorphism declrels morph)
                                                                      )
                                                            )

                         where
                            doNotFlip:: Bool -> AdlExpr -> AdlExpr
                            doNotFlip False expr1@(Relation (RelationType _)) = Flip expr1
                            doNotFlip _     expr1                     = expr1
   castExpressionToAdlExpr (lbos,declrels,_) (Tm morph) --RULE -> other Morphisms (I and V etc do not need to be flipped)
                                               = Relation (typeofRel
                                                          lbos
                                                          (srchDeclRelByMorphism declrels morph)
                                                          )
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
   typeofRel :: LowerboundsOfs -> DeclRelFound -> RelationType
   typeofRel _ (NotFoundDr t err) = TypeError (RTE_DeclError t) err
   typeofRel lbos (FoundDr d) = case d of
                    -- _ -> TypeError (show lbos) ; --DEBUG
                    Sgn{} -> RelationType ( lowerbound lbos (desrc d), lowerbound lbos (detgt d) );
                    --TODO -> why is there a despc and degen?
                    Isn{} -> RelationType ( lowerbound lbos (despc d), lowerbound lbos (despc d) );
                    --REMARK -> Vs degen is the source Concept, Vs despc the target Concept
                    Vs {} -> RelationType ( lowerbound lbos (degen d), lowerbound lbos (despc d) );
                    --TODO   -> when will there be an IsCompl?
                    --REMARK -> IsCompl{} will never be the result of ADL.MorphismAndDeclaration.makeDeclaration
                    --          makeDeclaration is used in srchDeclRelByMorphism
                    _ -> TypeError RTE_Fatal ("Unknown Declaration constructor. ")

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
--      8) the intersection expression can be defined given expression e1 and e2
--         if concepttype a1 and a2 are subsets of concepttype a AND
--         if concepttype b1 and b2 are subsets of concepttype b, the type is (a,b)
--         e1::(a1,b1), e2::(a2,b2) a>=a1 a>=a2 b>=b1 b>=b2 |- e3::e1\/e2 -> (a,b)
--      9) the union expression can be defined given expression e1 and e2
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

   --USE -> a ConceptType is a set of concepttypes implemented as a list of Concept
   --       the set consists of all concepttypes lower than or equal to the concepttype
   --       The Concept is a part of the expressions coming from the parser
   --       Use function lowerbound to get the ConceptType of a Concept
   --       You will need a LowerBoundsOfs object which can be built with function lowerboundsOfs
   --       based on Gens. GEN ... ISA ... definitions are parsed to Gens.
   type ConceptType = [Concept]

   --USE ->  Store the type of an expression or a type error
   data RelationType = RelationType (ConceptType, ConceptType) | TypeError TypeErrorType Error

   instance Show RelationType where
       showsPrec _ (RelationType t)     = showString (show t)
       showsPrec _ (TypeError t err)    = showString ("TypeError " ++ show t ++ err)

   --USE -> A type error can result from:
   --          - type inference rules ab->ab->ab or ab->bc->ac
   --          - a fatal error during type inference
   --          - an expression which cannot be interpreted
   --          - a problem with finding a declaration of a relation
   data TypeErrorType = RTE_AbAbAb | RTE_AbBcAc | RTE_ExprError ExprErrorType | RTE_DeclError NotFoundDrType | RTE_Fatal deriving (Show)


   --USE -> Relation will be the only expression already inferred possibly containing a TypeError
   data AdlExpr =   Relation    RelationType  --USE -> use typeofRel to get the RelationType
                  | Semicolon   {ex1::AdlExpr, ex2::AdlExpr}
                  | Dagger      {ex1::AdlExpr, ex2::AdlExpr}
                  | Flip        {ex1::AdlExpr}
                  | TrsClose    {ex1::AdlExpr}
                  | TrsRefClose {ex1::AdlExpr}
                  | Complement  {ex1::AdlExpr}
                  | Union       {ex1::AdlExpr, ex2::AdlExpr}
                  | Intersect   {ex1::AdlExpr, ex2::AdlExpr}
   --TODO -> why can't I specify an I or V for a Relation? Why are I and V morphisms and not expressions?
   --RULE -> I and V are cast as Relation RelationType and thus supported as morphisms
   --        | Identity    AdlExpr
   --        | Universe    RelationType
                  | ExprError ExprErrorType Error  -- deriving (Show)

   data ExprErrorType = EE_SubExpr | EE_Fatal deriving (Show)
   
   instance Show AdlExpr where
       showsPrec _ (Relation (TypeError _ _)) = showString "<error>" --DESCR -> override show of TypeError
       --showsPrec _ t@(Relation _)    = showString (show t)
       showsPrec _ (Relation (RelationType t))     = showString (show t)
       showsPrec _ (Semicolon e1 e2) = showString (show e1 ++ ";" ++ show e2)
       showsPrec _ (Dagger e1 e2)    = showString (show e1 ++ "!" ++ show e2)
       showsPrec _ (Union e1 e2)     = showString (show e1 ++ "/\\" ++ show e2)
       showsPrec _ (Intersect e1 e2) = showString (show e1 ++ "\\/" ++ show e2)
       showsPrec _ (Flip e1)         = showString (show e1 ++ "~")
       showsPrec _ (TrsClose e1)     = showString (show e1 ++ "+")
       showsPrec _ (TrsRefClose e1)  = showString (show e1 ++ "*")
       showsPrec _ (Complement e1)   = showString ("-" ++ show e1)
       showsPrec _ (ExprError _ err) = showString err

   lowerbound :: LowerboundsOfs -> Concept -> [Concept]
   lowerbound _ Anything = [Anything]
   lowerbound _ NOthing = []
   lowerbound _ S = [] --TODO -> check if this is correct
   lowerbound chds c = (lowerboundsToConcepts (lowerbounds (lowerboundsOf chds c)))

   infer :: AdlExpr -> RelationType
   infer (Relation rel) = rel   --DESCR -> Relation is already an RelationType
   infer (Semicolon expr1 expr2) = inferAbBcAc (infer expr1) (infer expr2)
   infer (Dagger expr1 expr2) = inferAbBcAc (infer expr1) (infer expr2)
   infer (Union expr1 expr2) = inferAbAbAb (infer expr1) (infer expr2)
   infer (Intersect expr1 expr2) = inferAbAbAb (infer expr1) (infer expr2)
   infer (Flip expr1) = inferAbBa (infer expr1)
   --RULE -> I and V are cast as Relation RelationType and thus supported as morphisms
   --        infer (Identity expr) = inferIdentity expr
   --        infer (Universe expr) = inferUniverse expr
   infer (Complement expr1) = inferAbAb (infer expr1)
   infer (TrsClose expr1) = inferAbAb (infer expr1)
   infer (TrsRefClose expr1) = inferAbAb (infer expr1)
   infer (ExprError t err)= TypeError (RTE_ExprError t) err --The expression is already known to be unknown

   --USE -> InferredCptType is an internal structure. CptType or CptTypeError will be stored in a RelationType
   data CptTypeErrorType = CTE_TypeMismatch deriving (Show)
   data InferredCptType = CptType ConceptType | CptTypeError CptTypeErrorType Error deriving (Show)

   --DESCR -> check if c1 is a subset of c2, if so return c1
   --         otherwise check if c2 is a subset of c1, if so return c2
   --         otherwise raise a type error
   inferCptType :: ConceptType -> ConceptType -> InferredCptType
   inferCptType c1 c2
                      | c1 >=-> c2 = CptType c1
                      | c2 >=-> c1 = CptType c2
                      | otherwise = CptTypeError CTE_TypeMismatch  --TODO -> better message
                      ( "\nt1 can be a\n" ++ showtypes c1
                      ++ "t2 can be a\n" ++ showtypes c2
                      ++ "However t1 is not allowed to be a\n" ++ showtypes (filter (notElem2 c2) c1)
                      ++ "and t2 is not allowed to be a\n"  ++ showtypes (filter (notElem2 c1) c2)
                      )
                      where
                      showtypes [] = "" --should not be needed
                      showtypes (ct:[]) = "\t" ++ show ct ++ "\n"
                      showtypes (ct:cts) = "\t" ++ show ct ++ " or\n" ++ showtypes cts

   --DESCR -> True if there are no elements in c2 that are not in c1  (c2 is a subset of c1)
   (>=->) :: ConceptType -> ConceptType -> Bool
   (>=->) c1 c2 = elem Anything c1 ||
                  (filter (notElem2 c1) c2) == []

   notElem2 :: (Eq a) => [a] -> a -> Bool
   notElem2 lst elm = not (elem elm lst)

   --DESCR -> infer  e1::(a,b1), e2::(b2,c) b1>=b b2>=b |- e3::e1 -> e2 -> (a,c)
   inferAbBcAc :: RelationType -> RelationType -> RelationType
   inferAbBcAc err@(TypeError _ _) _ = err   --pass errors up
   inferAbBcAc _ err@(TypeError _ _) = err
   inferAbBcAc (RelationType (src1,trg1)) (RelationType (src2,trg2)) =
                     checkAbBcAc (inferCptType trg1 src2 )
                     where
                          checkAbBcAc (CptTypeError CTE_TypeMismatch err)
                                      = TypeError RTE_AbBcAc ("The type of the target (t1) of the left expression does not match the type of the source (t2) of the right expression:\n\t" ++ err)
                          checkAbBcAc _ = RelationType (src1, trg2)

   --DESCR -> infer  e1::(a1,b1), e2::(a2,b2) a1>=a a2>=a b1>=b b2>=b |- e3::e1 -> e2 -> (a,b)
   inferAbAbAb :: RelationType -> RelationType -> RelationType
   inferAbAbAb err@(TypeError _ _) _                = err   --DESCR -> pass errors up
   inferAbAbAb _ err@(TypeError _ _)                = err
   inferAbAbAb (RelationType (src1,trg1)) (RelationType (src2,trg2))  =
                     checkAbAbAb (inferCptType src1 src2) (inferCptType trg1 trg2)
                     where
                          checkAbAbAb (CptTypeError CTE_TypeMismatch err) _
                                      = TypeError RTE_AbAbAb ("The type of the source (t1) of the left expression does not match the type of the source (t2) of the right expression:\n\t" ++ err)
                          checkAbAbAb _ (CptTypeError CTE_TypeMismatch err)
                                      = TypeError RTE_AbAbAb ("The type of the target (t1) of the left expression does not match the type of the target (t2) of the right expression:\n\t" ++ err)
                          checkAbAbAb (CptType infsrc) (CptType inftrg)
                                      = RelationType (infsrc, inftrg)

   --DESCR -> infer  e1::(a,b) |- e2::e1 -> (b,a)
   inferAbBa :: RelationType -> RelationType
   inferAbBa err@(TypeError _ _) = err
   inferAbBa (RelationType (src,trg)) = RelationType (trg,src)

   --DESCR -> infer  e1::(a,b) |- e2::e1 -> (a,b)
   inferAbAb :: RelationType -> RelationType
   inferAbAb t = t

---------------------------------------------------------------------------------------------
--Lowerbounds part: later in separate module
--lowerboundsOfs is the interesting function which returns the model and the tracklist to
--correlate to the ADL code. lowerboundsOfs is build in two phases. First the actual code
--declarations are enumerated (decllbofs). Then the lowerbounds are evaluated recursively, and folded.
--Each concept is evaluated once to prevent recursive loops. Therefore a list is passed around
--to keep track of the evaluated concepts.
--
--lowerboundsOf can be used to get the LowerboundsOf object for a Concept by Concept.
--lowerbounds can be used to get the lowerbounds from an Child entry
--lowerboundsToConcepts will transform lowerbounds to a list of concepts
--
--need to define a >= b
-- Anything = top, Nothing = bottom
-- c1::a, c2::b, a>=b   |- c1::a -> b
---------------------------------------------------------------------------------------------

   type LowerboundsOfs = [LowerboundsOf]

   --USE -> (lowerbound, tracklist of gen declarations in ADL code)
   type Lowerbound = (Concept,[Gen])

   --USE -> structure for storing all lowerbound concepts of a concept
   data LowerboundsOf = LbsOf (Concept       , [Lowerbound]) deriving (Show)

   --USE -> triple to pass around progress ([Concept]), intermediate result ([Lowerbound]),
   --       and progress original input (LowerboundsOfs)
   --       to get all lowerbounds of all Concepts and prevent looping
   type ConstrLbsOfResult = ([Concept],[Lowerbound],LowerboundsOfs)

   --DESCR -> equality of LowerboundsOf = equality of its source Concept = equality of the name of the source Concept
   instance Eq LowerboundsOf where
     (LbsOf (cpt,_))==(LbsOf (cpt',_)) = cpt==cpt'

   ------------------
   --PUBLIC FUNCTIONS
   ------------------

   --DESCR -> returns the model and the tracklist to correlate to the ADL code.
   --         lowerboundsOfs is build in two phases. First the actual code
   --         declarations are enumerated (declLbOfs). Then the children are resolved recursively, and folded (foldLbs).
   lowerboundsOfs :: Gens -> LowerboundsOfs
   lowerboundsOfs gens = foldLbs (declLbOfs gens) (declLbOfs gens)
         where
         --DESCR -> foreach LbsOf in declLbOfs, fold distinct the targets of its targets
         --         provide the LowerboundsOfs declared in the code, and the list of LowerboundsOfs to resolve
         foldLbs :: LowerboundsOfs -> LowerboundsOfs -> LowerboundsOfs
         foldLbs _ [] = []
         foldLbs decllbofs ((LbsOf (cpt,lbs)):lbsos) = (LbsOf(cpt,allLowerbounds decllbofs lbs)):(foldLbs decllbofs lbsos)
         --DESCR -> lowerbounds which are explicitly and directly declared in ADL
         declLbOfs :: Gens -> LowerboundsOfs
         declLbOfs gens' = foldr insertGen [] gens'

   --DESCR -> get the LowerboundsOf object from LowerboundsOfs by Concept
   --         (equality by concept name)
   lowerboundsOf :: LowerboundsOfs -> Concept -> LowerboundsOf
   lowerboundsOf [] cpt       =
                         --REMARK -> apparantly this Concept has no declared lowerbounds, return a list
                         --          with only itself as lowerbound
                         --TODO -> I could implement NOthing as bottom object
                         --TODO -> I must be sure that the lowerbounds are always consulted through function lowerboundsOf
                         --        because this function has an lbo entry for concepts not in ISA too
                         LbsOf (cpt,[(cpt,[])])
   lowerboundsOf (a@(LbsOf(cpt',_)):lbsos) cpt
         | cpt' == cpt =
                         --DESCR -> found -> return
                         a
         | otherwise   =
                         --DESCR -> try next
                         lowerboundsOf lbsos cpt



   --DESCR -> get the Lowerbounds of a LowerboundsOf object
   lowerbounds :: LowerboundsOf -> [Lowerbound]
   lowerbounds (LbsOf (_,lbs)) = lbs

   --DESCR -> Get a list of Lowerbounds as a list of Concepts
   lowerboundsToConcepts :: [Lowerbound] -> [Concept]
   lowerboundsToConcepts [] = []
   lowerboundsToConcepts ((lbcpt,_):lbs) = lbcpt:(lowerboundsToConcepts lbs)

   -------------------
   --PRIVATE FUNCTIONS
   -------------------

   --DESCR -> return all Lowerbounds of a Lowerbound,
   --         respecting the already resolved Concepts and results,
   --         given the explicit declarations from the ADL code
   constrLbsOfResult :: Lowerbound -> ConstrLbsOfResult -> ConstrLbsOfResult
   constrLbsOfResult  lb@(lbcpt,_) (progress,lbsres,decllbofs)
           --DESCR -> if lowerbound as Concept already in progress list
         | elem lbcpt progress =
                          --DESCR -> skip, already evaluated: so just forward result so far
                          (progress,lbsres,decllbofs)
         | otherwise    =
                          --DESCR -> add this lowerbound to the lowerbounds found so far (lb:lbsres),
                          --         and all the lowerbounds of this lowerbound (foldr ... lowerboundsOf decllbofs lbcpt),
                          --         register this lbcpt as evaluated (lbcpt:progress)
                          foldr constrLbsOfResult (lbcpt:progress,lb:lbsres,decllbofs) (lowerbounds (lowerboundsOf decllbofs lbcpt))

   --DESCR -> return all the Lowerbounds for a list of Lowerbounds,
   --         given the explicit declaration from the ADL code
   allLowerbounds :: LowerboundsOfs -> [Lowerbound] -> [Lowerbound]
   allLowerbounds decllbofs lbs = constrLbsOfRes (foldr constrLbsOfResult ([],[],decllbofs) lbs)
       where   --DESCR -> return lowerbounds found so far
               constrLbsOfRes :: ConstrLbsOfResult -> [Lowerbound]
               constrLbsOfRes (_,lbsres,_) = lbsres

   --DESCR -> insert an explicit ADL code declaration (Gen) of a lowerbound concept of a concept to the list
   --TODO  -> for some reason the upperbound is parsed as the genspc and the lowerbound the gengen
   --         or the parser mixes up the two or the current typechecker Agtry. It's implemented assuming the AGtry is correct.
   --         example     [LbsOf (Person,[(Boss    ,[GEN Person ISA Boss]),
   --                                     (Employee,[GEN Person ISA Employee])
   --                                  ])]    parsed as: genspc     gengen
   insertGen :: Gen -> LowerboundsOfs -> LowerboundsOfs
   --DESCR -> search for a LowerboundOf object for this upperbound concept in a LowerboundsOf list
   --         if the LowerboundsOf is not in the list, construct and add a new LowerboundsOf object to the end of the list
   --         put the concept as lowerbound of itself in the LowerboundsOf object
   insertGen gen [] = case gen of
                           G{} -> ( LbsOf (genspc gen, [(gengen gen,[gen])
                                                       , (genspc gen,[])
                                                       ]
                                           )
                                   ):[]  --DESCR -> insert
   insertGen gen (lbo@(LbsOf(cpt,lbs)):lbos)
          | case gen of G{} -> (genspc gen == cpt)
                               =
                               --DESCR -> if the LowerboundOf is located, add the lowerbound to the lowerbounds of the LowerboundOf
                               (LbsOf(genspc gen, insertGenLb lbs gen)):lbos --DESCR -> update  (insert lowerbound)
          | otherwise   =
                          --DESCR -> lowerboundOf object not located yet, try next and preserve all entries (lbo:)
                          lbo:(insertGen gen lbos)

   --DESCR -> insert a new lowerbound to the lowerbound list.
   --         if the lowerbound already exists, add track information to the lowerbound (declared twice)
   insertGenLb :: [Lowerbound] -> Gen -> [Lowerbound]
   --DESCR -> if the Lowerbound is not in the list, construct and add a new Lowerbound object to the end of the list
   insertGenLb [] gen = case gen of
                              G{} -> (gengen gen,[gen]):[] --DESCR -> insert
   insertGenLb (lb@(lbcpt,gens):lbs) gen
          | case gen of G{} -> (gengen gen == lbcpt)
                               =
                               --DESCR -> if the Lowerbound is located, add the Gen as tracking info to the lowerbound
                               (gengen gen, gen:gens):lbs --DESCR -> update tracklist
          | otherwise   = lb:(insertGenLb lbs gen) --DESCR -> not found yet, try next

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



