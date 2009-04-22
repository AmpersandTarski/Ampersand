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

--TODO -> Put information in the trace to be able to present the user the reason why a type error has occurred
--        The phd thesis (book) of Bastiaan talks about this as an Explanation System (p.24)
--TODO -> meerdere fouten in expressie, dan binnenste fout, 1 per expressie
--TODO -> The ADL.Rule contains all kinds of structures typechecker  only supports the one with constructor Ru
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
   import Data.Maybe  -- USE -> fromJust, isNothing
   import Data.Tree   -- USE -> data Tree a
   import qualified Data.Set as Set --
   
   import Classification --USE -> cast from data.Tree to Classification for enrichment
   import Typology --USE -> Isa structure for enrichment

   ---------------
   --MAIN function
   ---------------

   --USE -> The error if is of type String and contains a complete error message
   --       This is the only type needed outside of the TypeChecker.
   type Errors = [Error]
   type Error = String

   --USE   -> The Environment is used to communicate ready to use input information for type checking
   --         The Environment is needed to transform to AdlExpr objects
   --DESCR -> The environment consists of:
   --          a dictionary containing the lowerbounds of the Cpts from the contexts in scope, the Cpt is the key
   --          a list of all declared (direct) relations between two Cpts from the contexts in scope
   --          the contexts in scope, which will be the context under evaluation and its extended contexts (recursively)
   type Environment = (RelSet Cpt, DeclRels)

   --DESCR -> The parser composes an Architecture object. This function typechecks this object.
   --USE   -> This is the only function needed outside of the TypeChecker
   typecheck :: Architecture -> (Contexts, Errors)
   --typecheck arch@(Arch ctxs) = (ctxs,[]) --DEBUG -> uncomment to disable typechecker
   --typecheck (Arch ctxs) = iwantastring (srchContext ctxs "Test")  --DEBUG
   typecheck arch@(Arch ctxs) =
                                (enrichArch arch,
                                --EXTEND -> put extra checking rules of the Architecture object here
                                --DESCR  -> check ctx name uniqueness, if that's ok then check the contexts
                                checkCtxNameUniqueness ctxs
                                ++||
                                checkCtxExtLoops ctxs
                                ++||
                                checkArch arch
                                )

   ------------------
   --Enrich functions
   ------------------

   --TODO -> put extra information, derived from the patterns (and ???), in the contexts, like :
   --        Isa [] [] -> representing isa relations
   --        Rules -> active rules
   --        Declarations -> active declarations
   --        ObjectDefs   p.e. types of expressions
   enrichArch :: Architecture -> Contexts
   enrichArch arch@(Arch ctxs) = map enrichCtx ctxs
          where
          enrichCtx :: Context -> Context
          enrichCtx cx =
             case cx of
                  Ctx{} -> cx {ctxisa=hierarchy, ctxwrld=world, ctxrs=ctxrules, ctxds=ctxdecls, 
                               ctxos=[bindObjDef (AllCpt,AllCpt) od | od<-ctxos cx],
                               ctxks=ctxkeys}
                           {-
                           (ctxnm cx) --copy name
                           (ctxon cx) --copy extended ctxs
                           (hierarchy) --TODO -> Difference with AGtry, the concepts in mphats are also put in the list of concepts
                           (world)     --construct the world with this cx on top of the world
                           (ctxpats cx)--copy patterns
                           (ctxrules)  --rules from gens and patterns of this context only
                           (ctxdecls)  --declarations of this context only
                           (ctxcptdefs)--concept defs of this context only
                           (ctxks cx)  --TODO -> key defs, what's the ADL syntax?
                           (ctxos cx)   --change mphdcl and mphtyp on morphisms in expressions
                           (ctxpops cx) --copy populations
                              -}
                           where
                           ctxtree = buildCtxTree (Found cx) ctxs
                           Cl _ world = toClassification $ ctxtree
                           --in AGtry -> isa = Isa [(g,s)|G pos g s<- _mGen] (concs _mD>-rd [c|G pos g s<- _mGen, c<-[g,s]])
                           isar = [case g of G{} -> (gengen g,genspc g) | g<-allCtxGens ctxs]
                           isac = map fst isar ++ map snd isar
                           hierarchy = Isa
                                         isar
                                         (map toConcept $ Set.toList
                                                        $
                                                          allCtxCpts ctxs
                                                            Set.\\
                                                          Set.fromList (map fromConcept isac)
                                         )
                           ctxrules = ctxrulesgens ++ ctxrulespats
                           ctxrulespats = map bindRule $ allCtxRules [cx] --TODO -> assign type to rules
                           bindRule r =
                                       let
                                       [bindant,bindcon] = bindExprs [rrant r, rrcon r]
                                       in
                                       r {rrant=bindant, rrcon=bindcon, rrtyp=sign bindant}
                           {-in AGtry ->
                              [ Ru Implication
                                  (Tm (mIs _spec_concept))
                                    _pos
                                  (Tm (mIs _genus_concept))
                                  [Tm (mIs _spec_concept), Tm (mIs _genus_concept)]
                                  ""
                                  (_spec_concept, _genus_concept)
                                    _lhs_rnr
                                   _lhs_pn             ]
                              -}
                           ctxrulesgens = [rulefromgen g  | g<-allCtxGens [cx]]
                           --TODO -> move rulefromgen to function toRule in module Gen
                           rulefromgen (G {genfp = pos, gengen = gen, genspc = spc} )
                                       = Ru
                                           Implication
                                           (Tm (mIs spc))
                                           pos
                                           (Tm (mIs gen))
                                           [Tm (mIs spc), Tm (mIs gen)]
                                           []
                                           (spc,gen)
                                           0 --TODO -> if needed by somebody assign rulenumber
                                           [] --TODO -> if needed by somebody the Pattern the rule is in
                                              --        if somebody cares then I think it is consistent that the Gen keeps track of the pattern too
                           ctxdecls = declRels (allCtxPats [cx])
                           ctxcptdefs = allCtxCptDefs [cx]
                           allCtx = map fromFoundCtx $ flatten ctxtree
                           (isarel,rels) = (isaRel (allCtxCpts allCtx) (allCtxGens allCtx),
                                            declRels (allCtxPats allCtx))

                           bindExprs :: Expressions -> Expressions
                           bindExprs exprs = [bindExpr x (foldlubcpts (srcs exprs),foldlubcpts (tgts exprs))| x<-exprs]
                                    where
                                    foldlubcpts cpts = foldr (lubcpt isarel) AllCpt cpts
                                    srcs exprs = map rtsrc (rts exprs)
                                    tgts exprs = map rttgt (rts exprs)
                                    rts exprs = [infer isarel $ castExpressionToAdlExpr (isarel,rels) x | x<-exprs]
                                    rtsrc (RelationType (s,_)) = s
                                    rttgt (RelationType (_,t)) = t
                           bindExpr :: Expression -> (Cpt,Cpt) -> Expression
                           bindExpr expr (src,tgt) =
                                    let
                                    adlexpr = castExpressionToAdlExpr (isarel,rels) expr
                                    RelationType (src', tgt') = infer isarel adlexpr
                                    bindsource = lubcpt isarel src src'
                                    bindtarget = lubcpt isarel tgt tgt'
                                    bindtype = (bindsource,bindtarget)
                                    in
                                    case expr of
                                         Tm{} -> Tm $ bindMph (m expr) bindtype
                                         Tc{} -> Tc $ bindExpr (e expr) (src,tgt)
                                         K0{} -> Tc $ bindExpr (e expr) (src,tgt)
                                         K1{} -> Tc $ bindExpr (e expr) (src,tgt)
                                         Cp{} -> Tc $ bindExpr (e expr) (src,tgt)
                                         Fu{} -> Fu $ bindExprs (es expr)
                                         Fi{} -> Fi $ bindExprs (es expr)
                                         F{}  -> bindF (es expr)
                                                    where
                                                    bindF [] = F []
                                                    bindF (x:[]) = F [bindExpr x (src,tgt)]
                                                    bindF (left:right:[]) =
                                                          let
                                                          RelationType (_,targetleft)  = infer isarel $ castExpressionToAdlExpr (isarel,rels) left
                                                          RelationType (sourceright,_) = infer isarel $ castExpressionToAdlExpr (isarel,rels) right
                                                          middle = lubcpt isarel targetleft sourceright
                                                          in
                                                          F [bindExpr left (bindsource,middle), bindExpr right (middle,bindtarget)]
                                                    bindF (left:right) =
                                                          let
                                                          RelationType (_,targetleft)  = infer isarel $ castExpressionToAdlExpr (isarel,rels) left
                                                          RelationType (sourceright,_) = infer isarel $ castExpressionToAdlExpr (isarel,rels) (F right)
                                                          middle = lubcpt isarel targetleft sourceright
                                                          F boundright = (bindExpr (F right) (middle,bindtarget))
                                                          in
                                                          F $ (bindExpr left (bindsource,middle)):boundright
                                         --REMARK -> example Product 'voldoetAan' Eisen
                                         --                  Certificering 'stelt' ExtraVoorwaarden
                                         --                  GEN ExtraVoorwaarden ISA Eisen
                                         --                  (-stelt ! voldoetAan~)~ :: Product * Certificering
                                         --               -> voor alle extra voorwaarden geldt
                                         --                  of het Product voldoetAan ExtraVoorwaarden
                                         --                  of de ExtraVoorwaarden worden niet gesteld voor een certificering
                                         --               => Product voldoet aan de extra voorwaarden die gesteld worden door een certificering
                                         --                  Dus de b in het midden is ExtraVoorwaarden en niet Eisen, net als de relatieve compositie
                                         --
                                         --                  (-voldoetAan ! stelt~) :: Product * Certificering
                                         --               -> voor alle eisen geldt
                                         --                  of het is een Eis waar het Product niet aan voldoet
                                         --                  of de Eis wordt gesteld door een Certificering
                                         --               -> voor alle eisen geldt
                                         --                  of het is een ExtraVoorwaarde waar het Product niet aan voldoet
                                         --                  of de ExtraVoorwaarde wordt gesteld door een Certificering
                                         --                  en het Product voldoet aan geen enkele Eis die geen ExtraVoorwaarde is
                                         --               -> voor alle extra voorwaarden geldt
                                         --                  of het is een ExtraVoorwaarde waar het Product niet aan voldoet
                                         --                  of de ExtraVoorwaarde wordt gesteld door een Certificering
                                         --                  en het Product voldoet aan geen enkel concept dat geen ExtraVoorwaarde is
                                         --               => Het product is gerelateerd tot een certificering als het uitsluitend voldoet aan 
                                         --                  ExtraVoorwaarden die gesteld worden door de certificering. Het product voldoet dus
                                         --                  nooit aan een concept dat geen ExtraVoorwaarde is.
                                         --                  Dus ik kan uit de voeten met een b van het type ExtraVoorwaarden
                                         {-
                                         Statement:
                                         Als relatiealgebra wetten (zoals DeMorgan) in een type systeem met subtypes moet houden, dan
                                         moet je een expressie met ; en zijn equivalent met !  over hetzelfde type b evalueren (en ook hetzelfde type a en hetzelfde type c)
                                         (a b en c zijn de vrije variabelen in de evaluatieregels van ; en !)
                                         -}
                                         Fd{}  -> bindFd (es expr)
                                                    where
                                                    bindFd [] = Fd []
                                                    bindFd (x:[]) = Fd [bindExpr x (src,tgt)]
                                                    bindFd (left:right:[]) =
                                                          let
                                                          RelationType (_,targetleft)  = infer isarel $ castExpressionToAdlExpr (isarel,rels) left
                                                          RelationType (sourceright,_) = infer isarel $ castExpressionToAdlExpr (isarel,rels) right
                                                          middle = lubcpt isarel targetleft sourceright
                                                          in
                                                          Fd [bindExpr left (bindsource,middle), bindExpr right (middle,bindtarget)]
                                                    bindFd (left:right) =
                                                          let
                                                          RelationType (_,targetleft)  = infer isarel $ castExpressionToAdlExpr (isarel,rels) left
                                                          RelationType (sourceright,_) = infer isarel $ castExpressionToAdlExpr (isarel,rels) (Fd right)
                                                          middle = lubcpt isarel targetleft sourceright
                                                          Fd boundright = (bindExpr (Fd right) (middle,bindtarget))
                                                          in
                                                          Fd $ (bindExpr left (bindsource,middle)):boundright

                           bindMph :: Morphism -> (Cpt,Cpt) -> Morphism
                           bindMph m@(Mph {}) (src,tgt) =
                                    let
                                    tp = (toConcept $ lubcpt isarel src (fromConcept (source m)),
                                          toConcept $ lubcpt isarel tgt (fromConcept (target m)))
                                    FoundDr dcl = srchDeclRelByMorphism rels m
                                    in
                                    m {mphtyp=tp, mphdcl=dcl} --TODO
                           bindMph i@(I {}) (src,tgt) =
                                    let
                                    gen' = if isA isarel src tgt then src else tgt
                                    spc  = if isA isarel tgt src then src else tgt
                                    gen  = if gen' == AllCpt then spc else gen' --DESCR -> gen is not allowed to stay Anything
                                    in
                                    i {mphgen=toConcept gen, mphspc=toConcept spc}
                           bindMph v@(V {}) (src,tgt) =
                                    let
                                    tp = (toConcept $ lubcpt isarel src (fromConcept (source v)),
                                          toConcept $ lubcpt isarel tgt (fromConcept (target v)))
                                    in
                                    v {mphtyp=tp}
                           bindMph x _ = x --TODO -> other morphisms are returned as parsed, is this correct?
                           bindObjDef ::  (Cpt,Cpt) -> ObjectDef ->ObjectDef
                           bindObjDef (src,tgt) od =  --od
                                    let
                                    expr' = bindExpr (objctx od) (src,tgt)
                                    xxx oa = bindObjDef (middle,AllCpt) oa
                                      where
                                      oatree = castObjectDefToAdlExprTree (isarel,rels) oa
                                      RelationType (src',tgt') = infer isarel $ adlexpr $ oatree
                                      middle = lubcpt isarel (fromConcept (target (expr'))) (src')
                                      adlexpr (AETree (Info _ (Trace _ adlexprt)) _) = adlexprt
                                      adlexpr (AELeaf (Info _ (Trace _ adlexprl)))   = adlexprl
                                    objats'= map (xxx) (objats od)
                                    in
                                    od {objctx=expr', objats=objats'}
                           ctxkeys = [bindKeyDef kd | kd<-allCtxKeyDefs [cx]]
                           bindKeyDef :: KeyDef -> KeyDef
                           bindKeyDef kd = 
                                    let 
                                    expr' = bindExpr (kdctx kd) (AllCpt,AllCpt)
                                    od@(Obj {objats=kdats'}) = bindObjDef (AllCpt,AllCpt)
                                                                          (Obj {objats=kdats kd,
                                                                               objnm=kdlbl kd,
                                                                               objpos=kdpos kd,
                                                                               objctx=expr'})
                                    in
                                    kd {kdctx=expr', kdats=kdats'}



   -----------------
   --Check functions
   -----------------

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

   --TODO -> check for loops in context extensions
   --USE -> Contexts names must be unique
   checkCtxExtLoops :: Contexts -> Errors
   checkCtxExtLoops ctxs = composeError (foldr (++) [] [findLoops cx | cx<- ctxs])
                            where
                            composeError :: [ContextName] -> Errors
                            composeError [] = []
                            composeError cxnms = ["One or more CONTEXT loops have been detected involving contexts: "
                                                   ++ foldr (++) "\n" [ cxnm++"\n" | cxnm<-cxnms]
                                                   ]
                            --DESCR -> there is a loop if a context is not found in the CtxTree, but it is found in the original Contexts
                            findLoops :: Context -> [ContextName]
                            findLoops cx = [ctxName cxf | cxf <- flatten (buildCtxTree (Found cx) ctxs)
                                                          , not (foundCtx cxf)
                                                          , foundCtx (srchContext ctxs (ctxName cxf))]

   --TODO -> argument Context indicating the current Context, only check that context
   checkArch :: Architecture -> Errors
   checkArch arch = case arch of Arch{} -> dropWhile (==[])
                                        --TODO -> dropWhile is needed because composeError always returns an error
                                        --        actually I want to check that there's no error without repeating
                                        --        the complete checkCtx statement
                                           [composeError cx (checkCtx $ buildCtxTree (Found cx) (archContexts arch))
                                                     |cx<-(archContexts arch)]
                                        where
                                        cxName cx = case cx of Ctx{} -> ctxnm cx
                                        composeError :: Context -> Errors -> Error
                                        composeError _ [] = []
                                        composeError cx errs = "\nCHECKING WITH CONTEXT '"++cxName cx++"' AS MAIN CONTEXT:\n" ++
                                                               foldr (++) [] errs


   --TODO -> Because ctxon in Context is a [String], I do not have file information for wrong EXTEND declarations
   --        I could look at the parent and stuff like that, but that's inconsistent with other statements like Expressions
   checkCtx :: CtxTree -> Errors
   checkCtx cxtr = ["Extended context '"++ ctxName cxf ++"' could not be found.\n"
                                      | cxf<-allFndCtx, not(foundCtx cxf)]
                   ++|| --DESCR -> If extended context cannot be found then abort, else check what needs to be checked
                   (checkObjDefs env (allCtxObjDefs allCtx)
                   ++&&
                   checkRules env (allCtxRules allCtx) )
                   where
                   allFndCtx = flatten cxtr
                   allCtx = map fromFoundCtx allFndCtx
                   env = (isaRel (allCtxCpts allCtx) (allCtxGens allCtx),
                          declRels (allCtxPats allCtx))


   --DESCR -> abstract expressions from all objectdefs (castObjectDefsToAdlExprTrees) and infer their types (inferWithInfo)
   --         Then check the result (processResult)
   --         Return the list of error strings
   checkObjDefs :: Environment -> ObjectDefs -> Errors
   --checkObjDefs env objs = case obj of Obj{} -> [show (castObjectDefsToAdlExprTrees env objs 0)] --DEBUG
   checkObjDefs env@(isarel,_) objs =
                               (processResult2          --DESCR -> after map a list of list of metainfo2 RT
                                       (foldr (++) [] (map (inferTree isarel) (castObjectDefsToAdlExprTrees env objs 0)))
                                  )

   --DESCR -> abstract expressions from all objectdefs (castObjectDefsToAdlExprTrees) and infer their types (inferWithInfo)
   --         Then check the result (processResult)
   --         Return the list of error strings
   checkRules :: Environment -> Rules -> Errors
   --checkRules env rules =  [(show (castRulesToAdlExprs env rules))] --DEBUG
   checkRules env@(isarel,_) ruls = (processResult2 (map (inferWithInfo isarel) (castRulesToAdlExprs env ruls)))

   ------------------
   --Common functions
   ------------------

   infixl 6 ++&&
   infixl 6 ++||

   --DESCR -> same as ++
   --USE   -> use ++&& and ++|| to combine multiple checks
   (++&&) :: [a] -> [a] -> [a]
   (++&&) e1 e2 = e1 ++ e2

   --DESCR -> only return errors of the right check if left check did not have errors
   --USE   -> use ++&& and ++|| to combine multiple checks
   (++||) :: [a] -> [a] -> [a]
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

   --USE    -> The ContextCheckResult is needed to communicate the environment from a context and potential errors
   --REMARK -> From the environment only the Contexts containing the contexts in scope is used
   type ContextName = String
   data ContextFound = Found Context | NotFound ContextName

   --DESCR -> data Tree a = Node a [Tree a]
   type CtxTree = Tree ContextFound

   toClassification :: CtxTree -> Classification Context
   toClassification (Node (NotFound cxnm) _) = error ("TypeChecker.toClassification: NotFound " ++ cxnm)
   toClassification (Node (Found cx) tree)   = Cl cx (map toClassification tree)

   ------------------------
   --ContextFound functions
   ------------------------

   buildCtxTree :: ContextFound -> Contexts -> CtxTree
   buildCtxTree cxnf@(NotFound _) _          = Node cxnf []
   buildCtxTree cxf@(Found cx) ctxs
              --a context may be put in the CtxTree only once, so if you put it now, then don't use it again to build the sub trees (thus remove it)
              = Node cxf [buildCtxTree (srchContext ctxs cxon) (removeCtx ctxs cx) | cxon <- case cx of Ctx{} -> ctxon cx]

   --DESCR -> search for a context by name and return the first one found
   srchContext :: Contexts -> String -> ContextFound
   srchContext [] srchstr = NotFound srchstr
   srchContext (cx:ctxs) srchstr
            | case cx of Ctx{} -> (ctxnm cx==srchstr)
                                  = Found cx
            | otherwise = srchContext ctxs srchstr

   foundCtx :: ContextFound -> Bool
   foundCtx (Found _) = True
   foundCtx _         = False

   ctxName :: ContextFound -> ContextName
   ctxName (Found cx)      = case cx of Ctx{} -> ctxnm cx
   ctxName (NotFound cxnm) = cxnm

   fromFoundCtx :: ContextFound -> Context
   fromFoundCtx (NotFound cxnm) = error ("TypeChecker.fromFoundCtx: NotFound " ++ cxnm)
   fromFoundCtx(Found cx)    = cx

   -------------------
   --context functions
   -------------------

   --DESCR -> removes a context from contexts
   --REMARK -> if a context is removed of which the name is not unique, then all the contexts with that name will be removed
   --          context names should be unique
   removeCtx :: Contexts -> Context -> Contexts
   removeCtx ctxs cx = [cx' | cx'<-ctxs, not((case cx of Ctx{} -> ctxnm cx) == (case cx' of Ctx{} -> ctxnm cx'))]

   --DESCR -> all the Gens of Contexts
   allCtxGens :: Contexts -> Gens
   allCtxGens ctxs = foldr (++) [] [case cx of Ctx{} -> allPatGens (ctxpats cx) | cx<-ctxs]

   --DESCR -> all the Gens of patterns
   allPatGens :: Patterns -> Gens
   allPatGens ps = foldr (++) [] [case p of Pat{} -> ptgns p | p<-ps]

   --DESCR -> all the patterns of contexts
   allCtxPats :: Contexts -> Patterns
   allCtxPats ctxs = foldr (++) [] [case cx of Ctx{} -> ctxpats cx | cx<-ctxs]

   allCtxCptDefs :: Contexts -> ConceptDefs
   allCtxCptDefs ctxs = allPatCptDefs (allCtxPats ctxs) 
                      --REMARK -> Concept Defs defined outside of the patterns are parsed into one of the patterns
                      --          so we do not need to add them again.
                      --TODO -> why are they parsed into a pattern?
                      --  ++ foldr (++) [] [case cx of Ctx{} -> ctxcs cx | cx<-ctxs]
   
   allPatCptDefs :: Patterns -> ConceptDefs
   allPatCptDefs ps = foldr (++) [] [case p of Pat{} -> ptcds p | p<-ps]

   --DESCR -> all the ObjectDefs of Contexts
   allCtxObjDefs :: Contexts -> ObjectDefs
   allCtxObjDefs ctxs = foldr (++) [] [case cx of Ctx{} -> ctxos cx | cx<-ctxs]

   --DESCR -> all the Rules of Contexts
   allCtxRules :: Contexts -> Rules
   allCtxRules ctxs = foldr (++) [] [case cx of Ctx{} -> ctxrs cx ++ allPatRules (ctxpats cx) | cx<-ctxs]

   --DESCR -> all the Gens of patterns
   allPatRules :: Patterns -> Rules
   allPatRules ps = foldr (++) [] [case p of Pat{} -> ptrls p | p<-ps]

   allCtxCpts :: Contexts -> Cpts
   allCtxCpts ctxs = foldr Set.union Set.empty
                            [allDeclCpts (declRels (allCtxPats ctxs)),
                             allGenCpts (allCtxGens ctxs),
                             allMphCpts (allExprMphs (allCtxExprs ctxs))
                             ]

   allMphCpts :: Morphisms -> Cpts
   allMphCpts ms = Set.fromList $ map fromConcept $ foldr (++) []
                   [case mph of Mph{} -> mphats mph;
                                I{} -> mphats mph;
                                I{} -> mphats mph;
                                _ -> []
                    |mph<-ms]

   allExprMphs :: Expressions -> Morphisms
   allExprMphs es = foldr (++) [] 
                    [
                     let
                        mphs (Tc e') = mphs e'
                        mphs (F es') = allExprMphs es'
                        mphs (Fd es') = allExprMphs es'
                        mphs (Fi es') = allExprMphs es'
                        mphs (Fu es') = allExprMphs es'
                        mphs (K0 e') = mphs e'
                        mphs (K1 e') = mphs e'
                        mphs (Cp e') = mphs e'
                        mphs (Tm mph) = [mph]
                     in mphs e
                     | e<-es]


   allCtxExprs :: Contexts -> Expressions
   allCtxExprs ctxs = allObjDefExprs (allCtxObjDefs ctxs) ++
                      allRuleExprs (allCtxRules ctxs)
                      --TODO -> allKeyDefExprs

   allObjDefExprs :: ObjectDefs -> Expressions
   allObjDefExprs os = foldr (++) [] [case obj of Obj{} -> (objctx obj):(allObjDefExprs (objats obj))| obj<-os]

   allRuleExprs :: Rules -> Expressions
   allRuleExprs rs = foldr (++) [] [case rul of Ru{} -> [rrant rul,rrcon rul];
                                                _    -> [] | rul<-rs]

   allDeclCpts :: DeclRels -> Cpts
   --allDeclCpts [] = []
   allDeclCpts dls = Set.fromList $ [case d of Sgn{} -> fromConcept (desrc d) | d<-dls] ++
                                    [case d of Sgn{} -> fromConcept (detgt d) | d<-dls]

   allGenCpts :: Gens -> Cpts
   --allGenCpts [] = []
   allGenCpts gens = Set.fromList $ [case g of G{} -> fromConcept (gengen g) | g<-gens] ++
                                    [case g of G{} -> fromConcept (genspc g) | g<-gens]
                                    
   allCtxKeyDefs :: Contexts -> KeyDefs
   allCtxKeyDefs ctxs = (allPatKeyDefs (allCtxPats ctxs))
   --TODO -> all context keydefs are already parsed into a pattern for some unknown reason
   --          not needed: ++ (foldr (++) [] [case cx of Ctx{} -> ctxks cx | cx <-ctxs])

   allPatKeyDefs :: Patterns -> KeyDefs
   allPatKeyDefs ps = foldr (++) [] [case p of Pat{} -> ptkds p | p<-ps]

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
   inferWithInfo :: RelSet Cpt -> MetaInfo a AdlExpr -> MetaInfo a RelationType
   inferWithInfo isarel (Info info (Trace trc expr1)) = Info info (Trace trc (checkInferred (infer isarel expr1)))
          where
          checkInferred (TypeError t err) = TypeError t (errInExpr expr1 err)
          checkInferred t = t

   data AdlExprTree = AETree (MetaInfo2 AdlExpr) [AdlExprTree] | AELeaf (MetaInfo2 AdlExpr)  deriving (Show)

   inferTree :: RelSet Cpt -> AdlExprTree -> [MetaInfo2 RelationType]
   inferTree isarel (AELeaf expr)                                           = [inferWithInfo isarel expr]
   inferTree isarel (AETree exprinfo@(Info info (Trace trace  expr)) trees) =
            case infer isarel expr of
            --DESCR -> do not combine with child nodes if parent is in error and return error with position of parent
            TypeError _ _  -> [inferWithInfo isarel exprinfo]
            --DESCR -> combine parent with all child nodes and infer type if a TypeError then return it, else
            --         combine parent with all child nodes and put the combined expression in the child node and infer types of child trees
            RelationType _ ->
                       [inferWithInfo isarel (getNodeLeafExpr exprinfo (getNodeExpr subtree)) | subtree <- trees]
                       ++
                       (foldr (++) []
                       [inferTree isarel (addParentToTree expr subtree) | subtree <-trees
                                                                        , not (iserr(inferWithInfo isarel (getNodeLeafExpr exprinfo (getNodeExpr subtree))))])
             where
             --DESCR -> MetaInfo of child node on the combined expression
             getNodeLeafExpr :: MetaInfo2 AdlExpr -> MetaInfo2 AdlExpr -> MetaInfo2 AdlExpr
             getNodeLeafExpr (Info _ (Trace _ exprp)) (Info info (Trace trace exprc)) = (Info info (Trace trace (Semicolon exprp exprc)))
             getNodeExpr :: AdlExprTree -> MetaInfo2 AdlExpr
             getNodeExpr (AETree expr _) = expr
             getNodeExpr (AELeaf expr)   = expr
             addParentToTree :: AdlExpr -> AdlExprTree -> AdlExprTree
             addParentToTree exprp (AETree (Info info (Trace trace exprc)) trees) = (AETree (Info info (Trace trace (Semicolon exprp exprc))) trees)
             addParentToTree exprp (AELeaf (Info info (Trace trace exprc)))       = (AELeaf (Info info (Trace trace (Semicolon exprp exprc)))      )
             iserr :: MetaInfo2 RelationType -> Bool
             iserr (Info info (Trace trc (TypeError _ _))) = True
             iserr _ = False

   --DESCR -> function to write trace lines
   --EXTEND -> always use this function for writing trace lines to be able to easily change the implementation
   --          for writing trace lines, for example reversing the order of trace lines in a trace
   writeTrcLn :: String -> [String] -> a -> Trace a
   writeTrcLn ln trc x = Trace (trc ++ [ln,"\n--------\n"]) x -- Trace (ln:trc) x

   errInExpr :: AdlExpr -> Error -> Error
   errInExpr ex err = "\nError in expression: " ++ show ex
                           ++"\n"
                           ++err

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
   processResult1 ((Info (posi,_) trc@(Trace _ (TypeError t err)):ts)) = ((compose t err posi)
                                                                              -- ++"\nTRACE\n"++(show trc)++"\nENDTRACE\n\n"
                                                                              ):(processResult1 ts)
   processResult1 ((Info _ (Trace _ (RelationType _)):ts)) = processResult1 ts

   --USE -> MetaInfo of this type is used for checking Rules
   type MetaInfo2 a = MetaInfo (FilePos) a

   --DESCR -> Process type inference results of Rules
   --         If a type is inferred, then it's ok. In case of a TypeError return the error.
   processResult2 :: [MetaInfo2 RelationType] -> Errors
   processResult2 [] = []
   processResult2 ((Info (posi) trc@(Trace _ (TypeError t err)):ts)) = ((compose t err posi)
                                                                            -- ++"\nTRACE\n"++(show trc)++"\nENDTRACE\n\n"
                                                                            ):(processResult2 ts)
   processResult2 ((Info _ (Trace _(RelationType _)):ts))  = processResult2 ts

   --DESCR -> Combine code position information and an error string
   compose :: TypeErrorType ->Error -> FilePos -> Error
   compose RTE_Fatal         err posi = "\nError at " ++ show posi ++ "\nFATAL ERROR while infering types: " ++ err ++ "\n"
   compose (RTE_DeclError t) err posi = "\nError at " ++ show posi ++ "\nThere is a problem finding the declaration of a relation:\n\t" ++ composeNFD t err ++ "\n"
   compose (RTE_ExprError t) err posi = "\nError at " ++ show posi ++ "\nThere is a problem interpreting an expression:\n\t" ++ composeEE t err ++ "\n"
   compose RTE_AbAbAb        err posi = "\nError at " ++ show posi ++ "\nCould not infer type of expression (a,b) -> (a,b) -> (a,b): " ++ err ++ "\n"
   compose RTE_AbBcAc        err posi = "\nError at " ++ show posi ++ "\nCould not infer type of expression (a,b) -> (b,c) -> (a,c): " ++ err ++ "\n"
   compose RTE_AaAa          err posi = "\nError at " ++ show posi ++ "\nSource and target of homogeneous relation are not the same {I, SYM, ASY, RFX, or TRN}. " ++ err ++ "\n"
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
--{-
   castObjectDefToAdlExprTree :: Environment -> ObjectDef ->  AdlExprTree
   castObjectDefToAdlExprTree env od =
                     let
                     treelist = castObjectDefsToAdlExprTrees env [od] 0
                     in
                     if length treelist == 1 then head treelist else error "TypeChecker.castObjectDefToAdlExprTree: list must be length one."
 ---}
   --DESCR -> Cast all objectdefs based on the environment to a list of AdlExprTree
   castObjectDefsToAdlExprTrees :: Environment -> ObjectDefs -> Depth ->  [AdlExprTree] -- [(AdlExpr,MetaInfo)]
   castObjectDefsToAdlExprTrees _ [] _ = []
   castObjectDefsToAdlExprTrees env@(isarel,_) (obj:objs) currdepth = case obj of
                                   Obj{} -> if null (objats obj)
                                            then
                                                  --DESCR -> add this objdef as AdlExpr for evaluation
                                                  (AELeaf (Info (objpos obj) (writeTrcLn composetrace [] thisAsAdlExpr) ))
                                                  --DESCR -> add the sibling objdefs as AdlExpr for evaluation
                                                  :(castObjectDefsToAdlExprTrees env objs currdepth)
                                            else
                                                  [AETree (Info (objpos obj) (writeTrcLn composetrace [] thisAsAdlExpr))
                                                          (castObjectDefsToAdlExprTrees env (objats obj) (currdepth+1))]
                                                   ++ (castObjectDefsToAdlExprTrees env objs currdepth)
                                            where
                                                  thisAsAdlExpr = castExpressionToAdlExpr env (objctx obj)
                                                  composetrace =
                                                       ("Validating type of subexpression (expr1) in a SERVICE on depth " ++
                                                     --  show currdepth ++ " :\n" ++
                                                       "=> expr1 -> " ++ show thisAsAdlExpr ++ " of type " ++ show (infer isarel thisAsAdlExpr)
                                                       )

   --DESCR  -> cast the rule to an AdlExpr
   --TODO   -> more guards for different rules
   --RULE   -> SJ: Ja, een regel is een expressie. De regel a|-c is hetzelfde als de expressie -a\/c.
   --          a b c  1 2 3      V=a1a2a3b1b2b3c1c2c3    a=a1a2a3 c=a1a3b1c3  => b1b3 are the rule violating instances
   --          SJ: Het type van een regel is het type van de equivalente expressie, namelijk  typeOf a `lub` typeOf c (aannemende dat typeOf het type van een expressie bepaalt)
   castRulesToAdlExprs :: Environment -> Rules -> [MetaInfo2 AdlExpr]
   castRulesToAdlExprs _ [] = []
   castRulesToAdlExprs env@(isarel,_) (rul:ruls)
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
                                            = (Info Nowhere (Trace [] (ExprError EE_Fatal "Rule type Sg not implemented.")):(castRulesToAdlExprs env ruls))
                     | case rul of
                              Gc{} -> True
                              _ -> False
                                            = (Info Nowhere (Trace [] (ExprError EE_Fatal "Rule type Gc not implemented.")):(castRulesToAdlExprs env ruls))
                     | case rul of
                              Fr{} -> True
                              _ -> False
                                            = (Info Nowhere (Trace [] (ExprError EE_Fatal "Rule type Fr not implemented.")):(castRulesToAdlExprs env ruls))
                 -}
                     | otherwise            = (Info Nowhere (Trace [] (ExprError EE_Fatal "Unknown rule type.")):(castRulesToAdlExprs env ruls))
                        where
                            leftsubexpr = castExpressionToAdlExpr env (rrant rul)
                            rightsubexpr = castExpressionToAdlExpr env (rrcon rul)
                            castimplication = Implicate leftsubexpr rightsubexpr
                            composetrace1 =
                               ("ERROR IN RULE ->\n" ++
                                show  castimplication ++ "\n" ++
                                --TODO -> show rul is ugly
                                "=> subexpr1 -> " ++ show leftsubexpr  ++ " has type " ++ show (infer isarel leftsubexpr) ++ "\n" ++
                                "=> subexpr2 -> " ++ show rightsubexpr ++ " has type " ++ show (infer isarel rightsubexpr) ++ "\n" ++
                                traceruleerror (infer isarel castimplication)
                               )
                            castequivalence = Equality leftsubexpr rightsubexpr
                            composetrace2 =
                               ("ERROR IN RULE ->\n" ++
                                show castequivalence ++ "\n" ++
                                --TODO -> show rul is ugly
                                "=> subexpr1 -> " ++ show leftsubexpr  ++ " has type " ++ show (infer isarel leftsubexpr) ++ "\n" ++
                                "=> subexpr2 -> " ++ show rightsubexpr ++ " has type " ++ show (infer isarel rightsubexpr) ++ "\n" ++
                                traceruleerror (infer isarel castequivalence)
                               )
                            traceruleerror (TypeError RTE_AbAbAb err)
                                            = "ERROR DESCRIPTION -> subexpr1 does not match type of subexpr2:\n" ++ err
                            traceruleerror _ = ""


   --RULE -> The parser translates expressions with a flip on subexpressions to an expressions
   --        with only flips on morphisms of type Mph for example (r;s)~ is parsed as s~;r~
   --RULE -> flips on Universe V[A*B] will be returned by the parser as V[B*A]
   castExpressionToAdlExpr :: Environment -> Expression -> AdlExpr
   castExpressionToAdlExpr (_,declrels) (Tm morph@(Mph{}))
                                               = doNotFlip (mphyin morph)
                                                            (typeofRel
                                                                  (srchDeclRelByMorphism declrels morph)
                                                            )

                         where
                            doNotFlip:: Bool -> AdlExpr -> AdlExpr
                            doNotFlip False expr1@(Relation (RelationType _)) = Flip expr1
                            doNotFlip _     expr1                     = expr1
   castExpressionToAdlExpr (_,declrels) (Tm morph) --RULE -> other Morphisms (I and V etc do not need to be flipped)
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

   --EXTEND -> Loose the declarations of relations, and other ADL tool specifics, by already infering a type
   typeofRel :: DeclRelFound -> AdlExpr
   typeofRel (NotFoundDr t err) = Relation (TypeError (RTE_DeclError t) err)
   typeofRel (FoundDr d) =
                    case d of
                    -- _ -> TypeError (show lbos) ; --DEBUG
                    Sgn{} ->  if elem Sym (decprps d) || elem Asy (decprps d) || elem Trn (decprps d) || elem Rfx (decprps d)
                              then HomoRelation (RelationType ( fromConcept(desrc d), fromConcept (detgt d) ))
                              else Relation (RelationType ( fromConcept(desrc d), fromConcept (detgt d) ));
                    --TODO -> why is there a despc and degen?
                    Isn{} -> Identity (RelationType ( fromConcept(despc d), fromConcept (despc d) ));
                    --REMARK -> Vs degen is the source Cpt, Vs despc the target Cpt
                    Vs {} -> Universe (RelationType ( fromConcept(degen d), fromConcept (despc d) ));
                    --TODO   -> when will there be an IsCompl?
                    --REMARK -> IsCompl{} will never be the result of ADL.MorphismAndDeclaration.makeDeclaration
                    --          makeDeclaration is used in srchDeclRelByMorphism
                    _ -> Relation( TypeError RTE_Fatal ("Unknown Declaration constructor. "))

---------------------------------------------------------------------------------------------
--Type inference part: later in separate module
--
---------------------------------------------------------------------------------------------

   --USE ->  Store the type of an expression or a type error
   data RelationType = RelationType (Cpt, Cpt) | TypeError TypeErrorType Error

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
                  | HomoRelation RelationType
                  | Implicate   {ex1::AdlExpr, ex2::AdlExpr}
                  | Equality    {ex1::AdlExpr, ex2::AdlExpr}
                  | Complement  {ex1::AdlExpr}
                  | Flip        {ex1::AdlExpr}
                  | Union       {ex1::AdlExpr, ex2::AdlExpr}
                  | Intersect   {ex1::AdlExpr, ex2::AdlExpr}
                  | Semicolon   {ex1::AdlExpr, ex2::AdlExpr}
                  | Dagger      {ex1::AdlExpr, ex2::AdlExpr}
                  | Identity    RelationType
                  | Universe    RelationType        --TODO -> this is not in table in article
                  | TrsClose    {ex1::AdlExpr}      --TODO -> this is not in table in article
                  | TrsRefClose {ex1::AdlExpr}      --TODO -> this is not in table in article
                  | ExprError ExprErrorType Error  -- deriving (Show)

   data ExprErrorType = EE_SubExpr | EE_Fatal deriving (Show)

   instance Show AdlExpr where
       showsPrec _ (Relation (TypeError _ _)) = showString "<error>" --DESCR -> override show of TypeError
       showsPrec _ (HomoRelation (TypeError _ _)) = showString "<error>" --DESCR -> override show of TypeError
       showsPrec _ (Identity (TypeError _ _)) = showString "<error>" --DESCR -> override show of TypeError
       showsPrec _ (Universe (TypeError _ _)) = showString "<error>" --DESCR -> override show of TypeError
       --showsPrec _ t@(Relation _)    = showString (show t)
       showsPrec _ (Relation (RelationType t))     = showString (show t)
       showsPrec _ (Identity (RelationType t))     = showString ("I[" ++ show t ++ "]")
       showsPrec _ (Universe (RelationType t))     = showString ("V[" ++ show t ++ "]")
       showsPrec _ (HomoRelation (RelationType t)) = showString ("Homo[" ++ show t ++ "]")
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

   infer :: RelSet Cpt -> AdlExpr -> RelationType
   infer _ (Relation rel) = inferAbAb rel
   infer isarel (HomoRelation rel) = inferAaAa isarel rel
   infer _ (Universe rel) = inferAbAb rel
   infer isarel (Identity rel) = inferAaAa isarel rel
   infer isarel (Semicolon expr1 expr2) = inferAbBcAc isarel (infer isarel expr1) (infer isarel expr2)
   infer isarel (Dagger expr1 expr2) = inferAbBcAc isarel (infer isarel expr1) (infer isarel expr2)
   infer isarel (Union expr1 expr2) = extInferAbAbAb isarel expr1 expr2
   infer isarel (Intersect expr1 expr2) = extInferAbAbAb isarel expr1 expr2
   infer isarel (Implicate expr1 expr2) = extInferAbAbAb isarel expr1 expr2
   infer isarel (Equality expr1 expr2)  = extInferAbAbAb isarel expr1 expr2
   infer isarel (Flip expr1) = inferAbBa (infer isarel expr1)
   infer isarel (Complement expr1) = inferAbAb (infer isarel expr1)
   infer isarel (TrsClose expr1) = inferAaAa isarel (infer isarel expr1)     --TODO -> is this correct?
   infer isarel (TrsRefClose expr1) = inferAaAa isarel(infer isarel expr1)  --TODO -> is this correct?
   infer _ (ExprError t err)= TypeError (RTE_ExprError t) err --The expression is already known to be unknown


   extInferAbAbAb isarel expr1@(Identity rel) expr2 = inferAaAaAa isarel (infer isarel expr1) (infer isarel expr2)
   extInferAbAbAb isarel expr1 expr2@(Identity rel) = inferAaAaAa isarel (infer isarel expr1) (infer isarel expr2)
   extInferAbAbAb isarel expr1 expr2                = inferAbAbAb isarel (infer isarel expr1) (infer isarel expr2)

   --DESCR -> infer  e1::(a,b1), e2::(b2,c) b1>=b b2>=b |- e3::e1 -> e2 -> (a,c)
   inferAbBcAc :: RelSet Cpt -> RelationType -> RelationType -> RelationType
   inferAbBcAc _ err@(TypeError _ _) _ = err   --pass errors up
   inferAbBcAc _ _ err@(TypeError _ _) = err
   inferAbBcAc isarel (RelationType (a,b1)) (RelationType (b2,c))
             | diamond isarel b1 b2 = (RelationType (a,c))
             | otherwise              = TypeError RTE_AbBcAc ("The target of the left expression ("++ show b1 ++") does not match the source of the right expression ("++ show b2 ++")\n")

   --DESCR -> infer  e1::(a1,b1), e2::(a2,b2) a1>=a a2>=a b1>=b b2>=b |- e3::e1 -> e2 -> (a,b)
   inferAbAbAb :: RelSet Cpt -> RelationType -> RelationType -> RelationType
   inferAbAbAb _ err@(TypeError _ _) _                = err   --DESCR -> pass errors up
   inferAbAbAb _ _ err@(TypeError _ _)                = err
   inferAbAbAb isarel (RelationType (a,b)) (RelationType (p,q))
             | diamond isarel a p
               && diamond isarel b q    = (RelationType (lubcpt isarel a p,lubcpt isarel b q))
             | not (diamond isarel a p) = TypeError RTE_AbAbAb ("The source of the left expression (" ++ show a ++ ") does not match the source of the right expression (" ++ show p ++ ")\n")
             | otherwise                = TypeError RTE_AbAbAb ("The target of the left expression (" ++ show b ++ ") does not match the target of the right expression (" ++ show q ++ ")\n")

   --DESCR -> infer  e1::(a,b) |- e2::e1 -> (b,a)
   inferAbBa :: RelationType -> RelationType
   inferAbBa err@(TypeError _ _) = err
   inferAbBa (RelationType (src,trg)) = RelationType (trg,src)

   --DESCR -> infer  e1::(a,b) |- e2::e1 -> (a,b)
   inferAbAb :: RelationType -> RelationType
   inferAbAb t = t

   inferAaAaAa :: RelSet Cpt -> RelationType -> RelationType -> RelationType
   inferAaAaAa _ err@(TypeError _ _) _                = err   --DESCR -> pass errors up
   inferAaAaAa _ _ err@(TypeError _ _)                = err
   inferAaAaAa isarel t1@(RelationType (a,b)) t2@(RelationType (p,q))
                                        | a==b &&
                                          p==q      = (RelationType (lubcpt isarel a p,lubcpt isarel a p))
                                        | not(a==b) = TypeError RTE_AaAa ("Left expression: Source does not equal target -> source: " ++ show a ++ " target: " ++ show b ++ "\n")
                                        | otherwise = TypeError RTE_AaAa ("Right expression: Source does not equal target -> source: " ++ show p ++ " target: " ++ show q ++ "\n")

   --DESCR -> infer  e1::(a,a) |- e2::e1 -> (a,a)
   inferAaAa :: RelSet Cpt -> RelationType -> RelationType
   inferAaAa _ err@(TypeError _ _) = err    --TODO -> condition for homogeneous relations not described in inference table, should identity be src==trg as described in table?
   inferAaAa isarel t@(RelationType (src,trg)) | diamond isarel src trg  = RelationType (lubcpt isarel src trg, lubcpt isarel src trg)
                                               | otherwise               = TypeError RTE_AaAa ("Source does not equal target -> source: " ++ show src ++ " target: " ++ show trg ++ "\n")

---------------------------------------------------------------------------------------------

--REMARK -> Can not use data Cpt as a in RelSet a, because the  implementation of
--          instance Ord Cpt is not suitable. Ord is needed by a lot of Data.Set functions
   data Cpt = Cpt String | AllCpt | NoCpt
   type Cpts = Set.Set Cpt

   instance Show Cpt where
       showsPrec _ (Cpt a) = showString a
       showsPrec _ AllCpt = showString "Anything"
       showsPrec _ NoCpt = showString "Nothing"

   instance Eq Cpt where
       Cpt a == Cpt b = a==b
       AllCpt == AllCpt = True
       NoCpt == NoCpt = True
       _ == _ = False

   instance Ord Cpt where
       Cpt a <= Cpt b = a <= b
       AllCpt <= _ = True
       _ <= AllCpt = False
       NoCpt <= _  = False
       _ <= NoCpt  = True
    
   fromConcept :: Concept -> Cpt
   fromConcept (C {cptnm = nm}) = Cpt nm
   fromConcept Anything = AllCpt
   fromConcept NOthing = NoCpt
   fromConcept S = error "TypeChecker.hs function fromConcept: Singleton not supported."
   
   toConcept :: Cpt -> Concept  
   toConcept (Cpt nm) = cptnew nm
   toConcept AllCpt = Anything
   toConcept NoCpt = NOthing

---------------------------------------------------------------------------------------------

   --data RelSet a = RelSet [(a,a)] deriving (Show)
   type RelSet a = Set.Set (a,a)

   --DESCR -> if is in isaRel then predicate isa is true. reflects axiom 15-19
   --         reflexive transitive closure (R0 \/ transclose) of the declared GEN relations
   --         including that every concept has a top (NoCpt) and bottom (AllCpt)
   --REMARK -> AllCpt and NoCpt must not be in Cpts
   --          "Ampersand is restricted to concepts that are not bottom or top, but the two are needed to signal type errors"
   isaRel :: Cpts -> Gens -> RelSet Cpt
   isaRel cpts gens = foldr Set.union (Set.empty)
                        (Set.fromList [(a,NoCpt) | a<-Set.toList cpts      ]:
                         Set.fromList [(AllCpt,b) |  b<-Set.toList cpts     ]:
                         Set.fromList [(a,a) | a<-(NoCpt:AllCpt:Set.toList cpts)]:
                         transitiveclosure_w (Set.toList cpts) gens2rels:
                         []
                         )
                      where
                         gens2rels = Set.fromList [(fromConcept a, fromConcept b) | (a,b)<-(map gen2rel gens)]
                         gen2rel gen =  case gen of
                                        G{} -> (gengen gen, genspc gen) --TODO -> check if gengen actually contains gen and not spc

   --REMARK -> I could construct a list (set) with all top-to-bottom paths in isaRel,
   --          p.e. isaLists :: Relset Cpt -> [[Cpt]], but I won't
   --          If A 'lub' B results in something not NoCpt, A, or B, then I can conclude that A and B are
   --          not (A 'diamond' B) thus NoCpt (axiom 21,22,23)
   --DESCR -> returns c1 'lub' c2
   --lub :: RelSet a -> a -> a -> a --TODO -> make a class for a to define top and bottom
   lubcpt :: RelSet Cpt -> Cpt -> Cpt -> Cpt
   lubcpt isarel a b | isA isarel a b = b
                     | isA isarel b a = a
                     | not (diamond isarel a b) = NoCpt
                      -- | otherwise = should not be possible by definition of diamond

   diamond :: Ord a => RelSet a -> a -> a -> Bool
   diamond isarel a b = isA isarel a b || isA isarel b a

   --DESCR -> check if (c1,c2) exists in isaRel
   isA :: Ord a => RelSet a -> a -> a -> Bool
   isA r c1 c2 = Set.member (c1,c2) r

   --DESCR -> duplicated from clos1.Auxiliaries.hs only [a] is provided instead of
   --         computed from range(RelSet a) /\ domain(RelSet a)
   --         [a] contains all possible intermediates on the path
   --REMARK -> if for [a] the universe is provided this will be less efficient within this function
   --          then providing the most precise subset range(RelSet a) /\ domain(RelSet a). 
   --          However computing range(RelSet a) /\ domain(RelSet a) comes at a cost
   --          just like computing universe. The choice is left to the user of this function.
   --          p.e. the typechecker has already computed the universe for other purposes
   --TODO -> We could calculate the cost of providing unnecessary large [a] lists
   transitiveclosure_w :: Ord a => [a] -> RelSet a -> RelSet a
   transitiveclosure_w [] r     = r
   transitiveclosure_w (x:xs) r = transitiveclosure_w xs $ r `Set.union` (Set.fromList [(a,b')|(a,b)<-(Set.toList r),b==x,(a',b')<-(Set.toList r),a'==x])

---------------------------------------------------------------------------------------------
--Relations part: later in separate module
-- relations have nonlinear resources
--  {assumptions} |- (Building,Door)
--
---------------------------------------------------------------------------------------------

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



