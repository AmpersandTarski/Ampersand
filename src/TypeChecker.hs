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
--REMARK -> meerdere fouten in expressie, dan binnenste fout, 1 per expressie
--REMARK -> The ADL.Rule contains all kinds of structures typechecker only supports the one with constructor Ru and Sg.
--          Fr rules will generate a type error message, but the parser (see CC.hs) does not output Fr rules at the moment of writing this comment.
--          Gc rules are or will be deprecated. Gc rules will generate a type error message too.
--DESCR ->
--         types are inferred bottom up. First the type of the morphisms is inferred, then the types of the expressions using them are inferred
--         subexpressions are evaluated from left to right if applicable (thus only for the union, intersection, semicolon, and dagger)
--TODO -> Checking sick.adl results in a lot of ambiguous relations in expressions, because
--        this type checker just puts all patterns of this context and the extended contexts of this context on a heap
--        apparantly there is another definition for the objects in scope than the definition implemented.
--        Check the correctness of handling context extension and patterns.
module TypeChecker (typecheck, Error, Errors) where

import Adl          -- USE -> .MorphismAndDeclaration.makeDeclaration
                    --        and of course many data types
import Data.List    -- USE -> unionBy
import Data.Maybe() -- USE -> fromJust, isNothing
import Data.Tree    -- USE -> data Tree a
import qualified Data.Set as Set --

import Classification --USE -> cast from data.Tree to Classification for enrichment
import Typology --USE -> Isa structure for enrichment

import CC_aux (renumberRules)

import TypeInference.ITree
import TypeInference.AdlExpr
import TypeInference.Input
import TypeInferenceEngine

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
typecheck arch@(Arch ctxs) = (enriched, checkresult)  
--                    if null checkresult then
--                   (enriched,["TYPE -> " ++ show (sign proof) | (proof@(Proven _ trees),_)<-allproofs, tree<-trees])
--                   (enriched,[(show $ evaltree gamma tree) | (proof@(Proven gamma trees),_)<-allproofs, tree<-trees])
--                     (enriched,[ (printexpr $ fromRule ex) ++ "\n"|ex<-(allCtxRules enriched)])
--TODO -> stolen minnetje
-- ra~;sa = ra~;sa
-- -(ra~;sa) = -(ra~;sa) => geparsed als ra~;sa = ra~;-sa en ra~;sa=-(ra~;sa) (los gaan ze goed)
--                      (enriched,[show (ex) ++ "\n"|ex<-(allCtxRules ctxs)])
--                    (enriched,[printexpr ex |(BoundTo ex)<-[evaltree g tree|(Proven g trees,_)<-allproofs, tree<-trees]])
--                     else (enriched,checkresult)
   where
   --EXTEND -> put extra checking rules of the Architecture object here
   --DESCR  -> check ctx name uniqueness, if that's ok then check the contexts
   check1 = checkCtxNameUniqueness ctxs
   check2 = checkCtxExtLoops ctxs 
   (enriched, allproofs) = enrichArch arch  
   check3 = [(proof,fp) | (proof@(NoProof{}),fp)<-allproofs] --all type errors TODO -> pretty printing
   checkresult = if null check1 then if null check2 then if null check3 then [] else [show check3] else check2 else check1
   ------------------
   --Enrich functions
   ------------------

   --TODO -> put extra information, derived from the patterns (and ???), in the contexts, like :
   --        Isa [] [] -> representing isa relations
   --        Rules -> active rules
   --        Declarations -> active declarations
   --        ObjectDefs   p.e. types of expressions
enrichArch :: Architecture -> (Contexts,[(Proof,FilePos)])
enrichArch (Arch ctxs) = ( [enrichedctx | (enrichedctx,_)<-[enrichCtx cx ctxs|cx<-ctxs]]
                            , foldr (++) [] [infresult | (_,infresult)<-[enrichCtx cx ctxs|cx<-ctxs]])

--DESCR -> contains enrichment functionality which should be temporary
postenrich :: Context -> Context
postenrich cx@(Ctx{}) = addsgndecls $ renumber cx
renumber :: Context -> Context
renumber cx@(Ctx{}) = cx {ctxpats=renumberPats 1 (ctxpats cx),
                          ctxrs=renumberRules 1 (ctxrs cx)}
  where
  renumberPats :: Int -> Patterns -> Patterns
  renumberPats _ [] = []
  renumberPats i (p:[]) = (renumberPat i p):[]
  renumberPats i (p@(Pat{}):ps) = (renumberPat i p):renumberPats (i+(length $ ptrls p)) ps
  renumberPat :: Int -> Pattern -> Pattern
  renumberPat i p@(Pat{}) = p {ptrls=renumberRules i (ptrls p)}
addsgndecls ::Context -> Context
addsgndecls cx@(Ctx{}) = cx {ctxds=(ctxds cx)++allsgndecls }
  where allsgndecls = [srrel sg | sg@(Sg{})<-allPatRules $ allCtxPats [cx]]

enrichCtx :: Context -> Contexts -> (Context,[(Proof,FilePos)])
enrichCtx cx@(Ctx{}) ctxs = 
  (postenrich $ 
      cx {ctxisa=hierarchy, -- 
          ctxwrld=world, --
          ctxpats=ctxpatterns, -- 
          ctxrs=[rule | (rule,_,_)<-ctxrules], 
          ctxds=ctxdecls, -- 
          ctxos=[od | (od,_)<-ctxobjdefs], 
          ctxks=[kd | (kd,_)<-ctxkeys]} 
  ,  [(proof,fp)|(_,proof,fp)<-ctxrules]
   ++[(proof,fp)|(_,proofs)<-ctxobjdefs, (proof,fp)<-proofs]
   ++[(proof,fp)|(_,proofs)<-ctxkeys, (proof,fp)<-proofs])
                           {-
                           (ctxnm cx) --copy name
                           (ctxon cx) --copy extended ctxs
                           (hierarchy) --construct Isa with all Concepts in scope
                           (world)     --construct the world with this cx on top of the world
                           (ctxpats cx)--bind rules and keydefs in patterns
                           (ctxrules)  --rules from gens and patterns of this context only
                           (ctxdecls)  --declarations of this context only
                           (ctxcptdefs)--concept defs of this context only
                           (ctxks cx)  --bind keydefs
                           (ctxos cx)   --change mphdcl and mphtyp on morphisms in expressions
                           (ctxpops cx) --copy populations
                              -}
  where
  --ctxinf = ctx
  --TODO -> generate rules from props UNI etc
  --DESCR -> enriching ctxwrld
  ctxtree = buildCtxTree (Found cx) ctxs
  Cl _ world = toClassification $ ctxtree
  allCtx = map fromFoundCtx $ flatten ctxtree

  tc :: Concepts
  tc = allCtxCpts(allCtx) 
  --REMARK -> tc0 is defined in specification but not needed in implementation to lookup these constants. They are just used when needed
  --tc0 :: Concepts
  --tc0 = [Anything,NOthing]
  isatree = isaRels tc (allCtxGens(allCtx))
  isaStmts = map fromIsa isatree
  --DESCR -> All Relation decls in ctx
  rv :: Declarations
  rv = allCtxDecls (allCtx)
  --REMARK -> rc is defined in specification but not needed in implementation to lookup the constant relations (see mphStmts)
  --rc :: Declarations
  --rc = [Isn c c | c<-tc] ++ [Vs c1 c2 | c1<-tc, c2<-tc]
  --TODO -> I could split gamma in two
  gamma expr = (mphStmts expr) ++ gammaisa
  gammaisa = isaStmts
  mphStmts :: AdlExpr -> [Statement]
  mphStmts (Relation mp@(Mph{mphnm=r1}) i t) =
     let
     --REMARK -> inference rule T-RelDecl is evaluated to a TypeOf statement and not implemented explicitly
     --          T-RelDecl won't be in the inference tree for this reason.
     --TODO -> Set homo rel on True
     alternatives = [DeclExpr (Relation (mp{mphdcl=dc}) i $ fromSign (c1,c2)) (ishomo dclprops) 
                    | dc@(Sgn{decnm=decl,desrc=c1,detgt=c2, decprps=dclprops})<-rv, decl==r1]
     ishomo :: [Prop] -> Bool
     ishomo dclprops = foldr (||) False [elem p dclprops| p<-[Sym,Asy,Trn,Rfx]]
     in
     if null alternatives
     then [InfErr (UndeclRel (Relation mp i t))]
     else alternatives
  mphStmts (Relation mp@(I{mphats=[c1]}) i _) = [DeclExpr (Relation mp i $ fromSign (c1,c1)) True]
  mphStmts (Relation mp@(I{}) i _) = [DeclExpr (Relation mp i unknowntype) True]
  mphStmts (Relation mp@(V{mphats=[c1,c2]}) i _) = [DeclExpr (Relation mp i $ fromSign (c1,c2)) False]
  mphStmts (Relation mp@(V{}) i _) = [DeclExpr (Relation mp i unknowntype) False]
  mphStmts (Relation (Mp1{}) _ _ ) = [] --TODO -> ???
  mphStmts (Implicate expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Equality expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Union exprs _) = foldr (++) [] $ map mphStmts exprs
  mphStmts (Intersect exprs _) = foldr (++) [] $ map mphStmts exprs
  mphStmts (Semicolon expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Dagger expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Complement expr _) = mphStmts expr
  mphStmts (Flip expr _) = mphStmts expr

  --DESCR -> enriching ctxisa
  --in AGtry -> isa = Isa [(g,s)|G pos g s<- _mGen] (concs _mD>-rd [c|G pos g s<- _mGen, c<-[g,s]])
  hierarchy = Isa isar (Set.toList $ (Set.fromList $ allCtxCpts ctxs) Set.\\ (Set.fromList isac))
    where
    isar = [case g of G{} -> (gengen g,genspc g) | g<-allCtxGens ctxs]
    isac = foldr merge [] [map fst isar, map snd isar]

  --DESCR -> enriching ctxpats
  ctxpatterns = map bindPat (ctxpats cx)
  bindPat p@(Pat{}) = p {ptrls= bindrules ,ptkds= bindkds}
    where
    bindrules = [br | (br,_,_)<-map bindRule (ptrls p)]
    bindkds = [bk | (bk,_)<-map bindKeyDef (ptkds p)]

  --DESCR -> enriching ctxds
  --         take all the declarations from all patterns included (not extended) in this context
  ctxdecls = allCtxDecls [cx]

  --DESCR -> enriching ctxrs
  ctxrules :: [(Rule,Proof,FilePos)]
  ctxrules = ctxrulesgens ++ ctxrulespats
  ctxrulespats = map bindRule $ allCtxRules [cx]
  --REMARK -> The rules are numbered after enriching, see renumber :: Context -> Context
  --          Thus a rule in (cxrls cx) originating from a rule in a pattern, and the original rule
  --          have different numbers.
  bindRule :: Rule -> (Rule,Proof,FilePos)
  bindRule r@(Ru{})
    | rrsrt r==Truth = 
        let 
        proof = infer (gamma adlexpr) adlexpr
        adlexpr = fromRule r
        bindcon = case proof of
          Proven gm (inftree:_) -> bindSubexpr (rrcon r) $ evaltree gm inftree --bind subexpressions according to trees
          _ -> rrcon r --copy rule as parsed
        bindrtype =  case proof of
          Proven _ _ -> sign proof
          NoProof{} -> rrtyp r
        in
        (r {rrcon=bindcon, rrtyp=bindrtype},proof,rrfps r) 
    | otherwise = 
        let
        proof = infer (gamma adlexpr) adlexpr
        adlexpr = fromRule r
        bindant = case proof of
          Proven gm (inftree:_) -> bindSubexpr (rrant r) $ etant $ evaltree gm inftree
          _ -> rrant r --copy rule as parsed 
        bindcon = case proof of
          Proven gm (inftree:_) -> bindSubexpr (rrcon r) $ etcon $ evaltree gm inftree
          _ -> rrant r --copy rule as parsed
        bindrtype =  case proof of
          Proven _ _ -> sign proof
          NoProof{} -> rrtyp r
        etant et = 
          if rrsrt r==Implication 
          then case et of 
             (BoundTo (Implicate antex _ _)) -> BoundTo antex
             _ -> error $ "Error in TypeChecker.hs module TypeChecker function enrichCtx.bindRule.etant: " ++
                          "Expected a BoundTo implication rule statement."++show et++"."
          else case et of 
             (BoundTo (Equality antex _ _)) -> BoundTo antex
             _ -> error $ "Error in TypeChecker.hs module TypeChecker function enrichCtx.bindRule.etant: " ++
                          "Expected a BoundTo equivalence rule statement."++show et++"."
        etcon et = 
          if rrsrt r==Implication 
          then case et of 
             (BoundTo (Implicate _ conex _)) -> BoundTo conex
             _ -> error $ "Error in TypeChecker.hs module TypeChecker function enrichCtx.bindRule.etcon: " ++
                          "Expected a BoundTo implication rule statement."++show et++"."
          else case et of 
             (BoundTo (Equality _ conex _)) -> BoundTo conex
             _ -> error $ "Error in TypeChecker.hs module TypeChecker function enrichCtx.bindRule.etcon: " ++
                          "Expected a BoundTo equivalence rule statement."++show et++"."
        in 
        (r {rrant=bindant, rrcon=bindcon, rrtyp=bindrtype},proof,rrfps r)
  bindRule r@(Sg{}) = (r {srsig=bindsig, srtyp=sign bindsig, srrel= binddecl},proof,srfps r)
    where
    (bindsig,proof,_) = bindRule (srsig r)
    binddecl = (srrel r) {desrc=source bindsig, detgt=target bindsig}
  bindRule _ = error "Unsupported rule type while enriching the context. The type checker should have given an error."
  ctxrulesgens :: [(Rule,Proof,FilePos)]
  ctxrulesgens = [rulefromgen g | g<-allCtxGens [cx]]
  --TODO -> move rulefromgen to function toRule in module Gen
  --DESCR -> rules deducted from a gen are proven by the existence of a gen
  rulefromgen :: Gen -> (Rule,Proof,FilePos)
  rulefromgen (G {genfp = posi, gengen = gen, genspc = spc} )
    = (Ru
         Implication
         (Tm (mIs spc))
         posi
         (Tm (mIs gen))
         [Tm (mIs spc), Tm (mIs gen)]
         []
         (spc,gen)
         0  --REMARK -> rules are renumbered after enriching the context
         [] --REMARK -> if somebody cares then I think it is consistent that the Gen keeps track of the pattern too
       , Proven gammaisa [Stmt $ fromIsa (spc,gen)],posi) --TODO -> Type is not a good name for this proof tree

  --DESCR -> enriching ctxos
  --         bind the expression and nested object defs of all object defs in the context
  ctxobjdefs :: [(ObjectDef,[(Proof,FilePos)])]
  ctxobjdefs = [bindObjDef od Nothing | od<-ctxos cx]
  --add the upper expression to me and infer me and bind type
  --pass the new upper expression to the children and bindObjDef them
  bindObjDef ::  ObjectDef -> Maybe Expression -> (ObjectDef,[(Proof,FilePos)])
  bindObjDef od mbtopexpr =  (od {objctx=bindexpr, objats=bindats},(proof,objpos od):proofats)
    where
    expr = case mbtopexpr of
      Nothing -> (objctx od)
      Just topexpr -> F [topexpr,(objctx od)]
    proof = infer (gamma adlexpr) adlexpr
    adlexpr = fromExpression expr
    bindexpr = case proof of
      Proven gm (inftree:_) -> bindSubexpr (objctx od) $ removeF $ evaltree gm inftree
      _ -> (objctx od)
    inferats = [bindObjDef oa (Just expr) | oa<-objats od]
    bindats = [oa|(oa,_)<-inferats]
    proofats = foldr (++) [] [proofs|(_,proofs)<-inferats]
    removeF et = case mbtopexpr of
      Nothing -> et
      Just _ -> case et of 
          (BoundTo (Semicolon _ ex2 _)) -> BoundTo ex2
          _ -> error $ "Error in TypeChecker.hs module TypeChecker function enrichCtx.bindObjDef.removeF: " ++
                       "Expected a BoundTo relative composition expression statement."++show et++"."
  
  ctxkeys :: [(KeyDef,[(Proof,FilePos)])]
  ctxkeys = [bindKeyDef kd | kd<-allCtxKeyDefs [cx]]   
  bindKeyDef :: KeyDef -> (KeyDef,[(Proof,FilePos)])
  bindKeyDef kd = (kd {kdctx=bindexpr, kdats=bindats},(proof,kdpos kd):proofats)
    where
    proof = infer (gamma adlexpr) adlexpr
    adlexpr = fromExpression $ kdctx kd
    bindexpr = case proof of
      Proven gm (inftree:_) -> bindSubexpr (kdctx kd) $ evaltree gm inftree
      _ -> (kdctx kd)
    (Obj {objats=bindats},proofats) = bindObjDef 
                   (Obj {objats=kdats kd,
                         objnm=kdlbl kd,
                         objpos=kdpos kd,
                         objctx=bindexpr,
                         objstrs=[[]]}) Nothing

                                    
  --DESCR -> decomposing Statement is opposite of TypeInference.fromExpression
  bindSubexpr :: Expression -> Statement -> Expression
  bindSubexpr (Tc ex) x = Tc $ bindSubexpr ex x 
  bindSubexpr (K0 ex) x = K0 $ bindSubexpr ex x 
  bindSubexpr (K1 ex) x = K1 $ bindSubexpr ex x 
  bindSubexpr (Cp ex) (BoundTo (Complement adlex _)) = Cp $ bindSubexpr ex (BoundTo adlex) 
  bindSubexpr (F []) _ = F []
  bindSubexpr (F (ex:rexs)) x@(BoundTo (Semicolon adlex1 adlex2 _)) = 
    case rexs of
      rex:[] -> F [bindSubexpr ex (BoundTo adlex1), bindSubexpr rex (BoundTo adlex2)]
      _:_    -> let 
                F bexs = bindSubexpr (F rexs) (BoundTo adlex2) 
                in
                F (bindSubexpr ex (BoundTo adlex1):bexs)
      []     -> F [bindSubexpr ex x]
  bindSubexpr (Fd []) _ = Fd []
  bindSubexpr (Fd (ex:rexs)) x@(BoundTo (Dagger adlex1 adlex2 _)) = 
    case rexs of
      rex:[] -> Fd [bindSubexpr ex (BoundTo adlex1), bindSubexpr rex (BoundTo adlex2)]
      _:_    -> let 
                Fd bexs = bindSubexpr (Fd rexs) (BoundTo adlex2) 
                in
                Fd (bindSubexpr ex (BoundTo adlex1):bexs)
      []     -> Fd [bindSubexpr ex x]
  bindSubexpr (Fu subexs) (BoundTo (Union adlexs _)) = Fu $ bindSubexprs subexs adlexs 
  bindSubexpr (Fi subexs) (BoundTo (Intersect adlexs _)) = Fi $ bindSubexprs subexs adlexs
  bindSubexpr (Tm mp) (BoundTo (Flip adlex _)) = bindSubexpr  (Tm mp) (BoundTo adlex)
  bindSubexpr (Tm mp) stmt@(BoundTo adlex@(Relation{})) = 
    let
    (ec1,ec2) = evalstmt stmt
    gen = (toGen $ exprsrc adlex)
    in
    if (rel adlex)==mp 
    then Tm $ case mp of
      Mph{} -> mp {mphtyp=if mphyin mp then (ec1,ec2) else (ec2,ec1)}
      I{} -> mp {mphgen=if gen==Anything then ec1 else gen, mphspc=ec1}
      V{} -> mp {mphtyp=(ec1,ec2)}
      _ -> mp --TODO -> other morphisms are returned as parsed, is this correct?
    else error $ "wrong mp bindSubexpr"      
  bindSubexpr x y = error $ "mismatch bindSubexpr" ++ show x ++ show y

  bindSubexprs :: Expressions ->  [AdlExpr] -> Expressions
  bindSubexprs subexs [] = if null subexs then [] else error "not all subexprs matched"
  bindSubexprs [] adlexs = if null adlexs then [] else error "not all subexprs matched"
  bindSubexprs (subex:subexs) (adlex:adlexs) = 
      (bindSubexpr subex (BoundTo adlex)):(bindSubexprs subexs adlexs)

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
   ------------------
   --Common functions
   ------------------

   --DESCR -> function elem provided with own equality function
   --USE   -> use when not instance Eq a or if another predicate is needed then implemented by instance Eq a
elemBy :: (a->a->Bool)->a->[a]->Bool
elemBy _ _ [] = False --not in list
elemBy eq el (el':els) = (eq el el') || (elemBy eq el els)

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

