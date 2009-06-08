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
import TypeInference.Statements
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

printexpr gamma inv ex@(Relation mp _ tp)= show mp ++ "[" ++ (show $ evalstmt gamma (BoundTo ex, inv)) ++"]"
printexpr gamma inv (Implicate expr1 expr2 _)= printexpr gamma inv expr1 ++ "|-" ++ printexpr gamma inv expr2
printexpr gamma inv (Equality expr1 expr2 _)= printexpr gamma inv expr1 ++ "=" ++ printexpr gamma inv expr2
printexpr gamma inv (Union expr1 expr2 _)= printexpr gamma inv expr1 ++ "\\/" ++ printexpr gamma inv expr2
printexpr gamma inv (Intersect expr1 expr2 _)= printexpr gamma inv expr1 ++ "/\\" ++ printexpr gamma inv expr2
printexpr gamma inv (Semicolon expr1 expr2 _)= printexpr gamma inv expr1 ++ ";" ++ printexpr gamma inv expr2
printexpr gamma inv (Dagger expr1 expr2 _)= printexpr gamma inv expr1 ++ "!" ++ printexpr gamma inv expr2
printexpr gamma inv (Complement expr _)= "-" ++ printexpr gamma inv expr
printexpr gamma inv (Flip expr _)= printexpr gamma inv expr ++ "~"

--DESCR -> The parser composes an Architecture object. This function typechecks this object.
--USE   -> This is the only function needed outside of the TypeChecker
typecheck :: Architecture -> (Contexts, Errors)
typecheck arch@(Arch ctxs) = (enriched, checkresult)  
--                   ["TYPE -> " ++ show (sign proof) | (proof@(Proven _ trees),_)<-allproofs, tree<-trees])
--                   ["(show $ evaltree (tree,False)) | (proof@(Proven _ trees),_)<-allproofs, tree<-trees])
--                    [printexpr gamma inv ex |(gamma,(BoundTo ex,inv))<-[(g,evaltree (tree,False))|(Proven g trees,_)<-allproofs, tree<-trees]])
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
     alternatives = [TypeOf $ Relation mp i $ TT (CF (Generic,c1,c1)) (CF (Generic,c2,c2)) | Sgn{decnm=decl,desrc=c1,detgt=c2}<-rv, decl==r1]
     in
     if null alternatives
     then [InfErr (UndeclRel (Relation mp i t))]
     else alternatives
  mphStmts (Relation mp@(I{mphats=[c1]}) i _) = [TypeOf $ Relation mp i $ TT (CF (Generic,c1,c1)) (CF (Generic,c1,c1))]
  mphStmts (Relation mp@(I{}) i _) = [TypeOf $ Relation mp i $ TT (CF (Generic,Anything,Anything)) (CF (Generic,Anything,Anything))]
  mphStmts (Relation mp@(V{mphats=[c1,c2]}) i _) = [TypeOf $ Relation mp i $ TT (CF (Generic,c1,c1)) (CF (Generic,c2,c2))]
  mphStmts (Relation mp@(V{}) i _) = [TypeOf $ Relation mp i $ TT (CF (Generic,Anything,Anything)) (CF (Generic,Anything,Anything))]
  mphStmts (Relation (Mp1{}) _ _ ) = [] --TODO -> ???
  mphStmts (Implicate expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Equality expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Union expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Intersect expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
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
          Proven _ (inftree:_) -> bindSubexpr (rrcon r) $ evaltree (inftree,False) --bind subexpressions according to trees
          _ -> rrcon r --copy rule as parsed
        bindtype =  case proof of
          Proven _ _ -> sign proof
          NoProof{} -> rrtyp r
        in
        (r {rrcon=bindcon, rrtyp=bindtype},proof,rrfps r) 
    | otherwise = 
        let
        proof = infer (gamma adlexpr) adlexpr
        adlexpr = fromRule r
        bindant = case proof of
          Proven _ (inftree:_) -> bindSubexpr (rrant r) $ etant $ evaltree (inftree,False)
          _ -> rrant r --copy rule as parsed 
        bindcon = case proof of
          Proven _ (inftree:_) -> bindSubexpr (rrcon r) $ etcon $ evaltree (inftree,False)
          _ -> rrant r --copy rule as parsed
        bindtype =  case proof of
          Proven _ _ -> sign proof
          NoProof{} -> rrtyp r
        etant et = 
          if rrsrt r==Implication 
          then case et of 
             (BoundTo (Implicate antex _ _),inv) -> (BoundTo antex,inv)
             _ -> error $ "Error in TypeChecker.hs module TypeChecker function enrichCtx.bindRule.etant: " ++
                          "Expected a BoundTo implication rule statement."++show et++"."
          else case et of 
             (BoundTo (Equality antex _ _),inv) -> (BoundTo antex,inv)
             _ -> error $ "Error in TypeChecker.hs module TypeChecker function enrichCtx.bindRule.etant: " ++
                          "Expected a BoundTo equivalence rule statement."++show et++"."
        etcon et = 
          if rrsrt r==Implication 
          then case et of 
             (BoundTo (Implicate _ conex _),inv) -> (BoundTo conex,inv)
             _ -> error $ "Error in TypeChecker.hs module TypeChecker function enrichCtx.bindRule.etcon: " ++
                          "Expected a BoundTo implication rule statement."++show et++"."
          else case et of 
             (BoundTo (Equality _ conex _),inv) -> (BoundTo conex,inv)
             _ -> error $ "Error in TypeChecker.hs module TypeChecker function enrichCtx.bindRule.etcon: " ++
                          "Expected a BoundTo equivalence rule statement."++show et++"."
        in 
        (r {rrant=bindant, rrcon=bindcon, rrtyp=bindtype},proof,rrfps r)
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
      Proven _ (inftree:_) -> bindSubexpr (objctx od) $ removeF $ evaltree (inftree,False)
      _ -> (objctx od)
    inferats = [bindObjDef oa (Just expr) | oa<-objats od]
    bindats = [oa|(oa,_)<-inferats]
    proofats = foldr (++) [] [proofs|(_,proofs)<-inferats]
    removeF et = case mbtopexpr of
      Nothing -> et
      Just _ -> case et of 
          (BoundTo (Semicolon _ ex2 _),inv) -> (BoundTo ex2,inv)
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
      Proven _ (inftree:_) -> bindSubexpr (kdctx kd) $ evaltree (inftree,False)
      _ -> (kdctx kd)
    (Obj {objats=bindats},proofats) = bindObjDef 
                   (Obj {objats=kdats kd,
                         objnm=kdlbl kd,
                         objpos=kdpos kd,
                         objctx=bindexpr,
                         objstrs=[[]]}) Nothing

                                    
  --TODO
  --DESCR -> decomposing Statement is opposite of TypeInference.fromExpression
  bindSubexpr :: Expression -> (Statement,Bool) -> Expression
  bindSubexpr (Tc ex) x = Tc $ bindSubexpr ex x 
  bindSubexpr (K0 ex) x = K0 $ bindSubexpr ex x 
  bindSubexpr (K1 ex) x = K1 $ bindSubexpr ex x 
  bindSubexpr (Cp ex) (BoundTo (Complement adlex _),inv) = Cp $ bindSubexpr ex (BoundTo adlex,not inv) 
  bindSubexpr (F []) _ = F []
  bindSubexpr (F (ex:rexs)) x@(BoundTo (Semicolon adlex1 adlex2 _),inv) = 
    case rexs of
      rex:[] -> F [bindSubexpr ex (BoundTo adlex1,inv), bindSubexpr rex (BoundTo adlex2,inv)]
      _:_    -> let 
                F bexs = bindSubexpr (F rexs) (BoundTo adlex2,inv) 
                in
                F (bindSubexpr ex (BoundTo adlex1,inv):bexs)
      []     -> F [bindSubexpr ex x]
  bindSubexpr (Fd []) _ = Fd []
  bindSubexpr (Fd (ex:rexs)) x@(BoundTo (Dagger adlex1 adlex2 _),inv) = 
    case rexs of
      rex:[] -> Fd [bindSubexpr ex (BoundTo adlex1,inv), bindSubexpr rex (BoundTo adlex2,inv)]
      _:_    -> let 
                Fd bexs = bindSubexpr (Fd rexs) (BoundTo adlex2,inv) 
                in
                Fd (bindSubexpr ex (BoundTo adlex1,inv):bexs)
      []     -> Fd [bindSubexpr ex x]
  bindSubexpr (Fu []) _ = Fu []
  bindSubexpr (Fu (ex:rexs)) x@(BoundTo (Union adlex1 adlex2 _),inv) = 
    case rexs of
      rex:[] -> Fu [bindSubexpr ex (BoundTo adlex1,inv), bindSubexpr rex (BoundTo adlex2,inv)]
      _:_    -> let 
                Fu bexs = bindSubexpr (Fu rexs) (BoundTo adlex2,inv) 
                in
                Fu (bindSubexpr ex (BoundTo adlex1,inv):bexs)
      []     -> Fu [bindSubexpr ex x]
  bindSubexpr (Fi []) _ = Fi []
  bindSubexpr (Fi (ex:rexs)) x@(BoundTo (Intersect adlex1 adlex2 _),inv) = 
    case rexs of
      rex:[] -> Fi [bindSubexpr ex (BoundTo adlex1,inv), bindSubexpr rex (BoundTo adlex2,inv)]
      _:_    -> let 
                Fi bexs = bindSubexpr (Fi rexs) (BoundTo adlex2,inv) 
                in
                Fi (bindSubexpr ex (BoundTo adlex1,inv):bexs)
      []     -> Fi [bindSubexpr ex x]
  bindSubexpr (Tm mp) (BoundTo (Flip adlex _),inv) = bindSubexpr  (Tm mp) (BoundTo adlex,inv)
  bindSubexpr (Tm mp) (BoundTo adlex@(Relation{}),inv) = 
    if (rel adlex)==mp 
    then Tm $ case mp of
      Mph{} -> mp {mphtyp=if mphyin mp then (ec1,ec2) else (ec2,ec1) } --REMARK -> not bound to mphdecl because this can be read from the inference tree 
      I{} -> mp {mphgen=if gen==Anything then spc else gen, mphspc=spc}
      V{} -> mp {mphtyp=(ec1,ec2)}
      _ -> mp --TODO -> other morphisms are returned as parsed, is this correct?
    else error $ "wrong mp bindSubexpr"
      where
      (ec1,ec2) = if inv then (evalCT gammaisa $ inverseCT c1, evalCT gammaisa $ inverseCT c2) else (evalCT gammaisa c1,evalCT gammaisa c2) 
      c1=exprsrc adlex
      c2=exprtgt adlex
      (spc,gen) = case c1 of
         CF (_,x,y) -> (x,y)
         CT x -> (x,x)
         CTake (_,cs) -> (takec gammaisa (Specific,cs), takec gammaisa (Generic,cs))
  bindSubexpr x y = error $ "mismatch bindSubexpr" ++ show x ++ show y

{-
                           --DESCR -> Binding expressions and morphisms
                           bindExprs :: Expressions -> Expressions
                           bindExprs exprs = [bindExpr x (foldlubcpts (srcs exprs),foldlubcpts (tgts exprs))| x<-exprs]
                                    where
                                    foldlubcpts cpts = foldr (lubcpt isatree) AllCpt cpts
                                    srcs exprs' = map rtsrc (rts exprs')
                                    tgts exprs' = map rttgt (rts exprs')
                                    rts exprs' = [infer isatree $ castExpressionToAdlExpr (isatree,rv) x | x<-exprs']
                                    rtsrc (RelationType (s,_)) = s
                                    rtsrc _ = error "Error in function enrich -> bindExprs -> rtsrc: relation type could not be determined."
                                    rttgt (RelationType (_,t)) = t
                                    rttgt _ = error "Error in function enrich -> bindExprs -> rttgt: relation type could not be determined."
                           bindExpr :: Expression -> (Cpt,Cpt) -> Expression
                           bindExpr expr (src,tgt) =
                                    let
                                    adlexpr = castExpressionToAdlExpr (isatree,rv) expr
                                    RelationType (src', tgt') = infer isatree adlexpr
                                    bindsource = lubcpt isatree src src'
                                    bindtarget = lubcpt isatree tgt tgt'
                                    bindtype = (bindsource,bindtarget)
                                    in
                                    case expr of
                                         Tm{} -> Tm $ bindMph (m expr) bindtype
                                         Tc{} -> Tc $ bindExpr (e expr) (src,tgt)
                                         K0{} -> K0 $ bindExpr (e expr) (src,tgt)
                                         K1{} -> K1 $ bindExpr (e expr) (src,tgt)
                                         Cp{} -> Cp $ bindExpr (e expr) (src,tgt)
                                         Fu{} -> Fu $ bindExprs (es expr)
                                         Fi{} -> Fi $ bindExprs (es expr)
                                         F{}  -> bindF (es expr)
                                                    where
                                                    bindF [] = F []
                                                    bindF (x:[]) = F [bindExpr x (src,tgt)]
                                                    bindF (left:right:[]) =
                                                          let
                                                          RelationType (_,targetleft)  = infer isatree $ castExpressionToAdlExpr (isatree,rv) left
                                                          RelationType (sourceright,_) = infer isatree $ castExpressionToAdlExpr (isatree,rv) right
                                                          middle = lubcpt isatree targetleft sourceright
                                                          in
                                                          F [bindExpr left (bindsource,middle), bindExpr right (middle,bindtarget)]
                                                    bindF (left:right) =
                                                          let
                                                          RelationType (_,targetleft)  = infer isatree $ castExpressionToAdlExpr (isatree,rv) left
                                                          RelationType (sourceright,_) = infer isatree $ castExpressionToAdlExpr (isatree,rv) (F right)
                                                          middle = lubcpt isatree targetleft sourceright
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
                                                          RelationType (_,targetleft)  = infer isatree $ castExpressionToAdlExpr (isatree,rv) left
                                                          RelationType (sourceright,_) = infer isatree $ castExpressionToAdlExpr (isatree,rv) right
                                                          middle = lubcpt isatree targetleft sourceright
                                                          in
                                                          Fd [bindExpr left (bindsource,middle), bindExpr right (middle,bindtarget)]
                                                    bindFd (left:right) =
                                                          let
                                                          RelationType (_,targetleft)  = infer isatree $ castExpressionToAdlExpr (isatree,rv) left
                                                          RelationType (sourceright,_) = infer isatree $ castExpressionToAdlExpr (isatree,rv) (Fd right)
                                                          middle = lubcpt isatree targetleft sourceright
                                                          Fd boundright = (bindExpr (Fd right) (middle,bindtarget))
                                                          in
                                                          Fd $ (bindExpr left (bindsource,middle)):boundright

                           bindMph :: Morphism -> (Cpt,Cpt) -> Morphism
                           bindMph mp@(Mph {}) (src,tgt) =
                                    let
                                    tp = (toConcept $ lubcpt isatree src (fromConcept (source mp)),
                                          toConcept $ lubcpt isatree tgt (fromConcept (target mp)))
                                    FoundDr dcl = srchDeclRelByMorphism rv mp
                                    in
                                    mp {mphtyp=tp, mphdcl=dcl} --TODO
                           bindMph i@(I {}) (src,tgt) =
                                    let
                                    gen' = if isA isatree src tgt then src else tgt
                                    spc  = if isA isatree tgt src then src else tgt
                                    gen  = if gen' == AllCpt then spc else gen' --DESCR -> gen is not allowed to stay Anything
                                    in
                                    i {mphgen=toConcept gen, mphspc=toConcept spc}
                           bindMph vm@(V {}) (src,tgt) =
                                    let
                                    tp = (toConcept $ lubcpt isatree src (fromConcept (source vm)),
                                          toConcept $ lubcpt isatree tgt (fromConcept (target vm)))
                                    in
                                    vm {mphtyp=tp}
                           bindMph x _ = x --TODO -> other morphisms are returned as parsed, is this correct?

-}

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

