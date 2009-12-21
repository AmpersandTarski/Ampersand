--Words inside comments that are written in capitals only, such as TODO, provide information to programmers:
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

--REMARK -> In any expression, in case of multiple type errors, choose the innermost errors and yield the first one.
--REMARK -> The ADL.Rule contains all kinds of structures typechecker only supports the one with constructor Ru and Sg.
--          Fr rules will generate a type error message, but the parser (see CC.hs) does not output Fr rules at the moment of writing this comment.
--DESCR ->
--         types are inferred bottom up. First the type of the morphisms is inferred, then the types of the expressions using them are inferred
--         subexpressions are evaluated from left to right if applicable (thus only for the union, intersection, semicolon, and dagger)
module TypeChecker (typecheck, Error, Errors) where

import Auxiliaries  -- USE -> eqClass
import Adl          -- USE -> .MorphismAndDeclaration.makeDeclaration
                    --        and of course many data types
import Data.List    -- USE -> unionBy
import Data.Maybe() -- USE -> fromJust, isNothing
import Data.Tree    -- USE -> data Tree a
import qualified Data.Set as Set --

import Classification --USE -> cast from data.Tree to Classification for enrichment
import Typology --USE -> Isa structure for enrichment

import TypeInference.ITree
import TypeInference.AdlExpr
import TypeInference.Input
import TypeInferenceEngine
import ShowADL
import Collection     ( Collection (rd,uni) )

---------------
--MAIN function
---------------

--USE -> The error if is of type String and contains a complete error message
--       This is the only type needed outside of the TypeChecker.
type Errors = [Error]
type Error = String

--DESCR -> The parser composes an Architecture object. This function typechecks this object.
--REMARK -> After type checking not only the types are bound to expressions, but also other
--          enrichment functionality is implemented
--USE   -> This is the only function needed outside of the TypeChecker
typecheck :: Architecture -> (Contexts, Errors)
typecheck arch@(Arch ctxs) = (enriched, checkresult)  
   where
   --EXTEND -> put extra checking rules of the Architecture object here
   --DESCR  -> check ctx name uniqueness, if that's ok then check the contexts
   check1 = checkCtxNameUniqueness ctxs
   check2 = checkCtxExtLoops ctxs 
   (enriched, allproofs) = enrichArch arch  
   check3 = [(errproof,fp,rule) | (errproof@(NoProof{}),fp,rule)<-allproofs] --all type errors TODO -> pretty printing add original Expression and fp here
   printcheck3 = [show errproof 
                  ++ "\n   in " ++ printadl Nothing 0 rule   
                  ++ "\n   at " ++ show fp ++ "\n" |(errproof,fp,OrigRule rule)<-check3] 
              ++ [show errproof 
                  ++ "\n   in service definition expression " ++ printadl Nothing 0 expr  
                  ++ "\n   at " ++ show fp ++ "\n" |(errproof,fp,OrigObjDef expr)<-check3]
              ++ [show errproof 
                  ++ "\n   in key definition expression " ++ printadl Nothing 0 expr  
                  ++ "\n   at " ++ show fp ++ "\n" |(errproof,fp,OrigKeyDef expr)<-check3]
   check4 = checkSvcNameUniqueness ctxs
   check5 = checkPopulations ctxs
  -- check6 = checkSvcLabels ctxs
   checkresult = if null check1 then 
                    if null check2 then 
                       if null check4 then
                          if null check5 then 
                        --     if null check6 then 
                                if null check3 then [] 
                                else printcheck3 
                        --     else check6 
                          else check5 
                       else check4
                    else check2
                 else check1

------------------
--Enrich functions
------------------
enrichArch :: Architecture -> (Contexts,[(Proof,FilePos,OrigExpr)])
enrichArch (Arch ctxs) = ( [enrichedctx | (enrichedctx,_)<-[enrichCtx cx ctxs|cx<-ctxs]]
                            , concat [infresult | (_,infresult)<-[enrichCtx cx ctxs|cx<-ctxs]])

--DESCR -> contains enrichment functionality which should be temporary
postenrich :: Context -> Context
postenrich cx@(Ctx{}) = addsgndecls $ renumber cx
renumber :: Context -> Context
renumber cx@(Ctx{}) = cx {ctxpats=renumberedPats,
                          ctxrs=renumberRules i (ctxrs cx)}
  where
   (i,renumberedPats) = renumberPats 1 (ctxpats cx)
   renumberPats :: Int -> Patterns -> (Int,Patterns)
   renumberPats i (pat:ps) = (n,pat{ptrls=rs}:renumberedPs)
     where rs = renumberRules i (ptrls pat)
           (n,renumberedPs) = renumberPats (i+length rs) ps
   renumberPats i [] = (i,[])

   renumberRules :: Int -> [Rule] -> [Rule]
   renumberRules i (r:rs) = renumberRule i r: renumberRules (i+1) rs
   renumberRules i    []  = []

   renumberRule :: Int -> Rule -> Rule
   renumberRule n rule 
      = case rule of
          Ru{rrsrt = Automatic} -> rule{rrant = error ("!Fatal (module TypeChecker 120): illegal call to antecedent in renumberRule ("++showADL rule++")")
                                       ,runum = n
                                       }
          Ru{}                  -> rule{runum = n}
          Sg{}                  -> rule{srsig = renumberRule n (srsig rule)
                                       ,runum = n
                                       }
          Fr{}                  -> rule


addsgndecls ::Context -> Context
addsgndecls cx@(Ctx{}) = cx {ctxds=(ctxds cx)++allsgndecls }
  where allsgndecls = [srrel sg | sg@(Sg{})<-allPatRules $ allCtxPats [cx]]

data OrigExpr = OrigRule Rule | OrigObjDef Expression | OrigKeyDef Expression

-- enrichCtx supplies all information to the parse tree of a single context
-- that can only be computed after the type checking process.
-- This consists of:
--   - Rule binding:      This binds type information to expressions and morphisms in Rules and Signals.
--   - ObjectDef binding: Supply type information to expressions and morphisms in ObjectDefs
--   - Key binding:       Supply type information to expressions and morphisms in Keys
--   - Plug binding:      Supply type information to expressions and morphisms in SQL plugs and PHP plugs
--   - population:        All instances supplied by the ADL-script are bound to the right declarations and concepts.
--   - rule generation:   Rules that are derived from multiplicities specified in the ADL-script (UNI,TOT,INJ,SUR,RFX,TRN,SYM,ASY)
--   - rule generation:   Rules that are derived from Keys specified in the ADL-script

enrichCtx :: Context -> Contexts -> (Context,[(Proof,FilePos,OrigExpr)])
enrichCtx cx@(Ctx{}) ctxs = 
  (postenrich $ 
      cx {ctxisa=hierarchy, -- 
          ctxwrld=world, --
          ctxpats=map bindPat (ctxpats cx), -- 
          ctxrs=[rule | (rule,_,_)<-ctxrules], 
          ctxds=ctxdecls, -- 
          ctxos=[od | (od,_)<-ctxobjdefs], 
          ctxks=[kd | (kd,_)<-ctxkeys],
          ctxsql=[plug | (plug,_)<-ctxsqlplugs],
          ctxphp=[plug | (plug,_)<-ctxphpplugs]} 
  ,  [(proof,fp,OrigRule rule)|(rule,proof,fp)<-ctxrules]
   ++[(proof,fp,OrigObjDef expr)|(_,proofs)<-ctxobjdefs, (proof,fp,expr)<-proofs]
   ++[(proof,fp,OrigObjDef expr)|(_,proofs)<-ctxsqlplugs, (proof,fp,expr)<-proofs]
   ++[(proof,fp,OrigObjDef expr)|(_,proofs)<-ctxphpplugs, (proof,fp,expr)<-proofs]
   ++[(proof,fp,OrigKeyDef expr)|(_,proofs)<-ctxkeys, (proof,fp,expr)<-proofs])
                           {-
                           (ctxnm cx)   --copy name
                           (ctxon cx)   --copy extended ctxs
                           (hierarchy)  --construct Isa with all Concepts in scope
                           (world)      --construct the world with this cx on top of the world
                           (ctxpats cx) --bind rules and keydefs in patterns
                           (ctxrules)   --rules from gens and patterns of this context only
                           (ctxdecls)   --declarations of this context only
                           (ctxcptdefs) --concept defs of this context only
                           (ctxks cx)   --bind keydefs
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
  gamma expr = (mphStmts expr) ++ gammaisa
  gammaisa = isaStmts
  mphStmts :: AdlExpr -> [Statement]
  mphStmts (Relation mp@(Mph{mphnm=r1}) i t) =
     let
     --REMARK -> inference rule T-RelDecl is evaluated to a TypeOf statement and not implemented explicitly
     --          T-RelDecl won't be in the inference tree for this reason.
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
  mphStmts (Union exprs _) = concat $ map mphStmts exprs
  mphStmts (Intersect exprs _) = concat $ map mphStmts exprs
  mphStmts (Semicolon expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Dagger expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Complement expr _) = mphStmts expr
  mphStmts (Flip expr _) = mphStmts expr

  --DESCR -> enriching ctxisa
  --in AGtry -> isa = Isa [(g,s)|G pos g s<- _mGen] (concs _mD>-rd [c|G pos g s<- _mGen, c<-[g,s]])
  hierarchy = Isa isar $map populate (Set.toList $ (Set.fromList $ allCtxCpts ctxs) Set.\\ (Set.fromList isac))
    where
    isar = [case g of G{} -> (populate$gengen g,populate$genspc g) | g<-allCtxGens ctxs]
    isac = map populate$rd (map fst isar++map snd isar)

  --DESCR -> enriching ctxpats
  bindPat p@(Pat{}) = p {ptrls= bindrules ,ptkds= bindkds, ptdcs=addpopu}
    where
    bindrules = [br | (br,_,_)<-map bindRule $ 
                                (ptrls p)
                  --REMARK -> no rules generated in pattern because of generation of func spec, showadl etc. 
                  --           ++[r |d<-ptdcs p, r<-multRules d]
                  --          ++[r|(r,_,_)<-[rulefromgen g | g<-ptgns p]] 
                  ]
    bindkds = [bk | (bk,_)<-map bindKeyDef (ptkds p)]
    addpopu = let matches = [d' |d@(Sgn{})<-ptdcs p, d'@(Sgn{})<-ctxdecls, decfpos d==decfpos d']
              in matches ++ [d|d@(Sgn{})<-ptdcs p, not$elem (decfpos d) (map decfpos matches)]

  --DESCR -> enriching ctxds
  --         take all the declarations from all patterns included (not extended) in this context
  --         Use the sign of the declaration to populate the source and target
  --REMARK -> Only Sgn{} in list of all declarations context
  ctxdecls =  [d{desrc=populate$desrc d, detgt=populate$detgt d} | d@(Sgn{})<-popuRels]
  --DESCR -> Determines domain and range population per declaration (i.e. relation)
  --REMARK -> concepts have no population, use ctxdecls instead if needed.
  popuRels = [d{decpopu=decpopu d++
                        [pairx | pop<-ctxpops cx, comparepopanddecl (declarations cx) (popm pop) d, pairx<-popps pop]}
             |d<-declarations cx]
  --DESCR -> Determines source and target population based on domains and ranges of all decls
  --         Source and target need to be provided, because it can differ from the sign of the declaration
  --         The morphism (in an expression) refering to this declaration determines the type.
  popuMphDecl :: Morphism -> Morphism
  popuMphDecl mp = case mp of
      Mph{} -> mp { mphtyp=popusign$mphtyp mp
                  , mphats=map populate (mphats mp)
                  , mphdcl=popudecl$mphdcl mp}
      I{}   -> mp { mphgen=populate$mphgen mp
                  , mphspc=populate$mphspc mp
                  , mphats=map populate (mphats mp)}
      V{}   -> mp { mphtyp=popusign$mphtyp mp
                  , mphats=map populate (mphats mp)}
      _     -> mp
      where popusign (s,t) = (populate s, populate t)
            popudecl d = head [ (head cl){decpopu = foldr1 uni (map decpopu cl)} |cl<-eqClass (==) ctxdecls, head cl==d]
  --DESCR -> Add population to concept
  populate :: Concept -> Concept
  populate c@(C{}) = c{cptos=rd$[srcPaire p|d<-popuRels,p<-contents d,elem (source d,c) isatree]
                              ++[trgPaire p|d<-popuRels,p<-contents d,elem (target d,c) isatree]}
  populate c       = c

  --DESCR -> enriching ctxrs
  ctxrules :: [(Rule,Proof,FilePos)]
  ctxrules
     = [ bindRule r| r<-ctxrs cx ] ++               -- all rules that are declared in the ADL-script within
                                                    --     this context, but not in the patterns of this context
       [ rulefromKey k (name pat)
       | pat<-patterns cx, k<-ptkds pat] ++         -- all rules that are derived from all KEY statements in this context
       [rulefromKey k (name cx) | k<-ctxks cx] ++   -- all rules that are derived from all KEY statements in this context
       [rulefromgen g | g<-allCtxGens [cx]] ++      -- all rules that are derived from all GEN statements in this context
       [bindRule r |d<-declarations cx, r<-multRules d]  -- all multiplicity rules that are derived from all declarations in this context

  --DESCR -> The function bindRule attaches a type to a rule by producing a proof in the type system.
  --REMARK -> The rules are numbered after enriching, see renumber :: Context -> Context
  --          Thus a rule in (cxrls cx) originating from a rule in a pattern, and the original rule
  --          may have different numbers.
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
          _ -> rrcon r --copy rule as parsed
        bindrtype =  case proof of
          Proven _ _ -> sign proof
          NoProof{} -> rrtyp r
        etant et = 
          if rrsrt r==Implication 
          then case et of 
             (BoundTo (Implicate antex _ _)) -> BoundTo antex
             _ -> error $ "!Fatal (module TypeChecker 325): " ++
                          "Expected a BoundTo implication rule statement etant("++show et++")."
          else case et of 
             (BoundTo (Equality antex _ _)) -> BoundTo antex
             _ -> error $ "!Fatal (module TypeChecker 329): " ++
                          "Expected a BoundTo equivalence rule statement etant("++show et++")."
        etcon et = 
          if rrsrt r==Implication 
          then case et of 
             (BoundTo (Implicate _ conex _)) -> BoundTo conex
             _ -> error $ "!Fatal (module TypeChecker 335): " ++
                          "Expected a BoundTo implication rule statement etcon("++show et++")."
          else case et of 
             (BoundTo (Equality _ conex _)) -> BoundTo conex
             _ -> error $ "!Fatal (module TypeChecker 339): " ++
                          "Expected a BoundTo equivalence rule statement etcon("++show et++")."
        in 
        (r {rrant=bindant, rrcon=bindcon, rrtyp=bindrtype},proof,rrfps r)
  bindRule r@(Sg{}) = (r {srsig=bindsig, srtyp=sign bindsig, srrel= binddecl},proof,srfps r)
    where
    (bindsig,proof,_) = bindRule (srsig r)
    binddecl = (srrel r) {desrc=source bindsig, detgt=target bindsig}
  bindRule _ = error $ "!Fatal (module TypeChecker 347): function enrichCtx.bindRule: " ++
                       "Unsupported rule type while enriching the context. The type checker should have given an error."
  ctxrulesgens :: [(Rule,Proof,FilePos)]
  ctxrulesgens = [rulefromgen g | g<-allCtxGens [cx]]
  --TODO -> move rulefromgen to function toRule in module Gen
  --DESCR -> rules deducted from a gen are proven by the existence of a gen
  rulefromgen :: Gen -> (Rule,Proof,FilePos)
  rulefromgen (G {genfp = posi, gengen = gen', genspc = spc', genpat=pat} )
    = (Ru
         Implication    -- Implication of Equivalence
         (Tm $ mIs spc) -- left hand side (antecedent)
         posi           -- position in source file
         (Tm $ mIs gen) -- right hand side (consequent)
         []             -- explanation
         (gen,gen)      -- The type
         Nothing        -- This rule was not generated from a property of some declaration.
         0              -- Rule number. Will be assigned after enriching the context
         pat            -- For traceability: The name of the pattern. Unknown at this position but it may be changed by the environment.
         False          -- This rule was not specified as a rule in the ADL-script, but has been generated by a computer
       , Proven gammaisa [Stmt $ fromIsa (spc,gen)],posi)
    where
    spc = populate spc'
    gen = populate gen'

  rulefromKey :: KeyDef -> String -> (Rule,Proof,FilePos)
  rulefromKey key pat
    = (Ru
         Implication    -- Implication of Equivalence
         antc           -- the antecedent
         (pos key)      -- position in source file
         (Tm $ mIs c)   -- right hand side (consequent)
         []             -- explanation
         (c,c)          -- The type
         Nothing        -- This rule was not generated from a property of some declaration.
         0              -- Rule number. Will be assigned after enriching the context
         pat            -- For traceability: The name of the pattern. Unknown at this position but it may be changed by the environment.
         False          -- This rule was not specified as a rule in the ADL-script, but has been generated by a computer
       , Proven gammaisa [],pos key)
    where
     c    = target (kdctx key)
     antc = Fi [F [attexpr,flp attexpr]| attexpr<-[ctx att|att<-kdats key]]

  --DESCR -> enriching ctxos
  --         bind the expression and nested object defs of all object defs in the context
  ctxobjdefs :: [(ObjectDef,[(Proof,FilePos,Expression)])]
  ctxobjdefs = [bindObjDef od Nothing | od<-ctxos cx]
  ctxsqlplugs :: [(ObjectDef,[(Proof,FilePos,Expression)])]
  ctxsqlplugs = [bindObjDef plug Nothing | plug<-ctxsql cx]
  ctxphpplugs :: [(ObjectDef,[(Proof,FilePos,Expression)])]
  ctxphpplugs = [bindObjDef plug Nothing | plug<-ctxphp cx]
  --add the upper expression to me and infer me and bind type
  --pass the new upper expression to the children and bindObjDef them
  bindObjDef ::  ObjectDef -> Maybe Expression -> (ObjectDef,[(Proof,FilePos,Expression)])
  bindObjDef od mbtopexpr = (od {objctx=bindexpr, objats=bindats},(proof,objpos od,expr):proofats)
    where
    expr = case mbtopexpr of
      Nothing -> (objctx od)
      Just topexpr -> F [topexpr,(objctx od)]
    proof = infer (gamma adlexpr) adlexpr
    adlexpr = fromExpression expr
    bindexpr = case proof of
      Proven gm (inftree:_) -> bindSubexpr (objctx od) $ removeF $ evaltree gm inftree
      _ -> (objctx od)
    newtopexpr =  case mbtopexpr of
      Nothing -> bindexpr
      Just topexpr -> F [topexpr,bindexpr]
    inferats = [bindObjDef oa (Just newtopexpr) | oa<-objats od]
    bindats = [oa|(oa,_)<-inferats]
    proofats = concat [proofs|(_,proofs)<-inferats]
    removeF et = case mbtopexpr of
      Nothing -> et
      Just _ -> case et of 
          (BoundTo (Semicolon _ ex2 _)) -> BoundTo ex2
          _ -> error $ "!Fatal (module TypeChecker 420): function enrichCtx.bindObjDef.removeF: " ++
                       "Expected a BoundTo relative composition expression statement."++show et++"."
  
  ctxkeys :: [(KeyDef,[(Proof,FilePos,Expression)])]
  ctxkeys = [bindKeyDef kd | kd<-allCtxKeyDefs [cx]]   
  bindKeyDef :: KeyDef -> (KeyDef,[(Proof,FilePos,Expression)])
  bindKeyDef kd = (kd {kdctx=bindexpr, kdats=bindats},(proof,kdpos kd,kdctx kd):proofats)
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
  bindSubexpr ex@(Tm (Mph{mphyin=False})) (BoundTo (Flip adlex@(Relation{tt=TT{cts=c1,ctt=c2}}) _)) = 
              bindSubexpr ex (BoundTo (adlex{tt=TT c2 c1 1}))
  bindSubexpr (Tm mp) stmt@(BoundTo adlex@(Relation{})) = 
    let    
    (ec1,ec2)= evalstmt stmt
    gen = toGen $ exprsrc adlex
    in
    if (rel adlex)==mp 
    then Tm $ case mp of
      Mph{} -> popuMphDecl$ (rel adlex) {mphtyp=(ec1,ec2)}
                           --REMARK -> bind to the morphism from the gamma (with mphdcl set) = rel adlex
      I{} -> popuMphDecl$mp {mphgen=if gen==Anything then ec1 else gen, mphspc=ec1}
      V{} -> popuMphDecl$mp {mphtyp=(ec1,ec2)}
      _ -> mp --TODO -> other morphisms are returned as parsed, is this correct?
    else error  $ "!Fatal (module TypeChecker 481): function enrichCtx.bindSubexpr: " ++
                  "Morphisms are different: \nOriginal: " ++ show mp ++ "\nType checked: " ++ show (rel adlex)       
  bindSubexpr x y = error $ "!Fatal (module TypeChecker 483): function enrichCtx.bindSubexpr: " ++
                           "Expressions are different: \nOrginal: " ++ show x ++ "\nType checked: " ++ show y

  bindSubexprs :: Expressions ->  [AdlExpr] -> Expressions
  bindSubexprs subexs [] = if null subexs then [] 
                           else error $ "!Fatal (module TypeChecker 488): function enrichCtx.Subexprs: " ++
                                        "Not all subexprs are matched. Too many originals."
  bindSubexprs [] adlexs = if null adlexs then [] 
                           else error $ "!Fatal (module TypeChecker 491): function enrichCtx.Subexprs: " ++
                                        "Not all subexprs are matched. Too many type checked."
  bindSubexprs (subex:subexs) (adlex:adlexs) = (bindSubexpr subex (BoundTo adlex)):(bindSubexprs subexs adlexs)

-----------------
--Check functions
-----------------
--DESCR -> check rule: Every context must have a unique name
checkCtxNameUniqueness :: Contexts -> Errors
checkCtxNameUniqueness [] = []
checkCtxNameUniqueness (cx:ctxs) 
     | elemBy eqCtx cx ctxs = (notUniqError cx):checkCtxNameUniqueness ctxs
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

--USE -> Contexts names must be unique
checkCtxExtLoops :: Contexts -> Errors
checkCtxExtLoops ctxs = composeError (concat [findLoops cx | cx<- ctxs])
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

--DESCR -> check rule: Every SERVICE, PHPPLUG, and SQLPLUG must have a unique name
checkSvcNameUniqueness :: Contexts -> Errors
checkSvcNameUniqueness ctxs = 
    let svcs = [svc|cx<-ctxs, svc<-ctxos cx ++ ctxsql cx ++ ctxphp cx]
        printerrs [] = []
        printerrs (svcnms:lblnms) = 
          ["Service or plug name " ++ svcnm ++ " is not unique:"++ (concat ["\n"++show svcpos |svcpos<-svcposs])
          |(svcnm,svcposs)<-svcnms] 
          ++
          ["Label " ++ lblnm ++ " in service or plug must be unique on sibling level:"
                    ++ (concat ["\n"++show lblpos |lblpos<-lblposs])
          |lbl<-lblnms, (lblnm,lblposs)<-lbl]
    in  printerrs$checkLabels svcs

--DESCR -> group the services and sibling labels with the same name but different file positions
checkLabels :: [ObjectDef] -> [[(String,[FilePos])]]
checkLabels svcs =
    let orderby :: (Eq a, Eq b) => [(a,b)] ->  [(a,[b])]
        orderby xs =  [(x,rd [y|(x',y)<-xs,x==x']) |x<-rd [dx|(dx,_)<-xs] ]
    in  (orderby [(objnm svc,objpos svc)|svc<-svcs, svc'<-svcs, objnm svc==objnm svc', objpos svc/=objpos svc'])
        :[check | checks<-map checkLabels (map objats svcs), check<-checks]

   

--DESCR -> check rule: Every POPULATION must relate to a declaration
checkPopulations :: Contexts -> Errors
checkPopulations ctxs = if null unrelated then []
    else if null (related$head$unrelated)
         then ["Population of " ++ show (popm$head unrelated) ++ " cannot be related to any relation declaration " 
            ++ show (mphpos$popm$head unrelated)]
         else ["Population "++show (popm$head unrelated)++" at "++show (mphpos$popm$head unrelated)++"\n"
              ++ "can be related to multiple relation declarations:\n" 
              ++ (concat (related$head$unrelated))]
    where
    unrelated = [pop| cx<-ctxs, pop<-ctxpops cx
                    , length (related pop) /= 1]
    related pop = [name d++"["++show (source d)++"*"++show (target d)++"]"++" "++show (decfpos d)++"\n"
                  | d<-allCtxDecls ctxs, comparepopanddecl (allCtxDecls ctxs) (popm pop) d]


------------------
--Common functions
------------------

--DESCR -> function elem provided with own equality function
--USE   -> use when not instance Eq a or if another predicate is needed then implemented by instance Eq a
elemBy :: (a->a->Bool)->a->[a]->Bool
elemBy _ _ [] = False --not in list
elemBy eq el (el':els) = (eq el el') || (elemBy eq el els)

--DESCR -> compare function to compare the morphism identity of a population and a declaration
--         the population morphism is parsed with a declaration with source and target Anything 
--         so I need to use the mphats to compare and thus I can't use makeDeclaration
comparepopanddecl :: Declarations -> Morphism -> Declaration -> Bool
comparepopanddecl _ (Mph{mphnm=popnm, mphats=[c1,c2]}) d = popnm==name d && c1==source d && c2==target d
comparepopanddecl ds (Mph{mphnm=popnm, mphats=[], mphpos=mpos}) _ = 
   case onedecl popnm ds of
        Just True -> True
        _         -> error $ "!Fatal (module TypeChecker 589): function comparepopanddecl: " ++
                     "Ambiguous population "++show popnm++" at "++show mpos++"\nDefine a type on the population name."
comparepopanddecl _ _ _ = False

onedecl :: String -> Declarations -> Maybe Bool
onedecl _ [] = Nothing
onedecl nm (d:ds) = if nm==name d then
                      case (onedecl nm ds) of 
                           Nothing -> Just True
                           _       -> Just False
                    else (onedecl nm ds)

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
toClassification (Node (NotFound cxnm) _) = 
      error $ "!Fatal (module TypeChecker 615): function toClassification: " ++
              "NotFound " ++ cxnm
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
fromFoundCtx (NotFound cxnm) = error $ "!Fatal (module TypeChecker 646): function fromFoundCtx: NotFound " ++ cxnm
fromFoundCtx(Found cx)    = cx


