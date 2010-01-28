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
--import TypeInference.InfLib
import TypeInferenceEngine
import ShowADL
import Collection     ( Collection(..) )

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
   check1 = checkCtxNameUniqueness ctxs -- checks whether all contexts have unique names.
   check2 = checkCtxExtLoops ctxs -- check whether there are loops in the extends-relation (which existst between contexts)
   (enriched, allproofs) = enrichArch arch  
   check3 = [(errproof,fp,rule) | (errproof@(NoProof{}),fp,rule)<-allproofs] --all type errors TODO -> pretty printing add original Expression and fp here
   printcheck3 = [ show errproof 
                   ++ "\n   in " ++ showADL rule   
                   ++ "\n   at " ++ show fp ++ "\n"
                 | (errproof,fp,OrigRule rule)<-check3] ++
                 [ show errproof 
                   ++ "\n   in service definition expression " ++ showADL expr  
                   ++ "\n   at " ++ show fp ++ "\n"
                 | (errproof,fp,OrigObjDef expr)<-check3] ++
                 [ show errproof 
                   ++ "\n   in key definition expression " ++ showADL expr  
                   ++ "\n   at " ++ show fp ++ "\n"
                 | (errproof,fp,OrigKeyDef expr)<-check3]
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
-- Each error is exposed by a NoProof in the Proof-field, which carries the type errors.
-- Each valid type derivation is given by a Proven in the Proof-fiels, which carries the derivation.
enrichArch :: Architecture -> (Contexts,[(Proof,FilePos,OrigExpr)])
enrichArch (Arch ctxs) = ( [enrichedctx | (enrichedctx,_)<-[enrichCtx cx ctxs|cx<-ctxs]]
                            , concat [infresult | (_,infresult)<-[enrichCtx cx ctxs|cx<-ctxs]])

--DESCR -> contains enrichment functionality which should be temporary
postenrich :: Context -> Context
postenrich cx@(Ctx{}) = addsgndecls $ renumber cx
renumber :: Context -> Context
renumber cx@(Ctx{}) = cx {ctxpats=renumberPats (length (ctxrs cx)) (ctxpats cx),
                          ctxrs=[r{r_pat="", runum=i} | (i,r)<-zip [1..] (ctxrs cx)]}
  where
   renumberPats :: Int -> Patterns -> Patterns
   renumberPats n (pat:ps) = pat{ptrls=[r{r_pat=name pat, runum=i} | (i,r)<-zip [n..] (ptrls pat)]}
                             : renumberPats (n+length (ptrls pat)) ps
   renumberPats i [] = []

addsgndecls ::Context -> Context
addsgndecls cx@(Ctx{}) = cx {ctxds=(ctxds cx)++allsgndecls }
  where allsgndecls = [srrel r | r<-allPatRules (allCtxPats [cx]), isSignal r]

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
enrichCtx cx@(Ctx{}) ctxs = --if zzz then error(show xxx) else
  (postenrich $ 
      cx {ctxisa  = hierarchy, -- 
          ctxwrld = world, --
          ctxpats = [p | (p,_)<-ctxpatterns], -- all rules inside the scope of patterns
          ctxrs   = [rule | (rule,_,_)<-ctxCtxRules], -- all rules outside the scope of patterns and all rules from within patterns
          ctxds   = ctxdecls, -- 
          ctxos   = [od | (od,_)<-ctxobjdefs], 
          ctxks   = [kd | (kd,_)<-ctxCtxKeys],
          ctxsql  = [plug | (plug,_)<-ctxsqlplugs],
          ctxphp  = [plug | (plug,_)<-ctxphpplugs]} 
  ,  [(proof,fp,OrigRule rule)|(rule,proof,fp)<-ctxCtxRules]
   ++[(proof,fp,OrigRule rule)|(_,rs)<-ctxpatterns, (rule,proof,fp)<-rs]
   ++[(proof,fp,OrigObjDef expr)|(_,proofs)<-ctxobjdefs, (proof,fp,expr)<-proofs]
   ++[(proof,fp,OrigObjDef expr)|(_,proofs)<-ctxsqlplugs, (proof,fp,expr)<-proofs]
   ++[(proof,fp,OrigObjDef expr)|(_,proofs)<-ctxphpplugs, (proof,fp,expr)<-proofs]
   ++[(proof,fp,OrigKeyDef expr)|(_,proofs)<-ctxCtxKeys, (proof,fp,expr)<-proofs]
   ++[(proof,fp,OrigKeyDef expr)|(_,proofs)<-ctxPatKeys, (proof,fp,expr)<-proofs]
  ) 
                           {-
                           (ctxnm cx)   --copy name
                           (ctxon cx)   --copy extended ctxs
                           (hierarchy)  --construct Isa with all Concepts in scope
                           (world)      --construct the world with this cx on top of the world
                           (ctxpats cx) --bind rules and keydefs in patterns
                           (ctxCtxRules)   --rules from gens and patterns of this context only
                           (ctxdecls)   --relations declared in this context only, outside the scope of patterns
                           (ctxcptdefs) --concept defs of this context only
                           (ctxks cx)   --bind keydefs
                           (ctxos cx)   --change mphdcl and mphtyp on morphisms in expressions
                           (ctxpops cx) --copy populations
                              -}
  where
--  (zzz,xxx) = (True,adlinfertest (declarations allCtx) tc (gens allCtx) (head[rrant r|p<-ctxpats cx,r<-ptrls p]))
  --ctxinf = ctx
  --TODO -> generate rules from props UNI etc
  --DESCR -> enriching ctxwrld
  ctxtree = buildCtxTree (Found cx) ctxs
  Cl _ world = toClassification $ ctxtree
  allCtx = map fromFoundCtx $ flatten ctxtree

  tc :: Concepts
  tc = allCtxCpts allCtx
  --REMARK -> tc0 is defined in specification but not needed in implementation to lookup these constants. They are just used when needed
  --tc0 :: Concepts
  --tc0 = [Anything,NOthing]
  isatree = isaRels tc (gens allCtx)
  isaStmts = map fromIsa isatree
  gamma expr = mphStmts expr ++ gammaisa
  gammaisa = isaStmts
-- The function mphStmts finds bindings (Statements) for the morphisms that occur in AdlExpr mp.
  mphStmts :: AdlExpr -> [Statement]
  mphStmts (Relation mp@(Mph{mphnm=r1}) i t) =
     let
     --REMARK -> inference rule T-RelDecl is evaluated to a TypeOf statement and not implemented explicitly
     --          T-RelDecl won't be in the inference tree for this reason.
     alternatives = [DeclExpr (Relation (mp{mphdcl=dc}) i $ fromSign (c1,c2)) (ishomo dclprops) 
                    | dc@(Sgn{decnm=decl,desrc=c1,detrg=c2, decprps=dclprops})<-declarations allCtx, decl==r1]
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
  mphStmts (Relation mp@(Mp1{mphats=[c1]}) i _ ) = [DeclExpr (Relation mp i $ fromSign (c1,c1)) True]
  mphStmts (Relation mp@(Mp1{}) i _ ) = [DeclExpr (Relation mp i unknowntype) True]
  mphStmts (Implicate expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Equality expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Union exprs _) = concat $ map mphStmts exprs
  mphStmts (Intersect exprs _) = concat $ map mphStmts exprs
  mphStmts (Semicolon expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Dagger expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Complement expr _) = mphStmts expr
  mphStmts (Flip expr _) = mphStmts expr

  --DESCR -> enriching ctxisa
  {- WAAROM worden de concepten in de Isa structuur gepopuleerd? Gebeurt er ooit iets met deze populatie?
  -- WAAROM worden de concepten in isac nogmaals gepopuleerd, terwijl de concepten uit isar al gepopuleerd zijn?
  -- WAAROM loopt dit via Set.?
  hierarchy = Isa isar $map populate (Set.toList $ (Set.fromList $ allCtxCpts ctxs) Set.\\ (Set.fromList isac))
    where
    isar = [(populate$gengen g,populate$genspc g) | g<-gens ctxs]
    isac = map populate$rd (map fst isar++map snd isar)
  -}
  -- WAAROM wordt Anything weggefilterd?
  -- DAAROM (SJ) Gedurende het type checken zijn types nog niet toegekend, waardoor concs soms Anything bevat.
  -- Na het typechecking proces mag dat niet meer voorkomen, en dient concs altijd concepten van de vorm C{} op te leveren.
  hierarchy = Isa isar isac
    where
    isar = [(populate$gengen g,populate$genspc g) | g<-gens ctxs] -- gens levert uitsluitend concepten op van de vorm C{}, ofschoon dat hier niet gecheckt wordt.
    isac = map populate$[c | c@C{}<-concs ctxs] >- [c | g<-gens ctxs, c@C{}<-[gengen g,genspc g]] 


  --DESCR -> enriching ctxpats
  ctxpatterns :: [(Pattern,[(Rule,Proof,FilePos)])]
  ctxpatterns
     = [ bindPat p| p<-ctxpats cx ]               -- all rules that are declared in the ADL-script within
                                                    --     the patterns of this context
  bindPat p@(Pat{}) = (p {ptrls= boundrules ,ptkds= boundkds, ptdcs=addpopu},bindrules)
    where
    bindrules = map bindRule $ ptrls p
    boundrules = [br | (br,_,_)<-bindrules
                  --REMARK -> no rules generated in pattern because of generation of func spec, showadl etc. 
                  --           ++[r |d<-ptdcs p, r<-multRules d]
                  --          ++[r|(r,_,_)<-[rulefromgen g | g<-ptgns p]] 
                  ]
    --REMARK -> keydefs are copied into ctxkd, and need only be bound in the pattern, not checked like rules!
    --TODO -> Make this consistent! (i.e. consistent semantics of context before and after enrich)
    bindkds = map bindKeyDef (ptkds p)
    boundkds = [bk | (bk,_)<-bindkds]
    addpopu = let matches = [d' |d@Sgn{}<-ptdcs p, d'@Sgn{}<-ctxdecls, decfpos d==decfpos d']
              in matches ++ [d|d@Sgn{}<-ptdcs p, not$elem (decfpos d) (map decfpos matches)]

  --DESCR -> enriching ctxds
  --         take all declarations from patterns included (not extended) in this context
  --         Use the sign of the declaration to populate the source and target
  --REMARK -> Only Sgn{} in list of all declarations context
  ctxdecls =  [d{desrc=populate$desrc d, detrg=populate$detrg d} | d@Sgn{}<-popuRels]
  --DESCR -> Determines domain and range population per declaration (i.e. relation)
  --REMARK -> concepts have no population, use ctxdecls instead if needed.
  popuRels :: Declarations
  popuRels = mygroupby matches (declarations ctxs)
      where
      matches = [(p,d) |(p,Left d)<-[(pop,popdeclaration (declarations ctxs) pop) | cx<-ctxs, pop<-ctxpops cx]] 
      mygroupby :: [(Population, Declaration)] -> Declarations -> Declarations
      mygroupby [] res = res
      mygroupby ((p,d):pds) res = mygroupby pds addxtores
        where 
        signpos d d' = d==d' && decfpos d==decfpos d'
        inres = length [()|d'<-res, signpos d d']==1
        addxtores = 
          if inres --ONLY d' is used for merge, while decfpos d==decfpos d'
          then [mrg d' p|d'<-res, signpos d d']++[d'|d'<-res, not(signpos d d')] 
          else (mrg d p):res        
        mrg d p = d{decpopu=decpopu d++[pairx | pairx<-popps p]}
 
  --DESCR -> Determines source and target population based on domains and ranges of all relations
  --         Source and target need to be provided, because it can differ from the sign of the relation
  --         The morphism (in an expression) refering to this relation determines the type.
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
      Mp1{} -> mp { mph1typ=populate$mph1typ mp
                  , mphats=map populate (mphats mp)}
      where popusign (s,t) = (populate s, populate t)
            --lookup the populated declaration in popuRels
            popudecl d = 
              if null allpopd then d 
              else if length allpopd==1 then head allpopd
                   else error$ "!Fatal (module Typechecker 275): function popuMphDecl: " ++
                               "More than one declaration matching morphism "++show (mphnm mp)
                             ++" at "++show (mphpos mp)
                             ++".(remark for developer) Remove duplicate signatures from popuRels if you want to allow this."
               where allpopd = [popd|popd<-popuRels, d==popd] 
  --DESCR -> Add population to concept
  populate :: Concept -> Concept
  populate c@(C{}) = c{cptos=rd$[srcPaire p|d<-popuRels,p<-contents d,elem (source d,c) isatree]
                              ++[trgPaire p|d<-popuRels,p<-contents d,elem (target d,c) isatree]}
  populate c       = c

  ctxPatKeys :: [(KeyDef,[(Proof,FilePos,Expression)])]
  ctxPatKeys = [bindKeyDef kd | pat<-ctxpats cx, kd<-ptkds pat]   

--WAAROM (SJ) werden de multipliciteitsregels gecheckt? Zij zijn immers gegenereerd, en hoeven dus niet gecheckt te worden...
  --DESCR -> enriching ctxrs
  ctxCtxRules :: [(Rule,Proof,FilePos)]
  ctxCtxRules
     = [ bindRule r| r<-ctxrs cx ] ++            -- all rules that are declared in the ADL-script within
                                                 --     this context, but not in the patterns of this context
       [ rulefromKey k (name cx) | k<-ctxks cx]  -- all rules that are derived from all KEY statements in this context
--     [bindRule r |d@Sgn{}<-declarations cx, r<-multRules d]  -- rules that are derived from multiplicity properties in relations in this context

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
        (s,t) = bindrtype
        bindrtype =  case proof of
          Proven _ _ -> sign proof
          NoProof{} -> rrtyp r
        binddecl = (srrel r) {desrc=s, detrg=t}
        in
        (r {rrcon=bindcon, rrtyp=bindrtype, srrel=binddecl},proof,rrfps r) 
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
        (s,t) = bindrtype
        bindrtype =  case proof of
          Proven _ _ -> sign proof
          NoProof{} -> rrtyp r
        binddecl = (srrel r) {desrc=s, detrg=t}
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
        (r {rrant=bindant, rrcon=bindcon, rrtyp=bindrtype, srrel=binddecl},proof,rrfps r)

  ctxrulesgens :: [(Rule,Proof,FilePos)]
  ctxrulesgens = [rulefromgen g | g<-gens cx]
  --TODO -> move rulefromgen to function toRule in module Gen
  --DESCR -> rules deducted from a gen are proven by the existence of a gen
  rulefromgen :: Gen -> (Rule,Proof,FilePos)
  rulefromgen g
    = (Ru
         Implication    -- Implication of Equivalence
         (Tm $ mIs spc) -- left hand side (antecedent)
         (genfp g)      -- position in source file
         (Tm $ mIs gen) -- right hand side (consequent)
         []             -- explanation
         (gen,gen)      -- The type
         Nothing        -- This rule was not generated from a property of some declaration.
         0              -- Rule number. Will be assigned after enriching the context
         (genpat g)     -- For traceability: The name of the pattern. Unknown at this position but it may be changed by the environment.
         False          -- This rule was not specified as a rule in the ADL-script, but has been generated by a computer
         False          -- This is not a signal rule
         (Sgn (name gen++"ISA"++name spc) gen gen [] "" "" "" [] "" (genfp g) 0 False False "")          -- 
       , Proven gammaisa [Stmt $ fromIsa (spc,gen)], genfp g)
    where
    spc = populate (genspc g)
    gen = populate (gengen g)

  rulefromKey :: KeyDef -> String -> (Rule,Proof,FilePos)
  rulefromKey key pat
    = (Ru
         Implication    -- Implication of Equivalence
         antc           -- the antecedent
         (pos key)      -- position in source file
         cons           -- right hand side (consequent)
         []             -- explanation
         (c,c)          -- The type
         Nothing        -- This rule was not generated from a property of some declaration.
         0              -- Rule number. Will be assigned after enriching the context
         pat            -- For traceability: The name of the pattern. Unknown at this position but it may be changed by the environment.
         False          -- This rule was not specified as a rule in the ADL-script, but has been generated by a computer
         False          -- This is not a signal rule
         (Sgn (name key) c c [] "" "" "" [] "" (pos key) 0 False False "")          -- 
       , Proven gammaisa [],pos key)
    where
     c    = kdcpt key
     antc = Fi [F [attexpr,flp attexpr]| attexpr<-[ctx att|att<-kdats key]]
     cons = Tm (mIs c)

  --DESCR -> enriching ctxos
  --         bind the expression and nested object defs of all object defs in the context
  ctxobjdefs :: [(ObjectDef,[(Proof,FilePos,Expression)])]
  ctxobjdefs = [bindObjDef od Nothing | od<-objDefs cx]
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
      Nothing -> objctx od
      Just topexpr -> F [topexpr, objctx od]
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
  
  ctxCtxKeys :: [(KeyDef,[(Proof,FilePos,Expression)])]
  ctxCtxKeys = [bindKeyDef kd | kd<-ctxks cx]   
  bindKeyDef :: KeyDef -> (KeyDef,[(Proof,FilePos,Expression)])
  bindKeyDef kd = (kd {kdats=bindats},proofats)
    where
    (Obj {objats=bindats},proofats) = bindObjDef 
                   (Obj {objats=kdats kd,
                         objnm=kdlbl kd,
                         objpos=kdpos kd,
                         objctx=Tm (mIs (kdcpt kd)),
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
      Mp1{} -> popuMphDecl$mp {mph1typ=ec1}
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
--DESCR -> Eq Context  is defined such that contexts with equal names are considered equal.
checkCtxNameUniqueness :: Contexts -> Errors
checkCtxNameUniqueness [] = []
checkCtxNameUniqueness (cx:ctxs) 
     | elemBy (==) cx ctxs = ["Context name " ++ ctxnm cx ++ " is not unique"] ++ checkCtxNameUniqueness ctxs
     | otherwise           = checkCtxNameUniqueness ctxs

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
checkPopulations ctxs = [err|Right err<-[popdeclaration (declarations ctxs) pop | cx<-ctxs, pop<-ctxpops cx]]


------------------
--Common functions
------------------

--DESCR -> If no declaration can be found than an error message is returned
popdeclaration :: [Declaration] -> Population ->  Either Declaration String
popdeclaration ds p = 
   if length allmatches==1 then Left(head allmatches)
   else if null allmatches
        then Right$"Population of " ++ show (popm p) ++ " at " ++ show (mphpos$popm p) 
                ++ " cannot be related to any relation declaration."
        else Right$"Population of " ++ show (popm p) ++ " at " ++ show (mphpos$popm p) 
                ++ "can be related to multiple relation declarations:\n" 
                ++ concat [show x ++ "\n"|x<-allmatches]
    where
    allmatches = [d|d@(Sgn{})<-ds, matches d (popm p)]
    matches d (Mph{mphnm=popnm, mphats=[c1,c2]}) = popnm==name d && c1==source d && c2==target d
    matches d (Mph{mphnm=popnm, mphats=[]})      = popnm==name d
    matches _ m = error $ "!Fatal (module Population 24): function popdeclaration: " ++
                   "Unrecognized population morphism "++show (mphnm m)++" at "++show (mphpos m)++"."


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


