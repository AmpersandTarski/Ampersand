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

import Adl          -- USE -> .MorphismAndDeclaration.makeDeclaration
                    --        and of course many data types
--import Data.List    -- USE -> unionBy
--import Data.Maybe() -- USE -> fromJust, isNothing
import Data.Tree    -- USE -> data Tree a
--import qualified Data.Set as Set --

import Classification --USE -> cast from data.Tree to Classification for enrichment
import Typology --USE -> Isa structure for enrichment

import TypeInference.Input
import TypeInference.Isa
import TypeInference.InfAdlExpr
import TypeInference.InfExpression
import TypeInference.InfLibAG (InfTree)
import ShowADL
import Collection     ( Collection(..) )
import Text.Pandoc (Block)

---------------
--MAIN function
---------------

--USE -> The error if is of type String and contains a complete error message
--       This is the only type needed outside of the TypeChecker.
type Errors = [Error]
type Error = (String,[Block])

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
   (enriched, check3) = enrichArch arch  
  -- check3 = [(errproof,fp,rule) | (err,fp,rule)<-allproofs] --all type errors TODO -> pretty printing add original Expression and fp here
   printcheck3 = [ (err 
                   ++ "\n   in " ++ showADL rule   
                   ++ "\n   at " ++ show fp ++ "\n",block)
                 | (err,block,fp,OrigRule rule)<-check3] ++
                 [ (err 
                   ++ "\n   in service definition expression " ++ showADL expr  
                   ++ "\n   at " ++ show fp ++ "\n",block)
                 | (err,block,fp,OrigObjDef expr)<-check3] ++
                 [ (err 
                   ++ "\n   in key definition expression " ++ showADL expr  
                   ++ "\n   at " ++ show fp ++ "\n",block)
                 | (err,block,fp,OrigKeyDef expr)<-check3] ++
                 [ (err ++ "\n",block)
                 | (err,block,_,OrigExpl)<-check3]
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
enrichArch :: Architecture -> (Contexts,[(String,[Block],FilePos,OrigExpr)])
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
   renumberPats _ [] = []

addsgndecls ::Context -> Context
addsgndecls cx@(Ctx{}) = cx {ctxds=(ctxds cx)++allsgndecls }
  where allsgndecls = [srrel r | r<-allPatRules (allCtxPats [cx]), isSignal r]

data OrigExpr = OrigRule Rule | OrigObjDef Expression | OrigKeyDef Expression | OrigExpl

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

enrichCtx :: Context -> Contexts -> (Context,[(String,[Block],FilePos,OrigExpr)])
enrichCtx cx@(Ctx{}) ctxs = --if zzz then error(show xxx) else
  (postenrich $ 
      cx {ctxisa  = hierarchy, -- 
          ctxwrld = world, --
          ctxpats = [p | (p,_,_)<-ctxpatterns],                    -- contains all user defined rules inside the scope of patterns
          ctxrs   = [rule | r<-ctxrs cx, Left rule<-[bindRule r]], -- all user defined rules outside the scope of patterns
          ctxds   = ctxdecls, -- 
          ctxos   = [od | (od,_)<-ctxobjdefs], 
          ctxks   = [kd | (kd,_)<-ctxCtxKeys],
          ctxsql  = [plug | (plug,_)<-ctxsqlplugs],
          ctxphp  = [plug | (plug,_)<-ctxphpplugs],
          ctxps   = [x |Left x<-map enrichexpl (ctxps cx)]
         } 
  ,  [err|r<-ctxrs cx, Right err<-[bindRule r]]
   ++[err|(_,rs,_)<-ctxpatterns, Right err<-rs] --rule errors
   ++[(err,[],Nowhere,OrigExpl)|(_,_,errs)<-ctxpatterns, err<-errs] --explanation errors
   ++[(err,[],Nowhere,OrigExpl)|Right err<-map enrichexpl (ctxps cx)]
   ++[err|(_,checkedexprs)<-ctxobjdefs,  Right err<-checkedexprs]
   ++[err|(_,checkedexprs)<-ctxsqlplugs,  Right err<-checkedexprs]
   ++[err|(_,checkedexprs)<-ctxphpplugs,  Right err<-checkedexprs]
   ++[err|(_,checkedexprs)<-ctxCtxKeys, Right err<-checkedexprs]
   ++[err|(_,checkedexprs)<-ctxPatKeys, Right err<-checkedexprs]
  ) 
                           {-
                           (ctxnm cx)   --copy name
                           (ctxon cx)   --copy extended ctxs
                           (hierarchy)  --construct Isa with all Concepts in scope
                           (world)      --construct the world with this cx on top of the world
                           (ctxpats cx) --bind rules and keydefs in patterns
                           (ctxdecls)   --relations declared in this context only, outside the scope of patterns
                           (ctxcptdefs) --concept defs of this context only
                           (ctxks cx)   --bind keydefs
                           (ctxos cx)   --change mphdcl and mphtyp on morphisms in expressions
                           (ctxpops cx) --copy populations
                              -}
  where
  --DESCR -> use this function on all expressions
  enrich_expr :: Expression -> Either ((Concept,Concept), Expression,InfTree) (String,[Block])
  enrich_expr = infertype_and_populate popuMphDecl isas [d| d<-declarations ctxs, decusr d] (Anything,Anything)
  isas = isaRels (allCtxCpts ctxs) (gens ctxs)
  --DESCR -> enriching ctxwrld
  ctxtree = buildCtxTree (Found cx) ctxs
  Cl _ world = toClassification $ ctxtree
--TODO -> declared objects from all ctxs are in scope
--  allCtx = map fromFoundCtx $ flatten ctxtree

  isatree = isaRels (allCtxCpts ctxs) (gens ctxs)

  --DESCR -> enriching ctxisa
  {- WAAROM worden de concepten in de Isa structuur gepopuleerd? Gebeurt er ooit iets met deze populatie?
 - DAAROM  GMI: Ja
  -- WAAROM worden de concepten in isac nogmaals gepopuleerd, terwijl de concepten uit isar al gepopuleerd zijn? 
  -- DAAROM GMI: alleen concepten in een GEN .. ISA .. declaratie zitten in isar
  -- WAAROM loopt dit via Set.? 
  -- DAAROM GMI: Een Set is een lijst zonder doublures. Het had hier weinig nut omdat van de Set meteen weer een lijst wordt gemaakt in Hierarchy. De vraag zou eerder moeten zijn, waarom is Hierarchy een data structuur van twee lijsten en niet van twee Sets.
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
  ctxpatterns :: [(Pattern,[Either Rule (String,[Block],FilePos,OrigExpr)],[String])]
  ctxpatterns
     = [ bindPat p| p<-ctxpats cx ]               -- all rules that are declared in the ADL-script within
                                                    --     the patterns of this context
  bindPat p@(Pat{}) = (p {ptrls= boundrules ,ptkds= boundkds, ptdcs=addpopu, ptxps=[x |Left x<-pexpls]
                         ,inftestexpr=typedexprs [d| d<-declarations ctxs, decusr d] isas (testexpr p) 
                                   ++ [error (concat (concat xs)) | let xs=typeerrors [d| d<-declarations ctxs, decusr d] isas (testexpr p),not(null xs)]
                         }
                      ,bindrules
                      ,[err|Right err<-pexpls])
    where    
    bindrules = map bindRule $ ptrls p
    boundrules = [br | Left br<-bindrules
                  --REMARK -> no rules generated in pattern because of generation of func spec, showadl etc. 
                  --           ++[r |d<-ptdcs p, r<-multrules d]
                  --          ++[r|(r,_,_)<-[rulefromgen g | g<-ptgns p]] 
                  ]
    --REMARK -> keydefs are copied into ctxkd, and need only be bound in the pattern, not checked like rules!
    --TODO -> Make this consistent! (i.e. consistent semantics of context before and after enrich)
    bindkds = map bindKeyDef (ptkds p)
    boundkds = [bk | (bk,_)<-bindkds]
    addpopu = let matches = [d' |d@Sgn{}<-ptdcs p, d'@Sgn{}<-ctxdecls, decfpos d==decfpos d']
              in matches ++ [d|d@Sgn{}<-ptdcs p, not$elem (decfpos d) (map decfpos matches)]
    pexpls = map enrichexpl (ptxps p)

  --Every Explanation must relate to something
  enrichexpl :: PExplanation -> Either PExplanation String
  enrichexpl pExpl = case enrichexplobj (pexObj pExpl) of
                       Left _ -> Left pExpl
                       Right str -> Right str
  
  enrichexplobj :: PExplObj -> Either PExplObj String
  enrichexplobj x@(PExplConceptDef{}) = checkPExplobj (allCtxCpts ctxs) x 
  enrichexplobj (PExplDeclaration mph ) = case enrich_expr (Tm mph (-1)) of
     Left (_,Tm emph _,_) -> Left (PExplDeclaration emph )
     Right (err,_) -> Right ("Explanation for relation "++name mph++" could not be matched to a declaration because "++err)
     x -> error$ "!Fatal (module Typechecker 225): function enrichexplobj: impossible case."
  enrichexplobj x@(PExplRule{}) = checkPExplobj ([r|r<-ctxrs cx]++[r|p<-ctxpats cx,r<-ptrls p]) x
  enrichexplobj x@(PExplKeyDef{}) = checkPExplobj (ctxks cx) x
  enrichexplobj x@(PExplObjectDef{}) = checkPExplobj (objDefs cx) x
  enrichexplobj x@(PExplPattern{}) = checkPExplobj (ctxpats cx) x 
  enrichexplobj x@(PExplContext{}) = checkPExplobj [cx] x

  checkPExplobj :: (Identified a) => [a] -> PExplObj -> Either PExplObj String
  checkPExplobj xs x
     | elem (name x) (map name xs) = Left x
     | otherwise = Right ("There is an explanation for the non-existing "++explobj x++" " ++ name x)
   where
    explobj (PExplConceptDef _) = "concept"
    explobj (PExplDeclaration _) = "declaration"
    explobj (PExplRule _) = "rule"
    explobj (PExplKeyDef _) = "key definition"
    explobj (PExplObjectDef _) = "service definition"
    explobj (PExplPattern _) = "pattern"
   
  --DESCR -> enriching ctxds
  --         take all declarations from patterns included (not extended) in this context
  --         Use the sign of the declaration to populate the source and target
  --REMARK -> Only Sgn{} in list of all declarations context
  ctxdecls =  [d{desrc=populate$desrc d, detrg=populate$detrg d} | d@Sgn{}<-popuRels]
  --DESCR -> Determines domain and range population per declaration (i.e. relation)
  --REMARK -> concepts have no population, use ctxdecls instead if needed.
  popuRels :: Declarations
  popuRels = mygroupby matches (declarations_ctxs)
      where
      -- REMARK:  Use only declarations that are user defined, because generated declarations (e.g. decls from signal rules)
      --          are not typechecked yet! Signal rules cannot be populated at this point.
      --          They need to be populated AFTER type checking based on the populations of user defined relation declarations.
      declarations_ctxs =  [d|cx'<-ctxs,p<-ctxpats cx',d<-ptdcs p++ctxds cx']
      matches = [(p,d) |(p,Left d)<-[(pop,popdeclaration (declarations_ctxs) pop) | cx'<-ctxs, pop<-ctxpops cx']] 
      mygroupby :: [(Population, Declaration)] -> Declarations -> Declarations
      mygroupby [] res = res
      mygroupby ((p,d):pds) res = mygroupby pds addxtores
        where 
        
        inres = length [()|d'<-res, signpos d d']==1
        addxtores = 
          if inres --ONLY d' is used for merge, while decfpos d==decfpos d'
          then [mrg d' p|d'<-res, signpos d d']++[d'|d'<-res, not(signpos d d')] 
          else (mrg d p):res        
      mrg d p = d{decpopu=decpopu d++popps p}
      signpos d d' = d==d' && decfpos d==decfpos d'
 
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
               where allpopd = [popd|popd<-rd popuRels, d==popd] 
  --DESCR -> Add population to concept
  populate :: Concept -> Concept
  populate c@(C{}) = --c{cptos=Nothing
                     c{cptos= Just (rd$[srcPaire p|d<-popuRels,p<-contents' d,elem (source d,c) isatree]
                                     ++[trgPaire p|d<-popuRels,p<-contents' d,elem (target d,c) isatree]
                                   )
                      ,cptgE=(\c1 c2 -> elem (c1,c2) isas)}
  populate c       = c

  ctxPatKeys :: [(KeyDef,[Either Expression (String,[Block],FilePos,OrigExpr)])]
  ctxPatKeys = [bindKeyDef kd | pat<-ctxpats cx, kd<-ptkds pat]   

  --DESCR -> The function bindRule attaches a type to a rule by producing a proof in the type system.
  --REMARK -> The rules are numbered after enriching, see renumber :: Context -> Context
  --          Thus a rule in (cxrls cx) originating from a rule in a pattern, and the original rule
  --          may have different numbers.
  --          infertype_and_populate :: (Morphism -> Morphism) -> Concepts -> Gens -> Declarations -> Expression -> Either (InfType, Expression) String    popuMphDecl

   --DESCR -> the expression must have the same structure as (normExpr rule)
  ruleexpr_inv :: Rule -> Expression -> Rule
  ruleexpr_inv rule x
   -- normExpr rule == x 
      = case x of 
        Fux [Cpx a, c] -> if rrsrt rule==Implication then rule{rrant=a,rrcon=c} else err
        Fix [Fux [a, Cpx c], Fux [Cpx _,_]] -> if --a==a' && c==c' && 
                                               rrsrt rule==Equivalence 
                                            then rule{rrant=a,rrcon=c} else err
        _ -> if rrsrt rule==Truth then rule{rrcon=x} else err
   -- otherwise = err
    where 
    err = error("!Fatal (module TypeChecker 345): The expression ("++show x++") is not normExpr of rule "++show rule)

  bindRule :: Rule -> Either Rule (String,[Block],FilePos,OrigExpr)
  bindRule r@(Ru{}) = 
     if null err 
     then Left$ (bindexpr){rrtyp=(populate c1,populate c2),rrtyp_proof=inftree, srrel=signaldecl}
     else Right (err,block,rrfps r,OrigRule r) 
     where
     inf_r = enrich_expr (normExpr r)
     bindexpr = case inf_r of
       Left (_,inf_expr,_) -> ruleexpr_inv r inf_expr
       _ -> r
     (c1,c2) = case inf_r of
       Left (inf_t,_,_) -> inf_t
       _ ->  (NOthing,NOthing)
     (err,block) = case inf_r of
       Right x -> x
       _ -> ("",[])
     inftree = case inf_r of
       Left (_,infexpr,x) -> Just (x,infexpr)
       _ -> Nothing
     signaldecl = (srrel r){desrc=c1, detrg=c2}


  --DESCR -> enriching ctxos
  --         bind the expression and nested object defs of all object defs in the context
  ctxobjdefs :: [(ObjectDef,[Either Expression (String,[Block],FilePos,OrigExpr)])]
  ctxobjdefs = [bindObjDef od Nothing | od<-objDefs cx]
  ctxsqlplugs :: [(ObjectDef,[Either Expression (String,[Block],FilePos,OrigExpr)])]
  ctxsqlplugs = [bindObjDef plug Nothing | plug<-ctxsql cx]
  ctxphpplugs :: [(ObjectDef,[Either Expression (String,[Block],FilePos,OrigExpr)])]
  ctxphpplugs = [bindObjDef plug Nothing | plug<-ctxphp cx]
  --add the upper expression to me and infer me and bind type
  --pass the new upper expression to the children and bindObjDef them
  bindObjDef ::  ObjectDef -> Maybe Expression -> (ObjectDef,[Either Expression (String,[Block],FilePos,OrigExpr)])
  bindObjDef od mbtopexpr = (od {objctx=bindexpr, objctx_proof=inftree, objats=bindats},checkedexpr:checkedexprs)
    where 
    expr = case mbtopexpr of
      Nothing -> objctx od
      Just topexpr -> F [topexpr, objctx od]
    checkedexpr = if null err 
                  then Left bindexpr
                  else Right (err,block,objpos od,OrigObjDef expr)      
    inf_e = enrich_expr expr
    (err,block) = case inf_e of
      Right x -> x
      _ -> ("",[])
    bindexpr =  case mbtopexpr of
      Nothing -> case inf_e of
           Left (_,x,_) -> x
           _ -> objctx od 
      Just _ -> case inf_e of
           Left (_,F [_,x],_) -> x
           Left _ -> error $ "!Fatal (module TypeChecker 441): function enrichCtx.bindObjDef: " ++
                             "Expected a composition expression."++show inf_e++"."
           _ -> objctx od 
    inftree = case inf_e of
           Left (_,infexpr,x) -> Just (x,infexpr)
           _ -> Nothing
    ---------------objats------------
    newtopexpr =  case mbtopexpr of
      Nothing -> bindexpr
      Just topexpr -> F [topexpr,bindexpr]
    inferats = [bindObjDef oa (Just newtopexpr) | oa<-objats od]
    bindats = [oa|(oa,_)<-inferats]
    checkedexprs = concat [x|(_,x)<-inferats]
  -------end bindObjDef---------------------------------------------------------------
  
  ctxCtxKeys :: [(KeyDef,[Either Expression (String,[Block],FilePos,OrigExpr)])]
  ctxCtxKeys = [bindKeyDef kd | kd<-ctxks cx]   
  bindKeyDef :: KeyDef -> (KeyDef,[Either Expression (String,[Block],FilePos,OrigExpr)])
  bindKeyDef kd = (kd {kdats=bindats},checkedkeydefexprs)
    where
    checkedkeydefexprs = [Left x|Left x<-checkedexprs] ++ [Right (x,block,fp,OrigKeyDef y)|Right (x,block,fp,OrigObjDef y)<-checkedexprs]
    (Obj {objats=bindats},checkedexprs) = bindObjDef 
                   (Obj {objats=kdats kd,
                         objnm=kdlbl kd,
                         objpos=kdpos kd,
                         objctx=Tm (I [kdcpt kd] (kdcpt kd) (kdcpt kd) True) (-1), --REMARK -> mIs is not used because it does not set mphats
                         objctx_proof=Nothing,
                         objstrs=[[]]}) Nothing

                                    

-----------------
--Check functions
-----------------
--DESCR -> check rule: Every context must have a unique name
--DESCR -> Eq Context  is defined such that contexts with equal names are considered equal.
checkCtxNameUniqueness :: Contexts -> Errors
checkCtxNameUniqueness [] = []
checkCtxNameUniqueness (cx:ctxs) 
     | elemBy (==) cx ctxs = [("Context name " ++ ctxnm cx ++ " is not unique",[])] ++ checkCtxNameUniqueness ctxs
     | otherwise           = checkCtxNameUniqueness ctxs

--USE -> Contexts names must be unique
checkCtxExtLoops :: Contexts -> Errors
checkCtxExtLoops ctxs = composeError (concat [findLoops cx | cx<- ctxs])
    where
    composeError :: [ContextName] -> Errors
    composeError [] = []
    composeError cxnms = [("One or more CONTEXT loops have been detected involving contexts: "
                           ++ foldr (++) "\n" [ cxnm++"\n" | cxnm<-cxnms],[])
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
          [("Service or plug name " ++ svcnm ++ " is not unique:"++ (concat ["\n"++show svcpos |svcpos<-svcposs]),[])
          |(svcnm,svcposs)<-svcnms] 
          ++
          [("Label " ++ lbnm ++ " in service or plug must be unique on sibling level:"
                    ++ (concat ["\n"++show lbpos |lbpos<-lblposs]),[])
          |lbl<-lblnms, (lbnm,lblposs)<-lbl]
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
checkPopulations ctxs = [(err,[])|Right err<-[popdeclaration [d| d<-declarations ctxs, decusr d] pop | cx<-ctxs, pop<-ctxpops cx]]


------------------
--Common functions
------------------

--DESCR -> If no declaration can be found than an error message is returned
popdeclaration :: [Declaration] -> Population ->  Either Declaration String
popdeclaration ds p = 
   if length allmatches==1 then Left(head allmatches)
   else if null allmatches
        then Right$"A relation is missing for population of " ++ show (popm p) ++ " at " ++ show (mphpos$popm p) 
                ++ "."
        else Right$"Population of " ++ show (popm p) ++ " at " ++ show (mphpos$popm p) 
                ++ "can be related to multiple relation declarations:\n" 
                ++ concat [show x ++ "\n"|x<-allmatches]
                ++ "Define the type of the population."
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


