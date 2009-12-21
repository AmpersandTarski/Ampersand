{-# OPTIONS_GHC -Wall #-}
module TypeInferenceEngine where
import Adl.Concept
--import Adl.MorphismAndDeclaration
import Data.Maybe
import TypeInference.ITree
import TypeInference.AdlExpr
--import CommonClasses 

--------------------------------------
import TypeInference.Input
import Collection (Collection(uni))
import Adl
import Data.Fspec
--DESCR -> Infer the type of an expression within the boundaries of an fspec
--         Sign (NOthing,NOthing) indicates disjunct concepts
--         In that case you can use show to get an error string
--         or you can get a TypeError from analyseerror (NoType) or analyseamb (AmbiguousType)
--         Use inferrule to infer the type of a rule
data ExprOrRule = Eeor Expression | Reor Rule
infertype :: Fspc -> ExprOrRule -> (Sign,Proof)
infertype fSpec eor = (inftype,proof)
  where
  proof = infer (gamma adlexpr) adlexpr
  inftype = case proof of
                 Proven gm (inftree:_) -> evalstmt $ evaltree gm inftree
                 _                     -> (NOthing,NOthing)
  adlexpr = case eor of 
       Eeor expr -> fromExpression expr 
       Reor rule -> fromRule rule
  tc :: Concepts
  tc = concs (vrels fSpec) `uni` concs (vrules fSpec)
  isatree = isaRels tc $ allPatGens (patterns fSpec)
  rv :: Declarations
  rv = vrels fSpec
  gamma expr = (mphStmts expr) ++ gammaisa
  gammaisa = map fromIsa isatree
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
--------------------------------------
type BndStmt = Statement
type DeclStmt = Statement
type Alternatives = [(DeclStmt,Maybe ITree)]
type CptVar = Concept
type BndCptVar = (CptVar,Concept)

--DESCR -> return a type inference tree(s) for an expression given a gamma
infer :: Gamma -> AdlExpr -> Proof
infer gamma exr  = step4combinetrees step3inferstmts step2tree
  where
  --DESCR -> Checks if gamma contains an isa statement stating that c1 is-a c2
  isa' c1 c2 = elem (fromIsa (c1, c2)) gamma
  step1tree = tree $ unboundtree freecptvars
  --DESCR -> Get all used concept variables and leave them unbound by relating them to themselves
  step1cptvars = [(var,var) | var<-(takeWhile ((/=)(head $ free $ unboundtree freecptvars)) freecptvars)]
  (step2tree,step2cptvars,step2error) = foldr bindMphats (step1tree,step1cptvars,Nothing) (stmts step1tree)
  step3inferstmts :: [( [(Statement, Alternatives)] , [BndCptVar])]
  step3inferstmts = case step2error of
     Nothing -> inferstmts ( bind2declAlts, step2cptvars)
     justerr -> [([(EmptyStmt, [(EmptyStmt, justerr)] )],step2cptvars)] --one option, with no bound statements, only an error
     where
     --DESCR -> bind statements with concept vars to bound alternatives
     bind2declAlts :: [(BndStmt,Alternatives)]
     bind2declAlts = [(stmt,declAlts stmt)| stmt<-stmts step2tree]
        where
        --DESCR -> bound alternatives of statement with concept vars without inference tree
        declAlts :: BndStmt -> Alternatives
        declAlts (BoundTo expr) =
           let
           relvar = case expr of 
             Relation{} -> expr 
             Complement{sub=Relation{}} -> sub expr
             _ -> error $ "!Fatal (module TypeInferenceEngine 97): Function declAlts does not expect other expressions than Complement on or just Relation: "++show expr++"."
           undeclerr = [(EmptyStmt, Just $ Stmt stmt') | stmt'@(InfErr (UndeclRel relvar'))<-gamma, relvar==relvar']
           in
           if null undeclerr
           then [(stmt',Nothing) | stmt'@(DeclExpr relvar' _)<-gamma, relvar==relvar']
           else undeclerr
        declAlts stmt = error
          $ "!Fatal (module TypeInferenceEngine 104): " ++
            "function declAlts only expects BoundTo statements. "++show stmt++"."
  --DESCR -> Given the main inference tree and all alternative combinations, proof the type of an expression or proof a type error
  step4combinetrees :: [( [(BndStmt, Alternatives)] , [BndCptVar])] -> ITree -> Proof
  step4combinetrees alts basetree =
    let    
    noerrbindings = [ vars | (_,vars)<-noerralts ]
    noerralts = [(stms,vars) 
                        | (stms,vars)<-alts
                                    --(bnd,Alts)          (::,tree)                               
                        ,null [err |  (_,tstmserr)<-stms, (_,Just (Stmt err@(InfErr _)) )<-tstmserr]
                           --  only one ::-stmt
                        , null [()|(_,trs)<-stms, length trs/=1]
                ] 
    --DESCR -> all possible unambiguous gammas (alternatives) resulting in an error
    analysedata =  [( [InfErr x|InfErr x<-stmts (attachtrees alt basetree) 
                                          ++[err|(EmptyStmt, [(EmptyStmt, Just (Stmt err))])<-bstmts]
                      ]
                    , attachtrees ([],vars) basetree
                    ) | alt@(bstmts,vars)<-alts ] --concat
                      -- [ --All alternatives resulting in an tree consisting of just an errorrule
                        --ax1: expression |- basetree
                        --ax2: unambiguous gamma |- basetree -> error
                        --concl: expression, unambiguous gamma |- basetree -> error
                      --  [Stmt $ InfErr $
                        --      TypeError{gam=gamma
                          --             ,btree=basetree
                            --           ,errstmt=err
                              --         ,declexprs=[fst (head tstms) | (_,tstms)<-stms, not (null tstms)]}
                --         |  (_,tstmserr)<-stms, (_,Just (Stmt err@(InfErr _)) )<-tstmserr, length tstmserr==1]
                  --    | (stms,_)<-alts]
    --DESCR -> all possible unambiguous gammas (alternatives) resulting in a type
    allnoerrinftrees = [attachtrees alt basetree | alt<-noerralts ] 
    in
    if null noerralts --there are no alternatives inferring a type
    then NoProof (NoType gamma analysedata)  --return all alternatives
    else if eqbindings noerrbindings --ax: all alts resulting in a type bind all the concept variables to the same concepts
                                     --concl: expression, gamma |- typedexpression
         then Proven gamma allnoerrinftrees  --return inference trees of all alts resulting in a type
         else NoProof (AmbiguousType gamma allnoerrinftrees)  --return inference trees of all alts resulting in a type
------------------------------------------------------------------------------------
  freecptvars :: [Concept]
  freecptvars = [cptnew $ "$C" ++ show i | i<-allPositiveIntegers]
     where allPositiveIntegers :: [Integer]
           allPositiveIntegers = [1..]
  tree (t, _) = t
  free (_, f) = f
  --DESCR -> construct a base inference tree of BoundTo statements by decomposing the expr to infer, and put concept variables at all concept locations. Return all unused concept variable names too.
  --USE -> All concept variables need to be bound to a concept. And all leaves (Stmt BoundTo expr@(Relation{})) need to be inferred.
  unboundtree :: Concepts -> (ITree, Concepts)
  unboundtree [] = error $ "!Fatal (module TypeInferenceEngine 154): unboundtree expects " ++
                           "an infinite list of free concept variables."
  unboundtree (_:[]) = error $ "!Fatal (module TypeInferenceEngine 156): unboundtree expects " ++
                           "an infinite list of free concept variables."
  --DESCR -> decomposing step1: every expression to infer is bound to two fresh cpt vars. 
  unboundtree (c1:c2:fcpts) = bindsubexprs (bindto exr (CT c1) (CT c2) (expo exr 1)) fcpts
    where 
    expo:: AdlExpr -> Int -> Int
    expo x i = if isCompl x then expo (sub x) (-i) else i
    notexp1err subex = error $ "!Fatal (module TypeInferenceEngine 163): " ++
                         "Rule pattern match failed while decomposing expression: " ++show exr++".\n" ++show subex++ "."   
    bindto expr = \src tgt expt -> BoundTo expr{tt=TT src tgt expt}
    bindsubexprlist [] vars = ([],vars)
    bindsubexprlist (bt:bts) vars = (bt':bts',vars'')
        where
        (bt', vars') = bindsubexprs bt vars
        (bts', vars'') = bindsubexprlist bts vars'
    --DESCR -> decomposing step2: The BoundTo statement has been bound to cptvars. This statement will be proven by a matching rule resulting from the implemented algoritm below. The algoritm will only end if the statement binds a morphism or a complement on a morphism, because you cannot declare complex expressions in an ADL file and therefore a BindRule on a complex expression can never be inferred from an ADL file. For that reason if the statement binds a complex expression the algoritm ignores the match on the Bind rule. There will be exacly one other rule left which matches the statement on a complex expression.  Thus, the algoritm always terminates because all complex expressions will be decomposed by the only matching rule, eventually resulting in a set of BoundTo statements on morphism expressions and complement morphism expressions.
    bindsubexprs (BoundTo expr) = \(cb1:cb2:cbs:fcs) -> case expr of
          Intersect{tt=TT src tgt expt} -> if expt==1
            then (DisjRule{axs1=tree trees1, axs2=tree trees2}, free trees2)
            else notexp1err expr
              where
              noinvs = [bindto sb src tgt 1 |sb<-lst expr, (expo sb 1)==1]
              invs = [bindto sb src tgt (-1) |sb<-lst expr, (expo sb 1)==(-1)]
              trees1 = bindsubexprlist noinvs (cb1:cb2:cbs:fcs)
              trees2 = bindsubexprlist invs (free trees1)
          Union{tt=TT src tgt expt} ->  if expt==1 
            then (UnionRule{axs1=tree trees1, axs2=tree trees2}, free trees2) 
            else notexp1err expr
              where
              noinvs = [bindto sb src tgt 1 |sb<-lst expr, (expo sb 1)==1]
              invs = [bindto sb src tgt (-1) |sb<-lst expr, (expo sb 1)==(-1)]
              trees1 = bindsubexprlist noinvs (cb1:cb2:cbs:fcs)
              trees2 = bindsubexprlist invs (free trees1) 
          Implicate{tt=TT src tgt expt} -> if expt==1 then (ImplyRule (tree tree1), free tree1) else notexp1err expr
              where
              defimpl = Union{lst=[Complement{sub=(left expr),tt=unknowntype},right expr],tt=unknowntype}
              tree1 = bindsubexprs (bindto defimpl src tgt expt) (cb1:cb2:cbs:fcs)
          Equality{tt=TT src tgt expt} -> if expt==1 then (EqualRule (tree tree1), free tree1) else notexp1err expr
              where
              defeq = Intersect {lst=[Implicate{left=left expr,right=right expr,tt=unknowntype},
                                      Implicate{left=right expr,right=left expr,tt=unknowntype}
                                     ], tt=unknowntype}
              tree1 = bindsubexprs (bindto defeq src tgt expt) (cb1:cb2:cbs:fcs)
          Semicolon{tt=TT src tgt expt} ->  if expt==1 then (RelcompRule matchrule (tree tree1) (tree tree2), free tree2) else notexp1err expr             where
             matchrule = 
                if isCompl (left expr) && isCompl (right expr)
                then  CompDInv
                else if isCompl (left expr) then CompInv1
                     else if isCompl (right expr) then CompInv2
                          else Comp
             tree1 = bindsubexprs (bindto (left expr) src (CT cb1) (expo (left expr) 1)) (cb2:cbs:fcs)
             tree2 = bindsubexprs (bindto (right expr) (CT cb1) tgt (expo (right expr) 1)) (free tree1)
          Dagger{tt=TT src tgt expt} ->  if expt==1 then (AddcompRule matchrule (tree tree1) (tree tree2), free tree2) else notexp1err expr
             where
             matchrule = 
                if isCompl (left expr) && isCompl (right expr)
                then  CompDInv
                else if isCompl (left expr) then CompInv1
                     else if isCompl (right expr) then CompInv2
                          else Comp
             tree1 = bindsubexprs (bindto (left expr) src (CT cb1) (expo (left expr) 1)) (cb2:cbs:fcs)
             tree2 = bindsubexprs (bindto (right expr) (CT cb1) tgt (expo (right expr) 1)) (free tree1)
          Complement{tt=TT src tgt expt} -> matchrule
              where
              matchrule = case (sub expr) of
                Complement{} -> let tree1 = bindsubexprs (bindto (sub (sub expr)) src tgt expt) (cb1:cb2:cbs:fcs)
                                in (DComplRule (tree tree1), free tree1)
                Dagger{} -> if expt==(-1) then (DeMorganRule FAddcomp (tree tree1), free tree1) else notexp1err expr
                   where
                   tree1 = bindsubexprs (bindto dmexpr src tgt (-expt)) (cb1:cb2:cbs:fcs)
                   dmexpr = Semicolon{left=Complement{sub=left (sub expr),tt=unknowntype},
                                      right=Complement{sub=right (sub expr),tt=unknowntype},
                                      tt=unknowntype }
                Semicolon{} ->if expt==(-1) then (DeMorganRule FRelcomp (tree tree1), free tree1) else notexp1err expr
                   where
                   tree1 = bindsubexprs (bindto dmexpr src tgt (-expt)) (cb1:cb2:cbs:fcs)
                   dmexpr = Dagger{left=Complement{sub=left (sub expr),tt=unknowntype},
                                   right=Complement{sub=right (sub expr),tt=unknowntype},
                                   tt=unknowntype }
                Intersect{} -> if expt==(-1) then (DeMorganRule FDisj (tree tree1), free tree1) else notexp1err expr
                   where
                   tree1 = bindsubexprs (bindto dmexpr src tgt (-expt)) (cb1:cb2:cbs:fcs)
                   dmexpr = Union{lst=map compl $ lst (sub expr), tt=unknowntype }
                   compl ex = Complement{sub=ex,tt=unknowntype}
                Union{} -> if expt==(-1) then (DeMorganRule FUnion (tree tree1), free tree1) else notexp1err expr
                   where
                   tree1 = bindsubexprs (bindto dmexpr src tgt (-expt)) (cb1:cb2:cbs:fcs)
                   dmexpr = Intersect{lst=map compl $ lst (sub expr), tt=unknowntype }
                   compl ex = Complement{sub=ex,tt=unknowntype}
                Flip{sub=flipsub} -> (FlipRule Inv (tree tree1), free tree1)  
                   where 
                   tree1 = bindsubexprs (bindto Complement{sub=flipsub,tt=unknowntype} tgt src expt) (cb1:cb2:cbs:fcs)   
                Relation{} ->  if expt==(-1) 
                               --DESCR -> bind both the complement and sub expression of the complement
                               then (Stmt $ bindto (expr{sub=(sub expr){tt=TT src tgt (-expt)}}) src tgt (expt)
                                    , (cb1:cb2:cbs:fcs))
                               else notexp1err expr                                                   
                _ -> error $ "!Fatal (module TypeInferenceEngine 253): bindsubexprs: complement not expected on rule expression: " ++show expr ++"."                  
                 
          Flip{tt=TT src tgt expt} -> (FlipRule NoInv (tree tree1), free tree1)
              where
              tree1 = bindsubexprs (bindto (sub expr) tgt src expt ) (cb1:cb2:cbs:fcs)
          Relation{tt=TT src tgt expt}     -> (Stmt $ bindto expr src tgt expt, (cb1:cb2:cbs:fcs))
    bindsubexprs stmt = \_ -> error $ "!Fatal (module TypeInferenceEngine 259): bindsubexprs: Only BoundTo expression are expected: "++show (stmt)++"." 
  ------------------------------------------------------------------------------------
  --DESCR -> Given a Statement from an unboundtree, this tree, and the variable environment: bind the mphats of a Relation expression to certain concept variables in this expression.
  bindMphats :: Statement -> (ITree, [BndCptVar], Maybe ITree) -> (ITree, [BndCptVar], Maybe ITree)
  bindMphats (BoundTo r@(Relation{rel=mp, tt=TT{cts=ct1,ctt=ct2}}) ) (itree,vars, Nothing) =
     let
     (c1,c2,hasmphats) = case mp of
         Mph{mphats=[x,y], mphyin=True} -> (x,y,True)
         Mph{mphats=[x,y], mphyin=False} -> (y,x,True)
         I{mphats=[x]} -> (x,x,True)
         V{mphats=[x,y]} -> (x,y,True)
         _ -> let nomphatserr = error $ show $ "!Fatal (module TypeInferenceEngine 270): bindMphats: Do not bind mphats because there aren't any."
              in (nomphatserr,nomphatserr,False)
     --get the concept variable from the BoundTo statement to bind the first mphat to 
     var1 = val ct1
     --get the concept variable from the BoundTo statement to bind the second mphat to 
     var2 = val ct2 
     --get the binding of var1 from the variable environment
     cpt' = if iscptvar var1 then lookupvar vars var1 else notvarerr var1
     cpt'' = if var1==var2 --check if vars' must be used instead of vars
             then case mvars' of 
                    Nothing -> if iscptvar var2 then lookupvar vars var2 else notvarerr var2 
                    Just vars' -> if iscptvar var2 then lookupvar vars' var2 else notvarerr var2
             else if iscptvar var2 then lookupvar vars var2 else notvarerr var2
     mvars'  =
        if var1==cpt' || c1 `isa'` cpt' -- is unbound or more specific
        then Just $ bindvar vars var1 c1 --bind more specific
        else if cpt' `isa'` c1 --already bound to the same or more specific concept by other mphats
             then Just vars --variable is already bound to this mphat
             else Nothing
     --try to bind var2 to the second mphat
     mvars'' = case mvars' of 
        Nothing -> Nothing 
        Just vars' -> if var2==cpt'' || c2 `isa'` cpt'' -- is unbound or more specific
                      then Just $ bindvar vars' var2 c2 --bind more specific
                      else 
                        if cpt'' `isa'` c2 --already bound to the same or more specific concept by other mphats
                        then Just vars' --variable is already bound to this mphat
                        else Nothing
     notvarerr v' = error $ "!Fatal (module TypeInferenceEngine 298): bindMphats: " ++
                              "Concept variable "++show v'++" is not a concept variable."
     in
     if hasmphats
     then case mvars' of 
       Nothing -> (itree, vars, Just $ Stmt $ InfErr $ IErr DisjSrc c1 cpt' r)
       _ -> case mvars'' of
              Nothing -> (itree, vars, Just $ Stmt $ InfErr $ IErr DisjTrg c2 cpt'' r)
              Just vars'' -> (itree, vars'',Nothing)
     else (itree,vars, Nothing)
  bindMphats (BoundTo compl@(Complement{})) (itree,vars, Nothing) = bindMphats (BoundTo (sub compl)) (itree,vars, Nothing)
  bindMphats _ itree = itree

  ------------------------------------------------------------------------------------
  --DESCR -> infer all alternatives of all statements and bind concept vars in the var env along the way
  --EXTEND -> if the variable environment is changed then all previously inferred alts must be inferred again by deleting their inference trees!!!
  inferstmts :: ([(BndStmt,Alternatives)],[BndCptVar]) ->  [( [(BndStmt, Alternatives)] , [BndCptVar])]
  inferstmts (stms,vars) = if null toinfer || errsfnd --DONE!
                            then [(stms,vars)] 
                            --else: infer the next statement, and infer the other statements toinfer too
                            else concat [inferstmts alt |alt<-(inferalts $ head toinfer)]
     where
     --DESCR -> if a stmt has only one alternative which is an error, then inference has failed
     errsfnd =  not $ null [err | (_,alts)<-stms, (_,err@(Just (Stmt (InfErr _))))<-alts, length alts==1]
     --DESCR -> Get the next BoundTo statement and its alternatives that needs to be inferred
     toinfer = [(stmt,alts) | (stmt,alts)<-stms, uninferred alts]
     uninferred alts = not (null [alt | alt@(_,Nothing)<-alts])
     --DESCR -> infer all trees for all alternatives of this BoundTo statement
     --         return the inference state of all statements and the variable environment after inferring this statement
     inferalts :: (BndStmt,Alternatives) -> [( [(BndStmt, Alternatives)] , [BndCptVar])]
     inferalts (currstmt,curralts) = 
        [(
            (altafter $ inferalt currstmt alt) --infer this alt of this statement
            : (if (varenvchanged $ inferalt currstmt alt) --if the variable environment has been updated
               then resetotherstmts currstmt --reset all except the currently inferred
               else copyotherstmts currstmt) --copy all except the currently inferred
         , 
            varsafter $ inferalt currstmt alt --the var env state after inference of this statement
         )
        | (alt,_)<-curralts ]
     --DESCR -> returns all the stmts accept the currently inferred one
     copyotherstmts currstmt = [(otherstmt,otheralts) | (otherstmt,otheralts)<-stms, otherstmt/=currstmt ]
     --DESCR -> set all inference trees of alts of other stmts to Nothing
     resetotherstmts :: Statement -> [(Statement,Alternatives)] 
     resetotherstmts currstmt = [(otherstmt,[(alt,if alt==EmptyStmt then tr else Nothing) |(alt,tr)<-otheralts]) | (otherstmt,otheralts)<-copyotherstmts currstmt]
     --DESCR -> labelling functions to the triple result of inferalt
     altafter (x,_,_) = x
     varsafter (_,x,_) = x
     varenvchanged (_,_,x) = x
     --DESCR -> infer the BoundTo statement given the declared TypeOf statement. 
     --         The inference tree is bound to the alternative
     --         If the variable environment has been updated the Bool will be True.
     --         The algoritm will always end because the var env is only updated more specific
     --         thus at some point the specific will match (do not initiate loop) or be disjunct (error)
     inferalt :: BndStmt -> DeclStmt -> ((BndStmt, Alternatives),[BndCptVar], Bool)
     inferalt stmt@(BoundTo expr) alt =
       let
       ct1 = cts $ tt expr
       ct2 = ctt $ tt expr
       notvarerr v' = error $ "!Fatal (module TypeInferenceEngine 357): inferalt: " ++
                              "Concept variable "++show v'++" is not a concept variable."
       in
       case alt of
        DeclExpr{declex=declexpr,homo=hm} -> trydomain 
          where
          --TT (CF (Generic,_,c1')) (CF (Generic,_,c2')) = tt declexpr
          c1' = declaredcpt $ cts (tt declexpr)
          c2' = declaredcpt $ ctt (tt declexpr)
          returnerror dtp c1 c2 vs = ((stmt, [(alt, Just $ Stmt $ InfErr $ IErr dtp c1 c2 expr)]),vs, False)
          trydomain = 
             let 
             --lookup concept var if it is actually a var
             c1 = if iscptvar var1 then lookupvar vars var1 else notvarerr var1
             var1 = val ct1
             in 
             if iscptvar c1 --var1 is not bound yet
             then tryrange (Stmt alt) (var1,bindvar vars var1 c1', True) --bind variable var1 to c1'
             else --var1 is already bound
               if c1==c1' --alternative matches conclusion
               then tryrange (Stmt alt) (var1,vars, False)
               else 
                 if c1 `isa'` c1' --try specialization rule
                 then tryrange (SpecRule SpecDomain (Stmt $ fromIsa (c1, c1')) (Stmt alt)) (var1,vars, False)
                 else 
                   if c1' `isa'` c1
                   then tryrange (Stmt alt) (var1,bindvar vars var1 c1', True) --rebind the var
                   else returnerror DisjSrc c1 c1' vars
               
          tryrange ruledomain (var1, varsdomain, envchanged) = 
             let 
             --lookup concept var if it is actually a var
             c2 = if iscptvar var2 then lookupvar varsdomain var2 else notvarerr var2
             var2 = val ct2
             specto c = Just $ SpecRule SpecRange (Stmt $ fromIsa (c, c2')) ruledomain
             in
             if iscptvar c2
             --var2 is not bound yet, thus bind
             then checkhomo (Just ruledomain) ((var1,var2), bindvar varsdomain var2 c2', True)
             else --c2 is already bound
               if c2==c2' --alternative matches conclusion, thus pass what's calculated at trydomain
               then checkhomo (Just ruledomain) ((var1,var2),varsdomain, envchanged)
               else
                 if c2 `isa'` c2' --try specialization rule
                 then checkhomo (specto c2) ((var1,var2),varsdomain, envchanged)  
                 else 
                   if c2' `isa'` c2
                   then if (iscptvar var1 && var1==var2) --if the source is the same variable as the target
                        --rebind the var and schedule this alt to be inferred again by setting the tree to Nothing
                        then checkhomo Nothing ((var1,var2),bindvar varsdomain var2 c2', True) 
                        else checkhomo (Just ruledomain) ((var1,var2),bindvar varsdomain var2 c2', True) --just rebind
                   else returnerror DisjTrg c2 c2' varsdomain
          checkhomo mbrulerange ((var1,var2), varsrange, envchanged) =
             let 
             --lookup concept vars if it is actually a var
             c1 = if iscptvar var1 then lookupvar varsrange var1 else notvarerr var1
             c2 = if iscptvar var2 then lookupvar varsrange var2 else notvarerr var2
             rebindboth c =((stmt, [(alt, Nothing)]),bindvar (bindvar varsrange var1 c) var2 c, True)
             reinferanyway = case mbrulerange of Nothing -> True; _ -> False
             in
             if (not hm) || c1==c2 || reinferanyway --if not a homo, or source==target, or to be reinferred anyway  
             then ((stmt, [(alt, mbrulerange)]),varsrange, envchanged) --pass what's calculated at tryrange
             else -- 
               if c1 `isa'` c2 then rebindboth c1
               else if c2 `isa'` c1 then rebindboth c2
                 else returnerror DisjHomo c1 c2 varsrange
        _ -> error $ "!Fatal (module TypeInferenceEngine 423): inferalt: " ++
                     "The alternative statement "++show alt++" is not a TypeTo statement."
     inferalt _ _  = error
        $ "!Fatal (module TypeInferenceEngine 426): inferalt: " ++
          "This function only binds concept variables of BoundTo statements."
     ------------------------------------------
     -- endfunction -> inferalt stmt alt      --
   ------------------------------------------
   -- endfunction -> inferstmts (stmts,vars) --
-------------------------------------------
-- ENDFUNCTION -> infer gamma exr        --
---------------------------------

--DESCR -> returns the Concept to which a variable is bound in the variable environment
--         A variable is unbound if it is bound to itself
lookupvar ::  [BndCptVar] -> CptVar -> Concept
lookupvar vars lvar = if not (null bndvar) then head bndvar else varnotfnderr
       where
       bndvar = [cpt |(var,cpt)<-vars, var==lvar]
       varnotfnderr = error $ "!Fatal (module TypeInferenceEngine 442): lookupvar: " ++
                              "Concept variable could not be found in the variable environment: "++show lvar++"."

--DESCR -> Returns True if the Concept is a variable
iscptvar :: Concept -> Bool
iscptvar c = if (null $ name c) then False else (head $ name c) == '$'

--DESCR -> Update the binding of a concept variable in the variable environment
bindvar :: [BndCptVar] -> CptVar -> Concept -> [BndCptVar]
bindvar [] _ _ = []
bindvar ((var,cpt):vars) var' cpt' | var==var' = (var,cpt'):vars
                                   | otherwise = (var,cpt):(bindvar vars var' cpt')

--DESCR -> Bind the free concept variables in the tree to their bindings in the variable environment
rebindtree :: ITree -> [BndCptVar] -> ITree
rebindtree (DisjRule ts1 ts2) vars = DisjRule [rebindtree tr vars|tr<-ts1] [rebindtree tr vars|tr<-ts2]
rebindtree (RelcompRule ct tree1 tree2) vars = RelcompRule ct (rebindtree tree1 vars) (rebindtree tree2 vars)
rebindtree (BindRule bt tree) vars = BindRule bt (rebindtree tree vars)
rebindtree (SpecRule st tree1 tree2 ) vars = SpecRule st (rebindtree tree1 vars) (rebindtree tree2 vars)
rebindtree (AddcompRule ct tree1 tree2 ) vars = AddcompRule ct (rebindtree tree1 vars) (rebindtree tree2 vars)
rebindtree (UnionRule ts1 ts2 ) vars = UnionRule [rebindtree tr vars|tr<-ts1] [rebindtree tr vars|tr<-ts2]
rebindtree (ImplyRule tree1) vars = ImplyRule (rebindtree tree1 vars)
rebindtree (EqualRule tree1) vars = EqualRule (rebindtree tree1 vars)
rebindtree (DComplRule tree1) vars = DComplRule (rebindtree tree1 vars)
rebindtree (FlipRule it tree1) vars = FlipRule it (rebindtree tree1 vars)
rebindtree (DeMorganRule dmt tree1) vars = DeMorganRule dmt (rebindtree tree1 vars)
rebindtree (Stmt stmt) vars = Stmt (rebindstmt stmt)
  where
  rebindstmt :: BndStmt -> BndStmt
  rebindstmt (BoundTo expr) = BoundTo $ rebindexpr expr
  rebindstmt (InfErr (UndeclRel expr)) = InfErr $ UndeclRel $ rebindexpr expr
  rebindstmt (InfErr (IErr dtp c1 c2 expr)) = InfErr $ IErr dtp c1 c2 $ rebindexpr expr
  rebindstmt st = st
  rebindexpr :: AdlExpr -> AdlExpr
  rebindexpr expr@(Relation{}) = expr{tt=rebindtt (tt expr)}
  rebindexpr expr@(Complement{}) = expr{sub=rebindexpr (sub expr) }
  rebindexpr expr = error $ "!Fatal (module TypeInferenceEngine 478): rebindexpr: " ++
                            "Other expressions than Complement on or just Relation are not expected: "++show expr++"."
  rebindtt :: TypeTerm -> TypeTerm
  rebindtt tt1 = tt1{cts=rebindct $ cts tt1,ctt=rebindct $ ctt tt1}
  rebindct :: ConceptTerm -> ConceptTerm
  rebindct ct1 = ct1{val=rebindcpt $ val ct1}
  rebindcpt :: Concept -> Concept
  rebindcpt c = if iscptvar c then lookupvar vars c else c 

--DESCR -> returns true if all variable environments in the list contain the same variables bound to the same values
--         results in an error if the environments contain a different number of variables
eqbindings :: [[BndCptVar]] -> Bool
eqbindings [] = True
eqbindings (_:[]) = True
eqbindings (vars:vars':varss) =
   if length vars == length vars'
   then foldr (&&) (eqbindings (vars':varss)) [elem var vars' | var<-vars]
   else error $ "!Fatal (module TypeInferenceEngine 495): eqbindings: " ++
                "Lengths of concept variable lists differ:" ++ show (length vars) ++ " and " ++ show (length vars) ++ "."

--DESCR -> Attach all inference trees (which are proofs for the BoundTo statements in the main inference tree) using a Bind Rule
--USE -> Each BoundTo must be bound to exactly one alternative with an inference tree
attachtrees :: ( [(BndStmt, Alternatives)] , [BndCptVar]) -> ITree -> ITree
attachtrees (stms,vars) tree = rebindtree (foldr attachstmt tree stms) vars

--DESCR -> Attach the inference tree (which is the proof of a BoundTo statement in the main inference tree) using a Bind rule
--USE -> A BoundTo must be bound to exactly one alternative with an inference tree
attachstmt :: (BndStmt, Alternatives) -> ITree -> ITree
attachstmt bndstmt@(stmt,_) baseleaf@(Stmt stmt') 
   | stmt==stmt' = BindRule (matchbindtype stmt) $ treestmt bndstmt
   | otherwise   = baseleaf --DESCR -> not the stmt looking for, skip
attachstmt bndstmt (DisjRule ls1 ls2) = DisjRule (map (attachstmt bndstmt) ls1) (map (attachstmt bndstmt) ls2)
attachstmt bndstmt (RelcompRule ct baseleaf1 baseleaf2) = RelcompRule ct (attachstmt bndstmt baseleaf1) (attachstmt bndstmt baseleaf2)
attachstmt bndstmt (AddcompRule ct baseleaf1 baseleaf2 ) = AddcompRule ct (attachstmt bndstmt baseleaf1) (attachstmt bndstmt baseleaf2)
attachstmt bndstmt (UnionRule ls1 ls2 ) = UnionRule (map (attachstmt bndstmt) ls1) (map (attachstmt bndstmt) ls2)
attachstmt bndstmt (ImplyRule baseleaf1 ) = ImplyRule (attachstmt bndstmt baseleaf1) 
attachstmt bndstmt (EqualRule baseleaf1 ) = EqualRule (attachstmt bndstmt baseleaf1) 
attachstmt bndstmt (DComplRule baseleaf) = DComplRule (attachstmt bndstmt baseleaf)
attachstmt bndstmt (FlipRule it baseleaf) = FlipRule it (attachstmt bndstmt baseleaf)
attachstmt bndstmt (DeMorganRule dmt baseleaf) = DeMorganRule dmt (attachstmt bndstmt baseleaf)
attachstmt _ branch = branch --DESCR -> other rules are not a result of any BoundTo statement, so skip whole branch

--DESCR -> Returns the matching Bind rule given the conclusion of this rule
matchbindtype :: BndStmt -> BindType
matchbindtype (BoundTo expr) = case expr of 
   Relation{} -> if expon (tt expr)==1 
         then Bind 
         else error $ "!Fatal (module TypeInferenceEngine 525): matchbindtype: " ++
                     "The exponent of a morphism expression must be 1: "++show expr++"."
   Complement{sub=Relation{}} -> if expon (tt expr)==(-1) 
         then BindCompl 
         else error $ "!Fatal (module TypeInferenceEngine 529): matchbindtype: " ++
                      "The exponent of a complement morphism expression must be -1: "++show expr++"."
   _ ->  error $ "!Fatal (module TypeInferenceEngine 531): matchbindtype: " ++
                 "The expression is complex: "++show expr++"."
matchbindtype stmt = error $ "!Fatal (module TypeInferenceEngine 533): matchbindtype: " ++
                     "The statement "++show stmt++" is not a BoundTo."

--DESCR -> Returns the inference tree which is the proof of a BoundTo statement based on statements derived from ADL declarations
--USE -> A BoundTo must be bound to exactly one alternative with an inference tree.
--       In some other cases it is assumed that an error has been found, and inference has been aborted.
treestmt :: (BndStmt, Alternatives) -> ITree
--DESCR -> error because there should always be alternatives, or a list of declared relations or an UndeclErr.
treestmt (stmt@(BoundTo{}),[]) = error $ "!Fatal (module TypeInferenceEngine 541): treestmt: " ++
                                              "The statement "++show stmt++" is not bound to any alternative."
treestmt ((BoundTo{}),[(_, Just inftree )]) = inftree
--DESCR -> an alternative can be without inference tree if inference is aborted because of a type error
treestmt ((BoundTo{}),[( _ , Nothing)]) = Stmt EmptyStmt
--DESCR -> there can be more than one alternative if inference is aborted because of a type error
treestmt ((BoundTo{}),_) = Stmt EmptyStmt
treestmt (stmt,_) = error $ "!Fatal (module TypeInferenceEngine 548): treestmt: " ++
                            "The statement "++show stmt++" is not a BoundTo."
