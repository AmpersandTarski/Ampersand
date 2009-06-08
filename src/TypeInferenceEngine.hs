{-# OPTIONS_GHC -Wall #-}
--TODO -> Detect ambiguity by checking if all possible types infer the same type. If not, then ambiguous?
--        The type derivation is the set of inference tree, one for each option.
module TypeInferenceEngine where
import Adl.Concept
import Adl.MorphismAndDeclaration
import Data.Maybe
import TypeInference.ITree
import TypeInference.Statements
import TypeInference.AdlExpr
import CommonClasses 

type BndStmt = Statement
type DeclStmt = Statement
type Alternatives = [(DeclStmt,Maybe ITree)]
type CptVar = Concept
type BndCptVar = (CptVar,Concept)

--DESCR -> return a type inference tree(s) for an expression given a gamma
infer :: Gamma -> AdlExpr -> Proof
infer gamma exr  = step4combinetrees step3inferstmts step2tree
  where
  step1tree = tree $ unboundtree freecptvars
  --DESCR -> Get all used concept variables and leave them unbound by relating them to themselves
  step1cptvars = [(var,var) | var<-(takeWhile ((/=)(head $ free $ unboundtree freecptvars)) freecptvars)]
  (step2tree,step2cptvars) = foldr bindMphats (step1tree,step1cptvars) (stmts step1tree)
  step3inferstmts :: [( [(Statement, Alternatives)] , [BndCptVar])]
  step3inferstmts = inferstmts ( bind2declAlts, step2cptvars)
     where
     --DESCR -> bind statements with concept vars to bound alternatives
     bind2declAlts :: [(BndStmt,Alternatives)]
     bind2declAlts = [(stmt,declAlts stmt)| stmt<-stmts step2tree]
        where
        --DESCR -> bound alternatives of statement with concept vars without inference tree
        declAlts :: BndStmt -> Alternatives
        declAlts (BoundTo relvar) =
           let
           undeclerr = [(EmptyStmt, Just $ Stmt stmt') | stmt'@(InfErr (UndeclRel relvar'))<-gamma, relvar==relvar']
           in
           if null undeclerr
           then -- case relvar of
              --  (Relation (Mph{mphats=[explc1,explc2]}) _ _) -> [(BndStat relvar (explc1,explc2),Nothing)]
               -- _ ->
                [(stmt',Nothing) | stmt'@(TypeOf relvar')<-gamma, relvar==relvar']
           else undeclerr
        declAlts stmt = error
          $ "Error in TypeInferenceTree.hs module InferenceRules function infer.step3inferstmts.declAlts: " ++
            "This function only expects BoundTo statements. "++show stmt++"."
  --DESCR -> Given the main inference tree and all alternative combinations, proof the type of an expression or proof a type error
  step4combinetrees :: [( [(BndStmt, Alternatives)] , [BndCptVar])] -> ITree -> Proof
  step4combinetrees alts basetree =
    let
    noerrbindings = [ vars | (_,vars)<-noerralts ]
    noerralts = [(stms,vars) | ((stms,vars),errstmts)<-errstmtsOfAlts, null errstmts ]
    errstmtsOfAlts = [((stms,vars), [treestmt stmt| stmt<-stms,iserrtree $ treestmt stmt]) | (stms,vars)<-alts]
    iserrtree (Stmt stmt) = iserrstmt stmt
    iserrtree _ = False
    allinftrees = [attachtrees alt basetree | alt<-alts] 
    allnoerrinftrees = [attachtrees alt basetree | alt<-noerralts ] 
    in
    if null noerralts --there are no alternatives inferring a type
    then NoProof NoType allinftrees  --return inference trees of all alternatives
    else if eqbindings noerrbindings --all alternatives without errors bind all the concept variables to the same concepts
         then Proven gamma allnoerrinftrees  --return inference trees of all alternatives without errors
         else NoProof AmbiguousType allnoerrinftrees --return inference trees of all alternatives without errors
  ------------------------------------------------------------------------------------
  freecptvars = [cptnew $ "$C" ++ show i | i<-[1..]]
  tree (t, _) = t
  free (_, f) = f
  --DESCR -> construct a base inference tree of BoundTo statements by decomposing the expr to infer, and put concept variables at all concept locations. Return all unused concept variable names too.
  --USE -> All concept variables need to be bound to a concept. And all leaves (Stmt BoundTo expr@(Relation{})) need to be inferred.
  unboundtree :: Concepts -> (ITree, Concepts)
  unboundtree [] = error $ "Error in TypeInferenceTree.hs module InferenceRules function infer.unboundtree: " ++
                           "Provide an infinite list of free concept variables."
  unboundtree (_:[]) = error $ "Error in TypeInferenceTree.hs module InferenceRules function infer.unboundtree: " ++
                           "Provide an infinite list of free concept variables."
  unboundtree (c1:c2:fcpts) = bindsubexprs (bindto exr) (CT c1) (CT c2) fcpts False
    where
    bindto expr = \src tgt -> BoundTo expr{tt=TT src tgt}
    bindsubexprs stmt = \src tgt (cb1:cb2:fcs) inv ->
      case stmt src tgt of
        BoundTo expr -> case expr of
          Intersect{} -> (DisjRule (tree tree1) (tree tree2), free tree2)
              where
              tree1 = bindsubexprs (bindto $ left expr) src tgt (cb1:cb2:fcs) inv
              tree2 = bindsubexprs (bindto $ right expr) src tgt (free tree1) inv
          Union{} -> (UnionRule (tree tree1) (tree tree2), free tree2)
              where
              tree1 = bindsubexprs (bindto $ left expr) src tgt (cb1:cb2:fcs) inv
              tree2 = bindsubexprs (bindto $ right expr) src tgt (free tree1) inv
          Implicate{} -> (ImplyRule (tree tree1) (tree tree2), free tree2)
              where
              tree1 = bindsubexprs (bindto $ left expr) src tgt (cb1:cb2:fcs) inv
              tree2 = bindsubexprs (bindto $ right expr) src tgt (free tree1) inv
          Equality{} -> (EqualRule (tree tree1) (tree tree2), free tree2)
              where
              tree1 = bindsubexprs (bindto $ left expr) src tgt (cb1:cb2:fcs) inv
              tree2 = bindsubexprs (bindto $ right expr) src tgt (free tree1) inv
          Semicolon{} -> (RelcompRule (tree tree1) (tree tree2), free tree2)
             where
             genspec = if not inv then Specific else Generic
             tree1 = bindsubexprs (bindto $ left expr) src (CF (genspec,cb1,cb2)) fcs inv
             tree2 = bindsubexprs (bindto $ right expr) (CF (genspec,cb1,cb2)) tgt (free tree1) inv
          Dagger{} -> (AddcompRule (tree tree1) (tree tree2), free tree2)
             where
             genspec = if not inv then Generic else Specific
             tree1 = bindsubexprs (bindto $ left expr) src (CF (genspec,cb1,cb2)) fcs inv
             tree2 = bindsubexprs (bindto $ right expr) (CF (genspec,cb1,cb2)) tgt (free tree1) inv
          Complement{} -> (ComplRule (tree tree1), free tree1)
              where
              tree1 = bindsubexprs (bindto $ sub expr) src tgt (cb1:cb2:fcs) (not inv)
          Flip{} -> (FlipRule (tree tree1), free tree1)
              where
              tree1 = bindsubexprs (bindto $ sub expr) tgt src (cb1:cb2:fcs) inv
          Relation{}     -> (Stmt $ bindto expr src tgt, (cb1:cb2:fcs))
        _                    -> error $ "Error in TypeInferenceTree.hs module InferenceRules function infer.unboundtree.bindsubexprs: Only BoundTo expression are expected: "++show (stmt src tgt)++"." 
  ------------------------------------------------------------------------------------
  --DESCR -> Given a Statement from an unboundtree, this tree, and the variable environment: bind the mphats of a Relation expression to certain concept variables in this expression.
  bindMphats :: Statement -> (ITree, [BndCptVar]) -> (ITree, [BndCptVar])
  bindMphats stmt@(BoundTo r@(Relation{rel=Mph{mphats=[c1,c2]}, tt=TT ct1 ct2}) ) (itree,vars) =
     let
     --get the concept variable from the BoundTo statement to bind the first mphat to 
     var1 = case ct1 of 
        CT c -> c
        CF (f,c1',c2') -> if f==Generic then c2' else c1'
        CTake{} -> error $ "Error in TypeInferenceTree.hs module InferenceRules function bindMphats: " ++
                           "ConceptTerm CTake is not expected: "++show ct1++"." 
     --get the concept variable from the BoundTo statement to bind the second mphat to 
     var2 = case ct2 of 
        CT c -> c
        CF (f,c1',c2') -> if f==Generic then c2' else c1'
        CTake{} -> error $ "Error in TypeInferenceTree.hs module InferenceRules function bindMphats: " ++
                           "ConceptTerm CTake is not expected: "++show ct2++"."
     --get the binding of var1 from the variable environment
     bndvar1 = [(var,cpt) |(var,cpt)<-vars, var==var1]
     (var',cpt') = if not (null bndvar1) then head bndvar1 else varnotfnderr var1
     --get the binding of var2 from the variable environment
     bndvar2 = [(var,cpt) |(var,cpt)<-vars, var==var2]
     (var'',cpt'') = if not (null bndvar2) then head bndvar2 else varnotfnderr var2
     --try to bind var1 to the first mphat
     vars'  =
        if var'==cpt' -- is unbound
        then bindvar vars var1 c1 --bind unbound
        else if cpt'==c1 --already bound to the same concept by the mphats of another morphism
             then vars --variable is already bound to this mphat
             else [] --empty the variable environment
     --try to bind var2 to the second mphat
     vars'' =
        if var''==cpt'' -- is unbound
        then bindvar vars' var2 c2 --bind unbound
        else if cpt''==c2 --already bound to the same concept by the mphats of another morphism
             then vars' --variable is already bound to this mphat
             else [] --empty the variable environment
     varnotfnderr v = error $ "Error in TypeInferenceTree.hs module InferenceRules function bindMphats: " ++
                              "Concept variable could not be found in the variable environment: "++show v++"."
     in
     if null vars' && not (null vars) --if the variable environment is set to [] when binding source mphat
     --then var1 is already bound to another concept -> infer a type error
     then (attachstmt (stmt, [(EmptyStmt, Just $ Stmt $ InfErr $ IErr c1 cpt' r)]) itree, vars)
     else
        if null vars'' && not (null vars)  --if the variable environment is set to [] when binding target mphat
        --then var2 is already bound to another concept -> infer a type error
        then (attachstmt (stmt, [(EmptyStmt, Just $ Stmt $ InfErr $ IErr c2 cpt'' r)]) itree, vars)
        --else binding of the mphats is updated in the variable environment, now apply the new
        --variable binding to the unboundtree
        else (rebindtree itree vars'', vars'')
  bindMphats _ itree = itree
  ------------------------------------------------------------------------------------
  --DESCR -> infer all alternatives of all statements and bind concept vars in the var env along the way
  --EXTEND -> if the variable environment is changed then all previously inferred alts must be inferred again by deleting their inference trees!!!
  inferstmts :: ([(BndStmt,Alternatives)],[BndCptVar]) ->  [( [(BndStmt, Alternatives)] , [BndCptVar])]
  inferstmts (stms,vars) = if null toinfer --DONE!
                            then [(stms,vars)] --bind env to stmts and trees of alts?
                            --else: infer the next statement, and infer the other statements toinfer too
                            else foldr (++) [] [inferstmts alt |alt<-(inferalts $ head toinfer)]
     where
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
     --DESCR -> Checks if gamma contains an isa statement stating that c1 is-a c2
     isa c1 c2 = elem (fromIsa (c1, c2)) gamma
     --DESCR -> labelling functions to the triple result of inferalt
     altafter (x,_,_) = x
     varsafter (_,x,_) = x
     varenvchanged (_,_,x) = x
     --DESCR -> infer the BoundTo statement given the declared TypeOf statement. 
     --         The inference tree is bound to the alternative
     --         If the variable environment has been updated the Bool will be True.
     inferalt :: BndStmt -> DeclStmt -> ((BndStmt, Alternatives),[BndCptVar], Bool)
     inferalt stmt@(BoundTo expr) alt =
       let
       exprerror declexpr = error
        $ "Error in TypeInferenceTree.hs module InferenceRules function inferstmts: " ++
          "The alternative statement "++show declexpr ++" is not statement a statement on "++show expr++"."
       TT ct1 ct2 = tt expr
       in
       case alt of
        TypeOf declexpr -> tryinfer 
          where
          TT (CF (Generic,_,c1')) (CF (Generic,_,c2')) = tt declexpr
          tryinfer = if expr==declexpr then trydomain else exprerror declexpr
          trydomain = 
             let 
             --lookup concept var if it is actually a var
             c1 = if iscptvar var1 then lookupvar vars var1 else var1
             var1 = case ct1 of
                CT var1' -> var1'
                CF (_,var1',_) -> var1'
                _ -> error  $ "Error in TypeInferenceTree.hs module InferenceRules function inferstmts.inferalt: " ++
                              "Concept term CTake is not expected: "++show ct1++"."
             in
             if iscptvar c1 --var1 is not bound yet
             then tryrange (Stmt alt) (var1,bindvar vars var1 c1', True) --bind variable var1 to c1'
             else --var1 is already bound
               if c1==c1' --alternative matches conclusion
               then tryrange (Stmt alt) (var1,vars, False)
               else 
                 if c1 `isa` c1' --try specialization rule
                 then tryrange (SpecRule SpecDomain (Stmt $ fromIsa (c1, c1')) (Stmt alt)) (var1,vars, False)
                 else 
                   if c1' `isa` c1
                   then tryrange (Stmt alt) (var1,bindvar vars var1 c1', True) --rebind the var
                   else tryrange (Stmt $ InfErr $ IErr c1 c1' expr) (var1,vars, False)
               
          tryrange ruledomain (var1, varsdomain, envchanged) = 
             let 
             --lookup concept var if it is actually a var
             c2 = if iscptvar var2 then lookupvar varsdomain var2 else var2
             var2 = case ct2 of
                CT var2' -> var2'
                CF (_,var2',_) -> var2'
                _ -> error  $ "Error in TypeInferenceTree.hs module InferenceRules function inferstmts.inferalt: " ++
                              "Concept term CTake is not expected: "++show ct2++"."
             in
             if iscptvar c2 --var2 is not bound yet
             then ((stmt, [(alt, Just $ ruledomain)]),bindvar varsdomain var2 c2', True)
             else --c2 is already bound
               if c2==c2' --alternative matches conclusion
               then ((stmt, [(alt, Just $ ruledomain)]),varsdomain, envchanged)
               else
                 if c2 `isa` c2' --try specialization rule
                 then ((stmt, [(alt, Just $ SpecRule SpecRange (Stmt $ fromIsa (c2, c2')) ruledomain)]),varsdomain, envchanged)
                 else 
                   if c2' `isa` c2
                   then if var1==var2
                        --rebind the var and schedule this alt to be inferred again by setting the tree to Nothing
                        then ((stmt, [(alt, Nothing)]),bindvar varsdomain var2 c2', True) 
                        else  ((stmt, [(alt, Just $ ruledomain)]),bindvar varsdomain var2 c2', True) --rebind the var
                   else ((stmt, [(alt, Just $ Stmt $ InfErr $ IErr c2 c2' expr)]),varsdomain, envchanged)
        _ -> error $ "Error in TypeInferenceTree.hs module InferenceRules function inferstmts.inferalt: " ++
                     "The alternative statement "++show alt++" is not a TypeTo statement with two generic concept terms."
     inferalt _ _  = error
        $ "Error in TypeInferenceTree.hs module InferenceRules function inferstmts.inferalt: " ++
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
       varnotfnderr = error $ "Error in TypeInferenceTree.hs module InferenceRules function lookupvar: " ++
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
rebindtree (DisjRule tree1 tree2) vars = DisjRule (rebindtree tree1 vars) (rebindtree tree2 vars)
rebindtree (RelcompRule tree1 tree2) vars = RelcompRule (rebindtree tree1 vars) (rebindtree tree2 vars)
rebindtree (BindRule bt tree) vars = BindRule bt (rebindtree tree vars)
rebindtree (SpecRule st tree1 tree2 ) vars = SpecRule st (rebindtree tree1 vars) (rebindtree tree2 vars)
rebindtree (AddcompRule tree1 tree2 ) vars = AddcompRule (rebindtree tree1 vars) (rebindtree tree2 vars)
rebindtree (UnionRule tree1 tree2 ) vars = UnionRule (rebindtree tree1 vars) (rebindtree tree2 vars)
rebindtree (ImplyRule tree1 tree2 ) vars = ImplyRule (rebindtree tree1 vars) (rebindtree tree2 vars)
rebindtree (EqualRule tree1 tree2 ) vars = EqualRule (rebindtree tree1 vars) (rebindtree tree2 vars)
rebindtree (ComplRule tree1) vars = ComplRule (rebindtree tree1 vars)
rebindtree (FlipRule tree1) vars = FlipRule (rebindtree tree1 vars)
rebindtree (Stmt stmt) vars = Stmt (rebindstmt stmt)
  where
  rebindstmt :: BndStmt -> BndStmt
  rebindstmt (BoundTo expr) = BoundTo $ rebindexpr expr
  rebindstmt (InfErr (UndeclRel expr)) = InfErr $ UndeclRel $ rebindexpr expr
  rebindstmt (InfErr (IErr c1 c2 expr)) = InfErr $ IErr c1 c2 $ rebindexpr expr
  rebindstmt st = st
  rebindexpr :: AdlExpr -> AdlExpr
  rebindexpr expr@(Relation{}) = expr{tt=rebindtt (tt expr)}
  rebindexpr expr = error $ "Error in TypeInferenceTree.hs module InferenceRules function rebindtree.rebindexpr: " ++
                            "Other expressions than just Relation are not expected: "++show expr++"."
  rebindtt :: TypeTerm -> TypeTerm
  rebindtt (TT ct1 ct2) = TT (rebindct ct1) (rebindct ct2)
  rebindct :: ConceptTerm -> ConceptTerm
  rebindct (CT c) = CT $ rebindcpt c
  rebindct (CF (f,c1,c2)) = CF (f,rebindcpt c1,rebindcpt c2)
  rebindct ct@(CTake{}) = error $ "Error in TypeInferenceTree.hs module InferenceRules function rebindtree.rebindct: " ++
                                  "Concept term CTake is not expected: "++show ct++"."
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
   else error $ "Error in TypeInferenceTree.hs module InferenceRules function eqbindings: " ++
                "Lengths of concept variable lists differ:" ++ show (length vars) ++ " and " ++ show (length vars) ++ "."

--DESCR -> Attach all inference trees (which are proofs for the BoundTo statements in the main inference tree) using a Bind Rule
--USE -> Each BoundTo must be bound to exactly one alternative with an inference tree
attachtrees :: ( [(BndStmt, Alternatives)] , [BndCptVar]) -> ITree -> ITree
attachtrees (stms,vars) tree = rebindtree (foldr attachstmt tree stms) vars

--DESCR -> Attach the inference tree (which is the proof of a BoundTo statement in the main inference tree) using a Bind rule
--USE -> A BoundTo must be bound to exactly one alternative with an inference tree
attachstmt :: (BndStmt, Alternatives) -> ITree -> ITree
attachstmt bndstmt@(stmt,_) baseleaf@(Stmt stmt') | stmt==stmt' = BindRule (matchbindtype stmt) $ treestmt bndstmt
                                                  | otherwise   = baseleaf
attachstmt bndstmt (DisjRule baseleaf1 baseleaf2) = DisjRule (attachstmt bndstmt baseleaf1) (attachstmt bndstmt baseleaf2)
attachstmt bndstmt (RelcompRule baseleaf1 baseleaf2) = RelcompRule (attachstmt bndstmt baseleaf1) (attachstmt bndstmt baseleaf2)
attachstmt bndstmt (BindRule bt baseleaf) = BindRule bt (attachstmt bndstmt baseleaf)
attachstmt bndstmt (SpecRule st baseleaf1 baseleaf2 ) = SpecRule st (attachstmt bndstmt baseleaf1) (attachstmt bndstmt baseleaf2)
attachstmt bndstmt (AddcompRule baseleaf1 baseleaf2 ) = AddcompRule (attachstmt bndstmt baseleaf1) (attachstmt bndstmt baseleaf2)
attachstmt bndstmt (UnionRule baseleaf1 baseleaf2 ) = UnionRule (attachstmt bndstmt baseleaf1) (attachstmt bndstmt baseleaf2)
attachstmt bndstmt (ImplyRule baseleaf1 baseleaf2 ) = ImplyRule (attachstmt bndstmt baseleaf1) (attachstmt bndstmt baseleaf2)
attachstmt bndstmt (EqualRule baseleaf1 baseleaf2 ) = EqualRule (attachstmt bndstmt baseleaf1) (attachstmt bndstmt baseleaf2)
attachstmt bndstmt (ComplRule baseleaf) = ComplRule (attachstmt bndstmt baseleaf)
attachstmt bndstmt (FlipRule baseleaf) = FlipRule (attachstmt bndstmt baseleaf)

--DESCR -> Returns the matching Bind rule given the conclusion of this rule
matchbindtype :: BndStmt -> BindType
matchbindtype (BoundTo expr) = case tt expr of
   (TT (CT _) (CT _))                           -> Bind
   (TT (CF (Generic,_,_)) (CT _))               -> BindG1
   (TT (CT _) (CF (Generic,_,_)))               -> BindG2
   (TT (CF (Generic,_,_)) (CF (Generic,_,_)))   -> BindGG
   (TT (CF (Specific,_,_)) (CT _))              -> BindS1
   (TT (CT _) (CF (Specific,_,_)))              -> BindS2
   (TT (CF (Specific,_,_)) (CF (Specific,_,_))) -> BindSS
   (TT (CF (Specific,_,_)) (CF (Generic,_,_)))  -> BindSG
   (TT (CF (Generic,_,_)) (CF (Specific,_,_)))  -> BindGS
   _ -> error $ "Error in TypeInferenceTree.hs module InferenceRules function matchbindtype: " ++
                "BoundTo statement does not match a Bind rule pattern: "++show (BoundTo expr)++"."
matchbindtype stmt = error $ "Error in TypeInferenceTree.hs module InferenceRules function matchbindtype: " ++
                     "The statement "++show stmt++" is not a BoundTo."

--DESCR -> Returns the inference tree which is the proof of a BoundTo statement based on statements derived from ADL declarations
--USE -> A BoundTo must be bound to exactly one alternative with an inference tree
treestmt :: (BndStmt, Alternatives) -> ITree
treestmt (stmt@(BoundTo{}),[]) = error $ "Error in TypeInferenceTree.hs module InferenceRules function treestmt: " ++
                                              "The statement "++show stmt++" is not bound to any alternative."
treestmt ((BoundTo{}),[(_, Just inftree )]) = inftree
treestmt (stmt@(BoundTo{}),[(alt, Nothing)]) = error $ "Error in TypeInferenceTree.hs module InferenceRules function treestmt: " ++
                                                       "The statement "++show stmt++" is bound to alternative "++ show alt ++" without inference tree."
treestmt (stmt@(BoundTo{}),alts) = error $ "Error in TypeInferenceTree.hs module InferenceRules function treestmt: " ++
                                           "The statement "++show stmt++" is bound to more than one alternative: "++ show alts ++"."
treestmt (stmt,_) = error $ "Error in TypeInferenceTree.hs module InferenceRules function treestmt: " ++
                            "The statement "++show stmt++" is not a BoundTo."
