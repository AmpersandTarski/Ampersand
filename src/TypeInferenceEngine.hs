{-# OPTIONS_GHC -Wall #-}
--TODO -> Detect ambiguity by checking if all possible types infer the same type. If not, then ambiguous?
--        The type derivation is the set of inference tree, one for each option.
module TypeInferenceEngine where
import Adl.Concept
import Adl.MorphismAndDeclaration
import Adl.FilePos
import Data.Maybe
import TypeInference.ITree
import TypeInference.Statements
import TypeInference.AdlExpr

import CommonClasses --for ghci only

type BndStmt = Statement
type DeclStmt = Statement
type Alternatives = [(DeclStmt,Maybe ITree)]
type CptVar = Concept
type BndCptVar = (CptVar,Concept)

check ctx = [infer (gamma expr) expr | expr<-exprs]
            -- error $ show (mphStmts $ fromExpression "x/\\y")
  where
  --DESCR -> All Concepts in ctx
  tc :: Concepts
  tc = [cptnew "C1",cptnew "C2",cptnew "C3",cptnew "C4"]
  tc0 :: Concepts
  tc0 = [Anything,NOthing]
  isatree = [(cptnew "C1", cptnew "C3"), (cptnew "C2", cptnew "C4")]
--  isa c1 c2 = elem (c1,c2) isatree
  isaStmts = map fromIsa isatree
  --DESCR -> All Relation decls in ctx
  rv :: Declarations
  rv = [newdcl "x" (cptnew "C1") (cptnew "C2"), newdcl "y" (cptnew "C3") (cptnew "C4"), newdcl "amb1" (cptnew "Camb1") (cptnew "Camb1"), newdcl "amb1" (cptnew "Camb2") (cptnew "Camb2")]
  rc :: Declarations
  rc = [Isn c c | c<-tc] ++ [Vs c1 c2 | c1<-tc, c2<-tc]
  exprs = map fromExpression ["amb1[Camb2*Camb2];undecl[Camb2*Cx]","-(x;x~);x","x;undecl;x","y/\\x","amb1[Camb2*Camb2]","amb1","x;x;y"]
  --TODO -> I could split gamma in two
  gamma expr = (mphStmts expr)
                ++ isaStmts
  mphStmts :: AdlExpr -> [Statement]
  --TODO -> zoek in rv of declared is
  --mphStmts (Relation m@(Mph{mphnm=r1, mphats=[c1,c2]}) i _) =  [BndStat (Relation m i) (c1, c2)]
  --mphStmts (Relation m@(Mph{mphnm=r1, mphats=[]}) i _) =
  mphStmts (Relation m@(Mph{mphnm=r1}) i t) =
     let
     alternatives = [TypeStat (Relation m i t) (c1, c2) | Sgn{decnm=decl,desrc=c1,detgt=c2}<-rv, decl==r1]
     in
     if null alternatives
     then [InfErr (UndeclRel (Relation m i t)) "Relation has not been declared."]
     else alternatives
  --mphStmts (Relation m@(Mph{}) _ _)=  error $ "Error in TypeInferenceTree.hs module InferenceRules function fromMph: " ++
    --                                        "Morphism "++ show m ++" does not have mphats with length 0 or 2."
  mphStmts (Implicate expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Equality expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Union expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Intersect expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Semicolon expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Dagger expr1 expr2 _) = mphStmts expr1 ++ mphStmts expr2
  mphStmts (Complement expr _) = mphStmts expr
  mphStmts (Flip expr _) = mphStmts expr

--TODO -> put exr in tex in the trees as condition
--DESCR -> return a typed AdlExpr and the inference tree
--infer :: [Statement] -> AdlExpr -> (AdlExpr, ITree)
infer gamma exr  = -- (exr, combinetrees step3inferstmts step2tree)
                   error $ show (step2tree,step2cptvars)
  where
  step1tree = tree $ unboundtree freecptvars
  --DESCR -> Get all used concept variables and leave them unbound by relating them to themselves
  step1cptvars = [(var,var) | var<-(takeWhile ((/=)(head $ free $ unboundtree freecptvars)) freecptvars)]
  (step2tree,step2cptvars) = foldr bindMphats (step1tree,step1cptvars) (stmts step1tree)
  step3inferstmts :: [( [(Statement, Alternatives)] , [BndCptVar])]
  step3inferstmts = bindvars ( bind2declAlts, step2cptvars)
     where
     --DESCR -> bind statements with concept vars to bound alternatives
     bind2declAlts :: [(BndStmt,Alternatives)]
     bind2declAlts = [(stmt,declAlts stmt)| stmt<-stmts step2tree]
        where
        --DESCR -> bound alternatives of statement with concept vars without inference tree
        declAlts :: BndStmt -> Alternatives
        declAlts stmt@(BoundTo relvar) =
           let
           undeclerr = [(EmptyStmt, Just $ Stmt stmt') | stmt'@(InfErr (UndeclRel relvar') _)<-gamma, relvar==relvar']
           in
           if null undeclerr
           then -- case relvar of
              --  (Relation (Mph{mphats=[explc1,explc2]}) _ _) -> [(BndStat relvar (explc1,explc2),Nothing)]
               -- _ ->
                [(stmt',Nothing) | stmt'@(TypeStat relvar' _)<-gamma, relvar==relvar']
           else undeclerr
  ------------------------------------------------------------------------------------
  freecptvars = [cptnew $ "$C" ++ show i | i<-[1..]]
  --unboundtree :: (ITree, Concepts)
  tree (t, _) = t
  free (_, f) = f
  unboundtree (c1:c2:fcs) = bindsubexprs (bindto exr) (CT c1) (CT c2) fcs False
    where
    bindto expr = \src tgt -> BoundTo expr{tt=TT src tgt}
    --pattern matching a disjunction statement \env c1 c2 -> env |- (x /\ y)[c1*c2]
    --infer :: (InfTree a) => (Concept -> Concept -> Statement) -> Concept -> Concept -> a
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
          _       -> error ""
        _                    -> error ""
  ------------------------------------------------------------------------------------
  bindMphats :: Statement -> (ITree, [BndCptVar]) -> (ITree, [BndCptVar])
  bindMphats stmt@(BoundTo r@(Relation{rel=Mph{mphats=[c1,c2]}, tt=TT ct1 ct2}) ) (itree,vars) =
     let
     var1 = case ct1 of 
        CT c -> c
        CF (f,c1,c2) -> if f==Generic then c2 else c1
        CTake{} -> error $ "Error in TypeInferenceTree.hs module InferenceRules function bindMphats: " ++
                           "ConceptTerm CTake is not expected: "++show ct1++"." 
     var2 = case ct2 of 
        CT c -> c
        CF (f,c1,c2) -> if f==Generic then c2 else c1
        CTake{} -> error $ "Error in TypeInferenceTree.hs module InferenceRules function bindMphats: " ++
                           "ConceptTerm CTake is not expected: "++show ct2++"."
     --unbndvar1 = [(var,cpt) |(var,cpt)<-vars, var==var1, var==cpt]
     bndvar1 = [(var,cpt) |(var,cpt)<-vars, var==var1]
     (var',cpt') = if not (null bndvar1) then head bndvar1 else varnotfnderr var1
     bndvar2 = [(var,cpt) |(var,cpt)<-vars, var==var2]
     (var'',cpt'') = if not (null bndvar2) then head bndvar2 else varnotfnderr var2
     vars'  =
        if var'==cpt' -- is unbound
        then bindvar vars var1 c1 --bind unbound
        else if cpt'==c1 --already bound to the same concept by the mphats of another morphism
             then vars --variable is already bound to this mphat
             else [] --empty the variable environment
     vars'' =
        if var''==cpt'' -- is unbound
        then bindvar vars' var2 c2 --bind unbound
        else if cpt''==c2 --already bound to the same concept by the mphats of another morphism
             then vars' --variable is already bound to this mphat
             else [] --empty the variable environment
     varnotfnderr v = error $ "Error in TypeInferenceTree.hs module InferenceRules function bindMphats: " ++
                              "Concept variable could not be found in the variable environment: "++show v++"."
     in
     if null vars' && not (null vars) --if 
     then (attachstmt (stmt, [(EmptyStmt, Just $ Stmt $ InfErr IErr $ expectedErr c1 cpt' r)]) itree, vars)
     else
        if null vars'' && not (null vars)
        then (attachstmt (stmt, [(EmptyStmt, Just $ Stmt $ InfErr IErr $ expectedErr c2 cpt'' r)]) itree, vars)
        else (rebindtree itree vars'', vars'')
  bindMphats _ itree = itree
  ------------------------------------------------------------------------------------
  --DESCR -> infer all statements and bind concept vars along the way
  bindvars :: ([(BndStmt,Alternatives)],[BndCptVar]) ->  [( [(BndStmt, Alternatives)] , [BndCptVar])]
  bindvars (stmts,vars) = if null toinfer
                          then [(stmts,vars)]
                          else foldr (++) [] [bindvars alt |alt<-(inferalts $ head toinfer)]
                          --error $ show ([bindvars alt |alt<-(inferalts $ head toinfer)])
     where
     toinfer = [(stmt,alts) | (stmt,alts)<-stmts, uninferred alts]
     uninferred alts = if ln==(length alts) || ln==0 then ln==(length alts) else uninferror
        where
        ln = length [alt | alt@(_,Nothing)<-alts]
        uninferror = error $ "Error in TypeInferenceTree.hs module InferenceRules function bindvars: " ++
                             "Some of the alternatives are inferred and some are not: "++show alts++"."

     inferalts :: (BndStmt,Alternatives) -> [( [(BndStmt, Alternatives)] , [BndCptVar])]
     inferalts (currstmt,curralts) = map rebindvars
        [(
            (pickalt $ thisalt currstmt alt) --dit alternatief wordt gekozen
            :[(otherstmt,otheralts) | (otherstmt,otheralts)<-stmts, otherstmt/=currstmt ] --copy alles behalve het huidige te infereren statement
         ,  --de nieuwe vars tabel gegeven de keuze voor dit alternatief
            vars' $ thisalt currstmt alt
         )
        | (alt,Nothing)<-curralts ] --voor alle alternatieven
     pickalt (x,_) = x
     vars' (_,x) = x
     isa c1 c2 = elem (fromIsa (c1, c2)) gamma
     thisalt :: BndStmt -> DeclStmt -> ((BndStmt, Alternatives),[BndCptVar])
     thisalt stmt@(BndStat expr (c1,c2)) alt = -- ((stmt, [alt]),vars) --maak inf tree voor dit alternatief
       let
       exprerror expr' = error
        $ "Error in TypeInferenceTree.hs module InferenceRules function bindvars: " ++
          "The alternative statement "++show expr'++" is not statement a statement on "++show expr++"."
       in
       case alt of
        BndStat expr' (c1',c2') -> tryalt
          where
          tryalt = if expr==expr' then trydomain else exprerror expr'
          trydomain = if (head $ name c1)=='$' --c1 is a variable
                      then tryrange (Stmt alt) (bindvar vars c1 c1') --bind variable c1 to c1'
                      else --c1 is already bound
                        if c1==c1'
                        then tryrange (Stmt alt) vars
                        else ((stmt, [(alt, Just $ Stmt $ InfErr IErr $ expectedErr c1 c1' expr)]) ,vars)
          tryrange ruledomain varsdomain = if (head $ name c2)=='$' --c2 is a variable
                                then ((stmt, [(alt, Just $ ruledomain)]),bindvar varsdomain c2 c2')
                                else --c2 is already bound
                                  if c2==c2'
                                  then ((stmt, [(alt, Just $ ruledomain)]),varsdomain)
                                  else ((stmt, [(alt, Just $ Stmt $ InfErr IErr$ expectedErr c2 c2' expr)]),varsdomain)
        TypeStat expr' (c1',c2') -> tryalt
          where
          tryalt = if expr==expr' then trydomain else exprerror expr'
          trydomain = if (head $ name c1)=='$' --c1 is a variable
                      then tryrange (Stmt alt) (bindvar vars c1 c1') --bind variable c1 to c1'
                      else --c1 is already bound
                        if c1==c1' --alternative matches conclusion
                        then tryrange (Stmt alt) vars
                        else
                          if c1 `isa` c1' --try specialization rule
                          then tryrange (SpecRule (Stmt $ fromIsa (c1, c1')) (Stmt alt)) vars
                          else tryrange (Stmt $ InfErr IErr $ expectedErr c1 c1' expr) vars
          tryrange ruledomain varsdomain =
                                if (head $ name c2)=='$' --c2 is a variable
                                then ((stmt, [(alt, Just $ ruledomain)]),bindvar varsdomain c2 c2')
                                else --c2 is already bound
                                  if c2==c2' --alternative matches conclusion
                                  then ((stmt, [(alt, Just $ ruledomain)]),varsdomain)
                                  else
                                    if c2 `isa` c2' --try specialization rule
                                    then ((stmt, [(alt, Just $ SpecRule (Stmt $ fromIsa (c2, c2')) ruledomain)]),varsdomain)
                                    else ((stmt, [(alt, Just $ Stmt $ InfErr IErr $ expectedErr c2 c2' expr)]),varsdomain)
        _ -> error $ "Error in TypeInferenceTree.hs module InferenceRules function bindvars: " ++
                     "The alternative statement "++show alt++" is not a TypeStat or BoundTo statement."
     thisalt _ _  = error
        $ "Error in TypeInferenceTree.hs module InferenceRules function bindvars: " ++
          "This function only binds concept variables of BndStmt statements."
     ------------------------------------------
     -- endfunction -> thisalt stmt alt      --
   ------------------------------------------
   -- endfunction -> bindvars (stmts,vars) --
-------------------------------------------
-- ENDFUNCTION -> infer gamma exr        --
---------------------------------

expectedErr :: (Show a) => a -> a -> AdlExpr -> String
expectedErr x y expr = "Expected type " ++ show x ++ " does not match inferred type " ++ show y ++ ".\n"
                    ++ "at " ++ show expr ++ ".\n"

bindvar :: [BndCptVar] -> CptVar -> Concept -> [BndCptVar]
bindvar [] _ _ = []
bindvar ((var,cpt):vars) var' cpt' | var==var' = (var,cpt'):vars
                                   | otherwise = (var,cpt):(bindvar vars var' cpt')


--DESCR -> replace all bound concept variables in all statements
rebindvars :: ( [(Statement, Alternatives)] , [BndCptVar]) -> ( [(Statement, Alternatives)] , [BndCptVar])
rebindvars (stmts,vars) = ([(foldr rebindstmt stmt vars, alts) | (stmt,alts)<-stmts],vars)

--DESCR -> Substitutes the first Concept argument by the second in the ITree
--USE -> if a tree contains concept variable "$C1" then I can bind the variable to "ACpt" by bindCptvar tree "$C1" "Acpt"
bindCptvar :: ITree -> Concept -> Concept -> ITree
bindCptvar (Stmt bndstmt@(BoundTo{})) var cpt = Stmt $ rebindstmt (var,cpt) bndstmt
bindCptvar stmt@(Stmt _) _ _ = stmt --other statements do not have concept vars
bindCptvar (DisjRule tr1 tr2) var cpt = DisjRule (bindCptvar tr1 var cpt) (bindCptvar tr2 var cpt)
bindCptvar (RelcompRule tr1 tr2) var cpt = RelcompRule (bindCptvar tr1 var cpt) (bindCptvar tr2 var cpt)
bindCptvar (BindRule bt tr) var cpt = BindRule bt (bindCptvar tr var cpt)
bindCptvar (SpecRule tr1 tr2) var cpt = SpecRule (bindCptvar tr1 var cpt) (bindCptvar tr2 var cpt)


rebindstmt :: BndCptVar -> BndStmt -> BndStmt
rebindstmt bndvar (BoundTo expr) = BoundTo $ rebindexpr bndvar expr
rebindstmt _ stmt = error $ "Error in TypeInferenceTree.hs module InferenceRules function rebindstmt: " ++
                            "Other statements than BoundTo are not expected: "++show stmt++"."

rebindexpr :: BndCptVar -> AdlExpr -> AdlExpr
rebindexpr bndvar expr@(Relation{}) = expr{tt=rebindtt bndvar (tt expr)}
rebindexpr _ expr = error $ "Error in TypeInferenceTree.hs module InferenceRules function rebindexpr: " ++
                            "Other expressions than just Relation are not expected: "++show expr++"."

rebindtt :: BndCptVar -> TypeTerm -> TypeTerm
rebindtt bndvar (TT ct1 ct2) = TT (rebindct bndvar ct1) (rebindct bndvar ct2)

rebindct :: BndCptVar -> ConceptTerm -> ConceptTerm
rebindct bndvar@(var,cbnd) (CT c) = if var==c then CT cbnd else CT c
rebindct bndvar@(var,cbnd) (CF (f,c1,c2)) = 
   if var==c1 && var==c2 then CF (f,cbnd,cbnd) 
   else if var==c1 then CF (f,cbnd,c2)
        else if var==c2 then CF (f,c1,cbnd)
             else CF (f,c1,c2)
rebindct _ ct@(CTake{}) = error $ "Error in TypeInferenceTree.hs module InferenceRules function rebindct: " ++
                                  "Concept term CTake is not expected: "++show ct++"."

rebindtree :: ITree -> [BndCptVar] -> ITree
rebindtree (Stmt stmt) vars = Stmt (foldr rebindstmt stmt vars)
rebindtree (DisjRule tree1 tree2) vars = DisjRule (rebindtree tree1 vars) (rebindtree tree2 vars)
rebindtree (RelcompRule tree1 tree2) vars = RelcompRule (rebindtree tree1 vars) (rebindtree tree2 vars)
rebindtree (BindRule bt tree) vars = BindRule bt (rebindtree tree vars)
rebindtree (SpecRule tree1 tree2 ) vars = SpecRule (rebindtree tree1 vars) (rebindtree tree2 vars)

combinetrees :: [( [(BndStmt, Alternatives)] , [BndCptVar])] -> ITree -> InferredType
combinetrees alts unboundtree =
   let
   noerrbindings = [ vars | (stmts,vars)<-noerralts ]
   noerralts = [(stmts,vars) | ((stmts,vars),errstmts)<-errstmtsOfAlts, null errstmts ]
   errstmtsOfAlts = [((stmts,vars),catMaybes [errstmt stmt| stmt<-stmts]) | (stmts,vars)<-alts]
   allinftrees = [attachtrees alt unboundtree | alt<-alts]
   allnoerrinftrees = [attachtrees alt unboundtree | alt<-noerralts ]
   in
   if null noerralts --there are no alternatives inferring a type
   then TypeErrors NoType allinftrees  --return inference trees of all alternatives
   else if eqbindings noerrbindings --all alternatives without errors bind all the concept variables to the same concepts
        then Type allnoerrinftrees  --return inference trees of all alternatives without errors
        else TypeErrors AmbiguousType allnoerrinftrees --return inference trees of all alternatives without errors

eqbindings :: [[BndCptVar]] -> Bool
eqbindings [] = True
eqbindings (vars:[]) = True
eqbindings (vars:vars':varss) =
   if length vars == length vars'
   then foldr (&&) True [elem var vars' | var<-vars]
   else error $ "Error in TypeInferenceTree.hs module InferenceRules function allequal: " ++
                "Lengths of concept variable lists differ:" ++ show (length vars) ++ " and " ++ show (length vars) ++ "."

attachtrees :: ( [(BndStmt, Alternatives)] , [BndCptVar]) -> ITree -> ITree
attachtrees (stmts,vars) unboundtree = foldr attachstmt (rebindtree unboundtree vars) stmts

--DESCR -> Attach the sub inference tree which is the proof of a BoundTo statement in the main inference tree
--USE -> A BoundTo must be bound to exactly one alternative
--TODO -> change the type to (Statement,Alternative) -> ITree -> ITree
attachstmt :: (BndStmt, Alternatives) -> ITree -> ITree
attachstmt bndstmt@(stmt,_) baseleaf@(Stmt stmt') | stmt==stmt' = BindRule Bind $ treestmt bndstmt
                                                  | otherwise   = baseleaf
attachstmt bndstmt (DisjRule baseleaf1 baseleaf2) = DisjRule (attachstmt bndstmt baseleaf1) (attachstmt bndstmt baseleaf2)
attachstmt bndstmt (RelcompRule baseleaf1 baseleaf2) = RelcompRule (attachstmt bndstmt baseleaf1) (attachstmt bndstmt baseleaf2)
attachstmt bndstmt (BindRule bt baseleaf) = BindRule bt (attachstmt bndstmt baseleaf)
attachstmt bndstmt (SpecRule baseleaf1 baseleaf2 ) = SpecRule (attachstmt bndstmt baseleaf1) (attachstmt bndstmt baseleaf2)

--DESCR -> Returns the error string in case there is no proof of a BoundTo statement based on statements derived from ADL declarations
--USE -> A BoundTo must be bound to exactly one alternative
--TODO -> change the type to (Statement,Alternative) -> ITree to get ride of most error cases
errstmt :: (BndStmt, Alternatives) -> Maybe String
errstmt (stmt@(BoundTo expr),[]) = error $ "Error in TypeInferenceTree.hs module InferenceRules function errstmt: " ++
                                             "The statement "++show stmt++" is not bound to any alternative."
errstmt (stmt@(BoundTo{}),[(_, Just (Stmt (InfErr IErr err)) )]) = Just err
errstmt (stmt@(BoundTo{}),[(_, Just (Stmt (InfErr (UndeclRel r1) err)) )]) = Just $ "Relation " ++ show r1 ++ " has not been declared."
errstmt (stmt@(BoundTo{}),[alt]) = Nothing
errstmt (stmt@(BoundTo{}),alts) = error $ "Error in TypeInferenceTree.hs module InferenceRules function errstmt: " ++
                                          "The statement "++show stmt++" is bound to more than one alternative: "++ show alts ++"."
errstmr (stmt,_) = error $ "Error in TypeInferenceTree.hs module InferenceRules function errstmt: " ++
                           "The statement "++show stmt++" is not a BndStmt."

--DESCR -> Returns the inference tree which is the proof of a BoundTo statement based on statements derived from ADL declarations
--USE -> A BoundTo must be bound to exactly one alternative
--TODO -> change the type to (Statement,Alternative) -> ITree
treestmt :: (BndStmt, Alternatives) -> ITree
treestmt (stmt@(BoundTo expr),[]) = error $ "Error in TypeInferenceTree.hs module InferenceRules function errstmt: " ++
                                              "The statement "++show stmt++" is not bound to any alternative."
treestmt (stmt@(BoundTo{}),[(_, Just inftree )]) = inftree
treestmt (stmt@(BoundTo{}),[(alt, Nothing)]) = error $ "Error in TypeInferenceTree.hs module InferenceRules function treestmt: " ++
                                                       "The statement "++show stmt++" is bound to alternative "++ show alt ++" without inference tree."
treestmt (stmt@(BoundTo{}),alts) = error $ "Error in TypeInferenceTree.hs module InferenceRules function treestmt: " ++
                                           "The statement "++show stmt++" is bound to more than one alternative: "++ show alts ++"."
treestmt (stmt,_) = error $ "Error in TypeInferenceTree.hs module InferenceRules function treestmt: " ++
                            "The statement "++show stmt++" is not a BoundTo."
                            









--DESCR -> an inference tree represents a logical tree up-side-down with axiom statements in the leaves
--         combined by n-ary logical rules, where n is the number of axioms for that rule. An axiom of
--         a rule is thus an inference tree itself.
--EXTEND -> If an inference tree needs to be built with an unimplemented logical rule then this rule
--          needs to be implemented as a data type taking n axioms (all the top axioms of the logical tree
--          of the rule). This data type must be implemented as an instance of InfTree.
--class InfTree a where
 --DESCR -> evaluates the InfTree to a Statement
-- evaluate :: a -> Statement
 --DESCR -> returns a list of all BndStat Statements in the InfTree
-- boundTerms :: a -> BoundTerms



{-
data Axiom = Axiom Statement
instance InfTree Axiom where
 evaluate (Axiom x) = x
 boundTerms (Axiom x) = boundterm x
   where
   boundterm :: Statement -> BoundTerms
   boundterm x@(BndStat{}) = [x]
   boundterm _             = [] --[] if statement is not a bound term otherwise the [x]

data SpecIsaRule a b = SpecIsa a b
instance (InfTree a, InfTree b) => InfTree (SpecIsaRule a b) where
 evaluate (SpecIsa ax1 ax2) = specisarule (evaluate ax1) (evaluate ax2)
 boundTerms (SpecIsa ax1 ax2) = boundTerms ax1 ++ boundTerms ax2

specisarule :: Statement -> Statement -> Statement
specisarule (TypeStat x (c1,c2)) (DisjStat c3 c4) = TypeStat x (subst c1,subst c2)
  where
  subst c | c==c3 || c==c4 = NOthing
          | otherwise      = c
specisarule (TypeStat x (c1, c2)) (IsaStat cspc cgen) = TypeStat x (subst c1, subst c2)
  where
  subst c | c==cgen   = cspc
          | otherwise = c
specisarule _ _ = error "Bad use of Spec. IS-a rule."

data BindRule a = Bind a
instance (InfTree a) => InfTree (BindRule a) where
 evaluate (Bind ax1) = bindrule (evaluate ax1)
 boundTerms x@(Bind ax1) = (evaluate x):(boundTerms ax1)  --add the result of the Bind rule as bound term

bindrule :: Statement -> Statement
bindrule (TypeStat expr tp) = BndStat expr tp
bindrule _ = error "Bad use of Bind rule."

data IntUnRule a b = IntUn a b
instance (InfTree a, InfTree b) => InfTree (IntUnRule a b) where
 evaluate (IntUn ax1 ax2) = introunionrule (evaluate ax1) (evaluate ax2)
 boundTerms (IntUn ax1 ax2) = boundTerms ax1 ++ boundTerms ax2

introunionrule :: Statement -> Statement -> Statement
introunionrule x@(BndStat{}) y@(BndStat{}) = LAndStat x y
introunionrule _ _ = error "Bad use of Intro Union rule."

data DisjRule a = Disj a
instance (InfTree a) => InfTree (DisjRule a) where
 evaluate (Disj ax1) = disjrule (evaluate ax1)
 boundTerms x@(Disj ax1) = (evaluate x):(boundTerms ax1)

disjrule :: Statement -> Statement
disjrule (LAndStat (BndStat x (c1, c2)) (BndStat y (c3, c4)) )
  | not(c1==c3) = InfErr "Sources of disjunction do not match."
  | not(c2==c4) = InfErr "Targets of disjunction do not match."
  | otherwise = BndStat (x ++ "/\\" ++ y) (c1, c2)
disjrule _ = error "Bad use of Disjunction rule."
-}


