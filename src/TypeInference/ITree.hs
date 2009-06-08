{-# OPTIONS_GHC -Wall #-}
module TypeInference.ITree where
import Adl.Concept
import TypeInference.Statements
import TypeInference.AdlExpr

--DESCR -> Or I have a type, proofed by the fact that all alternatives resulting in a type, result in the same type.
--         Or I could not infer a type, proofed by the fact that all alternatives result in error(s).
--         Or I have an ambiguous type, proofed by the fact that some alternatives result in different types.
data Proof = Proven Gamma [ITree] | NoProof TypeErrorsType [ITree]  deriving (Show)
data TypeErrorsType = NoType | AmbiguousType deriving (Show)

instance Association Proof where
  source (Proven _ [] ) = Anything
  source (Proven gm inftrees ) = source $ evalstmt gm $ evaltree (head inftrees,False)
  source (NoProof _ _ ) = NOthing
  target (Proven _ [] ) = Anything
  target (Proven gm inftrees ) = target $ evalstmt gm $ evaltree (head inftrees,False)
  target (NoProof _ _ ) = NOthing

--DESCR -> For type inference we defined rules to be able to construct an inference tree, to infer a type or type error, for all expressions.
--         Stmt is a basic statement
--         BindRule binds a TypeStat statement resulting in a BndStat statement
--         SpecRule specifies the domain or range of the type in a TypeStat statement given a certain IsaStat statement
--         DisjRule determines the BndStat statement of a disjunction expression based on the BndStat of its left and right expression
--         RelCompRule determines the BndStat statement of a relative composition expression based on the BndStat of its left and right expression
data ITree = Stmt Statement
           | DisjRule ITree ITree
           | UnionRule ITree ITree
           | ImplyRule ITree ITree
           | EqualRule ITree ITree
           | RelcompRule ITree ITree
           | AddcompRule ITree ITree
           | BindRule BindType ITree
           | ComplRule ITree
           | FlipRule ITree
           | SpecRule SpecType ITree ITree deriving (Show)

data BindType = Bind | BindG1 | BindG2 | BindGG | BindS1 | BindS2 | BindSS | BindSG | BindGS deriving (Show)
data SpecType = SpecDomain | SpecRange deriving (Show) 

--DESCR -> returns all the Stmt statements in a tree
stmts :: ITree -> [Statement]
stmts (Stmt stmt) = [stmt]
stmts (DisjRule tr1 tr2) = (stmts tr1) ++ (stmts tr2)
stmts (UnionRule tr1 tr2) = (stmts tr1) ++ (stmts tr2)
stmts (RelcompRule tr1 tr2) = (stmts tr1) ++ (stmts tr2)
stmts (AddcompRule tr1 tr2) = (stmts tr1) ++ (stmts tr2)
stmts (ImplyRule tr1 tr2) = (stmts tr1) ++ (stmts tr2)
stmts (EqualRule tr1 tr2) = (stmts tr1) ++ (stmts tr2)
stmts (FlipRule tr) = stmts tr
stmts (ComplRule tr) = stmts tr
stmts (BindRule _ tr) = stmts tr
stmts (SpecRule _ tr1 tr2) = (stmts tr1) ++ (stmts tr2)

type Inverse = Bool
evaltree :: (ITree,Inverse) -> (Statement,Inverse)
evaltree (Stmt stmt,inv) = (stmt,inv)
evaltree (DisjRule tr1 tr2,inv) = (BoundTo $ Intersect ex rex $ TT c1 c2,inv)
   where
   (BoundTo ex,_) = evaltree (tr1,inv)
   (BoundTo rex,_) = evaltree (tr2,inv)
   c1 = if (exprsrc ex)==(exprsrc rex) 
        then exprsrc ex
        else CT NOthing
   c2 = if (exprtgt ex)==(exprtgt rex) 
        then exprtgt ex
        else CT NOthing
evaltree (UnionRule tr1 tr2,inv) = (BoundTo $ Union ex rex $ TT c1 c2,inv)
   where
   (BoundTo ex,_) = evaltree (tr1,inv)
   (BoundTo rex,_) = evaltree (tr2,inv)
   c1 = if (exprsrc ex)==(exprsrc rex) 
        then exprsrc ex
        else CT NOthing
   c2 = if (exprtgt ex)==(exprtgt rex) 
        then exprtgt ex
        else CT NOthing
evaltree (RelcompRule tr1 tr2,inv) = (BoundTo $ Semicolon 
                                        (ex {tt= TT c1 (CTake (Specific,[c2,c3,c4]))})
                                        (rex {tt= TT (CTake (Specific,[c2,c3,c4])) c5})
                                        $ TT c1 c5
                                      ,inv) 
   where
   (BoundTo ex,_) = evaltree (tr1,inv)
   (BoundTo rex,_) = evaltree (tr2,inv)
   c1 = exprsrc ex
   c2 = if c2l==c2r then c2l else NOthing
   CF (Specific,c2l,c3) = exprtgt ex
   CF (Specific,c2r,c4) = exprsrc rex
   c5 = exprtgt rex
evaltree (AddcompRule tr1 tr2,inv) = (BoundTo $ Dagger 
                                        (ex {tt= TT c1 (CTake (Generic,[c2,c3,c4]))})
                                        (rex {tt= TT (CTake (Generic,[c2,c3,c4])) c5})
                                        $ TT c1 c5
                                      ,inv) 
   where
   (BoundTo ex,_) = evaltree (tr1,inv)
   (BoundTo rex,_) = evaltree (tr2,inv)
   c1 = exprsrc ex
   c2 = if c2l==c2r then c2l else NOthing
   CF (Generic,c2l,c3) = exprtgt ex
   CF (Generic,c2r,c4) = exprsrc rex
   c5 = exprtgt rex
evaltree (ImplyRule tr1 tr2,inv) =  (BoundTo $ Implicate ex rex $ TT c1 c2,inv) 
   where
   (BoundTo ex,_) = evaltree (tr1,inv)
   (BoundTo rex,_) = evaltree (tr2,inv)
   c1 = if (exprsrc ex)==(exprsrc rex) 
        then exprsrc ex
        else CT NOthing
   c2 = if (exprtgt ex)==(exprtgt rex) 
        then exprtgt ex
        else CT NOthing
evaltree (EqualRule tr1 tr2,inv) = (BoundTo $ Equality ex rex $ TT c1 c2,inv) 
   where
   (BoundTo ex,_) = evaltree (tr1,inv)
   (BoundTo rex,_) = evaltree (tr2,inv)
   c1 = if (exprsrc ex)==(exprsrc rex) 
        then exprsrc ex
        else CT NOthing
   c2 = if (exprtgt ex)==(exprtgt rex) 
        then exprtgt ex
        else CT NOthing
evaltree (FlipRule tr,inv) = (BoundTo $ Flip sb $ TT c2 c1,inv) 
   where
   (BoundTo sb,_) = evaltree (tr,inv)
   c1 = exprsrc sb
   c2 = exprtgt sb
evaltree (ComplRule tr,inv) = (BoundTo $ Complement sb $ TT c1 c2,not inv) 
   where
   (BoundTo sb,_) = evaltree (tr,not inv)
   c1 = exprsrc sb
   c2 = exprtgt sb
evaltree (BindRule bt tr,inv) = (BoundTo (sb{tt=boundtype}),inv)  
   where
   (TypeOf sb,_) = evaltree (tr,inv)
   CF (Generic,c1,c2) = exprsrc sb
   CF (Generic,c3,c4) = exprtgt sb
   boundtype = case bt of
     Bind -> TT (CT c1) (CT c3)
     BindG1 -> TT (CF (Generic,c1,c2)) (CT c3)
     BindG2 -> TT (CT c1) (CF (Generic,c3,c4))
     BindGG -> TT (CF (Generic,c1,c2)) (CF (Generic,c3,c4))
     BindS1 -> TT (CF (Specific,c1,c2)) (CT c3)
     BindS2 -> TT (CT c1) (CF (Specific,c3,c4))
     BindSS -> TT (CF (Specific,c1,c2)) (CF (Specific,c3,c4))
     BindGS -> TT (CF (Generic,c1,c2)) (CF (Specific,c3,c4))
     BindSG -> TT (CF (Specific,c1,c2)) (CF (Generic,c3,c4))
evaltree (SpecRule st tr1 tr2,inv) = (TypeOf (sb{tt=boundtype}),inv) 
   where
   (IsaStat c3 c1,_) = evaltree (tr1,inv)
   (TypeOf sb,_) = evaltree (tr2,inv)
   CF (Generic,lc1,lc2) = exprsrc sb
   CF (Generic,rc1,rc2) = exprtgt sb
   boundtype = case st of
     SpecDomain -> if c1==lc1 then TT (CF (Generic,c3,lc2)) (exprtgt sb) else TT (CT NOthing) (CT NOthing)
     SpecRange ->  if c1==rc1 then TT (exprsrc sb) (CF (Generic,c3,rc2)) else TT (CT NOthing) (CT NOthing)

evalstmt :: Gamma -> (Statement, Inverse) -> Sign
evalstmt gm (BoundTo expr,inv) = if inv then (evalCT gm $ inverseCT c1, evalCT gm $ inverseCT c2) else (evalCT gm c1,evalCT gm c2) 
  where
  c1=exprsrc expr
  c2=exprtgt expr
evalstmt _ _ = (NOthing,NOthing) --use inverseCT :: ConceptTerm -> ConceptTerm
