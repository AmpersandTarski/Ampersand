{-# OPTIONS_GHC -Wall #-}
module TypeInference.ITree where
import Adl.Concept
import TypeInference.Statements
import TypeInference.AdlExpr

--DESCR -> Or I have a type, proofed by the fact that all alternatives resulting in a type, result in the same type.
--         Or I could not infer a type, proofed by the fact that all alternatives result in error(s).
--         Or I have an ambiguous type, proofed by the fact that some alternatives result in different types.
data Proof = Proven [ITree] | NoProof TypeErrorsType [ITree]  deriving (Show)
data TypeErrorsType = NoType | AmbiguousType deriving (Show)

instance Association Proof where
  source (Proven [] ) = Anything
  source (Proven inftrees ) = source $ evalstmt $ evaltree (head inftrees,False)
  source (NoProof _ inftrees ) = NOthing
  target (Proven [] ) = Anything
  target (Proven inftrees ) = target $ evalstmt $ evaltree (head inftrees,False)
  target (NoProof _ inftrees ) = NOthing

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
evaltree (DisjRule tr1 tr2,inv) = (BoundTo $ Intersect lex rex $ TT c1 c2,inv)
   where
   (BoundTo lex,_) = evaltree (tr1,inv)
   (BoundTo rex,_) = evaltree (tr2,inv)
   c1 = if (exprsrc lex)==(exprsrc rex) 
        then exprsrc lex
        else CT NOthing
   c2 = if (exprtgt lex)==(exprtgt rex) 
        then exprtgt lex
        else CT NOthing
evaltree (UnionRule tr1 tr2,inv) = (BoundTo $ Union lex rex $ TT c1 c2,inv)
   where
   (BoundTo lex,_) = evaltree (tr1,inv)
   (BoundTo rex,_) = evaltree (tr2,inv)
   c1 = if (exprsrc lex)==(exprsrc rex) 
        then exprsrc lex
        else CT NOthing
   c2 = if (exprtgt lex)==(exprtgt rex) 
        then exprtgt lex
        else CT NOthing
evaltree (RelcompRule tr1 tr2,inv) = (BoundTo $ Semicolon 
                                        (lex {tt= TT c1 (CTake (Specific,[c2,c3,c4]))})
                                        (rex {tt= TT (CTake (Specific,[c2,c3,c4])) c5})
                                        $ TT c1 c5
                                      ,inv) 
   where
   (BoundTo lex,_) = evaltree (tr1,inv)
   (BoundTo rex,_) = evaltree (tr2,inv)
   c1 = exprsrc lex
   c2 = if c2l==c2r then c2l else NOthing
   CF (Specific,c2l,c3) = exprtgt lex
   CF (Specific,c2r,c4) = exprsrc rex
   c5 = exprtgt rex
evaltree (AddcompRule tr1 tr2,inv) = (BoundTo $ Dagger 
                                        (lex {tt= TT c1 (CTake (Generic,[c2,c3,c4]))})
                                        (rex {tt= TT (CTake (Generic,[c2,c3,c4])) c5})
                                        $ TT c1 c5
                                      ,inv) 
   where
   (BoundTo lex,_) = evaltree (tr1,inv)
   (BoundTo rex,_) = evaltree (tr2,inv)
   c1 = exprsrc lex
   c2 = if c2l==c2r then c2l else NOthing
   CF (Generic,c2l,c3) = exprtgt lex
   CF (Generic,c2r,c4) = exprsrc rex
   c5 = exprtgt rex
evaltree (ImplyRule tr1 tr2,inv) =  (BoundTo $ Implicate lex rex $ TT c1 c2,inv) 
   where
   (BoundTo lex,_) = evaltree (tr1,inv)
   (BoundTo rex,_) = evaltree (tr2,inv)
   c1 = if (exprsrc lex)==(exprsrc rex) 
        then exprsrc lex
        else CT NOthing
   c2 = if (exprtgt lex)==(exprtgt rex) 
        then exprtgt lex
        else CT NOthing
evaltree (EqualRule tr1 tr2,inv) = (BoundTo $ Equality lex rex $ TT c1 c2,inv) 
   where
   (BoundTo lex,_) = evaltree (tr1,inv)
   (BoundTo rex,_) = evaltree (tr2,inv)
   c1 = if (exprsrc lex)==(exprsrc rex) 
        then exprsrc lex
        else CT NOthing
   c2 = if (exprtgt lex)==(exprtgt rex) 
        then exprtgt lex
        else CT NOthing
evaltree (FlipRule tr,inv) = (BoundTo $ Flip sub $ TT c2 c1,inv) 
   where
   (BoundTo sub,_) = evaltree (tr,inv)
   c1 = exprsrc sub
   c2 = exprtgt sub
evaltree (ComplRule tr,inv) = (BoundTo $ Complement sub $ TT c1 c2,not inv) 
   where
   (BoundTo sub,_) = evaltree (tr,not inv)
   c1 = exprsrc sub
   c2 = exprtgt sub
evaltree (BindRule bt tr,inv) = (BoundTo (sub{tt=boundtype}),inv)  
   where
   (TypeOf sub,_) = evaltree (tr,inv)
   CF (Generic,c1,c2) = exprsrc sub
   CF (Generic,c3,c4) = exprtgt sub
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
evaltree (SpecRule st tr1 tr2,inv) = (TypeOf (sub{tt=boundtype}),inv) 
   where
   (IsaStat c3 c1,_) = evaltree (tr1,inv)
   (TypeOf sub,_) = evaltree (tr2,inv)
   CF (Generic,lc1,lc2) = exprsrc sub
   CF (Generic,rc1,rc2) = exprtgt sub
   boundtype = case st of
     SpecDomain -> if c1==lc1 then TT (CF (Generic,c3,lc2)) (exprtgt sub) else TT (CT NOthing) (CT NOthing)
     SpecRange ->  if c1==rc1 then TT (exprsrc sub) (CF (Generic,c3,rc2)) else TT (CT NOthing) (CT NOthing)

evalstmt :: (Statement, Inverse) -> Sign
evalstmt (BoundTo expr,inv) = if inv then (evalCT $ inverseCT c1, evalCT $ inverseCT c2) else (evalCT c1,evalCT c2) 
  where
  c1=exprsrc expr
  c2=exprtgt expr
evalstmt _ = (NOthing,NOthing) --use inverseCT :: ConceptTerm -> ConceptTerm
