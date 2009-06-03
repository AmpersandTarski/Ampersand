{-# OPTIONS_GHC -Wall #-}
module TypeInference.ITree where
import Adl.Concept
import TypeInference.Statements
import TypeInference.AdlExpr

--DESCR -> Or I have a type, proofed by the fact that all alternatives resulting in a type, result in the same type.
--         Or I could not infer a type, proofed by the fact that all alternatives result in error(s).
--         Or I have an ambiguous type, proofed by the fact that some alternatives result in different types.
data InferredType = Type [ITree] | TypeErrors TypeErrorsType [ITree]  deriving (Show)
data TypeErrorsType = NoType | AmbiguousType deriving (Show)

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
           | SpecRule ITree ITree deriving (Show)

data BindType = Bind | BindG1 | BindG2 | BindGG | BindS1 | BindS2 | BindSS | BindSG | BindGS deriving (Show) 

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
stmts (SpecRule tr1 tr2) = (stmts tr1) ++ (stmts tr2)
