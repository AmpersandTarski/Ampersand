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
           | RelcompRule ITree ITree
           | BindRule ITree
           | SpecRule ITree ITree deriving (Show)

--DESCR -> Substitutes the first Concept argument by the second in the ITree
--USE -> if a tree contains concept variable "$C1" then I can bind the variable to "ACpt" by bindCptvar tree "$C1" "Acpt"
bindCptvar :: ITree -> Concept -> Concept -> ITree
bindCptvar stmt@(Stmt (BndStat expr (c1,c2))) var cpt
  | c1==var   = bindCptvar (Stmt $ BndStat expr (cpt,c2)) var cpt
  | c2==var   = Stmt $ BndStat expr (c1,cpt)
  | otherwise = stmt
bindCptvar stmt@(Stmt _) _ _ = stmt --other statements do not have concept vars
bindCptvar (DisjRule tr1 tr2) var cpt = DisjRule (bindCptvar tr1 var cpt) (bindCptvar tr2 var cpt)
bindCptvar (RelcompRule tr1 tr2) var cpt = RelcompRule (bindCptvar tr1 var cpt) (bindCptvar tr2 var cpt)
bindCptvar (BindRule tr) var cpt = BindRule (bindCptvar tr var cpt)
bindCptvar (SpecRule tr1 tr2) var cpt = SpecRule (bindCptvar tr1 var cpt) (bindCptvar tr2 var cpt)

--DESCR -> returns all the Stmt statements in a tree
stmts :: ITree -> [Statement]
stmts (Stmt stmt) = [stmt]
stmts (DisjRule tr1 tr2) = (stmts tr1) ++ (stmts tr2)
stmts (RelcompRule tr1 tr2) = (stmts tr1) ++ (stmts tr2)
stmts (BindRule tr) = stmts tr
stmts (SpecRule tr1 tr2) = (stmts tr1) ++ (stmts tr2)