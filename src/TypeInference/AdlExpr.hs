{-# OPTIONS_GHC -Wall #-}
module TypeInference.AdlExpr where
import Adl.MorphismAndDeclaration
import Adl.FilePos
import Adl.Concept

import CommonClasses --for ghci only

--TODO -> temporary
type Expression = String
newmph nm = Mph nm Nowhere [] (Anything,Anything) True (Vs Anything Anything)
newmphatt nm c1 c2 = Mph nm Nowhere [c1,c2] (Anything,Anything) True (Vs Anything Anything)
newdcl nm c1 c2 = Sgn nm c1 c2 [] "" "" "" [] "" Nowhere 0 False

data AdlExpr =   Relation    Morphism Int
               | Implicate   AdlExpr AdlExpr
               | Equality    AdlExpr AdlExpr
               | Complement  AdlExpr
               | Flip        AdlExpr
               | Union       AdlExpr AdlExpr
               | Intersect   AdlExpr AdlExpr
               | Semicolon   AdlExpr AdlExpr
               | Dagger      AdlExpr AdlExpr
               deriving (Show)
instance Eq AdlExpr where
  (Relation m i)==(Relation m' i') = (name m)==(name m') && i==i'
  (Implicate expr1 expr2)==(Implicate expr1' expr2') = expr1==expr1' && expr2==expr2'
  (Equality expr1 expr2)==(Equality expr1' expr2') = expr1==expr1' && expr2==expr2'
  (Union expr1 expr2)==(Union expr1' expr2') = expr1==expr1' && expr2==expr2'
  (Intersect expr1 expr2)==(Intersect expr1' expr2') = expr1==expr1' && expr2==expr2'
  (Semicolon expr1 expr2)==(Semicolon expr1' expr2') = expr1==expr1' && expr2==expr2'
  (Dagger expr1 expr2)==(Dagger expr1' expr2') = expr1==expr1' && expr2==expr2'
  (Complement expr)==(Complement expr') = expr==expr'
  (Flip expr)==(Flip expr') = expr==expr'
  _==_ = False

--DESCR -> casts an Adl.Expression to an AdlExpr
fromExpression :: Expression -> AdlExpr
fromExpression expr = fst (uniqueMphs 0 expr)
  where
  uniqueMphs :: Int -> Expression -> (AdlExpr,Int)
  uniqueMphs i "amb1" = (Relation (newmph "amb1") (i+1), i+1)
  uniqueMphs i "x" = (Relation (newmph "x") (i+1), i+1)
  uniqueMphs i "y" = (Relation (newmph "y") (i+1), i+1)
  uniqueMphs i "undecl" = (Relation (newmph "undecl") (i+1), i+1)
  uniqueMphs i "amb1[Camb2*Camb2]" = (Relation (newmphatt "amb1" (cptnew "Camb2") (cptnew "Camb2")) (i+1), i+1)
  uniqueMphs i "undecl[Camb2*Cx]" = (Relation (newmphatt "undecl" (cptnew "Camb2") (cptnew "Cx")) (i+1), i+1)
  uniqueMphs i "x;y" = (Semicolon (fst left) (fst right), snd right)
     where
     left = uniqueMphs i ("x")
     right = uniqueMphs (snd left) ("y")
  uniqueMphs i "x;x;y" = (Semicolon (fst left) (fst right), snd right)
     where
     left = uniqueMphs i ("x")
     right = uniqueMphs (snd left) ("x;y")
  uniqueMphs i "x/\\y" = (Intersect (fst left) (fst right), snd right)
     where
     left = uniqueMphs i ("x")
     right = uniqueMphs (snd left) ("y")
  uniqueMphs i "undecl;x" = (Semicolon (fst left) (fst right), snd right)
     where
     left = uniqueMphs i ("undecl")
     right = uniqueMphs (snd left) ("x")
  uniqueMphs i "x;undecl;x" = (Semicolon (fst left) (fst right), snd right)
     where
     left = uniqueMphs i ("x")
     right = uniqueMphs (snd left) ("undecl;x")
  uniqueMphs i "amb1[Camb2*Camb2];undecl[Camb2*Cx]" = (Semicolon (fst left) (fst right), snd right)
     where
     left = uniqueMphs i ("amb1[Camb2*Camb2]")
     right = uniqueMphs (snd left) ("undecl[Camb2*Cx]")
     
