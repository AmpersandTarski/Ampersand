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

data AdlExpr =   Relation    {rel::Morphism, id::Int, tt::TypeTerm}
               | Implicate   {left::AdlExpr, right::AdlExpr, tt::TypeTerm}
               | Equality    {left::AdlExpr, right::AdlExpr, tt::TypeTerm}
               | Complement  {sub::AdlExpr, tt::TypeTerm}
               | Flip        {sub::AdlExpr, tt::TypeTerm}
               | Union       {left::AdlExpr, right::AdlExpr, tt::TypeTerm}
               | Intersect   {left::AdlExpr, right::AdlExpr, tt::TypeTerm}
               | Semicolon   {left::AdlExpr, right::AdlExpr, tt::TypeTerm}
               | Dagger      {left::AdlExpr, right::AdlExpr, tt::TypeTerm}
               deriving (Show)
instance Eq AdlExpr where
  (Relation m i _)==(Relation m' i' _) = (name m)==(name m') && i==i'
  (Implicate expr1 expr2 _)==(Implicate expr1' expr2' _) = expr1==expr1' && expr2==expr2'
  (Equality expr1 expr2 _)==(Equality expr1' expr2' _) = expr1==expr1' && expr2==expr2'
  (Union expr1 expr2 _)==(Union expr1' expr2' _) = expr1==expr1' && expr2==expr2'
  (Intersect expr1 expr2 _)==(Intersect expr1' expr2' _) = expr1==expr1' && expr2==expr2'
  (Semicolon expr1 expr2 _)==(Semicolon expr1' expr2' _) = expr1==expr1' && expr2==expr2'
  (Dagger expr1 expr2 _)==(Dagger expr1' expr2' _) = expr1==expr1' && expr2==expr2'
  (Complement expr _)==(Complement expr' _) = expr==expr'
  (Flip expr _)==(Flip expr' _) = expr==expr'
  _==_ = False

--DESCR -> casts an Adl.Expression to an AdlExpr
fromExpression :: Expression -> AdlExpr
fromExpression expr = fst (uniqueMphs 0 expr)
  where
  uniqueMphs :: Int -> Expression -> (AdlExpr,Int)
  uniqueMphs i "amb1" = (Relation (newmph "amb1") (i+1) unknowntype, i+1)
  uniqueMphs i "x" = (Relation (newmph "x") (i+1) unknowntype, i+1)
  uniqueMphs i "y" = (Relation (newmph "y") (i+1) unknowntype, i+1)
  uniqueMphs i "undecl" = (Relation (newmph "undecl") (i+1) unknowntype, i+1)
  uniqueMphs i "amb1[Camb2*Camb2]" = (Relation (newmphatt "amb1" (cptnew "Camb2") (cptnew "Camb2")) (i+1) unknowntype, i+1)
  uniqueMphs i "undecl[Camb2*Cx]" = (Relation (newmphatt "undecl" (cptnew "Camb2") (cptnew "Cx")) (i+1) unknowntype, i+1)
  uniqueMphs i "x;y" = (Semicolon (fst left) (fst right) unknowntype, snd right)
     where
     left = uniqueMphs i ("x")
     right = uniqueMphs (snd left) ("y")
  uniqueMphs i "x;x;y" = (Semicolon (fst left) (fst right) unknowntype, snd right)
     where
     left = uniqueMphs i ("x")
     right = uniqueMphs (snd left) ("x;y")
  uniqueMphs i "x/\\y" = (Intersect (fst left) (fst right) unknowntype, snd right)
     where
     left = uniqueMphs i ("x")
     right = uniqueMphs (snd left) ("y")
  uniqueMphs i "y/\\x" = (Intersect (fst left) (fst right) unknowntype, snd right)
     where
     left = uniqueMphs i ("y")
     right = uniqueMphs (snd left) ("x")
  uniqueMphs i "undecl;x" = (Semicolon (fst left) (fst right) unknowntype, snd right)
     where
     left = uniqueMphs i ("undecl")
     right = uniqueMphs (snd left) ("x")
  uniqueMphs i "x;undecl;x" = (Semicolon (fst left) (fst right) unknowntype, snd right)
     where
     left = uniqueMphs i ("x")
     right = uniqueMphs (snd left) ("undecl;x")
  uniqueMphs i "amb1[Camb2*Camb2];undecl[Camb2*Cx]" = (Semicolon (fst left) (fst right) unknowntype, snd right)
     where
     left = uniqueMphs i ("amb1[Camb2*Camb2]")
     right = uniqueMphs (snd left) ("undecl[Camb2*Cx]")
  uniqueMphs i "-(x;x~);x" = (Semicolon (fst left) (fst right) unknowntype, snd right)
     where
     left = uniqueMphs i ("-(x;x~)")
     right = uniqueMphs (snd left) ("x")
  uniqueMphs i "x;x~" = (Semicolon (fst left) (fst right) unknowntype, snd right)
     where
     left = uniqueMphs i ("x")
     right = uniqueMphs (snd left) ("x~")
  uniqueMphs i "x~" = (Flip (fst sub) unknowntype, snd sub)
     where
     sub = uniqueMphs i ("x")
  uniqueMphs i "-(x;x~)" = (Complement (fst sub) unknowntype, snd sub)
     where
     sub = uniqueMphs i ("x;x~")

exprsrc :: AdlExpr -> ConceptTerm
exprsrc expr = ttsrc $ tt expr
exprtgt :: AdlExpr -> ConceptTerm
exprtgt expr = tttgt $ tt expr
ttsrc :: TypeTerm -> ConceptTerm
ttsrc (TT ct _) = ct
tttgt :: TypeTerm -> ConceptTerm
tttgt (TT _ ct) = ct
     
data TypeTerm = TT ConceptTerm ConceptTerm deriving (Show)
evalTT :: TypeTerm -> Sign
evalTT (TT ct1 ct2) = (evalCT ct1,evalCT ct2)
instance Eq TypeTerm where
   (TT ct1 ct2)==(TT ct1' ct2') = ct1==ct1' && ct2==ct2'
--   _ == _ = False
unknowntype = TT (CT Anything) (CT Anything)

data ConceptTerm = CT Concept | CF (GenSpec, Concept, Concept) | CTake (GenSpec, [Concept]) deriving (Show)
data GenSpec = Generic | Specific deriving (Show, Eq)
instance Eq ConceptTerm where
  (CT c)==(CT c') = c==c'
  (CF (f,c1,c2))==(CF (f',c1',c2')) = f==f' && c1==c1' && c2==c2'
  _ == _ = False 
evalCT :: ConceptTerm -> Concept
evalCT (CT c) = c
evalCT (CF (f,c1,c2)) 
  | f==Generic = c2
  | f==Specific = c1
evalCT (CTake (f, cs)) = error "not implemented evalCT CTake" 
inverseCT :: ConceptTerm -> ConceptTerm
inverseCT ct@(CT{}) = ct
inverseCT (CF (f,c1,c2)) 
  | f==Generic = CF (Specific,c1,c2)
  | f==Specific = CF (Generic,c1,c2)
inverseCT (CTake (f, cs)) 
  | f==Generic = CTake (Specific,cs)
  | f==Specific = CTake (Generic,cs)
