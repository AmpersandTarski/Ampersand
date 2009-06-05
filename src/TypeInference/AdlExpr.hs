{-# OPTIONS_GHC -Wall #-}
module TypeInference.AdlExpr where
import Adl.MorphismAndDeclaration
import Adl.FilePos
import Adl.Concept
import Adl.Expression
import Adl.Rule

import CommonClasses --for ghci only

--TODO -> temporary
--type Expression = String
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

--REMARK -> Equality is NOT on the type to be able to correlate a typed expression to its untyped declaration
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
fromExpression expr = fst (uniqueMphsE 0 expr)
  
--REMARK -> there will never be a Flip, because it is parsed flippedwise. The Flip is still implemented for other parse trees than the current ADL parse tree.
uniqueMphsE :: Int -> Expression -> (AdlExpr,Int)
uniqueMphsE i (Tm m@(Mph{mphyin=False})) = (Flip (Relation m (i+1) unknowntype) unknowntype,i+1)
uniqueMphsE i (Tm m) = (Relation m (i+1) unknowntype,i+1)
uniqueMphsE i (F []) = error $ "Error in AdlExpr.hs module TypeInference.AdlExpr function uniqueMphsE: " ++
                               "Expression has no sub expressions"++show (F [])++"." 
uniqueMphsE i (F (ex:[])) = uniqueMphsE i ex
uniqueMphsE i (F (lex:rexs)) = (Semicolon (fst left) (fst right) unknowntype, snd right)
   where
   left = uniqueMphsE i lex
   right = case rexs of
     rex:[] -> uniqueMphsE (snd left) rex
     rex:rs -> uniqueMphsE (snd left) (F rexs)
uniqueMphsE i (Fd []) = error $ "Error in AdlExpr.hs module TypeInference.AdlExpr function uniqueMphsE: " ++
                               "Expression has no sub expressions"++show (Fd [])++"." 
uniqueMphsE i (Fd (ex:[])) = uniqueMphsE i ex
uniqueMphsE i (Fd (lex:rexs)) = (Dagger (fst left) (fst right) unknowntype, snd right)
   where
   left = uniqueMphsE i lex
   right = case rexs of
     rex:[] -> uniqueMphsE (snd left) rex
     rex:rs -> uniqueMphsE (snd left) (Fd rexs)
uniqueMphsE i (Fi []) = error $ "Error in AdlExpr.hs module TypeInference.AdlExpr function uniqueMphsE: " ++
                               "Expression has no sub expressions"++show (Fi [])++"." 
uniqueMphsE i (Fi (ex:[])) = uniqueMphsE i ex
uniqueMphsE i (Fi (lex:rexs)) = (Intersect (fst left) (fst right) unknowntype, snd right)
   where
   left = uniqueMphsE i lex
   right = case rexs of
     rex:[] -> uniqueMphsE (snd left) rex
     rex:rs -> uniqueMphsE (snd left) (Fi rexs)
uniqueMphsE i (Fu []) = error $ "Error in AdlExpr.hs module TypeInference.AdlExpr function uniqueMphsE: " ++
                               "Expression has no sub expressions"++show (Fu [])++"." 
uniqueMphsE i (Fu (ex:[])) = uniqueMphsE i ex
uniqueMphsE i (Fu (lex:rexs)) = (Union (fst left) (fst right) unknowntype, snd right)
   where
   left = uniqueMphsE i lex
   right = case rexs of
     rex:[] -> uniqueMphsE (snd left) rex
     rex:rs -> uniqueMphsE (snd left) (Fu rexs)
uniqueMphsE i (Cp ex) = (Complement (fst sub) unknowntype, snd sub)
   where
   sub = uniqueMphsE i ex
uniqueMphsE i (Tc ex) = uniqueMphsE i ex
uniqueMphsE i (K0 ex) = uniqueMphsE i ex
uniqueMphsE i (K1 ex) = uniqueMphsE i ex

fromRule :: Rule -> AdlExpr
fromRule (Ru{rrsrt=Implication,rrant=lex,rrcon=rex}) = Implicate (fst left) (fst right) unknowntype
   where
   left = uniqueMphsE 0 lex
   right = uniqueMphsE (snd left) rex
fromRule (Ru{rrsrt=Equivalence,rrant=lex,rrcon=rex}) = Equality (fst left) (fst right) unknowntype
   where
   left = uniqueMphsE 0 lex
   right = uniqueMphsE (snd left) rex
fromRule (Ru{rrsrt=Truth,rrcon=sub}) = fst (uniqueMphsE 0 sub)
fromRule (Sg{srsig=rule}) = fromRule rule
fromRule rule = error $ "Error in AdlExpr.hs module TypeInference.AdlExpr function fromRule: " ++
                        "Rule type has not been implemented."++show rule++"." 

 
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

data ConceptTerm = CT Concept | CF (GenSpec, Concept, Concept) | CTake (GenSpec,Concepts) deriving (Show)
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
evalCT (CTake x) = takec x
inverseCT :: ConceptTerm -> ConceptTerm
inverseCT ct@(CT{}) = ct
inverseCT (CF (f,c1,c2)) 
  | f==Generic = CF (Specific,c1,c2)
  | f==Specific = CF (Generic,c1,c2)
inverseCT (CTake (f,cs))
  | f==Generic = CTake (Specific,cs)
  | f==Specific = CTake (Generic,cs)

takec :: (GenSpec,Concepts) -> Concept
takec (Generic,cs) = Anything --TODO
takec (Specific,cs) = Anything --TODO

