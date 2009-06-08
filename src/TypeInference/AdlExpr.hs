{-# OPTIONS_GHC -Wall #-}
module TypeInference.AdlExpr where
import Adl.MorphismAndDeclaration
import Adl.Concept
import Adl.Expression
import Adl.Rule
import CommonClasses

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
  (Relation mp i _)==(Relation mp' i' _) = (name mp)==(name mp') && i==i'
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
uniqueMphsE i (Tm mp@(Mph{mphyin=False})) = (Flip (Relation mp (i+1) unknowntype) unknowntype,i+1)
uniqueMphsE i (Tm mp) = (Relation mp (i+1) unknowntype,i+1)
uniqueMphsE _ (F []) = error $ "Error in AdlExpr.hs module TypeInference.AdlExpr function uniqueMphsE: " ++
                               "Expression has no sub expressions"++show (F [])++"." 
uniqueMphsE i (F (ex:rexs)) = (Semicolon (fst lft) (fst rght) unknowntype, snd rght)
   where
   lft = uniqueMphsE i ex
   rght = case rexs of
     rex:[] -> uniqueMphsE (snd lft) rex
     _:_    -> uniqueMphsE (snd lft) (F rexs)
     []     -> uniqueMphsE i ex
uniqueMphsE _ (Fd []) = error $ "Error in AdlExpr.hs module TypeInference.AdlExpr function uniqueMphsE: " ++
                               "Expression has no sub expressions"++show (Fd [])++"." 
uniqueMphsE i (Fd (ex:rexs)) = (Dagger (fst lft) (fst rght) unknowntype, snd rght)
   where
   lft = uniqueMphsE i ex
   rght = case rexs of
     rex:[] -> uniqueMphsE (snd lft) rex
     _:_    -> uniqueMphsE (snd lft) (Fd rexs)
     []     -> uniqueMphsE i ex
uniqueMphsE _ (Fi []) = error $ "Error in AdlExpr.hs module TypeInference.AdlExpr function uniqueMphsE: " ++
                               "Expression has no sub expressions"++show (Fi [])++"." 
uniqueMphsE i (Fi (ex:rexs)) = (Intersect (fst lft) (fst rght) unknowntype, snd rght)
   where
   lft = uniqueMphsE i ex
   rght = case rexs of
     rex:[] -> uniqueMphsE (snd lft) rex
     _:_    -> uniqueMphsE (snd lft) (Fi rexs)
     []     -> uniqueMphsE i ex
uniqueMphsE _ (Fu []) = error $ "Error in AdlExpr.hs module TypeInference.AdlExpr function uniqueMphsE: " ++
                               "Expression has no sub expressions"++show (Fu [])++"." 
uniqueMphsE i (Fu (ex:rexs)) = (Union (fst lft) (fst rght) unknowntype, snd rght)
   where
   lft = uniqueMphsE i ex
   rght = case rexs of
     rex:[] -> uniqueMphsE (snd lft) rex
     _:_    -> uniqueMphsE (snd lft) (Fu rexs)
     []     -> uniqueMphsE i ex
uniqueMphsE i (Cp ex) = (Complement (fst sb) unknowntype, snd sb)
   where
   sb = uniqueMphsE i ex
uniqueMphsE i (Tc ex) = uniqueMphsE i ex
uniqueMphsE i (K0 ex) = uniqueMphsE i ex
uniqueMphsE i (K1 ex) = uniqueMphsE i ex

fromRule :: Rule -> AdlExpr
fromRule (Ru{rrsrt=Implication,rrant=ex,rrcon=rex}) = Implicate (fst lft) (fst rght) unknowntype
   where
   lft = uniqueMphsE 0 ex
   rght = uniqueMphsE (snd lft) rex
fromRule (Ru{rrsrt=Equivalence,rrant=ex,rrcon=rex}) = Equality (fst lft) (fst rght) unknowntype
   where
   lft = uniqueMphsE 0 ex
   rght = uniqueMphsE (snd lft) rex
fromRule (Ru{rrsrt=Truth,rrcon=sb}) = fst (uniqueMphsE 0 sb)
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
instance Eq TypeTerm where
   (TT ct1 ct2)==(TT ct1' ct2') = ct1==ct1' && ct2==ct2'
--   _ == _ = False
unknowntype :: TypeTerm
unknowntype = TT (CT Anything) (CT Anything)

data ConceptTerm = CT Concept | CF (GenSpec, Concept, Concept) | CTake (GenSpec,Concepts) deriving (Show)
data GenSpec = Generic | Specific deriving (Show, Eq)
instance Eq ConceptTerm where
  (CT c)==(CT c') = c==c'
  (CF (f,c1,c2))==(CF (f',c1',c2')) = f==f' && c1==c1' && c2==c2'
  _ == _ = False 


