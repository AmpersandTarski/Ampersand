{-# OPTIONS_GHC -Wall #-}
module TypeInference.AdlExpr where
import Adl.MorphismAndDeclaration
import Adl.Concept
import Adl.Expression
import Adl.Rule
import CommonClasses

data AdlExpr =   Relation    {rel::Morphism, mphid::Int, homo::Bool, tt::TypeTerm}
               | Implicate   {left::AdlExpr, right::AdlExpr, tt::TypeTerm}
               | Equality    {left::AdlExpr, right::AdlExpr, tt::TypeTerm}
               | Complement  {sub::AdlExpr, tt::TypeTerm}
               | Flip        {sub::AdlExpr, tt::TypeTerm}
               | Union       {lst::[AdlExpr], tt::TypeTerm}
               | Intersect   {lst::[AdlExpr], tt::TypeTerm}
               | Semicolon   {left::AdlExpr, right::AdlExpr, tt::TypeTerm}
               | Dagger      {left::AdlExpr, right::AdlExpr, tt::TypeTerm}
               deriving (Show)

--REMARK -> Equality is NOT on the type to be able to correlate a typed expression to its untyped declaration
instance Eq AdlExpr where
  (Relation mp i _ _)==(Relation mp' i' _ _) = (name mp)==(name mp') && i==i'
  (Implicate expr1 expr2 _)==(Implicate expr1' expr2' _) = expr1==expr1' && expr2==expr2'
  (Equality expr1 expr2 _)==(Equality expr1' expr2' _) = expr1==expr1' && expr2==expr2'
  (Union exprs _)==(Union exprs' _) = foldr (&&) True $ [elem ex exprs'|ex<-exprs]++[elem ex exprs|ex<-exprs']
  (Intersect exprs _)==(Intersect exprs' _) = foldr (&&) True $ [elem ex exprs'|ex<-exprs]++[elem ex exprs|ex<-exprs']
  (Semicolon expr1 expr2 _)==(Semicolon expr1' expr2' _) = expr1==expr1' && expr2==expr2'
  (Dagger expr1 expr2 _)==(Dagger expr1' expr2' _) = expr1==expr1' && expr2==expr2'
  (Complement expr _)==(Complement expr' _) = expr==expr'
  (Flip expr _)==(Flip expr' _) = expr==expr'
  _==_ = False

instance Ord AdlExpr where
  x1<=x2 = amphid x1 <= amphid x2

amphid :: AdlExpr -> Int
amphid adlex = case adlex of
  Relation{} -> mphid adlex
  Implicate{} -> amphid $ left adlex 
  Equality{} -> amphid $ left adlex 
  Complement{} -> amphid $ sub adlex 
  Flip{} -> amphid $ sub adlex 
  Union{} -> if null (lst adlex) then -1 else amphid $ head $ lst adlex 
  Intersect{} -> if null (lst adlex) then -1 else amphid $ head $ lst adlex
  Semicolon{} -> amphid $ left adlex 
  Dagger{} -> amphid $ left adlex 

isCompl :: AdlExpr -> Bool
isCompl (Complement{}) = True
isCompl _ = False

--DESCR -> casts an Adl.Expression to an AdlExpr
fromExpression :: Expression -> AdlExpr
fromExpression expr = fst (uniqueMphsE 0 expr)
  
--REMARK -> there will never be a Flip, because it is parsed flippedwise. The Flip is still implemented for other parse trees than the current ADL parse tree.
uniqueMphsE :: Int -> Expression -> (AdlExpr,Int)
uniqueMphsE i (Tm mp@(Mph{mphyin=False})) = (Flip (Relation mp (i+1) False (fromSign $ sign mp)) unknowntype,i+1)
uniqueMphsE i (Tm mp) = (Relation mp (i+1) False (fromSign $ sign mp),i+1)
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
uniqueMphsE i (Fi (ex:rexs)) = case rexs of
     rex:[] -> let (mphrex,irex) = uniqueMphsE ilft rex
               in (Intersect [mphlft,mphrex] unknowntype, irex)
     _:_    -> (Intersect (mphlft:mphrexs) unknowntype, irexs)
     []     -> (Intersect [mphlft] unknowntype, ilft)
   where
   (mphlft,ilft) = uniqueMphsE i ex
   (Intersect mphrexs _, irexs) =uniqueMphsE ilft (Fi rexs)
uniqueMphsE _ (Fu []) = error $ "Error in AdlExpr.hs module TypeInference.AdlExpr function uniqueMphsE: " ++
                               "Expression has no sub expressions"++show (Fu [])++"." 
uniqueMphsE i (Fu (ex:rexs)) = case rexs of
     rex:[] -> let (mphrex,irex) = uniqueMphsE ilft rex
               in (Union [mphlft,mphrex] unknowntype, irex)
     _:_    -> (Union (mphlft:mphrexs) unknowntype, irexs)
     []     -> (Union [mphlft] unknowntype, ilft)
   where
   (mphlft,ilft) = uniqueMphsE i ex
   (Union mphrexs _, irexs) =uniqueMphsE ilft (Fu rexs)
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
ttsrc ct = cts ct
tttgt :: TypeTerm -> ConceptTerm
tttgt ct = ctt ct
     
data TypeTerm = TT {cts::ConceptTerm, ctt::ConceptTerm, expon::Int} deriving (Show)
instance Eq TypeTerm where
   (TT ct1 ct2 _)==(TT ct1' ct2' _) = ct1==ct1' && ct2==ct2'
--   _ == _ = False
unknowntype :: TypeTerm
unknowntype = TT (CT Anything) (CT Anything) 1

data ConceptTerm = CT {val::Concept} | CTAS {val::Concept, as::Concept} deriving (Show)
--DESCR -> a conceptterm equals another conceptterm if their values are equal
--The 'as' in CTAS will never be reassigned after being set from the declaration
instance Eq ConceptTerm where
  c==c' = val c==val c'

declaredcpt :: ConceptTerm -> Concept
declaredcpt ct = case ct of 
  CT{} -> val ct
  CTAS{} -> as ct

fromSign :: Sign -> TypeTerm
fromSign (x,y) = TT (CT x) (CT y) 1

reassignexpr :: (Maybe Concept,Maybe Concept) -> AdlExpr -> AdlExpr
reassignexpr (x,y) expr = case expr of
  Relation{} -> expr{tt=reassigntype (x,y) (tt expr)}
  Implicate{} -> expr{left=reassignexpr (x,y) (left expr)
                     ,right=reassignexpr (x,y) (right expr)
                     ,tt=reassigntype (x,y) (tt expr)}
  Equality{} -> expr{left=reassignexpr (x,y) (left expr)
                    ,right=reassignexpr (x,y) (right expr)
                    ,tt=reassigntype (x,y) (tt expr)}
  Complement{} -> expr{sub=reassignexpr (x,y) (sub expr) 
                      ,tt=reassigntype (x,y) (tt expr)}
  Flip{} -> expr{sub=reassignexpr (y,x) (sub expr)
                ,tt=reassigntype (x,y) (tt expr)}
  Union{} -> expr{lst=map (reassignexpr (x,y)) (lst expr)
                 ,tt=reassigntype (x,y) (tt expr)} 
  Intersect{} -> expr{lst=map (reassignexpr (x,y)) (lst expr)
                     ,tt=reassigntype (x,y) (tt expr)} 
  Semicolon{} -> expr{left=reassignexpr (x,Nothing) (left expr)
                     ,right=reassignexpr (Nothing,y) (right expr)
                     ,tt=reassigntype (x,y) (tt expr)} 
  Dagger{} -> expr{left=reassignexpr (x,Nothing) (left expr)
                  ,right=reassignexpr (Nothing,y) (right expr)
                  ,tt=reassigntype (x,y) (tt expr)} 

reassigntype :: (Maybe Concept,Maybe Concept) -> TypeTerm -> TypeTerm
reassigntype (Nothing,Nothing) tt1 = tt1
reassigntype (Just x,Just y) tt1 = tt1{cts=reassignval x (cts tt1),ctt=reassignval y (ctt tt1)}
reassigntype (Nothing,Just y) tt1 = tt1{ctt=reassignval y (ctt tt1)}
reassigntype (Just x,Nothing) tt1 = tt1{cts=reassignval x (cts tt1)}

reassignval :: Concept -> ConceptTerm -> ConceptTerm
reassignval c ct = ct{val=c}

inversetype :: TypeTerm -> TypeTerm
inversetype tt1 = tt1{expon= (-(expon tt1))}

