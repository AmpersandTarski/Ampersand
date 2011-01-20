{-# OPTIONS_GHC -Wall #-}
module TypeInference.Isa (isaRels) where
import Ampersand  
import qualified Data.Set as Set

---------------------------------------------------------------------------------------------

--REMARK -> Can not use data Cpt as a in RelSet a, because the  implementation of
--          instance Ord Cpt is not suitable. Ord is needed by a lot of Data.Set functions
--REMARK -> Cpt is introduced to implement a different instance of Ord
data Cpt = Cpt String | AllCpt | NoCpt | StonCpt
type Cpts = Set.Set Cpt

instance Show Cpt where
  showsPrec _ (Cpt a) = showString a
  showsPrec _ AllCpt = showString "Anything"
  showsPrec _ NoCpt = showString "Nothing"
  showsPrec _ StonCpt = showString "Singleton"

instance Eq Cpt where
  Cpt a == Cpt b = a==b
  AllCpt == AllCpt = True
  NoCpt == NoCpt = True
  StonCpt == StonCpt = True
  _ == _ = False

instance Ord Cpt where
  Cpt a <= Cpt b = a <= b
  AllCpt <= _ = True
  _ <= AllCpt = False
  NoCpt <= StonCpt = True
  NoCpt <= _  = False
  StonCpt <= _ = False
  _ <= NoCpt  = True
  _ <= StonCpt = True  

fromConcept :: Concept -> Cpt
fromConcept (C {cptnm = nm}) = Cpt nm
fromConcept Anything = AllCpt
fromConcept NOthing = NoCpt
fromConcept S = StonCpt
   
toConcept :: Cpt -> Concept
toConcept (Cpt nm) = cptnew nm
toConcept AllCpt = Anything
toConcept NoCpt = NOthing
toConcept StonCpt = S

---------------------------------------------------------------------------------------------
--data RelSet a = RelSet [(a,a)] deriving (Show)
type RelSet a = Set.Set (a,a)

isaRels :: [Concept] -> Gens Concept -> [(Concept,Concept)]
isaRels cs gs = if null checkrels
                then [(toConcept spc, toConcept gen)|(spc,gen)<-rs]
                else error ( "!Fatal (module TypeInference.Isa 56): "++show ["Concept "++show c1++" cannot be the specific of both "++show c2++" and "++show c3
                       ++ " if "++show c2++" and "++show c3 ++ 
                       " are not camparable. Please specify by means of GEN .. ISA .."
                       |(c1,c2,c3)<-checkrels])
  where
  rs = Set.toList $ isaRelSet (Set.fromList $ (map fromConcept cs))
  checkrels :: [(Cpt,Cpt,Cpt)]
  checkrels = [(c1,c2,c3) | (c1,c2)<-rs,(c1',c3)<-rs,c1==c1',not (elem (c2,c3) rs || elem (c3,c2) rs)
                                     ,not(c1==NoCpt),not(c2==AllCpt),not(c3==AllCpt),c2/=c3]
  --DESCR -> if is in isaRel then predicate isa is true. reflects axiom 15-19
  --         reflexive transitive closure (R0 \/ transclose) of the declared GEN relations
  --         including that every concept has a top (NoCpt) and bottom (AllCpt)
  --REMARK -> AllCpt and NoCpt must not be in Cpts
  --          "Ampersand is restricted to concepts that are not bottom or top, but the two are needed to signal type errors"
  isaRelSet :: Cpts -> RelSet Cpt
  isaRelSet cpts = 
     foldr Set.union (Set.empty)
        [Set.fromList [(NoCpt,a) | a<-Set.toList cpts      ],
         Set.fromList [(b,AllCpt) |  b<-Set.toList cpts     ],
         Set.fromList [(a,a) | a<-(NoCpt:AllCpt:Set.toList cpts)],
         transitiveclosure_w (Set.toList cpts) gens2rels]
         where
         gens2rels = Set.fromList [(fromConcept (genspc g), fromConcept (gengen g)) | g<-gs]

{-
Hi Stef,
 
I now have a function transitiveclosure_w that replaces transitiveclosure.
It makes quite a difference for longer paths and more concepts.

I have tested (one run on a bad laptop with arguments [a] = [1..n] and  RelSet a = [(x,x+1) | x<-[1..n]]

Gerard
-------------------
n         old (sec)       _w(sec)
10       0.06             0.02
20       0.95             0.13
30       6.02             0.45                
40       24.5             0.97                
50       73.6             2.11                
100     2387            34.8                   
200        -                615            
-}
--DESCR -> duplicated from clos1.Auxiliaries.hs only [a] is provided instead of
--         computed from range(RelSet a) /\ domain(RelSet a)
--         [a] contains all possible intermediates on the path
--REMARK -> if for [a] the universe is provided this will be less efficient within this function
--          then providing the most precise subset range(RelSet a) /\ domain(RelSet a). 
--          However computing range(RelSet a) /\ domain(RelSet a) comes at a cost
--          just like computing universe. The choice is left to the user of this function.
--          p.e. the typechecker has already computed the universe for other purposes
--TODO -> We could calculate the cost of providing unnecessary large [a] lists
transitiveclosure_w :: Ord a => [a] -> RelSet a -> RelSet a
transitiveclosure_w [] r     = r
transitiveclosure_w (x:xs) r = transitiveclosure_w xs $ r `Set.union` (Set.fromList [(a,b')|(a,b)<-(Set.toList r),b==x,(a',b')<-(Set.toList r),a'==x])

{-
   --DESCR -> R+ = R \/ R^2 \/ R^3 \/ ...
   --REMARK -> transitiveclosure must be evaluated completely. We don't know the number of loops up front
   --          we know that the function is evaluated if
   transitiveclosure :: Eq a => [a] -> RelSet a -> RelSet a
   transitiveclosure universe r = geteval $ allCumUnion universe r
            where
            --DESCR  -> if a cumUnion n matches a cumUnion n+1 then cumUnion n and higher are
            --          all equal and the transitiveclosure
            --REMARK -> transitiveclosure must get a matching set1 and set2 at some point
            --TODO -> I could implement == for RelSet
            geteval (set1:set2:sets) | stop set1 set2 = set1
                                     | otherwise  = geteval (set2:sets)
            stop (RelSet set1) (RelSet set2) = foldr (&&) True ([elem s2 set1 | s2<-set2]++[elem s1 set2 | s1<-set1])
-}
