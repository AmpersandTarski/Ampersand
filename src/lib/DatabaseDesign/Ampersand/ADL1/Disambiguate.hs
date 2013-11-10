{-# OPTIONS_GHC -Wall -XFlexibleInstances -XDataKinds #-}
{-# LANGUAGE RelaxedPolyRec #-}
module DatabaseDesign.Ampersand.ADL1.Disambiguate(disambiguate, gc, DisambPrim(..), findConceptOrONE, findConcept, pCpt2aCpt) where
import DatabaseDesign.Ampersand.Core.ParseTree
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree hiding (sortWith, maxima, greatest)
import DatabaseDesign.Ampersand.Basics (Identified(name), fatalMsg)
import Prelude hiding (head, sequence, mapM)
import Control.Applicative
import Data.Traversable
import qualified Data.Set as Set

disambiguate :: (Traversable d, Disambiguatable d) =>
                (a -> (TermPrim, DisambPrim)) -- disambiguation function
                -> d a -- object to be disambiguated
                -> d (TermPrim, DisambPrim) -- disambiguated object
disambiguate termPrimDisAmb x = fixpoint disambiguationStep (Change (fmap termPrimDisAmb x) False)

fatal :: Int -> String -> a
fatal = fatalMsg "ADL1.Disambiguate"

findConceptOrONE :: String -> A_Concept
findConceptOrONE "ONE" = ONE
findConceptOrONE x = findConcept x

findConcept :: String -> A_Concept
-- SJC: ONE should be tokenized, so it cannot occur as a string
-- especially because we require that concepts are identifiable by their name
-- hence if this line would change the semantics, we have either
-- (1) made a programming error in the call of findConcept (in which case you should call findConceptOrONE instead)
-- (2) made an error in the tokenizer/parser
findConcept "ONE" = fatal 200 "ONE is not a valid name for a concept"
findConcept x = PlainConcept 
            {cptnm = x
            ,cpttp = fatal 588 "Types of concepts are not defined here"
            ,cptdf = fatal 589 "df of concepts are not defined here"
            }

class Disambiguatable d where
  disambInfo :: d (TermPrim,DisambPrim)
   -> ( [(DisambPrim,SrcOrTgt)], [(DisambPrim,SrcOrTgt)] ) -- the inferred types (from the environment = top down)
   -> ( d ((TermPrim,DisambPrim), ([(DisambPrim,SrcOrTgt)],[(DisambPrim,SrcOrTgt)])) -- only the environment for the term (top down)
      , ( [(DisambPrim,SrcOrTgt)], [(DisambPrim,SrcOrTgt)] ) -- the inferred type, bottom up (not including the environment, that is: not using the second argument: prevent loops!)
      )

instance Disambiguatable P_Rule where
  disambInfo (P_Ru nm expr fps mean msg Nothing) x
   = (P_Ru nm exp' fps mean msg Nothing, rt)
   where (exp',rt) = disambInfo expr x
  disambInfo (P_Ru nm expr fps mean msg (Just viol)) x
   = (P_Ru nm exp' fps mean msg (Just viol'), rt)
   where (exp',rt) = disambInfo expr x
         (PairViewTerm viol',_)
          = (disambInfo (PairViewTerm viol) rt)
instance Disambiguatable PairViewTerm where
  disambInfo (PairViewTerm (PairView lst)) x
   = (PairViewTerm (PairView [pv' | pv <- lst, let (PairViewSegmentTerm pv',_) = disambInfo (PairViewSegmentTerm pv) x])
     , ([],[])) -- unrelated
instance Disambiguatable PairViewSegmentTerm where
  disambInfo (PairViewSegmentTerm (PairViewText s)) _ = (PairViewSegmentTerm (PairViewText s), ([],[]))
  disambInfo (PairViewSegmentTerm (PairViewExp st a)) (sr,tg) = (PairViewSegmentTerm (PairViewExp st res), rt)
    where t = case st of
               Src -> sr
               Tgt -> tg
          (res,rt) = disambInfo a (t,[])
instance Disambiguatable P_ViewD where
  disambInfo (P_Vd { vd_pos = o
                   , vd_lbl = s
                   , vd_cpt = c
                   , vd_ats = a
                   }) _ = (P_Vd o s c (map (\x -> fst (disambInfo x (c',[]))) a), (c',[]))
   where c' = [(Known (EDcI (Sign (pCpt2aCpt c) (pCpt2aCpt c))),Src)]

instance Disambiguatable P_ViewSegmt where
  disambInfo (P_ViewText a) _ = (P_ViewText a,([],[]))
  disambInfo (P_ViewHtml a) _ = (P_ViewHtml a,([],[]))
  disambInfo (P_ViewExp a) i = (P_ViewExp a',r)
    where (a',r) = disambInfo a i

instance Disambiguatable P_SubIfc where
  disambInfo (P_InterfaceRef a b) _   = (P_InterfaceRef a b,([],[]))
  disambInfo (P_Box o []   ) _        = (P_Box o [],([],[]))
  disambInfo (P_Box o (a:lst)) (x,_)  = (P_Box o (a':lst'),(r++nxt,[]))
   where (a', (r,_))            = disambInfo a (nxt++x,[])
         (P_Box _ lst',(nxt,_)) = disambInfo (P_Box o lst) (x++r,[])

instance Disambiguatable P_ObjDef where
  disambInfo (P_Obj a b c -- term/expression
                        d -- (potential) subobject
                        f)
                        (r,_) -- from the environment, only the source is important
   = (P_Obj a b c' d' f, (r0,[]) -- only source information should be relevant
     )
    where
     (d', (r1,_))
      = case d of
           Nothing -> (Nothing,([],[]))
           Just si -> (\(x,y)->(Just x,y)) $ disambInfo si (r2,[])
     (c', (r0,r2))
      = disambInfo c (r,r1)
instance Disambiguatable Term where
  disambInfo (PFlp o a  ) (ia1,ib1) = ( PFlp o a', (ib2,ia2) )
   where (a', (ia2,ib2)) = disambInfo a (ib1, ia1)
  disambInfo (PCpl o a  ) (ia1,ib1) = ( PCpl o a', (ia2,ib2) )
   where (a', (ia2,ib2)) = disambInfo a (ia1, ib1)
  disambInfo (PBrk o a  ) (ia1,ib1) = ( PBrk o a', (ia2,ib2) )
   where (a', (ia2,ib2)) = disambInfo a (ia1, ib1)
  disambInfo (PKl0 o a  ) (ia1,ib1) = ( PKl0 o a', (ia2++ib2,ia2++ib2) )
   where (a', (ia2,ib2)) = disambInfo a (ia1++ib1, ia1++ib1)
  disambInfo (PKl1 o a  ) (ia1,ib1) = ( PKl1 o a', (ia2++ib2,ia2++ib2) )
   where (a', (ia2,ib2)) = disambInfo a (ia1++ib1, ia1++ib1)
  disambInfo (Pequ o a b) (ia1,ib1) = ( Pequ o a' b', (ia2++ia3, ib2++ib3) )
   where (a', (ia2,ib2)) = disambInfo a (ia1++ia3, ib1++ib3)
         (b', (ia3,ib3)) = disambInfo b (ia1++ia2, ib1++ib2)
  disambInfo (Pimp o a b) (ia1,ib1) = ( Pimp o a' b', (ia2++ia3, ib2++ib3) )
   where (a', (ia2,ib2)) = disambInfo a (ia1++ia3, ib1++ib3)
         (b', (ia3,ib3)) = disambInfo b (ia1++ia2, ib1++ib2)
  disambInfo (PIsc o a b) (ia1,ib1) = ( PIsc o a' b', (ia2++ia3, ib2++ib3) )
   where (a', (ia2,ib2)) = disambInfo a (ia1++ia3, ib1++ib3)
         (b', (ia3,ib3)) = disambInfo b (ia1++ia2, ib1++ib2)
  disambInfo (PUni o a b) (ia1,ib1) = ( PUni o a' b', (ia2++ia3, ib2++ib3) )
   where (a', (ia2,ib2)) = disambInfo a (ia1++ia3, ib1++ib3)
         (b', (ia3,ib3)) = disambInfo b (ia1++ia2, ib1++ib2)
  disambInfo (PDif o a b) (ia1,ib1) = ( PDif o a' b', (ia2++ia3, ib2++ib3) )
   where (a', (ia2,ib2)) = disambInfo a (ia1++ia3, ib1++ib3)
         (b', (ia3,ib3)) = disambInfo b (ia1++ia2, ib1++ib2)
  disambInfo (PLrs o a b) (ia1,ib1) = ( PLrs o a' b', (ia, ib) )
   where (a', (ia,ic1)) = disambInfo a (ia1,ic2)
         (b', (ib,ic2)) = disambInfo b (ib1,ic1)
  disambInfo (PRrs o a b) (ia1,ib1) = ( PRrs o a' b', (ia, ib) )
   where (a', (ic1,ia)) = disambInfo a (ic2,ia1)
         (b', (ic2,ib)) = disambInfo b (ic1,ib1)
  disambInfo (PCps o a b) (ia1,ib1) = ( PCps o a' b', (ia, ib) )
   where (a', (ia,ic1)) = disambInfo a (ia1,ic2)
         (b', (ic2,ib)) = disambInfo b (ic1,ib1)
  disambInfo (PRad o a b) (ia1,ib1) = ( PRad o a' b', (ia, ib) )
   where (a', (ia,ic1)) = disambInfo a (ia1,ic2)
         (b', (ic2,ib)) = disambInfo b (ic1,ib1)
  disambInfo (PPrd o a b) (ia1,ib1) = ( PPrd o a' b', (ia, ib) )
   where (a', (ia,ic1)) = disambInfo a (ia1,ic2)
         (b', (ic2,ib)) = disambInfo b (ic1,ib1)
  disambInfo (Prim (a,b)) st = (Prim ((a,b), st), ([(b,Src)], [(b,Tgt)]) )

data DisambPrim
 = Rel [Expression]
 | Ident
 | Vee
 | Mp1 String
 | Known Expression

-- get concept:
gc :: Association expr => SrcOrTgt -> expr -> String
gc Src e = (name (source e))
gc Tgt e = (name (target e))

disambiguationStep :: (Disambiguatable d, Traversable d) => d (TermPrim, DisambPrim) -> Change (d (TermPrim, DisambPrim))
disambiguationStep x = traverse performUpdate withInfo
 where (withInfo, _) = disambInfo x ([],[])

performUpdate :: ((t, DisambPrim),
                     ([(DisambPrim, SrcOrTgt)], [(DisambPrim, SrcOrTgt)]))
                     -> Change (t, DisambPrim)
performUpdate ((t,unkn), (srcs',tgts'))
 = case unkn of
     Known _ -> pure (t,unkn)
     Rel xs  -> determineBySize (\x -> if length x == length xs then pure (Rel xs) else impure (Rel x))
                                id $
                (findMatch' (mustBeSrc,mustBeTgt) xs `orWhenEmpty` findMatch' (mayBeSrc,mayBeTgt) xs)
                `orWhenEmpty` xs
     Ident   -> determineBySize' (\ _ -> pure unkn) (\a -> EDcI (Sign (findConceptOrONE a) (findConceptOrONE a)))
                  possibleConcs
     Mp1 s   -> determineBySize' (\ _ -> pure unkn) (\a -> EMp1 s (Sign (findConceptOrONE a) (findConceptOrONE a)))
                  possibleConcs
     Vee     -> determineBySize (\ _ -> pure unkn) (\(a,b) -> (EDcV (Sign (findConceptOrONE a) (findConceptOrONE b))))
                  [(a,b) | a<-Set.toList mustBeSrc, b<-Set.toList mustBeTgt]
 where
   possibleConcs = (mustBeSrc `isc` mustBeTgt) `orWhenEmptyS`
                   (mustBeSrc `uni` mustBeTgt) `orWhenEmptyS`
                   (mayBeSrc  `isc` mayBeTgt ) `orWhenEmptyS`
                   (mayBeSrc  `uni` mayBeTgt )
   findMatch' (a,b) = findMatch (Set.toList a,Set.toList b)
   findMatch ([],[]) _ = []
   findMatch ([],tgts) lst
    = [x | x<-lst, gc Tgt x `elem` tgts]
   findMatch (srcs,[]) lst
    = [x | x<-lst, gc Src x `elem` srcs]
   findMatch (srcs,tgts) lst
    = [x | x<-lst, gc Src x `elem` srcs, gc Tgt x `elem` tgts]
   mustBeSrc = mustBe srcs'
   mustBeTgt = mustBe tgts'
   mayBeSrc = mayBe srcs'
   mayBeTgt = mayBe tgts'
   mustBe xs = Set.fromList [gc sot x | (Known x, sot) <- xs]
   mayBe  xs = Set.fromList [gc sot x | (Rel x' , sot) <- xs, x<-x']
   orWhenEmptyS a b = if (Set.null a) then b else a
   orWhenEmpty a b = if (null a) then b else a
   determineBySize' err ok s = determineBySize err ok (Set.toList s)
   determineBySize _   ok [a] = impure (t,Known (ok a))
   determineBySize err _  lst = fmap ((,) t) (err lst)
   impure x = Change x False
   isc = Set.intersection
   uni = Set.union

pCpt2aCpt :: P_Concept -> A_Concept
pCpt2aCpt pc
    = case pc of
        PCpt{} -> findConcept (p_cptnm pc)
        P_Singleton -> ONE   

data Change a = Change a Bool
instance Functor Change where
 fmap f (Change a b) = Change (f a) b
instance Applicative Change where
 (<*>) (Change f b) (Change a b2) = Change (f a) (b && b2)
 pure a = Change a True

fixpoint :: (a -> Change a) -- function for computing a fixpoint
         -> (Change a) -- has the fixpoint been reached?
         -> a
fixpoint _ (Change a True)  = a
fixpoint f (Change a False) = fixpoint f (f a)
