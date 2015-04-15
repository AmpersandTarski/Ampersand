{-# OPTIONS_GHC -Wall #-}
module Database.Design.Ampersand.ADL1.Disambiguate(disambiguate, gc, gc', DisambPrim(..),pCpt2aCpt) where
import Database.Design.Ampersand.Core.ParseTree
import Database.Design.Ampersand.Core.AbstractSyntaxTree hiding (sortWith, maxima, greatest)
import Database.Design.Ampersand.Basics (Named(name), fatalMsg)
import Prelude hiding (head, sequence, mapM)
import Control.Applicative
import Data.Traversable
import qualified Data.Set as Set

disambiguate :: (Disambiguatable d) =>
                (TermPrim -> (TermPrim, DisambPrim)) -- disambiguation function
                -> d TermPrim -- object to be disambiguated
                -> d (TermPrim, DisambPrim) -- disambiguated object
disambiguate termPrimDisAmb x = fixpoint disambiguationStep (Change (fmap termPrimDisAmb x) False)

fatal :: Int -> String -> a
fatal = fatalMsg "ADL1.Disambiguate"

findConcept :: String -> A_Concept
-- SJC: ONE should be tokenized, so it cannot occur as a string
-- especially because we require that concepts are identifiable by their name
-- hence if this line would change the semantics, we have either
-- (1) made a programming error in the call of findConcept (in which case you should call findConceptOrONE instead)
-- (2) made an error in the tokenizer/parser
findConcept "ONE" = fatal 200 "ONE is not a valid name for a concept"
findConcept x = 
   PlainConcept { cptnm = x
                }

-- To make something Disambiguatable, do the following:
-- (1) Make sure the type of the Disambiguatable thing has a type variable.
--     Suppose "Thing" should become disambiguatable, then "Thing" has "TermPrim" inside somewhere.
--     Change "data Thing =" into "data ThingPolymorphic a =", and change occurences of "TermPrim" into "a".
--     We changed "Thing" into "ThingPolymorphic" so we can create a type instance for "Thing":
--     type Thing = ThingPolymorphic TermPrim
--     This makes sure that "Thing" is the exact same type before and after this change.
-- (2) Make ThingPolymorphic an instance of Traversable. The default "deriving Traversable" should be fine.
-- (3) Make ThingPolymorphic an instance of Disambiguatable. It is your responsibility to prevent loops here. The instance looks like this:
--     disambInfo (Thing1 x y z) td = (Thing1 x' y' z', (bottomUpSourceTypes,bottomUpTargetTypes))
--      where (x',resultingTypesForX) = disambInfo x' topDownTypesForX
--            (y',resultingTypesForY) = disambInfo y' topDownTypesForY
--            (z',resultingTypesForZ) = disambInfo z' topDownTypesForZ
--     The variables topDownTypesFor... may depend on td,
--     the variables bottomUpSourceTypes and bottomUpTargetTypes may depend on resultingTypesFor...
--     Closing the loop (at the top of the structure) is done in the function "disambiguationStep".
--     Note that disambInfo actually performs two separate functions in one go: one to go top down, the other to go bottom up.
--     The top-down function may use parts of the bottom-up function, but not the other way around.
--     A nice example to look at is PCps:
--         disambInfo (PCps o a b) (ia1,ib1) = ( PCps o a' b', (ia, ib) )
--          where (a', (ia,ic1)) = disambInfo a (ia1,ic2) -- here ic2 is top-down, so that is ok
--                (b', (ic2,ib)) = disambInfo b (ic1,ib1)

-- this is *only* used internally!
data D_Concept
 = MustBe A_Concept
 | MayBe  A_Concept

class Traversable d => Disambiguatable d where
  disambInfo :: d (TermPrim,DisambPrim)
   -> ( [D_Concept], [D_Concept] ) -- the inferred types (from the environment = top down)
   -> ( d ((TermPrim,DisambPrim), ([D_Concept], [D_Concept])) -- only the environment for the term (top down)
      , ( [D_Concept], [D_Concept] ) -- the inferred type, bottom up (not including the environment, that is: not using the second argument: prevent loops!)
      )

instance Disambiguatable P_IdentDf where
  disambInfo (P_Id o nm c []) _ = ( P_Id o nm c [], ([],[]))
  disambInfo (P_Id o nm c (a:lst)) _     = (P_Id o nm c (a':lst'), (r++nxt, []))
       where (a', (r,_))                 = disambInfo a ([MustBe (pCpt2aCpt c)], [])
             (P_Id _ _ _ lst', (nxt,_))  = disambInfo (P_Id o nm c lst) ([MustBe (pCpt2aCpt c)], [])
instance Disambiguatable P_IdentSegmnt where
  disambInfo (P_IdentExp v) x = (P_IdentExp v', rt)
     where (v',rt) = disambInfo v x
instance Disambiguatable P_Rule where
  disambInfo (P_Ru nm expr fps mean msg Nothing) x
   = (P_Ru nm exp' fps mean msg Nothing, rt)
   where (exp',rt) = disambInfo expr x
  disambInfo (P_Ru nm expr fps mean msg (Just viol)) x
   = (P_Ru nm exp' fps mean msg (Just viol'), rt)
   where (exp',rt) = disambInfo expr x
         (PairViewTerm viol',_) -- SJ 20131123: disambiguation does not depend on the contents of this pairview, but must come from outside...
          = (disambInfo (PairViewTerm viol) rt)
instance Disambiguatable PairViewTerm where
  disambInfo (PairViewTerm (PairView lst)) x
   = (PairViewTerm (PairView [pv' | pv <- lst, let (PairViewSegmentTerm pv',_) = disambInfo (PairViewSegmentTerm pv) x])
     , ([],[])) -- unrelated
instance Disambiguatable PairViewSegmentTerm where
  disambInfo (PairViewSegmentTerm (PairViewText orig s)) _ = (PairViewSegmentTerm (PairViewText orig s), ([],[]))
  disambInfo (PairViewSegmentTerm (PairViewExp orig st a)) (sr,tg) = (PairViewSegmentTerm (PairViewExp orig st res), rt)
    where t = case st of
               Src -> sr
               Tgt -> tg
          (res,rt) = disambInfo a (t,[])
instance Disambiguatable P_ViewD where
  disambInfo (P_Vd { vd_pos  = o
                   , vd_lbl  = s
                   , vd_cpt  = c
                   , vd_isDefault = d
                   , vd_ats  = a
                   , vd_html = h
                   }) _ = (P_Vd o s c d h (map (\x -> fst (disambInfo x (c',[]))) a), (c',[]))
   where c' = [MustBe (pCpt2aCpt c)]

instance Disambiguatable P_ViewSegmt where
  disambInfo (P_ViewText a) _ = (P_ViewText a,([],[]))
  disambInfo (P_ViewHtml a) _ = (P_ViewHtml a,([],[]))
  disambInfo (P_ViewExp a) i = (P_ViewExp a',r)
    where (a',r) = disambInfo a i

instance Disambiguatable P_SubIfc where
  disambInfo (P_InterfaceRef a b) _      = (P_InterfaceRef a b,([],[]))
  disambInfo (P_Box o cl []   ) _        = (P_Box o cl [],([],[]))
  disambInfo (P_Box o cl (a:lst)) (x,_)  = (P_Box o cl' (a':lst'),(r++nxt,[]))
   where (a', (r,_))                = disambInfo a (nxt++x,[])
         (P_Box _ cl' lst',(nxt,_)) = disambInfo (P_Box o cl lst) (x++r,[])

instance Disambiguatable P_ObjDef where
  disambInfo (P_Obj a b c -- term/expression
                        v
                        d -- (potential) subobject
                        f)
                        (r,_) -- from the environment, only the source is important
   = (P_Obj a b c' v d' f, (r0,[]) -- only source information should be relevant
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
  disambInfo (PEqu o a b) (ia1,ib1) = ( PEqu o a' b', (ia2++ia3, ib2++ib3) )
   where (a', (ia2,ib2)) = disambInfo a (ia1++ia3, ib1++ib3)
         (b', (ia3,ib3)) = disambInfo b (ia1++ia2, ib1++ib2)
  disambInfo (PImp o a b) (ia1,ib1) = ( PImp o a' b', (ia2++ia3, ib2++ib3) )
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
  disambInfo (PDia o a b) (ia1,ib1) = ( PDia o a' b', (ia, ib) )
   where (a', (ia,ic1)) = disambInfo a (ia1,ic2)
         (b', (ib,ic2)) = disambInfo b (ib1,ic1)
  disambInfo (PCps o a b) (ia1,ib1) = ( PCps o a' b', (ia, ib) )
   where (a', (ia,ic1)) = disambInfo a (ia1,ic2)
         (b', (ic2,ib)) = disambInfo b (ic1,ib1)
  disambInfo (PRad o a b) (ia1,ib1) = ( PRad o a' b', (ia, ib) )
   where (a', (ia,ic1)) = disambInfo a (ia1,ic2)
         (b', (ic2,ib)) = disambInfo b (ic1,ib1)
  disambInfo (PPrd o a b) (ia1,ib1) = ( PPrd o a' b', (ia, ib) )
   where (a', (ia,ic1)) = disambInfo a (ia1,ic2)
         (b', (ic2,ib)) = disambInfo b (ic1,ib1)
  disambInfo (Prim (a,b)) st = (Prim ((a,b), st), (getConcepts Src b, getConcepts Tgt b) )

getConcepts :: SrcOrTgt -> DisambPrim -> [D_Concept]
getConcepts sot (Rel lst) = map (MayBe . gc' sot) lst
getConcepts sot (Known e) = [MustBe (gc' sot e)]
getConcepts _ _ = []

data DisambPrim
 = Rel [Expression] -- It is an expression, we don't know which, but it's going to be one of these (usually this is a list of relations)
 | Ident -- identity, and we know nothing about its type
 | Vee -- vee, type unknown
 | Mp1 String -- an atom, type unknown
 | Known Expression -- It is an expression, and we know exactly which. That is: disambiguation was succesful here
 deriving Show  -- Here, deriving Show serves debugging purposes only.

gc :: Association expr => SrcOrTgt -> expr -> String
gc sot = name . gc' sot

-- get concept:
gc' :: Association expr => SrcOrTgt -> expr -> A_Concept
gc' Src e = source e
gc' Tgt e = target e

disambiguationStep :: (Disambiguatable d, Traversable d) => d (TermPrim, DisambPrim) -> Change (d (TermPrim, DisambPrim))
disambiguationStep x = traverse performUpdate withInfo
 where (withInfo, _) = disambInfo x ([],[])

performUpdate :: ((t, DisambPrim),
                     ([D_Concept], [D_Concept]))
                     -> Change (t, DisambPrim)
performUpdate ((t,unkn), (srcs',tgts'))
 = case unkn of
     Known _ -> pure (t,unkn)
     Rel xs  -> determineBySize (\x -> if length x == length xs then pure (Rel xs) else impure (Rel x))
                ((findMatch' (mustBeSrc,mustBeTgt) xs `orWhenEmpty` findMatch' (mayBeSrc,mayBeTgt) xs)
                 `orWhenEmpty` xs)
     Ident   -> determineBySize suggest (map EDcI     (Set.toList possibleConcs))
     Mp1 s   -> determineBySize suggest (map (EMp1 s) (Set.toList possibleConcs))
     Vee     -> determineBySize (const (pure unkn))
                  [EDcV (Sign a b) | a<-Set.toList mustBeSrc, b<-Set.toList mustBeTgt]
 where
   suggest [] = pure unkn
   suggest lst = impure (Rel lst) -- TODO: find out whether it is equivalent to put "pure" here (which could be faster).
   possibleConcs = (mustBeSrc `isc` mustBeTgt) `orWhenEmptyS`
                   (mustBeSrc `uni` mustBeTgt) `orWhenEmptyS`
                   (mayBeSrc  `isc` mayBeTgt ) `orWhenEmptyS`
                   (mayBeSrc  `uni` mayBeTgt )
   findMatch' (a,b) = findMatch (Set.toList a,Set.toList b)
   findMatch ([],[]) _ = []
   findMatch ([],tgts) lst
    = [x | x<-lst, gc' Tgt x `elem` tgts]
   findMatch (srcs,[]) lst
    = [x | x<-lst, gc' Src x `elem` srcs]
   findMatch (srcs,tgts) lst
    = [x | x<-lst, gc' Src x `elem` srcs, gc' Tgt x `elem` tgts]
   mustBeSrc = mustBe srcs'
   mustBeTgt = mustBe tgts'
   mayBeSrc = mayBe srcs'
   mayBeTgt = mayBe tgts'
   mustBe xs = Set.fromList [x | (MustBe x) <- xs]
   mayBe  xs = Set.fromList [x | (MayBe x) <- xs]
   orWhenEmptyS a b = if (Set.null a) then b else a
   orWhenEmpty a b = if (null a) then b else a
   determineBySize _   [a] = impure (t,Known a)
   determineBySize err lst = fmap ((,) t) (err lst)
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
