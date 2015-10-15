{-# OPTIONS_GHC -Wall -Werror #-}
module Database.Design.Ampersand.ADL1.Disambiguate(disambiguate, orWhenEmpty, DisambPrim(..),pCpt2aCpt) where
import Database.Design.Ampersand.Core.ParseTree
import Database.Design.Ampersand.Core.AbstractSyntaxTree hiding (sortWith, maxima, greatest)
import Database.Design.Ampersand.Basics (fatalMsg)
import Control.Applicative
import Data.Traversable
import qualified Data.Set as Set

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


-- this is *only* used internally!
data D_Concept
 = MustBe A_Concept
 | MayBe  A_Concept 
 deriving (Show, Eq)

data Constraints = Cnstr {sourceConstraintsOf :: [D_Concept]
                         ,targetConstraintsOf :: [D_Concept]
                         }deriving Show

class Traversable d => Disambiguatable d where
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
  disambInfo :: d (TermPrim,DisambPrim)  --the thing that is disabmiguated
   -> Constraints -- the inferred types (from the environment = top down) 
   -> ( d ((TermPrim,DisambPrim), Constraints) -- only the environment for the term (top down)
      , Constraints -- the inferred type, bottom up (not including the environment, that is: not using the second argument: prevent loops!)
      )
  disambiguate :: 
                (TermPrim -> (TermPrim, DisambPrim)) -- disambiguation function
                -> d TermPrim -- object to be disambiguated
                -> d (TermPrim, DisambPrim) -- disambiguated object
  disambiguate termPrimDisAmb x = fixpoint disambiguationStep (Change (fmap termPrimDisAmb x) False)
    where
     fixpoint :: (a -> Change a) -- function for computing a fixpoint
              -> (Change a) -- has the fixpoint been reached?
              -> a
     fixpoint _ (Change a True)  = a
     fixpoint f (Change a False) = fixpoint f (f a)

  disambiguationStep :: d (TermPrim, DisambPrim) -> Change (d (TermPrim, DisambPrim))
  disambiguationStep thing = traverse performUpdate withInfo
      where (withInfo, _) = disambInfo thing noConstraints

noConstraints :: Constraints
noConstraints = Cnstr [][]  

--TODO: Rename to a more meaningfull name
fullConstraints :: Constraints -> Constraints
fullConstraints cs = Cnstr { sourceConstraintsOf = sourceConstraintsOf cs ++ targetConstraintsOf cs
                           , targetConstraintsOf = sourceConstraintsOf cs ++ targetConstraintsOf cs
                           }
                           
propagateConstraints :: Constraints -> Constraints -> Constraints
propagateConstraints topDown bottomUp 
  = Cnstr{sourceConstraintsOf = sourceConstraintsOf topDown ++ sourceConstraintsOf bottomUp
         ,targetConstraintsOf = targetConstraintsOf topDown ++ targetConstraintsOf bottomUp
         }
instance Disambiguatable P_IdentDf where
  disambInfo (P_Id o nm c []) _ = ( P_Id o nm c [], noConstraints)
  disambInfo (P_Id o nm c (a:lst)) _     = (P_Id o nm c (a':lst'), Cnstr (sourceConstraintsOf aRestr++sourceConstraintsOf nxt) [])
       where (a', aRestr)            = disambInfo a (Cnstr [MustBe (pCpt2aCpt c)] [])
             (P_Id _ _ _ lst', nxt)  = disambInfo (P_Id o nm c lst) (Cnstr [MustBe (pCpt2aCpt c)] [])
instance Disambiguatable P_IdentSegmnt where
  disambInfo (P_IdentExp v) x = (P_IdentExp v', rt)
     where (v',rt) = disambInfo v x
instance Disambiguatable P_Rule where
  disambInfo (P_Ru fps nm expr mean msg Nothing) x
   = (P_Ru fps nm exp' mean msg Nothing, rt)
   where (exp',rt) = disambInfo expr x
  disambInfo (P_Ru fps nm expr mean msg (Just viol)) x
   = (P_Ru fps nm exp' mean msg (Just viol'), rt)
   where (exp',rt) = disambInfo expr x
         (PairViewTerm viol',_) -- SJ 20131123: disambiguation does not depend on the contents of this pairview, but must come from outside...
          = (disambInfo (PairViewTerm viol) rt)
instance Disambiguatable PairViewTerm where
  disambInfo (PairViewTerm (PairView lst)) x
   = (PairViewTerm (PairView [pv' | pv <- lst, let (PairViewSegmentTerm pv',_) = disambInfo (PairViewSegmentTerm pv) x])
     , noConstraints) -- unrelated
instance Disambiguatable PairViewSegmentTerm where
  disambInfo (PairViewSegmentTerm (PairViewText orig s)) _ = (PairViewSegmentTerm (PairViewText orig s), noConstraints)
  disambInfo (PairViewSegmentTerm (PairViewExp orig st a)) constraints = (PairViewSegmentTerm (PairViewExp orig st res), rt)
    where (res,rt) = disambInfo a (Cnstr (case st of 
                                            Src -> sourceConstraintsOf constraints
                                            Tgt -> targetConstraintsOf constraints) [])
instance Disambiguatable P_ViewD where
  disambInfo (P_Vd { vd_pos  = o
                   , vd_lbl  = s
                   , vd_cpt  = c
                   , vd_isDefault = d
                   , vd_html = h
                   , vd_ats  = a
                   }) _ = ( P_Vd o s c d h (map (\x -> fst (disambInfo x constraints)) a)
                          , constraints
                          )
   where constraints = Cnstr [MustBe (pCpt2aCpt c)] []

instance Disambiguatable P_ViewSegmt where
  disambInfo (P_ViewText nr a) _ = (P_ViewText nr a,noConstraints)
  disambInfo (P_ViewHtml nr a) _ = (P_ViewHtml nr a,noConstraints)
  disambInfo (P_ViewExp nr a) i = (P_ViewExp nr a',r)
    where (a',r) = disambInfo a i

instance Disambiguatable P_SubIfc where
  disambInfo (P_InterfaceRef o a b) _      = (P_InterfaceRef o a b,noConstraints)
  disambInfo (P_Box o cl []   ) _        = (P_Box o cl [],noConstraints)
  disambInfo (P_Box o cl (a:lst)) env1  = 
     (P_Box o cl' (a':lst'),Cnstr (sourceConstraintsOf envA++sourceConstraintsOf envB) [])
   where (a', envA)              = disambInfo a                (Cnstr (sourceConstraintsOf envB++sourceConstraintsOf env1) [])
         (P_Box _ cl' lst',envB) = disambInfo (P_Box o cl lst) (Cnstr (sourceConstraintsOf env1++sourceConstraintsOf envA) [])

instance Disambiguatable P_ObjDef where
  disambInfo (P_Obj a b c -- term/expression
                        mCrud
                        v
                        d -- (potential) subobject
                        f)
                        env -- from the environment, only the source is important
   = (P_Obj a b c' mCrud v d' f, Cnstr (sourceConstraintsOf env2) [] -- only source information should be relevant
     )
    where
     (d', env1)
      = case d of
           Nothing -> (Nothing,noConstraints)
           Just si -> (\(x,y)->(Just x,y)) $ disambInfo si (Cnstr (targetConstraintsOf env2) [])
     (c', env2)
      = disambInfo c (Cnstr (sourceConstraintsOf env) (sourceConstraintsOf env1))
instance Disambiguatable Term where
  disambInfo (PFlp o a  ) env1 = ( PFlp o a', Cnstr (targetConstraintsOf envA)(sourceConstraintsOf envA) )
   where (a', envA) = disambInfo a (Cnstr (targetConstraintsOf env1)(sourceConstraintsOf env1))
  disambInfo (PCpl o a  ) env1 = ( PCpl o a', envA )
   where (a', envA) = disambInfo a env1
  disambInfo (PBrk o a  ) env1 = ( PBrk o a', envA )
   where (a', envA) = disambInfo a env1
  disambInfo (PKl0 o a  ) env1 = ( PKl0 o a', fullConstraints envA )
   where (a', envA) = disambInfo a (fullConstraints env1)
  disambInfo (PKl1 o a  ) env1 = ( PKl1 o a', fullConstraints envA )
   where (a', envA) = disambInfo a (fullConstraints env1)
  disambInfo (PEqu o a b) env1 = ( PEqu o a' b', propagateConstraints envA envB )
   where (a', envA) = disambInfo a (propagateConstraints env1 envB)
         (b', envB) = disambInfo b (propagateConstraints env1 envA)
  disambInfo (PInc o a b) env1 = ( PInc o a' b', propagateConstraints envA envB )
   where (a', envA) = disambInfo a (propagateConstraints env1 envB)
         (b', envB) = disambInfo b (propagateConstraints env1 envA)
  disambInfo (PIsc o a b) env1 = ( PIsc o a' b', propagateConstraints envA envB )
   where (a', envA) = disambInfo a (propagateConstraints env1 envB)
         (b', envB) = disambInfo b (propagateConstraints env1 envA)
  disambInfo (PUni o a b) env1 = ( PUni o a' b', propagateConstraints envA envB )
   where (a', envA) = disambInfo a (propagateConstraints env1 envB)
         (b', envB) = disambInfo b (propagateConstraints env1 envA)
  disambInfo (PDif o a b) env1 = ( PDif o a' b', propagateConstraints envA envB )
   where (a', envA) = disambInfo a (propagateConstraints env1 envB)
         (b', envB) = disambInfo b (propagateConstraints env1 envA)
  disambInfo (PLrs o a b) env1 = ( PLrs o a' b', Cnstr (sourceConstraintsOf envA) (sourceConstraintsOf envB) )
   where (a', envA) = disambInfo a (Cnstr (sourceConstraintsOf env1) (targetConstraintsOf envB))
         (b', envB) = disambInfo b (Cnstr (targetConstraintsOf env1) (targetConstraintsOf envA))
  disambInfo (PRrs o a b) env1 = ( PRrs o a' b', Cnstr (targetConstraintsOf envA) (targetConstraintsOf envB) )
   where (a', envA) = disambInfo a (Cnstr (sourceConstraintsOf envB) (sourceConstraintsOf env1))
         (b', envB) = disambInfo b (Cnstr (sourceConstraintsOf envA) (targetConstraintsOf env1))
  disambInfo (PDia o a b) env1 = ( PDia o a' b', Cnstr (sourceConstraintsOf envA) (sourceConstraintsOf envB))
   where (a', envA) = disambInfo a (Cnstr (sourceConstraintsOf env1) (targetConstraintsOf envB))
         (b', envB) = disambInfo b (Cnstr (targetConstraintsOf env1) (targetConstraintsOf envA))
  disambInfo (PCps o a b) env1 = ( PCps o a' b', Cnstr (sourceConstraintsOf envA) (targetConstraintsOf envB) )
   where (a', envA) = disambInfo a (Cnstr (sourceConstraintsOf env1) (sourceConstraintsOf envB))
         (b', envB) = disambInfo b (Cnstr (targetConstraintsOf envA) (targetConstraintsOf env1))
  disambInfo (PRad o a b) env1 = ( PRad o a' b', Cnstr (sourceConstraintsOf envA) (targetConstraintsOf envB) )
   where (a', envA) = disambInfo a (Cnstr (sourceConstraintsOf env1) (sourceConstraintsOf envB))
         (b', envB) = disambInfo b (Cnstr (targetConstraintsOf envA) (targetConstraintsOf env1))
  disambInfo (PPrd o a b) env1 = ( PPrd o a' b', Cnstr (sourceConstraintsOf envA) (targetConstraintsOf envB) )
   where (a', envA) = disambInfo a (Cnstr (sourceConstraintsOf env1) (sourceConstraintsOf envB))
         (b', envB) = disambInfo b (Cnstr (targetConstraintsOf envA) (targetConstraintsOf env1))
  disambInfo (Prim (a,b)) st = (Prim ((a,b), st), Cnstr (getDConcepts source b) (getDConcepts target b))

getDConcepts :: (Expression -> A_Concept) -> DisambPrim -> [D_Concept]
getDConcepts sot (Rel lst) = map (MayBe . sot) lst
getDConcepts sot (Known e) = [MustBe (sot e)]
getDConcepts _ _ = []

data DisambPrim
 = Rel [Expression] -- It is an expression, we don't know which, but it's going to be one of these (usually this is a list of relations)
 | Ident -- identity, and we know nothing about its type
 | Vee -- vee, type unknown
 | Mp1 PSingleton -- a singleton atomvalue, type unknown
 | Known Expression -- It is an expression, and we know exactly which. That is: disambiguation was succesful here
 deriving Show  -- Here, deriving Show serves debugging purposes only.

performUpdate :: ((t, DisambPrim),
                     Constraints)
                     -> Change (t, DisambPrim)
performUpdate ((t,unkn), Cnstr srcs' tgts')
 = case unkn of
     Known _ -> pure (t,unkn)
     Rel xs  -> determineBySize (\x -> if length x == length xs then pure (Rel xs) else impure (Rel x))
                ((findMatch' (mustBeSrc,mustBeTgt) xs `orWhenEmpty` findMatch' (mayBeSrc,mayBeTgt) xs)
                 `orWhenEmpty` xs)
     Ident   -> determineBySize suggest (map EDcI     (Set.toList possibleConcs))
     Mp1 x   -> determineBySize suggest (map (EMp1 x) (Set.toList possibleConcs))
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
    = [x | x<-lst, target x `elem` tgts]
   findMatch (srcs,[]) lst
    = [x | x<-lst, source x `elem` srcs]
   findMatch (srcs,tgts) lst
    = [x | x<-lst, source x `elem` srcs, target x `elem` tgts]
   mustBeSrc = mustBe srcs'
   mustBeTgt = mustBe tgts'
   mayBeSrc = mayBe srcs'
   mayBeTgt = mayBe tgts'
   mustBe xs = Set.fromList [x | (MustBe x) <- xs]
   mayBe  xs = Set.fromList [x | (MayBe x) <- xs]
   orWhenEmptyS a b = if (Set.null a) then b else a
   determineBySize _   [a] = impure (t,Known a)
   determineBySize err lst = fmap ((,) t) (err lst)
   impure x = Change x False
   isc = Set.intersection
   uni = Set.union

orWhenEmpty :: [a] -> [a] -> [a]
orWhenEmpty a b = if (null a) then b else a

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

