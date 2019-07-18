{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Ampersand.ADL1.Disambiguate
   ( disambiguate
   , orWhenEmpty
   , DisambPrim(..)
   , pCpt2aCpt
   ) where
import           Ampersand.Basics
import           Ampersand.Core.ParseTree
import           Ampersand.Core.AbstractSyntaxTree
import qualified Data.List.NonEmpty as NEL
import qualified RIO.Set as Set
import           Control.Arrow
import           Text.PrettyPrint.Leijen (Pretty(..),text)

-- this is *only* used internally!
data D_Concept
 = MustBe A_Concept
 | MayBe  A_Concept
 deriving (Show, Eq)

data Constraints = Cnstr {bottomUpSourceTypes :: [D_Concept]
                         ,bottomUpTargetTypes :: [D_Concept]
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
-- (3) Make ThingPolymorphic an instance of Disambiguatable. It is your responsibility to prevent loops here, which is tricky:
--       the result may not depend on the second argument (more later).
--     The instance looks like this:
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
--         disambInfo (PCps o a b) (ia1,ib1)
--            = ( PCps o a' b', (ia, ib) ) -- here only bottom-up information is allowed: don't use ia1 or ib1 here!
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
              -> Change a -> a
     fixpoint _ (Change a True)  = a
     fixpoint f (Change a False) = fixpoint f (f a)

  disambiguationStep :: d (TermPrim, DisambPrim) -> Change (d (TermPrim, DisambPrim))
  disambiguationStep thing = traverse performUpdate withInfo
      where (withInfo, _) = disambInfo thing noConstraints
  {-# MINIMAL disambInfo #-}

noConstraints :: Constraints
noConstraints = Cnstr [][]

--TODO: Rename to a more meaningfull name
fullConstraints :: Constraints -> Constraints
fullConstraints cs = Cnstr { bottomUpSourceTypes = bottomUpSourceTypes cs ++ bottomUpTargetTypes cs
                           , bottomUpTargetTypes = bottomUpSourceTypes cs ++ bottomUpTargetTypes cs
                           }

propagateConstraints :: Constraints -> Constraints -> Constraints
propagateConstraints topDown bottomUp
  = Cnstr{bottomUpSourceTypes = bottomUpSourceTypes topDown ++ bottomUpSourceTypes bottomUp
         ,bottomUpTargetTypes = bottomUpTargetTypes topDown ++ bottomUpTargetTypes bottomUp
         }
instance Disambiguatable P_IdentDf where
--  disambInfo (P_Id o nm c []) _ = ( P_Id o nm c [], noConstraints)
--  disambInfo (P_Id o nm c (a:lst)) _     = (P_Id o nm c (a':lst'), Cnstr (bottomUpSourceTypes aRestr++bottomUpSourceTypes nxt) [])
--       where (a', aRestr)            = disambInfo a (Cnstr [MustBe (pCpt2aCpt c)] [])
--             (P_Id _ _ _ lst', nxt)  = disambInfo (P_Id o nm c lst) (Cnstr [MustBe (pCpt2aCpt c)] [])
  disambInfo (P_Id o nm c atts) _     = (P_Id o nm c atts', Cnstr (concatMap bottomUpSourceTypes . NEL.toList $ restr') [])
     where
      (atts', restr') = NEL.unzip $
           fmap (\a -> disambInfo a (Cnstr [MustBe (pCpt2aCpt c)] [])) atts
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
          = disambInfo (PairViewTerm viol) rt
instance Disambiguatable PairViewTerm where
  disambInfo (PairViewTerm (PairView lst)) x
   = (PairViewTerm (PairView . NEL.fromList $ [pv' | pv <- NEL.toList lst, let (PairViewSegmentTerm pv',_) = disambInfo (PairViewSegmentTerm pv) x])
     , noConstraints) -- unrelated
instance Disambiguatable PairViewSegmentTerm where
  disambInfo (PairViewSegmentTerm (PairViewText orig s)) _ = (PairViewSegmentTerm (PairViewText orig s), noConstraints)
  disambInfo (PairViewSegmentTerm (PairViewExp orig st a)) constraints = (PairViewSegmentTerm (PairViewExp orig st res), rt)
    where (res,rt) = disambInfo a (Cnstr (case st of
                                            Src -> bottomUpSourceTypes constraints
                                            Tgt -> bottomUpTargetTypes constraints) [])
instance Disambiguatable P_ViewD where
  disambInfo P_Vd { pos  = o
                  , vd_lbl  = s
                  , vd_cpt  = c
                  , vd_isDefault = d
                  , vd_html = h
                  , vd_ats  = a
                  } _ = ( P_Vd o s c d h (fmap (\x -> fst (disambInfo x constraints)) a)
                        , constraints
                        )
   where constraints = Cnstr [MustBe (pCpt2aCpt c)] []

instance Disambiguatable P_ViewSegment where
  disambInfo (P_ViewSegment a b c) i = (P_ViewSegment a b c', r)
    where (c', r) = disambInfo c i
instance Disambiguatable P_ViewSegmtPayLoad where
  disambInfo (P_ViewText a) _ = (P_ViewText a,noConstraints)
  disambInfo (P_ViewExp a) i = (P_ViewExp a',r)
    where (a',r) = disambInfo a i

instance Disambiguatable P_SubIfc where
  disambInfo (P_InterfaceRef o a b) _      = (P_InterfaceRef o a b,noConstraints)
  disambInfo (P_Box o cl []   ) _        = (P_Box o cl [],noConstraints)
  disambInfo (P_Box o cl (a:lst)) env1  =
     (P_Box o cl' (a':lst'),Cnstr (bottomUpSourceTypes envA++bottomUpSourceTypes envB) [])
   where (a', envA)              = disambInfo a                (Cnstr (bottomUpSourceTypes envB++bottomUpSourceTypes env1) [])
         (P_Box _ cl' lst',envB) = disambInfo (P_Box o cl lst) (Cnstr (bottomUpSourceTypes env1++bottomUpSourceTypes envA) [])

instance Disambiguatable P_BoxItem where
  disambInfo (P_BxExpr a b c -- term/expression
                        mCrud
                        v
                        d -- (potential) subobject
                        )
                        env -- from the environment, only the source is important
   = (P_BxExpr a b c' mCrud v d', Cnstr (bottomUpSourceTypes env2) [] -- only source information should be relevant
     )
    where
     (d', env1)
      = case d of
           Nothing -> (Nothing,noConstraints)
           Just si -> Control.Arrow.first Just $ disambInfo si (Cnstr (bottomUpTargetTypes env2) [])
     (c', env2)
      = disambInfo c (Cnstr (bottomUpSourceTypes env) (bottomUpSourceTypes env1))
  disambInfo (P_BxTxt  a b c) _ = (P_BxTxt  a b c, noConstraints)

instance Disambiguatable Term where
  disambInfo (PFlp o a  ) env1 = ( PFlp o a', Cnstr (bottomUpTargetTypes envA)(bottomUpSourceTypes envA) )
   where (a', envA) = disambInfo a (Cnstr (bottomUpTargetTypes env1)(bottomUpSourceTypes env1))
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
  disambInfo (PLrs o a b) env1 = ( PLrs o a' b', Cnstr (bottomUpSourceTypes envA) (bottomUpSourceTypes envB) )
   where (a', envA) = disambInfo a (Cnstr (bottomUpSourceTypes env1) (bottomUpTargetTypes envB))
         (b', envB) = disambInfo b (Cnstr (bottomUpTargetTypes env1) (bottomUpTargetTypes envA))
  disambInfo (PRrs o a b) env1 = ( PRrs o a' b', Cnstr (bottomUpTargetTypes envA) (bottomUpTargetTypes envB) )
   where (a', envA) = disambInfo a (Cnstr (bottomUpSourceTypes envB) (bottomUpSourceTypes env1))
         (b', envB) = disambInfo b (Cnstr (bottomUpSourceTypes envA) (bottomUpTargetTypes env1))
  disambInfo (PDia o a b) env1 = ( PDia o a' b', Cnstr (bottomUpSourceTypes envA) (bottomUpTargetTypes envB))
   where (a', envA) = disambInfo a (Cnstr (bottomUpSourceTypes env1) (bottomUpSourceTypes envB))
         (b', envB) = disambInfo b (Cnstr (bottomUpTargetTypes envA) (bottomUpTargetTypes env1))
  disambInfo (PCps o a b) env1 = ( PCps o a' b', Cnstr (bottomUpSourceTypes envA) (bottomUpTargetTypes envB) )
   where (a', envA) = disambInfo a (Cnstr (bottomUpSourceTypes env1) (bottomUpSourceTypes envB))
         (b', envB) = disambInfo b (Cnstr (bottomUpTargetTypes envA) (bottomUpTargetTypes env1))
  disambInfo (PRad o a b) env1 = ( PRad o a' b', Cnstr (bottomUpSourceTypes envA) (bottomUpTargetTypes envB) )
   where (a', envA) = disambInfo a (Cnstr (bottomUpSourceTypes env1) (bottomUpSourceTypes envB))
         (b', envB) = disambInfo b (Cnstr (bottomUpTargetTypes envA) (bottomUpTargetTypes env1))
  disambInfo (PPrd o a b) env1 = ( PPrd o a' b', Cnstr (bottomUpSourceTypes envA) (bottomUpTargetTypes envB) )
   where (a', envA) = disambInfo a (Cnstr (bottomUpSourceTypes env1) [])
         (b', envB) = disambInfo b (Cnstr [] (bottomUpTargetTypes env1))
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
instance Pretty DisambPrim where
  pretty = text . show
instance Pretty a => Pretty (a,DisambPrim) where
  pretty (t,_) = pretty t
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
   possibleConcs = (mustBeSrc `Set.intersection` mustBeTgt) `orWhenEmptyS`
                   (mustBeSrc `Set.union` mustBeTgt) `orWhenEmptyS`
                   (mayBeSrc  `Set.intersection` mayBeTgt ) `orWhenEmptyS`
                   (mayBeSrc  `Set.union` mayBeTgt )
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
   orWhenEmptyS a b = if Set.null a then b else a
   determineBySize _   [a] = impure (t,Known a)
   determineBySize err lst = fmap ((,) t) (err lst)
   impure x = Change x False

orWhenEmpty :: [a] -> [a] -> [a]
orWhenEmpty a b = if null a then b else a

pCpt2aCpt :: P_Concept -> A_Concept
pCpt2aCpt pc
    = case pc of
        PCpt{} -> makeConcept (p_cptnm pc)
        P_Singleton -> ONE

data Change a = Change a Bool
instance Functor Change where
 fmap f (Change a b) = Change (f a) b
instance Applicative Change where
 (<*>) (Change f b) (Change a b2) = Change (f a) (b && b2)
 pure a = Change a True
