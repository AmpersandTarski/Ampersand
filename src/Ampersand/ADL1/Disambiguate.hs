{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Ampersand.ADL1.Disambiguate
  ( disambiguate,
    orWhenEmpty,
    DisambPrim (..)
  )
where

import Ampersand.Basics
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ParseTree
import Control.Arrow
import qualified RIO.NonEmpty as NE
import qualified RIO.NonEmpty.Partial as PARTIAL
import qualified RIO.Set as Set
import Text.PrettyPrint.Leijen (Pretty (..), text)

-- this is *only* used internally!
data DConcept
  = MustBe A_Concept
  | MayBe A_Concept
  deriving (Show, Eq)

data Constraints = Cnstr
  { bottomUpSourceTypes :: [DConcept],
    bottomUpTargetTypes :: [DConcept]
  }
  deriving (Show)

class (Traversable d) => Disambiguatable d where
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
  --     disambInfo cptMap (Thing1 x y z) td = (Thing1 x' y' z', (bottomUpSourceTypes,bottomUpTargetTypes))
  --      where (x',resultingTypesForX) = disambInfo cptMap x' topDownTypesForX
  --            (y',resultingTypesForY) = disambInfo cptMap y' topDownTypesForY
  --            (z',resultingTypesForZ) = disambInfo cptMap z' topDownTypesForZ
  --     The variables topDownTypesFor... may depend on td,
  --     the variables bottomUpSourceTypes and bottomUpTargetTypes may depend on resultingTypesFor...
  --     Closing the loop (at the top of the structure) is done in the function "disambiguationStep".
  --     Note that disambInfo actually performs two separate functions in one go: one to go top down, the other to go bottom up.
  --     The top-down function may use parts of the bottom-up function, but not the other way around.
  --     A nice example to look at is PCps:
  --         disambInfo cptMap (PCps o a b) (ia1,ib1)
  --            = ( PCps o a' b', (ia, ib) ) -- here only bottom-up information is allowed: don't use ia1 or ib1 here!
  --          where (a', (ia,ic1)) = disambInfo cptMap a (ia1,ic2) -- here ic2 is top-down, so that is ok
  --                (b', (ic2,ib)) = disambInfo cptMap b (ic1,ib1)
  disambInfo ::
    ConceptMap -> -- required to turn P_Concepts into proper A_Concepts (see issue #999)
    d (TermPrim, DisambPrim) -> -- the thing that is disabmiguated
    Constraints -> -- the inferred types (from the environment = top down)
    ( d ((TermPrim, DisambPrim), Constraints), -- only the environment for the term (top down)
      Constraints -- the inferred type, bottom up (not including the environment, that is: not using the second argument: prevent loops!)
    )
  disambiguate ::
    ConceptMap ->
    (TermPrim -> (TermPrim, DisambPrim)) -> -- disambiguation function
    d TermPrim -> -- object to be disambiguated
    d (TermPrim, DisambPrim) -- disambiguated object
  disambiguate cptMap termPrimDisAmb x = fixpoint (disambiguationStep cptMap) (Change (fmap termPrimDisAmb x))
    where
      fixpoint ::
        (a -> Change a) -> -- function for computing a fixpoint
        Change a ->
        a
      fixpoint _ (Stable a) = a
      fixpoint f (Change a) = fixpoint f (f a)

  disambiguationStep :: ConceptMap -> d (TermPrim, DisambPrim) -> Change (d (TermPrim, DisambPrim))
  disambiguationStep cptMap thing = traverse performUpdate withInfo
    where
      (withInfo, _) = disambInfo cptMap thing noConstraints
  {-# MINIMAL disambInfo #-}

noConstraints :: Constraints
noConstraints = Cnstr [] []

-- TODO: Rename to a more meaningfull name
fullConstraints :: Constraints -> Constraints
fullConstraints cs =
  Cnstr
    { bottomUpSourceTypes = bottomUpSourceTypes cs ++ bottomUpTargetTypes cs,
      bottomUpTargetTypes = bottomUpSourceTypes cs ++ bottomUpTargetTypes cs
    }

propagateConstraints :: Constraints -> Constraints -> Constraints
propagateConstraints topDown bottomUp =
  Cnstr
    { bottomUpSourceTypes = bottomUpSourceTypes topDown ++ bottomUpSourceTypes bottomUp,
      bottomUpTargetTypes = bottomUpTargetTypes topDown ++ bottomUpTargetTypes bottomUp
    }

instance Disambiguatable P_IdentDf where
  disambInfo pCpt2aCpt (P_Id orig nm lbl cpt atts) _ = (P_Id orig nm lbl cpt atts', Cnstr (concatMap bottomUpSourceTypes . NE.toList $ restr') [])
    where
      (atts', restr') =
        NE.unzip
          $ fmap (\a -> disambInfo pCpt2aCpt a (Cnstr [MustBe (pCpt2aCpt cpt)] [])) atts

instance Disambiguatable P_IdentSegmnt where
  disambInfo cptMap (P_IdentExp v) x = (P_IdentExp v', rt)
    where
      (v', rt) = disambInfo cptMap v x

instance Disambiguatable P_Rule where
  disambInfo cptMap (P_Rule fps nm lbl expr mean msg Nothing) x =
    (P_Rule fps nm lbl exp' mean msg Nothing, rt)
    where
      (exp', rt) = disambInfo cptMap expr x
  disambInfo cptMap (P_Rule fps nm lbl expr mean msg (Just viol)) x =
    (P_Rule fps nm lbl exp' mean msg (Just viol'), rt)
    where
      (exp', rt) = disambInfo cptMap expr x
      (PairViewTerm viol', _) -- SJ 20131123: disambiguation does not depend on the contents of this pairview, but must come from outside...
        =
        disambInfo cptMap (PairViewTerm viol) rt

instance Disambiguatable PairViewTerm where
  disambInfo cptMap (PairViewTerm (PairView lst)) x =
    ( PairViewTerm (PairView . PARTIAL.fromList $ [pv' | pv <- NE.toList lst, let (PairViewSegmentTerm pv', _) = disambInfo cptMap (PairViewSegmentTerm pv) x]),
      noConstraints -- unrelated
    )

instance Disambiguatable PairViewSegmentTerm where
  disambInfo _ (PairViewSegmentTerm (PairViewText orig s)) _ = (PairViewSegmentTerm (PairViewText orig s), noConstraints)
  disambInfo cptMap (PairViewSegmentTerm (PairViewExp orig st a)) constraints = (PairViewSegmentTerm (PairViewExp orig st res), rt)
    where
      (res, rt) =
        disambInfo
          cptMap
          a
          ( Cnstr
              ( case st of
                  Src -> bottomUpSourceTypes constraints
                  Tgt -> bottomUpTargetTypes constraints
              )
              []
          )

instance Disambiguatable P_ViewD where
  disambInfo
    pCpt2aCpt
    P_Vd
      { pos = orig,
        vd_nm = nm,
        vd_label = lbl,
        vd_cpt = cpt,
        vd_isDefault = isDef,
        vd_html = template,
        vd_ats = segments
      }
    _ =
      ( P_Vd orig nm lbl cpt isDef template (fmap (\x -> fst (disambInfo pCpt2aCpt x constraints)) segments),
        constraints
      )
      where
        constraints = Cnstr [MustBe (pCpt2aCpt cpt)] []

instance Disambiguatable P_Enforce where
  disambInfo cptMap (P_Enforce o a op b) env1 = (P_Enforce o a' op b', propagateConstraints envA envB)
    where
      (a', envA) = ((a, env1), Cnstr (getDConcepts source (snd a)) (getDConcepts target (snd a)))
      (b', envB) = disambInfo cptMap b (propagateConstraints env1 envA)

instance Disambiguatable P_ViewSegment where
  disambInfo cptMap (P_ViewSegment a b c) i = (P_ViewSegment a b c', r)
    where
      (c', r) = disambInfo cptMap c i

instance Disambiguatable P_ViewSegmtPayLoad where
  disambInfo _ (P_ViewText a) _ = (P_ViewText a, noConstraints)
  disambInfo cptMap (P_ViewExp a) i = (P_ViewExp a', r)
    where
      (a', r) = disambInfo cptMap a i

instance Disambiguatable P_SubIfc where
  disambInfo _ (P_InterfaceRef o a b) _ = (P_InterfaceRef o a b, noConstraints)
  disambInfo _ (P_Box o cl []) _ = (P_Box o cl [], noConstraints)
  disambInfo cptMap (P_Box o cl (a : lst)) env1 =
    (P_Box o cl' (a' : lst'), Cnstr (bottomUpSourceTypes envA ++ bottomUpSourceTypes envB) [])
    where
      (a', envA) = disambInfo cptMap a (Cnstr (bottomUpSourceTypes envB ++ bottomUpSourceTypes env1) [])
      (cl', lst', envB) = case disambInfo cptMap (P_Box o cl lst) (Cnstr (bottomUpSourceTypes env1 ++ bottomUpSourceTypes envA) []) of
        (P_Box _ cl'' lst'', envB'') -> (cl'', lst'', envB'')
        (P_InterfaceRef {}, _) -> fatal "Unexpected result of disambInfo"

instance Disambiguatable P_BoxItem where
  disambInfo
    cptMap
    ( P_BoxItemTerm
        localNm
        lbl
        orig
        term
        mCrud
        vw
        sub -- (potential) subobject
      )
    env -- from the environment, only the source is important
      =
      (P_BoxItemTerm localNm lbl orig c' mCrud vw d', Cnstr (bottomUpSourceTypes env2) []) -- only source information should be relevant
      where
        (d', env1) =
          case sub of
            Nothing -> (Nothing, noConstraints)
            Just si -> Control.Arrow.first Just $ disambInfo cptMap si (Cnstr (bottomUpTargetTypes env2) [])
        (c', env2) =
          disambInfo cptMap term (Cnstr (bottomUpSourceTypes env) (bottomUpSourceTypes env1))
  disambInfo _ (P_BxTxt localNm orig txt) _ = (P_BxTxt localNm orig txt, noConstraints)

instance Disambiguatable Term where
  disambInfo cptMap (PFlp o a) env1 = (PFlp o a', Cnstr (bottomUpTargetTypes envA) (bottomUpSourceTypes envA))
    where
      (a', envA) = disambInfo cptMap a (Cnstr (bottomUpTargetTypes env1) (bottomUpSourceTypes env1))
  disambInfo cptMap (PCpl o a) env1 = (PCpl o a', envA)
    where
      (a', envA) = disambInfo cptMap a env1
  disambInfo cptMap (PBrk o a) env1 = (PBrk o a', envA)
    where
      (a', envA) = disambInfo cptMap a env1
  disambInfo cptMap (PKl0 o a) env1 = (PKl0 o a', fullConstraints envA)
    where
      (a', envA) = disambInfo cptMap a (fullConstraints env1)
  disambInfo cptMap (PKl1 o a) env1 = (PKl1 o a', fullConstraints envA)
    where
      (a', envA) = disambInfo cptMap a (fullConstraints env1)
  disambInfo cptMap (PEqu o a b) env1 = (PEqu o a' b', propagateConstraints envA envB)
    where
      (a', envA) = disambInfo cptMap a (propagateConstraints env1 envB)
      (b', envB) = disambInfo cptMap b (propagateConstraints env1 envA)
  disambInfo cptMap (PInc o a b) env1 = (PInc o a' b', propagateConstraints envA envB)
    where
      (a', envA) = disambInfo cptMap a (propagateConstraints env1 envB)
      (b', envB) = disambInfo cptMap b (propagateConstraints env1 envA)
  disambInfo cptMap (PIsc o a b) env1 = (PIsc o a' b', propagateConstraints envA envB)
    where
      (a', envA) = disambInfo cptMap a (propagateConstraints env1 envB)
      (b', envB) = disambInfo cptMap b (propagateConstraints env1 envA)
  disambInfo cptMap (PUni o a b) env1 = (PUni o a' b', propagateConstraints envA envB)
    where
      (a', envA) = disambInfo cptMap a (propagateConstraints env1 envB)
      (b', envB) = disambInfo cptMap b (propagateConstraints env1 envA)
  disambInfo cptMap (PDif o a b) env1 = (PDif o a' b', propagateConstraints envA envB)
    where
      (a', envA) = disambInfo cptMap a (propagateConstraints env1 envB)
      (b', envB) = disambInfo cptMap b (propagateConstraints env1 envA)
  disambInfo cptMap (PLrs o a b) env1 = (PLrs o a' b', Cnstr (bottomUpSourceTypes envA) (bottomUpSourceTypes envB))
    where
      (a', envA) = disambInfo cptMap a (Cnstr (bottomUpSourceTypes env1) (bottomUpTargetTypes envB))
      (b', envB) = disambInfo cptMap b (Cnstr (bottomUpTargetTypes env1) (bottomUpTargetTypes envA))
  disambInfo cptMap (PRrs o a b) env1 = (PRrs o a' b', Cnstr (bottomUpTargetTypes envA) (bottomUpTargetTypes envB))
    where
      (a', envA) = disambInfo cptMap a (Cnstr (bottomUpSourceTypes envB) (bottomUpSourceTypes env1))
      (b', envB) = disambInfo cptMap b (Cnstr (bottomUpSourceTypes envA) (bottomUpTargetTypes env1))
  disambInfo cptMap (PDia o a b) env1 = (PDia o a' b', Cnstr (bottomUpSourceTypes envA) (bottomUpTargetTypes envB))
    where
      (a', envA) = disambInfo cptMap a (Cnstr (bottomUpSourceTypes env1) (bottomUpSourceTypes envB))
      (b', envB) = disambInfo cptMap b (Cnstr (bottomUpTargetTypes envA) (bottomUpTargetTypes env1))
  disambInfo cptMap (PCps o a b) env1 = (PCps o a' b', Cnstr (bottomUpSourceTypes envA) (bottomUpTargetTypes envB))
    where
      (a', envA) = disambInfo cptMap a (Cnstr (bottomUpSourceTypes env1) (bottomUpSourceTypes envB))
      (b', envB) = disambInfo cptMap b (Cnstr (bottomUpTargetTypes envA) (bottomUpTargetTypes env1))
  disambInfo cptMap (PRad o a b) env1 = (PRad o a' b', Cnstr (bottomUpSourceTypes envA) (bottomUpTargetTypes envB))
    where
      (a', envA) = disambInfo cptMap a (Cnstr (bottomUpSourceTypes env1) (bottomUpSourceTypes envB))
      (b', envB) = disambInfo cptMap b (Cnstr (bottomUpTargetTypes envA) (bottomUpTargetTypes env1))
  disambInfo cptMap (PPrd o a b) env1 = (PPrd o a' b', Cnstr (bottomUpSourceTypes envA) (bottomUpTargetTypes envB))
    where
      (a', envA) = disambInfo cptMap a (Cnstr (bottomUpSourceTypes env1) [])
      (b', envB) = disambInfo cptMap b (Cnstr [] (bottomUpTargetTypes env1))
  disambInfo _ (Prim (a, b)) st = (Prim ((a, b), st), Cnstr (getDConcepts source b) (getDConcepts target b))

getDConcepts :: (Expression -> A_Concept) -> DisambPrim -> [DConcept]
getDConcepts sot (Rel lst) = map (MayBe . sot) lst
getDConcepts sot (Known e) = [MustBe (sot e)]
getDConcepts _ _ = []

data DisambPrim
  = Rel [Relation] -- It is an expression, we don't know which, but it's going to be one of these (usually this is a list of relations)
  | Ident -- identity, and we know nothing about its type
  | Vee -- vee, type unknown
  | Mp1 PAtomValue -- a singleton atomvalue, type unknown
  | BinOper PBinOp -- a binary operator, type unknown
  | Known Expression -- It is an expression, and we know exactly which. That is: disambiguation was succesful here
  deriving (Show) -- Here, deriving Show serves debugging purposes only.

instance Pretty DisambPrim where
  pretty = text . show

instance (Pretty a) => Pretty (a, DisambPrim) where
  pretty (t, _) = pretty t

performUpdate ::
  ( (t, DisambPrim),
    Constraints
  ) ->
  Change (t, DisambPrim)
performUpdate ((t, unkn), Cnstr srcs' tgts') =
  case unkn of
    Known _ -> pure (t, unkn)
    Rel xs ->
      determineBySize
        (\x -> if length x == length xs then pure (Rel xs) else Change (Rel x))
        ( (findMatch' (mustBeSrc, mustBeTgt) xs `orWhenEmpty` findMatch' (mayBeSrc, mayBeTgt) xs)
            `orWhenEmpty` xs
        )
    Ident -> determineBySize suggest (map EDcI (Set.toList possibleConcs))
    BinOper oper -> determineBySize suggest (map (EBin oper) (Set.toList possibleConcs))
    Mp1 x -> determineBySize suggest (map (EMp1 x) (Set.toList possibleConcs))
    Vee ->
      determineBySize
        (const (pure unkn))
        [EDcV (Sign a b) | a <- Set.toList mustBeSrc, b <- Set.toList mustBeTgt]
  where
    suggest [] = pure unkn
    suggest lst = Change (Rel lst) -- TODO: find out whether it is equivalent to put "pure" here (which could be faster).
    possibleConcs =
      (mustBeSrc `Set.intersection` mustBeTgt)
        `orWhenEmptyS` (mustBeSrc `Set.union` mustBeTgt)
        `orWhenEmptyS` (mayBeSrc `Set.intersection` mayBeTgt)
        `orWhenEmptyS` (mayBeSrc `Set.union` mayBeTgt)
    findMatch' (a, b) = findMatch (Set.toList a, Set.toList b)
    findMatch ([], []) _ = []
    findMatch ([], tgts) lst =
      [x | x <- lst, target x `elem` tgts]
    findMatch (srcs, []) lst =
      [x | x <- lst, source x `elem` srcs]
    findMatch (srcs, tgts) lst =
      [x | x <- lst, source x `elem` srcs, target x `elem` tgts]
    mustBeSrc = mustBe srcs'
    mustBeTgt = mustBe tgts'
    mayBeSrc = mayBe srcs'
    mayBeTgt = mayBe tgts'
    mustBe xs = Set.fromList [x | (MustBe x) <- xs]
    mayBe xs = Set.fromList [x | (MayBe x) <- xs]
    orWhenEmptyS a b = if Set.null a then b else a
    determineBySize _ [a] = Change (t, Known a)
    determineBySize err lst = fmap (t,) (err lst)

orWhenEmpty :: [a] -> [a] -> [a]
orWhenEmpty a b = if null a then b else a

data Change a
  = Stable a
  | Change a

instance Functor Change where
  fmap f (Change a) = Change (f a)
  fmap f (Stable a) = Stable (f a)

instance Applicative Change where
  (<*>) (Stable f) (Stable a) = Stable (f a)
  (<*>) (Change f) (Stable a) = Change (f a)
  (<*>) (Change f) (Change a) = Change (f a)
  (<*>) (Stable f) (Change a) = Change (f a)
  pure = Stable
