{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Z-sets: finite-support maps into the integers, the change currency of
--   incremental evaluation (DBSP, arXiv 2203.16684). See
--   memorybank\/incremental-evaluation\/delta-calculus.md for the calculus and
--   proofs\/incremental\/ for the machine-checked delta rules these functions
--   implement (obligation ids in the comments below).
--
--   Invariant, everywhere: no key maps to weight 0.
module Ampersand.FSpec.Incremental.ZSet
  ( ZBag,
    ZRel,
    -- unary (bags of atoms)
    bagApply,
    bagNeg,
    bagH,
    bagDistinct,
    bagProd,
    bagSizeF,
    -- binary (bags of pairs, indexed by source)
    relApply,
    relNeg,
    relFlip,
    relH,
    relWeight,
    relFromPairs,
    relToPairs,
    relDiff,
    relNull,
    relSizeF,
    composeDeltaOld,
    composeFlipDelta,
    pointwiseDelta,
    rowWeights,
    colWeights,
    diagRel,
  )
where

import Ampersand.Basics
import qualified RIO.Map as Map

-- | A unary Z-set: atoms with non-zero integer weights.
type ZBag a = Map a Int

-- | A binary Z-set: pairs with non-zero integer weights, indexed by source.
type ZRel a = Map a (Map a Int)

nonZero :: Int -> Maybe Int
nonZero 0 = Nothing
nonZero w = Just w

-- | Group addition, tuned for applying a small delta to a large integral:
--   cost O(|delta| * log |integral|). (Obligation Z1.)
bagApply :: (Ord a) => ZBag a -> ZBag a -> ZBag a
bagApply integral delta = foldl' step' integral (Map.toList delta)
  where
    step' acc (x, w) = Map.alter (nonZero . (+ w) . fromMaybe 0) x acc

-- | Additive inverse. (Obligation Z1.)
bagNeg :: ZBag a -> ZBag a
bagNeg = Map.map negate

-- | The zero-crossing function H: given the pre-distinct integral and an
--   incoming delta, the delta of the /set/ @distinct(integral)@.
--   Support is contained in the delta's support. (Obligation Z5.)
bagH :: (Ord a) => ZBag a -> ZBag a -> ZBag a
bagH integral delta =
  Map.fromList
    [ (x, c)
      | (x, w) <- Map.toList delta,
        let old = Map.findWithDefault 0 x integral,
        let c = crossing old (old + w),
        c /= 0
    ]

crossing :: Int -> Int -> Int
crossing old new
  | old <= 0 && new > 0 = 1
  | old > 0 && new <= 0 = -1
  | otherwise = 0

-- | Clip weights to {0,1}: the set carried by a Z-set.
bagDistinct :: ZBag a -> ZBag a
bagDistinct = Map.mapMaybe (\w -> if w > 0 then Just 1 else Nothing)

-- | Weighted cartesian product of two unary Z-sets. (Obligation Z4.)
bagProd :: ZBag a -> ZBag a -> ZRel a
bagProd u v
  | Map.null v = Map.empty
  | otherwise = Map.mapMaybe row u
  where
    row wu = let r = Map.mapMaybe (nonZero . (* wu)) v in if Map.null r then Nothing else Just r

-- | Total absolute weight; also serves to force the structure.
bagSizeF :: ZBag a -> Int
bagSizeF = Map.foldl' (\ !acc w -> acc + abs w) 0

-- | Apply a delta to a binary integral, row by row. (Obligation Z1.)
relApply :: (Ord a) => ZRel a -> ZRel a -> ZRel a
relApply integral delta = foldl' step' integral (Map.toList delta)
  where
    step' acc (x, row) =
      Map.alter
        ( \old ->
            let merged = bagApply (fromMaybe Map.empty old) row
             in if Map.null merged then Nothing else Just merged
        )
        x
        acc

relNeg :: ZRel a -> ZRel a
relNeg = Map.map bagNeg

-- | Transpose. Linear, its own delta. (Obligations Z1, B6.)
relFlip :: (Ord a) => ZRel a -> ZRel a
relFlip m =
  Map.fromListWith
    (Map.unionWith (+))
    [(b, Map.singleton a w) | (a, row) <- Map.toList m, (b, w) <- Map.toList row]

-- | Zero-crossing on binary Z-sets. (Obligation Z5.)
relH :: (Ord a) => ZRel a -> ZRel a -> ZRel a
relH integral =
  Map.mapMaybeWithKey
    ( \x row ->
        let irow = Map.findWithDefault Map.empty x integral
            out =
              Map.fromList
                [ (y, c)
                  | (y, w) <- Map.toList row,
                    let old = Map.findWithDefault 0 y irow,
                    let c = crossing old (old + w),
                    c /= 0
                ]
         in if Map.null out then Nothing else Just out
    )

relWeight :: (Ord a) => ZRel a -> a -> a -> Int
relWeight m x y = maybe 0 (Map.findWithDefault 0 y) (Map.lookup x m)

relFromPairs :: (Ord a) => [((a, a), Int)] -> ZRel a
relFromPairs ps =
  Map.filter (not . Map.null)
    . Map.map (Map.filter (/= 0))
    $ Map.fromListWith (Map.unionWith (+)) [(x, Map.singleton y w) | ((x, y), w) <- ps]

-- | The pairs with positive weight (the carried set).
relToPairs :: ZRel a -> [(a, a)]
relToPairs m = [(x, y) | (x, row) <- Map.toList m, (y, w) <- Map.toList row, w > 0]

-- | @new − old@ for two set-valued Z-rels: the delta a recompute node emits.
relDiff :: (Ord a) => ZRel a -> ZRel a -> ZRel a
relDiff new old = relApply (relNeg old) new

relNull :: ZRel a -> Bool
relNull = Map.null

-- | Total absolute weight; also serves to force the structure.
relSizeF :: ZRel a -> Int
relSizeF = Map.foldl' (\ !acc row -> acc + bagSizeF row) 0

-- | @Δa ; b_old@ — the first term of the composition delta (obligation Z3):
--   iterate the delta, look up matching rows in the (source-indexed) old right
--   operand. Work: O(|Δa| × matching rows), never a scan of b.
composeDeltaOld :: (Ord a) => ZRel a -> ZRel a -> ZRel a
composeDeltaOld deltaA oldB =
  Map.filter (not . Map.null)
    $ Map.mapMaybe rowFor deltaA
  where
    rowFor mids =
      let out =
            Map.foldlWithKey'
              ( \acc m w -> case Map.lookup m oldB of
                  Nothing -> acc
                  Just ys -> bagApply acc (Map.mapMaybe (nonZero . (* w)) ys)
              )
              Map.empty
              mids
       in if Map.null out then Nothing else Just out

-- | @a_new ; Δb@ — the second term of the composition delta (obligation Z3),
--   computed against the incrementally maintained /flipped/ copy of a's output
--   (indexed by the middle atom). Work: O(|Δb| × matching rows).
composeFlipDelta :: (Ord a) => ZRel a -> ZRel a -> ZRel a
composeFlipDelta flipA =
  Map.foldlWithKey'
    ( \acc m ys -> case Map.lookup m flipA of
        Nothing -> acc
        Just xs ->
          relApply
            acc
            (Map.mapMaybe (\wx -> let r = Map.mapMaybe (nonZero . (* wx)) ys in if Map.null r then Nothing else Just r) xs)
    )
    Map.empty

-- | @Δa ⊙ b@ — pointwise product of a delta against a stored operand
--   (obligation Z2). Work: O(|Δa| × log |b|).
pointwiseDelta :: (Ord a) => ZRel a -> ZRel a -> ZRel a
pointwiseDelta delta other =
  Map.filter (not . Map.null)
    $ Map.mapMaybeWithKey
      ( \x row ->
          let out = Map.mapMaybeWithKey (\y w -> nonZero (w * relWeight other x y)) row
           in if Map.null out then Nothing else Just out
      )
      delta

-- | Sum of the weights per source atom: the linear projection to the domain.
rowWeights :: ZRel a -> ZBag a
rowWeights = Map.mapMaybe (nonZero . Map.foldl' (+) 0)

-- | Sum of the weights per target atom.
colWeights :: (Ord a) => ZRel a -> ZBag a
colWeights = rowWeights . relFlip

-- | The diagonal of a unary Z-set: I over a population (delta).
diagRel :: ZBag a -> ZRel a
diagRel = Map.mapWithKey Map.singleton
