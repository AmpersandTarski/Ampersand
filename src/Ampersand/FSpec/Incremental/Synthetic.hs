{-# LANGUAGE ScopedTypeVariables #-}

-- | Deterministic synthetic populations and transaction streams, shared by
--   the in-memory benchmark and the delta-SQL harness of
--   @ampersand incremental-bench@.
module Ampersand.FSpec.Incremental.Synthetic
  ( mix,
    randBelow,
    nth,
    benchAtom,
    synthesize,
    pickTx,
    pickReplayTx,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Data.Bits (shiftR, xor)
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.Set as Set

-- deterministic splitmix64
mix :: Word64 -> Word64
mix s =
  let z0 = s + 0x9E3779B97F4A7C15
      z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
   in z2 `xor` (z2 `shiftR` 31)

randBelow :: Int -> Word64 -> (Int, Word64)
randBelow n rng =
  let rng' = mix rng
   in (fromIntegral (rng' `mod` fromIntegral (max 1 n)), rng')

nth :: Int -> [a] -> a
nth i xs = case L.drop i xs of
  (x : _) -> x
  [] -> fatal "nth: index out of range"

-- | A deterministic synthetic atom for a concept, honouring its representation.
benchAtom :: ContextInfo -> A_Concept -> Int -> AAtomValue
benchAtom ci c i =
  case unsafePAtomVal2AtomValue tt (Just c) pav of
    Right v -> v
    Left msg -> fatal ("cannot synthesize an atom for " <> tshow c <> ": " <> msg)
  where
    tt = reprType ci c
    pav = case tt of
      Integer -> ScriptInt OriginUnknown (fromIntegral i)
      Float -> ScriptFloat OriginUnknown (fromIntegral i)
      Boolean -> ComnBool OriginUnknown (even i)
      _ -> ScriptString OriginUnknown ("bench_" <> tshow i)

-- | Synthetic initial population: per relation, @n@ distinct pairs drawn from
--   pools of @n@ atoms per concept.
synthesize :: ContextInfo -> [Relation] -> Int -> Word64 -> ([Population], Word64)
synthesize ci rels n rng0 = go rels rng0 []
  where
    go [] rng acc = (acc, rng)
    go (r : rs) rng acc =
      let (prs, rng') = drawPairs r n rng Set.empty
       in go rs rng'
            $ ARelPopu
              { popsrc = source r,
                poptgt = target r,
                popdcl = r,
                popps = Set.map (uncurry mkAtomPair) prs
              }
            : acc
    drawPairs r k rng acc
      | Set.size acc >= k = (acc, rng)
      | otherwise =
          let (i, rng1) = randBelow n rng
              (j, rng2) = randBelow n rng1
              p = (benchAtom ci (source r) i, benchAtom ci (target r) j)
           in drawPairs r k rng2 (Set.insert p acc)

-- | One replay transaction: delete a random existing pair, or re-insert a
--   pair deleted earlier in the stream. Draws only values that occur in the
--   real population, so it is type-safe on any model and mirrors the
--   realistic mutation profile. Returns the updated deleted-pool.
pickReplayTx ::
  Map Relation (Set (AAtomValue, AAtomValue)) ->
  [(Relation, (AAtomValue, AAtomValue))] -> -- pool of deleted pairs
  [Relation] ->
  Word64 ->
  (Maybe (Relation, (AAtomValue, AAtomValue), Int), [(Relation, (AAtomValue, AAtomValue))], Word64)
pickReplayTx shadow deletedPool rels rng0 =
  let (coin, rng1) = randBelow 2 rng0
      reinsert = coin == 1 && not (null deletedPool)
   in if reinsert
        then
          let (i, rng2) = randBelow (length deletedPool) rng1
              (r, p) = nth i deletedPool
              pool' = take i deletedPool <> drop (i + 1) deletedPool
           in (Just (r, p, 1), pool', rng2)
        else
          let nonEmpty = [r | r <- rels, not (Set.null (fromMaybe Set.empty (Map.lookup r shadow)))]
           in if null nonEmpty
                then (Nothing, deletedPool, rng1)
                else
                  let (ri, rng2) = randBelow (length nonEmpty) rng1
                      r = nth ri nonEmpty
                      existing = fromMaybe Set.empty (Map.lookup r shadow)
                      (pi', rng3) = randBelow (Set.size existing) rng2
                      p = nth pi' (Set.toAscList existing)
                   in (Just (r, p, -1), (r, p) : deletedPool, rng3)

-- | One random single-pair transaction against a shadow of the current pairs:
--   insert a fresh pair or delete an existing one in a random relation.
--   Returns Nothing when no fresh pair was found in a few tries.
pickTx ::
  Map Relation (Set (AAtomValue, AAtomValue)) ->
  [Relation] ->
  ContextInfo ->
  Int ->
  Word64 ->
  (Maybe (Relation, (AAtomValue, AAtomValue), Int), Text, Word64)
pickTx shadow rels ci n rng0 =
  let (ri, rng1) = randBelow (length rels) rng0
      r = nth ri rels
      existing = fromMaybe Set.empty (Map.lookup r shadow)
      (coin, rng2) = randBelow 2 rng1
      doInsert = coin == 0 || Set.null existing
   in if doInsert
        then
          let tryPair 0 rng = (Nothing, "skip", rng)
              tryPair (k :: Int) rng =
                let (i, rngA) = randBelow n rng
                    (j, rngB) = randBelow n rngA
                    p = (benchAtom ci (source r) i, benchAtom ci (target r) j)
                 in if p `Set.member` existing
                      then tryPair (k - 1) rngB
                      else (Just (r, p, 1), "ins", rngB)
           in tryPair (8 :: Int) rng2
        else
          let (i, rng3) = randBelow (Set.size existing) rng2
              p = nth i (Set.toAscList existing)
           in (Just (r, p, -1), "del", rng3)
