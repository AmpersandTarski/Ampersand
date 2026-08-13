{-# LANGUAGE ScopedTypeVariables #-}

-- | Incremental evaluation of rule-violation terms (DBSP-style, arXiv
--   2203.16684). Terms are compiled into circuits whose nodes hold the state
--   the delta rules need; one 'applyTx' step propagates a transaction's
--   changes through every circuit in time proportional to the change, not the
--   database. Ground truth for the semantics is 'fullContents'
--   ("Ampersand.FSpec.ToFSpec.Populated"); the calculus and its proof
--   obligations live in memorybank\/incremental-evaluation\/delta-calculus.md
--   and proofs\/incremental\/.
--
--   Constructs without a proven delta rule yet (residuals, diamond, relative
--   addition, bare complements, EBin, and identities\/full relations over
--   composite concepts) become 'KFallback' nodes: they re-evaluate their
--   sub-term with 'fullContents' whenever an input changed — correct by
--   construction, at recompute cost. Kleene closures re-run the closure on
--   their incrementally maintained child.
module Ampersand.FSpec.Incremental
  ( IncEngine (..),
    TxDelta (..),
    Circuit (..),
    mkEngine,
    engineInit,
    applyTx,
    popsView,
    violationSets,
    verifyAgainstOracle,
    verifyDetails,
    deepestMismatch,
    circuitFallbacks,
    ZR,
    ZB,
    relToAtomPairs,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.Core.AbstractSyntaxTree (smallerConcepts)
import Ampersand.FSpec.Incremental.ZSet
import Ampersand.FSpec.ToFSpec.Populated (fullContents)
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.Set as Set

type ZR = ZRel AAtomValue

type ZB = ZBag AAtomValue

-- | A transaction: pair deltas per (populated) relation and explicit atom
--   deltas per concept. Weights are +1 (insert) and -1 (delete).
data TxDelta = TxDelta
  { txRel :: !(Map Relation ZR),
    txCpt :: !(Map A_Concept ZB)
  }

-- | A circuit node: its current output (always a set: all weights 1), the
--   term it computes (for diagnostics), and the node-specific state of the
--   delta rule it implements (rule ids refer to delta-calculus.md §4).
data Circuit = Circuit
  { cOut :: !ZR,
    cExpr :: !Expression,
    cKind :: !Kind
  }

data Kind
  = -- | D4 + D6: a base relation; state = pre-distinct sum of feeding populations.
    KRel !Relation ![Relation] !ZR
  | -- | identity over a (plain) concept's population.
    KI !A_Concept
  | -- | full relation between two (plain) concepts' population sets.
    KV !A_Concept !A_Concept
  | -- | a constant singleton (or empty, for the SESSION guard); the content
    --   is emitted as a delta on the first step, then never again.
    KMp1 !(Maybe ZR)
  | -- | D7: re-evaluate the stored term via 'fullContents' when dirty.
    --   Fields: term, its relations, its concepts, recompute-on-any-change.
    KFallback !Expression !(Set Relation) !(Set A_Concept) !Bool
  | -- | D1: transpose (linear).
    KFlp !Circuit
  | -- | D2 + D6: union; state = pre-distinct integral.
    KUni !Circuit !Circuit !ZR
  | -- | D3 + D6: difference; state = pre-distinct integral.
    KDif !Circuit !Circuit !ZR
  | -- | D5: intersection as pointwise product (0\/1 × 0\/1 stays a set).
    KIsc !Circuit !Circuit
  | -- | D5 + D6: composition; state = pre-distinct integral and the flipped
    --   copy of the left child's output (index on the middle atom).
    KCps !Circuit !Circuit !ZR !ZR
  | -- | D5 + D6: cartesian product of the child terms' domain and codomain;
    --   state = weight integrals and carried sets of both projections.
    KPrd !Circuit !Circuit !ZB !ZB !ZB !ZB
  | -- | D7: reflexive-transitive closure over the child's maintained output.
    KKl0 !Circuit !A_Concept
  | -- | D7: transitive closure over the child's maintained output.
    KKl1 !Circuit

-- | The engine: all conjunct circuits plus the global population state.
data IncEngine = IncEngine
  { ieCI :: !ContextInfo,
    -- | (conjunct id, violation term, circuit)
    ieCircuits :: ![(Text, Expression, Circuit)],
    -- | current pairs per populated relation (the raw database)
    ieRelPairs :: !(Map Relation (Set (AAtomValue, AAtomValue))),
    -- | current explicitly populated atoms per concept
    ieCptAtoms :: !(Map A_Concept (Set AAtomValue)),
    -- | occurrence-count integral per plain concept (derived population)
    ieCptOcc :: !(Map A_Concept ZB),
    -- | carried set per plain concept: distinct of the occurrence integral
    ieCptSet :: !(Map A_Concept ZB),
    -- | term relation -> populated relations feeding it (pairsOf semantics)
    ieFeeders :: !(Map Relation [Relation]),
    -- | populated relation -> concepts fed by its left resp. right atoms
    ieCptFeed :: !(Map Relation ([A_Concept], [A_Concept])),
    -- | explicitly populated concept -> concepts fed (itself and larger)
    ieCptUp :: !(Map A_Concept [A_Concept]),
    -- | all plain concepts (population bookkeeping domain)
    ieAllCpts :: ![A_Concept]
  }

isPlain :: A_Concept -> Bool
isPlain PlainConcept {} = True
isPlain ONE = True
isPlain _ = False

-- | Compile a violation term into a circuit (empty state; run 'engineInit'
--   afterwards). The rewrites mirror delta-calculus.md §2: complements are
--   pushed inward (De Morgan, double negation) and absorbed into differences;
--   whatever keeps a bare complement — or is a residual, diamond, relative
--   addition, or EBin — becomes a fallback node.
compileTerm :: ContextInfo -> Expression -> Circuit
compileTerm ci = build . simplify
  where
    build :: Expression -> Circuit
    build e = case e of
      EBrk a -> build a
      _ -> buildNode e
    buildNode :: Expression -> Circuit
    buildNode e = case e of
      EUni (a, b) -> mk e (KUni (build a) (build b) relEmpty)
      EIsc (a, b) -> case (a, b) of
        -- a ∩ -b = a − b only when a's window fits inside b's complement
        -- window (typed complements!): sign a ≤ sign b'.
        (ECpl a', _) | signLeq (sign b) (sign a') -> mk e (KDif (build b) (build a') relEmpty)
        (_, ECpl b') | signLeq (sign a) (sign b') -> mk e (KDif (build a) (build b') relEmpty)
        _ -> mk e (KIsc (build a) (build b))
      EDif (a, b) -> case b of
        -- a − (-b) = a ∩ b only under the same signature guard.
        ECpl b' | signLeq (sign a) (sign b') -> mk e (KIsc (build a) (build b'))
        _ -> mk e (KDif (build a) (build b) relEmpty)
      ECps (a, b) -> mk e (KCps (build a) (build b) relEmpty relEmpty)
      EPrd (a, b) -> mk e (KPrd (build a) (build b) Map.empty Map.empty Map.empty Map.empty)
      EFlp a -> mk e (KFlp (build a))
      EBrk a -> build a
      EKl0 a
        | source a == target a && isPlain (source a) -> mk e (KKl0 (build a) (source a))
        | otherwise -> fallback e
      EKl1 a
        | source a == target a -> mk e (KKl1 (build a))
        | otherwise -> fallback e
      EDcD r -> mk e (KRel r [] relEmpty)
      EDcI c
        | isPlain c -> mk e (KI c)
        | otherwise -> fallback e
      EDcV sgn
        | isPlain (source sgn) && isPlain (target sgn) -> mk e (KV (source sgn) (target sgn))
        | otherwise -> fallback e
      EMp1 val c ->
        -- mirrors fullContents' EMp1 case, including the SESSION guard
        let content =
              if isSESSION c && tshow val == tshow ("_SESSION" :: Text)
                then Map.empty
                else
                  let av = safePSingleton2AAtomVal ci c val
                   in Map.singleton av (Map.singleton av 1)
         in mk e (KMp1 (Just content))
      _ -> fallback e
    fallback e =
      let cpts = Set.filter isPlain (concs e)
          conservative = not (all isPlain (Set.toList (concs e)))
       in mk e (KFallback e (bindedRelationsIn e) cpts conservative)
    mk :: Expression -> Kind -> Circuit
    mk = Circuit relEmpty
    relEmpty = Map.empty

-- | Push complements inward and rewrite derived operators, mirroring
--   'fullContents' (see delta-calculus.md §2 for the derivations).
--
--   Complements are TYPED (relative to the @V@ of a signature), so De Morgan
--   and double negation must track the signature of the complement being
--   pushed: @pushNeg S e@ denotes @V[S] − e@. Double negation
--   @-(-x)@ over @S@ collapses to @x@ only when @sign x = S@; otherwise the
--   ring @V[S] − V[sign x]@ survives. The oracle test on
--   testing\/Travis\/testcases\/prototype\/shouldSucceed\/specializeTest.adl
--   catches exactly this.
simplify :: Expression -> Expression
simplify expr = case expr of
  EEqu (l, r) -> simplify ((l .|-. r) .\/. (r .|-. l))
  EInc (l, r) -> simplify (notCpl l .\/. r)
  ECpl e -> pushNeg (sign e) e
  EBrk e -> simplify e
  EUni (a, b) -> EUni (simplify a, simplify b)
  EIsc (a, b) -> EIsc (simplify a, simplify b)
  EDif (a, b) -> EDif (simplify a, simplify b)
  ECps (a, b) -> ECps (simplify a, simplify b)
  EPrd (a, b) -> EPrd (simplify a, simplify b)
  EFlp a -> EFlp (simplify a)
  EKl0 a -> EKl0 (simplify a)
  EKl1 a -> EKl1 (simplify a)
  _ -> expr
  where
    -- @pushNeg s e@ = the complement of e relative to V[s]. Invariant of every
    -- recursive call: sign e ≤ s (holds because sub-term signatures never
    -- exceed their parent's join signature).
    pushNeg :: Signature -> Expression -> Expression
    pushNeg s e = case e of
      ECpl x
        | sign x == s -> simplify x
        | otherwise -> EUni (EDif (EDcV s, EDcV (sign x)), simplify x)
      EBrk x -> pushNeg s x
      EUni (a, b) -> EIsc (pushNeg s a, pushNeg s b)
      EIsc (a, b) -> EUni (pushNeg s a, pushNeg s b)
      -- V[s] − (a−b) = (V[s] − a) ∪ b, valid because b ⊆ V[sign b] ⊆ V[s]
      EDif (a, b) -> EUni (pushNeg s a, simplify b)
      EFlp x -> EFlp (pushNeg (flipSign s) x)
      EEqu (l, r) -> pushNeg s ((l .|-. r) .\/. (r .|-. l))
      EInc (l, r) -> pushNeg s (notCpl l .\/. r)
      _
        | sign e == s -> ECpl (simplify e)
        | otherwise -> EDif (EDcV s, simplify e)
    flipSign :: Signature -> Signature
    flipSign sgn = Sign (target sgn) (source sgn)

-- | Concept ordering: @c1 ≤ c2@ when c1 is c2 or one of its specializations.
cptLeq :: A_Concept -> A_Concept -> Bool
cptLeq c1 c2 = c1 == c2 || c1 `elem` smallerConcepts c2

-- | Window inclusion: @V[s1] ⊆ V[s2]@ (population-wise, since concept
--   populations include those of smaller concepts).
signLeq :: Signature -> Signature -> Bool
signLeq s1 s2 = source s1 `cptLeq` source s2 && target s1 `cptLeq` target s2

-- | Build an engine (state still empty; call 'engineInit').
mkEngine :: ContextInfo -> [A_Concept] -> [(Text, Expression)] -> IncEngine
mkEngine ci allConcepts terms =
  IncEngine
    { ieCI = ci,
      ieCircuits = [(nm, term, wireFeeders (compileTerm ci term)) | (nm, term) <- terms],
      ieRelPairs = Map.empty,
      ieCptAtoms = Map.empty,
      ieCptOcc = Map.empty,
      ieCptSet = Map.empty,
      ieFeeders = Map.empty, -- filled per transaction domain below
      ieCptFeed = Map.empty,
      ieCptUp = Map.empty,
      ieAllCpts = plains
    }
  where
    plains = L.nub (filter isPlain allConcepts <> [ONE])
    wireFeeders = id -- feeders are resolved against the populated relations at init

-- | Initialize: register the populated relations (fixing the feeder maps) and
--   run the initial population through the circuits as one big first
--   transaction — the backfill is the same code path as any other step.
engineInit :: IncEngine -> [Population] -> IncEngine
engineInit eng pops = fst (applyTx engWired tx0)
  where
    popRels = L.nub [popdcl p | p@ARelPopu {} <- pops]
    termRels =
      Set.toList . Set.unions $ [bindedRelationsIn term | (_, term, _) <- ieCircuits eng]
    engWired =
      eng
        { ieFeeders =
            Map.fromList
              [ ( r,
                  [ d | d <- popRels, name d == name r, source d `elem` (source r : smallerConcepts (source r)), target d `elem` (target r : smallerConcepts (target r))
                  ]
                )
                | r <- L.nub (termRels <> popRels)
              ],
          ieCptFeed =
            Map.fromList
              [ ( d,
                  ( [c | c <- ieAllCpts eng, c /= ONE, source d `elem` (c : smallerConcepts c)],
                    [c | c <- ieAllCpts eng, c /= ONE, target d `elem` (c : smallerConcepts c)]
                  )
                )
                | d <- popRels
              ],
          ieCptUp =
            Map.fromList
              [ (c, [c' | c' <- ieAllCpts eng, c' /= ONE, c `elem` (c' : smallerConcepts c')])
                | c <- L.nub [popcpt p | p@ACptPopu {} <- pops]
              ],
          ieCircuits =
            [ (nm, term, resolveFeeders circ) | (nm, term, circ) <- ieCircuits eng
            ]
        }
      where
        resolveFeeders c = c {cKind = go (cKind c)}
          where
            go k = case k of
              KRel r _ z ->
                KRel r [d | d <- popRels, name d == name r, source d `elem` (source r : smallerConcepts (source r)), target d `elem` (target r : smallerConcepts (target r))] z
              KFlp a -> KFlp (resolveFeeders a)
              KUni a b z -> KUni (resolveFeeders a) (resolveFeeders b) z
              KDif a b z -> KDif (resolveFeeders a) (resolveFeeders b) z
              KIsc a b -> KIsc (resolveFeeders a) (resolveFeeders b)
              KCps a b z f -> KCps (resolveFeeders a) (resolveFeeders b) z f
              KPrd a b p q s t -> KPrd (resolveFeeders a) (resolveFeeders b) p q s t
              KKl0 a cpt -> KKl0 (resolveFeeders a) cpt
              KKl1 a -> KKl1 (resolveFeeders a)
              _ -> k
    -- populations are merged set-wise per relation/concept first (mirroring
    -- initialpopsDefinedInScript), so tx0 carries weight 1 per pair and the
    -- raw-pair sets and the KRel integrals stay in lockstep.
    -- ONE's singleton population travels through tx0 like any other delta:
    -- pre-seeding it in 'mkEngine' instead would leave the base state of
    -- I[ONE]/V[..*ONE] circuits out of sync with their semantics (their
    -- constant content would never be emitted), which is exactly the base
    -- case of the circuit invariant in proofs/incremental/Circuit.thy.
    tx0 =
      TxDelta
        { txRel =
            Map.map (\prs -> relFromPairs [(pr, 1) | pr <- Set.toList prs])
              $ Map.fromListWith
                Set.union
                [ (popdcl p, Set.map (\pr -> (apLeft pr, apRight pr)) (popps p))
                  | p@ARelPopu {} <- pops
                ],
          txCpt =
            Map.map (Map.fromSet (const 1))
              $ Map.fromListWith
                Set.union
                ( (ONE, Set.singleton AtomValueOfONE)
                    : [ (popcpt p, Set.fromList (popas p))
                        | p@ACptPopu {} <- pops
                      ]
                )
        }

-- | The current population, in the shape 'fullContents' consumes. O(#relations).
popsView :: IncEngine -> [Population]
popsView eng =
  [ ARelPopu {popsrc = source d, poptgt = target d, popdcl = d, popps = Set.map (uncurry mkAtomPair) prs}
    | (d, prs) <- Map.toList (ieRelPairs eng)
  ]
    <> [ ACptPopu {popcpt = c, popas = Set.toList atoms}
         | (c, atoms) <- Map.toList (ieCptAtoms eng)
       ]

-- | One transaction step. Returns the new engine and, per conjunct, the delta
--   of its violation set.
applyTx :: IncEngine -> TxDelta -> (IncEngine, [(Text, ZR)])
applyTx eng tx = (eng', deltas)
  where
    -- population bookkeeping (linear, D4)
    occDelta :: Map A_Concept ZB
    occDelta =
      Map.filter (not . Map.null)
        $ Map.unionsWith
          bagApply
          ( [ -- fromListWith: a concept can occur in BOTH lists (e.g. r[G*G]
              -- feeds G from the left and the right); the weights must add.
              Map.fromListWith bagApply ([(c, lw) | c <- lcs] <> [(c, rw) | c <- rcs])
              | (d, dz) <- Map.toList (txRel tx),
                let (lcs, rcs) = Map.findWithDefault ([], []) d (ieCptFeed eng),
                let lw = rowWeights dz,
                let rw = colWeights dz,
                not (Map.null lw && Map.null rw)
            ]
              <> [ Map.fromList [(c', dz) | c' <- Map.findWithDefault [c] c (ieCptUp eng)]
                   | (c, dz) <- Map.toList (txCpt tx)
                 ]
          )
    cptSetDelta :: Map A_Concept ZB
    cptSetDelta =
      Map.filter (not . Map.null)
        $ Map.mapWithKey
          (\c dz -> bagH (Map.findWithDefault Map.empty c (ieCptOcc eng)) dz)
          occDelta
    feedDelta :: Map Relation ZR
    feedDelta =
      Map.filter (not . relNull)
        $ Map.fromList
          [ (r, foldl' relApply Map.empty [Map.findWithDefault Map.empty d (txRel tx) | d <- ds])
            | (r, ds) <- Map.toList (ieFeeders eng),
              any (`Map.member` txRel tx) ds
          ]
    newRelPairs = foldl' updRel (ieRelPairs eng) (Map.toList (txRel tx))
      where
        updRel acc (d, dz) =
          Map.alter
            ( \old ->
                let s = foldl' updPair (fromMaybe Set.empty old) [((x, y), w) | (x, row) <- Map.toList dz, (y, w) <- Map.toList row]
                 in if Set.null s then Nothing else Just s
            )
            d
            acc
        updPair s (p, w) = if w > 0 then Set.insert p s else Set.delete p s
    newCptAtoms = foldl' updCpt (ieCptAtoms eng) (Map.toList (txCpt tx))
      where
        updCpt acc (c, dz) =
          Map.alter
            ( \old ->
                let s = foldl' updAtom (fromMaybe Set.empty old) (Map.toList dz)
                 in if Set.null s then Nothing else Just s
            )
            c
            acc
        updAtom s (a, w) = if w > 0 then Set.insert a s else Set.delete a s
    newOcc = Map.foldlWithKey' (\acc c dz -> Map.alter (Just . (`bagApply` dz) . fromMaybe Map.empty) c acc) (ieCptOcc eng) occDelta
    newCptSet = Map.foldlWithKey' (\acc c dz -> Map.alter (Just . (`bagApply` dz) . fromMaybe Map.empty) c acc) (ieCptSet eng) cptSetDelta
    engMid = eng {ieRelPairs = newRelPairs, ieCptAtoms = newCptAtoms, ieCptOcc = newOcc, ieCptSet = newCptSet}
    env =
      StepEnv
        { seCI = ieCI eng,
          sePops = popsView engMid,
          seFeed = feedDelta,
          seCptSetOld = ieCptSet eng,
          seCptSetNew = newCptSet,
          seCptSetDelta = cptSetDelta,
          seDirtyRels = Map.keysSet feedDelta,
          seDirtyCpts = Map.keysSet cptSetDelta,
          seAnyDirty = not (Map.null feedDelta && Map.null cptSetDelta)
        }
    stepped = [(nm, term, step env circ) | (nm, term, circ) <- ieCircuits eng]
    deltas = [(nm, d) | (nm, _, (_, d)) <- stepped]
    eng' = engMid {ieCircuits = [(nm, term, c') | (nm, term, (c', _)) <- stepped]}

data StepEnv = StepEnv
  { seCI :: !ContextInfo,
    sePops :: ![Population],
    seFeed :: !(Map Relation ZR),
    seCptSetOld :: !(Map A_Concept ZB),
    seCptSetNew :: !(Map A_Concept ZB),
    seCptSetDelta :: !(Map A_Concept ZB),
    seDirtyRels :: !(Set Relation),
    seDirtyCpts :: !(Set A_Concept),
    seAnyDirty :: !Bool
  }

cptOld, cptNew, cptDelta :: StepEnv -> A_Concept -> ZB
cptOld env c = Map.findWithDefault Map.empty c (seCptSetOld env)
cptNew env c = Map.findWithDefault Map.empty c (seCptSetNew env)
cptDelta env c = Map.findWithDefault Map.empty c (seCptSetDelta env)

-- | Propagate a step through one circuit: returns the updated circuit and the
--   delta of its output set. The bilinear rules use the exact asymmetric
--   expansion @Δ(a⊗b) = Δa ⊗ old(b) + new(a) ⊗ Δb@ (obligations Z2-Z4); the
--   distinct states emit via the zero-crossing H (obligation Z5).
step :: StepEnv -> Circuit -> (Circuit, ZR)
step env (Circuit out expr kind) = case kind of
  KRel _ feeders zInt ->
    let dz = foldl' relApply Map.empty [Map.findWithDefault Map.empty d (seFeed env) | d <- feeders, d `Set.member` seDirtyRels env]
     in if relNull dz
          then noChange
          else
            let dOut = relH zInt dz
             in (Circuit (relApply out dOut) expr (replaceZ (relApply zInt dz)), dOut)
  KI c ->
    let dz = diagRel (cptDelta env c)
     in emitLinear dz
  KV a b ->
    let da = cptDelta env a
        db = cptDelta env b
     in if Map.null da && Map.null db
          then noChange
          else
            let dOut = relApply (bagProd da (cptOld env b)) (bagProd (cptNew env a) db)
             in emitLinear dOut
  KMp1 pending -> case pending of
    Nothing -> noChange
    Just content
      | relNull content -> (Circuit out expr (KMp1 Nothing), Map.empty)
      | otherwise -> (Circuit (relApply out content) expr (KMp1 Nothing), content)
  KFallback e rels cpts conservative ->
    let dirty =
          (conservative && seAnyDirty env)
            || any (`Set.member` seDirtyRels env) (Set.toList rels)
            || any (`Set.member` seDirtyCpts env) (Set.toList cpts)
     in if not dirty
          then noChange
          else
            let newOut = relFromAtomPairs (fullContents (seCI env) (sePops env) e)
                dOut = relDiff newOut out
             in (Circuit newOut expr kind, dOut)
  KFlp a ->
    let (a', da) = step env a
        dOut = relFlip da
     in (Circuit (relApply out dOut) expr (KFlp a'), dOut)
  KUni a b zInt ->
    let (a', da) = step env a
        (b', db) = step env b
        dz = relApply da db
     in if relNull dz
          then (Circuit out expr (KUni a' b' zInt), Map.empty)
          else
            let dOut = relH zInt dz
             in (Circuit (relApply out dOut) expr (KUni a' b' (relApply zInt dz)), dOut)
  KDif a b zInt ->
    let (a', da) = step env a
        (b', db) = step env b
        dz = relApply da (relNeg db)
     in if relNull dz
          then (Circuit out expr (KDif a' b' zInt), Map.empty)
          else
            let dOut = relH zInt dz
             in (Circuit (relApply out dOut) expr (KDif a' b' (relApply zInt dz)), dOut)
  KIsc a b ->
    let (a', da) = step env a
        bOld = cOut b
        (b', db) = step env b
        dOut = relApply (pointwiseDelta da bOld) (pointwiseDelta db (cOut a'))
     in (Circuit (relApply out dOut) expr (KIsc a' b'), dOut)
  KCps a b zInt flipA ->
    let (a', da) = step env a
        bOld = cOut b
        (b', db) = step env b
        flipA' = if relNull da then flipA else relApply flipA (relFlip da)
        dz = relApply (composeDeltaOld da bOld) (composeFlipDelta flipA' db)
     in if relNull dz
          then (Circuit out expr (KCps a' b' zInt flipA'), Map.empty)
          else
            let dOut = relH zInt dz
             in (Circuit (relApply out dOut) expr (KCps a' b' (relApply zInt dz) flipA'), dOut)
  KPrd a b domW domS codW codS ->
    let (a', da) = step env a
        (b', db) = step env b
        dDomW = rowWeights da
        dCodW = colWeights db
        dDomS = bagH domW dDomW
        dCodS = bagH codW dCodW
        domW' = bagApply domW dDomW
        codW' = bagApply codW dCodW
        domS' = bagApply domS dDomS
        codS' = bagApply codS dCodS
        dOut = relApply (bagProd dDomS codS) (bagProd domS' dCodS)
     in (Circuit (relApply out dOut) expr (KPrd a' b' domW' domS' codW' codS'), dOut)
  KKl0 a src ->
    let (a', da) = step env a
        popDirty = not (Map.null (cptDelta env src))
     in if relNull da && not popDirty
          then (Circuit out expr (KKl0 a' src), Map.empty)
          else
            let base = Map.unionWith (Map.unionWith (+)) (cOut a') (diagRel (cptNew env src))
                newOut = closureOf base
                dOut = relDiff newOut out
             in (Circuit newOut expr (KKl0 a' src), dOut)
  KKl1 a ->
    let (a', da) = step env a
     in if relNull da
          then (Circuit out expr (KKl1 a'), Map.empty)
          else
            let newOut = closureOf (cOut a')
                dOut = relDiff newOut out
             in (Circuit newOut expr (KKl1 a'), dOut)
  where
    noChange = (Circuit out expr kind, Map.empty)
    emitLinear dOut
      | relNull dOut = noChange
      | otherwise = (Circuit (relApply out dOut) expr kind, dOut)
    replaceZ z = case kind of
      KRel r fs _ -> KRel r fs z
      _ -> kind

-- | Closure via the same 'transClosureMap' the oracle uses, on the carried sets.
closureOf :: ZR -> ZR
closureOf =
  Map.map (Map.fromSet (const 1))
    . transClosureMap
    . Map.filter (not . Set.null)
    . Map.map (Map.keysSet . Map.filter (> 0))

relFromAtomPairs :: AAtomPairs -> ZR
relFromAtomPairs ps = relFromPairs [((apLeft p, apRight p), 1) | p <- Set.toList ps]

relToAtomPairs :: ZR -> AAtomPairs
relToAtomPairs = Set.fromList . map (uncurry mkAtomPair) . relToPairs

-- | The maintained violation set per conjunct.
violationSets :: IncEngine -> [(Text, AAtomPairs)]
violationSets eng = [(nm, relToAtomPairs (cOut c)) | (nm, _, c) <- ieCircuits eng]

-- | Compare every circuit against a fresh 'fullContents' evaluation of its
--   term over the current population. Returns the conjuncts that disagree,
--   with the term and (incremental size, oracle size).
verifyAgainstOracle :: IncEngine -> [(Text, Expression, Int, Int)]
verifyAgainstOracle eng = [(nm, term, i, o) | (nm, term, i, o, _, _) <- verifyDetails eng]

-- | Like 'verifyAgainstOracle', but per disagreeing conjunct it reports the
--   DEEPEST disagreeing sub-circuit: its term, sizes, and sample pairs the
--   incremental result misses resp. has too many.
verifyDetails :: IncEngine -> [(Text, Expression, Int, Int, [AAtomPair], [AAtomPair])]
verifyDetails eng =
  [ (nm, e, i, o, missing, extra)
    | (nm, term, c) <- ieCircuits eng,
      relToAtomPairs (cOut c) /= fullContents (ieCI eng) pops term,
      let (e, i, o, missing, extra) =
            fromMaybe
              -- top-level term and circuit term differ semantically
              ( term,
                Set.size (relToAtomPairs (cOut c)),
                Set.size (fullContents (ieCI eng) pops term),
                [],
                []
              )
              (deepestMismatch eng c)
  ]
  where
    pops = popsView eng

-- | How many nodes of each circuit run in fallback (recompute) mode — the
--   coverage measure of the delta calculus.
circuitFallbacks :: IncEngine -> [(Text, Int, Int)]
circuitFallbacks eng = [(nm, countAll c, countFb c) | (nm, _, c) <- ieCircuits eng]
  where
    countAll c = 1 + sum (map countAll (childrenOf c))
    countFb c =
      (case cKind c of KFallback {} -> 1; _ -> 0) + sum (map countFb (childrenOf c))

childrenOf :: Circuit -> [Circuit]
childrenOf c = case cKind c of
  KFlp a -> [a]
  KUni a b _ -> [a, b]
  KDif a b _ -> [a, b]
  KIsc a b -> [a, b]
  KCps a b _ _ -> [a, b]
  KPrd a b _ _ _ _ -> [a, b]
  KKl0 a _ -> [a]
  KKl1 a -> [a]
  _ -> []

-- | Diagnostic: the deepest sub-circuit whose maintained output differs from
--   a fresh oracle evaluation of its own term, with sample pairs (missing
--   from resp. extra in the incremental result).
deepestMismatch :: IncEngine -> Circuit -> Maybe (Expression, Int, Int, [AAtomPair], [AAtomPair])
deepestMismatch eng = go
  where
    pops = popsView eng
    go c =
      case mapMaybe go (childrenOf c) of
        (m : _) -> Just m
        [] ->
          let incSet = relToAtomPairs (cOut c)
              oraSet = fullContents (ieCI eng) pops (cExpr c)
           in if incSet == oraSet
                then Nothing
                else
                  Just
                    ( cExpr c,
                      Set.size incSet,
                      Set.size oraSet,
                      take 3 (Set.toList (oraSet `Set.difference` incSet)),
                      take 3 (Set.toList (incSet `Set.difference` oraSet))
                    )
