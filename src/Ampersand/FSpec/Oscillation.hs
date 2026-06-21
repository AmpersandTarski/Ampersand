-- | Stage 1 of the oscillation-risk analysis: a signed, refined rule-level
--   triggering graph over the ExecEngine's automated rules.
--
--   Background. The ExecEngine repairs violations of automated rules by running
--   their @VIOLATION@ scripts of @{EX}@ primitives. When two (or more) such rules
--   keep undoing each other's repairs, the loop never reaches a fixpoint and the
--   runtime aborts with /Maximum reruns exceeded/ — an *oscillation*. See
--   @docs/guides/oscillations/README.md@ for a worked case.
--
--   We cannot decide oscillation exactly (it is undecidable, just like chase
--   termination). The honest goal is a *sound over-approximation*: certify a rule
--   set as safe only when it provably terminates, and otherwise emit an
--   explanatory risk. False positives (a safe rule set flagged) are acceptable;
--   false negatives (a real oscillation missed) are to be avoided.
--
--   The analysis (this module).
--
--     * Nodes are the automated rules (maintained by the ExecEngine role).
--     * There is an edge @A -> B@ when a write in A's repair can create a fresh
--       violation of B. The edge is labelled *negative* when that write deletes
--       or merges (a non-monotone operation), and *positive* when it only inserts.
--     * /Refinement (monotonicity)./ A write does not blindly create an edge to
--       every rule that mentions the relation: it must be able to *increase* B's
--       set of violations. We compute, by a standard polarity walk over the
--       relation-algebra term, the sign(s) with which each relation occurs in B's
--       violation set @antecedent - consequent@. An insert can re-trigger B only
--       where the relation occurs positively; a delete only where it occurs
--       negatively. This prunes the spurious edges of a bare triggering graph
--       (e.g. a uniqueness/merge rule does not spuriously trigger itself).
--     * A cycle through a *negative* edge is the structural signature of
--       oscillation risk: somewhere on the cycle a repair removes what another
--       restores, so the combined repair operator is non-monotone and need not
--       converge. A purely positive (insert-only) cycle always converges to a
--       least fixpoint (Knaster–Tarski) and is therefore *not* flagged.
--
--   We report one warning per strongly-connected component that contains at least
--   one internal negative edge, naming the colliding rules and the relations on
--   which the opposing writes meet — the static twin of the runtime's
--   "Rules fixed in last run" message.
--
--   Design choices and their justification are documented in
--   @docs/ongoing-research/making-oscillation-risk-visible.md@.
module Ampersand.FSpec.Oscillation
  ( warnOscillationRisk,
    -- exposed for testing
    oscillationWarnings,
  )
where

import Ampersand.Basics
import Ampersand.Classes (bindedRelationsIn)
import Ampersand.Core.AbstractSyntaxTree
  ( Expression (..),
    Guarded,
    Relation,
    Rule,
    Warning,
    formalExpression,
    rrfps,
    rrviol,
    source,
    target,
  )
import Ampersand.Core.ParseTree
  ( PairView (..),
    PairViewSegment (..),
  )
import Ampersand.FSpec.FSpec (FSpec (..))
import Ampersand.Input.ADL1.CtxError (addWarnings, mkOscillationWarning)
import qualified Data.Graph as G
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified RIO.Text.Partial as Partial

-- | Sign of a write, of a relation occurrence, or of a triggering edge.
--   @Pos@ is monotone (insert / grow), @Neg@ is non-monotone (delete / shrink).
data Sign = Pos | Neg deriving (Eq, Ord, Show)

opp :: Sign -> Sign
opp Pos = Neg
opp Neg = Pos

-- | A modification performed by a rule's repair script: a signed write to a
--   named relation. The optional source/target concept names (supplied by
--   @InsPair@/@DelPair@) disambiguate overloaded relation names.
data Write = WriteRel !Sign !Text !(Maybe Text) !(Maybe Text)

-- | A negative (non-monotone) triggering edge, kept for the report.
data NegEdge = NegEdge
  { -- | rule whose repair does the opposing write
    neFrom :: !Text,
    -- | rule whose violation set the write can grow
    neTo :: !Text,
    -- | the relation on which they collide
    neShared :: !Text
  }

-- | Run the Stage-1 analysis and accumulate a 'Warning' for each detected risk
--   in the 'Guarded' pipeline. Hooked into 'pCtx2Fspec'.
warnOscillationRisk :: FSpec -> Guarded ()
warnOscillationRisk fSpec = addWarnings (oscillationWarnings fSpec) (pure ())

-- | The list of oscillation-risk warnings for an 'FSpec'. Pure, so it is
--   straightforward to test directly.
oscillationWarnings :: FSpec -> [Warning]
oscillationWarnings fSpec =
  [ mkOscillationWarning
      (rrfps (NE.head comp))
      (map fullName (NE.toList comp))
      [ neFrom e <> "'s repair deletes or merges " <> neShared e <> ", which is read by " <> neTo e
        | e <- negEdgesOf comp
      ]
    | comp <- riskyComponents
  ]
  where
    -- Automated rules, deterministically ordered so warnings are stable.
    rules :: [Rule]
    rules = L.sortOn fullName (execEngineRules fSpec)

    indexed :: [(Int, Rule)]
    indexed = zip [0 ..] rules

    -- Relations read by any automated rule. Only these can carry edges, so an
    -- atom-level merge/delete need only be expanded over this set.
    readRels :: [Relation]
    readRels = Set.toList (Set.unions [bindedRelationsIn (formalExpression r) | (_, r) <- indexed])

    -- Per rule index: the writes its repair performs.
    writesByIdx :: [(Int, [Write])]
    writesByIdx = [(i, repairWrites readRels r) | (i, r) <- indexed]

    -- Per rule index: the signed occurrences of each relation in its violation
    -- set. A relation present with @Pos@ can be re-triggered by an insert, one
    -- present with @Neg@ by a delete.
    polByIdx :: [(Int, Map.Map Relation (Set.Set Sign))]
    polByIdx = [(i, relationPolarities (formalExpression r)) | (i, r) <- indexed]

    -- All triggering edges, each carrying its sign and the relation involved.
    edges :: [(Int, Int, Sign, Text)]
    edges =
      [ (i, j, sgn, shared)
        | (i, ws) <- writesByIdx,
          (j, pol) <- polByIdx,
          w <- ws,
          (sgn, shared) <- triggerOf w pol
      ]

    -- Strongly-connected components of the graph, as non-empty lists of rules.
    components :: [NE.NonEmpty Rule]
    components =
      [ comp
        | scc <- G.stronglyConnComp [(i, i, succsOf i) | (i, _) <- indexed],
          Just comp <- [sccToRules scc]
      ]
      where
        succsOf i = L.nub [j | (i', j, _, _) <- edges, i' == i]
        sccToRules (G.CyclicSCC is) = NE.nonEmpty [r | (i, r) <- indexed, i `elem` is]
        sccToRules (G.AcyclicSCC _) = Nothing -- no cycle ⇒ guaranteed to terminate

    -- A component is risky iff it contains at least one internal negative edge.
    riskyComponents :: [NE.NonEmpty Rule]
    riskyComponents = [comp | comp <- components, not (null (negEdgesOf comp))]

    -- Internal negative edges of a component, rendered for the report.
    negEdgesOf :: NE.NonEmpty Rule -> [NegEdge]
    negEdgesOf comp =
      L.nubBy
        sameEdge
        [ NegEdge (fullName (ruleAt i)) (fullName (ruleAt j)) shared
          | (i, j, Neg, shared) <- edges,
            ruleAt i `elem` comp,
            ruleAt j `elem` comp
        ]
      where
        sameEdge a b = (neFrom a, neTo a, neShared a) == (neFrom b, neTo b, neShared b)

    ruleAt :: Int -> Rule
    ruleAt i = case lookup i indexed of
      Just r -> r
      Nothing -> fatal "oscillationWarnings: rule index out of range"

-- | Does write @w@ create a possible new violation of a rule with the given
--   relation-polarity map? An insert (@Pos@) re-triggers where the relation
--   occurs positively; a delete (@Neg@) where it occurs negatively. Returns the
--   edge sign (= the write's sign) and a description for every match.
triggerOf :: Write -> Map.Map Relation (Set.Set Sign) -> [(Sign, Text)]
triggerOf (WriteRel sgn nm mSrc mTgt) pol =
  [ (sgn, "relation " <> nm)
    | (rel, signs) <- Map.toList pol,
      matchesRel rel,
      sgn `Set.member` signs
  ]
  where
    matchesRel rel =
      fullName rel
        == nm
        && maybe True (== fullName (source rel)) mSrc
        && maybe True (== fullName (target rel)) mTgt

-- | The automated rules of an 'FSpec': those maintained by the ExecEngine role.
execEngineRules :: FSpec -> [Rule]
execEngineRules fSpec =
  L.nub
    [ rule
      | (role', rule) <- fRoleRuls fSpec,
        fullName role' == fullName nameOfExecEngineRole
    ]

-- * Repair scripts → writes

-- | The writes a rule's @VIOLATION@ repair script performs, parsed from its
--   @{EX}@ instructions. The classification follows the guide:
--
--     * @InsPair@ inserts into a relation (positive);
--     * @DelPair@ deletes from a relation (negative);
--     * @DelAtom;C@ deletes an atom and so its pairs in every relation on @C@
--       (negative on each such relation);
--     * @MrgAtoms;C@ merges two atoms, which both removes and re-routes pairs in
--       every relation on @C@ (so both a negative and a positive write on each);
--     * @InsAtom;C@/@NewStruct;C@ only create, so they are monotone — modelled as
--       positive writes on the relations on @C@ (a bare new atom that links to
--       nothing cannot grow another rule's violation set through a relation).
--
--   Relation and concept *names* always appear as literal text in a @{EX}@
--   instruction (they can never be computed from an expression), so reading them
--   from the 'PairView' text segments is robust even though atom values may be
--   expression segments. @relsOn@ resolves a concept name to the relations on it
--   that are actually read by some automated rule.
repairWrites :: [Relation] -> Rule -> [Write]
repairWrites readRels rule = case rrviol rule of
  Nothing -> []
  Just pv -> concatMap parseInstr (exInstructions pv)
  where
    relsOn :: Text -> [Relation]
    relsOn c = [d | d <- readRels, fullName (source d) == c || fullName (target d) == c]

    relW :: Sign -> Relation -> Write
    relW s d = WriteRel s (fullName d) (Just (fullName (source d))) (Just (fullName (target d)))

    parseInstr :: [Text] -> [Write]
    parseInstr ts = case ts of
      (fn : rest) -> case fn of
        "InsPair" -> insDelPair Pos rest
        "DelPair" -> insDelPair Neg rest
        "InsAtom" -> [relW Pos d | d <- onCpt rest]
        "NewStruct" -> [relW Pos d | d <- onCpt rest]
        "DelAtom" -> [relW Neg d | d <- onCpt rest]
        "MrgAtoms" -> concat [[relW Pos d, relW Neg d] | d <- onCpt rest]
        _ -> [] -- not a population-changing primitive (or plain text)
      [] -> []
      where
        onCpt (c : _) = relsOn c
        onCpt [] = []

    -- InsPair/DelPair: relation ; srcConcept ; srcAtom ; tgtConcept ; tgtAtom
    insDelPair :: Sign -> [Text] -> [Write]
    insDelPair s (rel : src : _ : tgt : _) = [WriteRel s rel (Just src) (Just tgt)]
    insDelPair s (rel : _) = [WriteRel s rel Nothing Nothing]
    insDelPair _ [] = []

-- | Split a violation 'PairView' into its @{EX}@ instructions, each as a list of
--   @;@-separated, trimmed fields. Expression segments (the @SRC@/@TGT@ atom
--   values) are flattened to the empty string; they never carry a function or
--   relation name, and instruction boundaries are recovered by splitting on the
--   @{EX}@ marker.
exInstructions :: PairView Expression -> [[Text]]
exInstructions (PairView segs) =
  [ map T.strip (Partial.splitOn ";" (T.strip instr))
    | instr <- drop 1 (Partial.splitOn "{EX}" flat) -- drop the text before the first {EX}
  ]
  where
    flat = T.concat (map segText (NE.toList segs))
    segText PairViewText {pvsStr = s} = s
    segText PairViewExp {} = ""

-- * Monotonicity (occurrence polarity)

-- | The signed occurrences of every relation in a rule's *violation set*. A rule
--   @a |- c@ is violated by @a - c@, so a relation occurs in the violation set
--   with the polarity it has in @a@ and the opposite of its polarity in @c@.
--   Growing a relation that occurs positively (or shrinking one that occurs
--   negatively) can create a new violation; this is what makes a triggering edge
--   real rather than merely syntactic.
relationPolarities :: Expression -> Map.Map Relation (Set.Set Sign)
relationPolarities expr =
  Map.fromListWith Set.union [(d, Set.singleton s) | (d, s) <- occs expr]
  where
    occs :: Expression -> [(Relation, Sign)]
    occs e = case e of
      EInc (a, c) -> go Pos a <> go Neg c
      EEqu (a, c) -> eitherWay a <> eitherWay c -- equivalence: violated either way
      _ -> eitherWay e -- a bare term: be conservative
      where
        eitherWay x = go Pos x <> go Neg x

    -- @go s e@: relations of @e@ paired with the sign they carry when @e@ occurs
    -- under accumulated polarity @s@.
    go :: Sign -> Expression -> [(Relation, Sign)]
    go s e = case e of
      EDcD d -> [(d, s)]
      EEqu (a, b) -> go s a <> go (opp s) a <> go s b <> go (opp s) b
      EInc (a, b) -> go s a <> go (opp s) b
      EIsc (a, b) -> go s a <> go s b
      EUni (a, b) -> go s a <> go s b
      EDif (a, b) -> go s a <> go (opp s) b
      ECps (a, b) -> go s a <> go s b
      ERad (a, b) -> go s a <> go s b
      EPrd (a, b) -> go s a <> go s b
      EKl0 e' -> go s e'
      EKl1 e' -> go s e'
      EFlp e' -> go s e'
      EBrk e' -> go s e'
      ECpl e' -> go (opp s) e'
      -- Residuals and the diamond are antitone in one argument; rather than risk
      -- the exact rule we treat both arguments as occurring with both signs.
      ELrs (a, b) -> bothSigns a <> bothSigns b
      ERrs (a, b) -> bothSigns a <> bothSigns b
      EDia (a, b) -> bothSigns a <> bothSigns b
      EDcI {} -> []
      EDcV {} -> []
      EBin {} -> []
      EMp1 {} -> []
      where
        bothSigns x = go Pos x <> go Neg x
