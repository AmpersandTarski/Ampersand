-- | CLASSIFY-graph helpers for the diagnosis spreadsheet (issue #1625,
-- columns D1–D4).
--
-- The graph is built once from 'AClassify' edges so the per-concept
-- queries (parents, children, depth, cycle-membership) can be answered
-- in O(1) lookups during 'extractDiagnostics'.
--
-- Conventions
--
--   * An edge points from the *specific* concept (child) to the more
--     *general* concept (parent), matching the on-disk
--     @CLASSIFY child ISA parent@ direction.
--   * 'classifyDepth' returns the length of the longest path from a
--     concept to any root (a concept without parents).  Cycles are
--     handled defensively: a visited-set short-circuits the recursion
--     and yields 0 for nodes inside a cycle.
--   * 'inCycle' uses a transitive-closure traversal: a concept is in a
--     cycle iff it can reach itself via parent edges.
module Ampersand.Diagnosis.ClassifyGraph
  ( ClassifyGraph,
    buildClassifyGraph,
    parentsOf,
    childrenOf,
    classifyDepth,
    inCycle,
  )
where

import Ampersand.Basics
import Ampersand.Core.AbstractSyntaxTree (AClassify (..), A_Concept)
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set

-- | Pre-computed view of the CLASSIFY-DAG.
data ClassifyGraph = ClassifyGraph
  { cgParents :: !(Map.Map A_Concept (Set.Set A_Concept)),
    cgChildren :: !(Map.Map A_Concept (Set.Set A_Concept)),
    cgInCycle :: !(Set.Set A_Concept)
  }

-- | All @(child, parent)@ edges contributed by a list of 'AClassify'
-- declarations.  Mirrors the construction used elsewhere in the
-- compiler (see 'AbstractSyntaxTree' near line 1581).
classifyEdges :: [AClassify] -> [(A_Concept, A_Concept)]
classifyEdges = concatMap go
  where
    go Isa {genspc = s, gengen = g} = [(s, g)]
    go IsE {genspc = s, genrhs = rs} = [(s, c) | c <- NE.toList rs]

buildClassifyGraph :: [AClassify] -> ClassifyGraph
buildClassifyGraph clas =
  let edges = classifyEdges clas
      parents = foldr (insertEdge fst snd) Map.empty edges
      children = foldr (insertEdge snd fst) Map.empty edges
      allCs =
        Set.fromList
          $ [c | (a, b) <- edges, c <- [a, b]]
      cycleSet = Set.filter (reachableViaParents parents) allCs
   in ClassifyGraph parents children cycleSet
  where
    insertEdge keyF valF (a, b) m =
      Map.insertWith Set.union (keyF (a, b)) (Set.singleton (valF (a, b))) m

parentsOf :: ClassifyGraph -> A_Concept -> Set.Set A_Concept
parentsOf g c = Map.findWithDefault Set.empty c (cgParents g)

childrenOf :: ClassifyGraph -> A_Concept -> Set.Set A_Concept
childrenOf g c = Map.findWithDefault Set.empty c (cgChildren g)

inCycle :: ClassifyGraph -> A_Concept -> Bool
inCycle g c = c `Set.member` cgInCycle g

-- | Length of the longest path from @c@ to a root, ignoring cycles.
classifyDepth :: ClassifyGraph -> A_Concept -> Int
classifyDepth g = go Set.empty
  where
    go :: Set.Set A_Concept -> A_Concept -> Int
    go visited c
      | c `Set.member` visited = 0 -- defensive: cycle short-circuit
      | otherwise =
          let ps = Set.toList (parentsOf g c)
           in if null ps
                then 0
                else 1 + foldr max 0 (map (go (Set.insert c visited)) ps)

-- | True iff @start@ can reach @start@ by following parent edges (i.e.
-- the node lies on a directed cycle).
reachableViaParents ::
  Map.Map A_Concept (Set.Set A_Concept) ->
  A_Concept ->
  Bool
reachableViaParents parents start = go Set.empty initial
  where
    initial = Map.findWithDefault Set.empty start parents
    go _ frontier | Set.null frontier = False
    go seen frontier
      | start `Set.member` frontier = True
      | otherwise =
          let next =
                Set.unions
                  [ Map.findWithDefault Set.empty c parents
                    | c <- Set.toList frontier
                  ]
              seen' = seen `Set.union` frontier
              frontier' = next `Set.difference` seen'
           in go seen' frontier'
