-- | Pattern dependency-graph helpers for the diagnosis spreadsheet
-- (issue #1625, columns C3, C4, C7, C8, C9).
--
-- The graph is built once from the FSpec so that per-pattern queries
-- (orphan-relations, unreachable process rules, depends-on lists)
-- can be answered with O(1) map lookups during 'extractDiagnostics'.
--
-- Conventions
--
--   * A pattern @P@ "depends on" pattern @Q@ iff a rule declared in
--     @P@ contains a relation declared in @Q@.  The relation must be
--     a true relation reference (@EDcD@); identity / V / singleton
--     leaves are cross-cutting and are not attributed to a specific
--     pattern.
--   * A relation is "orphan" iff no rule and no interface anywhere
--     in the script references it via @bindedRelationsIn@.
--   * A process rule is "unreachable" iff none of the roles that
--     maintain the rule is also the role for any interface.
module Ampersand.Diagnosis.PatternGraph
  ( PatternGraph,
    buildPatternGraph,
    dependsOn,
    dependedOnBy,
    isOrphanRelation,
    nrOrphanRelations,
    unreachableProcessRules,
    isSelfContained,
  )
where

import Ampersand.Basics
import Ampersand.Classes (bindedRelationsIn)
import Ampersand.Core.AbstractSyntaxTree
  ( Expression (..),
    Pattern,
    Relation,
    formalExpression,
    ifcRoles,
    ptdcs,
    ptrls,
  )
import Ampersand.FSpec.FSpec (FSpec (..))
import qualified RIO.Map as Map
import qualified RIO.Set as Set

-- | Pre-computed view of the cross-pattern dependency graph.
data PatternGraph = PatternGraph
  { pgDependsOn :: !(Map.Map Text (Set.Set Text)),
    pgDependedBy :: !(Map.Map Text (Set.Set Text)),
    -- | Relations that appear in any rule or interface body.
    pgUsedRels :: !(Set.Set Relation),
    -- | Roles for which at least one interface exists.
    pgRolesWithIfc :: !(Set.Set Text)
  }

dependsOn :: PatternGraph -> Pattern -> Set.Set Text
dependsOn g p = Map.findWithDefault Set.empty (label p) (pgDependsOn g)

dependedOnBy :: PatternGraph -> Pattern -> Set.Set Text
dependedOnBy g p = Map.findWithDefault Set.empty (label p) (pgDependedBy g)

-- | True iff the relation is referenced by no rule and no interface.
isOrphanRelation :: PatternGraph -> Relation -> Bool
isOrphanRelation g rel = rel `Set.notMember` pgUsedRels g

-- | Number of relations declared in the pattern that are not used.
nrOrphanRelations :: PatternGraph -> Pattern -> Int
nrOrphanRelations g p =
  length [() | rel <- Set.toList (ptdcs p), isOrphanRelation g rel]

-- | Process rules of the pattern that no interface ever exposes via a
-- maintaining role.
unreachableProcessRules :: FSpec -> PatternGraph -> Pattern -> Int
unreachableProcessRules fSpec g p =
  length
    [ ()
      | r <- Set.toList (ptrls p),
        isSignal fSpec r,
        let rolesMaint =
              [ fullName rol
                | (rol, r') <- fRoleRuls fSpec,
                  r == r'
              ],
        not (any (`Set.member` pgRolesWithIfc g) rolesMaint)
    ]

-- | A pattern is "self-contained" iff every @EDcD@ leaf in any of its
-- rules refers to a relation declared in the pattern itself.
isSelfContained :: Pattern -> Bool
isSelfContained p =
  let ownRels = ptdcs p
      ruleRels =
        Set.unions
          [ relationLeaves (formalExpression r)
            | r <- Set.toList (ptrls p)
          ]
   in ruleRels `Set.isSubsetOf` ownRels

----------------------------------------------------------------
-- Build the graph from the FSpec
----------------------------------------------------------------

buildPatternGraph :: FSpec -> PatternGraph
buildPatternGraph fSpec =
  let pats = vpatterns fSpec
      rules = Set.toList (vrules fSpec)
      ifcs = interfaceS fSpec
      -- Which pattern declares which relation?
      relToPat :: Map.Map Relation Text
      relToPat =
        Map.fromList
          [(r, label p) | p <- pats, r <- Set.toList (ptdcs p)]
      -- For every pattern, compute the set of "foreign" pattern names
      -- it points at via the EDcD-leaves of its rules.
      depsOf :: Pattern -> Set.Set Text
      depsOf p =
        let myName = label p
            relsInRules =
              Set.unions
                [ relationLeaves (formalExpression r)
                  | r <- Set.toList (ptrls p)
                ]
            patNames =
              Set.fromList
                [ pname
                  | rel <- Set.toList relsInRules,
                    Just pname <- [Map.lookup rel relToPat],
                    pname /= myName
                ]
         in patNames
      depsMap = Map.fromList [(label p, depsOf p) | p <- pats]
      reverseEdges =
        [ (target, Set.singleton src)
          | (src, tgts) <- Map.toList depsMap,
            target <- Set.toList tgts
        ]
      dependedByMap =
        Map.fromListWith Set.union reverseEdges
      usedRels =
        Set.unions
          ( [bindedRelationsIn r | r <- rules]
              <> [bindedRelationsIn ifc | ifc <- ifcs]
          )
      rolesWithIfc =
        Set.fromList
          [fullName rol | ifc <- ifcs, rol <- ifcRoles ifc]
   in PatternGraph
        { pgDependsOn = depsMap,
          pgDependedBy = dependedByMap,
          pgUsedRels = usedRels,
          pgRolesWithIfc = rolesWithIfc
        }

----------------------------------------------------------------
-- Local helper: enumerate the EDcD leaves of an expression as the
-- set of relations referenced.  Identity, V-relation, simple-binop
-- and singleton leaves are *not* relations and are intentionally
-- skipped.
----------------------------------------------------------------

relationLeaves :: Expression -> Set.Set Relation
relationLeaves expr = case expr of
  EEqu (a, b) -> relationLeaves a <> relationLeaves b
  EInc (a, b) -> relationLeaves a <> relationLeaves b
  EIsc (a, b) -> relationLeaves a <> relationLeaves b
  EUni (a, b) -> relationLeaves a <> relationLeaves b
  EDif (a, b) -> relationLeaves a <> relationLeaves b
  ELrs (a, b) -> relationLeaves a <> relationLeaves b
  ERrs (a, b) -> relationLeaves a <> relationLeaves b
  EDia (a, b) -> relationLeaves a <> relationLeaves b
  ECps (a, b) -> relationLeaves a <> relationLeaves b
  ERad (a, b) -> relationLeaves a <> relationLeaves b
  EPrd (a, b) -> relationLeaves a <> relationLeaves b
  EKl0 e -> relationLeaves e
  EKl1 e -> relationLeaves e
  EFlp e -> relationLeaves e
  ECpl e -> relationLeaves e
  EBrk e -> relationLeaves e
  EDcD d -> Set.singleton d
  EDcI _ -> Set.empty
  EBin _ _ -> Set.empty
  EDcV _ -> Set.empty
  EMp1 _ _ -> Set.empty
