-- | Compile-time cost profile per conjunct (issue #1692; design choices
--   DC-16 and DC-17, memorybank\/incremental-evaluation\/DesignChoices.md).
--
--   The corpus study of issue #1690 measured that the /shape/ of a violation
--   query fixes its growth exponent, while the /population/ decides when that
--   curve crosses the fixed fee of delta-scoped re-evaluation. Neither side
--   can decide alone: a shape-only classifier reached 21.7% precision, and
--   measured timings proved environment-sensitive. The division of labour is
--   therefore: the compiler publishes, per conjunct, the shape class and the
--   tables the violation query reads in full; the runtime holds that profile
--   against the live table sizes it already has, with one comparison per
--   conjunct.
--
--   The class vocabulary follows the case table of DC-16:
--
--   * @structural@ — the storage layout already enforces the property this
--     conjunct checks, so no query needs to run (see 'structurallyEnforced').
--   * @recursive@ — the term holds a Kleene closure; measured to explode at
--     toy sizes (finding 2 of the corpus study), so once an incremental route
--     exists it must always be taken.
--   * @anchored@ — the term is pinned to a named atom ('EMp1'); the query is
--     an index probe, flat in the database size.
--   * @scan@ — everything else; the query reads the listed tables in full,
--     and the largest of them decides when incremental maintenance pays.
module Ampersand.FSpec.Incremental.CostProfile
  ( CostClass (..),
    CostProfile (..),
    costClassText,
    costProfileFor,
    structurallyEnforced,
    scanTablesOf,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.FSpecAux (lookupConceptTable, relationTableInfos)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set

-- | The route class of a conjunct's violation query (contract DC-17).
data CostClass
  = Structural
  | Recursive
  | Anchored
  | Scan
  deriving (Eq, Show, Enum, Bounded)

-- | The wire name of a class in @conjuncts.json@.
costClassText :: CostClass -> Text
costClassText cls = case cls of
  Structural -> "structural"
  Recursive -> "recursive"
  Anchored -> "anchored"
  Scan -> "scan"

-- | What the compiler publishes per conjunct. 'cpScanTables' names the
--   tables the violation query reads in full — the runtime's threshold
--   input for the @scan@ class. It is empty for @structural@ (no query
--   runs) and for @anchored@ (the query probes, it does not scan).
data CostProfile = CostProfile
  { cpClass :: !CostClass,
    cpScanTables :: ![Text]
  }
  deriving (Eq, Show)

-- | Classify one conjunct, given the violation term the SQL generator also
--   receives (the 'conjNF' of the negated conjunct), so the profile speaks
--   about exactly the query that ships.
--
--   The class test order is fixed: a structurally enforced conjunct needs no
--   further looks (property-rule terms contain neither closures nor atom
--   anchors); a Kleene term must never count as anchored merely because an
--   'EMp1' occurs somewhere in it (the closure dominates the cost); what is
--   left is anchored on any 'EMp1' occurrence — the approximation the corpus
--   study validated at 100% recall — or a scan.
costProfileFor :: FSpec -> Conjunct -> Expression -> CostProfile
costProfileFor fSpec conj violTerm
  | any (structurallyEnforced fSpec) (NE.toList (rc_orgRules conj)) =
      CostProfile Structural []
  | hasKleene = CostProfile Recursive scanTables
  | hasAnchor = CostProfile Anchored []
  | otherwise = CostProfile Scan scanTables
  where
    scanTables = scanTablesOf fSpec violTerm
    hasKleene = any isKleene (subExpressions violTerm)
    isKleene expr = case expr of
      EKl0 {} -> True
      EKl1 {} -> True
      _ -> False
    hasAnchor = any isMp1 (primitives violTerm)

-- | Is this rule enforced by the storage layout itself?
--
--   True exactly for the property rules whose violation the generated schema
--   cannot represent: a @UNI@ rule of a relation stored unflipped in a wide
--   table whose key side is the table's SQL primary key, and an @INJ@ rule
--   of a relation stored flipped likewise. The DDL puts @UNIQUE@ on primary
--   key attributes ('Ampersand.Prototype.TableSpec'), so the key atom sits in
--   at most one row and the other side is a single column value: at most one
--   pair per key atom, in every database state the schema admits.
--
--   Everything else — link tables, specialization columns (no SQL uniqueness
--   constraint), scalar-represented keys (issue #341 leaves them without a
--   primary key), and the other seven properties — is deliberately not
--   structural; those conjuncts fall through to the query-shape classes.
--
--   Proof obligation: claim PRF-8 in docs\/proofs\/README.md (status
--   @stated@); the runtime may only skip the query where this predicate
--   holds, and its sampled self-check keeps covering skipped conjuncts.
structurallyEnforced :: FSpec -> Rule -> Bool
structurallyEnforced fSpec rule = case rrkind rule of
  Propty Uni rel -> keySideIsPrimary rel (not . rsStoredFlipped) rsSrcAtt
  Propty Inj rel -> keySideIsPrimary rel rsStoredFlipped rsTrgAtt
  _ -> False
  where
    -- A relation stored in several tables (a MULTITABLE union, issue #1716)
    -- has a key column per table and none across them, so its multiplicity
    -- is not structural and the conjunct runs as a query.
    keySideIsPrimary rel storedRightWay keyAtt =
      case relationTableInfos fSpec rel of
        [(_, store)] -> storedRightWay store && isPrimaryKey (keyAtt store)
        _ -> False

-- | The tables a term's compiled query reads in full, by name as the
--   framework knows them (@mysqlTable.name@ in @relations.json@).
--
--   The walk over-approximates: every relation occurrence contributes its
--   table, and every construct whose SQL ranges over whole concept
--   populations — identities, cartesian products, complements, residuals,
--   relative addition, diamonds, and value comparisons — contributes the
--   concept tables of its signature. Concepts without a concept table
--   (@ONE@, and unqueried concepts per issue #1672) contribute nothing.
--   Over-listing errs on the safe side of the gate's loss asymmetry: it can
--   cost the bounded protocol fee on a cheap query, never an unbounded
--   integral run on an expensive one.
scanTablesOf :: FSpec -> Expression -> [Text]
scanTablesOf fSpec =
  L.sort . L.nub . concatMap tablesOf . Set.toList . subExpressions
  where
    tablesOf :: Expression -> [Text]
    tablesOf expr = case expr of
      EDcD rel -> map (tableName . fst) (relationTableInfos fSpec rel)
      EDcI c -> conceptTable c
      EDcV sgn -> signatureTables sgn
      EBin _ sgn -> signatureTables sgn
      ECpl e -> signatureTables (sign e)
      ERad {} -> signatureTables (sign expr)
      ELrs {} -> signatureTables (sign expr)
      ERrs {} -> signatureTables (sign expr)
      EDia {} -> signatureTables (sign expr)
      _ -> []
    signatureTables sgn = case sgn of
      Sign src tgt -> conceptTable src <> conceptTable tgt
      ISgn c -> conceptTable c
    conceptTable :: A_Concept -> [Text]
    conceptTable c =
      -- a concept without a table of its own is read from the tables of its
      -- storage members (issue #1716); a concept nobody enumerates from none
      [ tableName plug
        | m <- storageMembersIn (conceptUnions fSpec) c,
          Just (plug, _) <- [lookupConceptTable fSpec m]
      ]
    tableName :: PlugSQL -> Text
    tableName = text1ToText . showUnique
