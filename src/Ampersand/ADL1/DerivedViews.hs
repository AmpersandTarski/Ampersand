-- | A derived relation as a database view (issue #1744).
--
--   An @ENFORCE r := term@ statement says that the population of @r@ equals the
--   population of @term@ at all times. The prototype realised that by two rules
--   that the ExecEngine runs after every transaction, writing the pairs of
--   @term@ into a stored table for @r@. Under @--derived-views@ the compiler
--   generates @r@ as a database view on the SQL of @term@ instead: nothing has
--   to be maintained, and @r@ can never lag behind.
--
--   The measurement of the two routes on the Landeneisenregister stands in
--   memorybank/derived-views/measurement-fc5.md, the design choices next to it.
--
--   This module decides which statements become views, and gives the helpers
--   that the rest of the compiler needs to treat such a relation consistently:
--   the definitions in the order in which the views can be created, and the
--   substitution of a view by its definition in a term.
module Ampersand.ADL1.DerivedViews
  ( StorageDecision (..),
    decideDerivedViews,
    viewDefinitions,
    viewDefinitionsInOrder,
    inlineViews,
    readsAClosure,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Classes
import qualified RIO.List as L
import qualified RIO.Set as Set

-- | Why a derived relation is a view or stays a stored table.
data StorageDecision
  = -- | The relation becomes a view.
    AsView
  | -- | The relation stays a stored table, for the reason given.
    Stored Text
  deriving (Eq, Show)

-- | Decide, for every @ENFORCE r := term@ statement, whether @r@ becomes a view.
--   The arguments: whether @--derived-views@ is on, the names given to
--   @--materialize@, whether a relation belongs to a context on another database,
--   the populations of the script, and all ENFORCE statements of the context.
--   The result pairs each statement with its decision; a statement that is not
--   @:=@ gets no decision and is left out.
--
--   A relation stays stored, with the reason, when: the flag is off; it is named
--   in @--materialize@; another ENFORCE statement writes the same relation (a view
--   cannot take extra pairs); a @POPULATION@ gives it pairs of its own; its
--   definition refers to itself or to a cycle of definitions (SQL has no
--   recursive view of that kind); or it belongs to another database (a view
--   is created in the database of the compiled context). Every reason is a
--   warning and not an error, so the flag never refuses a script that compiles
--   without it.
decideDerivedViews ::
  Bool ->
  [Text] ->
  (Relation -> Bool) ->
  [Population] ->
  [AEnforce] ->
  [(AEnforce, StorageDecision)]
decideDerivedViews flagOn materialized isForeign pops enfs
  | not flagOn = [(e, Stored "the option --derived-views is off") | e <- defining]
  | otherwise = [(e, decide e) | e <- defining]
  where
    defining = [e | e <- enfs, IsSameSet {} <- [enfOp e]]
    decide e
      | named (enfRel e) = Stored "it is named in --materialize"
      | writers (enfRel e) > 1 = Stored "another ENFORCE statement writes the same relation"
      | populated (enfRel e) = Stored "a POPULATION statement gives it pairs of its own"
      | isForeign (enfRel e) = Stored "it belongs to a context on another database"
      | enfRel e `elem` cyclic = Stored "its definition refers to itself, directly or through other derived relations"
      | otherwise = AsView
    named rel = any (`elem` materialized) [fullName rel, fullName rel <> tshow (sign rel), tshow rel]
    writers rel = length [() | e <- enfs, enfRel e == rel]
    populated rel = not (null [() | p@ARelPopu {} <- pops, popdcl p == rel, not (Set.null (popps p))])
    -- The candidates that passed the local tests; a cycle among them keeps them all stored.
    candidates =
      [ (enfRel e, Set.toList (bindedRelationsIn (enfExpr e)))
        | e <- defining,
          not (named (enfRel e)),
          writers (enfRel e) == 1,
          not (populated (enfRel e)),
          not (isForeign (enfRel e))
      ]
    cyclic = map fst (cyclicPart candidates)

-- | The part of a dependency list that lies on a cycle: repeatedly take away
--   every node that depends on no remaining node; what remains is cyclic.
cyclicPart :: [(Relation, [Relation])] -> [(Relation, [Relation])]
cyclicPart deps
  | null free = deps
  | otherwise = cyclicPart remaining
  where
    nodes = map fst deps
    (free, remaining) = L.partition (\(_, ds) -> not (any (`elem` nodes) ds)) deps

-- | The relations that are views, with their definitions.
viewDefinitions :: [AEnforce] -> [(Relation, Expression)]
viewDefinitions enfs = [(enfRel e, enfExpr e) | e <- enfs, enfStorage e == DerivedView]

-- | The view definitions in an order in which each view can be created:
--   a definition comes after every view it reads. The decision excludes cycles,
--   so the order exists; a remainder, which cannot occur, is appended as it is.
viewDefinitionsInOrder :: [(Relation, Expression)] -> [(Relation, Expression)]
viewDefinitionsInOrder = go []
  where
    go done [] = reverse done
    go done todo =
      case L.partition ready todo of
        ([], _) -> reverse done <> todo
        (rdy, rest) -> go (reverse rdy <> done) rest
      where
        ready (_, definition) = not (any (`elem` map fst todo) (Set.toList (bindedRelationsIn definition)))

-- | A term in which every view relation is replaced by its definition, until
--   no view relation is left. Views do not refer to themselves (see
--   'decideDerivedViews'), so the substitution ends.
inlineViews :: [(Relation, Expression)] -> Expression -> Expression
inlineViews [] e = e
inlineViews defs e = go (length defs + 1) e
  where
    go :: Int -> Expression -> Expression
    go 0 x = x
    go n x
      | any isView (Set.toList (bindedRelationsIn x)) = go (n - 1) (subst x)
      | otherwise = x
    isView r = isJust (L.lookup r defs)
    subst :: Expression -> Expression
    subst x = case x of
      EDcD r -> maybe x EBrk (L.lookup r defs)
      EEqu (a, b) -> EEqu (subst a, subst b)
      EInc (a, b) -> EInc (subst a, subst b)
      EIsc (a, b) -> EIsc (subst a, subst b)
      EUni (a, b) -> EUni (subst a, subst b)
      EDif (a, b) -> EDif (subst a, subst b)
      ELrs (a, b) -> ELrs (subst a, subst b)
      ERrs (a, b) -> ERrs (subst a, subst b)
      EDia (a, b) -> EDia (subst a, subst b)
      ECps (a, b) -> ECps (subst a, subst b)
      ERad (a, b) -> ERad (subst a, subst b)
      EPrd (a, b) -> EPrd (subst a, subst b)
      EKl0 a -> EKl0 (subst a)
      EKl1 a -> EKl1 (subst a)
      EFlp a -> EFlp (subst a)
      ECpl a -> ECpl (subst a)
      EBrk a -> EBrk (subst a)
      EDcI {} -> x
      EDcV {} -> x
      EBin {} -> x
      EMp1 {} -> x

-- | Whether a term, with the views it reads substituted, contains a Kleene
--   closure. A view with a closure recomputes the whole closure at every use.
readsAClosure :: [(Relation, Expression)] -> Expression -> Bool
readsAClosure defs = any isClosure . subterms . inlineViews defs
  where
    isClosure x = case x of
      EKl0 {} -> True
      EKl1 {} -> True
      _ -> False
    subterms x = x : concatMap subterms (directSubterms x)
    directSubterms x = case x of
      EEqu (a, b) -> [a, b]
      EInc (a, b) -> [a, b]
      EIsc (a, b) -> [a, b]
      EUni (a, b) -> [a, b]
      EDif (a, b) -> [a, b]
      ELrs (a, b) -> [a, b]
      ERrs (a, b) -> [a, b]
      EDia (a, b) -> [a, b]
      ECps (a, b) -> [a, b]
      ERad (a, b) -> [a, b]
      EPrd (a, b) -> [a, b]
      EKl0 a -> [a]
      EKl1 a -> [a]
      EFlp a -> [a]
      ECpl a -> [a]
      EBrk a -> [a]
      _ -> []
