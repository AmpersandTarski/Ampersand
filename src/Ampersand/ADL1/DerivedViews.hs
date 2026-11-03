-- | A derived relation as a database view (issue #1744).
--
--   An @ENFORCE r := term@ statement says that the population of @r@ equals the
--   population of @term@ at all times. The prototype realised that by two rules
--   that the ExecEngine runs after every transaction, writing the pairs of
--   @term@ into a stored table for @r@. Under @--derived-views@ the compiler
--   generates @r@ as a database view on the SQL of @term@ instead: nothing has
--   to be maintained, and @r@ can never lag behind.
--
--   The measurement of the two routes on FC5 stands in
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
    rulesInEnforceOrder,
    inlineViews,
    materialises,
    readsAClosure,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.Classes.Relational (guaranteedProperties)
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
--   @--materialize@, the names given to @--view@, whether a relation belongs to a context on another database,
--   the populations of the script, the rules of the context other than the
--   @Compute@ rules of the @:=@ statements, the interface fields (each with a
--   description for the warning), and all ENFORCE statements of the context.
--   The result pairs each statement with its decision; a statement that is not
--   @:=@ gets no decision and is left out.
--
--   A relation stays stored, with the reason, when: the flag is off; it is named
--   in @--materialize@; another ENFORCE statement writes the same relation (a view
--   cannot take extra pairs); a @POPULATION@ gives it pairs of its own; its
--   definition refers to itself or to a cycle of definitions (SQL has no
--   recursive view of that kind); it belongs to another database (a view
--   is created in the database of the compiled context); a rule reads it,
--   directly or through another view, and the database would compute its view
--   in full ('materialises': a union or a closure in the definition, with the
--   other views substituted); or an interface field reads it and its definition
--   holds a closure, which the database recomputes at every read. The last two
--   come from the falsification experiment of issue #1744 (tests H2 and H4) and
--   are decided together, until stable: a relation that stays stored can make a
--   view that reads it mergeable, so after each relation that is kept stored the
--   others are judged again; a relation named in @--view@ is a view in spite of its
--   readers. Every reason is a warning and not an error, so the
--   flag never refuses a script that compiles without it.
decideDerivedViews ::
  Bool ->
  [Text] ->
  [Text] ->
  (Relation -> Bool) ->
  [Population] ->
  [Rule] ->
  [(Text, Expression)] ->
  [AEnforce] ->
  [(AEnforce, StorageDecision)]
decideDerivedViews flagOn materialized forced isForeign pops rules ifcFields enfs
  | not flagOn = [(e, Stored "the option --derived-views is off") | e <- defining]
  | otherwise = [(e, decide e) | e <- defining]
  where
    defining = [e | e <- enfs, IsSameSet {} <- [enfOp e]]
    decide e
      | Just reason <- localReason e = Stored reason
      | enfRel e `elem` cyclic = Stored "its definition refers to itself, directly or through other derived relations"
      | Just reason <- L.lookup (enfRel e) costly = Stored reason
      | otherwise = AsView
    localReason :: AEnforce -> Maybe Text
    localReason e
      | named (enfRel e) = Just "it is named in --materialize"
      | writers (enfRel e) > 1 = Just "another ENFORCE statement writes the same relation"
      | populated (enfRel e) = Just "a POPULATION statement gives it pairs of its own"
      | isForeign (enfRel e) = Just "it belongs to a context on another database"
      | otherwise = Nothing
    named rel = any (`elem` materialized) (namesOf rel)
    forcedView rel = any (`elem` forced) (namesOf rel)
    namesOf rel = [fullName rel, fullName rel <> tshow (sign rel), tshow rel]
    writers rel = length [() | e <- enfs, enfRel e == rel]
    populated rel = not (null [() | p@ARelPopu {} <- pops, popdcl p == rel, not (Set.null (popps p))])
    -- The candidates that passed the local tests; a cycle among them keeps them all stored.
    candidates = [e | e <- defining, isNothing (localReason e)]
    cyclic = map fst (cyclicPart [(enfRel e, Set.toList (bindedRelationsIn (enfExpr e))) | e <- candidates])
    -- The relations that stay stored because of what reads them, with the reason.
    costly = settle [] [e | e <- candidates, enfRel e `notElem` cyclic]
    -- Keep one relation stored at a time and judge the rest again. A relation whose own
    -- definition holds the union or the closure goes first, so that a view that only
    -- reads it is judged with that relation stored.
    settle :: [(Relation, Text)] -> [AEnforce] -> [(Relation, Text)]
    settle stored views =
      case L.sortOn (not . materialises . enfExpr . fst) [(e, r) | e <- views, Just r <- [reasonFor views e]] of
        [] -> stored
        (e, r) : _ -> settle ((enfRel e, r) : stored) (filter (/= e) views)
    reasonFor views e
      | forcedView (enfRel e) = Nothing
      | materialises definition,
        (nm : _) <- readers =
          Just ("rule " <> nm <> " reads it, and the database would compute its view in full, because its definition holds a union or a closure")
      | readsAClosure defs (enfExpr e),
        (nm : _) <- fieldReaders =
          Just (nm <> " reads it, and its view holds a closure, which the database would recompute at every read")
      | otherwise = Nothing
      where
        defs = [(enfRel v, enfExpr v) | v <- views, v /= e]
        definition = inlineViews defs (enfExpr e)
        readsIt t = enfRel e `Set.member` bindedRelationsIn (inlineViews defs t)
        -- The rules that read the relation: the rules given, and the Compute rules of the
        -- statements that stay stored; not the property rules of the relation itself whose
        -- property the shape of the definition guarantees, which the compiler leaves out.
        readers =
          [ fullName r
            | r <- rules <> [r | v <- defining, v `notElem` views, r <- enfRules v],
              not (guaranteed r),
              readsIt (formalExpression r)
          ]
        guaranteed r = case rrkind r of
          Propty p rel -> rel == enfRel e && p `Set.member` guaranteedProperties (enfExpr e)
          _ -> False
        fieldReaders = [nm | (nm, t) <- ifcFields, readsIt t]

-- | Whether the database must materialise the query of a term before it can join it.
--   The query of a closure is a recursive common table expression, and the query of a union is a set operator;
--   MariaDB merges neither into an enclosing join, so the result is a temporary table without an index.
--   A query built from declared relations by composition, intersection and converse is merged,
--   and a join onto it uses the indexes of the tables underneath.
--   The test is deliberately one-sided: EXCEPT computes its whole right-hand side, also when the left-hand side is small,
--   so a term gets it only when the left join certainly has no index to use (measured in issue #1708).
--   The same test decides which derived relation stays stored under @--derived-views@
--   ('decideDerivedViews'); the report of the compiler checks it against the SQL of the view.
materialises :: Expression -> Bool
materialises e = case e of
  EKl0 {} -> True
  EKl1 {} -> True
  EUni {} -> True
  EFlp x -> materialises x
  EBrk x -> materialises x
  ECpl x -> materialises x
  ECps (a, b) -> materialises a || materialises b
  EIsc (a, b) -> materialises a || materialises b
  EDif (a, b) -> materialises a || materialises b
  ERad (a, b) -> materialises a || materialises b
  EPrd (a, b) -> materialises a || materialises b
  ELrs (a, b) -> materialises a || materialises b
  ERrs (a, b) -> materialises a || materialises b
  EDia (a, b) -> materialises a || materialises b
  EEqu (a, b) -> materialises a || materialises b
  EInc (a, b) -> materialises a || materialises b
  EDcD {} -> False
  EDcI {} -> False
  EBin {} -> False
  EDcV {} -> False
  EMp1 {} -> False

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
viewDefinitionsInOrder = inDependencyOrder fst snd

-- | Items that each define a relation by a term, in an order in which every item
--   comes after the items whose relation its term reads. The items that read no
--   relation of the remaining items go first, repeatedly; a remainder in which
--   every item reads another (a cycle) is appended in the order given.
inDependencyOrder :: (a -> Relation) -> (a -> Expression) -> [a] -> [a]
inDependencyOrder rel term = go []
  where
    go done [] = reverse done
    go done todo =
      case L.partition ready todo of
        ([], _) -> reverse done <> todo
        (rdy, rest) -> go (reverse rdy <> done) rest
      where
        ready item = not (any (`elem` map rel todo) (Set.toList (bindedRelationsIn (term item))))

-- | The rules a role maintains, in the order in which the ExecEngine does the least
--   work (issue #1749): first the rules of the ENFORCE statements, each statement
--   after the statements whose relation its term reads, and then the other rules in
--   the order given. The ExecEngine runs the rules of a round in this order and applies
--   every insertion at once, so a derived relation that reads another is computed
--   after it in the same round. The rules a round checks are the rules the
--   transaction has affected when the round starts, so a chain of derived relations
--   fills in one round when the transaction touches a relation that every link
--   reads, as an import does; otherwise a link enters the round after the one that
--   fills what it reads, as before. The order changes no fixpoint: the ExecEngine
--   repeats its rounds until no rule has a violation.
rulesInEnforceOrder :: [AEnforce] -> [Rule] -> [Rule]
rulesInEnforceOrder enfs rules = statementRules <> others
  where
    maintained = Set.fromList rules
    statementRules =
      [ r
        | e <- inDependencyOrder enfRel enfExpr enfs,
          r <- L.sort (enfRules e),
          r `Set.member` maintained
      ]
    others = filter (`notElem` concatMap enfRules enfs) rules

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
