-- | Which concepts need a concept table in the database?
--
-- A concept table has two jobs. It anchors the rows of the univalent and
-- injective relations of its typology, which are stored as columns in that same
-- table. And it answers the question "what are the atoms of this concept?".
-- A table without relation columns exists for the second job alone, so it is
-- worth generating only when something actually asks that question.
--
-- Only terms ask it. The SQL generator reads a concept table exactly where it
-- calls @sqlConceptTable@ or @sqlAttConcept@ (see "Ampersand.FSpec.SQL"), and
-- every such call is driven by an @I@, @V@ or atom literal in the term being
-- compiled. 'conceptsReadBy' mirrors @selectExpr@ to work out which concepts
-- those are; 'conceptsNeedingATable' applies it to the terms that reach the SQL
-- generator.
--
-- That mirroring is what makes this module correct, and it is also its
-- weakness: a change to @selectExpr@ that reads a concept table in a new place
-- must be reflected here. Erring towards reading /more/ concepts than
-- @selectExpr@ does is safe (it only keeps a table that could have gone);
-- erring the other way generates a query against a table that does not exist.
-- @getConceptTableInfo@ calls @fatal@ rather than generate such a query, so the
-- mistake is loud, but it is still a mistake. See issue #1672.
module Ampersand.FSpec.ToFSpec.ConceptTables
  ( conceptsNeedingATable,
    conceptsReadBy,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.FSpec.ToFSpec.NormalForms (conjNF)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE

-- | The concepts whose concept table @selectExpr@ reads when it compiles the
--   given term.
--
--   @ONE@ is never among them: @selectExpr@ compiles @I[ONE]@ and @V[ONE*ONE]@
--   to a literal, and its @EDcV@ case never forces the table lookup of an @ONE@
--   end.
conceptsReadBy :: Expression -> [A_Concept]
conceptsReadBy = L.nub . filter (/= ONE) . go
  where
    -- selectExpr = fromMaybe (nonSpecialSelectExpr expr) (maybeSpecialCase expr)
    go :: Expression -> [A_Concept]
    go expr = fromMaybe (nonSpecial expr) (special expr)

    -- Mirrors maybeSpecialCase. These cases compile a complement as an
    -- anti-join instead of against the closed world V[sign e], which is why
    -- they read far fewer concept tables than the general ECpl case.
    -- The `otherwise -> Nothing` guards matter: when they fire, selectExpr
    -- falls through to nonSpecialSelectExpr rather than to the later
    -- EIsc-with-complement cases.
    special :: Expression -> Maybe [A_Concept]
    special expr = case expr of
      EIsc (EDcI a, ECpl (ECps (EDcD r, EFlp (EDcD r')))) -- I[A] /\ -(r;r~), i.e. r [TOT]
        | r == r' -> Just [a]
        | otherwise -> Nothing
      EIsc (ECpl (ECps (EDcD r, EFlp (EDcD r'))), EDcI a) -- -(r;r~) /\ I[A]
        | r == r' -> Just [a]
        | otherwise -> Nothing
      EDif (EDcI a, ECps (EDcD r, EFlp (EDcD r'))) -- I[A] - r;r~
        | r == r' -> Just [a]
        | otherwise -> Nothing
      EIsc (expr1, ECpl expr2) -> Just (antiJoin False expr1 expr2)
      EIsc (ECpl expr1, expr2) -> Just (antiJoin False expr2 expr1)
      EIsc (expr1, EFlp (ECpl expr2)) -> Just (antiJoin True expr1 expr2)
      EIsc (EFlp (ECpl expr1), expr2) -> Just (antiJoin True expr2 expr1)
      _ -> Nothing

    -- <expr1> LEFT JOIN <expr2>: expr2 is compiled uncomplemented, and a plain
    -- relation is read straight from its own table.
    antiJoin :: Bool -> Expression -> Expression -> [A_Concept]
    antiJoin flipped expr1 expr2 =
      go expr1 <> case expr2 of
        EDcD _ -> []
        _ -> go (if flipped then flp expr2 else expr2)

    -- Mirrors nonSpecialSelectExpr, in its case order.
    nonSpecial :: Expression -> [A_Concept]
    nonSpecial expr = case expr of
      EIsc {} ->
        -- Atom literals in an intersection become value comparisons in the
        -- WHERE clause, so they read no table -- unless they are all there is,
        -- in which case the intersection is compiled as that atom literal.
        case filter (not . isMp1) . NE.toList . exprIsc2list $ expr of
          [] -> [source expr]
          ts -> concatMap go ts
      EUni (l, r) -> go l <> go r
      ECps {} -> composition expr
      EFlp e -> go e
      EMp1 _ c -> [c]
      EDcV sgn -> [source sgn, target sgn]
      EDcI c -> [c]
      EBin _ sgn -> [source sgn]
      -- A relation is read from its own table, never from a concept table.
      EDcD _ -> []
      EBrk e -> go e
      ECpl e -> case e of
        EDcV _ -> [] -- -V is the empty set
        EDcI c -> [c] -- -I reads the one concept table twice
        _ -> [source e, target e] <> go e -- compiled against V[sign e]
      EKl0 e -> source e : go e -- r* = I[source r] \/ r+
      EKl1 e -> go e
      EDif (EDcV sgn, x) -> case x of
        ECpl _ -> go (notCpl x)
        _ -> fromMaybe (go (notCpl x)) (special (EIsc (EDcV sgn, ECpl x)))
      -- The remaining operators are compiled by rewriting. Rewrite them the
      -- same way here rather than guessing what the rewrite needs.
      EEqu (l, r) -> go ((ECpl l .\/. r) ./\. (ECpl r .\/. l))
      EInc (l, r) -> go (ECpl l .\/. r)
      EDif (l, r) -> go (l ./\. ECpl r)
      ERrs (l, r) -> [target l, target r] <> go l <> go r
      ELrs (l, r) -> go (EFlp (flp r .\. flp l))
      EDia (l, r) -> go ((flp l .\. r) ./\. (l ./. flp r))
      ERad (l, rr) -> case rr of
        ECpl r -> go (EFlp (r .\. flp l))
        _ -> go (flp (notCpl l) .\. rr)
      EPrd (l, r) -> [target l, source r] <> go l <> go r

    -- A composition is compiled as "poles and fences". The first and last fence
    -- are always generated, because the source and target of the whole term
    -- depend on them. An inner fence that is V or -I is not generated at all,
    -- so `r;V;s` and `r;-I;s` read no concept table for that fence.
    composition :: Expression -> [A_Concept]
    composition expr = case NE.toList . exprCps2list $ expr of
      [] -> []
      [e] -> go e
      es -> concat (zipWith fence [0 :: Int ..] es)
        where
          lastNr = length es - 1
          fence i e
            | i == 0 || i == lastNr = go e
            | otherwise = case e of
                EDcV {} -> []
                ECpl EDcI {} -> []
                EBin _ sgn -> [source sgn] -- no fence, but the pole names the column
                _ -> go e

-- | The concepts that need a concept table of their own, given a context and
--   its conjuncts.
--
--   The terms considered are the ones handed to the SQL generator: the
--   normalised conjuncts (@generics/conjuncts.json@), the normalised rules
--   (@ampersand validate@), the interface terms (@generics/interfaces.json@
--   normalised, @--sqldump@ raw), the identity terms, the view terms
--   (@generics/concepts.json@, unnormalised) and the violation-view terms.
--
--   On top of that, @broadQuery@ looks up the concept table of the target of an
--   interface term that has a BOX below it, so those concepts are needed
--   whether or not their term mentions @I@.
conceptsNeedingATable :: env -> A_Context -> [Conjunct] -> [A_Concept]
conceptsNeedingATable env context conjs =
  L.nub
    $ concatMap conceptsReadBy termsReachingSQL
    <> broadQueryConcepts
  where
    normalized = conjNF env

    termsReachingSQL :: [Expression]
    termsReachingSQL =
      map (normalized . notCpl . rcConjunct) conjs
        <> map (normalized . notCpl . formalExpression) (toList (allRules context))
        <> concatMap (\o -> [objExpression o, normalized (objExpression o)]) allObjectDefs
        <> concatMap (NE.toList . identityAts) (identities context)
        <> [e | vd <- viewDefs context, ViewExp e <- map vsmLoad (vdats vd)]
        <> [ e
             | r <- toList (allRules context),
               Just (PairView pvsegs) <- [rrviol r],
               PairViewExp _ _ e <- NE.toList pvsegs
           ]

    -- broadQuery calls getConceptTableInfo on the target of an object's term
    -- whenever that object has a BOX with terms below it, and joins the concept
    -- table of the source of the terms in that BOX.
    broadQueryConcepts :: [A_Concept]
    broadQueryConcepts =
      filter (/= ONE)
        $ [ target (objExpression o)
            | o <- allObjectDefs,
              Just Box {siObjs = sObjs} <- [objmsub o],
              not (null [x | BxExpr x <- sObjs])
          ]
        <> [ source (objExpression x)
             | o <- allObjectDefs,
               Just Box {siObjs = sObjs} <- [objmsub o],
               BxExpr x <- sObjs
           ]

    allObjectDefs :: [ObjectDef]
    allObjectDefs = concatMap (objDefsIn . ifcObj) (ctxifcs context)
      where
        objDefsIn :: ObjectDef -> [ObjectDef]
        objDefsIn o = o : concatMap objDefsIn (fields o)
