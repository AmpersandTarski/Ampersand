{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Ampersand.FSpec.ToFSpec.NormalForms
  ( conjNF,
    cfProof,
    dfProofs, -- these are for confluence testing.
    makeAllConjs,
    conjuncts,
  )
where

import Ampersand.ADL1
import Ampersand.ADL1.P2A_Converters (ConceptMap, pCpt2aCpt)
import Ampersand.Basics
import Ampersand.Classes.Relational
import Ampersand.Core.ShowAStruct
import Ampersand.Core.ShowPStruct
import Ampersand.Input (parseRule)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Set.Partial as SetPartial
import qualified RIO.Text as T

{- SJC:
Ideas for future work:
-> Create a Unifier data type / class with
   > extend :: key -> value -> Unifier -> Maybe Unifier
   (returns Nothing if the key is already in the Unifier)
   > obtain :: key -> Unifier -> value
   (something like this twice! Handle types separate from relations)
   > think of a way in which substitution never fails (unify matching and substitution for this)
-> Make RTerm polymorphic, so we can treat variables and constants separately.
   We'd have RTerm Expression and RTerm (Text,Text,Text)
   We'd be able to derive fmap, and make RTerm Foldable.
-> Really long term: Unify RTerm and Expression in a way that still allows us to write simple code for binary operators.
   Would require separating = and |- from Expression, which is also nice.
-}

-- The following was built for the purpose of testing confluence.
-- These functions produce all derivations of results from the normalizer.
-- A useful side effect is that it implicitly tests for soundness.
dfProofs :: ConceptMap -> Expression -> [(Expression, Proof Expression)]
dfProofs cptMap = prfs True
  where
    prfs :: Bool -> Expression -> [(Expression, Proof Expression)]
    prfs dnf expr =
      L.nub [(rTerm2expr t, map makeExpr derivs) | (t, derivs) <- f (expr2RTerm expr)]
      where
        -- FIXME: Function f inside dfproofs does not terminate!! Must be fixed before fDeriveProofs can be made strict.
        f :: RTerm -> [(RTerm, [(RTerm, [Text], Text)])]
        f term =
          [(term, [(term, [], "<=>")]) | null dsteps]
            <> [ (t, (term, [showStep dstep], "<=>") : deriv)
                 | dstep <- dsteps,
                   (t, deriv) <- f (rhs dstep)
               ]
          where
            dsteps = [dstep | dstep <- dSteps (tceDerivRules cptMap) term, w (rhs dstep) < w term]
        w = weightNF dnf -- the weight function for disjunctive normal form.
        showStep dstep = " weight: " <> (tshow . w . lhs) dstep <> ",   " <> showIT tmpl <> " = " <> showIT stp <> "  with unifier: " <> showIT unif
          where
            (tmpl, unif, stp) = rul dstep
        makeExpr (term, explStr, logicSym) = (rTerm2expr term, explStr, logicSym)

-- Deriving normal forms and representing the neccessary derivation rules are defined by means of RTerms.
-- The data structure RTerm is a representation of relation algebra terms,
-- which is not redundant with respect to associativity and commutativity.
-- The reason for this is that we use term rewriting for normalization.
-- This algorithm performs poorly with commutative rules, because it may explode combinatorially.
data RTerm
  = RIsc {rTermSet :: Set RTerm} -- intersection is associative and commutative
  | RUni {rTermSet :: Set RTerm} -- union is associative and commutative
  | RDif {rTermLft :: RTerm, rTermRht :: RTerm}
  | RCpl {rTermUny :: RTerm}
  | RDia {rTermLft :: RTerm, rTermRht :: RTerm}
  | RLrs {rTermLft :: RTerm, rTermRht :: RTerm}
  | RRrs {rTermLft :: RTerm, rTermRht :: RTerm}
  | RRad {rTermList :: [RTerm]} -- ! is associative
  | RCps {rTermList :: [RTerm]} -- ; is associative
  | RPrd {rTermList :: [RTerm]} -- # is associative
  | RKl0 {rTermUny :: RTerm}
  | RKl1 {rTermUny :: RTerm}
  | RFlp {rTermUny :: RTerm}
  | RId A_Concept
  | RBind PBinOp A_Concept
  | RVee A_Concept A_Concept
  | RAtm PAtomValue A_Concept
  | RVar Name A_Concept A_Concept -- relation name, source name, target name.
  | RConst Expression
  deriving (Eq, Ord, Show)

-- The following condition must hold at all times for every RTerm, in order to make equality work
-- It ensures that nested RIsc terms do not occur, and RIsc terms are at least 2 terms of length.
-- The same holds for RUni, RRad, RCps, and RPrd.
isValid :: RTerm -> Bool
isValid (RIsc s) = and [not (isRIsc e) && isValid e && length ls > 1 | let ls = Set.toList s, e <- ls]
isValid (RUni s) = and [not (isRUni e) && isValid e && length ls > 1 | let ls = Set.toList s, e <- ls]
isValid (RDif l r) = isValid l && isValid r
isValid (RCpl e) = isValid e
isValid (RDia l r) = isValid l && isValid r
isValid (RLrs l r) = isValid l && isValid r
isValid (RRrs l r) = isValid l && isValid r
isValid (RRad ls) = and [not (isRRad e) && isValid e && length ls > 1 | e <- ls]
isValid (RCps ls) = and [not (isRCps e) && isValid e && length ls > 1 | e <- ls]
isValid (RPrd ls) = and [not (isRPrd e) && isValid e && length ls > 1 | e <- ls]
isValid (RKl0 e) = isValid e
isValid (RKl1 e) = isValid e
isValid (RFlp e) = isValid e
isValid _ = True

-- normRT exists to make an arbitrary term satisfy isValid.
-- So isValid (normRT term) is True, whil term and (normRT term) have the same meaning.
normRT :: RTerm -> RTerm
normRT (RIsc s) = (combSet RIsc . Set.fromList . flat isRIsc . map normRT . Set.toList) s
normRT (RUni s) = (combSet RUni . Set.fromList . flat isRUni . map normRT . Set.toList) s
normRT (RDif l r) = RDif (normRT l) (normRT r)
normRT (RCpl e) = RCpl (normRT e)
normRT (RDia l r) = RDia (normRT l) (normRT r)
normRT (RLrs l r) = RLrs (normRT l) (normRT r)
normRT (RRrs l r) = RRrs (normRT l) (normRT r)
normRT (RRad ls) = (combLst RRad . flat isRRad . map normRT) ls
normRT (RCps ls) = (combLst RCps . flat isRCps . map normRT) ls
normRT (RPrd ls) = (combLst RPrd . flat isRPrd . map normRT) ls
normRT (RKl0 e) = RKl0 (normRT e)
normRT (RKl1 e) = RKl1 (normRT e)
normRT (RFlp e) = RFlp (normRT e)
normRT term = term

isRIsc, isRUni, isRDif, isRCpl, isRDia, isRLrs, isRRrs, isRRad, isRCps, isRPrd, isRKl0, isRKl1, isRFlp, isRVar :: RTerm -> Bool
isRIsc RIsc {} = True
isRIsc _ = False
isRUni RUni {} = True
isRUni _ = False
isRDif RDif {} = True
isRDif _ = False
isRCpl RCpl {} = True
isRCpl _ = False
isRDia RDia {} = True
isRDia _ = False
isRLrs RLrs {} = True
isRLrs _ = False
isRRrs RRrs {} = True
isRRrs _ = False
isRRad RRad {} = True
isRRad _ = False
isRCps RCps {} = True
isRCps _ = False
isRPrd RPrd {} = True
isRPrd _ = False
isRKl0 RKl0 {} = True
isRKl0 _ = False
isRKl1 RKl1 {} = True
isRKl1 _ = False
isRFlp RFlp {} = True
isRFlp _ = False
isRVar RVar {} = True
isRVar _ = False

{- dSteps computes the terms that can be obtained in one rewrite step.
   It yields the steps, for the purpose of constructing the entire proof.
   The idea is that the environment picks one of the steps produced by dSteps.
-}
dSteps :: [DerivRule] -> RTerm -> [DerivStep]
dSteps drs x = dStps x
  where
    dStps :: RTerm -> [DerivStep]
    dStps (RIsc s) = dStepSets isRIsc RIsc s
    dStps (RUni s) = dStepSets isRUni RUni s
    dStps (RDif a b) = dStepBin isRDif RDif a b
    dStps (RCpl a) = dStepUny isRCpl RCpl a
    dStps (RDia a b) = dStepBin isRDia RDia a b
    dStps (RLrs a b) = dStepBin isRLrs RLrs a b
    dStps (RRrs a b) = dStepBin isRRrs RRrs a b
    dStps (RRad ls) = dStepLists isRRad RRad ls
    dStps (RCps ls) = dStepLists isRCps RCps ls
    dStps (RPrd ls) = dStepLists isRPrd RPrd ls
    dStps (RKl0 a) = dStepUny isRKl0 RKl0 a
    dStps (RKl1 a) = dStepUny isRKl1 RKl1 a
    dStps (RFlp a) = dStepUny isRFlp RFlp a
    dStps (RId _) =
      [ DStep
          { lhs = x, -- derivs gives the top level rewrites.
            rul = (term, unif, term'), -- only one rewrite is done in parallel in the top level.
            rhs = substitute rd unif term' -- so rest is left alone, if partition can be rewritten.
          }
        | (term@(RId a'), rewriteTerms) <- matchableRules, -- select rewrite rules with the proper combinator
          let unif = Set.fromList [(name a', x)], -- find unifiers such that: substitute "" unif term==rCombinator a
          term' <- rewriteTerms, -- enumerate right hand side RTerms in order to construct:  substitute "" unif term'
          let rd = showIT term <> " -> " <> showIT term', -- rule documentation for fatals in 'substitute'
          substitute rd unif term
            == x
            || fatal ("When analysing rule " <> rd <> " with unifier " <> showIT unif <> "\nsubstitute rd unif term:  " <> showIT (substitute rd unif term) <> "\ndiffers from:  " <> showIT x)
      ]
    dStps (RVee a b) =
      [ DStep
          { lhs = x, -- derivs gives the top level rewrites.
            rul = (term, unif, term'), -- only one rewrite is done in parallel in the top level.
            rhs = substitute rd unif term' -- so rest is left alone, if partition can be rewritten.
          }
        | (term@(RVee a' b'), rewriteTerms) <- matchableRules, -- select rewrite rules with the proper combinator
          let unif = Set.fromList [(name a', RId a), (name b', RId b)], -- find unifiers such that: substitute "" unif term==rCombinator a
          noDoubles unif, -- if one variable is bound to more than one different terms, the deal is off.
          term' <- rewriteTerms, -- enumerate right hand side RTerms in order to construct:  substitute "" unif term'
          let rd = showIT term <> " -> " <> showIT term', -- rule documentation for fatals in 'substitute'
          substitute rd unif term
            == x
            || fatal ("When analysing rule " <> rd <> " with unifier " <> showIT unif <> "\nsubstitute rd unif term:  " <> showIT (substitute rd unif term) <> "\ndiffers from:  " <> showIT x)
      ]
    dStps (RAtm a c) =
      [ DStep
          { lhs = x, -- derivs gives the top level rewrites.
            rul = (term, unif, term'), -- only one rewrite is done in parallel in the top level.
            rhs = substitute rd unif term' -- so rest is left alone, if partition can be rewritten.
          }
        | (term@(RAtm a' c'), rewriteTerms) <- matchableRules, -- select rewrite rules with the proper combinator
          a == a',
          let unif = Set.fromList [(name c', RId c)], -- find unifiers such that: substitute "" unif term==rCombinator a
          term' <- rewriteTerms, -- enumerate right hand side RTerms in order to construct:  substitute "" unif term'
          let rd = showIT term <> " -> " <> showIT term', -- rule documentation for fatals in 'substitute'
          substitute rd unif term
            == x
            || fatal ("When analysing rule " <> rd <> " with unifier " <> showIT unif <> "\nsubstitute rd unif term:  " <> showIT (substitute rd unif term) <> "\ndiffers from:  " <> showIT x)
      ]
    dStps RBind {} = [] -- @stefjoosten, can you have a look at this? Are there rewrite rules based on the operator of RBind?
    dStps RVar {} = fatal "Cannot rewrite a term with a variable in it." -- This should become a haskell type-error when RTerm is polymorphic
    dStps RConst {} = [] -- the only possibly matching rule has a single variable on the lhs, which we assume does not exist. SJ to SJC: Why? is there a reason why we don't want to include that situation?
    dStepUny ::
      (RTerm -> Bool) -> -- a predicate, isrComb, which tests whether some RTerm r has rCombinator as its root.
      (RTerm -> RTerm) -> -- the combinator
      RTerm -> -- its argument  (So, we are working with the RTerm   rCombinator a)
      [DerivStep] -- all derivation steps that start at  rCombinator a, which can be made using the available ruleset
      {- We are trying to find steps in case a term (rCombinator a) has a unary operator (i.e. RCpl, RKl0, RKl1, RFlp) as its root.
         First, we try to find a rewrite step on the root level of the term. The resulting steps are called "derivs".
         When that fails, we try to find the steps from subexpression a recursively.
      -}
    dStepUny isrComb rCombinator a =
      if (not . isValid . rCombinator) a
        then fatal ("Invalid term in dStepLists: " <> showIT (rCombinator a))
        else
          derivs
            <> [ DStep
                   { lhs = rCombinator a, -- try to find steps recursively
                     rul = rul step,
                     rhs = rCombinator (rhs step)
                   }
                 | step <- dStps a
               ]
      where
        derivs =
          [ DStep
              { lhs = rCombinator a, -- derivs gives the top level rewrites.
                rul = (term, unif, term'), -- only one rewrite is done in parallel in the top level.
                rhs = substitute rd unif term' -- so rest is left alone, if partition can be rewritten.
              }
            | (term, rewriteTerms) <- matchableRules,
              isrComb term, -- select rewrite rules with the proper combinator
              let subTerm = rTermUny term, -- now:   rCombinator subTerm = term
              unif <- matches subTerm a, -- find unifiers such that: substitute "" unif term==rCombinator a
              term' <- rewriteTerms, -- enumerate right hand side RTerms in order to construct:  substitute "" unif term'
              let rd = showIT term <> " -> " <> showIT term', -- rule documentation for fatals in 'substitute'
              substitute rd unif term
                == rCombinator a
                || fatal ("When analysing rule " <> rd <> " with unifier " <> showIT unif <> "\nsubstitute rd unif term:  " <> showIT (substitute rd unif term) <> "\ndiffers from\nrCombinator a:  " <> showIT (rCombinator a))
          ]

    -- dStepBin follows the same pattern as dStepUny, but for binary RTerms
    dStepBin :: (RTerm -> Bool) -> (RTerm -> RTerm -> RTerm) -> RTerm -> RTerm -> [DerivStep]
    dStepBin isrComb rCombinator a b =
      if (not . isValid) (rCombinator a b)
        then fatal ("Invalid term in dStepLists: " <> showIT (rCombinator a b))
        else
          derivs
            <> [ DStep
                   { lhs = rCombinator a b,
                     rul = rul rStp,
                     rhs = rCombinator a (rhs rStp)
                   }
                 | rStp <- dStps b
               ]
            <> [ DStep
                   { lhs = rCombinator a b,
                     rul = rul lStp,
                     rhs = rCombinator (rhs lStp) b
                   }
                 | lStp <- dStps a
               ]
      where
        derivs =
          [ DStep
              { lhs = rCombinator a b, -- derivs gives the top level rewrites.
                rul = (term, unif, term'), -- only one rewrite is done in parallel in the top level.
                rhs = substitute rd unif term' -- so rest is left alone, if partition can be rewritten.
              }
            | (term, rewriteTerms) <- matchableRules,
              isrComb term, -- select rewrite rules with the proper combinator
              let subLft = rTermLft term; subRht = rTermRht term, -- now:   rCombinator subTerm = term
              unif1 <- matches subLft a,
              unif2 <- matches subRht b, -- find unifiers such that: substitute "" unif term==rCombinator a
              let unif = Set.union unif1 unif2,
              noDoubles unif, -- if one variable is bound to more than one different terms, the deal is off.
              term' <- rewriteTerms, -- enumerate right hand side RTerms in order to construct:  substitute "" unif term'
              let rd = showIT term <> " -> " <> showIT term', -- rule documentation for fatals in 'substitute'
              substitute rd unif term
                == rCombinator a b
                || fatal ("When analysing rule " <> rd <> " with unifier " <> showIT unif <> "\nsubstitute rd unif term:  " <> showIT (substitute rd unif term) <> "\ndiffers from\nrCombinator a b:  " <> showIT (rCombinator a b))
          ]

    dStepLists :: (RTerm -> Bool) -> ([RTerm] -> RTerm) -> [RTerm] -> [DerivStep] -- Note: a and b are both RTerm
    dStepLists isrComb rCombinator ls =
      if (not . isValid . rCombinator) ls
        then fatal ("Invalid term in dStepLists: " <> showIT (rCombinator ls))
        else
          [ DStep
              { lhs = rCombinator ls, -- The original term
                rul = (term, unif, term'), -- only one rewrite step is done without parallelism.
                rhs = result
              }
            | (term, rewriteTerms) <- matchableRules,
              isrComb term,
              let subTerms = rTermList term,
              let n = length subTerms,
              (pre, segmentList, post) <- segments n,
              unif <- mix [matches l r | (l, r) <- safezip subTerms (map (combLst rCombinator) segmentList)],
              noDoubles unif, -- if one variable is bound to more than one different terms, the deal is off.
              term' <- rewriteTerms,
              let rd = showIT term <> " -> " <> showIT term', -- rule documentation for fatals in 'substitute'
              let original = flatLst (pre ++ substitute rd unif term : post), -- is equal to rCombinator ls
              let result = flatLst (pre ++ substitute rd unif term' : post),
              original
                == rCombinator ls
                || fatal
                  ( "When analysing rule "
                      <> rd
                      <> " with unifier "
                      <> showIT unif
                      <> " on:  "
                      <> showIT (rCombinator ls)
                      <> "\nWe substitute:  "
                      <> showIT (substitute rd unif term)
                      <> "\nby:             "
                      <> showIT (substitute rd unif term')
                      <> ".\nHowever, the original RTerm:  "
                      <> showIT (rCombinator ls)
                      <> "\ndiffers from flatLst (pre <> substitute rd unif term :post):\n  "
                      <> showIT original
                  )
          ]
            <> [ DStep
                   { lhs = rCombinator ls, -- is equal to: (pre<>lhs dstep:post)
                     rul = rul dstep,
                     rhs = flatLst (pre ++ rhs dstep : post)
                   }
                 | (pre, l, post) <- splitList ls,
                   dstep <- dStps l
               ]
      where
        segments :: Int -> [([RTerm], [[RTerm]], [RTerm])]
        segments n =
          [([], ds, []) | ds <- dist n ls]
            <> [(head ds, tail ds, []) | ds <- dist (n + 1) ls]
            <> [([], init ds, last ds) | ds <- dist (n + 1) ls]
            <> [(head ds, (init . tail) ds, last ds) | ds <- dist (n + 2) ls]
        flatLst :: [RTerm] -> RTerm
        flatLst = combLst rCombinator . flat isrComb

    dStepSets :: (RTerm -> Bool) -> (Set RTerm -> RTerm) -> Set RTerm -> [DerivStep]
    dStepSets isrComb rCombinator s =
      -- We try to perform a rewrite on the top level, i.e. on some subset of RTerms from s.
      -- Then, we add rewrites on any of the subexpressins in s.
      -- Example rCombinator s  = RUni { '1', aap, aap\noot, 'Piet', mies;vuur}
      [ DStep
          { lhs = rCombinator s, -- derivs gives the top level rewrites.
            rul = (term, unif, term'), -- only one rewrite is done in parallel in the top level.
            rhs = result -- so rest is left alone, if partition can be rewritten.
          }
        | null [() | e <- Set.toList s, not (isValid e), fatal ("Invalid subexpr: " <> showIT e)],
          -- s = { foo;foo~ , -(bar;bar~) , I[C]}
          (term, rewriteTerms) <- matchableRules,
          isrComb term, -- e.g. term = 'Piet' \/ r \/ p\q
          let subTerms = rTermSet term, -- e.g. subTerms = { 'Piet', r, p\q }
          let termVars = Set.filter isRVar subTerms, -- e.g. termVars = { r }
          let sameTrms = subTerms `Set.intersection` s, -- e.g. sameTrms = { 'Piet' }
          let subExprs = s `Set.difference` sameTrms, -- { '1', aap, aap\noot, mies;vuur }
          let toMatchs = (subTerms `Set.difference` sameTrms) `Set.difference` termVars, -- e.g. toMatchs = { p\q }
          let n = Set.size toMatchs, -- each element of toMatchs can be matched to one subTerm from subExprs.
          let m = Set.size termVars, -- each variable in subTerms must be matched to one subset from rest.
          (matchCandidates, rest) <- separate n subExprs, -- e.g. matchCandidates = {aap\noot} and rest={ '1', aap, mies;vuur }
          (restSets, remainder) <- partsplus m rest, -- e.g. restSets={ {'1', aap, mies;vuur} }
          let restTerms = Set.map (flatSet . Set.toList) restSets, -- e.g. restTerms={ RUni {'1', aap, mies;vuur} }
          Set.null restTerms
            || (isValid . flatSet . Set.toList) restTerms
            || fatal ("Invalid restTerms: " <> showIT (rCombinator restTerms)),
          let remTerm =
                let remT = combSet rCombinator remainder
                 in if Set.null remainder
                      then fatal "empty remTerm"
                      else if isValid remT then remT else fatal ("Invalid remTerm: " <> showIT remT), -- e.g. restTerms={ RUni {'1', aap, mies;vuur} }
          unif0 <-
            if Set.null toMatchs
              then [Set.empty]
              else
                if (not . isValid . combSet rCombinator) toMatchs
                  then fatal ("Invalid toMatchs: " <> showIT (rCombinator toMatchs))
                  else
                    if (not . isValid . combSet rCombinator) matchCandidates
                      then fatal ("Invalid matchCandidates: " <> showIT (rCombinator matchCandidates))
                      else matchSets rCombinator toMatchs matchCandidates, -- e.g. unif0={ p->aap, q->noot }
          unif1 <-
            if Set.null termVars
              then [Set.empty]
              else matchSets rCombinator termVars restTerms, -- e.g. unif1={ r->RUni {'1', aap, mies;vuur} }
          let unif = unif0 `Set.union` unif1, -- e.g. unif={ p->aap, q->noot, r->RUni {'1', aap, mies;vuur} }
          noDoubles unif,
          term' <- rewriteTerms,
          let rd = showIT term <> " -> " <> showIT term', -- rule documentation for fatals in 'substitute'
          let original =
                if Set.null remainder
                  then substitute rd unif term -- is equal to rCombinator ls
                  else flatSet [substitute rd unif term, remTerm],
          let result =
                if Set.null remainder
                  then substitute rd unif term'
                  else flatSet [substitute rd unif term', remTerm],
          original
            == rCombinator s
            || fatal
              ( "When analysing rule "
                  <> rd
                  <> " with unifier "
                  <> showIT unif
                  <> " on:  "
                  <> showIT (rCombinator s)
                  <> "\nWe substitute:  "
                  <> showIT original
                  <> "\nby:             "
                  <> showIT result
                  <> "\nHowever, the original RTerm:  "
                  <> showIT (rCombinator s)
                  <> "\ndiffers from subs term:       "
                  <> showIT original
              )
      ]
        <> [ DStep
               { lhs = rCombinator s, -- is equal to: (pre \/ lhs dstep)
                 rul = rul dstep,
                 rhs = flatSet (pre ++ rhs dstep : post)
               }
             | (pre, l, post) <- splitList (Set.toList s),
               dstep <- dStps l
           ]
      where
        partsplus :: (Ord a) => Int -> Set a -> [(Set (Set a), Set a)]
        partsplus n ss = [(p, Set.empty) | p <- parts n ss] <> [(Set.delete p prt, p) | prt <- parts (n + 1) ss, p <- Set.toList prt]
        flatSet :: [RTerm] -> RTerm
        flatSet = normRT . rCombinator . Set.fromList . flat isrComb

    matchableRules :: [(RTerm, [RTerm])]
    matchableRules =
      [ (template, rewriteTerms) -- each tuple may represent multiple rules.
        | cl <- eqCl lTerm (concatMap f drs), -- divide into classes to save a little on the number of matches.
          let template = lTerm (NE.head cl), -- This is the template against which to match full terms.
          let rewriteTerms = stepTerms template cl,
          not (null rewriteTerms)
      ]
      where
        f (DEquiR l r) = [DInclR l r, DInclR r l]
        f inclusion = [inclusion]
        stepTerms :: RTerm -> NE.NonEmpty DerivRule -> [RTerm]
        stepTerms template cl -- Only select rules with bindings within the template. Otherwise, we would have to "invent" bindings.
          =
          [term' | rule <- NE.toList cl, let term' = rTerm rule, vars term' `Set.isSubsetOf` vars template]
        vars :: RTerm -> Set Name
        vars (RIsc rs) = (Set.unions . map vars . Set.toList) rs
        vars (RUni rs) = (Set.unions . map vars . Set.toList) rs
        vars (RDif l r) = vars l `Set.union` vars r
        vars (RCpl e) = vars e
        vars (RDia l r) = vars l `Set.union` vars r
        vars (RLrs l r) = vars l `Set.union` vars r
        vars (RRrs l r) = vars l `Set.union` vars r
        vars (RRad rs) = foldr (Set.union . vars) Set.empty rs
        vars (RCps rs) = foldr (Set.union . vars) Set.empty rs
        vars (RPrd rs) = foldr (Set.union . vars) Set.empty rs
        vars (RKl0 e) = vars e
        vars (RKl1 e) = vars e
        vars (RFlp e) = vars e
        vars (RId c) = Set.fromList [name c]
        vars (RBind _ c) = Set.fromList [name c]
        vars (RVee s t) = Set.fromList [name s, name t]
        vars (RVar r s t) = Set.fromList [r, name s, name t]
        vars RConst {} = Set.empty
        vars RAtm {} = Set.empty

{-
     showMatchableRules :: [(RTerm,[RTerm])] -> Text
     showMatchableRules rs
      = concat ["\n   "<>showIT l<>" = "<>showIT t | (l,tms) <- rs, t<-tms ]
-}

splitList :: [a] -> [([a], a, [a])]
splitList lst = [(take i lst, l, drop (i + 1) lst) | (i, l) <- zip [0 ..] lst]

instance HasSignature RTerm where
  sign (RIsc a) = sign $ SetPartial.findMin a
  sign (RUni a) = sign $ SetPartial.findMin a
  sign (RDif a _) = sign a
  sign (RCpl a) = sign a
  sign (RDia a b) = Sign (source a) (target b)
  sign (RLrs a b) = Sign (source a) (source b)
  sign (RRrs a b) = Sign (target a) (target b)
  sign (RRad as) = Sign (source (head as)) (target (last as))
  sign (RCps as) = Sign (source (head as)) (target (last as))
  sign (RPrd as) = Sign (source (head as)) (target (last as))
  sign (RKl0 a) = sign a
  sign (RKl1 a) = sign a
  sign (RFlp a) = Sign (target a) (source a)
  sign (RId a) = Sign a a
  sign (RBind _ a) = Sign a a
  sign (RVee a b) = Sign a b
  sign (RAtm _ b) = Sign b b
  sign RVar {} = fatal "Cannot determine the sign of an RVar." -- This should become a haskell type-error when RTerm is polymorphic
  sign (RConst e) = sign e

expr2RTerm :: Expression -> RTerm
expr2RTerm expr =
  if isValid result then result else fatal ("expr2RTerm has produced an invalid result: " <> showIT result)
  where
    result =
      case expr of
        EEqu (l, r) -> expr2RTerm (EIsc (EInc (l, r), EInc (r, l)))
        EInc (l, r) -> expr2RTerm (EUni (ECpl l, r))
        EIsc (l, r) -> combSet RIsc (lSet `Set.union` rSet)
          where
            lSet = case expr2RTerm l of
              RIsc terms -> terms
              trm -> Set.singleton trm
            rSet = case expr2RTerm r of
              RIsc terms -> terms
              trm -> Set.singleton trm
        EUni (l, r) -> combSet RUni (lSet `Set.union` rSet)
          where
            lSet = case expr2RTerm l of
              RUni terms -> terms
              trm -> Set.singleton trm
            rSet = case expr2RTerm r of
              RUni terms -> terms
              trm -> Set.singleton trm
        EDif (l, r) -> RDif (expr2RTerm l) (expr2RTerm r)
        ECpl e -> RCpl (expr2RTerm e)
        EDia (l, r) -> RDia (expr2RTerm l) (expr2RTerm r)
        ELrs (l, r) -> RLrs (expr2RTerm l) (expr2RTerm r)
        ERrs (l, r) -> RRrs (expr2RTerm l) (expr2RTerm r)
        ERad (l, r) -> RRad (lLst <> rLst)
          where
            lLst = case expr2RTerm l of
              RRad terms -> terms
              trm -> [trm]
            rLst = case expr2RTerm r of
              RRad terms -> terms
              trm -> [trm]
        ECps (l, r) -> RCps (lLst <> rLst)
          where
            lLst = case expr2RTerm l of
              RCps terms -> terms
              trm -> [trm]
            rLst = case expr2RTerm r of
              RCps terms -> terms
              trm -> [trm]
        EPrd (l, r) -> RPrd (lLst <> rLst)
          where
            lLst = case expr2RTerm l of
              RPrd terms -> terms
              trm -> [trm]
            rLst = case expr2RTerm r of
              RPrd terms -> terms
              trm -> [trm]
        EKl0 e -> RKl0 (expr2RTerm e)
        EKl1 e -> RKl1 (expr2RTerm e)
        EFlp e -> RFlp (expr2RTerm e)
        EBrk e -> expr2RTerm e
        EDcD {} -> RConst expr
        EDcI c -> RId c
        EEps {} -> RConst expr
        EBin oper c -> RBind oper c
        EDcV sgn -> RVee (source sgn) (target sgn)
        EMp1 a c -> RAtm a c

--   --      _                    -> RConst expr   -- This alternative has been commented out to avoid an "overlapping patterns" warning from Haskell.

rTerm2expr :: RTerm -> Expression
-- implementation note: because RTerms contain variables, it is cumbersome to reconstruct the type. So we don't.
-- Once the variables have been replaced (by means of substitutions) by real terms, we get a type correct term again.
-- As a consequence, we cannot use ./\., .\/., etc. in this code.
rTerm2expr term =
  case term of
    RIsc rs -> case Set.toList (Set.map rTerm2expr rs) of
      [e] -> e
      [] -> fatal "empty set in RIsc is illegal."
      e : es -> let oper l r = EIsc (l, r) in foldr oper e es
    RUni rs -> case Set.toList (Set.map rTerm2expr rs) of
      [e] -> e
      [] -> fatal "empty set in RUni is illegal."
      e : es -> let oper l r = EUni (l, r) in foldr oper e es
    RDif l r -> EDif (rTerm2expr l, rTerm2expr r)
    RCpl e -> ECpl (rTerm2expr e)
    RDia l r -> EDia (rTerm2expr l, rTerm2expr r)
    RLrs l r -> ELrs (rTerm2expr l, rTerm2expr r)
    RRrs l r -> ERrs (rTerm2expr l, rTerm2expr r)
    RRad rs -> case map rTerm2expr rs of
      [e] -> e
      [] -> fatal "empty set in RRad is illegal."
      e : es -> let oper l r = ERad (l, r) in foldr oper e es
    RCps rs -> case map rTerm2expr rs of
      [e] -> e
      [] -> fatal "empty set in RCps is illegal."
      e : es -> let oper l r = ECps (l, r) in foldr oper e es
    RPrd rs -> case map rTerm2expr rs of
      [e] -> e
      [] -> fatal "empty set in RPrd is illegal."
      e : es -> let oper l r = EPrd (l, r) in foldr oper e es
    RKl0 e -> EKl0 $ rTerm2expr e
    RKl1 e -> EKl1 $ rTerm2expr e
    RFlp e -> EFlp $ rTerm2expr e
    RVar r s t -> EDcD (makeDecl r (Sign s t))
    RId c -> EDcI c
    RBind oper c -> EBin oper c
    RVee s t -> EDcV (Sign s t)
    RAtm a c -> EMp1 a c
    RConst e -> e
  where
    makeDecl nm sgn =
      Relation
        { decnm = nm,
          decsgn = sgn,
          declabel = Nothing,
          decprps = fatal "Illegal RTerm in rTerm2expr",
          decDefaults = fatal "Illegal RTerm in rTerm2expr",
          decpr = fatal "Illegal RTerm in rTerm2expr",
          decMean = fatal "Illegal RTerm in rTerm2expr",
          decfpos = fatal "Illegal RTerm in rTerm2expr",
          decusr = fatal "Illegal RTerm in rTerm2expr",
          decpat = fatal "Illegal RTerm in rTerm2expr",
          dechash = hash nm `hashWithSalt` sgn
        }

class ShowIT a where -- class meant for stuff not belonging to A-struct and/or P-struct
  showIT :: a -> Text

instance ShowIT RTerm where
  showIT = showExpr 0
    where
      showExpr :: Int -> RTerm -> Text
      showExpr i expr =
        case expr of
          RIsc ls -> wrap i 2 (T.intercalate " /\\ " [showExpr 3 e | e <- Set.toList ls])
          RUni ls -> wrap i 2 (T.intercalate " \\/ " [showExpr 3 e | e <- Set.toList ls])
          RDif l r -> wrap i 4 (showExpr 5 l <> " - " <> showExpr 5 r)
          RLrs l r -> wrap i 6 (showExpr 7 l <> " / " <> showExpr 7 r)
          RRrs l r -> wrap i 6 (showExpr 7 l <> " \\ " <> showExpr 7 r)
          RDia l r -> wrap i 6 (showExpr 7 l <> " <> " <> showExpr 7 r)
          RCps ls -> wrap i 2 (T.intercalate ";" [showExpr 3 e | e <- ls])
          RRad ls -> wrap i 2 (T.intercalate "!" [showExpr 3 e | e <- ls])
          RPrd ls -> wrap i 2 (T.intercalate "*" [showExpr 3 e | e <- ls])
          RKl0 e -> wrap i 9 (showExpr 9 e <> "*")
          RKl1 e -> wrap i 9 (showExpr 9 e <> "+")
          RFlp e -> wrap i 9 (showExpr 9 e <> "~")
          RCpl e -> wrap i 9 ("-" <> (showExpr 10 e))
          RVar r s t -> fullName r <> showSign2 s t
          RConst e -> wrap i i (showA e)
          RId c -> "I" <> showSign1 c
          RBind oper c -> tshow oper <> showSign1 c
          RVee s t -> "V" <> showSign2 s t
          RAtm val c -> showP val <> showSign1 c
      wrap :: Int -> Int -> Text -> Text
      wrap i j e' = if i <= j then e' else "(" <> e' <> ")"
      showSign2 :: A_Concept -> A_Concept -> Text
      showSign2 s t = "[" <> fullName s <> "*" <> fullName t <> "]"
      showSign1 :: A_Concept -> Text
      showSign1 c = "[" <> fullName c <> "]"

{- momentarily redundant
   unVar :: RTerm -> Text
   unVar (RVar r _ _) = r
   unVar _ = fatal "Illegal call on unVar"
-}

data DerivRule
  = DEquiR
      { lTerm :: RTerm, -- equivalence rule
        rTerm :: RTerm
      }
  | DInclR
      { lTerm :: RTerm, -- inclusion rule
        rTerm :: RTerm
      }

instance Show DerivRule where
  show r@DEquiR {} = T.unpack $ showIT (lTerm r) <> " = " <> showIT (rTerm r)
  show r@DInclR {} = T.unpack $ showIT (lTerm r) <> " |- " <> showIT (rTerm r)

-- For documentation purposes, the derivation rule which proves the step is included.

data DerivStep = DStep
  { lhs :: RTerm,
    rul :: (RTerm, Unifier, RTerm),
    rhs :: RTerm
  }

dRule :: ConceptMap -> Term TermPrim -> [DerivRule]
dRule cptMap term0 = case term0 of
  (PEqu _ l r) -> [DEquiR {lTerm = term2rTerm l, rTerm = term2rTerm r}]
  (PInc _ l r) -> [DInclR {lTerm = term2rTerm l, rTerm = term2rTerm r}]
  _ -> fatal ("Illegal use of dRule with term " <> showP term0)
  -- In order to write deriviation rules in the Ampersand syntax, RTerms are obtained by means of the (already available) Ampersand parser.
  -- For that reason, we need a function term2rTerm to translate a term obtained by parsing (type: Term TermPrim) to a RTerm.
  where
    term2rTerm :: Term TermPrim -> RTerm
    term2rTerm term1 =
      if isValid result
        then result
        else fatal ("term2rTerm has produced an invalid result: " <> showIT result)
      where
        result =
          case term1 of
            PEqu o l r -> term2rTerm (PIsc o (PInc o l r) (PInc o r l))
            PInc o l r -> term2rTerm (PUni o (PCpl o l) r)
            PIsc _ l r -> combSet RIsc (lSet `Set.union` rSet)
              where
                lSet = case term2rTerm l of
                  RIsc terms -> terms
                  trm -> Set.singleton trm
                rSet = case term2rTerm r of
                  RIsc terms -> terms
                  trm -> Set.singleton trm
            PUni _ l r -> combSet RUni (lSet `Set.union` rSet)
              where
                lSet = case term2rTerm l of
                  RUni terms -> terms
                  trm -> Set.singleton trm
                rSet = case term2rTerm r of
                  RUni terms -> terms
                  trm -> Set.singleton trm
            PDif _ l r -> RDif (term2rTerm l) (term2rTerm r)
            PCpl _ e -> RCpl (term2rTerm e)
            PDia _ l r -> RDia (term2rTerm l) (term2rTerm r)
            PLrs _ l r -> RLrs (term2rTerm l) (term2rTerm r)
            PRrs _ l r -> RRrs (term2rTerm l) (term2rTerm r)
            PRad _ l r -> RRad (lLst <> rLst)
              where
                lLst = case term2rTerm l of
                  RRad terms -> terms
                  trm -> [trm]
                rLst = case term2rTerm r of
                  RRad terms -> terms
                  trm -> [trm]
            PCps _ l r -> RCps (lLst <> rLst)
              where
                lLst = case term2rTerm l of
                  RCps terms -> terms
                  trm -> [trm]
                rLst = case term2rTerm r of
                  RCps terms -> terms
                  trm -> [trm]
            PPrd _ l r -> RPrd (lLst <> rLst)
              where
                lLst = case term2rTerm l of
                  RPrd terms -> terms
                  trm -> [trm]
                rLst = case term2rTerm r of
                  RPrd terms -> terms
                  trm -> [trm]
            PKl0 _ e -> RKl0 (term2rTerm e)
            PKl1 _ e -> RKl1 (term2rTerm e)
            PFlp _ e -> RFlp (term2rTerm e)
            PBrk _ e -> term2rTerm e
            Prim (PNamedR (PNamedRel _ str (Just sgn))) -> RVar str (pCpt2aCpt cptMap (pSrc sgn)) (pCpt2aCpt cptMap (pTgt sgn))
            Prim (Pid _ c) -> RId (pCpt2aCpt cptMap c)
            Prim (PBind _ oper c) -> RBind oper (pCpt2aCpt cptMap c)
            Prim (Pfull _ s t) -> RVee (pCpt2aCpt cptMap s) (pCpt2aCpt cptMap t)
            Prim (Patm _ a (Just c)) -> RAtm a (pCpt2aCpt cptMap c)
            Prim (PI _) -> fatal ("Cannot cope with untyped " <> showP term1 <> " in a dRule inside the normalizer.")
            Prim (PBin _ _) -> fatal ("Cannot cope with untyped " <> showP term1 <> " in a dRule inside the normalizer.")
            Prim (Patm _ _ Nothing) -> fatal ("Cannot cope with untyped " <> showP term1 <> " in a dRule inside the normalizer.")
            Prim (PVee _) -> fatal ("Cannot cope with untyped " <> showP term1 <> " in a dRule inside the normalizer.")
            Prim (PNamedR (PNamedRel _ _ Nothing)) ->
              fatal ("Cannot cope with untyped " <> showP term1 <> " in a dRule inside the normalizer.")

weightNF :: Bool -> RTerm -> Integer
weightNF dnf = w
  where
    w :: RTerm -> Integer
    w trm =
      case trm of
        RIsc ls -> sum (map w (Set.toList ls)) * if dnf then 1 else 2
        RUni ls -> sum (map w (Set.toList ls)) * if dnf then 2 else 1
        RDif l r -> (w l + w r + 10) * 4
        RCpl e -> (w e + 1) * 4
        RDia l r -> (w l + w r + 10) * 4
        RLrs l r -> (w l + w r + 10) * 4
        RRrs l r -> (w l + w r + 10) * 4
        RRad ls -> (sum (map w ls) + 1) * 4
        RCps ls -> (sum (map w ls) + 1) * 4
        RPrd ls -> (sum (map w ls) + 1) * 4
        RKl0 e -> (w e + 1) * 4
        RKl1 e -> (w e + 1) * 4
        RFlp e -> (w e + 1) * 8
        _ -> 1

-- If  'matches d expr'  yields  'Just ss', then  'substitute anything ss (lTerm d) == expr'

type Unifier = Set (Name, RTerm)

instance ShowIT Unifier where
  showIT s = "{" <> T.intercalate ", " [fullName nm <> "->" <> showIT t | (nm, t) <- Set.toList s] <> "}"

substitute ::
  Text -> -- A text to document fatals
  Unifier -> -- the substitution, which in reality is a set of text/term pairs.
  RTerm -> -- The term to be transformed to a term, with all variables replaced by subexpressions
  RTerm
substitute ruleDoc unifier term =
  if isValid result then result else fatal ("substitute has produced an invalid result: " <> showIT result)
  where
    result = subs term
    subs :: RTerm -> RTerm
    subs t | not (isValid t) = fatal ("Substituting an invalid term " <> showIT t)
    subs (RIsc s) = (combSet RIsc . Set.fromList . flat isRIsc . map subs . Set.toList) s
    subs (RUni s) = (combSet RUni . Set.fromList . flat isRUni . map subs . Set.toList) s
    subs (RDif l r) = RDif (subs l) (subs r)
    subs (RLrs l r) = RLrs (subs l) (subs r)
    subs (RRrs l r) = RRrs (subs l) (subs r)
    subs (RDia l r) = RDia (subs l) (subs r)
    subs (RCps ls) = (RCps . flat isRCps . map subs) ls
    subs (RRad ls) = (RRad . flat isRRad . map subs) ls
    subs (RPrd ls) = (RPrd . flat isRPrd . map subs) ls
    subs (RKl0 e) = RKl0 (subs e)
    subs (RKl1 e) = RKl1 (subs e)
    subs (RFlp e) = RFlp (subs e)
    subs (RCpl e) = RCpl (subs e)
    subs (RVar r _ _) = case substExprs r of
      [e] -> e
      [] -> fatal ("Rule:  " <> ruleDoc <> "\nVariable " <> fullName r <> " is not in term " <> showIT term <> " using unifier " <> tshow unifier)
      -- e.g. Variable r is not in term -V[A*B] /\ r[A*B] using unifier fromList [("A",RId Verzoek),("B",RId Persoon)]
      es -> fatal ("Rule:  " <> ruleDoc <> "\nVariable " <> fullName r <> " in term " <> showIT term <> " has been bound to multiple terms:\n   " <> T.intercalate "\n   " (map showIT es))
    subs (RId c) = case substExprs (name c) of
      [e] -> e -- This is e@(RId c')
      [] -> fatal ("Rule:  " <> ruleDoc <> "\nVariable " <> fullName c <> " is not in term " <> showIT term)
      es -> fatal ("Rule:  " <> ruleDoc <> "\nVariable " <> fullName c <> " in term " <> showIT term <> " has been bound to multiple terms:\n   " <> T.intercalate "\n   " (map showIT es))
    subs (RBind _ c) = case substExprs (name c) of
      [e] -> e -- This is e@(RBind oper c')
      [] -> fatal ("Rule:  " <> ruleDoc <> "\nVariable " <> fullName c <> " is not in term " <> showIT term)
      es -> fatal ("Rule:  " <> ruleDoc <> "\nVariable " <> fullName c <> " in term " <> showIT term <> " has been bound to multiple terms:\n   " <> T.intercalate "\n   " (map showIT es))
    subs (RVee s t) = case (substExprs (name s), substExprs (name t)) of
      ([RId s'], [RId t']) -> RVee s' t'
      (_, _) -> fatal ("Rule:  " <> ruleDoc <> "\nSomething wrong with RVee in term " <> showIT term <> " with unifier " <> tshow unifier)
    subs (RAtm a c) = case substExprs (name c) of
      [RId c'] -> RAtm a c'
      [] -> fatal ("Rule:  " <> ruleDoc <> "\nVariable " <> fullName c <> " is not in term " <> showIT term)
      es -> fatal ("Rule:  " <> ruleDoc <> "\nVariable " <> fullName c <> " in term " <> showIT term <> " has been bound to multiple terms:\n   " <> T.intercalate "\n   " (map showIT es))
    subs e@RConst {} = e
    --     subs t            = fatal ("Rule:  "<>ruleDoc<>"\nError: "<>showIT t<>"is not a variable.")  -- commented out, because it causes Haskell to emit an overlapping pattern warning.
    substExprs x = [e | (v, e) <- Set.toList unifier, v == x]

flat :: (RTerm -> Bool) -> [RTerm] -> [RTerm]
flat isrComb ls =
  case ls of
    [] -> fatal "Illegal empty list"
    es -> concat [if isrComb e then rTermListForSets e else [e] | e <- es]

--     es  -> if or [isrComb e | e<-es]   -- SJ: Apparently, the recursion in 'flat' is required. Without it, Misc/Kernmodel.adl did fail on 18 aug 2014.
--            then (flat isrComb . concat) [ if isrComb e then rTermListForSets e else [e] | e<-es]
--            else es
rTermListForSets :: RTerm -> [RTerm]
rTermListForSets (RIsc s) = Set.toList s
rTermListForSets (RUni s) = Set.toList s
rTermListForSets x = rTermList x

matches :: RTerm -> RTerm -> [Unifier]
matches term expr
  | not (isValid term) = fatal ("Invalid term " <> showIT term <> "\nbeing matched to term " <> showIT expr)
  | not (isValid expr) = fatal ("Matching term " <> showIT term <> "\nto invalid term " <> showIT expr)
  | otherwise =
      case (term, expr) of
        (RIsc es, RIsc es') -> matchSets RIsc es es'
        (RUni es, RUni es') -> matchSets RUni es es'
        (RDif l r, RDif l' r') -> matches l l' <> matches r r'
        (RLrs l r, RLrs l' r') -> matches l l' <> matches r r'
        (RRrs l r, RRrs l' r') -> matches l l' <> matches r r'
        (RDia l r, RDia l' r') -> matches l l' <> matches r r'
        (RCps ls, RCps ls') -> matchLists RCps ls ls'
        (RRad ls, RRad ls') -> matchLists RRad ls ls'
        (RPrd ls, RPrd ls') -> matchLists RPrd ls ls'
        (RKl0 e, RKl0 e') -> matches e e'
        (RKl1 e, RKl1 e') -> matches e e'
        (RFlp e, RFlp e') -> matches e e'
        (RCpl e, RCpl e') -> matches e e'
        (RId c, RId _) -> [Set.fromList [(name c, expr)]]
        (RVee s t, RVee s' t') -> [Set.fromList [(name s, RId s'), (name t, RId t')]]
        (RVar v s t, _) -> [Set.fromList [(v, expr), (name s, RId (source expr)), (name t, RId (target expr))]]
        (RAtm a c, RAtm a' c') -> [Set.singleton (name c, RId c') | a == a']
        (RConst e, RConst e') -> [Set.empty | e == e']
        (_, _) -> []

matchLists :: ([RTerm] -> RTerm) -> [RTerm] -> [RTerm] -> [Unifier]
matchLists rCombinator es es'
  | not (isValid (combLst rCombinator es)) = fatal ("Invalid term " <> showIT (rCombinator es) <> "\nbeing matched to term " <> showIT (rCombinator es'))
  | not (isValid (combLst rCombinator es')) = fatal ("Matching term " <> showIT (rCombinator es) <> "\nto invalid term " <> showIT (rCombinator es'))
  | otherwise =
      [ unif
        | let n = length es, -- the length of the template, which contains variables
          n /= 0 || fatal "n equals 0",
          ms <- dist n es', -- determine segments from es' (which is variable free) that have the same length as the template es
          not (or [null m | m <- ms])
            || fatal (T.concat ["\nms:  [" <> T.intercalate ", " (map showIT m) <> "]" | m <- ms]),
          let subTerms = map (combLst rCombinator) ms, -- make an RTerm from each sublist in ms
          unif <- mix [matches l r | (l, r) <- safezip es subTerms],
          noDoubles unif -- if one variable, v, is bound to more than one different terms, the deal is off.
      ]

mix :: [[Unifier]] -> [Unifier]
mix (ls : lss) = [Set.union e str | e <- ls, str <- mix lss]
mix [] = [Set.empty]

{- example:
   mix ["12","xyz","","p"] = [] -- (because the fourth element did not match!)
   mix ["12","xyz","p"]    = ["1xp","1yp","1zp","2xp","2yp","2zp"]
-}
matchSets :: (Set RTerm -> RTerm) -> Set RTerm -> Set RTerm -> [Unifier]
matchSets rCombinator es es'
  -- set sizes are not necessarily equal.
  | Set.null es || Set.null es' = fatal "cannot match empty sets"
  | or [not (isValid e) | e <- Set.toList es] = fatal ("Invalid subterm(s): " <> T.intercalate ", " [showIT e | e <- Set.toList es, not (isValid e)])
  | or [not (isValid e) | e <- Set.toList es'] = fatal ("Invalid subexpr(s): " <> T.intercalate ", " [showIT e | e <- Set.toList es', not (isValid e)])
  | otherwise =
      [ unif
        | let n = Set.size cdes, -- the length of the template, which contains variables
          partition' <- parts n cdes', -- determine segments from the term with the same length. partition' :: Set (Set RTerm)
          let subTerms = Set.map (combSet rCombinator) partition', -- make an RTerm from each subset in ms. subTerms :: Set RTerm
          template <- L.permutations (Set.toList cdes),
          unif <- mix [matches l r | (l, r) <- safezip template (Set.toList subTerms)],
          noDoubles unif -- if one variable, v, is bound to more than one different terms, the deal is off.
      ]
  where
    isct = es `Set.intersection` es' -- E.g.:  {'Piet'}
    cdes = es `Set.difference` isct -- the terms of es that are not in es' (a set of templates). E.g.: { r;s }
    cdes' = es' `Set.difference` isct -- candidates for binding to a variable: { a\b , a;b;c , d , e;f }  (a set of terms)

separate :: (Ord a) => Int -> Set a -> [(Set a, Set a)]
separate n s = [(part, s `Set.difference` part) | part <- subsetLength n (Set.toList s)]
  where
    subsetLength :: (Ord a) => Int -> [a] -> [Set a]
    subsetLength 0 _ = [Set.empty]
    subsetLength i (x : xs) = map (Set.insert x) (subsetLength (i - 1) xs) <> subsetLength i xs
    subsetLength _ [] = []

-- parts produces a fixed number of subsets
parts :: (Ord a) => Int -> Set a -> [Set (Set a)] -- ,   but within this where clause we must make it more specific.
parts n = Set.toList . Set.fromList . map (Set.fromList . map Set.fromList) . p n . Set.toList
  where
    p :: (Eq a) => Int -> [a] -> [[[a]]]
    p 0 _ = []
    p 1 xs = [[xs]]
    p 2 xs = [[ss, rest] | ss <- init (subsets xs), let rest = [e | e <- xs, e `notElem` ss], not (null rest)]
    p i xs = [twoSets <> tl | (hd : tl) <- p (i - 1) xs, twoSets <- p 2 hd]
    {- examples:
       parts 1 "abcd" = {{"abcd"}}
       parts 2 "abcd" = {{"a","bcd"},{"ab","cd"},{"abc","d"},{"abd","c"},{"ac","bd"},{"acd","b"},{"ad","bc"}}
       parts 3 "abcd" = {{"a","b","cd"},{"a","bc","d"},{"a","bd","c"},{"ab","c","d"},{"ac","b","d"},{"ad","b","c"}}
       parts 4 "abcd" = {{"a","b","c","d"}}
       parts 3 "abcde"
        = { {"a","b","cde"},{"a","bc","de"},{"a","bcd","e"},{"a","bce","d"},{"a","bd","ce"},{"a","bde","c"}
          , {"a","be","cd"},{"ab","c","de"},{"ab","cd","e"},{"ab","ce","d"},{"abc","d","e"},{"abd","c","e"}
          , {"abe","c","d"},{"ac","b","de"},{"ac","bd","e"},{"ac","be","d"},{"acd","b","e"},{"ace","b","d"}
          , {"ad","b","ce"},{"ad","bc","e"},{"ad","be","c"},{"ade","b","c"},{"ae","b","cd"},{"ae","bc","d"}
          , {"ae","bd","c"}
          }
       parts 6 "abcde" = {}
    -}
    subsets :: [a] -> [[a]]
    subsets [] = [[]]
    subsets (x : xs) = map (x :) (subsets xs) <> subsets xs

combLst :: ([RTerm] -> RTerm) -> [RTerm] -> RTerm
combLst rCombinator es =
  case es of
    [] -> fatal "Not allowed."
    [e] -> e
    _ -> rCombinator es

combSet :: (Set RTerm -> RTerm) -> Set RTerm -> RTerm
combSet rCombinator s =
  case Set.toList s of
    [] -> fatal "Not allowed."
    [e] -> e
    _ -> rCombinator s

-- Example: noDoubles { p->A;B, q->'Piet', p->'Z', r->A* } is False, because p binds two different terms.
noDoubles :: Unifier -> Bool
noDoubles unif = and [n == 1 | n <- (map length . eqCl fst . Set.toList) unif]

safezip :: [a] -> [b] -> [(a, b)]
safezip (a : as) (b : bs) = (a, b) : safezip as bs
safezip [] [] = []
safezip _ _ = fatal "Zip of two lists with different lengths!"

{-
   assignments {a,p} {2,3,4}
=
   { {(a,2), (p,3)}, {(a,2), (p,4)}, {(a,3), (p,4)}, {(a,3), (p,4)}, {(a,4), (p,4)}, {(a,4), (p,3)} }

   assignments :: (Ord a, Ord b) => Set a -> Set b -> [Set (a,b)]
   assignments xs ys = map Set.fromList (recur (Set.toList xs) (Set.toList ys))
    where
      recur [] _ = [[]]
      recur (v:vs) es = [ (v,e):pairs | e<-es, pairs<-recur vs [e' | e'<-es, e'/=e ] ]

-- The function 'names' exists for the purpose of hashing.
   names :: RTerm -> [Text]
   names term = nub (nms term)
    where nms trm = case trm of
                        RIsc ls    -> (nub . concatMap nms . Set.toList) ls
                        RUni ls    -> (nub . concatMap nms . Set.toList) ls
                        RDif l r   -> nms l<>nms r
                        RLrs l r   -> nms l<>nms r
                        RRrs l r   -> nms l<>nms r
                        RDia l r   -> nms l<>nms r
                        RCps ls    -> (nub . concatMap nms) ls
                        RRad ls    -> (nub . concatMap nms) ls
                        RPrd ls    -> (nub . concatMap nms) ls
                        RKl0 e     -> nms e
                        RKl1 e     -> nms e
                        RFlp e     -> nms e
                        RCpl e     -> nms e
                        RVar r s t -> [r<>":"<>s<>"*"<>t]
                        RId c      -> ["I["<>name c<>"]"]
                        RVee s t   -> ["V["<>name s<>"*"<>name t<>"]"]
                        RAtm a c   -> ["'"<>a<>"'["<>name c<>"]"]
                        RConst e   -> [showIT e]
-}

-- In order to write rules for the normalizer in a legible manner, I am using the Ampersand parser.
-- The terminal symbols, except I and V, are interpreted as variables in these rules.
-- As these rules may be used in two directions, all concept variables that are used on one side must be used on the other side as well.
-- relation names r, s, q are used as relation variables and A, B, C are used as concept variables.
-- If rules are ill formed, this will result in fatal errors.

-- Type conserving equivalences: The following equivalences have an identical signature on either side.
tceDerivRules :: ConceptMap -> [DerivRule]
tceDerivRules cptMap =
  concatMap
    (dRule cptMap . parseRule)
    --    [ "r[A*B]\\/s[A*B] = s[A*B]\\/r[A*B]"                         --  Commutativity of \/
    --    , "r[A*B]/\\s[A*B] = s[A*B]/\\r[A*B]"                         --  Commutativity of /\
    --    , "(r[A*B]\\/s[A*B])\\/q[A*B] = r[A*B]\\/(s[A*B]\\/q[A*B])"   --  Associativity of \/
    --    , "(r[A*B]/\\s[A*B])/\\q[A*B] = r[A*B]/\\(s[A*B]/\\q[A*B])"   --  Associativity of /\
    --    , "(r[A*B];s[B*C]);q[C*D] = r[A*B];(s[B*C];q[C*D])"           --  Associativity of ;
    --    , "(r[A*B]#s[B*C])#q[C*D] = r[A*B]#(s[B*C]#q[C*D])"           --  Associativity of #
    --    , "(r[A*B]!s[B*C])!q[C*D] = r[A*B]!(s[B*C]!q[C*D])"           --  Associativity of !
    [ "-(-r[A*B]) = r[A*B]", --  Double negation
      "(r[A*B]~)~ = r[A*B]", --  Double flip
      "-(r[A*B]~) = (-r[A*B])~", --  Peirce's[A*A] trick, which allows us to write -r[A*A]~
      "-r[A*B]/\\-s[A*B] = -(r[A*B]\\/s[A*B])", --  De Morgan
      "-r[A*B]\\/-s[A*B] = -(r[A*B]/\\s[A*B])", --  De Morgan
      "-r[B*A];-s[A*C] = -(r[B*A]!s[A*C])", --  De Morgan
      "-r[B*A]!-s[A*C] = -(r[B*A];s[A*C])", --  De Morgan
      "r[A*B]/\\-s[C*D] = r[A*B]-s[C*D]", --  Avoid complement
      "r[A*B]~/\\s[A*B]~ = (r[A*B]/\\s[A*B])~", --  Distribute flip
      "r[A*B]~/\\-s[C*D]~ = (r[A*B]-s[C*D])~", --  Avoid complement
      "r[A*B]~\\/s[A*B]~ = (r[A*B]\\/s[A*B])~", --  Distribute flip
      "(r[A*A]\\r[A*A]);(r[A*A]\\r[A*A]) = r[A*A]\\r[A*A]", --  Jipsen&Tsinakis
      "(r[A*A]/r[A*A]);(r[A*A]/r[A*A]) = r[A*A]/r[A*A]", --  Jipsen&Tsinakis
      "r[A*A];(r[A*A]\\r[A*A]) = r[A*A]", --  Jipsen&Tsinakis
      "(r[A*A]/r[A*A]);r[A*A] = r[A*A]", --  Jipsen&Tsinakis
      "I[A];r[A*B] = r[A*B]",
      "r[A*B];I[B] = r[A*B]",
      "(r[A*B]\\/s[A*B]);q[B*C] = r[A*B];q[B*C]\\/s[A*B];q[B*C]", --  Distribution
      "r[A*B];(s[B*C]\\/q[B*C]) = r[A*B];s[B*C]\\/r[A*B];q[B*C]", --  Distribution
      "-r[A*B]~!s[A*C] = r[A*B]\\s[A*C]", --  eliminate dagger
      "-r[A*B]!s[B*C] = r[A*B]~\\s[B*C]", --  eliminate dagger
      "r[A*C]!-s[B*C]~ = r[A*C]/s[B*C]", --  eliminate dagger
      "r[A*C]!-s[C*B] = r[A*C]/s[C*B]~", --  eliminate dagger
      --  , "r[A*B]#s[B*C]#q[C*D] = r[A*B]#q[C*D]"                      --  eliminate middle in cartesian product -- conditie toevoegen: s[A*B] /= -V
      "r[A*B]/\\r[A*B] = r[A*B]", --  Absorb equals
      "r[A*B]\\/r[A*B] = r[A*B]", --  Absorb equals
      "r[A*B]/\\V[A*B] = r[A*B]", --  Absorb V
      "V[A*B]/\\r[A*B] = r[A*B]", --  Absorb V
      "r[A*B]/\\-V[A*B] = -V[A*B]", --  Contradiction
      "-V[A*B]/\\r[A*B] = -V[A*B]", --  Contradiction
      "r[A*B]\\/V[A*B] = V[A*B]", --  Tautology
      "r[A*B]\\/-V[A*B] = r[A*B]", --  Absorb -V
      "r[A*B]/\\-r[A*B] = -V[A*B]", --  Contradiction
      "r[A*B]\\/-r[A*B] =  V[A*B]", --  Tautology
      "-r[A*B]\\/r[A*B] = V[A*B]", --  Tautology
      "(r[A*B]\\/ s[A*B])/\\ s[A*B] = s[A*B]", --  Absorption
      "(r[A*B]\\/-s[A*B])/\\ s[A*B] = s[A*B]-r[A*B]", --  Absorption
      "(r[A*B]/\\ s[A*B])\\/ s[A*B] = s[A*B]", --  Absorption
      "(r[A*B]/\\-s[A*B])\\/ s[A*B] = r[A*B]\\/s[A*B]", --  Absorption
      "(r[A*B]/\\ s[A*B])\\/-s[A*B] = r[A*B]\\/-s[A*B]", --  Absorption
      "r[A*A]* = r[A*A];r[A*A]*",
      "r[A*A]* = r[A*A]*;r[A*A]",
      "r[A*A]+ = r[A*A];r[A*A]+",
      "r[A*A]+ = r[A*A]+;r[A*A]",
      "I[A]\\/r[A*A]+ = r[A*A]*"
    ]

{-
-- Type conserving inclusions: The following inclusions have an identical signature on either side.
tciDerivRules :: [DerivRule]
tciDerivRules = concatMap (dRule.parseRule)
 [ "(r[A*B]\\I[A]);s[A*C] |- r[A*B]\\s[A*C]"                   --  T{r\\I[A]}=[B*A] ; T{(r\\I[A]);s}=[B*C] ; T{r\\s}=[B*C] ; Jipsen&Tsinakis
 , "r[A*C];(I[C]/s[B*C]) |- r[A*C]/s[B*C]"                     --  Jipsen&Tsinakis
 , "(r[A*B]\\s[A*C]);q[C*D] |- r[A*B]\\(s[A*C];q[C*D])"        --  Jipsen&Tsinakis
 , "r[A*B];(s[B*C]/q[D*C]) |- (r[A*B];s[B*C])/q[D*C]"          --  Jipsen&Tsinakis
 , "(r[A*B]\\s[A*C]);(s[A*C]\\q[A*D]) |- r[A*B]\\q[A*D]"       --  Jipsen&Tsinakis
 , "(r[A*B]/s[A*C]);(s[A*C]/q[D*B]) |- r[A*B]/q[D*B]"          --  Jipsen&Tsinakis
 , "r[A*B];(s[B*C]!q[C*D]) |- (r[A*B];s[B*C])!q[C*D]"          --  Peirce
 , "(r[A*B]!s[B*C]);q[C*D] |- r[A*B]!(s[B*C];q[C*D])"          --  Peirce
 , "(r[A*B]/\\s[A*B]);q[B*C] |- r[A*B];q[B*C]/\\s[A*B];q[B*C]" --  Distribution
 , "r[A*B];(s[B*C]/\\q[B*C]) |- r[A*B];s[B*C]/\\r[A*B];q[B*C]" --  Distribution
 , "(r[A*B];s[B*C])/s[B*C] |- r[A*B]"                          --  Absorption
 , "r[A*B]\\(r[A*B];s[B*C]) |- s[B*C]"                         --  Absorption
 , "I[A] |- r[A*A]*"
 , "r[A*B] |- V[A*B]"
 ]
-}
{-
-- Type altering equivalences: The following equivalences have an different signature on either side.
taeDerivRules :: [DerivRule]
taeDerivRules = concatMap (dRule.parseRule)
 [ "-r[A*B]\\/(q[A*C]/s[B*C]) = -(r[A*B];s[B*C])\\/q[A*C]"     -- T{ -r\\/(q/s)} = [A*B] ;   T{ -(r;s)\\/q} = [A*C] ; remove left residual (/)
 , "(r[A*B]\\q[A*C])\\/-s[B*C] = -(r[A*B];s[B*C])\\/q[A*C]"    -- T{ (r\\q)\\/-s)} = [B*C] ; T{ -(r;s)\\/q} = [A*C] ; remove right residual (\\)
 ]
-}
{-
-- | This delta is meant to be used as a placeholder for inserting or removing links from terms.
delta :: Signature -> Expression
delta sgn
 = EDcD Relation
              { decnm   = T.pack "Delta"
              , decsgn  = sgn
              , decprps = Set.empty
              , decprL  = ""
              , decprM  = ""
              , decprR  = ""
              , decMean =
                  [ Meaning (Markup Dutch   (text2Blocks ReST "Delta is bedoeld als variabele, die de plaats in een expressie vasthoudt waar paren worden ingevoegd of verwijderd."))
                  , Meaning (Markup English (text2Blocks ReST "Delta is meant as a variable, to be used as a placeholder for inserting or removing links from terms."))
                  ]
              , decfpos = Origin ("generated relation (Delta "<>show sgn<>")")
              , decusr  = False
              , decpat  = Nothing
              , dechash = hash sgn
              }
-}
{- Normalization of process algebra clauses -}

type Proof a = [(a, [Text], Text)]

{- A proof is a list of triples (e, ss, rel), where |e| is a term in the chain;
   |rel| is the relation (equality, inclusion, ...) relating the |e| with the next
   term, and |ss| is a documentation of the hint, stating the rule that has been applied.
   2015-09-12 Stef thinks that the end of the chain is the only triple with empty hint,
   being supported by the base case of |proofPA| below.

   WK: I typically do |(a, [(rel, hint, a)])|.
-}

{- Normalization of terms -}

simplify :: Expression -> Expression
simplify expr = expr'
  where
    (expr', _, _) = if null (simpProof shw expr) then fatal "last: empty list" else last (simpProof shw expr)
    shw _ = ""

simpProof :: (Expression -> Text) -> Expression -> Proof Expression
simpProof shw expr =
  if expr == res
    then [(expr, [], "<=>")]
    else (expr, steps, equ) : simpProof shw res
  where
    (res, steps, equ) = normStep shw True True expr

-- | The purpose of "normStep" is to elaborate a single step in a rewrite process,
-- in which the term is normalized by means of rewrite rules.
-- This function can be used for simplification, which means that an Expression is standardized
-- using associativity and other 'trivial' rules only.
-- These 'trivial' rules do not produce a step in the proof.
-- Use normstep shw eq True expr to do simplification only.
-- Use normstep shw eq False expr to obtain a single proof step or none when no rule is applicable.
-- This function returns a resulting term that is closer to a normal form.
-- The normal form is not unique. This function simply uses the first rewrite rule it encounters.
normStep ::
  (Expression -> Text) ->
  Bool ->
  Bool ->
  Expression ->
  (Expression, [Text], Text) -- This might be generalized to "Expression" if it weren't for the fact that flip is embedded in the Relation type.
normStep
  shw -- a function to print a term. Might be "showIT"
  eq -- If eq==True, only equivalences are used. Otherwise, inclusions are used as well.
  simpl -- If True, only simplification rules are used, which is a subset of all rules. Consequently, simplification is implied by normalization.
  expr =
    if sign expr == sign res
      then (res, ss, equ)
      else fatal ("Violation of sign expr==sign res in the normalizer\n  expr: sign( " <> showA expr <> " ) == " <> (text1ToText . showSign) res <> "\n  res:  sign( " <> showA res <> " ) == " <> (text1ToText . showSign) res)
    where
      {-SJ 20140720: You might wonder why we test sign expr==sign res, which was introduced as a result of ticket #409 (the residu bug)
      It turns out that many rewrite rules in the normalizer change the type of a term; an aspect I have been overlooking all the time.
      Until the new normalizer works, we will have to work with this one. So I have inserted this test to ensure that the type remains constant during normalization.
      -}

      (res, ss, equ) = nM True expr []
      nM :: Bool -> Expression -> [Expression] -> (Expression, [Text], Text)
      -- posCpl indicates whether the term is positive under a complement. It is False when expr is inside a complemented term.
      nM posCpl (EEqu (l, r)) _ | simpl = (t .==. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM posCpl l [] -- TODO: the use of posCpl is erroneous
          (f, steps', equ'') = nM posCpl r [] -- TODO: the use of posCpl is erroneous
      nM posCpl (EInc (l, r)) _ | simpl = (t .|-. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM (not posCpl) l []
          (f, steps', equ'') = nM posCpl r []
      nM posCpl (EUni (EUni (l, k), r)) rs = nM posCpl (l .\/. (k .\/. r)) rs -- standardize, using associativity of .\/.
      nM posCpl (EUni (l, r)) rs | simpl = (t .\/. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM posCpl l []
          (f, steps', equ'') = nM posCpl r (l : rs)
      nM posCpl (EIsc (EIsc (l, k), r)) rs = nM posCpl (l ./\. (k ./\. r)) rs -- standardize, using associativity of ./\.
      nM posCpl (EIsc (l, r)) rs | simpl = (t ./\. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM posCpl l []
          (f, steps', equ'') = nM posCpl r (l : rs)
      nM posCpl (ECps (ECps (l, k), r)) rs = nM posCpl (l .:. (k .:. r)) rs -- standardize, using associativity of .:.
      -- Note: function shiftL and shiftR make use of the fact that this normalizes to (l .:. (k .:. r))
      nM posCpl (ECps (l, r)) rs | simpl = (t .:. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM posCpl l []
          (f, steps', equ'') = nM posCpl r (l : rs)
      nM posCpl (ELrs (l, r)) _ | simpl = (t ./. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM posCpl l []
          (f, steps', equ'') = nM (not posCpl) r []
      nM posCpl (ERrs (l, r)) _ | simpl = (t .\. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM (not posCpl) l []
          (f, steps', equ'') = nM posCpl r []
      nM posCpl (ERad (ERad (l, k), r)) rs = nM posCpl (l .!. (k .!. r)) rs -- standardize, using associativity of .!.
      nM posCpl (ERad (l, r)) rs | simpl = (t .!. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM posCpl l []
          (f, steps', equ'') = nM posCpl r (l : rs)
      nM posCpl (EPrd (EPrd (l, k), r)) rs = nM posCpl (l .*. (k .*. r)) rs -- standardize, using associativity of .*.
      nM posCpl (EPrd (l, r)) _ | simpl = (t .*. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM posCpl l []
          (f, steps', equ'') = nM posCpl r []
      nM posCpl (EKl0 e) _ = (EKl0 res', steps, equ')
        where
          (res', steps, equ') = nM posCpl e []
      nM posCpl (EKl1 e) _ = (EKl1 res', steps, equ')
        where
          (res', steps, equ') = nM posCpl e []
      nM posCpl (ECpl (ECpl e)) rs = nM posCpl e rs
      nM posCpl (ECpl e) _ | simpl = (notCpl res', steps, equ')
        where
          (res', steps, equ') = nM (not posCpl) e []
      nM posCpl (EBrk e) _ = nM posCpl e []
      nM posCpl (EFlp (ECpl e)) rs = nM posCpl (notCpl (flp e)) rs
      nM _ x _ | simpl = (x, [], "<=>")
      -- up to here, simplification has been treated. The remaining rules can safely assume  simpl==False
      nM _ (EEqu (l, r)) _ = ((l .|-. r) ./\. (r .|-. l), ["remove ="], "<=>")
      nM _ (EInc (x, r@(ELrs (z, y)))) _ =
        if sign x == sign z -- necessary to guarantee that sign expr is equal to sign of the result
          then (x .:. y .|-. z, ["remove left residual (/)"], "<=>")
          else (notCpl x .\/. r, ["remove |-"], "<=>")
      nM _ (EInc (y, r@(ERrs (x, z)))) _ =
        if sign y == sign z -- necessary to guarantee that sign expr is equal to sign of the result
          then (x .:. y .|-. z, ["remove right residual (\\)"], "<=>")
          else (notCpl y .\/. r, ["remove |-"], "<=>")
      nM _ (EInc (l, r)) _ = (notCpl l .\/. r, ["remove |-"], "<=>")
      --   nM posCpl e@(ECpl EIsc{}) _           | posCpl==dnf = (deMorganEIsc e, ["De Morgan"], "<=>")
      --   nM posCpl e@(ECpl EUni{}) _           | posCpl/=dnf = (deMorganEUni e, ["De Morgan"], "<=>")
      nM _ e@(ECpl EIsc {}) _ = (deMorganEIsc e, ["De Morgan"], "<=>")
      nM _ e@(ECpl EUni {}) _ = (deMorganEUni e, ["De Morgan"], "<=>")
      nM _ e@(ECpl (ERad (_, ECpl {}))) _ = (deMorganERad e, ["De Morgan"], "<=>")
      nM _ e@(ECpl (ERad (ECpl {}, _))) _ = (deMorganERad e, ["De Morgan"], "<=>")
      nM _ e@(ECpl (ECps (ECpl {}, ECpl {}))) _ = (deMorganECps e, ["De Morgan"], "<=>")
      nM posCpl (ECpl e) _ = (notCpl res', steps, equ')
        where
          (res', steps, equ') = nM (not posCpl) e []
      nM _ (ECps (EEps c (Sign s _), EEps c' (Sign _ t'))) _ | c == c' = (EEps c (Sign s t'), [], "<=>")
      nM _ (ECps (EEps c (Sign s t), EEps c' (Sign _ t'))) _ | c == t = (EEps c' (Sign s t'), [], "<=>")
      nM _ (ECps (EEps c (Sign s _), EEps c' (Sign s' t'))) _ | s' == c' = (EEps c (Sign s t'), [], "<=>")
      nM _ (ECps (EEps c (Sign s _), ECps (EEps c' (Sign _ t'), r))) _ | c == c' = (ECps (EEps c (Sign s t'), r), [], "<=>")
      nM _ (ECps (EEps c (Sign s t), ECps (EEps c' (Sign _ t'), r))) _ | c == t = (ECps (EEps c' (Sign s t'), r), [], "<=>")
      nM _ (ECps (EEps c (Sign s _), ECps (EEps c' (Sign s' t'), r))) _ | s' == c' = (ECps (EEps c (Sign s t'), r), [], "<=>")
      nM _ (ECps (ERrs (x, e), y)) _ | not eq && isIdent e = (ERrs (x, y), ["Jipsen&Tsinakis: (x\\I);y |- x\\y"], "==>")
      nM _ (ECps (x, ELrs (e, y))) _ | not eq && isIdent e = (ELrs (x, y), ["Jipsen&Tsinakis: x;(I/y) |- x/y"], "==>")
      nM _ (ECps (ERrs (x, y), z)) _ | not eq = (ERrs (x, ECps (y, z)), ["Jipsen&Tsinakis: (x\\y);z |- x\\(y;z)"], "==>")
      nM _ (ECps (x, ELrs (y, z))) _ | not eq = (ERrs (x, ECps (y, z)), ["Jipsen&Tsinakis: x;(y/z) |- (x;y)/z"], "==>")
      nM _ (ECps (ERrs (x, y), ERrs (y', z))) _ | not eq && y == y' = (ERrs (x, z), ["Jipsen&Tsinakis: (x\\y);(y\\z) |- x\\z"], "==>")
      nM _ (ECps (ELrs (x, y), ELrs (y', z))) _ | not eq && y == y' = (ERrs (x, z), ["Jipsen&Tsinakis: (x/y);(y/z) |- x/z"], "==>")
      nM _ (ECps (ERrs (x, y), ERrs (y', z))) _ | y == y' && x == y && x == z = (ERrs (x, z), ["Jipsen&Tsinakis: (x\\x);(x\\x) = x\\x"], "<=>")
      nM _ (ECps (ELrs (x, y), ELrs (y', z))) _ | y == y' && x == y && x == z = (ERrs (x, z), ["Jipsen&Tsinakis: (x/x);(x/x) = x/x"], "<=>")
      nM _ (ECps (x, ERrs (y, z))) _ | x == y && x == z = (x, ["Jipsen&Tsinakis: x;(x\\x) = x"], "<=>")
      nM _ (ECps (ELrs (x, y), z)) _ | x == z && y == z = (x, ["Jipsen&Tsinakis: (x/x);x = x"], "<=>")
      nM _ (ECps (l, r)) _ | isIdent l = (r, ["I;x = x"], "<=>")
      nM _ (ECps (l, r)) _ | isIdent r = (l, ["x;I = x"], "<=>")
      nM True (ECps (r, ERad (s, q))) _ | not eq = ((r .:. s) .!. q, ["Peirce: r;(s!q) |- (r;s)!q"], "==>")
      nM True (ECps (ERad (r, s), q)) _ | not eq = (r .!. (s .:. q), ["Peirce: (r!s);q |- r!(s;q)"], "==>")
      nM _ x@(ECps (l@EFlp {}, r)) _ | not eq && flp l == r && isInj l = (EDcI (source x), ["r~;r |- I (r is univalent)"], "==>")
      nM _ x@(ECps (l, r)) _ | not eq && l == flp r && isInj l = (EDcI (source x), ["r;r~ |- I (r is injective)"], "==>")
      -- Issues #345 and #256: The following two rules may not be used, because properties are not yet proven but must be enforced. So the normalizer may not assume them.
      --  nM _      x@(ECps (l@EFlp{},r)) _ | flp l==r && isInj l && isTot l  = (EDcI (source x), ["r~;r=I because r is univalent and surjective"], "<=>")
      --  nM _      x@(ECps (l,       r)) _ | l==flp r && isInj l && isTot l  = (EDcI (source x), ["r;r~=I because r is injective and total"], "<=>")
      nM posCpl (ECps (l, r)) rs = (t .:. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM posCpl l []
          (f, steps', equ'') = nM posCpl r (l : rs)
      nM _ x@(EEps i sgn) _ | source sgn == i && i == target sgn = (EDcI i, ["source and target are equal to " <> fullName i <> ", so " <> showA x <> "=" <> showA (EDcI i)], "<=>")
      nM _ (ELrs (ECps (x, y), z)) _ | not eq && y == z = (x, ["(x;y)/y |- x"], "==>")
      nM _ (ELrs (ECps (x, y), z)) _
        | not eq && flp x == z =
            ( flp y,
              [ case (x, y) of
                  (EFlp _, EFlp _) -> "(SJ) (x~;y~)/x |- y"
                  (_, EFlp _) -> "(SJ) (x;y~)/x~ |- y"
                  (EFlp _, _) -> "(SJ) (x~;y)/x |- y~"
                  (_, _) -> "(SJ) (x;y)/x~ |- y~"
              ],
              "==>"
            )
      nM _ (ELrs (ELrs (x, z), y)) _ = (ELrs (x, ECps (y, z)), ["Jipsen&Tsinakis: x/yz = (x/z)/y"], "<=>") -- note: sign (x/yz) == sign ((x/z)/y)
      nM posCpl (ELrs (l, r)) _ = (t ./. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM posCpl l []
          (f, steps', equ'') = nM (not posCpl) r []
      nM _ (ERrs (y, ERrs (x, z))) _ = (ERrs (ECps (x, y), z), ["Jipsen&Tsinakis: xy\\z = y\\(x\\z)"], "<=>")
      nM _ (ERrs (x, ECps (y, z))) _ | not eq && x == y = (z, ["x\\(x;y) |- y"], "==>")
      nM posCpl (ERrs (l, r)) _ = (t .\. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM (not posCpl) l []
          (f, steps', equ'') = nM posCpl r []
      nM posCpl (EDia (l, r)) _ = (t .<>. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM posCpl l []
          (f, steps', equ'') = nM posCpl r []
      nM _ (ERad (l, r)) _ | isImin l = (r, ["-I!x = x"], "<=>")
      nM _ (ERad (l, r)) _ | isImin r = (l, ["x!-I = x"], "<=>")
      --     nM False  (ERad (ECps (r,s),q)) _            | not eq = (r.:.(s.!.q), ["Peirce: (r;s)!q |- r;(s!q)"],"==>")  -- SJ 20131124 TODO: check this rule. It is wrong!
      --     nM False  (ERad (r,ECps (s,q))) _            | not eq = ((r.!.s).:.q, ["Peirce: (r!s);q |- r!(s;q)"],"==>")  -- SJ 20131124 TODO: check this rule. It is wrong!
      nM _ (ERad (ECpl l, r)) _ = (flp l .\. r, [case l of EFlp {} -> "-l~!r = l\\r"; _ -> "-l!r = l~\\r"], "<=>")
      nM _ (ERad (l, ECpl r)) _ = (l ./. flp r, [case r of EFlp {} -> "l!-r~ = l/r"; _ -> "l!-r = l/r~"], "<=>")
      nM posCpl (ERad (l, r)) rs = (t .!. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM posCpl l []
          (f, steps', equ'') = nM posCpl r (l : rs)
      nM _ (EPrd (_l, EPrd (m, _r))) _ | isFalse m = (m, ["eliminate middle in cartesian product"], "<=>")
      nM posCpl (EPrd (l, r)) _ = (t .*. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM posCpl l []
          (f, steps', equ'') = nM posCpl r []
      nM posCpl x@(EIsc (l, r)) rs
        -- Absorb equals:    r/\r  -->  r
        | or [length cl > 1 | cl <- NE.toList absorbClasses] =
            ( foldr1 (./\.) (fmap NE.head absorbClasses),
              [shw e <> " /\\ " <> shw e <> " = " <> shw e | cl <- NE.toList absorbClasses, length cl > 1, let e = NE.head cl],
              "<=>"
            )
        -- Absorb True:    r/\V  --> r
        | isTrue l = (r, ["V/\\x = x"], "<=>")
        | isTrue r = (l, ["x/\\V = x"], "<=>")
        -- Inconsistency:    r/\-r   -->  False
        | not (null incons) =
            let i = head incons in (notCpl (EDcV (sign i)), [shw (notCpl i) <> " /\\ " <> shw i <> " = V-"], "<=>")
        -- Inconsistency:    x/\\V-  -->  False
        | isFalse l = (notCpl (EDcV (sign x)), ["-V/\\x = -V"], "<=>")
        | isFalse r = (notCpl (EDcV (sign x)), ["x/\\-V = -V"], "<=>")
        -- Absorb if r is antisymmetric:    r/\r~ --> I
        | t /= l || f /= r =
            (t ./\. f, steps <> steps', fEqu [equ', equ''])
        | not eq && or [length cl > 1 | cl <- absorbAsy] =
            ( foldr1
                (./\.)
                ( let absorbAsy1 = case absorbAsy of
                        [] -> fatal "impossible" -- because of above or-clause
                        h : tl -> h NE.:| tl
                      fun cl =
                        let e = NE.head cl
                         in if length cl > 1 then EDcI (source e) else e
                   in fmap fun absorbAsy1
                ),
              [shw e <> " /\\ " <> shw (flp e) <> " |- I, because" <> shw e <> " is antisymmetric" | cl <- absorbAsy, let e = NE.head cl],
              "==>"
            )
        -- Absorb if r is antisymmetric and reflexive:    r/\r~ = I
        | or [length cl > 1 | cl <- absorbAsyRfx] =
            ( foldr1
                (./\.)
                ( let absorbAsyRfx1 = case absorbAsyRfx of
                        [] -> fatal "impossible" -- because of above or-clause
                        h : tl -> h NE.:| tl
                      fun cl =
                        let e = NE.head cl
                         in if length cl > 1 then EDcI (source e) else e
                   in fmap fun absorbAsyRfx1
                ),
              [shw e <> " /\\ " <> shw (flp e) <> " = I, because" <> shw e <> " is antisymmetric and reflexive" | cl <- absorbAsyRfx, let e = NE.head cl],
              "<=>"
            )
        -- Absorb:    (x\\/y)/\\y  =  y
        | isEUni l && not (null absor0) =
            let t' = head absor0 in (r, ["absorb " <> shw l <> " because of " <> shw t' <> ", using law  (x\\/y)/\\y = y"], "<=>")
        | isEUni r && not (null absor0') =
            let t' = head absor0' in (r, ["absorb " <> shw r <> " because of " <> shw t' <> ", using law  (x\\/y)/\\x = x"], "<=>")
        -- Absorb:    (x\\/-y)/\\y  =  x/\\y
        | isEUni l && not (null absor1) =
            ( case head absor1 of
                (_, []) -> r
                (_, t' : ts) -> foldr (.\/.) t' ts ./\. r,
              ["absorb " <> shw t' <> ", using law (x\\/-y)/\\y  =  x/\\y" | (t', _) <- absor1],
              "<=>"
            )
        | isEUni r && not (null absor1') =
            ( case head absor1' of
                (_, []) -> l
                (_, t' : ts) -> l ./\. foldr (.\/.) t' ts,
              ["absorb " <> shw t' <> ", using law x/\\(y\\/-x)  =  x/\\y" | (t', _) <- absor1'],
              "<=>"
            )
        -- Avoid complements: x/\\-y = x-y
        | (not . null) negList && (not . null) posList =
            let posList' = head posList NE.:| tail posList
             in ( foldl' (.-.) (foldr1 (./\.) posList') (map notCpl negList),
                  ["Avoid complements, using law x/\\-y = x-y"],
                  "<=>"
                )
        | otherwise = (t ./\. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM posCpl l []
          (f, steps', equ'') = nM posCpl r (l : rs)
          absorbClasses :: NE.NonEmpty (NE.NonEmpty Expression)
          absorbClasses = case eqClass (==) (NE.toList $ exprIsc2list l <> exprIsc2list r) of
            [] -> fatal "Impossible"
            h : tl -> h NE.:| tl
          incons = NE.filter (\conjunct -> conjunct == notCpl l) $ exprIsc2list r
          absor0 =
            [ disjunct | disjunct <- NE.toList $ exprUni2list l, f' <- NE.toList . appendLeft rs $ exprIsc2list r, disjunct == f'
            ]
          absor0' =
            [ disjunct | disjunct <- NE.toList $ exprUni2list r, f' <- NE.toList . appendLeft rs $ exprIsc2list l, disjunct == f'
            ]
          absor1 =
            [ (disjunct, NE.filter (disjunct /=) (exprUni2list l))
              | disjunct <- NE.toList $ exprUni2list l,
                ECpl f' <- NE.toList . appendLeft rs $ exprIsc2list r,
                disjunct == f'
            ]
              <> [ (disjunct, NE.filter (disjunct /=) (exprUni2list l))
                   | disjunct@(ECpl t') <- NE.toList $ exprUni2list l,
                     f' <- NE.toList . appendLeft rs $ exprIsc2list r,
                     t' == f'
                 ]
          absor1' =
            [ (disjunct, NE.filter (disjunct /=) (exprUni2list r))
              | disjunct <- NE.toList $ exprUni2list r,
                ECpl f' <- NE.toList . appendLeft rs $ exprIsc2list l,
                disjunct == f'
            ]
              <> [ (disjunct, NE.filter (disjunct /=) (exprUni2list r))
                   | disjunct@(ECpl t') <- NE.toList $ exprUni2list r,
                     f' <- NE.toList . appendLeft rs $ exprIsc2list l,
                     t' == f'
                 ]
          absorbAsy :: [NE.NonEmpty Expression]
          absorbAsy = eqClass same (NE.toList eList) where e `same` e' = isAsy e && isAsy e' && e == flp e'
          absorbAsyRfx :: [NE.NonEmpty Expression]
          absorbAsyRfx = eqClass same (NE.toList eList) where e `same` e' = isRfx e && isAsy e && isRfx e' && isAsy e' && e == flp e'
          (negList, posList) = NE.partition isNeg (exprIsc2list l <> exprIsc2list r)
          eList :: NE.NonEmpty Expression
          eList = appendLeft rs $ exprIsc2list l <> exprIsc2list r
      nM posCpl (EUni (ECpl x, r@(ELrs (z, y)))) _ =
        if sign x == sign z -- necessary to guarantee that sign expr is equal to sign of the result
          then (notCpl (x .:. y) .\/. z, ["remove left residual (/)"], "<=>")
          else (notCpl t .\/. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM (not posCpl) x []
          (f, steps', equ'') = nM posCpl r []
      nM posCpl (EUni (l@(ELrs (z, y)), ECpl x)) _ =
        if sign x == sign z -- necessary to guarantee that sign expr is equal to sign of the result
          then (notCpl (x .:. y) .\/. z, ["remove left residual (/)"], "<=>")
          else (notCpl t .\/. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM (not posCpl) x []
          (f, steps', equ'') = nM posCpl l []
      nM posCpl (EUni (l@(ERrs (x, z)), ECpl y)) _ =
        if sign y == sign z -- necessary to guarantee that sign expr is equal to sign of the result
          then (notCpl (x .:. y) .\/. z, ["remove right residual (\\)"], "<=>")
          else (notCpl t .\/. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM (not posCpl) y []
          (f, steps', equ'') = nM posCpl l []
      nM posCpl (EUni (ECpl y, r@(ERrs (x, z)))) _ =
        if sign y == sign z -- necessary to guarantee that sign expr is equal to sign of the result
          then (notCpl (x .:. y) .\/. z, ["remove right residual (\\)"], "<=>")
          else (notCpl t .\/. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM (not posCpl) y []
          (f, steps', equ'') = nM posCpl r []
      nM posCpl x@(EUni (l, r)) rs
        -- Absorb equals:    r\/r  -->  r
        | t /= l || f /= r =
            (t .\/. f, steps <> steps', fEqu [equ', equ''])
        | or [length cl > 1 | cl <- NE.toList absorbClasses] -- yields False if absorbClasses is empty
          =
            ( foldr1 (.\/.) (fmap NE.head absorbClasses),
              [shw e <> " \\/ " <> shw e <> " = " <> shw e | cl <- NE.toList absorbClasses, length cl > 1, let e = NE.head cl],
              "<=>"
            )
        -- Tautologies:
        | (not . null) tauts = (EDcV (sign x), ["let e = " <> shw (head tauts) <> ". Since -e\\/e = V we get"], "<=>") -- r\/-r  -->  V
        | isTrue l = (EDcV (sign x), ["V\\/x = V"], "<=>") -- r\/V   -->  V
        | isTrue r = (EDcV (sign x), ["x\\/V = V"], "<=>")
        -- Absorb -V:    r\/-V  --> r
        | isFalse l = (r, ["-V\\/x = x"], "<=>")
        | isFalse r = (l, ["x\\/-V = x"], "<=>")
        -- Absorb:    (x/\\y)\\/y  =  y
        | isEIsc l && not (null absor0) = let t' = head absor0 in (r, ["absorb " <> shw l <> " because of " <> shw t' <> ", using law  (x/\\y)\\/y = y"], "<=>")
        | isEIsc r && not (null absor0') = let t' = head absor0' in (r, ["absorb " <> shw r <> " because of " <> shw t' <> ", using law  (x/\\y)\\/x = x"], "<=>")
        -- Absorb:    (x/\\-y)\\/y  =  x\\/y
        | isEIsc l && not (null absor1) =
            ( case head absor1 of
                (_, []) -> r
                (_, t' : ts) -> foldr (./\.) t' ts .\/. r,
              ["absorb " <> shw t' <> ", using law (x/\\-y)\\/y  =  x\\/y" | (t', _) <- absor1],
              "<=>"
            )
        | isEIsc r && not (null absor1') =
            ( case head absor1' of
                (_, []) -> l
                (_, t' : ts) -> l .\/. foldr (./\.) t' ts,
              ["absorb " <> shw t' <> ", using law x\\/(y/\\-x)  =  x\\/y" | (t', _) <- absor1'],
              "<=>"
            )
        | otherwise = (t .\/. f, steps <> steps', fEqu [equ', equ''])
        where
          (t, steps, equ') = nM posCpl l []
          (f, steps', equ'') = nM posCpl r (l : rs)
          -- absorption can take place if two terms are equal. So let us make a list of equal terms: absorbClasses (for substituting r\/r by r)
          absorbClasses :: NE.NonEmpty (NE.NonEmpty Expression)
          absorbClasses = case eqClass (==) (NE.toList $ exprUni2list l <> exprUni2list r) of
            [] -> fatal "Impossible"
            h : tl -> h NE.:| tl
          -- tautologies occur if -r\/r, so we are looking for pairs, (x,l) such that x== -l
          tauts = [t' | disjunct <- NE.toList $ exprUni2list r, disjunct == notCpl l, ECpl t' <- [disjunct, l]]
          absor0 :: [Expression]
          absor0 =
            [ t'
              | t' <- NE.toList $ exprIsc2list l,
                f' <- NE.toList . appendLeft rs $ exprUni2list r,
                t' == f'
            ]
          absor0' :: [Expression]
          absor0' =
            [ t'
              | t' <- NE.toList $ exprIsc2list r,
                f' <- NE.toList . appendLeft rs $ exprUni2list l,
                t' == f'
            ]
          absor1 :: [(Expression, [Expression])]
          absor1 =
            [ (t', NE.filter (t' /=) (exprIsc2list l))
              | t' <- NE.toList $ exprIsc2list l,
                ECpl f' <- NE.toList . appendLeft rs $ exprUni2list r,
                t' == f'
            ]
              <> [ (e, filter (e /=) (NE.toList $ exprIsc2list l))
                   | e@(ECpl t') <- NE.toList $ exprIsc2list l,
                     f' <- NE.toList . appendLeft rs $ exprUni2list r,
                     t' == f'
                 ]
          absor1' :: [(Expression, [Expression])]
          absor1' =
            [ (t', filter (t' /=) (NE.toList $ exprIsc2list r))
              | t' <- NE.toList $ exprIsc2list r,
                ECpl f' <- NE.toList . appendLeft rs $ exprUni2list l,
                t' == f'
            ]
              <> [ (e, filter (e /=) (NE.toList $ exprIsc2list r))
                   | e@(ECpl t') <- NE.toList $ exprIsc2list r,
                     f' <- NE.toList . appendLeft rs $ exprUni2list l,
                     t' == f'
                 ]
      -- Issue #72: The following rule may not be used, because properties are not yet proven but must be enforced. So the normalizer may not assume them.
      --  nM _ (EFlp e) _ | isSym e =  (e,[shw e<>" is symmetric"],"<=>")
      nM _ x _ = (x, [], "<=>")

fEqu :: [Text] -> Text
fEqu ss = if and [s == "<=>" | s <- ss] then "<=>" else "==>"

{-
   nfProof :: (Expression -> Text) -> Expression -> Proof Expression
   nfProof shw = nfPr shw True True -- The first boolean True means that clauses are derived using <=> derivations. The second True means that a disjunctive normal form is produced.
-}

nfPr :: (Expression -> Text) -> Bool -> Bool -> Expression -> [(Expression, [Text], Text)]
nfPr shw eq dnf expr =
  {-if showIT expr=="r \\/ s"
  then fatal ("Diagnose expr: "<>showIT expr<>"\n"<>
              "eq:            "<>show eq<>"\n"<>
              "dnf:           "<>show eq<>"\n"<>
              "res:           "<>showIT res<>"\n"<>
              "expr==res:     "<>show (expr==res)
             ) else-}
  if expr == res
    then [(expr, [], "<=>")]
    else (expr, steps, equ) : nfPr shw eq dnf (simplify res)
  where
    (res, steps, equ) = normStep shw eq False expr

conjNF, disjNF :: env -> Expression -> Expression
(conjNF, disjNF) = (pr False, pr True)
  where
    pr dnf _ expr =
      let proof = if dnf then dfProof else cfProof
          (e, _, _) = if null (proof expr) then fatal "last: empty list" else last (proof expr)
       in e

cfProof, dfProof :: Expression -> Proof Expression
(cfProof, dfProof) = (proof False, proof True)
  where
    proof :: Bool -> Expression -> Proof Expression
    proof dnf expr =
      [line | step, line <- init pr]
        <> [line | step', line <- init pr']
        <> [ last
               ( [(expr, [], "<=>")]
                   <> [line | step, line <- pr]
                   <> [line | step', line <- pr']
               )
           ]
      where
        pr = nfPr showA True dnf expr
        (expr', _, _) = if null pr then fatal "last: empty list" else last pr
        step = simplify expr /= simplify expr'
        pr' = nfPr showA True dnf expr'
        step' = simplify expr' /= simplify expr''
        (expr'', _, _) = if null pr' then fatal "last: empty list" else last pr'

isEUni :: Expression -> Bool
isEUni EUni {} = True
isEUni _ = False

isEIsc :: Expression -> Bool
isEIsc EIsc {} = True
isEIsc _ = False

conjuncts :: env -> Rule -> NE.NonEmpty Expression
conjuncts env =
  exprIsc2list
    --  . (\e -> trace ("conjNF of that term: "<>show e) e)
    . conjNF env
    --  . (\e -> trace ("FormalExpression: "<>show e) e)
    . formalExpression

allShifts :: env -> DnfClause -> [DnfClause]
allShifts env conjunct = map NE.head . eqClass (==) . filter pnEq . map normDNF $ (shiftL conjunct <> shiftR conjunct) -- we want to nub all dnf-clauses, but nub itself does not do the trick...
-- allShifts conjunct = error $ show conjunct<>concat [ "\n"<>show e'| e'<-shiftL conjunct<>shiftR conjunct] -- for debugging
  where
    {-
     diagnostic
      = intercalate "\n  "
          [ "shiftL: [ "<>intercalate "\n          , " [showHS env "\n            " e | e<-shiftL conjunct    ]<>"\n          ]"
          , "shiftR: [ "<>intercalate "\n          , " [showHS env "\n            " e | e<-shiftR conjunct    ]<>"\n          ]"
          ] -}
    shiftL :: DnfClause -> [DnfClause]
    shiftL dc =
      case (antcs dc, conss dc) of
        ([], _) -> [dc] --  shiftL doesn't work here. This is just to make sure that both antss and conss are really not empty
        (_, []) -> [dc] --  shiftL doesn't work here. This is just to make sure that both antss and conss are really not empty
        _ ->
          [ Dnf
              { antcs = ass,
                conss = case css of
                  [] -> case ass of
                    [] -> fatal "Impossible"
                    h : tl ->
                      let antcExpr = foldr (./\.) h tl
                       in if isEndo antcExpr then [EDcI (source antcExpr)] else fatal "antcExpr should be endorelation"
                  _ -> css
              }
            | (ass, css) <- L.nub (move (antcs dc) (conss dc))
          ]
      where
        -- example:  r;s /\ p;r |- x;y   and suppose x and y are both univalent.
        --  antcs =  [ r;s, p;r ]
        --  conss =  [ x;y ]
        move :: [Expression] -> [Expression] -> [([Expression], [Expression])]
        move ass [] = [(ass, [])]
        move ass css =
          (ass, css)
            : if and [(not . isEDcI) cs | cs <- css] -- all cs are nonempty because: (not.and.map isEDcI) cs ==> not (null cs)
              then
                [ ts | let headEs = map headECps css, length (eqClass (==) headEs) == 1, let h -- example: True, because map head css == [ "x" ]
                                                                                               =
                                                                                               head headEs, isUni h, ts <- -- example: h= "x"
                                                                                               -- example: assume True
                                                                                                                       move
                                                                                                                         [ if source h == source as then flp h .:. as else fatal "type mismatch"
                                                                                                                           | as <- ass
                                                                                                                         ]
                                                                                                                         (map tailECps css)
                ]
                  <> [ ts
                       | let lastEs -- example: ts<-move [ [flp "x","r","s"], [flp "x","p","r"] ]  [ ["y","z"] ]
                               =
                               map lastECps css,
                         length (eqClass (==) lastEs) == 1,
                         let l = head lastEs,
                         isInj l,
                         ts <-
                           move
                             [ if target as == target l then as .:. flp l else fatal "type mismatch"
                               | as <- ass
                             ]
                             (map initECps css) -- example: ts<-move [ ["r","s",flp "z"], ["p","r",flp "z"] ]  [ ["x","y"] ]
                     ]
              else []
    -- Here is (informally) what the example does:
    -- move [ r;s , p;r ] [ x;y ]
    -- ( [ r;s , p;r ] , [ x;y ] ): [ ts | ts<-move [flp x.:.as | as<-[ r;s , p;r ] [ y ] ] ]
    -- ( [ r;s , p;r ] , [ x;y ] ): ( [ x~;r;s , x~;p;r ] , [ y ] ): [ ts | ts<-move [flp y.:.as | as<-[ y~;x~;r;s , y~;x~;p;r ] [] ] ]
    -- ( [ r;s , p;r ] , [ x;y ] ): ( [ x~;r;s , x~;p;r ] , [ y ] ): [ [ y~;x~;r;s , y~;x~;p;r ] , [] ] ]
    -- [ ( [ r;s , p;r ] , [ x;y ] ), ( [ x~;r;s , x~;p;r ] , [ y ] ), ( [ y~;x~;r;s , y~;x~;p;r ] , [] ) ]

    shiftR :: DnfClause -> [DnfClause]
    shiftR dc =
      case (antcs dc, conss dc) of
        ([], _) -> [dc] --  shiftR doesn't work here. This is just to make sure that both antcs and conss are really not empty
        (_, []) -> [dc] --  shiftR doesn't work here. This is just to make sure that both antcs and conss are really not empty
        _ ->
          [ Dnf
              ( case ass of
                  [] -> case css of
                    [] -> fatal "Impossible!"
                    h : tl ->
                      let consExpr = foldr (.\/.) h tl
                       in if source consExpr == target consExpr then [EDcI (source consExpr)] else fatal "consExpr should be endorelation"
                  _ -> ass
              )
              css
            | (ass, css) <- L.nub (move (antcs dc) (conss dc))
          ]
      where
        -- example  "r;s /\ r;r |- x;y"   and suppose r is both surjective.
        --  ass =  [ r;s , r;r ]
        --  css =  [ x;y ]
        move :: [Expression] -> [Expression] -> [([Expression], [Expression])]
        move ass css =
          case ass of
            [] -> [] -- was [([EDcI (target (last css))],css)]
            _ ->
              (ass, css)
                : if and [(not . isEDcI) as | as <- ass]
                  then
                    [ ts | let headEs = map headECps ass, length (eqClass (==) headEs) == 1, let h -- example: True, because map headECps ass == [ "r", "r" ]
                                                                                                   =
                                                                                                   head headEs, isSur h, ts <- -- example: h= "r"
                                                                                                   -- example: assume True
                                                                                                                           move
                                                                                                                             (map tailECps ass)
                                                                                                                             [ if source h == source cs then flp h .:. cs else fatal "type mismatch"
                                                                                                                               | cs <- css
                                                                                                                             ]
                    ]
                      <> [ ts
                           | let lastEs -- example: ts<-move  [["s"], ["r"]] [ [flp "r","x","y","z"] ]
                                   =
                                   map lastECps ass,
                             length (eqClass (==) lastEs) == 1,
                             let l -- example: False, because map lastECps ass == [ ["s"], ["r"] ]
                                   =
                                   head lastEs,
                             isTot l,
                             ts <-
                               move
                                 (map initECps ass)
                                 [ if target cs == target l then cs .:. flp l else fatal "type mismatch"
                                   | cs <- css -- is dit goed? cs.:.flp l wordt links zwaar, terwijl de normalisator rechts zwaar maakt.
                                 ]
                         ]
                  else []
    -- Here is (informally) what the example does:
    -- move [ r;s , r;r ] [ x;y ]
    -- ( [ r;s , r;r ] , [ x;y ] ): move [ s , r ] [ r~;x;y ]
    -- ( [ r;s , r;r ] , [ x;y ] ): ( [ s , r ]  , [ r~;x;y ] ) : []
    -- [ [ r;s , r;r ] , [ x;y ] ), ( [ s , r ]  , [ r~;x;y ] ) ]
    --  diagnostic
    --    = "\n  antcs: [ "<>intercalate "\n         , " [showIT a | a<-antcs ]<>"\n       ]"<>
    --      "\n  conss: [ "<>intercalate "\n         , " [showIT c | c<-conss ]<>"\n       ]"<>
    --      "\n  move:  [ "<>intercalate "\n         , " ["("<>sh " /\\ " as<>"\n           ,"<>sh " \\/ " cs<>")" | (as,cs)<-move antcs conss ]<>"\n       ]"
    --  sh :: Text -> [Expression] -> Text
    --  sh str es = intercalate str [ showIT e | e<-es]

    normDNF :: DnfClause -> DnfClause
    normDNF dc =
      Dnf
        { antcs = case antcs dc of
            [] -> []
            h : tl -> NE.toList . exprIsc2list . conjNF env $ foldr1 (./\.) (h NE.:| tl),
          conss = case conss dc of
            [] -> []
            h : tl -> NE.toList . exprUni2list . disjNF env $ foldr1 (.\/.) (h NE.:| tl)
        }

    pnEq :: DnfClause -> Bool
    pnEq dc = antcs dc /= conss dc

    headECps :: Expression -> Expression
    headECps expr = f expr
      where
        f (ECps (l@ECps {}, _)) = f l
        f (ECps (l, _)) = l
        f _ = expr

    tailECps :: Expression -> Expression
    tailECps expr = f expr
      where
        f (ECps (ECps (l, r), q)) = f (ECps (l, ECps (r, q)))
        f (ECps (_, r)) = r
        f _ = EDcI (target expr)

    initECps :: Expression -> Expression
    initECps expr = f expr
      where
        f (ECps (l, ECps (r, q))) = initECps (ECps (ECps (l, r), q))
        f (ECps (l, _)) = l
        f _ = EDcI (source expr)

    lastECps :: Expression -> Expression
    lastECps expr = f expr
      where
        f (ECps (_, r@ECps {})) = f r
        f (ECps (_, r)) = r
        f _ = expr

    isEDcI :: Expression -> Bool
    isEDcI EDcI {} = True
    isEDcI _ = False

makeAllConjs :: env -> Rules -> [Conjunct]
makeAllConjs env allRls =
  [ Cjct
      { rc_id = toText1Unsafe $ "conj_" <> tshow (i :: Int),
        rc_orgRules = rs,
        rcConjunct = expr,
        rc_dnfClauses = allShifts env (expr2dnfClause expr)
      }
    | (i, (expr, rs)) <- zip [0 ..] conjExprs
  ]
  where
    conjExprs :: [(Expression, NE.NonEmpty Rule)]
    conjExprs = converseNE . map conjTupel . Set.toList $ allRls
    conjTupel rule = (rule, conjuncts env rule)
    expr2dnfClause :: Expression -> DnfClause
    expr2dnfClause = split (Dnf [] []) . NE.toList . exprUni2list
      where
        split :: DnfClause -> [Expression] -> DnfClause
        split (Dnf antc cons) (ECpl e : rest) = split (Dnf (e : antc) cons) rest
        split (Dnf antc cons) (e : rest) = split (Dnf antc (e : cons)) rest
        split dc [] = dc

appendLeft :: [a] -> NE.NonEmpty a -> NE.NonEmpty a
appendLeft lst ne =
  case reverse lst of
    [] -> ne
    x : xs -> appendLeft xs $ x NE.<| ne

foldr1 :: (Expression -> Expression -> Expression) -> NE.NonEmpty Expression -> Expression
foldr1 fun ne = foldr fun (NE.head ne) (NE.tail ne)

-- TODO: Get rid of head', tail', init' and last' in this module. These are introduced here
--  whith the introduction to RIO. However, in this module there is too much useage of these functions

head, last :: [a] -> a
head = fromMaybe (fatal "Illegal use of head") . L.headMaybe
last = fromMaybe (fatal "Illegal use of last") . L.lastMaybe

tail, init :: [a] -> [a]
tail = fromMaybe (fatal "Illegal use of tail") . L.tailMaybe
init = fromMaybe (fatal "Illegal use of init") . L.initMaybe

-- | dist will return all possible splits of a given list into n parts, such that the concatenation of the parts
--   will result in the original list.
dist :: Int -> [a] -> [[[a]]]
dist 1 ls = [[ls]]
dist 2 ls = [[take i ls, drop i ls] | i <- [1 .. length ls - 1]]
dist n ls = [init ds <> st | ds <- dist (n - 1) ls, let staart = last ds, length staart >= 2, st <- dist 2 staart]

{- examples:
dist 1 "abcd" = [["abcd"]]
dist 2 "abcd" = [["a","bcd"],["ab","cd"],["abc","d"]]
dist 3 "abcd" = [["a","b","cd"],["a","bc","d"],["ab","c","d"]]
dist 3 "abcdef" =
   [ ["a","b","cdef"]
   , ["a","bc","def"]
   , ["a","bcd","ef"]
   , ["a","bcde","f"]
   , ["ab","c","def"]
   , ["ab","cd","ef"]
   , ["ab","cde","f"]
   , ["abc","d","ef"]
   , ["abc","de","f"]
   , ["abcd","e","f"]
   ]
-}
