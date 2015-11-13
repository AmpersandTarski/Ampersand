{-# OPTIONS_GHC -XFlexibleInstances #-}
module Database.Design.Ampersand.FSpec.ToFSpec.NormalForms
  (delta,conjNF,disjNF,normPA,cfProof,dfProof,proofPA,simplify
  ,cfProofs, dfProofs  -- these are for confluence testing.
  , makeAllConjs, conjuncts
  ) where
  
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub, intercalate, permutations,partition)
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.ADL1.ECArule
import Database.Design.Ampersand.ADL1.Expression
import Database.Design.Ampersand.ADL1.P2A_Converters (pCpt2aCpt)
import Database.Design.Ampersand.Classes.Relational
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.Core.ParseTree
import Database.Design.Ampersand.Misc.Options
import Database.Design.Ampersand.Input (parseRule)
import Database.Design.Ampersand.FSpec.ShowADL  -- for debug purposes only
import Prelude hiding (head)
-- import Debug.Trace
fatal :: Int -> String -> a
fatal = fatalMsg "FSpec.ToFSpec.NormalForms"

{- SJC:
Ideas for future work:
-> Create a Unifier data type / class with
   > extend :: key -> value -> Unifier -> Maybe Unifier
   (returns Nothing if the key is already in the Unifier)
   > obtain :: key -> Unifier -> value
   (something like this twice! Handle types separate from relations)
   > think of a way in which substitution never fails (unify matching and substitution for this)
-> Make RTerm polymorphic, so we can treat variables and constants separately.
   We'd have RTerm Expression and RTerm (String,String,String)
   We'd be able to derive fmap, and make RTerm Foldable.
-> Really long term: Unify RTerm and Expression in a way that still allows us to write simple code for binary operators. Would require separating = and |- from Expression, which is also nice.
-}

-- The following was built for the purpose of testing confluence.
-- These functions produce all derivations of results from the normalizer.
-- A useful side effect is that it implicitly tests for soundness.
cfProofs, dfProofs :: Expression -> [(Expression, Proof Expression)]
(cfProofs, dfProofs) = (prfs False, prfs True)
 where
   prfs :: Bool -> Expression -> [(Expression, Proof Expression)]
   prfs dnf expr
    = nub [ (rTerm2expr t, map makeExpr derivs) | (t, derivs)<-f (expr2RTerm expr) ]
      where
        f :: RTerm -> [(RTerm,[(RTerm, [String], String)])]
        f term = [ (term,[(term, [], "<=>")]) | null dsteps ]++
                 [ (t, (term, [showStep dstep], "<=>"):deriv)
                 | dstep<-dsteps, (t,deriv)<-f (rhs dstep)
                 ]
                 where dsteps = [ dstep | dstep<-dSteps tceDerivRules term, w (rhs dstep)<w term]
        w = weightNF dnf -- the weight function for disjunctive normal form.
        showStep dstep = " weight: "++(show.w.lhs) dstep++",   "++showADL tmpl++" = "++showADL stp++"  with unifier: "++showADL unif
                         where (tmpl,unif,stp) = rul dstep
        makeExpr (term, explStr, logicSym) = (rTerm2expr term, explStr, logicSym)

-- Deriving normal forms and representing the neccessary derivation rules are defined by means of RTerms.
-- The data structure RTerm is a representation of relation algebra expressions,
-- which is not redundant with respect to associativity and commutativity.
-- The reason for this is that we use term rewriting for normalization.
-- This algorithm performs poorly with commutative rules, because it may explode combinatorially.
data RTerm = RIsc {rTermSet :: Set RTerm}  -- intersection is associative and commutative
           | RUni {rTermSet :: Set RTerm}  -- union is associative and commutative
           | RDif {rTermLft :: RTerm, rTermRht :: RTerm}
           | RCpl {rTermUny :: RTerm}
           | RDia {rTermLft :: RTerm, rTermRht :: RTerm}
           | RLrs {rTermLft :: RTerm, rTermRht :: RTerm}
           | RRrs {rTermLft :: RTerm, rTermRht :: RTerm}
           | RRad {rTermList :: [RTerm]}      -- ! is associative
           | RCps {rTermList :: [RTerm]}      -- ; is associative
           | RPrd {rTermList :: [RTerm]}      -- # is associative
           | RKl0 {rTermUny :: RTerm}
           | RKl1 {rTermUny :: RTerm}
           | RFlp {rTermUny :: RTerm}
           | RId  A_Concept
           | RVee A_Concept A_Concept
           | RAtm PSingleton A_Concept
           | RVar String String String  -- relation name, source name, target name.
           | RConst Expression
           deriving (Eq,Ord,Show)

-- The following condition must hold at all times for every RTerm, in order to make equality work
-- It ensures that nested RIsc terms do not occur, and RIsc terms are at least 2 terms of length.
-- The same holds for RUni, RRad, RCps, and RPrd.
isValid :: RTerm -> Bool
isValid (RIsc s)   = and [not (isRIsc e) && isValid e && length ls>1 | let ls=Set.toList s, e<-ls]
isValid (RUni s)   = and [not (isRUni e) && isValid e && length ls>1 | let ls=Set.toList s, e<-ls]
isValid (RDif l r) = isValid l && isValid r
isValid (RCpl e)   = isValid e
isValid (RDia l r) = isValid l && isValid r
isValid (RLrs l r) = isValid l && isValid r
isValid (RRrs l r) = isValid l && isValid r
isValid (RRad ls)  = and [not (isRRad e) && isValid e && length ls>1 | e<-ls]
isValid (RCps ls)  = and [not (isRCps e) && isValid e && length ls>1 | e<-ls]
isValid (RPrd ls)  = and [not (isRPrd e) && isValid e && length ls>1 | e<-ls]
isValid (RKl0 e)   = isValid e
isValid (RKl1 e)   = isValid e
isValid (RFlp e)   = isValid e
isValid _          = True

-- normRT exists to make an arbitrary term satisfy isValid.
-- So isValid (normRT term) is True, whil term and (normRT term) have the same meaning.
normRT :: RTerm -> RTerm
normRT (RIsc s)   = (combSet RIsc . Set.fromList . flat isRIsc . map normRT . Set.toList) s
normRT (RUni s)   = (combSet RUni . Set.fromList . flat isRUni . map normRT . Set.toList) s
normRT (RDif l r) = RDif (normRT l) (normRT r)
normRT (RCpl e)   = RCpl (normRT e)
normRT (RDia l r) = RDia (normRT l) (normRT r)
normRT (RLrs l r) = RLrs (normRT l) (normRT r)
normRT (RRrs l r) = RRrs (normRT l) (normRT r)
normRT (RRad ls)  = (combLst RRad . flat isRRad . map normRT) ls
normRT (RCps ls)  = (combLst RCps . flat isRCps . map normRT) ls
normRT (RPrd ls)  = (combLst RPrd . flat isRPrd . map normRT) ls
normRT (RKl0 e)   = RKl0 (normRT e)
normRT (RKl1 e)   = RKl1 (normRT e)
normRT (RFlp e)   = RFlp (normRT e)
normRT term       = term

isRIsc, isRUni, isRDif, isRCpl, isRDia, isRLrs, isRRrs, isRRad, isRCps, isRPrd, isRKl0, isRKl1, isRFlp, isRVar :: RTerm -> Bool
isRIsc (RIsc{}) = True
isRIsc _        = False
isRUni (RUni{}) = True
isRUni _        = False
isRDif (RDif{}) = True
isRDif _        = False
isRCpl (RCpl{}) = True
isRCpl _        = False
isRDia (RDia{}) = True
isRDia _        = False
isRLrs (RLrs{}) = True
isRLrs _        = False
isRRrs (RRrs{}) = True
isRRrs _        = False
isRRad (RRad{}) = True
isRRad _        = False
isRCps (RCps{}) = True
isRCps _        = False
isRPrd (RPrd{}) = True
isRPrd _        = False
isRKl0 (RKl0{}) = True
isRKl0 _        = False
isRKl1 (RKl1{}) = True
isRKl1 _        = False
isRFlp (RFlp{}) = True
isRFlp _        = False
isRVar (RVar{}) = True
isRVar _        = False

{- dSteps computes the expressions that can be obtained in one rewrite step.
   It yields the steps, for the purpose of constructing the entire proof.
   The idea is that the environment picks one of the steps produced by dSteps.
-}
dSteps :: [DerivRule] -> RTerm -> [DerivStep]
dSteps drs x = dStps x
 where
  dStps :: RTerm -> [DerivStep]
  dStps (RIsc s)     = dStepSets isRIsc RIsc s
  dStps (RUni s)     = dStepSets isRUni RUni s
  dStps (RDif a b)   = dStepBin isRDif RDif a b
  dStps (RCpl a)     = dStepUny isRCpl RCpl a
  dStps (RDia a b)   = dStepBin isRDia RDia a b
  dStps (RLrs a b)   = dStepBin isRLrs RLrs a b
  dStps (RRrs a b)   = dStepBin isRRrs RRrs a b
  dStps (RRad ls)    = dStepLists isRRad RRad ls
  dStps (RCps ls)    = dStepLists isRCps RCps ls
  dStps (RPrd ls)    = dStepLists isRPrd RPrd ls
  dStps (RKl0 a)     = dStepUny isRKl0 RKl0 a
  dStps (RKl1 a)     = dStepUny isRKl1 RKl1 a
  dStps (RFlp a)     = dStepUny isRFlp RFlp a
  dStps (RId _)      = [ DStep { lhs = x                                       -- derivs gives the top level rewrites.
                               , rul = (term, unif, term')                     -- only one rewrite is done in parallel in the top level.
                               , rhs = substitute rd unif term'                -- so rest is left alone, if partition can be rewritten.
                               }
                       | (term@(RId a'), rewriteTerms)<-matchableRules          -- select rewrite rules with the proper combinator
                       , let unif = Set.fromList [(name a',x)]                  -- find unifiers such that: substitute "" unif term==rCombinator a
                       , term'<-rewriteTerms                                    -- enumerate right hand side RTerms in order to construct:  substitute "" unif term'
                       , let rd = showADL term++" -> "++showADL term'           -- rule documentation for fatals in 'substitute'
                       , if substitute rd unif term==x then True else
                         fatal 122 ("When analysing rule "++rd++" with unifier "++showADL unif++"\nsubstitute rd unif term:  "++showADL (substitute rd unif term)++"\ndiffers from:  "++showADL x)
                       ]
  dStps (RVee a b)   = [ DStep { lhs = x                                       -- derivs gives the top level rewrites.
                               , rul = (term, unif, term')                     -- only one rewrite is done in parallel in the top level.
                               , rhs = substitute rd unif term'                -- so rest is left alone, if partition can be rewritten.
                               }
                       | (term@(RVee a' b'), rewriteTerms)<-matchableRules      -- select rewrite rules with the proper combinator
                       , let unif = Set.fromList [(name a',RId a), (name b',RId b)] -- find unifiers such that: substitute "" unif term==rCombinator a
                       , noDoubles unif                                         -- if one variable is bound to more than one different expressions, the deal is off.
                       , term'<-rewriteTerms                                    -- enumerate right hand side RTerms in order to construct:  substitute "" unif term'
                       , let rd = showADL term++" -> "++showADL term'           -- rule documentation for fatals in 'substitute'
                       , if substitute rd unif term==x then True else
                         fatal 134 ("When analysing rule "++rd++" with unifier "++showADL unif++"\nsubstitute rd unif term:  "++showADL (substitute rd unif term)++"\ndiffers from:  "++showADL x)
                       ]
  dStps (RAtm a c)   = [ DStep { lhs = x                                       -- derivs gives the top level rewrites.
                               , rul = (term, unif, term')                     -- only one rewrite is done in parallel in the top level.
                               , rhs = substitute rd unif term'                -- so rest is left alone, if partition can be rewritten.
                               }
                       | (term@(RAtm a' c'), rewriteTerms)<-matchableRules      -- select rewrite rules with the proper combinator
                       , a==a'
                       , let unif = Set.fromList [(name c',RId c)]              -- find unifiers such that: substitute "" unif term==rCombinator a
                       , term'<-rewriteTerms                                    -- enumerate right hand side RTerms in order to construct:  substitute "" unif term'
                       , let rd = showADL term++" -> "++showADL term'            -- rule documentation for fatals in 'substitute'
                       , if substitute rd unif term==x then True else
                         fatal 146 ("When analysing rule "++rd++" with unifier "++showADL unif++"\nsubstitute rd unif term:  "++showADL (substitute rd unif term)++"\ndiffers from:  "++showADL x)
                       ]
  dStps (RVar _ _ _) = fatal 147 "Cannot rewrite a term with a variable in it." -- This should become a haskell type-error when RTerm is polymorphic
  dStps (RConst _)   = [] -- the only possibly matching rule has a single variable on the lhs, which we assume does not exist. SJ to SJC: Why? is there a reason why we don't want to include that situation?

  dStepUny :: (RTerm -> Bool)    -- a predicate, isrComb, which tests whether some RTerm r has rCombinator as its root.
           -> (RTerm -> RTerm)   -- the combinator
           -> RTerm              -- its argument  (So, we are working with the RTerm   rCombinator a)
           -> [DerivStep]        -- all derivation steps that start at  rCombinator a, which can be made using the available ruleset
{- We are trying to find steps in case an expression (rCombinator a) has a unary operator (i.e. RCpl, RKl0, RKl1, RFlp) as its root.
   First, we try to find a rewrite step on the root level of the expression. The resulting steps are called "derivs".
   When that fails, we try to find the steps from subexpression a recursively.
-}
  dStepUny isrComb rCombinator a
   = if (not . isValid . rCombinator) a
     then fatal 180 ("Invalid expression in dStepLists: "++showADL (rCombinator a))
     else
     derivs ++
     [ DStep { lhs = rCombinator a                                     -- try to find steps recursively
             , rul = rul step
             , rhs = rCombinator (rhs step)
             }
     | step<-dStps a ]
     where derivs = [ DStep { lhs = rCombinator a                           -- derivs gives the top level rewrites.
                            , rul = (term, unif, term')                     -- only one rewrite is done in parallel in the top level.
                            , rhs = substitute rd unif term'                -- so rest is left alone, if partition can be rewritten.
                            }
                    | (term, rewriteTerms)<-matchableRules, isrComb term     -- select rewrite rules with the proper combinator
                    , let subTerm = rTermUny term                            -- now:   rCombinator subTerm = term
                    , unif<-matches subTerm a                                -- find unifiers such that: substitute "" unif term==rCombinator a
                    , term'<-rewriteTerms                                    -- enumerate right hand side RTerms in order to construct:  substitute "" unif term'
                    , let rd = showADL term++" -> "++showADL term'            -- rule documentation for fatals in 'substitute'
                    , if substitute rd unif term==rCombinator a then True else
                      fatal 177 ("When analysing rule "++rd++" with unifier "++showADL unif++"\nsubstitute rd unif term:  "++showADL (substitute rd unif term)++"\ndiffers from\nrCombinator a:  "++showADL (rCombinator a))
                    ]

-- dStepBin follows the same pattern as dStepUny, but for binary RTerms
  dStepBin :: (RTerm -> Bool) -> (RTerm -> RTerm -> RTerm) -> RTerm -> RTerm -> [DerivStep]
  dStepBin isrComb rCombinator a b
   = if (not . isValid) (rCombinator a b)
     then fatal 202 ("Invalid expression in dStepLists: "++showADL (rCombinator a b))
     else
     derivs ++
     [ DStep { lhs = rCombinator a b
             , rul = rul rStp
             , rhs = rCombinator a (rhs rStp)
             }
     | rStp<-dStps b ] ++
     [ DStep { lhs = rCombinator a b
             , rul = rul lStp
             , rhs = rCombinator (rhs lStp) b
             }
     | lStp<-dStps a ]
     where derivs = [ DStep { lhs = rCombinator a b             -- derivs gives the top level rewrites.
                            , rul = (term, unif, term')         -- only one rewrite is done in parallel in the top level.
                            , rhs = substitute rd unif term'    -- so rest is left alone, if partition can be rewritten.
                            }
                    | (term, rewriteTerms)<-matchableRules, isrComb term    -- select rewrite rules with the proper combinator
                    , let subLft = rTermLft term; subRht = rTermRht term    -- now:   rCombinator subTerm = term
                    , unif1 <- matches subLft a
                    , unif2 <- matches subRht b    -- find unifiers such that: substitute "" unif term==rCombinator a
                    , let unif = Set.union unif1 unif2
                    , noDoubles unif                          -- if one variable is bound to more than one different expressions, the deal is off.
                    , term'<-rewriteTerms                     -- enumerate right hand side RTerms in order to construct:  substitute "" unif term'
                    , let rd = showADL term++" -> "++showADL term'        -- rule documentation for fatals in 'substitute'
                    , if substitute rd unif term==rCombinator a b then True else
                      fatal 207 ("When analysing rule "++rd++" with unifier "++showADL unif++"\nsubstitute rd unif term:  "++showADL (substitute rd unif term)++"\ndiffers from\nrCombinator a b:  "++showADL (rCombinator a b))
                    ]

  dStepLists :: (RTerm -> Bool) -> ([RTerm] -> RTerm) -> [RTerm] -> [DerivStep] -- Note: a and b are both RTerm
  dStepLists isrComb rCombinator ls
   = if (not . isValid . rCombinator) ls
     then fatal 231 ("Invalid expression in dStepLists: "++showADL (rCombinator ls))
     else
     [ DStep { lhs = rCombinator ls       -- The original expression
             , rul = (term, unif, term')  -- only one rewrite step is done without parallelism.
             , rhs = result
             }
     | (term, rewriteTerms)<-matchableRules, isrComb term
     , let subTerms = rTermList term
     , let n=length subTerms
     , (pre,segmentList,post) <- segments n
     , unif <- mix [ matches l r | (l,r)<-safezip subTerms (map (combLst rCombinator) segmentList) ]
     , noDoubles unif                                      -- if one variable is bound to more than one different expressions, the deal is off.
     , term'<-rewriteTerms
     , let rd = showADL term++" -> "++showADL term'        -- rule documentation for fatals in 'substitute'
     , let original=flatLst (pre ++ substitute rd unif term :post)  -- is equal to rCombinator ls
     , let result  =flatLst (pre ++ substitute rd unif term':post)
     , if original==rCombinator ls then True else
       fatal 228 ("When analysing rule "++rd++" with unifier "++showADL unif++" on:  "++showADL (rCombinator ls)++
                  "\nWe substitute:  "++showADL (substitute rd unif term)++
                  "\nby:             "++showADL (substitute rd unif term')++
                  ".\nHowever, the original RTerm:  "++showADL (rCombinator ls)++
                  "\ndiffers from flatLst (pre ++ substitute rd unif term :post):\n  "++
                  showADL original
                 )
     ] ++
     [ DStep { lhs = rCombinator ls -- is equal to: (pre++lhs dstep:post)
             , rul = rul dstep
             , rhs = flatLst (pre++rhs dstep:post)
             }
     | (pre,l,post) <- splitList ls
     , dstep <- dStps l]
     where dist :: Int -> [RTerm] -> [[[RTerm]]]
           dist 1 es = [[es]]
           dist 2 es = [ [ take i es , drop i es ] | i<-[1..length es-1] ]
           dist n es = [ init ds++st | ds<-dist (n-1) es, let staart=last ds, length staart>=2, st<-dist 2 staart ]
           segments :: Int -> [([RTerm],[[RTerm]],[RTerm])]
           segments n
            = [ ([], ds, []) | ds<-dist n ls] ++
              [ (head ds, tail ds, []) | ds<-dist (n+1) ls ] ++
              [ ([], init ds, last ds) | ds<-dist (n+1) ls ] ++
              [ (head ds, (init.tail) ds, last ds) | ds<-dist (n+2) ls ]
           flatLst :: [RTerm] -> RTerm
           flatLst = combLst rCombinator . flat isrComb

  dStepSets ::  (RTerm -> Bool) -> (Set RTerm -> RTerm) -> Set RTerm -> [DerivStep]
  dStepSets isrComb rCombinator s
  -- We try to perform a rewrite on the top level, i.e. on some subset of RTerms from s.
  -- Then, we add rewrites on any of the subexpressins in s.
  -- Example rCombinator s  = RUni { '1', aap, aap\noot, 'Piet', mies;vuur}
   = [ DStep { lhs = rCombinator s            -- derivs gives the top level rewrites.
             , rul = (term, unif, term')      -- only one rewrite is done in parallel in the top level.
             , rhs = result                   -- so rest is left alone, if partition can be rewritten.
             }
     | null [ () | e<-Set.toList s, not (isValid e), fatal 313 ("Invalid subexpr: "++showADL e) ]
     -- s = { foo;foo~ , -(bar;bar~) , I[C]}
     , (term, rewriteTerms)<-matchableRules, isrComb term       -- e.g. term = 'Piet' \/ r \/ p\q
     , let subTerms = rTermSet term                             -- e.g. subTerms = { 'Piet', r, p\q }
     , let termVars = Set.filter isRVar subTerms                -- e.g. termVars = { r }
     , let sameTrms = subTerms `Set.intersection` s             -- e.g. sameTrms = { 'Piet' }
     , let subExprs = s `Set.difference` sameTrms               -- { '1', aap, aap\noot, mies;vuur }
     , let toMatchs = (subTerms `Set.difference` sameTrms) `Set.difference` termVars -- e.g. toMatchs = { p\q }
     , let n=Set.size toMatchs -- each element of toMatchs can be matched to one subTerm from subExprs.
     , (matchCandidates,rest)<-separate n subExprs              -- e.g. matchCandidates = {aap\noot} and rest={ '1', aap, mies;vuur }
     , let m=Set.size termVars -- each variable in subTerms must be matched to one subset from rest.
     , (restSets,remainder)<-partsplus m rest                   -- e.g. restSets={ {'1', aap, mies;vuur} }
     , let restTerms = (Set.map (flatSet . Set.toList)) restSets   -- e.g. restTerms={ RUni {'1', aap, mies;vuur} }
     , if Set.null restTerms then True else
       if (isValid . flatSet . Set.toList) restTerms then True else fatal 305 ("Invalid restTerms: "++showADL (rCombinator restTerms))
     , let remTerm   = let remT = combSet rCombinator remainder in
                       if Set.null remainder
                       then fatal 308 "empty remTerm"
                       else if isValid remT then remT else fatal 309 ("Invalid remTerm: "++showADL remT)            -- e.g. restTerms={ RUni {'1', aap, mies;vuur} }
     , unif0 <- if Set.null toMatchs then [Set.empty] else
                if (not.isValid.combSet rCombinator) toMatchs        then fatal 311 ("Invalid toMatchs: "++showADL (rCombinator toMatchs)) else
                if (not.isValid.combSet rCombinator) matchCandidates then fatal 312 ("Invalid matchCandidates: "++showADL (rCombinator matchCandidates)) else
                matchSets rCombinator toMatchs matchCandidates  -- e.g. unif0={ p->aap, q->noot }
     , unif1 <- if Set.null termVars then [Set.empty] else
                matchSets rCombinator termVars restTerms        -- e.g. unif1={ r->RUni {'1', aap, mies;vuur} }
     , let unif = unif0 `Set.union` unif1                       -- e.g. unif={ p->aap, q->noot, r->RUni {'1', aap, mies;vuur} }
     , noDoubles unif
     , term'<-rewriteTerms
     , let rd = showADL term++" -> "++showADL term'             -- rule documentation for fatals in 'substitute'
     , let original = if Set.null remainder
                      then substitute rd unif term              -- is equal to rCombinator ls
                      else flatSet [substitute rd unif term,  remTerm]
     , let result   = if Set.null remainder
                      then substitute rd unif term'
                      else flatSet [substitute rd unif term', remTerm]
     , if original==rCombinator s then True else
       fatal 327 ("When analysing rule "++rd++" with unifier "++showADL unif++" on:  "++showADL (rCombinator s)++
                  "\nWe substitute:  "++showADL original++
                  "\nby:             "++showADL result++
                  "\nHowever, the original RTerm:  "++showADL (rCombinator s)++
                  "\ndiffers from subs term:       "++showADL original
                 )
     ] ++
     [ DStep { lhs = rCombinator s -- is equal to: (pre \/ lhs dstep)
             , rul = rul dstep
             , rhs = flatSet (pre++rhs dstep:post)
             }
     | (pre,l,post) <- splitList (Set.toList s)
     , dstep <- dStps l]
     where partsplus :: Ord a => Int -> Set a -> [(Set (Set a), Set a)]
           partsplus n ss = [ (p,Set.empty) | p<-parts n ss ] ++ [ (Set.delete p prt, p) | prt<-parts (n+1) ss, p<-Set.toList prt ]
           flatSet :: [RTerm] -> RTerm
           flatSet = normRT . rCombinator . Set.fromList . flat isrComb

  matchableRules :: [(RTerm,[RTerm])]
  matchableRules
   = [ (template, rewriteTerms )     -- each tuple may represent multiple rules.
     | cl<-eqCl lTerm (concatMap f drs)  -- divide into classes to save a little on the number of matches.
     , let template = lTerm (head cl)   -- This is the template against which to match full expressions.
     , let rewriteTerms = stepTerms template cl
     , not (null rewriteTerms)
     ]
     where f (DEquiR l r) = [DInclR l r, DInclR r l]
           f inclusion = [inclusion]
           stepTerms :: RTerm -> [DerivRule] -> [RTerm]
           stepTerms template cl  -- Only select rules with bindings within the template. Otherwise, we would have to "invent" bindings.
            = [term' | rule<-cl, let term' = rTerm rule, vars term' `Set.isSubsetOf` vars template ]

{-
     showMatchableRules :: [(RTerm,[RTerm])] -> String
     showMatchableRules rs
      = concat ["\n   "++showADL l++" = "++showADL t | (l,tms) <- rs, t<-tms ]
-}

splitList :: [a] -> [([a],a,[a])]
splitList lst = [(take i lst,l,drop (i+1) lst) | (i,l) <- zip [0..] lst]

instance Association RTerm where
  sign (RIsc a)      = sign$ Set.findMin a
  sign (RUni a)      = sign$ Set.findMin a
  sign (RDif a _)    = sign a
  sign (RCpl a)      = sign a
  sign (RDia a b)    = Sign (source a) (target b)
  sign (RLrs a b)    = Sign (source a) (source b)
  sign (RRrs a b)    = Sign (target a) (target b)
  sign (RRad as)     = Sign (source (head as)) (target (last as))
  sign (RCps as)     = Sign (source (head as)) (target (last as))
  sign (RPrd as)     = Sign (source (head as)) (target (last as))
  sign (RKl0 a)      = sign a
  sign (RKl1 a)      = sign a
  sign (RFlp a)      = Sign (target a) (source a)
  sign (RId  a)      = Sign a a
  sign (RVee a b)    = Sign a b
  sign (RAtm _ b)    = Sign b b
  sign (RVar _ _ _)  = fatal 324 "Cannot determine the sign of an RVar." -- This should become a haskell type-error when RTerm is polymorphic
  sign (RConst e)    = sign e

-- In order to write deriviation rules in the Ampersand syntax, RTerms are obtained by means of the (already available) Ampersand parser.
-- For that reason, we need a function term2rTerm to translate a term obtained by parsing (type: Term TermPrim) to a RTerm.
term2rTerm :: Term TermPrim -> RTerm
term2rTerm term
   = if isValid result then result else fatal 385 ("term2rTerm has produced an invalid result: "++showADL result)
     where
      result
       = case term of
           PEqu o l r               -> term2rTerm (PIsc o (PInc o l r) (PInc o r l))
           PInc o l r               -> term2rTerm (PUni o (PCpl o l) r)
           PIsc _ l r               -> combSet RIsc (lSet `Set.union` rSet)
                                       where lSet = case term2rTerm l of
                                                      RIsc terms -> terms
                                                      trm        -> Set.singleton trm
                                             rSet = case term2rTerm r of
                                                      RIsc terms -> terms
                                                      trm        -> Set.singleton trm
           PUni _ l r               -> combSet RUni (lSet `Set.union` rSet)
                                       where lSet = case term2rTerm l of
                                                      RUni terms -> terms
                                                      trm        -> Set.singleton trm
                                             rSet = case term2rTerm r of
                                                      RUni terms -> terms
                                                      trm        -> Set.singleton trm
           PDif _ l r               -> RDif (term2rTerm l) (term2rTerm r)
           PCpl _ e                 -> RCpl (term2rTerm e)
           PDia _ l r               -> RDia (term2rTerm l) (term2rTerm r)
           PLrs _ l r               -> RLrs (term2rTerm l) (term2rTerm r)
           PRrs _ l r               -> RRrs (term2rTerm l) (term2rTerm r)
           PRad _ l r               -> RRad (lLst++rLst)
                                       where lLst = case term2rTerm l of
                                                      RRad terms -> terms
                                                      trm        -> [trm]
                                             rLst = case term2rTerm r of
                                                      RRad terms -> terms
                                                      trm        -> [trm]
           PCps _ l r               -> RCps (lLst++rLst)
                                       where lLst = case term2rTerm l of
                                                      RCps terms -> terms
                                                      trm        -> [trm]
                                             rLst = case term2rTerm r of
                                                      RCps terms -> terms
                                                      trm        -> [trm]
           PPrd _ l r               -> RPrd (lLst++rLst)
                                       where lLst = case term2rTerm l of
                                                      RPrd terms -> terms
                                                      trm        -> [trm]
                                             rLst = case term2rTerm r of
                                                      RPrd terms -> terms
                                                      trm        -> [trm]
           PKl0 _ e                 -> RKl0 (term2rTerm e)
           PKl1 _ e                 -> RKl1 (term2rTerm e)
           PFlp _ e                 -> RFlp (term2rTerm e)
           PBrk _ e                 -> term2rTerm e
           Prim (PNamedR (PNamedRel _ str (Just sgn))) -> RVar str (name (pSrc sgn)) (name (pTgt sgn))
           Prim (Pid _ c)           -> RId  (pCpt2aCpt c)
           Prim (Pfull _ s t)       -> RVee (pCpt2aCpt s) (pCpt2aCpt t)
           Prim (Patm _ a (Just c)) -> RAtm a (pCpt2aCpt c)
           _                        -> fatal 381 ("Cannot cope with untyped "++showADL term++" in a dRule inside the normalizer.")

expr2RTerm :: Expression -> RTerm
expr2RTerm expr
   = if isValid result then result else fatal 443 ("expr2RTerm has produced an invalid result: "++showADL result)
     where
      result
       = case expr of
          EEqu (l,r)           -> expr2RTerm (EIsc (EInc (l,r), EInc (r,l)))
          EInc (l,r)           -> expr2RTerm (EUni (ECpl l, r))
          EIsc (l,r)           -> combSet RIsc (lSet `Set.union` rSet)
                                  where lSet = case expr2RTerm l of
                                                 RIsc terms -> terms
                                                 trm        -> Set.singleton trm
                                        rSet = case expr2RTerm r of
                                                 RIsc terms -> terms
                                                 trm        -> Set.singleton trm
          EUni (l,r)           -> combSet RUni (lSet `Set.union` rSet)
                                  where lSet = case expr2RTerm l of
                                                 RUni terms -> terms
                                                 trm        -> Set.singleton trm
                                        rSet = case expr2RTerm r of
                                                 RUni terms -> terms
                                                 trm        -> Set.singleton trm
          EDif (l,r)           -> RDif (expr2RTerm l) (expr2RTerm r)
          ECpl e               -> RCpl (expr2RTerm e)
          EDia (l,r)           -> RDia (expr2RTerm l) (expr2RTerm r)
          ELrs (l,r)           -> RLrs (expr2RTerm l) (expr2RTerm r)
          ERrs (l,r)           -> RRrs (expr2RTerm l) (expr2RTerm r)
          ERad (l,r)           -> RRad (lLst++rLst)
                                  where lLst = case expr2RTerm l of
                                                 RRad terms -> terms
                                                 trm        -> [trm]
                                        rLst = case expr2RTerm r of
                                                 RRad terms -> terms
                                                 trm        -> [trm]
          ECps (l,r)           -> RCps (lLst++rLst)
                                  where lLst = case expr2RTerm l of
                                                 RCps terms -> terms
                                                 trm        -> [trm]
                                        rLst = case expr2RTerm r of
                                                 RCps terms -> terms
                                                 trm        -> [trm]
          EPrd (l,r)           -> RPrd (lLst++rLst)
                                  where lLst = case expr2RTerm l of
                                                 RPrd terms -> terms
                                                 trm        -> [trm]
                                        rLst = case expr2RTerm r of
                                                 RPrd terms -> terms
                                                 trm        -> [trm]
          EKl0 e               -> RKl0 (expr2RTerm e)
          EKl1 e               -> RKl1 (expr2RTerm e)
          EFlp e               -> RFlp (expr2RTerm e)
          EBrk e               -> expr2RTerm e
          EDcD{}               -> RConst expr
          EDcI c               -> RId c
          EEps{}               -> RConst expr
          EDcV sgn             -> RVee (source sgn) (target sgn)
          EMp1 a c             -> RAtm a c
--   --      _                    -> RConst expr   -- This alternative has been commented out to avoid an "overlapping patterns" warning from Haskell.

rTerm2expr :: RTerm -> Expression
-- implementation note: because RTerms contain variables, it is cumbersome to reconstruct the type. So we don't.
-- Once the variables have been replaced (by means of substitutions) by real expressions, we get a type correct expression again.
-- As a consequence, we cannot use ./\., .\/., etc. in this code.
rTerm2expr term
 = case term of
     RIsc rs    -> case Set.toList (Set.map rTerm2expr rs) of
                    [e] -> e
                    []  -> fatal 445 "empty set in RIsc is illegal."
                    es  -> let oper l r = EIsc (l,r) in foldr1 oper es
     RUni rs    -> case Set.toList (Set.map rTerm2expr rs) of
                    [e] -> e
                    []  -> fatal 449 "empty set in RUni is illegal."
                    es  -> let oper l r = EUni (l,r) in foldr1 oper es
     RDif l r   -> EDif (rTerm2expr l, rTerm2expr r)
     RCpl e     -> ECpl (rTerm2expr e)
     RDia l r   -> EDia (rTerm2expr l, rTerm2expr r)
     RLrs l r   -> ELrs (rTerm2expr l, rTerm2expr r)
     RRrs l r   -> ERrs (rTerm2expr l, rTerm2expr r)
     RRad rs    -> case map rTerm2expr rs of
                    [e] -> e
                    []  -> fatal 458 "empty set in RRad is illegal."
                    es  -> let oper l r = ERad (l,r) in foldr1 oper es
     RCps rs    -> case map rTerm2expr rs of
                    [e] -> e
                    []  -> fatal 462 "empty set in RCps is illegal."
                    es  -> let oper l r = ECps (l,r) in foldr1 oper es
     RPrd rs    -> case map rTerm2expr rs of
                    [e] -> e
                    []  -> fatal 466 "empty set in RPrd is illegal."
                    es  -> let oper l r = EPrd (l,r) in foldr1 oper es
     RKl0 e     -> EKl0$ rTerm2expr e
     RKl1 e     -> EKl1$ rTerm2expr e
     RFlp e     -> EFlp$ rTerm2expr e
     RVar r s t -> EDcD (makeDecl r (Sign (makeConcept s) (makeConcept t)))
     RId  c     -> EDcI c
     RVee s t   -> EDcV (Sign s t)
     RAtm a c   -> EMp1 a c
     RConst e   -> e
   where
     makeDecl nm sgn
      = Sgn { decnm   = nm
            , decsgn  = sgn
            , decprps = fatal 480 "Illegal RTerm in rTerm2expr"
            , decprps_calc = Nothing
            , decprL  = fatal 482 "Illegal RTerm in rTerm2expr"
            , decprM  = fatal 483 "Illegal RTerm in rTerm2expr"
            , decprR  = fatal 484 "Illegal RTerm in rTerm2expr"
            , decMean = fatal 485 "Illegal RTerm in rTerm2expr"
            , decfpos = fatal 486 "Illegal RTerm in rTerm2expr"
            , decusr  = fatal 488 "Illegal RTerm in rTerm2expr"
            , decpat  = fatal 489 "Illegal RTerm in rTerm2expr"
            , decplug = fatal 490 "Illegal RTerm in rTerm2expr"
            }
     makeConcept "ONE" = ONE
     makeConcept  str  = 
        PlainConcept { cptnm = str
                     }

instance ShowADL RTerm where
 showADL = showExpr 0
   where
     (   inter,   union',  diff,  lresi, rresi,  rDia,   rMul,rAdd,rPrd,closK0,closK1,flp',compl,   lpar, rpar, lbr, star,  rbr)
      = (" /\\ ", " \\/ ", " - ", " / ", " \\ ", " <> ", ";", "!", "*", "*"  , "+",   "~", ("-"++), "(",  ")",  "[", "*",   "]")
     showExpr :: Int -> RTerm -> String
     showExpr i expr
      = case expr of
          RIsc ls    -> wrap i 2 (intercalate inter  [showExpr 3 e | e<-Set.toList ls ])
          RUni ls    -> wrap i 2 (intercalate union' [showExpr 3 e | e<-Set.toList ls ])
          RDif l r   -> wrap i 4 (showExpr 5 l++diff++showExpr 5 r)
          RLrs l r   -> wrap i 6 (showExpr 7 l++lresi++showExpr 7 r)
          RRrs l r   -> wrap i 6 (showExpr 7 l++rresi++showExpr 7 r)
          RDia l r   -> wrap i 6 (showExpr 7 l++rDia ++showExpr 7 r)
          RCps ls    -> wrap i 2 (intercalate rMul [showExpr 3 e | e<-ls ])
          RRad ls    -> wrap i 2 (intercalate rAdd [showExpr 3 e | e<-ls ])
          RPrd ls    -> wrap i 2 (intercalate rPrd [showExpr 3 e | e<-ls ])
          RKl0 e     -> wrap i 9 (showExpr 9 e++closK0)
          RKl1 e     -> wrap i 9 (showExpr 9 e++closK1)
          RFlp e     -> wrap i 9 (showExpr 9 e++flp')
          RCpl e     -> wrap i 9 (compl (showExpr 10 e))
          RVar r s t -> r++lbr++s++star++t++rbr
          RConst e   -> wrap i i (showADL e)
          RId c      -> "I"++lbr++name c++rbr
          RVee s t   -> "V"++lbr++name s++star++name t++rbr
          RAtm val c -> showADL val++lbr++name c++rbr
     wrap :: Int -> Int -> String -> String
     wrap i j e' = if i<=j then e' else lpar++e'++rpar

{- momentarily redundant
   unVar :: RTerm -> String
   unVar (RVar r _ _) = r
   unVar _ = fatal 501 "Illegal call on unVar"
-}

vars :: RTerm -> Set String
vars (RIsc rs)     = (Set.unions . map vars . Set.toList) rs
vars (RUni rs)     = (Set.unions . map vars . Set.toList) rs
vars (RDif l r)    = vars l `Set.union` vars r
vars (RCpl e)      = vars e
vars (RDia l r)    = vars l `Set.union` vars r
vars (RLrs l r)    = vars l `Set.union` vars r
vars (RRrs l r)    = vars l `Set.union` vars r
vars (RRad rs)     = foldr Set.union Set.empty (map vars rs)
vars (RCps rs)     = foldr Set.union Set.empty (map vars rs)
vars (RPrd rs)     = foldr Set.union Set.empty (map vars rs)
vars (RKl0 e)      = vars e
vars (RKl1 e)      = vars e
vars (RFlp e)      = vars e
vars (RId  c)      = Set.fromList [name c]
vars (RVee s t)    = Set.fromList [name s, name t]
vars (RVar r s t)  = Set.fromList [r, s, t]
vars (RConst{})    = Set.empty
vars  RAtm{}       = Set.empty

data DerivRule = DEquiR { lTerm :: RTerm  -- equivalence rule
                        , rTerm :: RTerm
                        }
               | DInclR { lTerm :: RTerm  -- inclusion rule
                        , rTerm :: RTerm
                        }

instance Show DerivRule where
  showsPrec _ r@DEquiR{}  = showString (showADL (lTerm r)++" = " ++showADL (rTerm r))
  showsPrec _ r@DInclR{}  = showString (showADL (lTerm r)++" |- "++showADL (rTerm r))

-- For documentation purposes, the derivation rule which proves the step is included.

data DerivStep = DStep { lhs :: RTerm
                       , rul :: (RTerm,Unifier,RTerm)
                       , rhs :: RTerm
                       }

-- instance Show DerivStep where
--  showsPrec _ r@DStep{}  = showString ("    "++showADL (lhs r)++"\n =  {" ++show (rul r)++"}\n    " ++showADL (rhs r))



dRule :: Term TermPrim -> [DerivRule]
dRule (PEqu _ l r) = [DEquiR { lTerm=term2rTerm l, rTerm=term2rTerm r }]
dRule (PInc _ l r) = [DInclR { lTerm=term2rTerm l, rTerm=term2rTerm r }]
dRule term         = fatal 279 ("Illegal use of dRule with term "++showADL term)

slideDown :: (RTerm -> Integer) -> RTerm -> [(Integer,DerivStep)]
slideDown weight term
 = let w = weight term in
   case [dstep | dstep<-dSteps tceDerivRules term, weight (rhs dstep)<w] of
     dstep: _ -> (w,dstep): (slideDown weight) (rhs dstep)
     _        -> []

weightNF :: Bool -> RTerm -> Integer
weightNF dnf term = w term
 where
  w :: RTerm -> Integer
  w trm
   = case trm of
       RIsc ls  -> (sum (map w (Set.toList ls))) * if dnf then 1 else 2
       RUni ls  -> (sum (map w (Set.toList ls))) * if dnf then 2 else 1
       RDif l r -> (w l+w r+10) * 4
       RCpl e   -> (w e + 1) * 4
       RDia l r -> (w l+w r+10) * 4
       RLrs l r -> (w l+w r+10) * 4
       RRrs l r -> (w l+w r+10) * 4
       RRad ls  -> (sum (map w ls)+1)*4
       RCps ls  -> (sum (map w ls)+1)*4
       RPrd ls  -> (sum (map w ls)+1)*4
       RKl0 e   -> (w e + 1) * 4
       RKl1 e   -> (w e + 1) * 4
       RFlp e   -> (w e + 1) * 8
       _        -> 1

-- If  'matches d expr'  yields  'Just ss', then  'substitute anything ss (lTerm d) == expr'

type Unifier = Set (String, RTerm)

instance ShowADL Unifier where
  showADL s = "{"++intercalate ", " [ str++"->"++showADL t | (str,t)<-Set.toList s ]++"}"

substitute :: String    -- A string to document fatals
           -> Unifier   -- the substitution, which in reality is a set of string/expression pairs.
           -> RTerm     -- The term to be transformed to an expression, with all variables replaced by subexpressions
           -> RTerm
substitute ruleDoc unifier term
 = if isValid result then result else fatal 713 ("substitute has produced an invalid result: "++showADL result)
   where
    result = subs term
    subs :: RTerm -> RTerm
    subs t | not (isValid t) = fatal 680 ("Substituting an invalid term "++showADL t)
    subs (RIsc s)     = (combSet RIsc . Set.fromList . flat isRIsc . map subs . Set.toList) s
    subs (RUni s)     = (combSet RUni . Set.fromList . flat isRUni . map subs . Set.toList) s
    subs (RDif l r)   = RDif (subs l) (subs r)
    subs (RLrs l r)   = RLrs (subs l) (subs r)
    subs (RRrs l r)   = RRrs (subs l) (subs r)
    subs (RDia l r)   = RDia (subs l) (subs r)
    subs (RCps ls)    = (RCps . flat isRCps . map subs) ls
    subs (RRad ls)    = (RRad . flat isRRad . map subs) ls
    subs (RPrd ls)    = (RPrd . flat isRPrd . map subs) ls
    subs (RKl0 e  )   = RKl0 (subs e)
    subs (RKl1 e  )   = RKl1 (subs e)
    subs (RFlp e  )   = RFlp (subs e)
    subs (RCpl e  )   = RCpl (subs e)
    subs (RVar r _ _) = case [ e | (v,e)<-Set.toList unifier, v==r] of
                           [e] -> e
                           [] ->  fatal 378 ("Rule:  "++ruleDoc++"\nVariable "++r++" is not in term "++showADL term++ " using unifier "++show unifier)
                           -- e.g. Variable r is not in term -V[A*B] /\ r[A*B] using unifier fromList [("A",RId Verzoek),("B",RId Persoon)]
                           es ->  fatal 379 ("Rule:  "++ruleDoc++"\nVariable "++r++" in term "++showADL term++" has been bound to multiple expressions:\n   "++intercalate "\n   " [showADL e | e<-es])
    subs (RId c)      = case [ e | (v,e)<-Set.toList unifier, v==name c] of
                           [e] -> e  -- This is e@(RId c')
                           []  -> fatal 382 ("Rule:  "++ruleDoc++"\nVariable "++name c++" is not in term "++showADL term)
                           es  -> fatal 383 ("Rule:  "++ruleDoc++"\nVariable "++name c++" in term "++showADL term++" has been bound to multiple expressions:\n   "++intercalate "\n   " [showADL e | e<-es])
    subs (RVee s t)   = case ([ e | (v,e)<-Set.toList unifier, v==name s], [ e | (v,e)<-Set.toList unifier, v==name t]) of
                           ([RId s'], [RId t']) -> RVee s' t'
                           (_,_)  -> fatal 386 ("Rule:  "++ruleDoc++"\nSomething wrong with RVee in term "++showADL term++" with unifier "++show unifier)
    subs (RAtm a c)   = case [ e | (v,e)<-Set.toList unifier, v==name c] of
                           [RId c'] -> RAtm a c'
                           []  -> fatal 389 ("Rule:  "++ruleDoc++"\nVariable "++name c++" is not in term "++showADL term)
                           es  -> fatal 390 ("Rule:  "++ruleDoc++"\nVariable "++name c++" in term "++showADL term++" has been bound to multiple expressions:\n   "++intercalate "\n   " [showADL e | e<-es])
    subs e@RConst{}   = e
--     subs t            = fatal 392 ("Rule:  "++ruleDoc++"\nError: "++showADL t++"is not a variable.")  -- commented out, because it causes Haskell to emit an overlapping pattern warning.

flat :: (RTerm -> Bool) -> [RTerm] -> [RTerm]
flat isrComb ls
 = case ls of
    []  -> fatal 689 "Illegal empty list"
    es  -> concat [ if isrComb e then rTermListForSets e else [e] | e<-es]
--     es  -> if or [isrComb e | e<-es]   -- SJ: Apparently, the recursion in 'flat' is required. Without it, Misc/Kernmodel.adl did fail on 18 aug 2014.
--            then (flat isrComb . concat) [ if isrComb e then rTermListForSets e else [e] | e<-es]
--            else es
rTermListForSets :: RTerm -> [RTerm]
rTermListForSets (RIsc s) = Set.toList s
rTermListForSets (RUni s) = Set.toList s
rTermListForSets x = rTermList x

matches :: RTerm -> RTerm -> [Unifier]
matches term expr
  = if not (isValid term) then fatal 719 ("Invalid term "++showADL term++"\nbeing matched to expression "++showADL expr) else
    if not (isValid expr) then fatal 720 ("Matching term "++showADL term++"\nto invalid expression "++showADL expr) else
    case (term,expr) of
     (RIsc es,        RIsc es')   -> matchSets RIsc es es'
     (RUni es,        RUni es')   -> matchSets RUni es es'
     (RDif l r,       RDif l' r') -> matches l l' ++ matches r r'
     (RLrs l r,       RLrs l' r') -> matches l l' ++ matches r r'
     (RRrs l r,       RRrs l' r') -> matches l l' ++ matches r r'
     (RDia l r,       RDia l' r') -> matches l l' ++ matches r r'
     (RCps ls,        RCps ls')   -> matchLists RCps ls ls'
     (RRad ls,        RRad ls')   -> matchLists RRad ls ls'
     (RPrd ls,        RPrd ls')   -> matchLists RPrd ls ls'
     (RKl0 e,         RKl0 e')    -> matches e e'
     (RKl1 e,         RKl1 e')    -> matches e e'
     (RFlp e,         RFlp e')    -> matches e e'
     (RCpl e,         RCpl e')    -> matches e e'
     (RId  c,         RId _     ) -> [Set.fromList [(name c,expr)]]
     (RVee s t,       RVee s' t') -> [Set.fromList [(name s,RId s'), (name t,RId t')]]
     (RVar v s t,     _         ) -> [Set.fromList [(v,expr),(s,RId (source expr)),(t,RId (target expr))]]
     (RAtm a c,       RAtm a' c') -> [Set.singleton (name c,RId c') | a==a']
     (RConst e,       RConst e' ) -> [Set.empty | e==e']
     (_, _)                       -> []

matchLists :: ([RTerm] -> RTerm) -> [RTerm] -> [RTerm] -> [Unifier]
matchLists rCombinator es es'
 = if not (isValid (combLst rCombinator es) ) then fatal 754 ("Invalid term " ++showADL (rCombinator es)++"\nbeing matched to expression "++showADL (rCombinator es')) else
   if not (isValid (combLst rCombinator es')) then fatal 755 ("Matching term "++showADL (rCombinator es)++"\nto invalid expression " ++showADL (rCombinator es')) else
   [ unif
   | let n = length es              -- the length of the template, which contains variables
   , if n==0 then fatal 681 "n equals 0" else True
   , ms <- dist n es'     -- determine segments from es' (which is variable free) that have the same length as the template es
   , if or [null m | m<-ms]
     then fatal 683 (concat ["\nms:  ["++intercalate ", " (map showADL m)++"]" | m<-ms])
     else True
   , let subTerms = map (combLst rCombinator) ms     -- make an RTerm from each sublist in ms
   , unif<-mix [ matches l r | (l,r)<-safezip es subTerms ]
   , noDoubles unif                 -- if one variable, v, is bound to more than one different expressions, the deal is off.
   ]
   where
     dist :: Int -> [a] -> [[[a]]]
     dist 1 ls = [[ls]]
     dist 2 ls = [ [ take i ls , drop i ls ] | i<-[1..length ls-1] ]
     dist n ls = [ init ds++st | ds<-dist (n-1) ls, let staart=last ds, length staart>=2, st<-dist 2 staart ]
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
mix :: [[Unifier]] -> [Unifier]
mix (ls:lss) = [ Set.union e str | e<-ls, str<-mix lss ]
mix []       = [Set.empty]
{- example:
   mix ["12","xyz","","p"] = [] -- (because the fourth element did not match!)
   mix ["12","xyz","p"]    = ["1xp","1yp","1zp","2xp","2yp","2zp"]
-}
matchSets :: (Set RTerm -> RTerm) -> Set RTerm -> Set RTerm -> [Unifier]
matchSets rCombinator es es'
 = -- set sizes are not necessarily equal.
   if Set.null es || Set.null es' then fatal 858 "cannot match empty sets" else
   if or [ not (isValid e) | e<-Set.toList es ] then fatal 859 ("Invalid subterm(s): "++intercalate ", " [ showADL e | e<-Set.toList es,  not (isValid e) ]) else
   if or [ not (isValid e) | e<-Set.toList es'] then fatal 860 ("Invalid subexpr(s): "++intercalate ", " [ showADL e | e<-Set.toList es', not (isValid e) ]) else
   [ unif
   | let n = Set.size cdes                      -- the length of the template, which contains variables
   , partition' <- parts n cdes'                 -- determine segments from the expression with the same length. partition' :: Set (Set RTerm)
   , let subTerms = Set.map (combSet rCombinator) partition'      -- make an RTerm from each subset in ms. subTerms :: Set RTerm
   , template <- permutations (Set.toList cdes)
   , unif <- mix [ matches l r | (l,r) <- safezip template (Set.toList subTerms) ]
   , noDoubles unif                 -- if one variable, v, is bound to more than one different expressions, the deal is off.
   ]
   where
     isct  = es `Set.intersection` es'            -- E.g.:  {'Piet'}
     cdes  = es  `Set.difference` isct            -- the terms of es that are not in es' (a set of templates). E.g.: { r;s }
     cdes' = es' `Set.difference` isct            -- candidates for binding to a variable: { a\b , a;b;c , d , e;f }  (a set of expressions)

separate :: Ord a => Int -> Set a -> [(Set a, Set a)]
separate n s = [ (part, s `Set.difference` part) | part <- subsetLength n (Set.toList s) ]
 where
   subsetLength :: Ord a => Int -> [a] -> [Set a]
   subsetLength 0 _      = [Set.empty]
   subsetLength i (x:xs) = map (Set.insert x) (subsetLength (i-1) xs) ++ subsetLength i xs
   subsetLength _ []     = []

 -- parts produces a fixed number of subsets
parts :: Ord a => Int -> Set a -> [Set (Set a)]  -- ,   but within this where clause we must make it more specific.
parts n xsss = (Set.toList . Set.fromList . map Set.fromList . map (map Set.fromList) . p n . Set.toList) xsss
 where
   p :: Eq a => Int -> [a] -> [[[a]]]
   p 0 _  = []
   p 1 xs = [ [xs] ]
   p 2 xs = [ [ss,rest] | ss<-init (subsets xs), let rest=[ e | e<-xs, e `notElem` ss ], not (null rest) ]
   p i xs = [ twoSets++tl | (hd:tl)<-p (i-1) xs, twoSets<-p 2 hd ]
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
   subsets []  = [[]]
   subsets (x:xs) = map (x:) (subsets xs) ++ subsets xs

combLst :: ([RTerm] -> RTerm) -> [RTerm] -> RTerm
combLst rCombinator es
 = case es of
        []  -> fatal 791 "Not allowed."
        [e] -> e
        _   -> rCombinator es

combSet :: (Set RTerm -> RTerm) -> Set RTerm -> RTerm
combSet rCombinator s
 = case Set.toList s of
        []  -> fatal 798 "Not allowed."
        [e] -> e
        _   -> rCombinator s

-- Example: noDoubles { p->A;B, q->'Piet', p->'Z', r->A* } is False, because p binds two different expressions.
noDoubles :: Unifier -> Bool
noDoubles unif = and [ n==1 | n<-(map length . eqCl fst . Set.toList) unif ]

safezip :: [a] -> [b] -> [(a,b)]
safezip (a:as) (b:bs) = (a,b):safezip as bs
safezip [] [] = []
safezip _ _ = fatal 827 "Zip of two lists with different lengths!"

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
   names :: RTerm -> [String]
   names term = nub (nms term)
    where nms trm = case trm of
                        RIsc ls    -> (nub . concatMap nms . Set.toList) ls
                        RUni ls    -> (nub . concatMap nms . Set.toList) ls
                        RDif l r   -> nms l++nms r
                        RLrs l r   -> nms l++nms r
                        RRrs l r   -> nms l++nms r
                        RDia l r   -> nms l++nms r
                        RCps ls    -> (nub . concatMap nms) ls
                        RRad ls    -> (nub . concatMap nms) ls
                        RPrd ls    -> (nub . concatMap nms) ls
                        RKl0 e     -> nms e
                        RKl1 e     -> nms e
                        RFlp e     -> nms e
                        RCpl e     -> nms e
                        RVar r s t -> [r++":"++s++"*"++t]
                        RId c      -> ["I["++name c++"]"]
                        RVee s t   -> ["V["++name s++"*"++name t++"]"]
                        RAtm a c   -> ["'"++a++"'["++name c++"]"]
                        RConst e   -> [showADL e]
-}

-- In order to write rules for the normalizer in a legible manner, I am using the Ampersand parser.
-- The terminal symbols, except I and V, are interpreted as variables in these rules.
-- As these rules may be used in two directions, all concept variables that are used on one side must be used on the other side as well.
-- relation names r, s, q are used as relation variables and A, B, C are used as concept variables.
-- If rules are ill formed, this will result in fatal errors.

-- Type conserving equivalences: The following equivalences have an identical signature on either side.
tceDerivRules :: [DerivRule]
tceDerivRules = concatMap (dRule.parseRule)
--    [ "r[A*B]\\/s[A*B] = s[A*B]\\/r[A*B]"                         --  Commutativity of \/
--    , "r[A*B]/\\s[A*B] = s[A*B]/\\r[A*B]"                         --  Commutativity of /\
--    , "(r[A*B]\\/s[A*B])\\/q[A*B] = r[A*B]\\/(s[A*B]\\/q[A*B])"   --  Associativity of \/
--    , "(r[A*B]/\\s[A*B])/\\q[A*B] = r[A*B]/\\(s[A*B]/\\q[A*B])"   --  Associativity of /\
--    , "(r[A*B];s[B*C]);q[C*D] = r[A*B];(s[B*C];q[C*D])"           --  Associativity of ;
--    , "(r[A*B]#s[B*C])#q[C*D] = r[A*B]#(s[B*C]#q[C*D])"           --  Associativity of #
--    , "(r[A*B]!s[B*C])!q[C*D] = r[A*B]!(s[B*C]!q[C*D])"           --  Associativity of !
 [ "-(-r[A*B]) = r[A*B]"                                       --  Double negation
 , "(r[A*B]~)~ = r[A*B]"                                       --  Double flip
 , "-(r[A*B]~) = (-r[A*B])~"                                   --  Peirce's[A*A] trick, which allows us to write -r[A*A]~
 , "-r[A*B]/\\-s[A*B] = -(r[A*B]\\/s[A*B])"                    --  De Morgan
 , "-r[A*B]\\/-s[A*B] = -(r[A*B]/\\s[A*B])"                    --  De Morgan
 , "-r[B*A];-s[A*C] = -(r[B*A]!s[A*C])"                        --  De Morgan
 , "-r[B*A]!-s[A*C] = -(r[B*A];s[A*C])"                        --  De Morgan
 , "r[A*B]/\\-s[C*D] = r[A*B]-s[C*D]"                          --  Avoid complement
 , "r[A*B]~/\\s[A*B]~ = (r[A*B]/\\s[A*B])~"                    --  Distribute flip
 , "r[A*B]~/\\-s[C*D]~ = (r[A*B]-s[C*D])~"                     --  Avoid complement
 , "r[A*B]~\\/s[A*B]~ = (r[A*B]\\/s[A*B])~"                    --  Distribute flip
 , "(r[A*A]\\r[A*A]);(r[A*A]\\r[A*A]) = r[A*A]\\r[A*A]"        --  Jipsen&Tsinakis
 , "(r[A*A]/r[A*A]);(r[A*A]/r[A*A]) = r[A*A]/r[A*A]"           --  Jipsen&Tsinakis
 , "r[A*A];(r[A*A]\\r[A*A]) = r[A*A]"                          --  Jipsen&Tsinakis
 , "(r[A*A]/r[A*A]);r[A*A] = r[A*A]"                           --  Jipsen&Tsinakis
 , "I[A];r[A*B] = r[A*B]"
 , "r[A*B];I[B] = r[A*B]"
 , "(r[A*B]\\/s[A*B]);q[B*C] = r[A*B];q[B*C]\\/s[A*B];q[B*C]"  --  Distribution
 , "r[A*B];(s[B*C]\\/q[B*C]) = r[A*B];s[B*C]\\/r[A*B];q[B*C]"  --  Distribution
 , "-r[A*B]~!s[A*C] = r[A*B]\\s[A*C]"                          --  eliminate dagger
 , "-r[A*B]!s[B*C] = r[A*B]~\\s[B*C]"                          --  eliminate dagger
 , "r[A*C]!-s[B*C]~ = r[A*C]/s[B*C]"                           --  eliminate dagger
 , "r[A*C]!-s[C*B] = r[A*C]/s[C*B]~"                           --  eliminate dagger
--  , "r[A*B]#s[B*C]#q[C*D] = r[A*B]#q[C*D]"                      --  eliminate middle in cartesian product -- conditie toevoegen: s[A*B] /= -V
 , "r[A*B]/\\r[A*B] = r[A*B]"                                  --  Absorb equals
 , "r[A*B]\\/r[A*B] = r[A*B]"                                  --  Absorb equals
 , "r[A*B]/\\V[A*B] = r[A*B]"                                  --  Absorb V
 , "V[A*B]/\\r[A*B] = r[A*B]"                                  --  Absorb V
 , "r[A*B]/\\-V[A*B] = -V[A*B]"                                --  Contradiction
 , "-V[A*B]/\\r[A*B] = -V[A*B]"                                --  Contradiction
 , "r[A*B]\\/V[A*B] = V[A*B]"                                  --  Tautology
 , "r[A*B]\\/-V[A*B] = r[A*B]"                                 --  Absorb -V
 , "r[A*B]/\\-r[A*B] = -V[A*B]"                                --  Contradiction
 , "r[A*B]\\/-r[A*B] =  V[A*B]"                                --  Tautology
 , "-r[A*B]\\/r[A*B] = V[A*B]"                                 --  Tautology
 , "(r[A*B]\\/ s[A*B])/\\ s[A*B] = s[A*B]"                     --  Absorption
 , "(r[A*B]\\/-s[A*B])/\\ s[A*B] = s[A*B]-r[A*B]"               --  Absorption
 , "(r[A*B]/\\ s[A*B])\\/ s[A*B] = s[A*B]"                     --  Absorption
 , "(r[A*B]/\\-s[A*B])\\/ s[A*B] = r[A*B]\\/s[A*B]"            --  Absorption
 , "(r[A*B]/\\ s[A*B])\\/-s[A*B] = r[A*B]\\/-s[A*B]"           --  Absorption
 , "r[A*A]* = r[A*A];r[A*A]*"
 , "r[A*A]* = r[A*A]*;r[A*A]"
 , "r[A*A]+ = r[A*A];r[A*A]+"
 , "r[A*A]+ = r[A*A]+;r[A*A]"
 , "I[A]\\/r[A*A]+ = r[A*A]*"
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

head :: [a] -> a
head [] = fatal 30 "head must not be used on an empty list!"
head (a:_) = a

-- | This delta is meant to be used as a placeholder for inserting or removing links from expressions.
delta :: Signature -> Expression
delta sgn
 = EDcD   Sgn { decnm   = "Delta"
              , decsgn  = sgn
              , decprps = []
              , decprps_calc = Nothing
              , decprL  = ""
              , decprM  = ""
              , decprR  = ""
              , decMean = AMeaning [ --   A_Markup Dutch   (string2Blocks ReST "Delta is bedoeld als variabele, die de plaats in een expressie vasthoudt waar paren worden ingevoegd of verwijderd.")
                                     -- , A_Markup English (string2Blocks ReST "Delta is meant as a variable, to be used as a placeholder for inserting or removing links from expressions.")
                                   ]
              , decfpos = Origin ("generated relation (Delta "++show sgn++")")
              , decusr  = False
              , decpat  = ""
              , decplug = True
              }

{- Normalization of process algebra clauses -}

normPA :: Options -> PAclause -> PAclause
normPA opts pac = pac'
    where (pac',_,_) = if null (proofPA opts pac) then fatal 21 "last: empty list" else last (proofPA opts pac)

type Proof a = [(a, [String], String)]

{- A proof is a list of triples (e, ss, rel), where |e| is an expression in the chain;
   |rel| is the relation (equality, inclusion, ...) relating the |e| with the next
   expression, and |ss| is a documentation of the hint, stating the rule that has been applied.
   2015-09-12 Stef thinks that the end of the chain is the only triple with empty hint,
   being supported by the base case of |proofPA| below.

   WK: I typically do |(a, [(rel, hint, a)])|.
-}

proofPA :: Options -> PAclause -> Proof PAclause
proofPA opts = {-reverse.take 3.reverse.-}pPA
 where pPA pac' = case normstepPA opts pac' of
                    ( _ , []  ,equ) -> [(pac',[]   ,equ)]    -- is dus (pac,[],"<=>")
                    (res,steps,equ) -> (pac',steps,equ):pPA res

{- The following rewriter is used to simplify the actions inside eca rules.
-- WHY? Stef, kan je uitleggen wat hier gebeurt? Enig commentaar is hier wel op zijn plaats.
-- Ook zou het helpen om bij de verschillende constructoren van PAclause een beschrijving te geven van het idee er achter.
-- BECAUSE! Kan ik wel uitleggen, maar het is een heel verhaal. Dat moet tzt in een wetenschappelijk artikel gebeuren, zodat het er goed staat.
-- Het idee is dat een procesalgebra is weergegeven in Haskell combinatoren (gedefinieerd als PAclause(..), zie ADL.ECArule).
-- Die kun je vervolgens normaliseren met herschrijfregels op basis van gelijkheden die gelden in de bewuste procesalgebra.
-- Helaas zijn de herschrijfregels nu nog hard gecodeerd, zodat ik voor PAclause een afzonderlijke Haskell functie moet schrijven.
-- Hierna volgt de normalisator voor relatiealgebra-expressies, genaamd normStep. Die heeft dezelfde structuur,
-- maar gebruikt herschrijfregels vanuit gelijkheden die gelden in relatiealgebra.
-}
normstepPA :: Options -> PAclause -> (PAclause,[String],String)
normstepPA opts pac = (res,ss,"<=>")
 where
  (res,ss) = norm pac
  norm :: PAclause -> (PAclause,[String])
  norm (CHC [] ms)  = (Blk ms, ["Run out of options"])
  norm (CHC [r] ms) = (r', ["Flatten ONE"])
                    where r' = case r of
                                 Blk{} -> r
                                 _     -> r{paMotiv = ms}
  norm (CHC ds ms)  | (not.null) msgs = (CHC ops ms, msgs)
                    | (not.null) [d | d<-ds, isCHC d] = (CHC (nub [ d' | d<-ds, d'<-if isCHC d then let CHC ops' _ = d in ops' else [d] ]) ms, ["flatten CHC"])  -- flatten
                    | (not.null) [Nop | Nop{}<-ops] = (Nop{paMotiv=ms}, ["Choose to do nothing"])
                    | (not.null) [Blk | Blk{}<-ops] = (CHC [op | op<-ops, not (isBlk op)] ms, ["Choose anything but block"])
                    | (not.null) doubles = (CHC [ head cl | cl<-eqClass (==) ds ] ms, ["remove double occurrences"])
                    | otherwise = (CHC ds ms, [])
                    where nds  = map norm ds
                          msgs = concatMap snd nds
                          ops  = map fst nds
                          doubles = [ d | cl<-eqClass (==) ds, length cl>1, d<-cl ]
  norm (GCH [] ms)  = (Blk ms, ["Run out of options"])
  norm (GCH ds ms)  | (not.null) [() | (_,links,_)<-normds, isFalse links] = (GCH [(tOp,links,p) | (tOp,links,p)<-normds, not (isFalse links)] ms, ["Remove provably empty guard(s)."])
                    | (not.null) [()          | (_,  _    ,p)<-normds, isNop p]
                        = (GCH [(tOp,links,p) | (tOp,links,p)<-normds, not (isNop p)] ms, ["Remove unneccessary SELECT."])
                    | (not.null) doubles = (GCH [ (fst3 (head cl), foldr1 (.\/.) (map snd3 cl), thd3 (head cl)) | cl<-eqCl (\(tOp,_,p)->(tOp,p)) ds ] ms, ["remove double occurrences"])
                    | otherwise = (GCH ds ms, [])
                    where normds = [ (tOp, conjNF opts links, let (p',_)=norm p in p') | (tOp,links,p)<-ds]
                          doubles = [ d | cl<-eqCl (\(tOp,_,p)->(tOp,p)) ds, length cl>1, d<-cl ]
  norm (ALL [] ms)  = (Nop ms, ["ALL [] = No Operation"])
  norm (ALL [d] ms) = (d', ["Flatten ONE"])
                    where d' = case d of
                                 Blk{} -> d
                                 _     -> d{paMotiv = ms}
  norm (ALL ds ms)  | (not.null) msgs = (ALL ops ms, msgs)
                    | (not.null) [d | d<-ds, isAll d] = (ALL (nub [ d' | d<-ds, d'<-if isAll d then let ALL ops' _ = d in ops' else [d] ]) ms, ["flatten ALL"])  -- flatten
                    | (not.null) [Blk | Blk{}<-ops] = (Blk{paMotiv = [m | op@Blk{}<-ops,m<-paMotiv op]}, ["Block all"])
                    | (not.null) [Nop | Nop{}<-ops] = (ALL [op | op<-ops, not (isNop op)] ms, ["Ignore Nop"])
                    | (not.null) doubles = (CHC [ head cl | cl<-eqClass (==) ds ] ms, ["remove double occurrences"])
                    | (not.null) long    = (ALL ds' ms, ["Take the expressions for "++commaEng "and" [(name . paTo . head) cl |cl<-long]++"together"])
                    | otherwise = (ALL ds ms, [])
                    where ds'     = [ let p=head cl in
                                        if length cl==1 then p else p{paDelta=disjNF opts (foldr1 (.\/.) [paDelta c | c<-cl]), paMotiv=concatMap paMotiv cl}
                                    | cl<-dCls {- not (null cl) is guaranteed by eqCl -} ]
                                    ++[d | d<-ds, not (isDo d)]
                          nds     = map norm ds
                          msgs    = concatMap snd nds
                          ops     = map fst nds
                          doubles = [ d | cl<-eqClass (==) ds, length cl>1, d<-cl ]
                          dCls :: [[PAclause]]
                          dCls = eqCl to [d | d<-ds, isDo d]
                          long :: [[PAclause]]
                          long = [cl | cl<-dCls, length cl>1]
                          to d = case d of
                                   Do{} -> (paSrt d, paTo d)
                                   _    -> fatal 74 "illegal call of to(d)"
  norm (New c p ms)        = ( case p' of
                                Blk{} -> p'{paMotiv = ms}
                                _     -> New c (\x->let (p'', _) = norm (p x) in p'') ms
                             , msgs)
                             where (p', msgs) = norm (p (makePSingleton "x"))
  norm (Rmv c p ms)        = ( case p' of
                                Blk{} -> p'{paMotiv = ms}
                                _     -> Rmv c (\x->let (p'', _) = norm (p x) in p'') ms
                             , msgs)
                             where (p', msgs) = norm (p (makePSingleton "x"))
  norm p                   = (p, [])

{- Normalization of expressions -}

simplify :: Expression -> Expression
simplify expr = expr'
    where (expr',_,_) = if null (simpProof shw expr) then fatal 101 "last: empty list" else last (simpProof shw expr)
          shw _ = ""

simpProof :: (Expression -> String) -> Expression -> Proof Expression
simpProof shw expr
 = if expr==res
   then [(expr,[],"<=>")]
   else (expr,steps,equ):simpProof shw res
 where (res,steps,equ) = normStep shw True True True expr

-- | The purpose of "normStep" is to elaborate a single step in a rewrite process,
-- in which the expression is normalized by means of rewrite rules.
-- This function can be used for simplification, which means that an Expression is standardized
-- using associativity and other 'trivial' rules only.
-- These 'trivial' rules do not produce a step in the proof.
-- Use normstep shw eq True expr to do simplification only.
-- Use normstep shw eq False expr to obtain a single proof step or none when no rule is applicable.
-- This function returns a resulting expression that is closer to a normal form.
-- The normal form is not unique. This function simply uses the first rewrite rule it encounters.
normStep :: (Expression -> String) -> Bool -> Bool -> Bool ->
            Expression -> (Expression,[String],String) -- This might be generalized to "Expression" if it weren't for the fact that flip is embedded in the Relation type.
normStep shw   -- a function to print an expression. Might be "showADL"
         eq    -- If eq==True, only equivalences are used. Otherwise, inclusions are used as well.
         dnf   -- If dnf==True, the result is in disjunctive normal form, otherwise in conjunctive normal form
         simpl -- If True, only simplification rules are used, which is a subset of all rules. Consequently, simplification is implied by normalization.
         expr = if sign expr==sign res then (res,ss,equ) else
                fatal 166 ("Violation of sign expr==sign res in the normalizer\n  expr: sign( "++showADL expr++" ) == "++showSign res++"\n  res:  sign( "++showADL res++" ) == "++showSign res)
{-SJ 20140720: You might wonder why we test sign expr==sign res, which was introduced as a result of ticket #409 (the residu bug)
It turns out that many rewrite rules in the normalizer change the type of an expression; an aspect I have been overlooking all the time.
Until the new normalizer works, we will have to work with this one. So I have inserted this test to ensure that the type remains constant during normalization.
-}
 where
  (res,ss,equ) = nM True expr []
  nM :: Bool -> Expression -> [Expression] -> (Expression,[String],String)
-- posCpl indicates whether the expression is positive under a complement. It is False when expr is inside a complemented expression.
  nM posCpl (EEqu (l,r)) _     | simpl = (t .==. f, steps++steps', fEqu [equ',equ''])
                                         where (t,steps, equ')  = nM posCpl l []  -- TODO: the use of posCpl is erroneous
                                               (f,steps',equ'') = nM posCpl r []  -- TODO: the use of posCpl is erroneous
  nM posCpl (EInc (l,r)) _     | simpl = (t .|-. f, steps++steps', fEqu [equ',equ''])
                                         where (t,steps, equ')  = nM (not posCpl) l []
                                               (f,steps',equ'') = nM posCpl r []
  nM posCpl (EUni (EUni (l,k),r)) rs   = nM posCpl (l .\/. (k .\/. r)) rs  -- standardize, using associativity of .\/.
  nM posCpl (EUni (l,r)) rs    | simpl = (t .\/. f, steps++steps', fEqu [equ',equ''])
                                         where (t,steps, equ')  = nM posCpl l []
                                               (f,steps',equ'') = nM posCpl r (l:rs)
  nM posCpl (EIsc (EIsc (l,k),r)) rs   = nM posCpl (l ./\. (k ./\. r)) rs  -- standardize, using associativity of ./\.
  nM posCpl (EIsc (l,r)) rs    | simpl = (t ./\. f, steps++steps', fEqu [equ',equ''])
                                         where (t,steps, equ')  = nM posCpl l []
                                               (f,steps',equ'') = nM posCpl r (l:rs)
  nM posCpl (ECps (ECps (l,k),r)) rs   = nM posCpl (l .:. (k .:. r)) rs  -- standardize, using associativity of .:.
                                             -- Note: function shiftL and shiftR make use of the fact that this normalizes to (l .:. (k .:. r))
  nM posCpl (ECps (l,r)) rs    | simpl = (t .:. f, steps++steps', fEqu [equ',equ''])
                                          where (t,steps, equ')  = nM posCpl l []
                                                (f,steps',equ'') = nM posCpl r (l:rs)
  nM posCpl (ELrs (l,r)) _     | simpl = (t ./. f, steps++steps', fEqu [equ',equ''])
                                         where (t,steps, equ')  = nM posCpl l []
                                               (f,steps',equ'') = nM (not posCpl) r []
  nM posCpl (ERrs (l,r)) _     | simpl = (t .\. f, steps++steps', fEqu [equ',equ''])
                                         where (t,steps, equ')  = nM (not posCpl) l []
                                               (f,steps',equ'') = nM posCpl r []
  nM posCpl (EDia (l,r)) _     | simpl = (t .<>. f, steps++steps', fEqu [equ',equ''])
                                         where (t,steps, equ')  = nM posCpl l []  -- TODO: the use of posCpl is erroneous
                                               (f,steps',equ'') = nM posCpl r []  -- TODO: the use of posCpl is erroneous
  nM posCpl (ERad (ERad (l,k),r)) rs   = nM posCpl (l .!. (k .!. r)) rs  -- standardize, using associativity of .!.
  nM posCpl (ERad (l,r)) rs    | simpl = (t .!. f, steps++steps', fEqu [equ',equ''])
                                         where (t,steps, equ')    = nM posCpl l []
                                               (f,steps',equ'')   = nM posCpl r (l:rs)
  nM posCpl (EPrd (EPrd (l,k),r)) rs   = nM posCpl (l .*. (k .*. r)) rs  -- standardize, using associativity of .*.
  nM posCpl (EPrd (l,r)) _     | simpl = (t .*. f, steps++steps', fEqu [equ',equ''])
                                         where (t,steps, equ')  = nM posCpl l []
                                               (f,steps',equ'') = nM posCpl r []
  nM posCpl (EKl0 e)              _    = (EKl0 res', steps, equ')
                                         where (res',steps,equ') = nM posCpl e []
  nM posCpl (EKl1 e)              _    = (EKl1 res', steps, equ')
                                         where (res',steps,equ') = nM posCpl e []
  nM posCpl (ECpl (ECpl e))         rs = nM posCpl e rs
  nM posCpl (ECpl e) _         | simpl = (notCpl res',steps,equ')
                                         where (res',steps,equ') = nM (not posCpl) e []
  nM posCpl (EBrk e)                _  = nM posCpl e []
  nM posCpl (EFlp (ECpl e))         rs = nM posCpl (notCpl (flp e)) rs
  nM _      x _                | simpl = (x,[],"<=>")
-- up to here, simplification has been treated. The remaining rules can safely assume  simpl==False
  nM _      (EEqu (l,r)) _                            = ((l .|-. r) ./\. (r .|-. l), ["remove ="],"<=>")
  nM _      (EInc (x,r@(ELrs (z,y)))) _               = if sign x==sign z -- necessary to guarantee that sign expr is equal to sign of the result
                                                        then (x .:. y .|-. z, ["remove left residual (/)"],"<=>")
                                                        else (notCpl x .\/. r, ["remove |-"],"<=>")
  nM _      (EInc (y,r@(ERrs (x,z)))) _               = if sign y==sign z -- necessary to guarantee that sign expr is equal to sign of the result
                                                        then (x .:. y .|-. z, ["remove right residual (\\)"],"<=>")
                                                        else (notCpl y .\/. r, ["remove |-"],"<=>")
  nM _      (EInc (l,r)) _                            = (notCpl l .\/. r, ["remove |-"],"<=>")
--   nM posCpl e@(ECpl EIsc{}) _           | posCpl==dnf = (deMorganEIsc e, ["De Morgan"], "<=>")
--   nM posCpl e@(ECpl EUni{}) _           | posCpl/=dnf = (deMorganEUni e, ["De Morgan"], "<=>")
  nM _      e@(ECpl EIsc{}) _                         = (deMorganEIsc e, ["De Morgan"], "<=>")
  nM _      e@(ECpl EUni{}) _                         = (deMorganEUni e, ["De Morgan"], "<=>")
  nM _      e@(ECpl (ERad (_,ECpl{}))) _              = (deMorganERad e, ["De Morgan"], "<=>")
  nM _      e@(ECpl (ERad (ECpl{},_))) _              = (deMorganERad e, ["De Morgan"], "<=>")
  nM _      e@(ECpl (ECps (ECpl{},ECpl{}))) _         = (deMorganECps e, ["De Morgan"], "<=>")
  nM posCpl (ECpl e) _                                = (notCpl res',steps,equ')
                                                        where (res',steps,equ') = nM (not posCpl) e []
  nM _      (ECps (EEps c (Sign s _),EEps c' (Sign _  t'))) _ | c ==c' = (EEps c  (Sign s t'), [], "<=>")
  nM _      (ECps (EEps c (Sign s t),EEps c' (Sign _  t'))) _ | c ==t  = (EEps c' (Sign s t'), [], "<=>")
  nM _      (ECps (EEps c (Sign s _),EEps c' (Sign s' t'))) _ | s'==c' = (EEps c  (Sign s t'), [], "<=>")
  nM _      (ECps (EEps c (Sign s _),ECps(EEps c' (Sign _  t'),r))) _ | c ==c' = (ECps (EEps c  (Sign s t'),r), [], "<=>")
  nM _      (ECps (EEps c (Sign s t),ECps(EEps c' (Sign _  t'),r))) _ | c ==t  = (ECps (EEps c' (Sign s t'),r), [], "<=>")
  nM _      (ECps (EEps c (Sign s _),ECps(EEps c' (Sign s' t'),r))) _ | s'==c' = (ECps (EEps c  (Sign s t'),r), [], "<=>")
  nM _      (ECps (ERrs (x,e),y)) _ | not eq && isIdent e = (ERrs (x,y), ["Jipsen&Tsinakis: (x\\I);y |- x\\y"], "==>")
  nM _      (ECps (x,ELrs (e,y))) _ | not eq && isIdent e = (ELrs (x,y), ["Jipsen&Tsinakis: x;(I/y) |- x/y"], "==>")
  nM _      (ECps (ERrs (x,y),z)) _          | not eq = (ERrs (x,ECps (y,z)), ["Jipsen&Tsinakis: (x\\y);z |- x\\(y;z)"], "==>")
  nM _      (ECps (x,ELrs (y,z))) _          | not eq = (ERrs (x,ECps (y,z)), ["Jipsen&Tsinakis: x;(y/z) |- (x;y)/z"], "==>")
  nM _      (ECps (ERrs (x,y),ERrs (y',z))) _ | not eq && y==y' = (ERrs (x,z), ["Jipsen&Tsinakis: (x\\y);(y\\z) |- x\\z"], "==>")
  nM _      (ECps (ELrs (x,y),ELrs (y',z))) _ | not eq && y==y' = (ERrs (x,z), ["Jipsen&Tsinakis: (x/y);(y/z) |- x/z"], "==>")
  nM _      (ECps (ERrs (x,y),ERrs (y',z))) _ | y==y' && x==y && x==z = (ERrs (x,z), ["Jipsen&Tsinakis: (x\\x);(x\\x) = x\\x"], "<=>")
  nM _      (ECps (ELrs (x,y),ELrs (y',z))) _ | y==y' && x==y && x==z = (ERrs (x,z), ["Jipsen&Tsinakis: (x/x);(x/x) = x/x"], "<=>")
  nM _      (ECps (x,ERrs (y,z))) _    | x==y && x==z = (x, ["Jipsen&Tsinakis: x;(x\\x) = x"], "<=>")
  nM _      (ECps (ELrs (x,y),z)) _    | x==z && y==z = (x, ["Jipsen&Tsinakis: (x/x);x = x"], "<=>")
  nM _      (ECps (l,r)) _                | isIdent l = (r, ["I;x = x"], "<=>")
  nM _      (ECps (l,r)) _                | isIdent r = (l, ["x;I = x"], "<=>")
  nM True   (ECps (r,ERad (s,q))) _          | not eq = ((r.:.s).!.q, ["Peirce: r;(s!q) |- (r;s)!q"],"==>")
  nM True   (ECps (ERad (r,s),q)) _          | not eq = (r.!.(s.:.q), ["Peirce: (r!s);q |- r!(s;q)"],"==>")
  nM True   (ECps (EIsc (r,s),q)) _          | not eq = ((r.:.q)./\.(s.:.q), ["distribute ; over /\\"],"==>")
  nM True   (ECps (r,EIsc (s,q))) _          | not eq = ((r.:.s)./\.(r.:.q), ["distribute ; over /\\"],"==>")
  nM _      (ECps (EUni (q,s),r)) _                   = ((q.:.r).\/.(s.:.r), ["distribute ; over \\/"],"<=>")
  nM _      (ECps (l,EUni (q,s))) _                   = ((l.:.q).\/.(l.:.s), ["distribute ; over \\/"],"<=>")
  nM _      x@(ECps (l@EFlp{},r)) _ | not eq && flp l==r && isInj l   = (EDcI (source x), ["r~;r |- I (r is univalent)"], "==>")
  nM _      x@(ECps (l,       r)) _ | not eq && l==flp r && isInj l   = (EDcI (source x), ["r;r~ |- I (r is injective)"], "==>")
  nM _      x@(ECps (l@EFlp{},r)) _ | flp l==r && isInj l && isTot l  = (EDcI (source x), ["r~;r=I because r is univalent and surjective"], "<=>")
  nM _      x@(ECps (l,       r)) _ | l==flp r && isInj l && isTot l  = (EDcI (source x), ["r;r~=I because r is injective and total"], "<=>")
  nM posCpl (ECps (l,r))           rs                 = (t .:. f, steps++steps', fEqu [equ',equ''])
                                                          where (t,steps, equ')  = nM posCpl l []
                                                                (f,steps',equ'') = nM posCpl r (l:rs)
  nM _      x@(EEps i sgn) _ | source sgn==i && i==target sgn = (EDcI i, ["source and target are equal to "++name i++", so "++showADL x++"="++showADL (EDcI i)], "<=>")
  nM _      (ELrs (ECps (x,y),z)) _ | not eq && y==z  = (x,     ["(x;y)/y |- x"], "==>")
  nM _      (ELrs (ECps (x,y),z)) _ | not eq && flp x==z= (flp y, [case (x, y) of
                                                                        (EFlp _, EFlp _) -> "(SJ) (x~;y~)/x |- y"
                                                                        (     _, EFlp _) -> "(SJ) (x;y~)/x~ |- y"
                                                                        (EFlp _,      _) -> "(SJ) (x~;y)/x |- y~"
                                                                        (     _,      _) -> "(SJ) (x;y)/x~ |- y~"], "==>")
  nM _      (ELrs (ELrs (x,z),y)) _                     = (ELrs (x,ECps (y,z)), ["Jipsen&Tsinakis: x/yz = (x/z)/y"], "<=>") -- note: sign (x/yz) == sign ((x/z)/y)
  nM posCpl (ELrs (l,r)) _                              = (t ./. f, steps++steps', fEqu [equ',equ''])
                                                          where (t,steps, equ')  = nM posCpl l []
                                                                (f,steps',equ'') = nM (not posCpl) r []
  nM _      (ERrs (y,ERrs (x,z))) _                     = (ERrs (ECps (x,y),z), ["Jipsen&Tsinakis: xy\\z = y\\(x\\z)"], "<=>")
  nM _      (ERrs (x,ECps (y,z))) _ | not eq && x==y    = (z,     ["x\\(x;y) |- y"], "==>")
  nM posCpl (ERrs (l,r)) _                              = (t .\. f, steps++steps', fEqu [equ',equ''])
                                                          where (t,steps, equ')  = nM (not posCpl) l []
                                                                (f,steps',equ'') = nM posCpl r []
  nM posCpl (EDia (l,r)) _                              = (t .<>. f, steps++steps', fEqu [equ',equ''])
                                                          where (t,steps, equ')  = nM posCpl l []
                                                                (f,steps',equ'') = nM posCpl r []
  nM _      (ERad (l,r)) _                   | isImin l = (r, ["-I!x = x"], "<=>")
  nM _      (ERad (l,r)) _                   | isImin r = (l, ["x!-I = x"], "<=>")
--     nM False  (ERad (ECps (r,s),q)) _            | not eq = (r.:.(s.!.q), ["Peirce: (r;s)!q |- r;(s!q)"],"==>")  -- SJ 20131124 TODO: check this rule. It is wrong!
--     nM False  (ERad (r,ECps (s,q))) _            | not eq = ((r.!.s).:.q, ["Peirce: (r!s);q |- r!(s;q)"],"==>")  -- SJ 20131124 TODO: check this rule. It is wrong!
  nM False  (ERad (EUni (r,s),q)) _            | not eq = ((r.!.q).\/.(s.!.q), ["distribute ! over \\/"],"==>")
  nM False  (ERad (r,EUni (s,q))) _            | not eq = ((r.!.s).\/.(r.!.q), ["distribute ! over \\/"],"==>")
  nM _      (ERad (EIsc (q,s),r)) _                     = ((q.!.r)./\.(s.!.r), ["distribute ! over /\\"],"<=>")
  nM _      (ERad (l,EIsc (q,s))) _                     = ((l.!.q)./\.(l.!.s), ["distribute ! over /\\"],"<=>")
  nM _      (ERad(ECpl l,r))      _                     = (flp l .\. r, [case l of EFlp{} -> "-l~!r = l\\r"; _ -> "-l!r = l~\\r"], "<=>")
  nM _      (ERad(l,ECpl r))      _                     = (l ./. flp r, [case r of EFlp{} -> "l!-r~ = l/r"; _ -> "l!-r = l/r~"], "<=>")
  nM posCpl (ERad (l,r))         rs                     = (t .!. f, steps++steps', fEqu [equ',equ''])
                                                              where (t,steps, equ')  = nM posCpl l []
                                                                    (f,steps',equ'') = nM posCpl r (l:rs)
  nM _      (EPrd (l,EPrd (_,r))) _                     = (l .*. r, ["eliminate middle in cartesian product"], "<=>")
  nM posCpl (EPrd (l,r)) _                              = (t .*. f, steps++steps', fEqu [equ',equ''])
                                                              where (t,steps, equ')  = nM posCpl l []
                                                                    (f,steps',equ'') = nM posCpl r []
  nM posCpl (EIsc (EUni (l,k),r)) _       | posCpl/=dnf = ((l./\.r) .\/. (k./\.r), ["distribute /\\ over \\/"],"<=>")
  nM posCpl (EIsc (l,EUni (k,r))) _       | posCpl/=dnf = ((l./\.k) .\/. (l./\.r), ["distribute /\\ over \\/"],"<=>")
  nM posCpl x@(EIsc (l,r)) rs
-- Absorb equals:    r/\r  -->  r
      | or [length cl>1 |cl<-absorbClasses]
           = ( case absorbClasses of [] -> fatal 243 "Going into foldr1 with empty absorbClasses"; _ -> foldr1 (./\.) [head cl | cl<-absorbClasses]
             , [shw e++" /\\ "++shw e++" = "++shw e | cl<-absorbClasses, length cl>1, let e=head cl]
             , "<=>"
             )
-- Absorb True:    r/\V  --> r
      | isTrue l                      = (r, ["V/\\x = x"], "<=>")
      | isTrue r                      = (l, ["x/\\V = x"], "<=>")
-- Inconsistency:    r/\-r   -->  False
      | not (null incons)
           = let i = head incons in (notCpl (EDcV (sign i)), [shw (notCpl i)++" /\\ "++shw i++" = V-"], "<=>")
-- Inconsistency:    x/\\V-  -->  False
      | isFalse l                     = (notCpl (EDcV (sign x)), ["-V/\\x = -V"], "<=>")
      | isFalse r                     = (notCpl (EDcV (sign x)), ["x/\\-V = -V"], "<=>")
-- Absorb if r is antisymmetric:    r/\r~ --> I
      | t/=l || f/=r
           = (t ./\. f, steps++steps', fEqu [equ',equ''])
      | not eq && or [length cl>1 |cl<-absorbAsy]
           = ( foldr1 (./\.) [if length cl>1 then EDcI (source e) else e | cl<-absorbAsy, let e=head cl]
             , [shw e++" /\\ "++shw (flp e)++" |- I, because"++shw e++" is antisymmetric" | cl<-absorbAsy, let e=head cl]
             , "==>"
             )
-- Absorb if r is antisymmetric and reflexive:    r/\r~ = I
      | or [length cl>1 |cl<-absorbAsyRfx]
           = ( foldr1 (./\.) [if length cl>1 then EDcI (source e) else e | cl<-absorbAsyRfx, let e=head cl]
             , [shw e++" /\\ "++shw (flp e)++" = I, because"++shw e++" is antisymmetric and reflexive" | cl<-absorbAsyRfx, let e=head cl]
             , "<=>"
             )
-- Absorb:    (x\\/y)/\\y  =  y
      | isEUni l && not (null absor0)
           = let t'=head absor0  in (r, ["absorb "++shw l++" because of "++shw t'++", using law  (x\\/y)/\\y = y"], "<=>")
      | isEUni r && not (null absor0')
           = let t'=head absor0' in (r, ["absorb "++shw r++" because of "++shw t'++", using law  (x\\/y)/\\x = x"], "<=>")
-- Absorb:    (x\\/-y)/\\y  =  x/\\y
      | isEUni l && not (null absor1)
           = ( case head absor1 of
                 (_,[]) -> r
                 (_,ts) -> foldr1 (.\/.) ts ./\. r
             , ["absorb "++shw t'++", using law (x\\/-y)/\\y  =  x/\\y" | (t',_)<-absor1]
             , "<=>"
             )
      | isEUni r && not (null absor1')
           = ( case head absor1' of
                 (_,[]) -> l
                 (_,ts) -> l ./\. foldr1 (.\/.) ts
             , ["absorb "++shw t'++", using law x/\\(y\\/-x)  =  x/\\y" | (t',_)<-absor1']
             , "<=>"
             )
-- Avoid complements: x/\\-y = x-y
      | (not.null) negList && (not.null) posList
           = ( foldl (.-.) (foldr1 (./\.) posList) (map notCpl negList)
             , [ "Avoid complements, using law x/\\-y = x-y" ]
             , "<=>"
             )
      | otherwise = (t ./\. f, steps++steps', fEqu [equ',equ''])
      where (t,steps, equ')  = nM posCpl l []
            (f,steps',equ'') = nM posCpl r (l:rs)
            absorbClasses = eqClass (==) (exprIsc2list l++exprIsc2list r)
            incons = [conjunct |conjunct<-exprIsc2list r,conjunct==notCpl l]
            absor0  = [disjunct | disjunct<-exprUni2list l, f'<-rs++exprIsc2list r, disjunct==f']
            absor0' = [disjunct | disjunct<-exprUni2list r, f'<-rs++exprIsc2list l, disjunct==f']
            absor1  = [(disjunct, exprUni2list l>-[disjunct]) | disjunct<-exprUni2list l, ECpl f'<-rs++exprIsc2list r, disjunct==f']++
                      [(disjunct, exprUni2list l>-[disjunct]) | disjunct@(ECpl t')<-exprUni2list l, f'<-rs++exprIsc2list r, t'==f']
            absor1' = [(disjunct, exprUni2list r>-[disjunct]) | disjunct<-exprUni2list r, ECpl f'<-rs++exprIsc2list l, disjunct==f']++
                      [(disjunct, exprUni2list r>-[disjunct]) | disjunct@(ECpl t')<-exprUni2list r, f'<-rs++exprIsc2list l, t'==f']
            absorbAsy = eqClass same eList where e `same` e' = isAsy e && isAsy e' && e == flp e'
            absorbAsyRfx = eqClass same eList where e `same` e' = isRfx e && isAsy e && isRfx e' && isAsy e' && e == flp e'
            (negList,posList) = partition isNeg (exprIsc2list l++exprIsc2list r)
            eList  = rs++exprIsc2list l++exprIsc2list r
  nM posCpl (EUni (EIsc (l,k),r)) _  | posCpl==dnf    = ((l.\/.r) ./\. (k.\/.r), ["distribute \\/ over /\\"],"<=>")
  nM posCpl (EUni (l,EIsc (k,r))) _  | posCpl==dnf    = ((l.\/.k) ./\. (l.\/.r), ["distribute \\/ over /\\"],"<=>")
  nM posCpl (EUni (ECpl x,r@(ELrs (z,y)))) _          = if sign x==sign z -- necessary to guarantee that sign expr is equal to sign of the result
                                                        then (notCpl (x .:. y) .\/. z, ["remove left residual (/)"],"<=>")
                                                        else (notCpl t .\/. f, steps++steps', fEqu [equ',equ''])
                                                           where (t,steps, equ')  = nM (not posCpl) x []
                                                                 (f,steps',equ'') = nM posCpl r []
  nM posCpl (EUni (l@(ELrs (z,y)),ECpl x)) _          = if sign x==sign z -- necessary to guarantee that sign expr is equal to sign of the result
                                                        then (notCpl (x .:. y) .\/. z, ["remove left residual (/)"],"<=>")
                                                        else (notCpl t .\/. f, steps++steps', fEqu [equ',equ''])
                                                           where (t,steps, equ')  = nM (not posCpl) x []
                                                                 (f,steps',equ'') = nM posCpl l []
  nM posCpl (EUni (l@(ERrs (x,z)),ECpl y)) _          = if sign y==sign z -- necessary to guarantee that sign expr is equal to sign of the result
                                                        then (notCpl (x .:. y) .\/. z, ["remove right residual (\\)"],"<=>")
                                                        else (notCpl t .\/. f, steps++steps', fEqu [equ',equ''])
                                                           where (t,steps, equ')  = nM (not posCpl) y []
                                                                 (f,steps',equ'') = nM posCpl l []
  nM posCpl (EUni (ECpl y,r@(ERrs (x,z)))) _          = if sign y==sign z -- necessary to guarantee that sign expr is equal to sign of the result
                                                        then (notCpl (x .:. y) .\/. z, ["remove right residual (\\)"],"<=>")
                                                        else (notCpl t .\/. f, steps++steps', fEqu [equ',equ''])
                                                           where (t,steps, equ')  = nM (not posCpl) y []
                                                                 (f,steps',equ'') = nM posCpl r []
  nM posCpl x@(EUni (l,r)) rs
-- Absorb equals:    r\/r  -->  r
      | t/=l || f/=r
           = (t .\/. f, steps++steps', fEqu [equ',equ''])
      | or [length cl>1 |cl<-absorbClasses]   -- yields False if absorbClasses is empty
           = ( foldr1 (.\/.) [head cl | cl<-absorbClasses]  -- cl cannot be empty, because it is made by eqClass
             , [shw e++" \\/ "++shw e++" = "++shw e | cl<-absorbClasses, length cl>1, let e=head cl]
             , "<=>"
             )
-- Tautologies:
      | (not.null) tauts               = (EDcV (sign x), ["let e = "++ shw (head tauts)++". Since -e\\/e = V we get"], "<=>")   -- r\/-r  -->  V
      | isTrue l                       = (EDcV (sign x), ["V\\/x = V"], "<=>")                                                  -- r\/V   -->  V
      | isTrue r                       = (EDcV (sign x), ["x\\/V = V"], "<=>")
-- Absorb -V:    r\/-V  --> r
      | isFalse l                      = (r, ["-V\\/x = x"], "<=>")
      | isFalse r                      = (l, ["x\\/-V = x"], "<=>")
-- Absorb:    (x/\\y)\\/y  =  y
      | isEIsc l && not (null absor0)  = let t'=head absor0  in (r, ["absorb "++shw l++" because of "++shw t'++", using law  (x/\\y)\\/y = y"], "<=>")
      | isEIsc r && not (null absor0') = let t'=head absor0' in (r, ["absorb "++shw r++" because of "++shw t'++", using law  (x/\\y)\\/x = x"], "<=>")
-- Absorb:    (x/\\-y)\\/y  =  x\\/y
      | isEIsc l && not (null absor1)
           = ( case head absor1 of
                 (_,[]) -> r
                 (_,ts) -> foldr1 (./\.) ts .\/. r
             , ["absorb "++shw t'++", using law (x/\\-y)\\/y  =  x\\/y" | (t',_)<-absor1]
             , "<=>"
             )
      | isEIsc r && not (null absor1')
           = ( case head absor1' of
                 (_,[]) -> l
                 (_,ts) -> l .\/. foldr1 (./\.) ts
             , ["absorb "++shw t'++", using law x\\/(y/\\-x)  =  x\\/y" | (t',_)<-absor1' ]
             , "<=>"
             )
      | otherwise = (t .\/. f, steps++steps', fEqu [equ',equ''])
      where (t,steps, equ')  = nM posCpl l []
            (f,steps',equ'') = nM posCpl r (l:rs)
         -- absorption can take place if two terms are equal. So let us make a list of equal terms: absorbClasses (for substituting r\/r by r)
            absorbClasses = eqClass (==) (exprUni2list l++exprUni2list r)
         -- tautologies occur if -r\/r, so we are looking for pairs, (x,l) such that x== -l
            tauts = [t' |disjunct<-exprUni2list r,disjunct==notCpl l, ECpl t'<-[disjunct,l]]
            absor0  = [t' | t'<-exprIsc2list l, f'<-rs++exprUni2list r, t'==f']
            absor0' = [t' | t'<-exprIsc2list r, f'<-rs++exprUni2list l, t'==f']
            absor1  = [(t', exprIsc2list l>-[t']) | t'<-exprIsc2list l, ECpl f'<-rs++exprUni2list r, t'==f']++[(e, exprIsc2list l>-[e]) | e@(ECpl t')<-exprIsc2list l, f'<-rs++exprUni2list r, t'==f']
            absor1' = [(t', exprIsc2list r>-[t']) | t'<-exprIsc2list r, ECpl f'<-rs++exprUni2list l, t'==f']++[(e, exprIsc2list r>-[e]) | e@(ECpl t')<-exprIsc2list r, f'<-rs++exprUni2list l, t'==f']
  nM _ (EFlp e) _ | isSym e =  (e,[shw e++" is symmetric"],"<=>")
  nM _ x _               = (x,[],"<=>")

fEqu :: [String] -> String
fEqu ss = if and [s=="<=>" | s<-ss] then "<=>" else "==>"
{-
   nfProof :: (Expression -> String) -> Expression -> Proof Expression
   nfProof shw = nfPr shw True True -- The first boolean True means that clauses are derived using <=> derivations. The second True means that a disjunctive normal form is produced.
-}

nfPr :: (Expression -> String) -> Bool -> Bool -> Expression -> [(Expression, [String], String)]
nfPr shw eq dnf expr
 = {-if showADL expr=="r \\/ s"
   then fatal 360 ("Diagnose expr: "++showADL expr++"\n"++
                   "eq:            "++show eq++"\n"++
                   "dnf:           "++show eq++"\n"++
                   "res:           "++showADL res++"\n"++
                   "expr==res:     "++show (expr==res)
                  ) else-}
   if expr==res
   then [(expr,[],"<=>")]
   else (expr,steps,equ):nfPr shw eq dnf (simplify res)
 where (res,steps,equ) = normStep shw eq dnf False expr

conjNF, disjNF :: Options -> Expression -> Expression
(conjNF, disjNF) = (pr False, pr True)
 where pr dnf opts expr
        = case oldNormalizer opts of
           False -> let rterm = expr2RTerm expr
                    in (rTerm2expr.last.((:) (rterm)).map (rhs.snd).slideDown (weightNF dnf)) rterm
           True  -> let proof = if dnf then dfProof opts else cfProof opts
                        (e,_,_) = if null (proof expr) then fatal 340 "last: empty list" else last (proof expr)
                    in e

cfProof, dfProof :: Options -> Expression -> Proof Expression
(cfProof,dfProof) = (proof False, proof True)
 where
   proof :: Bool -> Options -> Expression -> Proof Expression
   proof dnf opts expr
    = case oldNormalizer opts of
       False -> [ (rTerm2expr term, explStr, logicSym) | (term, explStr, logicSym)<-prRT (expr2RTerm expr) ]
       True  -> [line | step, line<-init pr]++
                [line | step', line<-init pr']++
                [last ([(expr,[],"<=>")]++
                       [line | step, line<-pr]++
                       [line | step', line<-pr']
                      )]
      where
        prRT :: RTerm -> [(RTerm, [String], String)]
        prRT term
           = case slideDown (weightNF dnf) term of
               [] -> [ (term, ["weight: "++show (weightNF dnf term)], "<=>") ]
               ds -> [ (lhs d, [" weight: "++show w++",   "++showADL tmpl++" = "++showADL stp++"  with unifier: "++showADL unif | let (tmpl,unif,stp) = rul d], "<=>")
                     | (w,d)<-ds ] ++
                     [ (rhs d, [], "<=>")
                     | let (_,d) = last ds ]
        pr           = nfPr showADL True dnf expr
        (expr',_,_)  = if null pr then fatal 356 "last: empty list" else last pr
        step         = simplify expr/=simplify expr'
        pr'          = nfPr showADL True dnf expr'
        step'        = simplify expr'/=simplify expr''
        (expr'',_,_) = if null pr' then fatal 365 "last: empty list" else last pr'

{-
   cfProof :: Expression -> Proof Expression
   cfProof expr
    = [line | step, line<-init pr]++
      [line | step', line<-init pr']++
      [last ([(expr,[],"<=>")]++
             [line | step, line<-pr]++
             [line | step', line<-pr']
            )]
      where pr           = nfPr showADL True False (simplify expr)
            (expr',_,_)  = if null pr then fatal 328 "last: empty list" else last pr
            step         = simplify expr/=expr' -- obsolete?    || and [null s | (_,ss,_)<-pr, s<-ss]
            pr'          = nfPr showADL True False (simplify expr')
            step'        = simplify expr'/=simplify expr'' -- obsolete?    || and [null s | (_,ss,_)<-pr', s<-ss]
            (expr'',_,_) = if null pr' then fatal 337 "last: empty list" else last pr'

   dfProof :: Expression -> Proof Expression
   dfProof expr
    = [line | step, line<-init pr]++
      [line | step', line<-init pr']++
      [last ([(expr,[],"<=>")]++
             [line | step, line<-pr]++
             [line | step', line<-pr']
            )]
      where pr           = nfPr showADL True True expr
            (expr',_,_)  = if null pr then fatal 356 "last: empty list" else last pr
            step         = simplify expr/=simplify expr'
            pr'          = nfPr showADL True True expr'
            step'        = simplify expr'/=simplify expr''
            (expr'',_,_) = if null pr' then fatal 365 "last: empty list" else last pr'
-}

isEUni :: Expression -> Bool
isEUni EUni{}  = True
isEUni _       = False

isEIsc :: Expression -> Bool
isEIsc EIsc{}  = True
isEIsc _       = False





conjuncts :: Options -> Rule -> [Expression]
conjuncts opts = exprIsc2list.conjNF opts.rrexp

allShifts :: Options -> DnfClause -> [DnfClause]
allShifts opts conjunct =  (map head.eqClass (==).filter pnEq.map normDNF) (shiftL conjunct++shiftR conjunct)  -- we want to nub all dnf-clauses, but nub itself does not do the trick...
-- allShifts conjunct = error $ show conjunct++concat [ "\n"++show e'| e'<-shiftL conjunct++shiftR conjunct] -- for debugging
 where
 {-
  diagnostic
   = intercalate "\n  "
       [ "shiftL: [ "++intercalate "\n          , " [showHS opts "\n            " e | e<-shiftL conjunct    ]++"\n          ]"
       , "shiftR: [ "++intercalate "\n          , " [showHS opts "\n            " e | e<-shiftR conjunct    ]++"\n          ]"
       ] -}
  shiftL :: DnfClause -> [DnfClause]
  shiftL dc
   | null (antcs dc)|| null (conss dc) = [dc] --  shiftL doesn't work here. This is just to make sure that both antss and conss are really not empty
   | otherwise = [ Dnf { antcs = ass
                       , conss = case css of
                                   [] -> let antcExpr = foldr1 (./\.) ass in
                                         if isEndo antcExpr then [EDcI (source antcExpr)] else fatal 425 "antcExpr should be endorelation"
                                   _  -> css
                       }
                 | (ass,css)<-nub (move (antcs dc) (conss dc))
                 ]
   where
   -- example:  r;s /\ p;r |- x;y   and suppose x and y are both univalent.
   --  antcs =  [ r;s, p;r ]
   --  conss =  [ x;y ]
    move :: [Expression] -> [Expression] -> [([Expression],[Expression])]
    move ass [] = [(ass,[])]
    move ass css
     = (ass,css):
       if and [ (not.isEDcI) cs | cs<-css]     -- all cs are nonempty because: (not.and.map isEDcI) cs ==> not (null cs)
       then [ts | let headEs = map headECps css
                , length (eqClass (==) headEs) == 1                    -- example: True, because map head css == [ "x" ]
                , let h=head headEs                                    -- example: h= "x"
                , isUni h                                              -- example: assume True
                , ts<-move [if source h==source as then flp h.:.as else fatal 455 "type mismatch"
                           |as<-ass] (map tailECps css)]++ -- example: ts<-move [ [flp "x","r","s"], [flp "x","p","r"] ]  [ ["y","z"] ]
            [ts | let lastEs = map lastECps css
                , length (eqClass (==) lastEs) == 1
                , let l=head lastEs
                , isInj l
                , ts<-move [if target as==target l then as.:.flp l else fatal 461 "type mismatch"
                           |as<-ass] (map initECps css)]   -- example: ts<-move [ ["r","s",flp "z"], ["p","r",flp "z"] ]  [ ["x","y"] ]
       else []
   -- Here is (informally) what the example does:
   -- move [ r;s , p;r ] [ x;y ]
   -- ( [ r;s , p;r ] , [ x;y ] ): [ ts | ts<-move [flp x.:.as | as<-[ r;s , p;r ] [ y ] ] ]
   -- ( [ r;s , p;r ] , [ x;y ] ): ( [ x~;r;s , x~;p;r ] , [ y ] ): [ ts | ts<-move [flp y.:.as | as<-[ y~;x~;r;s , y~;x~;p;r ] [] ] ]
   -- ( [ r;s , p;r ] , [ x;y ] ): ( [ x~;r;s , x~;p;r ] , [ y ] ): [ [ y~;x~;r;s , y~;x~;p;r ] , [] ] ]
   -- [ ( [ r;s , p;r ] , [ x;y ] ), ( [ x~;r;s , x~;p;r ] , [ y ] ), ( [ y~;x~;r;s , y~;x~;p;r ] , [] ) ]

  shiftR :: DnfClause -> [DnfClause]
  shiftR dc
   | null (antcs dc) || null (conss dc) = [dc] --  shiftR doesn't work here. This is just to make sure that both antss and conss are really not empty
   | otherwise                = [ Dnf (case ass of
                                        [] -> let consExpr = foldr1 (.\/.) css in
                                              if source consExpr==target consExpr then [EDcI (source consExpr)] else fatal 463 "consExpr should be endorelation"
                                        _  -> ass
                                      ) css
                                | (ass,css)<-nub (move (antcs dc) (conss dc))
                                ]
   where
   -- example  "r;s /\ r;r |- x;y"   and suppose r is both surjective.
   --  ass =  [ r;s , r;r ]
   --  css =  [ x;y ]
    move :: [Expression] -> [Expression] -> [([Expression],[Expression])]
    move ass css =
     case ass of
      [] -> [] -- was [([EDcI (target (last css))],css)]
      _  ->
       (ass,css):
       if and [ (not.isEDcI) as | as<-ass]
       then [ts | let headEs = map headECps ass
                , length (eqClass (==) headEs) == 1                      -- example: True, because map headECps ass == [ "r", "r" ]
                , let h=head headEs                                      -- example: h= "r"
                , isSur h                                                -- example: assume True
                , ts<-move (map tailECps ass) [if source h==source cs then flp h.:.cs else fatal 496 "type mismatch"
                                              |cs<-css]]++   -- example: ts<-move  [["s"], ["r"]] [ [flp "r","x","y","z"] ]
            [ts | let lastEs = map lastECps ass
                , length (eqClass (==) lastEs) == 1                      -- example: False, because map lastECps ass == [ ["s"], ["r"] ]
                , let l=head lastEs
                , isTot l
                , ts<-move (map initECps ass) [if target cs==target l then cs.:.flp l else fatal 502 "type mismatch"
                                              |cs<-css]]     -- is dit goed? cs.:.flp l wordt links zwaar, terwijl de normalisator rechts zwaar maakt.
       else []
   -- Here is (informally) what the example does:
   -- move [ r;s , r;r ] [ x;y ]
   -- ( [ r;s , r;r ] , [ x;y ] ): move [ s , r ] [ r~;x;y ]
   -- ( [ r;s , r;r ] , [ x;y ] ): ( [ s , r ]  , [ r~;x;y ] ) : []
   -- [ [ r;s , r;r ] , [ x;y ] ), ( [ s , r ]  , [ r~;x;y ] ) ]
   --  diagnostic
   --    = "\n  antcs: [ "++intercalate "\n         , " [showADL a | a<-antcs ]++"\n       ]"++
   --      "\n  conss: [ "++intercalate "\n         , " [showADL c | c<-conss ]++"\n       ]"++
   --      "\n  move:  [ "++intercalate "\n         , " ["("++sh " /\\ " as++"\n           ,"++sh " \\/ " cs++")" | (as,cs)<-move antcs conss ]++"\n       ]"
   --  sh :: String -> [Expression] -> String
   --  sh str es = intercalate str [ showADL e | e<-es]

  normDNF :: DnfClause -> DnfClause
  normDNF dc = 
    Dnf { antcs = case antcs dc of
                   [] -> []
                   _  -> (exprIsc2list . conjNF opts . foldr1 (./\.)) (antcs dc)
        , conss = case conss dc of
                   [] -> []
                   _  -> (exprUni2list . disjNF opts . foldr1 (.\/.)) (conss dc)
        }

  pnEq :: DnfClause -> Bool
  pnEq dc = antcs dc /= conss dc

  headECps :: Expression -> Expression
  headECps expr = f expr
   where f (ECps (l@ECps{},_)) = f l
         f (ECps (l,_)) = l
         f _ = expr

  tailECps :: Expression -> Expression
  tailECps expr = f expr
   where f (ECps (ECps (l,r),q)) = f (ECps (l, ECps (r,q)))
         f (ECps (_,r)) = r
         f _ = EDcI (target expr)

  initECps :: Expression -> Expression
  initECps expr = f expr
   where f (ECps (l, ECps (r,q))) = initECps (ECps (ECps (l,r),q))
         f (ECps (l,_)) = l
         f _ = EDcI (source expr)

  lastECps :: Expression -> Expression
  lastECps expr = f expr
   where f (ECps (_,r@ECps{})) = f r
         f (ECps (_,r)) = r
         f _ = expr

  isEDcI :: Expression -> Bool
  isEDcI EDcI{} = True
  isEDcI _ = False


makeAllConjs :: Options -> [Rule] -> [Conjunct]
makeAllConjs opts allRls =
  let conjExprs :: [(Expression, [Rule])]
      conjExprs = converse [ (rule, conjuncts opts rule) | rule <- allRls ]
      
      conjs = [ Cjct { rc_id = "conj_"++show (i :: Int)
                     , rc_orgRules   = rs
                     , rc_conjunct   = expr
                     , rc_dnfClauses = allShifts opts (expr2dnfClause expr)
                     }
              | ((expr, rs),i) <- zip conjExprs [0..]
              ]
  in  conjs
   where
      expr2dnfClause :: Expression -> DnfClause
      expr2dnfClause conj = (split (Dnf [] []).exprUni2list) conj
       where
         split :: DnfClause -> [Expression] -> DnfClause
         split (Dnf antc cons) (ECpl e: rest) = split (Dnf (e:antc) cons) rest
         split (Dnf antc cons) (     e: rest) = split (Dnf antc (e:cons)) rest
         split dc              []             = dc

