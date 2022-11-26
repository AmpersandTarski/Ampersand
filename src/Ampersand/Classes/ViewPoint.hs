module Ampersand.Classes.ViewPoint (Language (..), enforce2Rules, ruleFromIdentity) where

import Ampersand.ADL1
import Ampersand.Basics hiding (Identity, Ord (..))
import Ampersand.Classes.Relational (HasProps (properties))
import Data.Text1 ((.<>))
import Data.Text1.Text1 ((<>.))
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set

-- Language exists because there are many data structures that behave like an ontology, such as Pattern, P_Context, and Rule.
-- These data structures are accessed by means of a common set of functions (e.g. rules, relations, etc.)

class Language a where
  relsDefdIn ::
    a ->
    -- | all relations that are declared in the scope of this viewpoint.
    --   These are user defined relations and all generated relarations,
    --   i.e. one relation for each GEN and one for each signal rule.
    --   Don't confuse relsDefdIn with bindedRelationsIn, which gives the relations that are
    --   used in a.)
    Relations
  udefrules ::
    a ->
    -- | all user defined rules that are maintained within this viewpoint,
    --   which are not property-, enforce- and not identity rules.
    Rules
  proprules ::
    a ->
    -- | all property rules that are maintained within this viewpoint.
    Rules
  proprules x =
    Set.fromList $
      [rulefromProp p d | d <- Set.elems $ relsDefdIn x, p <- Set.elems (properties d)]
  identityRules :: a -> Rules -- all identity rules that are maintained within this viewpoint.
  identityRules x = Set.fromList . map ruleFromIdentity $ identities x
  enforceRules :: a -> Rules -- all enforce rules that are maintained within this viewpoint.
  enforceRules x = Set.fromList . concatMap enforce2Rules . enforces $ x
  allRules :: a -> Rules
  allRules x = udefrules x `Set.union` proprules x `Set.union` identityRules x `Set.union` enforceRules x
  identities ::
    a ->
    -- | all keys that are defined in a
    [IdentityRule]
  viewDefs ::
    a ->
    -- | all views that are defined in a
    [ViewDef]
  enforces :: a -> [AEnforce] -- all Enforce statements that are defined in a
  gens ::
    a ->
    -- | all generalizations that are valid within this viewpoint
    [AClassify]
  patterns ::
    a ->
    -- | all patterns that are used in this viewpoint
    [Pattern]
  udefRoleRules ::
    a ->
    -- | all user defined RoleRules that are maintained within this viewpoint
    [A_RoleRule]
  allRoleRules :: a -> [A_RoleRule]
  allRoleRules x = udefRoleRules x <> (concatMap roleRuleFromEnforceRule . enforces $ x)

ruleFromIdentity :: IdentityRule -> Rule
ruleFromIdentity identity =
  mkKeyRule $
    foldr (./\.) h t
      .|-. EDcI (idCpt identity)
  where
    {-    diamond e1 e2 = (flp e1 .\. e2) ./\. (e1 ./. flp e2)  -}
    (h NE.:| t) =
      fmap ((\expr -> expr .:. flp expr) . objExpression . segment)
        . identityAts
        $ identity
    mkKeyRule term =
      Rule
        { rrnm =
            toName
              (nameSpaceOf identity)
              ("identity_" .<> tName identity),
          formalExpression = term,
          rrfps = origin identity, -- position in source file
          rrmean = map toMeaning [minBound ..],
          rrmsg = [],
          rrviol = Nothing,
          rrpat = idPat identity,
          rrkind = Identity (idCpt identity) -- This rule was not specified as a rule in the Ampersand script, but has been generated by a computer
        }
      where
        toMeaning lang =
          Meaning . Markup lang . string2Blocks ReST $
            case lang of
              English -> "Identity rule, following from identity " <> (text1ToText . tName) identity
              Dutch -> "Identiteitsregel, volgend uit identiteit " <> (text1ToText . tName) identity

instance (Eq a, Language a) => Language [a] where
  relsDefdIn = Set.unions . map relsDefdIn
  udefrules = Set.unions . map udefrules
  identities = concatMap identities
  viewDefs = concatMap viewDefs
  enforces = concatMap enforces
  gens = L.nub . concatMap gens
  patterns = concatMap patterns
  udefRoleRules = concatMap udefRoleRules

instance (Eq a, Language a) => Language (Set.Set a) where
  relsDefdIn = Set.unions . map relsDefdIn . Set.elems
  udefrules = Set.unions . map udefrules . Set.elems
  identities = L.nub . concatMap identities . Set.elems
  viewDefs = L.nub . concatMap viewDefs . Set.elems
  enforces = L.nub . concatMap enforces . Set.elems
  gens = L.nub . concatMap gens . Set.elems
  patterns = L.nub . concatMap patterns . Set.elems
  udefRoleRules = L.nub . concatMap udefRoleRules . Set.elems

instance Language A_Context where
  relsDefdIn context =
    uniteRels
      ( relsDefdIn (patterns context)
          `Set.union` ctxds context
      )
    where
      -- relations with the same name, but different properties (decprps,pragma,etc.) may exist and need to be united
      -- decprps are united, all others are taken from the head.
      uniteRels :: Relations -> Relations
      uniteRels ds =
        Set.fromList
          . map fun
          . eqClass (==)
          $ Set.elems ds
        where
          fun :: NE.NonEmpty Relation -> Relation
          fun rels =
            (NE.head rels)
              { decprps = Set.unions . fmap decprps $ rels
              }
  udefrules context = (Set.unions . map udefrules $ ctxpats context) `Set.union` ctxrs context
  identities context = concatMap identities (ctxpats context) <> ctxks context
  viewDefs context = concatMap viewDefs (ctxpats context) <> ctxvs context
  enforces context = concatMap enforces (ctxpats context) <> ctxEnforces context
  gens context = L.nub $ concatMap gens (ctxpats context) <> ctxgs context
  patterns = ctxpats
  udefRoleRules context = concatMap udefRoleRules (ctxpats context) <> ctxrrules context

instance Language Pattern where
  relsDefdIn = ptdcs
  udefrules = ptrls -- all user defined rules in this pattern
  identities = ptids
  viewDefs = ptvds
  enforces = ptenfs
  gens = ptgns
  patterns pat = [pat]
  udefRoleRules = ptrrs

roleRuleFromEnforceRule :: AEnforce -> [A_RoleRule]
roleRuleFromEnforceRule = map mkRoleRule . enforce2Rules
  where
    mkRoleRule rul =
      A_RoleRule
        { arPos = origin rul,
          arRoles = Role nameOfExecEngineRole NE.:| [],
          arRules = name rul NE.:| []
        }

enforce2Rules :: AEnforce -> [Rule]
enforce2Rules (AEnforce orig rel op expr mPat) =
  case op of
    IsSuperSet {} -> [insPair]
    IsSubSet {} -> [delPair]
    IsSameSet {} -> [insPair, delPair]
  where
    insPair = mkRule "InsPair" (EInc (expr, bindedRel))
    delPair = mkRule "DelPair" (EInc (bindedRel, expr))
    bindedRel = EDcD rel
    mkRule :: Text -> Expression -> Rule
    mkRule command fExpr =
      Rule
        { rrnm =
            toName
              (nameSpaceOf rel)
              ("Compute " .<> showRel rel <>. " using " <> command),
          formalExpression = fExpr,
          rrfps = orig,
          rrmean = [],
          rrmsg = [],
          rrviol =
            Just . PairView $
              PairViewText orig ("{EX} " <> command <> ";")
                NE.:| [ PairViewText orig $ (text1ToText . tName) rel <> ";" <> (text1ToText . tName) (source rel) <> ";",
                        PairViewExp orig Src (EDcI (source rel)),
                        PairViewText orig $ ";" <> (text1ToText . tName) (target rel) <> ";",
                        PairViewExp orig Tgt (EDcI (target rel))
                      ],
          rrpat = mPat,
          rrkind = Enforce
        }
