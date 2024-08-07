{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Ampersand.FSpec.Instances
  ( Instances (..),
  )
where

import Ampersand.ADL1
import Ampersand.Basics hiding (first, second)
import Ampersand.Classes
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.Motivations
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set

-- | Within a specific context there are all kinds of things.
--   These 'things' are instances (elements / atoms) of some
--   Concept. They are the atoms of the concepts, as looked
--   upon from the Formal Ampersand viewpoint.
class (Typeable a) => Instances a where
  instances :: FSpec -> Set.Set a
  instanceList :: FSpec -> [a]
  instanceList = Set.toList . instances
  {-# MINIMAL instances #-}

instance Instances A_Context where
  instances = maybe mempty Set.singleton . originalContext

instance Instances AClassify where
  instances = maybe mempty (Set.fromList . gens) . originalContext

instance Instances A_Concept where
  instances = concs . originalContext

instance Instances AConceptDef where
  instances = conceptDefInstances

instance Instances BoxItem where
  instances =
    Set.fromList
      . concatMap siObjs
      . filter isBox
      . Set.toList
      . subInterfaceInstances
    where
      isBox Box {} = True
      isBox _ = False

instance Instances Conjunct where
  instances = Set.fromList . allConjuncts

instance Instances Expression where
  instances = expressionInstances

instance Instances IdentityRule where
  instances = maybe mempty (Set.fromList . ctxks) . originalContext

instance Instances Rule where
  instances = maybe mempty allRules . originalContext -- This contains all rules declared inside a context but outside the patterns it contains.

instance Instances Interface where
  instances = interfaceInstances

instance Instances SubInterface where
  instances = subInterfaceInstances

-- instance Instances Meaning where
--  instances = meaningInstances
instance Instances Markup where
  instances fSpec =
    (Set.fromList . map explMarkup . Set.toList . purposeInstances $ fSpec)
      `Set.union` (Set.fromList . map ameaMrk . Set.toList . meaningInstances $ fSpec)

instance Instances ObjectDef where
  instances = objectDefInstances

instance Instances Pattern where
  instances = Set.fromList . vpatterns

instance Instances Population where
  instances = maybe mempty (Set.fromList . ctxpopus) . originalContext

instance Instances Purpose where
  instances = purposeInstances

instance Instances Relation where
  instances = relationInstances

instance Instances Role where
  instances = Set.fromList . map fst . fRoles

instance Instances A_RoleRule where
  instances = maybe mempty (Set.fromList . ctxrrules) . originalContext

instance Instances Signature where
  instances fSpec =
    (Set.fromList . map sign . Set.toList . relationInstances $ fSpec)
      `Set.union` (Set.fromList . map sign . Set.toList . expressionInstances $ fSpec)

instance Instances ViewDef where
  instances = maybe mempty (Set.fromList . viewDefs) . originalContext

instance Instances Meaning where
  instances = meaningInstances

instance Instances (PairView Expression) where
  instances = pairViewInstances

instance Instances (PairViewSegment Expression) where
  instances = Set.fromList . concatMap (NE.toList . ppv_segs) . instanceList

-- Set.toList . purposeInstances

-- --WARNING: Beware of loops!
-- To prevent loops in the definition of instances, it is considered bad
-- to use the `instances` function while defining it.
-- For this reason, some helper functions are defined here:
expressionInstances :: FSpec -> Set.Set Expression
expressionInstances = allExprs

interfaceInstances :: FSpec -> Set.Set Interface
interfaceInstances = maybe mempty (Set.fromList . ctxifcs) . originalContext

subInterfaceInstances :: FSpec -> Set.Set SubInterface
subInterfaceInstances = Set.fromList . mapMaybe objmsub . Set.toList . objectDefInstances

objectDefInstances :: FSpec -> Set.Set ObjectDef
objectDefInstances =
  Set.fromList
    . concatMap (objects . ifcObj)
    . interfaceInstances
  where
    objects :: ObjectDef -> [ObjectDef]
    objects obj = obj : fieldsRecursive obj

meaningInstances :: FSpec -> Set.Set Meaning
meaningInstances fSpec =
  (Set.fromList . concatMap meanings . Set.toList . relationInstances $ fSpec)
    `Set.union` (Set.fromList . concatMap meanings . Set.toList . ruleInstances $ fSpec)
    `Set.union` (Set.fromList . concatMap meanings . Set.toList . conceptDefInstances $ fSpec)

pairViewInstances :: FSpec -> Set.Set (PairView Expression)
pairViewInstances = Set.fromList . mapMaybe rrviol . Set.toList . ruleInstances

purposeInstances :: FSpec -> Set.Set Purpose
purposeInstances = fSexpls

relationInstances :: FSpec -> Set.Set Relation
relationInstances = maybe mempty relsDefdIn . originalContext

ruleInstances :: FSpec -> Set.Set Rule
ruleInstances = maybe mempty allRules . originalContext

conceptDefInstances :: FSpec -> Set.Set AConceptDef
conceptDefInstances = maybe mempty (Set.fromList . ctxcds) . originalContext
