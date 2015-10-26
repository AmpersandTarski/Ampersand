module Database.Design.Ampersand.Output.Statistics (Statistics(..)) where

import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.FSpec.FPA
import Database.Design.Ampersand.Basics (fatal)

-- TODO Deze module moet nog verder worden ingekleurd...

class Statistics a where
  nInterfaces :: a -> Int      -- ^ The number of interfaces in a
  nPatterns :: a -> Int      -- ^ The number of patterns in a
  nFpoints :: a -> Int      -- ^ The number of function points in a

instance Statistics a => Statistics [a] where
  nInterfaces xs = sum (map nInterfaces xs)
  nPatterns   xs = sum (map nPatterns xs)
  nFpoints    xs = sum (map nFpoints xs)

instance Statistics FSpec where
  nInterfaces fSpec = length (fActivities fSpec) --TODO -> check correctness
  nPatterns   fSpec = nPatterns (vpatterns fSpec)
  nFpoints    fSpec = sum [nFpoints ifc | ifc <- (interfaceS fSpec++interfaceG fSpec)]
                --       + sum [fPoints (fpa plug) | InternalPlug plug <- plugInfos fSpec]
-- TODO Deze module moet nog verder worden ingekleurd...

instance Statistics Pattern where
  nInterfaces _ = 0 --TODO -> check correctness
  nPatterns   _ = 1
  nFpoints   _  = fatal 43 "function points are not defined for patterns at all."

--   instance Statistics Activity where
--    nInterfaces _ = 1
--    nPatterns   _ = 0
--    nFpoints act  = fPoints (actFPA act) --TODO -> implement correct FPA qualification

-- \***********************************************************************
-- \*** Properties with respect to: Dataset                       ***
-- \*** TODO: both datasets and interfaces are represented as ObjectDef. This does actually make a difference for the function point count, so we have work....
instance Statistics Interface where
  nInterfaces _ = 1
  nPatterns   _ = 0
  nFpoints ifc  = fpVal $ fpaInterface ifc

--   instance Statistics ObjectDef where
--    nInterfaces (Obj{objmsub=Nothing}) = 2 -- this is an association, i.e. a binary relation --TODO -> check correctness
--    nInterfaces _ = 4 -- this is an entity with one or more attributes. --TODO -> check correctness
--    nPatterns   _ = 0
--    nFpoints    _ = fatal 60 "function points are not defined for ObjectDefs at all."
