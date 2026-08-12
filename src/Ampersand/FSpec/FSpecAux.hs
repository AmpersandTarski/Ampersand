module Ampersand.FSpec.FSpecAux (getRelationTableInfo, getConceptTableInfo, lookupConceptTable) where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.FSpec.FSpec
import RIO.List (repeat)

-- return table name and source and target column names for relation dcl
getRelationTableInfo :: FSpec -> Relation -> (PlugSQL, RelStore)
getRelationTableInfo fSpec dcl =
  case filter thisDcl . concatMap getRelInfos $ [p | InternalPlug p <- plugInfos fSpec] of
    [(p, store)] -> (p, store)
    [] -> fatal ("Relation not found: " <> fullName dcl)
    _ -> fatal ("Relation found multiple times: " <> fullName dcl)
  where
    getRelInfos :: PlugSQL -> [(PlugSQL, RelStore)]
    getRelInfos p = zip (repeat p) (dLkpTbl p)
    thisDcl :: (a, RelStore) -> Bool
    thisDcl (_, store) = rsDcl store == dcl

-- | The concept table of a concept, and the column in it that holds the atoms,
--   or Nothing when the concept has no concept table of its own. That happens
--   for ONE, and for any concept whose atoms no generated query enumerates
--   (issue #1672).
lookupConceptTable :: FSpec -> A_Concept -> Maybe (PlugSQL, SqlAttribute)
lookupConceptTable fSpec cpt =
  case lookupCpt fSpec cpt of
    [] -> Nothing
    [x] -> Just x -- Any of the resulting plugs should do.
    xs -> fatal ("Only one result expected:" <> tshow xs)

-- return table name and source and target column names for relation rel.
--   Use `lookupConceptTable` instead where a concept without a table is a
--   possibility rather than a bug.
getConceptTableInfo :: FSpec -> A_Concept -> (PlugSQL, SqlAttribute)
getConceptTableInfo fSpec cpt =
  case lookupConceptTable fSpec cpt of
    Nothing -> fatal ("No plug found for concept '" <> fullName cpt <> "'.")
    Just x -> x
