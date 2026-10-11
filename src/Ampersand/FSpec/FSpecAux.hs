module Ampersand.FSpec.FSpecAux (getRelationTableInfo, relationTableInfos, getConceptTableInfo, lookupConceptTable) where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.FSpec.FSpec
import RIO.List (repeat)

-- return table name and source and target column names for relation dcl
--   A relation stored in several tables (issue #1716) yields its first table;
--   the tables all have the same column layout, so a caller that needs the
--   layout can take any of them, and a caller that needs every table uses
--   'relationTableInfos'.
getRelationTableInfo :: FSpec -> Relation -> (PlugSQL, RelStore)
getRelationTableInfo fSpec dcl =
  case relationTableInfos fSpec dcl of
    (p, store) : _ -> (p, store)
    [] -> fatal ("Relation not found: " <> fullName dcl)

-- | Every table that stores (a part of) the relation, with the columns it
--   occupies there. One table for every relation, except a relation declared on
--   a concept that has no table of its own (a MULTITABLE union, issue #1716):
--   that relation has a column in the table of each member, and its pairs are
--   the union of what those tables hold.
relationTableInfos :: FSpec -> Relation -> [(PlugSQL, RelStore)]
relationTableInfos fSpec dcl =
  filter thisDcl . concatMap getRelInfos $ [p | InternalPlug p <- plugInfos fSpec]
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
