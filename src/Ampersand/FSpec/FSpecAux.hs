module Ampersand.FSpec.FSpecAux
  ( getRelationTableInfo,
    relationTableInfos,
    getConceptTableInfo,
    lookupConceptTable,
    isViewRelation,
    isViewPlug,
    derivedViewPlugs,
    hasUniqueConstraint,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.ToFSpec.ADL2Plug (suitableAsKey)
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

-- | Whether a relation is generated as a database view (issue #1744).
isViewRelation :: FSpec -> Relation -> Bool
isViewRelation fSpec rel = rel `elem` map fst (fDerivedViews fSpec)

-- | Whether a plug is the view of a derived relation, rather than a table (issue #1744).
--   Such a plug holds one relation, its own, and no concept.
isViewPlug :: FSpec -> PlugSQL -> Bool
isViewPlug fSpec plug = case (mainItem plug, dLkpTbl plug) of
  (Right rel, [store]) -> rsDcl store == rel && isViewRelation fSpec rel && null (cLkpTbl plug)
  _ -> False

-- | The views of the derived relations, each with its relation and defining term, in an
--   order in which the views can be created (issue #1744).
derivedViewPlugs :: FSpec -> [(PlugSQL, Relation, Expression)]
derivedViewPlugs fSpec = [(fst (getRelationTableInfo fSpec rel), rel, definition) | (rel, definition) <- fDerivedViews fSpec]

-- | Whether the generated database structure puts a UNIQUE constraint on this column of
--   this table, with @--foreign-keys@ on (issue #1744): a column that holds one side of a
--   relation and that the layout knows to be unique, of a type the database can index.
--   The identity column of a specialisation in a wide table is left out: it repeats the key
--   of its row, so it is unique by construction. The same predicate decides, in the cost
--   profile, that the conjunct of that multiplicity is structural (claim PRF-20).
hasUniqueConstraint :: FSpec -> PlugSQL -> SqlAttribute -> Bool
hasUniqueConstraint fSpec plug att =
  fForeignKeys fSpec
    && attUniq att
    && not (isPrimaryKey att)
    && suitableAsKey (attType att)
    && not (isViewPlug fSpec plug)
    && case plug of
      BinSQL {} -> True
      TblSQL {} -> case attExpr att of
        EDcD {} -> True
        EFlp (EDcD {}) -> True
        _ -> False

-- return table name and source and target column names for relation rel.
--   Use `lookupConceptTable` instead where a concept without a table is a
--   possibility rather than a bug.
getConceptTableInfo :: FSpec -> A_Concept -> (PlugSQL, SqlAttribute)
getConceptTableInfo fSpec cpt =
  case lookupConceptTable fSpec cpt of
    Nothing -> fatal ("No plug found for concept '" <> fullName cpt <> "'.")
    Just x -> x
