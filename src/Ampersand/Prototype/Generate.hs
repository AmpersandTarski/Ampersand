{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Prototype.Generate 
  (generateDBstructQueries, generateInitialPopQueries
  )
where

import           Ampersand.Basics
import           Ampersand.Core.AbstractSyntaxTree 
import           Ampersand.FSpec
import           Ampersand.FSpec.SQL
import           Ampersand.Prototype.TableSpec
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as Text

generateDBstructQueries :: FSpec -> Bool -> [SqlQuery]
generateDBstructQueries fSpec withComment 
  =    concatMap (tableSpec2Queries withComment)
           (   [ sessionTableSpec, signalTableSpec]
            ++ [plug2TableSpec p | InternalPlug p <- plugInfos fSpec]
           )
    <> additionalDatabaseSettings 
generateInitialPopQueries :: FSpec -> Bool -> [SqlQuery]
generateInitialPopQueries fSpec withComments
  = fillSignalTable (initialConjunctSignals fSpec) <>
    populateTablesWithPops withComments fSpec
  where
    fillSignalTable :: [(Conjunct, AAtomPairs)] -> [SqlQuery]
    fillSignalTable = mapMaybe fillWithSignal
    fillWithSignal :: (Conjunct, AAtomPairs) -> Maybe SqlQuery
    fillWithSignal (conj, violations) 
     = case Set.elems violations of
        []    -> Nothing
        viols -> Just query
          where query = insertQuery withComments tableName attrNames tblRecords
                tableName = getTableName signalTableSpec
                attrNames = ["conjId","src","tgt"]
                tblRecords = map mkRecord viols
                  where
                    mkRecord p = 
                       map Just ["'"++rc_id conj++"'", showValSQL (apLeft p), showValSQL (apRight p)]

populateTablesWithPops :: Bool -> FSpec -> [SqlQuery]
populateTablesWithPops withComments fSpec =
      mapMaybe populatePlug [p | InternalPlug p <- plugInfos fSpec]
      where
        populatePlug :: PlugSQL -> Maybe SqlQuery
        populatePlug plug 
          = case tableContents fSpec plug of
             []  -> Nothing
             tblRecords 
                 -> Just query
               where query = insertQuery withComments tableName attrNames tblRecords
                     tableName = Text.pack . name $ plug
                     attrNames = map (Text.pack . attName) . plugAttributes $ plug


