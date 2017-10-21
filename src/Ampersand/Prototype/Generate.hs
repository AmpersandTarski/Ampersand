{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Prototype.Generate 
  (generateDBstructQueries, generateInitialPopQueries
  )
where

import Ampersand.Core.AbstractSyntaxTree 
import Prelude hiding (writeFile,readFile,getContents,exp)
import Data.Maybe
import Data.Monoid
import qualified Data.Text as Text
import Ampersand.FSpec
import Ampersand.Prototype.TableSpec
import Ampersand.FSpec.SQL

generateDBstructQueries :: FSpec -> Bool -> [SqlQuery]
generateDBstructQueries fSpec withComment 
  =    concatMap (tableSpec2Queries withComment)
           (   [ sessionTableSpec, signalTableSpec]
            ++ [plug2TableSpec p | InternalPlug p <- plugInfos fSpec]
           )
    <> additionalDatabaseSettings 
generateInitialPopQueries :: FSpec -> [SqlQuery]
generateInitialPopQueries fSpec 
  = fillSignalTable (initialConjunctSignals fSpec) <>
    populateTablesWithPops fSpec
  where
    fillSignalTable :: [(Conjunct, [AAtomPair])] -> [SqlQuery]
    fillSignalTable = catMaybes . map fillWithSignal
    fillWithSignal :: (Conjunct, [AAtomPair]) -> Maybe SqlQuery
    fillWithSignal (conj, violations) 
     = case violations of
        []    -> Nothing
        viols -> Just query
          where query = insertQuery tableName attrNames tblRecords
                tableName = getTableName signalTableSpec
                attrNames = ["conjId","src","tgt"]
                tblRecords = map mkRecord viols
                  where
                    mkRecord p = 
                       map Just [rc_id conj, showValSQL (apLeft p), showValSQL (apRight p)]

populateTablesWithPops :: FSpec -> [SqlQuery]
populateTablesWithPops fSpec =
      catMaybes . map populatePlug $ [p | InternalPlug p <- plugInfos fSpec]
      where
        populatePlug :: PlugSQL -> Maybe SqlQuery
        populatePlug plug 
          = case tableContents fSpec plug of
             []  -> Nothing
             tblRecords 
                 -> Just query
               where query = insertQuery tableName attrNames tblRecords
                     tableName = Text.pack . name $ plug
                     attrNames = map (Text.pack . attName) . plugAttributes $ plug


