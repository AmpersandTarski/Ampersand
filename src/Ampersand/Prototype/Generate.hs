{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Prototype.Generate 
  (generateDBstructQueries, generateInitialPopQueries
  )
where

import           Ampersand.Basics
import           Ampersand.ADL1
import           Ampersand.FSpec
import           Ampersand.FSpec.SQL
import           Ampersand.Prototype.TableSpec
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as Text

generateDBstructQueries :: FSpec -> Bool -> [SqlQuery]
generateDBstructQueries fSpec withComment 
  =    concatMap (tableSpec2Queries withComment) ([plug2TableSpec p | InternalPlug p <- plugInfos fSpec])
    <> additionalDatabaseSettings 
generateInitialPopQueries :: FSpec -> Bool -> [SqlQuery]
generateInitialPopQueries fSpec withComments
  = populateTablesWithPops withComments fSpec

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


