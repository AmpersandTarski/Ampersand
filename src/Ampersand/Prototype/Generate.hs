{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Prototype.Generate 
  (generateDBstructQueries, generateInitialPopQueries, generateMetaPopQueries
  )
where

import Ampersand.Core.AbstractSyntaxTree 
import Prelude hiding (writeFile,readFile,getContents,exp)
import Data.Maybe
import Data.Monoid
import Data.String (IsString)
import qualified Data.Text as Text
import Ampersand.FSpec
import Ampersand.Prototype.TableSpec
import Ampersand.FSpec.SQL

doubleQuote :: (Data.String.IsString m, Monoid m) => m -> m
doubleQuote s = "\"" <> s <> "\""

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
    populateTablesWithPops False fSpec
  where
    fillSignalTable :: [(Conjunct, [AAtomPair])] -> [SqlQuery]
    fillSignalTable = catMaybes . map fillWithSignal
    fillWithSignal :: (Conjunct, [AAtomPair]) -> Maybe SqlQuery
    fillWithSignal (conj, violations) 
     = case violations of
        [] -> Nothing
        viols -> 
          Just . SqlQuery $
            [ "INSERT INTO "<>Text.pack (show (getTableName signalTableSpec))
            , "   ("<>Text.intercalate ", " (map (Text.pack . doubleQuote) ["conjId","src","tgt"])<>")"
            , "VALUES " <> Text.intercalate " , " 
                  [ "(" <>Text.intercalate ", " [showAsValue (rc_id conj), showValPHP (apLeft p), showValPHP (apRight p)]<> ")" 
                  | p <- viols
                  ]
            ]
       
generateMetaPopQueries :: FSpec -> [SqlQuery]
generateMetaPopQueries = populateTablesWithPops True

populateTablesWithPops :: Bool -> FSpec -> [SqlQuery]
populateTablesWithPops ignoreDoubles fSpec =
      concatMap populatePlug [p | InternalPlug p <- plugInfos fSpec]
      where
        populatePlug :: PlugSQL -> [SqlQuery]
        populatePlug plug 
          = case tableContents fSpec plug of
             []  -> []
             tblRecords 
                 -> [SqlQuery
                       [ "INSERT "<> (if ignoreDoubles then "IGNORE " else "") <>"INTO "
                             <>Text.pack (show (name plug))
                       , "   ("<>Text.intercalate ", " (map (Text.pack . show . attName) (plugAttributes plug))<>") "
                       , "VALUES " <> Text.intercalate " , " 
                          [ "(" <>valuechain md<> ")" | md<-tblRecords]
                       ]
                    ]
         where
           valuechain record 
             = Text.intercalate ", " 
                 [case att of 
                    Nothing -> "NULL"
                    Just val -> Text.pack . showValSQL $ val
                 | att <- record ]

showAsValue :: String -> Text.Text
showAsValue str = Text.pack ("'"<>f str<>"'")
  where f :: String -> String
        f str'= 
          case str' of
            []        -> []
            ('\'':cs) -> "\\\'"<> f cs  --This is required to ensure that the result of showValue will be a proper singlequoted string.
            (c:cs)    -> c : f cs

