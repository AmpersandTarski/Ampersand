{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Prototype.Generate 
  (generateDBstructQueries, generateInitialPopQueries, generateMetaPopQueries
  )
where

import Ampersand.Core.AbstractSyntaxTree 
import Prelude hiding (writeFile,readFile,getContents,exp)
import Data.Monoid
import Data.String (IsString)
import qualified Data.Text as Text
import Ampersand.FSpec
import Ampersand.Prototype.PHP

doubleQuote :: (Data.String.IsString m, Monoid m) => m -> m
doubleQuote s = "\"" <> s <> "\""

generateDBstructQueries :: FSpec -> Bool -> [Text.Text]
generateDBstructQueries fSpec withComment = 
   map (sqlQuery2Text withComment) $ generateDBstructQueries' fSpec withComment

generateDBstructQueries' :: FSpec -> Bool -> [SqlQuery]
generateDBstructQueries' fSpec withComment 
  =    concatMap (tableSpec2Queries withComment)
                 ( sessionTableSpec
                 : signalTableSpec
                 : [plug2TableSpec p | InternalPlug p <- plugInfos fSpec]
                 )
    <> additionalDatabaseSettings 
generateInitialPopQueries :: FSpec -> [Text.Text]
generateInitialPopQueries fSpec 
  = fillSignalTable (initialConjunctSignals fSpec) <>
    populateTablesWithPops False fSpec
  where
    fillSignalTable :: [(Conjunct, [AAtomPair])] -> [Text.Text]
    fillSignalTable [] = []
    fillSignalTable conjSignals 
     = [Text.unlines
            [ "INSERT INTO "<>Text.pack (show (getTableName signalTableSpec))
            , "   ("<>Text.intercalate ", " (map (Text.pack . doubleQuote) ["conjId","src","tgt"])<>")"
            , "VALUES " <> Text.intercalate " , " 
                  [ "(" <>Text.intercalate ", " [showAsValue (rc_id conj), showValPHP (apLeft p), showValPHP (apRight p)]<> ")" 
                  | (conj, viols) <- conjSignals
                  , p <- viols
                  ]
            ]
       ]
generateMetaPopQueries :: FSpec -> [Text.Text]
generateMetaPopQueries = populateTablesWithPops True

populateTablesWithPops :: Bool -> FSpec -> [Text.Text]
populateTablesWithPops ignoreDoubles fSpec =
      concatMap populatePlug [p | InternalPlug p <- plugInfos fSpec]
      where
        populatePlug :: PlugSQL -> [Text.Text]
        populatePlug plug 
          = case tableContents fSpec plug of
             []  -> []
             tblRecords 
                 -> [Text.unlines
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
                    Just val -> showValPHP val
                 | att <- record ]

showAsValue :: String -> Text.Text
showAsValue str = Text.pack ("'"<>f str<>"'")
  where f :: String -> String
        f str'= 
          case str' of
            []        -> []
            ('\'':cs) -> "\\\'"<> f cs  --This is required to ensure that the result of showValue will be a proper singlequoted string.
            (c:cs)    -> c : f cs

