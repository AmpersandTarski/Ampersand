{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Ampersand.Prototype.TableSpec
    ( TableSpec(tsCmnt)
    , getTableName
    , signalTableSpec
    , plug2TableSpec, tableSpec2Queries
    , dropTableSql, showColumsSql, createTableSql
    , insertQuery
    , additionalDatabaseSettings
    , queryAsPHP, queryAsSQL
    , doubleQuote, singleQuote)
where

import           Ampersand.Basics
import           Ampersand.ADL1
import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec
import           Ampersand.FSpec.SQL
import           Ampersand.FSpec.ToFSpec.ADL2Plug(suitableAsKey)
import           Ampersand.Prototype.ProtoUtil
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.String (IsString(fromString))
import qualified Data.Text as Text

data TableSpec
  = TableSpec { tsCmnt :: [String]  -- Without leading "// "
              , tsName :: String
              , tsflds :: [AttributeSpec]
              , tsKey  ::  String
              }
data AttributeSpec
  = AttributeSpec { fsname :: Text.Text
                  , fstype :: TType
                  , fsIsPrimKey :: Bool
                  , fsDbNull :: Bool
                  }

signalTableSpec :: TableSpec
signalTableSpec =
    TableSpec { tsCmnt = ["Signal table"]
              , tsName = "__all_signals__"
              , tsflds = [ AttributeSpec 
                             { fsname      = "conjId"
                             , fstype      = Alphanumeric
                             , fsIsPrimKey = False
                             , fsDbNull    = False
                             }
                         , AttributeSpec 
                             { fsname      = "src"
                             , fstype      = Alphanumeric
                             , fsIsPrimKey = False
                             , fsDbNull    = False
                             }
                         , AttributeSpec 
                             { fsname      = "tgt"
                             , fstype      = Alphanumeric
                             , fsIsPrimKey = False
                             , fsDbNull    = False
                             }        
                         ]
              , tsKey  = ""
              }
getTableName :: TableSpec -> Text.Text
getTableName = Text.pack . tsName


plug2TableSpec :: PlugSQL -> TableSpec
plug2TableSpec plug 
  = TableSpec 
     { tsCmnt = 
                   ["Plug "<>name plug
                   ,""
                   ,"attributes:"
                   ]<> concat
                   [ [showA (attExpr x)
                     ]
                   | x <- plugAttributes plug
                   ]
     , tsName = name plug
     , tsflds = map fld2AttributeSpec $ plugAttributes plug
     , tsKey  = case (plug, (head.plugAttributes) plug) of
                 (BinSQL{}, _)   -> if all (suitableAsKey . attType) (plugAttributes plug)
                                    then "PRIMARY KEY (" 
                                            <> intercalate ", " (map (show . attName) (plugAttributes plug))
                                            <> ")"
                                    else ""
                 (TblSQL{}, primFld) ->
                      case attUse primFld of
                         PrimaryKey _ -> "PRIMARY KEY (" <> (show . attName) primFld <> ")"
                         ForeignKey c -> fatal ("ForeignKey "<>name c<>"not expected here!")
                         PlainAttr    -> ""
     }

createTableSql :: Bool -> TableSpec -> SqlQuery
createTableSql withComment tSpec
  | withComment = SqlQueryPretty $
      ( map Text.pack . commentBlockSQL . tsCmnt $ tSpec
      ) <>
      [header] <>
      wrap cols <>
      wrap (maybeToList mKey) <>
      wrap endings 
  | otherwise = SqlQueryPlain $
      header <>
      " " <> Text.intercalate " " cols <>
      " " <> fromMaybe mempty mKey <>
      " " <> Text.unwords endings
  where
    header :: Text.Text
    header = "CREATE TABLE "<>(doubleQuote . Text.pack . tsName $ tSpec)
    cols :: [Text.Text]
    cols = [ Text.pack [pref] <> " " <> addColumn att 
           | (pref, att) <- zip ('(' : repeat ',') (tsflds tSpec)]
    mKey :: Maybe Text.Text
    mKey =
      case tsKey tSpec of
        "" -> Nothing
        x  -> Just . Text.pack $ ", "<> x
    endings :: [Text.Text]
    endings =   
      [ ", " <> doubleQuote "ts_insertupdate"<>" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP"]<>
      [ ") ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN" ]<>
      [ ", ROW_FORMAT = DYNAMIC"]
    wrap :: [Text.Text] -> [Text.Text]
    wrap = map (\col -> Text.replicate indnt " " <> col)
    indnt = 5
    addColumn :: AttributeSpec -> Text.Text
    addColumn att 
       =    doubleQuote (fsname att) <> " " 
         <> (Text.pack . showSQL . fstype) att 
         <> (if fsIsPrimKey att then " UNIQUE" else "")
         <> (if fsDbNull att then " DEFAULT NULL" else " NOT NULL")
         <> " /* "
         <> (Text.pack . show . fstype) att
         <> " */"
         
showColumsSql :: TableSpec -> SqlQuery
showColumsSql tSpec = SqlQuerySimple $
       "SHOW COLUMNS FROM "<>(doubleQuote . Text.pack . tsName $ tSpec)

dropTableSql :: TableSpec -> SqlQuery
dropTableSql tSpec = SqlQuerySimple $
       "DROP TABLE "<>(doubleQuote . Text.pack . tsName $ tSpec)

fld2AttributeSpec ::SqlAttribute -> AttributeSpec
fld2AttributeSpec att 
  = AttributeSpec { fsname = Text.pack (name att)
                  , fstype = attType att
                  , fsIsPrimKey = isPrimaryKey att
                  , fsDbNull = attDBNull att 
                  }


insertQuery :: SomeValue val =>
       Bool          -- prettyprinted?
    -> Text.Text     -- The name of the table
    -> [Text.Text]   -- The names of the attributes
    -> [[Maybe val]] -- The rows to insert
    -> SqlQuery
insertQuery withComments tableName attNames tblRecords
  | withComments = SqlQueryPretty $
     [ "INSERT INTO "<>doubleQuote tableName
     , "   ("<>Text.intercalate ", " (map doubleQuote attNames) <>")"
     , "VALUES " 
     ]
   <> (Text.lines . ("   "<>) .Text.intercalate "\n , " $ [ "(" <>valuechain md<> ")" | md<-tblRecords])
   <> [""]
  | otherwise = SqlQueryPlain $
        "INSERT INTO "<>doubleQuote tableName
     <> " ("<>Text.intercalate ", " (map doubleQuote attNames) <>")"
     <> " VALUES "
     <> (Text.intercalate ", " $ [ "(" <>valuechain md<> ")" | md<-tblRecords])
  where
    valuechain :: SomeValue val => [Maybe val] -> Text.Text
    valuechain record = Text.intercalate ", " [case att of Nothing -> "NULL" ; Just val -> repr val | att<-record]

class SomeValue a where
  repr :: a -> Text.Text
instance SomeValue AAtomValue where
  repr = Text.pack . showValSQL
instance SomeValue String where
  repr = Text.pack

tableSpec2Queries :: Bool -> TableSpec -> [SqlQuery]
tableSpec2Queries withComment tSpec = 
 createTableSql withComment tSpec :
 [SqlQuerySimple . Text.pack $ 
    ( "CREATE INDEX "<> show (tsName tSpec<>"_"<>show i)
    <>" ON "<>show (tsName tSpec) <> " ("
    <> (show . Text.unpack . fsname $ fld)<>")"
    )
 | (i,fld) <- zip [0..(maxIndexes - 1)]
            . filter (suitableAsKey . fstype)
            . filter (not . fsIsPrimKey)
            $ tsflds tSpec
 ]
   where maxIndexes :: Int
         maxIndexes = 62  --Limit the amount of indexes in edgecases causing mysql error 1069. (Issue #758) 
additionalDatabaseSettings :: [SqlQuery]
additionalDatabaseSettings = [ SqlQuerySimple "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE"]

doubleQuote :: (Data.String.IsString m, Monoid m) => m -> m
doubleQuote = enclose '\"'
singleQuote :: (Data.String.IsString m, Monoid m) => m -> m
singleQuote = enclose '`'
enclose :: (Data.String.IsString m, Monoid m) => Char -> m -> m
enclose c s = fromString [c] <> s <> fromString [c]

queryAsPHP :: SqlQuery -> Text.Text
queryAsPHP = showPhpStr . queryAsSQL
queryAsSQL :: SqlQuery -> Text.Text
queryAsSQL sql = 
  case sql of 
    SqlQuerySimple x  -> x
    SqlQueryPlain  x  -> x
    SqlQueryPretty xs -> Text.unlines xs
