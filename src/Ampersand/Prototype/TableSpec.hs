{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Ampersand.Prototype.TableSpec
    ( TableSpec(tsCmnt)
    , getTableName
    , sessionTableSpec, signalTableSpec
    , plug2TableSpec, tableSpec2Queries
    , dropTableSql, showColumsSql, createTableSql
    , insertQuery
    , additionalDatabaseSettings
    , queryAsPHP, queryAsSQL
    , doubleQuote, singleQuote)
where

import Prelude hiding (exp,putStrLn,readFile,writeFile)
import Data.Monoid
import Data.List
import Data.String (IsString(fromString))
import qualified Data.Text as Text
import Ampersand.Prototype.ProtoUtil
import Ampersand.FSpec.SQL
import Ampersand.FSpec
import Ampersand.FSpec.ToFSpec.ADL2Plug(suitableAsKey)
import Ampersand.Classes
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ShowAStruct

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


sessionTableSpec :: TableSpec
sessionTableSpec = 
    TableSpec { tsCmnt = ["Session timeout table"]
              , tsName = "__SessionTimeout__"
              , tsflds = [ AttributeSpec 
                             { fsname      = "SESSION"
                             , fstype      = Alphanumeric
                             , fsIsPrimKey = True
                             , fsDbNull    = False
                             }
                         , AttributeSpec 
                             { fsname      = "lastAccess"
                             , fstype      = Integer --HJO: Why not DateTime???
                             , fsIsPrimKey = False
                             , fsDbNull    = False
                             }
                         ]
              , tsKey  = "PRIMARY KEY (\"SESSION\")"
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
                     , "  "<>(show.properties.attExpr) x ]
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
createTableSql withComment tSpec = SqlQuery $
      ( if withComment 
        then map Text.pack . commentBlockSQL . tsCmnt $ tSpec
        else []
      ) <>
      [ "CREATE TABLE "<>(doubleQuote . Text.pack . tsName $ tSpec)] <>
      [ Text.replicate indnt " " <> Text.pack [pref] <> " " <> addColumn att 
      | (pref, att) <- zip ('(' : repeat ',') (tsflds tSpec)] <>
      ( if null (tsKey tSpec) 
        then []
        else [ Text.replicate indnt " " <> ", " <> Text.pack (tsKey tSpec) ]
      ) <>
      [ Text.replicate indnt " " <> ", " <> doubleQuote "ts_insertupdate"<>" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP"]<>
      [ Text.replicate indnt " " <> ") ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN" ]<>
      [ Text.replicate indnt " " <> ", ROW_FORMAT = DYNAMIC"]<>
      [ "" ]
  where
    indnt = 5
    addColumn :: AttributeSpec -> Text.Text
    addColumn att 
       =    doubleQuote (fsname att) <> " " 
         <> (Text.pack . showSQL . fstype) att 
         <> (if fsIsPrimKey att then " UNIQUE" else "")
         <> (if fsDbNull att then " DEFAULT NULL" else " NOT NULL")
showColumsSql :: TableSpec -> SqlQuery
showColumsSql tSpec = SqlQuery $
       ["SHOW COLUMNS FROM "<>(doubleQuote . Text.pack . tsName $ tSpec)]

dropTableSql :: TableSpec -> SqlQuery
dropTableSql tSpec = SqlQuery $
       ["DROP TABLE "<>(doubleQuote . Text.pack . tsName $ tSpec)]

fld2AttributeSpec ::SqlAttribute -> AttributeSpec
fld2AttributeSpec att 
  = AttributeSpec { fsname = Text.pack (name att)
                  , fstype = attType att
                  , fsIsPrimKey = isPrimaryKey att
                  , fsDbNull = attDBNull att 
                  }


insertQuery :: SomeValue val =>
       Text.Text        -- The name of the table
    -> [Text.Text]   -- The names of the attributes
    -> [[Maybe val]]  -- The rows to insert
    -> SqlQuery
insertQuery tableName attNames tblRecords = 
  SqlQuery $
     [ "INSERT INTO "<>doubleQuote tableName
     , "   ("<>Text.intercalate "," (map doubleQuote attNames) <>")"
     , "VALUES " 
     ]
   <> (Text.lines . ("   "<>) .Text.intercalate ("\n , ") $ [ "(" <>valuechain md<> ")" | md<-tblRecords])
   <> [""]
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
 (createTableSql withComment tSpec 
 ):
 [SqlQuery [ Text.pack $ "CREATE INDEX "<> show (tsName tSpec<>"_"<>(Text.unpack . fsname) fld)
                             <>" ON "<>show (tsName tSpec)
                             <>" ("<>(show . Text.unpack . fsname) fld<>")"
           ]
 | fld <- tsflds tSpec
 , not (fsIsPrimKey fld)
 , suitableAsKey (fstype  fld)
 ]

additionalDatabaseSettings :: [SqlQuery]
additionalDatabaseSettings = [ SqlQuery ["SET TRANSACTION ISOLATION LEVEL SERIALIZABLE"]]

doubleQuote :: (Data.String.IsString m, Monoid m) => m -> m
doubleQuote = enclose '\"'
singleQuote :: (Data.String.IsString m, Monoid m) => m -> m
singleQuote = enclose '`'
enclose :: (Data.String.IsString m, Monoid m) => Char -> m -> m
enclose c s = fromString [c] <> s <> fromString [c]

queryAsPHP :: SqlQuery -> Text.Text
queryAsPHP (SqlQuery xs) = showPhpStr (Text.unlines xs)
queryAsSQL :: SqlQuery -> Text.Text
queryAsSQL (SqlQuery xs) = Text.unlines xs

