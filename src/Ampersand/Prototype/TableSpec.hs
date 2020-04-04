{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Ampersand.Prototype.TableSpec
    ( TableSpec(tsCmnt)
    , getTableName
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
import           Data.String (IsString(fromString))
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

data TableSpec
  = TableSpec { tsCmnt :: [Text]  -- Without leading "// "
              , tsName :: Text
              , tsflds :: [AttributeSpec]
              , tsKey  ::  Text
              }
data AttributeSpec
  = AttributeSpec { fsname :: Text
                  , fstype :: TType
                  , fsIsPrimKey :: Bool
                  , fsDbNull :: Bool
                  }

getTableName :: TableSpec -> Text
getTableName = tsName


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
                   | x <- NE.toList $ plugAttributes plug
                   ]
     , tsName = name plug
     , tsflds = NE.toList . fmap fld2AttributeSpec $ plugAttributes plug
     , tsKey  = case (plug, (NE.head . plugAttributes) plug) of
                 (BinSQL{}, _)   -> if all (suitableAsKey . attType) (plugAttributes plug)
                                    then "PRIMARY KEY (" 
                                            <> T.intercalate ", " (NE.toList $ fmap (tshow . attName) (plugAttributes plug))
                                            <> ")"
                                    else ""
                 (TblSQL{}, primFld) ->
                      case attUse primFld of
                         PrimaryKey _ -> "PRIMARY KEY (" <> (tshow . attName) primFld <> ")"
                         ForeignKey c -> fatal ("ForeignKey "<>name c<>"not expected here!")
                         PlainAttr    -> ""
     }

createTableSql :: Bool -> TableSpec -> SqlQuery
createTableSql withComment tSpec
  | withComment = SqlQueryPretty $
      ( commentBlockSQL . tsCmnt $ tSpec
      ) <>
      [header] <>
      wrap cols <>
      wrap (maybeToList mKey) <>
      wrap endings 
  | otherwise = SqlQueryPlain $
      header <>
      " " <> T.intercalate " " cols <>
      " " <> fromMaybe mempty mKey <>
      " " <> T.unwords endings
  where
    header :: Text
    header = "CREATE TABLE "<>(doubleQuote . tsName $ tSpec)
    cols :: [Text]
    cols = [ T.pack [pref] <> " " <> addColumn att 
           | (pref, att) <- zip ('(' : L.repeat ',') (tsflds tSpec)]
    mKey :: Maybe Text
    mKey =
      case tsKey tSpec of
        "" -> Nothing
        x  -> Just $ ", "<> x
    endings :: [Text]
    endings =   
      [ ", " <> doubleQuote "ts_insertupdate"<>" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP"]<>
      [ ") ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN" ]<>
      [ ", ROW_FORMAT = DYNAMIC"]
    wrap :: [Text] -> [Text]
    wrap = map (\col -> T.replicate indnt " " <> col)
    indnt = 5
    addColumn :: AttributeSpec -> Text
    addColumn att 
       =    doubleQuote (fsname att) <> " " 
         <> (showSQL . fstype) att 
         <> (if fsIsPrimKey att then " UNIQUE" else "")
         <> (if fsDbNull att then " DEFAULT NULL" else " NOT NULL")
         <> " /* "
         <> (T.pack . show . fstype) att
         <> " */"
         
showColumsSql :: TableSpec -> SqlQuery
showColumsSql tSpec = SqlQuerySimple $
       "SHOW COLUMNS FROM "<>(doubleQuote . tsName $ tSpec)

dropTableSql :: TableSpec -> SqlQuery
dropTableSql tSpec = SqlQuerySimple $
       "DROP TABLE "<>(doubleQuote . tsName $ tSpec)

fld2AttributeSpec ::SqlAttribute -> AttributeSpec
fld2AttributeSpec att 
  = AttributeSpec { fsname = name att
                  , fstype = attType att
                  , fsIsPrimKey = isPrimaryKey att
                  , fsDbNull = attDBNull att 
                  }

insertQuery :: SomeValue val =>
       Bool          -- prettyprinted?
    -> Text     -- The name of the table
    -> NE.NonEmpty Text   -- The names of the attributes
    -> [[Maybe val]] -- The rows to insert
    -> SqlQuery
insertQuery withComments tableName attNames tblRecords
  | withComments = SqlQueryPretty $
     [ "INSERT INTO "<>doubleQuote tableName
     , "   ("<>T.intercalate ", " (NE.toList $ fmap doubleQuote attNames) <>")"
     , "VALUES " 
     ]
   <> (T.lines . ("   "<>) .T.intercalate "\n , " $ [ "(" <>valuechain md<> ")" | md<-tblRecords])
   <> [""]
  | otherwise = SqlQueryPlain $
        "INSERT INTO "<>doubleQuote tableName
     <> " ("<>T.intercalate ", " (NE.toList $ fmap  doubleQuote attNames) <>")"
     <> " VALUES "
     <> (T.intercalate ", " $ [ "(" <>valuechain md<> ")" | md<-tblRecords])
  where
    valuechain :: SomeValue val => [Maybe val] -> Text
    valuechain record = T.intercalate ", " [case att of Nothing -> "NULL" ; Just val -> repr val | att<-record]

class SomeValue a where
  repr :: a -> Text
instance SomeValue AAtomValue where
  repr = showValSQL
instance SomeValue Text where
  repr = id

tableSpec2Queries :: Bool -> TableSpec -> [SqlQuery]
tableSpec2Queries withComment tSpec = 
 createTableSql withComment tSpec :
 [SqlQuerySimple $ 
    ( "CREATE INDEX "<> tshow (tsName tSpec<>"_"<>tshow i)
    <>" ON "<>tshow (tsName tSpec) <> " ("
    <> (tshow . fsname $ fld)<>")"
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

queryAsPHP :: SqlQuery -> Text
queryAsPHP = showPhpStr . queryAsSQL
queryAsSQL :: SqlQuery -> Text
queryAsSQL sql = 
  case sql of 
    SqlQuerySimple x  -> x
    SqlQueryPlain  x  -> x
    SqlQueryPretty xs -> T.unlines xs
