{-# LANGUAGE FlexibleInstances #-}

module Ampersand.Prototype.TableSpec
  ( TableSpec (tsCmnt),
    getTableName,
    plug2TableSpec,
    createViewSql,
    createDerivedViewSql,
    constraintQueries,
    schemaViewQueries,
    databasePlaceholder,
    tableSpec2Queries,
    dropTableIfExistsSql,
    showColumsSql,
    createTableSql,
    insertQuery,
    additionalDatabaseSettings,
    queryAsPHP,
    queryAsSQL,
    doubleQuote,
    singleQuote,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Core.ShowAStruct
import Ampersand.FSpec
import Ampersand.FSpec.FSpecAux (hasUniqueConstraint, isViewPlug, lookupConceptTable)
import Ampersand.FSpec.SQL
import Ampersand.FSpec.ToFSpec.ADL2Plug (suitableAsKey)
import Ampersand.Prototype.ProtoUtil
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

data TableSpec = TableSpec
  { tsCmnt :: [Text], -- Without leading "// "
    tsName :: Text,
    tsflds :: [AttributeSpec],
    tsKey :: Text
  }

data AttributeSpec = AttributeSpec
  { fsname :: Text,
    fstype :: TType,
    fsIsPrimKey :: Bool,
    fsDbNull :: Bool
  }

getTableName :: TableSpec -> Text
getTableName = tsName

plug2TableSpec :: PlugSQL -> TableSpec
plug2TableSpec plug =
  TableSpec
    { tsCmnt =
        [ "Plug " <> text1ToText (showUnique plug),
          "",
          "attributes:"
        ]
          <> concat
            [ [ showA (attExpr x)
              ]
              | x <- NE.toList $ plugAttributes plug
            ],
      tsName = text1ToText (showUnique plug),
      tsflds = NE.toList . fmap fld2AttributeSpec $ plugAttributes plug,
      tsKey = case (plug, (NE.head . plugAttributes) plug) of
        (BinSQL {}, _) ->
          if all (suitableAsKey . attType) (plugAttributes plug)
            then
              "PRIMARY KEY ("
                <> T.intercalate ", " (NE.toList $ doubleQuote . text1ToText . sqlColumNameToText1 . attSQLColName <$> plugAttributes plug)
                <> ")"
            else ""
        (TblSQL {}, primFld) ->
          case attUse primFld of
            PrimaryKey _ -> "PRIMARY KEY (" <> (doubleQuote . text1ToText . sqlColumNameToText1 . attSQLColName) primFld <> ")"
            ForeignKey c -> fatal ("ForeignKey " <> fullName c <> "not expected here!")
            PlainAttr -> ""
    }

createTableSql :: Bool -> TableSpec -> SqlQuery
createTableSql withComment tSpec
  | withComment =
      SqlQueryPretty
        $ ( commentBlockSQL . tsCmnt $ tSpec
          )
        <> [header]
        <> wrap cols
        <> wrap (maybeToList mKey)
        <> wrap endings
  | otherwise =
      SqlQueryPlain
        $ header
        <> " "
        <> T.intercalate " " cols
        <> " "
        <> fromMaybe mempty mKey
        <> " "
        <> T.unwords endings
  where
    header :: Text
    header = "CREATE TABLE " <> (doubleQuote . tsName $ tSpec)
    cols :: [Text]
    cols =
      [ T.pack [pref] <> " " <> addColumn att
        | (pref, att) <- zip ('(' : L.repeat ',') (tsflds tSpec)
      ]
    mKey :: Maybe Text
    mKey =
      case tsKey tSpec of
        "" -> Nothing
        x -> Just $ ", " <> x
    endings :: [Text]
    endings =
      [", " <> doubleQuote "ts_insertupdate" <> " TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP"]
        <> [") ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN"]
        <> [", ROW_FORMAT = DYNAMIC"]
    wrap :: [Text] -> [Text]
    wrap = map (\col -> T.replicate indnt " " <> col)
    indnt = 5
    addColumn :: AttributeSpec -> Text
    addColumn att =
      doubleQuote (fsname att)
        <> " "
        <> (showSQL . fstype) att
        <> (if fsIsPrimKey att then " UNIQUE" else "")
        <> (if fsDbNull att then " DEFAULT NULL" else " NOT NULL")
        <> " /* "
        <> (T.pack . show . fstype) att
        <> " */"

-- | The place in a query where the name of another database belongs.
--   The script names the database, and the deployment may give it another name:
--   the prototype framework fills in the name it is given when it installs the database.
--   The argument is the name of the database as the script has it.
databasePlaceholder :: Text -> Text
databasePlaceholder db = doubleQuote ("{{db:" <> db <> "}}")

-- | A table in another database is known in this database as a view on that table.
--   So every query of this context reads and writes the one table, in the database that holds it.
--   The arguments are the name of that database and the name of the table there.
createViewSql :: Bool -> Text -> Text -> TableSpec -> SqlQuery
createViewSql withComment db remote tSpec
  | withComment =
      SqlQueryPretty
        $ commentBlockSQL (tsCmnt tSpec <> ["", "This table is in the database " <> db <> "."])
        <> [statement]
  | otherwise = SqlQueryPlain statement
  where
    statement =
      "CREATE VIEW "
        <> doubleQuote (tsName tSpec)
        <> " AS SELECT * FROM "
        <> databasePlaceholder db
        <> "."
        <> doubleQuote remote

-- | The view of a derived relation (issue #1744): @CREATE VIEW@ on the SQL of the term that
--   @ENFORCE r := term@ gives. The plug names the view; the relation and the term go into
--   the comment, so that a reader of @database.sql@ sees where the view comes from.
createDerivedViewSql :: Bool -> FSpec -> PlugSQL -> Relation -> Expression -> SqlQuery
createDerivedViewSql withComment fSpec plug rel definition
  | withComment =
      SqlQueryPretty
        $ commentBlockSQL
          [ "Plug " <> text1ToText (showUnique plug),
            "",
            "A view for the derived relation " <> showA rel <> ",",
            "defined by ENFORCE " <> fullName rel <> " := " <> showA definition <> "."
          ]
        <> [header <> " AS"]
        <> map (T.replicate 5 " " <>) (derivedViewQuery 5 fSpec definition)
  | otherwise = SqlQueryPlain (header <> " AS " <> derivedViewQueryPlain fSpec definition)
  where
    header = "CREATE VIEW " <> doubleQuote (text1ToText (showUnique plug))

-- | The declarative constraints of the database structure, with @--foreign-keys@ on
--   (issue #1744): a FOREIGN KEY from every column that holds an atom of a concept to the
--   key column of the table of that concept, where that key is a PRIMARY KEY (the table of
--   a scalar concept has none, issue #341, and a concept of a type that cannot be listed has
--   no table); an atom of a specialisation that shares a wide table has the key of that table,
--   so the column refers to that key. And a UNIQUE constraint on every column that
--   'hasUniqueConstraint' names.
--   @RESTRICT@ on both sides: the prototype framework removes the links of an atom before
--   it deletes the atom, and merges atoms by deleting and adding links, so a cascade would
--   hide a defect rather than help. The statements come after every table, because a
--   constraint names two tables. The argument says which plugs are views on another
--   database, which take no constraint and can be referred to by none.
constraintQueries :: FSpec -> (PlugSQL -> Bool) -> [SqlQuery]
constraintQueries fSpec isForeign =
  [ SqlQuerySimple
      ( "ALTER TABLE "
          <> tableOf plug
          <> " ADD FOREIGN KEY ("
          <> columnOf att
          <> ") REFERENCES "
          <> tableOf refPlug
          <> " ("
          <> columnOf refAtt
          <> ") ON DELETE RESTRICT ON UPDATE RESTRICT"
      )
    | plug <- plugs,
      att <- NE.toList (plugAttributes plug),
      ForeignKey c <- [attUse att],
      Just (refPlug, _) <- [lookupConceptTable fSpec c],
      -- the key of the table, which an atom of a specialisation in that table shares
      let refAtt = NE.head (plugAttributes refPlug),
      isPrimaryKey refAtt,
      not (isForeign refPlug),
      not (isViewPlug fSpec refPlug),
      showSQL (attType att) == showSQL (attType refAtt) -- the same SQL type: a specialisation may carry another TType than its key (ALPHANUMERIC against OBJECT)
  ]
    <> [ SqlQuerySimple ("ALTER TABLE " <> tableOf plug <> " ADD UNIQUE (" <> columnOf att <> ")")
         | plug <- plugs,
           att <- NE.toList (plugAttributes plug),
           hasUniqueConstraint fSpec plug att
       ]
  where
    plugs = [p | InternalPlug p <- plugInfos fSpec, not (isForeign p), not (isViewPlug fSpec p)]
    tableOf = doubleQuote . text1ToText . showUnique
    columnOf = doubleQuote . text1ToText . sqlColumNameToText1 . attSQLColName

-- | The named external schema, with @--schema-views@ on (issue #1744): one view per conjunct,
--   named by the id of the conjunct in @conjuncts.json@ and selecting its violations, and one
--   view per interface field, named @ifc_<interface>_<field>@ along the path of the field,
--   with the field names escaped as the REST API escapes them, selecting the pairs of the
--   field's term. A name longer than the database allows is cut and gets a hash, and so does
--   a name that another view has when case is ignored. Every query that the JSON files carry
--   gets a view: an interface field query differs from its view only by the filter on the
--   source atom that the prototype adds, and no query depends on anything but the tables.
schemaViewQueries :: Bool -> FSpec -> [SqlQuery]
schemaViewQueries withComment fSpec =
  [ viewOf
      nm
      ( [ "Conjunct " <> text1ToText (rc_id conj) <> ": " <> showA (rcConjunct conj),
          "Rules:"
        ]
          <> map (\r -> "  " <> fullName r <> ": " <> showA r) (NE.toList (rc_orgRules conj))
      )
      (conjNF () . notCpl . rcConjunct $ conj)
    | (nm, conj) <- zip conjNames (allConjuncts fSpec)
  ]
    <> [ viewOf nm ["Interface " <> fullName ifc <> ", field " <> path <> ": " <> showA (objExpression obj)] (conjNF () (objExpression obj))
         | (nm, (ifc, path, obj)) <- zip ifcNames fieldObjects
       ]
  where
    conjNames = uniqueNames (map (text1ToText . rc_id) (allConjuncts fSpec))
    ifcNames = uniqueNames [T.intercalate "_" ("ifc" : fullName ifc : map escape pathParts) | (ifc, _, _, pathParts) <- fieldObjectsWithPath]
    fieldObjects = [(ifc, path, obj) | (ifc, path, obj, _) <- fieldObjectsWithPath]
    fieldObjectsWithPath :: [(Interface, Text, ObjectDef, [Text])]
    fieldObjectsWithPath =
      [ (ifc, T.intercalate "/" pathParts, obj, pathParts)
        | ifc <- interfaceS fSpec,
          (pathParts, obj) <- objectsIn [] (ifcObj ifc)
      ]
    objectsIn :: [Text] -> ObjectDef -> [([Text], ObjectDef)]
    objectsIn path obj = (path, obj) : concat [objectsIn (path <> [fieldName sub]) sub | sub <- fields obj]
    fieldName :: ObjectDef -> Text
    fieldName = maybe "field" text1ToText . objPlainName
    escape :: Text -> Text
    escape nm = case T.uncons nm of
      Nothing -> nm
      Just (c, cs) -> text1ToText (escapeIdentifier (Text1 c cs))
    viewOf :: Text -> [Text] -> Expression -> SqlQuery
    viewOf nm cmnt expr
      | withComment =
          SqlQueryPretty
            $ commentBlockSQL cmnt
            <> ["CREATE VIEW " <> doubleQuote nm <> " AS"]
            <> map (T.replicate 5 " " <>) (case prettySQLQuery 5 fSpec expr of SqlQueryPretty xs -> xs; q -> [queryAsSQL q])
      | otherwise = SqlQueryPlain ("CREATE VIEW " <> doubleQuote nm <> " AS " <> sqlQuery fSpec expr)
    -- A name fits in 64 characters and is unique when case is ignored.
    uniqueNames :: [Text] -> [Text]
    uniqueNames nms = zipWith pick nms [0 :: Int ..]
      where
        fitted = map fit nms
        lower = map T.toLower fitted
        pick nm i
          | length (filter (== T.toLower (fit nm)) lower) > 1 = fit (nm <> "_" <> tshow i)
          | otherwise = fit nm
        fit nm
          | T.length nm <= 64 = nm
          | otherwise = T.take 56 nm <> "_" <> T.take 7 (tshow (sha1hash nm))

showColumsSql :: TableSpec -> SqlQuery
showColumsSql tSpec =
  SqlQuerySimple
    $ "SHOW COLUMNS FROM "
    <> (doubleQuote . tsName $ tSpec)

dropTableIfExistsSql :: TableSpec -> SqlQuery
dropTableIfExistsSql tSpec =
  SqlQuerySimple
    $ "DROP TABLE IF EXISTS "
    <> (doubleQuote . tsName $ tSpec)

fld2AttributeSpec :: SqlAttribute -> AttributeSpec
fld2AttributeSpec att =
  AttributeSpec
    { fsname = text1ToText . sqlColumNameToText1 . attSQLColName $ att,
      fstype = attType att,
      fsIsPrimKey = isPrimaryKey att,
      fsDbNull = attDBNull att
    }

insertQuery ::
  (SomeValue val) =>
  Bool -> -- prettyprinted?
  SqlName -> -- The name of the table
  NE.NonEmpty SqlName -> -- The names of the attributes
  [[Maybe val]] -> -- The rows to insert
  SqlQuery
insertQuery withComments tableName attNames tblRecords
  | withComments =
      SqlQueryPretty
        $ [ "INSERT INTO " <> doubleQuote (tshow tableName),
            "   (" <> T.intercalate ", " (NE.toList $ fmap (doubleQuote . text1ToText . sqlColumNameToText1) attNames) <> ")",
            "VALUES "
          ]
        <> (T.lines . ("   " <>) . T.intercalate "\n , " $ ["(" <> valuechain md <> ")" | md <- tblRecords])
        <> [""]
  | otherwise =
      SqlQueryPlain
        $ "INSERT INTO "
        <> doubleQuote (tshow tableName)
        <> " ("
        <> T.intercalate ", " (NE.toList $ fmap (doubleQuote . text1ToText . sqlColumNameToText1) attNames)
        <> ")"
        <> " VALUES "
        <> T.intercalate ", " ["(" <> valuechain md <> ")" | md <- tblRecords]
  where
    valuechain :: (SomeValue val) => [Maybe val] -> Text
    valuechain record = T.intercalate ", " [maybe "NULL" repr att | att <- record]

class SomeValue a where
  repr :: a -> Text

instance SomeValue AAtomValue where
  repr = showValSQL

instance SomeValue Text where
  repr = id

tableSpec2Queries :: Bool -> TableSpec -> [SqlQuery]
tableSpec2Queries withComment tSpec =
  createTableSql withComment tSpec
    : [ SqlQuerySimple
          ( "CREATE INDEX "
              <> doubleQuote (tsName tSpec <> "_" <> tshow i)
              <> " ON "
              <> doubleQuote (tsName tSpec)
              <> " ("
              <> doubleQuote (fsname fld)
              <> ")"
          )
        | (i, fld) <-
            zip [0 .. (maxIndexes - 1)]
              . filter (suitableAsKey . fstype)
              . filter (not . fsIsPrimKey)
              $ tsflds tSpec
      ]
  where
    maxIndexes :: Int
    maxIndexes = 62 -- Limit the amount of indexes in edgecases causing mysql error 1069. (Issue #758)

additionalDatabaseSettings :: [SqlQuery]
additionalDatabaseSettings = [SqlQuerySimple "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE"]

doubleQuote :: (IsString m, Monoid m) => m -> m
doubleQuote = enclose '\"'

singleQuote :: (IsString m, Monoid m) => m -> m
singleQuote = enclose '`'

enclose :: (IsString m, Monoid m) => Char -> m -> m
enclose c s = fromString [c] <> s <> fromString [c]

queryAsPHP :: SqlQuery -> Text
queryAsPHP = showPhpStr . queryAsSQL

queryAsSQL :: SqlQuery -> Text
queryAsSQL sql =
  case sql of
    SqlQuerySimple x -> x
    SqlQueryPlain x -> x
    SqlQueryPretty xs -> T.unlines xs
