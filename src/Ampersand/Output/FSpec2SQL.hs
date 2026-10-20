module Ampersand.Output.FSpec2SQL (dumpSQLqueries, databaseStructureSql) where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Core.ParseTree (contextDatabases, databaseOfLabel, ownDatabase)
import Ampersand.Core.ShowAStruct
import Ampersand.FSpec
import Ampersand.FSpec.Incremental.DeltaTerms (deltaPlugs)
import Ampersand.FSpec.SQL
import Ampersand.FSpec.ToFSpec.ADL2Plug (foreignTable)
import Ampersand.Prototype.TableSpec
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

databaseStructureSql :: FSpec -> Text
databaseStructureSql fSpec =
  T.intercalate "\n"
    $ header (longVersion appVersion)
    <> header "Database structure queries"
    <> map (addSeparator . queryAsSQL) (generateDBstructQueries fSpec True)

generateDBstructQueries :: FSpec -> Bool -> [SqlQuery]
generateDBstructQueries fSpec withComment =
  concatMap structure [p | InternalPlug p <- plugInfos fSpec]
    -- delta tables for incremental violation maintenance (issue #1684):
    -- one two-column table per relation occurring in a conjunct, holding the
    -- transaction's touched pairs; empty outside transactions.
    <> concatMap (tableSpec2Queries withComment . plug2TableSpec) (deltaPlugs fSpec)
    <> additionalDatabaseSettings
  where
    -- A context creates the tables of its own database: its own things and those of the contexts that land on it.
    -- For a table in another database it creates a view on that table.
    -- The view names the database by the placeholder for its name, and the table by the name it has there.
    structure plug = case foreignTable roots plug of
      Nothing -> tableSpec2Queries withComment (plug2TableSpec plug)
      Just (lbl, remote) -> case databaseOfLabel (metas fSpec) lbl of
        Just (db, _) -> [createViewSql withComment db remote (plug2TableSpec plug)]
        Nothing -> fatal ("The context " <> lbl <> " lands on no database.")
    -- The contexts on other databases, each with the label of the context that installs its database.
    roots :: [(Text, Text)]
    roots =
      [ (lbl, root)
        | (lbl, db, _) <- contextDatabases (metas fSpec),
          lbl /= "",
          Just db /= ownDatabase (metas fSpec),
          root <- take 1 [l | (l, db', "") <- contextDatabases (metas fSpec), db' == db]
      ]

dumpSQLqueries :: env -> FSpec -> Text
dumpSQLqueries env fSpec =
  T.intercalate "\n"
    $ header (longVersion appVersion)
    <> header "Database structure queries"
    <> map (addSeparator . queryAsSQL) (generateDBstructQueries fSpec True)
    <> header "Inconsistencies in conjuncts"
    <> concatMap showConjunct (allConjuncts fSpec)
    <> header "Queries per relation"
    <> concatMap showDecl (vrels fSpec)
    <> header "Queries of interfaces"
    <> concatMap showInterface y
  where
    y :: [Interface]
    y = interfaceS fSpec <> interfaceG fSpec
    showInterface :: Interface -> [Text]
    showInterface ifc =
      header ("INTERFACE: " <> fullName ifc)
        <> (map ("  " <>) . showObjDef . ifcObj) ifc
      where
        showObjDef :: ObjectDef -> [Text]
        showObjDef obj =
          (header . showA . objExpression) obj
            <> [queryAsSQL . prettySQLQueryWithPlaceholder 2 fSpec . objExpression $ obj]
            <> maybe [] showSubInterface (objmsub obj)
            <> header ("Broad query for the object at " <> (T.pack . show . origin) obj)
            <> [prettyBroadQueryWithPlaceholder 2 fSpec obj]
        showSubInterface :: SubInterface -> [Text]
        showSubInterface sub =
          case sub of
            Box {} -> concatMap showObjDef [e | BxExpr e <- siObjs sub]
            InterfaceRef {} -> []

    showConjunct :: Conjunct -> [Text]
    showConjunct conj =
      header (text1ToText . rc_id $ conj)
        <> [ "/*",
             "Conjunct term:",
             "  " <> (showA . rcConjunct $ conj),
             "Rules for this conjunct:"
           ]
        <> map (text1ToText . showRule) (NE.toList $ rc_orgRules conj)
        <> [ "*/",
             (queryAsSQL . prettySQLQuery 2 fSpec . conjNF env . notCpl . rcConjunct $ conj) <> ";",
             ""
           ]
      where
        showRule :: Rule -> Text1
        showRule r =
          toText1Unsafe "  - " <> (fullName1 r <> toText1Unsafe (": " <> showA r))
    showDecl :: Relation -> [Text]
    showDecl decl =
      header (showA decl)
        <> [(queryAsSQL . prettySQLQuery 2 fSpec $ decl) <> ";", ""]

header :: Text -> [Text]
header title =
  [ "/*",
    T.replicate width "*",
    "***" <> spaces firstspaces <> title <> spaces (width - 6 - firstspaces - l) <> "***",
    T.replicate width "*",
    "*/"
  ]
  where
    width = case L.maximumMaybe [80, l + 8] of
      Nothing -> fatal "Impossible"
      Just x -> x
    l = T.length title
    spaces :: Int -> Text
    spaces i = T.replicate i " "
    firstspaces :: Int
    firstspaces = (width - 6 - l) `quot` 2

addSeparator :: Text -> Text
addSeparator t = t <> ";"
