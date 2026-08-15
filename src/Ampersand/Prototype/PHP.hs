module Ampersand.Prototype.PHP
  ( evaluateExpSQL,
    createTempDatabase,
    probeMySqlServer,
    tempDbName,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
-- import Ampersand.Core.ShowAStruct (showA)
import Ampersand.FSpec
import Ampersand.FSpec.SQL
import Ampersand.Prototype.TableSpec
import RIO.Directory
import RIO.FilePath
import qualified RIO.Text as T
import System.Process (cwd, readCreateProcessWithExitCode, shell)

createTablePHP :: TableSpec -> [Text]
createTablePHP tSpec =
  map ("// " <>) (tsCmnt tSpec)
    <> [
         -- Drop table if it already exists
         "mysqli_query($DB_link, " <> queryAsPHP (dropTableIfExistsSql tSpec) <> ");",
         ""
       ]
    <> [ "$sql=" <> queryAsPHP (createTableSql False tSpec) <> ";",
         "mysqli_query($DB_link,$sql);",
         "if($err=mysqli_error($DB_link)) {",
         "  $error=true; echo $err.'<br />';",
         "}",
         ""
       ]

-- evaluate normalized exp in SQL
evaluateExpSQL :: (HasLogFunc env) => FSpec -> Text -> Expression -> RIO env [(Text, Text)]
evaluateExpSQL fSpec dbNm expr = do
  env <- ask
  let violationsExpr = conjNF env expr
      violationsQuery = prettySQLQuery 26 fSpec violationsExpr
  {- The following is for debugging purposes. Don't forget to switch on showA in the imports.
        sqlText = queryAsSQL violationsQuery
    logInfo ""
    logInfo $ "=== Original expression: " <> display (showA expr) <> " ==="
    logInfo $ "=== Normalized (conjNF): " <> display (showA violationsExpr) <> " ==="
    logInfo $ "=== Generated SQL: ==="
    logInfo $ display sqlText
    logInfo "=== END SQL ==="
    logInfo ""
  -}
  performQuery dbNm violationsQuery

performQuery ::
  (HasLogFunc env) =>
  Text ->
  SqlQuery ->
  RIO env [(Text, Text)]
performQuery dbNm queryStr = do
  queryResult <- (executePHPStr . showPHP) php
  if "Error" `T.isPrefixOf` queryResult -- not the most elegant way, but safe since a correct result will always be a list
    then do
      mapM_ (logInfo . display) (T.lines ("\n******Problematic query:\n" <> queryAsSQL queryStr <> "\n******"))
      fatal ("PHP/SQL problem: " <> queryResult)
    else case reads (T.unpack queryResult) of
      [(pairs, "")] -> return pairs
      _ -> fatal ("Parse error on php result: \n" <> (T.unlines . map ("     " <>) . T.lines $ queryResult))
  where
    php :: [Text]
    php =
      connectToMySqlServerPHP (Just dbNm)
        <> [ "$sql=" <> queryAsPHP queryStr <> ";",
             "$result=mysqli_query($DB_link,$sql);",
             "if(!$result) {",
             "  die('Error : Connect to server failed'.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');",
             "}",
             "$rows=Array();",
             "  while ($row = mysqli_fetch_array($result)) {",
             "    $rows[]=$row;",
             "    unset($row);",
             "  }",
             "echo '[';",
             "  for ($i = 0; $i < count($rows); $i++) {",
             "    if ($i==0) { ",
             "      echo ''; ",
             "    } else { ",
             "      echo ',';",
             "    };",
             "    echo '(\"'.addslashes($rows[$i]['src']).'\", \"'.addslashes($rows[$i]['tgt']).'\")';",
             "  }",
             "echo ']';"
           ]

-- call the command-line php with phpStr as input
executePHPStr :: (HasLogFunc env) => Text -> RIO env Text
executePHPStr phpStr = do
  tempdir <-
    liftIO getTemporaryDirectory
      `catch` ( \e -> do
                  let err = show (e :: IOException)
                  logWarn $ "Couldn't find temp directory. Using current directory : " <> displayShow err
                  return "."
              )
  let phpPath = tempdir </> "tmpPhpQueryOfAmpersand" <.> "php"
  liftIO $ createDirectoryIfMissing True (takeDirectory phpPath)
  writeFileUtf8 phpPath phpStr
  executePHP phpPath

executePHP :: (HasLogFunc env) => FilePath -> RIO env Text
executePHP phpPath = do
  let cp =
        (shell command)
          { cwd = Just (takeDirectory phpPath)
          }
      inputFile = phpPath
      outputFile = inputFile <> "Result"
      command = "php " <> show inputFile <> " > " <> show outputFile
  (exit_code, _out, err) <- liftIO (readCreateProcessWithExitCode cp "")
  case exit_code of
    ExitFailure code -> do
      fileContents <- readFileUtf8 phpPath
      mapM_ (logDebug . display)
        $ case fileContents of
          Left msg -> msg
          Right txt -> addLineNumbers . T.lines $ txt
      exitWith
        . PHPExecutionFailed
        $ [ "PHP execution failed (exit code " <> tshow code <> "). The PHP interpreter reported:"
          ]
        <> map ("  " <>) (reportedLines err)
        <> databaseHint err
    ExitSuccess -> do
      result <- readFileUtf8 outputFile
      case result of
        Right content -> do
          liftIO $ removeFile outputFile
          return content
        Left err' ->
          exitWith
            . PHPExecutionFailed
            $ "PHP execution failed:"
            : fmap ("  " <>) err'
  where
    reportedLines :: String -> [Text]
    reportedLines err = case take 10 . filter (not . T.null) . T.lines . T.pack $ err of
      [] -> ["(the interpreter produced no error output; is PHP installed and in PATH?)"]
      ls -> ls
    -- The most common failure by far is a refused database connection, which
    -- PHP reports as an uncaught mysqli exception. Name the cause and the way
    -- out, instead of leaving the reader with a bare interpreter error.
    databaseHint :: String -> [Text]
    databaseHint err
      | "mysqli" `T.isInfixOf` T.pack err =
          [ "",
            "This is a database-connection failure, not a PHP defect.",
            "Ampersand reads MYSQL_HOST, MYSQL_USER and MYSQL_PASSWORD (defaults: 127.0.0.1, root, empty password).",
            "Point MYSQL_HOST at a reachable MariaDB, for example:",
            "  docker run -d --name ampersand-regression-db -e MYSQL_ALLOW_EMPTY_PASSWORD=yes -p 127.0.0.1:3310:3306 mariadb:10.4",
            "  MYSQL_HOST=127.0.0.1:3310 ampersand validate <script.adl>"
          ]
      | otherwise = []

-- | Probe the MySQL server exactly as every generated PHP script reaches it:
--   the same connection code, reading the same environment variables. Returns
--   Nothing when the server answers, or the interpreter's error output when
--   it does not. The probe never exits the program; callers decide how to
--   report.
probeMySqlServer :: (HasLogFunc env) => RIO env (Maybe [Text])
probeMySqlServer = do
  tempdir <-
    liftIO getTemporaryDirectory
      `catch` ( \e -> do
                  let err = show (e :: IOException)
                  logWarn $ "Couldn't find temp directory. Using current directory : " <> displayShow err
                  return "."
              )
  let phpPath = tempdir </> "tmpPhpProbeOfAmpersand" <.> "php"
  liftIO $ createDirectoryIfMissing True (takeDirectory phpPath)
  writeFileUtf8 phpPath (showPHP php)
  let cp = (shell ("php " <> show phpPath)) {cwd = Just (takeDirectory phpPath)}
  (exit_code, out, err) <- liftIO (readCreateProcessWithExitCode cp "")
  return $ case exit_code of
    ExitSuccess
      | "DBOK" `T.isInfixOf` T.pack out -> Nothing
      -- Older PHP reports a refused connection through die(), which exits 0.
      | otherwise -> Just (probeLines (out <> "\n" <> err))
    ExitFailure _ -> Just (probeLines (err <> "\n" <> out))
  where
    php = connectToMySqlServerPHP Nothing <> ["echo 'DBOK';"]
    probeLines :: String -> [Text]
    probeLines s = case filter (not . T.null) . T.lines . T.pack $ s of
      [] -> ["(the PHP interpreter produced no output; is PHP installed and in PATH?)"]
      ls -> ls

addLineNumbers :: [Text] -> [Text]
addLineNumbers = zipWith (curry withNumber) [0 ..]
  where
    withNumber :: (Int, Text) -> Text
    withNumber (n, t) = "/*" <> T.take (5 - length (show n)) "00000" <> tshow n <> "*/ " <> t

-- | Truncate a list to show only the first and last n elements
-- If the list has <= 2*n elements, return the whole list
-- Otherwise, show first n, a separator line, and last n
truncateMiddle :: Int -> [Text] -> [Text]
truncateMiddle n xs
  | length xs <= 2 * n = xs
  | otherwise =
      take n xs
        <> ["... (" <> tshow (length xs - 2 * n) <> " lines omitted) ..."]
        <> drop (length xs - n) xs

showPHP :: [Text] -> Text
showPHP phpLines = T.unlines $ ["<?php"] <> phpLines <> ["?>"]

tempDbName :: FSpec -> Text
tempDbName fSpec = "TempDB_" <> localNameOf fSpec

-- | Database name should not contain specific characters. Also, it has some maximum length.
-- mkValidDBName :: Text -> Text
-- mkValidDBName = T.reverse . T.take 31 . T.reverse . removeAll [' ', '/', '.']
--   where
--     removeAll :: [Char] -> Text -> Text
--     removeAll cs t = case T.uncons t of
--       Nothing -> t
--       Just (h, tl) -> T.cons (if h `elem` cs then h else '_') (removeAll cs tl)
connectToMySqlServerPHP :: Maybe Text -> [Text]
connectToMySqlServerPHP mDbName =
  [ "// Try to connect to the MySQL server",
    "global $DB_host,$DB_user,$DB_pass;",
    "// Read connection parameters from environment variables (for containerized setups) or use defaults",
    "$DB_host = getenv('MYSQL_HOST') ?: '127.0.0.1';",
    "$DB_user = getenv('MYSQL_USER') ?: 'root';",
    "$DB_pass = getenv('MYSQL_PASSWORD') ?: '';",
    ""
  ]
    <> ( case mDbName of
           Nothing ->
             [ "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass);",
               "// Check connection",
               "if (mysqli_connect_errno()) {",
               "  die('Failed to connect to MySQL: ' . mysqli_connect_error());",
               "}",
               ""
             ]
           Just dbNm ->
             ["$DB_name='" <> dbNm <> "';"]
               <> connectToTheDatabasePHP
       )

connectToTheDatabasePHP :: [Text]
connectToTheDatabasePHP =
  [ "// Connect to the database",
    "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass,$DB_name);",
    "// Check connection",
    "if (mysqli_connect_errno()) {",
    "  die('Error : Failed to connect to the database: ' . mysqli_connect_error());",
    "  }",
    ""
  ]
    <> [ "$sql=\"SET SESSION sql_mode = 'ANSI,TRADITIONAL'\";", -- ANSI because of the syntax of the generated SQL
    -- TRADITIONAL because of some more safety
         "if (!mysqli_query($DB_link,$sql)) {",
         "  die('Error setting sql_mode: ' . mysqli_error($DB_link));",
         "  }",
         ""
       ]

createTempDatabase ::
  (HasLogFunc env) =>
  FSpec ->
  RIO env Bool
createTempDatabase fSpec = do
  result <-
    executePHPStr
      . showPHP
      $ phpStr
  logInfo
    $ if T.null result
      then "Temp database created succesfully."
      else
        display
          $ T.intercalate "\n"
          $ [ "Temp database creation failed! :",
              "The result:",
              result,
              "The statements (showing first and last 10 lines):"
            ]
          <> truncateMiddle 10 (addLineNumbers phpStr)

  return (T.null result)
  where
    phpStr :: [Text]
    phpStr =
      connectToMySqlServerPHP Nothing
        <> [ "/*** Set global varables to ensure the correct working of MySQL with Ampersand ***/",
             "",
             "    /* file_per_table is required for long columns */",
             "    $sql='SET GLOBAL innodb_file_per_table = true';",
             "    $result=mysqli_query($DB_link, $sql);",
             "       if(!$result)",
             "         die('Error '.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');",
             "",
             --             "    /* file_format = Barracuda is required for long columns */",
             --             "    /* Since MariaDB 10.2, the default is Barracuda. ",
             --             "    $sql='SET GLOBAL innodb_file_format = `Barracuda`';",
             --             "    $result=mysqli_query($DB_link, $sql);",
             --             "       if(!$result)",
             --             "         die('Error '.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');",
             --             "    */",
             --             "",
             --             "    /* large_prefix gives max single-column indices of 3072 bytes = win! */",
             --             "    /* Since MariaDB 10.2, the default is Barracuda. The innodb_large_prefix setting has become obsolete.",
             --             "    $sql='SET GLOBAL innodb_large_prefix = true';",
             --             "    $result=mysqli_query($DB_link, $sql);",
             --             "       if(!$result)",
             --             "         die('Error '.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');",
             --             "    */",
             --             "",
             "$DB_name='" <> tempDbName fSpec <> "';",
             "// Drop the database if it exists",
             "$sql=" <> queryAsPHP dropDB <> ";",
             "mysqli_query($DB_link,$sql);",
             "// Don't bother about the error if the database didn't exist...",
             "",
             "// Create the database",
             "$sql=" <> queryAsPHP createDB <> ";",
             "if (!mysqli_query($DB_link,$sql)) {",
             "  // For diagnosis, dump the current file, so we can see what is going on.",
             "  $trace = debug_backtrace();",
             "  $file = $trace[1]['file'];",
             "  $thisFile = file_get_contents($file);",
             "  fwrite(STDERR, $thisFile . \"\\n\");",
             "  die('Error creating the database: ' . mysqli_error($DB_link));",
             "  }",
             ""
           ]
        <> connectToTheDatabasePHP
        <> [ "/*** Create new SQL tables ***/",
             ""
           ]
        <> [ "",
             "//// Number of plugs: " <> T.pack (show (length (plugInfos fSpec)))
           ]
        -- Create all plugs
        <> concatMap (createTablePHP . plug2TableSpec) [p | InternalPlug p <- plugInfos fSpec]
        -- Populate all plugs
        <> concatMap populatePlugPHP [p | InternalPlug p <- plugInfos fSpec]
      where
        dropDB :: SqlQuery
        dropDB =
          SqlQuerySimple
            $ "DROP DATABASE IF EXISTS "
            <> singleQuote (tempDbName fSpec)
        createDB :: SqlQuery
        createDB =
          SqlQuerySimple
            $ "CREATE DATABASE "
            <> singleQuote (tempDbName fSpec)
            <> " DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN"
        populatePlugPHP plug =
          case tableContents fSpec plug of
            [] -> []
            tblRecords ->
              ( "mysqli_query($DB_link, " <> queryAsPHP query <> ");"
              )
                : ["if($err=mysqli_error($DB_link)) { $error=true; echo $err.'<br />'; }"]
              where
                query = insertQuery True (sqlname plug) attrNames tblRecords
                attrNames = attSQLColName <$> plugAttributes plug
