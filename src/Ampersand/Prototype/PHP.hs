{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Prototype.PHP 
         ( evaluateExpSQL
         , createTempDatabase
         , tempDbName
         ) where

import           Ampersand.Basics
import           Ampersand.ADL1
import           Ampersand.FSpec
import           Ampersand.FSpec.SQL
import           Ampersand.Misc.HasClasses
import           Ampersand.Prototype.ProtoUtil
import           Ampersand.Prototype.TableSpec
import qualified RIO.Text as T
import           System.Directory
import           System.FilePath
import           System.Process(cwd,shell,readCreateProcess)


createTablePHP :: TableSpec -> [Text]
createTablePHP tSpec =
  map ("// "<>) (tsCmnt tSpec) <>
  [-- Drop table if it already exists
    "if($columns = mysqli_query($DB_link, "<>queryAsPHP (showColumsSql tSpec)<>")){"
  , "    mysqli_query($DB_link, "<>queryAsPHP (dropTableSql tSpec)<>");"
  , "}"
  ] <>
  [ "$sql="<>queryAsPHP (createTableSql False tSpec)<>";"
  , "mysqli_query($DB_link,$sql);" 
  , "if($err=mysqli_error($DB_link)) {"
  , "  $error=true; echo $err.'<br />';"
  , "}"
  , ""
  ]



-- evaluate normalized exp in SQL
evaluateExpSQL :: (HasProtoOpts env, HasLogFunc env) => FSpec -> Text -> Expression ->  RIO env [(Text,Text)]
evaluateExpSQL fSpec dbNm expr = do
    env <- ask
    let violationsExpr = conjNF env expr
        violationsQuery = prettySQLQuery 26 fSpec violationsExpr
    performQuery dbNm violationsQuery

performQuery :: (HasProtoOpts env, HasLogFunc env) =>
                Text -> SqlQuery ->  RIO env [(Text,Text)]
performQuery dbNm queryStr = do
    env <- ask
    queryResult <- (executePHPStr . showPHP) (php env)
    if "Error" `T.isPrefixOf` queryResult -- not the most elegant way, but safe since a correct result will always be a list
    then do mapM_ (logInfo . display) (T.lines ("\n******Problematic query:\n"<>queryAsSQL queryStr<>"\n******"))
            fatal ("PHP/SQL problem: "<>queryResult)
    else case reads (T.unpack queryResult) of
           [(pairs,"")] -> return pairs
           _            -> fatal ("Parse error on php result: \n"<>(T.unlines . map ("     " <>) . T.lines $ queryResult))
     
   where 
    php :: HasProtoOpts env => env -> [Text]
    php env =
      connectToMySqlServerPHP env (Just dbNm) <>
      [ "$sql="<>queryAsPHP queryStr<>";"
      , "$result=mysqli_query($DB_link,$sql);"
      , "if(!$result)"
      , "  die('Error : Connect to server failed'.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');"
      , "$rows=Array();"
      , "  while ($row = mysqli_fetch_array($result)) {"
      , "    $rows[]=$row;"
      , "    unset($row);"
      , "  }"
      , "echo '[';"
      , "for ($i = 0; $i < count($rows); $i<>) {"
      , "  if ($i==0) echo ''; else echo ',';"
      , "  echo '(\"'.addslashes($rows[$i]['src']).'\", \"'.addslashes($rows[$i]['tgt']).'\")';"
      , "}"
      , "echo ']';"
      ]

-- call the command-line php with phpStr as input
executePHPStr :: (HasLogFunc env) => Text -> RIO env Text
executePHPStr phpStr = do
    tempdir <- liftIO getTemporaryDirectory 
                 `catch`
                     (\e -> do 
                          let err = show (e :: IOException)
                          logWarn $ "Couldn't find temp directory. Using current directory : " <> displayShow err
                          return "."
                     )
    let phpPath = tempdir </> "tmpPhpQueryOfAmpersand" <.> "php"
    liftIO $ createDirectoryIfMissing True (takeDirectory phpPath)
    writeFileUtf8 phpPath phpStr
    
    executePHP phpPath
    

executePHP :: FilePath ->  RIO env Text
executePHP phpPath = do
    let cp = (shell command) 
                { cwd = Just (takeDirectory phpPath)
                }
        inputFile = phpPath
        outputFile = inputFile<>"Result"
        command = "php "<>show inputFile<>" > "<>show outputFile
    _ <- liftIO $ readCreateProcess cp ""
    result <- readUTF8File outputFile
    case result of
      Right content -> do
            liftIO $ removeFile outputFile
            return content
      Left err -> exitWith . PHPExecutionFailed $ 
            "PHP execution failed:"
            : fmap ("  "<>) err
            
   

showPHP :: [Text] -> Text
showPHP phpLines = T.unlines $ ["<?php"]<>phpLines<>["?>"]


tempDbName :: HasProtoOpts a => FSpec -> a -> Text
tempDbName fSpec x = "TempDB_"<>case view dbNameL x of
                                  Nothing -> name fSpec
                                  Just nm -> nm

connectToMySqlServerPHP :: HasProtoOpts a => a -> Maybe Text-> [Text]
connectToMySqlServerPHP x mDbName =
    [ "// Try to connect to the MySQL server"
    , "global $DB_host,$DB_user,$DB_pass;"
    , "$DB_host='"<>addSlashes (view sqlHostL x) <>"';"
    , "$DB_user='"<>addSlashes (view sqlLoginL x)<>"';"
    , "$DB_pass='"<>addSlashes (view sqlPwdL x)  <>"';"
    , ""
    ]<>
    (case mDbName of
       Nothing   ->
         [ "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass);"
         , "// Check connection"
         , "if (mysqli_connect_errno()) {"
         , "  die('Failed to connect to MySQL: ' . mysqli_connect_error());"
         , "}"
         , ""
         ]
       Just dbNm ->
         ["$DB_name='"<>dbNm<>"';"]<>
         connectToTheDatabasePHP
    )
connectToTheDatabasePHP :: [Text]
connectToTheDatabasePHP =
    [ "// Connect to the database"
    , "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass,$DB_name);"
    , "// Check connection"
    , "if (mysqli_connect_errno()) {"
    , "  die('Error : Failed to connect to the database: ' . mysqli_connect_error());"
    , "  }"
    , ""
    ]<>
    [ "$sql=\"SET SESSION sql_mode = 'ANSI,TRADITIONAL'\";" -- ANSI because of the syntax of the generated SQL
                                                            -- TRADITIONAL because of some more safety
    , "if (!mysqli_query($DB_link,$sql)) {"
    , "  die('Error setting sql_mode: ' . mysqli_error($DB_link));"
    , "  }"
    , ""
    ]

createTempDatabase :: (HasProtoOpts env, HasLogFunc env) =>
                      FSpec ->  RIO env Bool
createTempDatabase fSpec = do
    env <- ask
    result <- executePHPStr .
              showPHP $ phpStr env
    logInfo $ 
         if T.null result 
          then "Temp database created succesfully."
          else display $ T.intercalate "\n" $
                 [ "Temp database creation failed! :"
                 , "The result:"
                 , result
                 , "The statements:"
                 ] <>
                 lineNumbers (phpStr env)
                 
    return (T.null result)
 where 
  lineNumbers :: [Text] -> [Text]
  lineNumbers = map withNumber . zip [1..]
    where
      withNumber :: (Int,Text) -> Text
      withNumber (n,t) = "/*"<>T.take (5-length(show n)) "00000"<>tshow n<>"*/ "<>t
  phpStr :: (HasProtoOpts env) => env -> [Text]
  phpStr env = 
    (connectToMySqlServerPHP env Nothing) <>
    [ "/*** Set global varables to ensure the correct working of MySQL with Ampersand ***/"
    , ""
    , "    /* file_per_table is required for long columns */"
    , "    $sql='SET GLOBAL innodb_file_per_table = true';"
    , "    $result=mysqli_query($DB_link, $sql);"
    , "       if(!$result)"
    , "         die('Error '.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');"
    , "" 
    , "    /* file_format = Barracuda is required for long columns */"
    , "    $sql='SET GLOBAL innodb_file_format = `Barracuda`';"
    , "    $result=mysqli_query($DB_link, $sql);"
    , "       if(!$result)"
    , "         die('Error '.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');"
    , ""
    , "    /* large_prefix gives max single-column indices of 3072 bytes = win! */"
    , "    $sql='SET GLOBAL innodb_large_prefix = true';"
    , "    $result=mysqli_query($DB_link, $sql);"
    , "       if(!$result)"
    , "         die('Error '.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');"
    , ""
    , "$DB_name='"<>tempDbName fSpec env <>"';"
    , "// Drop the database if it exists"
    , "$sql="<>queryAsPHP dropDB<>";"
    , "mysqli_query($DB_link,$sql);"
    , "// Don't bother about the error if the database didn't exist..."
    , ""
    , "// Create the database"
    , "$sql="<>queryAsPHP createDB<>";"
    , "if (!mysqli_query($DB_link,$sql)) {"
    , "  // For diagnosis, dump the current file, so we can see what is going on."
    , "  $trace = debug_backtrace();"
    , "  $file = $trace[1]['file'];"
    , "  $thisFile = file_get_contents($file);"
    , "  fwrite(STDERR, $thisFile . \"\\n\");"
    , "  die('Error creating the database: ' . mysqli_error($DB_link));"
    , "  }"
    , ""
    ] 
    <> 
    connectToTheDatabasePHP 
    <>       
    [ "/*** Create new SQL tables ***/"
    , ""
    ]
    <>
    [ ""
    , "//// Number of plugs: " <> T.pack (show (length (plugInfos fSpec)))
    ]
    -- Create all plugs
    <> concatMap (createTablePHP . plug2TableSpec) [p | InternalPlug p <- plugInfos fSpec]
    -- Populate all plugs
    <> concatMap populatePlugPHP [p | InternalPlug p <- plugInfos fSpec]

    where
      dropDB :: SqlQuery 
      dropDB = SqlQuerySimple $
           "DROP DATABASE "<>(singleQuote $ tempDbName fSpec env)
      createDB :: SqlQuery
      createDB = SqlQuerySimple $
           "CREATE DATABASE "<>(singleQuote $ tempDbName fSpec env)<>" DEFAULT CHARACTER SET UTF8 COLLATE utf8_bin"
      populatePlugPHP plug =
        case tableContents fSpec plug of
          [] -> []
          tblRecords 
             -> ( "mysqli_query($DB_link, "<> queryAsPHP query <>");"
                ):["if($err=mysqli_error($DB_link)) { $error=true; echo $err.'<br />'; }"]
               where query = insertQuery True tableName attrNames tblRecords
                     tableName = name plug
                     attrNames = fmap attName . plugAttributes $ plug
           
