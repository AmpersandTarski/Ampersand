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
import           Ampersand.Misc
import           Ampersand.Prototype.ProtoUtil
import           Ampersand.Prototype.TableSpec
import qualified RIO.List as L
import qualified RIO.Text as T
import           System.Directory
import           System.FilePath
import           System.Process


createTablePHP :: TableSpec -> [T.Text]
createTablePHP tSpec =
  map (T.pack . ("// "<>)) (tsCmnt tSpec) <>
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
evaluateExpSQL :: (HasOptions env, HasHandles env) => FSpec -> T.Text -> Expression ->  RIO env [(String,String)]
evaluateExpSQL fSpec dbNm expr = do
    env <- ask 
    let violationsExpr = conjNF (getOptions env) expr
        violationsQuery = prettySQLQuery 26 fSpec violationsExpr
    -- verboseLn ("evaluateExpSQL fSpec "++showA expr)
    -- verboseLn (intercalate "\n" . showPrf showA . cfProof opts) expr
    -- verboseLn "End of proof"
    performQuery dbNm violationsQuery

performQuery :: (HasOptions env, HasHandles env) =>
                T.Text -> SqlQuery ->  RIO env [(String,String)]
performQuery dbNm queryStr = do
    env <- ask
    queryResult <- (executePHPStr . showPHP) (php $ getOptions env)
    if "Error" `L.isPrefixOf` queryResult -- not the most elegant way, but safe since a correct result will always be a list
    then do mapM_ putStrLn (lines (T.unpack $ "\n******Problematic query:\n"<>queryAsSQL queryStr<>"\n******"))
            fatal ("PHP/SQL problem: "<>queryResult)
    else case reads queryResult of
           [(pairs,"")] -> return pairs
           _            -> fatal ("Parse error on php result: \n"<>(unlines . map ("     " ++) . lines $ queryResult))
     
   where 
    php :: Options -> [T.Text]
    php opts =
      connectToMySqlServerPHP opts (Just dbNm) <>
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
      , "for ($i = 0; $i < count($rows); $i++) {"
      , "  if ($i==0) echo ''; else echo ',';"
      , "  echo '(\"'.addslashes($rows[$i]['src']).'\", \"'.addslashes($rows[$i]['tgt']).'\")';"
      , "}"
      , "echo ']';"
      ]

-- call the command-line php with phpStr as input
executePHPStr :: (HasHandles env) => T.Text -> RIO env String
executePHPStr phpStr = do
    tempdir <- liftIO getTemporaryDirectory 
                 `catch`
                     (\e -> do 
                          let err = show (e :: IOException)
                          putStrLn ("Warning: Couldn't find temp directory. Using current directory : " <> err)
                          return "."
                     )
    let phpPath = tempdir </> "tmpPhpQueryOfAmpersand" <.> "php"
    writeFileUtf8 phpPath phpStr
    
    results <- executePHP phpPath
    return (normalizeNewLines results)
    
normalizeNewLines :: String -> String   --TODO: If Text is used for output, there will be no more need for normalization
normalizeNewLines = f . L.intercalate "\n" . lines
  where 
    f [] = []
    f ('\r':'\n':rest) = '\n':f rest
    f (c:cs) = c: f cs 

executePHP :: String ->  RIO env String
executePHP phpPath = do
    let cp = (shell command) 
                { cwd = Just (takeDirectory phpPath)
                }
        inputFile = phpPath
        outputFile = inputFile++"Result"
        command = "php "++show inputFile++" > "++show outputFile
    _ <- liftIO $ readCreateProcess cp ""
    result <- liftIO $ readUTF8File outputFile
    case result of
      Right content -> do
            liftIO $ removeFile outputFile
            return content
      Left err -> exitWith . PHPExecutionFailed $ 
            ["PHP execution failed:"
            ,"  Could not read file: "++outputFile
            ,"    "++ err
            ]
   

showPHP :: [T.Text] -> T.Text
showPHP phpLines = T.unlines $ ["<?php"]<>phpLines<>["?>"]


tempDbName :: Options -> T.Text
tempDbName Options{..} = "TempDB_"<>T.pack dbName

connectToMySqlServerPHP :: Options -> Maybe T.Text-> [T.Text]
connectToMySqlServerPHP Options{..} mDbName =
    [ "// Try to connect to the MySQL server"
    , "global $DB_host,$DB_user,$DB_pass;"
    , "$DB_host='"<>subst sqlHost <>"';"
    , "$DB_user='"<>subst sqlLogin<>"';"
    , "$DB_pass='"<>subst sqlPwd  <>"';"
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
  where
   subst :: String -> T.Text
   subst = addSlashes . T.pack
connectToTheDatabasePHP :: [T.Text]
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

createTempDatabase :: (HasOptions env, HasHandles env, HasVerbosity env) =>
                      FSpec ->  RIO env Bool
createTempDatabase fSpec = do
    env <- ask
    let opts = getOptions env
    result <- executePHPStr .
              showPHP $ phpStr opts
    verboseLn $ 
         if null result 
          then "Temp database created succesfully."
          else "Temp database creation failed! :\n"<>lineNumbers (phpStr opts)<>"\nThe result:\n"<>result
    return (null result)
 where 
  lineNumbers :: [T.Text] -> String
  lineNumbers = L.intercalate "  \n" . map withNumber . zip [1..] . map T.unpack
    where
      withNumber :: (Int,String) -> String
      withNumber (n,t) = "/*"<>take (5-length(show n)) "00000"<>show n<>"*/ "<>t
  phpStr :: Options -> [T.Text]
  phpStr opts = 
    (connectToMySqlServerPHP opts Nothing) <>
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
    , "$DB_name='"<>tempDbName opts <>"';"
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
           "DROP DATABASE "<>(singleQuote $ tempDbName opts)
      createDB :: SqlQuery
      createDB = SqlQuerySimple $
           "CREATE DATABASE "<>(singleQuote $ tempDbName opts)<>" DEFAULT CHARACTER SET UTF8 COLLATE utf8_bin"
      populatePlugPHP plug =
        case tableContents fSpec plug of
          [] -> []
          tblRecords 
             -> ( "mysqli_query($DB_link, "<> queryAsPHP query <>");"
                ):["if($err=mysqli_error($DB_link)) { $error=true; echo $err.'<br />'; }"]
               where query = insertQuery True tableName attrNames tblRecords
                     tableName = T.pack . name $ plug
                     attrNames = fmap (T.pack . attName) . plugAttributes $ plug
           
