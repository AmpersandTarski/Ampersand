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
import           Control.Exception
import           Data.List
import           Data.Monoid
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.Directory
import           System.FilePath
import           System.Process


createTablePHP :: TableSpec -> [Text.Text]
createTablePHP tSpec =
  map (Text.pack . ("// "<>)) (tsCmnt tSpec) <>
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
evaluateExpSQL :: FSpec -> Text.Text -> Expression -> IO [(String,String)]
evaluateExpSQL fSpec dbNm expr =
  -- verboseLn (getOpts fSpec) ("evaluateExpSQL fSpec "++showA expr)
  -- verboseLn (getOpts fSpec) (intercalate "\n" . showPrf showA . cfProof (getOpts fSpec)) expr
  -- verboseLn (getOpts fSpec) "End of proof"
  performQuery fSpec dbNm violationsQuery
 where violationsExpr = conjNF (getOpts fSpec) expr
       violationsQuery = prettySQLQuery 26 fSpec violationsExpr

performQuery :: FSpec -> Text.Text -> SqlQuery -> IO [(String,String)]
performQuery fSpec dbNm queryStr =
 do { queryResult <- (executePHPStr . showPHP) php
    ; if "Error" `isPrefixOf` queryResult -- not the most elegant way, but safe since a correct result will always be a list
      then do verboseLn opts{verboseP=True} (Text.unpack $ "\n******Problematic query:\n"<>queryAsSQL queryStr<>"\n******")
              fatal ("PHP/SQL problem: "<>queryResult)
      else case reads queryResult of
             [(pairs,"")] -> return pairs
             _            -> fatal ("Parse error on php result: \n"<>(unlines . indent 5 . lines $ queryResult))
    } 
   where 
    opts = getOpts fSpec
    php :: [Text.Text]
    php =
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
executePHPStr :: Text.Text -> IO String
executePHPStr phpStr =
 do { tempdir <- catch getTemporaryDirectory
                       (\e -> do let err = show (e :: IOException)
                                 hPutStr stderr ("Warning: Couldn't find temp directory. Using current directory : " <> err)
                                 return ".")
    ; (tempPhpFile, temph) <- openTempFile tempdir "tmpPhpQueryOfAmpersand.php"
    ; Text.hPutStr temph phpStr
    ; hClose temph
    ; results <- executePHP tempPhpFile
  --  ; removeFile tempPhpFile
    ; return (normalizeNewLines results)
    }
normalizeNewLines :: String -> String
normalizeNewLines = f . intercalate "\n" . lines
  where 
    f [] = []
    f ('\r':'\n':rest) = '\n':f rest
    f (c:cs) = c: f cs 

executePHP :: String -> IO String
executePHP phpPath =
 do { let cp = (shell command) 
                   { cwd = Just (takeDirectory phpPath)
                   }
          inputFile = phpPath
          outputFile = inputFile++"Result"
          command = "php "++show inputFile++" > "++show outputFile
    ; _ <- readCreateProcess cp ""
    ; result <- readFile outputFile
    ; removeFile outputFile
    ; return result
    }

showPHP :: [Text.Text] -> Text.Text
showPHP phpLines = Text.unlines $ ["<?php"]<>phpLines<>["?>"]


tempDbName :: Options -> Text.Text
tempDbName opts = "TempDB_"<>Text.pack (dbName opts)

connectToMySqlServerPHP :: Options -> Maybe Text.Text-> [Text.Text]
connectToMySqlServerPHP opts mDbName =
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
   subst :: (Options -> String) -> Text.Text
   subst x = addSlashes . Text.pack . x $ opts

connectToTheDatabasePHP :: [Text.Text]
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

createTempDatabase :: FSpec -> IO Bool
createTempDatabase fSpec =
 do { result <- executePHPStr .
           showPHP $ phpStr
    ; verboseLn (getOpts fSpec) 
         (if null result 
          then "Temp database created succesfully."
          else "Temp database creation failed! :\n"<>lineNumbers phpStr<>"\nThe result:\n"<>result  )
    ; return (null result)
    }
 where 
  lineNumbers :: [Text.Text] -> String
  lineNumbers = intercalate "  \n" . map withNumber . zip [1..] . map Text.unpack
    where
      withNumber :: (Int,String) -> String
      withNumber (n,t) = "/*"<>take (5-length(show n)) "00000"<>show n<>"*/ "<>t
  phpStr :: [Text.Text]
  phpStr = 
    connectToMySqlServerPHP (getOpts fSpec) Nothing <>
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
    ]<> 
    [ "$DB_name='"<>tempDbName (getOpts fSpec)<>"';"
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
    ] <> 
    connectToTheDatabasePHP <>       
    [ "/*** Create new SQL tables ***/"
    , ""
    ] <>
    [ ""
    , "//// Number of plugs: " <> Text.pack (show (length (plugInfos fSpec)))
    ]
    -- Create all plugs
    <> concatMap (createTablePHP . plug2TableSpec) [p | InternalPlug p <- plugInfos fSpec]
    -- Populate all plugs
    <> concatMap populatePlugPHP [p | InternalPlug p <- plugInfos fSpec]
  
    where
      dropDB :: SqlQuery 
      dropDB = SqlQuerySimple $
           "DROP DATABASE "<>(singleQuote . tempDbName . getOpts $ fSpec)
      createDB :: SqlQuery
      createDB = SqlQuerySimple $
           "CREATE DATABASE "<>(singleQuote . tempDbName . getOpts $ fSpec)<>" DEFAULT CHARACTER SET UTF8 COLLATE utf8_bin"
      populatePlugPHP plug =
        case tableContents fSpec plug of
          [] -> []
          tblRecords 
             -> ( "mysqli_query($DB_link, "<> queryAsPHP query <>");"
                ):["if($err=mysqli_error($DB_link)) { $error=true; echo $err.'<br />'; }"]
               where query = insertQuery True tableName attrNames tblRecords
                     tableName = Text.pack . name $ plug
                     attrNames = map (Text.pack . attName) . plugAttributes $ plug
           
