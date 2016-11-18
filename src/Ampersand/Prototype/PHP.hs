{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Prototype.PHP 
         ( evaluateExpSQL
         , signalTableSpec, getTableName, createTempDatabase, tempDbName) where

import Prelude hiding (exp,putStrLn,readFile,writeFile)
import Control.Exception
import Control.Monad
import Data.Monoid
import Data.List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Process
import System.IO hiding (hPutStr,hGetContents,putStrLn,readFile,writeFile)
import System.Directory
import System.FilePath
import Ampersand.Prototype.ProtoUtil
import Ampersand.FSpec.SQL
import Ampersand.FSpec
import Ampersand.Basics
import Ampersand.Misc
import Ampersand.Core.AbstractSyntaxTree


initMySQLGlobals :: [Text.Text]
initMySQLGlobals =
  [ "/*** Set global varables to ensure the correct working of MySQL with Ampersand ***/"
  , ""
  , "    /* file_per_table is required for long columns */"
  , "    $result=mysqli_query($DB_link, \"SET GLOBAL innodb_file_per_table = true\");"
  , "       if(!$result)"
  , "         die('Error '.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');"
  , "" 
  , "    /* file_format = Barracuda is required for long columns */"
  , "    $result=mysqli_query($DB_link, \"SET GLOBAL innodb_file_format = `Barracuda` \");"
  , "       if(!$result)"
  , "         die('Error '.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');"
  , ""
  , "    /* large_prefix gives max single-column indices of 3072 bytes = win! */"
  , "    $result=mysqli_query($DB_link, \"SET GLOBAL innodb_large_prefix = true \");"
  , "       if(!$result)"
  , "         die('Error '.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');"
  , ""
  ]

createTablesPHP :: FSpec -> [Text.Text]
createTablesPHP fSpec = 
        [ "/*** Create new SQL tables ***/"
        , ""
        ] <>
        createTablePHP sessionTableSpec <>
        setSqlModePHP<>
        createTablePHP signalTableSpec <>
        [ ""
        , "//// Number of plugs: " <> Text.pack (show (length (plugInfos fSpec)))
        ]
        -- Create all plugs
        <> concatMap (createTablePHP . plug2TableSpec) [p | InternalPlug p <- plugInfos fSpec]

--                 (headerCmmnt,tableName,crflds,engineOpts)
type TableSpec = (String,String,[Text.Text],String)

getTableName :: TableSpec -> Text.Text
getTableName (_,nm,_,_) = Text.pack nm

createTablePHP :: TableSpec -> [Text.Text]
createTablePHP (headerCmmnt,tableName,crflds,engineOpts) =
  [ Text.pack headerCmmnt
  -- Drop table if it already exists
  , "if($columns = mysqli_query($DB_link, "<>showPhpStr ("SHOW COLUMNS FROM `"<>Text.pack tableName<>"`")<>")){"
  , "    mysqli_query($DB_link, "<>showPhpStr ("DROP TABLE `"<>Text.pack tableName<>"`")<>");"
  , "}"
  ] <>
  [ "mysqli_query($DB_link,\"CREATE TABLE `"<>Text.pack tableName<>"`"] <>
  [ Text.replicate 23 " " <> Text.pack [pref] <> " " <> att | (pref, att) <- zip ('(' : repeat ',') crflds ] <>
  [ Text.replicate 23 " " <> ") ENGINE=" <>Text.pack engineOpts <> "\");"]<>
  [ "if($err=mysqli_error($DB_link)) {"
  , "  $error=true; echo $err.'<br />';"
  , "}"
  , "" 
  ]<>setSqlModePHP


plug2TableSpec :: PlugSQL -> TableSpec
plug2TableSpec plug
 = ( unlines $ commentBlock (["Plug "<>name plug,"","attributes:"]<>map (\x->showADL (attExpr x)<>"  "<>(show.properties.attExpr) x) (plugAttributes plug))
   , name plug
   , [ quote (Text.pack$ attName f)<>" " <> Text.pack (showSQL (attType f)) <> " DEFAULT NULL"
     | f <- plugAttributes plug ]<>
      case (plug, (head.plugAttributes) plug) of
           (BinSQL{}, _      ) -> []
           (TblSQL{}, primAtt) ->
                case attUse primAtt of
                   PrimaryKey _ -> [ "PRIMARY KEY (`"<>Text.pack (attName primAtt)<>"`)"]
                   ForeignKey c  -> fatal 195 ("ForeignKey "<>name c<>"not expected here!")
                   PlainAttr     -> []
   , "InnoDB DEFAULT CHARACTER SET UTF8 DEFAULT COLLATE UTF8_BIN")

signalTableSpec :: TableSpec
signalTableSpec =
  ( "// Signal table"
  , "__all_signals__"
  , [ "`conjId` VARCHAR(255) NOT NULL"
    , "`src` VARCHAR(255) NOT NULL"
    , "`tgt` VARCHAR(255) NOT NULL" ]
  , "InnoDB DEFAULT CHARACTER SET UTF8 DEFAULT COLLATE UTF8_BIN"
  )

sessionTableSpec :: TableSpec
sessionTableSpec
 = ( "// Session timeout table"
   , "__SessionTimeout__"
   , [ "`SESSION` VARCHAR(255) UNIQUE NOT NULL"
     , "`lastAccess` BIGINT NOT NULL" ]
   , "InnoDB DEFAULT CHARACTER SET UTF8 DEFAULT COLLATE UTF8_BIN" )

populateTablesPHP :: FSpec -> [Text.Text]
populateTablesPHP fSpec =
  fillSignalTable (initialConjunctSignals fSpec) <>
  populateTablesWithInitialPopsPHP fSpec
  where
    fillSignalTable []          = []
    fillSignalTable conjSignals =
      [ "mysqli_query($DB_link, "<>showPhpStr ("INSERT INTO "<> quote (getTableName signalTableSpec)
                                                                    <>" (`conjId`, `src`, `tgt`)"
                                              <>phpIndent 24<>"VALUES " <> 
                                              Text.intercalate (phpIndent 29<>", ") 
                                                [ "(" <>sqlConjId<>", "<>showValPHP (apLeft p)<>", "<>showValPHP (apRight p)<>")" 
                                                | (conj, viols) <- conjSignals
                                                , let sqlConjId = "'" <> Text.pack (rc_id conj) <> "'" -- conjunct id's do not need escaping (SJ 2016-07-07: In that case: why not escape with showValPHP for the sake of maintainability?)
                                                , p <- viols
                                                ])<>"\n"<>
        "            );"
      , "if($err=mysqli_error($DB_link)) { $error=true; echo $err.'<br />'; }"
      ]

populateTablesWithInitialPopsPHP :: FSpec -> [Text.Text]
populateTablesWithInitialPopsPHP fSpec =
  concatMap populatePlugPHP [p | InternalPlug p <- plugInfos fSpec]
  where
    populatePlugPHP plug
         = case tableContents fSpec plug of
               [] -> []
               tblRecords -> ( "mysqli_query($DB_link, "<>showPhpStr ("INSERT INTO "<>quote (Text.pack (name plug))
                                                           <>" ("<>Text.intercalate "," [quote (Text.pack$ attName f) |f<-plugAttributes plug]<>")"
                                                           <>phpIndent 17<>"VALUES " <> Text.intercalate (phpIndent 22<>", ") [ "(" <>valuechain md<> ")" | md<-tblRecords]
                                                           <>phpIndent 16 )
                                        <>");"
                             ):
                             ["if($err=mysqli_error($DB_link)) { $error=true; echo $err.'<br />'; }"]
     where
        valuechain record = Text.intercalate ", " [case att of Nothing -> "NULL" ; Just val -> showValPHP val | att<-record]


sqlServerConnectPHP :: FSpec -> [Text.Text]
sqlServerConnectPHP fSpec =
  [ "// Try to connect to the database"
  , "global $DB_host,$DB_user,$DB_pass;"
  , "$DB_host='"<>subst sqlHost <>"';"
  , "$DB_user='"<>subst sqlLogin<>"';"
  , "$DB_pass='"<>subst sqlPwd  <>"';"
  , ""
  , "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass);"
  , "// Check connection"
  , "if (mysqli_connect_errno()) {"
  , "  die(\"Failed to connect to MySQL: \" . mysqli_connect_error());"
  , "}"
  , ""
  ]<> initMySQLGlobals
   <> setSqlModePHP
  where 
    subst :: (Options -> String) -> Text.Text
    subst x = addSlashes . Text.pack . x . getOpts $ fSpec
createTempDbPHP :: String -> [Text.Text]
createTempDbPHP dbNm =
      [ "$DB_name='"<>addSlashes (Text.pack dbNm)<>"';"
      , "// Drop the database if it exists"
      , "$sql=\"DROP DATABASE $DB_name\";"
      , "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass);"
      , "// Check connection"
      , "if (mysqli_connect_errno()) {"
      , "  die(\"Failed to connect to MySQL: \" . mysqli_connect_error());"
      , "}"
      , ""
      , "mysqli_query($DB_link,$sql);"
      , "// Don't bother about the error if the database didn't exist..."
      , ""
      , "// Create the database"
      , "$sql=\"CREATE DATABASE $DB_name DEFAULT CHARACTER SET UTF8 COLLATE utf8_bin\";"
      , "if (!mysqli_query($DB_link,$sql)) {"
      , "  die(\"Error creating the database: \" . mysqli_error($DB_link));"
      , "  }"
      , ""
      , "// Connect to the freshly created database"
      , "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass,$DB_name);"
      , "// Check connection"
      , "if (mysqli_connect_errno()) {"
      , "  die(\"Failed to connect to the database: \" . mysqli_connect_error());"
      , "  }"
      , ""
      ]<>setSqlModePHP


-- evaluate normalized exp in SQL
evaluateExpSQL :: FSpec -> String -> Expression -> IO [(String,String)]
evaluateExpSQL fSpec dbNm exp =
 do { -- verboseLn (getOpts fSpec) ("evaluateExpSQL fSpec "++showADL exp)
    ; -- verboseLn (getOpts fSpec) (intercalate "\n" . showPrf showADL . cfProof (getOpts fSpec)) exp
    ; -- verboseLn (getOpts fSpec) "End of proof"
    ; performQuery fSpec dbNm (Text.pack violationsQuery)
    }
 where violationsExpr = conjNF (getOpts fSpec) exp
       violationsQuery = prettySQLQuery 26 fSpec violationsExpr

performQuery :: FSpec -> String -> Text.Text -> IO [(String,String)]
performQuery fSpec dbNm queryStr =
 do { queryResult <- (executePHPStr . showPHP) php
    ; if "Error" `isPrefixOf` queryResult -- not the most elegant way, but safe since a correct result will always be a list
      then do verboseLn opts{verboseP=True} (Text.unpack$ "\n******Problematic query:\n"<>queryStr<>"\n******")
              fatal 141 $ "PHP/SQL problem: "<>queryResult
      else case reads queryResult of
             [(pairs,"")] -> return pairs
             _            -> fatal 143 $ "Parse error on php result: \n"<>unlines . indent 5 . lines $ queryResult
    } 
   where
    opts = getOpts fSpec
    php =
      [ "// Try to connect to the database"
      , "$DB_name='"<>addSlashes (Text.pack dbNm)<>"';"
      , "global $DB_host,$DB_user,$DB_pass;"
      , "$DB_host='"<>addSlashes (Text.pack (sqlHost opts))<>"';"
      , "$DB_user='"<>addSlashes (Text.pack (sqlLogin opts))<>"';"
      , "$DB_pass='"<>addSlashes (Text.pack (sqlPwd opts))<>"';"
      , "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass,$DB_name);"
      , ""
      , "// Check connection"
      , "if (mysqli_connect_errno()) {"
      , "  die(\"Error: Failed to connect to $DB_name: \" . mysqli_connect_error());"
      , "  }"
      , ""
      ]<>setSqlModePHP<>
      [ "$sql="<>showPhpStr queryStr<>";"
      , "$result=mysqli_query($DB_link,$sql);"
      , "if(!$result)"
      , "  die('Error '.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');"
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
    ; (tempPhpFile, temph) <- openTempFile tempdir "tmpPhpQueryOfAmpersand"
    ; Text.hPutStr temph phpStr
    ; hClose temph
    ; results <- executePHP tempPhpFile
    ; removeFile tempPhpFile
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


-- | php code snippet to set the sql_mode
setSqlModePHP :: [Text.Text]
setSqlModePHP = 
       [ "$sql=\"SET SESSION sql_mode = 'ANSI,TRADITIONAL'\";" -- ANSI because of the syntax of the generated SQL
                                                               -- TRADITIONAL because of some more safety
       , "if (!mysqli_query($DB_link,$sql)) {"
       , "  die(\"Error setting sql_mode: \" . mysqli_error($DB_link));"
       , "  }"
       , ""
       ]

tempDbName :: String
tempDbName = "ampersandTempDB"


createTempDatabase :: FSpec -> IO ()
createTempDatabase fSpec =
 do { result <- executePHPStr .
           showPHP $ phpStr
    ; unless (null result) $ verboseLn (getOpts fSpec) result
    }
   where phpStr = 
           sqlServerConnectPHP fSpec <>
           createTempDbPHP tempDbName <>
           createTablesPHP fSpec <>
           populateTablesWithInitialPopsPHP fSpec

