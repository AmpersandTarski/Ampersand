module Database.Design.Ampersand.Prototype.PHP 
         ( executePHPStr, executePHP, showPHP, sqlServerConnectPHP, createTempDbPHP, setSqlModePHP
         , evaluateExpSQL, performQuery
         , createTablesPHP, populateTablesPHP, populateTablesWithInitialPopsPHP, plug2TableSpec
         , dropplug, historyTableSpec, sessionTableSpec, signalTableSpec, TableSpec, getTableName) where

import Prelude hiding (exp)
import Control.Exception
import Control.Monad
import Data.List
import System.Process
import System.IO hiding (hPutStr,hGetContents)
import System.Directory
import Database.Design.Ampersand hiding (putStr, origin)
import Database.Design.Ampersand.Prototype.ProtoUtil
import Database.Design.Ampersand.FSpec.SQL
import Database.Design.Ampersand.Basics (fatal)

createTablesPHP :: FSpec -> [String]
createTablesPHP fSpec =
        [ "/*** Create new SQL tables ***/"
        , ""
        ] ++
        concatMap createTablePHP [sessionTableSpec, historyTableSpec] ++
        [ "$time = explode(' ', microTime()); // copied from DatabaseUtils setTimestamp"
        , "$microseconds = substr($time[0], 2,6);"
        , "$seconds =$time[1].$microseconds;"
        , "date_default_timezone_set(\"Europe/Amsterdam\");"
        -- to prevent a php warning TODO: check if this is ok when Ampersand is used in different timezones
        , "$date = date(\"j-M-Y, H:i:s.\").$microseconds;"
        , "mysqli_query($DB_link, \"INSERT INTO `__History__` (`Seconds`,`Date`) VALUES ('$seconds','$date')\");"
        , "if($err=mysqli_error($DB_link)) {"
        , "  $error=true; echo $err.'<br />';"
        , "}"
        , ""
        ] ++setSqlModePHP++
        createTablePHP signalTableSpec ++
        [ ""
        , "//// Number of plugs: " ++ show (length (plugInfos fSpec))
        ]
        -- Create all plugs
        ++ concatMap (createTablePHP . plug2TableSpec) [p | InternalPlug p <- plugInfos fSpec]

--                 (headerCmmnt,tableName,crflds,engineOpts)
type TableSpec = (String,String,[String],String)

getTableName :: TableSpec -> String
getTableName (_,nm,_,_) = nm

createTablePHP :: TableSpec -> [String]
createTablePHP (headerCmmnt,tableName,crflds,engineOpts) =
  [ headerCmmnt
  -- Drop table if it already exists
  , "if($columns = mysqli_query($DB_link, "++showPhpStr ("SHOW COLUMNS FROM `"++tableName++"`")++")){"
  , "    mysqli_query($DB_link, "++showPhpStr ("DROP TABLE `"++tableName++"`")++");"
  , "}"
  ] ++
  [ "mysqli_query($DB_link,\"CREATE TABLE `"++tableName++"`"] ++
  [ replicate 23 ' ' ++ [pref] ++ " " ++ att | (pref, att) <- zip ('(' : repeat ',') crflds ] ++
  [ replicate 23 ' ' ++ ") ENGINE=" ++engineOpts ++ "\");"]++
  [ "if($err=mysqli_error($DB_link)) {"
  , "  $error=true; echo $err.'<br />';"
  , "}"
  , "" 
  ]++setSqlModePHP


plug2TableSpec :: PlugSQL -> TableSpec
plug2TableSpec plug
 = ( unlines $ commentBlock (["Plug "++name plug,"","attributes:"]++map (\x->showADL (attExpr x)++"  "++(show.properties.attExpr) x) (plugAttributes plug))
   , name plug
   , [ quote (attName f)++" " ++ showSQL (attType f) ++ (if fldauto f then " AUTO_INCREMENT" else " DEFAULT NULL")
     | f <- plugAttributes plug ]++
      case (plug, (head.plugAttributes) plug) of
           (BinSQL{}, _)   -> []
           (_,    primAtt) ->
                case attUse primAtt of
                   TableKey isPrim _ -> [ (if isPrim then "PRIMARY " else "")
                                          ++ "KEY (`"++attName primAtt++"`)"
                                        ]
                   ForeignKey c  -> fatal 195 ("ForeignKey "++name c++"not expected here!")
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

historyTableSpec :: TableSpec
historyTableSpec
 = ( "// Timestamp table"
   , "__History__"
   , [ "`Seconds` VARCHAR(255) DEFAULT NULL"
     , "`Date` VARCHAR(255) DEFAULT NULL" ]
   , "InnoDB DEFAULT CHARACTER SET UTF8 DEFAULT COLLATE UTF8_BIN" )

populateTablesPHP :: FSpec -> [String]
populateTablesPHP fSpec =
  fillSignalTable (initialConjunctSignals fSpec) ++
  populateTablesWithInitialPopsPHP fSpec
  where
    fillSignalTable []          = []
    fillSignalTable conjSignals =
      [ "mysqli_query($DB_link, "++showPhpStr ("INSERT INTO "++ quote (getTableName signalTableSpec)
                                                                    ++" (`conjId`, `src`, `tgt`)"
                                              ++phpIndent 24++"VALUES " ++ 
                                              intercalate (phpIndent 29++", ") 
                                                [ "(" ++sqlConjId++", "++showValPHP (apLeft p)++", "++showValPHP (apRight p)++")" 
                                                | (conj, viols) <- conjSignals
                                                , let sqlConjId = "'" ++ rc_id conj ++ "'" -- conjunct id's do not need escaping
                                                , p <- viols
                                                ])++"\n"++
        "            );"
      , "if($err=mysqli_error($DB_link)) { $error=true; echo $err.'<br />'; }"
      ]

populateTablesWithInitialPopsPHP :: FSpec -> [String]
populateTablesWithInitialPopsPHP fSpec =
  concatMap populatePlugPHP [p | InternalPlug p <- plugInfos fSpec]
  where
    populatePlugPHP plug
         = case tableContents fSpec plug of
               [] -> []
               tblRecords -> ( "mysqli_query($DB_link, "++showPhpStr ("INSERT INTO "++quote (name plug)
                                                           ++" ("++intercalate "," [quote (attName f) |f<-plugAttributes plug]++")"
                                                           ++phpIndent 17++"VALUES " ++ intercalate (phpIndent 22++", ") [ "(" ++valuechain md++ ")" | md<-tblRecords]
                                                           ++phpIndent 16 )
                                        ++");"
                             ):
                             ["if($err=mysqli_error($DB_link)) { $error=true; echo $err.'<br />'; }"]
     where
        valuechain record = intercalate ", " [case att of Nothing -> "NULL" ; Just val -> showValPHP val | att<-record]


dropplug :: PlugSQL -> String
dropplug plug = "DROP TABLE "++quote (name plug)++""

sqlServerConnectPHP :: FSpec -> [String]
sqlServerConnectPHP fSpec =
  [ "// Try to connect to the database"
  , "global $DB_host,$DB_user,$DB_pass;"
  , "$DB_host='"++addSlashes (sqlHost (getOpts fSpec))++"';"
  , "$DB_user='"++addSlashes (sqlLogin (getOpts fSpec))++"';"
  , "$DB_pass='"++addSlashes (sqlPwd (getOpts fSpec))++"';"
  
  , "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass);"
  , "// Check connection"
  , "if (mysqli_connect_errno()) {"
  , "  die(\"Failed to connect to MySQL: \" . mysqli_connect_error());"
  , "}"
  , ""
  ]++setSqlModePHP

createTempDbPHP :: String -> [String]
createTempDbPHP dbNm =
      [ "$DB_name='"++addSlashes dbNm++"';"
      , "// Drop the database if it exists"
      , "$sql=\"DROP DATABASE $DB_name\";"
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
      ]++setSqlModePHP


-- evaluate normalized exp in SQL
evaluateExpSQL :: FSpec -> String -> Expression -> IO [(String,String)]
evaluateExpSQL fSpec dbNm exp =
  fmap sort (performQuery (getOpts fSpec) dbNm violationsQuery)
 where violationsExpr = conjNF (getOpts fSpec) exp
       violationsQuery = prettySQLQuery fSpec 26 violationsExpr

performQuery :: Options -> String -> String -> IO [(String,String)]
performQuery opts dbNm queryStr =
 do { queryResult <- (executePHPStr . showPHP) php
    ; if "Error" `isPrefixOf` queryResult -- not the most elegant way, but safe since a correct result will always be a list
      then do verboseLn opts{verboseP=True} ("\n******Problematic query:\n"++queryStr++"\n******")
              fatal 141 $ "PHP/SQL problem: "++queryResult
      else case reads queryResult of
             [(pairs,"")] -> return pairs
             _            -> fatal 143 $ "Parse error on php result: "++show queryResult
    }
   where
    php =
      [ "// Try to connect to the database"
      , "$DB_name='"++addSlashes dbNm++"';"
      , "global $DB_host,$DB_user,$DB_pass;"
      , "$DB_host='"++addSlashes (sqlHost opts)++"';"
      , "$DB_user='"++addSlashes (sqlLogin opts)++"';"
      , "$DB_pass='"++addSlashes (sqlPwd opts)++"';"
      , "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass,$DB_name);"
      , "// Check connection"
      , "if (mysqli_connect_errno()) {"
      , "  die(\"Error: Failed to connect to $DB_name: \" . mysqli_connect_error());"
      , "  }"
      , ""
      ]++setSqlModePHP++
      [ "$sql="++showPhpStr queryStr++";"
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
executePHPStr :: String -> IO String
executePHPStr phpStr =
 do { tempdir <- catch getTemporaryDirectory
                       (\e -> do let err = show (e :: IOException)
                                 hPutStr stderr ("Warning: Couldn't find temp directory. Using current directory : " ++ err)
                                 return ".")

    ; (tempPhpFile, temph) <- openTempFile tempdir "phpInput"
    ; hPutStr temph phpStr
    ; hClose temph
    ; results <- executePHP Nothing tempPhpFile []
    ; removeFile tempPhpFile
    ; return results
    }
    
executePHP :: Maybe String -> String -> [String] -> IO String
executePHP mWorkingDir phpPath phpArgs =
 do { let cp = CreateProcess
                { cmdspec       = RawCommand "php" $ phpPath : phpArgs
                , cwd           = mWorkingDir
                , env           = Just [("TERM","dumb")] -- environment
                , std_in        = Inherit
                , std_out       = CreatePipe
                , std_err       = CreatePipe
                , close_fds     = False -- no need to close all other file descriptors
                , create_group  = False
                , delegate_ctlc = False -- don't let php handle ctrl-c
                }

    ; (_, mStdOut, mStdErr, _) <- createProcess cp
    ; outputStr <-
        case (mStdOut, mStdErr) of
          (Nothing, _) -> fatal 44 "no output handle"
          (_, Nothing) -> fatal 45 "no error handle"
          (Just stdOutH, Just stdErrH) ->
           do { --putStrLn "done"
              ; errStr <- hGetContents stdErrH
              ; seq (length errStr) $ return ()
              ; hClose stdErrH
              ; unless (null errStr) $
                  putStrLn $ "Error during PHP execution:\n" ++ errStr
              ; outputStr' <- hGetContents stdOutH --and fetch the results from the output pipe
              ; seq (length outputStr') $ return ()
              ; hClose stdOutH
              ; return outputStr'
              }
--    ; putStrLn $ "Results:\n" ++ outputStr
    ; return outputStr
    }

showPHP :: [String] -> String
showPHP phpLines = unlines $ ["<?php"]++phpLines++["?>"]


-- | php code snippet to set the sql_mode
setSqlModePHP :: [String]
setSqlModePHP = 
       [ "$sql=\"SET SESSION sql_mode = 'ANSI,TRADITIONAL'\";" -- ANSI because of the syntax of the generated SQL
                                                               -- TRADITIONAL because of some more safety
       , "if (!mysqli_query($DB_link,$sql)) {"
       , "  die(\"Error setting sql_mode: \" . mysqli_error($DB_link));"
       , "  }"
       , ""
       ]

