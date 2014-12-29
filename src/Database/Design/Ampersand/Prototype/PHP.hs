module Database.Design.Ampersand.Prototype.PHP 
         ( executePHP, showPHP, sqlServerConnectPHP, createTempDbPHP
         , createTablesPHP, populateTablesPHP, plug2TableSpec,dropplug,historyTableSpec,sessionTableSpec,TableSpec) where

import Prelude hiding (exp)
import Control.Exception
import Control.Monad
import Data.List
import System.Process
import System.IO hiding (hPutStr,hGetContents)
import System.Directory
import Database.Design.Ampersand hiding (putStr, origin)
import Database.Design.Ampersand.Prototype.RelBinGenBasics


fatal :: Int -> String -> a
fatal = fatalMsg "PHP"

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
        ] ++
        concatMap (createTablePHP . mkSignalTableSpec) (vconjs fSpec) ++
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
  [ replicate 23 ' ' ++ [pref] ++ " " ++ fld | (pref, fld) <- zip ('(' : repeat ',') crflds ] ++
  [replicate 23 ' ' ++ ") ENGINE=" ++engineOpts ++ "\");"]++
  [ "if($err=mysqli_error($DB_link)) {"
  , "  $error=true; echo $err.'<br />';"
  , "}"
  , "" ]


plug2TableSpec :: PlugSQL -> TableSpec
plug2TableSpec plug
 = ( unlines $ commentBlock (["Plug "++name plug,"","fields:"]++map (\x->showADL (fldexpr x)++"  "++show (multiplicities $ fldexpr x)) (plugFields plug))
   , name plug
   , [ quote (fldname f)++" " ++ showSQL (fldtype f) ++ (if fldauto f then " AUTO_INCREMENT" else " DEFAULT NULL")
     | f <- plugFields plug ]++
      case (plug, (head.plugFields) plug) of
           (BinSQL{}, _)   -> []
           (_,    primFld) ->
                case flduse primFld of
                   TableKey isPrim _ -> [ (if isPrim then "PRIMARY " else "")
                                          ++ "KEY (`"++fldname primFld++"`)"
                                        ]
                   ForeignKey c  -> fatal 195 ("ForeignKey "++name c++"not expected here!")
                   PlainAttr     -> []
   , "InnoDB DEFAULT CHARACTER SET UTF8")

mkSignalTableSpec :: Conjunct -> TableSpec
mkSignalTableSpec conj =
  ( "// Signal table for conjunct " ++ rc_id conj
  , "signals_" ++ rc_id conj
  , [ "`src` VARCHAR(255) NOT NULL"
    , "`tgt` VARCHAR(255) NOT NULL" ]
  , "InnoDB DEFAULT CHARACTER SET UTF8"
  )

sessionTableSpec :: TableSpec
sessionTableSpec
 = ( "// Session timeout table"
   , "__SessionTimeout__"
   , [ "`SESSION` VARCHAR(255) UNIQUE NOT NULL"
     , "`lastAccess` BIGINT NOT NULL" ]
   , "InnoDB DEFAULT CHARACTER SET UTF8" )

historyTableSpec :: TableSpec
historyTableSpec
 = ( "// Timestamp table"
   , "__History__"
   , [ "`Seconds` VARCHAR(255) DEFAULT NULL"
     , "`Date` VARCHAR(255) DEFAULT NULL" ]
   , "InnoDB DEFAULT CHARACTER SET UTF8" )

populateTablesPHP :: FSpec -> [String]
populateTablesPHP fSpec =
  concatMap fillSignalTable (initialConjunctSignals fSpec) ++
  concatMap populatePlugPHP [p | InternalPlug p <- plugInfos fSpec]
  where
    fillSignalTable (conj, viols) =
      [ "mysqli_query($DB_link, "++showPhpStr ("INSERT IGNORE INTO "++ quote (getTableName $ mkSignalTableSpec conj)
                                                                    ++" (`src`, `tgt`)"
                                              ++phpIndent 24++"VALUES " ++ 
                                              intercalate (phpIndent 29++", ") 
                                                [ "(" ++sqlAtomQuote src++", "++sqlAtomQuote tgt++")" 
                                                | (src, tgt) <- viols
                                                ])++"\n"++
        "            );"
      , "if($err=mysqli_error($DB_link)) { $error=true; echo $err.'<br />'; }"
      ]
    
    populatePlugPHP plug
         = case tblcontents (gens fSpec) (initialPops fSpec) plug of
               [] -> []
               tblRecords -> ( "mysqli_query($DB_link, "++showPhpStr ("INSERT IGNORE INTO "++quote (name plug)
                                                           ++" ("++intercalate "," [quote (fldname f) |f<-plugFields plug]++")"
                                                           ++phpIndent 17++"VALUES " ++ intercalate (phpIndent 22++", ") [ "(" ++valuechain md++ ")" | md<-tblRecords]
                                                           ++phpIndent 16 )
                                        ++");"
                             ):
                             ["if($err=mysqli_error($DB_link)) { $error=true; echo $err.'<br />'; }"]
     where
        valuechain record = intercalate ", " [case fld of Nothing -> "NULL" ; Just str -> sqlAtomQuote str | fld<-record]


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
  ]

createTempDbPHP :: String -> [String]
createTempDbPHP dbNm =
      [ "$DB_name='"++addSlashes dbNm++"';"
      , "// Drop the database if it exists"
      , "$sql=\"DROP DATABASE $DB_name\";"
      , "mysqli_query($DB_link,$sql);"
      , "// Don't bother about the error if the database didn't exist..."
      , ""
      , "// Create the database"
      , "$sql=\"CREATE DATABASE $DB_name DEFAULT CHARACTER SET UTF8\";"
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
      ]


-- call the command-line php with phpStr as input
executePHP :: String -> IO String
executePHP phpStr =
 do { tempdir <- catch getTemporaryDirectory
                       (\e -> do let err = show (e :: IOException)
                                 hPutStr stderr ("Warning: Couldn't find temp directory. Using current directory : " ++ err)
                                 return ".")

    ; (tempfile, temph) <- openTempFile tempdir "phpInput"
    ; hPutStr temph phpStr
    ; hClose temph

    ; let cp = CreateProcess
                { cmdspec       = RawCommand "php" [tempfile]
                , cwd           = Nothing -- path
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
    ; removeFile tempfile
--    ; putStrLn $ "Results:\n" ++ outputStr
    ; return outputStr
    }

showPHP :: [String] -> String
showPHP phpLines = unlines $ ["<?php"]++phpLines++["?>"]
