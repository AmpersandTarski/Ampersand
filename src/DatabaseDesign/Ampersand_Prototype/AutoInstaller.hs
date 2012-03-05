{-# OPTIONS_GHC -Wall #-}  
--hdbc and hdbc-odbc must be installed (from hackage)
--Resembles running Installer.php rev:1770 only at compile time through an ODBC connection with DSN=dsn
--the connection via ODBC should be the same as the connection via dbSettings.php
--Difference with Installer.php rev:1770:
-- + odbcinstall expects the database to exist (no CREATE DATABASE)
-- + only tables for plugs are created, not the additional ones i.e. __SessionTimeout__ and __History__
--Thus, before running odbcinstall, create an ODBC connection, the database and special tables.
module DatabaseDesign.Ampersand_Prototype.AutoInstaller (odbcinstall)
where 
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import DatabaseDesign.Ampersand_Prototype.Installer (createplug,dropplug)
import Database.HDBC.ODBC 
import Database.HDBC
import Data.List  (intercalate)
-- import DatabaseDesign.Ampersand.Version (fatalMsg)

--fatal :: Int -> String -> a
--fatal = fatalMsg "AutoInstaller"

odbcinstall :: Options -> Fspc -> String -> IO()
odbcinstall flags fSpec dsn = 
   do verboseLn flags ("Connecting to ODBC connection "++ dsn ++"...")
      conn<-connectODBC dsn
      verboseLn flags "Connected."
      verboseLn flags "Dropping tables..."
      _ <- drops conn [p | InternalPlug p<-plugInfos fSpec]
      verboseLn flags "Creating tables..."
      _ <- creates conn [p |InternalPlug p<-plugInfos fSpec]
      verboseLn flags "Populating tables..."
      _ <- inserts conn [p |InternalPlug p<-plugInfos fSpec]
      commit conn
      verboseLn flags "Committed."
      disconnect conn

--drop tables
--
--IGNORES all SQL errors!!!
drops :: (IConnection conn) => conn -> [PlugSQL] -> IO Integer
drops _ [] = return 1
drops conn (plug:plugs)  = 
   do _ <- handleSql (\_ -> return 0) (run conn (dropplug plug) [])
      drops conn plugs

--insert population
inserts :: (IConnection conn) => conn -> [PlugSQL] -> IO Integer
inserts _ [] = return 1
inserts conn (plug:plugs) = 
   do stmt<- prepare conn
             ("INSERT INTO "++name plug++"("++intercalate "," ["`"++fldname f++"` " |f<-tblfields plug]++")"
                                ++" VALUES ("++placeholders(tblfields plug)++")")
      executeMany stmt (map (map toSql) (tblcontents plug))
      inserts conn plugs
   where 
   placeholders :: [a] -> String
   placeholders [] = []
   placeholders (_:[]) = "?"
   placeholders (_:xs) = "?," ++ placeholders xs
         
--create tables
creates :: (IConnection conn) => conn -> [PlugSQL] -> IO Integer
creates _ [] = return 1
creates conn (plug:plugs) = 
   do _ <- run conn (crtbl ++ concat crflds ++ crengine) []
      creates conn plugs
   where (crtbl,crflds,crengine) = createplug plug
