--hdbc and hdbc-odbc must be installed (from hackage)
--Strongly resembles running Installer.php rev:1770 only at compile time through an ODBC connection with DSN=dsn
--the connection via ODBC should be the same as the connection via dbSettings.php
--Difference with Installer.php rev:1770:
-- + odbcinstall expects the database to exist (no CREATE DATABASE)
-- + no row inserted into __History__ at odbcinstall
module Database.Design.Ampersand.Prototype.AutoInstaller (odbcinstall)
where
import Database.Design.Ampersand.Prototype.CoreImporter
import Database.Design.Ampersand.Prototype.Installer (plug2TableSpecl,dropplug,TableSpec,sessionTableSpec,historyTableSpec)
import Database.Design.Ampersand.Prototype.RelBinGenBasics(quote)
import Database.HDBC.ODBC
import Database.HDBC
import Data.List  (intercalate)
-- import Database.Design.Ampersand.Version (fatalMsg)

--fatal :: Int -> String -> a
--fatal = fatalMsg "AutoInstaller"

odbcinstall :: FSpec -> String -> IO()
odbcinstall fSpec dsn =
   do verboseLn (getOpts fSpec) ("Connecting to ODBC connection "++ dsn ++"...")
      conn<-connectODBC dsn
      verboseLn (getOpts fSpec) "Connected."
      verboseLn (getOpts fSpec) "Dropping tables..."
      _ <- drops conn ("DROP TABLE `__History__`":"DROP TABLE `__SessionTimeout__`":[dropplug p | InternalPlug p<-plugInfos fSpec])
      verboseLn (getOpts fSpec) "Creating tables..."
      _ <- creates conn (historyTableSpec : sessionTableSpec : [plug2TableSpecl p |InternalPlug p<-plugInfos fSpec])
      verboseLn (getOpts fSpec) "Populating tables..."
      _ <- inserts conn (gens fSpec)(initialPops fSpec) [p |InternalPlug p<-plugInfos fSpec]
      commit conn
      verboseLn (getOpts fSpec) "Committed."
      disconnect conn

--drop tables
--IGNORES all SQL errors!!!
drops :: (IConnection conn) => conn -> [String] -> IO Integer
drops _ [] = return 1
drops conn (x:xs)  =
   do _ <- handleSql (\_ -> return 0) (run conn x [])
      drops conn xs

--insert population
inserts :: (IConnection conn) => conn -> [A_Gen] -> [Population] -> [PlugSQL] -> IO Integer
inserts _ _ _ [] = return 1
inserts conn a_gens udp (plug:plugs) =
   do stmt<- prepare conn
             ("INSERT INTO "++quote (name plug)++" ("++intercalate "," [quote (fldname f)++" " |f<-plugFields plug]++")"
                                ++" VALUES ("++placeholders(plugFields plug)++")")
      executeMany stmt [ map toSql tblRecord | tblRecord<-tblcontents a_gens udp plug]
      inserts conn a_gens udp plugs
   where
   placeholders :: [a] -> String
   placeholders [] = []
   placeholders (_:[]) = "?"
   placeholders (_:xs) = "?," ++ placeholders xs

--create tables
creates :: (IConnection conn) => conn -> [TableSpec] -> IO Integer
creates _ [] = return 1
creates conn ((_,crtbl,crflds,crengine):tbls) =
   do _ <- run conn (crtbl ++ concat crflds ++ crengine) []
      creates conn tbls
