{-# OPTIONS_GHC -Wall #-}  
--hdbc and hdbc-odbc must be installed (from hackage)
--Strongly resembles running Installer.php rev:1770 only at compile time through an ODBC connection with DSN=dsn
--the connection via ODBC should be the same as the connection via dbSettings.php
--Difference with Installer.php rev:1770:
-- + odbcinstall expects the database to exist (no CREATE DATABASE)
-- + no row inserted into __History__ at odbcinstall
module DatabaseDesign.Ampersand_Prototype.AutoInstaller (odbcinstall)
where 
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import DatabaseDesign.Ampersand_Prototype.Installer (plug2tbl,dropplug,CreateTable,sessiontbl,historytbl)
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
      _ <- drops conn ("DROP TABLE `__History__`":"DROP TABLE `__SessionTimeout__`":[dropplug p | InternalPlug p<-plugInfos fSpec])
      verboseLn flags "Creating tables..."
      _ <- creates conn (historytbl : sessiontbl : [plug2tbl p |InternalPlug p<-plugInfos fSpec])
      verboseLn flags "Populating tables..."
      _ <- inserts conn (gens fSpec)(userDefPops fSpec) [p |InternalPlug p<-plugInfos fSpec]
      commit conn
      verboseLn flags "Committed."
      disconnect conn

--drop tables
--IGNORES all SQL errors!!!
drops :: (IConnection conn) => conn -> [String] -> IO Integer
drops _ [] = return 1
drops conn (x:xs)  = 
   do _ <- handleSql (\_ -> return 0) (run conn x [])
      drops conn xs

--insert population
inserts :: (IConnection conn) => conn -> [A_Gen] -> [UserDefPop] -> [PlugSQL] -> IO Integer
inserts _ _ _ [] = return 1
inserts conn a_gens udp (plug:plugs) = 
   do stmt<- prepare conn
             ("INSERT INTO `"++name plug++"` ("++intercalate "," ["`"++fldname f++"` " |f<-plugFields plug]++")"
                                ++" VALUES ("++placeholders(plugFields plug)++")")
      executeMany stmt (map (map (toSql . mbnullstring)) (tblcontents a_gens udp plug))
      inserts conn a_gens udp plugs
   where 
   -- empty string = Nothing => toSql Nothing = NULL
   mbnullstring [] = Nothing
   mbnullstring x = Just x
   placeholders :: [a] -> String
   placeholders [] = []
   placeholders (_:[]) = "?"
   placeholders (_:xs) = "?," ++ placeholders xs
         
--create tables
creates :: (IConnection conn) => conn -> [CreateTable] -> IO Integer
creates _ [] = return 1
creates conn ((crtbl,crflds,crengine):tbls) = 
   do _ <- run conn (crtbl ++ concat crflds ++ crengine) []
      creates conn tbls
