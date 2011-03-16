{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}  
--hdbc and hdbc-odbc must be installed (from hackage)
--
--running PHP in IIS on the php.exe of XAMPP requires setting "cgi.force_redirect = 0" in the php.ini
--in IIS you can enable windows authentication
--
--like Installer.php
--thus:
-- 1) DROP IGNORE TABLES (sqlplugs fspec)
-- 2) CREATE TABLES (sqlplugs fspec)
-- 3) INSERT INTO TABLES (tblcontents sqlplug)
--the connection should be the same as the one in dbsettings.php
--dbsettings.php connects directly, this module through a DSN=atlas
--
--the atlas has two outputs: a database and pictures
--the database contains links to the pictures (see Main.hs)
module DatabaseDesign.Ampersand_Prototype.Apps.Atlas 
   (fillAtlas,picturesForAtlas)
where
import DatabaseDesign.Ampersand
import Database.HDBC.ODBC 
import Database.HDBC
import Data.List  (intercalate)
------
--runMany IGNORES all SQL errors!!!
--used to DROP tables if exist
runMany :: (IConnection conn) => conn -> [String] -> IO Integer
runMany _ [] = return 1
runMany conn (x:xs)  = 
   do _ <- handleSql (\_ -> return 0) (run conn x [])
      runMany conn xs

placeholders :: [a] -> String
placeholders [] = []
placeholders (_:[]) = "?"
placeholders (_:xs) = "?," ++ placeholders xs

--insert population of this Ampersand script of this user
inserts :: (IConnection conn) => conn -> [PlugSQL] -> IO Integer
inserts _ [] = return 1
inserts conn (tbl:tbls) = 
   do stmt<- prepare conn
             ("INSERT INTO "++name tbl++"("++intercalate "," ["`"++fldname f++"` "|f<-tblfields tbl]++")"
                                ++" VALUES ("++placeholders(tblfields tbl)++")")
      executeMany stmt (map (map toSql) (tblcontents tbl))
      inserts conn tbls

--create atlas tables for this namespace
creates :: (IConnection conn) => conn -> [PlugSQL] -> IO Integer
creates _ [] = return 1
creates conn (tbl:tbls) = 
   do _ <- run conn stmt []
      creates conn tbls
   where stmt = ("CREATE TABLE "++name tbl
               ++"("++intercalate "," 
                      ([createfld f|f<-tblfields tbl]
                     ++[" UNIQUE KEY (`"++fldname key++"`)"
                       | key <- tblfields tbl, flduniq key, not (fldnull key)]
                     ++[" UNIQUE INDEX (`"++fldname kernelfld++"`)" 
                       | kernelfld <- tblfields tbl, flduniq kernelfld, fldnull kernelfld])
               ++") TYPE=InnoDB DEFAULT CHARACTER SET latin1 COLLATE latin1_bin ")
         createfld fld = "`"++fldname fld++"` " ++ showSQL (fldtype fld) ++ autoIncr fld ++ nul fld
         nul fld = if fldnull fld then "" else " NOT NULL"
         autoIncr fld = if fldauto fld then " AUTO_INCREMENT" else ""

----------------------
fillAtlas :: Fspc -> Options -> IO()
fillAtlas fSpec flags = 
   do verboseLn flags "Generating pictures for atlas..."
      sequence_ [writePicture flags pict | pict <- picturesForAtlas flags fSpec]
      verboseLn flags "Connecting to atlas..."
      conn<-connectODBC "DSN=atlas"
      verboseLn flags "Connected."
      _ <- runMany conn ["DROP TABLE "++name p| InternalPlug p<-plugInfos fSpec]
      verboseLn flags "Creating tables..."
      _ <- creates conn [p|InternalPlug p<-plugInfos fSpec]
      verboseLn flags "Populating tables..."
      _ <- inserts conn [p|InternalPlug p<-plugInfos fSpec]
      commit conn
      verboseLn flags "Committed."
      disconnect conn

picturesForAtlas :: Options -> Fspc -> [Picture]
picturesForAtlas flags fSpec
   = [makePicture flags fSpec p | p <- patterns fSpec] ++
     [makePicture flags fSpec userRule | userRule <- rules fSpec]++
     [makePicture flags fSpec cpt | cpt <- (concs fSpec)]

----------------------------------------------------

