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
   (fillAtlas,picturesForAtlas,atlas2context)
where
import DatabaseDesign.Ampersand
import Database.HDBC.ODBC 
import Database.HDBC
import Data.List  (intercalate,nub)
import DatabaseDesign.Ampersand_Prototype.RelBinGenSQL (sqlRelPlugs,sqlPlugFields)
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

--select population from the atlas of this user
selects :: (IConnection conn) => conn -> [PlugSQL] -> IO [(PlugSQL,[[String]])]
selects _ [] = return []
selects conn (tbl:tbls) = 
   do rows <- quickQuery' conn ("SELECT * FROM "++name tbl++";") [] --REMARK quickQuery' is strict and needed to keep results for use after disconnecting
      xs <- selects conn tbls
      return ((tbl,map (map fromSql) rows):xs)

--rel is interpreted as a composition of decls or just one decl
--no need for more complex expressions, thus rel::[Relation Concept] instead of Expression(Relation Concept)
selrange conn tbls xfromdom rel =
   do rows <- if null stmt then return [] else quickQuery' conn stmt []
      return [fromSql y|row<-rows,length row==2,let [x,y]=row,fromSql x==xfromdom]
   where
   stmt = if null stmts then [] else head stmts    
   stmts = [selstmt tbl rel|tbl<-tbls] 
   loc r = let rellocs = [(tbl,head locs)|tbl<-tbls, let locs=sqlPlugFields tbl (Tm r (-1)), not(null locs)]
           in if null rellocs then error "rel not found" else head rellocs
   --selrstmt r = let (tbl,(fld1,fld2)) = loc r in "SELECT `" ++ name fld1 "`, `" ++ name fld2 "` FROM `" ++ name tbl ++ "`"
   selstmt tbl rs = let flds = nub(concat[if tbl==tbl' then [fldname fld1,fldname fld2] else error "tbl expected"|r<-rs,let (tbl',(fld1,fld2)) = loc r])
                    in  "SELECT `" ++ (intercalate "`, `" flds)++ "` FROM `" ++ name tbl ++ "`"
   --stmt = stmt' (selectExpr atlasfspec 0 s t expr) --selectExpr is mixed with PHP
   --stmt' (Just x) = x
   --stmt' Nothing = []
         
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
   do verboseLn flags "Connecting to atlas..."
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
readAtlas :: Fspc -> Options -> IO [(PlugSQL,[[(SqlField,String)]])]
readAtlas fSpec flags = 
   do verboseLn flags "Connecting to atlas..."
      conn<-connectODBC "DSN=atlas"
      verboseLn flags "Connected."
      verboseLn flags "Reading tables..."
      xs <- selects conn [p|InternalPlug p<-plugInfos fSpec]
      --verboseLn flags (show (length xs,map (name.fst) xs,length (map snd xs),[show rec|rec<-map snd xs]))
      disconnect conn
      verboseLn flags "Disconnected."
      return [(plug,[zip (fields plug) rec|rec<-recs])|(plug,recs)<-xs]

atomsof :: Fspc -> [(PlugSQL,[[(SqlField,String)]])] -> String -> [String]
atomsof fSpec tbls cptname 
   = [x|tbl<-tbls
       ,(plug,_,fld)<-sqlRelPlugs fSpec (Tm (mIs (cptnew cptname)) (-1))
       ,plug==fst tbl,rec<-snd tbl,(fld',x)<-rec,fld==fld']

atlas2context :: Fspc -> Options -> IO Context
atlas2context fSpec flags =
   do tbls <- readAtlas fSpec flags
      return (Ctx ((\xs-> if null xs then error "context?" else head xs)(atomsof fSpec tbls "Context"))
                  [] empty []
                  (atlas2patterns fSpec tbls)
                  [] []
                  [] --in pattern:(atlas2rules fSpec tbls)
                  [] --in pattern:(atlas2decls fSpec tbls)
                  [] --in pattern?:(atlas2concepts fSpec tbls)
                  [] []
                  [] --in pattern for rul and decl (and cpt?):(atlas2expls fSpec tbls)
                  (atlas2pops fSpec tbls)
                  [] [] (Tm(V [] (cptAnything,cptAnything)) (-1),[])
              )
atlas2patterns :: Fspc -> [(PlugSQL,[[(SqlField,String)]])] -> [Pattern]
atlas2patterns fSpec tbls 
 = [Pat { ptnm  = p
        , ptrls = []
        , ptgns = []
        , ptdcs = []
        , ptcds = []
        , ptkds = []
        , ptxps = []
        , testexpr = []
        , inftestexpr = []
        }
   |p<-atomsof fSpec tbls "Pattern"]
atlas2rules :: Fspc -> [(PlugSQL,[[(SqlField,String)]])] -> [Rule (Relation Concept)]
atlas2rules fSpec tbls = []
atlas2decls :: Fspc -> [(PlugSQL,[[(SqlField,String)]])] -> [Declaration Concept]
atlas2decls fSpec tbls = []
atlas2concepts :: Fspc -> [(PlugSQL,[[(SqlField,String)]])] -> [ConceptDef]
atlas2concepts fSpec tbls = []
atlas2expls :: Fspc -> [(PlugSQL,[[(SqlField,String)]])] -> [PExplanation]
atlas2expls fSpec tbls = []
atlas2pops :: Fspc -> [(PlugSQL,[[(SqlField,String)]])] -> [Population Concept]
atlas2pops fSpec tbls = []
