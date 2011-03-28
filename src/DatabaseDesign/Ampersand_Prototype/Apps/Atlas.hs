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
--selects :: (IConnection conn) => conn -> [PlugSQL] -> IO [(PlugSQL,[[String]])]
--selects _ [] = return []
--selects conn (tbl:tbls) = 
--   do rows <- quickQuery' conn ("SELECT * FROM "++name tbl++";") [] --REMARK quickQuery' is strict and needed to keep results for use after disconnecting
--      xs <- selects conn tbls
--      return ((tbl,map (map fromSql) rows):xs)

--rel is interpreted as a composition of decls or just one decl
	--no need for more complex expressions, thus rel::[Relation Concept] instead of Expression(Relation Concept)
selrange :: (IConnection conn) => conn -> [PlugSQL] -> String -> [Relation Concept] -> IO [String]
selrange conn tbls xfromdom rel =
   do xys <- selrel conn tbls rel
      return [y|(x,y)<-xys,null xfromdom || x==xfromdom] --null xfromdom implies any x from domain (see selatomsof)
   
--rel is interpreted as a composition of decls or just one decl
	--no need for more complex expressions, thus rel::[Relation Concept] instead of Expression(Relation Concept)
selrel :: (IConnection conn) => conn -> [PlugSQL] -> [Relation Concept] -> IO [(String,String)]
selrel conn tbls rel =
   do rows <- if null stmt then return [] else quickQuery' conn stmt []
      return [(fromSql x,fromSql y)|row<-rows,not(null row),let x=head row, let y=last row
             ]
   where
   stmt = if null stmts then [] else head stmts    
   stmts = [x|tbl<-tbls, let x=selstmt tbl rel, not(null x)] 
   loc r = let rellocs = [(tbl,head locs)|tbl<-tbls, let locs=sqlPlugFields tbl (Tm r (-1)), not(null locs)]
           in if null rellocs then error "rel not found" else head rellocs
   --selrstmt r = let (tbl,(fld1,fld2)) = loc r in "SELECT `" ++ name fld1 "`, `" ++ name fld2 "` FROM `" ++ name tbl ++ "`"
   selstmt tbl rs = let flds = nub(concat[[fldname fld1,fldname fld2]|r<-rs,let (tbl',(fld1,fld2)) = loc r,tbl==tbl'])
                    in  if null flds then [] else "SELECT `" ++ (intercalate "`, `" flds)++ "` FROM `" ++ name tbl ++ "`"
   --stmt = stmt' (selectExpr atlasfspec 0 s t expr) --selectExpr is mixed with PHP
   --stmt' (Just x) = x
   --stmt' Nothing = []
selatomsof :: (IConnection conn) => conn -> [PlugSQL] -> String -> IO [String]
selatomsof conn tbls cptname = let cpt=cptnew cptname in selrange conn tbls [] [mIs cpt]
         
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
         createfld fld = "`"++fldname fld++"` " 
                            --TODO -> Concepts should be attached to a SQL type. 
                            --        A concept::SQLText cannot be stored in a KEY or INDEX field i.e. the scalar plug cannot be created for such a concept
                            ++ if atlastxt fld then showSQL SQLText else showSQL (fldtype fld)
                            ++ autoIncr fld ++ nul fld
         nul fld = if fldnull fld then "" else " NOT NULL"
         autoIncr fld = if fldauto fld then " AUTO_INCREMENT" else ""
         atlastxt fld = not (flduniq fld) && elem ((name.target.fldexpr) fld) ["CptPurpose","RelPurpose","Explanation","PatPurpose"]

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
--readAtlas :: Fspc -> Options -> IO [(PlugSQL,[[(SqlField,String)]])]
--readAtlas fSpec flags = 
  -- do verboseLn flags "Connecting to atlas..."
    --  conn<-connectODBC "DSN=atlas"
      --verboseLn flags "Connected."
--      verboseLn flags "Reading tables..."
  --    xs <- selects conn [p|InternalPlug p<-plugInfos fSpec]
      --verboseLn flags (show (length xs,map (name.fst) xs,length (map snd xs),[show rec|rec<-map snd xs]))
    --  xxx<-selrange conn [p|InternalPlug p<-plugInfos fSpec] "ctxAtlas" [flp(makeRelation d)|d<-declarations fSpec,name d=="context"]
      --verboseLn flags (show xxx)
--      disconnect conn
  --    verboseLn flags "Disconnected."
    --  return [(plug,[zip (fields plug) rec|rec<-recs])|(plug,recs)<-xs]

--atomsof :: Fspc -> [(PlugSQL,[[(SqlField,String)]])] -> String -> [String]
--atomsof fSpec tbls cptname 
--   = [x|tbl<-tbls
--       ,(plug,_,fld)<-sqlRelPlugs fSpec (Tm (mIs (cptnew cptname)) (-1))
--       ,plug==fst tbl,rec<-snd tbl,(fld',x)<-rec,fld==fld']

atlas2context :: Fspc -> Options -> IO Context
atlas2context fSpec flags =
   do --tbls <- readAtlas fSpec flags
      verboseLn flags "Connecting to atlas..."
      conn<-connectODBC "DSN=atlas"
      verboseLn flags "Connected."
      -----------
      --select (strict) everything you need, then disconnect, then assemble it into a context and patterns and stuff
      --Context--
      cxs <- selatomsof conn tbls "Context"
      pats <- selrange conn tbls (ctxname cxs) [flp(makeRelation d)|d<-declarations fSpec,name d=="context"]
      --Relation
      relname <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="rel"]
      relsc <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="src"]
      reltg <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="trg"]
      relprp <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="propertyof"]
      propsyntax <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="propsyntax"]
      pragma1 <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="pragma1"]
      pragma2 <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="pragma2"]
      pragma3 <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="pragma3"]
      --Population--
      relcontent <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="content"]
      pairleft <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="left"]
      pairright <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="right"]
      atomsyntax <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="atomsyntax"]
      --Rule--
      ruleexpr <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="ruleexpr"]
      --Pattern--
      rulpattern <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="rulpattern"]
      isapattern <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="isapattern"]
      relpattern <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="relpattern"]
      --PExplainable--
      patpurpose <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="purpose",name(source d)=="Pattern"]
      rulpurpose <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="purpose",name(source d)=="UserRule"]
      relpurpose <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="purpose",name(source d)=="Relation"]
      cptpurpose <- selrel conn tbls [makeRelation d|d<-declarations fSpec,name d=="purpose",name(source d)=="Concept"]
      -----------
      disconnect conn
      verboseLn flags "Disconnected."
      rls<-parserules rulpattern ruleexpr
      --verboseLn flags (show(map showADL (atlas2pops relcontent relname relsc reltg  pairleft pairright atomsyntax)))
      (thectx,errs)<-makectx cxs pats rls rulpattern relpattern relname relsc reltg relcontent pairleft pairright atomsyntax relprp propsyntax pragma1 pragma2 pragma3
      if null errs then return thectx else error (show errs)
   where
   --parserule :: String -> String -> IO [Rule(Relation Concept)]
   parserules rulpattern ruleexpr = sequence [parseADL1Rule ("RULE \""++r++"\":"++ (geta ruleexpr r))|(r,p)<-rulpattern]
   ctxname cxs = if null cxs then error "context?" else head cxs
   tbls = [p|InternalPlug p<-plugInfos fSpec]
   makectx cxs pats rls rulpattern relpattern relname relsc reltg relcontent pairleft pairright atomsyntax relprp propsyntax pragma1 pragma2 pragma3
     = if null xs then error (show errs) else return (head xs,errs)
       where 
       (xs,errs) = typecheckAdl1 (Arch [rawctx]) []
       rawctx 
        = (Ctx 
           (ctxname cxs)
           [] empty []
           [atlas2pattern p rls rulpattern relpattern relname relsc reltg relprp propsyntax pragma1 pragma2 pragma3|p<-pats]
           [] []
           [] --in pattern:(atlas2rules fSpec tbls)
           [] --in pattern:(atlas2decls fSpec tbls)
           [] [] []
           [] --in pattern for rul and decl (and cpt?):(atlas2expls fSpec tbls)
           (atlas2pops relcontent relname relsc reltg  pairleft pairright atomsyntax)
           [] [] (Tm(V [] (cptAnything,cptAnything)) (-1),[])
          )

atlas2pattern :: String -> [Rule(Relation Concept)] -> [(String,String)] -> [(String,String)] -> [(String,String)]
                        -> [(String,String)] -> [(String,String)] -> [(String,String)]
                        -> [(String,String)] -> [(String,String)]
                        -> [(String,String)] -> [(String,String)] -> Pattern
atlas2pattern p rs rulpattern relpattern relname relsc reltg relprp propsyntax pragma1 pragma2 pragma3
 = Pat { ptnm  = p
       , ptrls = [r|(rulstr,p')<-rulpattern,p==p',r<-rs,name r==rulstr]
       , ptgns = []
       , ptdcs = [atlas2decl relstr i relname relsc reltg relprp propsyntax pragma1 pragma2 pragma3|(i,(relstr,p'))<-zip [1..] relpattern,p==p']
       , ptcds = []
       , ptkds = []
       , ptxps = []
       , testexpr = []
       , inftestexpr = []
       }

emptySignalDeclaration :: String -> Declaration Concept
emptySignalDeclaration nm
    = Sgn nm          -- decnm
          cptAnything -- desrc
          cptAnything -- detrg
          []          -- decprps
          []          -- decprps_calc
          ""          -- decprL
          ""          -- decprM
          ""          -- decprR
          []          -- decpopu
          Nowhere     -- decfpos
          0           -- decid
          True        -- deciss    -- initially, all rules are signals
          False       -- decusr
          ""          -- decpat
          True        -- decplug

geta :: [(String,String)] -> String -> String
geta f x = (\xs-> if null xs then error ("there is no geta for " ++ x) else head xs) [y|(x',y)<-f,x==x']
atlas2pops :: [(String,String)] -> [(String,String)] -> [(String,String)] -> [(String,String)] -> [(String,String)] -> [(String,String)] -> [(String,String)] -> [Population Concept]
atlas2pops relcontent relname relsc reltg pairleft pairright atomsyntax 
 = [Popu (makerel(fst(head cl))) (map (makepair.snd) cl)|cl<-eqCl fst relcontent,not(null cl)]
   where
   makepair xystr = (geta atomsyntax (geta pairleft xystr),geta atomsyntax (geta pairright xystr))
   makerel relstr = 
      let
      nm =geta relname relstr
      s = cptnew(geta relsc relstr)
      t = cptnew(geta reltg relstr)
      in
      Rel  { relnm = nm
           , relpos = Nowhere
           , relats = [s,t]
           , relsrc = s
           , reltrg = t
           , relyin = True
           , reldcl = emptySignalDeclaration nm
           }
atlas2decl :: String -> Int -> [(String,String)] -> [(String,String)] -> [(String,String)]
                            -> [(String,String)] -> [(String,String)] -> [(String,String)]
                            -> [(String,String)] -> [(String,String)] -> Declaration Concept
atlas2decl relstr i relname relsc reltg relprp propsyntax pragma1 pragma2 pragma3
 = Sgn nm          -- decnm
       s -- desrc
       t -- detrg
       [case geta propsyntax prp of 
          "UNI"->Uni
          "TOT"->Tot
          "INJ"->Inj
          "SUR"->Sur
          "RFX"->Rfx
          "IRF"->Irf
          "TRN"->Trn
          "SYM"->Sym
          "ASY"->Asy
          _ -> error "unknown prop in atlas"
       |(prp,rel)<-relprp,relstr==rel]          -- decprps
       []          -- decprps_calc
       [c|(rel,x)<-pragma1,relstr==rel,c<-x]          -- decprL
       [c|(rel,x)<-pragma2,relstr==rel,c<-x]          -- decprM
       [c|(rel,x)<-pragma3,relstr==rel,c<-x]          -- decprR
       []          -- decpopu
       (FilePos ( "Atlas DB", Pos i 0, []))     -- decfpos
       0           -- decid
       True        -- deciss    -- initially, all rules are signals
       True        -- decusr
       []          -- decpat
       False       -- decplug[Popu (makerel(fst(head cl))) (map (makepair.snd) cl)|cl<-eqCl fst relcontent,not(null cl)]
   where
   nm =geta relname relstr
   s = cptnew(geta relsc relstr)
   t = cptnew(geta reltg relstr)
   
