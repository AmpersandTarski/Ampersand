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
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import Database.HDBC.ODBC 
import Database.HDBC
import Data.List  (intercalate)
import DatabaseDesign.Ampersand_Prototype.RelBinGenSQL
-- import DatabaseDesign.Ampersand.Version (fatalMsg)

-- fatal :: Int -> String -> a
-- fatal = fatalMsg "Basics"
------
dsnatlas::String
dsnatlas = "DSN=Atlasv2"

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

--select population of concepts or declarations from the atlas of this user
--REMARK quickQuery' is strict and needed to keep results for use after disconnecting
type AtomVal = String
type CptTbl = [AtomVal]
type RelTbl = [(AtomVal,AtomVal)]
selectconcept :: (IConnection conn) => conn -> Fspc -> Concept -> IO CptTbl
selectconcept conn fs cpt
 = do rows <- quickQuery' conn stmt []
      return [fromSql x|[x]<-rows]
   where stmt = showsql$SqlSel1 (selectdomain fs cpt)
selectdecl :: (IConnection conn) => conn -> Fspc -> Relation Concept -> IO RelTbl
selectdecl conn fs rel
 = do rows <- quickQuery' conn stmt []
      return [(fromSql x,fromSql y)|[x,y]<-rows]
   where stmt = showsql$SqlSel2 (selectbinary fs rel)
         
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
                            ++ if atlastxt fld then showSQL (SQLVarchar 20000) else showSQL (fldtype fld) --SQLText has decoding problems??
                            ++ autoIncr fld ++ nul fld
         nul fld = if fldnull fld then "" else " NOT NULL"
         autoIncr fld = if fldauto fld then " AUTO_INCREMENT" else ""
         atlastxt fld = not (flduniq fld) && elem ((name.target.fldexpr) fld) ["CptPurpose","RelPurpose","Explanation","RulPurpose","PatPurpose","Description","Definition"]

----------------------
fillAtlas :: Fspc -> Options -> IO()
fillAtlas fSpec flags = 
   do verboseLn flags "Connecting to atlas..."
      conn<-connectODBC dsnatlas
      verboseLn flags "Connected."
--      _ <- error(show ["DROP TABLE "++name p| InternalPlug p<-plugInfos fSpec])
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
   = [makePicture flags fSpec Plain_CG p | p <- patterns fSpec] ++
     [makePicture flags fSpec Plain_CG userRule | userRule <- rules fSpec]++
     [makePicture flags fSpec Plain_CG cpt | cpt <- (concs fSpec)]

----------------------------------------------------

theonly :: [t] -> String -> t
theonly xs err
 | length xs==1 = head xs
 | null xs = error ("no x: " ++ err)
 | otherwise = error ("more than one x: " ++ err)
thehead :: [t] -> String -> t
thehead xs err
 | not(null xs) = head xs
 | otherwise = error ("no x:" ++ err)
therel :: Fspc -> String -> String -> String -> Relation Concept
therel fSpec relname relsource reltarget 
 = theonly [makeRelation d|d<-declarations fSpec
                          ,relname==name d
                          ,null relsource || relsource==name(source d)
                          ,null reltarget || reltarget==name(target d)]
           ("when searching for the relation x with searchpattern (name,source,target)" ++ show (relname,relsource,reltarget))

atlas2context :: Fspc -> Options -> IO A_Context
atlas2context fSpec flags =
   do --tbls <- readAtlas fSpec flags
      verboseLn flags "Connecting to atlas..."
      conn<-connectODBC dsnatlas
      verboseLn flags "Connected."
      -----------
      --select (strict) everything you need, then disconnect, then assemble it into a context and patterns and stuff
      --Context--
      cxs <- selectconcept conn fSpec (cptnew "Context")
      pats <- selectconcept conn fSpec (cptnew "Pattern")
      --Relation
      relname <- selectdecl conn fSpec (therel fSpec "rel" [] [])
      relsc <- selectdecl conn fSpec (therel fSpec "src" [] [])
      reltg <- selectdecl conn fSpec (therel fSpec "trg" [] [])
      relprp <- selectdecl conn fSpec (therel fSpec "propertyof" [] [])
      propsyntax <- selectdecl conn fSpec (therel fSpec "propsyntax" [] [])
      pragma1 <- selectdecl conn fSpec (therel fSpec "pragma1" [] [])
      pragma2 <- selectdecl conn fSpec (therel fSpec "pragma2" [] [])
      pragma3 <- selectdecl conn fSpec (therel fSpec "pragma3" [] [])
      --P_Population--
      relcontent <- selectdecl conn fSpec (therel fSpec "content" [] [])
      pairleft <- selectdecl conn fSpec (therel fSpec "left" [] [])
      pairright <- selectdecl conn fSpec (therel fSpec "right" [] [])
      atomsyntax <- selectdecl conn fSpec (therel fSpec "atomsyntax" [] [])
      --Rule--
      ruleexpr <- selectdecl conn fSpec (therel fSpec "ruleexpr" [] [])
      --Pattern--
      rulpattern <- selectdecl conn fSpec (therel fSpec "rulpattern" [] [])
      relpattern <- selectdecl conn fSpec (therel fSpec "relpattern" [] [])
      --PExplainable--
      patpurpose <- selectdecl conn fSpec (therel fSpec "purpose" "Pattern" [])
      rulpurpose <- selectdecl conn fSpec (therel fSpec "purpose" "UserRule" [])
      relpurpose <- selectdecl conn fSpec (therel fSpec "purpose" "Relation" [])
      cptpurpose <- selectdecl conn fSpec (therel fSpec "purpose" "Concept" [])
      cptdescribes <- selectdecl conn fSpec (therel fSpec "describes" "Concept" [])
      ruldescribes <- selectdecl conn fSpec (therel fSpec "describes" "UserRule" [])
      -----------
      disconnect conn
      verboseLn flags "Disconnected."
      rls<-parserules rulpattern ruleexpr ruldescribes
      --verboseLn flags (show(map showADL (atlas2pops relcontent relname relsc reltg  pairleft pairright atomsyntax)))
      (thectx,errs)<-makectx cxs pats rls rulpattern relpattern
                     relname relsc reltg relcontent pairleft pairright atomsyntax relprp propsyntax pragma1 pragma2 pragma3
                     patpurpose rulpurpose relpurpose cptpurpose cptdescribes
      if null errs then return thectx else error (show errs)

makectx :: CptTbl -> CptTbl -> [Rule(Relation Concept)] -> RelTbl -> RelTbl ->
           RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl ->
           RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> IO (A_Context,[String])
makectx cxs pats rls rulpattern relpattern 
        relname relsc reltg relcontent pairleft pairright atomsyntax relprp propsyntax pragma1 pragma2 pragma3
        patpurpose rulpurpose relpurpose cptpurpose cptdescribes
 = if null xs then error (show errs) else return (head xs,errs)
   where 
   (xs,errs') = typecheckAdl1 (Arch [rawctx]) []
   errs = map fst errs'
   rawctx 
    = PCtx {
         ctx_nm    = thehead cxs "no context found in Atlas DB"
       , ctx_on    = []
       , ctx_isa   = empty
       , ctx_wrld  = []
       , ctx_pats  = [atlas2pattern p rls rulpattern relpattern relname relsc reltg relprp propsyntax pragma1 pragma2 pragma3|p<-pats]
       , ctx_PPrcs = []
       , ctx_procs = []
       , ctx_rs    = [] --in pattern:(atlas2rules fSpec tbls)
       , ctx_ds    = [] --in pattern:(atlas2decls fSpec tbls)
       , ctx_cs    = [Cd (DBLoc "Atlas(ConceptDef)") x False y []|(x,y)<-cptdescribes,not(null y)]
       , ctx_ks    = []
       , ctx_ifcs  = []
       , ctx_ps    = atlas2pexpls patpurpose rulpurpose relpurpose cptpurpose relname relsc reltg
       , ctx_pops  = atlas2pops relcontent relname relsc reltg  pairleft pairright atomsyntax
       , ctx_sql   = []
       , ctx_php   = []
       , ctx_env   = (Tm(V (Anything,Anything)),[])
      }

parserules :: RelTbl -> RelTbl -> RelTbl -> IO [Rule(Relation Concept)]
parserules rulpattern ruleexpr ruldescribes
 = sequence [parseADL1Rule ("RULE \""++r++"\":"++ (geta ruleexpr r)
                          ++" PHRASE \""++geta ruldescribes r++"\"")|(r,_)<-rulpattern]

atlas2pattern :: AtomVal -> [Rule(Relation Concept)] -> RelTbl -> RelTbl -> RelTbl
                         -> RelTbl -> RelTbl -> RelTbl
                         -> RelTbl -> RelTbl
                         -> RelTbl -> RelTbl -> Pattern
atlas2pattern p rs rulpattern relpattern relname relsc reltg relprp propsyntax pragma1 pragma2 pragma3
 = Pat { ptnm  = p
       , ptpos = ParsedFrom(DBLoc "Atlas(Pattern)")
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
    = Sgn { decnm = nm
          , desrc = Anything
          , detrg = Anything
          , decprps = []
          , decprps_calc = []
          , decprL  = ""
          , decprM  = ""
          , decprR  = ""
          , decMean = ""
          , decpopu = []
          , decfpos = ParsedFrom(DBLoc "Atlas(Relation)")
          , decid   = 0
          , deciss  = True    -- initially, all rules are signals
          , decusr  = False
          , decpat  = ""
          , decplug = True
          }
geta :: [(String,String)] -> String -> String
geta f x = (\xs-> if null xs then error ("there is no geta for " ++ x) else head xs) [y|(x',y)<-f,x==x']
atlas2pops :: [(String,String)] -> [(String,String)] -> [(String,String)] -> [(String,String)] -> [(String,String)] -> [(String,String)] -> [(String,String)] -> [P_Population Concept]
atlas2pops relcontent relname relsc reltg pairleft pairright atomsyntax 
 = [Popu (makerel(fst(head cl)) relname relsc reltg) (map (makepair.snd) cl)|cl<-eqCl fst relcontent,not(null cl)]
   where
   makepair xystr = (geta atomsyntax (geta pairleft xystr),geta atomsyntax (geta pairright xystr))

atlas2decl :: String -> Int -> [(String,String)] -> [(String,String)] -> [(String,String)]
                            -> [(String,String)] -> [(String,String)] -> [(String,String)]
                            -> [(String,String)] -> [(String,String)] -> Declaration Concept
atlas2decl relstr i relname relsc reltg relprp propsyntax pragma1 pragma2 pragma3
 = Sgn { decnm = nm
       , desrc = s
       , detrg = t
       , decprps = [case geta propsyntax prp of 
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
                    |(prp,rel)<-relprp,relstr==rel
                    ]
       , decprps_calc 
                  = [case geta propsyntax prp of 
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
                    |(prp,rel)<-relprp,relstr==rel
                    ]
       , decprL = [c|(rel,x)<-pragma1,relstr==rel,c<-x]
       , decprM = [c|(rel,x)<-pragma2,relstr==rel,c<-x]
       , decprR = [c|(rel,x)<-pragma3,relstr==rel,c<-x]
       , decMean = ""
       , decpopu = []
       , decfpos = ParsedFrom(DBLoc$"Atlas(Declaration)"++show i)
       , decid  = 0
       , deciss = True  -- initially, all rules are signals
       , decusr = True
       , decpat = []
       , decplug =False       -- decplug[Popu (makerel(fst(head cl))) (map (makepair.snd) cl)|cl<-eqCl fst relcontent,not(null cl)]
       }
   where
   nm =geta relname relstr
   s = cptnew(geta relsc relstr)
   t = cptnew(geta reltg relstr)

atlas2pexpls :: [(String,String)] -> [(String,String)] -> [(String,String)] -> [(String,String)]
                                  -> [(String,String)] -> [(String,String)] -> [(String,String)] -> [PExplanation]
atlas2pexpls patpurpose rulpurpose relpurpose cptpurpose relname relsc reltg
 = --error(show (patpurpose, rulpurpose, relpurpose, cptpurpose)) ++
     [PExpl (DBLoc "Atlas(PatPurpose)") (PExplPattern x) Dutch [] y|(x,y)<-patpurpose]
  ++ [PExpl (DBLoc "Atlas(RulPurpose)") (PExplRule x) Dutch [] y|(x,y)<-rulpurpose]
  ++ [PExpl (DBLoc "Atlas(RelPurpose)") (PExplDeclaration r) Dutch [] y|(x,y)<-relpurpose, let r=makerel x relname relsc reltg]
  ++ [PExpl (DBLoc "Atlas(CptPurpose)") (PExplConceptDef x) Dutch [] y|(x,y)<-cptpurpose]

makerel :: String -> [(String, String)] -> [(String, String)] -> [(String, String)] -> Relation Concept
makerel relstr relname relsc reltg = 
      let
      nm =geta relname relstr
      s = cptnew(geta relsc relstr)
      t = cptnew(geta reltg relstr)
      in
      Rel  { relnm = nm
           , relpos = ParsedFrom(DBLoc "Atlas(Relation)")
           , relats = [s,t]
           , relsrc = s
           , reltrg = t
           , relyin = True
           , reldcl = emptySignalDeclaration nm
           }
