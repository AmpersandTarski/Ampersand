{-# LANGUAGE FlexibleInstances #-}  
{-# OPTIONS_GHC -Wall #-}  
--hdbc and hdbc-odbc must be installed (from hackage)
module DatabaseDesign.Ampersand_Prototype.Apps.RAP 
   (fillAtlas,picturesForAtlas,atlas2context,atlas2populations)
where 
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import DatabaseDesign.Ampersand_Prototype.AutoInstaller (odbcinstall)
import Database.HDBC.ODBC 
import Database.HDBC
import Data.Maybe (fromMaybe)
import DatabaseDesign.Ampersand_Prototype.RelBinGenSQL
-- import DatabaseDesign.Ampersand.Version (fatalMsg)

--fatal :: Int -> String -> a
--fatal = fatalMsg "Atlas"
------
dsnatlas::String
dsnatlas = "DSN=RAPv1"

----------------------------------------------------

fillAtlas :: Fspc -> Options -> IO()
fillAtlas fSpec flags = odbcinstall flags fSpec dsnatlas

picturesForAtlas :: Options -> Fspc -> [Picture]
picturesForAtlas flags fSpec
   = [makePicture flags fSpec Plain_CG p | p <- patterns fSpec] ++
     [makePicture flags fSpec Plain_CG userRule | userRule <- rules fSpec]++
     [makePicture flags fSpec Plain_CG cpt | cpt <- concs fSpec]

----------------------------------------------------

--select population of concepts or declarations from the atlas of this user
--REMARK quickQuery' is strict and needed to keep results for use after disconnecting
type AtomVal = String
type CptTbl = [AtomVal]
type RelTbl = [(AtomVal,AtomVal)]
selectconcept :: (IConnection conn) => conn -> Fspc -> A_Concept -> IO CptTbl
selectconcept conn fSpec cpt
 = do rows <- quickQuery' conn stmt []
      return [fromSql x |[x]<-rows]
   where  stmt = fromMaybe [] (selectExprMorph fSpec (-1) "fld1" "fld1" (I cpt))
selectdecl :: (IConnection conn) => conn -> Fspc -> Relation -> IO RelTbl
selectdecl conn fSpec rel
 = do rows <- quickQuery' conn stmt []
      return [(fromSql x,fromSql y) |[x,y]<-rows]
   where stmt = fromMaybe [] (selectExprMorph fSpec (-1) "fld1" "fld2" rel)


theonly :: [t] -> String -> t
theonly xs err
 | length xs==1 = head xs
 | null xs = error ("no x: " ++ err)
 | otherwise = error ("more than one x: " ++ err)
thehead :: [t] -> String -> t
thehead xs err
 | not(null xs) = head xs
 | otherwise = error ("no x:" ++ err)
therel :: Fspc -> String -> String -> String -> Relation
therel fSpec relname relsource reltarget 
 = theonly [makeRelation d |d<-declarations fSpec
                          ,relname==name d
                          ,null relsource || relsource==name(source d)
                          ,null reltarget || reltarget==name(target d)]
           ("when searching for the relation x with searchpattern (name,source,target)" ++ show (relname,relsource,reltarget))

parseexprs :: RelTbl -> IO [(String,P_Expression)]
parseexprs ruleexpr
 = do xs <- sequence [parseADL1pExpr x "Atlas(Rule)"|(_,x)<-ruleexpr]
      return (zip (map fst ruleexpr) xs)

atlas2populations :: Fspc -> Options -> IO String
atlas2populations fSpec flags =
   do verboseLn flags "Connecting to atlas..."
      conn<-connectODBC dsnatlas
      verboseLn flags "Connected."
      -----------
      --select (strict) everything you need, then disconnect, then assemble it into a context and patterns and stuff
      --Context--
      cxs <- selectconcept conn fSpec (newAcpt "Context" [])
      --Relation
      relname <- selectdecl conn fSpec (therel fSpec "rel" [] [])
      relsc <- selectdecl conn fSpec (therel fSpec "src" [] [])
      reltg <- selectdecl conn fSpec (therel fSpec "trg" [] [])
      --P_Population--
      relcontent <- selectdecl conn fSpec (therel fSpec "content" [] [])
      pairleft <- selectdecl conn fSpec (therel fSpec "left" [] [])
      pairright <- selectdecl conn fSpec (therel fSpec "right" [] [])
      atomsyntax <- selectdecl conn fSpec (therel fSpec "atomsyntax" [] [])
      -----------
      disconnect conn
      verboseLn flags "Disconnected."
      makepops cxs relname relsc reltg relcontent pairleft pairright atomsyntax

makepops :: CptTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> IO String
makepops cxs relname relsc reltg relcontent pairleft pairright atomsyntax
 = return ("CONTEXT "++cxnm++"\n"++concat (map showADL pops)++"\nENDCONTEXT")
   where
   cxnm    = thehead cxs "no context found in Atlas DB"
   pops    = atlas2pops relcontent relname relsc reltg  pairleft pairright atomsyntax

atlas2context :: Fspc -> Options -> IO A_Context
atlas2context fSpec flags =
   do --tbls <- readAtlas fSpec flags
      verboseLn flags "Connecting to atlas..."
      conn<-connectODBC dsnatlas
      verboseLn flags "Connected."
      -----------
      --select (strict) everything you need, then disconnect, then assemble it into a context and patterns and stuff
      --Context--
      cxs <- selectconcept conn fSpec (newAcpt "Context" [])
      pats <- selectconcept conn fSpec (newAcpt "Pattern" [])
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
      --PReferencable--
      patpurpose <- selectdecl conn fSpec (therel fSpec "purpose" "Pattern" [])
      rulpurpose <- selectdecl conn fSpec (therel fSpec "purpose" "UserRule" [])
      relpurpose <- selectdecl conn fSpec (therel fSpec "purpose" "Relation" [])
      cptpurpose <- selectdecl conn fSpec (therel fSpec "purpose" "Concept" [])
      cptdescribes <- selectdecl conn fSpec (therel fSpec "describes" "Concept" [])
      ruldescribes <- selectdecl conn fSpec (therel fSpec "describes" "UserRule" [])
      -----------
      disconnect conn
      verboseLn flags "Disconnected."
      rls <-parseexprs ruleexpr
      --verboseLn flags (show(map showADL (atlas2pops relcontent relname relsc reltg  pairleft pairright atomsyntax)))
      (actx,errs)<-makectx cxs (language flags) pats rulpattern rls ruldescribes relpattern
                     relname relsc reltg relcontent pairleft pairright atomsyntax relprp propsyntax pragma1 pragma2 pragma3
                     patpurpose rulpurpose relpurpose cptpurpose cptdescribes
      if nocxe errs then return actx else error (show errs)

makectx :: CptTbl -> Lang -> CptTbl -> RelTbl -> [(String,P_Expression)] -> RelTbl -> RelTbl ->
           RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl ->
           RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> IO (A_Context, CtxError)
makectx cxs lang pats rulpattern rls ruldescribes relpattern 
        relname relsc reltg relcontent pairleft pairright atomsyntax relprp propsyntax pragma1 pragma2 pragma3
        patpurpose rulpurpose relpurpose cptpurpose cptdescribes
 = return (typeCheck rawctx [])
   where
   rawctx 
    = PCtx {
         ctx_nm    = thehead cxs "no context found in Atlas DB"
       , ctx_pos   = [DBLoc "Atlas(Context)"]
       , ctx_lang  = Just lang
       , ctx_markup= Just LaTeX --ADLImportable writes LaTeX
       , ctx_thms  = []
       , ctx_pats  = [atlas2pattern p rulpattern rls (Just lang) ruldescribes relpattern relname relsc reltg relprp propsyntax pragma1 pragma2 pragma3 |p<-pats]
       , ctx_PPrcs = []
       , ctx_rs    = [] --in pattern:(atlas2rules fSpec tbls)
       , ctx_ds    = [] --in pattern:(atlas2decls fSpec tbls)
       , ctx_cs    = [Cd (DBLoc "Atlas(A_ConceptDef)") x False y [] [] |(x,y)<-cptdescribes,not(null y)]
       , ctx_ks    = []
       , ctx_gs    = []
       , ctx_ifcs  = []
       , ctx_ps    = atlas2pexpls patpurpose rulpurpose relpurpose cptpurpose relname relsc reltg
       , ctx_pops  = atlas2pops relcontent relname relsc reltg  pairleft pairright atomsyntax
       , ctx_sql   = []
       , ctx_php   = []
       , ctx_metas = []
       , ctx_experimental = False -- is set in Components.hs
      }

atlas2rule :: String -> [(String,P_Expression)] -> Maybe Lang -> RelTbl -> P_Rule
atlas2rule rulstr rls mlang ruldescribes
 = P_Ru { rr_nm   = rulstr
        , rr_exp  = geta rls rulstr  (error "while geta rls.")
        , rr_fps  = DBLoc "Atlas(Rule)"
        , rr_mean = [PMeaning $ P_Markup mlang Nothing (geta ruldescribes rulstr "")]
        , rr_msg  = []
        , rr_viol = Nothing
        }

atlas2pattern :: AtomVal -> RelTbl -> [(String,P_Expression)] -> Maybe Lang -> RelTbl -> RelTbl -> RelTbl
                         -> RelTbl -> RelTbl -> RelTbl
                         -> RelTbl -> RelTbl
                         -> RelTbl -> RelTbl -> P_Pattern
atlas2pattern p rulpattern rls mlang ruldescribes relpattern relname relsc reltg relprp propsyntax pragma1 pragma2 pragma3
 = P_Pat { pt_nm  = p
         , pt_pos = DBLoc "Atlas(Pattern)"
         , pt_end = DBLoc "Atlas(Pattern)"
         , pt_rls = [atlas2rule rulstr rls mlang ruldescribes|(rulstr,p')<-rulpattern,p==p']
         , pt_gns = []
         , pt_dcs = [atlas2decl relstr i relname relsc reltg relprp propsyntax pragma1 pragma2 pragma3 |(i,(relstr,p'))<-zip [1..] relpattern,p==p']
         , pt_cds = []
         , pt_kds = []
         , pt_xps = []
         , pt_pop = []
         }

geta :: [(String,b)] -> String -> b -> b
geta f x notfound = (\xs-> if null xs then notfound else head xs) [y |(x',y)<-f,x==x']
atlas2pops :: [(String,String)] -> [(String,String)] -> [(String,String)] -> [(String,String)] -> [(String,String)] -> [(String,String)] -> [(String,String)] -> [P_Population]
atlas2pops relcontent relname relsc reltg pairleft pairright atomsyntax 
 = [P_Popu r (P_Sign [s,t]) (map (makepair.snd) cl)
   |cl<-eqCl fst relcontent,not(null cl)
   , let r = makerel (fst(head cl)) relname
   , let s = PCpt(geta relsc (fst(head cl)) (error "while geta relsc1."))
   , let t = PCpt(geta reltg (fst(head cl)) (error "while geta reltg1."))]
   where
   makepair xystr = (geta atomsyntax (geta pairleft xystr  (error "while pairleft relsc."))  (error "while geta atomsyntax1.")
                    ,geta atomsyntax (geta pairright xystr (error "while pairright relsc.")) (error "while geta atomsyntax2."))

atlas2decl :: String -> Int -> [(String,String)] -> [(String,String)] -> [(String,String)]
                            -> [(String,String)] -> [(String,String)] -> [(String,String)]
                            -> [(String,String)] -> [(String,String)] -> P_Declaration
atlas2decl relstr i relname relsc reltg relprp propsyntax pragma1 pragma2 pragma3
 = P_Sgn { dec_nm = geta relname relstr (error "while geta relname1.")
         , dec_sign = P_Sign [PCpt(geta relsc relstr (error "while geta relsc2.")),PCpt(geta reltg relstr (error "while geta reltg2."))]
         , dec_prps = [case geta propsyntax prp  (error "while geta propsyntax.") of 
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
         , dec_prL = [c |(rel,x)<-pragma1,relstr==rel,c<-x]
         , dec_prM = [c |(rel,x)<-pragma2,relstr==rel,c<-x]
         , dec_prR = [c |(rel,x)<-pragma3,relstr==rel,c<-x]
         , dec_Mean = []
         , dec_conceptDef = Nothing
         , dec_popu = []
         , dec_fpos = DBLoc$"Atlas(Declaration)"++show i
         , dec_plug = False
         }

atlas2pexpls :: [(String,String)] -> [(String,String)] -> [(String,String)] -> [(String,String)]
                                  -> [(String,String)] -> [(String,String)] -> [(String,String)] -> [PPurpose]
atlas2pexpls patpurpose rulpurpose relpurpose cptpurpose relname relsc reltg
 = --error(show (patpurpose, rulpurpose, relpurpose, cptpurpose)) ++
     [PRef2 (DBLoc "Atlas(PatPurpose)") (PRef2Pattern x) (P_Markup Nothing Nothing y) []
     |(x,y)<-patpurpose]
  ++ [PRef2 (DBLoc "Atlas(RulPurpose)") (PRef2Rule x) (P_Markup Nothing Nothing y) []
     |(x,y)<-rulpurpose]
  ++ [PRef2 (DBLoc "Atlas(RelPurpose)") (PRef2Declaration (r, P_Sign [PCpt(geta relsc x (error "while geta relsc3."))
                                                                     ,PCpt(geta reltg x (error "while geta reltg3."))])) (P_Markup Nothing Nothing y) []
     |(x,y)<-relpurpose, let r=makerel x relname]
  ++ [PRef2 (DBLoc "Atlas(CptPurpose)") (PRef2ConceptDef x) (P_Markup Nothing Nothing y) []
     |(x,y)<-cptpurpose]

makerel :: String -> [(String, String)] -> P_Relation
makerel relstr relname
 = P_Rel  { rel_nm = geta relname relstr  (error "while geta relname2.")
          , rel_pos = DBLoc "Atlas(Relation)"
          }
