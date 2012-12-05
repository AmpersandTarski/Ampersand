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
--fatal = fatalMsg "Ampersand_Prototype.Apps.RAP"
------
dsnatlas::String
dsnatlas = "DSN=RAPv1"

----------------------------------------------------

fillAtlas :: Fspc -> Options -> IO()
fillAtlas fSpec flags = odbcinstall flags fSpec dsnatlas

picturesForAtlas :: Options -> Fspc -> [Picture]
picturesForAtlas flags fSpec
   = [makePicture flags fSpec Plain_CG p | p <- patterns fSpec] ++
     [makePicture flags fSpec Plain_CG userRule | userRule <- udefrules fSpec]++
     [makePicture flags fSpec Plain_CG cpt | cpt <- concs fSpec]

----------------------------------------------------

--select population of concepts or declarations from the atlas of this user
--REMARK quickQuery' is strict and needed to keep results for use after disconnecting
type AtomVal = String
type RelTbl = [(AtomVal,AtomVal)]
selectdecl :: (IConnection conn) => conn -> Fspc -> Relation -> IO RelTbl
selectdecl conn fSpec rel
 = do rows <- quickQuery' conn stmt []
      return [(fromSql x,fromSql y) |[x,y]<-rows]
   where stmt = fromMaybe [] (selectExprRelation fSpec (-1) "fld1" "fld2" rel)


theonly :: [t] -> String -> t
theonly xs err
 | length xs==1 = head xs
 | null xs = error ("no x: " ++ err)
 | otherwise = error ("more than one x: " ++ err)
therel :: Fspc -> String -> String -> String -> Relation
therel fSpec relname relsource reltarget 
 = theonly [makeRelation d |
              d<-declarations fSpec
             ,relname==name d
             ,null relsource || relsource==name(source d)
             ,null reltarget || reltarget==name(target d)]
           ("when searching for the relation x with searchpattern (name,source,target)" ++ show (relname,relsource,reltarget))
geta :: [(String,b)] -> String -> b -> b
geta f x notfound = (\xs-> if null xs then notfound else head xs) [y | (x',y)<-f,x==x']

{- obsolete?
makerel :: String -> P_Relation
makerel relstr
 = P_Rel  { rel_nm = relstr
          , rel_pos = DBLoc "Atlas(Relation)"
          }
-}

atlas2populations :: Fspc -> Options -> IO String
atlas2populations fSpec flags =
   do verboseLn flags "Connecting to atlas..."
      conn<-connectODBC dsnatlas
      verboseLn flags "Connected."
      -----------
      --select (strict) everything you need, then disconnect, then assemble it into a context with populations only
      --Context--
      r_ctxnm           <- selectdecl conn fSpec (therel fSpec "ctxnm" [] []) --ctxnm  ::Context->Conid
      --Concept--
      r_cptnm           <- selectdecl conn fSpec (therel fSpec "cptnm" [] []) --cptnm :: Concept->Conid
      r_cptos           <- selectdecl conn fSpec (therel fSpec "cptos" [] []) --cptos :: Concept*AtomID
      r_atomvalue       <- selectdecl conn fSpec (therel fSpec "atomvalue" [] []) --atomvalue::AtomID->Atom
      --Relation--
      r_decnm           <- selectdecl conn fSpec (therel fSpec "decnm" [] []) --decnm   ::Declaration->Varid
      r_decsgn          <- selectdecl conn fSpec (therel fSpec "decsgn" [] []) --decsgn ::Declaration->Sign
      r_src             <- selectdecl conn fSpec (therel fSpec "src" [] []) --src::Sign->Concept
      r_trg             <- selectdecl conn fSpec (therel fSpec "trg" [] []) --trg::Sign->Concept
      --P_Population--
      r_decpopu         <- selectdecl conn fSpec (therel fSpec "decpopu" [] []) --decpopu ::Declaration*PairID
      r_left            <- selectdecl conn fSpec (therel fSpec "left" [] []) --left::Pair->AtomID
      r_right           <- selectdecl conn fSpec (therel fSpec "right" [] []) --right::Pair->AtomID
      -----------
      disconnect conn
      verboseLn flags "Disconnected."
      makepops r_ctxnm r_decnm r_decsgn r_src r_trg r_cptnm r_decpopu r_left r_right r_cptos r_atomvalue

makepops :: RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> IO String
makepops r_ctxnm r_decnm r_decsgn r_src r_trg r_cptnm r_decpopu r_left r_right r_cptos r_atomvalue
 = return ("CONTEXT "++cxnm++"\n"++concatMap showADL pops++"\nENDCONTEXT")
   where
   cxnm    = snd(theonly r_ctxnm "no context found in Atlas DB")
   pops    = atlas2pops r_decnm r_decsgn r_src r_trg r_cptnm r_decpopu r_left r_right r_cptos r_atomvalue

atlas2context :: Fspc -> Options -> IO A_Context
atlas2context fSpec flags =
   do --tbls <- readAtlas fSpec flags
      verboseLn flags "Connecting to atlas..."
      conn<-connectODBC dsnatlas
      verboseLn flags "Connected."
      -----------
      --select (strict) everything you need, then disconnect, then assemble it into a context and patterns and stuff
      --Context--
      r_ctxnm           <- selectdecl conn fSpec (therel fSpec "ctxnm" [] []) --ctxnm  ::Context->Conid
      --not needed because there is only one context
      --ctxpats::Context*Pattern
      --ctxcs  ::Context*Concept
      --Pattern--
      r_ptnm            <- selectdecl conn fSpec (therel fSpec "ptnm" [] []) --ptnm  :: Pattern->Conid
      r_ptrls           <- selectdecl conn fSpec (therel fSpec "ptrls" [] []) --ptrls :: Pattern*Rule
      r_ptdcs           <- selectdecl conn fSpec (therel fSpec "ptdcs" [] []) --ptdcs :: Pattern*Declaration
      r_ptgns           <- selectdecl conn fSpec (therel fSpec "ptgns" [] []) --ptgns :: Pattern*Gen
      r_ptxps           <- selectdecl conn fSpec (therel fSpec "ptxps" [] []) --ptxps :: Pattern*Blob
      --Gen--
      r_gengen          <- selectdecl conn fSpec (therel fSpec "gengen" [] []) --gengen :: Gen->Concept
      r_genspc          <- selectdecl conn fSpec (therel fSpec "genspc" [] []) --genspc :: Gen->Concept
      --Concept--
      r_cptnm           <- selectdecl conn fSpec (therel fSpec "cptnm" [] []) --cptnm :: Concept->Conid
      r_cptpurpose      <- selectdecl conn fSpec (therel fSpec "cptpurpose" [] []) --cptpurpose:: Concept*Blob
      r_cptdf           <- selectdecl conn fSpec (therel fSpec "cptdf" [] []) --cptdf :: Concept*Blob
      r_cptos           <- selectdecl conn fSpec (therel fSpec "cptos" [] []) --cptos :: Concept*AtomID
      r_atomvalue       <- selectdecl conn fSpec (therel fSpec "atomvalue" [] []) --atomvalue::AtomID->Atom
      --Relation--
      r_decnm           <- selectdecl conn fSpec (therel fSpec "decnm" [] []) --decnm   ::Declaration->Varid
      r_decsgn          <- selectdecl conn fSpec (therel fSpec "decsgn" [] []) --decsgn ::Declaration->Sign
      r_src             <- selectdecl conn fSpec (therel fSpec "src" [] []) --src::Sign->Concept
      r_trg             <- selectdecl conn fSpec (therel fSpec "trg" [] []) --trg::Sign->Concept
      r_decprps         <- selectdecl conn fSpec (therel fSpec "decprps" [] []) --decprps::Declaration*PropertyRule
      r_declaredthrough <- selectdecl conn fSpec (therel fSpec "declaredthrough" [] []) --declaredthrough::PropertyRule*Property
      r_decprL          <- selectdecl conn fSpec (therel fSpec "decprL" [] []) --decprL  ::Declaration*String
      r_decprM          <- selectdecl conn fSpec (therel fSpec "decprM" [] []) --decprM  ::Declaration*String
      r_decprR          <- selectdecl conn fSpec (therel fSpec "decprR" [] []) --decprR  ::Declaration*String
      r_decmean         <- selectdecl conn fSpec (therel fSpec "decmean" [] []) --decmean ::Declaration * Blob
      r_decpurpose      <- selectdecl conn fSpec (therel fSpec "decpurpose" [] []) --decpurpose::Declaration * Blob
      --P_Population--
      r_decpopu         <- selectdecl conn fSpec (therel fSpec "decpopu" [] []) --decpopu ::Declaration*PairID
      r_left            <- selectdecl conn fSpec (therel fSpec "left" [] []) --left::PairID->AtomID
      r_right           <- selectdecl conn fSpec (therel fSpec "right" [] []) --right::PairID->AtomID
      --Rule--
      r_rrnm            <- selectdecl conn fSpec (therel fSpec "rrnm" [] []) --rrnm  :: Rule -> ADLid
      r_rrexp           <- selectdecl conn fSpec (therel fSpec "rrexp" [] []) --rrexp :: Rule -> ExpressionID
      r_rrmean          <- selectdecl conn fSpec (therel fSpec "rrmean" [] []) --rrmean:: Rule * Blob
      r_rrpurpose       <- selectdecl conn fSpec (therel fSpec "rrpurpose" [] []) --rrpurpose:: Rule * Blob
      --Expression--
      r_exprvalue'      <- selectdecl conn fSpec (therel fSpec "exprvalue" [] []) --exprvalue :: ExpressionID->Expression
      --not needed
      --rels  :: ExpressionID*Relation
      --relnm :: Relation -> Varid
      --relsgn:: Relation -> Sign
      --reldcl:: Relation -> Declaration
      -----------
      disconnect conn
      verboseLn flags "Disconnected."
      r_exprvalue <-parseexprs r_exprvalue' --parsing is the safest way to get the Term
      --verboseLn flags (show(map showADL (atlas2pops relcontent relname relsc reltg  pairleft pairright atomsyntax)))
      actx <- makectx r_ctxnm (language flags)
                     r_ptnm r_ptrls r_ptdcs r_ptgns r_ptxps          
                     r_gengen r_genspc
                     r_cptnm r_cptpurpose r_cptdf r_cptos r_atomvalue      
                     r_decnm r_decsgn r_src r_trg r_decprps r_declaredthrough r_decprL r_decprM r_decprR r_decmean r_decpurpose     
                     r_decpopu r_left r_right          
                     r_rrnm r_rrexp r_rrmean r_rrpurpose r_exprvalue
      case actx of
       (Errors x)  -> error (show x)
       (Checked x) -> return x
      where
      parseexprs :: RelTbl -> IO [(String,Term)]
      parseexprs r_exprvalue
       = do xs <- sequence [parseADL1pExpr x "Atlas(Rule)"|(_,x)<-r_exprvalue]
            return (zip (map fst r_exprvalue) xs)

makectx :: RelTbl -> Lang -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl
                  -> RelTbl -> RelTbl
                  -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl
                  -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl
                  -> RelTbl -> RelTbl -> RelTbl -> RelTbl
                  -> RelTbl -> RelTbl -> RelTbl -> [(AtomVal,Term)] -> IO (Guarded A_Context)
makectx r_ctxnm lang r_ptnm r_ptrls r_ptdcs r_ptgns r_ptxps          
                     r_gengen r_genspc
                     r_cptnm r_cptpurpose r_cptdf r_cptos r_atomvalue      
                     r_decnm r_decsgn r_src r_trg r_decprps r_declaredthrough r_decprL r_decprM r_decprR r_decmean r_decpurpose     
                     r_decpopu r_left r_right          
                     r_rrnm r_rrexp r_rrmean r_rrpurpose r_exprvalue
 = return a_context
   where
   (a_context,_,_) = typeCheck rawctx []
   rawctx 
    = PCtx {
         ctx_nm    = snd(theonly r_ctxnm "not one context in Atlas DB")
       , ctx_pos   = [DBLoc "Atlas(Context)"]
       , ctx_lang  = Just lang
       , ctx_markup= Just LaTeX --ADLImportable writes LaTeX
       , ctx_thms  = []
       , ctx_pats  = [atlas2pattern p lang
                                    r_ptrls r_ptdcs r_ptgns          
                                    r_gengen r_genspc
                                    r_cptnm
                                    r_decnm r_decsgn r_src r_trg r_decprps r_declaredthrough r_decprL r_decprM r_decprR r_decmean r_decpurpose 
                                    r_rrnm r_rrexp r_rrmean r_rrpurpose r_exprvalue
                     |p<-r_ptnm]
       , ctx_PPrcs = []
       , ctx_rs    = [] --in pattern:(atlas2rules fSpec tbls)
       , ctx_ds    = [] --in pattern:(atlas2decls fSpec tbls)
       , ctx_cs    = [Cd (DBLoc "Atlas(A_ConceptDef)") cnm False cdf [] [] 
                     | (cid,cdf)<-r_cptdf, not(null cdf) 
                     , let cnm = geta r_cptnm cid (error "while geta r_cptnm for cdf.")]
       , ctx_ks    = []
       , ctx_gs    = []
       , ctx_ifcs  = []
       , ctx_ps    = [PRef2 (DBLoc "Atlas(PatPurpose)") (PRef2Pattern pnm) (P_Markup Nothing Nothing ppurp) []
                     | (pid,ppurp)<-r_ptxps, not(null ppurp) 
                     , let pnm = geta r_ptnm pid (error "while geta r_ptnm for ppurp.")]
                  ++ [PRef2 (DBLoc "Atlas(CptPurpose)") (PRef2ConceptDef cnm) (P_Markup Nothing Nothing cpurp) []
                     | (cid,cpurp)<-r_cptpurpose, not(null cpurp) 
                     , let cnm = geta r_cptnm cid (error "while geta r_cptnm for cpurp.")]
       , ctx_pops  = atlas2pops r_decnm r_decsgn r_src r_trg r_cptnm r_decpopu r_left r_right r_cptos r_atomvalue
       , ctx_sql   = []
       , ctx_php   = []
       , ctx_metas = []
      }

atlas2pattern :: (AtomVal,AtomVal) -> Lang -> RelTbl -> RelTbl -> RelTbl
                  -> RelTbl -> RelTbl -> RelTbl
                  -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl
                  -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> [(AtomVal,Term)] -> P_Pattern 
atlas2pattern (pid,pnm) lang r_ptrls r_ptdcs r_ptgns          
                             r_gengen r_genspc
                             r_cptnm
                             r_decnm r_decsgn r_src r_trg r_decprps r_declaredthrough r_decprL r_decprM r_decprR r_decmean r_decpurpose 
                             r_rrnm r_rrexp r_rrmean r_rrpurpose r_exprvalue
 = P_Pat { pt_nm  = pnm
         , pt_pos = DBLoc "Atlas(Pattern)"
         , pt_end = DBLoc "Atlas(Pattern)"
         , pt_rls = [atlas2rule rid lang r_rrnm r_rrexp r_rrmean r_exprvalue 
                    | (pid',rid)<-r_ptrls, pid==pid', rid `notElem` map fst r_declaredthrough]
         , pt_gns = [PGen (DBLoc "Atlas(Gen)") (PCpt gnm) (PCpt snm)
                    | (pid',genid)<-r_ptgns, pid'==pid
                    , let gid = geta r_gengen genid (error "while geta r_gengen.")
                    , let sid = geta r_genspc genid (error "while geta r_genspc.")
                    , let gnm = geta r_cptnm gid (error "while geta r_cptnm for gen.")
                    , let snm = geta r_cptnm sid (error "while geta r_cptnm for spc.")]
         , pt_dcs = [atlas2decl rid i lang r_decnm r_decsgn r_src r_trg r_cptnm r_decprps r_declaredthrough r_decprL r_decprM r_decprR r_decmean
                    |(i,(pid',rid))<-zip [1..] r_ptdcs, pid==pid']
         , pt_cds = []
         , pt_kds = []
         , pt_xps = [PRef2 (DBLoc "Atlas(RulPurpose)") (PRef2Rule rnm) (P_Markup Nothing Nothing rpurp) []
                    | (pid',rid)<-r_ptrls, pid==pid'
                    , (rid',rpurp)<-r_rrpurpose, rid==rid', not(null rpurp)
                    , let rnm = geta r_rrnm rid (error "while geta r_rrnm for rpurp.")]
                 ++ [PRef2 (DBLoc "Atlas(RelPurpose)") 
                           (PRef2Declaration (PTyp OriginUnknown (Prel OriginUnknown rnm) (atlas2sign rid r_decsgn r_src r_trg r_cptnm)))
                           (P_Markup Nothing Nothing rpurp) []
                    | (pid',rid)<-r_ptdcs, pid==pid'
                    , (rid',rpurp)<-r_decpurpose, rid==rid', not(null rpurp)
                    , let rnm = geta r_decnm rid (error "while geta r_decnm for rpurp.")]
         , pt_pop = []
         }

atlas2rule :: AtomVal -> Lang -> RelTbl -> RelTbl -> RelTbl -> [(AtomVal,Term)] -> P_Rule
atlas2rule rid lang r_rrnm r_rrexp r_rrmean r_exprvalue
 = P_Ru { rr_nm   = geta r_rrnm rid (error "while geta r_rrnm.")
        , rr_exp  = geta r_exprvalue eid (error "while geta r_exprvalue.")
        , rr_fps  = DBLoc "Atlas(Rule)"
        , rr_mean = [PMeaning (P_Markup (Just lang) Nothing (geta r_rrmean rid ""))]
        , rr_msg  = []
        , rr_viol = Nothing
        }
   where eid = geta r_rrexp rid (error "while geta r_rrexp.")

atlas2sign :: AtomVal -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> P_Sign
atlas2sign rid r_decsgn r_src r_trg r_cptnm
 = P_Sign [PCpt srcnm , PCpt trgnm]
   where sid = geta r_decsgn rid (error "while geta r_decsgn.")
         srcid = geta r_src sid (error ("while geta r_src."++sid++show r_src))
         trgid = geta r_trg sid (error "while geta r_trg.")
         srcnm = geta r_cptnm srcid (error "while geta r_cptnm of srcid.")
         trgnm = geta r_cptnm trgid (error "while geta r_cptnm of trgid.")

atlas2pops :: RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> [P_Population]
atlas2pops r_decnm r_decsgn r_src r_trg r_cptnm r_decpopu r_left r_right r_cptos r_atomvalue 
 = [ P_RelPopu { p_rnme  = rnm
               , p_orig  = OriginUnknown
               , p_type  = rsgn
               , p_popps = rpop
               }
   | (rid,rnm)<-r_decnm
   , let rsgn = atlas2sign rid r_decsgn r_src r_trg r_cptnm
   , let rpop = [makepair pid | (rid',pid)<-r_decpopu, rid==rid']
   ]
   ++
   [P_CptPopu { p_cnme=geta r_cptnm (fst(head cl)) (error "while geta r_cptnm for CptPopu.")
              , p_orig  = OriginUnknown
              , p_popas=[a | (_,aid)<-cl, let a=geta r_atomvalue aid (error "while geta r_atomvalue of aid.")]
              }
   | cl<-eqCl fst r_cptos, not (null cl)]
   where 
   makepair pid = (lval,rval) 
         where lid = geta r_left pid (error "while geta r_left.")
               rid = geta r_right pid (error "while geta r_right.")
               lval = geta r_atomvalue lid (error "while geta r_atomvalue of lid.")
               rval = geta r_atomvalue rid (error "while geta r_atomvalue of rid.")

atlas2decl :: AtomVal -> Int -> Lang 
              -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> RelTbl -> P_Declaration
atlas2decl rid i lang r_decnm r_decsgn r_src r_trg r_cptnm r_decprps r_declaredthrough r_decprL r_decprM r_decprR r_decmean
 = P_Sgn { dec_nm = geta r_decnm rid (error "while geta r_decnm.")
         , dec_sign = atlas2sign rid r_decsgn r_src r_trg r_cptnm
         , dec_prps = [case geta r_declaredthrough prp (error "while geta r_declaredthrough.") of 
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
                      | (rid',prp)<-r_decprps, rid'==rid]
         , dec_prL = geta r_decprL rid ""
         , dec_prM = geta r_decprM rid ""
         , dec_prR = geta r_decprR rid ""
         , dec_Mean = [PMeaning (P_Markup (Just lang) Nothing (geta r_decmean rid ""))]
         , dec_conceptDef = Nothing
         , dec_popu = []
         , dec_fpos = DBLoc$"Atlas(Declaration)"++show i
         , dec_plug = False
         }    
