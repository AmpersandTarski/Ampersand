{-# OPTIONS_GHC -Wall #-}
--import an fSpec into the RAP specification
-- USE -> cmd: ampersand --importfile=some.adl --importformat=adl RAP.adl
module DatabaseDesign.Ampersand_Prototype.Apps.RAPImport   (importfspec,importfailed)
where
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import DatabaseDesign.Ampersand_Prototype.Version (prototypeVersionStr)
--import DatabaseDesign.Ampersand.Core.Poset (Poset(..),maxima)
import Prelude hiding (Ord(..))
--import DatabaseDesign.Ampersand.Input.ADL1.UU_Parsing (Message(..))
import DatabaseDesign.Ampersand_Prototype.Apps.RAPIdentifiers
import DatabaseDesign.Ampersand_Prototype.Apps.RAP         (picturesForAtlas)
import System.FilePath        (takeFileName,dropFileName,combine,addExtension, takeExtension, dropExtension)
import System.Directory       (getDirectoryContents,doesDirectoryExist,getModificationTime)
import System.Time
import qualified GHC.Exts (sortWith)
import Control.Monad
import Data.List (intercalate)
import Data.List.Split (splitOn)
import DatabaseDesign.Ampersand

fatal :: Int -> String -> a
fatal = fatalMsg "RAPImport"

-----------------------------------------------------------------------------
--exported functions---------------------------------------------------------
-----------------------------------------------------------------------------
importfspec ::  Fspc -> Options -> IO [P_Population]
importfspec fSpec flags 
 = let pics = picturesForAtlas flags fSpec
   in  do verbose flags "Writing pictures for RAP... "
          sequence_ [writePicture flags pict | pict <- pics]
          verbose flags "Getting all uploaded adl-files of RAP user... "
          usrfiles <- getUsrFiles flags
          return (makeRAPPops fSpec flags usrfiles pics)

importfailed :: Either ParseError P_Context -> String -> Options -> IO [P_Population]
importfailed imperr script flags 
 = do verbose flags "Getting all uploaded adl-files of RAP user... "
      usrfiles <- getUsrFiles flags
      return (makeFailedPops imperr script flags usrfiles)

-----------------------------------------------------------------------------
--common local functions-----------------------------------------------------
-----------------------------------------------------------------------------
getUsrFiles :: Options -> IO [(String,ClockTime)]
getUsrFiles flags = let fdir = let d=dropFileName (importfile flags) in if null d then "." else d
                   in  do {fns<-getDirectoryContents fdir >>= filterM (fmap not . doesDirectoryExist . combine fdir)
                          ;times<-mapM (getModificationTime . combine fdir) fns
                          ;return
                            (if length fns == length times 
                             then reverse $ GHC.Exts.sortWith snd (zip fns times)
                             else zip fns
                                    (repeat
                                       (toClockTime $
                                          CalendarTime 1980 February 27 16 15 0 0 Wednesday 0 "UTC" 0
                                            False)
                                    )
                            )
                          }
operations :: Options -> [(Int,String)]
operations flags
 | theme flags == StudentTheme = [(1,"load into Atlas"),(5,"generate func.spec.(pdf)")]
 | theme flags == StudentDesignerTheme = [(1,"load into Atlas"),(5,"generate func.spec.(pdf)"),(6,"generate prototype for students")]
 | theme flags == DesignerTheme = [(1,"load into Atlas"),(5,"generate func.spec.(pdf)"),(8,"generate prototype")]
 | otherwise = [(1,"load into Atlas")]
usr :: Options -> String
usr = namespace
srcfile :: Options -> (String,String)
srcfile flags = (dropFileName(importfile flags),takeFileName(importfile flags))
rapfiles :: Options -> [(String,ClockTime)] -> ([(String,String,ClockTime)],[(String,String,ClockTime)],(String,String))
rapfiles flags usrfiles 
 = ( [(dropFileName(importfile flags),fn,time) | (fn,time)<-usrfiles,takeExtension fn==".adl"] --adlfiles, server files of user with a .adl extension
   , [(dropFileName(importfile flags),fn,time) | (fn,time)<-usrfiles,takeExtension fn==".pop"] --popfiles, server files of user with a .pop extension
   , ("","empty.adl")                                                             --newfile, a copy of empty.adl, it contains an empty context
   )
--a triple which should correspond to a declaration from RAP.adl: (relation name, source name, target name)
--since the populations made by makeRAPPops will be added to the parsetree of RAP.adl, they will be checked and processed by p2aconverters
type RAPRelation = (String,String,String)
makepopu :: RAPRelation -> [(ConceptIdentifier,ConceptIdentifier)] -> P_Population
makepopu (r,src,trg) xys
 = P_TRelPop
         { p_rnme  = r
         , p_orig  = Origin "RAPImport.hs"
         , p_type  = P_Sign (PCpt src) (PCpt trg)
         , p_popps = [mkPair (getid x) (getid y) |(x,y)<-xys, not(null (getid x)), not(null (getid y)) ]
         }

-----------------------------------------------------------------------------
--make population functions--------------------------------------------------
-----------------------------------------------------------------------------
--make population for the import that failed due to a parse or type error
makeFailedPops :: Either ParseError P_Context -> String -> Options -> [(String,ClockTime)] -> [P_Population]
makeFailedPops imperr script flags usrfiles 
 =   --see trunk/apps/Atlas/RAP.adl
     (case imperr of 
         Left (Msg (a, pos, expr)) -> [makepopu ("parseerror","File","ParseError")          [(fid      , perrid fid)]
                                      ,makepopu ("pe_action","ParseError","String")         [(perrid fid, nonsid a)]
                                      ,makepopu ("pe_position","ParseError","String")       [(perrid fid, nonsid pos)]
                                      ,makepopu ("pe_expecting","ParseError","String")      [(perrid fid, nonsid (show expr))]
                                      ]
         Right (c,ce) ->  concat [te c ix | ix<-zip [1..] (errs id ce)]
     )
    ++makeFilePops flags usrfiles []
    where fid = fileid (srcfile flags)
          errs pce (Cxes ces) = concat [errs pce ce | ce<-ces]
          errs pce (CxeOrig ch tp nm o) = map pce (errs (\x->CxeOrig x tp nm o) ch)
          errs pce (Cxe ch ce) = map pce (errs (\x->Cxe x ce) ch)
          errs pce CxeNone = [pce CxeNone]
          errs _ _ = []
          msg (Cxe CxeNone ce) = ce
          msg (Cxe ch _) = msg ch
          msg (CxeOrig ch _ _ _) = msg ch
          msg (Cxes ces) = if null ces then "" else msg(head ces)
          msg _ = ""
          orig att ce@(CxeOrig (Cxe CxeNone _) _ _ _) = att ce
          orig att ce@(CxeOrig CxeNone _ _ _) = att ce
          orig att (CxeOrig ch _ _ _) = orig att ch
          orig att (Cxe ch _) = orig att ch
          orig att (Cxes ces) = if null ces then "" else orig att (head ces)
          orig _   _ = ""
          origline (CxeOrig (Cxe CxeNone _) _ _ p) = linenr p
          origline (CxeOrig CxeNone _ _ p) = linenr p
          origline (CxeOrig ch _ _ _) = origline ch
          origline (Cxe ch _) = origline ch
          origline (Cxes ces) = if null ces then 0 else origline (head ces)
          origline _ = 0
          te c (i,x) = makepopu ("typeerror","File","TypeError")          [(fid, eid)]
                      :makepopu ("te_message","TypeError","ErrorMessage") [(eid,nonsid (msg x))]
                      :makepopu ("te_position","TypeError","String")      [(eid,nonsid ("line "++show(origline x)))]
                      :makepopu ("te_origtype","TypeError","String")      [(eid,nonsid (orig cxetype x))]
                      :makepopu ("te_origname","TypeError","String")      [(eid,nonsid getline)]
                      :(if nocxe es 
                        then makeRAPPops (makeFspec opts cx) opts usrfiles []
                        else [])
                       where getline = let xs = drop ((origline x)-1) (lines script) in if null xs then "" else head xs
                             eid = terrid i fid 
                             (cx,es) = typeCheck nc []
                             nc = PCtx (ctx_nm c) (ctx_pos c) (ctx_lang c) (ctx_markup c) [] 
                                       [P_Pat (pt_nm p) (pt_pos p) (pt_end p) [] (pt_gns p) (pt_dcs p) [] [] [] [] | p<-ctx_pats c]
                                       [] [] [] [] [] [] [] [] [] [] [] [] False

{-
compilererror::File*CompilerError[UNI]
parseerror ::   CompilerError * ParseError[UNI]
pe_action ::    ParseError -> String
pe_position ::  ParseError -> String
pe_expecting :: ParseError -> String
typeerror ::   CompilerError * TypeError
te_message ::  TypeError * String [UNI]
te_nested ::   TypeError * TypeError
te_position :: TypeError * String [UNI]
te_origtype :: TypeError * String [UNI]
te_origname :: TypeError * String [UNI]
 - -}
--makeCtxErrorPops :: Options -> [(String,ClockTime)] -> ConceptIdentifier -> P_Context -> [P_Population]
--makeCtxErrorPops flags usrfiles eid pctx
-- = case cx of
--      Checked c -> makepopu ("te_message","TypeError","ErrorMessage") [] : makeRAPPops (makeFspec flags c) flags usrfiles []
--      Errors x ->  [makepopu ("te_message","TypeError","ErrorMessage") [(eid,nonsid (show x))]]
--   where (cx,_,_) = typeCheck nc []
--         nc = PCtx (ctx_nm pctx) (ctx_pos pctx) (ctx_lang pctx) (ctx_markup pctx) [] 
--                   [P_Pat (pt_nm p) (pt_pos p) (pt_end p) [] (pt_gns p) (pt_dcs p) [] [] [] [] | p<-ctx_pats pctx]
--                   [] [] [] [] [] [] [] [] [] [] [] [] []
{-makeCtxErrorPops eid c (Cxes xs) = []
makeCtxErrorPops eid c (CxeOrig cxe t nm o)
   | null cxe                                    = []
   | t `elem` ["pattern", "process", "interface"] = []
   | otherwise                                    = []
makeCtxErrorPops eid c (Cxe cxe x) = []
makeCtxErrorPops eid c CxeNone = []
makeCtxErrorPops eid c (PE msg) = []
-}


--make population for the user files on the server
--flags for file names and user name -> file names in the upload directory of the user
--                                   -> files that do not exist yet, but are reserved upfront e.g. files to save to
makeFilePops :: Options -> [(String,ClockTime)] -> [(String,String)] -> [P_Population]
makeFilePops flags usrfiles savefiles
 = let (adlfiles,popfiles,newfile) = rapfiles flags usrfiles
   in
     --see trunk/apps/Atlas/FSpec.adl
    [makepopu ("newfile","User","NewAdlFile")            [(usrid (usr flags), fileid newfile)]
    ,makepopu ("userrole","User","Role")                 [(usrid (usr flags), nonsid (case theme flags of StudentTheme->"Student";StudentDesignerTheme->"StudentDesigner";DesignerTheme->"Designer";_->"Student"))]
     --note that: 'srcfile' \/ inclfiles |- adlfiles \/ popfiles
    ,makepopu ("filename","File","FileName")             [(fileid (path,fn), nonsid fn)          | (path,fn, _  )<-adlfiles ++ popfiles]
    ,makepopu ("filename","File","FileName")             [(fileid (path,fn), nonsid fn)          | (path,fn     )<-newfile:savefiles ]
    ,makepopu ("filepath","File","FilePath")             [(fileid (path,fn), nonsid path)        | (path,fn, _  )<-adlfiles ++ popfiles]
    ,makepopu ("filepath","File","FilePath")             [(fileid (path,fn), nonsid path)        | (path,fn     )<-newfile:savefiles ]
    ,makepopu ("filetime","File","CalendarTime")         [(fileid (path,fn), nonsid (show time)) | (path,fn,time)<-adlfiles ++ popfiles]
--take 30 because of performance issue of prototype, takes too long to query the file overview pages
--the CONTEXT files page fired a query of over 500.000 chars to get an overview of 70 files, which took the browser 34secs using a localhost server
    ,makepopu ("uploaded","User","File")                 [(usrid (usr flags), fileid (path,fn))   | (path,fn, _  )<-take 30 adlfiles ++ take 30 popfiles]
    ,makepopu ("applyto","G","AdlFile")                  [(gid op fn, fileid (path,fn))          | (path,fn, _  )<-adlfiles, (op,_ )<-operations flags]
    ,makepopu ("functionname","G","String")              [(gid op fn, nonsid nm)                 | (_   ,fn, _  )<-adlfiles, (op,nm)<-operations flags]
    ,makepopu ("operation","G","Int")                    [(gid op fn, nonsid (show op))          | (_   ,fn, _  )<-adlfiles, (op,_ )<-operations flags]
    ]

--the fSpec to import into RAP -> flags for file names and user name -> file names in the upload directory of the user -> pictures for the fSpec
makeRAPPops :: Fspc -> Options -> [(String,ClockTime)] -> [Picture] -> [P_Population]
makeRAPPops fSpec flags usrfiles pics
 = let -- savepopfile is a SavePopFile (only POPULATIONS) which must be INCLUDEd to compile
       savepopfile = (tempdir, addExtension nextversion ".pop") 
       -- savectxfile is a SaveAdlFile in uploads/temp/ which should be renamed, moved, and loaded immediately to become an uploaded adl-file
       savectxfile = (tempdir, addExtension nextversion ".adl")
       --files will be saved in a temp dir first and moved next to check at the last moment that the file name does not exist yet
       tempdir =  combine (fst (srcfile flags)) "temp/"
       --mkversion drops extension
       mkversion i fnext 
         = let fn = dropExtension fnext
               revchunks = reverse(splitOn "." fn) --reverse to get the last at the head
               mkvchunk ('v':istr) = let ri = reads istr::[(Int,String)]
                                     in if null ri || (not . null . snd . head) ri --check whether ri is an integer or not
                                        then ('v':istr)++".v"++show i --add the version chunk to the non-version chunk which starts with a v
                                        else 'v':show i --replace the old version chunk with the new version chunk
               mkvchunk novchunk   = novchunk++".v"++show i --add the version chunk to the non-version chunk
           in if null revchunks then error "RAPImport.hs: no file name?"
              else intercalate "."$reverse(mkvchunk (head revchunks) : tail revchunks) --the last (head of reverse) should be a v(ersion)chunk
       --nextversion drops extension because mkversion does
       nextversion = let vs=[mkversion i fn | (i,fn)<-zip [(1::Int)..] ((repeat . snd . srcfile) flags)
                                            , mkversion i fn `notElem` map (dropExtension . fst) usrfiles]
                     in if null vs then error "RAPImport.hs: run out of next versions?" else head vs
       inclfiles = [(fst (srcfile flags),fn) | pos'<-fspos fSpec, let fn=takeFileName(filenm pos'), fn /= snd (srcfile flags)]
       cns = ctxns (srcfile flags)
   in
     --see trunk/apps/Atlas/FSpec.adl
     makeFilePops flags usrfiles [savepopfile,savectxfile]
     ++      
    [makepopu ("sourcefile","Context","AdlFile")         [(fsid (cns,fSpec), fileid (srcfile flags))]
    ,makepopu ("includes","Context","File")              [(fsid (cns,fSpec), fileid f)  | f<-inclfiles] 
    ,makepopu ("firstloadedwith","AdlFile","AdlVersion") [(fileid (srcfile flags), nonsid prototypeVersionStr)]
    ,makepopu ("savepopulation","Context","SavePopFile") [(fsid (cns,fSpec), fileid savepopfile)]
    ,makepopu ("savecontext","Context","SaveAdlFile")    [(fsid (cns,fSpec), fileid savectxfile)]
    ,makepopu ("imageurl","Image","URL")   [(imageid pic, nonsid[if c=='\\' then '/' else c | c<-addExtension (relPng pic) "png"])
                                                                       | pic<-pics]
    ,makepopu ("ptpic","Pattern","Image")  [(patid p    , imageid pic) | pic<-pics, pType pic==PTPattern, p<-patterns fSpec, name p==origName pic]
    ,makepopu ("rrpic","Rule","Image")     [(ruleid r   , imageid pic) | pic<-pics, pType pic==PTRule   , r<-udefrules fSpec   , name r==origName pic]
    ,makepopu ("cptpic","Concept","Image") [(cptid c    , imageid pic) | pic<-pics, pType pic==PTConcept, c<-concs fSpec   , name c==origName pic]
    ,makepopu ("countrules","Context","Int")  [(fsid (cns,fSpec), nonsid (show (length (udefrules fSpec))))]
    ,makepopu ("countdecls","Context","Int")  [(fsid (cns,fSpec), nonsid (show (length userdeclarations)))]
    ,makepopu ("countcpts","Context","Int")   [(fsid (cns,fSpec), nonsid (show (length (concs fSpec))))]
    ,makepopu ("rrviols","Rule","Violation") [(ruleid r, pairidid (x,y) (rulens r,r)) | (r,vs) <- allViolations fSpec, r `elem` raprules, (x,y)<-vs]
    ,makepopu ("decexample","Declaration","PragmaSentence") [(decid d , nonsid (decprL d++x++decprM d++y++decprR d))
                                                            | d<-userdeclarations, not(null (decprM d)), let (x,y) = head(pairsOf d++[("...","...")])]
    --see trunk/apps/Atlas/AST.adl
    ,makepopu ("ctxnm","Context","Conid")     [(fsid (cns,fSpec), nonsid (name fSpec))]
    ,makepopu ("ctxcs","Context","Concept")   [(fsid (cns,fSpec), cptid c)                | c<-concs fSpec] 
    ,makepopu ("cptnm","Concept","Conid")     [(cptid c      , nonsid (name c))        | c<-concs fSpec]
    ,makepopu ("cptos","Concept","AtomID")    [(cptid c      , atomidid x (isanm island)) | c<-concs fSpec, (island, c',x)<-atoms, c==c']
    ,makepopu ("inios","Concept","AtomID")    [(cptid c      , atomidid x (isanm island)) | c<-concs fSpec, (island, c',x)<-atoms, c==c']
    ,makepopu ("atomvalue","AtomID","Atom")   [(atomidid x (isanm island) , nonsid x)                  | (island, _ ,x)<-atoms]
    ,makepopu ("cptpurpose","Concept","Blob") [(cptid c      , nonsid (aMarkup2String (explMarkup ex)))
                                                                                       | c<-concs fSpec, ex<-explanations fSpec, explForObj c (explObj ex)]
    ,makepopu ("ordername","Order","String")  [(nonsid (isanm island) , nonsid (isanm island)) | island<-islands]
    ,makepopu ("order","Concept","Order")     [(cptid c            , nonsid (isanm island)) | island<-islands, c<-island]
    ,makepopu ("gengen","Isa","Concept") [(genid g, cptid (gengen g)) | g@Isa{}<-gens fSpec]
    ,makepopu ("genspc","Isa","Concept") [(genid g, cptid (genspc g)) | g<-gens fSpec]
    ,makepopu ("genrhs","Isa","Concept") [(genid g, cptid c) | g@IsE{}<-gens fSpec, c<-genrhs g]
    ,makepopu ("ctxpats","Context","Pattern")   [(fsid (cns,fSpec), patid p)         | p<-patterns fSpec]
    ,makepopu ("ptnm","Pattern","Conid")        [(patid p      , nonsid (name p)) | p<-patterns fSpec]
    ,makepopu ("ptrls","Pattern","Rule")        [(patid p      , ruleid r)        | p<-patterns fSpec, r<-udefrules p]
    ,makepopu ("ptrls","Pattern","Rule")        [(patid p      , ruleid r)        | p<-patterns fSpec, d<-declarations p,decusr d, pr<-rapmults d, let r=rulefromProp pr d]
    ,makepopu ("ptgns","Pattern","Isa")         [(patid p      , genid g)         | p<-patterns fSpec, g<-gens p]
    ,makepopu ("ptdcs","Pattern","Declaration") [(patid p      , decid d)         | p<-patterns fSpec, d<-declarations p,decusr d]
    ,makepopu ("ptxps","Pattern","Blob")        [(patid p, nonsid (aMarkup2String (explMarkup ex)))
                                                                                  | p<-patterns fSpec, ex<-explanations fSpec, explForObj p (explObj ex)]
    --RAP only knows PATTERN elements from a PROCESS, and reduces a PROCESS to a PATTERN with a name PROCESS_<name>
    ,makepopu ("ptpic","Pattern","Image")       [(prcid p    , imageid pic)       | pic<-pics, pType pic==PTProcess, p<-vprocesses fs, name p==origName pic]
    ,makepopu ("ctxpats","Context","Pattern")   [(fsid (cns,fs), prcid p)         | p<-vprocesses fs]
    ,makepopu ("ptnm","Pattern","Conid")        [(prcid p      , nonsid ("PROCESS_"++name p)) | p<-vprocesses fs]
    ,makepopu ("ptrls","Pattern","Rule")        [(prcid p      , ruleid r)        | p<-vprocesses fs, r<-rules (fpProc p)]
    ,makepopu ("ptrls","Pattern","Rule")        [(prcid p      , ruleid r)        | p<-vprocesses fs, d<-declarations (fpProc p),decusr d, pr<-rapmults d, let r=rulefromProp pr d]
    ,makepopu ("ptgns","Pattern","Gen")         [(prcid p      , genid g)         | p<-vprocesses fs, g<-gens (fpProc p)]
    ,makepopu ("ptdcs","Pattern","Declaration") [(prcid p      , decid d)         | p<-vprocesses fs, d<-declarations (fpProc p),decusr d]
    ,makepopu ("ptxps","Pattern","Blob")        [(prcid p, nonsid (aMarkup2String (explMarkup ex)))
                                                                                  | p<-vprocesses fs, ex<-explanations fs, explForObj (fpProc p) (explObj ex)]
    ,makepopu ("decnm","Declaration","Varid")               [(decid d , nonsid(name d))   | d<-userdeclarations]
    ,makepopu ("decsgn","Declaration","Sign")               [(decid d , sgnid (sign d))   | d<-userdeclarations]
    ,makepopu ("decprps","Declaration","PropertyRule")      [(decid d , ruleid r)         | d<-userdeclarations, p<-rapmults d, let r=rulefromProp p d]
    ,makepopu ("declaredthrough","PropertyRule","Property") [(ruleid r, nonsid(show p))   | d<-userdeclarations, p<-rapmults d, let r=rulefromProp p d]
    ,makepopu ("decprL","Declaration","String")             [(decid d , nonsid(decprL d)) | d<-userdeclarations]
    ,makepopu ("decprM","Declaration","String")             [(decid d , nonsid(decprM d)) | d<-userdeclarations]
    ,makepopu ("decprR","Declaration","String")             [(decid d , nonsid(decprR d)) | d<-userdeclarations]
    ,makepopu ("decmean","Declaration","Blob")              [(decid d , nonsid (aMarkup2String rdf)) 
                                                                                          | d<-userdeclarations, Just rdf <- [meaning Dutch d, meaning English d]]
    ,makepopu ("decpurpose","Declaration","Blob")           [(decid d , nonsid (aMarkup2String (explMarkup ex)))
                                                                                          | d<-userdeclarations, ex<-explanations fSpec, explForObj d (explObj ex)]
    ,makepopu ("decpopu","Declaration","PairID")            [(decid d , pairidid (x,y) (decns d,d)) 
                                                                                          | d<-userdeclarations, (x,y)<-pairsOf d]
    ,makepopu ("inipopu","Declaration","PairID")            [(decid d , pairidid (x,y) (decns d,d)) 
                                                                                          | d<-userdeclarations, (x,y)<-pairsOf d]
    ,makepopu ("inileft","PairID","Atom")                   [(pairidid (x,y) (decns d,d), nonsid x) 
                                                                                          | d<-userdeclarations, (x,y)<-pairsOf d]
    ,makepopu ("iniright","PairID","Atom")                   [(pairidid (x,y) (decns d,d), nonsid y) 
                                                                                          | d<-userdeclarations, (x,y)<-pairsOf d]
    ,makepopu ("reldcl","Relation","Declaration") [(relid (name d) (sign d), decid d)        | d<-userdeclarations]
    ,makepopu ("relnm","Relation","Varid")        [(relid (name d) (sign d), nonsid(name d)) | d<-userdeclarations]
    ,relsrc                          userdeclarations
    ,reltrg                          userdeclarations
    ,relleft  [(decns d, d, pairsOf d)     | d<-userdeclarations]
    ,relright [(decns d, d, pairsOf d)     | d<-userdeclarations]
    ,makepopu ("rrnm","Rule","ADLid")         [(ruleid r, nonsid (name r))                           | r<-raprules]
    ,makepopu ("rrexp","Rule","ExpressionID") [(ruleid r, expridid (rulens r,rrexp r))               | r<-raprules]
    ,makepopu ("rrmean","Rule","Blob")        [(ruleid r, nonsid (aMarkup2String rdf))               | r<-raprules, Just rdf <- [meaning Dutch r, meaning English r]]
    ,makepopu ("rrpurpose","Rule","Blob")     [(ruleid r, nonsid (aMarkup2String (explMarkup ex)))   | r<-raprules, ex<-explanations fSpec, explForObj r (explObj ex)]
    ,makepopu ("exprvalue","ExpressionID","Expression") 
                                              [(expridid (rulens r,rrexp r), nonsid (show(rrexp r))) | r<-raprules]
     -- link an expression to its relation terms 
     -- and create those of user-defined rules (those of property rules have already been created above).
     -- multiple creations of the same relation terms is not harmfull, because p2aconverters nubs populations.
    ,relrels      [(rulens r,rrexp r)          | r<-raprules]    
    ,relrelnm     (map       rrexp                 (udefrules fSpec))
    ,relreldcl    (map       rrexp                 (udefrules fSpec))
     --create pairs for violations (see rrviols above)
    ,relleft  [(rulens r,violationsexpr r,[ {-TODO: What should be in this list? -}]) | r<-raprules]
    ,relright [(rulens r,violationsexpr r,[ {-TODO: What should be in this list? -}]) | r<-raprules]
    ]
   where
   pairsOf :: Declaration -> [(String,String)]
   pairsOf d = case filter theDecl (initialPops fSpec) of
                 []    -> []
                 [pop] -> popps pop
                 _     -> fatal 273 "Multiple entries found in populationTable"
     where
       theDecl :: Population -> Bool
       theDecl p = popdcl p == d
   
   --SPEC PropertyRule ISA Rule
   raprules = udefrules fSpec ++ [rulefromProp p d | d<-userdeclarations, p<-rapmults d]
   rapmults = decprps
   --userdeclarations is defined because of absence of a function for user-defined declarations like rules for user-defined rules
   userdeclarations = filter decusr (declarations fSpec)
   --(order,specific qualification,value) => note: there may be more than one specific qualification for the same atom (island,x)
   atoms = [(island , c , x) 
           | island<-islands, c<-island, x<-atomsOf (initialPops fSpec) c
           , x `notElem` concat [atomsOf (initialPops fSpec) s | s<-island, s < c]]
   --the name of an isa-order is the combination of all maxima, in most cases there will be only one maximum.
   isanm island = intercalate "/" (map name (maxima island))
   --get the concept from the fSpec, not the isa-order, because the one in the isa-order is not populated
   (_,islands,_,_,_) = case [c | c@C{} <- concs fSpec] of
                           []  -> (undef,[],undef,undef,undef)
                           c:_ -> case c of 
                                    C{}  -> cptgE c
                                    ONE  -> fatal 299 "What to do with ONE???" --HJO, 20130216: Deze bug kwam aan het licht bij Roles.adl. Ik heb er de fatal message bij geplaatst, zodat diagnose eenvoudiger is.
                       where undef=fatal 297 "undef would cause a loop..."
   --populate relsrc and reltrg for typed data structures
   relsrc,reltrg :: Association r => [r] -> P_Population
   relsrc rs = makepopu ("src","Sign","Concept")      [(sgnid (sign r), cptid (source r)) | r<-rs]
   reltrg rs = makepopu ("trg","Sign","Concept")      [(sgnid (sign r), cptid (target r)) | r<-rs]
   --populate relleft and relright for populated and typed data structures
   relleft,relright :: (Association r) => [(IdentifierNamespace,r,[(String,String)])] -> P_Population
   relleft rs = makepopu ("left","PairID","AtomID")                [(pairidid (x,y) (ns,r), atomidid x (getisa$source r)) | (ns,r,pairs)<-rs, (x,y)<-pairs]
   relright rs = makepopu ("right","PairID","AtomID")              [(pairidid (x,y) (ns,r), atomidid y (getisa$target r)) | (ns,r,pairs)<-rs, (x,y)<-pairs]
   --makeLeft,makeRight :: [(IdentifierNamespace, [(ConceptIdentifier,ConceptIdentifier)])] -> P_Population
   --makeLeft  rs = makepopu ("left","PairID","AtomID")                [(pairidid (x,y) (ns,r), atomidid x (getisa$source r)) | (x,y)<-ps]
   --makeRight rs = makepopu ("left","PairID","AtomID")                [(pairidid (x,y) (ns,r), atomidid x (getisa$source r)) | (x,y)<-ps]
   
   getisa c = concat [isanm island | island<-islands, c `elem` island]
   --populate relrels, relrelnm, and relreldcl for expressions
   relrels :: [(IdentifierNamespace, Expression)] -> P_Population
   relrels exprs = makepopu ("rels","ExpressionID","Relation")   [(expridid (ns,expr), relid nm (sign d)) | (ns,expr)<-exprs, d@(Sgn{decnm=nm})<-declsUsedIn expr]
   relrelnm, relreldcl :: [Expression] -> P_Population
   relrelnm exprs = makepopu ("relnm","Relation","Varid")         [(relid nm (sign d), nonsid nm)         |      expr<-exprs, d@(Sgn{decnm=nm})<-declsUsedIn expr]
   relreldcl exprs = makepopu ("reldcl","Relation","Declaration") [(relid nm (sign d), decid d)           |      expr<-exprs, d@(Sgn{decnm=nm})<-declsUsedIn expr]
