{-# OPTIONS_GHC -Wall #-}
--import an fspec into the RAP specification
-- USE -> cmd: ampersand --importfile=some.adl --importformat=adl RAP.adl
module DatabaseDesign.Ampersand_Prototype.Apps.RAPImport   (importfspec,importfailed)
where
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import DatabaseDesign.Ampersand_Prototype.Apps.RAPIdentifiers
import DatabaseDesign.Ampersand_Prototype.Apps.RAP         (picturesForAtlas)
import System.FilePath        (takeFileName,dropFileName,combine,addExtension, takeExtension, dropExtension)
import System.Directory       (getDirectoryContents,doesDirectoryExist,getModificationTime)
import System.Time
import qualified GHC.Exts (sortWith)
import Control.Monad
import Data.List (intercalate)
import Data.List.Split (splitOn)

-----------------------------------------------------------------------------
--exported functions---------------------------------------------------------
-----------------------------------------------------------------------------
importfspec ::  Fspc -> Options -> IO [P_Population]
importfspec fspec opts 
 = let pics = picturesForAtlas opts fspec
   in  do verbose opts "Writing pictures for RAP... "
          sequence_ [writePicture opts pict | pict <- pics]
          verbose opts "Getting all uploaded adl-files of RAP user... "
          usrfiles <- getUsrFiles opts
          return (makeRAPPops fspec opts usrfiles pics)

importfailed :: String -> Options -> IO [P_Population]
importfailed imperr opts 
 = do verbose opts "Getting all uploaded adl-files of RAP user... "
      usrfiles <- getUsrFiles opts
      return (makeFailedPops imperr opts usrfiles)

-----------------------------------------------------------------------------
--common local functions-----------------------------------------------------
-----------------------------------------------------------------------------
getUsrFiles :: Options -> IO [(String,ClockTime)]
getUsrFiles opts = let fdir = let d=dropFileName (importfile opts) in if null d then "." else d
                   in  do {fns<-getDirectoryContents fdir >>= filterM (fmap not . (\x -> doesDirectoryExist (combine fdir x) ))
                          ;times<-sequence (map (getModificationTime . combine fdir) fns)
                          ;if length fns==length times
                           then return (reverse$GHC.Exts.sortWith snd (zip fns times))
                           else return (zip fns (repeat (toClockTime $ CalendarTime 1980 February 27 16 15 0 0 Wednesday 0 "UTC" 0 False)))
                          }
operations :: [(Int,String)]
operations = [(1,"laden"),(5,"genereer func.spec.(pdf)")]
usr :: Options -> String
usr = namespace
srcfile :: Options -> (String,String)
srcfile opts = (dropFileName(importfile opts),takeFileName(importfile opts))
rapfiles :: Options -> [(String,ClockTime)] -> ([(String,String,ClockTime)],[(String,String,ClockTime)],(String,String))
rapfiles opts usrfiles 
 = ( [(dropFileName(importfile opts),fn,time) | (fn,time)<-usrfiles,takeExtension fn==".adl"] --adlfiles, server files of user with a .adl extension
   , [(dropFileName(importfile opts),fn,time) | (fn,time)<-usrfiles,takeExtension fn==".pop"] --popfiles, server files of user with a .pop extension
   , ("","empty.adl")                                                             --newfile, a copy of empty.adl, it contains an empty context
   )
--a triple which should correspond to a declaration from RAP.adl: (relation name, source name, target name)
--since the populations made by makeRAPPops will be added to the parsetree of RAP.adl, they will be checked and processed by p2aconverters
type RAPRelation = (String,String,String)
makepopu :: RAPRelation -> [(ConceptIdentifier,ConceptIdentifier)] -> P_Population
makepopu (r,src,trg) xys
 = P_Popu{ p_popm  = P_Rel r (Origin "RAPImport.hs")
         , p_type  = P_Sign [PCpt src, PCpt trg]
         , p_popps = [mkPair (trim (getid x)) (trim (getid y)) |(x,y)<-xys, not(null (getid x)), not(null (getid y)) ]
         }

-----------------------------------------------------------------------------
--make population functions--------------------------------------------------
-----------------------------------------------------------------------------
--make population for the import that failed due to a parse or type error
makeFailedPops :: String -> Options -> [(String,ClockTime)] -> [P_Population]
makeFailedPops imperr opts usrfiles 
 =   --see trunk/apps/Atlas/RAP.adl
     makepopu ("compilererror","File","ErrorMessage")    [(fileid (srcfile opts)  , nonsid imperr)]
    :makeFilePops opts usrfiles []

--make population for the user files on the server
--flags for file names and user name -> file names in the upload directory of the user
--                                   -> files that do not exist yet, but are reserved upfront e.g. files to save to
makeFilePops :: Options -> [(String,ClockTime)] -> [(String,String)] -> [P_Population]
makeFilePops opts usrfiles savefiles
 = let (adlfiles,popfiles,newfile) = rapfiles opts usrfiles
   in
     --see trunk/apps/Atlas/FSpec.adl
    [makepopu ("newfile","User","NewAdlFile")            [(usrid (usr opts), fileid newfile)]
     --note that: 'srcfile' \/ inclfiles |- adlfiles \/ popfiles
    ,makepopu ("filename","File","FileName")             [(fileid (path,fn), nonsid fn)          | (path,fn, _  )<-adlfiles ++ popfiles]
    ,makepopu ("filename","File","FileName")             [(fileid (path,fn), nonsid fn)          | (path,fn     )<-newfile:savefiles ]
    ,makepopu ("filepath","File","FilePath")             [(fileid (path,fn), nonsid path)        | (path,fn, _  )<-adlfiles ++ popfiles]
    ,makepopu ("filepath","File","FilePath")             [(fileid (path,fn), nonsid path)        | (path,fn     )<-newfile:savefiles ]
    ,makepopu ("filetime","File","CalendarTime")         [(fileid (path,fn), nonsid (show time)) | (path,fn,time)<-adlfiles ++ popfiles]
    ,makepopu ("uploaded","User","File")                 [(usrid (usr opts), fileid (path,fn))   | (path,fn, _  )<-adlfiles ++ popfiles]
    ,makepopu ("applyto","G","AdlFile")                  [(gid op fn, fileid (path,fn))          | (path,fn, _  )<-adlfiles, (op,_ )<-operations]
    ,makepopu ("functionname","G","String")              [(gid op fn, nonsid nm)                 | (_   ,fn, _  )<-adlfiles, (op,nm)<-operations]
    ,makepopu ("operation","G","Int")                    [(gid op fn, nonsid (show op))          | (_   ,fn, _  )<-adlfiles, (op,_ )<-operations]
    ]

--the fspec to import into RAP -> flags for file names and user name -> file names in the upload directory of the user -> pictures for the fspec
makeRAPPops :: Fspc -> Options -> [(String,ClockTime)] -> [Picture] -> [P_Population]
makeRAPPops fs opts usrfiles pics
 = let -- savepopfile is a SavePopFile (only POPULATIONS) which must be INCLUDEd to compile
       savepopfile = (tempdir, addExtension nextversion ".pop") 
       -- savectxfile is a SaveAdlFile in uploads/temp/ which should be renamed, moved, and loaded immediately to become an uploaded adl-file
       savectxfile = (tempdir, addExtension nextversion ".adl")
       --files will be saved in a temp dir first and moved next to check at the last moment that the file name does not exist yet
       tempdir =  combine (fst (srcfile opts)) "temp/"
       --mkversion drops extension
       mkversion i fnext 
         = let fn = dropExtension fnext
               revchunks = reverse(splitOn "." fn) --reverse to get the last at the head
               mkvchunk ('v':istr) = let ri = (reads istr)::[(Int,String)]
                                     in if null ri || (not . null . snd . head) ri --check whether ri is an integer or not
                                        then ('v':istr)++".v"++show i --add the version chunk to the non-version chunk which starts with a v
                                        else 'v':show i --replace the old version chunk with the new version chunk
               mkvchunk novchunk   = novchunk++".v"++show i --add the version chunk to the non-version chunk
           in if null revchunks then error "RAPImport.hs: no file name?"
              else intercalate "."$reverse(mkvchunk (head revchunks) : tail revchunks) --the last (head of reverse) should be a v(ersion)chunk
       --nextversion drops extension because mkversion does
       nextversion = let vs=[mkversion i fn | (i,fn)<-zip [(1::Int)..] ((repeat . snd . srcfile) opts)
                                            , not(elem (mkversion i fn) (map (dropExtension . fst) usrfiles))]
                     in if null vs then error "RAPImport.hs: run out of next versions?" else head vs
       inclfiles = [(fst (srcfile opts),fn) | pos'<-fspos fs, let fn=takeFileName(filenm pos'), fn /= snd (srcfile opts)]
       cns = ctxns (srcfile opts)
   in
     --see trunk/apps/Atlas/FSpec.adl
     makeFilePops opts usrfiles [savepopfile,savectxfile]
     ++ 
    [makepopu ("sourcefile","Context","AdlFile")         [(fsid (cns,fs), fileid (srcfile opts))]
    ,makepopu ("includes","Context","File")              [(fsid (cns,fs), fileid f)  | f<-inclfiles] 
    ,makepopu ("savepopulation","Context","SavePopFile") [(fsid (cns,fs), fileid savepopfile)]
    ,makepopu ("savecontext","Context","SaveAdlFile")    [(fsid (cns,fs), fileid savectxfile)]
    ,makepopu ("imageurl","Image","URL")   [(imageid pic, nonsid[if c=='\\' then '/' else c | c<-addExtension (relPng pic) "png"])
                                                                       | pic<-pics]
    ,makepopu ("ptpic","Pattern","Image")  [(patid p    , imageid pic) | pic<-pics, pType pic==PTPattern, p<-patterns fs, name p==origName pic]
    ,makepopu ("rrpic","Rule","Image")     [(ruleid r   , imageid pic) | pic<-pics, pType pic==PTRule   , r<-rules fs   , name r==origName pic]
    ,makepopu ("cptpic","Concept","Image") [(cptid c    , imageid pic) | pic<-pics, pType pic==PTConcept, c<-concs fs   , name c==origName pic]
    ,makepopu ("countrules","Context","Int")  [(fsid (cns,fs), nonsid (show (length (rules fs))))]
    ,makepopu ("countdecls","Context","Int")  [(fsid (cns,fs), nonsid (show (length userdeclarations)))]
    ,makepopu ("countcpts","Context","Int")   [(fsid (cns,fs), nonsid (show (length (concs fs))))]
    ,makepopu ("rrviols","Rule","Violation") [(ruleid r, pairidid (x,y) (rulens r,r)) | r<-raprules, (x,y)<-ruleviolations r]
    ,makepopu ("decexample","Declaration","PragmaSentence") [(decid d , nonsid (decprL d++x++decprM d++y++decprR d))
                                                            | d<-userdeclarations, not(null (decprM d)), let (x,y) = head(contents d++[("...","...")])]
    --see trunk/apps/Atlas/AST.adl
    ,makepopu ("ctxnm","Context","Conid")     [(fsid (cns,fs), nonsid (name fs))]
    ,makepopu ("ctxcs","Context","Concept")   [(fsid (cns,fs) , cptid c)          | c<-concs fs] 
    ,makepopu ("cptnm","Concept","Conid")     [(cptid c       , nonsid (name c))  | c<-concs fs]
    ,makepopu ("cptos","Concept","AtomID")    [(cptid c       , atomidid x c)     | c<-concs fs, x<-cptos c]
    ,makepopu ("inios","Concept","AtomID")    [(cptid c       , atomidid x c)     | c<-concs fs, x<-cptos c]
    ,makepopu ("atomvalue","AtomID","Atom")   [(atomidid x c  , nonsid x)         | c<-concs fs, x<-cptos c]
    ,makepopu ("cptpurpose","Concept","Blob") [(cptid c       , nonsid (aMarkup2String (explMarkup ex)))
                                                                                  | c<-concs fs, ex<-explanations fs, explForObj c (explObj ex)]
    ,makepopu ("cptdf","Concept","Blob")      [(cptid c       , nonsid(cddef cd)) | c<-concs fs, cd<-cptdf c]
    ,makepopu ("gengen","Gen","Concept") [(genid g, cptid (target g)) | g<-gens fs]
    ,makepopu ("genspc","Gen","Concept") [(genid g, cptid (source g)) | g<-gens fs]
    ,makepopu ("ctxpats","Context","Pattern")   [(fsid (cns,fs), patid p)         | p<-patterns fs]
    ,makepopu ("ptnm","Pattern","Conid")        [(patid p      , nonsid (name p)) | p<-patterns fs]
    ,makepopu ("ptrls","Pattern","Rule")        [(patid p      , ruleid r)        | p<-patterns fs, r<-rules p]
    ,makepopu ("ptrls","Pattern","Rule")        [(patid p      , ruleid r)        | p<-patterns fs, d<-declarations p,decusr d, pr<-multiplicities d, let r=rulefromProp userdeclarations pr d]
    ,makepopu ("ptgns","Pattern","Gen")         [(patid p      , genid g)         | p<-patterns fs, g<-gens p]
    ,makepopu ("ptdcs","Pattern","Declaration") [(patid p      , decid d)         | p<-patterns fs, d<-declarations p,decusr d]
    ,makepopu ("ptxps","Pattern","Blob")        [(patid p, nonsid (aMarkup2String (explMarkup ex)))
                                                                                  | p<-patterns fs, ex<-explanations fs, explForObj p (explObj ex)]
    ,makepopu ("decnm","Declaration","Varid")               [(decid d , nonsid(name d))   | d<-userdeclarations]
    ,makepopu ("decsgn","Declaration","Sign")               [(decid d , sgnid (sign d))   | d<-userdeclarations]
    ,makepopu ("decprps","Declaration","PropertyRule")      [(decid d , ruleid r)         | d<-userdeclarations, p<-multiplicities d, let r=rulefromProp userdeclarations p d]
    ,makepopu ("declaredthrough","PropertyRule","Property") [(ruleid r, nonsid(show p))   | d<-userdeclarations, p<-multiplicities d, let r=rulefromProp userdeclarations p d]
    ,makepopu ("decprL","Declaration","String")             [(decid d , nonsid(decprL d)) | d<-userdeclarations]
    ,makepopu ("decprM","Declaration","String")             [(decid d , nonsid(decprM d)) | d<-userdeclarations]
    ,makepopu ("decprR","Declaration","String")             [(decid d , nonsid(decprR d)) | d<-userdeclarations]
    ,makepopu ("decmean","Declaration","Blob")              [(decid d , nonsid (aMarkup2String rdf)) 
                                                                                          | d<-userdeclarations, Just rdf <- [meaning Dutch d, meaning English d]]
    ,makepopu ("decpurpose","Declaration","Blob")           [(decid d , nonsid (aMarkup2String (explMarkup ex)))
                                                                                          | d<-userdeclarations, ex<-explanations fs, explForObj d (explObj ex)]
    ,makepopu ("decpopu","Declaration","PairID")            [(decid d , pairidid (x,y) (decns d,d)) 
                                                                                          | d<-userdeclarations, (x,y)<-contents d]
    ,makepopu ("inipopu","Declaration","PairID")            [(decid d , pairidid (x,y) (decns d,d)) 
                                                                                          | d<-userdeclarations, (x,y)<-contents d]
    ,makepopu ("reldcl","Relation","Declaration") [(relid (name d) (sign d), decid d)        | d<-userdeclarations]
    ,makepopu ("relnm","Relation","Varid")        [(relid (name d) (sign d), nonsid(name d)) | d<-userdeclarations]
    ,makepopu ("relsgn","Relation","Sign")        [(relid (name d) (sign d), sgnid (sign d)) | d<-userdeclarations]
    ,relsrc                          userdeclarations
    ,reltrg                          userdeclarations
    ,relpairvalue [(decns d, d) | d<-userdeclarations]
    ,relleft                         userdeclarations
    ,relright                        userdeclarations
    ,makepopu ("rrnm","Rule","ADLid")         [(ruleid r, nonsid (name r))                           | r<-raprules]
    ,makepopu ("rrexp","Rule","ExpressionID") [(ruleid r, expridid (rulens r,rrexp r))               | r<-raprules]
    ,makepopu ("rrmean","Rule","Blob")        [(ruleid r, nonsid (aMarkup2String rdf))               | r<-raprules, Just rdf <- [meaning Dutch r, meaning English r]]
    ,makepopu ("rrpurpose","Rule","Blob")     [(ruleid r, nonsid (aMarkup2String (explMarkup ex)))   | r<-raprules, ex<-explanations fs, explForObj r (explObj ex)]
    ,makepopu ("exprvalue","ExpressionID","Expression") 
                                              [(expridid (rulens r,rrexp r), nonsid (show(rrexp r))) | r<-raprules]
     -- link an expression to its relation terms 
     -- and create those of user-defined rules (those of property rules have already been created above).
     -- multiple creations of the same relation terms is not harmfull, because p2aconverters nubs populations.
    ,relrels      [(rulens r,rrexp r)          | r<-raprules]    
    ,relrelnm     (map       rrexp                 (rules fs))
    ,relrelsgn    (map       rrexp                 (rules fs))
    ,relreldcl    (map       rrexp                 (rules fs))
     --create pairs for violations (see rrviols above)
    ,relpairvalue [(rulens r,violationsexpr r) | r<-raprules]
    ,relleft      (map       violationsexpr         raprules )
    ,relright     (map       violationsexpr         raprules )
    ]
   where 
   --SPEC PropertyRule ISA Rule
   raprules = rules fs ++ [rulefromProp userdeclarations p d | d<-userdeclarations, p<-multiplicities d]
   --userdeclarations is defined because of absence of a function for user-defined declarations like rules for user-defined rules
   userdeclarations = filter decusr (declarations fs)
   --populate relsrc and reltrg for typed data structures
   relsrc,reltrg :: Association r => [r] -> P_Population
   relsrc rs = makepopu ("src","Sign","Concept")      [(sgnid (sign r), cptid (source r)) | r<-rs]
   reltrg rs = makepopu ("trg","Sign","Concept")      [(sgnid (sign r), cptid (target r)) | r<-rs]
   --make pair identifiers for populated and typed data structures r with a namespace restricted to r
   relpairvalue :: (Populated r,Association r) => [(IdentifierNamespace,r)] -> P_Population
   relpairvalue rs = makepopu ("pairvalue","PairID","Pair")      [(pairidid (x,y) (ns,r), pairid (x,y) (sign r)) | (ns,r)<-rs, (x,y)<-contents r]
   --populate relleft and relright for populated and typed data structures
   relleft,relright :: (Populated r,Association r) => [r] -> P_Population
   relleft rs = makepopu ("left","Pair","AtomID")                [(pairid (x,y) (sign r), atomidid x (source r)) |      r<-rs, (x,y)<-contents r]
   relright rs = makepopu ("right","Pair","AtomID")              [(pairid (x,y) (sign r), atomidid y (target r)) |      r<-rs, (x,y)<-contents r]
   --populate relrels, relrelnm, relreldcl and relrelsgn for expressions
   relrels :: [(IdentifierNamespace, Expression)] -> P_Population
   relrels exprs = makepopu ("rels","ExpressionID","Relation")   [(expridid (ns,expr), relid nm sgn) | (ns,expr)<-exprs, Rel{relnm=nm,relsgn=sgn}<-mors expr]
   relrelnm, relreldcl, relrelsgn :: [Expression] -> P_Population
   relrelnm exprs = makepopu ("relnm","Relation","Varid")         [(relid nm sgn, nonsid nm)         |      expr<-exprs, Rel{relnm=nm,relsgn=sgn}<-mors expr]
   relrelsgn exprs = makepopu ("relsgn","Relation","Sign")        [(relid nm sgn, sgnid sgn)         |      expr<-exprs, Rel{relnm=nm,relsgn=sgn}<-mors expr]
   relreldcl exprs = makepopu ("reldcl","Relation","Declaration") [(relid nm sgn, decid d)           |      expr<-exprs, Rel{relnm=nm,relsgn=sgn,reldcl=d}<-mors expr]
