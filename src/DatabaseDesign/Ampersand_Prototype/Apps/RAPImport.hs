{-# OPTIONS_GHC -Wall #-}
--import an fspec into the RAP specification
-- USE -> cmd: ampersand --importfile=some.adl --importformat=adl RAP.adl
module DatabaseDesign.Ampersand_Prototype.Apps.RAPImport   (importfspec)
where
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import DatabaseDesign.Ampersand_Prototype.Apps.RAPIdentifiers
import DatabaseDesign.Ampersand_Prototype.Apps.RAP         (picturesForAtlas)
import System.FilePath        (takeFileName,dropFileName,combine,addExtension,replaceExtension, takeExtension)
import System.Directory       (getDirectoryContents,doesDirectoryExist)
import Control.Monad

importfspec ::  Fspc -> Options -> IO [P_Population]
importfspec fspec opts 
 = let pics = picturesForAtlas opts fspec
       fdir = let d=dropFileName (importfile opts) in if null d then "." else d
   in  do verbose opts "Writing pictures for RAP... "
          sequence_ [writePicture opts pict | pict <- pics]
          verbose opts "Getting all uploaded adl-files of RAP user... "
          usrfiles <- getDirectoryContents fdir >>= filterM (fmap not . (\x -> doesDirectoryExist (combine fdir x) ))
          return (makeRAPPops fspec opts usrfiles pics)

--a triple which should correspond to a declaration from RAP.adl: (relation name, source name, target name)
--since the populations made by makeRAPPops will be added to the parsetree of RAP.adl, they will be checked and processed by p2aconverters
type RAPRelation = (String,String,String)
makepopu :: RAPRelation -> [(ConceptIdentifier,ConceptIdentifier)] -> P_Population
makepopu (r,src,trg) xys
 = P_Popu{ p_popm  = P_Rel r (Origin "RAPImport.hs")
         , p_type  = P_Sign [PCpt src, PCpt trg]
         , p_popps = [mkPair (trim (getid x)) (trim (getid y)) |(x,y)<-xys, not(null (getid x)), not(null (getid y)) ]
         }
--the fspec to import into RAP -> flags for file names and user name -> file names in the upload directory of the user -> pictures for the fspec
makeRAPPops :: Fspc -> Options -> [String] -> [Picture] -> [P_Population]
makeRAPPops fs opts usrfiles pics
 = let usr = namespace opts
       operations = [(1,"laden")]
       srcfile = (dropFileName(importfile opts),takeFileName(importfile opts))
       specfiles@[newfile,savepopfile,savectxfile]
               = [("","empty.adl") -- a new file is a copy of empty.adl, it contains an empty context
                 ,(fst srcfile, replaceExtension (snd srcfile) ".pop") -- .pop is a SavePopFile (only POPULATIONS) which must be INCLUDEd to compile
                 ,(combine (fst srcfile) "temp/", replaceExtension (snd srcfile) ".adl")] -- .adl is a SaveAdlFile in uploads/temp/ which should be renamed, moved, and loaded immediately to become an uploaded adl-file
       inclfiles = [(fst srcfile,fn) | pos'<-fspos fs, let fn=takeFileName(filenm pos'), fn /= snd srcfile]
       adlfiles  = [(dropFileName(importfile opts),fn) | fn<-usrfiles,takeExtension fn==".adl"]
       popfiles  = [(dropFileName(importfile opts),fn) | fn<-usrfiles,takeExtension fn==".pop"]
   in
     --see trunk/apps/Atlas/FSpec.adl
     makepopu ("sourcefile","Context","AdlFile")         [(fsid fs        , fileid srcfile)]
    :makepopu ("savepopulation","Context","SavePopFile") [(fsid fs        , fileid savepopfile)]
    :makepopu ("savecontext","Context","SaveAdlFile")    [(fsid fs        , fileid savectxfile)]
    :makepopu ("newfile","User","NewAdlFile")            [(usrid usr      , fileid newfile)]
    :makepopu ("includes","Context","File")              [(fsid fs        , fileid f)   | f          <-            inclfiles] 
    :makepopu ("filename","File","FileName")             [(fileid f, nonsid fn)         | f@(_   ,fn)<-adlfiles ++ inclfiles ++ specfiles ++ popfiles]
    :makepopu ("filepath","File","FilePath")             [(fileid f, nonsid path)       | f@(path,_ )<-adlfiles ++ inclfiles ++ specfiles ++ popfiles]
    :makepopu ("uploaded","User","File")                 [(usrid usr, fileid f)         | f          <-adlfiles ++ popfiles]
    :makepopu ("applyto","G","AdlFile")                  [(gid op fn, fileid f)         | f@(_   ,fn)<-adlfiles, (op,_ )<-operations]
    :makepopu ("functionname","G","String")              [(gid op fn, nonsid nm)        |   (_   ,fn)<-adlfiles, (op,nm)<-operations]
    :makepopu ("operation","G","Int")                    [(gid op fn, nonsid (show op)) |   (_   ,fn)<-adlfiles, (op,_ )<-operations]
    :makepopu ("imageurl","Image","URL")   [(imageid pic, nonsid[if c=='\\' then '/' else c | c<-addExtension (relPng pic) "png"])
                                                                       | pic<-pics]
    :makepopu ("ptpic","Pattern","Image")  [(patid p    , imageid pic) | pic<-pics, pType pic==PTPattern, p<-patterns fs, name p==origName pic]
    :makepopu ("rrpic","Rule","Image")     [(ruleid r   , imageid pic) | pic<-pics, pType pic==PTRule   , r<-rules fs   , name r==origName pic]
    :makepopu ("cptpic","Concept","Image") [(cptid c    , imageid pic) | pic<-pics, pType pic==PTConcept, c<-concs fs   , name c==origName pic]
    :makepopu ("countrules","Context","Int")  [(fsid fs  , nonsid (show (length (rules fs))))]
    :makepopu ("countdecls","Context","Int")  [(fsid fs  , nonsid (show (length userdeclarations)))]
    :makepopu ("countcpts","Context","Int")   [(fsid fs  , nonsid (show (length (concs fs))))]
    :makepopu ("rrviols","Rule","Violation") [(ruleid r, pairidid (x,y) (rulens r,r)) | r<-raprules, (x,y)<-ruleviolations r]
    --see trunk/apps/Atlas/AST.adl
    :makepopu ("ctxnm","Context","Conid")     [(fsid fs  , nonsid (name fs))]
    :makepopu ("ctxcs","Context","Concept")   [(fsid fs     , cptid c)          | c<-concs fs] 
    :makepopu ("cptnm","Concept","Conid")     [(cptid c     , nonsid (name c))  | c<-concs fs]
    :makepopu ("cptos","Concept","AtomID")    [(cptid c     , atomidid x c)     | c<-concs fs, x<-cptos c]
    :makepopu ("atomvalue","AtomID","Atom")   [(atomidid x c, nonsid x)         | c<-concs fs, x<-cptos c]
    :makepopu ("cptpurpose","Concept","Blob") [(cptid c     , nonsid (aMarkup2String (explMarkup ex)))
                                                                                | c<-concs fs, ex<-explanations fs, explForObj c (explObj ex)]
    :makepopu ("cptdf","Concept","Blob")      [(cptid c     , nonsid(cddef cd)) | c<-concs fs, cd<-cptdf c]
    :makepopu ("gengen","Gen","Concept") [(genid g, cptid (target g)) | g<-gens fs]
    :makepopu ("genspc","Gen","Concept") [(genid g, cptid (source g)) | g<-gens fs]
    :makepopu ("ctxpats","Context","Pattern")   [(fsid fs, patid p)         | p<-patterns fs]
    :makepopu ("ptnm","Pattern","Conid")        [(patid p, nonsid (name p)) | p<-patterns fs]
    :makepopu ("ptrls","Pattern","Rule")        [(patid p, ruleid r)        | p<-patterns fs, r<-rules p]
    :makepopu ("ptgns","Pattern","Gen")         [(patid p, genid g)         | p<-patterns fs, g<-gens p]
    :makepopu ("ptdcs","Pattern","Declaration") [(patid p, decid d)         | p<-patterns fs, d<-declarations p,decusr d]
    :makepopu ("ptxps","Pattern","Blob")        [(patid p, nonsid (aMarkup2String (explMarkup ex)))
                                                                            | p<-patterns fs, ex<-explanations fs, explForObj p (explObj ex)]
    :makepopu ("decnm","Declaration","Varid")               [(decid d , nonsid(name d))   | d<-userdeclarations]
    :makepopu ("decsgn","Declaration","Sign")               [(decid d , sgnid (sign d))   | d<-userdeclarations]
    :makepopu ("decprps","Declaration","PropertyRule")      [(decid d , ruleid r)         | d<-userdeclarations, p<-multiplicities d, let r=rulefromProp userdeclarations p d]
    :makepopu ("declaredthrough","PropertyRule","Property") [(ruleid r, nonsid(show p))   | d<-userdeclarations, p<-multiplicities d, let r=rulefromProp userdeclarations p d]
    :makepopu ("decprL","Declaration","String")             [(decid d , nonsid(decprL d)) | d<-userdeclarations]
    :makepopu ("decprM","Declaration","String")             [(decid d , nonsid(decprM d)) | d<-userdeclarations]
    :makepopu ("decprR","Declaration","String")             [(decid d , nonsid(decprR d)) | d<-userdeclarations]
    :makepopu ("decmean","Declaration","Blob")              [(decid d , nonsid (aMarkup2String rdf)) 
                                                                                          | d<-userdeclarations, Just rdf <- meaning Dutch d:meaning English d:[]]
    :makepopu ("decpurpose","Declaration","Blob")           [(decid d , nonsid (aMarkup2String (explMarkup ex)))
                                                                                          | d<-userdeclarations, ex<-explanations fs, explForObj d (explObj ex)]
    :makepopu ("decpopu","Declaration","PairID")            [(decid d , pairidid (x,y) (decns d,d)) 
                                                                                          | d<-userdeclarations, (x,y)<-contents d]
    :makepopu ("reldcl","Relation","Declaration") [(relid (name d) (sign d), decid d)        | d<-userdeclarations]
    :makepopu ("relnm","Relation","Varid")        [(relid (name d) (sign d), nonsid(name d)) | d<-userdeclarations]
    :makepopu ("relsgn","Relation","Sign")        [(relid (name d) (sign d), sgnid (sign d)) | d<-userdeclarations]
    :relsrc                          userdeclarations
    :reltrg                          userdeclarations
    :relpairvalue [(decns d, d) | d<-userdeclarations]
    :relleft                         userdeclarations
    :relright                        userdeclarations
    :makepopu ("rrnm","Rule","ADLid")         [(ruleid r, nonsid (name r))                           | r<-raprules]
    :makepopu ("rrexp","Rule","ExpressionID") [(ruleid r, expridid (rulens r,rrexp r))               | r<-raprules]
    :makepopu ("rrmean","Rule","Blob")        [(ruleid r, nonsid (aMarkup2String rdf))               | r<-raprules, Just rdf <- meaning Dutch r:meaning English r:[]]
    :makepopu ("rrpurpose","Rule","Blob")     [(ruleid r, nonsid (aMarkup2String (explMarkup ex)))   | r<-raprules, ex<-explanations fs, explForObj r (explObj ex)]
    :makepopu ("exprvalue","ExpressionID","Expression") 
                                              [(expridid (rulens r,rrexp r), nonsid (show(rrexp r))) | r<-raprules]
     -- link an expression to its relation terms 
     -- and create those of user-defined rules (those of property rules have already been created above).
     -- multiple creations of the same relation terms is not harmfull, because p2aconverters nubs populations.
    :relrels      [(rulens r,rrexp r)          | r<-raprules]    
    :relrelnm     (map       rrexp                 (rules fs))
    :relrelsgn    (map       rrexp                 (rules fs))
    :relreldcl    (map       rrexp                 (rules fs))
     --create pairs for violations (see rrviols above)
    :relpairvalue [(rulens r,violationsexpr r) | r<-raprules]
    :relleft      (map       violationsexpr        (raprules))
    :relright     (map       violationsexpr        (raprules))
    :[]
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
