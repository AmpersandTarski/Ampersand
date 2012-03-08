{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
--import an ADL1 file into the RAP specification
-- USE -> cmd: ampersand --importfile=some.adl --importformat=adl RAP.adl
module DatabaseDesign.Ampersand_Prototype.Apps.RAPImport   (makeRAPPops)
where
import Data.List
import Data.HashTable (hashString)
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import System.FilePath        (takeFileName,dropFileName)

--(relation name, source name, target name)
type RAPRelation = (String,String,String)

class RAPImportable a where
 rappops  :: a -> [P_Population]
instance RAPImportable a => RAPImportable [a] where
 rappops xs = concat (map rappops xs)

makepopu :: RAPRelation -> [(ConceptIdentifier,ConceptIdentifier)] -> P_Population
makepopu (r,src,trg) xys
 = P_Popu{ p_popm  = P_Rel r (Origin "RAPImport.hs")
         , p_type  = P_Sign [PCpt src, PCpt trg]
         , p_popps = [mkPair (trim x) (trim y) |(CID x,CID y)<-xys, not(null x), not(null y) ]
         }
----------------------------------------------------------------------------------
--these data types would force the developer to use the concept identifier and identifier namespace functions if they were abstract
--now they help to remind the developer to use concept identifier and identifier namespace functions instead of functions like name or show
data ConceptIdentifier = CID String
getid (CID x) = x
data IdentifierNamespace = CNS String

--concept identifier functions (concepts of RAP)
--there are no explicit concept identifier functions for the identifier of an atom x of a RAP concept on the outside of the conceptual model
-- e.g. Blob, String, Varid. Just code (CID x)
fsid fs = CID $ name fs
cptid c = CID $ name c
patid p = CID $ name p
ruleid r = CID $ name r
genid g = CID $ "(" ++ getid(cptid (source g)) ++ "," ++ getid(cptid (target g)) ++ ")"
sgnid sgn = CID $ getid(cptid (source sgn)) ++ "*" ++ getid(cptid (target sgn))
decid d = CID $ name d ++ "::" ++ getid(cptid (source d)) ++ "*" ++ getid(cptid (target d))
relid nm sgn = CID $ nm ++ "[" ++ getid(cptid (source sgn)) ++ "*" ++ getid(cptid (target sgn)) ++ "]"
expridid (CNS ns,expr) = CID $ ns ++ "#" ++ show expr
atomidid x c  = CID $ show$hashString (x ++ "[" ++ getid(cptid c) ++ "]") --limit of data length in database is 256
pairid (x,y) sgn = CID $ show$hashString (x  ++ "*" ++ y ++ "[" ++ getid(sgnid sgn) ++ "]") --limit of data length in database is 256
pairidid (x,y) (CNS ns,r) = CID $ ns ++ "#" ++ getid(pairid (x,y) (sign r))
imageid pic = CID$"Image_" ++ uniqueName pic
fileid fn = CID fn
usrid usr = CID usr
gid g fn = CID (getid g++fn)

--identifier namespace functions (namespaces for concept identifiers with a different namespace than CONTEXT)
rulens r = CNS $ "Rule_" ++ getid(ruleid r)
decns d  = CNS $ "Declaration_" ++ getid(decid d)
----------------------------------------------------------------------------------


makeRAPPops :: Fspc -> Options -> [String] -> [Picture] -> [P_Population]
makeRAPPops fs opts usrfiles pics
 = let usr = namespace opts
       operations = [(CID "1",CID "laden")] -- ,(CID "2",CID ""),(CID "3",CID ""),(CID "4",CID ""),(CID "5",CID "")]
   in
     makepopu ("sourcefile","Context","File") [(fsid fs, fileid (takeFileName(importfile opts)))]
    :makepopu ("filename","File","FileName") [(fileid fn, CID fn) | fn<-usrfiles]
    :makepopu ("filepath","File","FilePath") [(fileid fn, CID(dropFileName (importfile opts))) | fn<-usrfiles]
    :makepopu ("loaded","File","File") [(fileid (takeFileName(importfile opts)), fileid (takeFileName(importfile opts)))]
    :makepopu ("uploaded","User","File") [(usrid usr, fileid fn) | fn<-usrfiles]
    :makepopu ("applyto","G","File") [(gid op fn, fileid fn) | (op,_)<-operations, fn<-usrfiles]
    :makepopu ("functionname","G","String") [(gid op fn, nm) | (op,nm)<-operations, fn<-usrfiles]
    :makepopu ("operation","G","Int") [(gid op fn, op) | (op,_)<-operations, fn<-usrfiles]
    :makepopu ("newfile","User","NewFile") [(usrid usr, CID "empty.adl")]
    :makepopu ("countrules","Context","Int") [(fsid fs, CID (show (length (rules fs))))]
    :makepopu ("countdecls","Context","Int") [(fsid fs, CID (show (length userdeclarations)))]
    :makepopu ("countcpts","Context","Int") [(fsid fs, CID (show (length (concs fs))))]
    :makepopu ("ctxnm","Context","Conid") [(fsid fs, CID (name fs))]
    :makepopu ("ctxcs","Context","Concept") [(fsid fs, cptid c)       | c<-concs fs] 
    :makepopu ("cptnm","Concept","Conid")   [(cptid c, CID (name c))  | c<-concs fs]
    :makepopu ("cptos","Concept","AtomID")  [(cptid c, atomidid x c)  | c<-concs fs, x<-cptos c]
    :makepopu ("atomvalue","AtomID","Atom") [(atomidid x c, CID x)    | c<-concs fs, x<-cptos c]
    :makepopu ("cptpurpose","Concept","Blob") [(cptid c, CID (aMarkup2String (explMarkup ex))) | c<-concs fs, ex<-explanations fs, explForObj c (explObj ex)]
    :makepopu ("cptdf","Concept","Blob")    [(cptid c, CID(cddef cd)) | c<-concs fs, cd<-cptdf c]
    :makepopu ("gengen","Gen","Concept") [(genid g, cptid (target g)) | g<-gens fs]
    :makepopu ("genspc","Gen","Concept") [(genid g, cptid (source g)) | g<-gens fs]
    :makepopu ("ctxpats","Context","Pattern") [(fsid fs, patid p)      | p<-patterns fs]
    :makepopu ("ptnm","Pattern","Conid")      [(patid p, CID (name p)) | p<-patterns fs]
    :makepopu ("ptrls","Pattern","Rule")      [(patid p, ruleid r)     | p<-patterns fs, r<-rules p]
    :makepopu ("ptgns","Pattern","Gen")       [(patid p, genid g)      | p<-patterns fs, g<-gens p]
    :makepopu ("ptdcs","Pattern","Declaration") [(patid p, decid d)    | p<-patterns fs, d<-declarations p,decusr d]
    :makepopu ("ptxps","Pattern","Blob") [(patid p, CID (aMarkup2String (explMarkup ex))) | p<-patterns fs, ex<-explanations fs, explForObj p (explObj ex)]
    :makepopu ("decnm","Declaration","Varid") [(decid d, CID(name d))                   | d<-userdeclarations]
    :makepopu ("decsgn","Declaration","Sign") [(decid d, sgnid (sign d))                | d<-userdeclarations]
    :makepopu ("decprps","Declaration","PropertyRule") [(decid d, ruleid r) | d<-userdeclarations, p<-multiplicities d, let r=rulefromProp userdeclarations p d]
    :makepopu ("declaredthrough","PropertyRule","Property") [(ruleid r, CID(show p)) | d<-userdeclarations, p<-multiplicities d, let r=rulefromProp userdeclarations p d]
    :makepopu ("decprL","Declaration","String") [(decid d, CID(decprL d))              | d<-userdeclarations]
    :makepopu ("decprM","Declaration","String") [(decid d, CID(decprM d))              | d<-userdeclarations]
    :makepopu ("decprR","Declaration","String") [(decid d, CID(decprR d))              | d<-userdeclarations]
    :makepopu ("decmean","Declaration","Blob") [(decid d, CID (aMarkup2String rdf))     | d<-userdeclarations, Just rdf <- meaning Dutch d:meaning English d:[]]
    :makepopu ("decpurpose","Declaration","Blob") [(decid d, CID (aMarkup2String (explMarkup ex))) | d<-userdeclarations, ex<-explanations fs, explForObj d (explObj ex)]
    :makepopu ("decpopu","Declaration","PairID") [(decid d, pairidid (x,y) (decns d,d)) | d<-userdeclarations, (x,y)<-contents d]
    :makepopu ("reldcl","Relation","Declaration") [(relid (name d) (sign d), decid d)   | d<-userdeclarations]
    :makepopu ("relnm","Relation","Varid") [(relid (name d) (sign d), CID(name d))      | d<-userdeclarations]
    :makepopu ("relsgn","Relation","Sign") [(relid (name d) (sign d), sgnid (sign d))   | d<-userdeclarations]
    :relsrc                          userdeclarations
    :reltrg                          userdeclarations
    :relpairvalue [(decns d, d) | d<-userdeclarations]
    :relleft                         userdeclarations
    :relright                        userdeclarations
    :makepopu ("rrnm","Rule","ADLid") [(ruleid r, CID (name r))                              | r<-raprules]
    :makepopu ("rrexp","Rule","ExpressionID") [(ruleid r, expridid (rulens r,rrexp r))       | r<-raprules]
    :makepopu ("rrmean","Rule","Blob") [(ruleid r, CID (aMarkup2String rdf))                 | r<-raprules, Just rdf <- meaning Dutch r:meaning English r:[]]
    :makepopu ("rrpurpose","Rule","Blob") [(ruleid r, CID (aMarkup2String (explMarkup ex)))  | r<-raprules, ex<-explanations fs, explForObj r (explObj ex)]
    :makepopu ("exprvalue","ExpressionID","Expression") [(expridid (rulens r,rrexp r), CID (show(rrexp r))) | r<-raprules]
    :relrels      [(rulens r,rrexp r) | r<-raprules]
    :relrelnm     (map       rrexp                 (rules fs))
    :relrelsgn    (map       rrexp                 (rules fs))
    :relreldcl    (map       rrexp                 (rules fs))
    :relpairvalue [(rulens r,violationsexpr r) | r<-raprules]
    :relleft      (map       violationsexpr        (raprules))
    :relright     (map       violationsexpr        (raprules))
    :makepopu ("imageurl","Image","URL")   [(imageid pic, CID([if c=='\\' then '/' else c | c<-imgURL pic])) | pic<-pics]
    :makepopu ("ptpic","Pattern","Image")  [(patid p, imageid pic)         | pic<-pics, pType pic==PTPattern, p<-patterns fs, name p==origName pic]
    :makepopu ("rrpic","Rule","Image")     [(ruleid r, imageid pic)        | pic<-pics, pType pic==PTRule   , r<-rules fs   , name r==origName pic]
    :makepopu ("cptpic","Concept","Image") [(cptid c, imageid pic)         | pic<-pics, pType pic==PTConcept, c<-concs fs   , name c==origName pic]
    :[]
--     ++ rappops (violations fs) --the violations
   where 
   --SPEC PropertyRule ISA Rule
   raprules = rules fs ++ [rulefromProp userdeclarations p d | d<-userdeclarations, p<-multiplicities d]
   userdeclarations = filter decusr (declarations fs)
   relsrc,reltrg :: Association r => [r] -> P_Population
   relsrc rs = makepopu ("src","Sign","Concept")      [(sgnid (sign r), cptid (source r)) | r<-rs]
   reltrg rs = makepopu ("trg","Sign","Concept")      [(sgnid (sign r), cptid (target r)) | r<-rs]
   relpairvalue :: (Populated r,Association r) => [(IdentifierNamespace,r)] -> P_Population
   relpairvalue rs = makepopu ("pairvalue","PairID","Pair")      [(pairidid (x,y) (ns,r), pairid (x,y) (sign r)) | (ns,r)<-rs, (x,y)<-contents r]
   --populate relleft and relright for populated and typed data structures
   relleft,relright :: (Populated r,Association r) => [r] -> P_Population
   relleft rs = makepopu ("left","Pair","AtomID")      [(pairid (x,y) (sign r), atomidid x (source r)) | r<-rs, (x,y)<-contents r]
   relright rs = makepopu ("right","Pair","AtomID")    [(pairid (x,y) (sign r), atomidid y (target r)) | r<-rs, (x,y)<-contents r]
   --populate relrels, relrelnm, relreldcl and relrelsgn for expressions
   relrels :: [(IdentifierNamespace, Expression)] -> P_Population
   relrels exprs = makepopu ("rels","ExpressionID","Relation") [(expridid (ns,expr), relid nm sgn) | (ns,expr)<-exprs, Rel{relnm=nm,relsgn=sgn}<-mors expr]
   relrelnm, relreldcl, relrelsgn :: [Expression] -> P_Population
   relrelnm exprs = makepopu ("relnm","Relation","Varid") [(relid nm sgn, CID nm) | expr<-exprs, Rel{relnm=nm,relsgn=sgn}<-mors expr]
   relrelsgn exprs = makepopu ("relsgn","Relation","Sign") [(relid nm sgn, sgnid sgn) | expr<-exprs, Rel{relnm=nm,relsgn=sgn}<-mors expr]
   relreldcl exprs = makepopu ("reldcl","Relation","Declaration") [(relid nm sgn, decid d) | expr<-exprs, Rel{relnm=nm,relsgn=sgn,reldcl=d}<-mors expr]
