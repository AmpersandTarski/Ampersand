{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
--import an ADL1 file into the RAP specification
-- USE -> cmd: ampersand --importfile=some.adl --importformat=adl RAP.adl
module DatabaseDesign.Ampersand_Prototype.Apps.RAPImport   (makeRAPPops)
where
import Data.List
import DatabaseDesign.Ampersand_Prototype.CoreImporter

--(relation name, source name, target name)
type RAPRelation = (String,String,String)

class RAPImportable a where
 rappops  :: a -> [P_Population]
instance RAPImportable a => RAPImportable [a] where
 rappops xs = concat (map rappops xs)

makepopu :: RAPRelation -> [(String,String)] -> P_Population
makepopu (r,src,trg) xys
 = P_Popu{ p_popm  = P_Rel r (Origin "RAPImport.hs")
         , p_type  = P_Sign [PCpt src, PCpt trg]
         , p_popps = [mkPair (trim x) (trim y) |(x,y)<-xys, not(null x), not(null y) ]
         }
----------------------------------------------------------------------------------
--comments on and additional functions for string identifiers for Atlas concepts--
----------------------------------------------------------------------------------
--The string identifier of (Identified a) is (name a)
--EXAMPLES -> Concept,Rule,RelVar
--EXCEPTION -> Relation, Declaration (see relationid,declarationid)
--
--the string identifier of Paire x is (show x) (fst and snd of Paire are string identifiers of Atom elements)
--
--use #> to qualify an element::String of identified a
--in order to use the qualified element as an identifier
--EXAMPLES: Atom, Violation
--
--an element::String that does not need qualification is used as string identifier
--EXAMPLES: UAtom, Pragma1/2/3
--
--the identifier of Expression e is (show e)
--
--explainContent2String is used for Explanation,Purpose::[Block]
--
(#>) :: (Identified a) => a -> String -> String
(#>) a x = name a ++ "#" ++ x
--TODO -> generated ISA and isa relations have to be filtered from (declarations a).
declarationid :: Declaration -> String
declarationid x = name x ++ "::" ++ name(source x) ++ "*" ++ name(target x)
relationid :: Relation -> String
relationid x = name x ++ "::" ++ name(source x) ++ "*" ++ name(target x)
----------------------------------------------------------------------------------
{-
ctxnm  ::Context->Conid [INJ]
ctxpats::Context*Pattern
ctxcs  ::Context*Concept
-}
makeRAPPops :: Fspc -> [String] -> [Picture] -> [P_Population]
makeRAPPops fs usrfiles pics
   = makepopu ("ctxnm","Context","Conid") [(name fs, name fs)]
    :makepopu ("ctxpats","Context","Pattern") [(name fs, name p) | p<-patterns fs]
    :makepopu ("ctxcs","Context","Concept") [(name fs, name c) | c<-concs fs] 
    :rappops pics
--     ++ rappops (concs fs) --the content of concepts 
--     ++ rappops (vConceptDefs fs) --the explanations of concepts
--     ++ rappops (declarations fs) --the property rules + content of relations
--     ++ rappops (rules fs) --the rules
--     ++ rappops (gens fs) --the details of gens
--     ++ rappops (patterns fs) --the rules + relations + gens of patterns
--     ++ rappops (explanations fs) --the purposes
--     ++ rappops (violations fs) --the violations

{-
                                                  fnnxt = name fspec ++ "'" -- a name for a not yet existing next version
                                                  fdir = let d=dropFileName fn in if null d then "." else d
                                                  usr= namespace opts
                                                  getr r = if length r==1 then P_Rel {rel_nm = relnm (head r), rel_pos = relpos (head r)} else error "import error: no or multiple declarations for relvar"
                                                  impctx = [makeRelation d |d<-declarations atlas,name d=="loadcontext"]
                                                  impfil = [makeRelation d |d<-declarations atlas,name d=="loadedfile"]
                                                  impupl = [makeRelation d |d<-declarations atlas,name d=="newcontext"]
                                                  usrfil = [makeRelation d |d<-declarations atlas,name d=="fileof"]
                                                  --funrld = [makeRelation d |d<-declarations atlas,name d=="reload"]
                                                  funfsp = [makeRelation d |d<-declarations atlas,name d=="funcspec"]
                                                  funrep = [makeRelation d |d<-declarations atlas,name d=="report"]
                                                  funadl = [makeRelation d |d<-declarations atlas,name d=="showadl"]
                                                  loadcontext r 
                                                   = [P_Popu{ p_popm=getr r, p_type=P_Sign [], p_popps=[mkPair fn (name fspec),mkPair fnnxt fnnxt]}]
                                                  loadedfile r
                                                   = [P_Popu{ p_popm=getr r, p_type=P_Sign [], p_popps=[mkPair usr fn]         } | not (null usr)]
                                                  -- uploadfile r        = [P_Popu{ p_popm=getr r, p_type=[], p_popps=[mkPair usr "browse"]   } | not (null usr)]
                                                  --TODO -> the user has more files, how do I get them in this population
                                                  fileof r myfiles
                                                   = [P_Popu{ p_popm=getr r, p_type=P_Sign [], p_popps=[mkPair (combine fdir f) usr | f<-myfiles, not (null usr)] }]
                                                  contextfunction r x
                                                   = [P_Popu{ p_popm=getr r, p_type=P_Sign [], p_popps=[mkPair (name fspec) x] }]


                                                       ++loadcontext impctx
                                                       ++loadedfile impfil
                                                       ++contextfunction impupl "new context"
                                                       ++fileof usrfil myfiles
                                                       -- ++ contextfunction funrld (name fspec)
                                                       ++ contextfunction funfsp (takeBaseName fn ++ ".pdf")
                                                       ++ contextfunction funrep (name fspec)
                                                       ++ contextfunction funadl fnnxt
                                                        
 -}
instance RAPImportable Picture where
 rappops pic
  = let imageid = "Image_" ++ uniqueName pic
    in  makepopu ("imageurl","Image","URL") [(imageid, imgURL pic)]
       :makepopu ("ptpic","Pattern","Image")  [(origName pic, imageid) | pType pic==PTPattern]
       :makepopu ("rrpic","Rule","Image")     [(origName pic, imageid) | pType pic==PTRule]
       :makepopu ("cptpic","Concept","Image") [(origName pic, imageid) | pType pic==PTConcept]
       :[]

{-
instance RAPImportable A_Concept where
 rappops c 
   = let cptaof = [makeRelation d |d<-rap_dcs,name d=="atomof"]
         cptasx = [makeRelation d |d<-rap_dcs,name d=="atomsyntax"]
     in  (makepopu [(c#>x,c) |c<-cs,x<-atomsOf c] fst cptaof (name.snd))
        :(makepopu [(c#>x,x) |c<-cs,x<-atomsOf c] fst cptasx snd)
        :[]

instance RAPImportable ConceptDef where
 rappops cds 
   = let cptxpl = [setRelats(makeRelation d) |d<-rap_dcs,name d=="describes",name(source d)=="Concept"] 
     in  (makepopu cds cdcpt cptxpl cddef)
        :[]

instance RAPImportable Declaration where
 rappops ds' 
   = let ds = [d |d<-ds',decusr d]
         dclvar = [makeRelation d |d<-rap_dcs,name d=="rel"]
         dclsrc = [makeRelation d |d<-rap_dcs,name d=="src"]
         dcltrg = [makeRelation d |d<-rap_dcs,name d=="trg"]
         dclpr1 = [makeRelation d |d<-rap_dcs,name d=="pragma1"]
         dclpr2 = [makeRelation d |d<-rap_dcs,name d=="pragma2"]
         dclpr3 = [makeRelation d |d<-rap_dcs,name d=="pragma3"]
         --for every relation::generate all potential property rules
         --but relate only those that are actual properties of relations
         dpps = concat (map (dallpotentialproprulesof ds) ds)
         dps = concat (map (dallproprulesof ds) ds)
         dclpof = [makeRelation d |d<-rap_dcs,name d=="propertyof"]
         dclpex = [makeRelation d |d<-rap_dcs,name d=="propexpr"]
         dclpsc = [makeRelation d |d<-rap_dcs,name d=="source"]
         dclptg = [makeRelation d |d<-rap_dcs,name d=="target"]
         dclpus = [makeRelation d |d<-rap_dcs,name d=="uses"]
         dclprp = [makeRelation d |d<-rap_dcs,name d=="propsyntax"]
         --for every relation::relate to its content
         dcs = concat(map dcontentof ds)
         dprs = [(mkPair (source d#>x) (target d#>y),(x,y)) |d<-ds,(x,y)<-contents d]
         dclcnt = [makeRelation d |d<-rap_dcs,name d=="content"]
         dcldom = [makeRelation d |d<-rap_dcs,name d=="left"]
         dclrng = [makeRelation d |d<-rap_dcs,name d=="right"]
         dclupr = [makeRelation d |d<-rap_dcs,name d=="pairsyntax"]
         --description
         dcldcr = [setRelats(makeRelation d) |d<-rap_dcs,name d=="describes",name(source d)=="Relation"]
     in  (makepopu ds declarationid dclvar name)
        :(makepopu ds declarationid dclsrc (name.source))
        :(makepopu ds declarationid dcltrg (name.target))
        :(makepopu ds declarationid dclpr1 (dpragma 1))
        :(makepopu ds declarationid dclpr2 (dpragma 2))
        :(makepopu ds declarationid dclpr3 (dpragma 3))
        --properties
        :(makepopu dps (name.snd) dclpof (declarationid.fst.fst))
        :(makepopu dpps (name.snd) dclpex (showADL.rrexp.snd))
        :(makepopu dpps (showADL.snd) dclpsc (name.source.snd))
        :(makepopu dpps (showADL.snd) dclptg (name.target.snd))
        :(makepopu dpps (showADL.snd) dclpus (declarationid.fst.fst))
        :(makepopu dpps (name.snd) dclprp (show.snd.fst))
        --content
        :(makepopu dcs (declarationid.fst) dclcnt (show.snd))
        :(makepopu dcs (show.snd) dcldom (fst.snd))
        :(makepopu dcs (show.snd) dclrng (snd.snd))
        :(makepopu dprs (show.fst) dclupr (show.snd))
        --description
        :(makepopu ds declarationid dcldcr (\x -> maybe "" aMarkup2String (meaning Dutch x)))
        :[]
dpragma :: Integer -> Declaration -> String
dpragma i (Sgn{decprL=x1,decprM=x2,decprR=x3})
   | i==1 = x1 
   | i==2 = x2
   | i==3 = x3
   | otherwise = ""
dpragma _ _ = ""
dallpotentialproprulesof :: [Declaration] -> Declaration -> [((Declaration,Prop),Rule)]
dallpotentialproprulesof ds d = [((d,p),rulefromProp ds p d) |p<-allprops,not(elem p endoprops) || source d==target d]
dallproprulesof :: [Declaration] -> Declaration -> [((Declaration,Prop),Rule)]
dallproprulesof ds d = [((d,p),rulefromProp ds p d) |p<-multiplicities d]
dcontentof :: (Populated a,Association a) => a -> [(a,Paire)]
dcontentof d = [(d,mkPair (source d#>x) (target d#>y)) |(x,y)<-contents d]

instance RAPImportable Rule where
 rappops rs 
   = let rulexp = [makeRelation d |d<-rap_dcs,name d=="ruleexpr"] 
         rulsrc = [makeRelation d |d<-rap_dcs,name d=="source"]
         rultrg = [makeRelation d |d<-rap_dcs,name d=="target"]
         ruluss = [makeRelation d |d<-rap_dcs,name d=="uses"]
         ruldcr = [setRelats(makeRelation d) |d<-rap_dcs,name d=="describes",name(source d)=="UserRule"]  
     in  (makepopu rs name rulexp (showADL.rrexp))
        :(makepopu rs (showADL.rrexp) rulsrc (name.source))
        :(makepopu rs (showADL.rrexp) rultrg (name.target))
        :(makepopu [(rul,rel) |rul<-rs,rel@(Rel{})<-mors rul] (showADL.rrexp.fst) ruluss (relationid.snd))
        :(makepopu rs name ruldcr (\x -> maybe "" aMarkup2String (meaning Dutch x)))
        :[]

instance RAPImportable Pattern where
 rappops ps 
   = let patrel = [makeRelation d |d<-rap_dcs,name d=="relpattern"] 
         patrul = [makeRelation d |d<-rap_dcs,name d=="rulpattern"] 
         patgen = [makeRelation d |d<-rap_dcs,name d=="isapattern"] 
         isaspc = [makeRelation d |d<-rap_dcs,name d=="spec"] 
         isagen = [makeRelation d |d<-rap_dcs,name d=="gen"] 
     in  (makepopu [(d,p) |p<-ps,d<-declarations p,decusr d] (declarationid.fst) patrel (name.snd))
        :(makepopu [(r,p) |p<-ps,r<-rules p]        (name.fst) patrul (name.snd))
        :(makepopu [(g,p) |p<-ps,g<-gens p]         (show.fst) patgen (name.snd))
        :(makepopu [g     |p<-ps,g<-gens p]         show       isaspc (name.source))
        :(makepopu [g     |p<-ps,g<-gens p]         show       isagen (name.target))
        :[]

--instance RAPImportable (RuleRelation,Paire) where
--  rappops viols =[]

instance RAPImportable (Rule,Paire) where
 rappops viols 
   = let rulvio = [makeRelation d |d<-rap_dcs,name d=="violates"]
         violpr = [makeRelation d |d<-rap_dcs,name d=="violationpair"]
         viodom = [makeRelation d |d<-rap_dcs,name d=="left"]
         viorng = [makeRelation d |d<-rap_dcs,name d=="right"]
         vioupr = [makeRelation d |d<-rap_dcs,name d=="pairsyntax"]
     in  (makepopu viols violationid rulvio (showADL.rrexp.fst))
        :(makepopu viols violationid violpr violationpair)
        :(makepopu viols violationpair viodom (\(r,(x,_))->source r#>x))
        :(makepopu viols violationpair viorng (\(r,(_,y))->target r#>y))
        :(makepopu viols violationpair vioupr (show.snd))
        :[]
violationid :: (Rule,Paire) -> String
violationid (r,p) = r #> show p 
violationpair :: (Rule,Paire) -> String
violationpair (r,(x,y)) = show(mkPair (source r#>x) (target r#>y))

instance RAPImportable Purpose where
 rappops es 
   = let purcpt = [setRelats(makeRelation d) |d<-rap_dcs,name d=="purpose",name(source d)=="Concept"] 
         purrul = [setRelats(makeRelation d) |d<-rap_dcs,name d=="purpose",name(source d)=="UserRule"]  
         purpat = [setRelats(makeRelation d) |d<-rap_dcs,name d=="purpose",name(source d)=="Pattern"] 
         purrel = [setRelats(makeRelation d) |d<-rap_dcs,name d=="purpose",name(source d)=="Relation"]  
     in  (makepopu [((amPandoc . explMarkup) e,cdcpt cd) |e<-es, case explObj e of (ExplConceptDef _)->True;_ -> False,let ExplConceptDef cd = explObj e]
                  snd purcpt ((blocks2String ReST False).fst))
        :(makepopu [((amPandoc . explMarkup) e,r) |e<-es, case explObj e of (ExplRule _)->True;_ -> False,let ExplRule r = explObj e]
                  (name.snd) purrul ((blocks2String ReST False).fst))
        :(makepopu [((amPandoc . explMarkup) e,pstr) |e<-es, case explObj e of (ExplPattern _)->True;_ -> False,let ExplPattern pstr = explObj e]
                  snd purpat ((blocks2String ReST False).fst))
        :(makepopu [((amPandoc . explMarkup) e,d) |e<-es, case explObj e of (ExplDeclaration _)->True;_ -> False,let ExplDeclaration d = explObj e]
                  (declarationid.snd) purrel ((blocks2String ReST False).fst))
        :[]
-}
