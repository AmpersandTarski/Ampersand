{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
--for example, importing an ADL1 file into the Atlas application as defined in DatabaseDesign\Ampersand_Prototype\Apps\atlas.adl
--             USE -> cmd: ampersand --importfile=some.adl --importformat=adl atlas.adl
module DatabaseDesign.Ampersand_Prototype.Apps.ADL1Importable   (makePopulations, ADL1Importable(..))
where
import Data.List
import DatabaseDesign.Ampersand_Prototype.CoreImporter

-- The purpose of makePopulations is to translate a functional specification (fSpec :: Fspc) into the population of an Atlas ( [P_Population] ).
-- This Atlas is built up of a number of declarations, which are taken (manually) from the RAP metamodel and which are specified in the parameter decls :: [Declaration].
-- These declarations can be populated by the call 'makePopulations (declarations atlas) fspec', assuming that 'declarations atlas' contains the right relations.
makePopulations :: [Declaration] -> Fspc -> [P_Population]
makePopulations decls fSpec = makeADL1Populations decls [fSpec]  --TODO : #104 Must be restored.

--The purpose of makeADL1Populations is to generate a population in the Atlas for a Haskell-type in the A-structure
class ADL1Importable a where
 makeADL1Populations  :: [Declaration] -> [a] -> [P_Population]

-------------------
--local functions--
-------------------
--USE -> setRelats for atlas declarations with overloaded names
setRelats :: Relation -> Relation
setRelats r = id r -- { relats = [source r,target r]}


--makes relationships f(x) r g(x) for x<-xs
makepopu :: [t] -> (t -> String) -> [Relation] -> (t -> String) -> P_Population
makepopu xs f [r] g 
 = P_Popu{ p_popm  = P_Rel {rel_nm = relnm r, rel_pos = relpos r}
         , p_type  = [PCpt (name (source r)), PCpt (name (target r))]
         , p_popps = [mkPair (trim$f x) (trim$g x) |x<-xs, not(null(f x)), not(null(g x)) ] --REMARK -> Our MySql configuration trims spaces
         }
makepopu _ _ rs _ 
 = if null rs 
   then error ("import error: relation " ++ show rs ++ " does not exist in the target context.")
   else error ("import error: p_popm can only be one relation: " ++ show rs)
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


instance ADL1Importable A_Concept where
 makeADL1Populations atlasds cs 
   = let cptaof = [makeRelation d |d<-atlasds,name d=="atomof"]
         cptasx = [makeRelation d |d<-atlasds,name d=="atomsyntax"]
     in  (makepopu [(c#>x,c) |c<-cs,x<-cptos' c] fst cptaof (name.snd))
        :(makepopu [(c#>x,x) |c<-cs,x<-cptos' c] fst cptasx snd)
        :[]

instance ADL1Importable ConceptDef where
 makeADL1Populations atlasds cds 
   = let cptxpl = [setRelats(makeRelation d) |d<-atlasds,name d=="describes",name(source d)=="Concept"] 
     in  (makepopu cds cdcpt cptxpl cddef)
        :[]

instance ADL1Importable Declaration where
 makeADL1Populations atlasds ds' 
   = let ds = [d |d<-ds',decusr d]
         dclvar = [makeRelation d |d<-atlasds,name d=="rel"]
         dclsrc = [makeRelation d |d<-atlasds,name d=="src"]
         dcltrg = [makeRelation d |d<-atlasds,name d=="trg"]
         dclpr1 = [makeRelation d |d<-atlasds,name d=="pragma1"]
         dclpr2 = [makeRelation d |d<-atlasds,name d=="pragma2"]
         dclpr3 = [makeRelation d |d<-atlasds,name d=="pragma3"]
         --for every relation::generate all potential property rules
         --but relate only those that are actual properties of relations
         dpps = concat (map (dallpotentialproprulesof ds) ds)
         dps = concat (map (dallproprulesof ds) ds)
         dclpof = [makeRelation d |d<-atlasds,name d=="propertyof"]
         dclpex = [makeRelation d |d<-atlasds,name d=="propexpr"]
         dclpsc = [makeRelation d |d<-atlasds,name d=="source"]
         dclptg = [makeRelation d |d<-atlasds,name d=="target"]
         dclpus = [makeRelation d |d<-atlasds,name d=="uses"]
         dclprp = [makeRelation d |d<-atlasds,name d=="propsyntax"]
         --for every relation::relate to its content
         dcs = concat(map dcontentof ds)
         dprs = [(mkPair (source d#>x) (target d#>y),(x,y)) |d<-ds,(x,y)<-contents d]
         dclcnt = [makeRelation d |d<-atlasds,name d=="content"]
         dcldom = [makeRelation d |d<-atlasds,name d=="left"]
         dclrng = [makeRelation d |d<-atlasds,name d=="right"]
         dclupr = [makeRelation d |d<-atlasds,name d=="pairsyntax"]
         --description
         dcldcr = [setRelats(makeRelation d) |d<-atlasds,name d=="describes",name(source d)=="Relation"]
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
        :(makepopu ds declarationid dcldcr (\x -> aMarkup2String (meaning Dutch x)))
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

instance ADL1Importable Rule where
 makeADL1Populations atlasds rs 
   = let rulexp = [makeRelation d |d<-atlasds,name d=="ruleexpr"] 
         rulsrc = [makeRelation d |d<-atlasds,name d=="source"]
         rultrg = [makeRelation d |d<-atlasds,name d=="target"]
         ruluss = [makeRelation d |d<-atlasds,name d=="uses"]
         ruldcr = [setRelats(makeRelation d) |d<-atlasds,name d=="describes",name(source d)=="UserRule"]  
     in  (makepopu rs name rulexp (showADL.rrexp))
        :(makepopu rs (showADL.rrexp) rulsrc (name.source))
        :(makepopu rs (showADL.rrexp) rultrg (name.target))
        :(makepopu [(rul,rel) |rul<-rs,rel@(Rel{})<-mors rul] (showADL.rrexp.fst) ruluss (relationid.snd))
        :(makepopu rs name ruldcr (\x -> aMarkup2String (meaning Dutch x)))
        :[]

instance ADL1Importable Pattern where
 makeADL1Populations atlasds ps 
   = let patrel = [makeRelation d |d<-atlasds,name d=="relpattern"] 
         patrul = [makeRelation d |d<-atlasds,name d=="rulpattern"] 
         patgen = [makeRelation d |d<-atlasds,name d=="isapattern"] 
         isaspc = [makeRelation d |d<-atlasds,name d=="spec"] 
         isagen = [makeRelation d |d<-atlasds,name d=="gen"] 
     in  (makepopu [(d,p) |p<-ps,d<-declarations p,decusr d] (declarationid.fst) patrel (name.snd))
        :(makepopu [(r,p) |p<-ps,r<-rules p]        (name.fst) patrul (name.snd))
        :(makepopu [(g,p) |p<-ps,g<-gens p]         (show.fst) patgen (name.snd))
        :(makepopu [g     |p<-ps,g<-gens p]         show       isaspc (name.source))
        :(makepopu [g     |p<-ps,g<-gens p]         show       isagen (name.target))
        :[]

--instance ADL1Importable (RuleRelation,Paire) where
--  makeADL1Populations atlasds viols =[]

instance ADL1Importable (Rule,Paire) where
 makeADL1Populations atlasds viols 
   = let rulvio = [makeRelation d |d<-atlasds,name d=="violates"]
         violpr = [makeRelation d |d<-atlasds,name d=="violationpair"]
         viodom = [makeRelation d |d<-atlasds,name d=="left"]
         viorng = [makeRelation d |d<-atlasds,name d=="right"]
         vioupr = [makeRelation d |d<-atlasds,name d=="pairsyntax"]
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

instance ADL1Importable Explanation where
 makeADL1Populations atlasds es 
   = let purcpt = [setRelats(makeRelation d) |d<-atlasds,name d=="purpose",name(source d)=="Concept"] 
         purrul = [setRelats(makeRelation d) |d<-atlasds,name d=="purpose",name(source d)=="UserRule"]  
         purpat = [setRelats(makeRelation d) |d<-atlasds,name d=="purpose",name(source d)=="Pattern"] 
         purrel = [setRelats(makeRelation d) |d<-atlasds,name d=="purpose",name(source d)=="Relation"]  
     in  (makepopu [((amPandoc . explMarkup) e,cdcpt cd) |e<-es, case explObj e of (ExplConceptDef _)->True;_ -> False,let ExplConceptDef cd = explObj e]
                  snd purcpt ((blocks2String ReST False).fst))
        :(makepopu [((amPandoc . explMarkup) e,r) |e<-es, case explObj e of (ExplRule _)->True;_ -> False,let ExplRule r = explObj e]
                  (name.snd) purrul ((blocks2String ReST False).fst))
        :(makepopu [((amPandoc . explMarkup) e,pstr) |e<-es, case explObj e of (ExplPattern _)->True;_ -> False,let ExplPattern pstr = explObj e]
                  snd purpat ((blocks2String ReST False).fst))
        :(makepopu [((amPandoc . explMarkup) e,d) |e<-es, case explObj e of (ExplDeclaration _)->True;_ -> False,let ExplDeclaration d = explObj e]
                  (declarationid.snd) purrel ((blocks2String ReST False).fst))
        :[]

instance ADL1Importable Picture where
 makeADL1Populations atlasds ps 
   = let picpat = [setRelats(makeRelation d) |d<-atlasds,name d=="picture",name(source d)=="Pattern"] 
         picrul = [setRelats(makeRelation d) |d<-atlasds,name d=="picture",name(source d)=="UserRule"]  
         piccpt = [setRelats(makeRelation d) |d<-atlasds,name d=="picture",name(source d)=="Concept"]  
     in  (makepopu [p |p<-ps, pType p==PTPattern] origName picpat imgURL)
        :(makepopu [p |p<-ps, pType p==PTRule] origName picrul imgURL)
        :(makepopu [p |p<-ps, pType p==PTConcept] origName piccpt imgURL)
        :[]

instance ADL1Importable Fspc where
  makeADL1Populations atlasds fss 
    = concat[  contextelements (concs fs) ctxcps fs --the concepts
            ++ makeADL1Populations atlasds (concs fs) --the content of concepts 
            ++ makeADL1Populations atlasds (vConceptDefs fs) --the explanations of concepts
            ++ makeADL1Populations atlasds (declarations fs) --the property rules + content of relations
            ++ makeADL1Populations atlasds sortedrules --the rules
            ++ nextid sortedrules rulnxt --REMARK -> sort rules to define the next rule
         --   ++ makeADL1Populations atlasds (gens fs) --the details of gens
            ++ contextelements (patterns fs) ctxpts fs --the patterns
            ++ makeADL1Populations atlasds (patterns fs) --the rules + relations + gens of patterns
            ++ makeADL1Populations atlasds (fSexpls fs) --the purposes
            ++ makeADL1Populations atlasds (violations fs) --the violations
            --REMARK -> the pictures are not part of fspec, but derived from them (see Main.hs of prototype.exe)
            |fs<-fss --REMARK -> probably there will be only one fs
            ,let ctxcps = [makeRelation d |d<-atlasds,name d=="cptcontext"]
            ,let ctxpts = [makeRelation d |d<-atlasds,name d=="context"]
            ,let rulnxt = [makeRelation d |d<-atlasds,name d=="next"]
            ,let sortedrules = sortBy comparerules (rules fs) 
            ]
--contextelements calculates the populations of all r::t->P_Context in the Atlas
--By design, there is only one P_Context element identified by the name of fspec
contextelements :: (Identified t, Identified a) => [t] -> [Relation] -> a -> [P_Population]
contextelements xs [r] fspec
 = [P_Popu{ p_popm = P_Rel {rel_nm = relnm r, rel_pos = relpos r}
          , p_type  = [PCpt (name (source r)), PCpt (name (target r))]
          , p_popps=[mkPair (name x) (name fspec) |x<-xs]
          }]
contextelements _ _ _
 = error "import error: no or multiple declarations for relvar"
--rules are ordered by source, target, and name
comparerules :: (Identified rel,Association rel) => rel -> rel -> Ordering  
comparerules x y
  | source x /= source y = name(source x) `compare` name(source y)
  | target x /= target y = name(target x) `compare` name(target y)
  | otherwise = name x `compare` name y
--nextid calculates the population of r::t*t from a list of t
--the next element of element x is the next element y in the list
--if x is the last, then the next element of x is the head of the list
nextid :: (Identified t) => [t] -> [Relation] -> [P_Population]
nextid xs [r]
 = [P_Popu{ p_popm = P_Rel {rel_nm = relnm r, rel_pos = relpos r}
          , p_type  = [PCpt (name (source r)), PCpt (name (target r))]
          , p_popps=nxt xs []
          }]
   where nxt [] rnxt = rnxt --end
         nxt (x:[]) [] = [mkPair (name x) (name x)] --start+end
         nxt (x:y:tl) [] = nxt (y:tl) [mkPair (name x) (name y)] --start
         nxt (x:y:tl) rnxt = nxt (y:tl) ((mkPair (name x) (name y)):rnxt) --run
         nxt (x:[]) rnxt = (mkPair (name x) (fst(last rnxt))):rnxt --end
nextid _ _
 = error "import error: no or multiple declarations for relvar"
