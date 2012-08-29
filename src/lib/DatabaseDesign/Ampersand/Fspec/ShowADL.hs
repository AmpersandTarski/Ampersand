{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
  -- The purpose of ShowADL is to print things in Ampersand source format.
  -- Rule: The semantics of each fSpec produced by the compiler is identical to the semantics  of (parse (showADL fSpec)).
  -- Rule: The standard show is used only for simple error messages during testing.
  -- Question (SJC): If STRING is the code produced by showADL, would STRING == showADL (parse STRING) (context (parse STRING)) be true?
  -- Answer (SJ):   No, not for every STRING. Yet, for every fSpec we want  semantics fSpec == semantics (parse (showADL fSpec)).
  --                Note that 'parse' and 'semantics' do not exist in this shape, so the actual expression is slightly more complicated.
  --
  -- Every Expression should be disambiguated before printing to ensure unambiguity.
module DatabaseDesign.Ampersand.Fspec.ShowADL
    ( ShowADL(..), LanguageDependent(..))
where
import DatabaseDesign.Ampersand.Core.ParseTree
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.Basics      (fatalMsg,eqCl,Collection(..),Identified(..))
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Fspec.Fspec
import Data.List

fatal :: Int -> String -> a
fatal = fatalMsg "Fspec.ShowADL"

class ShowADL a where
 showADL :: a -> String

-- there are data types yielding language dependent data like an Expression
-- the LanguageDependent class provides function(s) to map language dependent functions on those data if any.
-- for example to print all expressions in a data structure with a practical amount of type directives through application of 'disambiguate'
-- (instances have been created when needed)
--
-- LanguageDependent is part of ShowAdl because the only application at time of writing is to disambiguate expressions for the purpose of printing
class LanguageDependent a where
  mapexprs :: (Language l, ConceptStructure l, Identified l) => (l -> Expression -> Expression) -> l -> a -> a
  mapexprs _ _ = id

instance LanguageDependent a => LanguageDependent (Maybe a) where
  mapexprs _ _ Nothing  = Nothing
  mapexprs f l (Just x) = Just $ mapexprs f l x

instance LanguageDependent (a, Expression) where
  mapexprs f l (x,e) = (x, f l e)
instance LanguageDependent Rule where
  mapexprs f l rul = rul{rrexp = f l (rrexp rul)}
instance LanguageDependent Interface where
  mapexprs f l ifc = ifc{ifcViols = map (mapexprs f l) (ifcViols ifc), ifcObj = mapexprs f l (ifcObj ifc)}
instance LanguageDependent ObjectDef where
  mapexprs f l obj = obj{objctx = f l (objctx obj), objmsub = mapexprs f l $ objmsub obj}
instance LanguageDependent SubInterface where
  mapexprs _ _ iref@(InterfaceRef _) = iref
  mapexprs f l (Box objs) = Box $ map (mapexprs f l) objs
instance LanguageDependent Relation where
  mapexprs _ _ = id
instance LanguageDependent ECArule where
  mapexprs _ _ = id
instance LanguageDependent Event where
  mapexprs _ _ = id
--------------------------------------------------------------
instance ShowADL ObjectDef where
-- WHY (HJ)? In deze instance van ShowADL worden diverse zaken gebruikt die ik hier niet zou verwachten.
--              Het vertroebelt de code ook een beetje, want nu moeten er dingen als 'source' en
--              'target' hier al bekend zijn.
--              Dat lijkt me hier nog niet op z'n plaats, als je alleen maar wat wilt kunnen 'prettyprinten'. 
-- BECAUSE (SJ): Dit blijft nog even zo, omdat showADL gebruikt wordt in het genereren van interfaces.
--              Zolang we dat nog niet onder de knie hebben blijft de code wat troebel.
 showADL obj = " : "++showADL (objctx obj)++
               recur "\n  " (objmsub obj)
  where recur :: String -> Maybe SubInterface -> String
        recur _   Nothing = ""
        recur ind (Just (InterfaceRef nm)) = ind++" INTERFACE "++showstr nm
        recur ind (Just (Box objs))
         = ind++" BOX [ "++
           intercalate (ind++"     , ") 
                               [ showstr (name o)++
                                  (if null (objstrs o) then "" else " {"++intercalate ", " [showstr (unwords ss) | ss<-objstrs o]++"}")++
                                  " : "++showADL (objctx o)++
                                  recur (ind++"      ") (objmsub o)
                               | o<-objs
                               ]++
           ind++"     ]"

instance ShowADL Meta where
 showADL (Meta _ metaObj nm val) = 
   "META "++(if null $ showADL metaObj then "" else showADL metaObj++" ") ++show nm++" "++show val

instance ShowADL MetaObj where
 showADL ContextMeta = ""
 
instance ShowADL Purpose where
 showADL expl = "PURPOSE "++showADL (explObj expl)
                ++showADL (amLang (explMarkup expl))
                ++showADL (amFormat (explMarkup expl))
                ++(if null (explRefId expl) then "" else " REF "++explRefId expl)
                ++ "{+"++aMarkup2String (explMarkup expl)++"-}"

instance ShowADL PandocFormat where
 showADL LaTeX = "LATEX"
 showADL HTML  = "HTML"
 showADL ReST  = "REST"
 showADL Markdown = "MARKDOWN"
 
instance ShowADL A_Markup where
 showADL m 
     = "IN " ++showADL (amLang m) 
    ++ " "++showADL (amFormat m) 
    ++ "{+"++aMarkup2String m++"-}"
    
instance ShowADL Lang where
 showADL Dutch   = "DUTCH"
 showADL English = "ENGLISH"
   
--instance ShowADL (Maybe Lang) where
-- showADL  Nothing       = "IN DUTCH"
-- showADL (Just Dutch  ) = "IN DUTCH"
-- showADL (Just English) = "IN ENGLISH"
   
instance ShowADL ExplObj where
 showADL e = case e of
      ExplConceptDef cd  -> "CONCEPT "++cdcpt cd
      ExplDeclaration d  -> "RELATION "++showADL (makeRelation d)
      ExplRule r         -> "RULE "++showstr (name r)
      ExplKeyDef kd      -> "KEY "++showADL kd
      ExplPattern str    -> "PATTERN "++ showstr str
      ExplProcess str    -> "PROCESS "++str
      ExplInterface str  -> "INTERFACE "++showstr str
      ExplContext str    -> "CONTEXT "++showstr str
      ExplFspc str       -> "CONTEXT "++showstr str

showstr :: String -> String
showstr str = "\""++str++"\""

-- The declarations of the pattern are supplemented by all declarations needed to define the rules.
-- Thus, the resulting pattern is self-contained with respect to declarations.
instance ShowADL Process where
 showADL prc
  = "PROCESS " ++ name prc 
    ++ (if null (rules prc)     then "" else "\n  " ++intercalate "\n  " (map showADL (rules prc))     ++ "\n")
    ++ (if null (maintains prc) then "" else "\n  " ++                        showRM prc               ++ "\n")
    ++ (if null (mayEdit prc)   then "" else "\n  " ++                        showRR prc               ++ "\n")
    ++ (if null (conceptDefs prc)    then "" else "\n  " ++intercalate "\n  " (map showADL (conceptDefs prc))    ++ "\n")
    ++ (if null (prcKds prc)    then "" else "\n  " ++intercalate "\n  " (map showADL (prcKds prc))    ++ "\n")
    ++ (if null (prcXps prc)    then "" else "\n  " ++intercalate "\n  " (map showADL (prcXps prc))    ++ "\n")
    ++ "ENDPROCESS"
    where -- TODO: the following definitions should be unneccessary, but 'map showADL (maintains prc)' and "map showADL (mayEdit prc)" don't work... 
      showRM :: Process -> String
      showRM pr = intercalate "\n  " [ "ROLE "++role++" MAINTAINS "++intercalate ", " [name rul | (_,rul)<-cl]
                                     | cl<-eqCl fst (maintains pr), let role = fst (head cl)]
      showRR :: Process -> String
      showRR pr = intercalate "\n  " [ "ROLE "++role++" EDITS "++intercalate ", " [name rul | (_,rul)<-cl]
                                     | cl<-eqCl fst (mayEdit pr), let role = fst (head cl)]

instance ShowADL (String,Rule) where
 showADL (role,rul) = "ROLE "++role++" MAINTAINS "++show (name rul)

instance ShowADL (String,Relation) where
 showADL           (role,rel) = "ROLE "++role++" EDITS "++showADL rel

instance ShowADL (String,Interface) where
 showADL (role,ifc) = "ROLE "++role++" USES "++show (name ifc)
 
instance ShowADL Pattern where
 showADL pat
  = "PATTERN " ++ showstr (name pat) ++ "\n"
    ++ (if null (ptrls pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptrls pat)) ++ "\n")
    ++ (if null (ptgns pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptgns pat)) ++ "\n")
    ++ (if null (ptdcs pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptdcs pat)) ++ "\n")
    ++ (if null (conceptDefs pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (conceptDefs pat)) ++ "\n")
    ++ (if null (ptkds pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptkds pat)) ++ "\n")
    ++ "ENDPATTERN"

instance ShowADL PairViewSegment where
 showADL (PairViewText str)         = "TXT " ++ show str
 showADL (PairViewExp srcOrTgt e) = showADLSrcOrTgt srcOrTgt ++ " " ++ showADL e

showADLSrcOrTgt :: SrcOrTgt -> String
showADLSrcOrTgt Src = "SRC"
showADLSrcOrTgt Tgt = "TGT"
        
instance ShowADL Rule where
 showADL r
  = "RULE \""++rrnm r++"\" : "++showADL (rrexp r)
     ++ concat ["\n     MEANING "++showADL mng | mng <- ameaMrk $ rrmean r ]
     ++ concat ["\n     MESSAGE "++showADL msg | msg <- rrmsg r]
     ++ case rrviol r of
          Nothing                -> ""
          Just (PairView pvSegs) -> "\n     VIOLATION ("++intercalate ", " (map showADL pvSegs)++")"
       
instance ShowADL A_Gen where
 showADL (Gen _ g s _) = "SPEC "++showADL s++" ISA "++showADL g

instance ShowADL RoleRelation where
 showADL r
  = "ROLE "++intercalate ", " (map show (rrRoles r))++" EDITS "++intercalate ", " (map showADL (rrRels r))

instance ShowADL RoleRule where
 showADL r = "ROLE "++intercalate ", " (map show (mRoles r))++" MAINTAINS "++intercalate ", " (map show (mRules r))

instance ShowADL Interface where
 showADL ifc 
  = "INTERFACE "++showstr(name ifc)
          ++(if null (ifcParams ifc) then [] else "("++intercalate ", " [showADL r | r<-ifcParams ifc]++")")
          ++(if null (ifcArgs ifc) then [] else "{"++intercalate ", " [showstr(unwords strs) | strs<-ifcArgs ifc]++"}")
          ++showADL (ifcObj ifc)

instance ShowADL KeyDef where
 showADL kd 
  = "KEY "++kdlbl kd
          ++ ": " ++name (kdcpt kd)
          ++ "(" ++intercalate ", " (map showADL $ kdats kd) ++ ")"

instance ShowADL KeySegment where
 showADL (KeyExp objDef) = (if null (name objDef) then "" else "\""++name objDef++"\":") ++ showADL (objctx objDef)
 showADL (KeyText str) = "TXT " ++ show str
 showADL (KeyHtml str) = "PRIMHTML " ++ show str
                             

-- showADL Relation only prints complete signatures to ensure unambiguity.
-- therefore, when printing expressions, do not apply this function to print relations, but apply one that prints names only
instance ShowADL Relation where
 showADL rel = showADL (ETyp (ERel rel) (sign rel))

instance ShowADL Expression where
 showADL = showExpr (" = ", " |- ", " /\\ ", " \\/ ", " - ", " / ", " \\ ", ";", "!", "*", "*", "+", "~", ("-"++), "(", ")", "[", "*", "]") . insParentheses
-- NOTE: retain space after \\, because of unexpected side effects if it is used just before an 'r' or 'n'....

instance ShowADL Declaration where
 showADL decl = 
  case decl of
     Sgn{decusr = False} -> fatal 323 "call to ShowADL for declarations can be done on user defined relations only." 
     Sgn{} -> name decl++" :: "++name (source decl)++(if null ([Uni,Tot]>-multiplicities decl) then " -> " else " * ")++name (target decl)++
              (let mults=if null ([Uni,Tot]>-multiplicities decl) then multiplicities decl>-[Uni,Tot] else multiplicities decl in
               if null mults then "" else "["++intercalate "," (map showADL mults)++"]")++
              (if null(decprL decl++decprM decl++decprR decl) then "" else
               " PRAGMA "++unwords (map show [decprL decl,decprM decl,decprR decl]))
               ++ concatMap meaning (ameaMrk (decMean decl))
               ++ maybe "" (\(RelConceptDef srcOrTgt def) -> " DEFINE "++showADLSrcOrTgt srcOrTgt ++ " " ++ def) (decConceptDef decl)
     Isn{}     -> fatal 330 "Illegal call to ShowADL (Isn{}). Isn{} is of type Declaration and it is not user defined. A call to ShowADL for declarations can be done on user defined declarations only." 
     Iscompl{} -> fatal 331 "Illegal call to ShowADL (Iscompl{}). Iscompl{} is of type Declaration and it is not user defined. A call to ShowADL for declarations can be done on user defined declarations only." 
     Vs{}      -> fatal 332 "Illegal call to ShowADL (Vs{}). Vs{} is of type Declaration and it is not user defined. A call to ShowADL for declarations can be done on user defined declarations only." 
   where
     meaning :: A_Markup -> String
     meaning pmkup = " MEANING "++showADL pmkup

instance ShowADL P_Markup where
 showADL (P_Markup lng fmt str) = case lng of
                                     Nothing -> ""
                                     Just l  -> "IN "++show l++" "
                                 ++
                                  case fmt of
                                     Nothing -> ""
                                     Just f  -> " "++show f++" "
                                 ++
                                  "{+"++str++"-} "
 
instance ShowADL Prop where
 showADL = show

instance ShowADL A_Concept where
 showADL c = show (name c)

instance ShowADL ConceptDef where
 showADL cd
  = "\n  CONCEPT "++show (cdcpt cd)++" "++show (cddef cd)++" "++(if null (cdref cd) then "" else show (cdref cd))

instance ShowADL A_Context where
 showADL context
  = "CONTEXT " ++name context
    ++ (if null (ctxmetas context) then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxmetas context))++ "\n")
    ++ (if null (ctxprocs context) then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxprocs context))++ "\n") -- All processes
    ++ (if null (ctxifcs context)  then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxifcs context)) ++ "\n")
    ++ (if null (ctxpats context)  then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxpats context)) ++ "\n")
    ++ (if null (ctxrs context)    then "" else "\n"      ++intercalate "\n"   (map showADL (ctxrs context))   ++ "\n")
    ++ (if null (ctxds context)    then "" else "\n"      ++intercalate "\n"   (map showADL (ctxds context))   ++ "\n")
    ++ (if null (ctxks context)    then "" else "\n"      ++intercalate "\n"   (map showADL (ctxks context))   ++ "\n")
    ++ (if null (ctxgs context)    then "" else "\n"      ++intercalate "\n"   (map showADL (ctxgs context))   ++ "\n")
    ++ (if null cds                then "" else "\n"      ++intercalate "\n"   (map showADL  cds           )   ++ "\n")
    ++ (if null showADLpops        then "" else "\n"++intercalate "\n\n" showADLpops                           ++ "\n")
    ++ (if null (ctxsql context)   then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxsql context)) ++ "\n")
    ++ (if null (ctxphp context)   then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxphp context)) ++ "\n")
    ++ "\n\nENDCONTEXT"
    where showADLpops = [ showADL Popu{popm=makeRelation d, popps=decpopu d}
                        | d<-declarations context, decusr d, not (null (decpopu d))]
                        ++ [showADL (P_CptPopu{p_popm=name c, p_popps=[(a,a) | a<-atomsOf c]}) | c<-concs context, c/=ONE, not(null (atomsOf c))]
          cds = conceptDefs context >- (concatMap conceptDefs (ctxpats context) ++ concatMap conceptDefs (ctxprocs context))

instance ShowADL Fspc where
 showADL fSpec
  = "CONTEXT " ++name fSpec
    ++ (if null (map ifcObj [] {- map fsv_ifcdef (fActivities fSpec) -})     
        then "" 
        else "\n"++intercalate "\n\n" (map (showADL . ifcObj) [] {- map fsv_ifcdef (fActivities fSpec) -})     ++ "\n")
    ++ (if null (metas fSpec)    then "" else "\n"++intercalate "\n\n" (map showADL (metas fSpec))    ++ "\n")
    ++ (if null (patterns fSpec)    then "" else "\n"++intercalate "\n\n" (map showADL (patterns fSpec))    ++ "\n")
    ++ (if null cds then "" else "\n"++intercalate "\n"   (map showADL cds) ++ "\n")
    ++ (if null (gens fSpec) then "" else "\n"++intercalate "\n"   (map showADL (gens fSpec >- concatMap gens (patterns fSpec))) ++ "\n")
    ++ (if null (keyDefs fSpec)       then "" else "\n"++intercalate "\n"   (map showADL (keyDefs fSpec >- concatMap keyDefs (patterns fSpec)))       ++ "\n")
    ++ (if null (declarations fSpec) then "" else "\n"++intercalate "\n"   (map showADL (declarations fSpec >- concatMap declarations (patterns fSpec))) ++ "\n")
    ++ (if null (rules fSpec) then "" else "\n"++intercalate "\n"   (map showADL (rules fSpec >- concatMap rules (patterns fSpec))) ++ "\n")
    ++ (if null (fSexpls fSpec) then "" else "\n"++intercalate "\n"   (map showADL (fSexpls fSpec)) ++ "\n")
    ++ (if null showADLpops         then "" else "\n"++intercalate "\n\n" showADLpops                                    ++ "\n")
    ++ (if null (interfaceS fSpec)    then "" else "\n"++intercalate "\n\n" (map showADL (interfaceS fSpec))    ++ "\n")
    ++ "\n\nENDCONTEXT"
    where showADLpops = [ showADL Popu{popm=makeRelation d, popps=decpopu d}
                        | d<-declarations fSpec, decusr d, not (null (decpopu d))]
                        ++ [showADL (P_CptPopu{p_popm=name c, p_popps=[(a,a) | a<-atomsOf c]}) | c<-concs fSpec, c/=ONE, not(null (atomsOf c))]
          cds = vConceptDefs fSpec >- (concatMap conceptDefs patts ++ concatMap (conceptDefs.fpProc) procs)
          patts = if null (themes fSpec)
                  then patterns fSpec
                  else [ pat | pat<-patterns fSpec, name pat `elem` themes fSpec ]
          procs = if null (themes fSpec)
                  then vprocesses fSpec
                  else [ prc | prc<-vprocesses fSpec, name prc `elem` themes fSpec ]

instance ShowADL ECArule where
  showADL eca = "ECA #"++show (ecaNum eca)
instance ShowADL Event where
  showADL = show
instance (ShowADL a, ShowADL b) => ShowADL (a,b) where
 showADL (a,b) = "(" ++ showADL a ++ ", " ++ showADL b ++ ")"

instance ShowADL P_Population where
 showADL pop@(P_Popu{})
  = "POPULATION "++name pop++(if null pConcepts then [] else "["++name(head pConcepts)++"*"++name(last pConcepts)++"]")++" CONTAINS\n"++
    indent++"[ "++intercalate ("\n"++indent++", ") (map (\(x,y)-> showatom x++" * "++ showatom y) (p_popps pop))++indent++"]"
    where indent = "   "; P_Sign pConcepts=p_type pop
 showADL pop@(P_CptPopu{})
  = "POPULATION "++p_popm pop++" CONTAINS\n"++
    indent++"[ "++intercalate ("\n"++indent++", ") (map (\(x,_)-> showatom x) (p_popps pop)) ++indent++"]"
    where indent = "   "

instance ShowADL Population where
 showADL (Popu r pairs)
  = "POPULATION "++showADL r++" CONTAINS\n"++
    indent++"[ "++intercalate ("\n"++indent++", ") (map (\(x,y)-> showatom x++" * "++ showatom y) pairs)++indent++"]"
    where indent = "   "
showatom :: String -> String
showatom x = "'"++[if c=='\'' then '`' else c|c<-x]++"'"              

--used to compose error messages at p2a time
instance ShowADL Term where
 showADL = showPExpr (" = ", " |- ", " /\\ ", " \\/ ", " - ", " / ", " \\ ", ";", "!", "*", "*", "+", "~", "(", ")", "[", "*", "]")
   where
    showPExpr (equi,impl,inter,union',diff,lresi,rresi,rMul,rAdd,rPrd,closK0,closK1,flp',lpar,rpar,lbr,star,rbr) expr
     = showchar (insP_Parentheses expr)
      where
       showchar (PI _)                                   = "I"
       showchar (Pid c)                                  = "I["++showADL c++"]"
       showchar (Pnid c)                                 = "-I["++showADL c++"]"
       showchar (Patm _ a [])                            = "'"++a++"'"
       showchar (Patm _ a cs)                            = "'"++a++"'["++show (head cs)++"]"
       showchar Pnull                                    = "-V"
       showchar (Pfull _ [])                             = "V"
       showchar (Pfull _ cs)                             = "V["++intercalate "*" (map show cs)++"]"
       showchar (Prel _ rel)                             = rel
       showchar (Pflp _ rel)                             = rel++flp'
       showchar (Pequ _ l r)                             = showchar l++equi++showchar r
       showchar (Pimp _ l r)                             = showchar l++impl++showchar r
       showchar (PIsc _ l r)                             = showchar l++inter++showchar r
       showchar (PUni _ l r)                             = showchar l++union'++showchar r
       showchar (PDif _ l r)                             = showchar l++diff ++showchar r
       showchar (PLrs _ l r)                             = showchar l++lresi++showchar r
       showchar (PRrs _ l r)                             = showchar l++rresi++showchar r
       showchar (PCps _ l r)                             = showchar l++rMul++showchar r
       showchar (PRad _ l r)                             = showchar l++rAdd++showchar r
       showchar (PPrd _ l r)                             = showchar l++rPrd++showchar r
       showchar (PKl0 _ e)                               = showchar e++closK0
       showchar (PKl1 _ e)                               = showchar e++closK1
       showchar (PFlp _ e)                               = showchar e++flp'
       showchar (PCpl _ e)                               = '-':showchar e
       showchar (PBrk _ e)                               = lpar++showchar e++rpar
       showchar (PTyp _ e@(Prel{}) psgn)                 = showchar e++showsign psgn
       showchar (PTyp _ (Pflp _ rel) (P_Sign{psign=xs})) = rel++showsign (P_Sign{psign=reverse xs})++flp'
       showchar (PTyp _ e psgn)                          = lpar++showchar e++rpar++showsign psgn
       showsign (P_Sign{psign=[x]})                      = lbr++showADL x++rbr
       showsign (P_Sign{psign=xs })                      = lbr++showADL (head xs)++star++showADL (last xs)++rbr

insP_Parentheses :: Term -> Term
insP_Parentheses = insPar 0
      where
       wrap :: Integer -> Integer -> Term -> Term
       wrap i j e' = if i<=j then e' else PBrk (origin e') e'
       insPar :: Integer -> Term -> Term
       insPar i (Pequ o l r) = wrap i     0 (Pequ o (insPar 1 l) (insPar 1 r))
       insPar i (Pimp o l r) = wrap i     0 (Pimp o (insPar 1 l) (insPar 1 r))
       insPar i (PIsc o l r) = wrap (i+1) 2 (PIsc o (insPar 2 l) (insPar 2 r))
       insPar i (PUni o l r) = wrap (i+1) 2 (PUni o (insPar 2 l) (insPar 2 r))
       insPar i (PDif o l r) = wrap i     4 (PDif o (insPar 5 l) (insPar 5 r))
       insPar i (PLrs o l r) = wrap i     6 (PLrs o (insPar 7 l) (insPar 7 r))
       insPar i (PRrs o l r) = wrap i     6 (PRrs o (insPar 7 l) (insPar 7 r))
       insPar i (PCps o l r) = wrap (i+1) 8 (PCps o (insPar 8 l) (insPar 8 r))
       insPar i (PRad o l r) = wrap (i+1) 8 (PRad o (insPar 8 l) (insPar 8 r))
       insPar i (PPrd o l r) = wrap (i+1) 8 (PPrd o (insPar 8 l) (insPar 8 r))
       insPar _ (PKl0 o e)   = PKl0 o (insPar 10 e)
       insPar _ (PKl1 o e)   = PKl1 o (insPar 10 e)
       insPar _ (PFlp o e)   = PFlp o (insPar 10 e)
       insPar _ (PCpl o e)   = PCpl o (insPar 10 e)
       insPar i (PBrk _ e)   = insPar i e
       insPar _ (PTyp o e t) = PTyp o (insPar 10 e) t
       insPar _ x            = x


--used to compose error messages at p2a time
instance ShowADL P_Relation where
 showADL rel = case rel of
      P_Rel{} -> rel_nm rel
      P_I{}   -> " I "
      P_V{}   -> " V "
      P_Mp1{} -> rel_1val rel

--used to compose error messages at p2a time
instance ShowADL P_Concept where
 showADL = name

instance ShowADL PAclause where
    showADL = showPAclause "\n "
     where
      showPAclause indent pa@Chc{}
       = let indent'=indent++"   " in "execute ONE from"++indent'++intercalate indent' [showPAclause indent' p' | p'<-paCls pa]
      showPAclause indent pa@All{}
       = let indent'=indent++"   " in "execute ALL of"++indent'++intercalate indent' [showPAclause indent' p' | p'<-paCls pa]
      showPAclause indent pa@Do{}
       = ( case paSrt pa of
            Ins -> "INSERT INTO "
            Del -> "DELETE FROM ")++
         show (paTo pa)++
         " SELECTFROM "++
         show (paDelta pa)
         ++concat [ indent++showConj rs | (_,rs)<-paMotiv pa ]
      showPAclause indent pa@Sel{}
       = let indent'=indent++"   " in
        "SELECT x:"++show (paCpt pa)++" FROM "++showADL (paExp pa)++";"++indent'++showPAclause indent' (paCl pa "x")
      showPAclause indent pa@New{}
       = let indent'=indent++"   " in
        "CREATE x:"++show (paCpt pa)++";"++indent'++showPAclause indent' (paCl pa "x")
      showPAclause indent pa@Rmv{}
       = let indent'=indent++"   " in
        "REMOVE x:"++show (paCpt pa)++";"++indent'++showPAclause indent' (paCl pa "x")
      showPAclause _ Nop{} = "Nop"
      showPAclause _ Blk{} = "Blk"
      showPAclause _ (Let _ _ _) = fatal 390 "showPAclasue not defined for `Let`. Consult your dealer!"
      showPAclause _ (Ref _)     = fatal 391 "showPAclasue not defined for `Ref`. Consult your dealer!"
      showConj rs
              = "(TO MAINTAIN"++intercalate ", " [name r | r<-rs]++")"
