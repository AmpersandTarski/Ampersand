{-# OPTIONS_GHC -Wall -XUndecidableInstances -XFlexibleContexts -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances #-}
  -- | BECAUSE (HJ) Wat is precies het doel van Show vs ShowADL ??
  -- ANTWOORD (SJ): De standaard-show is alleen bedoeld voor simpele foutmeldingen tijdens het testen.
  --                showADL is bedoeld om Ampersand syntax te genereren.
  --                showADL is contextonafhankelijk, en produceert syntactisch correcte Ampersand code, die echter wel typefouten zou kunnen bevatten,
  --                namelijk dubbelzinnigheden die met een expliciet type opgelost hadden kunnen worden.
  --                showADLcode maakt gebruik van ontologische informatie in Fspc, namelijk vRels en isa, om in dit soort gevallen het type
  --                expliciet te maken.
  --                Daarmee produceert showADLcode volledig correcte Ampersand code, dus typecorrect en zonder service-warnings.
  -- Question (SJC): If STRING is the code produced by showADLcode fSpec, would STRING == showADL (parse STRING) (context (parse STRING)) be true?
  -- Answer (SJ):   No, not for every STRING. Yet, for every fSpec we want  semantics fSpec == semantics (parse (showADLcode fSpec fSpec)).
  --                Note that 'parse' and 'semantics' do not exist in this shape, so the actual expression is slightly more complicated.
module ShowADL ( ShowADL(..), disambiguate, mphatsoff)
  where
   import Char                            (isAlphaNum)
   import Collection                      (Collection(..))
   import Ampersand
   import Data.Fspec 
   import Data.List
   import Auxiliaries                     (eqCl)
   import Data.Explain
 
   class ShowADL a where
    showADL :: a -> String
    showADLcode :: Fspc -> a -> String
    showADLcode _ x = showADL x



{--------------------------------------------  
   class ShowADL a where
    showADL :: a -> String
    showADLcode ::  Fspc -> a -> String
--------------------------------------------}


--------------------------------------------------------------
   --EXTEND -> showADLcode must be the inverse of parse. Concrete: Haskell code generated from the original file must be literally equivalent to Haskell code generated from the showADLcode string.
   --TODO -> check equivalence of generated Haskell code 
   --TODO -> comments in original script must also be printed
   --TODO -> what about extends? Answer: ignore untill revised
   --WHY -> aren't ONE Anything NOthing etc reserved words on pString, pConid, (etc?)? Answer: check if errors can be produced without reserved words. If so add reserved words, otherwise don't
   --TODO -> sort on file position
   --TODO -> ALWAYS cannot be used in combination with -p -l or -s and maybe more, because something tries to retrieve the rrant, which is an error.
   --TODO -> ALWAYS pProps (ObjectDef) is ignored. It may be enabled some day to communicate interface policies
   --TODO -> move the flips from Relation to Expression data type
   --TODO -> remove application of double complement rule from the parser
   --TODO -> remove removal of brackets on ; expression from the parser

   mphatson :: Eq c => Expression (Relation c) -> Expression (Relation c)
   mphatson = mapExpression f
    where f m = case m of
                 Mph{mphats=[]}->if inline m then m{mphats=[source m,target m]} else  m{mphats=[target m,source m]}
                 _ -> m

   mphatsoff :: Expression (Relation c) -> Expression (Relation c)
   mphatsoff = mapExpression f
    where f m = case m of
                 Mph{} -> m{mphats=[]}
                 _     -> m


   instance ShowADL ObjectDef where
   -- WHY (HJ)? In deze instance van ShowADL worden diverse zaken gebruikt die ik hier niet zou verwachten.
   --              Het vertroebelt de code ook een beetje, want nu moeten er dingen als 'inline', 'source' en
   --              'target' hier al bekend zijn.
   --              Dat lijkt me hier nog niet op z'n plaats, als je alleen maar wat wilt kunnen 'prettyprinten'. 
   -- BECAUSE (SJ): Dit blijft nog even zo, omdat showADL gebruikt wordt in het genereren van services.
   --              Zolang we dat nog niet onder de knie hebben blijft de code wat troebel.
    showADL obj = "  SERVICE "++name obj++" : I["++(name (target (objctx obj)))++"]"++
                  recur "\n  " (objats obj)
     where recur :: String -> [ObjectDef] -> String
           recur ind objs
            = ind++" = [ "++
              intercalate (ind++"   , ") 
                                [ name o++
                                     (if null (objstrs o) then "" else " {"++intercalate ", " [intercalate " " (map str ss)| ss<-objstrs o]++"}")++
                                     " : "++(-- if isIdent (objctx o) then showSign[target (objctx o)] else -- TODO: deze functionaliteit aanzetten
                                             if isTrue  (objctx o) then showSign[S,target (objctx o)] else
                                             showADL (objctx o))++
                                     if null (objats o) then "" else recur (ind++"     ") (objats o)
                                   | o<-objs ]++
              ind++"   ]"
           str ss | and [isAlphaNum c| c<-ss] = ss
                  | otherwise                 = "\""++ss++"\""
    showADLcode fSpec obj = "  SERVICE "++name obj++" : "++showADLcode fSpec (objctx obj)++
                            recur "\n  " (objats obj)
     where recur :: String -> [ObjectDef] -> String
           recur ind objs
            = ind++" = [ "++
              intercalate (ind++"   , ") 
                                  [ name o++(if name o `elem` cls then show i else "")++
                                     (if null (objstrs o) then "" else " {"++intercalate ", " [intercalate " " (map str ss)| ss<-objstrs o]++"}")++
                                     " : "++showADLcode fSpec (objctx o)++
                                     if null (objats o) then "" else recur (ind++"     ") (objats o)
                                  | (o,i)<-zip objs [(1::Integer)..]
                                  , cls<-[[name c|cl<-eqCl name (declarations fSpec), length cl>1, c<-take 1 cl]]
                                  ]++
              ind++"   ]"
           str ss | and [isAlphaNum c| c<-ss] = ss
                  | otherwise                 = "\""++ss++"\""

   instance ShowADL Explanation where
    showADL (Expl eObj lang ref expla) = "EXPLAIN "++showADL eObj++" IN "++show lang++(if null ref then "" else " REF "++ref)++ showADL expla
    showADLcode fSpec (Expl eObj lang ref expla) = "EXPLAIN "++showADLcode fSpec eObj++" IN "++show lang++(if null ref then "" else " REF "++ref)++ showADL expla

   instance ShowADL ExplObj where
      showADL e = case e of
         ExplConceptDef cd -> "CONCEPT "++name cd
         ExplDeclaration d -> "RELATION "++showADL d
         ExplRule r        -> "RULE "++name r
         ExplKeyDef kd     -> "KEY "++name kd
         ExplObjectDef od  -> "SERVICE "++name od
         ExplPattern str   -> "PATTERN "++str
         ExplContext str   -> "CONTEXT "++str
      showADLcode fSpec e = case e of
         ExplConceptDef cd -> "CONCEPT "++name cd
         ExplDeclaration d -> "RELATION "++showADLcode fSpec d
         ExplRule r        -> "RULE "++name r
         ExplKeyDef kd     -> "KEY "++name kd
         ExplObjectDef od  -> "SERVICE "++name od
         ExplPattern str   -> "PATTERN "++str
         ExplContext str   -> "CONTEXT "++str 

   instance ShowADL ExplainContent where
    showADL expla 
       -- TODO: afspraken maken over de vertaling van explanations in adlcode van en naar Pandoc... (Nu alleen nog de inlines Str String en Linebreak)
     = show expla -- (if '\n' `elem` expla then "\n{+ "++expla++"-}" else " -+ "++expla)

   -- The declarations of the pattern are supplemented by all declarations needed to define the rules.
   -- Thus, the resulting pattern is self-contained with respect to declarations.
   instance ShowADL Pattern where
    showADL pat
     = "PATTERN " ++ name pat 
       ++ (if null (ptrls pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptrls pat)) ++ "\n")
       ++ (if null (ptgns pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptgns pat)) ++ "\n")
       ++ (if null (ptdcs pat)  then "" else "\n  " ++intercalate "\n  " (map showADL ds         ) ++ "\n")
       ++ (if null (ptcds pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptcds pat)) ++ "\n")
       ++ (if null (ptkds pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptkds pat)) ++ "\n")
       ++ "ENDPATTERN"
       where ds = ptdcs pat++[d| d@Sgn{}<-declarations pat `uni` rd [makeDeclaration m| m<-mors (ptrls pat) `uni` mors (ptkds pat)]
                               , decusr d, not (d `elem` ptdcs pat)]
    showADLcode fSpec pat
     = "PATTERN " ++ name pat 
       ++ (if null (ptrls pat)  then "" else "\n  " ++intercalate "\n  " (map (showADLcode fSpec) (ptrls pat)) ++ "\n")
       ++ (if null (ptgns pat)  then "" else "\n  " ++intercalate "\n  " (map (showADLcode fSpec) (ptgns pat)) ++ "\n")
       ++ (if null (ptdcs pat)  then "" else "\n  " ++intercalate "\n  " (map (showADLcode fSpec) ds         ) ++ "\n")
       ++ (if null (ptcds pat)  then "" else "\n  " ++intercalate "\n  " (map (showADLcode fSpec) (ptcds pat)) ++ "\n")
       ++ (if null (ptkds pat)  then "" else "\n  " ++intercalate "\n  " (map (showADLcode fSpec) (ptkds pat)) ++ "\n")
       ++ "ENDPATTERN"
       where ds = ptdcs pat++[d| d@Sgn{}<-declarations pat `uni` rd [makeDeclaration m| m<-mors (ptrls pat) `uni` mors (ptkds pat)]
                               , decusr d, not (d `elem` ptdcs pat)]


   instance (Show c, SpecHierarchy c, Association r c, Show r, ShowADL c, ShowADL (Expression r)) => ShowADL (Rule r) where
    showADL r
     = (if isSignal r
       then "RULE "++name r++" SIGNALS "
       else "RULE "++name r++" MAINTAINS ")++
       case rrsrt r of
            Truth          -> showADL (rrcon r)
            Implication    -> showADL (rrant r) ++" |- "++showADL (rrcon r)
            Equivalence    -> showADL (rrant r) ++" = " ++showADL (rrcon r)
            Generalization -> showADL (G (pos r) (source (rrcon r)) (source (rrant r)) (r_pat r))
    showADLcode fSpec r
     = (if isSignal r
       then "RULE "++name r++" SIGNALS "
       else "RULE "++name r++" MAINTAINS ")++
       case rrsrt r of
            Truth          -> showADLcode fSpec (rrcon r)
            Implication    -> showADLcode fSpec (rrant r) ++" |- "++showADLcode fSpec (rrcon r)
            Equivalence    -> showADLcode fSpec (rrant r) ++" = " ++showADLcode fSpec (rrcon r)
            Generalization -> showADLcode fSpec (G (pos r) (source (rrcon r)) (source (rrant r)) (r_pat r))

   instance ShowADL c => ShowADL (Gen c) where
    showADL (G _ g s _) = "GEN "++showADL s++" ISA "++showADL g
    showADLcode fSpec (G _ g s _) = "GEN "++showADLcode fSpec s++" ISA "++showADLcode fSpec  g

   instance ShowADL RoleService where
    showADL r = "ROLE "++intercalate ", " (map show (rsRoles r))++" USES "++intercalate ", " (map show (rsServices r))
    showADLcode _ r = showADL r

   instance ShowADL RoleRelation where
    showADL r
     = "ROLE "++intercalate ", " (map show (rrRoles r))++" EDITS "++intercalate ", " (map showADL (rrRels r))
    showADLcode fSpec r
     = "ROLE "++intercalate ", " (map show (rrRoles r))++" EDITS "++intercalate ", " (map (showADLcode fSpec) (rrRels r))

   instance ShowADL Service where
    showADL sv 
     = "SERVICE "++svName sv
             ++"("++intercalate ", " [showADL m | m<-svParams sv]++")\n"
             ++"{"++intercalate ", " [intercalate " " strs | strs<-svArgs sv]++"}\n"
             ++showADL (svObj sv)
    showADLcode fSpec sv 
     = "SERVICE "++svName sv
             ++"("++intercalate ", " [showADLcode fSpec m | m<-svParams sv]++")\n"
             ++"{"++intercalate ", " [intercalate " " strs | strs<-svArgs sv]++"}\n"
             ++showADLcode fSpec (svObj sv)

   instance ShowADL KeyDef where
    showADL kd 
     = "KEY "++kdlbl kd
             ++": "++name (kdcpt kd)
             ++"("++intercalate ", " [(if null (name o) then "" else name o++":") ++ showADL (objctx o)
                                     | o<-kdats kd]++")"
    showADLcode fSpec kd 
     = "KEY "++kdlbl kd
             ++": "++name (kdcpt kd)
             ++"("++intercalate ", " [(if null (name o) then "" else name o++":") ++ showADLcode fSpec (objctx o)
                                     | o<-kdats kd]++")"


-- The function 'disambiguate' must ensure that an expression, when printed, can be parsed with no ambiguity.
-- Besides, it must be readable as well.
-- The effect is that the mphats attribute of all relations in an expression will be set,
-- if otherwise there would be multiple interpretations possible.
-- In the absence of disambiguity, the mphats attribute remains empty.
-- This is done by looking at the concept sets between two adjacent terms.
-- Internal to this definition, type information has been given for documentation purposes.
-- Since fSpec is specific for Concept, the type for disambiguate is specific to Expression (Relation Concept).
-- One call to 'declarations fSpec' is the only reason why the type of disambiguate is specific for Concept.
   disambiguate :: Fspc -> Expression (Relation Concept) -> Expression (Relation Concept)
   disambiguate _   (Tm mph i)  = Tm mph i
   disambiguate fSpec (Fux fs)  = Fux [disambiguate fSpec f| f<-fs]
   disambiguate fSpec (Fix fs)  = Fix [disambiguate fSpec f| f<-fs]
   disambiguate _     (Fdx [])  = Fdx []
   disambiguate fSpec (Fdx [t]) = Fdx [disambiguate fSpec t]
   disambiguate fSpec (Fdx ts)  = Fdx (disamb fSpec [disambiguate fSpec t| t<-ts])
   disambiguate _     (F [])    = F   []
   disambiguate fSpec (F [t])   = F   [disambiguate fSpec t]
   disambiguate fSpec (F ts)    = F   (disamb fSpec [disambiguate fSpec t| t<-ts])
   disambiguate fSpec (K0x e')  = K0x (disambiguate fSpec e')
   disambiguate fSpec (K1x e')  = K1x (disambiguate fSpec e')
   disambiguate fSpec (Cpx e')  = Cpx (disambiguate fSpec e')
   disambiguate fSpec (Tc f)    = Tc  (disambiguate fSpec f)

-- Disamb disambiguates a list of terms (expressions) that come from an Fd or F expression.
   disamb :: Fspc -> [Expression (Relation Concept)] -> [Expression (Relation Concept)]
   disamb fSpec ts
     = let ims=strands1 triples in
       if length ims==1 then head ims++[t|(_,_,t)<-[last triples]] else disamb fSpec (concat ims++[t|(_,_,t)<-[last triples]])
       where
-- The list 'triples' contains the concept sets between two adjacent terms.
-- A concept set contains all possibilities for allocating a concept (i.e. type) between two adjacent terms
-- A concept set with more than one concept shows a possible ambiguity,  that 'disamb' will resolve.
--      triples :: [(Expression (Relation Concept),[Concept],Expression (Relation Concept))]
        triples
         = [ (t, (rd.map last.types) t `isc` (rd.map head.types) t', t')
           | (t,t')<-zip (init ts) (tail ts)]
-- First we make alternating strands of triples:
--  triples without a problem (i.e. the concept set is not longer than 1); these are dealt with by p1
--  and triples with an ambiguity; these are partially disambiguated by pn.
--  Since pn does its job partially, we have put a fixpoint over the entire function 'disamb', making sure the whole job is done.
--      strands1 :: [(Expression (Relation Concept),[Concept],Expression (Relation Concept))]
--                  ->  [[Expression (Relation Concept)]]
        strands1 []  = []
        strands1 iss = p1 (takeWhile select iss): strandsn (dropWhile select iss)
                       where select (_,m,_) = length m<=1
--      strandsn :: [(Expression (Relation Concept),[Concept],Expression (Relation Concept))]
--                  ->  [[Expression (Relation Concept)]]
        strandsn []  = []
        strandsn iss = pn (takeWhile select iss): strands1 (dropWhile select iss)
                       where select (_,m,_) = length m>1
--      p1 :: [(Expression (Relation Concept),[Concept],Expression (Relation Concept))] -> [Expression (Relation Concept)]
        p1 iss = [s| (s,_,_)<-iss]
--      pn :: [(Expression (Relation Concept),[Concept],Expression (Relation Concept))] -> [Expression (Relation Concept)]
        pn [] = error("!Fatal (module ShowADL 265): calling pn with empty list")
        pn [(s,_,t)] = [s,Tm (mIs (target s `lub` source t)) (-1)]
        pn iss = [s|(s,_,_)<-lss]++[mphatson s|(s,_,_)<-[head rss]]++[s|(s,_,_)<-tail rss]
                 where lss = take halfway iss
                       rss = drop halfway iss
                       halfway = length iss `div` 2
-- The following function is used to force the type of a relation to be printed.
--      types :: Expression (Relation Concept) -> [[Concept]]
        types (Tm mph _) = if null (mphats mph)
                           then rd [ if inline mph then [source d,target d] else [target d,source d]
                                   | d<-declarations fSpec, name mph==name d]   -- Note: fSpec is specific for Concept, so this is the only reason why the type of disamb is specific to Expression (Relation Concept)
                           else [mphats mph]
        types (Fux fs)   = foldr isc [] [types f| f<-fs]
        types (Fix fs)   = foldr isc [] [types f| f<-fs]
        types (Fdx ts')  = types (F ts') -- a nifty trick to save code. After all, the type computation is identical to F...
        types (F  [])    = error "!Fatal (module ShowADL 280): types (F []) = [[Anything,Anything]]"
        types (F  ts')   = [[s,t]| s<-(rd.map head.head) ttyps, t<-(rd.map last.last) ttyps]
                          where
--                         iscSets :: [[Concept]] -- the list of 'in-between-concepts'
                           iscSets 
                            = [(rd.map head.types.head) ts'] ++
                              [ (rd.map last.types) t `isc` (rd.map head.types) t'
                              | (t,t')<-zip (init ts') (tail ts')] ++
                              [(rd.map last.types.last) ts']
                           ttyps -- the list of which each element corresponds to this list of types allocatable to t, in which t<-ts')
                            = [ [d| d<-types t, head d `elem` scs, last d `elem` tgs]
                              | ((scs,tgs),t)<-zip (zip (init iscSets) (tail iscSets)) ts'
                              ]
        types (K0x e')  = types e'
        types (K1x e')  = types e'
        types (Cpx e')  = types e'
        types (Tc f)    = types f

   instance ShowADL (Expression (Relation Concept)) where
    showADL e = show e
    showADLcode fSpec expr  = showExpr (" \\/ ", "/\\", "!", ";", "*", "+", "-", "(", ")") expr
      where
       showExpr (union',inter,rAdd,rMul,clos0,clos1,compl,lpar,rpar) expr'
        = (showchar.insParentheses.disambiguate fSpec.mphatsoff) expr'
         where
          showchar (Tm mph _) = showADLcode fSpec mph
          showchar (Fux [])   = "-V"
          showchar (Fux fs)   = intercalate union' [showchar f| f<-fs]
          showchar (Fix [])   = "V"
          showchar (Fix fs)   = intercalate inter [showchar f| f<-fs]
          showchar (Fdx [])   = "-I"
          showchar (Fdx ts)   = intercalate rAdd [showchar t| t<-ts]
          showchar (F [])     = "I"
          showchar (F ts)     = intercalate rMul [showchar t| t<-ts]
          showchar (K0x e')   = showchar e'++clos0
          showchar (K1x e')   = showchar e'++clos1
          showchar (Cpx e')   = compl++showchar e'
          showchar (Tc f)     = lpar++showchar f++rpar

   instance (Eq c, Identified c, ShowADL c) => ShowADL (Relation c) where
    showADL m@Mph{}
     = ({- if take 5 nm=="Clos_" then drop 5 nm++"*" else -} decnm s)++
       (if null (mphats m)
            then (if       inline m && source m==source s && target m==target s
                    || not(inline m)&& source m==target s && target m==source s
                  then "" else showSign [source m,target m])
            else showSign (mphats m))++
       if inline m then "" else "~"
       where s = makeDeclaration m
    showADL (I atts g s yin)
     = "I"++if null atts then "" else showSign atts++if g==s then "" else if yin then "" else "~"
    showADL (V atts _)
     = "V"++if null atts then "" else showSign atts
    showADL m@(Mp1{})
     = "'"++mph1val m++"'"++(showSign [mph1typ m])
    showADLcode _ mph@Mph{}
     = decnm (mphdcl mph)++
       (if null (mphats mph) then "" else showSign (mphats mph))++
       if inline mph then "" else "~"
       -- where dss = [(name.head) cl| cl<-eqCl name (declarations fSpec), length cl>1]
    showADLcode _ (I atts g s yin)
     = "I"++if null atts then showSign [g,s] else showSign atts++if g==s then "" else if yin then "" else "~"
    showADLcode _ (V atts (a,b))
     = "V"++if null atts then showSign [a,b] else showSign atts
    showADLcode _ m@(Mp1{})
     = "'"++mph1val m++"'"++(showSign [mph1typ m])

   instance (Identified c, Conceptual c) => ShowADL (Declaration c) where
    showADL decl@Sgn{}
     = if not (decusr decl) then error("!Fatal (module ShowADL 347): call to ShowADL for declarations can be done on user defined relations only.") else
       name decl++" :: "++name (source decl)++(if null ([Uni,Tot]>-multiplicities decl) then " -> " else " * ")++name (target decl)++
       (let mults=if null ([Uni,Tot]>-multiplicities decl) then multiplicities decl>-[Uni,Tot] else multiplicities decl in
        if null mults then "" else showL(map showADL mults))++
       (if null(decprL decl++decprM decl++decprR decl) then "" else " PRAGMA "++intercalate " " (map show [decprL decl,decprM decl,decprR decl]))
 -- obsolete 18 July 2010       ++ (if null (decexpl decl) then "" else " EXPLANATION \""++decexpl decl++"\"")
       ++"."
        where
          showL   :: [String] -> String
          showL xs = "["++intercalate "," xs++"]"
    showADL (Isn g s)
     = "I["++show (name g)++(if g==s then "" else "*"++show (name s))++"]"
    showADL (Iscompl g s)
     = "-I["++show (name g)++(if g==s then "" else "*"++show (name s))++"]"
    showADL (Vs g s)
     = "V["++show (name g)++(if g==s then "" else "*"++show (name s))++"]"

   instance ShowADL Prop where
    showADL p = show p

   instance ShowADL (Population Concept) where
    showADL (Popu m ps)
     = "POPULATION "++showADL m++" CONTAINS\n"++
       indent++"[ "++intercalate ("\n"++indent++"; ") (map show ps)++indent++"]"
       where indent = "   "
    showADLcode fSpec (Popu m ps)
     = "POPULATION "++showADLcode fSpec m++" CONTAINS\n"++
       indent++"[ "++intercalate ("\n"++indent++"; ") (map show ps)++indent++"]"
       where indent = "   "

   instance ShowADL Concept where
    showADL c = show (name c)

   instance ShowADL ConceptDef where
    showADL cd
     = "\n  CONCEPT "++show (name cd)++" "++show (cddef cd)++" "++(if null (cdref cd) then "" else show (cdref cd))

   instance ShowADL Architecture where
    showADL arch = intercalate "\n\n" (map showADL (archContexts arch))

   -- In de body van de context worden de regels afgedrukt die in de context zijn gedefinieerd, maar buiten de patterns.
   -- Daarbij worden de relaties afgedrukt die bij deze regels horen, zodat het geheel zelfstandig leesbaar is.
   instance ShowADL Context where
    showADL _ = error("!Fatal (module showADL 390): showADL on Contexts is deliberately undefined. Please use showADLcode fSpec instead.")
    showADLcode fSpec context
     = "CONTEXT " ++name context
       ++ (if null (ctxon context)   then "" else "EXTENDS "++intercalate ", "   (ctxon context)                 ++ "\n")
       ++ (if null (ctxros context)  then "" else "\n"      ++intercalate "\n\n" (map (showADLcode fSpec) (ctxros context))  ++ "\n") -- ^All role service assignments
       ++ (if null (ctxmed context)  then "" else "\n"      ++intercalate "\n\n" (map (showADLcode fSpec) (ctxmed context))  ++ "\n") -- ^If a role-relation combination occurs here, this role may edit that relation.
       ++ (if null (ctxsvcs context) then "" else "\n"      ++intercalate "\n\n" (map (showADLcode fSpec) (ctxsvcs context)) ++ "\n")
       ++ (if null (ctxcs context)   then "" else "\n"      ++intercalate "\n"   (map (showADLcode fSpec) (ctxcs context))   ++ "\n")
       ++ (if null (ctxds context)   then "" else "\n"      ++intercalate "\n"   (map (showADLcode fSpec) (ctxds context))   ++ "\n")
       ++ (if null (ctxks context)   then "" else "\n"      ++intercalate "\n"   (map (showADLcode fSpec) (ctxks context))   ++ "\n")
       ++ (if null (ctxpats context) then "" else "\n"      ++intercalate "\n\n" (map (showADLcode fSpec) (ctxpats context)) ++ "\n")
       ++ (if null (ctxpops context) then "" else "\n"      ++intercalate "\n\n" (map (showADLcode fSpec) (ctxpops context)) ++ "\n")
       ++ "\n\nENDCONTEXT"


-- WHY?  Stef, what is the added value of ShowADL Context now we have ShowADL Fspc ?
-- BECAUSE (SJ) for debugging purposes, it might be useful to be able to print a Context in .Ampersand syntax...
   instance ShowADL Fspc where
    showADL fSpec = showADLcode fSpec fSpec
    showADLcode fSpec' fSpec
     = "CONTEXT " ++name fSpec
       ++ (if null (objDefs fSpec)     then "" else "\n"++intercalate "\n\n" (map (showADLcode fSpec') (objDefs fSpec))     ++ "\n")
       ++ (if null (patterns fSpec)    then "" else "\n"++intercalate "\n\n" (map (showADLcode fSpec') (patterns fSpec))    ++ "\n")
       ++ (if null (conceptDefs fSpec) then "" else "\n"++intercalate "\n"   (map (showADLcode fSpec') (conceptDefs fSpec)) ++ "\n")
       ++ (if null (vgens fSpec)       then "" else "\n"++intercalate "\n"   (map (showADLcode fSpec') (vgens fSpec))       ++ "\n")
       ++ (if null (vkeys fSpec)       then "" else "\n"++intercalate "\n"   (map (showADLcode fSpec') (vkeys fSpec))       ++ "\n")
       ++ (if null ds                  then "" else "\n"++intercalate "\n"   (map (showADLcode fSpec') ds)                  ++ "\n")
       ++ (if null showADLpops         then "" else "\n"++intercalate "\n\n" showADLpops                                    ++ "\n")
       ++ "\n\nENDCONTEXT"
       where showADLpops = [ showADLcode fSpec' (Popu{popm=makeMph d, popps=decpopu d})
                           | d<-declarations fSpec, not (null (decpopu d))]
             ds = [d| d@Sgn{}<-declarations fSpec, decusr d]
{-
   instance SelfExplained ECArule where   --TODO: Wat doet deze definitie in ShowADL??? (Omdat ShowADL er in wordt gebruikt....)
     autoExplains flags r
      = [string2AutoExplain (defaultFlags {language = English}) 
          ( case p of
             Chc {} -> "Pick from "++show (length (paCls p))++" options, in order to maintain "++shMotivEng (paMotiv p)++"."
             All {} -> "Execute "++show (length (paCls p))++" ECA-rules, in order to maintain "++shMotivEng (paMotiv p)++"."
             Do  {paSrt=Ins} -> "Insert tuple(s) in "++showADL (paTo p)++" to maintain "++shMotivEng (paMotiv p)++"."
             Do  {paSrt=Del} -> "Remove tuple(s) from "++showADL (paTo p)++" to maintain "++shMotivEng (paMotiv p)++"."
             Sel {} -> "Select an element from "++showADL (paTo p)++" to maintain "++shMotivEng (paMotiv p)++"."
             New {} -> "Create a new element in "++showADL (paTo p)++" to maintain "++shMotivEng (paMotiv p)++"."
             Rmv {} -> "Remove an element from "++showADL (paTo p)++" to maintain "++shMotivEng (paMotiv p)++"."
             Nop {} -> "Do nothing to maintain "++shMotivEng (paMotiv p)++", because it is still valid."
             Blk {} -> "Abort to prevent violation of "++shMotivEng (paMotiv p)
           )] ++
         [string2AutoExplain (defaultFlags {language = Dutch})
           ( case p of
             Chc {} -> "Kies uit "++show (length (paCls p))++" opties, om te garanderen dat "++shMotivDut (paMotiv p)++"."
             All {} -> "Voer "++show (length (paCls p))++" ECA-rules uit, om te garanderen dat "++shMotivDut (paMotiv p)++"."
             Do  {paSrt=Ins} -> "Voer tweetal(len) in in "++showADL (paTo p)++" om te garanderen dat "++shMotivDut (paMotiv p)++"."
             Do  {paSrt=Del} -> "Verwijder tweetal(len) uit "++showADL (paTo p)++" om te garanderen dat "++shMotivDut (paMotiv p)++"."
             Sel {} -> "Selecteer een element uit "++showADL (paTo p)++" om te garanderen dat "++shMotivDut (paMotiv p)++"."
             New {} -> "Maak een nieuw element aan in "++showADL (paTo p)++" om te garanderen dat "++shMotivDut (paMotiv p)++"."
             Rmv {} -> "Verwijder een element uit "++showADL (paTo p)++" om te garanderen dat "++shMotivDut (paMotiv p)++"."
             Nop {} -> "Doe niets om te garanderen dat "++shMotivDut (paMotiv p)++", want het geldt nog steeds."
             Blk {} -> "Breek af, om een overtreding te voorkomen van "++shMotivDut (paMotiv p)
            )]
      where
       p = ecaAction r
       shMotivEng ms = commaEng "and" [ showADL conj++" FROM "++intercalate "," ["R"++show (nr r')| r'<-rs]++")"| (conj,rs)<-ms]
       shMotivDut ms = shMotivEng ms  -- TODO: Nog even de nederlandse versie organiseren... 
-}
 
   instance ShowADL UnOp where
    showADL K0 = "*"
    showADL K1 = "+"
    showADL Cp = "-"
    showADL Co = "~"

   instance ShowADL MulOp where
    showADL Fc = ";"
    showADL Fd = "!"
    showADL Fi = "/\\"
    showADL Fu = "\\/"
    showADL Ri = "|-"
    showADL Re = "="


