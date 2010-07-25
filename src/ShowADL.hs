  -- | DAAROM (HJ) Wat is precies het doel van Show vs ShowADL ??
  -- ANTWOORD (SJ): De standaard-show is alleen bedoeld voor simpele foutmeldingen tijdens het testen.
  --                showADL is bedoeld om ADL source code te genereren.
  --                showADL is contextonafhankelijk, en produceert syntactisch correcte ADL-code, die echter wel typefouten zou kunnen bevatten,
  --                namelijk dubbelzinnigheden die met een expliciet type opgelost hadden kunnen worden.
  --                showADLcode maakt gebruik van ontologische informatie in Fspc, namelijk vRels en isa, om in dit soort gevallen het type
  --                expliciet te maken.
  --                Daarmee produceert showADLcode volledig correcte ADL-code, dus typecorrect en zonder service-warnings.
{-# OPTIONS_GHC -XFlexibleInstances #-}
  module ShowADL ( ShowADL(..), disambiguate, mphatsoff)
  where
   import Char                            (isAlphaNum,isUpper)
   import CommonClasses
   import Collection                      (Collection(..))
   import Adl
   
   import Data.Fspec 
   import Strings                         (chain, commaEng)
   import Auxiliaries                     (eqCl,showL)
  
 --  import TypeInferenceEngine
 --  import TypeInference.ITree
 
   class ShowADL a where
    showADL :: a -> String
    showADLcode :: Fspc -> a -> String
    showADLcode fSpec x = showADL x

{--------------------------------------------  
   class ShowADL a where
    showADL :: a -> String
    showADLcode ::  Fspc -> a -> String
--------------------------------------------}

   lb :: String
   lb = "\n"
   indent :: Int -> String
   indent i = take (3*i) [' '|_<-[1..]]
   printlist :: (String,String,String) -> [String] -> String 
   printlist _ [] = [] --tail will not be on empty list
   printlist (start,delim,end) xs = start ++ (drop (length delim) postfix) ++ end
          where
          postfix :: String
          postfix = [c|x<-xs, c<-(delim++x)]  
   --DESCR -> put string in quotes if it contains strange characters (like spaces)
   printquotes ss | and [isAlphaNum c| c<-ss] = ss
                  | otherwise = "\""++ss++"\""

--------------------------------------------------------------
   --EXTEND -> showADLcode must be the inverse of parse. Concrete: Haskell code generated from the original file must be literally equivalent to Haskell code generated from the showADLcode string.
   --TODO -> check equivalence of generated Haskell code 
   --TODO -> comments in original script must also be printed
   --TODO -> what about extends? Answer: ignore untill revised
   --WHY -> aren't ONE Anything NOthing etc reserved words on pString, pConid, (etc?)? Answer: check if errors can be produced without reserved words. If so add reserved words, otherwise don't
   --TODO -> sort on file position
   --TODO -> ALWAYS cannot be used in combination with -p -l or -s and maybe more, because something tries to retrieve the rrant, which is an error.
   --TODO -> ALWAYS pProps (ObjectDef) is ignored. It may be enabled some day to communicate interface policies
   --TODO -> move the flips from Morphism to Expression data type
   --TODO -> remove application of double complement rule from the parser
   --TODO -> remove removal of brackets on ; expression from the parser


   mapExpr :: (Morphism->Morphism) -> Expression -> Expression
   mapExpr f expr = case expr of
      F xs  -> F  [mapExpr f x| x<-xs]
      Fd xs -> Fd [mapExpr f x| x<-xs]
      Fu xs -> Fu [mapExpr f x| x<-xs]
      Fi xs -> Fi [mapExpr f x| x<-xs]
      Tm mp i -> Tm (f mp) i
      Tc x  -> Tc $ mapExpr f x
      Cp x  -> Cp $ mapExpr f x
      K0 x  -> K0 $ mapExpr f x
      K1 x  -> K1 $ mapExpr f x

   mphatson :: Expression -> Expression
   mphatson = mapExpr f
    where f mp = case mp of
                 Mph{mphats=[], mphtyp=(c1,c2)}->if inline mp then mp{mphats=[c1,c2]} else  mp{mphats=[c2,c1]}
                 _ -> mp

   mphatsoff :: Expression -> Expression
   mphatsoff = mapExpr f
    where f mp = case mp of
                 Mph{} -> mp{mphats=[]}
                 _     -> mp


   instance ShowADL ObjectDef where
   -- WAAROM (HJ)? In deze instance van ShowADL worden diverse zaken gebruikt die ik hier niet zou verwachten.
   --              Het vertroebelt de code ook een beetje, want nu moeten er dingen als 'inline', 'source' en
   --              'target' hier al bekend zijn.
   --              Dat lijkt me hier nog niet op z'n plaats, als je alleen maar wat wilt kunnen 'prettyprinten'. 
   -- ANTWOORD (SJ): Dit blijft nog even zo, omdat showADL gebruikt wordt in het genereren van services.
   --              Zolang we dat nog niet onder de knie hebben blijft de code wat troebel.
    showADL obj = "  SERVICE "++name obj++" : I["++(name (target (objctx obj)))++"]"++
                  recur "\n  " (objats obj)
     where recur :: String -> [ObjectDef] -> String
           recur ind objs
            = ind++" = [ "++
              chain (ind++"   , ") [ name o++
                                     (if null (objstrs o) then "" else " {"++chain ", " [chain " " (map str ss)| ss<-objstrs o]++"}")++
                                     " : "++(if isIdent (objctx o) then showSign[target (objctx o)] else
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
              chain (ind++"   , ") [ name o++(if name o `elem` cls then show i else "")++
                                     (if null (objstrs o) then "" else " {"++chain ", " [chain " " (map str ss)| ss<-objstrs o]++"}")++
                                     " : "++showADLcode fSpec (objctx o)++
                                     if null (objats o) then "" else recur (ind++"     ") (objats o)
                                  | (o,i)<-zip objs [1..]
                                  , cls<-[[name c|cl<-eqCl name (vrels fSpec), length cl>1, c<-take 1 cl]]
                                  ]++
              ind++"   ]"
           atts = [ m | a<-objats obj, Tm m _<-[objctx a] ]
           str ss | and [isAlphaNum c| c<-ss] = ss
                  | otherwise                 = "\""++ss++"\""

   instance ShowADL Explanation where
    showADL (ExplConcept     cdef  lang ref expla) = "EXPLAIN CONCEPT "   ++name cdef++" IN "++show lang++(if null ref then "" else " REF "++ref)++(if '\n' `elem` expla then "\n{+ "++expla++"-}" else " -+ "++expla)
    showADL (ExplDeclaration d     lang ref expla) = "EXPLAIN RELATION "  ++showADL d++" IN "++show lang++(if null ref then "" else " REF "++ref)++(if '\n' `elem` expla then "\n{+ "++expla++"-}" else " -+ "++expla)
    showADL (ExplRule        r     lang ref expla) = "EXPLAIN RULE "      ++name r++   " IN "++show lang++(if null ref then "" else " REF "++ref)++(if '\n' `elem` expla then "\n{+ "++expla++"-}" else " -+ "++expla)
    showADL (ExplKeyDef      k     lang ref expla) = "EXPLAIN KEY "       ++name k++   " IN "++show lang++(if null ref then "" else " REF "++ref)++(if '\n' `elem` expla then "\n{+ "++expla++"-}" else " -+ "++expla)
    showADL (ExplObjectDef   o     lang ref expla) = "EXPLAIN SERVICE "   ++name o++   " IN "++show lang++(if null ref then "" else " REF "++ref)++(if '\n' `elem` expla then "\n{+ "++expla++"-}" else " -+ "++expla)
    showADL (ExplPattern     pname lang ref expla) = "EXPLAIN PATTERN "   ++pname++    " IN "++show lang++(if null ref then "" else " REF "++ref)++(if '\n' `elem` expla then "\n{+ "++expla++"-}" else " -+ "++expla)
    showADLcode fSpec (ExplConcept     cdef  lang ref expla) = "EXPLAIN CONCEPT "   ++name cdef++  " IN "++show lang++(if null ref then "" else " REF "++ref)++(if '\n' `elem` expla then "\n{+ "++expla++"-}" else " -+ "++expla)
    showADLcode fSpec (ExplDeclaration d     lang ref expla) = "EXPLAIN RELATION "  ++showADLcode fSpec d++" IN "++show lang++(if null ref then "" else " REF "++ref)++(if '\n' `elem` expla then "\n{+ "++expla++"-}" else " -+ "++expla)
    showADLcode fSpec (ExplRule        r     lang ref expla) = "EXPLAIN RULE "      ++name r++     " IN "++show lang++(if null ref then "" else " REF "++ref)++(if '\n' `elem` expla then "\n{+ "++expla++"-}" else " -+ "++expla)
    showADLcode fSpec (ExplKeyDef      k     lang ref expla) = "EXPLAIN KEY "       ++name k++     " IN "++show lang++(if null ref then "" else " REF "++ref)++(if '\n' `elem` expla then "\n{+ "++expla++"-}" else " -+ "++expla)
    showADLcode fSpec (ExplObjectDef   o     lang ref expla) = "EXPLAIN SERVICE "   ++name o++     " IN "++show lang++(if null ref then "" else " REF "++ref)++(if '\n' `elem` expla then "\n{+ "++expla++"-}" else " -+ "++expla)
    showADLcode fSpec (ExplPattern     pname lang ref expla) = "EXPLAIN PATTERN "   ++pname++      " IN "++show lang++(if null ref then "" else " REF "++ref)++(if '\n' `elem` expla then "\n{+ "++expla++"-}" else " -+ "++expla)

   -- The declarations of the pattern are supplemented by all declarations needed to define the rules.
   -- Thus, the resulting pattern is self-contained with respect to declarations.
   instance ShowADL Pattern where
    showADL pat
     = "PATTERN " ++ name pat 
       ++ (if null (ptrls pat)  then "" else "\n  " ++chain "\n  " (map showADL (ptrls pat)) ++ "\n")
       ++ (if null (ptgns pat)  then "" else "\n  " ++chain "\n  " (map showADL (ptgns pat)) ++ "\n")
       ++ (if null (ptdcs pat)  then "" else "\n  " ++chain "\n  " (map showADL ds         ) ++ "\n")
       ++ (if null (ptcds pat)  then "" else "\n  " ++chain "\n  " (map showADL (ptcds pat)) ++ "\n")
       ++ (if null (ptkds pat)  then "" else "\n  " ++chain "\n  " (map showADL (ptkds pat)) ++ "\n")
       ++ "ENDPATTERN"
       where ds = ptdcs pat++[d| d@Sgn{}<-declarations pat `uni` decls (ptrls pat) `uni` decls (ptkds pat)
                               , decusr d, not (d `elem` ptdcs pat)]
    showADLcode fSpec pat
     = "PATTERN " ++ name pat 
       ++ (if null (ptrls pat)  then "" else "\n  " ++chain "\n  " (map (showADLcode fSpec) (ptrls pat)) ++ "\n")
       ++ (if null (ptgns pat)  then "" else "\n  " ++chain "\n  " (map (showADLcode fSpec) (ptgns pat)) ++ "\n")
       ++ (if null (ptdcs pat)  then "" else "\n  " ++chain "\n  " (map (showADLcode fSpec) ds         ) ++ "\n")
       ++ (if null (ptcds pat)  then "" else "\n  " ++chain "\n  " (map (showADLcode fSpec) (ptcds pat)) ++ "\n")
       ++ (if null (ptkds pat)  then "" else "\n  " ++chain "\n  " (map (showADLcode fSpec) (ptkds pat)) ++ "\n")
       ++ "ENDPATTERN"
       where ds = ptdcs pat++[d| d@Sgn{}<-declarations pat `uni` decls (ptrls pat) `uni` decls (ptkds pat)
                               , decusr d, not (d `elem` ptdcs pat)]


   instance ShowADL Rule where
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

   instance ShowADL Gen where
    showADL (G pos g s _) = "GEN "++showADL s++" ISA "++show g
    showADLcode fSpec (G pos g s _) = "GEN "++showADLcode fSpec s++" ISA "++showADLcode fSpec  g
{-
 KEY pairs: Pair(  SERVICE l : I[Atom]
   = [ 
     ],  SERVICE r : I[Atom]
   = [ 
     ])

 KEY pairs: Pair(l:left,r:right)
  Kd {kdlbl = "pairs", kdctx = I, kdats = [Obj {objnm = "l", objctx = left, objats = [], objstrs = []},Obj {objnm = "r", objctx = right, objats = [], objstrs = []}]}

   pKeyDef           = kd <$ pKey "KEY" <*> pLabel <*> pConcept <* pSpec '(' <*> pList1Sep (pSpec ',') pKeyAtt <* pSpec ')'
                        where kd :: Label -> Concept -> ObjectDefs -> KeyDef 
                              kd (Lbl nm p _) c ats = Kd p nm c ats

   data KeyDef = Kd { kdpos :: FilePos      -- ^ position of this definition in the text of the ADL source file (filename, line number and column number).
                    , kdlbl :: String       -- ^ the name (or label) of this Key. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
                    , kdcpt :: Concept      -- ^ this expression describes the instances of this object, related to their context
                    , kdats :: ObjectDefs   -- ^ the constituent attributes (i.e. name/expression pairs) of this key.
-}
   instance ShowADL KeyDef where
    showADL kd 
     = "KEY "++kdlbl kd
             ++": "++name (kdcpt kd)
             ++"("++chain ", " [(if null (name o) then "" else name o++":") ++ showADL (objctx o)
                               | o<-kdats kd]++")"
    showADLcode fSpec kd 
     = "KEY "++kdlbl kd
             ++": "++name (kdcpt kd)
             ++"("++chain ", " [(if null (name o) then "" else name o++":") ++ showADLcode fSpec (objctx o)
                               | o<-kdats kd]++")"


-- disambiguate :: Fspc -> Expression -> Expression
-- This function must ensure that an expression, when printed, can be parsed with no ambiguity.
-- Besides, it must be readable as well.
   disambiguate :: Fspc -> Expression -> Expression
   disambiguate fSpec (Tm mph i) = Tm mph i
   disambiguate fSpec (Fu fs)  = Fu [disambiguate fSpec f| f<-fs]
   disambiguate fSpec (Fi fs)  = Fi [disambiguate fSpec f| f<-fs]
   disambiguate fSpec (Fd [])  = Fd []
   disambiguate fSpec (Fd [t]) = Fd [disambiguate fSpec t]
   disambiguate fSpec (Fd ts)  = Fd (disamb fSpec [disambiguate fSpec t| t<-ts])
   disambiguate fSpec (F [])   = F  []
   disambiguate fSpec (F [t])  = F  [disambiguate fSpec t]
   disambiguate fSpec (F ts)   = F  (disamb fSpec [disambiguate fSpec t| t<-ts])
   disambiguate fSpec (K0 e')  = K0 (disambiguate fSpec e')
   disambiguate fSpec (K1 e')  = K1 (disambiguate fSpec e')
   disambiguate fSpec (Cp e')  = Cp (disambiguate fSpec e')
   disambiguate fSpec (Tc f)   = Tc (disambiguate fSpec f)

-- Disamb disambiguates a list of terms (expressions) that come from an Fd or F expression.
-- This is done by looking at the concept sets between two adjacent terms.
   disamb :: Fspc -> Expressions -> Expressions
   disamb fSpec ts
     = let ims=strands1 triples in
       if length ims==1 then head ims++[t|(s,m,t)<-[last triples]] else disamb fSpec (concat ims++[t|(s,m,t)<-[last triples]])
       where
-- The list 'triples' contains the concept sets between two adjacent terms.
-- A concept set contains all possibilities for allocating a concept (i.e. type) between two adjacent terms
-- A concept set with more than one concept shows a possible ambiguity,  that 'disamb' will resolve.
        triples
         = [ (t, (rd.map last.types) t `isc` (rd.map head.types) t', t')
           | (t,t')<-zip (init ts) (tail ts)]
-- First we make alternating strands of triples:
--  triples without a problem (i.e. the concept set is not longer than 1); these are dealt with by p1
--  and triples with an ambiguity; these are partially disambiguated by pn.
--  Since pn does its job partially, we have put a fixpoint over the entire function 'disamb', making sure the whole job is done.
        strands1 :: [(Expression,Concepts,Expression)] -> [Expressions]
        strands1 []  = []
        strands1 iss = p1 (takeWhile select iss): strandsn (dropWhile select iss)
                       where select (s,m,t) = length m<=1
        strandsn []  = []
        strandsn iss = pn (takeWhile select iss): strands1 (dropWhile select iss)
                       where select (s,m,t) = length m>1
        p1 iss = [s| (s,m,t)<-iss]
        pn [] = error("!Fatal (module ShowADL 441): calling pn with empty list")
        pn [(s,m,t)] = [s,Tm (mIs (target s `lub` source t)) (-1)]
        pn iss = [s|(s,m,t)<-lss]++[mphatson s|(s,m,t)<-[head rss]]++[s|(s,m,t)<-tail rss]
                 where lss = take halfway iss
                       rss = drop halfway iss
                       halfway = length iss `div` 2
-- The following function is used to force the type of a relation to be printed.
        types (Tm mph _) = if null (mphats mph) then rd [if inline mph then [source d,target d] else [target d,source d]|d<-vrels fSpec, name mph==name d] else [mphats mph]
        types (Fu fs)  = foldr isc [] [types f| f<-fs]
        types (Fi fs)  = foldr isc [] [types f| f<-fs]
        types (Fd ts)  = types (F ts) -- a nifty trick to save code. After all, the type computation is identical to F...
        types (F  [])  = [[Anything,Anything]]
        types (F  ts)  = [[s,t]| s<-(rd.map head.head) ttyps, t<-(rd.map last.last) ttyps]
                         where
                          iscSets 
                           = [(rd.map head.types.head) ts] ++
                             [ (rd.map last.types) t `isc` (rd.map head.types) t
                             | (t,t')<-zip (init ts) (tail ts)] ++
                             [(rd.map last.types.last) ts]
                          ttyps
                           = [ [d| d<-types t, head d `elem` scs, last d `elem` tgs]
                             | ((scs,tgs),t)<-zip (zip (init iscSets) (tail iscSets)) ts
                             ]
        types (K0 e')  = types e'
        types (K1 e')  = types e'
        types (Cp e')  = types e'
        types (Tc f)   = types f

   instance ShowADL Expression where
    showADL e = show e
    showADLcode fSpec expr  = showExpr (" \\/ ", "/\\", "!", ";", "*", "+", "-", "(", ")") expr
      where
       showExpr (union,inter,rAdd,rMul,clos0,clos1,compl,lpar,rpar) expr'
        = (showchar.insParentheses.disambiguate fSpec.mphatsoff) expr'
         where
          showchar (Tm mph _) = showADLcode fSpec mph
          showchar (Fu [])  = "-V"
          showchar (Fu fs)  = chain union [showchar f| f<-fs]
          showchar (Fi [])  = "V"
          showchar (Fi fs)  = chain inter [showchar f| f<-fs]
          showchar (Fd [])  = "-I"
          showchar (Fd ts)  = chain rAdd [showchar t| t<-ts]
          showchar (F [])   = "I"
          showchar (F ts)   = chain rMul [showchar t| t<-ts]
          showchar (K0 e')  = showchar e'++clos0
          showchar (K1 e')  = showchar e'++clos1
          showchar (Cp e')  = compl++showchar e'
          showchar (Tc f)   = lpar++showchar f++rpar

   instance ShowADL Morphism where
    showADL m@Mph{}
     = ({- if take 5 nm=="Clos_" then drop 5 nm++"*" else -} decnm s)++
       (if null (mphats m)
            then (if yin && sgn==(source s, target s) || not yin && sgn==(target s,source s) then "" else showSign [a,b])
            else showSign (mphats m))++
       if yin then "" else "~"
       where s = mphdcl m; yin = mphyin m; sgn@(a,b) = mphtyp m
    showADL (I atts g s yin)
     = "I"++if null atts then "" else showSign atts++if g==s then "" else if yin then "" else "~"
    showADL (V atts (a,b))
     = "V"++if null atts then "" else showSign atts
    showADL m@(Mp1{})
     = "'"++mph1val m++"'"++(showSign [mph1typ m])
    showADLcode fSpec mph@Mph{}
     = decnm (mphdcl mph)++
       (if null (mphats mph) then "" else showSign (mphats mph))++
       if inline mph then "" else "~"
       -- where dss = [(name.head) cl| cl<-eqCl name (vrels fSpec), length cl>1]
    showADLcode fSpec (I atts g s yin)
     = "I"++if null atts then showSign [g,s] else showSign atts++if g==s then "" else if yin then "" else "~"
    showADLcode fSpec (V atts (a,b))
     = "V"++if null atts then showSign [a,b] else showSign atts
    showADLcode fSpec m@(Mp1{})
     = "'"++mph1val m++"'"++(showSign [mph1typ m])

   instance ShowADL Declaration where
    showADL decl@Sgn{}
     = if not (decusr decl) then error("!Fatal (module ShowADL 304): call to ShowADL for declarations can be done on user defined relations only.") else
       name decl++" :: "++name (source decl)++(if null ([Uni,Tot]>-multiplicities decl) then " -> " else " * ")++name (target decl)++
       (let mults=if null ([Uni,Tot]>-multiplicities decl) then multiplicities decl>-[Uni,Tot] else multiplicities decl in
        if null mults then "" else showL(map showADL mults))++
       (if null(decprL decl++decprM decl++decprR decl) then "" else " PRAGMA "++chain " " (map show [decprL decl,decprM decl,decprR decl]))
 -- obsolete 18 July 2010       ++ (if null (decexpl decl) then "" else " EXPLANATION \""++decexpl decl++"\"")
       ++"."
    showADL (Isn g s)
     = "I["++show (name g)++(if g==s then "" else "*"++show (name s))++"]"
    showADL (Iscompl g s)
     = "-I["++show (name g)++(if g==s then "" else "*"++show (name s))++"]"
    showADL (Vs g s)
     = "V["++show (name g)++(if g==s then "" else "*"++show (name s))++"]"

   instance ShowADL Prop where
    showADL p = show p

   instance ShowADL Population where
    showADL (Popu m ps)
     = "POPULATION "++showADL m++" CONTAINS\n"++
       indent++"[ "++chain ("\n"++indent++"; ") (map show ps)++indent++"]"
       where indent = "   "
    showADLcode fSpec (Popu m ps)
     = "POPULATION "++showADLcode fSpec m++" CONTAINS\n"++
       indent++"[ "++chain ("\n"++indent++"; ") (map show ps)++indent++"]"
       where indent = "   "

   instance ShowADL Concept where
    showADL c = show (name c)

   instance ShowADL ConceptDef where
    showADL cd
     = "\n  CONCEPT "++show (name cd)++" "++show (cddef cd)++" "++(if null (cdref cd) then "" else show (cdref cd))

   instance ShowADL Architecture where
    showADL arch = chain "\n\n" (map showADL (archContexts arch))

   -- In de body van de context worden de regels afgedrukt die in de context zijn gedefinieerd, maar buiten de patterns.
   -- Daarbij worden de relaties afgedrukt die bij deze regels horen, zodat het geheel zelfstandig leesbaar is.
   instance ShowADL Context where
    showADL context = error("!Fatal (module showADL 369): showADL on Contexts is deliberately undefined. Please use showADLcode fSpec instead.")
    showADLcode fSpec context
     = "CONTEXT " ++name context
       ++ (if null (ctxon context)   then "" else "EXTENDS "++chain ", "   (ctxon context)                 ++ "\n")
       ++ (if null (ctxos context)   then "" else "\n"      ++chain "\n\n" (map (showADLcode fSpec) (ctxos context))   ++ "\n")
       ++ (if null (ctxcs context)   then "" else "\n"      ++chain "\n"   (map (showADLcode fSpec) (ctxcs context))   ++ "\n")
       ++ (if null ds                then "" else "\n"      ++chain "\n"   (map (showADLcode fSpec) ds             )   ++ "\n")
       ++ (if null (ctxks context)   then "" else "\n"      ++chain "\n"   (map (showADLcode fSpec) (ctxks context))   ++ "\n")
       ++ (if null (ctxpats context) then "" else "\n"      ++chain "\n\n" (map (showADLcode fSpec) (ctxpats context)) ++ "\n")
       ++ (if null (ctxpops context) then "" else "\n"      ++chain "\n\n" (map (showADLcode fSpec) (ctxpops context)) ++ "\n")
       ++ "\n\nENDCONTEXT"
       where ds = [d| d@Sgn{}<-ctxds context `uni` decls (ctxrs context) `uni` decls (mors (ctxks context)), decusr d]


-- WAAROM?  Stef, wat is de toegevoegde waarde van ShowADL Context nu we ShowADL Fspc hebben?
-- DAAROM (SJ) voor debugging is het wel eens handig om een Context in ADL te kunnen afdrukken...
   instance ShowADL Fspc where
    showADL fSpec = showADLcode fSpec fSpec
    showADLcode fSpec' fSpec
     = "CONTEXT " ++name fSpec
       ++ (if null (objDefs fSpec)     then "" else "\n"++chain "\n\n" (map (showADLcode fSpec') (objDefs fSpec))     ++ "\n")
       ++ (if null (patterns fSpec)    then "" else "\n"++chain "\n\n" (map (showADLcode fSpec') (patterns fSpec))    ++ "\n")
       ++ (if null (conceptDefs fSpec) then "" else "\n"++chain "\n"   (map (showADLcode fSpec') (conceptDefs fSpec)) ++ "\n")
       ++ (if null (vgens fSpec)       then "" else "\n"++chain "\n"   (map (showADLcode fSpec') (vgens fSpec))       ++ "\n")
       ++ (if null (vkeys fSpec)       then "" else "\n"++chain "\n"   (map (showADLcode fSpec') (vkeys fSpec))       ++ "\n")
       ++ (if null ds                  then "" else "\n"++chain "\n"   (map (showADLcode fSpec') ds)                  ++ "\n")
       ++ (if null showADLpops         then "" else "\n"++chain "\n\n" showADLpops                                    ++ "\n")
       ++ "\n\nENDCONTEXT"
       where showADLpops = [ showADLcode fSpec' (Popu{popm=makeMph d, popps=decpopu d})
                           | d<-declarations fSpec, not (null (decpopu d))]
             ds = [d| d@Sgn{}<-vrels fSpec, decusr d]

   instance Explained ECArule where
     explain options r
      = case p of
         Chc {} -> "Pick from "++show (length (paCls p))++" options, in order to maintain "++shMotiv (paMotiv p)++"."
         All {} -> "Execute "++show (length (paCls p))++" ECA-rules, in order to maintain "++shMotiv (paMotiv p)++"."
         Do  {paSrt=Ins} -> "Insert tuple(s) in "++showADL (paTo p)++" to maintain "++shMotiv (paMotiv p)++"."
         Do  {paSrt=Del} -> "Remove tuple(s) from "++showADL (paTo p)++" to maintain "++shMotiv (paMotiv p)++"."
         Sel {} -> "Select an element from "++showADL (paTo p)++" to maintain "++shMotiv (paMotiv p)++"."
         New {} -> "Create a new element in "++showADL (paTo p)++" to maintain "++shMotiv (paMotiv p)++"."
         Rmv {} -> "Remove an element from "++showADL (paTo p)++" to maintain "++shMotiv (paMotiv p)++"."
         Nop {} -> "Do nothing to maintain "++shMotiv (paMotiv p)++", because it is still valid."
         Blk {} -> "Abort to prevent violation of "++shMotiv (paMotiv p)
      where
       p = ecaAction r
       shMotiv ms = commaEng "and" [ showADL conj++" FROM "++chain "," ["R"++show (nr r)| r<-rs]++")"| (conj,rs)<-ms]


---------------------------------------
--FUNCTIONS
---------------------------------------

   types fSpec (Tm m _)
    = rd [ if inline m then (desrc d, detrg d) else (detrg d, desrc d)
         | d<-vrels fSpec, name d==name m ]
   types fSpec (F []) = []
   types fSpec (F fs)
    = rd [ (s,t')
         | (s,t)<-types fSpec (head fs), (s',t')<-types fSpec (last fs)
         ]  -- assuming that the expression is type correct (i.e. there exist intermediate types between all terms)
   types fSpec (Fd []) = []
   types fSpec (Fd fs)
    = rd [ (s,t')
         | (s,t)<-types fSpec (head fs), (s',t')<-types fSpec (last fs)
         ]  -- assuming that the expression is type correct (i.e. there exist intermediate types between all terms)
   types fSpec (Fu ts)
    = rd [ (s,t)
         | term<-ts, (s,t)<-types fSpec term
         ]
   types fSpec (Fi ts)
    = rd [ (s,t)
         | term<-ts, (s,t)<-types fSpec term
         ]
   types fSpec (Cp e) = types fSpec e
   types fSpec (K0 e) = types fSpec e
   types fSpec (K1 e) = types fSpec e


