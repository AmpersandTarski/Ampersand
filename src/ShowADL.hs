  -- | DAAROM (HJ) Wat is precies het doel van Show vs ShowADL ??
  -- ANTWOORD (SJ): ShowADL is bedoeld om ADL source code te genereren.
  --                De standaard-show is alleen bedoeld voor simpele foutmeldingen tijdens het testen.
  --                showADL is contextonafhankelijk, en produceert syntactisch correcte ADL-code.
  --                showADLcode maakt gebruik van ontologische informatie in Fspc, namelijk vRels en isa.
  --                Daarmee produceert showADLcode volledig correcte ADL-code,
  --                dus typecorrect en zonder service-warnings.

  module ShowADL ( ShowADL(..) )
  where
   import Char                            (isAlphaNum,isUpper)
   import CommonClasses                   (Identified(..),ABoolAlg(..))
   import Collection                      (Collection(..))
   import Adl.Concept                     (Concept,Morphic(..),Association(..))
   import Adl.ConceptDef                  (ConceptDef(..))
   import Adl.Context                     (Architecture(..),Context(..))
   import Adl.Expression                  (Expression(..))
   import Adl.FilePos                     (FilePos)
   import Adl.Gen                         (Gen(..))
   import Adl.KeyDef                      (KeyDef(..))
 --  import Adl.Label                       ()
   import Adl.MorphismAndDeclaration      (Morphism(..),Declaration(..)
                                          ,isIdent,mIs)  
   import Adl.ObjectDef                   (ObjectDef(..))
 --  import Adl.Pair                        ()
   import Adl.Pattern                     (Pattern(..))
   import Adl.Population                  (Population(..))
   import Adl.Prop                        (Prop)
   import Adl.Rule                        (Rule(..),RuleType(..))
   
   import Data.Fspec(Fspc,vrels)-- TODO FspecDef hoort hier natuurlijk niet!
   import Strings                         (chain)
   import Auxiliaries                     (eqCl,showL)
   
   class ShowADL a where
    showADL :: a -> String
    showADLcode :: Fspc -> a -> String
    showADLcode fSpec x = showADL x

   instance ShowADL Architecture where
    showADL arch = chain "\n\n" (map showADL (archContexts arch))

   instance ShowADL Context where
    showADL context
     = "CONTEXT " ++name context
       ++ (if null (ctxon context)   then "" else "EXTENDS "++chain ", "   (ctxon context)                 ++ "\n")
       ++ (if null (ctxos context)   then "" else "\n"      ++chain "\n\n" (map showADL (ctxos context))   ++ "\n")
       ++ (if null (ctxcs context)   then "" else "\n"      ++chain "\n"   (map showADL (ctxcs context))   ++ "\n")
       ++ (if null (ctxds context)   then "" else "\n"      ++chain "\n"   (map showADL (ctxds context))   ++ "\n")
       ++ (if null (ctxks context)   then "" else "\n"      ++chain "\n"   (map showADL (ctxks context))   ++ "\n")
       ++ (if null (ctxpats context) then "" else "\n"      ++chain "\n\n" (map showADL (ctxpats context)) ++ "\n")
       ++ (if null (ctxpops context) then "" else "\n"      ++chain "\n\n" (map showADL (ctxpops context)) ++ "\n")
       ++ "\n\nENDCONTEXT"
   --    where decls = declarations context>-declarations (ctxpats context)
   --          cdefs = conceptDefs context>-conceptDefs (ctxpats context)

   instance ShowADL Pattern where
    showADL pat
     = "PATTERN " ++ name pat 
       ++ (if null (ptrls pat)  then "" else "\n  " ++chain "\n  " (map showADL (ptrls pat)) ++ "\n")
       ++ (if null (ptgns pat)  then "" else "\n  " ++chain "\n  " (map showADL (ptgns pat)) ++ "\n")
       ++ (if null (ptdcs pat)  then "" else "\n  " ++chain "\n  " (map showADL (ptdcs pat)) ++ "\n")
       ++ (if null (ptcds pat)  then "" else "\n  " ++chain "\n  " (map showADL (ptcds pat)) ++ "\n")
       ++ (if null (ptkds pat)  then "" else "\n  " ++chain "\n  " (map showADL (ptkds pat)) ++ "\n")
       ++ "ENDPATTERN"
    showADLcode fSpec pat
     = "PATTERN " ++ name pat 
       ++ (if null (ptrls pat)  then "" else "\n  " ++chain "\n  " (map (showADLcode fSpec) (ptrls pat)) ++ "\n")
       ++ (if null (ptgns pat)  then "" else "\n  " ++chain "\n  " (map (showADLcode fSpec) (ptgns pat)) ++ "\n")
       ++ (if null (ptdcs pat)  then "" else "\n  " ++chain "\n  " (map (showADLcode fSpec) (ptdcs pat)) ++ "\n")
       ++ (if null (ptcds pat)  then "" else "\n  " ++chain "\n  " (map (showADLcode fSpec) (ptcds pat)) ++ "\n")
       ++ (if null (ptkds pat)  then "" else "\n  " ++chain "\n  " (map (showADLcode fSpec) (ptkds pat)) ++ "\n")
       ++ "ENDPATTERN"


   instance ShowADL Rule where
    showADL r@(Sg p rule expla sgn nr pn signal) = "SIGNAL "++name signal++" ON "++ showADL rule
    showADL r@(Fr d expr _) = showADL d ++ "\n" ++ show (name d)++" = "++showADL expr
    showADL r@(Ru c antc p cons cpu expla sgn nr pn)
     | c==Truth = "ALWAYS "++showADL cons++
                  if null cpu then "" else " COMPUTING " ++ show cpu 
     | c==Implication = showADL antc ++" |- "++showADL cons++
                  if null cpu then "" else " COMPUTING " ++ show cpu 
     | c==Equivalence = showADL antc ++" = " ++showADL cons++
                  if null cpu then "" else " COMPUTING " ++ show cpu 
    showADL r@(Gc _ antc cons cpu _ _ _)
              = "GLUE "++showADL antc++" = "++showADL cons++
                  if null cpu then "" else " COMPUTING " ++ show cpu
    showADLcode fSpec r@(Sg p rule expla sgn nr pn signal) = "SIGNAL "++name signal++" ON "++ showADLcode fSpec rule
    showADLcode fSpec r@(Fr d expr _) = showADLcode fSpec d ++ "\n" ++ show (name d)++" = "++showADLcode fSpec expr
    showADLcode fSpec r@(Ru c antc p cons cpu expla sgn nr pn)
     | c==Truth = "ALWAYS "++showADLcode fSpec cons++
                  if null cpu then "" else " COMPUTING " ++ show cpu 
     | c==Implication = showADLcode fSpec antc ++" |- "++showADLcode fSpec cons++
                  if null cpu then "" else " COMPUTING " ++ show cpu 
     | c==Equivalence = showADLcode fSpec antc ++" = " ++showADLcode fSpec cons++
                  if null cpu then "" else " COMPUTING " ++ show cpu 
    showADLcode fSpec r@(Gc _ antc cons cpu _ _ _)
              = "GLUE "++showADLcode fSpec antc++" = "++showADLcode fSpec cons++
                  if null cpu then "" else " COMPUTING " ++ show cpu

   instance ShowADL Gen where
    showADL (G pos g s) = "GEN "++showADL s++" ISA "++show g
    showADLcode fSpec (G pos g s) = "GEN "++showADLcode fSpec s++" ISA "++show g

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
                                     " : "++(if isIdent (objctx o) then "["++str (name (target (objctx o)))++"]" else
                                             if isTrue  (objctx o) then "[ONE*"++str (name (target (objctx o)))++"]" else
                                             showADL (objctx o))++
                                     if null (objats o) then "" else recur (ind++"     ") (objats o)
                                   | o<-objs ]++
              ind++"   ]"
           str ss | and [isAlphaNum c| c<-ss] = ss
                  | otherwise                 = "\""++ss++"\""
    showADLcode fSpec obj = "  SERVICE "++name obj++" : I["++(name (target (objctx obj)))++"]"++
                  recur "\n  " (objats obj)
     where recur :: String -> [ObjectDef] -> String
           recur ind objs
            = ind++" = [ "++
              chain (ind++"   , ") [ name o++(if name o `elem` cls then show i else "")++
                                     (if null (objstrs o) then "" else " {"++chain ", " [chain " " (map str ss)| ss<-objstrs o]++"}")++
                                     " : "++(if isIdent (objctx o) then "["++str (name (target (objctx o)))++"]" else
                                             if isTrue  (objctx o) then "[ONE*"++str (name (target (objctx o)))++"]" else
                                             showADLcode fSpec (objctx o))++
                                     if null (objats o) then "" else recur (ind++"     ") (objats o)
                                  | (o,i)<-zip objs [1..]
                                  , cls<-[[name c|cl<-eqCl name (vrels fSpec), length cl>1, c<-take 1 cl]]
                                  ]++
              ind++"   ]"
           atts = [ m | a<-objats obj, Tm m<-[objctx a] ]
           str ss | and [isAlphaNum c| c<-ss] = ss
                  | otherwise                 = "\""++ss++"\""

   instance ShowADL KeyDef where
    showADL kd 
    -- Oorspronkelijk: = "KEY "++kdlbl kd++">"++name (target (kdctx kd))++"("++chain "," (map showADL (kdats kd))++")"
     = "KEY "++kdlbl kd
             ++">"++"name (target("++showADL (kdctx kd)++")"
             ++"("++chain "," (map showADL (kdats kd))++")"
    showADLcode fSpec kd 
    -- Oorspronkelijk: = "KEY "++kdlbl kd++">"++name (target (kdctx kd))++"("++chain "," (map showADLcode fSpec (kdats kd))++")"
     = "KEY "++kdlbl kd
             ++">"++"name (target("++showADLcode fSpec (kdctx kd)++")"
             ++"("++chain "," (map (showADLcode fSpec) (kdats kd))++")"

   instance ShowADL Expression where
    showADLcode fSpec (F fs) = show (F [ e| (f,i)<-zip fs (is++[[]]), es<-[[f],i], e<-es])
      where is = [ [Tm (mIs (target f `lub` source f'))| length ts>1]
                 | (f,f')<-zip fs (tail fs)
                 , ts<-[[t `lub` s'|(s,t)<-types fSpec f, (s',t')<-types fSpec f', t `order` s']]]
    showADL e = show e


   types fSpec (Tm m)
    = rd [ if mphyin m then (desrc d, detgt d) else (detgt d, desrc d)
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


   instance ShowADL Morphism where
    showADL m@(Mph nm pos atts sgn@(a,b) yin s)
     = ({- if take 5 nm=="Clos_" then drop 5 nm++"*" else -} decnm s)++
       (if null atts
            then (if yin && sgn==(source s, target s) || not yin && sgn==(target s,source s) then "" else showSign [a,b])
            else showSign atts)++
       if yin then "" else "~"
    showADL (I atts g s yin)
     = "I"++if null atts then "" else showSign atts++if g==s then "" else if yin then "" else "~"
    showADL (V atts (a,b))
     = "V"++if null atts then "" else showSign atts
    showADL (Mp1 str sgn)
     = "'"++str++"'"++(showSign [sgn])
    showADLcode fSpec m@(Mph nm pos atts sgn@(a,b) yin s)
     = ({- if take 5 nm=="Clos_" then drop 5 nm++"*" else -} decnm s)++
       (if null atts
            then (if name m `elem` dss then showSign [a,b] else "")
            else showSign atts)++
       if yin then "" else "~"
       where dss = [(name.head) cl| cl<-eqCl name (vrels fSpec), length cl>1]
    showADLcode fSpec (I atts g s yin)
     = "I"++if null atts then "" else showSign atts++if g==s then "" else if yin then "" else "~"
    showADLcode fSpec (V atts (a,b))
     = "V"++if null atts then "" else showSign atts
    showADLcode fSpec (Mp1 str sgn)
     = "'"++str++"'"++(showSign [sgn])

   showSign cs = "["++chain "*" (map name cs)++"]"

   instance ShowADL Declaration where
    showADL decl@(Sgn nm a b props prL prM prR cs expla _ _ sig)
     = if sig then "SIGNAL "++nm++" ON ("++name a++" * "++name b++")" else
       nm++" :: "++name a++" * "++name b++
       (if null props then "" else showL(map showADL props))++
       (if null(prL++prM++prR) then "" else " PRAGMA "++chain " " (map show [prL,prM,prR]))++
       (if null expla then "" else " EXPLANATION \""++expla++"\"")
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
     = nlIndent++"pop_"++name m++name (source m)++name (target m)++nlIndent++" = [ "++chain (nlIndent'++"; ") (map show ps)++nlIndent'++"]"
       where nlIndent = "\n      "; nlIndent' = nlIndent++"    "
             source (Mph nm pos atts (a,b) yin d) = a
             target (Mph nm pos atts (a,b) yin d) = b

   instance ShowADL Concept where
    showADL c = show (name c)

   instance ShowADL ConceptDef where
    showADL cd
     = "\n  CONCEPT "++show (name cd)++" "++show (cddef cd)++" "++(if null (cdref cd) then "" else show (cdref cd))
