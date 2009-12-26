  -- | DAAROM (HJ) Wat is precies het doel van Show vs ShowADL ??
  -- ANTWOORD (SJ): De standaard-show is alleen bedoeld voor simpele foutmeldingen tijdens het testen.
  --                showADL is bedoeld om ADL source code te genereren.
  --                showADL is contextonafhankelijk, en produceert syntactisch correcte ADL-code, die echter wel typefouten zou kunnen bevatten,
  --                namelijk dubbelzinnigheden die met een expliciet type opgelost hadden kunnen worden.
  --                showADLcode maakt gebruik van ontologische informatie in Fspc, namelijk vRels en isa, om in dit soort gevallen het type
  --                expliciet te maken.
  --                Daarmee produceert showADLcode volledig correcte ADL-code, dus typecorrect en zonder service-warnings.
{-# OPTIONS_GHC -XFlexibleInstances #-}
  module ShowADL ( ShowADL(..), PrintADL(..), disambiguate, mphatsoff)
  where
   import Char                            (isAlphaNum,isUpper)
   import CommonClasses
   import Collection                      (Collection(..))
   import Adl
   
   import FspecDef 
   import Strings                         (chain)
   import Auxiliaries                     (eqCl,showL)
  
   import TypeInferenceEngine
   import TypeInference.ITree
 
   class ShowADL a where
    showADL :: a -> String
    showADLcode :: Fspc -> a -> String
    showADLcode fSpec x = showADL x

{--------------------------------------------  
--TODO -> merge showADL and printADL 
   class (PrintADL a) =>  ShowADL a where
    showADL :: a -> String
    showADL x = printadl Nothing 0 x
    showADLcode ::  Fspc -> a -> String
    showADLcode fSpec x = printadl (Just fSpec) 0 x
   instance ShowADL ObjectDef 
   instance ShowADL Pattern
   instance ShowADL Rule
   instance ShowADL Gen
   instance ShowADL KeyDef
   instance ShowADL Expression
   instance ShowADL Morphism
   instance ShowADL Declaration
   instance ShowADL Prop
   instance ShowADL Population
   instance ShowADL Concept
   instance ShowADL ConceptDef
--------------------------------------------}
   class PrintADL a where
    --DESCR -> FSpec, Object to print, Indentation level
    --Fspc=Nothing is allowed to be nothing before makeFspec
    printadl :: Maybe Fspc -> Int -> a -> String

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
   adlprintlist :: (PrintADL a) => Maybe Fspc -> Int -> (String,String,String) -> [a]  -> String
   adlprintlist fSpec i opts xs = printlist opts [printadl fSpec i x|x<-xs, (not.null) $ printadl fSpec i x]
   adlprintlistlb :: (PrintADL a) => Maybe Fspc -> Int -> [a]  -> String
   adlprintlistlb fSpec i xs = adlprintlist fSpec i ("", lb, lb) xs
   --DESCR -> put string in quotes if it contains strange characters (like spaces)
   printquotes ss | and [isAlphaNum c| c<-ss] = ss
                  | otherwise = "\""++ss++"\""

--------------------------------------------------------------
   --EXTEND -> printadl must be the inverse of parse. Concrete: Haskell code generated from the original file must be literally equivalent to Haskell code generated from the printadl string.
   --TODO -> check equivalence of generated Haskell code 
   --REMARK -> comments in original script will not be printed
   --TODO -> what about extends? Answer: ignore untill revised
   --TODO -> Pat "CONTEXT" should become obsolete (declare ds cs ks in a pattern)
   --WHY -> aren't ONE Anything NOthing etc reserved words on pString, pConid, (etc?)? Answer: check if errors can be produced without reserved words. If so add reserved words, otherwise don't
   --TODO -> sort on file position
   --TODO -> where do all the Other Topics patterns come from, it's not just one pattern? Answer: they will disappear when revising FTheme
   --TODO -> RULE cannot be used in combination with -p -l or -s and maybe more, because something tries to retrieve the rrant, which is an error.
   --TODO -> ALWAYS pProps (ObjectDef) is ignored. It may be enabled some day to communicate interface policies
   --TODO -> move the flips from Morphism to Expression data type
   --TODO -> remove application of double complement rule from the parser
   --TODO -> remove removal of brackets on ; expression from the parser

   -- pops = [Popu mph prs| CPop mph prs<-ces]
   -- CPop ->  pKey "POPULATION" <*> pMorphism <* pKey "CONTAINS" <*> pContent
   instance PrintADL Fspc where
    printadl _ _ fSpec = 
      let 
      i = 0
      popmph d = Mph{mphnm=decnm d, mphpos=Nowhere,mphats=popats,mphtyp=(desrc d,detgt d),mphyin=True,mphdcl=d} 
         where popats = if (desrc d)/=Anything && (detgt d)/=Anything then [desrc d,detgt d] else []
      in
      "CONTEXT " ++ name fSpec ++ lb
         --REMARK -> Pattern "CONTEXT" will be printed as a pattern --> no ds cs ks outside the pattern only pops and objs
      ++ printadl (Just fSpec) i (patterns fSpec)
      ++ printadl (Just fSpec) i (serviceS fSpec)
      ++ printlist ("",lb,lb) [printadl (Just fSpec) i (Popu{popm=popmph d,popps=decpopu d})
                              | d<-vrels fSpec, (not.null)(decpopu d)]
      ++ "ENDCONTEXT"

   instance PrintADL Population where
    printadl fSpec i pop = "POPULATION " ++ printadl fSpec 0 (popm pop) ++ " CONTAINS"
                                ++ printlist (lb++indent (i+1)++"[ "
                                             ,";"++lb++indent (i+1)++"  "
                                             ,lb++indent (i+1)++"]"++lb) 
                                             [printlist ("",", ","") [show rec]| rec<-popps pop]

   instance PrintADL ObjectDef where
    printadl fSpec i obj = 
      (if i==0 then "SERVICE " else "")
       ++ objnm obj 
       ++ printlist (" {",", ","}") [printlist (""," ","") (map printquotes strs) | strs<-objstrs obj]
       ++ " : " 
       ++ printadl fSpec i (objctx obj)
       ++ printadl fSpec (i+1) (objats obj)
       ++ (if i==0 then lb else "")

   instance PrintADL [ObjectDef] where
    printadl fSpec i objs = 
       if i==0 then [c|obj<-objs, c<-((printadl fSpec i obj)++lb)] 
       else adlprintlist fSpec (i+1) (lb++indent i ++ "= [ "
                                     ,lb++indent i++"  ,"++" "
                                     ,"]") objs 
   
   --REMARK -> show Morphism does not print mphats, so I need my own show Expression and show Morphism
   --REMARK -> postfix complements are printed prefix
   instance PrintADL Expression where
    printadl fSpec i expr' = printexpr ((nonambigExpr fSpec.insParentheses) expr') --REMARK -> insert minimal brackets
      where      
      --REMARK -> cannot recursively use printadl, because insParentheses also removes redundant brackets 
      printexpr expr =  
        case expr of
          Tm m  -> printadl fSpec i m
          Fu fs -> printlist ("","\\/","") [printexpr x|x<-fs]
          Fi fs -> printlist ("","/\\","") [printexpr x|x<-fs]
          F  ts -> printlist ("",";","") [printexpr x|x<-ts]
          Fd ts -> printlist ("","!","") [printexpr x|x<-ts]
          Cp e  -> "-" ++ printexpr e
          Tc e  -> "(" ++ printexpr e ++ ")"
          K0 e  -> printexpr e ++ "*"
          K1 e  -> printexpr e ++ "+"

   --REMARK -> if you want mphats to be printed then fill the mphats
   --EXTEND -> print what has been written
   --REMARK -> show Morphism does not print mphats
   instance PrintADL Morphism where
    printadl fSpec i mph = case mph of
       Mph{mphats=[c]} -> name mph ++ showSign[c,c] ++ if inline mph then "" else "~"
       Mph{mphats=[c1,c2]} -> name mph ++ showSign[c1,c2] ++ if inline mph then "" else "~"
       Mph{} -> name mph ++ if inline mph then "" else "~"
       I{mphats=[c]} -> "I" ++ showSign[c] 
       I{} -> "I" 
       V{mphats=[c1,c2]} -> "V" ++ showSign[c1,c2] 
       V{} -> "V"
       Mp1{} -> name mph

   instance PrintADL Pattern where
    printadl fSpec i p = if null patelems then "" else
      "PATTERN " ++ (if (ptnm p)=="CONTEXT" then "AdlContext" else ptnm p) ++ lb
      ++ patelems
      ++ "ENDPATTERN" ++ lb
      where patelems = printadl fSpec i (ptrls p)
                    ++ printadl fSpec i (ptgns p)
                    ++ printadl fSpec i (ptdcs p)
                    ++ printadl fSpec i (ptcds p)
                    ++ printadl fSpec i (ptkds p)

   instance PrintADL [Pattern] where
    printadl fSpec i ps = adlprintlistlb fSpec i ps

   nonambigRule :: Maybe Fspc -> Rule -> Rule
   nonambigRule fSpec' rule = case fSpec' of
      Nothing -> rule
      Just fSpec -> case infertype fSpec (Reor rule) of
           (_,NoProof (AmbiguousType _ _)) -> if (rrsrt rule)==Truth 
                                              then rule{rrcon=mphatson (rrcon rule)}
                                              else rule{rrant=mphatson (rrant rule), rrcon=mphatson (rrcon rule)}
           _ -> rule
   nonambigExpr :: Maybe Fspc -> Expression -> Expression
   nonambigExpr fSpec' expr =  case fSpec' of
      Nothing -> expr
      Just fSpec ->  case infertype fSpec (Eeor expr) of
           (_,NoProof (AmbiguousType _ _)) -> mphatson expr
           _ -> expr
   mapExpr :: (Morphism->Morphism) -> Expression -> Expression
   mapExpr f expr = case expr of
      F xs  -> F  [mapExpr f x| x<-xs]
      Fd xs -> Fd [mapExpr f x| x<-xs]
      Fu xs -> Fu [mapExpr f x| x<-xs]
      Fi xs -> Fi [mapExpr f x| x<-xs]
      Tm mp -> Tm (f mp)
      Tc x  -> Tc $ mapExpr f x
      Cp x  -> Cp $ mapExpr f x
      K0 x  -> K0 $ mapExpr f x
      K1 x  -> K1 $ mapExpr f x

   mphatson :: Expression -> Expression
   mphatson = mapExpr f
    where f mp = case mp of
                 Mph{mphats=[], mphtyp=(c1,c2)}->if mphyin mp then mp{mphats=[c1,c2]} else  mp{mphats=[c2,c1]}
                 _ -> mp

   mphatsoff :: Expression -> Expression
   mphatsoff = mapExpr f
    where f mp = case mp of
                 Mph{} -> mp{mphats=[]}
                 _     -> mp

   instance PrintADL Rule where
    printadl fSpec i r' = 
      let r=nonambigRule fSpec r'
      in
      case r of
       -- pSignal -> ( pKey "SIGNAL" *> pMorphism <* pKey "ON" ) `opt` 
       --            (Mph "" Nowhere [] (cptAnything,cptAnything) True 
       --                    (Sgn "" cptAnything cptAnything [] "" "" "" [] "" Nowhere 0 False))
       -- pMorphism -> Mph nm ... (Sgn nm cptAnything cptAnything [] "" "" "" [] "" Nowhere 0 (nm/=""))
       --Declaration srrel only has a name of the parsed morphism. 
       --So name on Morphism must be the inverse of pMorphism => the inverse of name on Declaration.
       --reason:  name m = name (makeDeclaration m) 
       --  and  
       --         name (Sgn nm _ _ _ _ _ _ _ _ _ _ _) = nm
       --         name (Isn _ _)                      = "I"
       --         name (Iscompl _ _)                  = "-I"
       --         name (Vs _ _)                       = "V"
       --This is not the inverse, but it is a bit what you want because it throws away flips and mphats and complements etc which we do not want in signal names, only I -I and V will return which we actually do not want, but probably nobody ever makes an attempt, thus...
       --WHY -> is a signal name parsed as a Morphism?
      --REMARK -> the rule in Sg is equivalent to the rule without SIGNAL .. ON prefix
      Sg{} -> "SIGNAL" ++ name (srrel r) ++ "ON" ++ printadl fSpec i (srsig r)
      Ru{rrsrt=rt} -> let
                      str1 = if rt==Truth then "RULE " 
                             else printadl fSpec i (rrant r) 
                                  ++ if rt==Implication then " |- " else " = "
                      str2 = printadl fSpec i (rrcon r)
                      str3 = if null (rrxpl r) then "" 
                             else lb ++ "EXPLANATION \"" ++ (rrxpl r) ++ "\""
                      in str1 ++ str2 ++ str3

   instance PrintADL [Rule] where
    printadl fSpec i rs = adlprintlistlb fSpec i rs

   instance PrintADL Gen where
    printadl fSpec i g = "GEN " ++ cnm (genspc g) ++ " ISA " ++ cnm (gengen g) 
      where cnm c = case c of 
                      C{} -> cptnm c
                      _ -> error $ "!Fatal (module ShowADL 250): Anything, NOthing and ONE are not allowed on GEN .. ISA .."

   instance PrintADL [Gen] where
    printadl fSpec i gs = adlprintlistlb fSpec i gs

   instance PrintADL Declaration where
    printadl fSpec i d = 
       decnm d ++ " :: "
       ++ printadl fSpec i (desrc d)
       ++ (if isfunc then " -> " else " * ")
       ++ printadl fSpec i  (detgt d)
       ++ printadl fSpec i printprops
       ++ " PRAGMA " ++ printlist ("\"","\" \"","\"") [decprL d,decprM d, decprR d]
       ++ lb ++ "EXPLANATION  \"" ++ (decexpl d) ++ "\""
       --REMARK -> population printed to POPULATION
       ++ "."
       where isfunc = elem Uni (decprps d) && elem Tot (decprps d)
             printprops = let rmfprps x = x==Uni||x==Tot
                          in if isfunc then filter rmfprps (decprps d) else (decprps d)
             

   instance PrintADL [Declaration] where
    printadl fSpec i ds = adlprintlistlb fSpec i ds

   --REMARK -> instance Show Prop implements inverse
   instance PrintADL Prop where
    printadl _ _ p = show p

   instance PrintADL [Prop] where
    printadl fSpec i ps = adlprintlist fSpec i (" [",", ","]") ps

   --REMARK -> name implements inverse (except for Anything and NOthing)
   instance PrintADL Concept where
    printadl _ _ c = name c 
 
   --REMARK -> empty cdrefs are printed even when not written in original script
   instance PrintADL ConceptDef where
    printadl _ _ cd = "CONCEPT " ++ printlist ("\"","\" \"", "\"") [cdnm cd, cddef cd, cdref cd]

   instance PrintADL [ConceptDef] where
    printadl fSpec i cds = adlprintlistlb fSpec i cds 

   --REMARK -> only the name of the main label is used
   instance PrintADL KeyDef where
    printadl fSpec i kd = 
       "KEY " ++ kdlbl kd ++ ": " 
       ++ printadl fSpec i (kdctx kd)
       ++ printlist (lb++indent (i+2)++"[", ","++lb++indent (i+2)++" ","]") [printkdat kdat|kdat<-kdats kd]
       where 
       printkdat obj =
          objnm obj 
          ++ printlist (" {",", ","}") [printlist (""," ","") (map printquotes strs) | strs<-objstrs obj]
          ++ " : " 
          ++ printadl fSpec i (objctx obj)

   instance PrintADL [KeyDef] where
    printadl i kds = adlprintlistlb i kds
--------------------------------------------

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
    showADL r@(Ru{rrsrt=c,rrant=antc,rrcon=cons})
     | c==Truth = "ALWAYS "++showADL cons
     | c==Implication = showADL antc ++" |- "++showADL cons
     | c==Equivalence = showADL antc ++" = " ++showADL cons
    showADLcode fSpec r@(Sg p rule expla sgn nr pn signal) = "SIGNAL "++name signal++" ON "++ showADLcode fSpec rule
    showADLcode fSpec r@(Ru{rrsrt=c,rrant=antc,rrcon=cons})
     | c==Truth = "ALWAYS "++showADLcode fSpec cons
     | c==Implication = showADLcode fSpec antc ++" |- "++showADLcode fSpec cons
     | c==Equivalence = showADLcode fSpec antc ++" = " ++showADLcode fSpec cons

   instance ShowADL Gen where
    showADL (G pos g s _) = "GEN "++showADL s++" ISA "++show g
    showADLcode fSpec (G pos g s _) = "GEN "++showADLcode fSpec s++" ISA "++show g


   instance ShowADL KeyDef where
    showADL kd 
     = "KEY "++kdlbl kd
             ++":"++"name (target("++showADL (kdctx kd)++")"
             ++"("++chain "," (map showADL (kdats kd))++")"
    showADLcode fSpec kd 
     = "KEY "++kdlbl kd
             ++":"++"name (target("++showADLcode fSpec (kdctx kd)++")"
             ++"("++chain "," (map (showADLcode fSpec) (kdats kd))++")"

-- disambiguate :: Fspc -> Expression -> Expression
-- This function must ensure that an expression, when printed, can be parsed with no ambiguity.
-- Besides, it must be readable as well.
   disambiguate :: Fspc -> Expression -> Expression
   disambiguate fSpec (Tm mph) = Tm mph
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
   disamb :: Fspc -> [Expression] -> [Expression]
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
        strands1 :: [(Expression,[Concept],Expression)] -> [[Expression]]
        strands1 []  = []
        strands1 iss = p1 (takeWhile select iss): strandsn (dropWhile select iss)
                       where select (s,m,t) = length m<=1
        strandsn []  = []
        strandsn iss = pn (takeWhile select iss): strands1 (dropWhile select iss)
                       where select (s,m,t) = length m>1
        p1 iss = [s| (s,m,t)<-iss]
        pn [] = error("!Fatal (module ShowADL 441): calling pn with empty list")
        pn [(s,m,t)] = [s,Tm (mIs (target s `lub` source t))]
        pn iss = [s|(s,m,t)<-lss]++[mphatson s|(s,m,t)<-[head rss]]++[s|(s,m,t)<-tail rss]
                 where lss = take halfway iss
                       rss = drop halfway iss
                       halfway = length iss `div` 2
-- The following function is used to force the type of a relation to be printed.
        types (Tm mph) = if null (mphats mph) then rd [if mphyin mph then [source d,target d] else [target d,source d]|d<-vrels fSpec, name mph==name d] else [mphats mph]
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
    showADLcode fSpec expr  = showExpr ("\\/", "/\\", "!", ";", "*", "+", "-", "(", ")") expr
      where
       showExpr (union,inter,rAdd,rMul,clos0,clos1,compl,lpar,rpar) expr'
        = (showchar.insParentheses.disambiguate fSpec.mphatsoff) expr'
         where
          showchar (Tm mph) = showADLcode fSpec mph
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
    showADL m@(Mp1{})
     = "'"++mph1val m++"'"++(showSign [mph1typ m])
    showADLcode fSpec mph@Mph{}
     = decnm (mphdcl mph)++
       (if null (mphats mph) then "" else showSign (mphats mph))++
       if mphyin mph then "" else "~"
       -- where dss = [(name.head) cl| cl<-eqCl name (vrels fSpec), length cl>1]
    showADLcode fSpec (I atts g s yin)
     = "I"++if null atts then showSign [g,s] else showSign atts++if g==s then "" else if yin then "" else "~"
    showADLcode fSpec (V atts (a,b))
     = "V"++if null atts then showSign [a,b] else showSign atts
    showADLcode fSpec m@(Mp1{})
     = "'"++mph1val m++"'"++(showSign [mph1typ m])

   instance ShowADL Declaration where
    showADL decl@(Sgn nm a b props prL prM prR cs expla _ _ sig _)
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


---------------------------------------
--FUNCTIONS
---------------------------------------

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


