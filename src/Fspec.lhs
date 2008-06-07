>  module Fspec (zed,glossary,functionalSpecText,projectSpecText) where

>  import System
>  -- import System.IO.Unsafe (unsafePerformIO) -- maakt het aanroepen van neato vanuit Haskell mogelijk.
>  import Char
>  import CommonClasses ( Identified(name)
>                        ,Collection (isc,(>-),empty)  )
>  import Auxiliaries
>  import Classification
>  import Typology
>  import CC_aux
>  import Calc
>  import PredLogic
>  import HtmlFilenames
>  import Graphic
>  import ERmodel
>  import ClassDiagram

>  projectSpecText contexts contextname language
>   = putStrLn ("\nGenerating project plan for "++name context)                >>
>     writeFile (name context++".csv") (projectClassic context spec language)  >>
>     putStr ("\nMicrosoft Project file "++name context++".csv written... ")
>     where
>      context  = head ([c| c<-contexts, name c==contextname]++
>                       [Ctx (contextname++" is not defined") [] empty [] [] [] [] []])
>      spec = funcSpec context (erAnalysis context) language
>      (entities, relations, ruls) = erAnalysis context

>  functionalSpecText contexts contextname graphicstyle language
>   = putStrLn ("\nGenerating functional specification for "++name context) >>
>     putStr (funcSpecText context spec language)
>     where
>      context  = head ([c| c<-contexts, name c==contextname]++
>                       [Ctx (contextname++" is not defined") [] empty [] [] [] [] []])
>      spec = funcSpec context (erAnalysis context) language
>      (entities, relations, ruls) = erAnalysis context

>  zed contexts contextname graphicstyle language filename
>   = putStr ("\nGenerating functional specification for context "++name context++" in the current directory.\n") >>
>     graphics context (fnContext context) False context >>            -- generate abbreviated (full==False) class diagram
>     sequence_ [ graphics context (fnPattern context pat) True pat    -- generate fully fledge (full==True) class diagram
>               | pat<-patterns context] >>
>     lprint context language spec filename >>
>     putStr ("\nStatistics of "++name context++"\n") >>
>     putStr ("\n  nr. of classes:   "++show (length ents)++"\n") >>
>     putStr ("\n  nr. of relations: "++show (length rels)++"\n") >>
>     putStr ("\n  nr. of rules:     "++show (length ruls)++"\n") >>
>     putStr ("\n  nr. of services:  "++show (nServices spec)++"\n") >>
>     putStr ("\n  nr. of patterns:  "++show (nPatterns spec)++"\n")
>     where
>      context  = head ([c| c<-contexts, name c==contextname]++
>                       [Ctx (contextname++" is not defined") [] empty [] [] [] [] []])
>      spec = funcSpec context (ents,rels,ruls) language
>      (ents,rels,ruls) = erAnalysis context
>    -- the following is copied from Atlas.lhs. TODO: remove double code.
>      graphics context fnm full b
>       = cdSpecs context fnm full b >>
>         graphSpecs fnm b
>        where
>           cdSpecs context fnm full b
>            = writeFile (fnm++"_CD.dot") (cdDataModel context full "dot" b)      >>
>              putStrLn ("Class diagram "++fnm++"_CD.dot written... ")      >>
>              processCdDataModelFile  (fnm ++"_CD")
>           graphSpecs fnm b
>            = writeFile (fnm++".dot") (dotGraph context graphicstyle fnm b)         >>
>              putStrLn ("Graphics specification "++fnm++".dot written... ") >>
>              processDotgraphFile  fnm

>  glossary contexts contextname language
>   = putStr ("\nGenerating Glossary for "++name context++" in the current directory.") >>
>     lglossary language context
>     where
>      context  = head ([c| c<-contexts, name c==contextname]++
>                       [Ctx (contextname++" is not defined") [] empty [] [] [] [] []])

>  data Fspec = Fspc Pattern [Funit]
>  data Funit = Uspc String Pattern
>                    [(Concept,[(Morphism,[Rule])],FPA,[Morphism],[(Expression,Rule)])]
>                    [ServiceSpec] -- services
>  data ServiceSpec = Sspc String       -- name of the service
>                          FPA          -- function point analysis information
>                          [ParamSpec]  -- parameters
>                          [ParamSpec]  -- results
>                          [Rule]       -- Invariants
>                          [String]     -- Preconditions
>                          [String]     -- Postconditions
>  data ParamSpec   = Pspc String       -- name of the parameter
>                          String       -- type of the parameter
>                   | Pbool

>  class Statistics a where
>   nServices :: a -> Int
>   nPatterns :: a -> Int
>   nPatterns x = 0
>  instance Statistics Fspec where
>   nServices (Fspc p us) = nServices us
>   nPatterns (Fspc p us) = 1
>  instance Statistics a => Statistics [a] where
>   nServices xs = sum (map nServices xs)
>  instance Statistics Funit where
>   nServices (Uspc nm pat ents svs) = length svs

bron van de FPA: www.nesma.nl

>  data FPA = ILGV FPcompl -- bevat permanente, voor de gebruiker relevante gegevens. De gegevens worden door het systeem gebruikt en onderhouden. Onder "onderhouden" verstaat FPA het toevoegen, wijzigen of verwijderen van gegevens.
>           | KGV  FPcompl -- bevat permanente, voor de gebruiker relevante gegevens. Deze gegevens worden door het systeem gebruikt, maar worden door een ander systeem onderhouden (voor dat andere systeem is het dus een ILGV).
>           | IF   FPcompl -- verwerkt gegevens in een ILGV van het systeem. (dus create, update en delete functies)
>           | UF   FPcompl -- presenteert gegevens uit het systeem. Voorbeelden: het afdrukken van alle debiteuren; het aanmaken van facturen; het aanmaken van een diskette met betalingsopdrachten; het medium is hierbij niet van belang: papier, scherm, magneetband, datacom, enzovoorts.
>           | OF   FPcompl -- is een speciaal (eenvoudig) soort uitvoerfunctie. Een opvraagfunctie presenteert gegevens uit het systeem op basis van een uniek identificerend zoekgegeven, waarbij geen aanvullende bewerkingen (zoals berekeningen of het bijwerken van een gegevensverzameling) plaats hebben. Voorbeeld: Het tonen van de gegevens van de klant met klantnummer 123456789.
>           | NO           -- een onderdeel waaraan geen functiepunten worden toegekend.
>             deriving Eq
>  data FPcompl = Eenvoudig | Gemiddeld | Moeilijk deriving Eq

>  instance ShowLang FPcompl where
>   showLang Dutch Eenvoudig   = "Eenvoudig"
>   showLang Dutch Gemiddeld   = "Gemiddeld"
>   showLang Dutch Moeilijk    = "Moeilijk"
>   showLang English Eenvoudig = "Simple"
>   showLang English Gemiddeld = "Average"
>   showLang English Moeilijk  = "Difficult"

>  instance ShowLang FPA where
>   showLang lang (ILGV c) = "ILGV "++showLang lang c
>   showLang lang (KGV  c) = "KGV "++showLang lang c
>   showLang lang (IF   c) = "IF "++showLang lang c
>   showLang lang (UF   c) = "UF "++showLang lang c
>   showLang lang (OF   c) = "OF "++showLang lang c
>   showLang lang NO       = ""

>  fPoints (ILGV Eenvoudig) = 7
>  fPoints (ILGV Gemiddeld) = 10
>  fPoints (ILGV Moeilijk ) = 15
>  fPoints (KGV  Eenvoudig) = 5
>  fPoints (KGV  Gemiddeld) = 7
>  fPoints (KGV  Moeilijk ) = 10
>  fPoints (IF   Eenvoudig) = 3
>  fPoints (IF   Gemiddeld) = 4
>  fPoints (IF   Moeilijk ) = 6
>  fPoints (UF   Eenvoudig) = 4
>  fPoints (UF   Gemiddeld) = 5
>  fPoints (UF   Moeilijk ) = 7
>  fPoints (OF   Eenvoudig) = 3
>  fPoints (OF   Gemiddeld) = 4
>  fPoints (OF   Moeilijk ) = 6
>  fPoints NO               = 0
>  complexity (ILGV c) = c
>  complexity (KGV  c) = c
>  complexity (IF   c) = c
>  complexity (UF   c) = c
>  complexity (OF   c) = c
>  complexity NO       = Eenvoudig

De volgende functie vult een procedure in voor een bepaalde rol.
Aanpak:
1. Alle relaties, die in de regels van deze service genoemd worden, zijn zichtbaar voor deze rol.
2. Een conjunct kan in stand gehouden worden als één van de termen ervan door deze rol geedit kan worden.
3. Alle conjuncts uit de service moeten door deze rol in deze service in stand gehouden worden.
4. Als een conjunct automatisch in stand gehouden wordt, is er geen eis aan de service
5. Als een conjunct handmatig in stand gehouden wordt:
5.a. moet overtreding ervan aan deze rol worden gesignaleerd;
5.b. moet één van de termen van deze conjunct editbaar zijn door deze rol.
Te bepalen:
 - welke velden zijn zichtbaar?
 - welke velden zijn editbaar?
 - welke signalen zijn zichtbaar?


  newInsProc :: ServiceSpec -> Role -> ProcSpec

>  namet :: Identified a => a -> String
>  namet    = firstCaps.map toLower.name
>  nameAt a = firstCaps ((map toLower.name.target) a++"_"++name a)
>  tt a = "{\\tt "++a++"}"
>  nameAtt a = tt (nameAt a)
>  newEnt :: Context -> (Concept,[(Morphism,b)]) -> [Morphism] -> [(Expression,Rule)] -> ServiceSpec
>-- The first alternative is for concepts that are not entities.
>  newEnt context (c,as) [] rs
>   = Sspc (firstCaps ("new"++firstCaps (name c))) (IF Gemiddeld)
>          [ Pspc (map toLower (name c)) (name c)] [Pspc "obj" (handle context c)]
>          (dressRules
>          [ (clause,rule)
>          | (conj,rule)<-rs
>          , clause@(Fu terms)<-[lClause conj]
>          , not (null (map fst as `isc` mors [t| Cp t<-terms]))])
>--    Pre (example:) {Assume l=atom_left and r=atom_right}
>          (["\\hbox{\\tt "++map toLower (name c)++"}="++idNam (map toLower (name c))]++[tt (name a)++"="++idNam (nameAt a)|(a,_)<-as])
>--    Post (example:) {o in Pair and o left l and o right r}
>          (["{\\tt obj}="++idNam (map toLower (name c))]++
>           [tt ("obj."++name a)++"="++idNam (nameAt a)|(a,_)<-as])
>     where varName = uName (map (name.fst) as)
>  newEnt context (c,as) cs rs
>   = Sspc (firstCaps ("new"++firstCaps (name c))) (IF Gemiddeld) [ Pspc (varName (name a)) (handle context (target a)) | a<-cs] [Pspc "obj" (handle context c)]
>          (dressRules
>          [ (clause,rule)
>          | (conj,rule)<-rs
>          , clause@(Fu terms)<-[lClause conj]
>          , not (null (map fst as `isc` mors [t| Cp t<-terms]))])
>--    Pre (example:) {Assume l=atom_left and r=atom_right}
>          [ {- tt (varName (name a))++"="++idNam (nameAt a)|a<-cs -}] -- comment left behind in the code, just in case you need values for the parameters.
>--    Post (example:) {o in Pair and o left l and o right r}
>          ([tt ("obj."++name a)++"="++tt (varName (name a))|a<-cs])
>     where varName = uName (map (name.fst) as)
>  getEnt :: Context -> (Concept,[(Morphism,b)]) -> ServiceSpec
>  getEnt context (c,as)
>   = Sspc (firstCaps ("get"++name c)) (OF Eenvoudig) [Pspc "x" (handle context c)] [Pspc (varName (name a)) (handle context (target a)) | (a,_)<-as] []
>--    Pre (example:) {Assume x=O, O left l, O right r, O src s, and O trg t}
>          ([ tt ("x."++name a)++"="++idNam (nameAt a) |(a,_)<-as])
>--    Post (example:) {left=l, right=r, src=s, and trg=t}
>          [tt (varName (name a))++"="++idNam (nameAt a)|(a,_)<-as]
>     where varName = uName (map (name.fst) as)
>  keyEnt :: Context -> (Concept,[(Morphism,b)]) -> (String,[Morphism]) -> ServiceSpec
>  keyEnt context (c,as) (key,ks)
>   = Sspc (firstCaps ("sel"++name c++"_by_"++if null key then chain "_" (map name ks) else key))
>          (OF Eenvoudig)
>          [ Pspc (varName (name a)) (handle context (target a)) | a<-ks]
>          [Pspc "obj" (handle context c)]
>          []
>--    Pre (example:) {Assume l=atom_left and r=atom_right}
>          [let args = [tt ("x."++name a)++"="++tt (varName (name a)) | a<-ks]++
>                      [tt ("x."++name a)++"="++idNam (nameAt a) | (a,_)<-as, not (a `elem` ks)]
>           in
>           "\\hbox{There is an {\\tt x}}\\in"++idName c++"\\ \\hbox{such that}"++
>           (if length args==1 then "\\ "++concat args else
>            "\\\\\\hspace{2em}$\\begin{array}[b]{lll}\n("++
>            chain "\\\\\n" (map ('&':) args)++
>            "&)\n\\end{array}$"
>           )]
>--    Post (example:) {Assume x=O, O left l, O right r, O src s, and O trg t}
>          ([ tt "obj"++"="++tt "x"])
>     where varName = uName (map name ks)
>  delKeyEnt :: Context -> (Concept,[(Morphism,b)]) -> (String,[Morphism]) -> [(Expression,Rule)] -> ServiceSpec
>  delKeyEnt context (c,as) (key,ks) rs
>   = Sspc (firstCaps ("del"++name c++"_by_"++if null key then chain "_" (map name ks) else key))
>          (IF Gemiddeld)
>          [ Pspc (varName (name a)) (handle context (target a)) | a<-ks]
>          []
>          (dressRules
>          [ (clause,rule)
>          | (conj,rule)<-rs
>          , clause@(Fu terms)<-[rClause conj]
>          , not (null (map fst as `isc` mors [t| t<-terms, isPos t]))])
>--    Pre (example:) 
>          []
>--    Post (example:) {Assume x=O, O left l, O right r, O src s, and O trg t}
>          ["\\hbox{\\tt obj}\\in"++idName c++"\\ \\hbox{implies that not}"++
>           (if length ks==1 then let a=head ks in "\\ ("++tt ("obj."++name a)++"="++idNam (nameAt a)++")" else
>           "\\\\\\hspace{2em}$\\begin{array}[b]{lll}\n("++
>           chain "\\\\\n" ["&"++tt ("obj."++name a)++"="++tt (varName (name a))|a<-ks]++
>           "&)\n\\end{array}$"
>           )]
>     where varName = uName (map name ks)
>  delEnt :: Context -> (Concept,[(Morphism,b)]) -> [(Expression,Rule)] -> ServiceSpec
>  delEnt context (c,as) rs
>   = Sspc (firstCaps ("del"++name c)) (IF Gemiddeld)
>          [Pspc "x" (handle context c)] []
>          (dressRules
>          [ (clause,rule)
>          | (conj,rule)<-rs
>          , clause@(Fu terms)<-[rClause conj]
>          , not (null (map fst as `isc` mors [t| t<-terms, isPos t]))])
>--    Pre
>          [if length as==1 then let (a,_)=head as in tt ("x."++name a)++"="++idNam (nameAt a) else
>           "$\\begin{array}[t]{lll}\n"++
>           chain "\\\\\nand" ["&"++tt ("x."++name a)++"="++idNam (nameAt a)|(a,_)<-as]++
>           "&\n\\end{array}$"
>          | not (null as)]
>--    Post 
>          [if null as then "\\hbox{\\tt x}\\not\\in"++idName c else
>           "\\hbox{\\tt obj}\\in"++idName c++"\\ \\hbox{implies that not}"++
>           (if length as==1 then let (a,_)=head as in "\\ ("++tt ("obj."++name a)++"="++idNam (nameAt a)++")" else
>           "\\\\\\hspace{2em}$\\begin{array}[b]{lll}\n("++
>           chain "\\\\\nand" ["&"++tt ("obj."++name a)++"="++idNam (nameAt a)|(a,_)<-as]++
>           "&)\n\\end{array}$"
>           )]
>--   where varName = uName (map (name.fst) as)
>  updEnt :: Context -> (Concept,[(Morphism,b)]) -> [Morphism] -> [(Expression,Rule)] -> ServiceSpec
>  updEnt context (c,as) cs rs
>   = Sspc (firstCaps ("upd"++name c)) (IF Gemiddeld) (Pspc "x" (handle context c): [ Pspc (varName (name a)) (handle context (target a)) | (a,_)<-as]) []
>          (dressRules rs)
>--    Pre (example:) {Assume x left l, x right r, x src s, and x trg t}
>          []
>--    Post (example:) {x left l, x right r, x src s, and x trg t}
>          [ tt ("x."++name a)++"="++tt (varName (name a))|(a,_)<-as]
>     where varName = uName (map (name.fst) as)
>  newPair :: Context -> [Declaration] -> [(Expression,Rule)] -> Declaration -> String -> ServiceSpec
>  newPair context relations clauses r nm
>   = Sspc (firstCaps ("assoc"++"_"++nm))
>          (IF Gemiddeld)
>          [ Pspc s (handle context (source r)), Pspc t (handle context (target r))] []
>          (dressRules [ (clause,rule)
>                      | (conj,rule)<-clauses
>                      , clause@(Fu terms)<-[lClause conj]
>                      , r `elem` map declaration (mors [t| Cp t<-terms])])
>          [] ["("++s++","++t++")\\ \\in\\ "++idName r]
>     where varName = uName (map name [source r, target r])
>           s = if homogeneous r then tt "s" else (tt.varName.name.source) r
>           t = if homogeneous r then tt "t" else (tt.varName.name.target) r
>  isPair :: Context -> [Declaration] -> Declaration -> String -> ServiceSpec
>  isPair context relations r nm
>   = Sspc (firstCaps ("member_"++nm)) (OF Eenvoudig)
>          [Pspc "s" (handle context (source r)),Pspc "t" (handle context (target r))] [Pbool] []
>          [] [tt (firstCaps ("member_"++nm)++"("++s++","++t++")")++"\\ \\hbox{yields true iff}\\ ("++tt s++","++tt t++")\\in"++idName r]
>     where varName = uName (map name [source r, target r])
>           s = if homogeneous r then "s" else (varName.name.source) r
>           t = if homogeneous r then "t" else (varName.name.target) r
>  delPair :: Context -> [Declaration] -> [(Expression,Rule)] -> Declaration -> String -> ServiceSpec
>  delPair context relations clauses r nm
>   = Sspc (firstCaps ("remove"++"_"++nm)) (IF Gemiddeld)
>          [Pspc s (handle context (source r)),Pspc t (handle context (target r))] []
>          (dressRules
>          [ (clause,rule)
>          | (conj,rule)<-clauses
>          , clause@(Fu terms)<-[rClause conj]
>          , r `elem` map declaration (mors [t| t<-terms, isPos t])])
>          [] ["("++s++","++t++")\\ \\not\\in\\ "++idName r]
>     where varName = uName (map name [source r, target r])
>           s = if homogeneous r then tt "s" else (tt.varName.name.source) r
>           t = if homogeneous r then tt "t" else (tt.varName.name.target) r
>  srcObjs :: Context -> [Declaration] -> Declaration -> String -> ServiceSpec
>  srcObjs context relations r nm
>   = Sspc srvName
>          (OF Eenvoudig)
>          [Pspc s (handle context (source r))]
>          [if isFunction r 
>           then Pspc outName (handle context (target r))
>           else Pspc outName ("\\{"++handle context (target r)++"\\}")] []
>          []
>          [if isFunction r
>           then tt outName++"\\ =\\ "++t++", where ("++s++","++t++")\\in"++idName r
>           else tt outName++"\\ =\\ \\{"++t++"|\\ ("++s++","++t++")\\in"++idName r++"\\}"]
>     where srvName = if homogeneous(r) then firstCaps ("trg_"++nm) else
>                     if isFunction r 
>                     then firstCaps ((map toLower.name.target) r++"_"++nm)
>                     else firstCaps ((plural English .map toLower.name.target) r++"_"++nm)
>           outName | isFunction r                = t
>                   | Uni `elem` multiplicities r = "handle"
>                   | otherwise                   = "handles"
>           varName = uName (map name [source r, target r])
>           s = if homogeneous r then tt "s" else (tt.varName.name.source) r
>           t = if homogeneous r then tt "t" else (tt.varName.name.target) r
>  trgObjs :: Context -> [Declaration] -> Declaration -> String -> ServiceSpec
>  trgObjs context relations r nm
>   = Sspc srvName
>          (OF Eenvoudig)
>          [Pspc t (handle context (target r))]
>          [if isFunction (flp r) 
>           then Pspc outName (handle context (source r))
>           else Pspc outName ("\\{"++handle context (source r)++"\\}")] []
>          []
>          [if isFunction (flp r)
>           then tt outName++"\\ =\\ "++s++", where ("++s++","++t++")\\in"++idName r
>           else tt outName++"\\ =\\ \\{"++s++"|\\ ("++s++","++t++")\\in"++idName r++"\\}"]
>     where srvName = if homogeneous(r) then firstCaps ("src_"++nm) else
>                     if isFunction r 
>                     then firstCaps ((map toLower.name.source) r++"_"++nm)
>                     else firstCaps ((plural English .map toLower.name.source) r++"_"++nm)
>           outName | isFunction (flp r)          = s
>                   | Inj `elem` multiplicities r = "handle"
>                   | otherwise                   = "handles"
>           varName = uName (map name [source r, target r])
>           s = if homogeneous r then tt "s" else (tt.varName.name.source) r
>           t = if homogeneous r then tt "t" else (tt.varName.name.target) r

>  firstCaps :: String -> String
>  firstCaps "" = ""
>  firstCaps "_" = ""
>  firstCaps ('_':'_':str) = firstCaps ('_':str)
>  firstCaps ('_':c:str) = toUpper c:firstCaps str
>  firstCaps (c:str) = c:firstCaps str

>  applyMLatex (Sgn nm _ _ _ prL prM prR _ _ _ _) d c = if null (prL++prM++prR) then "$"++d++"$\\ "++firstCaps nm++"\\ $"++c++"$" else prL++"$"++d++"$"++prM++"$"++c++"$"++prR
>  applyMLatex (Isn _ _)                          d c = "$"++d++"$ equals $"++c++"$"
>  applyMLatex (Iscompl _ _)                      d c = "$"++d++"$ differs from $"++c++"$"
>  applyMLatex (Vs _ _)                           d c = show True

The following functional specification, funcSpec, computes which relations are may be affected by compute rules.
Assuming that they will be computed in all cases, all other relations are treated as input parameters.
This assumption, however, is not true.
TODO: determine which relations are affected but not computed, and report as an error.

>  funcSpec context (entities,relations,ruls) language
>   = [ Fspc pat 
>            ([ Uspc (firstCaps (name c)) pat [(c,as,if null as then NO else ILGV Eenvoudig,cs,rs)]
>                    ( [ newEnt context (c,as) cs rs ] ++ [ getEnt context (c,as)]                      ++
>                      concat [ [keyEnt context (c,as) (key,ks), delKeyEnt context (c,as) (key,ks) rs]
>                             | (e,key,ks)<-keys pat, e==c]                                             ++
>                      [ delEnt context (c,as) rs ]                                                     ++
>                      [ updEnt context (c,as) cs rs| not (null cs) ]
>                    )
>             | (c,as,fpa,cs,rs)<-ec]++clss pat ec newConcs++asss pat newDecls)
>     | (pat,newConcs,newDecls)<-zip3 (patterns context) (firsts [] (map concs (patterns context))) (firsts [] (map declarations (patterns context)))
>     , car<-[definedEnts context pat], not (null car), ec<-[ents car] ]
>     where
      
Het verhaal:
In elk hoofdstuk wordt een pattern behandeld.
Elke entiteit wordt in precies één hoofdstuk behandeld.
De overige concepten worden behandeld in het hoofdstuk waarin het voor de eerste maal voorkomt.
c is het concept
as zijn de attributen van dat concept
cs zijn de vrij in te vullen relaties. Dat zijn degenen die niet 'affected' (ofwel automatisch uitgerekend) zijn.
rs zijn de betrokken regels

>      ents car = [ (c,as,if null as then NO else ILGV Eenvoudig,cs,rs)
>                 | (c,as)<-car
>                 , rs<-[[(conj,rule) |rule<-rules context, c `elem` concs rule, conj<-conjuncts rule]]
>                 , cs<-[[a| (a,_)<-as, not (declaration a `elem` affected)]], not (null cs)
>                 ]
>      clss pat ec new
>       = [ Uspc (if language==English then "Other Classes" else "Andere Classes") pat car
>                [ service
>                | (c,_,_,_,rs)<-car
>                , service <- [ newEnt context (c,[]) [] rs ]                                                ++
>                             [ keyEnt context (c,[]) (key,ks) | (e,key,ks)<-keys pat, e==c] ++
>                             [ delEnt context (c,[]) rs ]
>                ]
>         ] where car = [ (c,[],NO,[],rs)
>                       | c<-concs pat,  not (c `elem` map fst entities), c `elem` new
>                       , rs<-[[(conj,rule) |rule<-rules pat, conj<-conjuncts rule, c `elem` concs conj]]
>                       ]
>      asss pat new
>       = [ Uspc ((if language==English then "Associations of " else "Associaties van ")++firstCaps (name pat)) pat [] ss
>         | ss<-[[ service
>                | r<-relations, not (isSgnl r), r `elem` new
>                , nm <- [name r++if length [d|d<-relations, name d==name r]==1 then "" else
>                                 name (source r)++name (target r)]
>                , rs<-[[(conj,rule) |rule<-rules context, conj<-conjuncts rule, r `elem` declarations conj]]
>                , service <- [ newPair context relations rs r nm
>                             , isPair context relations r nm
>                             , delPair context relations rs r nm
>                             , srcObjs context relations r nm
>                             , trgObjs context relations r nm ]
>                ]]
>         , not (null ss)
>         ]
>      hcs = [hc| rule<-rules context, hc<-triggers rule ]
>      affected :: [Declaration]
>      affected = rd[s| hc@(fOps, e, bOp, toExpr, frExpr, rule)<-hcs, s<-declarations toExpr]

>  funcSpecText context fspcs English
>   = "Functional Specification:\n"++chain "\n\n" (map fSpec fspcs)
>     where
>      fSpec (Fspc pat units)
>       = "Chapter "++firstCaps (name pat)++"\n\n"++chain "\n\n" (map fUnit units)
>      fUnit unit@(Uspc unm pat car specs)
>       = "Section "++unm++fpaUnit unit English++
>           chain "\n\n"
>           [ ( if null pre then "" else "\n  {Assume: "++chain " and " pre ++"}") ++
>             nm++"("++chain ";" [p++":"++c| Pspc p c<-input]++")"++chain ";" [" "++p++" "++c| Pspc p c<-output]++
>             ( if null post then "" else "\n  {"++chain " and " post ++"}") ++
>             ( if null rs then "" else
>               if length rs>1
>               then "\n\tInvariants:\n   "++chain "\n   " [fixSpaces 3 (show (nr r))++") "++showOO r |r<-rs]
>               else "\n\tInvariant: "++showOO (head rs) ++ "(Rule "++show (nr (head rs))++")"
>             )++
>             if fpa==NO then "" else
>             "\n\n\tThis service is qualified in the FPA as "++showLang English (complexity fpa)++"."
>           | Sspc nm fpa input output rs pre post<-specs
>           ]

>  funcSpecText context fspcs Dutch
>   = "Functionele Specificatie:\n"++chain "\n\n" (map fSpec fspcs)
>     where
>      fSpec (Fspc pat units)
>       = "Hoofdstuk "++firstCaps (name pat)++"\n\n"++chain "\n\n" (map fUnit units)
>      fUnit unit@(Uspc unm pat car specs)
>       = "Sectie "++unm++fpaUnit unit Dutch++
>           chain "\n\n"
>           [ ( if null pre then "" else "\n  {Stel: "++chain " and " pre ++"}") ++
>             nm++"("++chain ";" [p++":"++c| Pspc p c<-input]++")"++chain ";" [" "++p++" "++c| Pspc p c<-output]++
>             ( if null post then "" else "\n  {"++chain " and " post ++"}") ++
>             ( if null rs then "" else
>               if length rs>1
>               then "\n\tInvarianten:\n   "++chain "\n   " [fixSpaces 3 (show (nr r))++") "++showOO r |r<-rs]
>               else "\n\tInvariant: "++showOO (head rs) ++ "(Rule "++show (nr (head rs))++")"
>             )++
>             "\n\n\tDeze service is gekwalificeerd in de FPA als "++showLang Dutch (complexity fpa)++"."
>           | Sspc nm fpa input output rs pre post<-specs
>           ]

  data Funit = Uspc String Pattern
                    [(Concept,[(Morphism,[Rule])],FPA,[Morphism],[(Expression,Rule)])]
                    [ServiceSpec] -- services

Elk pattern komt in een hoofdstuk, waarbinnen alle referenties in zichzelf compleet zijn.
Hierdoor zijn de hoofdstukken afzonderlijk distribueerbaar.
Aan het begin van elk hoofdstuk worden de attributen gedefinieerd van de entiteiten die in dit hoofdstuk worden behandeld.
Alle overige relaties worden voor het eerste gebruik gedefinieerd.

>  fpaUnit unit@(Uspc unm pat car specs) language
>   = (if length car==1
>      then let (c,_,fpa,_,_) = head car in
>           fpaText language unm c fpa++
>           "\tFPA "++complex++":\n\n"++
>           "\\begin{tabular}{|l|l|r|}\\hline concept&type&fp\\\\\\hline\n  "++
>           chain "\\\\\n"
>           [nm++"&"++showLang language fpa++"&"++show (fPoints fpa)
>           | Sspc nm fpa input output rs pre post<-specs]++
>           "\\\\\\hline\n\\end{tabular}\n\n"
>      else "\tFPA "++complex++":\n\n"++
>           "\\begin{tabular}{|l|l|r|}\\hline concept&type&fp\\\\\\hline\n  "++
>           chain "\\\\\n"
>           ([name c++"&"++showLang language fpa++"&"++show (fPoints fpa)
>            | (c,_,fpa,_,_) <- car]++
>            [nm++"&"++showLang language fpa++"&"++show (fPoints fpa)
>            | Sspc nm fpa input output rs pre post<-specs])++
>           "\\\\\\hline\n\\end{tabular}\n\n"
>     )++
>     (if length car+length specs<=1 then "" else
>      "\t"++unm++aswhole++
>      show (sum[fPoints fpa| (_,_,fpa,_,_)<-car]+sum [fPoints fpa| Sspc nm fpa input output rs pre post<-specs])++
>      worth++".\n\n"
>     )
>     where
>      fpaText language unm c NO = ""
>      fpaText English unm c fpa
>       = (if unm == name c then "\n\tThis concept" else "\n\tname c")++
>         " is qualified in the FPA as a data collection of type "++showLang English fpa++".\n"++
>         "\tIt is worth "++show (fPoints fpa)++" function points.\n\n"
>      fpaText Dutch unm c fpa
>       = (if unm == name c then "\n\tDit concept" else "\n\tname c")++
>         " is gekwalificeerd in  FPA als een gegevensverzameling van type "++showLang Dutch fpa++".\n"++
>         "\tDat levert "++show (fPoints fpa)++" functiepunten op.\n\n"
>      complex | language==English = "complexity"
>              | language==Dutch   = "complexiteit"
>      aswhole | language==English = " as a whole is worth "
>              | language==Dutch   = " als geheel is"
>      worth   | language==English = " function points"
>              | language==Dutch   = " functiepunten waard"

>  funcSpecLaTeX context fspcs language
>   = chain "\n\n" (map fSpec fspcs)
>     where
>      cname = name context
>      fSpec (Fspc pat units)
>       = latexChapter ((addSlashes.name) pat) ("Fspec "++firstCaps (name pat))++
>         latexFigure (latexCenter ("  \\includegraphics[scale=.3]{"++cname++"_"++clname (name pat)++"_CD.png}")++
>         captiontext language pat)++"\n"++
>         ( if null attributes then "" else introtext language attributes++
>           "\\begin{eqnarray}\n"++
>           chain "\\\\\n" ["  "++idName d++"&:&"++idName (source d)++" \\times "++idName (target d)
>                          | (a,d,cnm)<-attributes]++
>           "\n\\end{eqnarray}\n"++
>           intratext language attributes++
>           "\\begin{eqnarray}\n"++
>           chain "\\\\\n" [ "\\hbox{For each }"++tt cnm++"\\in"++idNam cnm++"&&"++
>                            ( if inline a
>                              then tt cnm++"\\ "++idName d++"\\ "++tt (cnm++"."++name a)
>                              else tt (cnm++"."++name a)++"\\ "++idName d++"\\ "++tt cnm
>                            )++"\\label{sgn:"++firstCaps(name pat++":"++name d++name (source d)++name (target d))++"}"
>                          | (a,d,cnm)<-attributes] ++
>           "\n\\end{eqnarray}\n")++
> --      str4++
> --      "\\begin{tabular}{|lll|}\\hline relation&variables&fact\\\\\\hline\n  "++
> --      chain "\\\\\n" ["  "++firstCaps (name d)++"&"++v (source d)++",\\ "++idNam (v (target d))++"&"++applyMLatex d (v (source d)) (v (target d))
> --                     | d<-declarations new, v<-[tt.uName [name (source d),name (target d)].name] ]++
> --      "\\\\\\hline\n\\end{tabular}\n\n"++
>         chain "\n\n" [fUnit u new| (u,new)<-(zip units.firsts [])
>                                              [ [d| Sspc nm fpa input output rs pre post<-specs, d<-declarations rs
>                                                  , isSgn d && not (isSgnl d) && not (d `elem` map snd3 attributes)]
>                                              | u@(Uspc unm pat car specs)<-units, not (null specs) ]
>                      ]
>         where
>           varName = uName [name c| u@(Uspc unm pat car specs)<-units, (c,as,fpa,cs,rs)<-car, not (null as)]
>           attributes = [ (a, d, varName (name c))
>                        | u@(Uspc unm pat car specs)<-units 
>                        , (c,as,fpa,cs,rs')<-car, (a,_)<-as, d<-declarations a
>                        ]

>      isAttribute d = null ([Uni,Tot]>-multiplicities d) || null ([Sur,Inj]>-multiplicities d)
>      fUnit unit@(Uspc unm pat car []) newdecs = ""
>      fUnit unit@(Uspc unm pat car specs) newdecs
>       = latexSection (firstCaps unm) ("Fspc "++firstCaps (unm++name pat))++
>         fpaUnit unit language++
>         lettext language unm++
>         ( if null decls then "" else atttext language unm attributes++
>           (if language == English then commaEng "and" else commaNL "en")
>            ["\\ref{sgn:"++firstCaps(name pat++":"++name d++name (source d)++name (target d))++"}" | d<-decls]++".\n"
>         )++str2++
>         chain "\n\n"
>           [ srvSchema pat language (name pat) spc [m|m<-new, not (m `elem` attributes), declaration m `elem` newdecs]
>           | (spc,new)<-zip specs (firsts [] [ [m|m<-mors rs, isMph m]
>                                             | Sspc nm fpa input output rs pre post<-specs ])
>           ]
>        where attributes = [ m | (c,mrs,fpa,ms,ers)<-car, (m,rs)<-mrs]
>              decls = rd (map declaration attributes)
>      captiontext English pat
>       = "\n\\caption{Data structure of "++(addSlashes.name) pat++"}\n\\label{fig:"++clname (name pat)++"}"
>      captiontext Dutch pat
>       = "\n\\caption{Gegevensstructuur van "++((addSlashes.name) pat)++"}\n\\label{fig:"++clname (name pat)++"}"
>      str2 | language==English = "The services are defined in the following subsections.\n"
>           | language==Dutch   = "De services zijn in de volgende secties gedefinieerd.\n"
>      lettext English "Other Classes"
>       = "Services for other classes used in this chapter are defined here.\n"
>      lettext English nm
>       = if take 16 nm == "Associations of "
>         then "To complete this chapter, we define services on associations between classes.\n"
>         else "Let "++idNam nm++" be the set of all "++plural English (map toLower nm)++" in the system.\n"
>      lettext Dutch "Andere Classes"
>       = "Services voor andere classes die in dit hoofdstuk zijn gebruikt, worden in deze sectie gedefinieerd.\n"
>      lettext Dutch nm
>       = if take 16 nm == "Associaties van "
>         then "Tenslotte volgen nog een aantal services op associaties tussen classes."
>         else "Stel "++idNam nm++" is de verzameling van alle "++plural Dutch (map toLower nm)++" in het systeem.\n"
>      introtext English attributes
>       = if length attributes>1
>         then "To describe the behaviour of the services defined in this chapter, we introduce one relation for each attribute:\n"
>         else "To describe the behaviour of the services defined in this chapter, we introduce:\n"
>      introtext Dutch attributes
>       = if length attributes>1
>         then "Om het gedrag te beschrijven van de services in dit hoofdstuk, introduceren we \\'e\\'en relatie voor elk attribuut:\n"
>         else "Om het gedrag te beschrijven van de services in dit hoofdstuk, introduceren we:\n"
>      intratext English attributes
>       = "Attributes are defined in terms of these relations\n"
>      intratext Dutch attributes
>       = "Attributen zijn in termen van deze relaties gedefinieerd.\n"
>      atttext English nm attributes
>       = if length attributes>1
>         then "The attributes of "++nm++" are defined in "
>         else "The attribute of "++nm++" is defined in "
>      atttext Dutch nm attributes
>       = if length attributes>1
>         then "De attributen van "++nm++" zijn gedefinieerd in "
>         else "Het attribuut van "++nm++" is gedefinieerd in "

>  firsts seen (ds:dss) = new: firsts (seen++new) dss where new = ds>-seen
>  firsts seen [] = []
>  handle context c = firstCaps (name c)++if c `elem` (map fst entities) then "Handle" else ""
>   where (entities,relations,ruls) = erAnalysis context

>  srvSchema pat language cnm (Sspc nm fpa input output rs pre post) new
>   = latexSubsection nm nm++
>     "{\\tt "++firstCaps nm++"}("++
>     "\\begin{array}[t]{lr@{~:~}ll}\n"++
>     ( let pars=["{\\tt In}&{\\tt " ++firstCaps p++"}&{\\tt "++c++"}"| Pspc p c<-input]++
>                ["{\\tt Out}&{\\tt "++firstCaps p++"}&{\\tt "++c++"}"| Pspc p c<-output]
>       in if length pars==1 then concat pars++"\\ )" else chain "&;\\\\\n" pars++"&)"
>     )++
>     "\n\\end{array}\n"++
>     str1++  -- When called, this service behaves as follows:
>     "\\begin{tabular}{l}\n"++
>     chain "\\\\\n" (["\\hspace{2em}\\{Pre: $"++
>                                            "\\begin{array}[t]{ll}\n"++
>                                            chain "\\hbox{and}\\\\\n" [p++"&"| p<-pre]++
>                                            (if length pre>1 then "\\hspace{2em}\\}" else "\\}")++
>                                            "\n\\end{array}$\n"| not (null pre)]++
>--                   ["\\hspace{2em}\\{Pre: $"++chain "\\ \\hbox{and}\\ " [p| p<-pre]++"$\\}"| not (null pre)]++
>                     [call]++
>                     ["\\hspace{2em}\\{Post: $"++
>                                            "\\begin{array}[t]{ll}\n"++
>                                            chain "\\hbox{and}\\\\\n" [p++"&"| p<-post]++
>                                            (if length post>1 then "\\hspace{2em}\\}" else "\\}")++
>                                            "\n\\end{array}$\n"| not (null post)]
>                     )++"\n"++
>     "\\end{tabular}\n"++
>     -- For defining invariants, we introduce:
>     ( if null new then "" else str3++  -- For defining invariants, we introduce:
>       "\\begin{eqnarray}\n  "++
>       chain "\\\\\n" ["  "++idNam (firstCaps (name d))++"&:&"++idName (source d)++" \\times "++idName (target d)++"\\label{sgn:"++firstCaps(cnm++":"++name d++name (source d)++name (target d))++"}"
>                      | d<-declarations new ]++
>       "\n\\end{eqnarray}\n\n"++
>       if null facts then "" else
>        if length facts == 1 then let (d,s,t,v)=head facts in "\tRelation "++firstCaps (name d)++"represents facts.\n\tExpression "++v s++" "++firstCaps (name d)++" "++v t++" means: "++applyM d (v s) (v t)++".\n" else
>        str4++  -- These relations represent facts according to the following table
>        "\n\\begin{tabular}{|lll|}\\hline relation&variables&fact\\\\\\hline\n  "++
>        chain "\\\\\n" ["  "++firstCaps (name d)++"&"++v s++",\\ "++v t++"&"++applyMLatex d (v s) (v t)
>                       | (d,s,t,v)<-facts]++
>        "\\\\\\hline\n\\end{tabular}\n\n")++
>     (if null rs then "" else
>      if length rs>1
>      then str5++  -- The service call "++call++" must maintain the following rules:
>           "\\[\\begin{array}{l}\n   "++
>           chain "\\\\\n   " [show (nr r)++")~~~"++(lshow language.assemble.normRule) r |r<-rs]++"\n"++
>           "\\end{array}\\]\n"
>      else "\n\nInvariant: \\("++(lshow language.assemble.normRule.head) rs++"\\)\\ "++str6++show (nr (head rs))++")\n")++
>     (if null ms then "" else
>      if length rs==1
>      then str7++  -- Cross reference table to the relations used:
>      "\\begin{tabular}{|lllrr|}\\hline\nname&source&target&def.&pg.\\\\\\hline\n   "++
>      chain "\\\\\n   " [(idNam.firstCaps.name) d++"&"++(idNam.name.source) d++"&"++(idNam.name.target) d++"&"++"\\ref{sgn:"++firstCaps(name pat++":"++name d++name (source d)++name (target d))++"}&"++"\\pageref{sgn:"++firstCaps(name pat++":"++name d++name (source d)++name (target d))++"}"
>                        |(d,rs)<-ms, isSgn d]++
>      "\\\\\\hline\n\\end{tabular}\n"
>      else str8++  -- Cross references to the relations used in these rules:
>      "\\begin{tabular}{|lllrrl|}\\hline\nname&source&target&def.&pg.&used in\\\\\\hline\n   "++
>      chain "\\\\\n   " [(idNam.firstCaps.name) d++"&"++(idNam.name.source) d++"&"++(idNam.name.target) d++"&"++"\\ref{sgn:"++firstCaps(name pat++":"++name d++name (source d)++name (target d))++"}&"++"\\pageref{sgn:"++firstCaps(name pat++":"++name d++name (source d)++name (target d))++"}&"++chain "," (map (show.nr) rs)
>                        |(d,rs)<-ms, isSgn d]++
>      "\\\\\\hline\n\\end{tabular}\n"
>     )
>     where
>      facts = [ (d,s,t,tt.uName [s,t]
>                )
>              | (d,ms)<-[(declaration (head cl), cl)| cl<-eqCl declaration new]
>              , applyM d "" "" /= ""
>              , s<-[if homogeneous d then "src_"++name (source d) else name (source d)]
>              , t<-[if homogeneous d then "trg_"++name (target d) else name (target d)]
>              ]
>      ms = [ (snd (head cl), map fst cl)
>           | cl<-eqCl snd [(r,d)| r<-rs, d<-declarations r]]
>      call = "{\\tt "++firstCaps nm++"}("++chain "," ["{\\tt "++firstCaps p++"}"| Pspc p c<-input++output]++")"
>      str1 | language==English = "\nWhen called, this service behaves as follows:\n\n"
>           | language==Dutch   = "\nBij aanroep gedraagt deze service zich als volgt:\n\n"
>      str3 | language==English = "\nFor defining invariants, we introduce:\n"
>           | language==Dutch   = "\nVoor het defini\\\"eren van invarianten, introduceren we\n"
>      str4 | language==English = "\nThese relations represent facts according to the following table:\n"
>           | language==Dutch   = "\nDeze relaties representeren de volgende feiten:\n"
>      str5 | language==English = "\n\nThe service call "++call++" must maintain the following rules:\n"
>           | language==Dutch   = "\n\nDe aanroep "++call++" moet de volgende regels in stand houden:\n"
>      str6 | language==English = "(Rule "
>           | language==Dutch   = "(Regel "
>      str7 | language==English = "\nCross reference table to the relations used:\n\n"
>           | language==Dutch   = "\nKruisreferentietabel naar de gebruikte relaties:\n\n"
>      str8 | language==English = "\nCross references to the relations used in these rules:\n\n"
>           | language==Dutch   = "\nKruisreferenties naar de relaties die in deze regels zijn gebruikt\n\n"

>  dressRules :: [(Expression,Rule)] -> [Rule]
>  dressRules clauses = [ if length cl>1 then rule else makeRule rule clause | cl<-(map rd.eqCl snd) clauses, (clause,rule)<-take 1 cl]
>   where
>    f (Fu cl) (Fu cl') = [e| e<-cl, isPos e] `eq` [e| Cp e<-cl'] &&
>                         [e| Cp e<-cl] `eq` [e| e<-cl', isPos e]
>    self (Fu cl)       = [e| e<-cl, isPos e] `eq` [e| Cp e<-cl]
>    a `eq` b = length a==length b && length a==length (a `isc` b)

>  laGen :: String -> String -> IO()
>  laGen fnm latexcode
>   = writeFile (fnm++".tex") latexcode           >>
>     putStr ("\nLaTeX file "++fnm++".tex written... ") >>
>     processFile fnm
>  processFile fnm =
>     do putStr ("\nProcessing "++fnm++".tex ... :")
>        result <- system ("pdflatex -interaction=batchmode "++fnm++".tex")
>        case result of
>            ExitSuccess   -> putStrLn ("  "++fnm++".pdf created.")
>            ExitFailure x -> putStrLn $ "Failure: " ++ show x
>        putStr ("\nReprocessing "++fnm++".tex ... :")
>        result <- system ("pdflatex -interaction=nonstopmode "++fnm++".tex")
>        case result of
>            ExitSuccess   -> putStrLn ("  "++fnm++".pdf created.")
>            ExitFailure x -> putStrLn $ "Failure: " ++ show x

>  class LATEX a where
>   lshow  :: Lang -> a -> String
>   ltshow :: String -> Typology Concept -> Lang -> a -> String
>   ltshow nm typ = lshow

>  definingPattern context entities c
>   = ( fst . head . sort' ((0-).length.snd) ) [(p,rs)| (p,(c',_),rs)<-ps, c'==c]
>     where
>      ps = [(p,e,[r|(_,_,r)<-cl])| cl<-eqCl (\(p,(c,as),r)->(name p,c)) rs, (p,e,_)<-take 1 cl]
>      rs = [(pat,(c,as), rule)
>           | pat<-patterns context, rule<-rules pat, (c,as)<-entities, c `elem` concs rule]
>  definedEnts context pat
>   = [ e
>     | cl<-eqCl (\(p,(c,_),rs)->c) ps, (p,e,rs)<-take 1 (sort' (\(p,e,rs)->0-length rs) cl), p==name pat]
>     where
>      ps = [ (p,e,[r|(_,_,r)<-cl])
>           | cl <-eqCl (\(p,(c,_),_)->(p,c)) rs, (p,e,_)<-take 1 cl
>           ]
>      rs = [ (name p,e,rule)
>           | p<-patterns context, rule<-rules p, e@(c,as)<-entities, c `elem` concs rule]
>      (entities, relations, ruls) = erAnalysis context

>  instance LATEX Context where
>   lshow language ctx = ltshow (name ctx) (typology (isa ctx)) language ctx
>   ltshow cname typ language ctx@(Ctx nm on isa world dc ms cs ks)
>    = (chain "\n". filter (not.null))
>      (if language==Dutch then 
>      ["\\title{Functionele Specificatie\\\\"++addSlashes cname++"}"
>      , "\\maketitle"
>      , "\\tableofcontents"
>      , latexChapter "Inleiding" "Inleiding"
>      , "\tDit document definieert de servicelaag van een systeem genaamd "++addSlashes cname++"."
>      , "\tHet definieert infrastructuur-services in een systeem waarin mensen en applicaties samenwerken"
>      , "\tom afspraken na te leven die gelden in de context van "++addSlashes cname++"."
>      , "\tDeze afspraken worden weergegeven door bedrijfsregels."
>      , "\tDeze regels staan beschreven in hoofdstuk \\ref{chp:Afspraken}, geordend op thema."
>      , "\tEen gegevensanalyse volgt in hoofdstuk \\ref{chp:Gegevensanalyse}."
>      , "\tIn de daarop volgende hoofdstukken is elk thema"
>      , "\tuitgewerkt in definities van services."
>      , "\tDeze services ondersteunen gezamenlijk alle afspraken uit hoofdstuk \\ref{chp:Afspraken}."
>      , "\tDeze ondersteuning bestaat uit het voorkomen dat een afspraak wordt overtreden,"
>      , "\tof het signaleren van overtredingen (opdat mensen kunnen ingrijpen)"
>      , "\tof het herstellen van een regel (door automatische acties op de database uit te voeren)."
>      , latexChapter "Afspraken" "Afspraken"
>      , "\tDit hoofdstuk bespreekt verschillende afspraken, die in volgende hoofdstukken zijn uitgewerkt tot een volledige functionele specificatie."
>      , chain "\n\n" [ latexSection ("Afspraken over "++(addSlashes.name) pat) ("Afspraken"++cname++"Pat"++firstCaps (name pat)) ++
>                       latexEnumerate ([addSlashes (explainRule ctx language r)|r<-rules pat++signals pat, null (cpu r)]++
>                                       [addSlashes (explainMult ctx language d)|d<-declarations pat, (not.null) (multiplicities d)])
>                     | pat<-dc]
>      , latexChapter "Conceptuele Analyse" "Conceptuele Analyse"
>      , "\tDit hoofdstuk geeft een analyse van de regels uit hoofdstuk \\ref{chp:Afspraken}."
>      , "\tIeder thema in dat hoofdstuk wordt geanalyseerd in termen van relaties"
>      , "\ten elke afspraak krijgt een formele representatie."
>      , "\n\tDe resultaten van functiepunt analyse staan vermeld in tabel \\ref{tab:FPA}"
>      , spec2fp ctx Dutch spec
>      , chain "\n\n" [ latexSection ("Regels over "++(addSlashes.name) pat) ("Rules"++cname++"Pat"++firstCaps (name pat)) ++
> --                      "Een conceptuele analyse over "++(addSlashes.name) pat++" is weergegeven in figuur \\ref{fig: concAnal"++clname (firstCaps (name pat))++"}.\n\n"++
> --                      latexFigureHere (latexCenter ("  \\includegraphics[scale=.3]{"++cname++"_"++clname (name pat)++".png}")++
> --                      "\n\\caption{Conceptuele analyse van "++(addSlashes.name) pat++"}\n\\label{fig: concAnal"++clname (firstCaps (name pat))++"}\n")++
>                       latex "longtable" ["{|r|p{\\columnwidth}|}\\hline"]
>                             (chain "\n" [show (nr r)++"&"++addSlashes (explainArt ctx language r)++"\\\\\n&Relaties:\\\\"++
>                                          "&\\(\\begin{array}{rcl}\n"++
>                                               chain "\\\\\n" [idName d++"&:&"++(idName.source) d++"\\times"++(idName.target) d
>                                                              | d<-declarations r]++
>                                               "\\end{array}\\)\\\\\n&Regel:\\\\"++
>                                          "&\\(\\begin{array}{l}"++(lshow language.assemble.normRule) r++"\\end{array}\\)\\\\\\hline"|r<-rules pat]
>         -- als het relAlg moet zijn:     "&\\("++lshow language r                    ++"\\)\\\\\\hline"|r<-rules pat]
>                              )
>                     | pat<-dc]
>      , latexChapter "Gegevensanalyse" "Gegevensanalyse"
>      , "\tDe keuzes, zoals beschreven in hoofdstuk \\ref{chp:Afspraken} zijn in een gegevensanalyse vertaald naar"
>      , "\thet klassediagram in figuur \\ref{fig:"++cname++"CD}."
>      , latexFigure (latexCenter ("  \\includegraphics[scale=.3]{"++cname++"_CD.png}")++
>        "\n\\caption{Gegevensmodel van "++addSlashes cname++"}\n\\label{fig:"++cname++"CD}")
>      , "\tDit hoofdstuk geeft een uitwerking van de gegevensanalyse in de vorm van functionele specificaties."
>      , funcSpecLaTeX ctx (funcSpec ctx (erAnalysis ctx) Dutch) Dutch
>      , latexChapter "Terminologie" ("typology"++cname)
>--      , "De terminologie is ontleend aan het Divisie Informatieplan \\cite{TPDI},"
>--      , "de begrippenlijst voor \\mulF{} \\cite{MultiFit} en de begrippen gebruikt in de voorstudie SBD \\cite{SBD}."
>--      , "In geval van conflicten gaan begrippen uit \\cite{TPDI} v\\'o\\'or \\cite{MultiFit} en begrippen uit \\cite{MultiFit} v\\'o\\'or \\cite{SBD}."
>      , if null cs then "" else
>        latex "longtable" ["{|p{4cm}|p{10cm}|}\\hline"]
>             (chain "\n" ["\\textbf{"++nm++"} & "++addSlashes def++
>                          (if null ref then "" else "~\\cite{"++ref++"}")++
>                          "\\\\\n\\hline"
>                         | Cd pos nm def ref<-conceptdefs ctx, C nm (==) [] `elem` concs ctx])
>      , if null cList then "" else
>        if length cList==1 then "\tHet concept "++idName(head cList)++" heeft geen tekstuele definitie in sectie \\ref{typology"++cname++"}." else
>        "\tDe volgende concepten zijn (nog) niet opgenomen in de woordenlijst: "++commaNL "en" (map idName (sord' name cList))++"."
>      , "\\bibliographystyle{plain}"
>      , "\\bibliography{"++lname ctx++"}"
>      , "\\label{bibliography"++lname ctx++"}"
>      ] else if language==English then
>      ["\\title{Functional Specification\\\\ "++addSlashes cname++"}"
>      , "\\maketitle"
>      , "\\tableofcontents"
>      , latexChapter "Introduction" "Introduction"
>      , "\tThis document defines the service layer of a system called "++addSlashes cname++"."
>      , "\tIt defines infrastructural services in a system in which people and applications collaborate"
>      , "\tto maintain agreements and commitments that apply to the context of "++addSlashes cname++"."
>      , "\tThese agreements and commitments are represented by rules."
>      , "\tThey are presented in chapter \\ref{chp:Principles}, arranged by theme."
>      , "\tA data analysis is presented in chapter \\ref{chp:Data Analysis}."
>      , "\tSubsequent chapters elaborate each theme by defining all applicable services."
>      , "\tTogether, these services support all rules from chapter \\ref{chp:Principles}."
>      , "\tThis support consists of either preventing that a rule is violated,"
>      , "\tsignalling violations (for human intervention),"
>      , "\tor fixing the content of databases (by automatic actions) to restore a rule."
>      , latexChapter "Principles" "Principles"
>      , "\tThis chapter introduces guiding principles of "++addSlashes cname++"."
>      , "\tSubsequent chapters elaborate these principles into complete formal specifications."
>      , chain "\n\n" [ latexSection ("Design choices about "++(addSlashes.name) pat) ("DesignChoices"++cname++"Pat"++firstCaps (name pat)) ++
>                       latexEnumerate ([addSlashes (explainRule ctx language r)|r<-rules pat++signals pat, null (cpu r)]++
>                                       [addSlashes (explainMult ctx language d)|d<-declarations pat, (not.null) (multiplicities d)])
>                     | pat<-dc]
>      , latexChapter "Conceptual Analysis" "Conceptual Analysis"
>      , "\tThis chapter provides an analysis of the principles described in chapter \\ref{chp:Principles}. Each section in that chapter is analysed in terms of relations and each principle is then translated in a rule."
>      , spec2fp ctx English spec
>      , chain "\n\n" [ latexSection ("Rules about "++(addSlashes.name) pat) ("Rules"++cname++"Pat"++firstCaps (name pat)) ++
> --                      "A conceptual analysis of "++(addSlashes.name) pat++" is represented in figure \\ref{fig: concAnal"++clname (firstCaps (name pat))++"}.\n\n"++
> --                      latexFigureHere (latexCenter ("  \\includegraphics[scale=.3]{"++cname++"_"++clname (name pat)++".png}")++
> --                      "\n\\caption{Conceptual analysis of "++(addSlashes.name) pat++"}\n\\label{fig: concAnal"++clname (firstCaps (name pat))++"}")++
>                       latex "longtable" ["{|r|p{\\columnwidth}|}\\hline"]
>                             (chain "\n" [show (nr r)++"&"++addSlashes (explainArt ctx language r)++"\\\\\n&Relations:\\\\"++
>                                          "&\\(\\begin{array}{rcl}\n"++
>                                               chain "\\\\\n" [idName d++"&:&"++(idName.source) d++"\\times"++(idName.target) d
>                                                              | d<-declarations r]++
>                                               "\\end{array}\\)\\\\\n&Rule:\\\\"++
>                                          "&\\(\\begin{array}{l}"++(lshow language.assemble.normRule) r++"\\end{array}\\)\\\\\\hline"|r<-rules pat]
>         -- als het relAlg moet zijn:     "&\\("++lshow language r                    ++"\\)\\\\\\hline"|r<-rules pat]
>                              )
>                     | pat<-dc]
>      , latexChapter "Data Analysis" "Data Analysis"
>      , "\tA data analysis of the principles from the previous chapter (\\ref{chp:Principles}) yields a class diagram,"
>      , "\twhich shown in figure \\ref{fig:"++cname++"CD}."
>      , latexFigure (latexCenter ("  \\includegraphics[scale=.4]{"++cname++"_CD.png}")++
>        "\n\\caption{Data structure of "++addSlashes cname++"}\n\\label{fig:"++cname++"CD}")
>      , "\tDetails are provided in the following sections."
>      , funcSpecLaTeX ctx (funcSpec ctx (erAnalysis ctx) English) English
>      , latexChapter "Glossary" ("typology"++cname)
>      , if null cs then "" else
>        latex "longtable" ["{|p{4cm}|p{10cm}|}\\hline"]
>              (chain "\n" ["\\textbf{"++nm++"} & "++addSlashes def++"~\\cite{"++ref++"}\\\\\n\\hline"
>                          | Cd pos nm def ref<-conceptdefs ctx, C nm (==) [] `elem` concs ctx])
>      , if null cList then "" else
>        if length cList==1 then "\tThe concept "++idName(head cList)++" has no textual definition in the glossary (section \\ref{typology"++cname++"})." else
>        "\tThe following concepts are not described in the glossary: "++commaEng "and" (map idName (sord' name cList))++"."
>      , "\\bibliographystyle{plain}"
>      , "\\bibliography{"++lname ctx++"}"
>      , "\\label{bibliography"++lname ctx++"}"
>      ] else [] )
>      where
>       spec = funcSpec ctx (erAnalysis ctx) language
>       cList = concs ctx>-rd [C nm (==) []| Cd pos nm def ref<-conceptdefs ctx]
>--       nav :: Classification Concept
>--       nav  = sortCl before (Cl (Anything (genE ctx)) (makeTrees typ))
>       mms  = declarations ctx
>--       degree c = length [m | m<-mms, source m==c || target m==c]
>--       c `before` c' = degree c > degree c'
>--       caps :: Classification Concept -> [String]
>--       caps (Cl c cls) = ["\\disjn{"++chain ", " (map (lshow language.root) cls)++"}"| length cls>1] ++ (concat . map capss) cls ++
>--                         concat (map caps cls)
>--       capss (Cl g cls) = ["\\super{"++lshow language g++"}{"++lshow language s++"}" | s<-map root cls]

>  explainRule :: Context -> Lang -> Rule -> String
>  explainRule thisCtx l r
>   = if null (explain r)
>     then (if l==English then "Artificial explanation: " else
>           if l==Dutch   then "Kunstmatige uitleg: " else
>           error("Module PredLogic: unsupported language"))++(lang l .assemble.normRule) r
>     else explain r
>  explainMult :: Context -> Lang -> Declaration -> String
>  explainMult thisCtx Dutch d
>   | null ([Sym,Asy]         >- multiplicities d) = name d++" is een eigenschap van "++(unCap.plural Dutch .name.source) d++"."
>   | null ([Sym,Rfx,Trn]     >- multiplicities d) = name d++" is een equivalentierelatie tussen "++(unCap.plural Dutch .name.source) d++"."
>   | null ([Asy,Trn]         >- multiplicities d) = name d++" is een ordeningsrelatie tussen "++(unCap.plural Dutch .name.source) d++"."
>   | null ([Sym,Asy]         >- multiplicities d) = name d++" is een eigenschap van "++(unCap.plural Dutch .name.source) d++"."
>   | null ([Uni,Tot,Inj,Sur] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("precies één "++(unCap.name.target) d)
>                                                    ++" en vice versa."
>   | null ([Uni,Tot,Inj    ] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("precies één "++(unCap.name.target) d)
>                                                    ++", maar niet voor elke "++(unCap.name.target) d++" hoeft er een "++(unCap.name.source) d++" te zijn."
>   | null ([Uni,Tot,    Sur] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("precies één "++(unCap.name.target) d)
>                                                    ++", maar elke "++(unCap.name.target) d++" is gerelateerd aan één of meer "++(unCap.plural Dutch .name.source) d++"."
>   | null ([Uni,    Inj,Sur] >- multiplicities d) = "Er is precies één "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "
>                                                    ++applyM d "b" "a"
>                                                    ++", maar niet voor elke "++(unCap.name.source) d++" hoeft er een "++(unCap.name.target) d++" te zijn."
>   | null ([    Tot,Inj,Sur] >- multiplicities d) = "Er is precies één "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "
>                                                    ++applyM d "b" "a"
>                                                    ++", maar elke "++(unCap.name.source) d++" mag gerelateerd zijn aan meerdere "++(unCap.plural Dutch .name.target) d++"."
>   | null ([Uni,Tot        ] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("precies één "++(unCap.name.target) d)++"."
>   | null ([Uni,    Inj    ] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("ten hoogste één "++(unCap.name.target) d)
>                                                    ++" en elke "++(unCap.name.target) d++" is gerelateerd aan ten hoogste één "++(unCap.name.source) d++"."
>   | null ([Uni,        Sur] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("ten hoogste één "++(unCap.name.target) d)
>                                                    ++", terwijl elke "++(unCap.name.target) d++" is gerelateerd aan tenminste één "++(unCap.name.source) d++"."
>   | null ([    Tot,Inj    ] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("tenminste één "++(unCap.name.target) d)
>                                                    ++", terwijl elke "++(unCap.name.target) d++" is gerelateerd aan ten hoogste één "++(unCap.name.source) d++"."
>   | null ([    Tot,    Sur] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("tenminste één "++(unCap.name.target) d)
>                                                    ++" en elke "++(unCap.name.target) d++" is gerelateerd aan tenminste één "++(unCap.name.source) d++"."
>   | null ([        Inj,Sur] >- multiplicities d) = "Er is precies één "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "
>                                                    ++applyM d "b" "a"++"."
>   | null ([            Sur] >- multiplicities d) = "Er is tenminste één "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "
>                                                    ++applyM d "b" "a"++"."
>   | null ([        Inj    ] >- multiplicities d) = "Er is hooguit één "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "
>                                                    ++applyM d "b" "a"++"."
>   | null ([    Tot        ] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("ten minste een "++(unCap.name.target) d)++"."
>   | null ([Uni            ] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("nul of één "++(unCap.name.target) d)++"."
>   | otherwise                                    = applyM d ("een "++(unCap.name.source) d) ("een "++(unCap.name.target) d) ++"."
>  explainMult thisCtx _ d -- default English
>   | null ([Sym,Asy]         >- multiplicities d) = name d++" is a property of "++(unCap.plural English .name.source) d++"."
>   | null ([Sym,Rfx,Trn]     >- multiplicities d) = name d++" is an equivalence relation on "++(unCap.plural English .name.source) d++"."
>   | null ([Asy,Trn]         >- multiplicities d) = name d++" is an ordering relation on "++(unCap.plural English .name.source) d++"."
>   | null ([Uni,Tot,Inj,Sur] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("exactly one "++(unCap.name.target) d)
>                                                    ++" and vice versa."
>   | null ([Uni,Tot,Inj    ] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("exactly one "++(unCap.name.target) d)
>                                                    ++", but not for every "++(unCap.name.target) d++" there must be a "++(unCap.name.source) d++"."
>   | null ([Uni,Tot,    Sur] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("exactly one "++(unCap.name.target) d)
>                                                    ++", but every "++(unCap.name.target) d++" is related to one or more "++(unCap.plural English .name.source) d++"."
>   | null ([Uni,    Inj,Sur] >- multiplicities d) = "There is exactly one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
>                                                    ++applyM d "b" "a"
>                                                    ++", but not for every "++(unCap.name.source) d++" there must be a "++(unCap.name.target) d++"."
>   | null ([    Tot,Inj,Sur] >- multiplicities d) = "There is exactly one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
>                                                    ++applyM d "b" "a"
>                                                    ++", but every "++(unCap.name.source) d++" is related to one or more "++(unCap.plural English .name.target) d++"."
>   | null ([Uni,Tot        ] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("exactly one "++(unCap.name.target) d)++"."
>   | null ([Uni,    Inj    ] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("at most one "++(unCap.name.target) d)
>                                                    ++" and every "++(unCap.name.target) d++" is related to at most one "++(unCap.name.source) d++"."
>   | null ([Uni,        Sur] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("at most one "++(unCap.name.target) d)
>                                                    ++", whereas every "++(unCap.name.target) d++" is related to at least one "++(unCap.name.source) d++"."
>   | null ([    Tot,Inj    ] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("at least one "++(unCap.name.target) d)
>                                                    ++", whereas every "++(unCap.name.target) d++" is related to at most one "++(unCap.name.source) d++"."
>   | null ([    Tot,    Sur] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("at least one "++(unCap.name.target) d)
>                                                    ++" and every "++(unCap.name.target) d++" is related to at least one "++(unCap.name.source) d++"."
>   | null ([        Inj,Sur] >- multiplicities d) = "There is exactly one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
>                                                    ++applyM d "b" "a"++"."
>   | null ([            Sur] >- multiplicities d) = "There is at least one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
>                                                    ++applyM d "b" "a"++"."
>   | null ([        Inj    ] >- multiplicities d) = "There is at most one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
>                                                    ++applyM d "b" "a"++"."
>   | null ([    Tot        ] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("at least one "++(unCap.name.target) d)++"."
>   | null ([Uni            ] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("zero or one "++(unCap.name.target) d)++"."
>   | otherwise                                    = applyM d ("a "++(unCap.name.source) d) ("a "++(unCap.name.target) d) ++"."

>  lglos language ctx = ltglos (name ctx) (typology (isa ctx)) language ctx
>  ltglos cname typ language ctx@(Ctx nm on isa world dc ms cs ks)
>    = (chain "\n". filter (not.null))
>      (if language==Dutch then 
>      [ "\\newcommand{\\mulF}{MultiF{\\it it}}"
>      , "\\title{Woordenlijst voor "++addSlashes cname++"}"
>      , "\\maketitle"
>      , if null cs then "" else
>        latex "longtable" ["{|p{4cm}|p{10cm}|}\\hline"]
>             (chain "\n" ["\\bf "++nm++" & "++addSlashes def++
>                          (if null ref then "" else "~\\cite{"++ref++"}")++
>                          "\\\\\n\\hline"
>                         | Cd pos nm def ref<-conceptdefs ctx])
>      , if null cList then "" else
>        if length cList==1 then "\tHet concept "++idName(head cList)++" heeft geen tekstuele definitie in sectie \\ref{typology"++cname++"}." else
>        "\tDe volgende concepten zijn niet opgenomen in de woordenlijst: "++commaNL "en" (map idName (sord' name cList))++"."
>      , "\\bibliographystyle{plain}"
>      , "\\bibliography{"++lname ctx++"}"
>      , "\\label{bibliography"++lname ctx++"}"
>      ] else if language==English then
>      [ "\\title{Design of "++addSlashes cname++"}"
>      , "\\maketitle"
>      , latexChapter "Glossary" ("typology"++cname)
>      , if null cs then "" else
>        latex "longtable" ["{|p{4cm}|p{10cm}|}\\hline"] (chain "\n" ["\\bf "++nm++" & "++addSlashes def++"~\\cite{"++ref++"}\\\\\n\\hline"
>                          | Cd pos nm def ref<-conceptdefs ctx])
>      , if null cList then "" else
>        if length cList==1 then "\tThe concept "++idName(head cList)++" has no textual definition in the glossary (section \\ref{typology"++cname++"})." else
>        "\tThe following concepts are not described in the glossary: "++commaEng "and" (map idName (sord' name cList))++"."
>      , "\\bibliographystyle{plain}"
>      , "\\bibliography{"++lname ctx++"}"
>      , "\\label{bibliography"++lname ctx++"}"
>      ] else [] )
>      where
>       cList = concs ctx>-rd [C nm (==) []| Cd _ nm _ _<-conceptdefs ctx]
>--       nav :: Classification Concept
>--       nav  = sortCl before (Cl (Anything (genE ctx)) (makeTrees typ))
>       mms  = declarations ctx
>--       degree c = length [m | m<-mms, source m==c || target m==c]
>--       c `before` c' = degree c > degree c'
>--       caps :: Classification Concept -> [String]
>--       caps (Cl c cls) = ["\\disjn{"++chain ", " (map (lshow language.root) cls)++"}"| length cls>1] ++ (concat . map capss) cls ++
>--                         concat (map caps cls)
>--       capss (Cl g cls) = ["\\super{"++lshow language g++"}{"++lshow language s++"}" | s<-map root cls]


>  instance Identified (Typology a) where
>   name typ = ""

> -- lname and clname clean strings
>  lname :: Identified a => a -> String
>  lname  = clname . name
>  clname str = [if isAlphaNum c then c else if c `elem` "/\\" then c else '_'| c<- str]

>  lIntro :: Lang -> String
>  lIntro language
>    = chain "\n"
>        [ "\\documentclass[10pt,a4paper]{report}"
>        , if language==Dutch then "\\usepackage[dutch]{babel}" else ""
>        , "\\parskip 10pt plus 2.5pt minus 4pt  % Extra vertical space between paragraphs."
>        , "\\parindent 0em                      % Width of paragraph indentation."
>        , "\\usepackage{graphicx}"
>        , "\\usepackage{amssymb}"
>        , "\\usepackage{amsmath}"
>--        , "\\usepackage{zed-csp}"
>        , "\\usepackage{longtable}"
>        , "\\def\\id#1{\\mbox{\\em #1\\/}}"
>        , "\\def\\define#1{\\label{dfn:#1}{\\em #1}}"

        , "\\newcommand{\\source}[1]{\\hbox{\\id{source}(#1)}}}"
        , "\\newcommand{\\target}[1]{\\id{target}(#1)}"
        , "\\newcommand{\\functional}[1]{\\id{univalent}(#1)}"
        , "\\newcommand{\\total}[1]{\\id{total}(#1)}"
        , "\\newcommand{\\surjective}[1]{\\id{surjective}(#1)}"
        , "\\newcommand{\\injective}[1]{\\id{surjective}(#1)}"
        , "\\newcommand{\\symmetric}[1]{\\id{symmetric}(#1)}"
        , "\\newcommand{\\antisymmetric}[1]{\\id{antisymmetric}(#1)}"
        , "\\newcommand{\\transitive}[1]{\\id{transitive}(#1)}"
        , "\\newcommand{\\reflexive}[1]{\\id{reflexive}(#1)}"

>        , "\\newcommand{\\iden}{\\mathbb{I}}"
>        , "\\newcommand{\\ident}[1]{\\mathbb{I}_{#1}}"
>        , "\\newcommand{\\full}{\\mathbb{V}}"
>        , "\\newcommand{\\fullt}[1]{\\mathbb{V}_{[#1]}}"
>        , "\\newcommand{\\relAdd}{\\dagger}"
>        , "\\newcommand{\\flip}[1]{{#1}^\\smallsmile} %formerly:  {#1}^\\backsim"
>        , "\\newcommand{\\kleeneplus}[1]{{#1}^{+}}"
>        , "\\newcommand{\\kleenestar}[1]{{#1}^{*}}"
>        , "\\newcommand{\\cmpl}[1]{\\overline{#1}}"
>        , "\\newcommand{\\rel}{\\times}"
>        , "\\newcommand{\\compose}{;}"
>        , "\\newcommand{\\subs}{\\vdash}"
>        , "\\newcommand{\\fun}{\\rightarrow}"
>        , "\\newcommand{\\isa}{\\sqsubseteq}"
>        , "\\newcommand{\\N}{\\mbox{\\msb N}}"
>        , "\\newcommand{\\disjn}[1]{\\id{disjoint}(#1)}"
>        , "\\newcommand{\\fsignat}[3]{\\id{#1}:\\id{#2}\\mbox{$\\rightarrow$}\\id{#3}}"
>        , "\\newcommand{\\signat}[3]{\\mbox{${#1}_{[{#2},{#3}]}$}}"
>        , "\\newcommand{\\declare}[3]{\\id{#1}:\\id{#2}\\mbox{$\\times$}\\id{#3}}"
>        , "\\newcommand{\\fdeclare}[3]{\\id{#1}:\\id{#2}\\mbox{$\\fun$}\\id{#3}}"]

>  lglossary :: Lang -> Context -> IO ()
>  lglossary language context
>    = putStr ("Creating glossary for "++name context++" towards LaTeX.\n")   >>
>      laGen ("gloss"++lname context)
>       (lIntro language++concat
>        [ "\n\\begin{document}"
>        , if language==English then "\n" else "\n\\selectlanguage{dutch}\n"
>        , chain "\n" [lglos language c| c<-(rd' name . preCl . Cl context . wrld) context]
>        , "\n\\end{document}"
>        ])


>  lprint :: Context -> Lang -> [Fspec] -> String -> IO ()
>  lprint context language spec filename
>    = putStr ("Analyzing "++name context++" towards LaTeX.\n")   >>
>      laGen filename
>       (lIntro language++concat
>        [ "\n\\begin{document}"
>        , if language==English then "\n" else "\n\\selectlanguage{dutch}\n"
>        , chain "\n" [lshow language c| c<-(rd' name . preCl . Cl context . wrld) context]
>        , "\n\\end{document}"
>        ])


Latex markup for Z-schema:

\begin{schema}{Workflow}
  \concept{Procedure}:\power\concept{Object}\\
  \concept{Step}:\power\concept{Object}\\
  \concept{Group}:\power\concept{Object}\\
  \declaration{in}:\concept{Step}\fun\concept{Procedure}\\
  \declaration{follows}:\concept{Step}\rel\concept{Step}\\
  \declaration{performs}:\concept{Group}\pinj\concept{Step}\\
\where
  \declaration{follows}\comp\declaration\subs\declaration{in}
--flip is \inv
\end{schema}

lpattern gets the complete typology of the context, in order to produce the right generic concepts (powersets).

>  latexSchema :: String -> [String] -> String
>  latexSchema nm body = latex "schema" ["{"++nm++"}"] (chain "\\\\\n" body)

>  lschema cname (Typ pths) language pat@(Pat nm rs gen pms cs ks)
>   = if null rs then "void" else
>     (latexSchema nm . filter (not.null))
>     [ {- "  "++cname++" Concepts"
>     , "\\where"
>     , -} chain "\n" ["  "++(ltshow cname (Typ pths) language.assemble.normRule) r++"\\\\" | r <- rules pat]
>     , chain "\n" ["  "++(ltshow cname (Typ pths) language.assemble.normRule) s++"\\\\" | s <- specs pat]
>     ] ++ "\n" ++
>     (chain "\n\n" . map (addSlashes.explain) . rules) pat
>     where
>      parents c = f [C (name p) (==) []| [p,s]<-pths, name s==name c]
>      f []  = "Anything"
>      f [c] = concat [if c==' ' then "\\ " else [c]| c<-name c]
>      f cs  = "("++chain "\\cup" [concat [if c==' ' then "\\ " else [c]| c<-name c] | c<-cs]++")"


>-- TODO: kijk naar de definities van rules en specs (in CC_aux) om dubbele code te vermijden
>  contDef cname typ language pat@(Pat nm rs gen pms cs ks)
>   = lschema cname
>             typ language
>             (Pat (nm++" "++show (length (rules pat)+1))
>                  [Ru 'E' (F [Tm m]) pos expr cpu "" sgn nr pn
>                  | Gc pos m expr cpu sgn nr pn<-specs pat] [] [] [] [])

>  instance LATEX Prop where
>   lshow language Uni = if language==Dutch then "univalent"    else "univalent"
>   lshow language Inj = if language==Dutch then "injectief"    else "injective"
>   lshow language Sur = if language==Dutch then "surjectief"   else "surjective"
>   lshow language Tot = if language==Dutch then "totaal"       else "total"
>   lshow language Sym = if language==Dutch then "symmetrisch"  else "symmetric"
>   lshow language Asy = if language==Dutch then "antisymmetrisch" else "antisymmetric"
>   lshow language Trn = if language==Dutch then "transitief"   else "transitive"
>   lshow language Rfx = if language==Dutch then "reflexief"    else "reflexive"   

>  instance (Identified a,LATEX a) => LATEX [a] where
>   lshow language xs = chain "\n\n" [lshow language x| x<-xs]

>  idName :: Identified a => a -> String
>  idName c = idNam (name c)
>  idNam c = "\\id{"++concat [if c `elem` [' ','_'] then "\\ " else [c]| c<-firstCaps c]++"}"

>  instance LATEX Declaration where
>   lshow language mm@(Sgn _ _ _ _ _ _ _ _ _ _ _)
>    = if language==Dutch then
>       "\\label{rel:"++firstCaps (lname mm++lname (source mm)++lname (target mm))++"}\n"++
>              wrapMath(idName mm++":: "++ lsign mm)++"\\\\\n"++
>              (if null ps then "" else
>               if null ([Uni,Tot]>-ps) -- zo ja dan is het een functie
>                then if null pfs
>                     then idName mm++" is een functie. "
>                     else "Deze functie is "++commaNL "en" [lshow language p| p<-pfs]++".\\\\\n"
>                else "Deze relatie is " ++ commaNL "en" [lshow language p| p<-ps]++".\\\\\n") ++
>              (if null cs
>               then "Betekenis: "++applyMLatex mm ("\\langle"++lshow language(source mm)++"\\rangle") ("\\langle"++lshow language(target mm)++"\\rangle")++"."
>               else "Als bijvoorbeeld (`"++head(head cs)++"', `"++last(head cs)++"') in de relatie "++idName mm++" voorkomt, dan betekent dit: "++applyMLatex mm ("`"++head(head cs)++"'") ("`"++last(head cs)++"'"))++"."
>      else if language==English then
>       "\\label{rel:"++firstCaps (lname mm++lname (source mm)++lname (target mm))++"}\n"++
>              wrapMath(idName mm++":: "++ lsign mm)++"\\\\\n"++
>              (if null ps then "" else
>               if null ([Uni,Tot]>-ps) -- zo ja dan is het een functie
>                then if null pfs
>                     then idName mm++" is a function. "
>                     else "This function is "++commaEng "and" [lshow language p| p<-pfs]++".\\\\\n"
>                else "This relation is " ++ commaEng "and" [lshow language p| p<-ps]++".\\\\\n") ++
>              (if null cs
>               then "Meaning: "++applyMLatex mm ("\\langle"++lshow language(source mm)++"\\rangle") ("\\langle"++lshow language(target mm)++"\\rangle")++"."
>               else "If, for example (`"++head(head cs)++"', `"++last(head cs)++"') occurs in relation "++idName mm++", this means: "++applyMLatex mm ("`"++head(head cs)++"'") ("`"++last(head cs)++"'"))++"."
>      else ""
>      where cs=contents mm
>            ps=multiplicities mm; pfs = ps>-[Uni,Tot]
>   lshow language mm@(Isn _ _) = ""

>  instance LATEX Concept where
>   lshow language c = idName c

>  instance LATEX Morphism where
>   lshow language m
>           | isIdent m = "\\ident{"++name (source m)++"}"
>           | isNot m   = "\\cmpl{\\ident{"++name (source m)++"}}"
>           | otherwise = if inline m then idName m else "\\flip{"++idName m++"}"

>  lsign (Sgn nm d c ps _ _ _ _ _ _ _)
>                      | m Uni&& m Inj && m Sur && m Inj = a++"\\rel"++b
>                      | m Uni&& m Tot                   = a++"\\rightarrow"++b
>                      | otherwise                       = a++"\\rel"++b
>        where m e = e `elem` ps; a=idName d; b=idName c

>{- if used in Zed context, more specific arrows are:
>                      | not (m Uni)         = a++"\\rel"++b
>                      | m Inj&&m Tot&&m Sur = a++"\\bij"++b
>                      | m Inj&&m Tot        = a++"\\inj"++b
>                      | m Inj               = a++"\\pinj"++b
>                      |        m Tot&&m Sur = a++"\\surj"++b
>                      |        m Tot        = a++"\\rightarrow"++b
>                      |               m Sur = a++"\\psurj"++b
>                      | otherwise           = a++"\\pfun"++b
>        where m e = e `elem` ps; a=idName d; b=idName c -}

  instance (Eq a, Identified a, LATEX a) => LATEX (Typology a) where
   lshow language ts = lshow language (Cl none (makeTrees ts))

>  instance (Eq a, Show a, Identified a) => LATEX (Classification a) where
>   lshow language cl
>    = recur cl
>      where
>       recur (Cl r cls)
>        = name r++(latexDotted . map recur . sort' name) cls

       sort [] = []
       sort (e:cls) = sort [c| c<-cls, name c<name e] ++ [e] ++
                      sort [c| c<-cls, name c>=name e]

>  instance LATEX Expression where
>   lshow language (Tm m)   = lshow language m
>   lshow language (F ts)   = chain "\\compose" (map (lshow language) ts)
>   lshow language (Fd ts)  = chain "\\relAdd" (map (lshow language) ts)
>   lshow language (Fu fs)  = chain "\\cup" (map (lshow language) fs)
>   lshow language (Fi fs)  = chain "\\cap" (map (lshow language) fs)
>   lshow language (Cp e)   = "\\cmpl{"++lshow language e++"}"
>   lshow language (K0 e)   = "\\kleenestar{"++lshow language e++"}"
>   lshow language (K1 e)   = "\\kleeneplus{"++lshow language e++"}"

>  instance LATEX Rule where
>   ltshow cname typ language (Gc pos m expr cpu sgn nr pn) = ltshow cname typ language (Ru 'E' (F [Tm m]) pos expr cpu "" sgn nr pn)
>   ltshow cname typ language r = (ltshow cname typ language.assemble.normRule) r
>   lshow language r | ruleType r=='I' && fEmpty (antecedent r) = lshow language (consequent r)
>                    | ruleType r=='I' && fEmpty (consequent r) = lshow language (Cp (antecedent r))
>                    | ruleType r=='I'                          = lshow language (antecedent r)++"\\subs"++lshow language (consequent r)
>                    | ruleType r=='E' = lshow language (antecedent r)++"="++lshow language (consequent r)
>                    | ruleType r=='A' = lshow language (consequent r)
>                    | otherwise       = lshow language (antecedent r)++"="++lshow language (consequent r)

  instance LATEX PthExpr where
   lshow (E ms)     = chain "\\compose" (map (lshow language) ms)
   lshow language (A ms ms') = lshow language ms ++ "\\cap" ++ lshow language ms'

>  instance LATEX PredLogic where
>   lshow language x = predLshow ("\\forall ", "\\exists ", implies, "\\Leftrightarrow", "=", "\\not =", "\\vee", "\\wedge", "\\neg", rel, fun, mathVars, "\\\\\n  ", "\\ ") x
>                      where rel m lhs rhs = lhs++"\\ "++idName m++"\\ "++rhs
>                            fun m x = idName m++"("++x++")"
>                            implies antc cons = antc++"\\ \\Rightarrow\\ "++cons

>  uName :: [String] -> String -> String
>  uName nms n = concat [v| (nm,v)<-zip nms' vs, n==nm]
>   where nms' = rd nms
>         vs = f (map (map toLower.take 1) nms')
>         f (v:vs)          = if v `elem` vs then f (g v (map show [1..]) (v:vs)) else v: f vs
>         f []              = []
>         g e (i:is) (v:vs) = if v==e then (v++i): g e is vs else v: g e (i:is) vs
>         g e _ []          = []

Basic LATEX markup
TODO: complete all accents and test

>  addSlashes (' ': '\"': cs) = " ``"++addSlashes cs
>  addSlashes ('\"': ' ': cs) = "'' "++addSlashes cs
>  addSlashes ('\\': cs) = "\\\\"++addSlashes cs
>  addSlashes ('_': cs) = "\\_"++addSlashes cs
>  addSlashes ('&': cs)  = "\\&"++addSlashes cs
>  addSlashes ('é': cs) = "\\'e"++addSlashes cs
>  addSlashes ('è': cs) = "\\`e"++addSlashes cs
>  addSlashes ('ï': cs) = "\\\"{\\i}"++addSlashes cs
>  addSlashes ('á': cs) = "\\'a"++addSlashes cs
>  addSlashes ('à': cs) = "\\`a"++addSlashes cs
>  addSlashes ('ó': cs) = "\\'o"++addSlashes cs
>  addSlashes ('ò': cs) = "\\`o"++addSlashes cs
>  addSlashes (c: cs)    = c:addSlashes cs
>  addSlashes _          = ""

>  wrapMath str = "$"++str++"$"

>  latexCenter = latex "center" []
>  latexFigure = latex "figure" ["[htb]"]
>  latexFigureHere = latex "figure" ["[h]"]

>  latex :: String -> [String] -> String -> String
>  latex command params content
>   = "\\begin{"++command++"}"++chain "," params++"\n"++content++"\n\\end{"++command++"}"

>  latexSubsection title reference
>   = "\n\\subsection*{"++title++"}\n" ++"\\label{ssct:"++reference++"}\n"

>  latexSection title reference
>   = "\n\\section{"++title++"}\n" ++"\\label{sct:"++reference++"}\n"

>  latexChapter title reference
>   = "\n\\chapter{"++title++"}\n" ++"\\label{chp:"++reference++"}\n"

>  latexDotted ls
>   = if null ls then "" else
>     "\n"++chain "\n" (["\\begin{itemize}"]++["\\item "++l|l<-ls]++["\\end{itemize}"])

>  latexEnumerate ls
>   = if null ls then "" else
>     chain "\n" (["\\begin{enumerate}"]++["\\item "++l|l<-ls]++["\\end{enumerate}"])

>  projectClassic :: Context -> [Fspec] -> Lang -> String
>  projectClassic context fs language
>   = "ID;Task_Name;Duration;Type;Outline_Level;Predecessors;Milestone;Rollup\n"++
>     (task2proj.spec2task context English) fs

>  data Task = Tsks String   -- name of group of tasks
>                   String   -- affix
>                   [String] -- dependencies (refer by name)
>                   [Task]
>            | Tsk  String   -- name of task
>                   String   -- duration
>                   String   -- affix
>                   [String] -- dependencies (refer by name)
>            | Mils String   -- name of milestone
>                   [String] -- dependencies (refer by name)

>  render tasks
>   = [ chain ";" [id nm,nm,dur,typ,outl,dependencies deps affix,ms,rup]
>     | (nm,dur,typ,outl,affix,ms,rup,deps)<-tasks]
>     where id t = if null result then error ("No "++t++" known in task list") else
>                  if length result>1 then error ("multiple tasks named "++t++" in task list")else
>                  head result
>                  where result = [show i| (i,(nm,dur,typ,outl,affix,ms,rup,deps))<-zip [1..] tasks,t==nm]
>           dependencies deps affix
>            = (if length deps>1 then "\""++chain ";" (map id deps)++"\"" else concat (map id deps))++affix

>  type NSpace = [(Int,String)]
>  task2proj t = chain "\n" result
>   where
>    result = task 0 1 (milSpace 1 t) t
 
>    task :: Int->Int->NSpace->Task->[String]
>    task indent i namespace (Tsk taskname dur affix deps)
>     = [ chain ";" [show i,taskname,dur,"Fixed Units",show indent,render namespace deps++affix,"No","No"] ]
>    task indent i namespace (Mils taskname deps)
>     = [ chain ";" [show i,taskname,"0 days","Fixed Units",show indent,render namespace deps,"Yes","No"] ]
>    task indent i namespace t@(Tsks taskname affix deps ts)
>     = [ chain ";" [show i,taskname,"","Fixed Duration",show indent,render namespace deps++affix,"No","Yes"] ]++
>       tsk (nms (i+1) ts++namespace) (i+1) ts
>       where tsk namespace i (t:ts) = task (indent+1) i namespace t ++ tsk namespace (i+count t) ts
>             tsk namespace i [] = []
>    nm (Tsk taskname dur affix deps) = taskname
>    nm (Mils taskname deps)          = taskname
>    nm (Tsks taskname affix deps ts) = taskname
>    count (Tsk taskname dur affix deps) = 1
>    count (Mils taskname deps)          = 1
>    count (Tsks taskname affix deps ts) = 1+sum (map count ts)
>    nms :: Int->[Task]->NSpace
>    nms i []     = []
>    nms i (t:ts) = (i,nm t): nms (i+count t) ts
>    milSpace i (Tsk taskname dur affix deps) = []
>    milSpace i (Mils taskname deps)          = [(i,taskname)]
>    milSpace i (Tsks taskname affix deps (t:ts)) = milSpace i t++milSpace (i+count t) (Tsks taskname affix deps ts)
>    milSpace i (Tsks taskname affix deps []) = []
>    render :: NSpace->[String]->String
>    render namespace deps
>     = if length deps>1 then "\""++chain ";" (map lookup deps)++"\"" else concat (map lookup deps)
>       where lookup nm
>              = head ([show i| (i,n)<-namespace,n==nm]++error ("Name \""++nm++"\" not found in function task2proj."))
>
>  showOO = objOrShow.assemble.normRule
>
>  spec2fp context lang fspcs
>   = "\n\\begin{table}[htb]\\begin{center}\n"++
>     "\\begin{tabular}{|l|r|}\\hline &"++str2++"\\\\\\hline\n  "++   -- str2 = "Function Point Analysis"
>     (chain "\\\\\n" . map (\cl -> fst (head cl)++"&"++show (sum (map snd cl))) . eqCl fst) us++
>     "\\\\\\hline\n\\end{tabular}\n"++
>     "\\caption{"++str1++"}\\label{tab:FPA}"++
>     "\n\\end{center}\\end{table}\n"++
>     "\n\t"++str0++" \\ref{tab:FPA}."++   -- str0 = "The results of a global function point analysis conformant to IFPUG principles are given in table"
>     "\n\t"++str3++" "++(show.sum.map snd) us++" "++str2++"."
>     where
>      us= [ (unm, fps)
>          | Fspc pat units<-fspcs, Uspc unm pat car specs<-units
>          , fps<-[sum ([fPoints fpa| Sspc nm fpa input output rs pre post<-specs]++
>                       [fPoints fpa| (c,_,fpa,_,_)<-car])
>                 ], fps>0
>          ]
>      str0 | lang==English = "The results of a global function point analysis conformant to IFPUG principles are given in table"
>           | lang==Dutch   = "De resultaten van een globale functiepunt analyse conform de richtlijnen van IFPUG zijn weergegeven in tabel"
>      str1 | lang==English = "Function Point Analysis"
>           | lang==Dutch   = "Functiepunt Analyse"
>      str2 | lang==English = "function points"
>           | lang==Dutch   = "functiepunten"
>      str3 | lang==English = "This yields a total of"
>           | lang==Dutch   = "Het totaal is"
>  spec2task context English fspcs
>   = Tsks "Project" "" [] [design fspcs,tools fspcs,build fspcs,implement fspcs]
>     where
>      design fs
>       = Tsks "Design phase" "" []
>         [ Tsk "select design & build teams" "1 mon" "" []
>         , Tsk "Requirements elicitation" "2 mons" "" ["select design & build teams"]
>         , Tsk "Approve requirements" "1 mon" "" ["Requirements elicitation"]
>         , Tsk "Definition study" "2 mons" "" ["Requirements elicitation"]
>         , Mils "Concept" ["Definition study"]
>         , Tsk "Approve concept" "1 mon" "" ["Concept"]
>         , Tsk "Process modeling" "4 mons" "" ["Concept"]
>         , Tsk "Data definition" "2 mons" "" ["Concept"]
>         , Tsk "Approve data definition" "1 mon" "" ["Data definition"]
>         , Tsk "Functional Specification" "4 mons" "" ["Data definition"]
>         , Mils "Functional Spec" ["Functional Specification"]
>         , Tsk "Approve func. spec." "1 mon" "" ["Functional Spec"]
>         ]
>      tools fs
>       = Tsks "Tools phase" "" ["Concept"]
>         [ Tsk "Selection" "1 mon" "" []
>         , Tsk "Tendering & acquistion" "3 mons" "" ["Selection"]
>         , Tsk "installation" "1 mon" "" ["Tendering & acquistion"]
>         ]
>      build fs
>       = Tsks "Building phase" "" ["Design phase","Tools phase"]
>         [ Tsks "process logic" "" []
>               [ Tsk "build" "3 mons" "" []
>               , Tsk "unit test" "2 mons" "FF+1 mon" ["build"]
>               ]
>         , Tsks "user interfaces" "" []
>               [ Tsk "build" "9 mons" "" []
>               , Tsk "unit test" "8 mons" "FF+1 mon" ["build"]
>               ]
>         , Tsks "service layer & database" "" ["Functional Spec"]
>               [ Tsk "build database" "2 mons" "" []
>               , Tsk "build services" "11 mons" "SS+1 mon" ["build database"]
>               , Tsk "unit test" "7 mons" "FF+1 mon" ["build services"]
>               ]
>         , Tsk "Integration test" "3 mons" "" ["process logic","user interfaces","service layer & database"]
>         , Tsk "Acceptance test" "1 mon" "" ["Integration test"]
>         ]
>      implement fs
>       = Tsks "Implementation phase" "" []
>         [ Tsk "Preparation" "6 mons" "FF" ["Building phase"]
>         , Tsk "Roll-out" "2 mons" "" ["Preparation"]
>         , Tsk "user training" "2 mons" "FF-2 wks" ["Roll-out"]
>         , Tsk "Transfer" "1 mon" "" ["Roll-out"]
>         , Tsk "after care" "3 mons" "" ["Transfer"]
>         ]
>      (entities, relations, ruls) = erAnalysis context
