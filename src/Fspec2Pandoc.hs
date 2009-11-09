{-# OPTIONS_GHC -Wall #-}
--TODO -> May be we can look at GetText function for help with internationalization. Brian O'Sullivan is working (has started) on an internationalization library. Maybe some day...
--TODO -> Block: Header Int [Inline] - Int indicates level of header. If I look at pandoc code TexInfo.hs blockToTexinfo ln.208 I would expect chapter,section,sub,subsub respectively. But I get section,sub,subsub,plain text respectively. So now I've written chapters as 0 setting a [Inline] -> [Tex "\\chapter{...}"]. I do not know yet how this relates to other formats like rtf.
module Fspec2Pandoc (fSpec2Pandoc,render2Pandoc,writeRTF,writeLaTeX)
where
import Adl
import FspecDef
import Text.Pandoc  
import Strings      (remSpaces)
  --Als de compiler hierover struikelt, dan moet je pandoc installeren. Dat is overigens in de volgende 3 stappen:
                          -- 1) Eerst installeer je Cabal (zie http://www.haskell.org/cabal/) en dan roep je op je command line: 
                          -- 2) cabal-install pandoc  (onder windows: cabal install pandoc)
                          -- 3) Het kan zijn dat dit nog niet werkt, zie http://groups.google.com/group/pandoc-discuss/browse_thread/thread/a8fc3a627aeec7f2
                          --    als dat het geval is, kan deze module worden overruled in Generators.hs                                 
import Version        (versionbanner)
import Languages      (Lang(..))
import Options        (Options(..),FspecFormat(..))
import Rendering.AdlExplanation
import Rendering.ClassDiagram

--import Statistics

--DESCR ->
--The functional specification starts with an introduction
--The second chapter defines the functionality of the system by datasets and rules.
--Datasets are specified through PLUGS in ADL. The dataset is build around one concept, 
--also called the theme. Functionalities defined on the theme by one or more plugs are
--described together with the rules that apply to the dataset. Rules not described by
--the dataset are described in the last section of chapter 2.
--The third chapter is intended for the analyst. It contains all the rules mentioned in
--natural language in the second chapter. It presents the trace from natural language
--to the formal rule.
--The fourth chapter presents a datamodel together with all the multiplicity rules.
--The following chapters each present a SERVICE
--The specification end with a glossary.
render2Pandoc :: Options -> String -> Pandoc -> String
render2Pandoc flags customheader pandoc = case fspecFormat flags of
   FPandoc -> prettyPandoc pandoc
   FWord -> let wropts = defaultWriterOptions{writerStandalone=True}
            in writeRTF wropts pandoc
   FLatex -> let wropts = defaultWriterOptions{writerStandalone=True, writerHeader=customheader, writerTableOfContents=True,writerNumberSections=True}
             in writeLaTeX wropts pandoc
   FHtml -> writeHtmlString defaultWriterOptions pandoc
   FUnknown -> prettyPandoc pandoc --REMARK -> will not occur at time of implementation because of user IO error.

chpintrolabel :: String
chpintrolabel="chpIntro"
chpdplabel :: String
chpdplabel="chpDesignPrinciples"
chpcalabel :: String
chpcalabel="chpConceptualAnalysis"
chpdalabel :: String
chpdalabel="chpDataAnalysis"
--chpgloslabel :: String
--chpgloslabel="chpGlossary"
fSpec2Pandoc :: Fspc -> Options -> Pandoc
fSpec2Pandoc fSpec flags = Pandoc meta docContents
    where meta = Meta title authors date
          title = [Str (case (language flags) of
                         Dutch   -> "Functionele Specificatie van "
                         English -> "Functional Specification of "
                       )]
                ++[Quoted  SingleQuote [Str (name fSpec)] ] 
          authors = [versionbanner]
          date = show(genTime flags)
          
          docContents = (introduction level fSpec flags)
                     ++ (designPrinciples level fSpec flags)
                     ++ (conceptualAnalysis level fSpec flags)
                     ++ (dataAnalysis level fSpec flags)
                     ++ [chpbs | svc<-FspecDef.services fSpec, chpbs<-servicechap level fSpec flags svc]
                     ++ (glossary level fSpec flags)
          level = 0 --0=chapter, 1=section, 2=subsection, 3=subsubsection, _=plain text
------------------------------------------------------------                
introduction :: Int -> Fspc -> Options ->  [Block]
introduction lev fSpec flags = header ++ introContents (language flags)
    where 
        header = labeledHeader lev chpintrolabel (case (language flags) of
                                       Dutch   ->  "Inleiding"   
                                       English ->  "Introduction"
                                   )

        introContents Dutch = 
         [Para 
                [Str "Dit document definieert de servicelaag van een systeem genaamd "
                , Quoted  SingleQuote [Str (name fSpec)] 
                , Str ". Het definieert infrastructuur-services in een systeem waarin mensen en applicaties samenwerken "
                , Str "om afspraken na te leven die gelden in de context van "
                , Quoted  SingleQuote [Str (name fSpec)] 
                , Str ". Deze afspraken worden weergegeven door bedrijfsregels. "
                , Str "Deze regels staan beschreven in hoofdstuk "
                , xrefReference chpdplabel
                , Str ", geordend op thema. "
                , Str "Een gegevensanalyse volgt in hoofdstuk "
                , xrefReference chpdalabel
                , Str ". In de daarop volgende hoofdstukken is elk thema "
                , Str "uitgewerkt in definities van services. "
                , Str "Deze services ondersteunen gezamenlijk alle afspraken uit hoofdstuk "
                , xrefReference chpdplabel
                , Str ". Deze ondersteuning bestaat uit het voorkomen dat een afspraak wordt overtreden, "
                , Str "of het signaleren van overtredingen (opdat mensen kunnen ingrijpen), "
                , Str "of het herstellen van een regel (door automatische acties op de database uit te voeren)."]]

        introContents English = 
         [Para
                [Str "This document defines the service layer of a system called "
                , Quoted  SingleQuote [Str (name fSpec)] 
                , Str "It defines infrastructural services in a system in which people and applications collaborate"
                , Str "to maintain agreements and commitments that apply to the context of "
                , Quoted  SingleQuote [Str (name fSpec)] 
                , Str "These agreements and commitments are represented by rules."
                , Str "They are presented in chapter "
                , xrefReference chpdplabel
                , Str ", arranged by theme."
                , Str "A data analysis is presented in chapter "
                , xrefReference chpdalabel
                , Str ". Subsequent chapters elaborate each theme by defining all applicable services."
                , Str "Together, these services support all rules from chapter "
                , xrefReference chpdplabel
                , Str ". This support consists of either preventing that a rule is violated,"
                , Str "signalling violations (for human intervention),"
                , Str "or fixing the content of databases (by automatic actions) to restore a rule."]]  
------------------------------------------------------------
designPrinciples :: Int -> Fspc -> Options ->  [Block]
designPrinciples lev fSpec flags = header ++ dpIntro 
                                   ++ [b|t<-themes fSpec,b<-dpSection t,tconcept t/=Anything]
                                   ++ [b|t<-themes fSpec,b<-remainingrulesSection (trules t),tconcept t==Anything]
  where 
  header :: [Block]
  header = labeledHeader lev chpdplabel (case (language flags) of
                                 Dutch   ->  "Ontwerpregels"   
                                 English ->  "Design Rules"
                             )
  dpIntro :: [Block]
  dpIntro = 
    (case (language flags) of
        Dutch -> [Para
                  [ Str "Dit hoofdstuk definieert de ontwerpregels van "
                  , Quoted  SingleQuote [Str (name fSpec)] 
                  , Str ". Deze regels moeten door de oplossing worden nageleefd. "
                  , Str "Controle daarop vindt plaats door het architectuurteam. "
                  , Str "Tezamen vormen deze regels de architectuur van "
                  , Quoted  DoubleQuote [Str (name fSpec)] 
                  , Str "."]]
        English -> [Para
                     [ Str "This chapter defines de design principles of "
                     , Quoted  SingleQuote [Str (name fSpec)] 
                     , Str ". The implementation must assert these rules. "]
                 ]
     )
  --TODO -> It may be nice to print the class of the dataset from the class diagram
  dpSection :: FTheme -> [Block]
  dpSection t = [Header (lev+1) [Str (name$tconcept t)]] --new section to explain this theme
             ++ [b|f<-tfunctions t, b<-explainFunctionNL f, language flags==Dutch] --explain the functions in the theme
             ++ [Para [Str$explainRule flags r]|r<-trules t] --explanation of all rules in the theme
    where
    listDataset obj = 
                  [BulletList 
                     [[Plain [Str$objnm objat]]|objat<-objats obj]
                  ]
    listKeys keys' = Emph [il|objat<-keys', il<-[Str " ",Str$objnm objat]]
    explainFunctionNL f = case wsaction f of {
         WSCreate -> [Para [Emph [Str$"Nieuw ",Str$name$tconcept t]]]
                   ++ [Para [Str "Voor het aanmaken van een ",Str$name$tconcept t
                            ,Str " moeten de volgende datavelden aangeleverd worden:"]]
                   ++ describemsgs;
         WSRead -> [Para [Emph [Str$"Bekijk "++(name$tconcept t)]]]
                   ++ describereadmsgs;
         WSUpdate -> [Para [Emph [Str$"Bewerk "++(name$tconcept t)]]]
                   ++ [Para [Str "Van een ",Str$name$tconcept t
                            ,Str " kunnen de volgende datavelden gewijzigd worden:"]]
                   ++ describemsgs;
         WSDelete -> [Para [Emph [Str$"Verwijder "++(name$tconcept t)]]]
                   ++ [Para [Str "Als een ",Str$name$tconcept t
                            ,Str " verwijderd wordt, dan worden de volgende datavelden verwijderd:"]]
                   ++ describemsgs}
         where
         describemsgs = [b|obj<-wsmsgin f++wsmsgout f, b<-listDataset obj]
         describereadmsgs = [Para [Str "Een ",Str$name$tconcept t
                                  ,Str " kan geselecteerd worden op basis van "
                                  ,listKeys$wsmsgin f
                                  ,Str "."]|(not.null) (wsmsgin f)]
                         ++ [b|obj<-wsmsgout f
                               , b<-[Para [Str "De volgende datavelden van een ",Str$name$tconcept t
                                          ,Str " kunnen bekeken worden:"]]
                                 ++ listDataset obj
                            ]
  remainingrulesSection :: [Rule] -> [Block]
  remainingrulesSection rs = 
     [Header (lev+1) [Str (case language flags of
                             Dutch   -> "Algemene ontwerpregels"
                             English -> "General designrules"
                          )
                     ]
     ] --new section to explain this theme
     ++ [Para [Str$explainRule flags r]|r<-rs] --explanation of all rules in the theme
     
------------------------------------------------------------
conceptualAnalysis :: Int -> Fspc -> Options ->  [Block]
conceptualAnalysis lev fSpec flags = header ++ caIntro ++ [b|p<-vpatterns fSpec,b<-caSection p]
  where 
  header :: [Block]
  header = labeledHeader lev chpcalabel (case (language flags) of
                                Dutch   ->  "Conceptuele Analyse"   
                                English ->  "Conceptual Analysis"
                               )
  fpalabel = "tableFPA"
  caIntro :: [Block]
  caIntro = 
   (case (language flags) of
      Dutch -> [Para
                 [ Str "Dit hoofdstuk geeft een analyse van de regels uit hoofdstuk "
                 , xrefReference chpdplabel
                 , Str ". Ieder thema in dat hoofdstuk wordt geanalyseerd in termen van relaties "
                 , Str "en elke afspraak krijgt een formele representatie. "
                 , Str "De resultaten van functiepunt analyse staan vermeld in tabel "
                 , xrefReference fpalabel
                 , Str "."]]
      English -> [Para
                  [ Str "This chapter provides an analysis of the principles described in chapter "
                  , xrefReference chpdplabel
                  , Str ". Each section in that chapter is analysed in terms of relations "
                  , Str "and each principle is then translated in a rule. "
                  , Str "The results of function-point analysis is given in table "
                  , xrefReference fpalabel
                  , Str "."]]
   ) ++ fpa2Blocks
  --TODO -> is an fpa on themes correct or should it be on the total fspec, or should it not matter, i.e. is the sum of services in the fspec equivalent to the sum of services of all themes? 
  --Table [Inline] [Alignment] [Double]      [[Block]] [[[Block]]]
  --      Caption  Clm algnmt  rel.clm.width clm hdrs  rows
  fpa2Blocks  = 
       [Table [Str "Function Point Analysis", xrefLabel fpalabel] 
              [AlignLeft, AlignRight] --TODO -> how do I specify drawing of lines?
              [0.25,0.1] --TODO -> can't this be automatic or something
              [[Plain [Space]],[Plain [Str "points"]]] 
              [ [ [Plain [Str (name p)]]
        --        , [Plain [Str (show $ nFpoints t)]] ] 
                  , [Plain [Str "?"]] ] --TODO -> there is a loop in fspc->ftheme->funit->fviewdef&servicespec, coming from adl2fspec (remainingDS & pats)
              | p<-vpatterns fSpec]
       ]  
  caSection :: Pattern -> [Block]
  caSection p = 
   (case (language flags) of
      Dutch -> (if null themerules then [] --nothing to explain for this theme -> skip
           else [Header (lev+1) [Str $ "Regels over " ++ (name p)]] --new section to explain this theme
             ++ [x | (not.null) (concs p),x<-printfigure]
             ++ rules2table)
      English -> (if null themerules then [] --nothing to explain for this theme -> skip
           else [Header (lev+1) [Str $ "Rules about " ++ (name p)]] --new section to explain this theme
             ++ [x | (not.null) (concs p),x<-printfigure]
             ++ rules2table)
    )
    where
    --query copied from FSpec.hs revision 174
    themerules = [r|r<-declaredRules p++signals p, null (cpu r)]
    printfigure = case (language flags) of
      Dutch -> [Para [x | x<-[Str "Zie figuur ", xrefReference figlabel, Str ". "]] ]
            ++ [Plain [x | x<-xrefFigure ("Conceptuele analyse van "++filenm) filenm figlabel ]]
      English -> [Para [x | x<-[Str "See figure ", xrefReference figlabel, Str ". "]] ]
              ++ [Plain [x | x<-xrefFigure ("Conceptual analysis of "++filenm) filenm figlabel ]]
      where filenm = remSpaces (name p)
            figlabel = "figca:" ++ filenm
    rules2table = case fspecFormat flags of
      --REMARK -> pandoc does not support longtable (or something similar?)
      FLatex -> [Plain $ 
                  [ TeX "\\begin{center} \n"
                  , TeX "\\begin{longtable}{|r|p{\\columnwidth}|} \n"
                  , TeX "\\hline \n"
                  ]
               ++ [ inline' | r<-themerules, inline'<-explainCaRule r]
               ++ [ TeX "\\end{longtable} \n"
                  , TeX "\\end{center} \n"
                  ]
                ]            
      _ -> [Table [] 
                  [AlignCenter, AlignLeft] --TODO -> how do I specify drawing of lines?
                  [0.05,0.8] --TODO -> can't this be automatic or something
                  [] 
                  [ [ [Plain [Str $ show (nr r)]]
                      , [Para (explainCaRule r)] ]
                  | r<-themerules]
           ] 
    --query copied from FSpec.hs revision 174   latex "longtable" ["{|r|p{\\columnwidth}|}\\hline"]
    explainCaRule r = [ Str $ show (nr r)
                      , TeX "\n & "
                      , Str (explainRule flags r)] --TODO -> alignment is incorrect
                   ++ printlb
                   ++ [Str "Relations:"]
                   ++ printlb
                   ++ [ TeX " \\( \\begin{array}{rcl} \n"]
                   ++ [ inline' | mph<-morlist r, inline'<-printmphdetail flags mph]
                   ++ [ TeX " \\end{array} \\) "]
                   ++ printlb
                   ++ [Str "Rule:"]
                   ++ printlb
                   ++ (printrule flags r)
                   ++ [ TeX " \\\\ \\hline \n"]
------------------------------------------------------------
--DESCR -> the data analysis contains a section for each class diagram in the fspec
--         the class diagram and multiplicity rules are printed
dataAnalysis :: Int -> Fspc -> Options ->  [Block]
dataAnalysis lev fSpec flags = header ++ daContents
  where 
  header :: [Block]
  header = labeledHeader lev chpdalabel (case (language flags) of
                                     Dutch   ->  "Gegevensanalyse"   
                                     English ->  "Data Analysis"
                                 )
--    fpalabel = "tableFPA2"
  daContents :: [Block]
  daContents = 
 --  (case (language flags) of
 --     Dutch -> [Para
 --                [ Str "De keuzes, zoals beschreven in hoofdstuk "
 --                , xrefReference chpdplabel
 --                , Str " zijn in een gegevensanalyse vertaald naar klassediagram(men)."
 --                , Str "Dit hoofdstuk geeft een uitwerking van de gegevensanalyse in de vorm van functionele specificaties. "
 --                ]]
 --     English -> [] --TODO
 --  )
 --  ++
      [x|(OOclassdiagram{nameandcpts=(fnm,_)})<-classdiagrams fSpec, x<-describeCD (remSpaces fnm)]
      where 
      describeCD cdnm' = 
       (case (language flags) of
          Dutch ->
                [ Header (lev+1) [Str cdnm']
                , Plain$xrefFigure ("Klassediagram van "++cdnm') filenm figlabel
             ]++[ Para [Str d]|d<-themedecls] --explanation of all multiplicities]
          English -> []) --TODO
          where       
          filenm = "CD_"++ cdnm'
          figlabel = "figcd:" ++  cdnm'
          --REMARK -> cdnm' should be implemented as (name pattern) in ClassDiagram.hs
          p = let pats = [pat|pat<-vpatterns fSpec, name pat==cdnm']
              in if length pats==1 then head pats
                 else error $ "Error in Fspec2Pandoc.hs module Fspec2Pandoc function dataAnalysis.daContents.describeCD: "
                           ++ "Pattern names need to be unique."
          --query copied from FSpec.hs revision 174
          themedecls = [explainDecl (language flags) d|d<-ptdcs p,(not.null) (multiplicities d)]
------------------------------------------------------------
servicechap :: Int -> Fspc -> Options -> Fservice ->  [Block]
servicechap lev _ flags svc = header ++ svcContents  --TODO
  where
  svcname = name (fsv_objectdef svc)
  header :: [Block]
  header = labeledHeader lev ("chpSvc"++svcname) ("Service: " ++ svcname)
  svcContents :: [Block]
  svcContents = 
   case (language flags) of
      Dutch -> [ --Para  $
             --     [inline | rl<-frules svc, inline<-printrule flags rl++printlb]
             --  ++ [Str "TRBOUNDARY"]++printlb   
             --  ++ [inline | rl<-trBoundary svc, inline<-[TeX "$ "]++printexpr flags rl++[TeX " $"] ++printlb]
               ]
  --          ++ [sect | meth<-methods svc, sect<-printmethod meth]
      English -> [] --TODO
--  printmethod meth = 
  
------------------------------------------------------------
glossary :: Int -> Fspc -> Options ->  [Block]
glossary _ _ _ = []  --TODO
------------------------------------------------------------

--   xrefChptReference :: String -> [Inline]
--   xrefChptReference myLabel = [TeX ("\\ref{section:"++myLabel++"}")] --TODO werkt nog niet correct
---   xrefTableReference :: String -> [Inline]
--   xrefTableReference myLabel = [TeX ("\\ref{tab:"++myLabel++"}")]
labeledHeader :: Int -> String -> String -> [Block]
labeledHeader 0 lbl str = 
                 [Para [TeX ("\\chapter{"++str++"}")]]
              ++ [Para [xrefLabel lbl]]
labeledHeader lev lbl str =
                 [Header lev ([Str str])]
              ++ [Para [xrefLabel lbl]]
 
xrefReference :: String -> Inline    -- uitbreidbaar voor andere rendering dan LaTeX
xrefReference myLabel = TeX ("\\ref{"++myLabel++"}")
xrefLabel :: String -> Inline        -- uitbreidbaar voor andere rendering dan LaTeX
xrefLabel myLabel = TeX ("\\label{"++myLabel++"}")

--Image [Inline] Target
--      alt.text (URL,title)
xrefFigure :: String -> String -> String -> [Inline]
xrefFigure caption filenm figlabel = 
   [ TeX "\\begin{figure}[h]\n\\begin{center}\n\\scalebox{.3}[.3]{"
   , Image [] (filenm ++ ".png", figlabel)
   , TeX "}\n"
   , TeX ("\\caption{"++caption++"}\n") 
   , xrefLabel (figlabel )
   , TeX "\n\\end{center}\n\\end{figure}"]

addinfix :: Inline -> [[Inline]] -> [Inline] 
addinfix _ [] = [] --tail will not be on empty list
addinfix delim xs = tail [inline' | inlines<-postfix, inline'<-inlines]
   where
   postfix :: [[Inline]] 
   postfix = [delim:x|x<-xs] 

printlb :: [Inline]
printlb = [TeX "\n \\linebreak ", Str "\n"]

--EXTEND -> symbol string must reflect latex symbol identifier
printsymbol :: Options -> String -> Inline
printsymbol flags symb = case fspecFormat flags of
  FLatex -> TeX $ "\\" ++ symb ++ " "
  _ -> case symb of
    "times" -> Str "X"
    "mathbb{I}" -> Str "I"
    "mathbb{V}" -> Str "V"
    "vdash" -> Str "|-"
    "equiv" -> Str "="
    "smile" -> Str "~"
    "cup" -> Str "\\/"
    "cap" -> Str "/\\"
    "dagger" -> Str "!"
    _ -> Str $ "%" ++ symb ++ "%"

--DESCR -> pandoc print functions for Adl data structures
printcompl :: Options -> [Inline] -> [Inline]
printcompl flags inlines = case fspecFormat flags of
  FLatex -> [TeX " \\overline{"] ++ inlines ++ [TeX "} "]
  _ -> (Str "-"):inlines
printmphdetail :: Options -> Morphism -> [Inline]
printmphdetail flags mph = printmphname flags mph
             ++ [TeX " &", Str ":", TeX "& "]
             ++ printtype flags mph
             ++ [TeX " \\\\ ", Str "\n" ]
printmph :: Options -> Morphism -> [Inline]
printmph flags mph = (printmphname flags mph) ++ (printflip flags mph)
printflip :: Options -> Morphism -> [Inline]
printflip flags mph = case mph of
   Mph{} -> if mphyin mph then [] 
            else [ Superscript [TeX "$",printsymbol flags "smile", TeX "$"] ]
   _ -> []
printmphname :: Options -> Morphism -> [Inline]
printmphname flags mph = case mph of
   Mph{} -> [Str (name mph)]
   I{}   -> [printsymbol flags "mathbb{I}", Subscript [TeX "$",Str $ name (mphspc mph), TeX "$"] ]
   V{}   -> [printsymbol flags "mathbb{V}", Subscript ((TeX "$"):(printtype flags mph)++[TeX "$"]) ]
   Mp1{} -> [Str "?"]
printtype :: Options -> Morphism -> [Inline]
printtype flags mph = [ Str $ (name.source) mph
              , printsymbol flags "times", Space
              , Str $ (name.target) mph
              ]
printrule :: Options -> Rule -> [Inline]
printrule flags r = case r of
   Ru {} -> case rrsrt r of 
     Implication -> [TeX " $ "] ++ lexpr ++ [printsymbol flags "vdash"] ++ rexpr ++ [TeX " $ "]
     Equivalence -> [TeX " $ "] ++ lexpr ++ [printsymbol flags "equiv"] ++ rexpr ++ [TeX " $ "]
     Truth -> [TeX " $ "] ++ rexpr ++ [TeX " $ "]
     Generalization -> undefined
     Automatic -> undefined
     where
     lexpr = printexpr flags (rrant r)
     rexpr = printexpr flags (rrcon r)
   Sg {} -> (Str "SIGNAL "):(printrule flags (srsig r))
   Gc {} -> [Str "?"]
   Fr {} -> [Str "?"]
printexpr :: Options -> Expression -> [Inline]
printexpr flags expr = case expr of
   Tm {} -> printmph flags (m expr)
   Tc {} -> [Str "("] ++ (printexpr flags (e expr)) ++ [Str ")"]
   F  {} -> addinfix (Str ";") [printexpr flags sub | sub<-es expr]
   Fd {} -> addinfix (printsymbol flags "dagger") [printexpr flags sub | sub<-es expr]
   Fi {} -> addinfix (printsymbol flags "cap") [printexpr flags sub | sub<-es expr]
   Fu {} -> addinfix (printsymbol flags "cup") [printexpr flags sub | sub<-es expr]
   K0 {} -> printexpr flags (e expr) ++ [Superscript [Str "*"]]
   K1 {} -> printexpr flags (e expr) ++ [Superscript [Str "+"]]
   Cp {} -> printcompl flags $ printexpr flags (e expr)

