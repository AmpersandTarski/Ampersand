{-# OPTIONS_GHC -Wall #-}
--TODO -> Block: Header Int [Inline] - Int indicates level of header. If I look at pandoc code TexInfo.hs blockToTexinfo ln.208 I would expect chapter,section,sub,subsub respectively. But I get section,sub,subsub,plain text respectively. So now I've written chapters as 0 setting a [Inline] -> [Tex "\\chapter{...}"]. I do not know yet how this relates to other formats like rtf.
module Fspec2Pandoc (fSpec2Pandoc,render2Pandoc,writeRTF,writeLaTeX)
where

   import Adl
   import FspecDef
   import Text.Pandoc   
     --Als de compiler hierover struikelt, dan moet je pandoc installeren. Dat is overigens in de volgende 3 stappen:
                             -- 1) Eerst installeer je Cabal (zie http://www.haskell.org/cabal/) en dan roep je op je command line: 
                             -- 2) cabal-install pandoc  (onder windows: cabal install pandoc)
                             -- 3) Het kan zijn dat dit nog niet werkt, zie http://groups.google.com/group/pandoc-discuss/browse_thread/thread/a8fc3a627aeec7f2
                             --    als dat het geval is, kan deze module worden overruled in Generators.hs                                 
   import Version        (versionbanner)
   import Languages      (Lang(..))
   import Options        (Options(..),FspecFormat(..))
   import Rendering.AdlExplanation
   import Statistics
 
   render2Pandoc :: Options -> String -> Pandoc -> String
   render2Pandoc flags customheader pandoc = case fspecFormat flags of
      FPandoc -> prettyPandoc pandoc
      FWord -> let wropts = defaultWriterOptions{writerStandalone=True}
               in writeRTF wropts pandoc
      --DESCR -> FLatex
      -- $ writerHeader options $$ extras $$ secnumline $$ verbatim $$ titletext $$ authorstext $$ datetext $$ "\\begin{document}" $$ maketitle $$ "" $$ "\\tableofcontents\n" $$ writerIncludeBefore options $$ main $$ writerIncludeAfter options $$ "\\end{document}"
      --writerStandalone=True -> writes a header [header=all untill ($$ "\\tableofcontents\n")]
      --                         writes ($$ "\\end{document}")
      --writerTableOfContents -> writes ($$ "\\tableofcontents\n")
      --writerIncludeBefore is empty
      --writerIncludeAfter is empty
      --writerNumberSections=True to number all sections (no secnumline)
      FLatex -> let wropts = defaultWriterOptions{writerStandalone=True, writerHeader=customheader, writerTableOfContents=True,writerNumberSections=True}
                in writeLaTeX wropts pandoc
      FHtml -> writeHtmlString defaultWriterOptions pandoc
      FUnknown -> prettyPandoc pandoc --REMARK -> will not occur at time of implementation because of user IO error.

   chpintrolabel="chpIntro"
   chpdplabel="chpDesignPrinciples"
   chpcalabel="chpConceptualAnalysis"
   chpdalabel="chpDataAnalysis"
   chpgloslabel="chpGlossary"
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
   designPrinciples lev fSpec flags = header ++ dpIntro ++ [b|t<-themes fSpec,b<-dpSection t]
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
     dpSection :: Ftheme -> [Block]
     dpSection t = (if null (themedecls ++ themerules) then [] --nothing to explain for this theme -> skip
              else [Header (lev+1) [Str (name t)]] --new section to explain this theme
                ++ [Para [Str d]|d<-themedecls] --explanation of all multiplicities in the theme
                ++ [Para [Str r]|r<-themerules]) --explanation of all (non-computed) rules in the theme
       where
       --query copied from FSpec.hs revision 174
       themedecls = [explainDecl (language flags) d|u<-units t, d<-ptdcs (pattern u),(not.null) (multiplicities d)]
       --query copied from FSpec.hs revision 174
       themerules = [explainRule (language flags) r|u<-units t, r<-declaredRules (pattern u)++signals (pattern u), null (cpu r)]
------------------------------------------------------------
   conceptualAnalysis :: Int -> Fspc -> Options ->  [Block]
   conceptualAnalysis lev fSpec flags = header ++ caIntro ++ [b|t<-themes fSpec,b<-caSection t]
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
                 [ [ [Plain [Str (name t)]]
           --        , [Plain [Str (show $ nFpoints t)]] ] 
                     , [Plain [Str "?"]] ] --TODO -> there is a loop in fspc->ftheme->funit->fviewdef&servicespec, coming from adl2fspec (remainingDS & pats)
                 | t<-themes fSpec]
          ]  
     caSection :: Ftheme -> [Block]
     caSection t = 
      (case (language flags) of
         Dutch -> (if null themerules then [] --nothing to explain for this theme -> skip
              else [Header (lev+1) [Str $ "Regels over " ++ (name t)]] --new section to explain this theme
                ++ [Para [Str "Een conceptuele analyse van "
                         ,Str (name t)
                         ,Str " wordt weergegeven door figuur "
                         ,xrefReference figlabel
                         ,Str "."]]
--TODO                ++ [Para [xrefImage ((name t)++".png") figlabel]]
                ++ rules2table)
         English -> (if null themerules then [] --nothing to explain for this theme -> skip
              else [Header (lev+1) [Str $ "Rules about " ++ (name t)]] --new section to explain this theme
                ++ [Para [Str "A conceptual analysis of "
                         ,Str (name t)
                         ,Str " is represented in figure "
                         ,xrefReference figlabel
                         ,Str "."]]
--TODO                ++ [Para [xrefImage ((name t)++".png") figlabel]]
                ++ rules2table)
       )
       where
       figlabel = "cafig:" ++ (name t) 
       --query copied from FSpec.hs revision 174
       themerules = [r|u<-units t, r<-declaredRules (pattern u)]

       rules2table = case fspecFormat flags of
         --REMARK -> pandoc does not support longtable (or something similar?)
         FLatex -> [Plain $ 
                     [ TeX "\\begin{center} \n"
                     , TeX "\\begin{longtable}{|r|p{\\columnwidth}|} \n"
                     , TeX "\\hline \n"
                     ]
                  ++ [ inline | r<-themerules, inline<-explainCaRule r]
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
                         , Str (explainRule (language flags) r) --TODO -> alignment is incorrect
                         , TeX "\n \\linebreak ", Str "\n"
                         , Str "Relations:"
                         , TeX "\n \\linebreak ", Str "\n"
                         , TeX " \\( \\begin{array}{rcl} \n"
                         ]
                      ++ [ inline | m<-morlist r, inline<-printmphdetail m]
                      ++ [ TeX " \\end{array} \\) "
                         , TeX "\n \\linebreak ", Str "\n"
                         , Str "Rule:"
                         , TeX "\n \\linebreak ", Str "\n"
                         ] 
                      ++ (printrule r)
                      ++ [ TeX " \\\\ \\hline \n"
                         ]
       printmphdetail m = printmphname m
                       ++ [TeX " &", Str ":", TeX "& "]
                       ++ printtype m
                       ++ [TeX " \\\\ ", Str "\n" ]
       printmph m = (printmphname m) ++ (printflip m)
       printflip m = case m of
          Mph{} -> if mphyin m then [] 
                   else [ Superscript [TeX "$",printsymbol "smile" flags, TeX "$"] ]
          _ -> []
       printmphname m = case m of
          Mph{} -> [Str (name m)]
          I{}   -> [printsymbol "mathbb{I}" flags, Subscript [TeX "$",Str $ name (mphspc m), TeX "$"] ]
          V{}   -> [printsymbol "mathbb{V}" flags, Subscript ((TeX "$"):(printtype m)++[TeX "$"]) ]
          Mp1{} -> [Str "?"]
       printtype m = [ Str $ (name.source) m
                     , printsymbol "times" flags, Space
                     , Str $ (name.target) m
                     ]
       printrule r = case r of
          Ru {} -> case rrsrt r of 
            Implication -> [TeX " $ "] ++ lexpr ++ [printsymbol "vdash" flags] ++ rexpr ++ [TeX " $ "]
            Equivalence -> [TeX " $ "] ++ lexpr ++ [printsymbol "equiv" flags] ++ rexpr ++ [TeX " $ "]
            Truth -> [TeX " $ "] ++ rexpr ++ [TeX " $ "]
            where
            lexpr = printexpr (rrant r)
            rexpr = printexpr (rrcon r)
          Sg {} -> (Str "[SIGNAL] "):(printrule (srsig r))
          _ -> [Str "?"]
       printexpr expr = case expr of
          Tm {} -> printmph (m expr)
          Tc {} -> [Str "("] ++ (printexpr (e expr)) ++ [Str ")"]
          F  {} -> addinfix (Str ";") [printexpr sub | sub<-es expr]
          Fd {} -> addinfix (printsymbol "dagger" flags) [printexpr sub | sub<-es expr]
          Fi {} -> addinfix (printsymbol "cap" flags) [printexpr sub | sub<-es expr]
          Fu {} -> addinfix (printsymbol "cup" flags) [printexpr sub | sub<-es expr]
          K0 {} -> printexpr (e expr) ++ [Superscript [Str "*"]]
          K1 {} -> printexpr (e expr) ++ [Superscript [Str "+"]]
          Cp {} -> printcompl (printexpr (e expr)) flags
       addinfix :: Inline -> [[Inline]] -> [Inline] 
       addinfix _ [] = [] --tail will not be on empty list
       addinfix delim xs = tail [inline | inlines<-postfix, inline<-inlines]
          where
          postfix :: [[Inline]] 
          postfix = [delim:x|x<-xs] 
------------------------------------------------------------
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
      (case (language flags) of
         Dutch -> [Para
                    [ Str "De keuzes, zoals beschreven in hoofdstuk "
                    , xrefReference chpdplabel
                    , Str " zijn in een gegevensanalyse vertaald naar het klassediagram in figuur "
                    , xrefReference ("fig:"++baseName flags++"CD")
                    , Str "Dit hoofdstuk geeft een uitwerking van de gegevensanalyse in de vorm van functionele specificaties. "
            --        , xrefReference fpalabel
                    , Str "."]]
         English -> [] --TODO
      ) -- ++ [Para [Image [Str "BlaDieBlah"] ( "eenplaatje.png", "TestPlaatje" )]]
        ++ [] --TODO daadwerkelijke gegevensanalyse toevoegen
------------------------------------------------------------
   glossary :: Int -> Fspc -> Options ->  [Block]
   glossary lev fSpec flags = []  --TODO
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
   xrefImage :: String -> String -> Inline
   xrefImage url lbl = Image [Str ("Missing figure " ++ url)] (url, lbl)

   --EXTEND -> symbol string must reflect latex symbol identifier
   printsymbol :: String -> Options -> Inline
   printsymbol symb flags = case fspecFormat flags of
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
   printcompl inlines flags = case fspecFormat flags of
     FLatex -> [TeX " \\overline{"] ++ inlines ++ [TeX "} "]
     _ -> (Str "-"):inlines

   
