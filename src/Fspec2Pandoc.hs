{-# OPTIONS_GHC -Wall #-}
--TODO -> May be we can look at GetText function for help with internationalization. Brian O'Sullivan is working (has started) on an internationalization library. Maybe some day...
--TODO -> Block: Header Int [Inline] - Int indicates level of header. If I look at pandoc code TexInfo.hs blockToTexinfo ln.208 I would expect chapter,section,sub,subsub respectively. But I get section,sub,subsub,plain text respectively. So now I've written chapters as 0 setting a [Inline] -> [Tex "\\chapter{...}"]. I do not know yet how this relates to other formats like rtf.
module Fspec2Pandoc (fSpec2Pandoc,laTeXheader)
where
import Char
import Collection       (Collection (..))
import Adl
import Data.Plug
import ShowADL
import CommonClasses    (showSign)
import FspecDef
import Strings          (remSpaces, spread, commaNL, commaEng)
import Text.Pandoc  
  --Als de compiler hierover struikelt, dan moet je pandoc installeren. Dat is overigens in de volgende 3 stappen:
                          -- 1) Eerst installeer je Cabal (zie http://www.haskell.org/cabal/) en dan roep je op je command line: 
                          -- 2) cabal-install pandoc  (onder windows: cabal install pandoc)
                          -- 3) Het kan zijn dat dit nog niet werkt, zie http://groups.google.com/group/pandoc-discuss/browse_thread/thread/a8fc3a627aeec7f2
                          --    als dat het geval is, kan deze module worden overruled in Generators.hs                                 
import Version          (versionbanner)
import Languages        (Lang(..),plural)
import PredLogic        (expr2predLogic)
import Options          (Options(..),FspecFormat(..))
import ShowECA          (showECA)
import NormalForms      (conjNF,normECA) -- ,proofPA)  Dit inschakelen voor het bewijs...
import Rendering.AdlExplanation
import Rendering.ClassDiagram
import System.FilePath
import Switchboard      (switchboard)
import Data.GraphViz    (printDotGraph)
import Classes.Graphics (toDot)
import Picture

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

laTeXheader :: Options->String
laTeXheader flags
   = chain "\n" (
     [ "\\documentclass[10pt,a4paper]{report}"
     , "\\parskip 10pt plus 2.5pt minus 4pt  % Extra vertical space between paragraphs."
     , "\\parindent 0em                      % Width of paragraph indentation."
     , "\\usepackage{theorem}"
     ] ++
     ( case language flags of
        Dutch   -> [ "\\usepackage[dutch]{babel}"
                   , "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{definition}{Definitie}[section]"
                   , "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{designrule}[definition]{Functionele eis}" ]
        _       -> [ "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{definition}{Definition}[section]"
                   , "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{designrule}[definition]{Requirement}" ]
     )++
     [ "\\usepackage{amssymb}"
     , "\\usepackage{amsmath}"
     , "\\usepackage{longtable}"
     , "\\def\\id#1{\\mbox{\\em #1\\/}}"
     , "\\def\\define#1{\\label{dfn:#1}{\\em #1}}"
     , "\\newcommand{\\iden}{\\mathbb{I}}"
     , "\\newcommand{\\ident}[1]{\\mathbb{I}_{#1}}"
     , "\\newcommand{\\full}{\\mathbb{V}}"
     , "\\newcommand{\\fullt}[1]{\\mathbb{V}_{[#1]}}"
     , "\\newcommand{\\relAdd}{\\dagger}"
     , "\\newcommand{\\flip}[1]{{#1}^\\smallsmile} %formerly:  {#1}^\\backsim"
     , "\\newcommand{\\kleeneplus}[1]{{#1}^{+}}"
     , "\\newcommand{\\kleenestar}[1]{{#1}^{*}}"
     , "\\newcommand{\\cmpl}[1]{\\overline{#1}}"
     , "\\newcommand{\\rel}{\\times}"
     , "\\newcommand{\\compose}{;}"
     , "\\newcommand{\\subs}{\\vdash}"
     , "\\newcommand{\\fun}{\\rightarrow}"
     , "\\newcommand{\\isa}{\\sqsubseteq}"
     , "\\newcommand{\\N}{\\mbox{\\msb N}}"
     , "\\newcommand{\\disjn}[1]{\\id{disjoint}(#1)}"
     , "\\newcommand{\\fsignat}[3]{\\id{#1}:\\id{#2}\\mbox{$\\rightarrow$}\\id{#3}}"
     , "\\newcommand{\\signat}[3]{\\mbox{${#1}_{[{#2},{#3}]}$}}"
     , "\\newcommand{\\declare}[3]{\\id{#1}:\\id{#2}\\mbox{$\\times$}\\id{#3}}"
     , "\\newcommand{\\fdeclare}[3]{\\id{#1}:\\id{#2}\\mbox{$\\fun$}\\id{#3}}"
     ] ++ (if language flags == Dutch then [ "\\selectlanguage{dutch}" ] else [] )++
     [ "%  -- end of header. Use 'ADL --headerfile myHeader.tex' to replace the previous by your own header file." ] )

chpintrolabel :: String
chpintrolabel="chpIntro"
chpFRlabel :: String
chpFRlabel="chpFunctionalRequirements"
chpCAlabel :: String
chpCAlabel="chpConceptualAnalysis"
chpDAlabel :: String
chpDAlabel="chpDataAnalysis"
chpFPAlabel :: String
chpFPAlabel="chpFPAnalysis"
chpGlosLabel :: String
chpGlosLabel="chpGlossary"

--TODO: Invent a syntax for meta information that is included in the source file...

--The following general requirements apply to the functional specification document:
--Descriptive title, number, identifier, etc. of the specification
--Date of last effective revision and revision designation
--A logo (trademark recommended) to declare the document copyright, ownership and origin
--Table of Contents
--Person, office, or agency responsible for questions on the specification, updates, and deviations.
--The significance, scope or importance of the specification and its intended use.
--Terminology, definitions and abbreviations to clarify the meanings of the specification
--Test methods for measuring all specified characteristics
--Material requirements: physical, mechanical, electrical, chemical, etc. Targets and tolerances.
--Performance testing requirements. Targets and tolerances.
--Drawings, photographs, or technical illustrations
--Workmanship
--Certifications required.
--Safety considerations and requirements
--Environmental considerations and requirements
--Quality control requirements, Sampling (statistics), inspections, acceptance criteria
--Person, office, or agency responsible for enforcement of the specification.
--Completion and delivery.
--Provisions for rejection, reinspection, rehearing, corrective measures
--References and citations for which any instructions in the content maybe required to fulfill the traceability and clarity of the document
--Signatures of approval, if necessary
--Change record to summarize the chronological development, revision and completion if the document is to be circulated internally
--Annexes and Appendices that are expand details, add clarification, or offer options.

fSpec2Pandoc :: Fspc -> Options -> (Pandoc, [Picture])
fSpec2Pandoc fSpec flags = ( Pandoc meta docContents , pictures )
    where meta = Meta title authors date
          title = [Str (case (language flags) of
                         Dutch   -> "Functionele Specificatie van "
                         English -> "Functional Specification of "
                       )]
                ++[Quoted  SingleQuote [Str (name fSpec)] ] 
          authors = case (language flags) of
                         Dutch   -> ["Auteur(s) hier plaatsen","(Dit document is gegenereerd door "++versionbanner++")"]
                         English -> ["Put author(s) here","(This document was generated by "++versionbanner++")"]
          date = show(genTime flags)
          
          (docContents,pictures)
           = ( introduction       level fSpec flags  ++
               designPrinciples   level fSpec flags  ++
               caTxt                                 ++
               daTxt                                 ++
               [b| (blocks,_)<-svcs, b<-blocks]      ++
               fpAnalysis         level fSpec flags  ++
               glossary level fSpec flags
             , [daPic]++caPics++[p| (_,pictures)<-svcs, p<-pictures] )
             where svcs = [serviceChap level fSpec flags svc | svc  <-FspecDef.services fSpec]
                   (daTxt,daPic)  = dataAnalysis       level fSpec flags
                   (caTxt,caPics) = conceptualAnalysis level fSpec flags
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
         [ Para 
                [ Str "Dit document definieert de functionaliteit van een informatiesysteem genaamd "
                , Quoted  SingleQuote [Str (name fSpec)] 
                , Str ". Het definieert business-services in een systeem waarin mensen en applicaties samenwerken om afspraken na te leven. "
                , Str "Een aantal van deze afspraken is gebruikt om deze functionele specificatie samen te stellen. "
                , Str "De betreffende afspraken staan opgesomd in hoofdstuk ", xrefReference chpFRlabel, Str ", geordend op thema. "
                , Str "Elk informatiesysteem wat voldoet aan deze functionele specificatie ondersteunt het naleven van deze afspraken. "
                , Str "Om dit doel te bereiken, bestaat ", Str (name fSpec), Str " uit een verzameling business services. "
                , Str "Door alle functionaliteit uitsluitend via deze services te ontsluiten waarborgt ", Str (name fSpec)
                , Str " dat gebruikers de afspraken uit hoofdstuk ", xrefReference chpFRlabel, Str " naleven. "
                ]
          , Para 
                [ Str "De conceptuele analyse in hoofdstuk ", xrefReference chpCAlabel
                , Str " is bedoeld voor informatici om ", Str (name fSpec), Str " te bouwen. "
                , Str "Tevens is het bedoeld voor testers om te valideren of alle afspraken uit hoofdstuk ", xrefReference chpFRlabel, Str " worden nageleefd. "
                , Str "Hoofdstuk ", xrefReference chpCAlabel, Str " bevat dan ook een formele representatie van elke afspraak. "
                , Str "Daarmee ligt de consistentie van alle afspraken vast en is de interpretatie van de afspraken eenduidig."
                ]
          , Para 
                [ Str "De hoofdstukken die dan volgen zijn bedoeld voor de bouwers van ", Str (name fSpec), Str ". "
                , Str "De gegevensanalyse in hoofdstuk "
                , xrefReference chpDAlabel
                , Str " beschrijft de gegevensverzamelingen waarop het systeem wordt gebouwd. "
                , Str "Elk volgend hoofdstuk definieert een business service definities van services. "
                , Str "Deze services ondersteunen gezamenlijk alle afspraken uit hoofdstuk ", xrefReference chpFRlabel
                , Str ". Deze ondersteuning bestaat uit het voorkomen dat een afspraak wordt overtreden, "
                , Str "of het signaleren van overtredingen (opdat mensen kunnen ingrijpen), "
                , Str "of het herstellen van een regel (door automatische acties op de database uit te voeren)."
                ]
         ]

        introContents English = 
         [Para
                [Str "This document defines the functionality of an information system called "
                , Quoted  SingleQuote [Str (name fSpec)] 
                , Str ". It defines business services in a system where people and applications work together in order to fullfill their commitments. "
                , Str "Many of these commitments follow from rules that are maintained by the business, the so called business rules. "
                , Str "A number of these rules have been used to assemble this functional specification. "
                , Str "Those rules are listed in chapter ", xrefReference chpFRlabel, Str ", ordered by theme. "
                , Str "Every information system that satisfies this functional specification supports the fulfillment of the commitments. "
                , Str "In order to achieve this goal ", Str (name fSpec), Str " consists of business services. "
                , Str "By disclosing all functionality through these services, ", Str (name fSpec)
                , Str " ensures that users will abide by the rules put forward in chapter ", xrefReference chpFRlabel, Str ". "
                ]
          , Para 
                [ Str "The conceptual analysis in chapter ", xrefReference chpCAlabel
                , Str " is meant for software engineers to build ", Str (name fSpec), Str ". "
                , Str "It is also intended to help testers to validate whether all requirements from chapter ", xrefReference chpFRlabel, Str " are met. "
                , Str "Chapter ", xrefReference chpCAlabel, Str " contains a formal representation of each commitment precisely for that reason. "
                , Str "It defines the consistency of all commitments, yielding an unambiguous interpretation."
                ]
          , Para 
                [ Str "Chapters that follow have the builders of ", Str (name fSpec), Str " as their intended audience. "
                , Str "The data analysis in chapter "
                , xrefReference chpDAlabel
                , Str " describes the data sets upon which . ", Str (name fSpec), Str " is built. "
                , Str "One service is described in a self contained way in a chapter of its own, ensuring that builders can focus on building a single service at a time. "
                , Str "Together, these service fulfill all commitments from chapter ", xrefReference chpFRlabel
                , Str ". This means to prevent that rules are violated, "
                , Str "to signal violations to people in order to let them intervene, "
                , Str "or to restore a rule by means of automatic actions in the database."]]  
------------------------------------------------------------
designPrinciples :: Int -> Fspc -> Options ->  [Block]
designPrinciples lev fSpec flags = header ++ dpIntro ++ dpRequirements
  where
  header :: [Block]
  header = labeledHeader lev chpFRlabel (case (language flags) of
                                 Dutch   ->  "Functionele Eisen"   
                                 English ->  "Functional Requirements"
                             )
  dpIntro :: [Block]
  dpIntro = 
    case language flags of
        Dutch   -> [Para
                     [ Str "Dit hoofdstuk beschrijft de functionele eisen ten behoeve van ", Str (name fSpec), Str ". "
                     , Str "Elke afspraak die gebruikers gezamenlijk naleven "
                     , Str "en door ", Str (name fSpec), Str " moet worden ondersteund, "
                     , Str "is opgenomen als functionele eis in dit hoofdstuk. "
                     , Str "Formuleringen in dit hoofdstuk dienen dan ook zorgvuldig te worden getoetst met en door "
                     , Str "al degenen die op welke wijze dan ook de noodzakelijke kennis en voldoende autoriteit bezitten. "
                     , Str "Zij zijn immers verantwoordelijk voor de geldende regels. "
                     , Str "De hoofdarchitect is verantwoordelijk voor de onderlinge consistentie van deze afspraken "
                     , Str "en het bouwbaar zijn van het daaruit afgeleide systeem. "
                     , Str "Om deze reden schrijft de architect de afspraken zelf op, "
                     , Str "om ze te laten toetsen door de betrokkenen uit de organisatie. "
                     , Str "Van het voorliggende document is dit hoofdstuk het enige dat het fiat van gebruikers nodig heeft. "
                     , Str "Alle hierop volgende hoofdstukken zijn technisch van aard en bedoeld voor bouwers, testers en auditors. "
                     ]]
        English -> [Para
                     [ Str "This chapter defines the functional requirements of ", Str (name fSpec), Str ". "
                     , Str "Each requirement users must fulfill "
                     , Str "by support of ", Str (name fSpec), Str ", "
                     , Str "serves as a function requirement in this chapter. "
                     , Str "The precise phrasing of each requirement must therefore be scrutinized "
                     , Str "with and by those who have the knowledge and who are responsible for the actual rules. "
                     , Str "The chief architect is responsible for the consistency of all rules "
                     , Str "and for a buildable design. "
                     , Str "For this reason, the rules are written by the architect "
                     , Str "and validated by the apporopriate stakeholders. "
                     , Str "The current chapter requires user approval on behalf of the patron. "
                     , Str "All following chapters are technical and are meant for builders, testers and auditors. "
                     ]]

  dpRequirements :: [Block]
  dpRequirements = dpSections (rd (map r_pat (rules fSpec++signals fSpec))) [] [] [] 1
  --TODO -> It may be nice to print the class of the dataset from the class diagram
  dpSections [] _ _ _ _ = []
  dpSections (thm:thms)     -- The name of the patterns that are used in this specification.
             seenConcepts   -- All concepts that have been defined in earlier sections
             seenRelations  -- All relations whose multiplicities have been defined in earlier sections.
             seenRules      -- All rules that have been defined in earlier sections.
             i              -- unique definition numbers (start at 1)
   = [Header (lev+1) [Str thm]]  --new section to explain this theme
     ++ sctConcepts  -- tells which new concepts are introduced in this section.
     ++ [ DefinitionList (sctRules ++ sctSignals) ]   -- tells which rules and signals are being introduced
     ++ dpSections thms seenCss seenDss seenRss i''
    where
     (sctRules,   i',  seenCrs, seenDrs, seenRrs) = dpRule patRules i seenConcepts seenRelations []
     (sctSignals, i'', seenCss, seenDss, seenRss) = dpRule patSignals i' seenCrs seenDrs seenRrs
     conceptdefs  = [(c,cd)| c<-concs fSpec, cd<-conceptDefs fSpec, cdnm cd==name c]
     patRules     = [r| r<-rules fSpec,   r_pat r==thm, r_usr r]
     patSignals   = [s| s<-signals fSpec, r_pat s==thm, r_usr s]
     newConcepts  = concs        (patRules++patSignals) >- seenConcepts
     newRelations = filter (not.isIdent) (declarations (patRules++patSignals) >- seenRelations)
     dpRule [] i seenConcepts seenDeclarations seenRules = ([], i, seenConcepts, seenDeclarations, seenRules)
     dpRule (r:rs) i seenConcepts seenDeclarations seenRules
      = ( [ ([Str$ name c], [Para [Str$ cddef cd]]) |(n,(c,cd))<-zip [i..] cds]++
          [ ([Str$ name d], [Para [Str$ explainMult flags d]]) |(n,d)<-zip [i+length cds..] nds] ++
          [ ([Str$ name r], [Para [Str$ explainRule flags r]]) ] ++ dpNext
        , i'
        , seenCs
        , seenDs
        , seenRs
        )
        where
         cds = [(c,cd)| (c,cd)<-conceptdefs, c `elem` ncs]
         ncs = concs r >- seenConcepts
         nds = declarations r >- seenDeclarations
         ( dpNext, i', seenCs,  seenDs, seenRs ) = dpRule rs (i+length cds+length nds+1) (concs r `uni` seenConcepts) (declarations r `uni` seenDeclarations) (r:seenRules)
     sctConcepts
      = if null newConcepts then [] else
          case language flags of
           Dutch   ->  [ Para $ (case [name c|(c,cd)<-conceptdefs, c `elem` newConcepts] of
                                  []  -> []
                                  [c] -> [ Str $ "Deze sectie introduceert het concept "
                                         , Str $ c
                                         , Str $ ". "]
                                  cs  -> [ Str $ "Deze sectie introduceert de concepten "
                                         , Str $ commaNL "en" cs
                                         , Str $ ". "]
                                )++
                                (case [name c| c<-newConcepts, (c',cd)<-conceptdefs, c==c'] of
                                  []  -> []
                                  [c] -> [ Str $ "Concept "
                                         , Str $ c
                                         , Str $ " wordt in deze sectie voor het eerst gebruikt en heeft geen definitie in deze functionele specificatie. "]
                                  cs  -> [ Str $ "De concepten "
                                         , Str $ commaNL "en" cs
                                         , Str $ " worden in deze sectie voor het eerst gebruikt en hebben geen definitie in deze functionele specificatie. "]
                                )
                       ]
           English ->  [ Para $ (case [name c|(c,cd)<-conceptdefs, c `elem` newConcepts] of
                                  []  -> []
                                  [c] -> [ Str $ "This section introduces concept "
                                         , Str $ c
                                         , Str $ ". "]
                                  cs  -> [ Str $ "This section introduces concepts "
                                         , Str $ commaEng "and" cs
                                         , Str $ ". "]
                                )++
                                (case [name c| c<-newConcepts, (c',cd)<-conceptdefs, c==c'] of
                                  []  -> []
                                  [c] -> [ Str $ "Concept "
                                         , Str $ c
                                         , Str $ " is introduced in this section without a definition."]
                                  cs  -> [ Str $ "Concepts "
                                         , Str $ commaEng "and" cs
                                         , Str $ " are introduced in this section without definition. "]
                                )
                       ]

  dpSection :: FTheme -> [Block]
  dpSection t = []
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
conceptualAnalysis :: Int -> Fspc -> Options -> ([Block],[Picture])
conceptualAnalysis lev fSpec flags = (header ++ caIntro ++ caBlocks , pictures)
  where
  (caBlocks,pictures) = ( [b| (blocks,_)<-ca, b<-blocks], [picture| (_,picture)<-ca] )
                        where ca=caSections (patterns fSpec)
  header :: [Block]
  header = labeledHeader lev chpCAlabel (case (language flags) of
                                Dutch   ->  "Conceptuele Analyse"   
                                English ->  "Conceptual Analysis"
                               )
  caIntro :: [Block]
  caIntro = 
   (case (language flags) of
      Dutch   -> [Para
                  [ Str "Dit hoofdstuk geeft een analyse van de regels uit hoofdstuk "
                  , xrefReference chpFRlabel
                  , Str ". Ieder thema in dat hoofdstuk wordt geanalyseerd in termen van relaties "
                  , Str "en elke afspraak krijgt een formele representatie. "
                  ]]
      English -> [Para
                  [ Str "This chapter provides an analysis of the principles described in chapter "
                  , xrefReference chpFRlabel
                  , Str ". Each section in that chapter is analysed in terms of relations "
                  , Str "and each principle is then translated in a rule. "
                  ]]
   )

  caSections :: [Pattern] -> [([Block],Picture)]
  caSections pats = iterat 1 pats
   where
    iterat :: Int -> [Pattern] -> [([Block],Picture)]
    iterat n (pat:ps)
     = ( [Header (lev+1) [Str $ name pat]]    -- new section to explain this theme
       ++ (case language flags of             -- announce the conceptual diagram
             Dutch   -> [Para [x | x<-[Str "Figuur ", xrefReference (reference pict), Str " geeft een conceptuele analyse van dit thema."]] ]
             English -> [Para [x | x<-[Str "Figure ", xrefReference (reference pict), Str " shows a conceptual analysis of this theme."]] ]
          )
       ++ [Plain (xrefFigure1 pict)]          -- draw the conceptual diagram
       ++ (if null (themerules pat) then [] else [OrderedList (n, Decimal, DefaultDelim) (themerules pat)])
       , pict):  iterat (n+length (themerules pat)) ps
       where pict = makePicture flags (name pat) PTPattern pStr   -- the Picture that represents this service's knowledge graph
             pGph = toDot fSpec flags pat                         -- the DotGraph String that represents this service's knowledge graph
             pStr = printDotGraph pGph                            -- the String that represents this service's knowledge graph
    iterat n [] = []
    --query copied from FSpec.hs revision 174
    themerules  :: Pattern -> [[Block]]
    themerules pat = [[Plain [Str $ "R"++show (nr r),Str $ latexEsc (explainRule flags r)]]|r<-rules pat]
      
------------------------------------------------------------
--DESCR -> the data analysis contains a section for each class diagram in the fspec
--         the class diagram and multiplicity rules are printed
dataAnalysis :: Int -> Fspc -> Options -> ([Block],Picture)
dataAnalysis lev fSpec flags = ( header ++ daContents ++ daMultiplicities ++ daPlugs , classDiagramPicture )
 where 
  header :: [Block]
  header = labeledHeader lev chpDAlabel (case (language flags) of
                                     Dutch   ->  "Gegevensstructuur"   
                                     English ->  "Data structure"
                                 )
  daContents :: [Block]
  daContents = 
   (case (language flags) of
     Dutch   -> [Para
                  [ Str $ "De eisen, die in hoofdstuk "
                  , xrefReference chpFRlabel
                  , Str $ " beschreven zijn, zijn in een gegevensanalyse vertaald naar het klassediagram van figuur "
                  , xrefReference (figlabel classDiagramPicture)
                  , Str $ ". Er zijn "++count flags (length classes) "gegevensverzameling"++","
                  , Str $ " "++count flags (length assocs) "associatie"++","
                  , Str $ " "++count flags (length geners) "generalisatie"++" en"
                  , Str $ " "++count flags (length aggrs) "aggregatie"++"."
                  , Str $ " "++nm++" kent in totaal "++count flags (length cs) "concept"++"."
                  ]]
     English -> [Para
                  [ Str $ "The requirements, which are listed in chapter "
                  , xrefReference chpFRlabel
                  , Str $ ", have been translated into the class diagram in figure "
                  , xrefReference (reference classDiagramPicture)
                  , Str $ ". There are "++count flags (length classes) "data set"++","
                  , Str $ " "++count flags (length assocs) "association"++","
                  , Str $ " "++count flags (length geners) "generalisation"++", and"
                  , Str $ " "++count flags (length aggrs) "aggregation"++"."
                  , Str $ " "++nm++" has a total of "++count flags (length cs) "concept"++"."
                  ]] --TODO
   ) ++ [ Plain $ xrefFigure1 classDiagramPicture ]  -- TODO: explain all multiplicities]
   where OOclassdiagram classes assocs aggrs geners (nm, cs) = classDiagram

  classDiagram :: ClassDiag
  classDiagram = cdAnalysis fSpec flags

  classDiagramPicture :: Picture
  classDiagramPicture = makePicture flags (name fSpec) PTClassDiagram cdDot
      where
       cdDot = classdiagram2dot classDiagram

  daMultiplicities :: [Block]
  daMultiplicities
   = [ if language flags==Dutch
       then Para [ Str $ "De relaties in "++name fSpec++" hebben de volgende multipliciteitsrestricties. "
                 ]
       else Para [ Str $ "The relations in "++name fSpec++" have the following multiplicity constraints. "
                 ]
     , Para  $ [ TeX $ "\\begin{tabular}{|l|cccc|}\\hline\n"
               , if language flags==Dutch
                 then TeX $ "relatie&totaal&univalent&surjectief&injectief\\\\ \\hline\\hline\n"
                 else TeX $ "relation&total&univalent&surjective&injective\\\\ \\hline\\hline\n"
               ]++
               [ TeX $ chain "&" [ "\\(\\signat{"++latexEsc (name d)++"}{"++latexEscShw (source d)++"}{"++latexEscShw (target d)++"}\\)"              -- veld
                                 , if isTot d || d `elem` tots then "\\(\\surd\\)" else ""
                                 , if isUni d || d `elem` unis then "\\(\\surd\\)" else ""
                                 , if isSur d || d `elem` surs then "\\(\\surd\\)" else ""
                                 , if isInj d || d `elem` injs then "\\(\\surd\\)" else ""
                                 ]++"\\\\\n"
               | d<-declarations fSpec
               ]++
               [ TeX $ "\\hline\n\\end{tabular}"
               ]
     ]++
     [ Para [ if language flags==Dutch
                then TeX $ latexEsc "Een relatie, \\id{"++name d++"}, is homogeen en heeft de volgende eigenschappen: "
                else TeX $ latexEsc "One relation, \\id{"++name d++"}, is homogeneous and has the following properties: "]
     | length hMults==1, d<-hMults ]++
     [ Para [ if language flags==Dutch
                then TeX $ latexEsc "In aanvulling daarop hebben de homogene relaties de volgende eigenschappen: "
                else TeX $ latexEsc "Additionally, the homogeneous relations come with the following properties: "]
     | length hMults>1 ]++
     [ Para  $ [ TeX $ "\\begin{tabular}{|l|ccccc|}\\hline\n"
               , if language flags==Dutch
                 then TeX $ "relatie&Reflexief&Transitief&Symmetrisch&Antisymmetrisch&Eigenschap\\\\ \\hline\\hline\n"
                 else TeX $ "relation&Reflexive&Transitive&Symmetric&Antisymmetric&Property\\\\ \\hline\\hline\n"
               ]++
               [ TeX $ chain "&" [ "\\signat{"++latexEsc (name d)++"}{"++latexEscShw (source d)++"}{"++latexEscShw (target d)++"}"              -- veld
                                 , if isRfx d            then "\\(\\surd\\)" else ""
                                 , if isTrn d            then "\\(\\surd\\)" else ""
                                 , if isSym d            then "\\(\\surd\\)" else ""
                                 , if isAsy d            then "\\(\\surd\\)" else ""
                                 , if isAsy d && isSym d then "\\(\\surd\\)" else ""
                                 ]++"\\\\\n"
               | d<-hMults
               ]++
               [ TeX $ "\\hline\n\\end{tabular}"
               ]
     | length hMults>0 ]
     where
      hMults = [d| d<-declarations fSpec, homogeneous d]
      clauses = rd [clause | Quad _ ccrs<-vquads fSpec, (_,shifts)<-cl_conjNF ccrs, clause<-shifts]
      strands (F fs) = [fs]
      strands _      = []    -- <--  we could maybe do better than this...
      is = rd [m| Fu fus<-clauses
                , isIdent (Fi [notCp f| f<-fus, isPos f])
                , f<-filter isNeg fus
                , s<-strands f
                , e<-[head s, flp (last s)]
                , m<-mors e
                ]
      ts = rd [m| Fu fus<-clauses
                , isIdent (Fi [notCp f| f<-fus, isNeg f])
                , f<-filter isPos fus
                , s<-strands f
                , e<-[head s, flp (last s)]
                , m<-mors e
                ]
      tots = [d| t<-ts, inline t, d<-declarations t]
      unis = [d| t<-is, inline t, d<-declarations t]
      surs = [d| t<-ts, not (inline t), d<-declarations t]
      injs = [d| t<-is, not (inline t), d<-declarations t]

  -- daPlugs shows only user defined rules that shall be maintained within a data set.
  -- multiplicity rules, for example, are not among them.
  -- Plugs that are associations between data sets are ignored.
  daPlugs :: [Block]
  daPlugs = [b | p<-plugs fSpec, fld<-take 1 (fields p), flduniq fld, b<-daPlug p]

  daPlug p
   = if null content then [] else plugHeader ++ content
     where
       plugHeader = labeledHeader (lev+1) ("sct:plug "++name p) (name p)
       content = plugRules ++ plugSignals
       plugRules
        = case [r| r<-rules   fSpec, r_usr r, null (declarations (mors r) >- declarations p)] of
                   []  -> [ Para [ Str "This data set has no integrity rules other than the multiplicities specified earlier. " ]]
                   [r] -> [ Para [ Str "This data set shall maintain the following integrity rule. " ]
                          , Para [ Math DisplayMath $ showMathcode fSpec r]
                          ]
                   rs  -> [ Para [ Str "This data set shall maintain the following integrity rules. " ]
                          , BulletList [[Para [Math DisplayMath $ showMathcode fSpec r]]| r<-rs ]
                          ]
       plugSignals
        = case [r| r<-signals fSpec, r_usr r, null (declarations (mors r) >- declarations p)] of
                   []  -> []
                   [s] -> [ Para [ Str "This data set generates one signal. " ]
                          , Para [ Math DisplayMath $ showMathcode fSpec s]
                          ]
                   ss  -> [ Para [ Str "This data set generates the following signals. " ]
                          , BulletList [[Para [Math DisplayMath $ showMathcode fSpec s]]| s<-ss ]
                          ]

------------------------------------------------------------
serviceChap :: Int -> Fspc -> Options -> Fservice ->  ([Block],[Picture])
serviceChap lev fSpec flags svc
 = ( header ++ svcIntro ++ txtKnowledgeGraph ++ svcFieldTables ++ txtSwitchboard
   , [picKnowledgeGraph, picSwitchboard]
   )
 where
  svcname    = name svc
  header :: [Block]
  header = labeledHeader lev ("chpSvc"++svcname) ("Service: " ++ svcname)
  svcIntro :: [Block]
  svcIntro
   = case (language flags) of
      Dutch ->   [ Para
                    ([ Str $ "Service "++svcname++" werkt vanuit een instantie van "++name (target (objctx (fsv_objectdef svc)))++"." ]++
                     f (objctx (fsv_objectdef svc))++
                     [ Str $ svcInsDelConcepts ] )
                 ]
      English -> [ Para
                    ([ Str $ "Service "++svcname++" operates from one instance of "++name (target (objctx (fsv_objectdef svc)))++"." ]++
                     f (objctx (fsv_objectdef svc))++
                     [ Str $ svcInsDelConcepts ] )
                 ]
     where
      f (Tm _) = []
      f expr   = [Str $ lang flags (expr2predLogic (conjNF(F[v (S,source expr),expr])))]
  svcInsDelConcepts
   = let ics = fsv_creating svc>-fsv_deleting svc
         dcs = fsv_deleting svc>-fsv_creating svc
         ucs = fsv_deleting svc `isc` fsv_creating svc
     in case (language flags) of
      Dutch -> " "++
          if null ics && null dcs && null ucs then "Deze service kan niets maken of verwijderen." else
          if null ics && null dcs             then "Instanties van "++commaNL "en" (map name ucs)++" kunnen door deze service worden aangemaakt en verwijderd." else
          if null ics       &&       null ucs then "Deze service kan instanties van "++commaNL "en" (map name ucs)++" verwijderen." else
          if             null dcs && null ucs then "Instanties van "++commaNL "en" (map name ucs)++" kunnen worden aangemaakt door deze service." else
          if                         null ucs then "Instanties van "++commaNL "en" (map name ucs)++" kunnen worden toegevoegd en instanties van "++f dcs++" kunnen worden verwijderd door deze service." else
          if             null dcs             then "Deze service kan instanties van "++commaNL "en" (map name ics)++" creeren, en "++commaNL "en" (map name ucs)++" kunnen ook worden verwijderd." else
          if null ics                         then "Deze service kan instanties van "++commaNL "en" (map name ucs)++" wijzigen, maar "++commaNL "en" (map name dcs)++" kunnen alleen worden verwijderd." else
          "Deze service maakt nieuwe instanties van concept"++f ics++". Hij kan instanties van concept"++f dcs++" verwijderen, terwijl instanties van "++commaNL "en" (map name ucs)++" zowel gemaakt als vernietigd kunnen worden."
          where f [x] = " "++name x++" "
                f xs  = "en "++commaNL "en" (map name xs)
      English -> " "++
          if null ics && null dcs && null ucs then "No concepts can be changed by this service." else
          if null ics && null dcs             then "Concept"++f ucs++" can be both inserted and deleted by this service." else
          if null ics       &&       null ucs then "Concept"++f dcs++" can be deleted by this service." else
          if             null dcs && null ucs then "Concept"++f ics++" can be inserted by this service." else
          if                         null ucs then "Concept"++f ics++" can be inserted, and concept"++f dcs++" can be deleted by this service." else
          if             null dcs             then "By this service, concept"++f ucs++" can be changed, but"++f ics++" can be created but not deleted." else
          if null ics                         then "By this service, concept"++f ucs++" can be changed, but"++f dcs++" can only be deleted." else
          "This service can create new instances of concept"++f ics++". It can delete instances of concept"++f dcs++", and instances of concept"++f ucs++" can be either created and removed."
          where f [x] = " "++name x
                f xs  = "s "++commaEng "and" (map name xs)
  svcFieldTables
   = [ Para  $ [ if language flags==Dutch
                 then Str $ "In deze service zijn de volgende velden zichtbaar. "
                 else Str $ "This service has the following fields. "
               ]
     , Para  $ [ TeX $ "\\begin{tabular}{|lll|}\\hline\n"
               , if language flags==Dutch
                 then TeX $ "veld&concept&relatie\\\\ \\hline\\hline\n"
                 else TeX $ "field&concept&relation\\\\ \\hline\\hline\n"
               ]++
               [ TeX $ chain "&" [ latexEsc (fld_name f)              -- veld
                                 , latexEscShw (target (fld_expr f))  -- concept
                                 , "\\("++rel f++"\\)"                -- relatie
                                 ]++"\\\\\n"
               | f<-fsv_fields svc
               ]++
               [ TeX $ "\\hline\n\\end{tabular}"
               ]
     , Para  $ [ if language flags==Dutch
                 then Str $ "Deze velden hebben de volgende eigenschappen. "
                 else Str $ "These fields have the following properties. "
               ]
     , Para  $ [ TeX $ "\\begin{tabular}{|l|cccc|}\\hline\n"
               , if language flags==Dutch
                 then TeX $ "veld&lijst&verplicht&nieuw&verwijderbaar\\\\ \\hline\\hline\n"
                 else TeX $ "field&list&obligatory&new&remove\\\\ \\hline\\hline\n"
               ]++
               [ TeX $ chain "&" [ latexEsc (fld_name f)              -- veld
                                 , s fld_list f                       -- lijst
                                 , s fld_must f                       -- verplicht veld
                                 , s fld_insAble f                    -- nieuwe waardes mogen (anders alleen selecteren)
                                 , s fld_delAble f]++"\\\\\n"         -- waardes mogen verwijderd worden
               | f<-fsv_fields svc
               ]++
               [ TeX $ "\\hline\n\\end{tabular}"
               ]
     ]
     where s f fld = if f fld then "\\(\\surd\\)" else ""
           rel f = if fld_editable f
                   then showMathcode fSpec (makeInline (fld_mph f))
                   else ""

  txtKnowledgeGraph :: [Block]
  txtKnowledgeGraph
   = (case language flags of                                     -- announce the knowledge graph
           Dutch   -> [Para [x | x<-[ Str "Figuur ", xrefReference (reference picKnowledgeGraph)
                                    , Str " geeft de kennisgraaf weer voor deze service."]] ]
           English -> [Para [x | x<-[ Str "Figure ", xrefReference (reference picKnowledgeGraph)
                                    , Str " shows the knowledge graph of this service."]] ]
     )
     ++ [Plain (xrefFigure1 picKnowledgeGraph)]                  -- draw the knowledge graph

  picKnowledgeGraph :: Picture
  picKnowledgeGraph = makePicture flags (name svc) PTConcept kn  -- the Picture that represents this service's knowledge graph
     where
      knGph = toDot fSpec flags svc                              -- the DotGraph String that represents this service's knowledge graph
      kn    = printDotGraph knGph                                -- the String that represents this service's knowledge graph

  txtSwitchboard :: [Block]
  txtSwitchboard
   = (case language flags of                                     -- announce the switchboard diagram
           Dutch   -> [Para [x | x<-[ Str "Figuur ", xrefReference (reference picSwitchboard)
                                    , Str " geeft een schakelpaneel (switchboard diagram) weer voor deze service."]] ]
           English -> [Para [x | x<-[ Str "Figure ", xrefReference (reference picSwitchboard)
                                    , Str " shows a switchboard diagram of this service."]] ]
     )
     ++ [Plain (xrefFigure1 picSwitchboard)]                     -- draw the switchboard

  picSwitchboard :: Picture
  picSwitchboard = makePicture flags (name svc) PTSwitchBoard sb -- the Picture that represents this service's knowledge graph
     where
      sbGph = switchboard fSpec svc                              -- the DotGraph String that represents this service's knowledge graph
      sb    = printDotGraph sbGph                                -- the String that represents this service's knowledge graph

  svcECA :: [Block]
  svcECA
   = case (language flags) of
      Dutch   -> [ Para [ Str "ECA rules:\n   ",Str "tijdelijk ongedocumenteerd" ] ]
{-                 [ Para
                    [ Str $ showECA fSpec "\n>     "  (normECA (eca arg) arg)
-- Dit inschakelen          ++"\n------ Derivation ----->"
--  voor het bewijs         ++showProof (showECA fSpec "\n>     ") (proofPA (ecaAction (eca arg)))
--                          ++"\n<------End Derivation --"
                    ]
                 | eca<-fsv_ecaRules svc, arg<-[error ("TODO: hier moet een declaratie \"Delta\" staan")]
                 ]
-}
      English -> [] --TODO

------------------------------------------------------------
fpAnalysis :: Int -> Fspc -> Options ->  [Block]
fpAnalysis lev fSpec flags = header ++ caIntro ++ fpa2Blocks
 where 
  header :: [Block]
  header = labeledHeader lev chpFPAlabel (case (language flags) of
                                Dutch   ->  "Functiepunt Analyse"   
                                English ->  "Function Point Analysis"
                               )
  tableFPAlabel = "tableFPA"
  caIntro :: [Block]
  caIntro = 
   (case (language flags) of
      Dutch   -> [Para
                  [ Str "De specificatie van "
                  , Str (name fSpec)
                  , Str " is geanalyseerd door middel van een functiepuntentelling"
                  , xrefCitation "IFPUG"
                  , Str "."
                  ]]
      English -> [Para
                  [ Str "The specification of "
                  , Str (name fSpec)
                  , Str " has been analysed by counting function points"
                  , xrefCitation "IFPUG"
                  , Str "."
                  ]]
   )
  --TODO -> is an fpa on themes correct or should it be on the total fspec, or should it not matter, i.e. is the sum of services in the fspec equivalent to the sum of services of all themes? 
  --Table [Inline] [Alignment] [Double]      [[Block]] [[[Block]]]
  --      Caption  Clm algnmt  rel.clm.width clm hdrs  rows
  fpa2Blocks :: [Block]
  fpa2Blocks  = 
       [Table [Str "Function Point Analysis", xrefLabel tableFPAlabel] 
              [AlignLeft, AlignRight] --TODO -> how do I specify drawing of lines?
              [0.25,0.1] --TODO -> can't this be automatic or something
              [[Plain [Space]],[Plain [Str "points"]]] 
              [ [ [Plain [Str (name p)]]
        --        , [Plain [Str (show $ nFpoints t)]] ] 
                  , [Plain [Str "?"]] ] --TODO -> there is a loop in fspc->ftheme->funit->fviewdef&servicespec, coming from adl2fspec (remainingDS & pats)
              | p<-patterns fSpec]
       ]  
------------------------------------------------------------
glossary :: Int -> Fspc -> Options ->  [Block]
glossary _ _ _ = []  --TODO
------------------------------------------------------------
type Proof expr = [(expr,[String],String)]
showProof :: (expr->String) -> Proof expr -> String
showProof sh [(expr,_,_)]        = "\n      "++sh expr++"\n"
showProof sh ((expr,ss,equ):prf) = "\n      "++sh expr++
                                   "\n"++(if null ss then "\n   "++equ else if null equ then chain " " ss else "   "++equ++" { "++chain "; " ss++" }")++
                                   showProof sh prf
                                   --where e'= if null prf then "" else let (expr,_,_):_ = prf in showHS options "" expr 
showProof _  []                  = ""
-----Linguistic goodies--------------------------------------

count :: Options -> Int -> String -> String
count flags n x
 = case (language flags, n) of
      (Dutch  , 0) -> "geen "++plural Dutch x
      (Dutch  , 1) -> "een "++x
      (Dutch  , 2) -> "twee "++plural Dutch x
      (Dutch  , 3) -> "drie "++plural Dutch x
      (Dutch  , 4) -> "vier "++plural Dutch x
      (Dutch  , 5) -> "vijf "++plural Dutch x
      (Dutch  , 6) -> "zes "++plural Dutch x
      (Dutch  , _) -> show n++" "++plural Dutch x
      (English, 0) -> "no "++plural English x
      (English, 1) -> "one "++x
      (English, 2) -> "two "++plural English x
      (English, 3) -> "three "++plural English x
      (English, 4) -> "four "++plural English x
      (English, 5) -> "five "++plural English x
      (English, 6) -> "six "++plural English x
      (English, _) -> show n++" "++plural English x

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
xrefCitation :: String -> Inline    -- uitbreidbaar voor andere rendering dan LaTeX
xrefCitation myLabel = TeX ("\\cite{"++myLabel++"}")

--Image [Inline] Target
--      alt.text (URL,title)
xrefFigure1 :: Picture -> [Inline]
xrefFigure1 pict = 
   [ TeX "\\begin{figure}[htb]\n\\begin{center}\n\\scalebox{.3}[.3]{"
   , Image [Str $ "Here, "++fullPng pict++ " should have been visible"] ((reference pict), (title pict))
   , TeX "}\n"
   , TeX ("\\caption{"++title pict++"}\n") 
   , xrefLabel (reference pict)
   , TeX "\n\\end{center}\n\\end{figure}"]

addinfix :: Inline -> [[Inline]] -> [Inline] 
addinfix _ [] = [] --tail will not be on empty list
addinfix delim xs = tail [inline' | inlines<-postfix, inline'<-inlines]
   where
   postfix :: [[Inline]] 
   postfix = [delim:x|x<-xs] 

printlb :: [Inline]
printlb = [TeX "\n \\linebreak ", Str "\n"]


--DESCR -> pandoc print functions for Adl data structures
---------------------------------------
-- LaTeX math markup
---------------------------------------

class ShowMath a where
 showMath :: a -> String
 showMathcode :: Fspc -> a -> String
 showMathcode fSpec x = showMath x

instance ShowMath Rule where
 showMath r = error ("!Fatal (module Fspec2Pandoc 585): Please supply specification of the context in showMath "++showADL r)
 showMathcode fSpec r@(Sg p rule expla sgn nr pn signal) = "\\verb#SIGNAL # \\id{"++name signal++"}\\ \\verb# ON #"++ showMathcode fSpec rule
 showMathcode fSpec r@(Fr d expr _) = showMath d ++ "\n" ++ show (name d)++" = "++showMathcode fSpec expr
 showMathcode fSpec r
  | rrsrt r==Truth = "\\verb#ALWAYS # "++showMathcode fSpec (rrcon r)
  | rrsrt r==Implication = showMathcode fSpec (rrant r) ++"\\ \\subs\\ "++showMathcode fSpec (rrcon r)
  | rrsrt r==Equivalence = showMathcode fSpec (rrant r) ++"\\ =\\ " ++showMathcode fSpec (rrcon r)

instance ShowMath Expression where
 showMath e           = (showchar.insParentheses) e
 showMathcode fSpec e = (showchar.insParentheses.disambiguate fSpec.mphatsoff) e

showchar (Tm mph) = showMath mph
showchar (Fu [])  = "\\cmpl{\\full}"
showchar (Fu fs)  = chain "\\cup" [showchar f| f<-fs]     -- union
showchar (Fi [])  = "\\full"
showchar (Fi fs)  = chain "\\cap" [showchar f| f<-fs]     -- intersection
showchar (Fd [])  = "\\cmpl{\\iden}"
showchar (Fd ts)  = chain "\\relAdd" [showchar t| t<-ts]  -- relative addition (dagger)
showchar (F [])   = "\\iden"
showchar (F ts)   = chain "\\compose" [showchar t| t<-ts] -- relative multiplication (semicolon)
showchar (K0 e')  = "\\kleenestar{"++showchar e'++"}"
showchar (K1 e')  = "\\kleeneplus{"++showchar e'++"}"
showchar (Cp e')  = "\\cmpl{"++showchar e'++"}"
showchar (Tc f)   = "("++showchar f++")"

instance ShowMath Morphism where
 showMath mph@(Mph nm pos atts sgn@(a,b) yin s)
  = if yin then mstr else "\\flip{"++mstr++"}"
    where
      mstr = "\\id{"++latexEsc (name mph)++"}"++
             if null (mphats mph)
             then (if yin && sgn==(source s, target s) || not yin && sgn==(target s,source s) then "" else showSign [a,b])
             else showSign (mphats mph)
 showMath (I atts g s yin)
  = if null atts then "\\iden" else "\\ident{"++showSign atts++"}"
 showMath (V atts (a,b))
  = if null atts then "\\full" else "\\fullt{"++showSign atts++"}"
 showMath m@(Mp1{})
  = "'"++mph1val m++"'"++(showSign [mph1typ m])


instance ShowMath Declaration where
 showMath decl@(Sgn nm a b props prL prM prR cs expla _ _ sig _)
  = if sig then "\\verb#SIGNAL# "++"\\id{"++latexEsc nm++")" else
    "\\declare{"++latexEsc nm++"}{"++latexEsc (name a)++"}{"++latexEsc (name b)++"}"
--       (if null props then "" else showL(map showMath props))++
--       (if null(prL++prM++prR) then "" else " \\verb#PRAGMA# "++chain " " (map show [prL,prM,prR]))++
--       (if null expla then "" else " \\verb#EXPLANATION# \"\\text{"++expla++"}\"")
 showMath (Isn g s)
  = "\\iden"
 showMath (Vs g s)
  = "\\full"
 showMath (Iscompl g s)
  = "\\cmpl{\\iden}"


-- matharray :: Options -> [Block] -> [Block]
matharray flags regels
  = case fspecFormat flags of
      FLatex -> [Plain $ 
                  [ TeX $ " \\( \\begin{array}{rcl} \n"
                  , TeX $ chain "&" [ entry | regel<-regels, entry<-regel] ++ "\\\\" 
                  , TeX $ " \\end{array} \\) "]
                ]            
      _      -> [Plain $ 
                  [ Str "???" ]
                ]            

tabular flags n regels
  = case fspecFormat flags of
      FLatex -> [Plain $ 
                  [ TeX $ "\\begin{tabular}{"++['l'|_<-[1..n]]++"}\n" ++
                          chain "\\\\\n" [ chain "&" [ entry | entry<-regel] | regel<-regels] ++
                          "\\end{tabular}" ]
                ]            
      _      -> [Plain $ 
                  [ Str "???" ]
                ]            

latexEscShw x = latexEsc (show x)
latexEsc x
 = f x
   where f "" = ""
         f ('_':str) = "\\underline{\\ }"++f str
         f (c:str)   = c: f str
