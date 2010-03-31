{-# OPTIONS_GHC -Wall #-}
--TODO -> May be we can look at GetText function for help with internationalization. Brian O'Sullivan is working (has started) on an internationalization library. Maybe some day...
module Fspec2Pandoc (fSpec2Pandoc,laTeXtemplate)
where
import Char
import Collection       (Collection (..))
import Adl
import Data.Plug
import Picture
import Data.Fspec
import Strings          (upCap, commaNL, commaEng, chain)
import Text.Pandoc  
  --Als de compiler hierover struikelt, dan moet je pandoc installeren. Dat is overigens in de volgende 3 stappen:
                          -- 1) Eerst installeer je Cabal (zie http://www.haskell.org/cabal/) en dan roep je op je command line: 
                          -- 2) cabal-install pandoc  (onder windows: cabal install pandoc)
                          -- 3) Het kan zijn dat dit nog niet werkt, zie http://groups.google.com/group/pandoc-discuss/browse_thread/thread/a8fc3a627aeec7f2
                          --    als dat het geval is, kan deze module worden overruled in Generators.hs    
                          -- Built on pandoc 1.4                             
import Version          (versionbanner)
import Languages        (Lang(..))
import PredLogic        (showPredLogic)
import Options hiding (services) --importing (Options(..),FspecFormat(..))
import NormalForms      (conjNF) -- ,proofPA)  Dit inschakelen voor het bewijs...
import Rendering.AdlExplanation
import Rendering.ClassDiagram
import Switchboard      (switchboard1)
import Classes.Graphics (makePicture)
import FPA
import Statistics
import Rendering.PandocAux
import Rendering.InfTree2Pandoc

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
    where meta = Meta titl authors date
          titl = [Str (case (language flags) of
                        Dutch   -> "Functionele Specificatie van "
                        English -> "Functional Specification of "
                      )]
               ++[Quoted SingleQuote [Str (name fSpec)] ] 
          authors = case (language flags) of
                         Dutch   -> [[Str "Auteur(s) hier plaatsen"]
                                    ,[Str ("(Dit document is gegenereerd door "++versionbanner++")")]]
                         English -> [[Str "Put author(s) here"]
                                    ,[Str ("(This document was generated by "++versionbanner++")")]]
          date = [Str (show(genTime flags))]
          
          (docContents,pictures)
           = if theme flags == "gerard" then ([pandoctree(rrtyp_proof r)|r<-rules fSpec],[])
             else
             ( introduction       level fSpec flags  ++
               designPrinciples   level fSpec flags  ++
               caTxt                                 ++
               daTxt                                 ++
               [b| (blocks,_)<-svcs, b<-blocks]      ++
               if not studentversion then fpAnalysis level fSpec flags else []  ++
               glossary level fSpec flags
             , [daPic]++caPics++[p| (_,pics)<-svcs, p<-pics] )
             where svcs = [serviceChap level fSpec flags svc | svc  <-services fSpec,not studentversion]
                   (daTxt,daPic)  = dataAnalysis       level fSpec flags
                   (caTxt,caPics) = conceptualAnalysis level fSpec flags
                   studentversion = theme flags == "student"
          level = 0 --1=chapter, 2=section, 3=subsection, 4=subsubsection, _=plain text
------------------------------------------------------------                

introduction :: Int -> Fspc -> Options ->  [Block]
introduction lev fSpec flags = header ++ introContents (language flags)
    where 
        header = labeledHeader lev chpintrolabel (case language flags of
                                                     Dutch   ->  "Inleiding"   
                                                     English ->  "Introduction"
                                                 )
        --TODO: different intro for theme flags == "student"
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
                , Str " describes the data sets upon which ", Str (name fSpec), Str " is built. "
                , Str "Each service is described in a self contained way in a chapter of its own, ensuring that builders can focus on building a single service at a time. "
                , Str "Together, these services fulfill all commitments from chapter ", xrefReference chpFRlabel, Str ". "
                ]]  
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
        Dutch   -> [ Para
                     [ Str "Dit hoofdstuk beschrijft de functionele eisen ten behoeve van ", Str (name fSpec), Str ". "
                     , Str "Elke afspraak die gebruikers gezamenlijk naleven "
                     , Str "en door ", Str (name fSpec), Str " moet worden ondersteund, "
                     , Str "is opgenomen als functionele eis in dit hoofdstuk. "
                     , Str "Elke eis is voorzien van een nummer, die in volgende hoofdstukken gebruikt wordt om naar deze eis te verwijzen."
                     ]
                   , Para
                     [ Str "Formuleringen in dit hoofdstuk dienen zorgvuldig te worden getoetst met en door "
                     , Str "al degenen die op welke wijze dan ook de noodzakelijke kennis en voldoende autoriteit bezitten. "
                     , Str "Zij zijn immers verantwoordelijk voor de geldende regels. "
                     , Str "De hoofdarchitect is verantwoordelijk voor de onderlinge consistentie van deze afspraken "
                     , Str "en het bouwbaar zijn van het daaruit afgeleide systeem. "
                     , Str "Om deze reden schrijft de architect de afspraken zelf op, "
                     , Str "om ze te laten toetsen door de betrokkenen uit de organisatie. "
                     , Str "Van het voorliggende document is dit hoofdstuk het enige dat het fiat van gebruikers nodig heeft. "
                     , Str "Alle hierop volgende hoofdstukken zijn technisch van aard en bedoeld voor bouwers, testers en auditors. "
                     ]]
        English -> [ Para
                     [ Str "This chapter defines the functional requirements of ", Str (name fSpec), Str ". "
                     , Str "Each requirement users must fulfill "
                     , Str "by support of ", Str (name fSpec), Str ", "
                     , Str "serves as a function requirement in this chapter. "
                     , Str "Each requirement has a unique number, which is used in subsequent chapters for reference."
                     ]
                   , Para
                     [ Str "The precise phrasing of each requirement must therefore be scrutinized "
                     , Str "with and by those who have the knowledge and who are responsible for the actual rules. "
                     , Str "The chief architect is responsible for the consistency of all rules "
                     , Str "and for a buildable design. "
                     , Str "For this reason, the rules are written by the architect "
                     , Str "and validated by the apporopriate stakeholders. "
                     , Str "The current chapter requires user approval on behalf of the patron. "
                     , Str "All following chapters are technical and are meant for builders, testers and auditors. "
                     ]]

  dpRule [] n seenConcs seenDeclarations = ([], n, seenConcs, seenDeclarations)
  dpRule (r:rs) n seenConcs seenDeclarations
   = ( [ [Para (symDefLabel c: makeDefinition flags (name c) (cddef cd))] |(c,cd)<-cds]++
       [ [Para [symReqLabel d, Str$ explainDecl flags d]] |d<-nds] ++
       [ [Para [symReqLabel r, Str$ explainRule flags r]] ] ++ dpNext
     , n''
     , seenCs'
     , seenDs'
     )
     where
      ncs    = concs r >- seenConcs                                             -- all concepts that are used for the first time
      cds    = [(c,cd)| c<-ncs, cd<-conceptDefs fSpec, cdnm cd==name c]         -- lookup their concept definitions, where available
      seenCs = concs r `uni` seenConcs
      nds    = [d|d<-decls r, explainDecl flags d/=""] >- seenDeclarations      -- all declarations that are used for the first time
      seenDs = [d|d<-decls r, explainDecl flags d/=""] `uni` seenDeclarations   -- all declarations that are used for the first time
      n' = n+length cds+length nds+1
      ( dpNext, n'', seenCs', seenDs') = dpRule rs n' seenCs seenDs

  dpRequirements :: [Block]
  dpRequirements
   = dpSections dpRule (rd (map r_pat (rules fSpec++signals fSpec))) [] [] 1
     where
  --TODO -> It may be nice to print the class of the dataset from the class diagram
      dpSections _              -- a function that assembles the text for one rule.
                 []             -- There are no more patterns left. There may be material left, though...
                 seenConcepts   -- All concepts that have been defined in earlier sections
                 seenRelations  -- All relations whose multiplicities have been defined in earlier sections.
                 i              -- unique definition numbers (start at 1)
       = if emptySection then [] else 
            [Para [Str intro]] ++ --new section to explain this theme
            [OrderedList (i, Decimal, DefaultDelim) [[b]|b<-paraConcs ++ paraDecls]]   -- tells which rules and signals are being introduced
        where
         intro        = case language flags of
                         English -> "At the end of this chapter, the following definitions have to be made for the sake of completeness."
                         Dutch   -> "Aan het eind van dit hoofdstuk gebiedt de volledigheid nog om het volgende te definieren."
         emptySection = null conceptdefs && null newRelations
         conceptdefs  = [(c,cd)| c<-newConcepts, cd<-conceptDefs fSpec, cdnm cd==name c]  -- show only those definitions that are actually used in this specification.
         newConcepts  = concs newRelations >- seenConcepts
         newRelations = [d| d@Sgn{}<-decls ([r| r<-rules fSpec++signals fSpec]), decusr d, d `notElem` seenRelations, not (null (multiplicities d))]
         paraConcs    = [Para (symDefLabel c: makeDefinition flags (name c) (cddef cd)) |(c,cd)<-conceptdefs]
         paraDecls    = [Para [symReqLabel d, Str$ explainMult flags d] |d<-newRelations]
      dpSections dpRul          -- a function that assembles the text for one rule.
                 (thm:thms)     -- The name of the patterns that are used in this specification.
                 seenConcepts   -- All concepts that have been defined in earlier sections
                 seenRelations  -- All relations whose multiplicities have been defined in earlier sections.
                 i              -- unique definition numbers (start at 1)
       = if emptySection then [] else [Header (lev+1) [Str thm]]  --new section to explain this theme
         ++ sctConcepts  -- tells which new concepts are introduced in this section.
         ++ [ OrderedList (i, Decimal, DefaultDelim) (sctRules ++ sctSignals)| not (null (sctRules ++ sctSignals)) ]   -- tells which rules and signals are being introduced
         ++ dpSections dpRul thms (seenCss) (seenDss) i''
        where
         emptySection = null newConcepts && null (sctRules ++ sctSignals)
         (sctRules,   i',  seenCrs, seenDrs) = dpRul patRules i seenConcepts seenRelations
         (sctSignals, i'', seenCss, seenDss) = dpRul patSignals i' seenCrs seenDrs
         conceptdefs  = [(c,cd)| c<-concs fSpec, cd<-conceptDefs fSpec, cdnm cd==name c]  -- show only those definitions that are actually used in this specification.
         patRules     = [r| r<-rules fSpec,   r_pat r==thm, r_usr r]
         patSignals   = [s| s<-signals fSpec, r_pat s==thm]
         newConcepts  = concs (patRules++patSignals) >- seenConcepts
         newRelations = filter (not.isIdent) (decls (patRules++patSignals) >- seenRelations)
         sctConcepts
          = if null newConcepts then [] else
              case language flags of
               Dutch   ->  [ Para $ (case [name c|(c,_)<-conceptdefs, c `elem` newConcepts] of
                                      []  -> []
                                      [c] -> [ Str $ "Deze sectie introduceert het concept "
                                             , Str $ c
                                             , Str $ ". "]
                                      cs  -> [ Str $ "Deze sectie introduceert de concepten "
                                             , Str $ commaNL "en" cs
                                             , Str $ ". "]
                                    )++
                                    (case [name c| c<-newConcepts, c `notElem` [c'| (c',_)<-conceptdefs]] of
                                      []  -> []
                                      [c] -> [ Str $ "Concept "
                                             , Str $ c
                                             , Str $ " wordt in deze sectie geintroduceerd zonder definitie. "]
                                      cs  -> [ Str $ "Deze sectie introduceert concepten "
                                             , Str $ commaNL "en" cs
                                             , Str $ " zonder definitie. "]
                                    )
                           ]
               English ->  [ Para $ (case [name c|(c,_)<-conceptdefs, c `elem` newConcepts] of
                                      []  -> []
                                      [c] -> [ Str $ "This section introduces concept "
                                             , Str $ c
                                             , Str $ ". "]
                                      cs  -> [ Str $ "This section introduces concepts "
                                             , Str $ commaEng "and" cs
                                             , Str $ ". "]
                                    )++
                                    (case [name c| c<-newConcepts, c `notElem` [c'| (c',_)<-conceptdefs]] of
                                      []  -> []
                                      [c] -> [ Str $ "Concept "
                                             , Str $ c
                                             , Str $ " is introduced in this section without a definition."]
                                      cs  -> [ Str $ "This section introduces concepts "
                                             , Str $ commaEng "and" cs
                                             , Str $ " without definition. "]
                                    )
                           ]
     
------------------------------------------------------------
conceptualAnalysis :: Int -> Fspc -> Options -> ([Block],[Picture])
conceptualAnalysis lev fSpec flags = (header ++ caIntro ++ caBlocks , pictures)
  where
  (caBlocks,pictures) = ( [b| (blocks,_)<-ca, b<-blocks], [picture| (_,picture)<-ca] )
                        where ca=caSections (patterns fSpec)
  header :: [Block]
  header = labeledHeader lev chpCAlabel (case language flags of
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
  caSections pats = iterat pats 1 [] []
   where
    iterat :: [Pattern] -> Int -> [Concept] -> [Declaration] -> [([Block],Picture)]
    iterat [] _ _ _ = []
    iterat (pat:ps) i seenConcepts seenDeclarations
     = ( [Header (lev+1) [Str (name pat)]]    -- new section to explain this theme
       ++ (if (useGraphics flags) 
            then 
              (case language flags of             -- announce the conceptual diagram
                Dutch   -> [Para [x | x<-[Str "Figuur ", xrefReference (figlabel pict), Str " geeft een conceptuele analyse van dit thema."]] ]
                English -> [Para [x | x<-[Str "Figure ", xrefReference (figlabel pict), Str " shows a conceptual analysis of this theme."]] ]
              ) ++ [Plain (xrefFigure1 pict)]          -- draw the conceptual diagram
            else []
          )
       ++ (if null blocks then [] else [DefinitionList blocks])
       , pict):  iterat ps i'' seenCss seenDss
       where
         pict = makePicture flags fSpec pat   -- the Picture that represents this service's knowledge graph
         blocks  :: [([Inline], [[Block]])]
         blocks = sctRules ++ sctSignals
         (sctRules,   i',  seenCrs, seenDrs) = dpRule patRules i seenConcepts seenDeclarations
         (sctSignals, i'', seenCss, seenDss) = dpRule patSignals i' seenCrs seenDrs
         patRules     = [r| r<-rules fSpec,   r_pat r==name pat, r_usr r]
         patSignals   = [s| s<-signals fSpec, r_pat s==name pat]

    dpRule [] n seenConcs seenDeclarations = ([], n, seenConcs, seenDeclarations)
    dpRule (r:rs) n seenConcs seenDeclarations
     = ( [ ( [Str (name r)]
           , [ [ Plain [Str (explainRule flags r)]] ++
               [ Plain (text1)| not (null nds)] ++
               pandocEqnArray [ ([TeX ("\\id{"++latexEsc (name d)++"}")], [TeX ":"], [TeX ("\\id{"++latexEsc (name (source d))++"}"++(if isFunction d then "\\fun" else "\\times" )++"\\id{"++latexEsc (name (target d))++"}"), symDefLabel d])
                              |d<-nds] ++
               [ Plain (text2)| not (null rds)] ++
               [ Plain (text3)| isSignal r] ++
               pandocEquation [TeX (if isSignal r then showMathcode fSpec (conjNF (Cp (normExpr r))) else showMathcode fSpec r), symDefLabel r] ++
               [ Plain (text4) | length nds>1]
             ] 
           ) ] ++ dpNext
       , n'
       , seenCs
       , seenDs
       )
       where
        text1
         = case length nds of
             1 -> let d = head nds in
                  [TeX ("In order to formalize this, we introduce "++(if isFunction d then "function" else "relation")++"~"),symDefRef d,TeX ":"]
             l -> [TeX "The formalization (equation~", symDefRef r,TeX (") requires the following "++count flags l "relation"++".")]
        text2
         = (case (length nds,length rds,language flags) of
             (0,1,Dutch)   -> [Str "Definitie ", symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ") wordt gebruikt"]
             (0,1,English) -> [Str "We use definition ", symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ")"]
             (0,_,Dutch)   -> [Str "We gebruiken definities ", TeX (commaNL "en" [str |d<-rds, let TeX str=symDefRef d])]
             (0,_,English) -> [Str "We use definitions ", TeX (commaEng "and" [str |d<-rds, let TeX str=symDefRef d])]
             (_,1,Dutch)   -> [Str "Daarnaast gebruiken we definitie ", symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ")"]
             (_,1,English) -> [Str "Besides, we use definition ", symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ")"]
             (_,_,Dutch)   -> [Str "Daarnaast gebruiken we definities ", TeX (commaNL "en" [str |d<-rds, let TeX str=symDefRef d])]
             (_,_,English) -> [Str "Besides, we use definitions ", TeX (commaEng "and" [str |d<-rds, let TeX str=symDefRef d])]
           )++
           (case (length nds,language flags) of
             (1,Dutch)   -> [TeX " om eis~", symReqRef r, TeX " (pg.~", symReqPageRef r, Str ") te formaliseren:"]
             (1,English) -> [TeX " to formalize requirement~", symReqRef r, TeX " (page~", symReqPageRef r, Str "):"]
             _           -> [Str ". "]
           )
        text3
         = case language flags of
                 Dutch   -> [Str " Een signaal wordt afgegeven als:"]
                 English -> [Str " A signal is produced when:"]
        text4
         = case language flags of
             Dutch   -> [TeX "Dit komt overeen met eis~", symReqRef r, TeX " op pg.~", symReqPageRef r, Str "."]
             English -> [TeX "This corresponds to requirement~", symReqRef r, TeX " on page~", symReqPageRef r, Str "."]
        ncs = concs r >- seenConcs            -- newly seen concepts
        cds = [(c,cd)| c<-ncs, cd<-conceptDefs fSpec, cdnm cd==name c]    -- ... and their definitions
        nds = [d| d<-decls r >- seenDeclarations, isSgn d]     -- newly seen declarations
        rds = [d| d<-decls r `isc` seenDeclarations, isSgn d]  -- previously seen declarations
        ( dpNext, n', seenCs,  seenDs ) = dpRule rs (n+length cds+length nds+1) (concs r `uni` seenConcs) (decls r `uni` seenDeclarations)

------------------------------------------------------------
--DESCR -> the data analysis contains a section for each class diagram in the fspec
--         the class diagram and multiplicity rules are printed
dataAnalysis :: Int -> Fspc -> Options -> ([Block],Picture)
dataAnalysis lev fSpec flags = ( header ++ daContents ++ daAssociations remainingDecls ++ [b | p<-datasets fSpec, b<-daPlug p] , classDiagramPicture )
 where 
  remainingDecls = declarations fSpec >- [d | p<-datasets fSpec, d<-decls p]

  header :: [Block]
  header = labeledHeader lev chpDAlabel (case language flags of
                                              Dutch   ->  "Gegevensstructuur"   
                                              English ->  "Data structure"
                                        )
  daContents :: [Block]
  daContents = 
   (case language flags of
     Dutch   -> [Para $
                  ( if (useGraphics flags) 
                     then 
                       [ Str $ "De eisen, die in hoofdstuk "
                       , xrefReference chpFRlabel
                       , Str $ " beschreven zijn, zijn in een gegevensanalyse vertaald naar het klassediagram van figuur "
                       , xrefReference (figlabel classDiagramPicture) ]
                     else []
                  )++
                  [ Str $ (case length (classes classDiagram) of
                            0 -> ". Er zijn"
                            1 -> ". Er is precies een gegevensverzameling,"
                            _ -> ". Er zijn "++count flags (length (classes classDiagram)) "gegevensverzameling"++","
                          )
                  , Str $ " "++count flags (length (assocs classDiagram)) "associatie"++","
                  , Str $ " "++count flags (length (geners classDiagram)) "generalisatie"++" en"
                  , Str $ " "++count flags (length (aggrs classDiagram)) "aggregatie"++"."
                  , Str $ " "++name fSpec++" kent in totaal "++count flags (length (concs fSpec)) "concept"++"."
                  ]]
     English -> [Para $
                  [ Str $ "The requirements, which are listed in chapter "
                  , xrefReference chpFRlabel
                  , Str $ ", have been translated into the class diagram in figure "
                  , xrefReference (figlabel classDiagramPicture)
                  , Str $ (case length (classes classDiagram) of
                            0 -> ". There are"
                            1 -> ". There is one data set,"
                            _ -> ". There are "++count flags (length (classes classDiagram)) "data set"++","
                          )
                  , Str $ " "++count flags (length (assocs classDiagram)) "association"++","
                  , Str $ " "++count flags (length (geners classDiagram)) "generalisation"++", and"
                  , Str $ " "++count flags (length (aggrs classDiagram)) "aggregation"++"."
                  , Str $ " "++name fSpec++" has a total of "++count flags (length (concs fSpec)) "concept"++"."
                  ]] --TODO
   ) ++ [ Plain $ xrefFigure1 classDiagramPicture ]  -- TODO: explain all multiplicities]

  classDiagram :: ClassDiag
  classDiagram = cdAnalysis fSpec flags

  classDiagramPicture :: Picture
  classDiagramPicture = makePicture flags fSpec classDiagram
--      where
--       cdDot = classdiagram2dot classDiagram

-- The properties of various declations are documented in different tables.
-- First, we document the heterogeneous properties of all relations
-- Then, the homogeneous poperties are given, and finally
-- the signals are documented.
  daAssociations :: [Declaration] -> [Block]
  daAssociations ds
   = [ if language flags==Dutch
       then Para [ Str $ upCap (name fSpec)++" heeft de volgende associaties en multipliciteitsrestricties. "
                 ]
       else Para [ Str $ upCap (name fSpec)++" has the following associations and multiplicity constraints. "
                 ]
     , Para  $ [ TeX $ "\\begin{tabular}{|l|cc|}\\hline\n"
               , if language flags==Dutch
                 then TeX $ "relatie&totaal&surjectief\\\\ \\hline\\hline\n"
                 else TeX $ "relation&total&surjective\\\\ \\hline\\hline\n"
               ]++
               [ TeX $ chain "&" [ if source d==target d
                                   then "\\(\\signt{"++latexEsc (name d)++"}{"++latexEscShw (source d)++"}\\)"              -- veld
                                   else "\\(\\signat{"++latexEsc (name d)++"}{"++latexEscShw (source d)++"}{"++latexEscShw (target d)++"}\\)"              -- veld
                                 , if isTot d then "\\(\\surd\\)" else ""
                                 , if isSur d then "\\(\\surd\\)" else ""
                                 ]++"\\\\\n"
               | d@Sgn{}<-ds, decusr d, not (isProp d)
               ]++
               [ TeX $ "\\hline\n\\end{tabular}"
               ]
     ]++
-- the homogeneous properties:
     [ Para [ if language flags==Dutch
                then TeX $ "Een relatie, \\id{"++latexEsc (name d)++"}, is homogeen en heeft de volgende eigenschappen: "
                else TeX $ "One relation, \\id{"++latexEsc (name d)++"}, is homogeneous and has the following properties: "]
     | length hMults==1, d<-hMults ]++
     [ Para [ if language flags==Dutch
                then TeX $ "In aanvulling daarop hebben de homogene relaties de volgende eigenschappen: "
                else TeX $ "Additionally, the homogeneous relations come with the following properties: "]
     | length hMults>1 ]++
     [ Para  $ [ TeX $ "\\begin{tabular}{|l|ccccc|}\\hline\n"
               , if language flags==Dutch
                 then TeX $ "relatie&Rfx&Trn&Sym&Asy&Prop\\\\ \\hline\\hline\n"
                 else TeX $ "relation&Rfx&Trn&Sym&Asy&Prop\\\\ \\hline\\hline\n"
               ]++
               [ TeX $ chain "&" [ "\\signt{"++latexEsc (name d)++"}{"++latexEscShw (source d)++"}"              -- veld
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
     | length hMults>0 ]++
-- the signals
     [ Para [ if language flags==Dutch
                then TeX $ "Er is een enkel signaal: \\id{"++latexEsc (name d)++"}."
                else TeX $ "There is but one signal: \\id{"++latexEsc (name d)++"}." ]
     | length sgnls==1, d<-sgnls ]++
     [ Para [ if language flags==Dutch
                then TeX $ "De volgende signalen bestaan: "++commaNL "en" ["\\id{"++latexEsc (name d)++"}" | d<-sgnls]
                else TeX $ "The following signals exist: "++commaEng "and" ["\\id{"++latexEsc (name d)++"}" | d<-sgnls]]
     | length sgnls>1 ]
     where
      hMults  = [d| d@Sgn{}<-declarations fSpec, homogeneous d, decusr d]
      sgnls   = [d| d@Sgn{}<-ds, isSignal d] -- all signal declarations are not user defined, so this is disjoint from hMults
-- The properties of various declations are documented in different tables.
-- First, we document the heterogeneous properties of all relations
-- Then, the homogeneous poperties are given, and finally
-- the signals are documented.

  daAttributes :: Plug -> [Block]
  daAttributes p
   = [ if language flags==Dutch
       then Para [ Str $ "De attributen van "++name p++" hebben de volgende multipliciteitsrestricties. "
                 ]
       else Para [ Str $ "The attributes in "++name p++" have the following multiplicity constraints. "
                 ]
     , Para  $ [ TeX $ "\\begin{tabular}{|llcc|}\\hline\n"
               , if language flags==Dutch
                 then TeX $ "attribuut&type&verplicht&uniek\\\\ \\hline\\hline\n"
                 else TeX $ "attribute&type&compulsory&unique\\\\ \\hline\\hline\n"
               ]++
               [ TeX $ chain "&" [ latexEsc (fldname fld)
                                 , latexEscShw (target (fldexpr fld))
                                 , if fldnull fld then "" else "\\(\\surd\\)"
                                 , if flduniq fld then "\\(\\surd\\)" else ""
                                 ]++"\\\\\n"
               | fld<-fields p
               ]++
               [ TeX "\\hline\n\\end{tabular}"
               ]
     ]++
-- the homogeneous properties have already been reported in the general section of this chapter.
-- the signals
     [ Para [ if language flags==Dutch
                then TeX $ "Er is een enkel signaal: \\id{"++latexEsc (name d)++"}."
                else TeX $ "There is but one signal: \\id{"++latexEsc (name d)++"}." ]
     | length sgnls==1, d<-sgnls ]++
     [ Para [ if language flags==Dutch
                then TeX $ "De volgende signalen bestaan: "++commaNL "en" ["\\id{"++latexEsc (name d)++"}" | d<-sgnls]
                else TeX $ "The following signals exist: "++commaEng "and" ["\\id{"++latexEsc (name d)++"}" | d<-sgnls]]
     | length sgnls>1 ]
     where
      sgnls   = [d| d@Sgn{}<-declarations fSpec, isSignal d] -- all signal declarations are not user defined, so this is disjoint from hMults
{- voorgestelde multipliciteitenanalyse....
      clauses = rd [clause | Quad _ ccrs<-vquads fSpec, (_,shifts)<-cl_conjNF ccrs, clause<-shifts]
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
      strands (F fs) = [fs]
      strands _      = []    -- <--  we could maybe do better than this...
      tots = [d| t<-ts, inline t, d<-decls t]
      unis = [d| t<-is, inline t, d<-decls t]
      surs = [d| t<-ts, not (inline t), d<-decls t]
      injs = [d| t<-is, not (inline t), d<-decls t]
-}

  -- daPlugs describes data sets.
  -- These can be recognized by:
  --    1. the first field has the "unique" attribute on (otherwise it is a binary association)
  --    2. there is more than one field (otherwise it is a scalar).
  -- The text gives all rules that are maintained internally within the data structure,
  -- because they might very well be implemented as database integrity rules.
  -- Multiplicity rules are not reported separately, because they are already taken care of in the multiplicity tables.
  -- Plugs that are associations between data sets and scalars are ignored.

  daPlug :: Plug -> [Block]
  daPlug p
   = if null content then [] else plugHeader ++ content
     where
       plugHeader = labeledHeader (lev+1) ("sct:plug "++name p) (name p)
       content = daAttributes p ++ plugRules ++ plugSignals
       plugRules
        = case language flags of
           English -> case [r| r@(Ru{})<-rules fSpec, r_usr r, null (decls r >- decls p)] of
                       []  -> []
                       [r] -> [ Para [ Str "This data set shall maintain the following integrity rule. " ]
                              , Para [ Math DisplayMath $ showMathcode fSpec r]
                              ]
                       rs  -> [ Para [ Str "This data set shall maintain the following integrity rules. " ]
                              , BulletList [[Para [Math DisplayMath $ showMathcode fSpec r]]| r<-rs ]
                              ]
           Dutch   -> case [r| r@(Ru{})<-rules fSpec, r_usr r, null (decls r >- decls p)] of
                       []  -> []
                       [r] -> [ Para [ Str "Deze gegevensverzameling handhaaft de volgende integriteitsregel. " ]
                              , Para [ Math DisplayMath $ showMathcode fSpec r]
                              ]
                       rs  -> [ Para [ Str "Deze gegevensverzameling handhaaft de volgende integriteitsregels. " ]
                              , BulletList [[Para [Math DisplayMath $ showMathcode fSpec r]]| r<-rs ]
                              ]
       plugSignals
        = case language flags of
           English -> case [r| r<-signals fSpec, null (decls r >- decls p)] of
                       []  -> []
                       [s] -> [ Para [ Str "This data set generates one signal. " ]
                              , Para [ Math DisplayMath $ showMathcode fSpec s]
                              ]
                       ss  -> [ Para [ Str "This data set generates the following signals. " ]
                              , BulletList [[Para [Math DisplayMath $ showMathcode fSpec s]]| s<-ss ]
                              ]
           Dutch   -> case [r| r<-signals fSpec, null (decls r >- decls p)] of
                       []  -> []
                       [s] -> [ Para [ Str "Deze gegevensverzameling genereert \\'e\\'en signaal. " ]  -- Zou "één" moeten zijn ipv "\\'e\\'en", maar dit geeft een lexical error in string/character literal (UTF-8 decoding error) in de Haskell compiler
                              , Para [ Math DisplayMath $ showMathcode fSpec s]
                              ]
                       ss  -> [ Para [ Str "Deze gegevensverzameling genereert de volgende signalen. " ]
                              , BulletList [[Para [Math DisplayMath $ showMathcode fSpec s]]| s<-ss ]
                              ]

------------------------------------------------------------
serviceChap :: Int -> Fspc -> Options -> Fservice ->  ([Block],[Picture])
serviceChap lev fSpec flags svc
 = ( header ++ svcIntro
      ++ (if useGraphics flags then txtKnowledgeGraph else [])
      ++ svcFieldTables
      ++ (if useGraphics flags && flgSwitchboard flags then txtSwitchboard else [])
   , [picKnowledgeGraph]++[picSwitchboard| flgSwitchboard flags]
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
                     [ Str $ svcInsDelConcepts ]++
                     [ Str $ svcAutoRules ] )
                 ]
      English -> [ Para
                    ([ Str $ "Service "++svcname++" operates from one instance of "++name (target (objctx (fsv_objectdef svc)))++"." ]++
                     f (objctx (fsv_objectdef svc))++
                     [ Str $ svcInsDelConcepts ]++
                     [ Str $ svcAutoRules ] )
                 ]
     where
      f (Tm _ _) = []
      f expr   = [Str $ showPredLogic flags (conjNF(F[v (S,source expr),expr]))]
  svcInsDelConcepts
   = let ics = fsv_creating svc>-fsv_deleting svc
         dcs = fsv_deleting svc>-fsv_creating svc
         ucs = fsv_deleting svc `isc` fsv_creating svc
     in case (language flags) of
      Dutch -> " "++
          if null ics && null dcs && null ucs then "Deze service maakt of verwijdert geen objecten langs geautomatiseerde weg." else
          if null ics && null dcs             then "Om regels te handhaven, mogen instanties van "++commaNL "en" (map name ucs)++" door deze service geautomatiseerd worden aangemaakt en verwijderd." else
          if null ics       &&       null ucs then "Om regels te handhaven, mag deze service instanties van "++commaNL "en" (map name ucs)++" geautomatiseerd verwijderen." else
          if             null dcs && null ucs then "Om regels te handhaven, mogen instanties van "++commaNL "en" (map name ucs)++" geautomatiseerd worden aangemaakt door deze service." else
          if                         null ucs then "Instanties van "++commaNL "en" (map name ucs)++" mogen worden toegevoegd en instanties van "++f dcs++" mogen worden verwijderd door deze service. Dat gebeurt geautomatiseerd en uitsluitend waar nodig om regels te handhaven." else
          if             null dcs             then "Deze service mag instanties van "++commaNL "en" (map name ics)++" creeren, terwijl instanties van "++commaNL "en" (map name ucs)++" ook verwijderd mogen worden. Alleen waar nodig mag dit plaatsvinden om regels te handhaven." else
          if null ics                         then "Deze service mag instanties van "++commaNL "en" (map name ucs)++" wijzigen, maar instanties van "++commaNL "en" (map name dcs)++" mogen alleen worden verwijderd. Dat mag slechts dan gebeuren wanneer dat nodig is voor het handhaven van regels." else
          "Deze service maakt nieuwe instanties van concept"++f ics++". Hij mag instanties van concept"++f dcs++" verwijderen, terwijl instanties van "++commaNL "en" (map name ucs)++" zowel gemaakt als vernietigd mogen worden."
          where f [x] = " "++name x++" "
                f xs  = "en "++commaNL "en" (map name xs)
      English -> " "++
          if null ics && null dcs && null ucs then "In this service, no objects are made or removed automatically." else
          if null ics && null dcs             then "In order to maintain rules, instances of "++f ucs++" may be created or deleted by this service automatically." else
          if null ics       &&       null ucs then "In order to maintain rules, instances of "++f dcs++" may be deleted automatically by this service." else
          if             null dcs && null ucs then "In order to maintain rules, instances of "++f ics++" may be automatically inserted by this service." else
          if                         null ucs then "Concept"++f ics++" may be inserted, and concept"++f dcs++" may be deleted by this service. This happens only if necessary for maintaining rules." else
          if             null dcs             then "By this service, concept"++f ucs++" may be changed, but"++f ics++" may be created but not deleted. This happens only if necessary for maintaining rules." else
          if null ics                         then "By this service, concept"++f ucs++" may be changed, but"++f dcs++" may only be deleted. This happens only if necessary for maintaining rules." else
          "This service can create new instances of concept"++f ics++". It may delete instances of concept"++f dcs++", and instances of concept"++f ucs++" may be either created and removed. Such actions will take place only in order to maintain rules."
          where f [x] = " "++name x
                f xs  = "s "++commaEng "and" (map name xs)
  svcAutoRules
   = let ars = rd [r|q<-fsv_quads svc, r<-[cl_rule (qClauses q)], r_usr r] -- rules that are maintained by automated functionality
         mrs = [r|r<-fsv_rules svc, r_usr r, r `notElem` ars]-- rules that may be affected, but are maintained manually
      --   mss = ""-- signals that can be emptied by this service
     in case (language flags) of
      Dutch ->   chain " " 
                 [ case length ars of
                    0 -> ""
                    1 -> " Regel "++name (head ars)++" wordt door deze service gehandhaafd zonder interventie van de gebruiker."
                    _ -> " Regels "++commaNL "en" (map name ars)++" worden door deze service gehandhaafd zonder interventie van de gebruiker. "
                 , case length mrs of
                    0 -> ""
                    1 -> " Regel "++name (head mrs)++" wordt door de gebruiker van deze service gehandhaafd."
                    _ -> "Regels "++commaNL "en" (map name mrs)++" worden door de gebruiker van deze service gehandhaafd. "
                 ]
      English -> chain " " 
                 [ case length ars of
                    0 -> ""
                    1 -> " Rule "++name (head ars)++" is being maintained by this service, without intervention of the user."
                    _ -> " Rules "++commaEng "and" (map name ars)++" are being maintained by this service, without intervention of the user."
                 , case length mrs of
                    0 -> ""
                    1 -> " Rule "++name (head mrs)++" is being maintained by the user."
                    _ -> " Rules "++commaEng "and" (map name mrs)++" are being maintained by the user."
                 ]
                 

  svcFieldTables
   = if null (fsv_fields svc) then [] else
     if length (fsv_fields svc)==1
     then [ Para  $ [ if language flags==Dutch
                      then Str $ "In deze service is het volgende veld zichtbaar: "
                      else Str $ "This service has one field: "
                    ]
          , head [b| BulletList [bs]<-[flds], b<-bs]
          ]
     else [ Para  $ [ if language flags==Dutch
                      then Str $ "In deze service zijn de volgende velden zichtbaar. "
                      else Str $ "This service has the following fields. "
                    ]
          , flds
          ]
     where flds :: Block
           flds = BulletList [recur (objctx (fsv_objectdef svc)) f| f<-fsv_fields svc]
            where recur :: Expression -> Field -> [Block]
                  recur e f | null (fld_sub f) = fld e f
                            | otherwise        = fld e f ++
                                                 [ BulletList [recur (F [e,fld_expr f']) f'| f'<-fld_sub f] ]
           fld e f = [ Para [ Str (fld_name f++if null cols then "" else "("++chain ", " cols++")") ]
                     , Para [ Str "display on start: ", Math InlineMath $ showMathcode fSpec (conjNF e) ]
                     ] {- ++
                     [ Para [ Str $ "exec on insert: "++ showECA fSpec "\n>     "  (fld_onIns f arg)]
                     | fld_insAble f, arg<-[error ("!TODO (module Fspec2Pandoc 707): hier moet een declaratie \"Delta\" staan")] ]
-}
            where cols = ["lijst"        | fld_list    f]++
                         ["verplicht"    | fld_must    f]++
                         ["nieuw"        | fld_insAble f]++
                         ["verwijderbaar"| fld_delAble f]
                     

  txtKnowledgeGraph :: [Block]
  txtKnowledgeGraph
   = (case language flags of                                     -- announce the knowledge graph
           Dutch   -> [Para [x | x<-[ Str "Figuur ", xrefReference (figlabel picKnowledgeGraph)
                                    , Str " geeft de kennisgraaf weer voor deze service."]] ]
           English -> [Para [x | x<-[ Str "Figure ", xrefReference (figlabel picKnowledgeGraph)
                                    , Str " shows the knowledge graph of this service."]] ]
     )
     ++ [Plain (xrefFigure1 picKnowledgeGraph)]                  -- draw the knowledge graph

  picKnowledgeGraph :: Picture
  picKnowledgeGraph = makePicture flags fSpec svc  -- the Picture that represents this service's knowledge graph
--     where
--      knGph = toDot fSpec flags svc                              -- the DotGraph String that represents this service's knowledge graph
--      kn    = printDotGraph knGph                                -- the String that represents this service's knowledge graph

  txtSwitchboard :: [Block]
  txtSwitchboard
   = (case language flags of                                     -- announce the switchboard diagram
           Dutch   -> [Para [x | x<-[ Str "Figuur ", xrefReference (figlabel picSwitchboard)
                                    , Str " geeft een schakelpaneel (switchboard diagram) weer voor deze service."]] ]
           English -> [Para [x | x<-[ Str "Figure ", xrefReference (figlabel picSwitchboard)
                                    , Str " shows a switchboard diagram of this service."]] ]
     )
     ++ [Plain (xrefFigure1 picSwitchboard)]                     -- draw the switchboard

  picSwitchboard :: Picture
  picSwitchboard = makePicture flags fSpec (switchboard1 fSpec svc) -- the Picture that represents this service's knowledge graph
--     where
--      sbGph = switchboard fSpec svc                              -- the DotGraph String that represents this service's knowledge graph
--      sb    = printDotGraph sbGph                                -- the String that represents this service's knowledge graph

{-  svcECA :: [Block]
  svcECA
   = case (language flags) of
      Dutch   -> [ Para [ Str "ECA rules:\n   ",Str "tijdelijk ongedocumenteerd" ] ]
                 [ Para
                    [ Str $ showECA fSpec "\n>     "  (normECA (eca arg) arg)
-- Dit inschakelen          ++"\n------ Derivation ----->"
--  voor het bewijs         ++showProof (showECA fSpec "\n>     ") (proofPA (ecaAction (eca arg)))
--                          ++"\n<------End Derivation --"
                    ]
                 | eca<-fsv_ecaRules svc, arg<-[error ("!TODO (module Fspec2Pandoc 754): hier moet een declaratie \"Delta\" staan")]
                 ]
      English -> [] --TODO
-}

------------------ Function Point Analysis --------------------
-- TODO: Engels en Nederlands netjes scheiden.
-- TODO: Andere formaten dan LaTeX ondersteunen.

fpAnalysis :: Int -> Fspc -> Options ->  [Block]
fpAnalysis lev fSpec flags = header ++ caIntro ++ fpa2Blocks
 where 
  header :: [Block]
  header = labeledHeader lev chpFPAlabel
                         (case language flags of
                               Dutch   ->  "Functiepunt Analyse"   
                               English ->  "Function Point Analysis"
                         )
  caIntro :: [Block]
  caIntro = 
   (case (language flags) of
      Dutch   -> [Para
                  [ Str "De specificatie van "
                  , Str (name fSpec)
                  , Str " is geanalyseerd door middel van een functiepuntentelling"
                  , xrefCitation "IFPUG"
                  , Str ". "
                  , Str $ "Dit heeft geresulteerd in een geschat totaal van "++(show.nFpoints) fSpec++" functiepunten."
                  ]]
      English -> [Para
                  [ Str "The specification of "
                  , Str (name fSpec)
                  , Str " has been analysed by counting function points"
                  , xrefCitation "IFPUG"
                  , Str ". "
                  , Str $ "This has resulted in an estimated total of "++(show.nFpoints) fSpec++" function points."
                  ]]
   )

  fpa2Blocks :: [Block]
  fpa2Blocks
   = case fspecFormat flags of
      FLatex -> [Para $ 
                  [ TeX $ "\\begin{tabular}{|l|l|r|}\\hline \n" ++
                          chain "&" ["data set", "analysis", "points"] ++"\\\\\\hline\n"++
                          chain "\\\\\n" [ chain "&" [latexEsc (name plug), latexEscShw (plfpa plug), (latexEscShw.fPoints.plfpa) plug]
                                         | plug<-datasets fSpec
                                         , fPoints (plfpa plug)>0] ++
                          "\\\\\\hline\\end{tabular}" ]
                ,Para $ 
                  [ TeX $ "\\begin{tabular}{|l|l|r|}\\hline \n" ++
                          chain "&" ["service", "analysis", "points"] ++"\\\\\\hline\n"++
                          chain "\\\\\n" [ chain "&" [latexEsc (name svc), latexEscShw (fsv_fpa svc), (latexEscShw.fPoints.fsv_fpa) svc] | svc<-services fSpec] ++
                          "\\\\\\hline\\end{tabular}" ]
                ]            
      _      -> [Plain $ 
                  [ Str "???" ]
                ]   
         
------------------------------------------------------------
glossary :: Int -> Fspc -> Options ->  [Block]
glossary _ _ _ = []  --TODO
------------------------------------------------------------
--type Proof expr = [(expr,[String],String)]
--showProof :: (expr->String) -> Proof expr -> String
--showProof sh [(expr,_,_)]        = "\n      "++sh expr++"\n"
--showProof sh ((expr,ss,equ):prf) = "\n      "++sh expr++
--                                   "\n"++(if null ss then "\n   "++equ else if null equ then chain " " ss else "   "++equ++" { "++chain "; " ss++" }")++
--                                   showProof sh prf
--                                   --where e'= if null prf then "" else let (expr,_,_):_ = prf in showHS options "" expr 
--showProof _  []                  = ""


