{-# OPTIONS_GHC -Wall #-}
--TODO -> May be we can look at GetText function for help with internationalization. Brian O'Sullivan is working (has started) on an internationalization library. Maybe some day...
module Fspec2Pandoc (fSpec2Pandoc)--,laTeXtemplate)
where
import Auxiliaries      (eqCl)
import Collection       (Collection (..))
import Ampersand
import Data.Plug
import Picture
import Data.List
import Data.Fspec
import Strings          (upCap, commaNL, commaEng, preciesEen)
import Text.Pandoc  
import Version          (versionbanner)
import Languages        (Lang(..))
import PredLogic        (PredLogicShow(..), showLatex)
import Options hiding   (services) --importing (Options(..),FspecFormat(..),DocTheme(..))
import NormalForms      (conjNF) -- ,proofPA)  Dit inschakelen voor het bewijs...
import Rendering.AdlExplanation (explain,explain2Blocks)
import Rendering.ClassDiagram
import Switchboard      (switchboard1)
import Classes.Graphics (makePicture)
import FPA
import Statistics
import Rendering.PandocAux

--DESCR ->
--The functional specification starts with an introduction
--The second chapter defines the functionality of the system for stakeholders.
--Because we assume these stakeholders to speak the language of the primary process without any technical knowledge,
--the second chapter contains natural language only. 
--The third chapter is intended for the analyst. It contains all the rules mentioned in
--natural language in the second chapter. It presents the trace from natural language
--to the formal rule.
--The fourth chapter presents a datamodel together with all the multiplicity rules.
-- by datasets and rules.
--Datasets are specified through PLUGS in Ampersand. The dataset is build around one concept, 
--also called the theme. Functionalities defined on the theme by one or more plugs are
--described together with the rules that apply to the dataset. Rules not described by
--the dataset are described in the last section of chapter 2.
--The following chapters each present a SERVICE
--The specification end with a glossary.



chpintrolabel :: String
chpintrolabel="chpIntro"
chpFRlabel :: String
chpFRlabel="chpFunctionalRequirements"
chpDiaglabel :: String
chpDiaglabel="chpDiagnosis"
chpCAlabel :: String
chpCAlabel="chpConceptualAnalysis"
chpPAlabel :: String
chpPAlabel="chpProcessAnalysis"
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

--TODO [Picture] should be separated from here. Now it is too much entangled, which makes it too complex (and hence errorprone). 
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
          
          docContents
           = ( introduction  level fSpec flags           ++
               natLangReqs   level fSpec flags           ++
               diagnosis     level fSpec flags           ++
               caTxt                                     ++
               (if noProcesses fSpec then [] else paTxt) ++
               daTxt                                     ++
               if studentversion then [] else [b| (blocks,_)<-svcs, b<-blocks] ++
               if studentversion then fpAnalysis level fSpec flags else [] ++
               glossary level fSpec flags 
               )
             where svcs = [serviceChap level fSpec flags svc | svc  <-services fSpec,not studentversion]
                   (caTxt,_) = conceptualAnalysis level fSpec flags
                   paTxt     = processAnalysis    level fSpec flags
                   (daTxt,_) = dataAnalysis       level fSpec flags
                   studentversion = theme flags == StudentTheme
          pictures = [daPic]++caPics++[p| (_,pics)<-svcs, p<-pics] 
             where svcs = [serviceChap level fSpec flags svc | svc  <-services fSpec,not studentversion]
                   (_,caPics) = conceptualAnalysis level fSpec flags
                   (_,daPic)  = dataAnalysis       level fSpec flags
                   studentversion = theme flags == StudentTheme
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
                , Quoted  SingleQuote [Str (name fSpec)], Str ". "
                , Str "Het definieert business-services in een systeem waarin mensen en applicaties samenwerken "
                , Str "om afspraken na te leven. "
                , Str "Veel van deze afspraken volgen uit regels, die de business voortdurend naleeft. "
                , Str "Een aantal van deze afspraken is gebruikt om de onderhavige functionele specificatie"
                , Note [Para [Str "Het gebruik van geldende afspraken als functionele eis is een kenmerk van de Ampersand aanpak, die gebruikt is bij het samenstellen van dit document. "]]
                , Str " samen te stellen. "
                , Str "Deze eisen staan opgesomd in hoofdstuk ", xrefReference chpFRlabel, Str ", geordend op thema. "
                ]
          , Para 
                [ Str "Een diagnose in hoofdstuk ", xrefReference chpDiaglabel
                , Str " is bedoeld voor de auteurs om gebreken uit hun Ampersand model op te sporen. "
                ]
          , Para 
                [ Str "De conceptuele analyse in hoofdstuk ", xrefReference chpCAlabel
                , Str " is bedoeld voor requirements engineers en architecten om de afspraken uit hoofdstuk "
                , xrefReference chpCAlabel, Str " te valideren. "
                , Str "Tevens is het bedoeld voor testers om eenduidige testgevallen te kunnen bepalen. "
                , Str "Dit hoofdstuk bevat dan ook een formele representatie van elke afspraak. "
                , Str "Daarmee ligt de consistentie van alle afspraken vast en is de interpretatie van de eisen eenduidig."
                ]
          , Para 
                [ Str "De hoofdstukken die dan volgen zijn bedoeld voor de bouwers van ", Str (name fSpec), Str ". "
                , Str "De gegevensanalyse in hoofdstuk "
                , xrefReference chpDAlabel
                , Str " beschrijft de gegevensverzamelingen waarop het systeem wordt gebouwd. "
                , Str "Elk volgend hoofdstuk definieert één business service. "
                , Str "Hierdoor kunnen bouwers zich concentreren op één service tegelijk. "
                , Str "Tezamen ondersteunen deze services alle afspraken uit hoofdstuk ", xrefReference chpFRlabel, Str ". "
                , Str "Door alle functionaliteit uitsluitend via deze services te ontsluiten waarborgt ", Str (name fSpec)
                , Str " compliance ten aanzien van de eisen uit hoofdstuk ", xrefReference chpFRlabel, Str " ."
                ]
         ]

        introContents English = 
         [ Para
                [Str "This document defines the functionality of an information system called "
                , Quoted  SingleQuote [Str (name fSpec)], Str ". "
                , Str "It defines business services in a system where people and applications work together "
                , Str "in order to fullfill their commitments. "
                , Str "Many of these commitments follow from rules that are continuously being maintained by the business. "
                , Str "A number of these rules have been used to assemble this functional specification"
                , Note [Para [Str "To use agreements as functional requirements characterizes the Ampersand approach, which has been used to produce this document. "]]
                , Str ". "
                , Str "Those rules are listed in chapter ", xrefReference chpFRlabel, Str ", ordered by theme. "
                ]
          , Para 
                [ Str "A diagnosis in chapter ", xrefReference chpDiaglabel
                , Str " is meant to help the authors identify shortcomings in their Ampersand script."
                ]
          , Para 
                [ Str "The conceptual analysis in chapter ", xrefReference chpCAlabel
                , Str " is meant for requirements engineers and architects to validate the requirements from chapter "
                , xrefReference chpCAlabel, Str ". "
                , Str "It is also meant for testers to come up with correct test cases. "
                , Str "Therefore, this chapter contains a formal representation of each commitment. "
                , Str "It defines the consistency of all commitments, "
                , Str "yielding an unambiguous interpretation of all requirements."
                ]
          , Para 
                [ Str "Chapters that follow have the builders of ", Str (name fSpec), Str " as their intended audience. "
                , Str "The data analysis in chapter "
                , xrefReference chpDAlabel
                , Str " describes the data sets upon which ", Str (name fSpec), Str " is built. "
                , Str "Each subsequent chapter defines one business service. "
                , Str "Services are described in a self contained way, each one in a chapter of its own. "
                , Str "This is done to let builders focus on building a single service at a time. "
                , Str "Together, these services fulfill all commitments from chapter ", xrefReference chpFRlabel, Str ". "
                , Str "By disclosing all functionality through these services, ", Str (name fSpec)
                , Str " ensures that users will abide by the rules put forward in chapter ", xrefReference chpFRlabel, Str ". "
                ]]  
------------------------------------------------------------
natLangReqs :: Int -> Fspc -> Options ->  [Block]
natLangReqs lev fSpec flags = header ++ dpIntro ++ dpRequirements
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
                     [ Str "Dit hoofdstuk beschrijft de functionele eisen ten behoeve van ", Str (name fSpec), Str " in natuurlijke taal. "
                     , Str "Elke functionele eis moet worden (of is al) goedgekeurd door een daartoe aangewezen belanghebbende. "
                     , Str "Daarom is elke eis met grote zorgvuldigheid geformuleerd. "
                     , Str "Alle begrippen en basiszinnen, die worden gebruikt in een functionele eis worden in dit hoofdstuk geïntroduceerd. "
                     , Str "Samen vormen zij een taal, "
                     , Str "die door de betreffende belanghebbenden is (of wordt) afgesproken ten behoeve van ", Str (name fSpec), Str ". "
                     , Str "Elke functionele eis kan worden begrepen in termen van deze basiszinnen. "
                     , Str "Wanneer alle belanghebbenden afspreken dat zij deze basiszinnen gebruiken, "
                     , Str "delen zij precies voldoende taal om deze functionele eisen op dezelfde manier te begrijpen. "
                     , Str "Daarom wordt elke basiszin behandeld als volwaardige functionele eis. "
                     , Str "Alle functionele eisen zijn genummerd omwille van de traceerbaarheid. "
                     ]]
        English -> [ Para
                     [ Str "This chapter defines the functional requirements of ", Str (name fSpec), Str " in natural language. "
                     , Str "Every functional requirement must be (or has been) signed off by an appropriate stakeholder. "
                     , Str "Therefore, much care has been given to the appropriate formulation of each requirement. "
                     , Str "All concepts and basic sentences that are used in functional requirements will be introduced in this chapter. "
                     , Str "Together, they form a language, "
                     , Str "which must be (or has been) agreed upon by the stakeholders for the sake of ", Str (name fSpec), Str ". "
                     , Str "Every functional requirement can be understood in terms of these basic sentences. "
                     , Str "If all stakeholders agree to use these basic sentences, "
                     , Str "they share precisely enough language to share their understanding of the functional requirements. "
                     , Str "That is why basic sentences are treated as any other functional requirement. "
                     , Str "All functional requirements have been numbered for the sake of traceability. "
                     ]]
  dpRequirements :: [Block]
  dpRequirements = theBlocks
    where
      (theBlocks,_) = aThemeAtATime toBeProcessedStuff ts newCounter 
      ts = rd (map r_pat (vrules fSpec)) --Only process patterns that contain user-defined rules and signals. 
      toBeProcessedStuff = ( allConceptsThatMustBeShown
                           , allRelsThatMustBeShown
                           , allRulesThatMustBeShown ) 
         where
           allConceptsThatMustBeShown     -- All concepts that have at least one explanation. Explanations are 
                                          -- currently bound to the conceptDefinitions of a concept.   
              = [(c, cd)| c <-concs fSpec
                        , cd <- vConceptDefs fSpec
                        , name c == name cd
                        , not (null (explain fSpec flags cd))
                ]           
           allRelsThatMustBeShown         -- All relations used in this specification, that are used in rules.
                                          -- and only those declarations that have at least one explanation.
              = [m| m@Mph{}<-mors fSpec
                  , not (null ( explain fSpec flags m))
                ]
           allRulesThatMustBeShown         
               = [r| r<-vrules fSpec      -- All *user declared* rules that apply in the entire Fspc, including all signals
                   , r_usr r
                 ]
      aThemeAtATime :: ([(Concept,ConceptDef)],[Relation Concept],[Rule (Relation Concept)]) -- all stuff that still must be processed into the comming sections
                    -> [String]          -- the names of the patterns that must be processed into this specification
                    -> Counter           -- unique definition counters
                    -> ([Block],Counter) -- The blocks that define the resulting document and the last used unique definition number
      aThemeAtATime  still2doPre themes' iPre
           = case themes' of
              []  -> printOneTheme Nothing still2doPre iPre
              _   -> (blocksOfOneTheme ++ blocksOfThemes,iPost)
         where
           (x:xs) = themes'
           (still2doCCDsPre, still2doRelsPre, still2doRulesPre) = still2doPre
           (blocksOfOneTheme,iPostFirst) = printOneTheme (Just x) processNow iPre
           (blocksOfThemes,iPost)     = aThemeAtATime stuff2PrintLater xs iPostFirst
           processNow = (ccds2PrintNow, rels2PrintNow, [r| r<-rules2PrintNow, r_usr r])
           rules2PrintNow =[r| r<-still2doRulesPre, r_pat r == x]
           rules2PrintLater = still2doRulesPre >- rules2PrintNow
           rels2PrintNow =[m| m<-still2doRelsPre, (m `eleM` mors rules2PrintNow)]
           rels2PrintLater = still2doRelsPre >- rels2PrintNow
           ccds2PrintNow = [(c,cd)|(c,cd)<- still2doCCDsPre, c `eleM` (concs rules2PrintNow)]
           ccds2PrintLater = still2doCCDsPre >- ccds2PrintNow
           stuff2PrintLater = (ccds2PrintLater, rels2PrintLater, rules2PrintLater)
           
-- | printOneTheme tells the story in natural language of a single theme.
-- | For this purpose, Ampersand authors should take care in composing explanations.
-- | Each explanation should state the purpose (and nothing else).
      printOneTheme :: Maybe String -- name of the theme to process (if any)
                    -> ([(Concept,ConceptDef)],[Relation Concept],[Rule (Relation Concept)])  -- Stuff to print in this section
                    -> Counter      -- first free number to use for numbered items
                    -> ([Block],Counter)-- the resulting blocks and the last used number.
      printOneTheme nm (ccds2print, rels2print, rules2print) counters1
              = ( header' ++ explainsPat ++ concBlocks ++ relBlocks ++ ruleBlocks
                , counters4
                )
           where 
              (concBlocks,counters2) = sctcs ccds2print  counters1
              (relBlocks,counters3) = sctds rels2print counters2
              (ruleBlocks,counters4) = sctrs rules2print counters3
              
              header' :: [Block]
              header'  = [Header 1 [Str (case nm of
                                               Just s -> s
                                               Nothing -> case language flags of
                                                            Dutch -> "Losse eindjes..."
                                                            English -> "Loose ends..." 
                                        )
                        ]         ]
              explainsPat :: [Block]
              explainsPat = case [p|p <- vpatterns fSpec , name p == themeName] of
                              []  -> [Para 
                                      (case language flags of
                                        Dutch   -> [Str "In deze paragraaf worden feittypes en concepten genoemd, "
                                                   ,Str "die in de voorgaande paragrafen niet aan bod zijn gekomen."]
                                        English -> [Str "This paragraph shows remaining fact types and concepts "
                                                   ,Str "that have not been used in previous paragraphs."]
                                      )
                                     ]
                              [p] -> explains2Blocks (explain fSpec flags p)
                              _   -> error ("!Fatal (module Fspec2Pandoc 313): Multiple themes are called '"++themeName++"'.") 

              themeName = case nm of
                             Just s -> s
                             Nothing  -> "" 
              sctcs :: [(Concept, ConceptDef)] -> Counter -> ([Block],Counter)
              sctcs xs c0
                = case xs of
                    []  -> ([],c0)
                    _   -> ( case language flags of
				               Dutch   ->  [ Para (case conceptNamesIntro of
				                                    []  -> []
				                                    [c] -> [ Str $ "Deze sectie introduceert het concept "
				                                           , Emph [Str $ c]]
				                                    cs  -> [ Str $ "Deze sectie introduceert de concepten "
				                                           , Str $ commaNL "en" cs]
				                                  )
				                           ]
				               English ->  [ Para (case conceptNamesIntro of
				                                    []  -> []
				                                    [c] -> [ Str $ "This section introduces concept "
				                                           , Emph [Str $ c]
				                                           , Str $ ". "]
				                                    cs  -> [ Str $ "This section introduces concepts "
				                                           , Str $ commaEng "and" cs
				                                           , Str $ ". "]
				                                    )
				                           ]
                             ++
                             concat (map singleConceptStuff xs)
                           , c0
                           )
                    
                  where
                      conceptNamesIntro = [name cpt|(cpt,_)<-xs]
                      singleConceptStuff :: (Concept,ConceptDef) -> [Block] -- ^ this function takes a tuple of a concept and -if it exists- its definition. It returns a list of [Blocks] representing the text to print for it.
                      singleConceptStuff (c,cd) = explains cd  ++
                                                  [Para (symDefLabel c: makeDefinition flags (name c) (cddef cd))]
                      explains cd = explains2Blocks (explain fSpec flags cd) 

-- sctds prints the requirements related to relations that are introduced in this theme.
              sctds :: [Relation Concept] -> Counter -> ([Block],Counter)
              sctds xs c0 
                = case xs of
                    []  -> ([],c0)
                    _   -> (fstBlocks ++ restBlocks,c2)
                  where
                      d':ds' = xs
                      (fstBlocks,c1) = relBlock d' c0
                      (restBlocks,c2) = sctds ds' c1
                      relBlock :: Relation Concept -> Counter -> ([Block],Counter)
                      relBlock m cnt = ([DefinitionList [( [Str (case language flags of
				                                                     Dutch   -> "Eis "
				                                                     English -> "Requirement ")
                                                             ,Str (show(getEisnr cnt))
                                                             ,Str ":"]
                                                            ,   [[Para ([symReqLabel (makeDeclaration m)])]
                                                             ++ explains2Blocks (explain fSpec flags m)]
                                                           )
                                                          ]
                                          ]
                                         ,incEis cnt)
                                                       
              sctrs :: [Rule (Relation Concept)] -> Counter -> ([Block],Counter)
              sctrs xs c0 
                = case xs of
                    []  -> ([],c0)
                    _   -> (fstBlocks ++ restBlocks,c2)
                  where
                      r':rs' = xs
                      (fstBlocks,c1) = ruleBlock r' c0
                      (restBlocks,c2) = sctrs rs' c1
                      ruleBlock :: Rule (Relation Concept) -> Counter -> ([Block],Counter)
                      ruleBlock r2 cnt = ([DefinitionList [( [Str (case language flags of
				                                                     Dutch   -> "Eis "
				                                                     English -> "Requirement ")
                                                             ,Str (show(getEisnr cnt))
                                                             ,Str ":"]
                                                            ,  [ [Para ([symReqLabel r2])] ++
                                                                 explains2Blocks (explain fSpec flags r2)]
                                                           )
                                                          ]
                                          ]
                                         ,incEis cnt)
                      

------------------------------------------------------------
diagnosis :: Int -> Fspc -> Options ->  [Block]
diagnosis lev fSpec flags = header ++ diagIntro ++ missingConceptDefs ++ missingRels
  where
  header :: [Block]
  header = labeledHeader lev chpDiaglabel (case (language flags) of
                                             Dutch   ->  "Diagnose"   
                                             English ->  "Diagnosis"
                                         )
  diagIntro :: [Block]
  diagIntro = 
   (case (language flags) of
      Dutch   -> [Para
                  [ Str "Dit hoofdstuk geeft een analyse van het Ampersand-script van ", Str (name fSpec), Str ". "
                  , Str "Deze analyse is bedoeld voor de auteurs van dit script. "
                  , Str "Op basis hiervan kunnen zij het script completeren en mogelijke tekortkomingen verbeteren. "
                  ]]
      English -> [Para
                  [ Str "This chapter provides an analysis of the Ampersand script of ", Str (name fSpec), Str ". "
                  , Str "This analysis is intended for the authors of this script. "
                  , Str "It can be used to complete the script or to improve possible flaws. "
                  ]]
   )


  missingConceptDefs :: [Block]
  missingConceptDefs
   = case (language flags, missing) of
      (Dutch,[])  -> [Para 
                       [Str "Alle concepten in dit document zijn voorzien van een definitie."]
                     ]
      (Dutch,[c]) -> [Para 
                       [Str "Het concept ", Quoted SingleQuote [Str (name c)], Str " heeft geen definitie."]
                     ]
      (Dutch,xs)  -> [Para 
                       [Str "De concepten: ", Str (commaEng "and" (map name xs)), Str " hebben geen definitie."]
                     ]
      (English,[])  -> [Para 
                       [Str "All concepts in this document have been provided with a definition."]
                     ]
      (English,[c]) -> [Para 
                       [Str "The concept ", Quoted SingleQuote [Str (name c)], Str " remains without a definition."]
                     ]
      (English,xs)  -> [Para 
                       [Str "Concepts ", Str (commaEng "and" (map name xs)), Str " remain without a definition."]
                     ]
   where missing = [c| c <-concs fSpec
                     , cd <- vConceptDefs fSpec
                     , name c == name cd
                     , null (explain fSpec flags cd)
                   ]++
                   [c| c <-concs fSpec
                     , null [cd | cd <- vConceptDefs fSpec, name c == name cd]
                   ]
  missingRels :: [Block]
  missingRels
   = case (language flags, missing) of
      (Dutch,[])  -> [Para 
                       [Str "Alle relaties in dit document zijn voorzien van een uitleg."]
                     ]
      (Dutch,[m]) -> [Para 
                       [ Str "De relatie ", TeX (showMathcode fSpec m)
                       , Str " wordt nergens uitgelegd. "
                       , Str "U kunt overwegen om het statement: PURPOSE RELATION ", TeX (showMathcode fSpec m)
                       , Str " toe te voegen."
                     ] ]
      (Dutch,ms)  -> [Para 
                       [ Str "Relaties ", TeX (commaNL "en" (map (showMathcode fSpec) ms))
                       , Str " worden niet uitgelegd. "
                       , Str "U kunt overwegen om PURPOSE RELATION statements toe te voegen."
                     ] ]
      (English,[])  -> [Para 
                         [Str "All relations in this document have been provided with a purpose."]
                       ]
      (English,[m]) -> [Para 
                         [ Str "Relation ", TeX (showMathcode fSpec m)
                         , Str " remains unexplained. "
                         , Str "You might consider to add the statement: PURPOSE RELATION "
                         , TeX (showMathcode fSpec m), Str "."
                       ] ]
      (English,ms)  -> [Para 
                         [ Str "Relations ", TeX (commaEng "and" (map (showMathcode fSpec) ms))
                         , Str " remain unexplained. "
                         , Str "You might consider to add PURPOSE RELATION statements."
                       ] ]
   where missing = [m| m <-mors fSpec
                     , null (explain fSpec flags m)
                   ]

------------------------------------------------------------
conceptualAnalysis :: Int -> Fspc -> Options -> ([Block],[Picture])
conceptualAnalysis lev fSpec flags = (header ++ caIntro ++ caBlocks , pictures)
  where
  header :: [Block]
  header = labeledHeader lev chpCAlabel (case language flags of
                                            Dutch   ->  "Conceptuele Analyse"   
                                            English ->  "Conceptual Analysis"
                                        )
  caIntro :: [Block]
  caIntro = 
--   explains2Blocks (explain fSpec flags fSpec) ++ -- hier moet de explanation van de context komen.
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
  (caBlocks,pictures) = ( [b| (blocks,_)<-ca, b<-blocks], [picture| (_,picture)<-ca] )
                        where ca=caSections (patterns fSpec)

  caSections :: [Pattern] -> [([Block],Picture)]
  caSections pats = iterat pats 1 [] []
   where
    iterat :: [Pattern] -> Int -> [Concept] -> [Declaration Concept] -> [([Block],Picture)]
    iterat [] _ _ _ = []
    iterat (pat:ps) i seenConcepts seenDeclarations
     = ( [Header (lev+1) [Str (name pat)]]    -- new section to explain this theme
       ++ sctMotivation                       -- The section startss with the reason why this theme exists,
       ++ (if (useGraphics flags)             -- followed by a conceptual model for this theme
            then 
              (case language flags of             -- announce the conceptual diagram
                Dutch   -> [Para [x | x<-[Str "Figuur ", xrefReference (figlabel pict), Str " geeft een conceptueel diagram van dit thema."]] ]
                English -> [Para [x | x<-[Str "Figure ", xrefReference (figlabel pict), Str " shows a conceptual diagram of this theme."]] ]
              ) ++ [Plain (xrefFigure1 pict)]          -- draw the conceptual diagram
            else []
          )
                                              -- now provide the text of this theme.
       ++ (if null blocks then [] else [DefinitionList blocks])
       , pict):  iterat ps i'' seenCss seenDss
       where
         pict = makePicture flags fSpec pat   -- the Picture that represents this service's knowledge graph
         blocks  :: [([Inline], [[Block]])]
         blocks = sctRules ++ sctSignals
         sctMotivation
          = explains2Blocks (explain fSpec flags pat)
         (sctRules,   i',  seenCrs, seenDrs) = dpRule patRules i seenConcepts seenDeclarations
         (sctSignals, i'', seenCss, seenDss) = dpRule patSignals i' seenCrs seenDrs
         patRules     = [r| r<-rules fSpec,   r_pat r==name pat, r_usr r]
         patSignals   = [s| s<-signals fSpec, r_pat s==name pat]

    dpRule [] n seenConcs seenDeclarations = ([], n, seenConcs, seenDeclarations)
    dpRule (r:rs) n seenConcs seenDeclarations
     = ( [ ( [Str (name r)]
           , [ (explains2Blocks (explain fSpec flags r)) ++                  -- Als eerste de uitleg van de betreffende regel..
               (concat [explains2Blocks (explain fSpec flags d)|d<-nds]) ++  -- Dan de uitleg van de betreffende relaties
               [ Plain (text1)| not (null nds)] ++
               pandocEqnArray [ ([TeX (texOnly_Id(name d))], [TeX ":"], [TeX (texOnly_Id(name (source d))++(if isFunction d then texOnly_fun else texOnly_rel )++texOnly_Id(name(target d))), symDefLabel d])
                              |d<-nds] ++
               [ Plain (text2)| not (null rds)] ++
               [ Plain (text3)| isSignal r] ++
               (if showPredExpr flags
                then pandocEquation [TeX (showLatex (toPredLogic r)), symDefLabel r]
                else pandocEquation [TeX (showMathcode fSpec r), symDefLabel r]
               )++
               [ Plain (text4) | length nds>1]
             ] 
           ) ] ++ dpNext
       , n'
       , seenCs
       , seenDs
       )
       where
        text1
         = case (length nds,language flags) of
             (1,Dutch)   -> let d = head nds in
                            [Str ("Om dit te formaliseren is een "++(if isFunction d then "functie" else "relatie")++" "),Str (name d),Str " nodig (",symDefRef d,Str "):"]
             (1,English) -> let d = head nds in
                            [Str ("In order to formalize this, a "++(if isFunction d then "function" else "relation")++" "),Str (name d),Str " is introduced (",symDefRef d,Str "):"]
             (l,Dutch)   -> [TeX "Om te komen tot de formalisatie in vergelijking~", symDefRef r,Str (" zijn de volgende "++count flags l "relatie"++" nodig.")]
             (l,English) -> [TeX "To arrive at the formalization in equation~", symDefRef r,Str (", the following "++count flags l "relation"++" are introduced.")]
        text2
         = (case (length nds,length rds,language flags) of
             (0,1,Dutch)   -> [Str "Definitie ", symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ") wordt gebruikt"]
             (0,1,English) -> [Str "We use definition ", symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ")"]
             (0,_,Dutch)   -> [Str "We gebruiken definities ", TeX (commaNL "en" [str |d<-rds, let TeX str=symDefRef d])]
             (0,_,English) -> [Str "We use definitions ", TeX (commaEng "and" [str |d<-rds, let TeX str=symDefRef d])]
             (_,1,Dutch)   -> [Str "Daarnaast gebruiken we definitie ", symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ")"]
             (_,1,English) -> [Str "We use definition ", symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ")"]
             (_,_,Dutch)   -> [Str "Ook gebruiken we definities ", TeX (commaNL "en" [str++"("++texOnly_Id (name d)++")" |d<-rds, let TeX str=symDefRef d])]
             (_,_,English) -> [Str "We also use definitions ", TeX (commaEng "and" [str++"("++texOnly_Id (name d)++")" |d<-rds, let TeX str=symDefRef d])]
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
        nds = [d| d<-ds >- seenDeclarations, isSgn d]     -- newly seen declarations
        rds = [d| d<-ds `isc` seenDeclarations, isSgn d]  -- previously seen declarations
        ( dpNext, n', seenCs,  seenDs ) = dpRule rs (n+length cds+length nds+1) (concs r `uni` seenConcs) (ds `uni` seenDeclarations)
        ds  = map makeDeclaration (mors r)
------------------------------------------------------------
--DESCR -> the data analysis contains a section for each class diagram in the fspec
--         the class diagram and multiplicity rules are printed

-- If an Ampersand script contains no reference to any role whatsoever, a process analysis is meaningless.
-- In that case it will not be printed. To detect whether this is the case, we can look whether the
-- roleServices and mayEdit attributes remain empty.
noProcesses :: Fspc -> Bool
noProcesses fSpec = null (roleServices fSpec) && null (mayEdit fSpec)

processAnalysis :: Int -> Fspc -> Options -> [Block]
processAnalysis lev fSpec flags
 = header ++ roleServiceBlocks
 where 
  header :: [Block]
  header = labeledHeader lev chpPAlabel (case language flags of
                                              Dutch   ->  "Procesanalyse"   
                                              English ->  "Process Analysis"
                                        )
  roleServiceBlocks :: [Block]
  roleServiceBlocks
   = [ if language flags==Dutch
       then Para [ Str $ upCap (name fSpec)++" kent rollen aan services toe. "
                 , Str $ "De volgende tabel toont de services waar een rol toegang toe heeft."
                 ]
       else Para [ Str $ upCap (name fSpec)++" assigns roles to services. "
                 , Str $ "The following table shows to which services each role has access."
                 ]
-- the table containing the role-services assignments
     , Para  $ [ TeX $ "\\begin{tabular}{|l|l|}\\hline\n"
               , if language flags==Dutch
                 then TeX $ "Rol&Service\\\\ \\hline\n"
                 else TeX $ "Role&Service\\\\ \\hline\n"
               ]++
               [ TeX $ intercalate "\\\\ \\hline\n" 
                       [ role++"&"++name svc++
                         concat[ "\\\\\n&"++name (snd rs) | rs<-tail rsClass]
                       | rsClass<-eqCl fst (roleServices fSpec)
                       , let role=fst (head rsClass), let svc=snd (head rsClass)
                       ]
               ]++
               [ TeX $ "\\\\ \\hline\n" | not (null rolelessSvs)]++
               [ TeX $ intercalate "\\\\\n" [ "&"++name svc | svc<-rolelessSvs] ]++
               [ TeX $ "\\\\ \\hline\n\\end{tabular}" ]
     , if language flags==Dutch
       then Para [ Str $ upCap (name fSpec)++" kent rollen aan relaties toe. "
                 , Str $ "De volgende tabel toont de relaties waarvan de inhoud gewijzigd kan worden door iemand die een bepaalde rol vervult."
                 ]
       else Para [ Str $ upCap (name fSpec)++" assigns roles to relations. "
                 , Str $ "The following table shows the relations, the content of which can be altered by anyone who fulfills a given role."
                 ]
-- the table containing the role-services assignments
     , Para  $ [ TeX $ "\\begin{tabular}{|l|l|}\\hline\n"
               , if language flags==Dutch
                 then TeX $ "Rol&Relatie\\\\ \\hline\n"
                 else TeX $ "Role&Relation\\\\ \\hline\n"
               ]++
               [ TeX $ intercalate "\\\\ \\hline\n" 
                       [ role++"&"++showMathcode fSpec m++
                         concat[ "\\\\\n&"++showMathcode fSpec (snd rs) | rs<-tail rrClass]
                       | rrClass<-eqCl fst (mayEdit fSpec)
                       , let role=fst (head rrClass), let m=snd (head rrClass)
                       ]
               ]++
               [ TeX $ "\\\\ \\hline\n" | not (null rolelessRels)]++
               [ TeX $ intercalate "\\\\\n" [ "&"++showMathcode fSpec d | d<-rolelessRels] ]++
               [ TeX $ "\\\\ \\hline\n\\end{tabular}"
               ]
     ]
     where
      rolelessSvs  = [ svc | svc<-services fSpec, not (name svc `elem` (rd.map (name.snd)) (roleServices fSpec)) ]
      rolelessRels = [ d | d<-declarations fSpec, not (d `elem` (rd.map snd) (mayEdit fSpec)) ]

------------------------------------------------------------
--DESCR -> the data analysis contains a section for each class diagram in the fspec
--         the class diagram and multiplicity rules are printed
dataAnalysis :: Int -> Fspc -> Options -> ([Block],Picture)
dataAnalysis lev fSpec flags
 = ( header ++ daContents ++ daAssociations remainingDecls ++
   [b | p<-datasets fSpec, b<-daPlug p] , classDiagramPicture )
 where 
  remainingDecls = mors fSpec >- [m | p<-datasets fSpec, m<-mors p]

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
--       cdDot = classdiagram2dot flags classDiagram

-- The properties of various declations are documented in different tables.
-- First, we document the heterogeneous properties of all relations
-- Then, the homogeneous poperties are given, and finally
-- the signals are documented.
  daAssociations :: [Relation Concept] -> [Block]
  daAssociations ms
   = [ if language flags==Dutch
       then Para [ Str $ upCap (name fSpec)++" heeft de volgende associaties en multipliciteitsrestricties. "
                 ]
       else Para [ Str $ upCap (name fSpec)++" has the following associations and multiplicity constraints. "
                 ]
-- the heterogeneous properties:
     , Para  $ [ TeX $ "\\begin{tabular}{|l|cc|}\\hline\n"
               , if language flags==Dutch
                 then TeX $ "relatie&totaal&surjectief\\\\ \\hline\\hline\n"
                 else TeX $ "relation&total&surjective\\\\ \\hline\\hline\n"
               ]++
               [ TeX $ intercalate "&" 
                                 [ if source m==target m
                                   then "\\(\\signt{"++name m++"}{"++latexEscShw (source m)++"}\\)"              -- veld
                                   else "\\(\\signat{"++name m++"}{"++latexEscShw (source m)++"}{"++latexEscShw (target m)++"}\\)"              -- veld
                                 , if isTot m then "\\(\\surd\\)" else ""
                                 , if isSur m then "\\(\\surd\\)" else ""
                                 ]++"\\\\\n"
               | m@Mph{}<-ms, not (isProp m)
               ]++
               [ TeX $ "\\hline\n\\end{tabular}"
               ]
     ]++
-- the homogeneous properties:
     [ Para [ if language flags==Dutch
                then TeX $ "Een relatie, "++texOnly_Id(name d)++", is homogeen en heeft de volgende eigenschappen: "
                else TeX $ "One relation, "++texOnly_Id(name d)++", is homogeneous and has the following properties: "]
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
               [ TeX $ intercalate "&" 
                                 [ "\\signt{"++name d++"}{"++latexEscShw (source d)++"}"              -- veld
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
-- the keys
     [ Para [ if language flags==Dutch
                then TeX $ "Er is "++preciesEen++" key: "++texOnly_Id(name k)++"."
                else TeX $ "There is but one key: "++texOnly_Id(name k)++"." ]
     | length keyds==1, k<-keyds ]++
     [ Para [ if language flags==Dutch
                then TeX $ "De volgende keys bestaan: "++commaNL "en" [texOnly_Id(name k) | k<-keyds]
                else TeX $ "The following keys exist: "++commaEng "and" [texOnly_Id(name k)| k<-keyds]]
     | length keyds>1 ]++
-- the signals
     [ Para [ if language flags==Dutch
                then TeX $ "Er is "++preciesEen++" signaal: "++texOnly_Id(name d)++"."
                else TeX $ "There is but one signal: "++texOnly_Id(name d)++"." ]
     | length sgnls==1, d<-sgnls ]++
     [ Para [ if language flags==Dutch
                then TeX $ "De volgende signalen bestaan: "++commaNL "en" [texOnly_Id(name d) | d<-sgnls]
                else TeX $ "The following signals exist: "++commaEng "and" [texOnly_Id(name d)| d<-sgnls]]
     | length sgnls>1 ]
     where
      hMults  = [m| m@Mph{}<- mors fSpec, homogeneous m]
      sgnls   = [m| m@Mph{}<-ms, isSignal m] -- all signal declarations are not user defined, so this is disjoint from hMults
      keyds   = keyDefs fSpec -- all key definitions
-- The properties of various declations are documented in different tables.
-- First, we document the heterogeneous properties of all relations
-- Then, the homogeneous poperties are given, and finally
-- the signals are documented.

  daAttributes :: PlugSQL -> [Block]
  daAttributes p
   = [ if language flags==Dutch
       then Para [ Str $ "De attributen van "++name p++" hebben de volgende multipliciteitsrestricties. "
                 ]
       else Para [ Str $ "The attributes in "++name p++" have the following multiplicity constraints. "
                 ]
     , Para  $ [ TeX $ "\\begin{tabular}{|llcc|}\\hline\n"
               , if language flags==Dutch
                 then TeX $ "attribuut&type&verplicht&uniek\\\\ \\hline\\hline\n"
                 else TeX $ "attribute&type&mandatory&unique\\\\ \\hline\\hline\n"
               ]++
               [ TeX $ intercalate 
                             "&" [ fldname fld
                                 , latexEscShw (target (fldexpr fld))
                                 , if fldnull fld then "" else "\\(\\surd\\)"
                                 , if flduniq fld then "\\(\\surd\\)" else ""
                                 ]++"\\\\\n"
               | fld<-tail $ tblfields p  -- tail haalt het eerste veld, zijnde I[c], eruit omdat die niet in deze tabel thuishoort.
               ]++
               [ TeX "\\hline\n\\end{tabular}"
               ]
     ]
-- the homogeneous properties have already been reported in the general section of this chapter.
     where
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
      tots = [d| t<-ts, inline t, d<-map makeDeclaration (mors t)]
      unis = [d| t<-is, inline t, d<-map makeDeclaration (mors t)]
      surs = [d| t<-ts, not (inline t), d<-map makeDeclaration (mors t)]
      injs = [d| t<-is, not (inline t), d<-map makeDeclaration (mors t)]
-}

  -- daPlugs describes data sets.
  -- These can be recognized by:
  --    1. the first field has the "unique" attribute on (otherwise it is a binary association)
  --    2. there is more than one field (otherwise it is a scalar).
  -- The text gives all rules that are maintained internally within the data structure,
  -- because they might very well be implemented as database integrity rules.
  -- Multiplicity rules are not reported separately, because they are already taken care of in the multiplicity tables.
  -- Plugs that are associations between data sets and scalars are ignored.

  daPlug :: PlugSQL -> [Block]
  daPlug p
   = if null content then [] else plugHeader ++ content
     where
       plugHeader = labeledHeader (lev+1) ("sct:plug "++name p) (name p)
       content = daAttributes p ++ plugRules ++ plugSignals ++ plugKeydefs ++ iRules
       plugRules
        = case language flags of
           English -> case [r| r@(Ru{})<-rules fSpec, r_usr r, null (mors r >- mors p)] of
                       []  -> []
                       [r] -> [ Para [ Str "This data set shall maintain the following integrity rule. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic r)) ]
                                else Para [ Math DisplayMath $ showMathcode fSpec r]
                              ]
                       rs  -> [ Para [ Str "This data set shall maintain the following integrity rules. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic r)) ]]| r<-rs ]
                                else BulletList [[Para [Math DisplayMath $ showMathcode fSpec r]]| r<-rs ]
                              ]
           Dutch   -> case [r| r@(Ru{})<-rules fSpec, r_usr r, null (mors r >- mors p)] of
                       []  -> []
                       [r] -> [ Para [ Str "Deze gegevensverzameling handhaaft de volgende integriteitsregel. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic r)) ]
                                else Para [ Math DisplayMath $ showMathcode fSpec r]
                              ]
                       rs  -> [ Para [ Str "Deze gegevensverzameling handhaaft de volgende integriteitsregels. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic r)) ]]| r<-rs ]
                                else BulletList [[Para [Math DisplayMath $ showMathcode fSpec r]]| r<-rs ]
                              ]
       plugKeydefs
        = case language flags of
           English -> case [k| k<-keyrules fSpec, null (mors k >- mors p)] of
                       []  -> []
                       [s] -> [ Para [ Str "This data set contains one key. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic s)) ]
                                else Para [ Math DisplayMath $ showMathcode fSpec s]
                              ]
                       ss  -> [ Para [ Str "This data set contains the following keys. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic s)) ]]| s<-ss ]
                                else BulletList [[Para [Math DisplayMath $ showMathcode fSpec s]]| s<-ss ]
                              ]
           Dutch   -> case [k| k<-keyrules fSpec, null (mors k >- mors p)] of
                       []  -> []
                       [s] -> [ Para [ Str ("Deze gegevensverzameling genereert "++preciesEen++" key. ") ] 
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic s)) ]
                                else Para [ Math DisplayMath $ showMathcode fSpec s]
                              ]
                       ss  -> [ Para [ Str "Deze gegevensverzameling genereert de volgende keys. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic s)) ]]| s<-ss ]
                                else BulletList [[Para [Math DisplayMath $ showMathcode fSpec s]]| s<-ss ]
                              ]
       plugSignals
        = case language flags of
           English -> case [r| r<-signals fSpec, null (mors r >- mors p)] of
                       []  -> []
                       [s] -> [ Para [ Str "This data set generates one signal. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic s)) ]
                                else Para [ Math DisplayMath $ showMathcode fSpec s]
                              ]
                       ss  -> [ Para [ Str "This data set generates the following signals. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic s)) ]]| s<-ss ]
                                else BulletList [[Para [Math DisplayMath $ showMathcode fSpec s]]| s<-ss ]
                              ]
           Dutch   -> case [r| r<-signals fSpec, null (mors r >- mors p)] of
                       []  -> []
                       [s] -> [ Para [ Str ("Deze gegevensverzameling genereert "++preciesEen++" signaal. ") ] 
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic s)) ]
                                else Para [ Math DisplayMath $ showMathcode fSpec s]
                              ]
                       ss  -> [ Para [ Str "Deze gegevensverzameling genereert de volgende signalen. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic s)) ]]| s<-ss ]
                                else BulletList [[Para [Math DisplayMath $ showMathcode fSpec s]]| s<-ss ]
                              ]
       iRules
        = case language flags of
           English -> case irs of
                       []  -> []
                       [e] -> [ Para [ Str "This data set shall maintain the following integrity rule. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic e)) ]
                                else Para [ Math DisplayMath $ showMathcode fSpec e]
                              ]
                       es  -> [ Para [ Str "This data set shall maintain the following integrity rules. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic e)) ]]| e<-es ]
                                else BulletList [[Para [Math DisplayMath $ showMathcode fSpec e]]| e<-es ]
                              ]
           Dutch   -> case irs of
                       []  -> []
                       [e] -> [ Para [ Str "Deze gegevensverzameling handhaaft de volgende integriteitsregel. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic e)) ]
                                else Para [ Math DisplayMath $ showMathcode fSpec e]
                              ]
                       es  -> [ Para [ Str "Deze gegevensverzameling handhaaft de volgende integriteitsregels. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic e)) ]]| e<-es ]
                                else BulletList [[Para [Math DisplayMath $ showMathcode fSpec e]]| e<-es ]
                              ]
          where irs = [Fux fs| Quad m ccrs<-vquads fSpec
                            , r_usr (cl_rule ccrs), isIdent m, source m `elem` pcpts
                            , (_,shifts)<-cl_conjNF ccrs
                            , Fux fs<-shifts
                            , let ns=[t| Cpx t<-fs], length ns==1, Tm nega _<-ns
                            , m==nega
                            ]
                pcpts = case p of
                  ScalarSQL{} -> [cLkp p]
                  _           -> map fst (cLkpTbl p)

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
      Dutch ->   intercalate " "
                 [ case length ars of
                    0 -> ""
                    1 -> " Regel "++name (head ars)++" wordt door deze service gehandhaafd zonder interventie van de gebruiker."
                    _ -> " Regels "++commaNL "en" (map name ars)++" worden door deze service gehandhaafd zonder interventie van de gebruiker. "
                 , case length mrs of
                    0 -> ""
                    1 -> " Regel "++name (head mrs)++" wordt door de gebruiker van deze service gehandhaafd."
                    _ -> "Regels "++commaNL "en" (map name mrs)++" worden door de gebruiker van deze service gehandhaafd. "
                 ]
      English -> intercalate " "
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
            where recur :: Expression (Relation Concept) -> Field -> [Block]
                  recur e f | null (fld_sub f) = fld e f
                            | otherwise        = fld e f ++
                                                 [ BulletList [recur (F [e,fld_expr f']) f'| f'<-fld_sub f] ]
           fld e f = [ Para [ Str (dealWithUnderscores (fld_name f)++if null cols then "" else "("++intercalate ", " cols++")") ]
                     , Para [ Str "display on start: ", Math InlineMath $ showMathcode fSpec (conjNF e) ]
                     ] {- ++
                     [ Para [ Str $ "exec on insert: "++ showECA fSpec "\n>     "  (fld_onIns f arg)]
                     | fld_insAble f, arg<-[error ("!TODO (module Fspec2Pandoc 1036    ): hier moet een declaratie \"Delta\" staan")] ]
-}
            where cols = ["lijst"        | fld_list    f]++
                         ["verplicht"    | fld_must    f]++
                         ["nieuw"        | fld_insAble f]++
                         ["verwijderbaar"| fld_delAble f]
                     
           dealWithUnderscores :: [Char] -> [Char]
           dealWithUnderscores x = 
                  case x of 
                    []     -> []
                    '_':cs -> "\\_" ++ dealWithUnderscores cs
                    c:cs   -> c : dealWithUnderscores cs
           
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
                 | eca<-fsv_ecaRules svc, arg<-[error ("!TODO (module Fspec2Pandoc 1092): hier moet een declaratie \"Delta\" staan")]
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
                          intercalate "&" ["data set", "analysis", "points"] ++"\\\\\\hline\n"++
                          intercalate "\\\\\n" [ intercalate "&" [name plug, latexEscShw (fpa plug), (latexEscShw.fPoints.fpa) plug]
                                         | plug<-datasets fSpec
                                         , fPoints (fpa plug)>0] ++
                          "\\\\\\hline\\end{tabular}" ]
                ,Para $ 
                  [ TeX $ "\\begin{tabular}{|l|l|r|}\\hline \n" ++
                          intercalate "&" ["service", "analysis", "points"] ++"\\\\\\hline\n"++
                          intercalate "\\\\\n" [ intercalate "&" [name svc, latexEscShw (fsv_fpa svc), (latexEscShw.fPoints.fsv_fpa) svc] | svc<-services fSpec] ++
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
--                                   "\n"++(if null ss then "\n   "++equ else if null equ then intercalate " " ss else "   "++equ++" { "++intercalate "; " ss++" }")++
--                                   showProof sh prf
--                                   --where e'= if null prf then "" else let (expr,_,_):_ = prf in showHS options "" expr 
--showProof _  []                  = ""

data Counter = Counter { --getConc :: Int
                    --     getDecl :: Int
                    --   , getRule :: Int
                        getEisnr:: Int
                       }
newCounter :: Counter
newCounter = Counter 1
incEis :: Counter -> Counter
--incConc x = x{getConc = getConc x + 1}
--incDecl x = x{getDecl = getDecl x + 1}
--incRule x = x{getRule = getRule x + 1}
incEis x = x{getEisnr = getEisnr x + 1}
explains2Blocks :: [Explanation] -> [Block]
explains2Blocks es = concat (map explain2Blocks es)
