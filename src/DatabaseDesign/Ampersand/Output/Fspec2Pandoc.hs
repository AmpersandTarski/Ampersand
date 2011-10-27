{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.Fspec2Pandoc (fSpec2Pandoc)--,laTeXtemplate)
where
import DatabaseDesign.Ampersand.Basics  
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes
import Data.List
import DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms
import DatabaseDesign.Ampersand.Fspec.ToFspec.Calc (reprAsRule)
import DatabaseDesign.Ampersand.Fspec.FPA (fpa) 
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Fspec.Fspec
import Text.Pandoc
import Text.Pandoc.Builder  (toList, codeBlock)
import DatabaseDesign.Ampersand.Output.PredLogic        (PredLogicShow(..), showLatex)
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec.Switchboard      (SwitchBdDiagram(..), switchboardAct,sbDiagram)
import DatabaseDesign.Ampersand.Output.AdlExplanation (purpose,meaning,Explainable(..))
import DatabaseDesign.Ampersand.Output.Statistics (Statistics(..))
import DatabaseDesign.Ampersand.Output.PandocAux

fatal :: Int -> String -> a
fatal = fatalMsg "Output.Fspec2Pandoc"

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
--The following chapters each present a INTERFACE
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
          titl = [ Str (case (language flags, diagnosisOnly flags) of
                        (Dutch  , False) -> "Functionele Specificatie van "
                        (English, False) -> "Functional Specification of "
                        (Dutch  ,  True) -> "Diagnose van "
                        (English,  True) -> "Diagnosis of "
                       )
                 , Quoted SingleQuote [Str (name fSpec)]
                 ] 
          authors = case language flags of
                         Dutch   -> [[Str "Auteur(s) hier plaatsen"]
                                    ,[Str ("(Dit document is gegenereerd door "++ampersandVersionStr++")")]]
                         English -> [[Str "Put author(s) here"]
                                    ,[Str ("(This document was generated by "++ampersandVersionStr++")")]]
          date = [Str (show(genTime flags))]

          -- | The following code controls the structure of the document.
          docContents
           | diagnosisOnly flags = diagTxt
           | otherwise           =
               chpIntroduction  level fSpec flags          ++   -- this chapter gives a general introduction. No text from the script is used other than the name of the context.
               chpNatLangReqs   level fSpec flags          ++   -- this chapter gives an account of this context in natural language.
                                                                --   It sums up all requirements and explains their purpose. This is intended for stakeholders without
                                                                --   any skills in formal specification or information systems modeling.
               (if noDiagnosis flags then [] else diagTxt) ++   -- This chapter is meant for the author. It points to places in the text that might need work.
               caTxt                                       ++   -- This chapter is the conceptual analysis. It is meant for the design team to verify whether the natural language phrases and their formal counterparts match.
               (if noProcesses fSpec then [] else paTxt)   ++   -- This chapter discusses the processes and patterns in this context.
               fpAnalysis level fSpec flags                ++   -- This chapter does a function point analysis on the specification.
               daTxt                                       ++   -- This chapter provides a data analysis together with a data model.
                                                                --   It is meant for implementors who must build the system.
               [b | studentversion, (blocks,_)<-acts, b<-blocks] ++ -- in the student version, document the activities
               (if genEcaDoc flags then chpECArules level fSpec flags else [])    ++ -- This chapter reports on the ECA rules generated in this system.
               glossary level fSpec flags                        -- At the end, a glossary is generated.
               
          acts = [interfaceChap level fSpec flags act | act <-fActivities fSpec]
          pictures = if diagnosisOnly flags then diagPics else
                     daPics++caPics++diagPics++paPics++[p | (_,pics)<-acts, p<-pics] 
          (caTxt  ,caPics)   = chpConceptualAnalysis level fSpec flags
          (diagTxt,diagPics) = chpDiagnosis          level fSpec flags
          (paTxt  ,paPics)   = chpProcessAnalysis    level fSpec flags
          (daTxt  ,daPics)   = chpDataAnalysis       level fSpec flags
          studentversion = theme flags == StudentTheme
          level = 0 --1=chapter, 2=section, 3=subsection, 4=subsubsection, _=plain text
------------------------------------------------------------                

chpIntroduction :: Int -> Fspc -> Options ->  [Block]
chpIntroduction lev fSpec flags = header ++ introContents (language flags)
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
                , Str "Een aantal van deze afspraken is gebruikt als functionele eis om de onderhavige functionele specificatie"
                , Note [Para [Str "Het gebruik van geldende afspraken als functionele eis is een kenmerk van de Ampersand aanpak, die gebruikt is bij het samenstellen van dit document. "]]
                , Str " samen te stellen. "
                , Str "Deze eisen staan opgesomd in hoofdstuk ", xrefReference chpFRlabel, Str ", geordend op thema. "
                ]
          , Para 
                [ Str "De diagnose in hoofdstuk ", xrefReference chpDiaglabel
                , Str " is bedoeld voor de auteurs om gebreken uit hun Ampersand model op te sporen. "
                ]
          , Para 
                [ Str "De conceptuele analysis in hoofdstuk ", xrefReference chpCAlabel
                , Str " is bedoeld voor requirements engineers en architecten om de afspraken uit hoofdstuk "
                , xrefReference chpFRlabel, Str " te valideren en te formaliseren. "
                , Str "Tevens is het bedoeld voor testers om eenduidige testgevallen te kunnen bepalen. "
                , Str "De formalisatie in dit hoofdstuk maakt consistentie van de functionele specificatie bewijsbaar. "
                , Str "Ook garandeert het een eenduidige interpretatie van de eisen."
                ]
          , Para 
                [ Str "De hoofdstukken die dan volgen zijn bedoeld voor de bouwers van ", Quoted  SingleQuote [Str (name fSpec)], Str ". "
                , Str "De gegevensanalyse in hoofdstuk "
                , xrefReference chpDAlabel
                , Str " beschrijft de gegevensverzamelingen waarop ", Quoted  SingleQuote [Str (name fSpec)], Str " wordt gebouwd. "
                , Str "Elk volgend hoofdstuk definieert één business service. "
                , Str "Hierdoor kunnen bouwers zich concentreren op één service tegelijk. "
                , Str "Tezamen ondersteunen deze services alle afspraken uit hoofdstuk ", xrefReference chpFRlabel, Str ". "
                , Str "Door alle functionaliteit uitsluitend via deze services te ontsluiten waarborgt ", Quoted  SingleQuote [Str (name fSpec)]
                , Str " compliance ten aanzien van alle eisen uit hoofdstuk ", xrefReference chpFRlabel, Str " ."
                ]
         ]

        introContents English = 
         [ Para
                [Str "This document defines the functionality of an information system called "
                , Quoted  SingleQuote [Str (name fSpec)], Str ". "
                , Str "It defines business services in a system where people and applications work together "
                , Str "in order to fullfill their commitments. "
                , Str "A number of these rules have been used as functional requirement to assemble this functional specification"
                , Note [Para [Str "To use agreements as functional requirements characterizes the Ampersand approach, which has been used to produce this document. "]]
                , Str ". "
                , Str "Those rules are listed in chapter ", xrefReference chpFRlabel, Str ", ordered by theme. "
                ]
          , Para 
                [ Str "The diagnosis in chapter ", xrefReference chpDiaglabel
                , Str " is meant to help the authors identify shortcomings in their Ampersand script."
                ]
          , Para 
                [ Str "The conceptual analysis in chapter ", xrefReference chpCAlabel
                , Str " is meant for requirements engineers and architects to validate and formalize the requirements from chapter "
                , xrefReference chpFRlabel, Str ". "
                , Str "It is also meant for testers to come up with correct test cases. "
                , Str "The formalization in this chapter makes consistency of the functional specification provable. "
                , Str "It also yields an unambiguous interpretation of all requirements."
                ]
          , Para 
                [ Str "Chapters that follow have the builders of ", Quoted  SingleQuote [Str (name fSpec)], Str " as their intended audience. "
                , Str "The data analysis in chapter "
                , xrefReference chpDAlabel
                , Str " describes the data sets upon which ", Quoted  SingleQuote [Str (name fSpec)], Str " is built. "
                , Str "Each subsequent chapter defines one business service. "
                , Str "This allows builders focus on a single service at a time. "
                , Str "Together, these services fulfill all commitments from chapter ", xrefReference chpFRlabel, Str ". "
                , Str "By disclosing all functionality exclusively through these services, ", Quoted  SingleQuote [Str (name fSpec)]
                , Str " ensures compliance to all rules from chapter ", xrefReference chpFRlabel, Str ". "
                ]]  
------------------------------------------------------------
chpNatLangReqs :: Int -> Fspc -> Options ->  [Block]
chpNatLangReqs lev fSpec flags = header ++ dpIntro ++ dpRequirements
  where
  header :: [Block]
  header = labeledHeader lev chpFRlabel (case language flags of
                                             Dutch   ->  "Gemeenschappelijke taal"   
                                             English ->  "Shared Language"
                                         )
  dpIntro :: [Block]
  dpIntro = 
    case language flags of
        Dutch   -> [ Para
                     [ Str "Dit hoofdstuk beschrijft een natuurlijke taal, waarin functionele eisen ten behoeve van "
                     , Quoted  SingleQuote [Str (name fSpec)]
                     , Str " kunnen worden besproken en uitgedrukt. "
                     , Str "Hiermee wordt beoogd dat verschillende belanghebbenden de eisen op dezelfde manier begrijpen. "
                     , Str "De taal van ", Quoted  SingleQuote [Str (name fSpec)], Str " bestaat uit begrippen en basiszinnen, "
                     , Str "waarin functionele eisen worden uitgedrukt. "
                     , Str "Wanneer alle belanghebbenden afspreken dat zij deze basiszinnen gebruiken, "
                     , Str "althans voor zover het ", Quoted  SingleQuote [Str (name fSpec)], Str " betreft, "
                     , Str "delen zij precies voldoende taal om functionele eisen op dezelfde manier te begrijpen. "
                     , Str "Alle definities zijn genummerd omwille van de traceerbaarheid. "
                     ]]
        English -> [ Para
                     [ Str "This chapter defines the natural language, in which functional requirements of "
                     , Quoted  SingleQuote [Str (name fSpec)]
                     , Str " can be discussed and expressed. "
                     , Str "The purpose of this chapter is to create shared understanding among stakeholders. "
                     , Str "The language of ", Quoted  SingleQuote [Str (name fSpec)], Str " consists of concepts and basic sentences. "
                     , Str "All functional requirements are expressed in these terms. "
                     , Str "When stakeholders can agree upon this language, "
                     , Str "at least within the scope of ", Quoted  SingleQuote [Str (name fSpec)], Str ", "
                     , Str "they share precisely enough language to have meaningful discussions about functional requirements. "
                     , Str "All definitions have been numbered for the sake of traceability. "
                     ]]
  dpRequirements :: [Block]
  dpRequirements = theBlocks
    where
      (theBlocks,_) = aThemeAtATime toBeProcessedStuff (patterns fSpec) newCounter 
      toBeProcessedStuff = ( conceptsWithDefinition
                           , conceptsWithPurpose
                           , if length allRelsThatMustBeShown == length (nub allRelsThatMustBeShown) then allRelsThatMustBeShown
                             else fatal 250 "Some relations occur multiply in allRelsThatMustBeShown"
                           , [r | r<-vrules fSpec, r_usr r ] )  -- All user declared rules
         where
           conceptsWithDefinition     -- All concepts that have at least one explanation. Explanations are 
                                          -- currently bound to the conceptDefinitions of a concept.   
              = [(c, cds)
                | c <-concs fSpec
                , let cds = [cd | cd <- vConceptDefs fSpec
                               , name c == name cd
                               , not (null (cddef cd))]
                , not (null cds)]           
           conceptsWithPurpose     -- All concepts that have at least one explanation. Explanations are 
                                          -- currently bound to the conceptDefinitions of a concept.   
              = [(c, pps)
                | c <-concs fSpec
                , let pps = [p | let ps=purpose fSpec (language flags) c, p<-ps]
                , not (null pps)]           
           allRelsThatMustBeShown         -- All relations used in this specification, that are used in rules.
                                          -- and only those declarations that have at least one purpose.
              = [r | r@Rel{}<-mors fSpec
                  , not (null ( purpose fSpec (language flags) r))
                ]
                 
      aThemeAtATime :: ( [(A_Concept,[ConceptDef])]    -- all concepts that have one or more definitions. These definitions are to be used into this section and the sections to come
                       , [(A_Concept,[Explanation])]   -- all concepts that have one or more purposes. These purposes are to be used into this section and the sections to come
                       , [Relation]                    -- all relations to be processed into this section and the sections to come
                       , [Rule])                       -- all rules to be processed into this section and the sections to come
                    -> [Pattern]         -- the patterns that must be processed into this specification
                    -> Counter           -- unique definition counters
                    -> ([Block],Counter) -- The blocks that define the resulting document and the last used unique definition number
      aThemeAtATime  (still2doCdefsPre, still2doCpurpPre, still2doRelsPre, still2doRulesPre) pats iPre
           = case pats of
              []  -> printOneTheme Nothing (still2doCdefsPre, still2doRelsPre, still2doRulesPre) iPre
              _   -> (blocksOfOneTheme ++ blocksOfThemes,iPost)
         where
           (x:xs) = pats
           (blocksOfOneTheme,iPostFirst) = printOneTheme (Just x) thisThemeStuff iPre
           (blocksOfThemes,iPost)     = aThemeAtATime stuff2PrintLater xs iPostFirst
           thisThemeStuff = (thisThemeCdefs, thisThemeRels, [r | r<-thisThemeRules, r_usr r])
           thisThemeRules = [r | r<-still2doRulesPre, r_env r == name x ]      -- only user defined rules, because generated rules are documented in whatever caused the generation of that rule.
           rules2PrintLater = still2doRulesPre >- thisThemeRules
           thisThemeRels = [r | r<-still2doRelsPre, r `eleM` mors thisThemeRules] `uni`            -- all relations used in this theme's rules
                           [ makeRelation d | d<-declarations x, (not.null) (multiplicities d)] -- all relations used in multiplicity rules
           rels2PrintLater = still2doRelsPre >- thisThemeRels
           thisThemeCdefs = [(c,cd) |(c,cd)<- still2doCdefsPre, c `eleM` concs thisThemeRules]
           thisThemeCpurps = [(c,ps) |(c,ps)<- still2doCpurpPre, c `eleM` concs thisThemeRules]
           cDefs2PrintLater = still2doCdefsPre >- thisThemeCdefs
           cPurps2PrintLater = still2doCpurpPre >- thisThemeCpurps
           stuff2PrintLater = (cDefs2PrintLater, cPurps2PrintLater, rels2PrintLater, rules2PrintLater)
           
      -- | printOneTheme tells the story in natural language of a single theme.
      -- For this purpose, Ampersand authors should take care in composing explanations.
      -- Each explanation should state the purpose (and nothing else).
      printOneTheme :: Maybe Pattern -- name of the theme to process (if any)
                    -> ( [(A_Concept,[ConceptDef])]    -- all concepts that have one or more definitions, to be printed in this section
                       , [Relation]          -- Relations to print in this section
                       , [Rule])  -- Rules to print in this section
                    -> Counter      -- first free number to use for numbered items
                    -> ([Block],Counter)-- the resulting blocks and the last used number.
      printOneTheme mPat (cDefs2print, rels2print, rules2print) counters1
              = ( header' ++ explainsPat ++ sctcsIntro cDefs2print ++ concBlocks ++ relBlocks ++ ruleBlocks
                , counters4
                )
           where 
              (concBlocks,counters2) = sctcs cDefs2print counters1
              (relBlocks, counters3) = sctds rels2print  counters2
              (ruleBlocks,counters4) = sctrs rules2print counters3
              themeName = case mPat of
                           Nothing  -> ""
                           Just pat -> name pat
              header' :: [Block]
              header'  = [Header 1 [Str (case (mPat,language flags) of
                                              (Nothing, Dutch  ) -> "Losse eindjes..."
                                              (Nothing, English) -> "Loose ends..."
                                              _                  -> themeName
                                        )
                         ]         ]
              explainsPat :: [Block]
              explainsPat
               = case mPat of
                         Nothing  -> [Para 
                                      (case language flags of
                                        Dutch   -> [Str "Deze paragraaf beschrijft de relaties en concepten die niet in voorgaande secties zijn beschreven."]
                                        English -> [Str "This paragraph shows remaining fact types and concepts "
                                                   ,Str "that have not been described in previous paragraphs."]
                                      )]
                         Just pat -> explains2Blocks (purpose fSpec (language flags) pat)

              sctcsIntro :: [(A_Concept, [ConceptDef])] -> [Block]
              sctcsIntro xs
                = case xs of
                    []   -> []
                    ccds -> case language flags of
                              Dutch   ->  [Para$ (case ([Emph [Str $ unCap (name c)] |(c,_)<-xs], length [p |p <- vpatterns fSpec , name p == themeName]) of
                                                       ([] ,_) -> []
                                                       ([_],1) -> [ Str $ "In het volgende wordt de taal geïntroduceerd ten behoeve van "++themeName++". " | themeName/=""]
                                                       (cs ,1) -> [ Str "Nu volgen definities van de concepten "]++
                                                                  commaNLPandoc (Str "en") cs++
                                                                  [ Str ". Daarna worden de basiszinnen en regels geïntroduceerd."]
                                                       ([c],_) -> [ Str "Deze sectie introduceert het concept "
                                                                  , c]
                                                       (cs ,_) -> [ Str "Deze sectie introduceert de concepten "]++
                                                                  commaNLPandoc (Str "en") cs++
                                                                  [ Str ". "]
                                                 )++
                                                 (let cs = [(c,cds) | (c,cds)<-ccds, length cds>1] in
                                                  case (cs, length cs==length ccds) of
                                                   ([] ,   _  ) -> []
                                                   ([(c,_)]  , False) -> [ Str $ "Eén daarvan, "++name c++", heeft meerdere definities. " ]
                                                   (_        , False) -> [ Str "Daarvan hebben "]++commaNLPandoc (Str "en") (map (Str . name . fst) cs)++[Str " meerdere definities. "]
                                                   ([(_,cds)], True ) -> [ Str $ "Deze heeft "++count flags (length cds) "definitie"++". " ]
                                                   (_        , True ) -> [ Str "Elk daarvan heeft meerdere definities. "]
                                                 )
                                          ]
                              English ->  [Para$ (case ([Emph [Str $ unCap (name c)] |(c,_)<-xs], length [p |p <- vpatterns fSpec , name p == themeName]) of
                                                       ([] ,_) -> []
                                                       ([_],1) -> [ Str $ "The sequel introduces the language of "++themeName++". " | themeName/=""]
                                                       (cs ,1) -> [ Str "At this point, the definitions of "]++
                                                                  commaEngPandoc (Str "and") cs++
                                                                  [ Str " are given. Directly after that, the basic sentences and rules are introduced."]
                                                       ([c],_) -> [ Str "This section introduces concept "
                                                                  , Emph [c]]
                                                       (cs ,_) -> [ Str "This section introduces concepts "]++
                                                                  commaEngPandoc (Str "and") cs++
                                                                  [ Str ". "]
                                                 )++
                                                 (let cs = [(c,cds) | (c,cds)<-ccds, length cds>1] in
                                                  case (cs, length cs==length ccds) of
                                                   ([] ,   _  ) -> []
                                                   ([(c,_)]  , False) -> [ Str $ "One of these concepts, "++name c++", has multiple definitions. " ]
                                                   (_        , False) -> [ Str "Of those concepts "]++commaEngPandoc (Str "and") (map (Str . name . fst) cs)++[Str " have multiple definitions. "]
                                                   ([(_,cds)], True ) -> [ Str $ "It has "++count flags (length cds) "definition"++". " ]
                                                   (_        , True ) -> [ Str "Each one has several definitions. "]
                                                 )
                                          ]

              sctcs :: [(A_Concept, [ConceptDef])] -> Counter -> ([Block],Counter)
              sctcs xs c0 
                = case xs of
                    []  -> ([],c0)
                    _   -> (fstBlocks ++ restBlocks,c2)
                  where
                      d':ds' = xs
                      (fstBlocks,c1) = conceptdefinitions d' c0
                      (restBlocks,c2) = sctcs ds' c1
                      conceptdefinitions :: (A_Concept,[ConceptDef]) -> Counter -> ([Block],Counter)
                      conceptdefinitions (c,cds) cnt -- ^ this function takes a tuple of a concept and -if it exists- its definition. It returns a list of [Blocks] representing the text to print for it.
                      -- First print a glossary entry (only implemented for LaTeX for now), then write the definition and then explain its purpose(s).
                       = ( explains2Blocks (purpose fSpec (language flags) c)++
                           [DefinitionList [ ( [ Str (case language flags of
                                                        Dutch   -> "Definitie "
                                                        English -> "Definition ")
                                               , Str (show(getEisnr cnt))
                                               , Str ":"]
                                             , [ [ Para $ concat [ [ RawInline "latex" (symDefLabel c++"\n") | i==1]++
                                                                   [ RawInline "latex" ("\\glossary{name={"++name cd++"}, description={"++cddef cd++"}}\n")
                                                                   | fspecFormat flags==FLatex
                                                                   ] ++
                                                                   makeDefinition flags (name c) (cddef cd)
                                                                 | (i::Int,cd)<-zip [1..] cds]
                                                 ]
                                               ]
                                             )
                                           ]
                           ]
                         , incEis cnt )

              -- | sctds prints the requirements related to relations that are introduced in this theme.
              sctds :: [Relation] -> Counter -> ([Block],Counter)
              sctds xs c0 
                = case xs of
                    []  -> ([],c0)
                    _   -> (fstBlocks ++ restBlocks,c2)
                  where
                      d':ds' = xs
                      (fstBlocks,c1) = relBlock d' c0
                      (restBlocks,c2) = sctds ds' c1
                      relBlock :: Relation -> Counter -> ([Block],Counter)
                      relBlock rel cnt = ( explains2Blocks (purpose fSpec (language flags) rel) ++
                                           [DefinitionList [ ( [ Str (case language flags of
                                                                        Dutch   -> "Eis "
                                                                        English -> "Requirement ")
                                                               , Str (show(getEisnr cnt))
                                                               , Str ":"]
                                                             , [ Plain [RawInline "latex" $ symReqLabel (makeDeclaration rel)]:
                                                                 meaning (language flags) rel]
                                                             )
                                                           ]
                                           ]
                                         , incEis cnt)
                                                       
              sctrs :: [Rule] -> Counter -> ([Block],Counter)
              sctrs xs c0 
                = case xs of
                    []  -> ([],c0)
                    _   -> (fstBlocks ++ restBlocks,c2)
                  where
                      r':rs' = xs
                      (fstBlocks,c1) = ruleBlock r' c0
                      (restBlocks,c2) = sctrs rs' c1
                      ruleBlock :: Rule -> Counter -> ([Block],Counter)
                      ruleBlock r2 cnt = ( explains2Blocks (purpose fSpec (language flags) r2) ++
                                           [DefinitionList [ ( [Str (case language flags of
                                                                       Dutch   -> "Eis"
                                                                       English -> "Requirement")
                                                               ,Space
                                                               ,Str (show(getEisnr cnt))
                                                               ,if name r2=="" then Str ":" else Str (" ("++name r2++"):")]
                                                             , [ Plain [RawInline "latex" $ symReqLabel r2] :
                                                                 (let expls = [block | Means l econt<-rrxpl r2, l==language flags, block<-econt] in
                                                                  if null expls
                                                                  then explains2Blocks (autoMeaning (language flags) r2) 
                                                                  else expls 
                                                                 )]
                                                             )
                                                           ]
                                           ]
                                         , incEis cnt)
                      

------------------------------------------------------------
chpDiagnosis :: Int -> Fspc -> Options -> ([Block],[Picture])
chpDiagnosis lev fSpec flags
 = ( header ++                -- the chapter header
     diagIntro ++             -- an introductory text
     roleomissions ++         -- says which role-rule, role-interface, and role-relation assignments are missing
     roleRuleTable ++         -- gives an overview of rule-rule assignments
     missingConceptDefs ++    -- says which concept definitions are missing
     missingRels ++           -- says which relation declarations are missing
     relsNotUsed ++           -- says which relations are not used in any rule
     missingRules ++          -- says which rule definitions are missing
     invariantsInProcesses ++ -- 
     processrulesInPatterns++ -- 
     populationReport++       -- says which relations are populated.
     wipReport++              -- sums up the work items (i.e. the violations of process rules)
     violationReport          -- sums up the violations caused by the population of this script.
   , pics )
  where
  header :: [Block]
  header = labeledHeader lev chpDiaglabel (case language flags of
                                             Dutch   ->  "Diagnose"   
                                             English ->  "Diagnosis"
                                         )
  diagIntro :: [Block]
  diagIntro = 
    case language flags of
      Dutch   -> [Para
                  [ Str "Dit hoofdstuk geeft een analysis van het Ampersand-script van ", Quoted  SingleQuote [Str (name fSpec)], Str ". "
                  , Str "Deze analysis is bedoeld voor de auteurs van dit script. "
                  , Str "Op basis hiervan kunnen zij het script completeren en mogelijke tekortkomingen verbeteren. "
                  ]]
      English -> [Para
                  [ Str "This chapter provides an analysis of the Ampersand script of ", Quoted  SingleQuote [Str (name fSpec)], Str ". "
                  , Str "This analysis is intended for the authors of this script. "
                  , Str "It can be used to complete the script or to improve possible flaws. "
                  ]]
   

  roleRuleTable :: [Block]
  roleRuleTable 
    | null (fRoleRuls fSpec) && null(fRoleRels fSpec) = 
        case language flags of
          Dutch    -> [Para [ Str $ upCap (name fSpec)++" specificeert geen rollen. " ]]
          English  -> [Para [ Str $ upCap (name fSpec)++" does not define any roles. " ]]
    | null [r | r<-vrules fSpec, isSignal r ] =
        case language flags of
          Dutch    -> [Para [ Str $ upCap (name fSpec)++" kent geen procesregels. " ]]
          English  -> [Para [ Str $ upCap (name fSpec)++" does not define any process rules. " ]]
    | otherwise =
        (case language flags of
          Dutch    -> Para [ Str $ upCap (name fSpec)++" kent regels aan rollen toe. "
                            , Str "De volgende tabel toont welke regels door een bepaalde rol kunnen worden gehandhaafd."]
          English  -> Para [ Str $ upCap (name fSpec)++" assigns rules to roles. "
                            , Str "The following table shows the rules that are being maintained by a given role."]
        ) :
        [Table []  -- the table containing the role-rule assignments
        (AlignLeft:[AlignCenter |_<-rs])
        (0.0:[0.0 |_<-rs])
        (( case language flags of
          Dutch   -> [Plain [Str "regel"]] 
          English -> [Plain [Str "rule" ]] 
        ) :    [ [Plain [Str r]] | r <- rs ]
        )
        [ [Plain [Str (name rul)]]:[f r rul | r<-rs] | rul<-ruls ] 
        ]
     where
      rs = nub ( [r | (r,_) <- fRoleRuls fSpec]++
                 [r | (r,_) <- fRoleRels fSpec] )
      ruls = [r | r<-vrules fSpec, isSignal r ]
      f r rul | (r,rul) `elem` maintained      = [Plain [Math InlineMath "\\surd"]]
              | (r,rul) `elem` dead            = [Plain [Math InlineMath "\\times"]]
              | (r,rul) `elem` fRoleRuls fSpec = [Plain [Math InlineMath "\\odot"]]
              | otherwise                      = []
      maintained  -- (r,rul) `elem` maintained means that r can maintain rul without restrictions.
       = [ (r,rul)
         | (r,rul)<-fRoleRuls fSpec
         , and [(r,rel) `elem` fRoleRels fSpec | rel<-mors rul]
         ]
      dead -- (r,rul) `elem` maintained means that r cannot maintain rul without restrictions.
       = [ (r,rul)
         | (r,rul)<-fRoleRuls fSpec
         , (not.or) [(r,rel) `elem` fRoleRels fSpec | rel<-mors rul]
         ]

  roleomissions :: [Block]
  roleomissions
   = [ case language flags of
         Dutch   ->
           Plain [ Str $ upCap (name fSpec)++" kent geen regels aan rollen toe. "
                  , Str "Een generieke rol, User, zal worden gedefinieerd om al het werk te doen wat in het bedrijfsproces moet worden uitgevoerd."
                  ]
         English ->
           Plain [ Str $ upCap (name fSpec)++" does not assign rules to roles. "
                  , Str "A generic role, User, will be defined to do all the work that is necessary in the business process."
                  ]
     | (null.fRoleRuls) fSpec && (not.null.rules) fSpec] ++
     [ case language flags of
         Dutch   ->
           Plain [ Str $ upCap (name fSpec)++" specificeert niet welke rollen de inhoud van welke relaties mogen wijzigen. "
                  , Str ""
                  ]
         English ->
           Plain [ Str $ upCap (name fSpec)++" does not specify which roles may change the contents of which relations. "
                  , Str ""
                  ]
     | null (fRoleRels fSpec), (not.null.fRoleRuls) fSpec ||(not.null.fRoleRels) fSpec]

  missingConceptDefs :: [Block]
  missingConceptDefs
   = case (language flags, missing) of
      (Dutch,[])  -> [Para
                       [Str "Alle concepten in dit document zijn voorzien van een bestaansreden."]
                     | (not.null.concs) fSpec]
      (Dutch,[c]) -> [Para
                       [Str "De bestaansreden van concept ", Quoted SingleQuote [Str (name c)], Str " is niet gedocumenteerd."]
                     ]
      (Dutch,xs)  -> [Para $
                       [Str "De bestaansreden van de concepten: "]++commaNLPandoc (Str "en") (map (Str . name) xs)++[Str " is niet gedocumenteerd."]
                     ]
      (English,[])  -> [Para 
                        [Str "All concepts in this document have been provided with a purpose."]
                     | (not.null.concs) fSpec]
      (English,[c]) -> [Para 
                         [Str "The concept ", Quoted SingleQuote [Str (name c)], Str " remains without a purpose."]
                     ]
      (English,xs)  -> [Para $
                       [Str "Concepts "]++commaEngPandoc (Str "and") (map (Str . name) xs)++[Str " remain without a purpose."]
                     ]
   where missing = [c | c <-concs (declarations fSpec)
                     , cd <- vConceptDefs fSpec
                     , name c == name cd
                     , null (purpose fSpec (language flags) cd)
                   ]++
                   [c | c <-concs fSpec
                     , null [cd | cd <- vConceptDefs fSpec, name c == name cd]
                   ]

  missingRels :: [Block]
  missingRels
   = case (language flags, missing) of
      (Dutch,[])  -> [Para 
                       [Str "Alle relaties in dit document zijn voorzien van een reden van bestaan (purpose)."]
                     | (not.null.mors.rules) fSpec]
      (Dutch,[r]) -> [Para 
                       [ Str "De reden waarom relatie ", r
                       , Str " bestaat wordt niet uitgelegd."
                     ] ]
      (Dutch,rs)  -> [Para $
                       [ Str "Relaties "]++commaNLPandoc (Str "en") rs++
                       [ Str " zijn niet voorzien van een reden van bestaan (purpose)."
                     ] ]
      (English,[])  -> [Para 
                         [Str "All relations in this document have been provided with a purpose."]
                       | (not.null.mors.rules) fSpec]
      (English,[r]) -> [Para 
                         [ Str "The purpose of relation ", r
                         , Str " remains unexplained."
                       ] ]
      (English,rs)  -> [Para $
                         [ Str "The purpose of relations "]++commaEngPandoc (Str "and") rs++
                         [ Str " is not documented."
                       ] ]
     where missing = [(Math InlineMath . showMathDamb fSpec . ERel) r  -- ERel is always typeable, so showMathDamb may be used.
                     | r@(Rel{}) <-mors fSpec, not (isIdent r)
                     , null (purpose fSpec (language flags) r)
                     ]

  relsNotUsed :: [Block]
  pics        :: [Picture]
  (relsNotUsed,pics)
   = ( ( case (language flags, notUsed) of
          (Dutch,[])  -> [Para 
                           [Str "Alle relaties in dit document worden in één of meer regels gebruikt."]
                         | (not.null.mors.rules) fSpec]
          (Dutch,[r]) -> [Para 
                           [ Str "De relatie ", r
                           , Str " wordt in geen enkele regel gebruikt. "
                         ] ]
          (Dutch,rs)  -> [Para $
                           [ Str "Relaties "]++commaNLPandoc (Str "en") rs++
                           [ Str " worden niet gebruikt in regels. "
                         ] ]
          (English,[])  -> [Para 
                             [Str "All relations in this document are being used in one or more rules."]
                           | (not.null.mors.rules) fSpec]
          (English,[r]) -> [Para 
                             [ Str "Relation ", r
                             , Str " is not being used in any rule. "
                           ] ]
          (English,rs)  -> [Para $
                             [ Str "Relations "]++commaEngPandoc (Str "and") rs++
                             [ Str " are not used in any rule. "
                           ] ] ) ++
       ( case (language flags, pictsWithUnusedRels) of
          (Dutch,[pict])     -> [ Para [ Str "Figuur "
                                       , xrefReference (figlabel pict)
                                       , Str " geeft een conceptueel diagram met alle relaties."
                                       ] 
                                , Plain (xrefFigure1 pict)
                                ]
          (English,[pict])   -> [ Para [ Str "Figure "
                                       , xrefReference (figlabel pict)
                                       , Str " shows a conceptual diagram with all relations."
                                       ]
                                , Plain (xrefFigure1 pict)
                                ]
          (Dutch,picts)   -> concat
                                  [ Para [ Str "Figuur "
                                         , xrefReference (figlabel pict)
                                         , Str " geeft een conceptueel diagram met alle relaties die gedeclareerd zijn in "
                                         , Quoted SingleQuote [Str (name pat)]
                                         , Str "."
                                         ] 
                                    : [Plain (xrefFigure1 pict)]
                                  | (pict,pat)<-zip picts pats ]
          (English,picts) -> concat
                                  [ Para [ Str "Figure "
                                         , xrefReference (figlabel pict)
                                         , Str " shows a conceptual diagram with all relations declared in "
                                         , Quoted SingleQuote [Str (name pat)]
                                         , Str "."
                                         ]
                                    : [Plain (xrefFigure1 pict)]
                                  | (pict,pat)<-zip picts pats ] )
       , pictsWithUnusedRels           -- draw the conceptual diagram
     )
     where notUsed = nub [(Math InlineMath . showMathDamb fSpec . ERel . makeRelation) d  -- makeRelation d is always typeable, so showMathDamb may be used.
                         | d@(Sgn{}) <- declarations fSpec, decusr d
                         , d `notElem` (nub . map makeDeclaration . mors . rules) fSpec
                         ]
           pats = [ pat | pat<-patterns fSpec, (not.null) (declarations pat>-map makeDeclaration (mors pat)) ]
           pictsWithUnusedRels = [makePicture flags fSpec Rel_CG pat | pat<-pats ]

  missingRules :: [Block]
  missingRules
   = case (language flags, missingPurp, missingExpl) of
      (Dutch,[],[])    -> [ Para [Str "Alle regels in dit document zijn voorzien van een uitleg."]
                          | (length.rules) fSpec>1]
      (Dutch,rs,rs')   -> [Para 
                           (case rs>-rs' of
                              []  -> []
                              [r] -> [ Str "De bestaansreden van regel ", Emph [Str (name r)]
                                     , Str (" op regelnummer "++ln r++" van bestand "++fn r)
                                     , Str " wordt niet uitgelegd. "
                                     ]
                              rls -> (upC . commaNLPandoc (Str "en")  )
                                        [let nrs = [(Str . show . lineNumber) l | l<-cl] in
                                         strconcat ([Str ("op regelnummer"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaNLPandoc (Str "en") nrs++
                                                    [Str " van bestand "]++[(Str . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " worden regels gedefinieerd, waarvan de bestaansreden niet wordt uitgelegd. " ]
                            ++
                            case rs'>-rs of
                              []  -> []
                              [r] -> [ Str "De betekenis van regel ", Emph [Str (name r)]
                                     , Str (" op regelnummer "++ln r++" van bestand "++fn r)
                                     , Str " wordt uitgelegd in taal die door de computer is gegenereerd. "
                                     ]
                              rls -> (upC . commaNLPandoc (Str "en")  )
                                        [let nrs = [(Str . show . lineNumber) l | l<-cl] in
                                         strconcat ([Str ("op regelnummer"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaNLPandoc (Str "en") nrs++
                                                    [Str " van bestand "]++[(Str . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " staan regels, waarvan de betekenis wordt uitgelegd in taal die door de computer is gegenereerd. " ]
                            ++
                            case rs `isc` rs' of
                              []  -> []
                              [r] -> [ Str "Regel ", Emph [Str (name r)]
                                     , Str (" op regelnummer "++ln r++" van bestand "++fn r++" wordt niet uitgelegd. ")
                                     ]
                              rls -> (upC . commaNLPandoc (Str "en")  )
                                        [let nrs = [(Str . show . lineNumber) l | l<-cl] in
                                         strconcat ([Str ("op regelnummer"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaNLPandoc (Str "en") nrs++
                                                    [Str " van bestand "]++[(Str . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " worden regels gedefinieerd, zonder verdere uitleg. " ]
                           )
                          ]
      (English,[],[])  -> [ Para [Str "All rules in this document have been provided with a meaning and a purpose."]
                          | (length.rules) fSpec>1]
      (English,rs,rs') -> [Para $
                           ( case rs>-rs' of
                              []  -> []
                              [r] -> [ Str "The purpose of rule ", Emph [Str (name r)]
                                     , Str (" on line "++ln r++" of file "++fn r)
                                     , Str " is not documented. "
                                     ]
                              rls -> (upC . commaEngPandoc (Str "and") )
                                        [let nrs = [(Str . show . lineNumber) l | l<-cl] in
                                         strconcat ([ Str ("on line number"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaEngPandoc (Str "and") nrs ++
                                                    [Str " of file "]++[(Str . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " rules are defined without documenting their purpose. " ]
                           ) ++
                           ( case rs'>-rs of
                              []  -> []
                              [r] -> [ Str "The meaning of rule ", Emph [Str (name r)]
                                     , Str (" on line "++ln r++" of file "++fn r)
                                     , Str " is documented by means of computer generated language. "
                                     ]
                              rls -> (upC . commaEngPandoc (Str "and") )
                                        [let nrs = [(Str . show . lineNumber) l | l<-cl] in
                                         strconcat ([ Str ("on line number"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaEngPandoc (Str "and") nrs ++
                                                    [Str " of file "]++[(Str . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " rules are defined, the meaning of which is documented by means of computer generated language. " ]
                           ) ++
                           ( case rs `isc` rs' of
                              []  -> []
                              [r] -> [ Str "Rule ", Emph [Str (name r)]
                                     , Str (" on line "++ln r++" of file "++fn r++" is not documented. ")
                                     ]
                              rls -> (upC . commaEngPandoc (Str "and") )
                                        [let nrs = [(Str . show . lineNumber) l | l<-cl] in
                                         strconcat ([ Str ("on line number"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaEngPandoc (Str "and") nrs ++
                                                    [Str " of file "]++[(Str . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " rules are defined without any explanation. " ]
                           )
                          ]
     where missingPurp
            = nub [ r
                  | r<-rules fSpec
                  , null (purpose fSpec (language flags) r)
                  ]
           missingExpl
            = nub [ r
                  | r<-rules fSpec
                  , null [block | Means l econt<-rrxpl r, l==language flags, block<-econt]
                  ]
           upC (Str str:strs) = Str (upCap str):strs
           upC str = str
           fn r = locnm (origin r)
           ln r = locln (origin r)
           strconcat :: [Inline] -> Inline
           strconcat strs = (Str . concat) [ str | Str str<-strs]

  locnm (FileLoc(FilePos(filename,_,_))) = filename
  locnm (DBLoc str) = str
  locnm _ = "NO FILENAME"
  locln (FileLoc(FilePos(_,Pos l _,_))) = show l
  locln (DBLoc str) = str
  locln p = fatal 875 ("funny position "++show p++" in function 'locln'")

-- TODO: give richer feedback...
  invariantsInProcesses :: [Block]
  invariantsInProcesses
   = (case (language flags, prs, vprocesses fSpec) of
      (Dutch,  [],[] )  -> []
      (English,[],[] )  -> []
      (Dutch,  [],[p])  -> [ Para [ Str $ "Alle regels in proces "++name p++" zijn gekoppeld aan rollen." ]]
      (English,[],[p])  -> [ Para [ Str $ "All rules in process "++name p++" are linked to roles." ]]
      (Dutch,  [], _ )  -> [ Para [ Str "Alle regels in alle processen zijn gekoppeld aan rollen." ]]
      (English,[], _ )  -> [ Para [ Str "All rules in all processes are linked to roles." ]]
      (Dutch,  _ , _ )  -> [ Para [ Str "De volgende tabel toont welke regels in welke processen niet aan een rol gekoppeld zijn."
                                  , Str "Dit heeft als consequentie dat de computer de betreffende regel(s) zal handhaven."
                                  ]]
      (English,_ , _ )  -> [ Para [ Str "The following table shows which rules are not linked to a role within a particular process."
                                  , Str "This has as consequence that these rule(s) will be maintained by the computer."
                                  ]]
     )++
-- the table containing the role-rule assignments
     [ Table [] [AlignLeft,AlignLeft] [0.0,0.0]
       ( case language flags of
          Dutch   -> [ [Plain [Str "proces" ]] , [Plain [Str "regel"]] ]
          English -> [ [Plain [Str "process"]] , [Plain [Str "rule" ]] ]
       )
       [ [[Plain [Str (name p)]], [Plain (intercalate [Str ", "] [[Str (name r)] | r<-rs])]]
       | (p,rs)<-prs
       ]
     | not (null prs)]
     where prs = [(fp,rs) | fp<-vprocesses fSpec, let rs=invariants (proc fp), not (null rs) ]

  processrulesInPatterns :: [Block]
  processrulesInPatterns
   = [ case (language flags, vprocesses fSpec,prs) of
        (Dutch,  [p],[])  -> Para [ Str "Alle rol-regel-koppelingen gaan over regels die binnen proces ", Quoted SingleQuote [Str (name p)], Str " gedefinieerd zijn. " ]
        (English,[p],[])  -> Para [ Str "All role-rule assigments involve rules that are defined in process ", Quoted SingleQuote [Str (name p)], Str ". " ]
        (Dutch,  _,[])    -> Para [ Str "Voor elk proces geldt dat alle rol-regel-koppelingen gaan over regels die binnen dat proces zijn gedefinieerd." ]
        (English,_,[])    -> Para [ Str "The role-rule assignments in any of the described processes have been assigned to rules within that same process." ]
        (Dutch,  _,[(p,rol,rul)])
                          -> Para [ Str "Er is één koppeling tussen een rol en een regel van buiten het proces: "
                                  , Str "Rol ", Quoted SingleQuote [Str rol], Str " uit proces ", Quoted SingleQuote [Str (name p)], Str " is gebonden aan regel ", Quoted SingleQuote [Str (name rul)], Str " uit ", Quoted SingleQuote [Str (r_env rul)], Str "."
                                  ]
        (English,_,[(p,rol,rul)])
                          -> Para [ Str "There is one role that is assigned to a rule outside the process: "
                                  , Str "Role ", Quoted SingleQuote [Str rol], Str ", defined in process ", Quoted SingleQuote [Str (name p)], Str ", is assigned to rule ", Quoted SingleQuote [Str (name rul)], Str " from ", Quoted SingleQuote [Str (r_env rul)], Str "."
                                  ]
        (Dutch,  [p],_)   -> Para [ Str "De volgende tabel toont welke regels in welke patterns aan een rol gekoppeld zijn. "
                                  , Str "Dit heeft als consequentie dat de computer de betreffende regel(s) in proces ", Quoted SingleQuote [Str (name p)], Str " zal handhaven. "
                                  ]
        (English,[p],_)   -> Para [ Str "The following table shows which rules from outside the process are linked to a role in the process. "
                                  , Str "This has as consequence that these rule(s) will be maintained in the corresponding process ", Quoted SingleQuote [Str (name p)], Str ". "
                                  ]
        (Dutch,  _,_)     -> Para [ Str "Er zijn koppelingen tussen rollen en regels, die buiten de grenzen van het proces reiken. "
                                  , Str "De volgende tabel toont welke regels in welke patterns aan een rol gekoppeld zijn. "
                                  , Str "Dit heeft als consequentie dat de computer de betreffende regel(s) in de bijbehorende processen zal handhaven."
                                  ]
        (English,_,_)     -> Para [ Str "There are roles assigne to rules outside the bounds of the process. "
                                  , Str "The following table shows which rules that are defined in a pattern are linked to a role within a process."
                                  , Str "This has as consequence that these rule(s) will be maintained in the corresponding process(es)."
                                  ]
     | (not.null.vprocesses) fSpec && (not.null) [rra | fp<-vprocesses fSpec, rra<-maintains (proc fp)]
     ]        ++          
-- the table containing the role-rule assignments
     [ Table []
       ([AlignLeft]++[AlignLeft | multProcs]++[AlignLeft,AlignLeft])
       ([0.0]++[0.0 | multProcs]++[0.0,0.0])
       ( case language flags of
          Dutch   ->
              [[Plain [Str "rol"]] ]++[[Plain [Str "in proces" ]] | multProcs]++[[Plain [Str "regel"]], [Plain [Str "uit"  ]] ]
          English ->
              [[Plain [Str "role"]]]++[[Plain [Str "in process"]] | multProcs]++[[Plain [Str "rule" ]], [Plain [Str "from" ]] ]
       )
       [ [[Plain [Str rol]]]++[[Plain [Str (name p)]] | multProcs]++[[Plain [Str (name rul)]], [Plain [Str (r_env rul)]]]
       | (p,rol,rul)<-prs
       ] 
     | length prs>1]
     where prs = [(p,rol,rul) | p<-map proc (vprocesses fSpec), (rol,rul)<-maintains p, name rul `notElem` map name (rules p) ]
           multProcs = length (vprocesses fSpec)>1

  populationReport :: [Block]
  populationReport
   = [ Para (case (language flags, ds, declarations fSpec) of
        (Dutch,  [], [] ) -> [ Str "Dit script is leeg. " ]
        (English,[], [] ) -> [ Str "This script is empty. " ]
        (Dutch,  [],  _ ) -> [ Str "Dit script bevat geen populatie. " ]
        (English,[],  _ ) -> [ Str "This script contains no population. " ]
        (Dutch,  [d],[_]) -> [ Str "Relatie ", Math InlineMath (showMathDamb fSpec d), Str " heeft een populatie van ", Str (count flags (length (decpopu d)) "paar"), Str ". " ]  -- Every d is typeable, so showMathDamb may be used.
        (English,[d],[_]) -> [ Str "Relation ", Math InlineMath (showMathDamb fSpec d), Str " has ", Str (count flags (length (decpopu d)) "pair"), Str " in its population. " ]
        (Dutch,  [d], _ ) -> [ Str "Alleen relatie ", Math InlineMath (showMathDamb fSpec d), Str " heeft een populatie. Deze bevat ", Str (count flags (length (decpopu d)) "paar"), Str ". " ]
        (English,[d], _ ) -> [ Str "Only relation ", Math InlineMath (showMathDamb fSpec d), Str " is populated. It contains ", Str (count flags (length (decpopu d)) "pair"), Str ". " ]
        (Dutch,   _ , _ ) -> [ Str "De onderstaande tabel geeft de populatie van de verschillende relaties weer. " ]
        (English, _ , _ ) -> [ Str "The following table represents the population of various relations. " ])
     ] ++
     [ Table []
        [AlignLeft,AlignRight]
        [0.0,0.0]
        (case language flags of
          Dutch   -> [[Plain [Str "Concept"]], [Plain [Str "Populatie"]  ]]
          English -> [[Plain [Str "Concept"]], [Plain [Str "Population"] ]]
        )
        [ [[Plain [Str (name c)]], [Plain [(Str . show . length . cptos) c]]]
        | c<-cs
        ]
     | length cs>=1 ] ++
     [ Table []
        [AlignLeft,AlignRight]
        [0.0,0.0]
        (case language flags of
          Dutch   -> [[Plain [Str "Relatie"]],  [Plain [Str "Populatie"]  ]]
          English -> [[Plain [Str "Relation"]], [Plain [Str "Population"] ]]
        )
        [ [[Plain [Math InlineMath (showMathDamb fSpec d)]], [Plain [(Str . show . length . decpopu) d]]]  -- Every d is typeable, so showMathDamb may be used.
        | d<-ds
        ]
     | length ds>1 ]
     where
      ds = [d | d<-declarations fSpec, (not.null.decpopu) d]
      cs = [c | c@C{}<-concs fSpec, (not.null.cptos) c]

  wipReport :: [Block]
  wipReport
   = [ Para (case (language flags, concat popwork,popwork) of
              (Dutch,  [],_)       -> [ Str "De populatie in dit script beschrijft geen onderhanden werk. "
                                      | (not.null) [d | d<-declarations fSpec, not (null (decpopu d))] ]
              (English,[],_)       -> [ Str "The population in this script does not specify any work in progress. "
                                      | (not.null) [d | d<-declarations fSpec, not (null (decpopu d))] ]
              (Dutch,  [(r,ps)],_) -> [ Str "Regel ", quoterule r, Str (" laat "++count flags (length ps) "taak"++" zien.") ]
              (English,[(r,ps)],_) -> [ Str "Rule ", quoterule r, Str (" shows "++count flags (length ps) "task"++".") ]
              (Dutch,  _,[_])      -> [ Str "Dit script bevat onderhanden werk. De volgende tabel bevat details met regelnummers in het oorspronkelijk script-bestand." ]
              (English,_,[_])      -> [ Str "This script contains work in progress. The following table provides details with line numbers from the original script file." ]
              (Dutch,  _,_)        -> [ Str "Dit script bevat onderhanden werk. De volgende tabellen geven details met regelnummers in de oorspronkelijk script-bestanden." ]
              (English,_,_)        -> [ Str "This script contains work in progress. The following tables provide details with line numbers from the original script files." ]
            )
     ]        ++          
-- the following table actually belongs to the intro
     [ Table []
       [AlignLeft,AlignRight,AlignRight]
       [0.0,0.0,0.0]
       ( case language flags of
          Dutch   ->
              [[Plain [Str "regel"]], [Plain $[Str ((locnm . origin . fst . head) cl++" ") |length popwork>1]++[Str "script",LineBreak,Str "regel#"]], [Plain [Str "#signalen"] ]]
          English ->
              [[Plain [Str "rule" ]], [Plain $[Str ((locnm . origin . fst . head) cl++" ") |length popwork>1]++[Str "line#"]], [Plain [Str "#signals"] ]]
       )
       [ [[Plain [Str (name r)]], [Plain [(Str . locln . origin) r]], [Plain [(Str . show . length) ps]]]
       | (r,ps)<-cl, length ps>0
       ]
     | (length.concat) popwork>1, cl<-popwork ]        ++          
-- the tables containing the actual work in progress population
     concat
     [ [ Para ( (case language flags of
                  Dutch   -> Str "Regel"
                  English -> Str "Rule"):
                [Space,quoterule r,Space]++
                if fspecFormat flags==FLatex then [ Str "(", RawInline "latex" $ symReqRef r, Str ") "] else []++
                (case language flags of
                  Dutch   -> [ Str "luidt: " ]
                  English -> [ Str "says: "  ]
                )  
              )]  ++text r++
       [Plain ( case language flags of
                  Dutch  ->
                     [ Str "Deze regel bevat nog werk (voor "]++
                     commaNLPandoc (Str "of") (nub [Str rol | (rol, rul)<-fRoleRuls fSpec, r==rul])++[Str ")"]++
                     (if length ps == 1 then [Str ", te weten "]++oneviol r ps++[Str ". "] else
                      [ Str (". De volgende tabel laat de "++(if length ps>10 then "eerste tien " else "")++"items zien die aandacht vragen.")]
                     )
                  English ->
                     [ Str "This rule contains work"]++
                     commaEngPandoc (Str "or") (nub [Str rol | (rol, rul)<-fRoleRuls fSpec, r==rul])++[Str ")"]++
                     if length ps == 1 then [Str " by "]++oneviol r ps++[Str ". "] else
                      [ Str ("The following table shows the "++(if length ps>10 then "first ten " else "")++"items that require attention.")]
                     
              ) ]++
       [ violtable r ps | length ps>1]
     | (r,ps)<-concat popwork ]
     where
      text r
       = if null expls
         then explains2Blocks (autoMeaning (language flags) r) 
         else expls 
         where expls = [Plain (block++[Space]) | Means l econt<-rrxpl r, l==language flags, Para block<-econt]
      quoterule r
       = if name r==""
         then case language flags of
               English -> Str ("on "++show (origin r))
               Dutch   -> Str ("op "++show (origin r))
         else Quoted SingleQuote [Str (name r)]
      oneviol r [(a,b)]
       = if source r==target r && a==b
         then [Quoted  SingleQuote [Str (name (source r)),Space,Str a]]
         else [Str "(",Str (name (source r)),Space,Str a,Str ", ",Str (name (target r)),Space,Str b,Str ")"]
      oneviol _ _ = fatal 810 "oneviol must have a singleton list as argument."
      popwork :: [[(Rule,[(String, String)])]]
      popwork  = eqCl (locnm.origin.fst) [(r,ps) | r<-[r | r<-vrules fSpec, isSignal r ], let ps=ruleviolations r, not (null ps)]

  violationReport :: [Block]
  violationReport
   = [ Para (case (language flags, popviols, multviols) of
        (Dutch,  [],[])      -> [ Str "De populatie in dit script overtreedt geen regels. "
                                | (not.null) [d | d<-declarations fSpec, not (null (decpopu d))] ]
        (English,[],[])      -> [ Str "The population in this script violates no rule. "
                                | (not.null) [d | d<-declarations fSpec, not (null (decpopu d))] ]
        (Dutch,  [], _:_:_ )  -> [ Str "De populatie in dit script overtreedt alleen multipliciteitsregels. "
                                | (not.null) [d | d<-declarations fSpec, not (null (decpopu d))] ]
        (English,[], _:_:_ ) -> [ Str "The population in this script violates multiplicity rules only. "
                                | (not.null) [d | d<-declarations fSpec, not (null (decpopu d))] ]
        (Dutch,  [(r,ps)],_) -> [ Str "Regel ", quoterule r, Str (" veroorzaakt "++count flags (length ps) "overtreding"++". ") ]
        (English,[(r,ps)],_) -> [ Str "Rule ", quoterule r, Str (" causes "++count flags (length ps) "violation"++". ") ]
        (Dutch,  _,_)        -> [ Str "De onderstaande tabellen geven overtredingen van regels weer. " ]
        (English,_,_)        -> [ Str "The following tables represent rule violations. " ])
     ]        ++          
-- the table containing the rule violation counts
     [ Table []
       [AlignLeft,AlignRight,AlignRight]
       [0.0,0.0,0.0]
       ( case language flags of
          Dutch   ->
             [[Plain [Str "regel"]], [Plain $[Str ((locnm . origin . fst . head) cl++" ") |length popviol>1]++[Str "regel#"]], [Plain [Str "#overtredingen"] ]]
          English ->
             [[Plain [Str "rule" ]], [Plain $[Str ((locnm . origin . fst . head) cl++" ") |length popviol>1]++[Str "line#"]], [Plain [Str "#violations"] ]]
       )
       [ [[Plain [Str (name r)]], [Plain [(Str . locln . origin) r]], [Plain [(Str . show . length) ps]]]
       | (r,ps)<-cl, length ps>0
       ]
     | (length.concat) popviol>1, cl<-popviol ]        ++          
-- the table containing the multiplicity counts
     [ Table []
       [AlignLeft,AlignRight,AlignRight]
       [0.0,0.0,0.0]
       ( case language flags of
           Dutch   ->
              [[Plain [Str "regel"]], [Plain $[Str ((locnm . origin . fst . head) cl++" ") |length multviol>1]++[Str "regel#"]], [Plain [Str "#overtredingen"] ]]
           English ->
              [[Plain [Str "rule" ]], [Plain $[Str ((locnm . origin . fst . head) cl++" ") |length multviol>1]++[Str "line#"]], [Plain [Str "#violations"] ]]
       )
       [ [[Plain [Str (name r)]], [Plain [(Str . locln . origin) r]], [Plain [(Str . show . length) ps]]]
       | (r,ps)<-cl, length ps>0
       ]
     | (length.concat) multviol>1, cl<-multviol ]        ++          
-- the tables containing the actual violations of user defined rules
     concat
     [ [ Para ( (case language flags of
                   Dutch   -> Str "Regel"
                   English -> Str "Rule"):
                [Space,quoterule r,Space]++
                if fspecFormat flags==FLatex then [ Str "(", RawInline "latex" $ symReqRef r, Str ") "] else []++
                (case language flags of
                    Dutch   -> [ Str "luidt: " ]
                    English -> [ Str "says: "])
              )]  ++text r++
       [Plain ( case language flags of
                  Dutch   ->
                     Str "Deze regel wordt overtreden":
                     (if length ps == 1 then [Str " door "]++oneviol r ps++[Str ". "] else
                      [ Str (". De volgende tabel laat de "++if length ps>10 then "eerste tien " else ""++"overtredingen zien.")]
                     )
                  English ->
                     Str "This rule is violated":
                     (if length ps == 1 then [Str " by "]++oneviol r ps++[Str ". "] else
                      [ Str ("The following table shows the "++if length ps>10 then "first ten " else ""++"violations.")]
                     )
              )]++
       [ violtable r ps | length ps>1]
     | (r,ps)<-popviols ]++
-- the tables containing the actual violations of multiplicity rules
     concat
     [ textMult r++
       [Plain ( case language flags of
                 Dutch   ->
                   if length ps == 1 then [Str "Deze regel wordt overtreden door "]++oneviol r ps++[Str ". "] else
                    [ Str ("De volgende tabel laat de "++(if length ps>10 then "eerste tien overtredingen zien." else count flags (length ps) ((unCap.name.source)r)++" zien die deze regel overtreden."))]
                   
                 English ->
                   if length ps == 1 then [Str "This rule is violated by "]++oneviol r ps++[Str ". "] else
                    [ Str ("The following table shows the "++(if length ps>10 then "first ten violations." else count flags (length ps) ((unCap.name.source)r)++" that violate this rule."))]
                   
              )]++
       [ violtable r ps | length ps>1]
     | (r,ps)<-multviols ]
     where
     text r = if null expls
              then explains2Blocks (autoMeaning (language flags) r) 
              else expls 
              where expls = [Plain (block++[Space]) | Means l econt<-rrxpl r, l==language flags, Para block<-econt]
     textMult r
            = if null expls
              then explains2Blocks (autoMeaning (language flags) r) 
              else expls 
              where expls = [Plain ([Str "De relatie ",Space]++block++[Str ".",Space]) | Means l econt<-rrxpl r, l==language flags, Para block<-econt]
     quoterule r = if name r==""
                   then Str ("on "++show (origin r))
                   else Quoted SingleQuote [Str (name r)]
     oneviol r [(a,b)]
      = if source r==target r && a==b
        then [Quoted  SingleQuote [Str (name (source r)),Space,Str a]]
        else [Str "(",Str (name (source r)),Space,Str a,Str ", ",Str (name (target r)),Space,Str b,Str ")"]
     oneviol _ _ = fatal 810 "oneviol must have a singleton list as argument."
     popviols = [(r,ps) | r<-invariants fSpec++keyrules fSpec
                        , let ps=ruleviolations r, not (null ps)]
     multviols = [(r,ps) | r<-multrules fSpec
                         , let ps=ruleviolations r, not (null ps)]
     popviol :: [[(Rule,[(String, String)])]]
     popviol  = eqCl (locnm.origin.fst) [(r,ps) | r<-invariants fSpec, let ps=ruleviolations r, not (null ps)]
     multviol :: [[(Rule,[(String, String)])]]
     multviol  = eqCl (locnm.origin.fst) [(r,ps) | r<-multrules fSpec, let ps=ruleviolations r, not (null ps)]

  violtable r ps
      = if isIdent (antecedent r)  -- note: treat 'isIdent (consequent r) as binary table.
        then Table []
             [AlignLeft]
             [0.0]
             [[Plain [(Str . name . source) r]]]
             [ [[Plain [Str a]]]
             | (a,_)<-take 10 ps
             ]
        else Table []
             [AlignLeft,AlignLeft]
             [0.0,0.0]
             [[Plain [(Str . name . source) r]], [Plain [(Str . name . target) r] ]]
             [ [[Plain [Str a]], [Plain [Str b]]]
             | (a,b)<-take 10 ps
             ]
        

------------------------------------------------------------
chpConceptualAnalysis :: Int -> Fspc -> Options -> ([Block],[Picture])
chpConceptualAnalysis lev fSpec flags = (header ++ caIntro ++ caBlocks , pictures)
  where
  header :: [Block]
  header = labeledHeader lev chpCAlabel (case language flags of
                                            Dutch   ->  "Conceptuele Analyse"   
                                            English ->  "Conceptual Analysis"
                                        )
  caIntro :: [Block]
  caIntro =
   explains2Blocks (purpose fSpec (language flags) fSpec) ++ -- This explains the purpose of this context.
   (case language flags of
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
  (caBlocks,pictures) = ( [b | (blocks,_)<-ca, b<-blocks], [picture | (_,picture)<-ca] )
                        where ca=caSections (patterns fSpec)

  caSections :: [Pattern] -> [([Block],Picture)]
  caSections pats = iterat pats 1 [] []
   where
    iterat :: [Pattern] -> Int -> [A_Concept] -> [Declaration] -> [([Block],Picture)]
    iterat [] _ _ _ = []
    iterat (pat:ps) i seenConcepts seenDeclarations
     = ( [Header (lev+1) [Str (name pat)]]    -- new section to explain this theme
       ++ sctMotivation                       -- The section startss with the reason why this theme exists,
                                              -- followed by a conceptual model for this theme
       ++ ( case (genGraphics flags, language flags) of             -- announce the conceptual diagram
                 (True,Dutch  ) -> [Para [Str "Figuur ", xrefReference (figlabel pict), Str " geeft een conceptueel diagram van dit thema."]
                                   ,Plain (xrefFigure1 pict)]          -- draw the conceptual diagram
                 (True,English) -> [Para [Str "Figure ", xrefReference (figlabel pict), Str " shows a conceptual diagram of this theme."]
                                   ,Plain (xrefFigure1 pict)]          -- draw the conceptual diagram
                 _              -> []
          )                                   -- now provide the text of this theme.
       ++ (if null blocks then [] else [DefinitionList blocks])
       , pict):  iterat ps i'' seenCss seenDss
       where
         pict = (makePicture flags fSpec Rel_CG pat)   -- the Picture that represents this pattern's knowledge graph
                {caption = case language flags of
                            Dutch   ->"Conceptdiagram van "++name pat
                            English ->"Concept diagram of "++name pat}
         blocks  :: [([Inline], [[Block]])]
         blocks = sctRules ++ sctSignals
         sctMotivation
          = explains2Blocks (purpose fSpec (language flags) pat)
         (sctRules,   i',  seenCrs, seenDrs) = dpRule fSpec flags (invariants pat) i seenConcepts seenDeclarations
         (sctSignals, i'', seenCss, seenDss) = dpRule fSpec flags (processRules pat) i' seenCrs seenDrs

dpRule :: Fspc -> Options -> [Rule] -> Int -> [A_Concept] -> [Declaration]
          -> ([([Inline], [[Block]])], Int, [A_Concept], [Declaration])
dpRule fSpec flags = dpR
 where
   dpR [] n seenConcs seenDeclarations = ([], n, seenConcs, seenDeclarations)
   dpR (r:rs) n seenConcs seenDeclarations
     = ( ( [Str (name r)]
         , [ explains2Blocks (purpose fSpec (language flags) r) ++                   -- Als eerste de uitleg van de betreffende regel..
             concat [explains2Blocks (purpose fSpec (language flags) d) |d<-nds] ++  -- Dan de uitleg van de betreffende relaties
             [ Plain text1 | not (null nds)] ++
             pandocEqnArray [ ( texOnly_Id(name d)
                              , ":"
                              , texOnly_Id(name (source d))++(if isFunction d then texOnly_fun else texOnly_rel)++texOnly_Id(name(target d))++symDefLabel d
                              )
                            |d<-nds] ++
             [ Plain text2 | not (null rds)] ++
             [ Plain text3 ] ++
             (if showPredExpr flags
              then pandocEquation ((showLatex.toPredLogic.reprAsRule r.conjNF.rrexp) r++symDefLabel r)
              else let cr = (conjNF.rrexp) r in
                   if isTypeable cr
                   then pandocEquation ((showMathDamb fSpec.reprAsRule r) cr++symDefLabel r)
                   else fatal 1101 ("Untypeable "++show cr)
             )++
             [ Plain text4 | isSignal r] ++
             (if not (isSignal r) then [] else
              if showPredExpr flags
              then pandocEquation ((showLatex.toPredLogic.reprAsRule r.disjNF.notCpl.rrexp) r++symDefLabel r)
              else let dr = (disjNF.notCpl.rrexp) r in
                   if isTypeable dr
                   then pandocEquation ((showMathDamb fSpec.reprAsRule r) dr++symDefLabel r)
                   else fatal 1110 ("Untypeable "++show dr)
             )++
             [ Plain text5 | length nds>1]
           ] 
         ): dpNext
       , n'
       , seenCs 
       , seenDs
       )
       where
        text1
         = case (length nds,language flags) of
             (1,Dutch)   -> let d = head nds in
                            [Str ("Om dit te formaliseren is een "++(if isFunction d then "functie" else "relatie")++" "),Str (name d),Str " nodig (",RawInline "latex" $ symDefRef d,Str "):"]
             (1,English) -> let d = head nds in
                            [Str "In order to formalize this, a ", Str (if isFunction d then "function" else "relation"), Space, Str (name d),Str " is introduced (",RawInline "latex" $ symDefRef d,Str "):"]
             (l,Dutch)   -> [Str "Om te komen tot de formalisatie in vergelijking",RawInline "latex" "~",RawInline "latex" $ symDefRef r,Str (" zijn de volgende "++count flags l "relatie"++" nodig.")]
             (l,English) -> [Str "To arrive at the formalization in equation",RawInline "latex" "~",RawInline "latex" $ symDefRef r,Str (", the following "++count flags l "relation"++" are introduced.")]
        text2
         = (case (length nds,length rds,language flags) of
             (0,1,Dutch)   -> [Str "Definitie ",RawInline "latex" $ symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ") wordt gebruikt"]
             (0,1,English) -> [Str "We use definition ",RawInline "latex" $ symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ")"]
             (0,_,Dutch)   -> Str "We gebruiken definities ":commaNLPandoc (Str "en") [RawInline "latex" $ symDefRef d++" ("++texOnly_Id (name d)++")" |d<-rds]
             (0,_,English) -> Str "We use definitions ":commaEngPandoc (Str "and") [RawInline "latex" $ symDefRef d++" ("++texOnly_Id (name d)++")" |d<-rds]
             (_,1,Dutch)   -> [Str "Daarnaast gebruiken we definitie ",RawInline "latex" $ symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ")"]
             (_,1,English) -> [Str "Beside that, we use definition ",RawInline "latex" $ symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ")"]
             (_,_,Dutch)   -> Str "Ook gebruiken we definities ":commaNLPandoc (Str "en") [RawInline "latex" $ symDefRef d++" ("++texOnly_Id (name d)++")" |d<-rds]
             (_,_,English) -> Str "We also use definitions ":commaEngPandoc (Str "and") [RawInline "latex" $ symDefRef d++" ("++texOnly_Id (name d)++")" |d<-rds]
           )++
           (case (length nds,language flags) of
             (1,Dutch)   -> [Str " om eis",RawInline "latex" "~",RawInline "latex" $ symReqRef r, Str " (pg.",RawInline "latex" "~",RawInline "latex" $ symReqPageRef r, Str ") te formaliseren:"]
             (1,English) -> [Str " to formalize requirement",RawInline "latex" "~",RawInline "latex" $ symReqRef r, Str " (page",RawInline "latex" "~",RawInline "latex" $ symReqPageRef r, Str "):"]
             _           -> [Str ". "]
           )
        text3
         = case (language flags,isSignal r) of
            (Dutch  ,False) -> [Str "Dit betekent: "]
            (English,False) -> [Str "This means: "]
            (Dutch  ,True)  -> [Str "Activiteiten, die door deze regel zijn gedefinieerd, zijn afgerond zodra: "]
            (English,True)  -> [Str "Activities that are defined by this rule are finished when: "]
        text4
         = case language flags of
                 Dutch   -> [Str " Deze activiteiten worden opgestart door:"]
                 English -> [Str " These activities are signalled by:"]
        text5
         = case (language flags,isSignal r) of
             (Dutch  ,False) -> [Str "Dit komt overeen met eis",RawInline "latex" "~",RawInline "latex" $ symReqRef r, Str " op pg.",RawInline "latex" "~",RawInline "latex" $ symReqPageRef r, Str "."]
             (English,False) -> [Str "This corresponds to requirement",RawInline "latex" "~",RawInline "latex" $ symReqRef r, Str " on page",RawInline "latex" "~",RawInline "latex" $ symReqPageRef r, Str "."]
             (Dutch  ,True)  -> [ Str "Dit komt overeen met "
                                , Quoted  SingleQuote [Str (name r)]
                                , Str " (",RawInline "latex" $ symReqRef r, Str " op pg.",RawInline "latex" "~",RawInline "latex" $ symReqPageRef r, Str ")."]
             (English,True)  -> [Str "This corresponds to "
                                , Quoted  SingleQuote [Str (name r)]
                                , Str " (",RawInline "latex" $ symReqRef r, Str " op pg.",RawInline "latex" "~",RawInline "latex" $ symReqPageRef r, Str ")."]
        ncs = concs r >- seenConcs            -- newly seen concepts
        cds = [(c,cd) | c<-ncs, cd<-conceptDefs fSpec, cdnm cd==name c]    -- ... and their definitions
        ds  = map makeDeclaration (mors r)
        nds = [d | d@(Sgn{})<-ds >- seenDeclarations]     -- newly seen declarations
        rds = [d | d@(Sgn{})<-ds `isc` seenDeclarations]  -- previously seen declarations
        ( dpNext, n', seenCs,  seenDs ) = dpR rs (n+length cds+length nds+1) (ncs++seenConcs) (nds++seenDeclarations)

------------------------------------------------------------
--DESCR -> the process analysis contains a section for each process in the fspec
-- If an Ampersand script contains no reference to any role whatsoever, a process analysis is meaningless.
-- In that case it will not be printed. To detect whether this is the case, we can look whether the
-- mayEdit attributes remain empty.
noProcesses :: Fspc -> Bool
noProcesses fSpec = null (fRoleRels fSpec) && null (fRoleRuls fSpec)

chpProcessAnalysis :: Int -> Fspc -> Options -> ([Block],[Picture])
chpProcessAnalysis lev fSpec flags
 = (header ++ roleRuleBlocks ++ roleRelationBlocks ++ processSections , pictures)
 where
  pictures = [pict | (_,picts)<-procSections (vprocesses fSpec),pict<-picts]
  processSections :: [Block]
  processSections
   = if null (vprocesses fSpec) then [] else [block | (bs,_)<-procSections (vprocesses fSpec), block<-bs]

  header :: [Block]
  header
   = labeledHeader lev chpPAlabel (case language flags of
                                              Dutch   ->  "Procesanalyse"   
                                              English ->  "Process Analysis"
                                        ) ++
     explains2Blocks (purpose fSpec (language flags) fSpec) ++ -- This explains the purpose of this context.
     [ case language flags of
         Dutch   ->
            Plain [ Str $ upCap (name fSpec)++" kent geen regels aan rollen toe. "
                  , Str "Een generieke rol, User, zal worden gedefinieerd om al het werk te doen wat in het bedrijfsproces moet worden uitgevoerd."
                  ]
         English ->
            Plain [ Str $ upCap (name fSpec)++" does not assign rules to roles. "
                  , Str "A generic role, User, will be defined to do all the work that is necessary in the business process."
                  ]
     | null (fRoleRuls fSpec)] ++
     [ case language flags of
         Dutch   ->
            Plain [ Str $ upCap (name fSpec)++" specificeert niet welke rollen de inhoud van welke relaties mogen wijzigen. "
                  , Str ""
                  ]
         English ->
            Plain [ Str $ upCap (name fSpec)++" does not specify which roles may change the contents of which relations. "
                  , Str ""
                  ]
     | null (fRoleRels fSpec)]
     
  roleRuleBlocks :: [Block]
  roleRuleBlocks
   = if null (fRoleRuls fSpec) && (not.null.rules) fSpec then [] else
     [ case language flags of
          Dutch   ->
            Para [ Str $ upCap (name fSpec)++" kent regels aan rollen toe. "
                 , Str "De volgende tabel toont de regels die door een bepaalde rol worden gehandhaafd."
                 ]
          English ->
            Para [ Str $ upCap (name fSpec)++" assigns rules to roles. "
                 , Str "The following table shows the rules that are being maintained by a given role."
                 ]
-- the table containing the role-rule assignments
     , Para  $ [ RawInline "latex" "\\begin{tabular}{|l|l|}\\hline\n"
               , case language flags of
                  Dutch   -> RawInline "latex" "Rol&Regel\\\\ \\hline\n"
                  English -> RawInline "latex" "Role&Rule\\\\ \\hline\n"
               ]++
               [ RawInline "latex" $ intercalate "\\\\ \\hline\n   " 
                       [ role++" & "++name r++
                         concat[ "\\\\\n   &"++name rul | rul<-map snd (tail rrClass)]
                       | rrClass<-eqCl fst (fRoleRuls fSpec)
                       , let role=fst (head rrClass), let r=snd (head rrClass)
                       ]
               ]++
               [ RawInline "latex" "\\\\ \\hline\n\\end{tabular}"
               ]
     ]

-- the table containing the role-relation assignments
  roleRelationBlocks :: [Block]
  roleRelationBlocks
   = if null (fRoleRels fSpec) then [] else
     [ case language flags of
          Dutch   ->
            Para [ Str $ upCap (name fSpec)++" kent rollen aan relaties toe. "
                 , Str "De volgende tabel toont de relaties waarvan de inhoud gewijzigd kan worden door iemand die een bepaalde rol vervult."
                 ]
          English ->
            Para [ Str $ upCap (name fSpec)++" assigns roles to relations. "
                 , Str "The following table shows the relations, the content of which can be altered by anyone who fulfills a given role."
                 ]
     , Para  $ [ RawInline "latex" "\\begin{tabular}{|l|l|}\\hline\n"
               , RawInline "latex"
                    (case  language flags of
                       Dutch   -> "Rol&Relatie\\\\ \\hline\n"
                       English -> "Role&Relation\\\\ \\hline\n")
               ]++
               [ RawInline "latex" $ intercalate "\\\\ \\hline\n   " 
                       [ role++" & $"++showMathDamb fSpec r++"$"++
                         concat[ "\\\\\n   &$"++showMathDamb fSpec (snd rs)++"$" | rs<-tail rrClass]
                       | rrClass<-eqCl fst (fRoleRels fSpec)
                       , let role=fst (head rrClass), let r=snd (head rrClass)
                       ]
               ]++
               [ RawInline "latex" "\\\\ \\hline\n" | not (null rolelessRels)]++
               [ RawInline "latex" $ intercalate "\\\\\n   " [ "&$"++showMathDamb fSpec d++"$" | d<-rolelessRels] | not (null rolelessRels)]++
               [ RawInline "latex" "\\\\ \\hline\n\\end{tabular}"
               ]
     ]
     where
      rolelessRels = [ d | d<-declarations fSpec, d `notElem` (nub.map (makeDeclaration.snd)) (fRoleRels fSpec) ]

  emptyProcess :: Process -> Bool
  emptyProcess p = null (rules p)
  
-- the sections in which processes are analyzed
  procSections :: [FProcess] -> [([Block],[Picture])]
  procSections fprocs = iterat [fp |fp<-fprocs, not (emptyProcess (proc fp))] 1 (concs (patterns fSpec)) ((concatMap declarations.patterns) fSpec)
   where
    iterat :: [FProcess] -> Int -> [A_Concept] -> [Declaration] -> [([Block],[Picture])]
    iterat [] _ _ _ = []
    iterat (fproc:fps) i seenConcepts seenDeclarations
     = ( [Header (lev+1) [Str (name fproc)]]    -- new section to explain this theme
       ++ sctMotivation                       -- The section startss with the reason why this process exists,
       ++ txtProcessModel fproc
       ++ txtLangModel fproc
       ++ (if null sctRules then [] else [DefinitionList sctRules])
       , [picProcessModel fproc, picLangModel fproc]):  iterat fps i' seenCrs seenDrs
       where
         sctMotivation
          = explains2Blocks (purpose fSpec (language flags) fproc)
         sctRules  :: [([Inline], [[Block]])]
         (sctRules,i',seenCrs,seenDrs) = dpRule fSpec flags (rules (proc fproc)) i seenConcepts seenDeclarations

  txtLangModel :: FProcess->[Block]
  txtLangModel fp
   = if not (genGraphics flags) then [] else
     -- (if name ifc==name (head (fActivities fSpec)) then processModelIntro else [])++
      [Para (case language flags of                                     -- announce the conceptual diagram
             Dutch   -> [ Str "Het conceptueel diagram in figuur ", xrefReference (figlabel pict)
                        , Str " geeft een overzicht van de taal waarin dit proces wordt uitgedrukt."]
             English -> [ Str "The conceptual diagram of figure ", xrefReference (figlabel pict)
                        , Str " provides an overview of the language in which this process is expressed."])
      ,Plain (xrefFigure1 pict)]                     -- draw the diagram
     where pict = picLangModel fp

  txtProcessModel :: FProcess->[Block]
  txtProcessModel p
   = if not (genGraphics flags) then [] else
     -- (if name ifc==name (head (fActivities fSpec)) then processModelIntro else [])++
     [Para (case language flags of                                     -- announce the processModel diagram
             Dutch   -> [ Str "Figuur ", xrefReference (figlabel pict)
                        , Str " geeft het procesmodel weer."]
             English -> [ Str "Figure ", xrefReference (figlabel pict)
                        , Str " shows the process model."])
     ,Plain (xrefFigure1 pict)]                     -- draw the diagram
     where pict = picProcessModel p

  picLangModel :: FProcess->Picture
  picLangModel fproc
   = ((makePictureObj flags (name fproc) PTProcLang . printDotGraph . conceptualGraph fSpec flags Rel_CG) fproc)   -- the Picture that represents this process's knowledge graph with all user defined relations (controlled by Rel_CG)
                {caption = case language flags of
                            Dutch   ->"Basiszinnen van "++name fproc
                            English ->"Basic sentences of "++name fproc}

  picProcessModel :: FProcess->Picture
  picProcessModel fproc
   = (makePicture flags fSpec Plain_CG fproc) -- the Picture that represents this interface's knowledge graph with only those relations that are used in rules (controlled by Plain_CG).
        {caption = case language flags of
                    Dutch   ->"Procesmodel van "++name fproc
                    English ->"Process model of "++name fproc}

------------------------------------------------------------
--DESCR -> the data analysis contains a section for each class diagram in the fspec
--         the class diagram and multiplicity rules are printed
chpDataAnalysis :: Int -> Fspc -> Options -> ([Block],[Picture])
chpDataAnalysis lev fSpec flags
 = ( header ++ 
     daContents ++
     daAssociations remainingDecls ++
     [b | InternalPlug p<-plugInfos fSpec, b<-daPlug p] ++
     [Header (lev+1) [Str $ case language flags of
                              Dutch   ->  "Schakelpaneel"   
                              English ->  "Switchboard"
                     ]] ++
     txtSwitchboard

   , [classificationPicture, classDiagramPicture, picSwitchboard] )
 where 
  remainingDecls = mors fSpec >- [r | p<-plugInfos fSpec, r<-mors p]

  header :: [Block]
  header = labeledHeader lev chpDAlabel (case language flags of
                                              Dutch   ->  "Gegevensstructuur"   
                                              English ->  "Data structure"
                                        )
  -- | a short introduction
  daContents :: [Block]
  daContents = 
   (case language flags of
     Dutch   -> [Para $
                  ( if genGraphics flags 
                    then 
                     ( if null (gens fSpec) then [] else
                       [ Str "Een aantal concepten uit hoofdstuk "
                       , xrefReference chpFRlabel
                       , Str " zit in een classificatiestructuur. Deze is in figuur "
                       , xrefReference (figlabel classificationPicture)
                       , Str " weergegeven. " ] 
                     ) ++
                       [ Str "De eisen, die in hoofdstuk "
                       , xrefReference chpFRlabel
                       , Str " beschreven zijn, zijn in een gegevensanalyse vertaald naar het gegevensmodel van figuur "
                       , xrefReference (figlabel classDiagramPicture)
                       , Str ". " ]
                    else []
                  )++
                  [ Str (case length (classes classDiagram) of
                            0 -> "Er zijn"
                            1 -> "Er is één gegevensverzameling,"
                            _ -> "Er zijn "++count flags (length (classes classDiagram)) "gegevensverzameling"++","
                        )
                  , Str $ " "++count flags (length (assocs classDiagram)) "associatie"++","
                  , Str $ " "++count flags (length (geners classification)) "generalisatie"++" en"
                  , Str $ " "++count flags (length (aggrs classDiagram)) "aggregatie"++"."
                  , Str $ " "++name fSpec++" kent in totaal "++count flags (length (concs fSpec)) "concept"++"."
                  ]]
     English -> [Para $
                  ( if genGraphics flags 
                    then 
                     ( if null (gens fSpec) then [] else
                       [ Str "A number of concepts from chapter "
                       , xrefReference chpFRlabel
                       , Str " are organized in a classification structure. This is represented in figure "
                       , xrefReference (figlabel classificationPicture)
                       , Str ". " ] ) ++
                       [ Str "The requirements, which are listed in chapter "
                       , xrefReference chpFRlabel
                       , Str ", have been translated into the data model in figure "
                       , xrefReference (figlabel classDiagramPicture)
                       , Str ". " ]
                    else []
                  )++
                  [ Str (case length (classes classDiagram) of
                            0 -> "There are"
                            1 -> "There is one data set,"
                            _ -> "There are "++count flags (length (classes classDiagram)) "data set"++","
                        )
                  , Str $ " "++count flags (length (assocs classDiagram)) "association"++","
                  , Str $ " "++count flags (length (geners classification)) "generalisation"++", and"
                  , Str $ " "++count flags (length (aggrs classDiagram)) "aggregation"++"."
                  , Str $ " "++name fSpec++" has a total of "++count flags (length (concs fSpec)) "concept"++"."
                  ]] --TODO
   ) ++ [ Plain $ xrefFigure1 classificationPicture, Plain $ xrefFigure1 classDiagramPicture ]  -- TODO: explain all multiplicities]

  classDiagram :: ClassDiag
  classDiagram = cdAnalysis fSpec flags

  classification :: ClassDiag
  classification = clAnalysis fSpec flags

  classificationPicture :: Picture
  classificationPicture
    = (makePicture flags fSpec Gen_CG classification)
        {caption = case language flags of
                    Dutch   ->"Classificatie van "++name fSpec
                    English ->"Classification of "++name fSpec}

  classDiagramPicture :: Picture
  classDiagramPicture
   = (makePicture flags fSpec Plain_CG classDiagram)
        {caption = case language flags of
                    Dutch   ->"Datamodel van "++name fSpec
                    English ->"Data model of "++name fSpec}

  picSwitchboard :: Picture
  picSwitchboard
   = (makePicture flags fSpec Plain_CG sb) 
        {caption = case language flags of
                    Dutch   ->"Schakelpaneel van "++name fSpec
                    English ->"Switchboard of "++name fSpec}
     where
       sb :: SwitchBdDiagram
       sb = (sbDiagram fSpec . fSwitchboard) fSpec -- the Picture that represents this interface's knowledge graph

  txtSwitchboard :: [Block]
  txtSwitchboard
   = (case language flags of                                     -- announce the switchboard diagram
           Dutch   -> [Para [ Str "Figuur ", xrefReference (figlabel picSwitchboard)
                            , Str " geeft het schakelpaneel (switchboard diagram) weer. "
                            , Str "Dit wordt gebruikt bij het ontwerpen van de database functionaliteit."] ]
           English -> [Para [ Str "Figure ", xrefReference (figlabel picSwitchboard)
                            , Str " shows the switchboard diagram."
                            , Str "This is used in designing the database functionality."] ]
     )
     ++ [Plain (xrefFigure1 picSwitchboard)]                     -- draw the switchboard

-- The properties of various declations are documented in different tables.
-- First, we document the heterogeneous properties of all relations
-- Then, the endo-poperties are given, and finally
-- the signals are documented.
  daAssociations :: [Relation] -> [Block]
  daAssociations rs = heteroMultiplicities ++ endoProperties ++ keyDocumentation
   where
    heteroMultiplicities
     = case [r | r@Rel{}<-rs, not (isProp r), not (isAttribute r)] of
        []  -> []
        [r] -> [ case language flags of
                   Dutch   ->
                      Para [ Str $ upCap (name fSpec)++" heeft één associatie: "++(showADL . disambiguate fSpec . ERel) r++". Deze associatie "++ -- ERel r is typeable for each r, so disambibuate (ERel r) is defined.
                                   case (isTot r, isSur r) of
                                    (False, False) -> "heeft geen beperkingen ten aanzien van multipliciteit."
                                    (True,  False) -> "is totaal."
                                    (False, True ) -> "is surjectief."
                                    (True,  True ) -> "is totaal en surjectief."
                           ]
                   English ->
                      Para [ Str $ upCap (name fSpec)++" has one association: "++(showADL . disambiguate fSpec . ERel) r++". This association "++ -- ERel r is typeable for each r, so disambibuate (ERel r) is defined.
                                   case (isTot r, isSur r) of
                                    (False, False) -> "has no restrictions with respect to multiplicities. "
                                    (True,  False) -> "is total."
                                    (False, True ) -> "is surjective."
                                    (True,  True ) -> "is total and surjective."
                           ]]
        rs' -> case [r | r<-rs', (not.null) ([Tot,Sur] `isc` multiplicities r) ] of
               []  -> []
               [r] -> [ case language flags of
                          Dutch   ->
                             Para [ Str $ upCap (name fSpec)++" heeft "++count flags (length rs) "associatie"++". "
                                  , Str $ " Daarvan is "++(showADL . disambiguate fSpec . ERel) r++if isTot r then "totaal" else "surjectief"
                                  ]
                          English   ->
                            Para [ Str $ upCap (name fSpec)++" has "++count flags (length rs) "association"++". "
                                  , Str $ " Association "++(showADL . disambiguate fSpec . ERel) r++" is "++if isTot r then "total" else "surjective"
                                  ]
                      ]
               _   -> [ case language flags of
                          Dutch   ->
                             Para [ Str $ upCap (name fSpec)++" heeft de volgende associaties en multipliciteitsrestricties. "
                                  ]
                          English   ->
                             Para [ Str $ upCap (name fSpec)++" has the following associations and multiplicity constraints. "
                                  ]
                      , Table [] [AlignLeft,AlignCenter,AlignCenter] [0.0,0.0,0.0]
                        ( case language flags of
                          Dutch   ->
                               [ [Plain [Str "relatie"]]
                               , [Plain [Str "totaal"]]
                               , [Plain [Str "surjectief"]]]
                          English   ->
                               [ [Plain [Str "relation"]]
                               , [Plain [Str "total"]]
                               , [Plain [Str "surjective"]]]
                        )
                        [[[Plain [Math InlineMath (showMathDamb fSpec r)]] -- r is a relation, and therefore isTypeable r. So  showMathDamb fSpec r  exists.
                         ,[Plain [Math InlineMath "\\surd" | isTot r]]
                         ,[Plain [Math InlineMath "\\surd" | isSur r]]]
                        | r<-rs', not (isAttribute r)
                        ]
                      ]
    isAttribute r = (not.null) ([Uni,Inj] `isc` multiplicities r)
    endoProperties
     = if null [ m | d<-declarations fSpec, m<-multiplicities d, m `elem` [Rfx,Irf,Trn,Sym,Asy]]
       then []
       else [ Para [ case language flags of
                          Dutch   ->
                            Str $ "Er is één endorelatie, "++texOnly_Id(name d)++" met de volgende eigenschappen: "
                          English   ->
                            Str $ "There is one endorelation, "++texOnly_Id(name d)++" with the following properties: "]
            | length hMults==1, d<-hMults ]++
            [ Para [ case language flags of
                          Dutch   ->
                            Str $ "In aanvulling daarop hebben de endorelaties de volgende eigenschappen: "
                          English   ->
                            Str $ "Additionally, the endorelations come with the following properties: "]
            | length hMults>1 ]++
            [Table [] [AlignLeft,AlignCenter,AlignCenter,AlignCenter,AlignCenter,AlignCenter,AlignCenter] [0.0,0.0,0.0,0.0,0.0,0.0,0.0]
             [[case language flags of
                          Dutch   -> Plain [Str "relatie"] 
                          English -> Plain [Str "relation"]  ]
             ,[Plain [Str "Rfx"]]
             ,[Plain [Str "Irf"]]
             ,[Plain [Str "Trn"]]
             ,[Plain [Str "Sym"]]
             ,[Plain [Str "Asy"]]
             ,[Plain [Str "Prop"]]]
             [[[Plain [Math InlineMath (showMathDamb fSpec d)]] -- d is a declaration, and therefore typeable. So  showMathDamb fSpec d  exists.
              ,[Plain [Math InlineMath "\\surd" | isRfx d ]]
              ,[Plain [Math InlineMath "\\surd" | isIrf d ]]
              ,[Plain [Math InlineMath "\\surd" | isTrn d ]]
              ,[Plain [Math InlineMath "\\surd" | isSym d ]]
              ,[Plain [Math InlineMath "\\surd" | isAsy d ]]
              ,[Plain [Math InlineMath "\\surd" | isAsy d && isSym d ]]]
             | d<-hMults]
            | length hMults>0 ]
       where
        hMults  = [r | r@Rel{}<- mors fSpec, isEndo r]
    keyDocumentation
     = if null (keyDefs fSpec)
       then []
       else [ case language flags of
               Dutch   ->
                 Para  [Str "Er is ",Str preciesEen,Str " key: ",Str (texOnly_Id(name k)),Str "."]
               English ->
                 Para  [Str "There is but one key: ",Str (texOnly_Id(name k)),Str "." ]
            | length keyds==1, k<-keyds ]++
            [ case language flags of
               Dutch   ->
                 Para $ Str "De volgende keys bestaan: ": commaNLPandoc (Str "en") [Str $ texOnly_Id(name k) | k<-keyds]
               English ->
                 Para $ Str "The following keys exist: ": commaEngPandoc (Str "and") [Str $ texOnly_Id(name k) | k<-keyds]
            | length keyds>1 ]
       where
        keyds   = keyDefs fSpec -- all key definitions
-- The properties of various declations are documented in different tables.
-- First, we document the heterogeneous properties of all relations
-- Then, the endo-poperties are given, and finally
-- the process rules are documented.

  daAttributes :: PlugSQL -> [Block]
  daAttributes p
   = if length (tblfields p)<=1 then [] else
     [ case language flags of
               Dutch   ->
                 Para [ Str $ "De attributen van "++name p++" hebben de volgende multipliciteitsrestricties. "
                 ]
               English ->
                 Para [ Str $ "The attributes in "++name p++" have the following multiplicity constraints. "
                 ]
     ,Table [] [AlignLeft,AlignLeft,AlignCenter,AlignCenter] [0.0,0.0,0.0,0.0]
      ( case language flags of
          Dutch   ->
             [[Plain [Str "attribuut"]]
             ,[Plain [Str "type"]]
             ,[Plain [Str "verplicht"]]
             ,[Plain [Str "uniek"]]]
          English ->
             [[Plain [Str "attribute"]]
             ,[Plain [Str "type"]]
             ,[Plain [Str "mandatory"]]
             ,[Plain [Str "unique"]]] )
      [ if isProp (fldexpr fld) && fld/=head (tblfields p)
        then [ [Plain [Str (fldname fld)]]
             , [Plain [ Str "Bool"]]
             , [Plain [Math InlineMath "\\surd"]]
             , []
             ]
        else [ [Plain [if fld==head (tblfields p) || null ([Uni,Inj,Sur]>-multiplicities (fldexpr fld))
                       then Str  "key "
                       else Str (fldname fld)]]
             , [Plain [ (Str . latexEscShw.name.target.fldexpr) fld]]
             , [Plain [Math InlineMath "\\surd" | not (fldnull fld)]]
             , [Plain [Math InlineMath "\\surd" | flduniq fld]]
             ]
      | fld<-tblfields p  -- tail haalt het eerste veld, zijnde I[c], eruit omdat die niet in deze tabel thuishoort.
      ]
      
     ]
-- the endo-properties have already been reported in the general section of this chapter.
{-     where
--  voorgestelde multipliciteitenanalyse....
      clauses = nub [clause | Quad _ ccrs<-vquads fSpec, (_,shifts)<-cl_conjNF ccrs, clause<-shifts]
      is = nub [r | EUni fus<-clauses
                  , isIdent (EIsc [notCpl f | f<-fus, isPos f])
                  , f<-filter isNeg fus
                  , s<-strands f
                  , e<-[head s, flp (last s)]
                  , r<-mors e
                ]
      ts = nub [r | EUni fus<-clauses
                  , isIdent (EIsc [notCpl f | f<-fus, isNeg f])
                  , f<-filter isPos fus
                  , s<-strands f
                  , e<-[head s, flp (last s)]
                  , r<-mors e
                  ]
      strands (ECps fs) = [fs]
      strands _      = []    -- <--  we could maybe do better than this...
      tots = [d | t<-ts, inline t, d<-map makeDeclaration (mors t)]
      unis = [d | t<-is, inline t, d<-map makeDeclaration (mors t)]
      surs = [d | t<-ts, not (inline t), d<-map makeDeclaration (mors t)]
      injs = [d | t<-is, not (inline t), d<-map makeDeclaration (mors t)]
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
           English -> case [r | r<-invariants fSpec, null (mors r >- mors p)] of
                       []  -> []
                       [r] -> [ Para [ Str "Within this data set, the following integrity rule shall be true at all times. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic r)) ]
                                else if isTypeable (rrexp r)
                                     then Para [ Math DisplayMath $ showMathDamb fSpec r]
                                     else fatal 1635 ("Untypeable "++show r)
                              ]
                       rs  -> [ Para [ Str "Within this data set, the following integrity rules shall be true at all times. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic r)) ]] | r<-rs ]
                                else BulletList [ if isTypeable (rrexp r)
                                                  then [Para [Math DisplayMath $ showMathDamb fSpec r]]
                                                  else fatal 1642 ("Untypeable "++show r)
                                                | r<-rs ]
                              ]
           Dutch   -> case [r | r<-invariants fSpec, null (mors r >- mors p)] of
                       []  -> []
                       [r] -> [ Para [ Str "Binnen deze gegevensverzameling dient de volgende integriteitsregel te allen tijde waar te zijn. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic r)) ]
                                else if isTypeable (rrexp r)
                                     then Para [ Math DisplayMath $ showMathDamb fSpec r]
                                     else fatal 1652 ("Untypeable "++show r)
                              ]
                       rs  -> [ Para [ Str "Binnen deze gegevensverzameling dienen de volgende integriteitsregels te allen tijde waar te zijn. " ]
                              , if showPredExpr flags
                                then BulletList [ [Para [ Math DisplayMath (showLatex (toPredLogic r)) ]] | r<-rs ]
                                else BulletList [ if isTypeable (rrexp r)
                                                  then [Para [Math DisplayMath $ showMathDamb fSpec r]]
                                                  else fatal 1659 ("Untypeable "++show r)
                                                | r<-rs ]
                              ]
       plugKeydefs
        = case language flags of
           English -> case [k | k<-keyrules fSpec, null (mors k >- mors p)] of
                       []  -> []
                       [s] -> [ Para [ Str "This data set contains one key. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic s)) ]
                                else if isTypeable (rrexp s)
                                     then Para [ Math DisplayMath $ showMathDamb fSpec s]
                                     else fatal 1671 ("Untypeable "++show s)
                              ]
                       ss  -> [ Para [ Str "This data set contains the following keys. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic s)) ]] | s<-ss ]
                                else BulletList [ if isTypeable (rrexp s)
                                                  then [Para [Math DisplayMath $ showMathDamb fSpec s]]
                                                  else fatal 1678 ("Untypeable "++show s)
                                                | s<-ss ]
                              ]
           Dutch   -> case [k | k<-keyrules fSpec, null (mors k >- mors p)] of
                       []  -> []
                       [s] -> [ Para [ Str ("Deze gegevensverzameling genereert "++preciesEen++" key. ") ] 
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic s)) ]
                                else if isTypeable (rrexp s)
                                     then Para [ Math DisplayMath $ showMathDamb fSpec s]
                                     else fatal 1688 ("Untypeable "++show s)
                              ]
                       ss  -> [ Para [ Str "Deze gegevensverzameling genereert de volgende keys. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic s)) ]] | s<-ss ]
                                else BulletList [ if isTypeable (rrexp s)
                                                  then [Para [Math DisplayMath $ showMathDamb fSpec s]]
                                                  else fatal 1695 ("Untypeable "++show s)
                                                | s<-ss ]
                              ]
       plugSignals
        = case (language flags, [r | r<-vrules fSpec, isSignal r , null (mors r >- mors p)]) of
    --       English -> case [r | r<-vrules fSpec, isSignal r , null (mors r >- mors p)] of
            (_      , [])  -> []
            (English, [s]) -> [ Para [ Str "This data set generates one process rule. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic s)) ]
                                else if isTypeable (rrexp s)
                                     then Para [ Math DisplayMath $ showMathDamb fSpec s]
                                     else fatal 1707 ("Untypeable "++show s)
                              ] 
            (English, ss)  -> [  Para [ Str "This data set generates the following process rules. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic s)) ]] | s<-ss ]
                                else BulletList [ if isTypeable (rrexp s)
                                                  then [Para [Math DisplayMath $ showMathDamb fSpec s]]
                                                  else fatal 1714 ("Untypeable "++show s)
                                                | s<-ss ]
                              ]
            (Dutch  ,  [s]) -> [ Para [ Str ("Deze gegevensverzameling genereert "++preciesEen++" procesregel. ") ] 
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic s)) ]
                                else if isTypeable (rrexp s)
                                     then Para [ Math DisplayMath $ showMathDamb fSpec s]
                                     else fatal 1724 ("Untypeable "++show s)
                              ]
            (Dutch  ,  ss ) -> [ Para [ Str "Deze gegevensverzameling genereert de volgende procesregels. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic s)) ]] | s<-ss ]
                                else BulletList [ if isTypeable (rrexp s)
                                                  then [Para [Math DisplayMath $ showMathDamb fSpec s]]
                                                  else fatal 1731 ("Untypeable "++show s)
                                                | s<-ss ]
                              ]
       iRules
        = case language flags of
           English -> case irs of
                       []  -> []
                       [e] -> [ Para [ Str "The following rule defines the integrity of data within this data set. It must remain true at all times. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic e)) ]
                                else if isTypeable e
                                     then Para [ Math DisplayMath $ showMathDamb fSpec e]
                                     else fatal 1743 ("Untypeable "++show e)
                              ]
                       es  -> [ Para [ Str "The following rules define the integrity of data within this data set. They must remain true at all times. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic e)) ]] | e<-es ]
                                else BulletList [ if isTypeable e
                                                  then [Para [Math DisplayMath $ showMathDamb fSpec e]]
                                                  else fatal 1750 ("Untypeable "++show e)
                                                | e<-es ]
                              ]
           Dutch   -> case irs of
                       []  -> []
                       [e] -> [ Para [ Str "De volgende regel definieert de integriteit van gegevens binnen deze gegevensverzameling. Hij moet te allen tijde blijven gelden. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic e)) ]
                                else if isTypeable e
                                     then Para [ Math DisplayMath $ showMathDamb fSpec e]
                                     else fatal 1760 ("Untypeable "++show e)
                              ]
                       es  -> [ Para [ Str "De volgende regels definiëren de integriteit van gegevens binnen deze gegevensverzameling. Zij moeten te allen tijde blijven gelden. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic e)) ]] | e<-es ]
                                else BulletList [ if isTypeable e
                                                  then [Para [Math DisplayMath $ showMathDamb fSpec e]]
                                                  else fatal 1767 ("Untypeable "++show e)
                                                | e<-es ]
                              ]
          where irs = [EUni fs | Quad r ccrs<-vquads fSpec
                             , r_usr (cl_rule ccrs), isIdent r, source r `elem` pcpts
                             , (_,shifts)<-cl_conjNF ccrs
                             , EUni fs<-shifts
                             , let ns=[t | ECpl t<-fs], length ns==1, ERel nega<-ns
                             , r==nega
                             ]
                pcpts = case p of
                  ScalarSQL{} -> [cLkp p]
                  _           -> map fst (cLkpTbl p)

------------------------------------------------------------
chpECArules :: Int -> Fspc -> Options ->  [Block]
chpECArules lev fSpec flags
 = header ++ ecaIntro ++ ifcECA
 where
  header :: [Block]
  header = labeledHeader lev "chpECA" "ECA rules"
  ecaIntro :: [Block]
  ecaIntro
   = [ Plain $ case language flags of
       Dutch   -> [Str "Dit hoofdstuk bevat de ECA regels." ]
       English -> [Str "This chapter lists the ECA rules." ]
     ]
  ifcECA :: [Block]
  ifcECA
   = case language flags of
      Dutch   -> Para [ Str "ECA rules:",LineBreak, Str "   ",Str "tijdelijk ongedocumenteerd" ] : 
                 [ BlockQuote (toList (codeBlock
                      ( showECA fSpec "\n     " eca
-- Dit inschakelen          ++[LineBreak, Str "------ Derivation ----->"]
--  voor het bewijs         ++(showProof (showECA fSpec [LineBreak, Str ">     ") (proofPA (ecaAction (eca arg)))
--                          ++[LineBreak, Str "<------End Derivation --"]
                      ) ) )
                 | eca<-vEcas fSpec, not (isNop (ecaAction eca))]
      English -> Para [ Str "ECA rules:",LineBreak, Str "   ",Str "temporarily not documented" ] :
                 [ BlockQuote (toList (codeBlock
                    ( showECA fSpec "\n" eca )))
                 | eca<-vEcas fSpec, not (isNop (ecaAction eca))]

------------------------------------------------------------
interfaceChap :: Int -> Fspc -> Options -> Activity ->  ([Block],[Picture])
interfaceChap lev fSpec flags act
 = ( header ++ ifcIntro
      ++ (if genGraphics flags then txtKnowledgeGraph else [])
      -- ifcFieldTables
      ++ (if graphic then txtSwitchboard else [])
   , picKnowledgeGraph : [picSwitchboard | graphic]
   )
 where
  graphic :: Bool
  graphic = genGraphics flags && theme flags /= StudentTheme
  header :: [Block]
  header = labeledHeader lev ("chpIfc"++name act) (name act)
  ifcIntro :: [Block]
  ifcIntro
   = ( let expls = [expl |expl<-actXpls act, explLang expl==language flags] in  -- tells us why this interface exists
       if not (null expls) then explains2Blocks expls else
       [ Plain $ case language flags of
        Dutch   -> [Str "Waartoe activiteit ", Quoted SingleQuote [Str (name act)], Str" bestaat is niet gedocumenteerd." ]
        English -> [Str "For what purpose activity ", Quoted SingleQuote [Str (name act)], Str" exists remains undocumented." ]
       ]
     )++
     ifcAutoRules++
     (if genEcaDoc flags then ifcEcaRules else [])

{-
  ifcInsDelConcepts :: [Block]
  ifcInsDelConcepts
   = let ics = fsv_creating act>-fsv_deleting act
         dcs = fsv_deleting act>-fsv_creating act
         ucs = fsv_deleting act `isc` fsv_creating act
     in case language flags of
      Dutch -> [Plain [Space]]++
          if null ics && null dcs && null ucs then [Plain $ [Str "Deze interface maakt of verwijdert geen objecten langs geautomatiseerde weg."]] else
          if null ics && null dcs             then [Plain $ [Str "Om regels te handhaven, mogen instanties van "]++commaNLPandoc (Str "en") (map (Str . name) ucs)++[Str " door deze interface geautomatiseerd worden aangemaakt en verwijderd."]] else
          if null ics       &&       null ucs then [Plain $ [Str "Om regels te handhaven, mag deze interface instanties van "]++commaNLPandoc (Str "en") (map (Str . name) ucs)++[Str " geautomatiseerd verwijderen."]] else
          if             null dcs && null ucs then [Plain $ [Str "Om regels te handhaven, mogen instanties van "]++commaNLPandoc (Str "en") (map (Str . name) ucs)++[Str " geautomatiseerd worden aangemaakt door deze interface."]] else
          if                         null ucs then [Plain $ [Str "Instanties van "]++commaNLPandoc (Str "en") (map (Str . name) ucs)++[Str " mogen worden toegevoegd en instanties van "]++f dcs++[Str " mogen worden verwijderd door deze interface. Dat gebeurt geautomatiseerd en uitsluitend waar nodig om regels te handhaven."]] else
          if             null dcs             then [Plain $ [Str "Deze interface mag instanties van "]++commaNLPandoc (Str "en") (map (Str . name) ics)++[Str " creeren, terwijl instanties van "]++commaNLPandoc (Str "en") (map (Str . name) ucs)++[Str " ook verwijderd mogen worden. Alleen waar nodig mag dit plaatsvinden om regels te handhaven."]] else
          if null ics                         then [Plain $ [Str "Deze interface mag instanties van "]++commaNLPandoc (Str "en") (map (Str . name) ucs)++[Str " wijzigen, maar instanties van "]++commaNLPandoc (Str "en") (map (Str . name) dcs)++[Str " mogen alleen worden verwijderd. Dat mag slechts dan gebeuren wanneer dat nodig is voor het handhaven van regels."]] else
          [Plain $ [Str "Deze interface maakt nieuwe instanties van concept"]++f ics++[Str ". Hij mag instanties van concept"]++f dcs++[Str " verwijderen, terwijl instanties van "]++commaNLPandoc (Str "en") (map (Str . name) ucs)++[Str " zowel gemaakt als vernietigd mogen worden."]]
          where f [x] = [Space, Str (name x), Space]
                f xs  = [Str "en "]++commaNLPandoc (Str "en") (map (Str . name) xs)
      English -> [Plain [Space]]++
          if null ics && null dcs && null ucs then [Plain $ [Str "In this interface, no objects are made or removed automatically."]] else
          if null ics && null dcs             then [Plain $ [Str "In order to maintain rules, instances of "]++f ucs++[Str " may be created or deleted by this interface automatically."]] else
          if null ics       &&       null ucs then [Plain $ [Str "In order to maintain rules, instances of "]++f dcs++[Str " may be deleted automatically by this interface."]] else
          if             null dcs && null ucs then [Plain $ [Str "In order to maintain rules, instances of "]++f ics++[Str " may be automatically inserted by this interface."]] else
          if                         null ucs then [Plain $ [Str "Concept"]++f ics++[Str " may be inserted, and concept"]++f dcs++[Str " may be deleted by this interface. This happens only if necessary for maintaining rules."]] else
          if             null dcs             then [Plain $ [Str "By this interface, concept"]++f ucs++[Str " may be changed, but"]++f ics++[Str " may be created but not deleted. This happens only if necessary for maintaining rules."]] else
          if null ics                         then [Plain $ [Str "By this interface, concept"]++f ucs++[Str " may be changed, but"]++f dcs++[Str " may only be deleted. This happens only if necessary for maintaining rules."]] else
          [Plain $ [Str "This interface can create new instances of concept"]++f ics++[Str ". It may delete instances of concept"]++f dcs++[Str ", and instances of concept"]++f ucs++[Str " may be either created and removed. Such actions will take place only in order to maintain rules."]]
          where f [x] = [Space, Str (name x)]
                f xs  = [Str "s "]++commaEngPandoc (Str "and") (map (Str . name) xs)
-}

  ifcAutoRules :: [Block]
  ifcAutoRules
   = case language flags of
      Dutch ->   [Plain ([Str "Activiteit",Space, Quoted SingleQuote [(Str . name . actRule) act], Space,Str "moet door een gebruiker met rol "]++commaNLPandoc (Str "of") rols++[Str" worden uitgevoerd."] ++
                         case length auts of
                          0 -> []
                          1 -> [Space,Str "Daarbij wordt regel",Space]++auts++[Space,Str "gehandhaafd zonder interventie van de gebruiker."]
                          _ -> [Space,Str "Daarbij worden regels",Space]++commaNLPandoc (Str "en") auts++[Space,Str "gehandhaafd zonder interventie van de gebruiker."]
                 )]
      English -> [Plain ([Str "Activity",Space, Quoted SingleQuote [(Str . name . actRule) act], Space,Str "must be performed by a user with role "]++commaEngPandoc (Str "or") rols++[Str"."] ++
                         case length auts of
                          0 -> []
                          1 -> [Space,Str "During that activity, rule",Space]++auts++[Space,Str "will be maintained without intervention of a user."]
                          _ -> [Space,Str "During that activity, rules",Space]++commaEngPandoc (Str "and") auts++[Space,Str "will be maintained without intervention of a user."]
                 )]
     where
        auts = nub [ Quoted  SingleQuote [Str (name r)] | q<-actQuads act, let r=(cl_rule.qClauses) q, r_usr r]
        rols = nub [Str r | (r,rul)<-fRoleRuls fSpec, rul==actRule act]

  ifcEcaRules :: [Block]
  ifcEcaRules
   = ( case (language flags, actEcas act) of
        (Dutch,[])   -> [Plain [Str "Alle veranderingen die een gebruiker uitvoert zijn handmatig. Er is geen geautomatiseerde functionaliteit in deze activiteit."]]
        (English,[]) -> [Plain [Str "All changes a user makes are done by hand. There is no automated functionality in this activity."]]
        (Dutch,_)    -> [Plain [Str "De volgende tabel laat zien welke edit-acties welke functie aanroepen."]]
        (English,_)  -> [Plain [Str "The following table shows which edit actions invoke which function."]]
     )++
     if length (actEcas act)<1 then [] else
     [ Table [] [AlignLeft,AlignLeft,AlignLeft] [0.0,0.0,0.0]
       ( case language flags of
          Dutch   ->
              [[Plain [Str "actie"]]
              ,[Plain [Str "relatie"]]
              ,[Plain [Str "regel"]]]
          English   ->
              [[Plain [Str "action"]]
              ,[Plain [Str "relation"]]
              ,[Plain [Str "rule"]]] )
       [ [ [Plain [ (Str . show . eSrt.ecaTriggr) eca]]
         , [Plain [ (Str . show . eRel.ecaTriggr) eca]]
         , [Plain (shwEca eca)]
         ] 
       | eca<-actEcas act  -- tail haalt het eerste veld, zijnde I[c], eruit omdat die niet in deze tabel thuishoort.
       ]]
     where
      shwEca :: ECArule -> [Inline]
      shwEca eca
       | isBlk (ecaAction eca)
            = Str "error: rule ":
              commaEngPandoc (Str "and")
                       [ Quoted  SingleQuote [Str (name r)]
                       | (_,r)<-paMotiv (ecaAction eca)
                       ] 
       | isNop (ecaAction eca)
            = [ Str "no op"]
       | otherwise = [ Str "ECA rule ", Str ((show . ecaNum ) eca) ]
{-
  ifcFieldTables
   = if null (fsv_fields act) then [] else
     if length (fsv_fields act)==1
     then [ Para  $ [ case language flags of
                       Dutch   -> Str "In deze interface is het volgende veld zichtbaar: "
                       English -> Str "This interface has one field: "
                    ]
          , head [b | BulletList [bs]<-[flds], b<-bs]
          ]
     else [ Para  $ [ case language flags of
                       Dutch   -> Str "In deze interface zijn de volgende velden zichtbaar. "
                       English -> Str "This interface has the following fields. "
                    ]
          , flds
          ]
     where flds :: Block
           flds = BulletList [recur ((objctx.ifcObj.fsv_ifcdef) act) f | f<-fsv_fields act]
            where recur :: Expression -> Field -> [Block]
                  recur e f | null (fld_sub f) = fld e f
                            | otherwise        = fld e f ++
                                                 [ BulletList [recur (ECps [e,fld_expr f']) f' | f'<-fld_sub f] ]
           fld e f = [ Para [ Str (dealWithUnderscores (fld_name f)++if null cols then "" else "("++intercalate ", " cols++")") ]
                     , Para [ Str "display on start: ", Math InlineMath $ showMathDamb fSpec (conjNF e) ]
                     ] 
            where cols = ["lijst"         | fld_list    f]++
                         ["verplicht"     | fld_must    f]++
                         ["nieuw"         | fld_insAble f]++
                         ["verwijderbaar" | fld_delAble f]
                     
           dealWithUnderscores :: String -> String
           dealWithUnderscores x = 
                  case x of 
                    []     -> []
                    '_':cs -> "\\_" ++ dealWithUnderscores cs
                    c:cs   -> c : dealWithUnderscores cs
-}

  txtKnowledgeGraph :: [Block]
  txtKnowledgeGraph
   = (case language flags of                                     -- announce the knowledge graph
           Dutch   -> [Para [ Str "Figuur ", xrefReference (figlabel picKnowledgeGraph)
                            , Str " geeft de kennisgraaf weer voor deze interface."]]
           English -> [Para [ Str "Figure ", xrefReference (figlabel picKnowledgeGraph)
                            , Str " shows the knowledge graph of this interface."]]
     )
     ++ [Plain (xrefFigure1 picKnowledgeGraph)]                  -- draw the knowledge graph

  picKnowledgeGraph :: Picture
  picKnowledgeGraph
   = (makePicture flags fSpec Plain_CG act)  -- the Picture that represents this interface's knowledge graph
        {caption = case language flags of
                    Dutch   ->"Taaldiagram van "++name act
                    English ->"Language diagram of "++name act}
--     where
--      knGph = conceptualGraph fSpec flags Plain_CG act         -- the DotGraph String that represents this interface's knowledge graph
--      kn    = printDotGraph knGph                     -- the String that represents this interface's knowledge graph

  txtSwitchboard :: [Block]
  txtSwitchboard
   = (if name act==name (head (fActivities fSpec)) then switchboardIntro else [])++
     (case language flags of                                     -- announce the switchboard diagram
           Dutch   -> [Para [ Str "Figuur ", xrefReference (figlabel picSwitchboard)
                            , Str " geeft het schakelpaneel (switchboard diagram) weer voor deze interface."]]
           English -> [Para [ Str "Figure ", xrefReference (figlabel picSwitchboard)
                            , Str " shows the switchboard diagram of this interface."]]
     )
     ++ [Plain (xrefFigure1 picSwitchboard)]                     -- draw the switchboard

  picSwitchboard :: Picture
  picSwitchboard
   = (makePicture flags fSpec Plain_CG(switchboardAct fSpec act)) -- the Picture that represents this interface's knowledge graph
        {caption = case language flags of
                    Dutch   ->"Schakelpaneel van "++name act
                    English ->"Switchboard of "++name act}
--     where
--      sbGph = switchboardAct fSpec act                              -- the DotGraph String that represents this interface's knowledge graph
--      sb    = printDotGraph sbGph                                -- the String that represents this interface's knowledge graph

  switchboardIntro :: [Block]
  switchboardIntro
   = if not graphic then [] else
     [ Para $ case language flags of                             -- tells us for who this interface exists
        Dutch   -> [ Str "Iedere sectie in dit hoofdstuk beschrijft één activiteit. "
                   , Str "Tijdens het uitvoeren van een activiteit zal een gebruiker populatie invoegen of verwijderen in verschillende relaties. "
                   , Str "Hierdoor kunnen invarianten potentieel worden overtreden. "
                   , Str "(Een invariant is een bedrijfsregel die op ieder moment waar moet blijven.) "
                   , Str "De software die nodig is om invarianten waar te maken wordt automatisch gegenereerd. "
                   , Str "De structuur van deze software wordt geïllustreerd door een zogenaamd schakelpaneel (switchboard-diagram), "
                   , Str "waarvan u de eerste in figuur X aantreft. "
                   , Str "Elk switchboard diagram bestaat uit drie kolommen: "
                   , Str "Invariante regels staan in het midden en relaties staan aan de (linker en rechter) zijkanten. "
                   , Str "Een pijl ter linkerzijde wijst van een relatie die ge-edit wordt naar een regel die daardoor mogelijk overtreden wordt. "
                   , Str "Elke pijl ter rechterzijde van een regel representeert een edit-actie die nodig is om het waar-zijn ervan te herstellen. "
                   , Str "Deze pijl wijst naar de relatie waarin deze herstel-actie moet worden uitgevoerd. "
                   , Str "Een pijl gelabeled met '+' duidt op een insert event; een pijl met '-' op  "
                   , Str "Hierdoor onstaat een accuraat beeld op welke manier de activiteit alle invarianten handhaaft. "
                   ]
        English -> [ Str "Every section in this chapter describes one activity. "
                   , Str "While performing an activity, users will insert or delete population in various relations. "
                   , Str "This may potentially violate invariants. "
                   , Str "(An invariant is a business rule rules that must remain true at all times.) "
                   , Str "The software to maintain the truth of invariant rules is generated automatically. "
                   , Str "The structure of that software is illustrated by a so called switchboard diagram, "
                   , Str "the first of which you will find in figure X. "
                   , Str "Each switchboard diagram consists of three columns: "
                   , Str "Invariant rules are drawn in the middle, and relations occur on the (right and left hand) sides. "
                   , Str "An arrow on the left hand side points from a relation that may be edited to a rule that may be violated as a consequence thereof. "
                   , Str "Each arrow on the right hand side of a rule represents an edit action that is required to restore its truth. "
                   , Str "It points to the relation that is edited for that purpose. "
                   , Str "If that arrow is labeled '+', it involves an insert event; if labeled '-' it refers to a delete event. "
                   , Str "This yields an accurate perspective on the way in which invariants are maintained. "
                   ]
     ]

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
   case language flags of
      Dutch   -> [Para
                  [ Str "De specificatie van "
                  , Quoted  SingleQuote [Str (name fSpec)]
                  , Str " is geanalyseerd door middel van een functiepuntentelling"
                  , xrefCitation "IFPUG"
                  , Str ". "
                  , Str $ "Dit heeft geresulteerd in een geschat totaal van "++(show.nFpoints) fSpec++" functiepunten."
                  ]]
      English -> [Para
                  [ Str "The specification of "
                  , Quoted  SingleQuote [Str (name fSpec)]
                  , Str " has been analysed by counting function points"
                  , xrefCitation "IFPUG"
                  , Str ". "
                  , Str $ "This has resulted in an estimated total of "++(show.nFpoints) fSpec++" function points."
                  ]]
   

  fpa2Blocks :: [Block]
  fpa2Blocks
   = [ Table [] [AlignLeft,AlignLeft,AlignRight] [0.0,0.0,0.0]
                  ( case language flags of
                      Dutch   -> [ [Plain [Str "gegevensverzameling"]]
                                 , [Plain [Str "analyse"]]
                                 , [Plain [Str "FP"]]]
                      English -> [ [Plain [Str "data set"]]
                                 , [Plain [Str "analysis"]]
                                 , [Plain [Str "FP"]]]
                  )
                  [[[Plain [(Str . name)                 plug]]
                   ,[Plain [(Str . show . fpa)           plug]]
                   ,[Plain [(Str . show . fPoints . fpa) plug]]]
                  | plug<-plugInfos fSpec, fPoints (fpa plug)>0
                  ]
{- was:         [ Para $ 
                  [ Math InlineMath $ "\\begin{tabular}{|l|l|r|}\\hline \n" ++
                          intercalate "&" ["data set", "analysis", "points"] ++"\\\\\\hline\n"++
                          intercalate "\\\\\n" [ intercalate "&" [name plug, show (fpa plug), (show.fPoints.fpa) plug]
                                         | plug<-plugInfos fSpec
                                         , fPoints (fpa plug)>0] ++
                          "\\\\\\hline\\end{tabular}" ]
-}
                , Table [] [AlignLeft,AlignLeft,AlignRight] [0.0,0.0,0.0]
                  ( case language flags of
                     Dutch   ->
                         [ [Plain [Str "interface"]]
                         , [Plain [Str "analyse"]]
                         , [Plain [Str "FP"]]]
                     English ->
                         [ [Plain [Str "interface"]]
                         , [Plain [Str "analysis"]]
                         , [Plain [Str "FP"]]]
                  )
                  [ [ [Plain [(Str . name)                    act]]
                    , [Plain [(Str . show . actFPA)           act]]
                    , [Plain [(Str . show . fPoints . actFPA) act]]]
                  | act<-fActivities fSpec
                  ]
                ]            

------------------------------------------------------------
glossary :: Int -> Fspc -> Options ->  [Block]
glossary _ fSpec flags
 = if fspecFormat flags==FLatex
   then [ Para [RawInline "latex" "\\printglossary"] ]
   else [ Table [] [AlignLeft,AlignLeft,AlignLeft] [0.0,0.0,0.0]
          ( case language flags of
               Dutch   ->
                 [ [Plain [Str "term"]] , [Plain [Str "definitie"]] , [Plain [Str "bron"]]]
               English ->
                 [ [Plain [Str "term"]] , [Plain [Str "definition"]], [Plain [Str "source"]]]
          )
          [ [ [Plain [(Str . name)  cd]], [Plain [(Str . cddef) cd]], [Plain [(Str . cdref) cd]]]
          | cd<-conceptDefs fSpec, name cd `elem` map name (concs fSpec)
          ]]


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
explains2Blocks = concatMap explCont
