{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterIntroduction
where
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
import Data.Time.Format

chpIntroduction :: Fspc -> Options -> Blocks
chpIntroduction fSpec flags =
      chptHeader (fsLang fSpec) Intro
   <> fromList purposesOfContext  -- the motivation(s) of this context 
   <> readingGuide                -- tells what can be expected in this document.
  where 
    readingGuide 
      = case fsLang fSpec of
          Dutch
            -> para ( text "Dit document"
                   <> (note.para.text) ("Dit document is gegenereerd op "++date++" om "++time++", dmv. "++ampersandVersionStr++".")
                   <> text " definieert de functionaliteit van een informatiesysteem. "
                   <> text "Concreet definieert dit document een database op platform onafhankelijke wijze, inclusief business-services. "
                   <> text "Dit gebeurt met behulp van bedrijfsregels"
                   <> (note.para.text) "Het ontwerpen met bedrijfsregels is een kenmerk van de Ampersand aanpak, die gebruikt is bij het samenstellen van dit document. "
                   <> text ", die voldoende precies worden geformuleerd om bedrijfsprocessen en een bijbehorend informatiesysteem eenduidig te kunnen specificeren."
                   <> (if SharedLang `elem` chaptersInDoc flags
                       then ( if canXRefer flags 
                              then text "Alle afspraken staan opgesomd in hoofdstuk "
                                <> xRefReference flags SharedLang
                              else text "Alle afspraken staan opgesomd in het hoofdstuk genaamd "
                                <> (doubleQuoted.chptTitle (fsLang fSpec)) SharedLang
                            ) <> text ", geordend op thema. "
                       else text "Deze afspraken zijn niet opgenomen in dit document."
                      )
                    )
            <> if Diagnosis `elem` chaptersInDoc flags
               then para ((if canXRefer flags 
                           then text "De diagnose in hoofdstuk "
                             <> xRefReference flags Diagnosis
                           else text "De diagnose in het hoofdstuk genaamd"
                             <> (doubleQuoted.chptTitle (fsLang fSpec)) Diagnosis
                          ) <> text " is bedoeld voor de auteurs om gebreken uit hun Ampersand model op te sporen. "
                         )
               else mempty
            <> if ConceptualAnalysis `elem` chaptersInDoc flags
               then para ( ( if canXRefer flags 
                             then text "De conceptuele analyse in hoofdstuk "
                               <> xRefReference flags ConceptualAnalysis
                             else text "De conceptuele analyse in het hoofdstuk genaamd"
                               <> (doubleQuoted.chptTitle (fsLang fSpec)) ConceptualAnalysis
                           ) <> text " is bedoeld voor requirements engineers en architecten om de gemaakte afspraken"
                             <> text " te valideren en te formaliseren. "
                             <> text "Tevens is het bedoeld voor testers om eenduidige testgevallen te kunnen bepalen. "
                             <> text "De formalisatie in dit hoofdstuk maakt consistentie van de functionele specificatie bewijsbaar. "
                             <> text "Ook garandeert het een eenduidige interpretatie van de afspraken."
                         )
               else mempty
            <> if DataAnalysis `elem` chaptersInDoc flags
               then para ( text "De hoofdstukken die dan volgen zijn bedoeld voor de bouwers van "
                        <> (singleQuoted.text.name) fSpec
                        <> text ". "
                        <> ( if canXRefer flags 
                             then text "De gegevensanalyse in hoofdstuk "
                               <> xRefReference flags DataAnalysis
                             else text "De gegevensanalyse in het hoofdstuk genaamd"
                               <> (doubleQuoted.chptTitle (fsLang fSpec)) DataAnalysis
                           )
                        <> text " beschrijft de gegevensverzamelingen waarop "
                        <> (singleQuoted.text.name) fSpec
                        <> text " wordt gebouwd. "
                        <> text "Elk volgend hoofdstuk definieert één business service. "
                        <> text "Hierdoor kunnen bouwers zich concentreren op één service tegelijk. "
                         )
                 <> para ( text "Tezamen ondersteunen deze services alle geldende afspraken. "
                        <> text "Door alle functionaliteit uitsluitend via deze services te ontsluiten waarborgt "
                        <> (singleQuoted.text.name) fSpec
                        <> text " compliance ten aanzien van alle gestelde afspraken. "
                         )
               else mempty


          English
            -> para ( text "This document"
                   <> (note.para.text) ("This document was generated at "++date++" on "++time++", using "++ampersandVersionStr++".")
                   <> text " defines the functionality of an information system. "
                   <> text "It defines a database in a platform-independent way, together with business services."
                   <> text "The design was made by formulating business rules"
                   <> (note.para.text) "Rule based design characterizes the Ampersand approach, which has been used to produce this document. "
                   <> text " precisely enough that the business process and the information system can be specified from them."
                   <> (if SharedLang `elem` chaptersInDoc flags
                       then ( if canXRefer flags 
                              then text "The business rules are listed in chapter "
                                <> xRefReference flags SharedLang
                                <> text ", ordered by theme. "
                              else text "The business rules are listed in the chapter named "
                                <> (doubleQuoted.chptTitle (fsLang fSpec)) SharedLang
                            ) <> text ", ordered by theme. " 
                       else text "The business rules are not included in this document."
                      )
                    ) 
             <> if Diagnosis `elem` chaptersInDoc flags
               then para ((if canXRefer flags 
                           then text "The diagnosis in chapter "
                             <> xRefReference flags Diagnosis
                           else text "The diagnosis in the chapter named "
                             <> (doubleQuoted.chptTitle (fsLang fSpec)) Diagnosis
                          ) <> text " is meant to help the authors identify shortcomings in their Ampersand script."
                         )
               else mempty
            <> if ConceptualAnalysis `elem` chaptersInDoc flags
               then para ( ( if canXRefer flags 
                             then text "The conceptual analysis in chapter "
                               <> xRefReference flags ConceptualAnalysis
                             else text "The conceptual analysis in the chapter named "
                               <> (doubleQuoted.chptTitle (fsLang fSpec)) ConceptualAnalysis
                           ) <> text " is meant for requirements engineers and architects to validate and formalize the requirements. "
                             <> text "It is also meant for testers to come up with correct test cases. "
                             <> text "The formalization in this chapter makes consistency of the functional specification provable. "
                             <> text "It also yields an unambiguous interpretation of all requirements."
                         )
               else mempty
            <> if DataAnalysis `elem` chaptersInDoc flags
               then para ( text "Chapters that follow have the builders of "
                        <> (singleQuoted.text.name) fSpec
                        <> text " as their intended audience. "
                        <> ( if canXRefer flags 
                             then text "The data analysis in chapter "
                               <> xRefReference flags DataAnalysis
                             else text "The data analysis in the chapter named "
                               <> (doubleQuoted.chptTitle (fsLang fSpec)) DataAnalysis
                           )
                        <> text " describes the data sets upon which "
                        <> (singleQuoted.text.name) fSpec
                        <> text " is built. "
                        <> text "Each subsequent chapter defines one business service. "
                        <> text "This allows builders to focus on a single service at a time. "
                         )
                 <> para ( text "Together, these services fulfill all commitments. "
                        <> text "By disclosing all functionality exclusively through these services, "
                        <> (singleQuoted.text.name) fSpec
                        <> text " ensures compliance to all rules aggreed upon."
                         )
               else mempty
      
    date = formatTime (lclForLang (fsLang fSpec)) "%-d-%-m-%Y" (genTime flags)
    time = formatTime (lclForLang (fsLang fSpec)) "%H:%M:%S" (genTime flags)
 
    purposesOfContext = concat [amPandoc (explMarkup p) | p<-purposesDefinedIn fSpec (fsLang fSpec) fSpec]
    
 