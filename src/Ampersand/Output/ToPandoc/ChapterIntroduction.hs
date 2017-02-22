{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.Output.ToPandoc.ChapterIntroduction
where
import Ampersand.Output.ToPandoc.SharedAmongChapters
import Data.Time.Format

chpIntroduction :: FSpec -> Blocks
chpIntroduction fSpec =
      xDefBlck fSpec Intro
   <> fromList purposesOfContext  -- the motivation(s) of this context
   <> readingGuide                -- tells what can be expected in this document.
  where
    readingGuide
      = case fsLang fSpec of
          Dutch
            -> para ( text "Dit document"
                   <> (note.para.text) ("Dit document is gegenereerd op "++date++" om "++time++", dmv. "++ampersandVersionStr++".")
                   <> text " definieert de functionaliteit van een informatiesysteem genaamd "
                   <> (singleQuoted.text.name) fSpec
                   <> text ". "
                   <> text "Het definieert de database en de business-services van " <> (text.name) fSpec <> text " door middel van bedrijfsregels"
                   <> (note.para.text) "Het ontwerpen met bedrijfsregels is een kenmerk van de Ampersand aanpak, die gebruikt is bij het samenstellen van dit document. "
                   <> text ". "
                   <> if SharedLang `elem` chaptersInDoc (getOpts fSpec)
                      then text "Deze afspraken staan opgesomd in "
                        <> xRef SharedLang
                        <> text ", geordend op thema. "
                      else text "Deze afspraken zijn niet opgenomen in dit document."
                    )
            <> if Diagnosis `elem` chaptersInDoc (getOpts fSpec)
               then para (   text "De diagnose in " 
                          <> xRef Diagnosis
                          <> text " is bedoeld voor de auteurs om gebreken uit hun Ampersand model op te sporen. "
                         )
               else mempty
            <> if ConceptualAnalysis `elem` chaptersInDoc (getOpts fSpec)
               then para (   text "De conceptuele analyse in "
                          <> xRef ConceptualAnalysis
                          <> text " is bedoeld voor requirements engineers en architecten om de gemaakte afspraken"
                          <> text " te valideren en te formaliseren. "
                          <> text "Tevens is het bedoeld voor testers om eenduidige testgevallen te kunnen bepalen. "
                          <> text "De formalisatie in dit hoofdstuk maakt consistentie van de functionele specificatie bewijsbaar. "
                          <> text "Ook garandeert het een eenduidige interpretatie van de afspraken."
                         )
               else mempty
            <> if DataAnalysis `elem` chaptersInDoc (getOpts fSpec)
               then para ( text "De hoofdstukken die dan volgen zijn bedoeld voor de bouwers van "
                        <> (singleQuoted.text.name) fSpec
                        <> text ". "
                        <> text "De gegevensanalyse in "
                        <> xRef DataAnalysis
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
                   <> text " defines the functionality of an information system called "
                   <> (singleQuoted.text.name) fSpec
                   <> text ". "
                   <> text "It defines the database and the business services of " <> (text.name) fSpec <> text " by means of business rules"
                   <> (note.para.text) "Rule based design characterizes the Ampersand approach, which has been used to produce this document. "
                   <> text ". "
                   <> if SharedLang `elem` chaptersInDoc (getOpts fSpec)
                      then text "Those rules are listed in "
                        <> xRef SharedLang
                        <> text ", ordered by theme. "
                      else text "Those rules are not included in this document."
                    )
             <> if Diagnosis `elem` chaptersInDoc (getOpts fSpec)
               then para (  text "The diagnosis in "
                         <> xRef Diagnosis
                         <> text " is meant to help the authors identify shortcomings in their Ampersand script."
                         )
               else mempty
            <> if ConceptualAnalysis `elem` chaptersInDoc (getOpts fSpec)
               then para (  text "The conceptual analysis in "
                         <> xRef ConceptualAnalysis
                         <> text " is meant for requirements engineers and architects to validate and formalize the requirements. "
                         <> text "It is also meant for testers to come up with correct test cases. "
                         <> text "The formalization in this chapter makes consistency of the functional design provable. "
                         <> text "It also yields an unambiguous interpretation of all requirements."
                         )
               else mempty
            <> if DataAnalysis `elem` chaptersInDoc (getOpts fSpec)
               then para ( text "Chapters that follow have the builders of "
                        <> (singleQuoted.text.name) fSpec
                        <> text " as their intended audience. "
                        <> text "The data analysis in "
                        <> xRef DataAnalysis
                        <> text " describes the data sets upon which "
                        <> (singleQuoted.text.name) fSpec
                        <> text " is built. "
                        <> text "Each subsequent chapter defines one business service. "
                        <> text "This allows builders to focus on a single service at a time. "
                         )
                 <> para ( text "Together, these services fulfill all commitments. "
                        <> text "By disclosing all functionality exclusively through these services, "
                        <> (singleQuoted.text.name) fSpec
                        <> text " ensures compliance to all rules agreed upon."
                         )
               else mempty

    date = formatTime (lclForLang (fsLang fSpec)) "%-d-%-m-%Y" (genTime (getOpts fSpec))
    time = formatTime (lclForLang (fsLang fSpec)) "%H:%M:%S" (genTime (getOpts fSpec))

    purposesOfContext = concat [amPandoc (explMarkup p) | p<-purposesDefinedIn fSpec (fsLang fSpec) fSpec]
