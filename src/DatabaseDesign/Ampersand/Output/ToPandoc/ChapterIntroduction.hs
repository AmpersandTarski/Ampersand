{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterIntroduction
where
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
import DatabaseDesign.Ampersand.Basics  
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Output.PandocAux
import Locale
import Data.Time.Format


chpIntroduction :: Int -> Fspc -> Options ->  [Block]
chpIntroduction lev fSpec flags = header ++ introContents (language flags)
    where 
        header = labeledHeader lev (xLabel Intro) (case language flags of
                                                     Dutch   ->  "Inleiding"   
                                                     English ->  "Introduction"
                                                  )
        contextPurposes = purposesDefinedIn fSpec (language flags) fSpec
        --TODO: different intro for theme flags == "student"
        date = formatTime defaultTimeLocale "%-d-%-m-%Y" (genTime flags)
        time = formatTime defaultTimeLocale "%H:%M:%S" (genTime flags)
        introContents Dutch = 
          ( if null contextPurposes
            then [ Para 
                        [ Str "Dit document"
                        , Note [Para [Str $ "Dit document is gegenereerd op "++date++" om "++time++", dmv. "++ampersandVersionStr++"."]]
                        , Str " definieert de functionaliteit van een informatiesysteem genaamd "
                        , Quoted  SingleQuote [Str (name fSpec)], Str ". "
                        , Str "Het definieert business-services in een systeem waarin mensen en applicaties samenwerken "
                        , Str "om afspraken na te leven. "
                        , Str "Een aantal van deze afspraken is gebruikt als functionele eis om de onderhavige functionele specificatie"
                        , Note [Para [Str "Het gebruik van geldende afspraken als functionele eis is een kenmerk van de Ampersand aanpak, die gebruikt is bij het samenstellen van dit document. "]]
                        , Str " samen te stellen. "
                        , Str "Deze eisen staan opgesomd in hoofdstuk ", xrefReference FunctionalRequirements, Str ", geordend op thema. "
                        ]]
            else concat [amPandoc (explMarkup p) | p<-contextPurposes] )++
          [ Para 
                [ Str "De diagnose in hoofdstuk ", xrefReference Diagnosis
                , Str " is bedoeld voor de auteurs om gebreken uit hun Ampersand model op te sporen. "
                ]
          , Para 
                [ Str "De conceptuele analyse in hoofdstuk ", xrefReference ConceptualAnalysis
                , Str " is bedoeld voor requirements engineers en architecten om de afspraken uit hoofdstuk "
                , xrefReference FunctionalRequirements, Str " te valideren en te formaliseren. "
                , Str "Tevens is het bedoeld voor testers om eenduidige testgevallen te kunnen bepalen. "
                , Str "De formalisatie in dit hoofdstuk maakt consistentie van de functionele specificatie bewijsbaar. "
                , Str "Ook garandeert het een eenduidige interpretatie van de eisen."
                ]
          , Para 
                [ Str "De hoofdstukken die dan volgen zijn bedoeld voor de bouwers van ", Quoted  SingleQuote [Str (name fSpec)], Str ". "
                , Str "De gegevensanalyse in hoofdstuk "
                , xrefReference DataAnalysis
                , Str " beschrijft de gegevensverzamelingen waarop ", Quoted  SingleQuote [Str (name fSpec)], Str " wordt gebouwd. "
                , Str "Elk volgend hoofdstuk definieert één business service. "
                , Str "Hierdoor kunnen bouwers zich concentreren op één service tegelijk. "
                , Str "Tezamen ondersteunen deze services alle afspraken uit hoofdstuk ", xrefReference FunctionalRequirements, Str ". "
                , Str "Door alle functionaliteit uitsluitend via deze services te ontsluiten waarborgt ", Quoted  SingleQuote [Str (name fSpec)]
                , Str " compliance ten aanzien van alle eisen uit hoofdstuk ", xrefReference FunctionalRequirements, Str " ."
                ]
         ]

        introContents English = 
         [ Para
                [ Str "This document"
                , Note [Para [Str $ "This document was generated at "++date++" on "++time++", using "++ampersandVersionStr++"."]]
                , Str " defines the functionality of an information system called "
                , Quoted  SingleQuote [Str (name fSpec)], Str ". "
                , Str "It defines business services in a system where people and applications work together "
                , Str "in order to fullfill their commitments. "
                , Str "A number of these rules have been used as functional requirement to assemble this functional specification"
                , Note [Para [Str "To use agreements as functional requirements characterizes the Ampersand approach, which has been used to produce this document. "]]
                , Str ". "
                , Str "Those rules are listed in chapter ", xrefReference FunctionalRequirements, Str ", ordered by theme. "
                ]
          , Para 
                [ Str "The diagnosis in chapter ", xrefReference Diagnosis
                , Str " is meant to help the authors identify shortcomings in their Ampersand script."
                ]
          , Para 
                [ Str "The conceptual analysis in chapter ", xrefReference ConceptualAnalysis
                , Str " is meant for requirements engineers and architects to validate and formalize the requirements from chapter "
                , xrefReference FunctionalRequirements, Str ". "
                , Str "It is also meant for testers to come up with correct test cases. "
                , Str "The formalization in this chapter makes consistency of the functional specification provable. "
                , Str "It also yields an unambiguous interpretation of all requirements."
                ]
          , Para 
                [ Str "Chapters that follow have the builders of ", Quoted  SingleQuote [Str (name fSpec)], Str " as their intended audience. "
                , Str "The data analysis in chapter "
                , xrefReference DataAnalysis
                , Str " describes the data sets upon which ", Quoted  SingleQuote [Str (name fSpec)], Str " is built. "
                , Str "Each subsequent chapter defines one business service. "
                , Str "This allows builders focus on a single service at a time. "
                , Str "Together, these services fulfill all commitments from chapter ", xrefReference FunctionalRequirements, Str ". "
                , Str "By disclosing all functionality exclusively through these services, ", Quoted  SingleQuote [Str (name fSpec)]
                , Str " ensures compliance to all rules from chapter ", xrefReference FunctionalRequirements, Str ". "
                ]]  
