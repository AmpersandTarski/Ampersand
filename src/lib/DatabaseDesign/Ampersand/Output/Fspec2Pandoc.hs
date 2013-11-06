{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.Fspec2Pandoc (fSpec2Pandoc)
where
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterInterfaces         (chpInterfacesBlocks, chpInterfacesPics)
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterIntroduction       (chpIntroduction)
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterNatLangReqs        (chpNatLangReqs)
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterDiagnosis          (chpDiagnosis)
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterConceptualAnalysis (chpConceptualAnalysis)
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterProcessAnalysis    (chpProcessAnalysis)
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterECArules           (chpECArules)
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterDataAnalysis       (chpDataAnalysis)
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterSoftwareMetrics    (fpAnalysis)
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterGlossary           (chpGlossary)
import Data.Time.Format (formatTime)
import Data.List (nub)
--import Debug.Trace
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
fSpec2Pandoc fSpec flags = ( myDoc , concat picturesByChapter )
  where 
    myDoc = 
      ( (setTitle  
           (text
             (case (language flags, diagnosisOnly flags) of
               (Dutch  , False) -> "Functionele Specificatie van "
               (English, False) -> "Functional Specification of "
               (Dutch  ,  True) -> "Diagnose van "
               (English,  True) -> "Diagnosis of "
             )
            <>
            (singleQuoted.text.name) fSpec
           )
        )
      . (setAuthors (case metaValues "authors" fSpec of
                [] -> case language flags of
                        Dutch   -> [text "Specificeer auteurs in ADL met: META \"authors\" \"<auteursnamen>\""]
                        English -> [text "Specify authors in ADL with: META \"authors\" \"<author names>\""]
                xs -> map text (nub xs))  --reduce doubles, for when multiple script files are included, this could cause authors to be mentioned several times.
        )
      . (setDate (text (formatTime (lclForLang flags) "%-d %B %Y" (genTime flags))))
      ) 
      (doc (foldr (<>) mempty docContents))
    docContents :: [Blocks]
    picturesByChapter :: [[Picture]]
    (docContents, picturesByChapter) = unzip [fspec2Blocks chp | chp<-chaptersInDoc flags]

    fspec2Blocks :: Chapter -> (Blocks, [Picture])
    fspec2Blocks Intro              = (chpIntroduction        fSpec flags, [])
    fspec2Blocks SharedLang         = (chpNatLangReqs       0 fSpec flags, [])
    fspec2Blocks Diagnosis          = chpDiagnosis            fSpec flags
    fspec2Blocks ConceptualAnalysis = chpConceptualAnalysis 0 fSpec flags
    fspec2Blocks ProcessAnalysis    = chpProcessAnalysis    0 fSpec flags
    fspec2Blocks DataAnalysis       = chpDataAnalysis         fSpec flags 
    fspec2Blocks SoftwareMetrics    = (fpAnalysis             fSpec flags, [])
    fspec2Blocks EcaRules           = (chpECArules            fSpec flags, [])
    fspec2Blocks Interfaces         = (chpInterfacesBlocks  0 fSpec flags, chpInterfacesPics fSpec flags)
    fspec2Blocks Glossary           = (chpGlossary          0 fSpec flags, [])
    

