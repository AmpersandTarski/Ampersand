{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.Fspec2Pandoc (fSpec2Pandoc)--,laTeXtemplate)
where
import DatabaseDesign.Ampersand.Basics  
import DatabaseDesign.Ampersand.Fspec
import Text.Pandoc.Builder
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterConceptualAnalysis
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterDataAnalysis
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterDiagnosis
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterECArules
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterGlossary
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterInterfaces
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterIntroduction
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterNatLangReqs
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterProcessAnalysis
import DatabaseDesign.Ampersand.Output.ToPandoc.ChapterSoftwareMetrics
import Data.Time.Format
import qualified Data.Sequence as Seq
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

--TODO [Picture] should be separated from here. Now it is too much entangled, which makes it too complex (and hence errorprone). 
fSpec2Pandoc :: Fspc -> Options -> (Pandoc, [Picture])
fSpec2Pandoc fSpec flags = ( myDoc , pictures )
  where 
    myDoc = 
      ( (setTitle  
           (str
             (case (language flags, diagnosisOnly flags) of
               (Dutch  , False) -> "Functionele Specificatie van "
               (English, False) -> "Functional Specification of "
               (Dutch  ,  True) -> "Diagnose van "
               (English,  True) -> "Diagnosis of "
             )
            <>
            (singleQuoted.str.name) fSpec
           )
        )
      . (setAuthors (case metaValues "authors" fSpec of
                [] -> case language flags of
                        Dutch   -> [text "Specificeer auteurs in ADL met: META \"authors\" \"<auteursnamen>\""]
                        English -> [text "Specify authors in ADL with: META \"authors\" \"<author names>\""]
                xs -> map text xs)
        )
      . (setDate (text (formatTime (lclForLang flags) "%-d %B %Y" (genTime flags))))
      ) 
      (doc docContents)
    
     
    -- | The following code controls the structure of the document.
    docContents :: Blocks
    docContents
     | diagnosisOnly flags         = fromList diagTxt
     | studentversion              =
         fromList (chpIntroduction  level fSpec flags) <>
         fromList (chpNatLangReqs   level fSpec flags) <>
         fromList caTxt                                <>
         fromList daTxt                                <>
         fromList (glossary level fSpec flags)
     | otherwise                   =
         fromList (chpIntroduction  level fSpec flags) <> -- this chapter gives a general introduction. No text from the script is used other than the name of the context.
         fromList (chpNatLangReqs   level fSpec flags) <> -- this chapter gives an account of this context in natural language.
                                                          --   It sums up all requirements and explains their purpose. This is intended for stakeholders without
                                                          --   any skills in formal specification or information systems modeling.
         (if noDiagnosis flags 
          then Blocks Seq.empty
          else fromList diagTxt)                       <> -- This chapter is meant for the author. It points to places in the text that might need work.
         fromList caTxt                                <> -- This chapter is the conceptual analysis. It is meant for the design team to verify whether the natural language phrases and their formal counterparts match.
         (if noProcesses fSpec 
          then Blocks Seq.empty 
          else fromList paTxt)                         <> -- This chapter discusses the processes and patterns in this context.
         fromList (fpAnalysis level fSpec flags)       <> -- This chapter does a function point analysis on the specification.
         fromList daTxt                                <> -- This chapter provides a data analysis together with a data model.
                                                          --   It is meant for implementors who must build the system.
         fromList actsTxt                              <>
         (if genEcaDoc flags 
          then fromList (chpECArules level fSpec flags)   -- This chapter reports on the ECA rules generated in this system.
          else Blocks Seq.empty)                       <>
         fromList (glossary level fSpec flags)            -- At the end, a glossary is generated.
         
    pictures 
     | diagnosisOnly flags         = diagPics
     | studentversion              = daPics++caPics
     | otherwise                   = daPics++caPics++diagPics++paPics++actsPics 
    (caTxt  ,caPics)   = chpConceptualAnalysis      level fSpec flags
    (diagTxt,diagPics) = chpDiagnosis               level fSpec flags
    (paTxt  ,paPics)   = chpProcessAnalysis         level fSpec flags
    (daTxt  ,daPics)   = chpDataAnalysis            level fSpec flags
    (actsTxt,actsPics) = let acts=[interfaceChap level fSpec flags act | act <-fActivities fSpec] in (concat$map fst acts,concat$map snd acts)
    studentversion = theme flags == StudentTheme
    level = 0 --1=chapter, 2=section, 3=subsection, 4=subsubsection, _=plain text
