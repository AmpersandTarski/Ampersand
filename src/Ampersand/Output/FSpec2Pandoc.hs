{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.Output.FSpec2Pandoc (fSpec2Pandoc)
where
import Ampersand.Output.ToPandoc.SharedAmongChapters
import Ampersand.Output.ToPandoc.ChapterInterfaces            (chpInterfacesBlocks)
import Ampersand.Output.ToPandoc.ChapterIntroduction          (chpIntroduction)
import Ampersand.Output.ToPandoc.ChapterNatLangReqs           (chpNatLangReqs)
import Ampersand.Output.ToPandoc.ChapterDiagnosis             (chpDiagnosis)
import Ampersand.Output.ToPandoc.ChapterConceptualAnalysis    (chpConceptualAnalysis)
import Ampersand.Output.ToPandoc.ChapterProcessAnalysis       (chpProcessAnalysis)
import Ampersand.Output.ToPandoc.ChapterDataAnalysis          (chpDataAnalysis)
import Ampersand.Output.ToPandoc.ChapterSoftwareMetrics       (fpAnalysis)
import Ampersand.Output.ToPandoc.ChapterFunctionPointAnalysis (chpFunctionPointAnalysis)
import Ampersand.Output.ToPandoc.ChapterGlossary              (chpGlossary)
import Data.Time.Format (formatTime)
import Data.List (nub)
import Text.Pandoc.CrossRef

--import Debug.Trace
--DESCR ->
--The functional design document starts with an introduction
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
--The following chapters each present one INTERFACE
--The specification end with a glossary.

--TODO: Invent a syntax for meta information that is included in the source file...

--The following general requirements apply to the functional design document:
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

fSpec2Pandoc :: FSpec -> (Pandoc, [Picture])
fSpec2Pandoc fSpec = (thePandoc,thePictures)
  where
    -- shorthand for easy localizing    
    l :: LocalizedStr -> String
    l = localize (fsLang fSpec)
    
    wrap :: Pandoc -> Pandoc
    wrap (Pandoc m bs) = Pandoc m $ runCrossRef m' Nothing crossRefBlocks bs 
      where 
        m' =   figureTitle ( (str.l) (NL "Figuur" ,EN "Figure"))
            <> tableTitle  ( (str.l) (NL "Tabel"  ,EN "Table" ))
            <> figPrefix [str "fig.", str "figs."]
            <> eqnPrefix [(str.l) (NL "relatie",EN "relation")
                         ,(str.l) (NL "relaties", EN "relations")]
            <> tblPrefix [str "tbl.", str "tbls."]
            <> lstPrefix [(str.l) (NL "afspraak",EN "agreement")
                         ,(str.l) (NL "afspraken", EN "agreements")]
            <> secPrefix [(str.l) (NL "hoofdstuk",EN "chapter")
                         ,(str.l) (NL "hoofdstukken", EN "chapters")]
            <> cref True
            <> chapters True

    thePandoc = wrap .
        setTitle
           (case metaValues "title" fSpec of
                [] -> text (case (fsLang fSpec, diagnosisOnly (getOpts fSpec)) of
                                 (Dutch  , False) -> "Functioneel Ontwerp van "
                                 (English, False) -> "Functional Design of "
                                 (Dutch  ,  True) -> "Diagnose van "
                                 (English,  True) -> "Diagnosis of "
                           ) <> (singleQuoted.text.name) fSpec
                titles -> (text.concat.nub) titles --reduce doubles, for when multiple script files are included, this could cause titles to be mentioned several times.
           )
        
      . setAuthors ( 
           case metaValues "authors" fSpec of
             [] -> case fsLang fSpec of
                     Dutch   -> [text "Specificeer auteurs in Ampersand met: META \"authors\" \"<auteursnamen>\""]
                     English -> [text "Specify authors in Ampersand with: META \"authors\" \"<author names>\""]
             xs -> map text $ nub xs  --reduce doubles, for when multiple script files are included, this could cause authors to be mentioned several times.
           ++  [ subscript . text $ "(Generated with "++ampersandVersionStr++")" | development (getOpts fSpec) ]

        )
      . setDate (text (formatTime (lclForLang (fsLang fSpec)) "%-d %B %Y" (genTime (getOpts fSpec))))
      . doc . mconcat $ blocksByChapter
    
    thePictures = concat picturesByChapter
    blocksByChapter :: [Blocks]
    picturesByChapter :: [[Picture]]
    (blocksByChapter, picturesByChapter) = unzip [fspec2Blocks chp | chp<-chaptersInDoc (getOpts fSpec)]

    fspec2Blocks :: Chapter -> (Blocks, [Picture])
    fspec2Blocks Intro                 = (chpIntroduction           fSpec, [])
    fspec2Blocks SharedLang            = (chpNatLangReqs          0 fSpec, [])
    fspec2Blocks Diagnosis             = chpDiagnosis               fSpec
    fspec2Blocks ConceptualAnalysis    = chpConceptualAnalysis    0 fSpec
    fspec2Blocks ProcessAnalysis       = (chpProcessAnalysis        fSpec, [])
    fspec2Blocks DataAnalysis          = chpDataAnalysis            fSpec
    fspec2Blocks SoftwareMetrics       = (fpAnalysis                fSpec, [])
    fspec2Blocks Interfaces            = (chpInterfacesBlocks       fSpec, [])
    fspec2Blocks FunctionPointAnalysis = (chpFunctionPointAnalysis  fSpec, [])
    fspec2Blocks Glossary              = (chpGlossary             0 fSpec, [])

