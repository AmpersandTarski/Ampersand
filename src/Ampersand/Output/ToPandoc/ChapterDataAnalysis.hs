{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.ToPandoc.ChapterDataAnalysis (chpDataAnalysis) where

import Ampersand.ADL1 hiding (Association)
import Ampersand.Output.ToPandoc.SharedAmongChapters hiding (Association)
import Ampersand.FSpec.Crud
import Ampersand.Graphic.ClassDiagram --(Class(..),CdAttribute(..))
import Ampersand.Graphic.Fspec2ClassDiagrams
import Ampersand.Output.PredLogic
import Data.Char
import Data.List
import Data.Function (on)
import qualified Text.Pandoc.Builder

------------------------------------------------------------
--DESCR -> the data analysis contains a section for each class diagram in the fSpec
--         the class diagram and multiplicity rules are printed
chpDataAnalysis :: FSpec -> (Blocks,[Picture])
chpDataAnalysis fSpec = (theBlocks, thePictures)
 where
   -- shorthand for easy localizing    
  l :: LocalizedStr -> String
  l = localize (fsLang fSpec)
  sectionLevel = 2
 
  theBlocks
    =  chptHeader (fsLang fSpec) DataAnalysis  -- The header
    <> (case fsLang fSpec of
             Dutch   -> para ( "Dit hoofdstuk bevat het resultaat van de gegevensanalyse. "
                            <> "De opbouw is als volgt:"
                             )
                     <> para (  "We beginnen met het classificatiemodel, gevolgd door "
                            <>  "een overzicht van alle relaties, die samen de basis vormen van de rest van deze analyse. "
                            <>  "Ten slotte volgen achtereenvolgend het logische- en technische gegevensmodel."
                             )
             English -> para (  "This chapter contains the result of the data analysis. "
                            <>  "It is structured as follows:"
                             )
                     <> para (  "We start with the classification model, followed by "
                            <>  "a list of all relations, that are the foundation of the rest of the analysis. "
                            <>  "Finally, the logical and technical data model are discussed."
                             )
       )
    <>  if null (classes $ clAnalysis fSpec) 
        then mempty
        else 
          (   header sectionLevel
                  (text.l $ (NL "Classificaties", EN "Classifications")
                  )
           <> para (case fsLang fSpec of
                     Dutch   ->  "Een aantal concepten zit in een classificatiestructuur. "
                              <> (if canXRefer (getOpts fSpec)
                                  then  "Deze is in figuur " <> xRefReference (getOpts fSpec) classificationPicture <> "weergegeven."
                                  else "Deze is in onderstaand figuur weergegeven."
                                 )
                     English -> "A number of concepts is organized in a classification structure. "
                              <> (if canXRefer (getOpts fSpec)
                                  then "This is shown in figure " <> xRefReference (getOpts fSpec) classificationPicture <> "."
                                  else "This is shown in the figure below."
                                 )
                   )
                <> para (showImage (getOpts fSpec) classificationPicture)
           )

    <> daRulesSection
    <> logicalDataModelBlocks
    <> technicalDataModelBlocks
    <> crudMatrixSection 
  thePictures
    =  [classificationPicture, logicalDataModelPicture, technicalDataModelPicture]
  classificationPicture = makePicture fSpec PTClassDiagram

      

  logicalDataModelBlocks =
         header sectionLevel
                    (case fsLang fSpec of
                      Dutch   -> text "Logisch gegevensmodel"
                      English -> text "Logical data model"
                    )
      <> para (case fsLang fSpec of
                 Dutch   -> text "De afspraken zijn vertaald naar een gegevensmodel. "
                           <> ( if canXRefer (getOpts fSpec)
                                then text "Dit gegevensmodel is in figuur " <> xRefReference (getOpts fSpec) logicalDataModelPicture <> text " weergegeven."
                                else text "Dit gegevensmodel is in onderstaand figuur weergegeven. "
                              )
                 English -> text "The functional requirements have been translated into a data model. "
                           <> ( if canXRefer (getOpts fSpec)
                                then text "This model is shown by figure " <> xRefReference (getOpts fSpec) logicalDataModelPicture <> text "."
                                else text "This model is shown by the figure below. "
                              )
              )
       <> para (showImage (getOpts fSpec) logicalDataModelPicture)
       <> let nrOfClasses = length (classes oocd)
          in case fsLang fSpec of
               Dutch   -> para (case nrOfClasses of
                                  0 -> text "Er zijn geen gegevensverzamelingen."
                                  1 -> text "Er is één gegevensverzameling, die in de volgende paragraaf in detail is beschreven:"
                                  _ -> text ("Er zijn "++count Dutch nrOfClasses "gegevensverzameling"++". ")
                                    <> text "De details van elk van deze gegevensverzameling worden, op alfabetische volgorde, in de twee nu volgende tabellen beschreven:"
                               )
               English -> para (case nrOfClasses of
                                  0 -> text "There are no entity types."
                                  1 -> text "There is only one entity type:"
                                  _ -> text ("There are "++count English nrOfClasses "entity type" ++".")
                                    <> text "The details of each entity type are described (in alphabetical order) in the following two tables:"
                               )
       <> conceptTable True
       <> conceptTable False
       <> mconcat (map detailsOfClass (sortBy (compare `on` name) (classes oocd)))

  logicalDataModelPicture = makePicture fSpec PTLogicalDM

  oocd :: ClassDiag
  oocd = cdAnalysis fSpec

  conceptTable :: Bool    -- this bool is introduced to split the table into two separate tables. The first table contains
                          -- the concepts that have their own table in the logical data model. The second table contains
                          -- all other concepts.  
               -> Blocks
  conceptTable keys = 
    table (if keys
           then text.l $ (NL "Logische gegevensverzamelingen"
                         ,EN "Logical entity types")
           else text.l $ (NL "Overige attributen"
                         ,EN "Other attributes"
                         )
          )
         [(AlignLeft,1/6),(AlignLeft,4/6),(AlignLeft,1/6)]
         [ (plain.text.l) (NL "Concept"       , EN "Concept")
         , (plain.text.l) (NL "Betekenis"     , EN "Meaning")
         , (plain.text.l) (NL "Type"          , EN "Type") 
         ] 
         [ [ (plain.text.name) c
           ,   meaningOf c
            <> fromList (maybe mempty (concatMap $ amPandoc . explMarkup) $ purposeOf fSpec (fsLang fSpec) c)
           , mempty
           ]
         | c <- sortBy (compare `on` name) . filter keyFilter . delete ONE $ concs fSpec
         ]
     where
       keyFilter :: A_Concept -> Bool
       keyFilter cpt = (    keys &&      isKey  cpt)
                     ||(not keys && (not.isKey) cpt)
       isKey :: A_Concept -> Bool
       isKey cpt = cpt `elem` ooCpts oocd
       meaningOf :: A_Concept -> Blocks
       meaningOf = mconcat . map (fromList . string2Blocks ReST . cddef) . concDefs fSpec 
       
  detailsOfClass :: Class -> Blocks
  detailsOfClass cl =
           header (sectionLevel+1) 
                  ((text.l) (NL "Gegevensverzameling: ", EN "Entity type: ") <> (emph.strong.text.name) cl)
        <> case clcpt cl of
             Nothing -> mempty
             Just cpt -> purposes2Blocks (getOpts fSpec) (purposesDefinedIn fSpec (fsLang fSpec) cpt)
        <> (para . text . l) ( NL "Deze gegevensverzameling bevat de volgende attributen: "
                             , EN "This entity type has the following attributes: "
                             )
        <> simpleTable [(plain.text.l) (NL "Attribuut", EN "Attribute")
                       ,(plain.text.l) (NL "Type"     , EN "Type")
                       ,mempty
                       ]
                       ( [[ (plain.text) "Id"
                          , (plain.text.name) cl
                          , (plain.text.l) (NL "Sleutel" , EN "Primary key")
                         ]] 
                         <>
                         [[ (plain.text.name) attr
                          , (plain.text.attTyp) attr
                          , (plain.text.l) (if attOptional attr 
                                            then (NL "Optioneel", EN "Optional")
                                            else (NL "Verplicht", EN "Mandatory")
                                           )] | attr <- clAtts cl]
                       )
        <> let asscs = [ assoc | assoc <- assocs oocd, assSrc assoc == clName cl || assTgt assoc == clName cl
                       ] 
           in  case asscs of
                 [] -> para ( text (name cl) <> text (l (NL " heeft geen associaties.", EN " has no associations.")))
                 _  -> para ( text (name cl) <> text (l (NL " heeft de volgende associaties: ", EN " has the following associations: ")))
                         <> orderedList (map assocToRow asscs) 
    where
        
     assocToRow :: Ampersand.Graphic.ClassDiagram.Association -> Blocks
     assocToRow assoc  =
         plain (  (text.assrhr) assoc
                <>(text.l) (NL " (vanaf ",EN " (from ")
                <>(text.assSrc) assoc
                <>(text.l) (NL " naar ", EN " to ")
                <>(text.assTgt) assoc
                <>text ")."
               ) 
     {- <>
        if (null.assrhr) assoc
        then fatal 192 "Shouldn't happen: flip the relation for the right direction!"
        else para $ case fsLang fSpec of
           Dutch   ->   case assrhm assoc of
                              Mult MinZero MaxOne  -> "Ieder(e) " <> (emph.text.assSrc) assoc <> " heeft hooguit één "   <> (emph.text.assTgt) assoc <> "."
                              Mult MinZero MaxMany -> mempty
                              Mult MinOne  MaxOne  -> "Ieder(e) " <> (emph.text.assSrc) assoc <> " heeft precies één "    <> (emph.text.assTgt) assoc <> "."
                              Mult MinOne  MaxMany -> "Ieder(e) " <> (emph.text.assSrc) assoc <> " heeft ten minste één " <> (emph.text.assTgt) assoc <> "."
                      <> case asslhm assoc of
                              Mult MinZero MaxOne  -> " Ieder(e) " <> (emph.text.assTgt) assoc <> " kan hooguit één "      <> (emph.text.assSrc) assoc <> " hebben."
                              Mult MinZero MaxMany -> mempty
                              Mult MinOne  MaxOne  -> " Ieder(e) " <> (emph.text.assTgt) assoc <> " kan precies één " <> (emph.text.assSrc) assoc <>       " hebben."
                              Mult MinOne  MaxMany -> " Ieder(e) " <> (emph.text.assTgt) assoc <> " kan ten minste één "    <> (emph.text.assSrc) assoc <> " hebben."
                      
           English ->    case assrhm assoc of
                              Mult MinZero MaxOne  -> "Every " <> (emph.text.assSrc) assoc <>" has at most one "  <> (emph.text.assTgt) assoc <>  "."
                              Mult MinZero MaxMany -> mempty
                              Mult MinOne  MaxOne  -> "Every " <> (emph.text.assSrc) assoc <>" has exactly one "  <> (emph.text.assTgt) assoc <>  "."
                              Mult MinOne  MaxMany -> "Every " <> (emph.text.assSrc) assoc <>" has at least one " <> (emph.text.assTgt) assoc <>  "."
                      <> case asslhm assoc of
                              Mult MinZero MaxOne  -> " For this association each " <> (emph.text.assTgt) assoc <> " has at most one "  <> (emph.text.assSrc) assoc <> "."
                              Mult MinZero MaxMany -> mempty
                              Mult MinOne  MaxOne  -> " For this association each " <> (emph.text.assTgt) assoc <> " has exactly one "  <> (emph.text.assSrc) assoc <> "."
                              Mult MinOne  MaxMany -> " For this association each " <> (emph.text.assTgt) assoc <> " has at least one " <> (emph.text.assSrc) assoc <> "."
     -}

  crudMatrixSection :: Blocks
  crudMatrixSection =
       header sectionLevel (text.l $ (NL "Logisch gegevensmodel", EN "Logical data model"))
    <> mconcat
        [ simpleTable [ plainText "Concept", plainText "C", plainText "R", plainText "U", plainText "D" ]
            [ [ plainText $ name cncpt
              , mconcat . map (plainText . name) $ ifcsC
              , mconcat . map (plainText . name) $ ifcsR
              , mconcat . map (plainText . name) $ ifcsU
              , mconcat . map (plainText . name) $ ifcsD ]
            | (cncpt, (ifcsC, ifcsR, ifcsU, ifcsD)) <- crudObjsPerConcept (crudInfo fSpec)
            ]
        ]
  

  technicalDataModelBlocks = 
       header sectionLevel
                (case fsLang fSpec of
                    Dutch   ->  "Technisch datamodel"
                    English ->  "Technical datamodel"
                )
    <> para (case fsLang fSpec of
               Dutch   ->   "De afspraken zijn vertaald naar een technisch datamodel. "
                         <> ( if canXRefer (getOpts fSpec)
                              then "Dit model is in figuur " <> xRefReference (getOpts fSpec) technicalDataModelPicture <> " weergegeven."
                              else "Dit model is in onderstaand figuur weergegeven. "
                            )
               English ->   "The functional requirements have been translated into a technical data model. "
                         <> ( if canXRefer (getOpts fSpec)
                              then "This model is shown by figure " <> xRefReference (getOpts fSpec) technicalDataModelPicture <> "."
                              else "This model is shown by the figure below. "
                            )
            )
    <> para (showImage (getOpts fSpec) technicalDataModelPicture)
    <> para (let nrOfTables = length (filter isTable (plugInfos fSpec))
             in
             case fsLang fSpec of
        Dutch   -> text ("Het technisch datamodel bestaat uit de volgende "++show nrOfTables++" tabellen:")
        English -> text ("The technical datamodel consists of the following "++show nrOfTables++" tables:")
            )
    <> mconcat [detailsOfplug p | p <- sortBy (compare `on` (map toLower . name)) (plugInfos fSpec), isTable p]
   where
      isTable :: PlugInfo -> Bool
      isTable (InternalPlug TblSQL{}) = True
      isTable (InternalPlug BinSQL{}) = True
      isTable ExternalPlug{} = False
      detailsOfplug :: PlugInfo -> Blocks
      detailsOfplug p =
           header 3 (   case (fsLang fSpec, p) of
                          (Dutch  , InternalPlug{}) ->  "Tabel: "
                          (Dutch  , ExternalPlug{}) ->  "Dataservice: "
                          (English, InternalPlug{}) ->  "Table: "
                          (English, ExternalPlug{}) ->  "Data service: "
                     <> text (name p)
                    )
        <> case p of
             InternalPlug tbl@TblSQL{}
               -> (case fsLang fSpec of
                Dutch
                   -> para (text $ "Deze tabel heeft de volgende "++(show.length.attributes) tbl++" attributen:")
                English
                   -> para (text $ "This table has the following "++(show.length.attributes) tbl++" attributes:")
                  )
               <> showAttributes (plugAttributes tbl)
             InternalPlug bin@BinSQL{}
               -> para (   (text.l) (NL "Dit is een koppeltabel, die "
                                    ,EN "This is a link-table, implementing ")
                        <> primExpr2pandocMath (fsLang fSpec) 
                                               (case dLkpTbl bin of
                                                  [store] -> EDcD (rsDcl store)
                                                  ss       -> fatal 540 $ "Exactly one relation sould be stored in BinSQL. However, there are "++show (length ss)
                                               )
                        <> (text.l) (NL " implementeert. De tabel bestaat uit de volgende kolommen:"
                                    ,EN ". It contains the following columns:")
                       )
                     <> showAttributes (plugAttributes bin)

             ExternalPlug _
               -> case fsLang fSpec of
                    Dutch   -> para "De details van deze service zijn in dit document (nog) niet verder uitgewerkt."
                    English -> para "The details of this dataservice are not available in this document."
      showAttributes :: [SqlAttribute] -> Blocks
      showAttributes atts = bulletList (map showAttribute atts)
        where
          showAttribute att =
                para (  (strong.text.attName) att
                      <> linebreak
                      <> case attUse att of
                            PrimaryKey _ -> case fsLang fSpec of
                                              Dutch   -> "Dit attribuut is de primaire sleutel. "
                                              English -> "This attribute is the primary key. "
                            ForeignKey c -> case fsLang fSpec of
                                              Dutch   -> "Dit attribuut verwijst naar een voorkomen in de tabel "
                                              English -> "This attribute is a foreign key to "
                                               <> (text.name) c
                            PlainAttr    -> case fsLang fSpec of
                                              Dutch   -> "Dit attribuut implementeert "
                                              English -> "This attribute implements "
                                          <> primExpr2pandocMath (fsLang fSpec) (attExpr att)
                                          <> "."
                      <> linebreak
                      <> (code.show.attType) att
                      <> ", "
                      <> (case fsLang fSpec of
                            Dutch
                              ->  (if attNull att then "Optioneel" else "Verplicht")
                               <> (if attUniq att then ", Uniek" else "")
                               <> "."
                            English
                              ->  (if attNull att then "Optional" else "Mandatory")
                               <> (if attUniq att then ", Unique" else "")
                               <> "."
                         )
                     )
  technicalDataModelPicture = makePicture fSpec PTTechnicalDM

  daRulesSection :: Blocks
  daRulesSection = mconcat 
      [ header sectionLevel . text $ l (NL "Regels", EN "Rules")
      , para . text $ l (NL "TODO: uitleg paragraaf", EN "TODO: explain section")
      , docRules (NL "Procesregels", EN "Process rules")
                 ( NL "TODO: uitleg procesregels"
                 , EN "TODO: explain process rules")
                 ( NL "Deze specificatie bevat geen procesregels."
                 , EN "This specification does not contain any process rules.")
                 (NL "Procesregel", EN "Process rule")
                 prcssRules
      , docRules (NL "Invarianten", EN "Invariants")
                 ( NL "TODO: uitleg invarianten"
                 , EN "TODO: explain invariants")
                 ( NL "Deze specificatie bevat geen invarianten."
                 , EN "This specification does not contain any invariants.")
                 (NL "Invariant", EN "Invariant")
                 userInvariants
      ]
   where
    (prcssRules, userInvariants) = partition isSignal $ vrules fSpec
    docRules :: LocalizedStr -> LocalizedStr -> LocalizedStr -> LocalizedStr -> [Rule] -> Blocks
    docRules _     _     noRules _       []    = para . text $ l noRules
    docRules title intro _       heading rules = mconcat $
      [ header (sectionLevel+1) . text $ l title 
      , para . text $ l intro
      ] ++
      map (docRule heading) rules
  
    docRule :: LocalizedStr -> Rule -> Blocks
    docRule heading rule = mconcat
       [ plain $ strong (text (l heading ++ ": ") <> emph (text (rrnm rule)))
       , fromList $ maybe mempty (concatMap $ amPandoc . explMarkup) $ purposeOf fSpec (fsLang fSpec) rule
       , fromList $ meaning2Blocks (fsLang fSpec) rule
       , if showPredExpr (getOpts fSpec)
         then let predicate = toPredLogic rule
              in  if format == Frtf then
                     plain $ linebreak <> singleton (RawInline (Text.Pandoc.Builder.Format "rtf") (showRtf predicate)) 
                  else
                    pandocEqnArrayWithLabel (XRefDataAnalRule rule) (showLatex predicate)
         else if format == FLatex
              then fromList $ pandocEquation (showMath rule)
              else (plain . text $ l (NL "Ampersand expressie:", EN "Ampersand expression:")) <>
                   (plain . code $ showADL (rrexp rule))
       , plain $ singleton $ RawInline (Text.Pandoc.Builder.Format "latex") "\\bigskip" -- also causes a skip in rtf (because of non-empty plain)
       , if isSignal rule
         then mempty
         else (para.text.l)
                (NL $ "Overtredingen van deze regel leiden tot een foutmelding aan de gebruiker: "
                          ++"\"TODO\"."
                ,EN $ "Violations of this rule will result in an error message for the user: "
                          ++"\"TODO\"."
                )   
       ]   
      where format = fspecFormat (getOpts fSpec) -- todo: bit hacky to use the output format here, but otherwise we need a major refactoring
  
primExpr2pandocMath :: Lang -> Expression -> Inlines
primExpr2pandocMath lang e =
 case e of
  (EDcD d ) ->
           case lang of
             Dutch -> text "de relatie "
             English -> text "the relation "
        <> math ((name.source) d++ " \\xrightarrow {"++name d++"} "++(name.target) d)
  (EFlp (EDcD d)) ->
           case lang of
             Dutch -> text "de relatie "
             English -> text "the relation "
        <> math ((name.source) d++ " \\xleftarrow  {"++name d++"} "++(name.target) d)
  (EIsc (r1,_)) ->
           let srcTable = case r1 of
                            EDcI c -> c
                            _      -> fatal 767 ("Unexpected expression: "++show r1)
           in
           case lang of
             Dutch -> text "de identiteitsrelatie van "
             English -> text "the identityrelation of "
        <> math (name srcTable)
  (EDcI c) ->
            case lang of
             Dutch -> text "de identiteitsrelatie van "
             English -> text "the identityrelation of "
        <> math (name c)
  (EEps c _) -> 
            case lang of
             Dutch -> text "de identiteitsrelatie van "
             English -> text "the identityrelation of "
        <> math (name c)
  _   -> fatal 223 ("Have a look at the generated Haskell to see what is going on..\n"++show e)
