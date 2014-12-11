{-# LANGUAGE OverloadedStrings #-}
module Database.Design.Ampersand.Output.ToPandoc.ChapterDataAnalysis (chpDataAnalysis)
where
import Database.Design.Ampersand.Output.ToPandoc.SharedAmongChapters
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.Output.PandocAux
import Database.Design.Ampersand.FSpec.Graphic.ClassDiagram --(Class(..),CdAttribute(..))
import Database.Design.Ampersand.Output.PredLogic
import Database.Design.Ampersand.FSpec.Motivations
import Data.List
import Data.Function (on)
import qualified Text.Pandoc.Builder

fatal :: Int -> String -> a
fatal = fatalMsg "Output.ToPandoc.ChapterDataAnalysis"

------------------------------------------------------------
--DESCR -> the data analysis contains a section for each class diagram in the fSpec
--         the class diagram and multiplicity rules are printed
chpDataAnalysis :: FSpec -> (Blocks,[Picture])
chpDataAnalysis fSpec = (theBlocks, thePictures)
 where

  theBlocks
    =  chptHeader (fsLang fSpec) DataAnalysis  -- The header
    <> (case fsLang fSpec of
             Dutch   -> para ( "Dit hoofdstuk bevat het resultaat van de gegevensanalyse. "
                            <> "De opbouw is als volgt:"
                             )
                     <> para ( if summaryOnly
                               then  "We beginnen met "
                               else  "We beginnen met het classificatiemodel, gevolgd door "
                            <>  "een overzicht van alle relaties, die samen de basis vormen van de rest van deze analyse. "
                            <>  "tenslotte volgen achtereenvolgend het logische- en technische gegevensmodel."
                             )
             English -> para (  "This chapter contains the result of the data analysis. "
                            <>  "It is structured as follows:"
                             )
                     <> para ( if summaryOnly
                               then  "We start with "
                               else  "We start with the classification model, followed by "
                            <>  "a list of all relations, that are the foundation of the rest of the analysis. "
                            <>  "Finally, the logical and technical data model are discussed."
                             )
       )
    <> if summaryOnly then mempty else classificationBlocks
    <> daBasicsBlocks
    <> daRulesBlocks
    <> logicalDataModelBlocks
    <> technicalDataModelBlocks
  thePictures
    =  [classificationPicture | not summaryOnly]
    ++ logicalDataModelPictures ++ technicalDataModelPictures

  daBasicsBlocks                                         = daBasicsSection           sectionLevel fSpec
  daRulesBlocks                                          = daRulesSection            sectionLevel fSpec
  (classificationBlocks    , classificationPicture     ) = classificationSection     sectionLevel fSpec
  (logicalDataModelBlocks  , logicalDataModelPictures  ) = logicalDataModelSection   sectionLevel fSpec
  (technicalDataModelBlocks, technicalDataModelPictures) = technicalDataModelSection sectionLevel fSpec
  sectionLevel = 2

  -- | In some cases, only a summary of the data analysis is required as output.
  summaryOnly :: Bool
  summaryOnly = theme (getOpts fSpec) `elem` [StudentTheme]

classificationSection :: Int -> FSpec -> (Blocks,Picture)
classificationSection lev fSpec = (theBlocks,pict)
 where
  theBlocks =
       header lev (case fsLang fSpec of
                    Dutch   ->  "Classificaties"
                    English ->  "Classifications"
                  )
    <> content
  content =
    if null (classes classificationModel)
    then para (case fsLang fSpec of
              Dutch   ->  "Er zijn geen classificaties gedefinieerd."
              English ->  "No classifications have been defined"
              )
    else para (case fsLang fSpec of
              Dutch   ->  "Een aantal concepten zit in een classificatiestructuur. "
                       <> (if canXRefer (getOpts fSpec)
                           then  "Deze is in figuur " <> xRefReference (getOpts fSpec) pict <> "weergegeven."
                           else "Deze is in onderstaand figuur weergegeven."
                          )
              English -> "A number of concepts is organized in a classification structure. "
                       <> (if canXRefer (getOpts fSpec)
                           then "This is shown in figure " <> xRefReference (getOpts fSpec) pict <> "."
                           else "This is shown in the figure below."
                          )
            )
         <> para (showImage (getOpts fSpec) pict)

   where
  classificationModel :: ClassDiag
  classificationModel = clAnalysis fSpec

  pict :: Picture
  pict = makePicture fSpec PTClassDiagram

logicalDataModelSection :: Int -> FSpec -> (Blocks,[Picture])
logicalDataModelSection lev fSpec = (theBlocks, [pict])
 where
  theBlocks =
       header lev (case fsLang fSpec of
                    Dutch   -> text "Logisch gegevensmodel"
                    English -> text "Logical data model"
                )
    <> para (case fsLang fSpec of
               Dutch   -> (text "De afspraken zijn vertaald naar een gegevensmodel. "
                         <> ( if canXRefer (getOpts fSpec)
                              then text "Dit gegevensmodel is in figuur " <> xRefReference (getOpts fSpec) pict <> text " weergegeven."
                              else text "Dit gegevensmodel is in onderstaand figuur weergegeven. "
                          ) )
               English -> (text "The functional requirements have been translated into a data model. "
                         <> ( if canXRefer (getOpts fSpec)
                              then text "This model is shown by figure " <> xRefReference (getOpts fSpec) pict <> text "."
                              else text "This model is shown by the figure below. "
                          ) )
            )
     <> para (showImage (getOpts fSpec) pict)
     <> let nrOfClasses = length (classes oocd)
        in case fsLang fSpec of
             Dutch   -> para (case nrOfClasses of
                                0 -> text "Er zijn geen gegevensverzamelingen."
                                1 -> text "Er is één gegevensverzameling, die in de volgende paragraaf in detail is beschreven:"
                                _ -> text ("Er zijn "++count Dutch nrOfClasses "gegevensverzameling"++". ")
                                  <> text "De details van elk van deze gegevensverzameling worden, op alfabetische volgorde, in de nu volgende paragrafen beschreven:"
                             )
             English -> para (case nrOfClasses of
                                0 -> text "There are no entity types."
                                1 -> text "There is only one entity type:"
                                _ -> text ("There are "++count English nrOfClasses "entity type" ++".")
                                  <> text "The details of each entity type are described (in alfabetical order) in the following paragraphs:"
                             )
     <> mconcat (map detailsOfClass (sortBy (compare `on` name) (classes oocd)))

  pict :: Picture
  pict = makePicture fSpec PTLogicalDM

  oocd :: ClassDiag
  oocd = cdAnalysis fSpec

  detailsOfClass :: Class -> Blocks
  detailsOfClass cl =
       (   header (lev+1) ((case fsLang fSpec of
                       Dutch   -> text "Gegevensverzameling: "
                       English -> text "Entity type: "
                     )
                     <> (emph.strong.text.name) cl)
        <> case clcpt cl of
             Nothing -> mempty
             Just (_, purposes)  -> purposes2Blocks (getOpts fSpec) purposes
        <> case fsLang fSpec of
             Dutch   -> para $ text "Deze gegevensverzameling bevat de volgende attributen: "
             English -> para $ text "This entity type has the following attributes: "
        <> simpleTable (case fsLang fSpec of
                          Dutch   -> [(plain.text) "Attribuut"
                                     ,(plain.text) "Type"
                                     ,mempty
                                     ]
                          English -> [(plain.text) "Attribute"
                                     ,(plain.text) "Type"
                                     ,mempty
                                     ]
                       )
                       ( [[ (plain.text) "Id"
                          , (plain.text.name) cl
                          , (plain.text) (case fsLang fSpec of
                                            Dutch -> "Sleutel"
                                            English -> "Primary key"
                                         )
                         ]]
                    <> [ [ (plain.text.name) attr
                         , (plain.text.attTyp) attr
                         , (plain.text) $ case (fsLang fSpec,attOptional attr) of
                                            (Dutch  ,True ) -> "Optioneel"
                                            (English,True ) -> "Optional"
                                            (Dutch  ,False) -> "Verplicht"
                                            (English,False) -> "Mandatory"
                         ]
                         | attr <- clAtts cl]
                       )
        <> case fsLang fSpec of
             Dutch   -> para ( text (name cl) <> text " heeft de volgende associaties: ")
             English -> para ( text (name cl) <> text " has the following associations: ")
        <> orderedList [assocToRow assoc | assoc <- assocs oocd
                         , assSrc assoc == clName cl || assTgt assoc == clName cl]
       )
    where
     assocToRow :: Database.Design.Ampersand.FSpec.Graphic.ClassDiagram.Association -> Blocks
     assocToRow assoc  =
        (para.text.assrhr) assoc <>
        purposes2Blocks (getOpts fSpec) (asspurp assoc) <>
        (case assmean assoc of Just markup -> fromList (amPandoc markup); Nothing -> mempty ) <>
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

technicalDataModelSection :: Int -> FSpec -> (Blocks,[Picture])
technicalDataModelSection lev fSpec = (theBlocks,[pict])
 where
   theBlocks =
       header lev (case fsLang fSpec of
                    Dutch   ->  "Technisch datamodel"
                    English ->  "Technical datamodel"
                      )
    <> para (case fsLang fSpec of
               Dutch   -> ( "De afspraken zijn vertaald naar een technisch datamodel. "
                         <> ( if canXRefer (getOpts fSpec)
                              then "Dit model is in figuur " <> xRefReference (getOpts fSpec) pict <> " weergegeven."
                              else "Dit model is in onderstaand figuur weergegeven. "
                          ) )
               English -> ( "The functional requirements have been translated into a technical data model. "
                         <> ( if canXRefer (getOpts fSpec)
                              then "This model is shown by figure " <> xRefReference (getOpts fSpec) pict <> "."
                              else "This model is shown by the figure below. "
                          ) )
            )
    <> para (showImage (getOpts fSpec) pict)
    <> para (let nrOfTables = length (filter isTable (plugInfos fSpec))
             in
             case fsLang fSpec of
        Dutch   -> text ("Het technisch datamodel bestaat uit de volgende "++show nrOfTables++" tabellen:")
        English -> text ("The technical datamodel consists of the following "++show nrOfTables++"tables:")
            )
    <> mconcat [detailsOfplug p | p <- sortBy (compare `on` name) (plugInfos fSpec), isTable p]
    where
      isTable :: PlugInfo -> Bool
      isTable (InternalPlug TblSQL{}) = True
      isTable (InternalPlug BinSQL{}) = True
      isTable (InternalPlug ScalarSQL{}) = False
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
                   -> para (text $ "Deze tabel heeft de volgende "++(show.length.fields) tbl++" velden:")
                English
                   -> para (text $ "This table has the following "++(show.length.fields) tbl++" fields:")
                  )
               <> showFields (plugFields tbl)
             InternalPlug bin@BinSQL{}
               -> para
                       (case fsLang fSpec of
                         Dutch
                           ->  "Dit is een koppeltabel, die "
                            <> primExpr2pandocMath (fsLang fSpec) (mLkp bin)
                            <> " implementeert. De tabel bestaat uit de volgende kolommen:"

                         English
                           ->  "This is a link-table, implementing "
                            <> primExpr2pandocMath (fsLang fSpec) (mLkp bin)
                            <> ". It contains the following columns:"
                       )
                     <> showFields (plugFields bin)

             InternalPlug ScalarSQL{}
                -> mempty
             ExternalPlug _
               -> case fsLang fSpec of
                    Dutch   -> para "De details van deze service zijn in dit document (nog) niet verder uitgewerkt."
                    English -> para "The details of this dataservice are not available in this document."
      showFields :: [SqlField] -> Blocks
      showFields flds = bulletList (map showField flds)
        where
          showField fld =
--FIXME 20140525: Onderstaande code vervangen door afl te leiden van `flduse`. Daar zit deze info al in verwerkt!
             let isPrimaryKey = case fldexpr fld of
                                  e@EDcI{} -> e==fldexpr (head flds) -- The first field represents the most general concept
                                  _        -> False
                 mForeignKey  = case fldexpr fld of
                                  EIsc (EDcI c,_) -> Just c
                                  _               -> Nothing
             in para (  (strong.text.fldname) fld
                      <> linebreak
                      <> (if isPrimaryKey
                          then case fsLang fSpec of
                                Dutch   -> "Dit attribuut is de primaire sleutel. "
                                English -> "This attribute is the primary key. "
                          else
                          case mForeignKey of
                           Just c ->  case fsLang fSpec of
                                         Dutch   -> "Dit attribuut verwijst naar een voorkomen in de tabel "
                                         English -> "This attribute is a foreign key to "
                                     <> (text.name) c
                           Nothing -- (no foreign key...)
                             -> --if isBool
                                --then
                                --else
                                  (case fsLang fSpec of
                                     Dutch   -> "Dit attribuut implementeert "
                                     English -> "This attribute implements "
                                  <> primExpr2pandocMath (fsLang fSpec) (fldexpr fld)
                                  <> "."
                                  )
                         )
                      <> linebreak
                      <> (code.show.fldtype) fld
                      <> ", "
                      <> (case fsLang fSpec of
                            Dutch
                              ->  (if fldnull fld then "Optioneel" else "Verplicht")
                               <> (if flduniq fld then ", Uniek" else "")
                               <> "."
                            English
                              ->  (if fldnull fld then "Optional" else "Mandatory")
                               <> (if flduniq fld then ", Unique" else "")
                               <> "."
                         )
                     )
   pict :: Picture
   pict = makePicture fSpec PTTechnicalDM

daBasicsSection :: Int -> FSpec -> Blocks
-- | The function daBasicsSection lists the basic sentences that have been used in assembling the data model.
daBasicsSection lev fSpec = theBlocks
 where
  theBlocks =
       header lev (case fsLang fSpec of
                    Dutch   -> "Basiszinnen"
                    English -> "Fact types"
                  )
   <> case fsLang fSpec of
        Dutch   -> para ( "In deze paragraaf worden de basiszinnen opgesomd, die een rol spelen bij het ontwerp van de gegevensstructuur. "
                       <> "Per basiszin wordt de naam en het bron- en doelconcept gegeven, alsook de eigenschappen van deze relatie."
                        )
        English -> para ( "This section enumerates the fact types, that have been used in the design of the datastructure. "
                       <> "For each fact type its name, the source and target concept and the properties are documented."
                        )
   <> definitionList (map toDef (relsInThemes fSpec))
    where
      toDef :: Declaration -> (Inlines, [Blocks])
      toDef d
        = ( (math.showMath) d
          , [   para linebreak
             <> fromList (meaning2Blocks (fsLang fSpec) d)

          <> para (   ((strong.text) (case fsLang fSpec of
                                       Dutch   -> "Eigenschappen"
                                       English -> "Properties"
                                    ))
                   <> text ": "
                   <> if null (multiplicities d)
                      then text "--"
                      else inlineIntercalate (str ", ") [ text (showADL m) | m <-multiplicities d]
                  )
          <> para linebreak
            ]

          )

daRulesSection :: Int -> FSpec -> Blocks
daRulesSection lev fSpec = theBlocks
 where
  theBlocks = mconcat 
    [ header lev . text $ l (NL "Regels", EN "Rules")
    , para . text $ l (NL "TODO: uitleg paragraaf", EN "TODO: explain section")
    , docRules (NL "Procesregels", EN "Process rules")
               ( NL "TODO: uitleg procesregels"
               , EN "TODO: explain process rules")
               ( NL "Deze specificatie bevat geen procesregels."
               , EN "This specification does not contain any process rules.")
               (NL "Procesregel", EN "Process rule")
               processRules
    , docRules (NL "Invarianten", EN "Invariants")
               ( NL "TODO: uitleg invarianten"
               , EN "TODO: explain invariants")
               ( NL "Deze specificatie bevat geen invarianten."
               , EN "This specification does not contain any invariants.")
               (NL "Invariant", EN "Invariant")
               userInvariants
    , docRules (NL "Afgeleide regels", EN "Derived rules") -- TODO: maybe call these "derived invariants" instead?
               ( NL "TODO: uitleg afgeleide regels"
               , EN "TODO: explain derived rules")
               ( NL "Deze specificatie bevat geen afgeleide regels."
               , EN "This specification does not contain any derived rules.")
               (NL "Afgeleide regel", EN "Derived rule") $
               grules fSpec
    ]
  (processRules, userInvariants) = partition isSignal $ vrules fSpec
  docRules :: LocalizedStr -> LocalizedStr -> LocalizedStr -> LocalizedStr -> [Rule] -> Blocks
  docRules _ _ noRules _       [] = para . text $ l noRules
  docRules title intro noRules heading rules = mconcat $
    [ header (lev+1) . text $ l title 
    , para . text $ l intro
    ] ++
    map (docRule heading) rules
  
  docRule :: LocalizedStr -> Rule -> Blocks
  docRule heading rule = mconcat $
     [ plain $ strong (text (l heading ++ ": ") <> emph (text (rrnm rule)))
     , fromList $ meaning2Blocks (fsLang fSpec) rule
     , if showPredExpr (getOpts fSpec)
       then let pred = toPredLogic rule
            in  if fspecFormat (getOpts fSpec) == Frtf then -- todo: bit hacky to check format here, but otherwise we need a major refactoring
                  plain $ linebreak <> (singleton $ RawInline (Text.Pandoc.Builder.Format "rtf") (showRtf pred)) 
                else
                  fromList $ pandocEqnArrayOnelabel (symDefLabel rule) (showLatex pred)
       else (plain . text $ l (NL "ADL expressie:", EN "ADL expression:")) <>
            (plain . code $ showADL (rrexp rule))
     , plain $ singleton $ RawInline (Text.Pandoc.Builder.Format "latex") "\\bigskip" -- also causes a skip in rtf (because of non-empty plain)
     ]
  
  -- shorthand for easy localizing    
  l :: LocalizedStr -> String
  l lstr = localize (fsLang fSpec) lstr
     
     -- The properties of various relations are documented in different tables.
-- First, we document the heterogeneous properties of all relations
-- Then, the endo-poperties are given, and finally
-- the signals are documented.
{-
  daAssociations :: [Relation] -> [Block]
  daAssociations rs = heteroMultiplicities ++ endoProperties ++ identityDocumentation ++ viewDocumentation
   where
    heteroMultiplicities
     = case [r | r@Rel{}<-rs, not (isProp r), not (isAttribute r)] of
        []  -> []
        [r] -> [ case fsLang fSpec of
                   Dutch   ->
                      Para [ Str $ upCap (name fSpec)++" heeft één associatie: "++showADL r++". Deze associatie "++
                                   case (isTot r, isSur r) of
                                    (False, False) -> "heeft geen beperkingen ten aanzien van multipliciteit."
                                    (True,  False) -> "is totaal."
                                    (False, True ) -> "is surjectief."
                                    (True,  True ) -> "is totaal en surjectief."
                           ]
                   English ->
                      Para [ Str $ upCap (name fSpec)++" has one association: "++showADL r++". This association "++
                                   case (isTot r, isSur r) of
                                    (False, False) -> "has no restrictions with respect to multiplicities. "
                                    (True,  False) -> "is total."
                                    (False, True ) -> "is surjective."
                                    (True,  True ) -> "is total and surjective."
                           ]]
        rs' -> case [r | r<-rs', (not.null) ([Tot,Sur] `isc` multiplicities r) ] of
               []  -> []
               [r] -> [ case fsLang fSpec of
                          Dutch   ->
                             Para [ Str $ upCap (name fSpec)++" heeft "++count Dutch (length rs) "associatie"++". "
                                  , Str $ " Daarvan is "++showADL r++if isTot r then "totaal" else "surjectief"
                                  ]
                          English   ->
                            Para [ Str $ upCap (name fSpec)++" has "++count English (length rs) "association"++". "
                                  , Str $ " Association "++showADL r++" is "++if isTot r then "total" else "surjective"
                                  ]
                      ]
               _   -> [ case fsLang fSpec of
                          Dutch   ->
                             Para [ Str $ upCap (name fSpec)++" heeft de volgende associaties en multipliciteitsrestricties. "
                                  ]
                          English   ->
                             Para [ Str $ upCap (name fSpec)++" has the following associations and multiplicity constraints. "
                                  ]
                      , Table [] [AlignLeft,AlignCenter,AlignCenter] [0.0,0.0,0.0]
                        ( case fsLang fSpec of
                          Dutch   ->
                               [ [Plain [Str "relatie"]]
                               , [Plain [Str "totaal"]]
                               , [Plain [Str "surjectief"]]]
                          English   ->
                               [ [Plain [Str "relation"]]
                               , [Plain [Str "total"]]
                               , [Plain [Str "surjective"]]]
                        )
                        [[[Plain [Math InlineMath (showMath r)]] -- r is a relation.
                         ,[Plain [Math InlineMath "\\surd" | isTot r]]
                         ,[Plain [Math InlineMath "\\surd" | isSur r]]]
                        | r<-rs', not (isAttribute r)
                        ]
                      ]
    isAttribute r = (not.null) ([Uni,Inj] `isc` multiplicities r)
    endoProperties
     = if null [ m | r<-hMults, m<-multiplicities r, m `elem` [Rfx,Irf,Trn,Sym,Asy]]
       then []
       else [ Para (case fsLang fSpec of
                          Dutch   ->
                            [Str "Er is één endorelatie, ", Math InlineMath (showMath d), Str " met de volgende eigenschappen: "]
                          English   ->
                            [Str "There is one endorelation, ", Math InlineMath (showMath d), Str " with the following properties: "] )
            | length hMults==1, d<-hMults ]++
            [ Para [ case fsLang fSpec of
                          Dutch   ->
                            Str "In aanvulling daarop hebben de endorelaties de volgende eigenschappen: "
                          English   ->
                            Str "Additionally, the endorelations come with the following properties: "]
            | length hMults>1 ]++
            [Table [] [AlignLeft,AlignCenter,AlignCenter,AlignCenter,AlignCenter,AlignCenter,AlignCenter] [0.0,0.0,0.0,0.0,0.0,0.0,0.0]
             [[case fsLang fSpec of
                          Dutch   -> Plain [Str "relatie"]
                          English -> Plain [Str "relation"]  ]
             ,[Plain [Str "Rfx"]]
             ,[Plain [Str "Irf"]]
             ,[Plain [Str "Trn"]]
             ,[Plain [Str "Sym"]]
             ,[Plain [Str "Asy"]]
             ,[Plain [Str "Prop"]]]
             [[[Plain [Math InlineMath (showMath d)]] -- d is a declaration, and therefore typeable. So  showMath d  exists.
              ,[Plain [Math InlineMath "\\surd" | isRfx d ]]
              ,[Plain [Math InlineMath "\\surd" | isIrf d ]]
              ,[Plain [Math InlineMath "\\surd" | isTrn d ]]
              ,[Plain [Math InlineMath "\\surd" | isSym d ]]
              ,[Plain [Math InlineMath "\\surd" | isAsy d ]]
              ,[Plain [Math InlineMath "\\surd" | isAsy d && isSym d ]]]
             | d<-hMults]
            | length hMults>0 ]
       where
        hMults :: [Declaration]
        hMults  = [decl | decl@Sgn{}<- relsUsedIn fSpec, isEndo decl
                        , null (themes fSpec) || decpat decl `elem` themes fSpec]
    identityDocumentation
     = case (identities fSpec, fsLang fSpec) of
        ([], _)              -> []
        ([k], Dutch)         -> [ Para  [Str "Er is één identiteit: ",Str (name k),Str "."]]
        ([k], English)       -> [ Para  [Str "There is but one identity: ",Str (name k),Str "." ]]
        (identities, Dutch)  -> [ Para $ Str "De volgende identiteiten bestaan: ": commaNLPandoc (Str "en") [Str (name i) | i<-identities]]
        (identities, English)-> [ Para $ Str "The following identities exist: ": commaEngPandoc (Str "and") [Str (name i) | i<-identities]]

    viewDocumentation
     = case (viewDefs fSpec, fsLang fSpec) of
        ([], _) -> []
        ([v], Dutch)   -> [ Para  [Str "Er is één view: ",Str (name v),Str "."]]
        ([v], English) -> [ Para  [Str "There is but one view: ",Str (name v),Str "." ]]
        (viewds, Dutch) -> [ Para $ Str "De volgende views bestaan: ": commaNLPandoc (Str "en") [Str (name v) | v<-viewds]]
        (viewds, English)->[ Para $ Str "The following views exist: ": commaEngPandoc (Str "and") [Str (name v) | v<-viewds]]

-- The properties of various declations are documented in different tables.
-- First, we document the heterogeneous properties of all relations
-- Then, the endo-poperties are given, and finally
-- the process rules are documented.

  daAttributes :: PlugSQL -> [Block]
  daAttributes p
   = if length (plugFields p)<=1 then [] else
     [ case fsLang fSpec of
               Dutch   ->
                 Para [ Str $ "De attributen van "++name p++" hebben de volgende multipliciteitsrestricties. "
                 ]
               English ->
                 Para [ Str $ "The attributes in "++name p++" have the following multiplicity constraints. "
                 ]
     ,Table [] [AlignLeft,AlignLeft,AlignCenter,AlignCenter] [0.0,0.0,0.0,0.0]
      ( case fsLang fSpec of
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
      [ if isProp (fldexpr fld) && fld/=head (plugFields p)
        then [ [Plain [Str (fldname fld)]]
             , [Plain [ Str "Bool"]]
             , [Plain [Math InlineMath "\\surd"]]
             , []
             ]
        else [ [Plain [if fld==head (plugFields p) || null ([Uni,Inj,Sur]>-multiplicities (fldexpr fld))
                       then Str  "key "
                       else Str (fldname fld)]]
             , [Plain [ (Str . latexEscShw.name.target.fldexpr) fld]]
             , [Plain [Math InlineMath "\\surd" | not (fldnull fld)]]
             , [Plain [Math InlineMath "\\surd" | flduniq fld]]
             ]
      | fld<-plugFields p  -- tail haalt het eerste veld, zijnde I[c], eruit omdat die niet in deze tabel thuishoort.
      ]

     ]
-- the endo-properties have already been reported in the general section of this chapter.

  -- daPlugs describes data sets.
  -- These can be recognized by:
  --    1. the first field has the "unique" attribute on (otherwise it is a binary association)
  --    2. there is more than one field (otherwise it is a scalar).
  -- The text gives all rules that are maintained internally within the data structure,
  -- because they might very well be implemented as database integrity rules.
  -- Multiplicity rules are not reported separately, because they are already taken care of in the multiplicity tables.
  -- Plugs that are associations between data sets and scalars are ignored.

  daPlug :: Options -> PlugSQL -> [Block]
  daPlug opts p
   = if null content then [] else plugHeader ++ content
     where
       thing2block r = pandocEqnArrayOnelabel (symDefLabel r) ((showLatex.toPredLogic) r)
       plugHeader = toList $ labeledThing opts (lev+1) ("sct:Plug "++escapeNonAlphaNum (name p)) (name p)
       content = daAttributes p ++ plugRules ++ plugSignals ++ plugIdentities ++ iRules
       plugRules
        = case fsLang fSpec of
           English -> case [r | r<-invariants fSpec, null [d | d@Sgn{} <- relsMentionedIn r, d `notElem` relsUsedIn p]] of
                       []  -> []
                       [r] -> [ Para [ Str "Within this data set, the following integrity rule shall be true at all times. " ]]++
                              if showPredExpr opts
                              then pandocEqnArrayOnelabel (symDefLabel r) ((showLatex.toPredLogic) r)
                              else [ Para [ Math DisplayMath $ showMath r]]
                       rs  -> [ Para [ Str "Within this data set, the following integrity rules shall be true at all times. " ]
                              , if showPredExpr opts
                                then BulletList [(pandocEqnArrayOnelabel (symDefLabel r) . showLatex . toPredLogic) r | r<-rs ]
                                else BulletList [ [Para [Math DisplayMath $ showMath r]] | r<-rs ]
                              ]
           Dutch   -> case [r | r<-invariants fSpec, null [d | d@Sgn{} <- relsMentionedIn r, d `notElem` relsUsedIn p]] of
                       []  -> []
                       [r] -> [ Para [ Str "Binnen deze gegevensverzameling dient de volgende integriteitsregel te allen tijde waar te zijn. " ]]++
                              if showPredExpr opts
                              then pandocEqnArrayOnelabel (symDefLabel r) ((showLatex.toPredLogic) r)
                              else [ Para [ Math DisplayMath $ showMath r]]
                       rs  -> [ Para [ Str "Binnen deze gegevensverzameling dienen de volgende integriteitsregels te allen tijde waar te zijn. " ]
                              , if showPredExpr opts
                                then BulletList [(pandocEqnArrayOnelabel (symDefLabel r) . showLatex . toPredLogic) r | r<-rs ]
                                else BulletList [ [Para [Math DisplayMath $ showMath r]] | r<-rs ]
                              ]
       plugIdentities
        = case fsLang fSpec of
           English -> case [k | k<-identityRules fSpec, null [d | d@Sgn{} <- relsMentionedIn k, d `notElem` relsUsedIn p]] of
                       []  -> []
                       [s] -> [ Para [ Str "This data set contains one key. " ]]++
                              if showPredExpr opts
                              then pandocEqnArrayOnelabel (symDefLabel s) ((showLatex.toPredLogic) s)
                              else [ Para [ Math DisplayMath $ showMath s]]
                       ss  -> [ Para [ Str "This data set contains the following keys. " ]
                              , if showPredExpr opts
                                then BulletList [(pandocEqnArrayOnelabel (symDefLabel s) . showLatex . toPredLogic) s | s<-ss ]
                                else BulletList [ [Para [Math DisplayMath $ showMath s]]
                                                | s<-ss ]
                              ]
           Dutch   -> case [k | k<-identityRules fSpec, null [d | d@Sgn{} <- relsMentionedIn k, d `notElem` relsUsedIn p]] of
                       []  -> []
                       [s] -> [ Para [ Str ("Deze gegevensverzameling genereert één key. ") ]]++
                              if showPredExpr opts
                              then pandocEqnArrayOnelabel (symDefLabel s) ((showLatex.toPredLogic) s)
                              else [ Para [ Math DisplayMath $ showMath s]]
                       ss  -> [ Para [ Str "Deze gegevensverzameling genereert de volgende keys. " ]
                              , if showPredExpr opts
                                then BulletList [(pandocEqnArrayOnelabel (symDefLabel s) . showLatex . toPredLogic) s | s<-ss ]
                                else BulletList [ [Para [Math DisplayMath $ showMath s]] | s<-ss ]
                              ]
       plugSignals
        = case (fsLang fSpec, [r | r<-vrules fSpec, isSignal r , null [d | d@Sgn{} <- relsMentionedIn r, d `notElem` relsUsedIn p]]) of
    --       English -> case [r | r<-vrules fSpec, isSignal r , null [d | d@Sgn{} <- relsMentionedIn r, d `notElem` relsUsedIn p]] of
            (_      , [])  -> []
            (English, [s]) -> [ Para [ Str "This data set generates one process rule. " ]]++
                              if showPredExpr opts
                              then pandocEqnArrayOnelabel (symDefLabel s) ((showLatex.toPredLogic) s)
                              else [ Para [ Math DisplayMath $ showMath s]]
            (English, ss)  -> [  Para [ Str "This data set generates the following process rules. " ]
                              , if showPredExpr opts
                                then BulletList [(pandocEqnArrayOnelabel (symDefLabel s) . showLatex . toPredLogic) s | s<-ss ]
                                else BulletList [ [Para [Math DisplayMath $ showMath s]] | s<-ss ]
                              ]
            (Dutch  , [s]) -> [ Para [ Str ("Deze gegevensverzameling genereert één procesregel. ") ]]++
                              if showPredExpr opts
                              then pandocEqnArrayOnelabel (symDefLabel s) ((showLatex.toPredLogic) s)
                              else [ Para [ Math DisplayMath $ showMath s]]
            (Dutch  , ss ) -> [ Para [ Str "Deze gegevensverzameling genereert de volgende procesregels. " ]
                              , if showPredExpr opts
                                then BulletList [(pandocEqnArrayOnelabel (symDefLabel s) . showLatex . toPredLogic) s | s<-ss ]
                                else BulletList [ [Para [Math DisplayMath $ showMath s]] | s<-ss ]
                              ]
       iRules
        = case fsLang fSpec of
           English -> case irs of
                       []  -> []
                       [e] -> [ Para [ Str "The following rule defines the integrity of data within this data set. It must remain true at all times. " ]]++
                              if showPredExpr opts
                              then pandocEqnArrayOnelabel "" ((showLatex.toPredLogic) e)
                              else [ Para [ Math DisplayMath $ showMath e]]
                       es  -> [ Para [ Str "The following rules define the integrity of data within this data set. They must remain true at all times. " ]
                              , if showPredExpr opts
                                then BulletList [(pandocEqnArrayOnelabel "" . showLatex . toPredLogic) e | e<-es ]
                                else BulletList [ [Para [Math DisplayMath $ showMath e]] | e<-es ]
                              ]
           Dutch   -> case irs of
                       []  -> []
                       [e] -> [ Para [ Str "De volgende regel definieert de integriteit van gegevens binnen deze gegevensverzameling. Hij moet te allen tijde blijven gelden. "]]++
                              if showPredExpr opts
                              then pandocEqnArrayOnelabel "" ((showLatex.toPredLogic) e)
                              else [ Para [ Math DisplayMath $ showMath e]]
                       es  -> [ Para [ Str "De volgende regels definiëren de integriteit van gegevens binnen deze gegevensverzameling. Zij moeten te allen tijde blijven gelden. " ]
                              , if showPredExpr opts
                                then BulletList [(pandocEqnArrayOnelabel "" . showLatex . toPredLogic) e | e<-es ]
                                else BulletList [ [Para [Math DisplayMath $ showMath e]] | e<-es ]
                              ]
          where irs = [ dnf2expr dc
                      | q<-vquads fSpec
                      , r_usr (qRule q)==UserDefined, isIdent (qDcl q), source (qDcl q) `elem` pcpts
                      , x<-qConjuncts q
                      , dc@(Dnf [EDcD nega] _)<-rc_dnfClauses x
                      , qDcl q==nega
                      ]
                pcpts = case p of
                  ScalarSQL{} -> [cLkp p]
                  _           -> map fst (cLkpTbl p)
-}

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
  _   -> fatal 223 ("Have a look at the generated Haskell to see what is going on..\n"++show e)
