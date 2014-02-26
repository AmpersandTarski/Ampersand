{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterDataAnalysis (chpDataAnalysis)
where 
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Output.PredLogic        (PredLogicShow(..), showLatex)
import DatabaseDesign.Ampersand.Output.PandocAux
import DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram --(Class(..),CdAttribute(..))
import Data.List (sortBy)
import Data.Function (on)

fatal :: Int -> String -> a
fatal = fatalMsg "Output.ToPandoc.ChapterDataAnalysis"

------------------------------------------------------------
--DESCR -> the data analysis contains a section for each class diagram in the fspec
--         the class diagram and multiplicity rules are printed
chpDataAnalysis :: Fspc -> Options -> (Blocks,[Picture])
chpDataAnalysis fSpec flags = (theBlocks, thePictures)
 where 
 
  theBlocks 
    =  chptHeader (fsLang fSpec) DataAnalysis  -- The header
    <> (case fsLang fSpec of 
             Dutch   -> para ( text "Dit hoofdstuk bevat het resultaat van de gegevensanalyse. "
                            <> text "De opbouw is als volgt:"
                             )
                     <> para ( if summaryOnly
                               then text "We beginnen met "
                               else text "We beginnen met het classificatiemodel, gevolgd door "
                            <> text "een overzicht van alle relaties, die samen de basis vormen van de rest van deze analyse. "
                            <> text "tenslotte volgen achtereenvolgend het logische- en technische gegevensmodel."
                             )   
             English -> para ( text "This chapter contains the result of the data analisys. "
                            <> text "It is structured as follows:"
                             )
                     <> para ( if summaryOnly
                               then text "We start with "
                               else text "We start with the classification model, followed by "
                            <> text "a list of all relations, that are the foundation of the rest of the analisys. "
                            <> text "Finally, the logical and technical data model are discussed."
                             )   
       )
    <> if summaryOnly then mempty else classificationBlocks
    <> daBasicBlocks
    <> logicalDataModelBlocks
    <> technicalDataModelBlocks
  thePictures
    =  [classificationPicture | not summaryOnly]
    ++ logicalDataModelPictures ++ technicalDataModelPictures

  daBasicBlocks                                          = daBasicsSection           sectionLevel fSpec flags
  (classificationBlocks    , classificationPicture     ) = classificationSection     sectionLevel fSpec flags
  (logicalDataModelBlocks  , logicalDataModelPictures  ) = logicalDataModelSection   sectionLevel fSpec flags
  (technicalDataModelBlocks, technicalDataModelPictures) = technicalDataModelSection sectionLevel fSpec flags
  sectionLevel = 2

  -- | In some cases, only a summary of the data analysis is required as output.
  summaryOnly :: Bool
  summaryOnly = theme flags `elem` [StudentTheme]
  

classificationSection :: Int -> Fspc -> Options -> (Blocks,Picture)
classificationSection lev fSpec flags = (theBlocks,pict)
 where 
  theBlocks =
       header lev (case fsLang fSpec of
                    Dutch   -> text "Classificaties"
                    English -> text "Classifications"
                  )
    <> content
  content = 
    if null (classes classificationModel)
    then para $ text "Er zijn geen classificaties gedefinieerd."
    else para (case fsLang fSpec of 
              Dutch   -> text "Een aantal concepten zit in een classificatiestructuur. "
                       <> (if canXRefer flags 
                           then text "Deze is in figuur " <> xRefReference flags pict <> text "weergegeven."
                           else text "Deze is in onderstaand figuur weergegeven."
                          )
              English -> text "A number of concepts is organized in a classification structure. "
                       <> (if canXRefer flags 
                           then text "This is shown in figure " <> xRefReference flags pict <> text "."
                           else text "This is shown in the figure below."
                          )
            )
         <> para (showImage flags pict)
     
   where
  classificationModel :: ClassDiag
  classificationModel = clAnalysis fSpec flags

  pict :: Picture
  pict = (makePicture flags fSpec Gen_CG classificationModel)
          {caption = case fsLang fSpec of
                       Dutch   ->"Classificatie van "++name fSpec
                       English ->"Classification of "++name fSpec}


logicalDataModelSection :: Int -> Fspc -> Options -> (Blocks,[Picture])
logicalDataModelSection lev fSpec flags = (theBlocks, [pict])
 where
  theBlocks =
       header lev (case fsLang fSpec of
                    Dutch   -> text "Logisch gegevensmodel"
                    English -> text "Logical datamodel"
                )
    <> para (case fsLang fSpec of
               Dutch   -> (text "De afspraken zijn vertaald naar een gegevensmodel. "
                         <> ( if canXRefer flags
                              then text "Dit gegevensmodel is in figuur " <> xRefReference flags pict <> text " weergegeven."
                              else text "Dit gegevensmodel is in onderstaand figuur weergegeven. "
                          ) )
               English -> (text "The functional requirements have been translated into a data model. "
                         <> ( if canXRefer flags
                              then text "This model is shown by figure " <> xRefReference flags pict <> text "."
                              else text "This model is shown by the figure below. "
                          ) )
            )
     <> para (showImage flags pict)
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
  pict = (makePicture flags fSpec Plain_CG oocd)
           {caption = case fsLang fSpec of
                        Dutch   -> "Logisch gegevensmodel van "++name fSpec
                        English -> "Logical data model of "++name fSpec}      
  oocd :: ClassDiag
  oocd = cdAnalysis fSpec flags
  
  detailsOfClass :: Class -> Blocks
  detailsOfClass cl = 
           header (lev+1) ((case fsLang fSpec of
                       Dutch   -> text "Gegevensverzameling: "
                       English -> text "Entity type: "
                     )
                     <> (emph.strong.text.name) cl)
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
                         , assSrc assoc == root || assTgt assoc == root]
                       
    where
     root = case clcpt cl of
       Nothing -> fatal 193 "A class in the logical data model should have a root concept."
       Just c  -> Left c
     assocToRow :: DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram.Association -> Blocks
     assocToRow assoc  =                 
        -- showDummy assoc <>
        if (null.assrhr) assoc
        then fatal 192 "Shouldn't happen: flip the relation for the right direction!"
        else case fsLang fSpec of
           Dutch   -> para (   text "Ieder(e) "
                            <> (emph.text.nm.assSrc) assoc
                            <> let rel = (singleQuoted.text.assrhr) assoc
                                   rel' = text ""
                               in (case assrhm assoc of
                                     Mult MinZero MaxOne  -> text " "  <> rel <> text " maximaal één " 
                                     Mult MinZero MaxMany -> text " "  <> rel <> text " geen tot meerdere "
                                     Mult MinOne  MaxOne  -> text " moet " <> rel <> text " precies één "
                                     Mult MinOne  MaxMany -> text " moet " <> rel <> text " ten minste één "
                                  ) 
                            <> (emph.text.nm.assTgt) assoc 
                            <> text ". Over deze relatie geldt omgekeerd dat "
                            <> text "ieder(e) "
                            <> (emph.text.nm.assTgt) assoc
                            <> (case asslhm assoc of
                                     Mult MinZero MaxOne  -> text " "  <> rel' <> text " maximaal één " 
                                     Mult MinZero MaxMany -> text " "  <> rel' <> text " geen tot meerdere "
                                     Mult MinOne  MaxOne  -> text " moet " <> rel' <> text " precies één "
                                     Mult MinOne  MaxMany -> text " moet " <> rel' <> text " ten minste één "
                                  )
                            <> (emph.text.nm.assSrc) assoc
                            <> text " kan hebben."
                           )
           English -> para (   text "Every "
                            <> (emph.text.nm.assSrc) assoc
                            <> let rel = (singleQuoted.text.assrhr) assoc
                                   rel' = text ""
                               in (case assrhm assoc of
                                     Mult MinZero MaxOne  -> text " "  <> rel <> text " at most one " 
                                     Mult MinZero MaxMany -> text " "  <> rel <> text " zero or more "
                                     Mult MinOne  MaxOne  -> text " must " <> rel <> text " exactly one "
                                     Mult MinOne  MaxMany -> text " must " <> rel <> text " at least one "
                                  ) 
                            <> (emph.text.nm.assTgt) assoc 
                            <> text ". For the other way round, for this relation holds that "
                            <> text "each "
                            <> (emph.text.nm.assTgt) assoc
                            <> (case asslhm assoc of
                                     Mult MinZero MaxOne  -> text " "  <> rel' <> text " at most one " 
                                     Mult MinZero MaxMany -> text " "  <> rel' <> text " zero or more "
                                     Mult MinOne  MaxOne  -> text " must " <> rel' <> text " exactly one "
                                     Mult MinOne  MaxMany -> text " must " <> rel' <> text " at least one "
                                  )
                            <> (emph.text.nm.assSrc) assoc
                            <> text "."
                           )
             
     showDummy :: DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram.Association -> Blocks
     showDummy assoc = --(plain.text.show) assoc 
             (para.text) ("assSrc: "++(nm.assSrc) assoc)
          <> (para.text) ("asslhm: "++(show.asslhm) assoc)
          <> (para.text) ("asslhr: "++(     asslhr) assoc)
          <> (para.text) ("assTgt: "++(nm.assTgt) assoc)
          <> (para.text) ("assrhm: "++(show.assrhm) assoc)
          <> (para.text) ("assrhr: "++(     assrhr) assoc)

     nm :: Identified a => Either a String -> String
     nm x = case x of 
              Left c  -> name c
              Right s -> s

technicalDataModelSection :: Int -> Fspc -> Options -> (Blocks,[Picture])
technicalDataModelSection lev fSpec flags = (theBlocks,[pict])
 where 
   theBlocks =
       header lev (case fsLang fSpec of
                    Dutch   -> text "Technisch datamodel"
                    English -> text "Technical datamodel"
                      )
    <> para (case fsLang fSpec of
               Dutch   -> (text "De afspraken zijn vertaald naar een technisch datamodel. "
                         <> ( if canXRefer flags
                              then text "Dit model is in figuur " <> xRefReference flags pict <> text " weergegeven."
                              else text "Dit model is in onderstaand figuur weergegeven. "
                          ) )
               English -> (text "The functional requirements have been translated into a technical data model. "
                         <> ( if canXRefer flags
                              then text "This model is shown by figure " <> xRefReference flags pict <> text "."
                              else text "This model is shown by the figure below. "
                          ) )
            )
    <> para (showImage flags pict)
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
                          (Dutch  , InternalPlug{}) -> text "Tabel: "
                          (Dutch  , ExternalPlug{}) -> text "Dataservice: "
                          (English, InternalPlug{}) -> text "Table: "
                          (English, ExternalPlug{}) -> text "Data service: "
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
                           ->  text "Dit is een koppeltabel, die "
                            <> primExpr2pandocMath (fsLang fSpec) (mLkp bin)
                            <> text " implementeert. De tabel bestaat uit de volgende kolommen:"      
                           
                         English
                           ->  text "This is a link-table, implementing "
                            <> primExpr2pandocMath (fsLang fSpec) (mLkp bin)
                            <> text ". It contains the following columns:"  
                       )
                     <> showFields (plugFields bin)
                   
                  
             InternalPlug ScalarSQL{} 
                -> mempty
             ExternalPlug _
               -> case fsLang fSpec of
                    Dutch   -> para (text "De details van deze service zijn in dit document (nog) niet verder uitgewerkt.")
                    English -> para (text "The details of this dataservice are not available in this document.")
      showFields :: [SqlField] -> Blocks
      showFields flds = bulletList (map showField flds)
        where 
          eRelIs = [c | EDcI c <- map fldexpr flds]
          showField fld =
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
                                Dutch   -> text ("Dit attribuut is de primaire sleutel. ")
                                English -> text ("This attribute is the primary key. ")
                          else 
                          case mForeignKey of 
                           Just c ->  case fsLang fSpec of
                                         Dutch   -> text ("Dit attribuut verwijst naar een voorkomen in de tabel ")
                                         English -> text ("This attribute is a foreign key to ")
                                     <> (text.name) c
                           Nothing -- (no foreign key...)
                             -> --if isBool
                                --then 
                                --else
                                  (case fsLang fSpec of
                                     Dutch   -> text ("Dit attribuut implementeert ")
                                     English -> text ("This attribute implements ")
                                  <> primExpr2pandocMath (fsLang fSpec) (fldexpr fld)
                                  <> text "."
                                  )
                         )
                      <> linebreak 
                      <> (code.show.fldtype) fld
                      <> text ", "
                      <> (case fsLang fSpec of
                            Dutch 
                              -> text (if fldnull fld then "Optioneel" else "Verplicht")
                               <>text (if flduniq fld then ", Uniek" else "")
                               <>text "."
                            English 
                              -> text (if fldnull fld then "Optional" else "Mandatory")
                               <>text (if flduniq fld then ", Unique" else "")
                               <>text "."
                         )
                     )
   pict :: Picture
   pict = (makePicture flags fSpec Plain_CG oocd)
            {caption = case fsLang fSpec of
                         Dutch   -> "Technisch gegevensmodel van "++name fSpec
                         English -> "Technical data model of "++name fSpec}      
   oocd :: ClassDiag
   oocd = tdAnalysis fSpec flags
  
                   
             
daBasicsSection :: Int -> Fspc -> Options -> Blocks
-- | The function daBasicsSection lists the basic sentences that have been used in assembling the data model.
daBasicsSection lev fSpec flags = theBlocks
 where 
  theBlocks =
       header lev (case fsLang fSpec of
                    Dutch   -> text "Basiszinnen"
                    English -> text "Fact types"
                  )
   <> case fsLang fSpec of 
        Dutch   -> para (text "In deze paragraaf worden de basiszinnen opgesomd, die een rol spelen bij het ontwerp van de gegevensstructuur. "
                       <>text "Per basiszin wordt de naam en het bron- en doelconcept gegeven, alsook de eigenschappen van deze relatie."
                        )
        English -> para (text "This section enumerates the fact types, that have been used in the design of the datastructure. "
                       <>text "For each fact type its name, the source and target concept and the properties are documented."
                        )       
   <> definitionList (map toDef declsInRelevantThemes)
-- HJO, 20130526: onderstaande tabel vervangen door bovenstaande lijst, ivm leesbaarheid.
--   <>  table 
--        (-- caption -- 
--         case fsLang fSpec of
--           Dutch   -> text "Deze tabel bevat de basiszinnen, die gebruikt zijn als basis voor de gegevensanalyse."
--           English -> text "This table lists the fact types, that have been used in assembling the data models."
--        )
--        [(AlignLeft,0.4)       , (AlignCenter, 0.4)         , (AlignCenter, 0.2)]
--        ( case fsLang fSpec of
--           Dutch   -> [plain (text "Relatie") , plain (text "Beschrijving"), plain (text "Eigenschappen")]
--           English -> [plain (text "Relation"), plain (text "Description") , plain (text "Properties")   ]
--        ) 
--        (map toRow declsInRelevantThemes)
    where
      declsInRelevantThemes = 
        -- a declaration is considered relevant iff it is declared or used in one of the relevant themes.
         [d | d<-declarations fSpec
         , decusr d
         , (  decpat d `elem` themes fSpec  
               || d `elem` declsUsedIn [p | p<-            patterns fSpec   , name p `elem` themes fSpec]
               || d `elem` declsUsedIn [p | p<-map fpProc (vprocesses fSpec), name p `elem` themes fSpec]
           )
         ]
      toRow :: Declaration -> [Blocks]
      toRow d
        = [ (plain.math.showMath) d
          , fromList (meaning2Blocks (fsLang fSpec) d)
          , plain ( inlineIntercalate (str ", ") [ text (showADL m) | m <-multiplicities d])
          ]
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

-- The properties of various declarations are documented in different tables.
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
        hMults  = [decl | decl@Sgn{}<- declsUsedIn fSpec, isEndo decl
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

-}
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
{-     where
--  voorgestelde multipliciteitenanalyse....
      clauses = nub [clause | Quad _ ccrs<-vquads fSpec, (_,dnfClauses)<-cl_conjNF ccrs, clause<-dnfClauses]
      is = nub [r | EUni fus<-clauses
                  , isIdent (EIsc [notCpl f | f<-fus, isPos f] sgn)
                  , f<-filter isNeg fus
                  , s<-strands f
                  , e<-[head s, flp (last s)]
                  , r<-declsUsedIn e
                ]
      ts = nub [r | EUni fus<-clauses
                  , isIdent (EIsc [notCpl f | f<-fus, isNeg f] sgn)
                  , f<-filter isPos fus
                  , s<-strands f
                  , e<-[head s, flp (last s)]
                  , r<-declsUsedIn e
                  ]
      strands (ECps fs) = [fs]
      strands _      = []    -- <--  we could maybe do better than this...
      tots = [d | t<-ts, inline t, d<-map makeDeclaration (declsUsedIn t)]
      unis = [d | t<-is, inline t, d<-map makeDeclaration (declsUsedIn t)]
      surs = [d | t<-ts, not (inline t), d<-map makeDeclaration (declsUsedIn t)]
      injs = [d | t<-is, not (inline t), d<-map makeDeclaration (declsUsedIn t)]
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
       thing2block r = pandocEqnArrayOnelabel (symDefLabel r) ((showLatex.toPredLogic) r)
       plugHeader = toList $ labeledThing flags (lev+1) ("sct:Plug "++escapeNonAlphaNum (name p)) (name p)
       content = daAttributes p ++ plugRules ++ plugSignals ++ plugIdentities ++ iRules
       plugRules
        = case fsLang fSpec of
           English -> case [r | r<-invariants fSpec, null (declsUsedIn r >- declsUsedIn p)] of
                       []  -> []
                       [r] -> [ Para [ Str "Within this data set, the following integrity rule shall be true at all times. " ]]++
                              if showPredExpr flags
                              then pandocEqnArrayOnelabel (symDefLabel r) ((showLatex.toPredLogic) r)
                              else [ Para [ Math DisplayMath $ showMath r]]
                       rs  -> [ Para [ Str "Within this data set, the following integrity rules shall be true at all times. " ]
                              , if showPredExpr flags
                                then BulletList [(pandocEqnArrayOnelabel (symDefLabel r) . showLatex . toPredLogic) r | r<-rs ]
                                else BulletList [ [Para [Math DisplayMath $ showMath r]] | r<-rs ]
                              ]
           Dutch   -> case [r | r<-invariants fSpec, null (declsUsedIn r >- declsUsedIn p)] of
                       []  -> []
                       [r] -> [ Para [ Str "Binnen deze gegevensverzameling dient de volgende integriteitsregel te allen tijde waar te zijn. " ]]++
                              if showPredExpr flags
                              then pandocEqnArrayOnelabel (symDefLabel r) ((showLatex.toPredLogic) r)
                              else [ Para [ Math DisplayMath $ showMath r]]
                       rs  -> [ Para [ Str "Binnen deze gegevensverzameling dienen de volgende integriteitsregels te allen tijde waar te zijn. " ]
                              , if showPredExpr flags
                                then BulletList [(pandocEqnArrayOnelabel (symDefLabel r) . showLatex . toPredLogic) r | r<-rs ]
                                else BulletList [ [Para [Math DisplayMath $ showMath r]] | r<-rs ]
                              ]
       plugIdentities
        = case fsLang fSpec of
           English -> case [k | k<-identityRules fSpec, null (declsUsedIn k >- declsUsedIn p)] of
                       []  -> []
                       [s] -> [ Para [ Str "This data set contains one key. " ]]++
                              if showPredExpr flags
                              then pandocEqnArrayOnelabel (symDefLabel s) ((showLatex.toPredLogic) s)
                              else [ Para [ Math DisplayMath $ showMath s]]
                       ss  -> [ Para [ Str "This data set contains the following keys. " ]
                              , if showPredExpr flags
                                then BulletList [(pandocEqnArrayOnelabel (symDefLabel s) . showLatex . toPredLogic) s | s<-ss ]
                                else BulletList [ [Para [Math DisplayMath $ showMath s]]
                                                | s<-ss ]
                              ]
           Dutch   -> case [k | k<-identityRules fSpec, null (declsUsedIn k >- declsUsedIn p)] of
                       []  -> []
                       [s] -> [ Para [ Str ("Deze gegevensverzameling genereert één key. ") ]]++
                              if showPredExpr flags
                              then pandocEqnArrayOnelabel (symDefLabel s) ((showLatex.toPredLogic) s)
                              else [ Para [ Math DisplayMath $ showMath s]]
                       ss  -> [ Para [ Str "Deze gegevensverzameling genereert de volgende keys. " ]
                              , if showPredExpr flags
                                then BulletList [(pandocEqnArrayOnelabel (symDefLabel s) . showLatex . toPredLogic) s | s<-ss ]
                                else BulletList [ [Para [Math DisplayMath $ showMath s]] | s<-ss ]
                              ]
       plugSignals
        = case (fsLang fSpec, [r | r<-vrules fSpec, isSignal r , null (declsUsedIn r >- declsUsedIn p)]) of
    --       English -> case [r | r<-vrules fSpec, isSignal r , null (declsUsedIn r >- declsUsedIn p)] of
            (_      , [])  -> []
            (English, [s]) -> [ Para [ Str "This data set generates one process rule. " ]]++
                              if showPredExpr flags
                              then pandocEqnArrayOnelabel (symDefLabel s) ((showLatex.toPredLogic) s)
                              else [ Para [ Math DisplayMath $ showMath s]]
            (English, ss)  -> [  Para [ Str "This data set generates the following process rules. " ]
                              , if showPredExpr flags
                                then BulletList [(pandocEqnArrayOnelabel (symDefLabel s) . showLatex . toPredLogic) s | s<-ss ]
                                else BulletList [ [Para [Math DisplayMath $ showMath s]] | s<-ss ]
                              ]
            (Dutch  , [s]) -> [ Para [ Str ("Deze gegevensverzameling genereert één procesregel. ") ]]++
                              if showPredExpr flags
                              then pandocEqnArrayOnelabel (symDefLabel s) ((showLatex.toPredLogic) s)
                              else [ Para [ Math DisplayMath $ showMath s]]
            (Dutch  , ss ) -> [ Para [ Str "Deze gegevensverzameling genereert de volgende procesregels. " ]
                              , if showPredExpr flags
                                then BulletList [(pandocEqnArrayOnelabel (symDefLabel s) . showLatex . toPredLogic) s | s<-ss ]
                                else BulletList [ [Para [Math DisplayMath $ showMath s]] | s<-ss ]
                              ]
       iRules
        = case fsLang fSpec of
           English -> case irs of
                       []  -> []
                       [e] -> [ Para [ Str "The following rule defines the integrity of data within this data set. It must remain true at all times. " ]]++
                              if showPredExpr flags
                              then pandocEqnArrayOnelabel "" ((showLatex.toPredLogic) e)
                              else [ Para [ Math DisplayMath $ showMath e]]
                       es  -> [ Para [ Str "The following rules define the integrity of data within this data set. They must remain true at all times. " ]
                              , if showPredExpr flags
                                then BulletList [(pandocEqnArrayOnelabel "" . showLatex . toPredLogic) e | e<-es ]
                                else BulletList [ [Para [Math DisplayMath $ showMath e]] | e<-es ]
                              ]
           Dutch   -> case irs of
                       []  -> []
                       [e] -> [ Para [ Str "De volgende regel definieert de integriteit van gegevens binnen deze gegevensverzameling. Hij moet te allen tijde blijven gelden. "]]++
                              if showPredExpr flags
                              then pandocEqnArrayOnelabel "" ((showLatex.toPredLogic) e)
                              else [ Para [ Math DisplayMath $ showMath e]]
                       es  -> [ Para [ Str "De volgende regels definiëren de integriteit van gegevens binnen deze gegevensverzameling. Zij moeten te allen tijde blijven gelden. " ]
                              , if showPredExpr flags
                                then BulletList [(pandocEqnArrayOnelabel "" . showLatex . toPredLogic) e | e<-es ]
                                else BulletList [ [Para [Math DisplayMath $ showMath e]] | e<-es ]
                              ]
          where irs = [ dnf2expr dc
                      | Quad r ccrs<-vquads fSpec
                      , r_usr (cl_rule ccrs)==UserDefined, isIdent r, source r `elem` pcpts
                      , x<-cl_conjNF ccrs
                      , dc@(Dnf [EDcD nega] _)<-rc_dnfClauses x
                      , r==nega
                      ]
                pcpts = case p of
                  ScalarSQL{} -> [cLkp p]
                  _           -> map fst (cLkpTbl p)

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
