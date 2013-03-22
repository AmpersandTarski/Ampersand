{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterDataAnalysis
where 
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Output.PredLogic        (PredLogicShow(..), showLatex)
import DatabaseDesign.Ampersand.Output.PandocAux
import DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram --(Class(..),CdAttribute(..))
import Data.List (nub,sortBy)
import Data.Function (on)

fatal :: Int -> String -> a
fatal = fatalMsg "Output.ToPandoc.ChapterDataAnalysis.hs"

------------------------------------------------------------
--DESCR -> the data analysis contains a section for each class diagram in the fspec
--         the class diagram and multiplicity rules are printed
chpDataAnalysis :: Int -> Fspc -> Options -> (Blocks,[Picture])
chpDataAnalysis lev fSpec flags = (theBlocks, thePictures)
 where 
  -- | In some cases, only a summary of the data analysis is required as output.
  summaryOnly :: Bool
  summaryOnly = theme flags `elem` [StudentTheme]
  
  -- | The user can specify that only specific themes should be taken into account 
  -- in the output. However, when no themes are specified, all themes are relevant.
  relevantThemes = if null (themes fSpec)
                   then map name (patterns fSpec) ++ map name (vprocesses fSpec)
                   else themes fSpec
  
  classificationModel :: Maybe ClassDiag
  classificationModel = if summaryOnly
                        then Nothing
                        else clAnalysis fSpec flags

  classificationPicture :: ClassDiag -> Picture
  classificationPicture cl
     = (makePicture flags fSpec Gen_CG cl)
          {caption = case language flags of
                       Dutch   ->"Classificatie van "++name fSpec
                       English ->"Classification of "++name fSpec}

  classificationBlocks :: ClassDiag -> Picture -> Blocks
  classificationBlocks cl pict = 
       header 2 (case language flags of
                  Dutch   -> text "Classificaties"
                  English -> text "Classifications"
                )
    <> para (case language flags of 
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
    

  logicalDataModel :: ClassDiag
  logicalDataModel = cdAnalysis fSpec flags
  
  logicalDataModelPicture :: ClassDiag -> Picture
  logicalDataModelPicture cd
                 = (makePicture flags fSpec Plain_CG cd)
                        {caption = case language flags of
                                    Dutch   ->"Datamodel van "++name fSpec
                                    English ->"Data model of "++name fSpec}
  
  logicalDataModelBlocks :: ClassDiag -> Picture -> Blocks
  logicalDataModelBlocks dm pict =
       header 2 (case language flags of
                    Dutch   -> text "Logisch gegevensmodel"
                    English -> text "Logical datamodel"
                )
    <> para (case language flags of
               Dutch   -> (text "De functionele eisen zijn vertaald naar een gegevensmodel. "
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
     <> if summaryOnly 
        then mempty
        else let nrOfClasses = length (classes dm)
             in case language flags of
                  Dutch   -> para (case nrOfClasses of
                                     0 -> text "Er zijn geen gegevensverzamelingen."
                                     1 -> text "Er is één gegevensverzameling, die in de volgende paragraaf in detail is beschreven:"
                                     _ -> text ("Er zijn "++count flags nrOfClasses "gegevensverzameling"++". ") 
                                       <> text "De details van elk van deze gegevensverzameling worden, op alfabetische volgorde, in de nu volgende paragrafen beschreven:"
                                  )
                  English -> para (case nrOfClasses of
                                     0 -> text "There are no entity types."
                                     1 -> text "There is only one entity type:"
                                     _ -> text ("There are "++count flags nrOfClasses "entity type" ++".")
                                       <> text "The details of each entity type are described (in alfabetical order) in the following paragraphs:"
                                  )
          <> mconcat [detailsOfClass cd | cd <- sortBy (compare `on` name) (classes dm)]
        
        
    where
      detailsOfClass :: Class -> Blocks
      detailsOfClass cl = 
           header 3 ((case language flags of
                       Dutch   -> text "Gegevensverzameling: "
                       English -> text "Entity type: "
                     )
                     <> (emph.strong.text.name) cl)
        <> case language flags of 
             Dutch   -> para $ text "Deze gegevensverzameling bevat de volgende attributen: "
             English -> para $ text "This entity type has the following attributes: "
        <> simpleTable (case language flags of
                          Dutch   -> [(plain.text) "Attribuut"
                                     ,(plain.text) "Type"
                                     ,mempty
                                     ]
                          English -> [(plain.text) "Attribute"
                                     ,(plain.text) "Type"
                                     ,mempty
                                     ]
                       )
                       [ [ (plain.text.name) attr
                         , (plain.text.attTyp) attr
                         , (plain.text) $ case (language flags,attOptional attr) of
                                            (Dutch  ,True ) -> "Optioneel"
                                            (English,True ) -> "Optional"
                                            (Dutch  ,False) -> "Verplicht"
                                            (English,False) -> "Mandatory"
                         ]
                         | attr <- clAtts cl]  
        <> case language flags of 
             Dutch   -> para ( text (name cl) <> text " heeft de volgende associaties: ")
             English -> para ( text (name cl) <> text " has the following associations: ")
        <> simpleTable (case language flags of
                          Dutch   -> [(para.text) "Associatie"
                                     ]
                          English -> [(para.text) "Association"
                                     ]
                       )
                       [ [ if (not.null.assrhr) assoc && assSrc assoc == clcpt cl
                           then case language flags of
                                  Dutch   -> para (   text (case assrhm assoc of
                                                              Mult MinZero _ -> "Sommige "
                                                              Mult MinOne  _ -> "Elk(e) "
                                                           ) 
                                                   <> (emph.text.name.assSrc) assoc
                                                   <> text " "
                                                   <> (singleQuoted.text.     assrhr) assoc
                                                   <> text (case assrhm assoc of
                                                              Mult _ MaxOne  -> fatal 167 "Deze zou als attribuut opgenomen moeten zijn in de Class!"
                                                              Mult _ MaxMany -> " één of meerdere "
                                                           ) 
                                                   <> (emph .text.name.assTrg) assoc
                                                  )
                           else 
                           if (not.null.asslhr) assoc && assTrg assoc == clcpt cl
                           then case language flags of
                                  Dutch   -> para ( text "---TODO---")
                           else (para.text) "Dit zou niet voor moeten komen!"
                            --      English -> para ()
--                         , (para.text.name.assSrc) assoc
--                         , (para.text.show.asslhm) assoc
--                         , (para.text.     asslhr) assoc
--                         , (para.text.name.assTrg) assoc
--                         , (para.text.show.assrhm) assoc
--                         , (para.text.     assrhr) assoc
                         ] 

                         | assoc <- assocs dm, assSrc assoc == clcpt cl || assTrg assoc == clcpt cl]  
               
               
                
  technicalDataModelBlocks :: Blocks
  technicalDataModelBlocks =
       header 2 (case language flags of
                    Dutch   -> text "Technisch datamodel"
                    English -> text "Technical datamodel"
                )
    <> para (let nrOfTables = length (plugInfos fSpec) in
             case language flags of
        Dutch   -> text ("Het technisch datamodel bestaat uit de volgende "++show nrOfTables++" tabellen:")
        English -> text ("The technical datamodel consists of the following "++show nrOfTables++"tables:")
            )
    <> mconcat [detailsOfplug p | p <- sortBy (compare `on` name) (plugInfos fSpec)]
    where
      detailsOfplug :: PlugInfo -> Blocks
      detailsOfplug p = 
           header 3 (   case (language flags, p) of
                          (Dutch  , InternalPlug{}) -> text "Tabel: "
                          (Dutch  , ExternalPlug{}) -> text "Dataservice: "
                          (English, InternalPlug{}) -> text "Table: "
                          (English, ExternalPlug{}) -> text "Data service: "
                     <> text (name p)
                    ) 
        <> case p of
             InternalPlug tbl@TblSQL{} 
               -> (case language flags of
                Dutch 
                   -> para (text $ "Deze tabel heeft de volgende "++(show.length.fields) tbl++" velden:")
                English 
                   -> para (text $ "This table has the following "++(show.length.fields) tbl++" fields:")
                  )
               <> showFields (plugFields tbl)
             InternalPlug bin@BinSQL{} 
               -> (let rel = case mLkp bin of
                               ERel r _ -> r
                               _        -> fatal 254 "relation unknown. contact your dealer!"
                   in para 
                       (case language flags of
                         Dutch
                           ->  text "Dit is een koppeltabel, die "
                            <> mathRel (mLkp bin)
                            <> text " implementeert. De tabel bestaat uit de volgende kolommen:"      
                           
                         English
                           ->  text "This is a link-table, implementing "
                            <> mathRel (mLkp bin)
                            <> text ". It contains the following columns:"  
                       )
                     <> showFields (plugFields bin)
                   )
                  
             InternalPlug sclr@ScalarSQL{} 
               -> para (case language flags of
                          Dutch   -> text "Dit is een enumeratie. Deze tabel heeft "
                                   <>text "slechts één kolom:"
                          English -> text "This table contains an enumeration. "
                                   <>text "This table has a single column only:"
                       )
               <> showFields (plugFields sclr)
             ExternalPlug _
               -> case language flags of
                    Dutch   -> para (text "De details van deze service zijn in dit document (nog) niet verder uitgewerkt.")
                    English -> para (text "The details of this dataservcie are not available in this document.")
      showFields :: [SqlField] -> Blocks
      showFields flds = bulletList (map showField flds)
        where 
          eRelIs = [c | ERel (I c@C{}) _ <- map fldexpr flds]
          showField fld =
             let isPrimeryKey = case fldexpr fld of
                                  ERel (I c@C{}) _ -> foldl1 join eRelIs == c 
                                  _                -> False 
                 mForeignKey  = case fldexpr fld of
                                  EIsc (ERel (I c) _ ,_ ) _ -> Just c
                                  _                         -> Nothing  
             in para (  (strong.text.fldname) fld
                      <> linebreak 
    
                      <>   (code.show.fldtype) fld
                      <> text ", "
                      <> (case language flags of
                            Dutch 
                              -> text (if fldnull fld then "Optioneel" else "Verplicht")
                               <>text (if flduniq fld then ", Uniek" else "")
                               <>text "."
                            English 
                              -> text (if fldnull fld then "Optional" else "Mandatory")
                               <>text (if flduniq fld then ", Unique" else "")
                               <>text "."
                         )
                      <> linebreak
                      <> (strong.code.show.flduse) fld
                      <> linebreak  
                      <> (if isPrimeryKey 
                          then case language flags of
                                Dutch   -> text ("Dit attribuut is de primaire sleutel. ")
                                English -> text ("This attribute is the primary key. ")
                          else 
                          case mForeignKey of 
                           Just c ->  case language flags of
                                         Dutch   -> text ("Dit attribuut verwijst naar een voorkomen in de tabel ")
                                         English -> text ("This attribute is a foreign key to ")
                                     <> (text.name) c
                           Nothing -- (no foreign key...)
                             -> --if isBool
                                --then 
                                --else
                                  (case language flags of
                                     Dutch   -> text ("Dit attribuut implementeert ")
                                     English -> text ("This attribute implements ")
                                  <> mathRel (fldexpr fld)
                                  <> text "."
                                  )
                         )
                     )
                   
             
  thePictures 
    = logicalDataModelPicture logicalDataModel
    : case classificationModel of
         Nothing -> []
         Just cl -> [classificationPicture cl] 

  theBlocks 
    =  chptHeader flags DataAnalysis  -- The header
    <> (case language flags of 
             Dutch   -> para $ text "Dit hoofdstuk is nog niet uit-ontwikkeld!"
             English -> para $ text "This chapter needs more work. It is work in progress!"
       )
    <> (case classificationModel of --If there is a classification model, show it in the output
          Nothing -> mempty
          Just cl -> classificationBlocks cl (classificationPicture cl)
       )
    <> -- Fact type table containing the relevant relations:
       daBasics [d | d<-declarations fSpec
                   , decusr d
                   , (  decpat d `elem` relevantThemes
                     || d `elem` [r | pat<-patterns fSpec  , name pat `elem` relevantThemes, r@Sgn{}<-declsUsedIn pat]
                     || d `elem` [r | prc<-vprocesses fSpec, name prc `elem` relevantThemes, r@Sgn{}<-declsUsedIn (fpProc prc)]
                     )
                ]
    
    <> logicalDataModelBlocks logicalDataModel (logicalDataModelPicture logicalDataModel) 
    <> horizontalRule 
    <> technicalDataModelBlocks
    <> horizontalRule 

  remainingRels = (if null (themes fSpec)
                   then declsUsedIn fSpec
                   else [d | d@Sgn{}<-declsUsedIn fSpec, decpat d `elem` themes fSpec]
                  ) >- [r | p<-plugInfos fSpec, r<-declsUsedIn p]



--  theRest :: [a]
--  theRest
--   = fromList (
--       daAssociations remainingRels ++
--       [b | p<-entities, b<-daPlug p]
--              )
 
 -- The declarations that are used in entities need not be drawn in pictures, because they are attributes.
  entities = if null (themes fSpec)
             then [p | InternalPlug p<-plugInfos fSpec]
             else nub [p | c<-concs remainingRels, (p,_)<-lookupCpt fSpec c ]
             
        
  
  -- | a short summary of the statistics. This text also serves as an introduction
--  daIntro :: Lang -> Blocks
--  daIntro lang = fromList $ intro lang ++ classificationPictures ++ classDiagramPictureParagraph
--    where 
--     intro Dutch =
--                [Para $
--                  ( if genGraphics flags 
--                    then 
--                     ( if null [() | Just _<-[classification]] then [] else   -- if there is no classification, print nothing
--                       [ Str "Een aantal concepten uit hoofdstuk "
--                       , xrefReference FunctReqts
--                       , Str " zit in een classificatiestructuur. Deze is in figuur "
--                       , xrefReference classificationPicture
--                       , Str " weergegeven. " ] 
--                     ) ++
--                     [ Str "De eisen, die in hoofdstuk "
--                     , xrefReference FunctReqts
--                     , Str " beschreven zijn, zijn in een gegevensanalyse vertaald naar het gegevensmodel van figuur "
--                     , xrefReference classDiagramPicture
--                     , Str ". " ]
--                    else []
--                  )++
--                  [ Str (case length (classes classDiagram) of
--                            0 -> "Er zijn"
--                            1 -> "Er is één gegevensverzameling,"
--                            _ -> "Er zijn "++count flags (length (classes classDiagram)) "gegevensverzameling"++","
--                        )
--                  , Space, Str $ count flags (length (assocs classDiagram)) "associatie" ]++
--                  ( case classification of
--                     Nothing -> []
--                     Just cl -> [ Str $ ", "++count flags (length (geners cl)) "generalisatie" ] ) ++
--                  [ Str $ " en "++count flags (length (aggrs classDiagram)) "aggregatie"++"."
--                  , Str $ " "++name fSpec++" kent in totaal "++count flags (length (concs fSpec)) "concept"++"."
--                  ]]
--     intro English = [Para $
--                  ( if genGraphics flags 
--                    then 
--                     ( if null [() | Just _<-[classification]] then [] else   -- if there is no classification, print nothing
--                       [ Str "A number of concepts from chapter "
--                       , xrefReference FunctReqts
--                       , Str " is organized in a classification structure. This is represented in figure "
--                       , xrefReference classificationPicture
--                       , Str ". " ] ) ++
--                     [ Str "The requirements, which are listed in chapter "
--                     , xrefReference FunctReqts
--                     , Str ", have been translated into the data model in figure "
--                     , xrefReference classDiagramPicture
--                     , Str ". " ]
--                    else []
--                  )++
--                  [ Str (case length (classes classDiagram) of
--                            0 -> "There are"
--                            1 -> "There is one data set,"
--                            _ -> "There are "++count flags (length (classes classDiagram)) "data set"++","
--                        )
--                  , Space, Str $ count flags (length (assocs classDiagram)) "association"]++
--                  ( case classification of
--                     Nothing -> []
--                     Just cl -> [ Str $ ", "++count flags (length (geners cl)) "generalisation"++"," ] ) ++
--                  [ Str $ " and "++count flags (length (aggrs classDiagram)) "aggregation"++"."
--                  , Str $ " "++name fSpec++" has a total of "++count flags (length (concs fSpec)) "concept"++"."
--                  ]] --TODO
--  classificationPictures :: [Block]
--  classificationPictures = [ Plain $ xrefFigure1 classificationPicture | Just _<-[classification]]
--  
--  classDiagramPictureParagraph :: [Block]
--  classDiagramPictureParagraph = [ Plain $ xrefFigure1 classDiagramPicture ]  -- TODO: explain all multiplicities]
--
--  daPicsOnly :: [Block]
--  daPicsOnly = 
--   (case language flags of
--     Dutch   -> [Para
--                  ( if genGraphics flags 
--                    then 
--                     ( if null [() | Just _<-[classification]] then [] else   -- if there is no classification, print nothing
--                       [ Str "Een aantal concepten uit hoofdstuk "
--                       , xrefReference FunctReqts
--                       , Str " zit in een classificatiestructuur. Deze is in figuur "
--                       , xrefReference classificationPicture
--                       , Str " weergegeven. " ] 
--                     ) ++
--                     [ Str "De eisen, die in hoofdstuk "
--                     , xrefReference FunctReqts
--                     , Str " beschreven zijn, zijn in een gegevensanalyse vertaald naar het gegevensmodel van figuur "
--                     , xrefReference classDiagramPicture
--                     , Str ". " ]
--                    else []
--                  )]
--     English -> [Para
--                  ( if genGraphics flags 
--                    then 
--                     ( if null [() | Just _<-[classification]] then [] else   -- if there is no classification, print nothing
--                       [ Str "A number of concepts from chapter "
--                       , xrefReference FunctReqts
--                       , Str " is organized in a classification structure. This is represented in figure "
--                       , xrefReference classificationPicture
--                       , Str ". " ] ) ++
--                     [ Str "The requirements, which are listed in chapter "
--                     , xrefReference FunctReqts
--                     , Str ", have been translated into the data model in figure "
--                     , xrefReference classDiagramPicture
--                     , Str ". " ]
--                    else []
--                  )]
--   ) ++ [ Plain $ xrefFigure1 classificationPicture | Just _<-[classification]] ++[ Plain $ xrefFigure1 classDiagramPicture ]


{- The switchboard should probably not be in the chapter "Data analysis" 
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
           Dutch   -> [Para [ Str "Figuur ", xrefReference picSwitchboard
                            , Str " geeft het schakelpaneel (switchboard diagram) weer. "
                            , Str "Dit wordt gebruikt bij het ontwerpen van de database functionaliteit."] ]
           English -> [Para [ Str "Figure ", xrefReference picSwitchboard
                            , Str " shows the switchboard diagram."
                            , Str "This is used in designing the database functionality."] ]
     )
     ++ [Plain (xrefFigure1 picSwitchboard)]                     -- draw the switchboard
-}

  -- | The function daBasics lists the basic sentences that have been used in assembling the data model.
  daBasics :: [Declaration] -> Blocks
  daBasics ds
   | summaryOnly = mempty
   | otherwise   =
      table 
        (-- caption -- 
         case language flags of
           Dutch   -> text "Deze tabel bevat de basiszinnen, die gebruikt zijn om het gegevensmodel te genereren."
           English -> text "This table contains the fact types, used to generate the datamodel."
        )
        [(AlignLeft,0.4)       , (AlignCenter, 0.4)         , (AlignCenter, 0.2)]
        ( case language flags of
           Dutch   -> [plain (text "Relatie") , plain (text "Beschrijving"), plain (text "Eigenschappen")]
           English -> [plain (text "Relation"), plain (text "Description") , plain (text "Properties")   ]
        ) 
        (map toRow ds)
    where
      toRow :: Declaration -> [Blocks]
      toRow d
        = [ (plain.math.showMath) d
          , fromList (meaning2Blocks (language flags) d)
          ,  plain ( inlineIntercalate (str ", ") [ text (showADL m) | m <-multiplicities d])
          ]

-- The properties of various declarations are documented in different tables.
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
               [r] -> [ case language flags of
                          Dutch   ->
                             Para [ Str $ upCap (name fSpec)++" heeft "++count flags (length rs) "associatie"++". "
                                  , Str $ " Daarvan is "++showADL r++if isTot r then "totaal" else "surjectief"
                                  ]
                          English   ->
                            Para [ Str $ upCap (name fSpec)++" has "++count flags (length rs) "association"++". "
                                  , Str $ " Association "++showADL r++" is "++if isTot r then "total" else "surjective"
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
       else [ Para (case language flags of
                          Dutch   ->
                            [Str "Er is één endorelatie, ", Math InlineMath (showMath d), Str " met de volgende eigenschappen: "]
                          English   ->
                            [Str "There is one endorelation, ", Math InlineMath (showMath d), Str " with the following properties: "] )
            | length hMults==1, d<-hMults ]++
            [ Para [ case language flags of
                          Dutch   ->
                            Str "In aanvulling daarop hebben de endorelaties de volgende eigenschappen: "
                          English   ->
                            Str "Additionally, the endorelations come with the following properties: "]
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
    keyDocumentation
     = case (keyDefs fSpec, language flags) of
        ([], _) -> []
        ([k], Dutch)   -> [ Para  [Str "Er is één key: ",Str (name k),Str "."]]
        ([k], English) -> [ Para  [Str "There is but one key: ",Str (name k),Str "." ]]
        (keyds, Dutch) -> [ Para $ Str "De volgende keys bestaan: ": commaNLPandoc (Str "en") [Str (name k) | k<-keyds]]
        (keyds, English)->[ Para $ Str "The following keys exist: ": commaEngPandoc (Str "and") [Str (name k) | k<-keyds]]

-- The properties of various declations are documented in different tables.
-- First, we document the heterogeneous properties of all relations
-- Then, the endo-poperties are given, and finally
-- the process rules are documented.

  daAttributes :: PlugSQL -> [Block]
  daAttributes p
   = if length (plugFields p)<=1 then [] else
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
      clauses = nub [clause | Quad _ ccrs<-vquads fSpec, (_,hornClauses)<-cl_conjNF ccrs, clause<-hornClauses]
      is = nub [r | EUni fus<-clauses
                  , isIdent (EIsc [notCpl f | f<-fus, isPos f] sgn)
                  , f<-filter isNeg fus
                  , s<-strands f
                  , e<-[head s, flp (last s)]
                  , r<-mors e
                ]
      ts = nub [r | EUni fus<-clauses
                  , isIdent (EIsc [notCpl f | f<-fus, isNeg f] sgn)
                  , f<-filter isPos fus
                  , s<-strands f
                  , e<-[head s, flp (last s)]
                  , r<-mors e
                  ]
      strands (ECps fs _) = [fs]
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
       plugHeader = toList $ labeledThing flags (lev+1) ("sct:Plug "++escapeNonAlphaNum (name p)) (name p)
       content = daAttributes p ++ plugRules ++ plugSignals ++ plugKeydefs ++ iRules
       plugRules
        = case language flags of
           English -> case [r | r<-invariants fSpec, null (declsUsedIn r >- declsUsedIn p)] of
                       []  -> []
                       [r] -> [ Para [ Str "Within this data set, the following integrity rule shall be true at all times. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic r)) ]
                                else Para [ Math DisplayMath $ showMath r]
                              ]
                       rs  -> [ Para [ Str "Within this data set, the following integrity rules shall be true at all times. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic r)) ]] | r<-rs ]
                                else BulletList [ [Para [Math DisplayMath $ showMath r]] | r<-rs ]
                              ]
           Dutch   -> case [r | r<-invariants fSpec, null (declsUsedIn r >- declsUsedIn p)] of
                       []  -> []
                       [r] -> [ Para [ Str "Binnen deze gegevensverzameling dient de volgende integriteitsregel te allen tijde waar te zijn. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic r)) ]
                                else Para [ Math DisplayMath $ showMath r]
                              ]
                       rs  -> [ Para [ Str "Binnen deze gegevensverzameling dienen de volgende integriteitsregels te allen tijde waar te zijn. " ]
                              , if showPredExpr flags
                                then BulletList [ [Para [ Math DisplayMath (showLatex (toPredLogic r)) ]] | r<-rs ]
                                else BulletList [ [Para [Math DisplayMath $ showMath r]]
                                                | r<-rs ]
                              ]
       plugKeydefs
        = case language flags of
           English -> case [k | k<-keyrules fSpec, null (declsUsedIn k >- declsUsedIn p)] of
                       []  -> []
                       [s] -> [ Para [ Str "This data set contains one key. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic s)) ]
                                else Para [ Math DisplayMath $ showMath s]
                              ]
                       ss  -> [ Para [ Str "This data set contains the following keys. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic s)) ]] | s<-ss ]
                                else BulletList [ [Para [Math DisplayMath $ showMath s]]
                                                | s<-ss ]
                              ]
           Dutch   -> case [k | k<-keyrules fSpec, null (declsUsedIn k >- declsUsedIn p)] of
                       []  -> []
                       [s] -> [ Para [ Str ("Deze gegevensverzameling genereert één key. ") ] 
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic s)) ]
                                else Para [ Math DisplayMath $ showMath s]
                              ]
                       ss  -> [ Para [ Str "Deze gegevensverzameling genereert de volgende keys. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic s)) ]] | s<-ss ]
                                else BulletList [ [Para [Math DisplayMath $ showMath s]] | s<-ss ]
                              ]
       plugSignals
        = case (language flags, [r | r<-vrules fSpec, isSignal r , null (declsUsedIn r >- declsUsedIn p)]) of
    --       English -> case [r | r<-vrules fSpec, isSignal r , null (mors r >- mors p)] of
            (_      , [])  -> []
            (English, [s]) -> [ Para [ Str "This data set generates one process rule. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic s)) ]
                                else Para [ Math DisplayMath $ showMath s]
                              ] 
            (English, ss)  -> [  Para [ Str "This data set generates the following process rules. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic s)) ]] | s<-ss ]
                                else BulletList [ [Para [Math DisplayMath $ showMath s]] | s<-ss ]
                              ]
            (Dutch  ,  [s]) -> [ Para [ Str ("Deze gegevensverzameling genereert één procesregel. ") ] 
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic s)) ]
                                else Para [ Math DisplayMath $ showMath s]
                              ]
            (Dutch  ,  ss ) -> [ Para [ Str "Deze gegevensverzameling genereert de volgende procesregels. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic s)) ]] | s<-ss ]
                                else BulletList [ [Para [Math DisplayMath $ showMath s]] | s<-ss ]
                              ]
       iRules
        = case language flags of
           English -> case irs of
                       []  -> []
                       [e] -> [ Para [ Str "The following rule defines the integrity of data within this data set. It must remain true at all times. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic e)) ]
                                else Para [ Math DisplayMath $ showMath e]
                              ]
                       es  -> [ Para [ Str "The following rules define the integrity of data within this data set. They must remain true at all times. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic e)) ]] | e<-es ]
                                else BulletList [ [Para [Math DisplayMath $ showMath e]] | e<-es ]
                              ]
           Dutch   -> case irs of
                       []  -> []
                       [e] -> [ Para [ Str "De volgende regel definieert de integriteit van gegevens binnen deze gegevensverzameling. Hij moet te allen tijde blijven gelden. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic e)) ]
                                else Para [ Math DisplayMath $ showMath e]
                              ]
                       es  -> [ Para [ Str "De volgende regels definiëren de integriteit van gegevens binnen deze gegevensverzameling. Zij moeten te allen tijde blijven gelden. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic e)) ]] | e<-es ]
                                else BulletList [ [Para [Math DisplayMath $ showMath e]] | e<-es ]
                              ]
          where irs = [ horn2expr hc
                      | Quad r ccrs<-vquads fSpec
                      , r_usr (cl_rule ccrs)==UserDefined, isIdent r, source r `elem` pcpts
                      , (_,hornClauses)<-cl_conjNF ccrs
                      , hc@(Hc [ERel nega _] _)<-hornClauses
                      , sameDecl r nega
                      ]
                pcpts = case p of
                  ScalarSQL{} -> [cLkp p]
                  _           -> map fst (cLkpTbl p)

  mathRel :: Expression -> Inlines
  mathRel (ERel r           _) = 
      case language flags of
        Dutch -> text "de relatie " 
        English -> text "the relation "
   <> math ((name.source) r++ " \\xrightarrow {"++name r++"} "++(name.target) r)
  mathRel (EFlp (ERel r _ ) _) = 
      case language flags of
        Dutch -> text "de relatie " 
        English -> text "the relation "
   <> math ((name.source) r++ " \\xleftarrow  {"++name r++"} "++(name.target) r)
  mathRel (EIsc (r1,_)     _) = 
      let srcTable = case r1 of
                       ERel (I c) _ -> c
                       _          -> fatal 767 ("Unexpected expression: "++show r1)
      in 
      case language flags of
        Dutch -> text "de identiteitsrelatie van " 
        English -> text "the identityrelation of "
   <> math (name srcTable) 
  mathRel t@(ETyp (ERel (I _) _ ) _) =
      case language flags of
        Dutch   -> text "het feit of deze " <> (math.name.source) t <> text " een "  <> (math.name.target) t <> text " is." 
        English -> text "weather this "     <> (math.name.source) t <> text " is a " <> (math.name.target) t <> text " or not." 
  --  mathRel (ECps (r1,r2)     _) = mathRel r1 <> math " ; "     <> mathRel r2
  mathRel expr                    = fatal 223 ("Have a look at the generated Haskell to see what is going on..\n"++show expr) 
  
  
                          
  
