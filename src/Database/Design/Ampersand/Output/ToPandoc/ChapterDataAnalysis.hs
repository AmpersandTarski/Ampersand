{-# LANGUAGE OverloadedStrings #-}
module Database.Design.Ampersand.Output.ToPandoc.ChapterDataAnalysis (chpDataAnalysis) where

import Database.Design.Ampersand.ADL1 hiding (Association)
import Database.Design.Ampersand.Output.ToPandoc.SharedAmongChapters hiding (Association)
import Database.Design.Ampersand.FSpec.Crud
import Database.Design.Ampersand.Graphic.ClassDiagram --(Class(..),CdAttribute(..))
import Database.Design.Ampersand.Graphic.Fspec2ClassDiagrams
import Database.Design.Ampersand.Output.PredLogic
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
  l lstr = localize (fsLang fSpec) lstr
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
                 Dutch   -> (text "De afspraken zijn vertaald naar een gegevensmodel. "
                           <> ( if canXRefer (getOpts fSpec)
                                then text "Dit gegevensmodel is in figuur " <> xRefReference (getOpts fSpec) logicalDataModelPicture <> text " weergegeven."
                                else text "Dit gegevensmodel is in onderstaand figuur weergegeven. "
                            ) )
                 English -> (text "The functional requirements have been translated into a data model. "
                           <> ( if canXRefer (getOpts fSpec)
                                then text "This model is shown by figure " <> xRefReference (getOpts fSpec) logicalDataModelPicture <> text "."
                                else text "This model is shown by the figure below. "
                            ) )
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
       (   header (sectionLevel+1) 
                  (((text.l) (NL "Gegevensverzameling: ", EN "Entity type: ") <> (emph.strong.text.name) cl))
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
       )
    where
        
     assocToRow :: Database.Design.Ampersand.Graphic.ClassDiagram.Association -> Blocks
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
        [ simpleTable [ plainText "Concept", plainText "C", plainText "R", plainText "U", plainText "D" ] $
            [ [ plainText $ name cncpt
              , mconcat $ map (plainText . name) ifcsC
              , mconcat $ map (plainText . name) ifcsR
              , mconcat $ map (plainText . name) ifcsU
              , mconcat $ map (plainText . name) ifcsD ]
            | (cncpt, (ifcsC, ifcsR, ifcsU, ifcsD)) <- crudObjsPerConcept (crudInfo fSpec)
            ]
        ]
  

  technicalDataModelBlocks = 
   (   header sectionLevel
                (case fsLang fSpec of
                    Dutch   ->  "Technisch datamodel"
                    English ->  "Technical datamodel"
                )
    <> para (case fsLang fSpec of
               Dutch   -> ( "De afspraken zijn vertaald naar een technisch datamodel. "
                         <> ( if canXRefer (getOpts fSpec)
                              then "Dit model is in figuur " <> xRefReference (getOpts fSpec) technicalDataModelPicture <> " weergegeven."
                              else "Dit model is in onderstaand figuur weergegeven. "
                          ) )
               English -> ( "The functional requirements have been translated into a technical data model. "
                         <> ( if canXRefer (getOpts fSpec)
                              then "This model is shown by figure " <> xRefReference (getOpts fSpec) technicalDataModelPicture <> "."
                              else "This model is shown by the figure below. "
                          ) )
            )
    <> para (showImage (getOpts fSpec) technicalDataModelPicture)
    <> para (let nrOfTables = length (filter isTable (plugInfos fSpec))
             in
             case fsLang fSpec of
        Dutch   -> text ("Het technisch datamodel bestaat uit de volgende "++show nrOfTables++" tabellen:")
        English -> text ("The technical datamodel consists of the following "++show nrOfTables++" tables:")
            )
    <> mconcat [detailsOfplug p | p <- sortBy (compare `on` (map toLower . name)) (plugInfos fSpec), isTable p]
   ) 
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
                   -> para (text $ "Deze tabel heeft de volgende "++(show.length.attributes) tbl++" attributen:")
                English
                   -> para (text $ "This table has the following "++(show.length.attributes) tbl++" attributes:")
                  )
               <> showAttributes (plugAttributes tbl)
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
                     <> showAttributes (plugAttributes bin)

             InternalPlug ScalarSQL{}
                -> mempty
             ExternalPlug _
               -> case fsLang fSpec of
                    Dutch   -> para "De details van deze service zijn in dit document (nog) niet verder uitgewerkt."
                    English -> para "The details of this dataservice are not available in this document."
      showAttributes :: [SqlAttribute] -> Blocks
      showAttributes atts = bulletList (map showAttribute atts)
        where
          showAttribute att =
--FIXME 20140525: Onderstaande code vervangen door afl te leiden van `attUse`. Daar zit deze info al in verwerkt!
             let isPrimaryKey = case attExpr att of
                                  e@EDcI{} -> e==attExpr (head atts) -- The first attribute represents the most general concept
                                  _        -> False
                 mForeignKey  = case attExpr att of
                                  EIsc (EDcI c,_) -> Just c
                                  _               -> Nothing
             in para (  (strong.text.attName) att
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
                                  <> primExpr2pandocMath (fsLang fSpec) (attExpr att)
                                  <> "."
                                  )
                         )
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
    docRule heading rule = mconcat $
       [ plain $ strong (text (l heading ++ ": ") <> emph (text (rrnm rule)))
       , fromList $ maybe mempty (concatMap $ amPandoc . explMarkup) $ purposeOf fSpec (fsLang fSpec) rule
       , fromList $ meaning2Blocks (fsLang fSpec) rule
       , if showPredExpr (getOpts fSpec)
         then let predicate = toPredLogic rule
              in  if format == Frtf then
                     plain $ linebreak <> (singleton $ RawInline (Text.Pandoc.Builder.Format "rtf") (showRtf predicate)) 
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
        rs' -> case [r | r<-rs', (not.null) ([Tot,Sur] `isc` properties r) ] of
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
    isAttribute r = (not.null) ([Uni,Inj] `isc` properties r)
    endoProperties
     = if null [ m | r<-hMults, m<-properties r, m `elem` [Rfx,Irf,Trn,Sym,Asy]]
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
   = if length (plugAttributes p)<=1 then [] else
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
      [ if isProp (attExpr att) && att/=head (plugAttributes p)
        then [ [Plain [Str (attName att)]]
             , [Plain [ Str "Bool"]]
             , [Plain [Math InlineMath "\\surd"]]
             , []
             ]
        else [ [Plain [if att==head (plugAttributes p) || null ([Uni,Inj,Sur]>-properties (attExpr att))
                       then Str  "key "
                       else Str (attName att)]]
             , [Plain [ (Str . latexEscShw.name.target.attExpr) att]]
             , [Plain [Math InlineMath "\\surd" | not (attNull att)]]
             , [Plain [Math InlineMath "\\surd" | attUniq att]]
             ]
      | att<-plugAttributes p  -- tail haalt het eerste veld, zijnde I[c], eruit omdat die niet in deze tabel thuishoort.
      ]

     ]
-- the endo-properties have already been reported in the general section of this chapter.

  -- daPlugs describes data sets.
  -- These can be recognized by:
  --    1. the first attribute has the "unique" property on (otherwise it is a binary association)
  --    2. there is more than one attribute (otherwise it is a scalar).
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
