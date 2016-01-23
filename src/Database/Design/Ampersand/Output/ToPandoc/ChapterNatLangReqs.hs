{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Design.Ampersand.Output.ToPandoc.ChapterNatLangReqs (
      chpNatLangReqs
 ) where

import Data.Char hiding (Space)
import Data.List
import Data.List.Split
import Data.Maybe
--import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Output.ToPandoc.SharedAmongChapters

chpNatLangReqs :: Int -> FSpec -> Blocks
chpNatLangReqs lev fSpec =
      --  *** Header ***
   chptHeader (fsLang fSpec) SharedLang
   <> --  *** Intro  ***
    case fsLang fSpec of
        Dutch   -> para
                     (  "Dit hoofdstuk beschrijft een natuurlijke taal, waarin functionele eisen ten behoeve van "
                     <> (singleQuoted.str.name) fSpec
                     <> " kunnen worden besproken en uitgedrukt. "
                     <> "Hiermee wordt beoogd dat verschillende belanghebbenden hun afspraken op dezelfde manier begrijpen. "
                     <> "De taal van "
                     <> (singleQuoted. str. name) fSpec
                     <> " bestaat uit begrippen en basiszinnen, "
                     <> "waarin afspraken worden uitgedrukt. "
                     <> "Wanneer alle belanghebbenden afspreken dat zij deze basiszinnen gebruiken, "
                     <> "althans voor zover het "
                     <> (singleQuoted. str. name) fSpec
                     <> " betreft, "
                     <> "delen zij precies voldoende taal om afspraken op dezelfde manier te begrijpen. "
                     <> "Alle definities zijn genummerd omwille van de traceerbaarheid. "
                     )
        English -> para
                     (  "This chapter defines the natural language, in which functional requirements of "
                     <> (singleQuoted.str.name) fSpec
                     <> " can be discussed and expressed. "
                     <> "The purpose of this chapter is to create shared understanding among stakeholders. "
                     <> "The language of "
                     <> (singleQuoted.str.name) fSpec
                     <> " consists of concepts and basic sentences. "
                     <> "All functional requirements are expressed in these terms. "
                     <> "When stakeholders can agree upon this language, "
                     <> "at least within the scope of "
                     <> (singleQuoted.str.name) fSpec
                     <> ", they share precisely enough language to have meaningful discussions about functional requirements. "
                     <> "All definitions have been numbered for the sake of traceability. "
                     )
   <> --  *** Requirements ***
   (mconcat . map printOneTheme . orderingByTheme) fSpec
   <> --  *** Legal Refs ***
     if genLegalRefs (getOpts fSpec) then legalRefs else mempty

  where
  -- shorthand for easy localizing    
  l :: LocalizedStr -> String
  l lstr = localize (fsLang fSpec) lstr
  legalRefs :: Blocks
  legalRefs = (header (lev+2) sectionTitle)
            <> table caption'
                     [(AlignLeft,1/4),(AlignLeft,3/4)]
                     [plain lawHeader, plain articleHeader]  --headers
                     [ [(para.str.aOlLaw) art  , (para.str.unscanRef.aOlArt) art]
                     | art <-(sort.nub.concatMap getArticlesOfLaw.getRefs) fSpec  ]

         where (sectionTitle, lawHeader, articleHeader, caption') =
                 case fsLang fSpec of
                   Dutch   -> ("Referentietabel", "Wet", "Artikel", "Referentietabel van de wetsartikelen")
                   English -> ("Reference table", "Law", "Article", "Reference table of articles of law")
               getRefs ::FSpec ->  [LawRef]
               getRefs f = concatMap catMaybes ((map (map toLawRef).map explRefIds.explanations) f)


  -- | printOneTheme tells the story in natural language of a single theme.
  -- For this purpose, Ampersand authors should take care in composing explanations.
  -- Each explanation should state the purpose (and nothing else).
  printOneTheme :: ThemeContent -> Blocks
  printOneTheme tc 
    | (not . null . themes) fSpec && (isNothing . patOfTheme) tc 
        = mempty   -- The document is partial (because themes have been defined), so we don't print loose ends.
    | otherwise 
        =   --  *** Header of the theme: ***
            headerWithLabel (XRefNaturalLanguageTheme (patOfTheme tc))
                            (lev+2)
                            (case (patOfTheme tc,fsLang fSpec) of
                                (Nothing, Dutch  ) -> "Losse eindjes..."
                                (Nothing, English) -> "Loose ends..."
                                (Just pat, _     ) -> text (name pat)
                            )
          <> --  *** Purpose of the theme: ***
             (case patOfTheme tc of
                 Nothing  -> 
                   (para.str.l) 
                     (NL "Deze paragraaf beschrijft de relaties en concepten die niet in voorgaande secties zijn beschreven."
                     ,EN "This paragraph shows remaining artifacts that have not been described in previous paragraphs."
                     )
                 Just pat -> 
                   case purposesDefinedIn fSpec (fsLang fSpec) pat of
                     []    -> printIntro    (cptsOfTheme tc)
                     purps -> purposes2Blocks (getOpts fSpec) purps
             )
          <> (mconcat . map printConcept . cptsOfTheme ) tc
          <> (mconcat . map printRel     . dclsOfTheme ) tc
          <> (mconcat . map printRule    . rulesOfTheme) tc
      where
                           
-- The following paragraph produces an introduction of one theme (i.e. pattern or process).
       printIntro :: [Numbered CptCont] -> Blocks
       printIntro [] = mempty
       printIntro nCpts
         = case patOfTheme tc of
             Nothing  -> mempty
             Just pat -> 
               ((para ((str.l) (NL "In het volgende wordt de taal geïntroduceerd ten behoeve van "
                                 ,EN "The sequel introduces the language of ")
                              <> (str.name) pat <> ".")
                )<>
                ( case nCpts of
                   [] 
                     -> fatal 136 "Unexpected. There should be at least one concept to introduce."
                   [x]
                     -> para(   (str.l) (NL "Nu volgt de definitie van het begrip "
                                        ,EN "At this point, the definition of ")
                             <> (showCpt x) 
                             <> (str.l) (NL "."
                                        ,EN " is given.")
                            )
                   _ 
                     -> para(   (str.l) (NL "Nu volgen definities van de begrippen "
                                        ,EN "At this point, the definitions of ")
                             <> commaPandocAnd (fsLang fSpec) (map showCpt (sortWith theNr nCpts)) 
                             <> (str.l) (NL "."
                                        ,EN " are given.")
                             )
               )<>
               ( case filter hasMultipleDefs nCpts of
                   []  -> mempty
                   [x] -> para(  (str.l) (NL "Het begrip "
                                         ,EN "Concept ")
                              <> showCpt x 
                              <> (str.l) (NL " heeft meerdere definities."
                                         ,EN " is multiple defined.")
                              )
                   multipleDefineds
                       -> para(  (str.l) (NL "De begrippen "
                                         ,EN "Concepts ")
                              <> commaPandocAnd (fsLang fSpec) (map showCpt multipleDefineds) 
                              <> (str.l) (NL " hebben meerdere definities."
                                         ,EN " are multiple defined.")
                              )
               )
              )                                    
         where
           showCpt :: Numbered CptCont -> Inlines
           showCpt = emph.text.name.cCpt.theLoad
           hasMultipleDefs :: Numbered CptCont -> Bool
           hasMultipleDefs x = 
              case cCptDefs (theLoad x) of
                (_:_:_) -> True
                _       -> False       

  printConcept :: Numbered CptCont -> Blocks
  printConcept nCpt 
        = -- Purposes:
           (printPurposes . cCptPurps . theLoad) nCpt
         <> case (cCptDefs.theLoad) nCpt of
             []    -> mempty  -- There is no definition of the concept
             [cd] -> printCDef cd Nothing
             cds  -> mconcat
                    [printCDef cd (Just ("."++ [suffx])) 
                    |(cd,suffx) <- zip cds ['a' ..]  -- There are multiple definitions. Which one is the correct one?
                    ]
        where
         printCDef :: ConceptDef -- the definition to print
                -> Maybe String -- when multiple definitions exist of a single concept, this is to distinguish
                -> Blocks
         printCDef cDef suffx
           = definitionList 
              [(   str (l (NL"Definitie " ,EN "Definition "))
                <> case fspecFormat (getOpts fSpec) of
                                    FLatex -> (str . show .theNr) nCpt
                                    _      -> (str . name) cDef  
                <> str (fromMaybe "" suffx) <> ":" 
               , [para (   newGlossaryEntry (name cDef++fromMaybe "" suffx) (cddef cDef)
                        <> (case fspecFormat (getOpts fSpec) of
                                    FLatex -> rawInline "latex"
                                                ("~\\marge{\\gls{"++escapeNonAlphaNum 
                                                      (name cDef++fromMaybe "" suffx)++"}}")
                                    _      -> mempty)
                        <> str (cddef cDef)
                        <> if null (cdref cDef) then mempty
                           else str (" ["++cdref cDef++"]")
                       ) 
                 ] 
               )
              ]

  printRel :: Numbered DeclCont -> Blocks
  printRel nDcl
       =   (printPurposes . cDclPurps . theLoad) nDcl
        <> case (cDclMeaning . theLoad) nDcl of
              Just m -> definitionList [( definitionListItemLabel 
                                           (XRefNaturalLanguageDeclaration dcl)
                                              (l (NL "Afspraak ", EN "Agreement ")++show(theNr nDcl)
                                               ++if development (getOpts fSpec)
                                                 then (" ("++name nDcl++")") 
                                                 else ""
                                              )
                                        , [printMeaning m]
                                        )]
              _      -> mempty
        <> case samples of
              []  -> mempty
              [_] -> plain ((str.l) (NL "Een frase die hiermee gemaakt kan worden is bijvoorbeeld:"
                                    ,EN "A phrase that can be formed is for instance:")
                           )
              _   -> plain ((str.l) (NL "Frasen die hiermee gemaakt kunnen worden zijn bijvoorbeeld:"
                                    ,EN "Phrases that can be made are for instance:")
                                        )
        <> if null samples then mempty
           else bulletList [ plain $ mkPhrase dcl sample
                           | sample <- samples]
         
         where dcl = cDcl . theLoad $ nDcl
               samples = take 3 . cDclPairs . theLoad $ nDcl

  printRule :: Numbered RuleCont -> Blocks
  printRule nRul
   =  (printPurposes . cRulPurps . theLoad) nRul
    <> case (cRulMeaning . theLoad) nRul of
        Nothing 
          -> mempty
        Just m
          -> definitionList 
               [(definitionListItemLabel
                   (XRefNaturalLanguageRule . cRul . theLoad $ nRul)
                   ((case fsLang fSpec of
                       Dutch   -> "Afspraak "
                       English -> "Agreement "
                    )++
                    show (theNr nRul)++
                    if development (getOpts fSpec)  
                    then (" ("++name nRul++")")
                    else ""
                   )
                , [printMeaning m]
                ) 
               ]
      

  mkPhrase :: Declaration -> AAtomPair -> Inlines
  mkPhrase decl pair -- srcAtom tgtAtom
   = case decl of
       Sgn{} | null (prL++prM++prR)
                  ->    (atomShow . upCap) srcAtom
                     <> devShow (source decl) 
                     <> (pragmaShow.l) (NL " correspondeert met ", EN " corresponds to ")
                     <> atomShow tgtAtom
                     <> devShow (target decl)
                     <> (pragmaShow.l) (NL " in de relatie ",EN " in relation ")
                     <> atomShow (name decl)
                     <> "."
             | otherwise
                  ->    (if null prL then mempty
                         else pragmaShow (upCap prL) <> " ")
                     <> devShow (source decl)
                     <> atomShow srcAtom <> " "
                     <> (if null prM then mempty
                         else pragmaShow prM <> " ")
                     <> devShow (target decl)
                     <> atomShow tgtAtom
                     <> (if null prR then mempty
                         else " " <> pragmaShow prR)
                     <> "."

       Isn{}     -> fatal 299 "Isn  is not supposed to be here expected here."
       Vs{}      -> fatal 300 "Vs  is not supposed to be here expected here."
   where srcAtom = showValADL (apLeft pair)
         tgtAtom = showValADL (apRight pair)
         prL = decprL decl
         prM = decprM decl
         prR = decprR decl
         atomShow = str
         pragmaShow = emph . str
         devShow c = if (development (getOpts fSpec)) then "("<> (str.name) c <> ")" else mempty
                   


data LawRef = LawRef { lawRef :: String}
data ArticleOfLaw = ArticleOfLaw { aOlLaw :: String
                                 , aOlArt :: [Either String Int]
                                 } deriving Eq
toLawRef:: String -> Maybe LawRef
toLawRef s = case s of
              [] -> Nothing
              _  -> (Just . LawRef) s

-- the article is everything but the law (and we also drop any trailing commas)
getArticlesOfLaw :: LawRef -> [ArticleOfLaw]
getArticlesOfLaw ref = map buildLA  ((splitOn ", ".unwords.init.words.lawRef) ref)
   where
     buildLA art = ArticleOfLaw ((last.words.lawRef) ref) (scanRef art)
       where
    -- group string in number and text sequences, so "Art 12" appears after "Art 2" when sorting (unlike in normal lexicographic string sort)
         scanRef :: String -> [Either String Int]
         scanRef "" = []
         scanRef str'@(c:_) | isDigit c = scanRefInt str'
                            | otherwise = scanRefTxt str'
         scanRefTxt "" = []
         scanRefTxt str' = let (txt, rest) = break isDigit str'
                           in  Left txt : scanRefInt rest

         scanRefInt "" = []
         scanRefInt str' = let (digits, rest) = break (not . isDigit) str'
                           in  Right (read digits) : scanRefTxt rest

instance Ord ArticleOfLaw where
 compare a b =
   case compare (aOlLaw a) (aOlLaw b) of
     EQ   -> compare (aOlArt a) (aOlArt b)
     ord' -> ord'

unscanRef :: [Either String Int] -> String
unscanRef scannedRef = concat $ map (either id show) scannedRef
             
             
printPurposes :: [Purpose] -> Blocks
printPurposes  = fromList . concat . map (amPandoc . explMarkup) 

printMeaning :: A_Markup -> Blocks
printMeaning = fromList . amPandoc