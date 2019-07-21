{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ampersand.Output.ToPandoc.ChapterNatLangReqs (
      chpNatLangReqs
 ) where

import           Ampersand.Output.ToPandoc.SharedAmongChapters
import           RIO.Char hiding (Space)
import qualified RIO.List as L
import qualified Data.List.NonEmpty as NEL
import           Data.List.Split(splitOn)
import qualified RIO.Set as Set

chpNatLangReqs :: (HasDirOutput env, HasGenFuncSpec env) 
   => env -> Int -> FSpec -> Blocks
chpNatLangReqs env lev fSpec =
      --  *** Header ***
   xDefBlck env fSpec SharedLang
   <> --  *** Intro  ***
    case fsLang fSpec of
        Dutch   -> para
                     (  "Dit hoofdstuk beschrijft functionele eisen ten behoeve van "
                     <> (singleQuoted.str.name) fSpec
                     <> " in natuurlijke taal. "
                     <> "Het hoofdstuk bevat definities en afspraken. "
                     <> "Hiermee wordt beoogd dat verschillende belanghebbenden hun afspraken op dezelfde manier kunnen begrijpen. "
                     <> "Alle definities en afspraken zijn genummerd omwille van de traceerbaarheid. "
                     )
        English -> para
                     (  "This chapter describes functional requirements for "
                     <> (singleQuoted.str.name) fSpec
                     <> " in natural language. "
                     <> "It contains definitions and agreements. "
                     <> "The purpose of this chapter is to create shared understanding among stakeholders. "
                     <> "All definitions and agreements have been numbered for the sake of traceability. "
                     )
   <> --  *** Requirements ***
   (mconcat . map printOneTheme . orderingByTheme) fSpec
   <> --  *** Legal Refs ***
     if genLegalRefs then legalRefs else mempty

  where
  -- shorthand for easy localizing    
  l :: LocalizedStr -> String
  l = localize (fsLang fSpec)
  genLegalRefs = view genLegalRefsL env
  legalRefs :: Blocks
  legalRefs =  header (lev+2) sectionTitle
            <> table caption'
                     [(AlignLeft,1/4),(AlignLeft,3/4)]
                     [plain lawHeader, plain articleHeader]  --headers
                     [ [(para.str.aOlLaw) art  , (para.str.unscanRef.aOlArt) art]
                     | art <-(L.sort . L.nub . concatMap getArticlesOfLaw.getRefs) fSpec  ]

         where (sectionTitle, lawHeader, articleHeader, caption') =
                 case fsLang fSpec of
                   Dutch   -> ("Referentietabel", "Wet", "Artikel", "Referentietabel van de wetsartikelen")
                   English -> ("Reference table", "Law", "Article", "Reference table of articles of law")
               getRefs ::FSpec ->  [LawRef]
               getRefs = concatMap (mapMaybe toLawRef . explRefIds) . purposesDefinedIn fSpec (fsLang fSpec)


  -- | printOneTheme tells the story in natural language of a single theme.
  -- For this to work out, Ampersand authors should take care in composing PURPOSEs.
  printOneTheme :: ThemeContent -> Blocks
  printOneTheme tc 
    | isNothing (patOfTheme tc) &&
        null (cptsOfTheme tc) &&
        null (dclsOfTheme tc) &&
        null (rulesOfTheme tc) = mempty
    | otherwise =
             --  *** Header of the theme: ***
            xDefBlck env fSpec (XRefSharedLangTheme (patOfTheme tc))
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
                     purps -> purposes2Blocks env purps
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
                 para ((str.l) (NL "In het volgende wordt de taal geïntroduceerd ten behoeve van "
                                 ,EN "The sequel introduces the language of ")
                              <> (str.name) pat <> ".")
              <>
{-
                ( case nCpts of
                   [] 
                     -> fatal "Unexpected. There should be at least one concept to introduce."
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
-}
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
         <> case (nubByText.cCptDefs.theLoad) nCpt of
             []    -> mempty  -- There is no definition of the concept
             [cd] -> printCDef cd Nothing
             cds  -> mconcat
                    [printCDef cd (Just ("."++ [suffx])) 
                    |(cd,suffx) <- zip cds ['a' ..]  -- There are multiple definitions. Which one is the correct one?
                    ]
        where
         fspecFormat = view fspecFormatL env
         nubByText = L.nubBy (\x y -> cddef x ==cddef y && cdref x == cdref y) -- fixes https://github.com/AmpersandTarski/Ampersand/issues/617
         printCDef :: ConceptDef -- the definition to print
                -> Maybe String -- when multiple definitions exist of a single concept, this is to distinguish
                -> Blocks
         printCDef cDef suffx
           = definitionList 
              [(   str (l (NL"Definitie " ,EN "Definition "))
                <> ( if fspecFormat `elem` [Fpdf, Flatex] 
                     then (str . show .theNr) nCpt
                     else (str . name) cDef  
                   )  
                <> str (fromMaybe "" suffx) <> ":" 
               , [para (   newGlossaryEntry (name cDef++fromMaybe "" suffx) (cddef cDef)
                        <> ( if fspecFormat `elem` [Fpdf, Flatex]
                             then rawInline "latex"
                                    ("~"++texOnlyMarginNote 
                                            ("\\gls{"++escapeNonAlphaNum 
                                                        (name cDef++fromMaybe "" suffx)
                                                ++"}"
                                            )
                                    )
                             else mempty
                           )
                        <> str (cddef cDef)
                        <> if null (cdref cDef) then mempty
                           else str (" ["++cdref cDef++"]")
                       ) 
                 ] 
               )
              ]

  printRel :: Numbered DeclCont -> Blocks
  printRel nDcl =
         (printPurposes . cDclPurps . theLoad) nDcl
      <> definitionList 
            [(   (str.l) (NL "Afspraak ", EN "Agreement ")
              <> ": " <> (xDefInln fSpec (XRefSharedLangRelation dcl))
             , -- (xDefInln fSpec (XRefSharedLangRelation dcl) 
              mempty --  [xDefBlck fSpec (XRefSharedLangRelation dcl)]
              <>[printMeaning (fsLang fSpec) dcl]
              <>(case Set.elems $ properties dcl of
                    []  -> mempty
                    ps  -> [plain (   (str.l) (NL "Deze relatie is ",EN "This relation is " )
                                   <> (commaPandocAnd (fsLang fSpec) (map (str . propFullName (fsLang fSpec)) ps)<>"."
                                      )
                                  )
                           ]
                )    
             )   
            ]
      <> case samples of
            []  -> mempty
            [_] -> plain ((str.l) (NL "Een frase die hiermee gemaakt kan worden is bijvoorbeeld:"
                                  ,EN "A phrase that can be formed is for instance:")
                         )
            _   -> plain ((str.l) (NL "Frasen die hiermee gemaakt kunnen worden zijn bijvoorbeeld:"
                                  ,EN "Phrases that can be made are for instance:")
                         )
      <> case samples of
            []  -> mempty
            _   -> bulletList . map (plain . mkPhrase dcl) $ samples
    where dcl = cDcl . theLoad $ nDcl
          samples = take 3 . Set.elems . cDclPairs . theLoad $ nDcl
  printRule :: Numbered RuleCont -> Blocks
  printRule nRul =
         xDefBlck env fSpec (XRefSharedLangRule rul)
      <> (printPurposes . cRulPurps . theLoad) nRul
      -- <> definitionList 
      --       [(   str (l (NL "Afspraak ", EN "Agreement "))
      --         <> ": TODO"
      --        , case (cRulMeaning . theLoad) nRul of
      --            Nothing 
      --               -> [plain $
      --                       (str.l) (NL "De regel ",EN "The rule ")
      --                    <> (emph.str.name) rul
      --                    <> (str.l) (NL " is ongedocumenteerd.",EN " is undocumented.")
      --                  ]
      --            Just m
      --               -> [printMeaning m]
      --        )
      --       ]
     where rul = cRul . theLoad $ nRul
  mkPhrase :: Relation -> AAtomPair -> Inlines
  mkPhrase decl pair -- srcAtom tgtAtom
   | null (prL++prM++prR)
                   =    (atomShow . upCap) srcAtom
                     <> (pragmaShow.l) (NL " correspondeert met ", EN " corresponds to ")
                     <> atomShow tgtAtom
                     <> (pragmaShow.l) (NL " in de relatie ",EN " in relation ")
                     <> atomShow (name decl)
                     <> "."
   | otherwise
                  =    (if null prL then mempty
                         else pragmaShow (upCap prL) <> " ")
                     <> atomShow srcAtom <> " "
                     <> (if null prM then mempty
                         else pragmaShow prM <> " ")
                     <> atomShow tgtAtom
                     <> (if null prR then mempty
                         else " " <> pragmaShow prR)
                     <> "."
   where srcAtom = showValADL (apLeft pair)
         tgtAtom = showValADL (apRight pair)
         prL = decprL decl
         prM = decprM decl
         prR = decprR decl
         atomShow = str
         pragmaShow = emph . str
                   
data LawRef = LawRef { lawRef :: String}
data ArticleOfLaw = ArticleOfLaw { aOlLaw :: String
                                 , aOlArt :: [Either String Int]
                                 } deriving Eq
toLawRef:: String -> Maybe LawRef
toLawRef s = case s of
              [] -> Nothing
              _  -> (Just . LawRef) s
wordsOf :: LawRef -> NEL.NonEmpty String
wordsOf ref = case words . lawRef $ ref of
                [] -> fatal $ "string in LaWRef must not be empty."
                h:tl -> h NEL.:| tl
-- the article is everything but the law (and we also drop any trailing commas)
getArticlesOfLaw :: LawRef -> [ArticleOfLaw]
getArticlesOfLaw ref = map buildLA . splitOn ", " . unwords .NEL.init .wordsOf $ ref
                             
   where
     buildLA art = ArticleOfLaw ((NEL.last . wordsOf) ref) (scanRef art)
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
         scanRefInt str' = let (digits, rest) = span isDigit str'
                           in  Right (case readMaybe digits of
                                        Nothing  -> fatal $ "Impossible: This cannot be interpreted as digits: "<> digits
                                        Just x -> x
                                     ) : scanRefTxt rest

instance Ord ArticleOfLaw where
 compare a b =
   case compare (aOlLaw a) (aOlLaw b) of
     EQ   -> compare (aOlArt a) (aOlArt b)
     ord' -> ord'

unscanRef :: [Either String Int] -> String
unscanRef = concatMap (either id show)
             
             
