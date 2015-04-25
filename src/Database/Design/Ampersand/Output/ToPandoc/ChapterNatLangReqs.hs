{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Design.Ampersand.Output.ToPandoc.ChapterNatLangReqs where

import Data.Char hiding (Space)
import Data.List
import Data.List.Split
import Data.Maybe
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Output.ToPandoc.SharedAmongChapters
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.Output.PandocAux

fatal :: Int -> String -> a
fatal = fatalMsg "Output.ToPandoc.ChapterNatLangReqs"

{- TODO: This module needs to be rewritten from scratch. Instead of deciding on the fly what should be included,
         a datastructure needs to be added to the fSpec, which contains per theme the concepts, rules and relations
         that need to be printed.
-}
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
                  case patOfTheme tc of
                      Nothing  -> case fsLang fSpec of
                                     Dutch   -> para $
                                                    "Deze paragraaf beschrijft de relaties en concepten die "
                                                 <> "niet in voorgaande secties zijn beschreven."
                                     English -> para $
                                                    "This paragraph shows remaining fact types and concepts "
                                                 <> "that have not been described in previous paragraphs."
                      Just pat -> purposes2Blocks (getOpts fSpec) (purposesDefinedIn fSpec (fsLang fSpec) pat)
               <> --  *** Introduction text of the theme: ***
                  printIntro    (cptsOfTheme tc)
               <> printConcepts (cptsOfTheme tc)
               <> printRels     (dclsOfTheme tc)
               <> printRules    (rulesOfTheme tc)
           where
                           
-- The following paragraph produces an introduction of one theme (i.e. pattern or process).
              printIntro :: [Numbered CptCont] -> Blocks
              printIntro [] = mempty
              printIntro nCpts
                = case patOfTheme tc of
                    Nothing  -> mempty
                    Just pat -> 
                      ( para ((str.l) (NL "In het volgende wordt de taal geïntroduceerd ten behoeve van "
                                               ,EN "The sequel introduces the language of ")
                                      <> (str.name) pat <> ".")
                      <> case  partition hasMultipleDefs (map theLoad nCpts) of
                        ([],[]) 
                         -> fatal 136 "Unexpected. There should be at least one concept to introduce."
                        (multipleDefined,properDefined)
                         -> ( if null properDefined
                              then mempty
                              else para 
                                    (  (str.l) (NL "Nu volgen definities van de concepten "
                                               ,EN "At this point, the definitions of ")
                                    <> commaPandocAnd (fsLang fSpec) (map showCpt properDefined) 
                                    <> (str.l) (NL "."
                                               ,EN " are given.")
                                    )
                            ) <>
                            ( if null multipleDefined
                              then mempty
                              else para 
                                    (  (str.l) (NL "De concepten "
                                               ,EN "Concepts ")
                                    <> commaPandocAnd (fsLang fSpec) (map showCpt multipleDefined) 
                                    <> (str.l) (NL " hebben meerdere definities. Hierdoor vallen gaten in de nummering van de definities."
                                               ,EN " are multiple defined. Please check the diagnose for details.")
                                    )
                            ) 
                      )                                    

              showCpt :: CptCont -> Inlines
              showCpt = text.name.cCpt
              hasMultipleDefs :: CptCont -> Bool
              hasMultipleDefs x = 
                 case cCptDefs x of
                   (_:_:_) -> True
                   _       -> False       
              printConcepts :: [Numbered CptCont] -> Blocks
              printConcepts = mconcat . map printConcept
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
                       , [para (   case fspecFormat (getOpts fSpec) of
                                            FLatex -> rawInline "latex"
                                                        ("~\\marge{\\gls{"++escapeNonAlphaNum 
                                                              (name cDef++fromMaybe "" suffx)++"}}")
                                            _      -> mempty
                                <> newGlossaryEntry (name cDef++fromMaybe "" suffx) (cddef cDef)
                                <> str (cddef cDef)
                                <> if null (cdref cDef) then mempty
                                   else str (" ["++cdref cDef++"]")
                               ) 
                         ] 
                       )
                      ]


              printRels :: [Numbered DeclCont] -> Blocks
              printRels = mconcat . map printRel
              printRel :: Numbered DeclCont -> Blocks
              printRel nDcl
               =   (printPurposes . cDclPurps . theLoad) nDcl
                <> definitionList [( definitionListItemLabel 
                                         (XRefNaturalLanguageDeclaration dcl)
                                         (l (NL "Afspraak ", EN "Agreement ")++show(theNr nDcl)
                                          ++if development (getOpts fSpec)
                                            then (" ("++name nDcl++")") 
                                            else ""
                                         )
                                   , case (cDclMeaning . theLoad) nDcl of
                                      Nothing -> fatal 241 "Declarations without meaning should not be printed here. (see issue AmpersandTarski/ampersand/issues/44)"
                                      Just m  -> [printMeaning m]
                                   )
                                  ] 
                <> case samples of
                      []  -> mempty
                      [_] -> para ((str.l) (NL "Een frase die hiermee gemaakt kan worden is bijvoorbeeld:"
                                           ,EN "A phrase that can be formed is for instance:")
                                  )
                      _   -> para ((str.l) (NL "Frasen die hiermee gemaakt kunnen worden zijn bijvoorbeeld:"
                                            ,EN "Phrases that can be made are for instance:")
                                  )
                <> if null samples then mempty
                   else bulletList [ para $ mkSentence dcl sample
                                   | sample <- samples]
                 
                 where dcl = cDcl . theLoad $ nDcl
                       samples = take 3 . cDclPairs . theLoad $ nDcl
--                       sampleSentences =
--                         [ para $ mkSentence (development (getOpts fSpec)) dcl srcViewAtom tgtViewAtom
--                         | p <-samplePop
--                         , let srcViewAtom = showViewAtom fSpec (Just dcl) (source dcl) (srcPaire p)
--                         , let tgtViewAtom = showViewAtom fSpec Nothing (target dcl) (trgPaire p)
--                         ] 

              printRules :: [Numbered RuleCont] -> Blocks
              printRules = mconcat . map printRule

  printRule :: Numbered RuleCont -> Blocks
  printRule nRul
   =  (printPurposes . cRulPurps . theLoad) nRul
    <> definitionList [(definitionListItemLabel
                            (XRefNaturalLanguageRule . cRul . theLoad $ nRul)
                            ((case fsLang fSpec of
                                        Dutch   -> "Afspraak "
                                        English -> "Agreement "
                                   )++show (theNr nRul)
                                    ++if development (getOpts fSpec)  
                                      then (" ("++name nRul++")")
                                      else ""
                                )
                        , case (cRulMeaning . theLoad) nRul of
                            Nothing -> fatal 284 "It is very odd to have a rule without a meaning."
                            Just m  -> [printMeaning m]
                        ) 
                       ]
      

  mkSentence :: Declaration -> Paire -> Inlines
  mkSentence  decl pair -- srcAtom tgtAtom
   = case decl of
       Sgn{} | null (prL++prM++prR)
                  ->    (latexSpecial . upCap) srcAtom
                     <> devShow (source decl) 
                     <> (str.l) (NL " correspondeert met ", EN " corresponds to ")
                     <> latexSpecial tgtAtom
                     <> devShow (target decl)
                     <> (str.l) (NL " in de relatie ",EN " in relation ")
                     <> latexSpecial (name decl)
                     <> "."
             | otherwise
                  ->    if null prL then mempty
                        else latexSpecial (upCap prL++" ")
                     <> devShow (source decl)
                     <> latexSpecial (srcAtom++" "++prM++" ")
                     <> devShow (target decl)
                     <> latexSpecial (tgtAtom)
                     <> if null prR then mempty
                        else latexSpecial (" "++prR)
                     <> "."

       Isn{}     -> fatal 299 "Isn  is not supposed to be here expected here."
       Vs{}      -> fatal 300 "Vs  is not supposed to be here expected here."
   where srcAtom = srcPaire pair
         tgtAtom = trgPaire pair
         prL = decprL decl
         prM = decprM decl
         prR = decprR decl
         latexSpecial = if fspecFormat (getOpts fSpec)==FLatex then rawInline "latex" . latexEscShw else str
         devShow c = if (development (getOpts fSpec)) then str $ "("++name c++")" else mempty
                   

-- TODO: fix showing/not showing based on relation
-- TODO: what about relations in the target view?
-- TODO: move these to some auxiliaries or utils
showViewAtom :: FSpec -> Maybe Declaration -> A_Concept -> String -> String
showViewAtom fSpec mDec cncpt atom =
  case mapMaybe (getView fSpec) (cncpt : largerConcepts (vgens fSpec) cncpt) of
    []    -> atom
    view:_ -> case mDec of
              Nothing -> concatMap showViewSegment (vdats view)
              Just md -> if (not.null) [() | ViewExp objDef <- vdats view, EDcD d<-[objctx objDef], d==md]
                         then atom
                         else concatMap showViewSegment (vdats view)
             -- if we are showing one of the view relations, don't expand the view
     where showViewSegment (ViewText str') = str'
           showViewSegment (ViewHtml str') = str'
           showViewSegment (ViewExp objDef) =
             case [ trgPaire p | p <- fullContents (vgens fSpec) (initialPops fSpec) (objctx objDef), atom == srcPaire p ] of
               []         -> ""
               viewAtom:_ -> viewAtom
        -- justViewRels = map (Just . objctx) [objDef | ViewExp objDef <- vdats view]

{-
getIdentity :: FSpec -> A_Concept -> Maybe IdentityDef
getIdentity fSpec cncpt =
  case filter ((== cncpt) .  idCpt) (vIndices fSpec) of
    []         -> Nothing
    identity:_ -> Just identity
-}

getView :: FSpec -> A_Concept -> Maybe ViewDef
getView fSpec cncpt =
  case filter ((== cncpt) .  vdcpt) (vviews fSpec) of
    []       -> Nothing
    viewDef:_ -> Just viewDef

data LawRef = LawRef { lawRef :: String}
data ArticleOfLaw = ArticleOfLaw { aOlLaw :: String
                                 , aOlArt :: [Either String Int]
                                 } deriving Eq
toLawRef:: String -> Maybe LawRef
toLawRef s = case s of
              [] -> Nothing
              _  -> (Just . LawRef) s
getLaw :: ArticleOfLaw -> Inlines
getLaw x = (str.aOlLaw) x

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