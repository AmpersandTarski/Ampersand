{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterNatLangReqs where

import Data.Char hiding (Space)
import Data.List
import Data.List.Split
import GHC.Exts (sortWith)
import Data.Maybe
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters hiding (sortWith)
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Output.PandocAux
import Text.Pandoc.Builder

fatal :: Int -> String -> a
fatal = fatalMsg "Output.ToPandoc.ChapterNatLangReqs"

{- TODO: This module needs to be rewritten from scratch. Instead of deciding on the fly what should be included, 
         a datastructure needs to be added to the fSpec, which contains per theme the concepts, rules and relations
         that need to be printed.  
-}
chpNatLangReqs :: Int -> Fspc -> Options ->  Blocks
chpNatLangReqs lev fSpec flags = 
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
   fromList dpRequirementsOld
--   dpRequirementesNew 
   <> --  *** Legal Refs ***
     if genLegalRefs flags then legalRefs else mempty
       
   
  where
  legalRefs :: Blocks
  legalRefs = (labeledThing flags (lev+1) "LegalRefs" sectionTitle) 
            <> table caption'
                     [(AlignLeft,1/4),(AlignLeft,3/4)]
                     [plain lawHeader, plain articleHeader]  --headers
                     [ [(para.str.aOlLaw) art  , (para.str.unscanRef.aOlArt) art]
                     | art <-(sort.nub.concatMap getArticlesOfLaw.getRefs) fSpec  ]
                     
         where (sectionTitle, lawHeader, articleHeader, caption') = 
                 case fsLang fSpec of
                   Dutch   -> ("Referentietabel", "Wet", "Artikel", "Referentietabel van de wetsartikelen")
                   English -> ("Reference table", "Law", "Article", "Reference table of articles of law")
               getRefs ::Fspc ->  [LawRef]
               getRefs f = concatMap catMaybes ((map (map toLawRef).map explRefIds.explanations) f)

  dpRequirementesNew :: Blocks
  dpRequirementesNew =  let (_,blocks) = foldl printTheme (newCounter,mempty)  (orderingByTheme fSpec)
                        in blocks
    where
     printTheme :: (Counter,Blocks)
                -> ( Maybe Theme   -- A theme is about either a pattern or a process. 
                   , [Rule]        -- The rules of that theme
                   , [Declaration] -- The relations that are used in a rule of this theme, but not in any rule of a previous theme.
                   , [A_Concept]   -- The concepts that are used in a rule of this theme, but not in any rule of a previous theme.
                   ) -> (Counter,Blocks)
     printTheme (counter,bs) themeStuff  =
        let (blocks,count') = printOneTheme themeStuff counter
        in (count',bs <> blocks)
  
  dpRequirementsOld :: [Block]
  dpRequirementsOld = theBlocks
    where
      (theBlocks,_) = if null (themes fSpec)
                      then printThemes toBeProcessedStuff newCounter $ map PatternTheme (patterns fSpec) ++ map (ProcessTheme . fpProc) (vprocesses fSpec)
                      else printThemes toBeProcessedStuff newCounter $ [ PatternTheme pat | pat<-patterns fSpec, name pat `elem` themes fSpec ] ++
                                                                       [ ProcessTheme $ fpProc fprc | fprc<-vprocesses fSpec, name fprc `elem` themes fSpec ] 
      toBeProcessedStuff = ( conceptsWith
                           , allRelsThatMustBeShown
                           , [r | r<-vrules fSpec, r_usr r == UserDefined] )  -- All user declared rules
         where
           conceptsWith     -- All concepts that have at least one non-empty definition (must be the first)  
              = [ c
                | c@PlainConcept{} <- concs fSpec
                , (not.null) (concDefs fSpec c)
                ]           
           allRelsThatMustBeShown -- All relations declared in this specification that have at least one user-defined purpose.
              = [ d | d <- relsDefdIn fSpec
                , decusr d
                , not . null $ purposesDefinedIn fSpec (fsLang fSpec) d
                ]
                 
      printThemes :: (   [A_Concept]     -- all concepts that have one or more definitions or purposes. These are to be used into this section and the sections to come
                       , [Declaration]           -- all relations to be processed into this section and the sections to come
                       , [Rule])                 -- all rules to be processed into this section and the sections to come
                    -> Counter           -- unique definition counters
                    -> [Theme]         -- the patterns that must be processed into this specification
                    -> ([Block],Counter) -- The blocks that define the resulting document and the last used unique definition number
      printThemes  (still2doCPre, still2doRelsPre, still2doRulesPre) iPre allThemes 
           = case allThemes of
              []  -> if null still2doCPre && null still2doRelsPre && null still2doRelsPre
                     then ([],iPre)
                     else let (a,b) = printOneTheme (Nothing, still2doRulesPre, still2doRelsPre, still2doCPre) iPre
                          in (toList a,b)
              _   -> (toList blocksOfOneTheme ++ blocksOfThemes,iPost)
         where
           (thm:thms) = allThemes
           (blocksOfOneTheme,iPostFirst) = printOneTheme  thisThemeStuff iPre
           (blocksOfThemes,iPost)        = printThemes stuff2PrintLater iPostFirst thms
           thisThemeStuff    = (Just thm, [r | r<-thisThemeRules, r_usr r == UserDefined], thisThemeRels, thisThemeCs)
           thisThemeRules    = [r | r<-still2doRulesPre, r_env r == name thm ]      -- only user defined rules, because generated rules are documented in whatever caused the generation of that rule.
           rules2PrintLater  = still2doRulesPre >- thisThemeRules
           thisThemeRels     = [ d | d <- still2doRelsPre
                               , decpat d == name thm ||         -- all relations declared in this theme, combined
                                 d `eleM` relsMentionedIn thisThemeRules] -- all relations used in this theme's rules
           rels2PrintLater   = [x | x <-still2doRelsPre, (not.or) [ x==y | y <- thisThemeRels ]] 
           thisThemeCs       = [c | c <- still2doCPre, c `eleM` (concs thisThemeRules ++ concs thisThemeRels)] -- relations are rules ('Eis') too
           concs2PrintLater  = still2doCPre >- thisThemeCs
           stuff2PrintLater  = (concs2PrintLater, rels2PrintLater, rules2PrintLater)
           
      -- | printOneTheme tells the story in natural language of a single theme.
      -- For this purpose, Ampersand authors should take care in composing explanations.
      -- Each explanation should state the purpose (and nothing else).
  printOneTheme :: ( Maybe Theme    -- The theme to process (if any)
                   , [Rule]         -- Rules to print in this section
                   , [Declaration]  -- Relations to print in this section
                   , [A_Concept]    -- Concepts, to print in this section
                   )
                -> Counter      -- first free number to use for numbered items
                -> (Blocks,Counter)-- the resulting blocks and first free number after processing of the theme.
  printOneTheme (mTheme, rules2print, rels2print, concs2print) counter0
    = case (mTheme, themes fSpec) of
       (Nothing, _:_) 
          -> ( mempty, counter0 )         -- The document is partial (because themes have been defined), so we don't print loose ends.
       _  -> ( (  --  *** Header of the theme: ***
                 labeledThing flags (lev+1) 
                              (xLabel DataAnalysis++case mTheme of
                                                          Nothing ->  "_LooseEnds"
                                                          _       -> themeName
                                                   )
                                  (case (mTheme,fsLang fSpec) of
                                      (Nothing, Dutch  ) -> "Losse eindjes..."
                                      (Nothing, English) -> "Loose ends..."
                                      _                  -> themeName
                                  )


               <> --  *** Purpose of the theme: ***
                  case mTheme of 
                      Nothing  -> case fsLang fSpec of
                                     Dutch   -> para $ 
                                                    "Deze paragraaf beschrijft de relaties en concepten die "
                                                 <> "niet in voorgaande secties zijn beschreven."
                                     English -> para $
                                                    "This paragraph shows remaining fact types and concepts "
                                                 <> "that have not been described in previous paragraphs."
                      Just pat -> purposes2Blocks flags (purposesDefinedIn fSpec (fsLang fSpec) pat)
                <> --  *** Introduction text of the theme: ***
                   printIntro (filter isDefined concs2print) 
                <> fromList reqdefs
                )
             , Counter (getEisnr counter0 + length reqs)
             )
           where
              isDefined :: A_Concept -> Bool
              isDefined = not.null.concDefs fSpec
              -- the concepts for which one of the relations of this theme contains a source or target definition
              -- (these will be printed, regardless whether the concept was printed before)
              
              -- sort the requirements by file position
              reqs = sortWith fst [ ((i,filenm org, linenr org,colnr org), bs) 
                                  | (i,org,bs)<- addIndex 0 (printConcepts concs2print) ++ 
                                                 addIndex 2 (printRels rels2print) ++ addIndex 3 (printRules rules2print)]
               where addIndex i ps = [ (i::Int,fs, sn) | (fs,sn) <- ps ] -- add an index to sort first on category (concept, rel, ..)
              
              -- make blocks for requirements
              reqblocks = [(pos,req (Counter cnt)) | (cnt,(pos,req))<-zip [(getEisnr counter0)..] reqs]
              reqdefs = concatMap snd reqblocks

              themeName = case mTheme of
                           Nothing  -> ""
                           Just pat -> name pat
                           --Just (PatternTheme pat) -> "Pattern "++name pat
                           --Just (ProcessTheme prc) -> "Process "++name prc
                                          

-- The following paragraph produces an introduction of one theme (i.e. pattern or process).
              printIntro :: [A_Concept] -> Blocks
              printIntro [] = mempty
              printIntro ccds
                = case fsLang fSpec of
                      Dutch   -> (case ([(emph.str.unCap) cname | cname<-map name ccds]
                                       , length [p |p <- map PatternTheme (patterns fSpec) ++ map (ProcessTheme . fpProc) (vprocesses fSpec), name p == themeName]
                                       ) of
                                    ([] ,_) -> mempty
                                    ([_],1) -> case mTheme of
                                                 Nothing  -> mempty
                                                 Just tme -> para ( "In het volgende wordt de taal geïntroduceerd ten behoeve van "
                                                                 <> (str.name) tme <> "."
                                                                  )
                                    (cs ,1) -> para ( "Nu volgen definities van de concepten "
                                                   <> commaNLPandoc' ("en") cs <> ".")
                                    ([c],_) -> para ( "Deze sectie introduceert het concept " <>  c )
                                    (cs ,_) -> para ( "Deze sectie introduceert de concepten " <>
                                                          commaNLPandoc' "en" cs <> ". ")
                                 ) <>  
                                 (let cs = [(c,cds) | (c)<-ccds, let cds = concDefs fSpec c,length cds>1] in
                                   case (cs, length cs==length ccds) of
                                    ([]       ,   _  ) -> mempty
                                    ([(c,_)]  , False) -> para ("Eén daarvan, " <> (str.name) c <>", heeft meerdere definities. ")
                                    (_        , False) -> para ("Daarvan hebben "<>commaNLPandoc' "en" (map (str. name . fst) cs)<> " meerdere definities. ")
                                    ([(_,cds)], True ) -> para ("Deze heeft "<>str(count Dutch (length cds) "definitie")<>". ")
                                    (_        , True ) -> para ("Elk daarvan heeft meerdere definities. ")
                                 )
                                  
                      English -> (case ([(emph.str.unCap) cname | cname<-map name ccds]
                                       , length [p |p <- map PatternTheme (patterns fSpec) ++ map (ProcessTheme . fpProc) (vprocesses fSpec), name p == themeName]
                                       ) of
                                    ([] ,_) -> mempty
                                    ([_],1) -> case mTheme of
                                                 Nothing  -> mempty
                                                 Just tme -> para ( "The sequel introduces the language of "
                                                                 <> (str.name) tme <> "."
                                                                  )
                                    (cs ,1) -> para ( "At this point, the definitions of "
                                                   <> commaEngPandoc' "and" cs <>" are given.")
                                    ([c],_) -> para ("This section introduces concept " <> c )
                                    (cs ,_) -> para ("This section introduces concepts " <>
                                                       commaEngPandoc' "and" cs <> ". ")
                                 )<> 
                                 (let cs = [(c,cds) | c <- ccds, let cds = concDefs fSpec c, length cds>1] in
                                   case (cs, length cs==length ccds) of
                                    ([]       ,   _  ) -> mempty
                                    ([(c,_)]  , False) -> para ("One of these concepts, " <> (str.name) c <>", has multiple definitions. ")
                                    (_        , False) -> para ("Of those concepts "<>commaEngPandoc' "and" (map (str. name . fst) cs)<> " have multiple definitions. ")
                                    ([(_,cds)], True ) -> para ("It has "<>str(count English (length cds) "definition")<>". ")
                                    (_        , True ) -> para ("Each one has several definitions. ")
                                 )
                                  

              -- | the origin of c is the origin of the head of uniquecds c
              --   after sorting by origin the counter will be applied
              printConcepts :: [A_Concept] -> [(Origin, Counter -> [Block])]
              printConcepts cs = map printConcept cs
              printConcept :: A_Concept -> (Origin, Counter -> [Block])
              printConcept c = (case uniquecds fSpec c of
                                  []      -> OriginUnknown 
                                  (_,x):_ -> origin x 
                               , cptBlock c
                               )
              -- | make a block for a c with all its purposes and definitions
              cptBlock :: A_Concept -> Counter -> [Block]
              cptBlock cpt cnt = concat [amPandoc (explMarkup e) | e<-exps] 
                  ++ zipWith cdBlock
                       (if length (uniquecds fSpec cpt) == 1 then [(cnt, "")] else
                          [(cnt, '.' : show i) | i <- [(1 :: Int) ..]])
                       [ (nm, symDefLabel cd, cddef cd, cdref cd) | (nm, cd) <- uniquecds fSpec cpt ]
                  where exps = case purposeOf fSpec (fsLang fSpec) cpt of
                                 Nothing -> []
                                 Just ps -> ps
              -- | make a block for a concept definition
              cdBlock :: (Counter,String) -> (String,String,String,String) -> Block
              cdBlock (cnt,xcnt) (nm,lbl,def',ref) = DefinitionList 
                                        [( [ Str (case fsLang fSpec of
                                                                 Dutch   -> "Definitie "
                                                                 English -> "Definition ")
                                           , Str (show (getEisnr cnt)++xcnt)
                                           , Str ":"]
                                         , [ makeDefinition flags (getEisnr cnt)  nm lbl def' ref ])]


              -- | sctds prints the requirements related to relations that are introduced in this theme.
              printRels :: [Declaration] -> [(Origin, Counter -> [Block])]
              printRels = map (\dcl -> (origin dcl, printRel dcl))
              printRel :: Declaration -> Counter -> [Block]
              printRel dcl cnt 
               = Plain [RawInline (Text.Pandoc.Builder.Format "latex") "\\bigskip"] :
                 toList (purposes2Blocks flags purps)
                 ++ 
                 [ DefinitionList [ ( [ Str (case fsLang fSpec of
                                                      Dutch   -> "Afspraak "
                                                      English -> "Agreement ")
                                     , Str (show(getEisnr cnt))
                                     ,if development flags && name dcl/="" then Str (" ("++name dcl++"):") else Str ":"]
                                   , [ Plain [RawInline (Text.Pandoc.Builder.Format "latex") $ symReqLabel dcl]:
                                       meaning2Blocks (fsLang fSpec) dcl
                                     ]
                                   )] ]++
                 ( case (fsLang fSpec, length samplePop) of
                        (_      , 0) -> []
                        (Dutch  , 1) -> [Para [Str "Een frase die hiermee gemaakt kan worden is bijvoorbeeld:"]]
                        (English, 1) -> [Para [Str "A phrase that can be formed is for instance:"]]
                        (Dutch  , _) -> [Para [Str "Frasen die hiermee gemaakt kunnen worden zijn bijvoorbeeld:"]]
                        (English, _) -> [Para [Str "Phrases that can be made are for instance:"]]
                 ) ++
                 sampleSentences
                 where purps     = purposesDefinedIn fSpec (fsLang fSpec) dcl
                       samplePop = (take 3 . fullContents (gens fSpec) (initialPops fSpec) . EDcD) dcl
                       sampleSentences =
                         [ Para $ mkSentence (development flags) dcl srcViewAtom tgtViewAtom 
                         | p <-samplePop
                         , let srcViewAtom = showViewAtom fSpec (Just dcl) (source dcl) (srcPaire p) 
                         , let tgtViewAtom = showViewAtom fSpec Nothing (target dcl) (trgPaire p)
                         ] ++
                         (if null samplePop then [] else [Plain [RawInline (Text.Pandoc.Builder.Format "latex") "\\medskip"]])

              printRules :: [Rule] -> [(Origin,Counter -> [Block])]
              printRules = map (\rul -> (origin rul, printRule rul))

  printRule :: Rule -> Counter -> [Block]
  printRule rul cnt 
   =  Plain [RawInline (Text.Pandoc.Builder.Format "latex") "\\bigskip"] :
      toList (purposes2Blocks flags purps)
      ++
      [ DefinitionList [ ( [ Str (case fsLang fSpec of
                                    Dutch   -> "Afspraak "
                                    English -> "Agreement ")
                           , Str (show(getEisnr cnt))
                           , if development flags && name rul/="" then Str (" ("++name rul++"):") else Str ":"]
                         , [ Plain [ RawInline (Text.Pandoc.Builder.Format "latex") $ symReqLabel rul] :
                                       meaning2Blocks (fsLang fSpec) rul
                           ]
                         )
                       ] 
      | not (null$meaning2Blocks (fsLang fSpec) rul)]
   where purps = purposesDefinedIn fSpec (fsLang fSpec) rul
          
  mkSentence :: Bool -> Declaration -> String -> String -> [Inline]
  mkSentence isDev decl srcAtom tgtAtom
   = case decl of
       Sgn{} | null (prL++prM++prR) 
                  -> [str' (upCap srcAtom), Space] ++ devShow (source decl) ++ [Str "corresponds",Space,Str "to",Space,str' tgtAtom, Space] ++ devShow (target decl) ++[Str "in",Space,Str "relation",Space,str' (decnm decl),Str "."]
             | otherwise 
                  -> leftHalf prL ++ rightHalf
                    where prL = decprL decl
                          prM = decprM decl
                          prR = decprR decl
                          leftHalf ""    = devShow (source decl)
                          leftHalf prLft = [str' (upCap prLft), Space] ++ devShow (source decl)
                          rightHalf = [str' srcAtom,Space,str' prM, Space] ++ devShow (target decl) ++ [str' tgtAtom]++(if null prR then [] else [Space,str' prR]) ++ [str' "."]

       Isn{}     -> devShow (source decl) ++ [str' (upCap srcAtom),Space,Str "equals",Space,str' tgtAtom,Str "."]
       Vs{}      -> [Str "True"]
   where str' = if fspecFormat flags==FLatex then RawInline (Text.Pandoc.Builder.Format "latex") . latexEscShw else Str
         devShow c | isDev     = [Str "(", str' $ name c, Str ") "] -- only show the concept when --dev option is given
                   | otherwise = []

-- TODO: fix showing/not showing based on relation
-- TODO: what about relations in the target view?
-- TODO: move these to some auxiliaries or utils
showViewAtom :: Fspc -> Maybe Declaration -> A_Concept -> String -> String
showViewAtom fSpec mDec cncpt atom =
  case mapMaybe (getView fSpec) (cncpt : largerConcepts (gens fSpec) cncpt) of
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
             case [ trgPaire p | p <- fullContents (gens fSpec) (initialPops fSpec) (objctx objDef), atom == srcPaire p ] of
               []         -> ""
               viewAtom:_ -> viewAtom  
        -- justViewRels = map (Just . objctx) [objDef | ViewExp objDef <- vdats view]

{-
getIdentity :: Fspc -> A_Concept -> Maybe IdentityDef
getIdentity fSpec cncpt = 
  case filter ((== cncpt) .  idCpt) (vIndices fSpec) of
    []         -> Nothing 
    identity:_ -> Just identity 
-}

getView :: Fspc -> A_Concept -> Maybe ViewDef
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
             