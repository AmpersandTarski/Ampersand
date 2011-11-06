{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterNatLangReqs
where
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
import DatabaseDesign.Ampersand.Basics  
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes
import Data.List
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Output.AdlExplanation (purpose,meaning,Explainable(..))
import DatabaseDesign.Ampersand.Output.PandocAux

fatal :: Int -> String -> a
fatal = fatalMsg "ChapterNatLangReqs.hs"

chpNatLangReqs :: Int -> Fspc -> Options ->  [Block]
chpNatLangReqs lev fSpec flags = header ++ dpIntro ++ dpRequirements
  where
  header :: [Block]
  header = labeledHeader lev (xLabel FunctionalRequirements)
                                         (case language flags of
                                             Dutch   ->  "Gemeenschappelijke taal"   
                                             English ->  "Shared Language"
                                         )
  dpIntro :: [Block]
  dpIntro = 
    case language flags of
        Dutch   -> [ Para
                     [ Str "Dit hoofdstuk beschrijft een natuurlijke taal, waarin functionele eisen ten behoeve van "
                     , Quoted  SingleQuote [Str (name fSpec)]
                     , Str " kunnen worden besproken en uitgedrukt. "
                     , Str "Hiermee wordt beoogd dat verschillende belanghebbenden de eisen op dezelfde manier begrijpen. "
                     , Str "De taal van ", Quoted  SingleQuote [Str (name fSpec)], Str " bestaat uit begrippen en basiszinnen, "
                     , Str "waarin functionele eisen worden uitgedrukt. "
                     , Str "Wanneer alle belanghebbenden afspreken dat zij deze basiszinnen gebruiken, "
                     , Str "althans voor zover het ", Quoted  SingleQuote [Str (name fSpec)], Str " betreft, "
                     , Str "delen zij precies voldoende taal om functionele eisen op dezelfde manier te begrijpen. "
                     , Str "Alle definities zijn genummerd omwille van de traceerbaarheid. "
                     ]]
        English -> [ Para
                     [ Str "This chapter defines the natural language, in which functional requirements of "
                     , Quoted  SingleQuote [Str (name fSpec)]
                     , Str " can be discussed and expressed. "
                     , Str "The purpose of this chapter is to create shared understanding among stakeholders. "
                     , Str "The language of ", Quoted  SingleQuote [Str (name fSpec)], Str " consists of concepts and basic sentences. "
                     , Str "All functional requirements are expressed in these terms. "
                     , Str "When stakeholders can agree upon this language, "
                     , Str "at least within the scope of ", Quoted  SingleQuote [Str (name fSpec)], Str ", "
                     , Str "they share precisely enough language to have meaningful discussions about functional requirements. "
                     , Str "All definitions have been numbered for the sake of traceability. "
                     ]]
  dpRequirements :: [Block]
  dpRequirements = theBlocks
    where
      (theBlocks,_) = aThemeAtATime toBeProcessedStuff (patterns fSpec) newCounter 
      toBeProcessedStuff = ( conceptsWithDefinition
                           , conceptsWithPurpose
                           , if length allRelsThatMustBeShown == length (nub allRelsThatMustBeShown) then allRelsThatMustBeShown
                             else fatal 250 "Some relations occur multiply in allRelsThatMustBeShown"
                           , [r | r<-vrules fSpec, r_usr r ] )  -- All user declared rules
         where
           conceptsWithDefinition     -- All concepts that have at least one explanation. Explanations are 
                                          -- currently bound to the conceptDefinitions of a concept.   
              = [(c, cds)
                | c <-concs fSpec
                , let cds = [cd | cd <- vConceptDefs fSpec
                               , name c == name cd
                               , not (null (cddef cd))]
                , not (null cds)]           
           conceptsWithPurpose     -- All concepts that have at least one explanation. Explanations are 
                                          -- currently bound to the conceptDefinitions of a concept.   
              = [(c, pps)
                | c <-concs fSpec
                , let pps = [p | let ps=purpose fSpec (language flags) c, p<-ps]
                , not (null pps)]           
           allRelsThatMustBeShown         -- All relations used in this specification, that are used in rules.
                                          -- and only those declarations that have at least one purpose.
              = [r | r@Rel{}<-mors fSpec
                  , not (null ( purpose fSpec (language flags) r))
                ]
                 
      aThemeAtATime :: ( [(A_Concept,[ConceptDef])]    -- all concepts that have one or more definitions. These definitions are to be used into this section and the sections to come
                       , [(A_Concept,[Explanation])]   -- all concepts that have one or more purposes. These purposes are to be used into this section and the sections to come
                       , [Relation]                    -- all relations to be processed into this section and the sections to come
                       , [Rule])                       -- all rules to be processed into this section and the sections to come
                    -> [Pattern]         -- the patterns that must be processed into this specification
                    -> Counter           -- unique definition counters
                    -> ([Block],Counter) -- The blocks that define the resulting document and the last used unique definition number
      aThemeAtATime  (still2doCdefsPre, still2doCpurpPre, still2doRelsPre, still2doRulesPre) pats iPre
           = case pats of
              []  -> printOneTheme Nothing (still2doCdefsPre, still2doRelsPre, still2doRulesPre) iPre
              _   -> (blocksOfOneTheme ++ blocksOfThemes,iPost)
         where
           (x:xs) = pats
           (blocksOfOneTheme,iPostFirst) = printOneTheme (Just x) thisThemeStuff iPre
           (blocksOfThemes,iPost)     = aThemeAtATime stuff2PrintLater xs iPostFirst
           thisThemeStuff = (thisThemeCdefs, thisThemeRels, [r | r<-thisThemeRules, r_usr r])
           thisThemeRules = [r | r<-still2doRulesPre, r_env r == name x ]      -- only user defined rules, because generated rules are documented in whatever caused the generation of that rule.
           rules2PrintLater = still2doRulesPre >- thisThemeRules
           thisThemeRels = [r | r<-still2doRelsPre, r `eleM` mors thisThemeRules] `uni`            -- all relations used in this theme's rules
                           [ makeRelation d | d<-declarations x, (not.null) (multiplicities d)] -- all relations used in multiplicity rules
           rels2PrintLater = still2doRelsPre >- thisThemeRels
           thisThemeCdefs = [(c,cd) |(c,cd)<- still2doCdefsPre, c `eleM` concs thisThemeRules]
           thisThemeCpurps = [(c,ps) |(c,ps)<- still2doCpurpPre, c `eleM` concs thisThemeRules]
           cDefs2PrintLater = still2doCdefsPre >- thisThemeCdefs
           cPurps2PrintLater = still2doCpurpPre >- thisThemeCpurps
           stuff2PrintLater = (cDefs2PrintLater, cPurps2PrintLater, rels2PrintLater, rules2PrintLater)
           
      -- | printOneTheme tells the story in natural language of a single theme.
      -- For this purpose, Ampersand authors should take care in composing explanations.
      -- Each explanation should state the purpose (and nothing else).
      printOneTheme :: Maybe Pattern -- name of the theme to process (if any)
                    -> ( [(A_Concept,[ConceptDef])]    -- all concepts that have one or more definitions, to be printed in this section
                       , [Relation]          -- Relations to print in this section
                       , [Rule])  -- Rules to print in this section
                    -> Counter      -- first free number to use for numbered items
                    -> ([Block],Counter)-- the resulting blocks and the last used number.
      printOneTheme mPat (cDefs2print, rels2print, rules2print) counters1
              = ( header' ++ explainsPat ++ sctcsIntro cDefs2print ++ concBlocks ++ relBlocks ++ ruleBlocks
                , counters4
                )
           where 
              (concBlocks,counters2) = sctcs cDefs2print counters1
              (relBlocks, counters3) = sctds rels2print  counters2
              (ruleBlocks,counters4) = sctrs rules2print counters3
              themeName = case mPat of
                           Nothing  -> ""
                           Just pat -> name pat
              header' :: [Block]
              header'  = [Header 1 [Str (case (mPat,language flags) of
                                              (Nothing, Dutch  ) -> "Losse eindjes..."
                                              (Nothing, English) -> "Loose ends..."
                                              _                  -> themeName
                                        )
                         ]         ]
              explainsPat :: [Block]
              explainsPat
               = case mPat of
                         Nothing  -> [Para 
                                      (case language flags of
                                        Dutch   -> [Str "Deze paragraaf beschrijft de relaties en concepten die niet in voorgaande secties zijn beschreven."]
                                        English -> [Str "This paragraph shows remaining fact types and concepts "
                                                   ,Str "that have not been described in previous paragraphs."]
                                      )]
                         Just pat -> explains2Blocks (purpose fSpec (language flags) pat)

              sctcsIntro :: [(A_Concept, [ConceptDef])] -> [Block]
              sctcsIntro xs
                = case xs of
                    []   -> []
                    ccds -> case language flags of
                              Dutch   ->  [Para$ (case ([Emph [Str $ unCap (name c)] |(c,_)<-xs], length [p |p <- vpatterns fSpec , name p == themeName]) of
                                                       ([] ,_) -> []
                                                       ([_],1) -> [ Str $ "In het volgende wordt de taal geÃ¯ntroduceerd ten behoeve van "++themeName++". " | themeName/=""]
                                                       (cs ,1) -> [ Str "Nu volgen definities van de concepten "]++
                                                                  commaNLPandoc (Str "en") cs++
                                                                  [ Str ". Daarna worden de basiszinnen en regels geÃ¯ntroduceerd."]
                                                       ([c],_) -> [ Str "Deze sectie introduceert het concept "
                                                                  , c]
                                                       (cs ,_) -> [ Str "Deze sectie introduceert de concepten "]++
                                                                  commaNLPandoc (Str "en") cs++
                                                                  [ Str ". "]
                                                 )++
                                                 (let cs = [(c,cds) | (c,cds)<-ccds, length cds>1] in
                                                  case (cs, length cs==length ccds) of
                                                   ([] ,   _  ) -> []
                                                   ([(c,_)]  , False) -> [ Str $ "EÃ©n daarvan, "++name c++", heeft meerdere definities. " ]
                                                   (_        , False) -> [ Str "Daarvan hebben "]++commaNLPandoc (Str "en") (map (Str . name . fst) cs)++[Str " meerdere definities. "]
                                                   ([(_,cds)], True ) -> [ Str $ "Deze heeft "++count flags (length cds) "definitie"++". " ]
                                                   (_        , True ) -> [ Str "Elk daarvan heeft meerdere definities. "]
                                                 )
                                          ]
                              English ->  [Para$ (case ([Emph [Str $ unCap (name c)] |(c,_)<-xs], length [p |p <- vpatterns fSpec , name p == themeName]) of
                                                       ([] ,_) -> []
                                                       ([_],1) -> [ Str $ "The sequel introduces the language of "++themeName++". " | themeName/=""]
                                                       (cs ,1) -> [ Str "At this point, the definitions of "]++
                                                                  commaEngPandoc (Str "and") cs++
                                                                  [ Str " are given. Directly after that, the basic sentences and rules are introduced."]
                                                       ([c],_) -> [ Str "This section introduces concept "
                                                                  , Emph [c]]
                                                       (cs ,_) -> [ Str "This section introduces concepts "]++
                                                                  commaEngPandoc (Str "and") cs++
                                                                  [ Str ". "]
                                                 )++
                                                 (let cs = [(c,cds) | (c,cds)<-ccds, length cds>1] in
                                                  case (cs, length cs==length ccds) of
                                                   ([] ,   _  ) -> []
                                                   ([(c,_)]  , False) -> [ Str $ "One of these concepts, "++name c++", has multiple definitions. " ]
                                                   (_        , False) -> [ Str "Of those concepts "]++commaEngPandoc (Str "and") (map (Str . name . fst) cs)++[Str " have multiple definitions. "]
                                                   ([(_,cds)], True ) -> [ Str $ "It has "++count flags (length cds) "definition"++". " ]
                                                   (_        , True ) -> [ Str "Each one has several definitions. "]
                                                 )
                                          ]

              sctcs :: [(A_Concept, [ConceptDef])] -> Counter -> ([Block],Counter)
              sctcs xs c0 
                = case xs of
                    []  -> ([],c0)
                    _   -> (fstBlocks ++ restBlocks,c2)
                  where
                      d':ds' = xs
                      (fstBlocks,c1) = conceptdefinitions d' c0
                      (restBlocks,c2) = sctcs ds' c1
                      conceptdefinitions :: (A_Concept,[ConceptDef]) -> Counter -> ([Block],Counter)
                      conceptdefinitions (c,cds) cnt -- ^ this function takes a tuple of a concept and -if it exists- its definition. It returns a list of [Blocks] representing the text to print for it.
                      -- First print a glossary entry (only implemented for LaTeX for now), then write the definition and then explain its purpose(s).
                       = ( explains2Blocks (purpose fSpec (language flags) c)++
                           [DefinitionList [ ( [ Str (case language flags of
                                                        Dutch   -> "Definitie "
                                                        English -> "Definition ")
                                               , Str (show(getEisnr cnt))
                                               , Str ":"]
                                             , [ makeDefinition flags cds ]
                                             )
                                           ]
                           ]
                         , incEis cnt )

              -- | sctds prints the requirements related to relations that are introduced in this theme.
              sctds :: [Relation] -> Counter -> ([Block],Counter)
              sctds xs c0 
                = case xs of
                    []  -> ([],c0)
                    _   -> (fstBlocks ++ restBlocks,c2)
                  where
                      d':ds' = xs
                      (fstBlocks,c1) = relBlock d' c0
                      (restBlocks,c2) = sctds ds' c1
                      relBlock :: Relation -> Counter -> ([Block],Counter)
                      relBlock rel cnt = ( explains2Blocks (purpose fSpec (language flags) rel) ++
                                           [DefinitionList [ ( [ Str (case language flags of
                                                                        Dutch   -> "Eis "
                                                                        English -> "Requirement ")
                                                               , Str (show(getEisnr cnt))
                                                               , Str ":"]
                                                             , [ Plain [RawInline "latex" $ symReqLabel (makeDeclaration rel)]:
                                                                 meaning (language flags) rel]
                                                             )
                                                           ]
                                           ]
                                         , incEis cnt)
                                                       
              sctrs :: [Rule] -> Counter -> ([Block],Counter)
              sctrs xs c0 
                = case xs of
                    []  -> ([],c0)
                    _   -> (fstBlocks ++ restBlocks,c2)
                  where
                      r':rs' = xs
                      (fstBlocks,c1) = ruleBlock r' c0
                      (restBlocks,c2) = sctrs rs' c1
                      ruleBlock :: Rule -> Counter -> ([Block],Counter)
                      ruleBlock r2 cnt = ( explains2Blocks (purpose fSpec (language flags) r2) ++
                                           [DefinitionList [ ( [Str (case language flags of
                                                                       Dutch   -> "Eis"
                                                                       English -> "Requirement")
                                                               ,Space
                                                               ,Str (show(getEisnr cnt))
                                                               ,if name r2=="" then Str ":" else Str (" ("++name r2++"):")]
                                                             , [ Plain [RawInline "latex" $ symReqLabel r2] :
                                                                 (let expls = [block | Means l econt<-rrxpl r2, l==language flags, block<-econt] in
                                                                  if null expls
                                                                  then explains2Blocks (autoMeaning (language flags) r2) 
                                                                  else expls 
                                                                 )]
                                                             )
                                                           ]
                                           ]
                                         , incEis cnt)
                      

