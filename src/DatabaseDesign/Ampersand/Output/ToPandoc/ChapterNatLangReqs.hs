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
import DatabaseDesign.Ampersand.Output.AdlExplanation
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
      (theBlocks,_) = if null (themes fSpec)
                      then aThemeAtATime toBeProcessedStuff (patterns fSpec) (newCounter,Counter 0)
                      else aThemeAtATime toBeProcessedStuff [ pat | pat<-patterns fSpec, name pat `elem` themes fSpec ] (newCounter,Counter 0)
      toBeProcessedStuff = ( conceptsWith
                           , if length allRelsThatMustBeShown == length (nub allRelsThatMustBeShown) then allRelsThatMustBeShown
                             else fatal 250 "Some relations occur multiply in allRelsThatMustBeShown"
                           , [r | r<-vrules fSpec, r_usr r ] )  -- All user declared rules
         where
           conceptsWith     -- All concepts that have at least one definition or one userdefined purpose. 
              = [(c, pps)
                | c <-concs fSpec
                , let pps = [p | p <- purposes fSpec (language flags) c, explUserdefd p]
                , not (null (cptdf c)) || not (null pps)]           
           allRelsThatMustBeShown         -- All relations used in this specification, that are used in rules.
                                          -- and only those declarations that have at least one userdefined purpose.
              = [r | r@Rel{}<-mors fSpec
                   , (not . null) ( purposes fSpec (language flags) r)
                ]
                 
      aThemeAtATime :: ( [(A_Concept,[Purpose])]   -- all concepts that have one or more definitions or purposes. These are to be used into this section and the sections to come
                       , [Relation]                                 -- all relations to be processed into this section and the sections to come
                       , [Rule])                                    -- all rules to be processed into this section and the sections to come
                    -> [Pattern]         -- the patterns that must be processed into this specification
                    -> (Counter,Counter)           -- unique definition counters
                    -> ([Block],(Counter,Counter)) -- The blocks that define the resulting document and the last used unique definition number
      aThemeAtATime  (still2doCPre, still2doRelsPre, still2doRulesPre) pats iPre 
           = case pats of
              []  -> printOneTheme Nothing (still2doCPre, still2doRelsPre, still2doRulesPre) iPre
              _   -> (blocksOfOneTheme ++ blocksOfThemes,iPost)
         where
           (x:xs) = pats
           (blocksOfOneTheme,iPostFirst) = printOneTheme (Just x) thisThemeStuff iPre
           (blocksOfThemes,iPost)        = aThemeAtATime stuff2PrintLater xs iPostFirst
           thisThemeStuff    = (thisThemeCs, thisThemeRels, [r | r<-thisThemeRules, r_usr r])
           thisThemeRules    = [r | r<-still2doRulesPre, r_env r == name x ]      -- only user defined rules, because generated rules are documented in whatever caused the generation of that rule.
           rules2PrintLater  = still2doRulesPre >- thisThemeRules
           thisThemeRels     = [r | r<-still2doRelsPre, r `eleM` mors thisThemeRules] `uni`            -- all relations used in this theme's rules
                               [ makeRelation d | d<-declarations x, (not.null) (multiplicities d)] -- all relations used in multiplicity rules
           rels2PrintLater   = still2doRelsPre >- thisThemeRels
           thisThemeCs       = [(c,ps) |(c,ps)<- still2doCPre, c `eleM` (concs thisThemeRules ++ concs thisThemeRels)] -- relations are rules ('Eis') too
           concs2PrintLater  = still2doCPre >- thisThemeCs
           stuff2PrintLater  = (concs2PrintLater, rels2PrintLater, rules2PrintLater)
--           (blocksOfThemes,iPost)     = aThemeAtATime stuff2PrintLater xs iPostFirst
--           thisThemeStuff = (thisThemeCdefs, thisThemeRels, [r | r<-thisThemeRules, r_usr r])
--           thisThemeRules = [r | r<-still2doRulesPre, r_env r == name x ]      -- only user defined rules, because generated rules are documented in whatever caused the generation of that rule.
--           rules2PrintLater = still2doRulesPre >- thisThemeRules
--           thisThemeRels = [r | r<-still2doRelsPre, r `eleM` mors thisThemeRules] `uni`            -- all relations used in this theme's rules
--                           [ makeRelation d | d<-declarations x, (not.null) (multiplicities d)] -- all relations used in multiplicity rules
--           rels2PrintLater = still2doRelsPre >- thisThemeRels
--           thisThemeCdefs = [(c,cd) |(c,cd)<- still2doCdefsPre, c `eleM` (concs thisThemeRules ++ concs thisThemeRels)]
--           thisThemeCpurps = [(c,ps) |(c,ps)<- still2doCpurpPre, c `eleM` (concs thisThemeRules ++ concs thisThemeRels)]
--           cDefs2PrintLater = still2doCdefsPre >- thisThemeCdefs
--           cPurps2PrintLater = still2doCpurpPre >- thisThemeCpurps
--           stuff2PrintLater = (cDefs2PrintLater, cPurps2PrintLater, rels2PrintLater, rules2PrintLater)
           
      -- | printOneTheme tells the story in natural language of a single theme.
      -- For this purpose, Ampersand authors should take care in composing explanations.
      -- Each explanation should state the purpose (and nothing else).
      printOneTheme :: Maybe Pattern -- name of the theme to process (if any)
                    -> ( [(A_Concept,[Purpose])]    -- all concepts that have one or more definitions, to be printed in this section
                       , [Relation]          -- Relations to print in this section
                       , [Rule])             -- Rules to print in this section
                    -> (Counter,Counter)      -- first free number to use for numbered items
                    -> ([Block],(Counter,Counter))-- the resulting blocks and the last used number.
      printOneTheme mPat (concs2print, rels2print, rules2print) (reqcnt,defcnt)
              = case (mPat, themes fSpec) of
                 (Nothing, _:_) -> ( [], (reqcnt,defcnt) )         -- The document is partial (because themes have been defined), so we don't print loose ends.
                 _              -> ( header' ++ explainsPat ++ sctcsIntro concs2print ++ reqdefblocks
                                   , ( Counter (getEisnr reqcnt + length reqs)
                                     , Counter (snd(last cntss)) )
                                   )
           where
              -- sort the requirements by file position
              reqs = sort' fst [((linenr a,colnr a), bs) | (a,bs)<-sctds rels2print ++ sctrs rules2print ]
              -- make blocks for requirements
              reqblocks = [(pos,req (Counter cnt)) | (cnt,(pos,req))<-zip [(getEisnr reqcnt)..] reqs]
              -- sort the definitions by file position
              defs = sort' fst [((linenr a,colnr a), (i, def)) | (a, i, def)<-sctcs concs2print]
              -- cntss is a list of pairs which should be interpreted as counters [fst..snd] for concept definitions of A_Concept c
              -- for example: + fst==snd for c with only one concept def i.e. [fst..snd]==[fst]
              --              + fst-1==snd for c without concept defs i.e. [fst..snd]==[]
              --              + fst+1==snd for c with two concept defs i.e. [fst..snd]==[fst,fst+1]
              cntss :: [(Int,Int)]
              cntss = tail $ foldl (\x y -> x ++ [(snd(last x)+1,snd(last x) + head y)]) 
                                   [(fatal 133 "",getEisnr defcnt)] 
                                   [[i] | (_,(i,_))<-defs]
              -- make blocks for concepts
              defblocks = [(pos,def cnts) | (cnts, (pos,(_,def)))<-zip [ map Counter [i..j] | (i,j) <- cntss] defs ]
              -- sort the requirements and concept definitions
              reqsdefs = sort' fst (reqblocks ++ defblocks)
              -- make one [block] of requirements and concept definitions
              reqdefblocks = concat (map snd reqsdefs)

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
                         Just pat -> purposes2Blocks purps
                                     where purps = purposes fSpec (language flags) pat

              sctcsIntro :: [(A_Concept, [Purpose])] -> [Block]
              sctcsIntro [] = []
              sctcsIntro ccds 
                = case language flags of
                              Dutch   ->  [Para$ (case ([Emph [Str $ unCap (name c)] | (c,_)<-ccds], length [p |p <- vpatterns fSpec , name p == themeName]) of
                                                       ([] ,_) -> []
                                                       ([_],1) -> [ Str $ "In het volgende wordt de taal geïntroduceerd ten behoeve van "++themeName++". " | themeName/=""]
                                                       (cs ,1) -> [ Str "Nu volgen definities van de concepten "]++
                                                                  commaNLPandoc (Str "en") cs++
                                                                  [ Str ". Daarna worden de basiszinnen en regels geïntroduceerd."]
                                                       ([c],_) -> [ Str "Deze sectie introduceert het concept "
                                                                  , c]
                                                       (cs ,_) -> [ Str "Deze sectie introduceert de concepten "]++
                                                                  commaNLPandoc (Str "en") cs++
                                                                  [ Str ". "]
                                                 )++
                                                 (let cs = [(c,cds,cps) | (c,cps)<-ccds, let cds = cptdf c,length cds>1] in
                                                  case (cs, length cs==length ccds) of
                                                   ([]         ,   _  ) -> []
                                                   ([(c,_,_)]  , False) -> [ Str $ "Eén daarvan, "++name c++", heeft meerdere definities. " ]
                                                   (_          , False) -> [ Str "Daarvan hebben "]++commaNLPandoc (Str "en") (map (Str . name . fst3) cs)++[Str " meerdere definities. "]
                                                   ([(_,cds,_)], True ) -> [ Str $ "Deze heeft "++count flags (length cds) "definitie"++". " ]
                                                   (_          , True ) -> [ Str "Elk daarvan heeft meerdere definities. "]
                                                 )
                                          ]
                              English ->  [Para$ (case ([Emph [Str $ unCap (name c)] |(c,_)<-ccds], length [p |p <- vpatterns fSpec , name p == themeName]) of
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
                                                 (let cs = [(c,cds,cps) | (c,cps)<-ccds, let cds = cptdf c, length cds>1] in
                                                  case (cs, length cs==length ccds) of
                                                   ([]         ,   _  ) -> []
                                                   ([(c,_,_)]  , False) -> [ Str $ "One of these concepts, "++name c++", has multiple definitions. " ]
                                                   (_          , False) -> [ Str "Of those concepts "]++commaEngPandoc (Str "and") (map (Str . name . fst3) cs)++[Str " have multiple definitions. "]
                                                   ([(_,cds,_)], True ) -> [ Str $ "It has "++count flags (length cds) "definition"++". " ]
                                                   (_          , True ) -> [ Str "Each one has several definitions. "]
                                                 )
                                          ]
                  where fst3 (a,_,_) = a

              -- | the origin of c is the origin of the head of uniquecds c
              --   the integer defines the number of concept defs for c
              --   after sorting by origin the counters will be applied
              sctcs :: [(A_Concept, [Purpose])] -> [(Origin, Int, [Counter] -> [Block])]
              sctcs = let mborigin c = if null(uniquecds c) then OriginUnknown else (origin . snd . head . uniquecds) c
                      in map (\(c,exps) -> (mborigin c, length (uniquecds c), cptBlock (c,exps)))
              -- | make a block for a c with all its purposes and definitions
              cptBlock :: (A_Concept, [Purpose]) -> [Counter] -> [Block]
              cptBlock (c,exps) cnts = concat [amPandoc (explMarkup e) | e<-exps] ++ map cdBlock (zip cnts (uniquecds c))
              -- | make a block for a concept definition
              cdBlock :: (Counter,(String,ConceptDef)) -> Block
              cdBlock (cnt,(cdnm,cd)) = DefinitionList 
                                        [( [ Str (case language flags of
                                                                 Dutch   -> "Definitie "
                                                                 English -> "Definition ")
                                           , Str (show (getEisnr cnt))
                                           , Str ":"]
                                         , [ makeDefinition flags (getEisnr cnt,cdnm,cd) ])]


              -- | sctds prints the requirements related to relations that are introduced in this theme.
              sctds :: [Relation] -> [(Origin, Counter -> [Block])]
              sctds = map (\rel -> (origin rel, relBlock rel))
              relBlock :: Relation -> Counter -> [Block]
              relBlock rel cnt 
               = purposes2Blocks purps
                 ++ 
                 [DefinitionList [ ( [ Str (case language flags of
                                                      Dutch   -> "Eis "
                                                      English -> "Requirement ")
                                             , Str (show(getEisnr cnt))
                                             , Str ":"]
                                           , [ Plain [RawInline "latex" $ symReqLabel (makeDeclaration rel)]:
                                               meaning2Blocks (language flags) (makeDeclaration rel)]
                                           )] ]
                 where purps = purposes fSpec (language flags) rel
              sctrs :: [Rule] -> [(Origin,Counter -> [Block])]
              sctrs = map (\rul -> (origin rul, ruleBlock rul))
              ruleBlock :: Rule -> Counter -> [Block]
              ruleBlock rul cnt 
               =  purposes2Blocks purps
                  ++
                  [DefinitionList [ ( [Str (case language flags of
                                                               Dutch   -> "Eis"
                                                               English -> "Requirement")
                                                       ,Space
                                                       ,Str (show(getEisnr cnt))
                                                       ,if name rul=="" then Str ":" else Str (" ("++name rul++"):")]
                                                     , [ Plain [RawInline "latex" $ symReqLabel rul] :
                                                         meaning2Blocks (language flags) rul
                                                      ]
                                                     )
                                                   ] | not (null$meaning2Blocks (language flags) rul)]
                                 where purps = purposes fSpec (language flags) rul
                      

