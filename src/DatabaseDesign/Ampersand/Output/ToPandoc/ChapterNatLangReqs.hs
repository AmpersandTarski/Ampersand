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
import Data.Maybe
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Output.AdlExplanation
import DatabaseDesign.Ampersand.Output.PandocAux

fatal :: Int -> String -> a
fatal = fatalMsg "ChapterNatLangReqs.hs"

chpNatLangReqs :: Int -> Fspc -> Options ->  [Block]
chpNatLangReqs lev fSpec flags = header ++ dpIntro ++ dpRequirements ++ if genLegalRefs flags then legalRefs else []
  where
  header :: [Block]
  header = labeledHeader lev (xLabel FunctionalRequirements)
                                         (case language flags of
                                             Dutch   ->  "Gemeenschappelijke taal"   
                                             English ->  "Shared Language"
                                         )
                                         
  legalRefs = [ Header 1 [Str sectionTitle
                         ]   
              ,  Plain [ RawInline "latex" $  unlines $
                         [ "\\begin{longtable}{lp{10cm}}"
                         , "\\hline "
                         , "{\\bf "++lawHeader ++ "} & {\\bf " ++ articleHeader ++"} \\\\"
                         , "\\hline"
                         , "\\endhead\n" ] ++ 
                         [ getWet ref ++ " & " ++ getArtikel ref ++"\\\\\n"
                         | ref <- nub . filter (not . null) . map explRefId $ explanations fSpec 
                         ] ++
                         [ "\\end{longtable}" ]
                       ]
                         
              ]
         where getWet ref = reverse . takeWhile (/=' ') . reverse $ ref --  the law is the last word in the ref
               getArtikel ref = reverse . dropWhile (`elem` " ,") .dropWhile (/=' ') . reverse $ ref 
               -- the article is everything but the law (and we also drop any trailing commas)
               (sectionTitle, lawHeader, articleHeader) = 
                 case language flags of
                   Dutch   -> ("Referentietabel", "Wet", "Artikel")
                   English -> ("Reference table", "Law", "Article")

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
                      then aThemeAtATime toBeProcessedStuff (patterns fSpec) newCounter
                      else aThemeAtATime toBeProcessedStuff [ pat | pat<-patterns fSpec, name pat `elem` themes fSpec ] newCounter
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
                    -> Counter           -- unique definition counters
                    -> ([Block],Counter) -- The blocks that define the resulting document and the last used unique definition number
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
                               [ makeRelation d | d@Sgn{decusr=True} <- declarations x, (not.null) (multiplicities d)] -- all relations used in multiplicity rules
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
                    -> Counter      -- first free number to use for numbered items
                    -> ([Block],Counter)-- the resulting blocks and the last used number.
      printOneTheme mPat (concs2print, rels2print, rules2print) counter0
              = case (mPat, themes fSpec) of
                 (Nothing, _:_) -> ( [], counter0 )         -- The document is partial (because themes have been defined), so we don't print loose ends.
                 _              -> ( header' ++ explainsPat ++ sctcsIntro concs2print ++ reqdefs
                                   , Counter (getEisnr counter0 + length reqs)
                                   )
           where
              -- sort the requirements by file position
              reqs = sort' fst [((linenr a,colnr a), bs) | (a,bs)<-sctds rels2print ++ sctrs rules2print ++ sctcs concs2print]
              -- make blocks for requirements
              reqblocks = [(pos,req (Counter cnt)) | (cnt,(pos,req))<-zip [(getEisnr counter0)..] reqs]
              reqdefs = concatMap snd reqblocks

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
                         Just pat -> purposes2Blocks flags purps
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
              --   after sorting by origin the counter will be applied
              sctcs :: [(A_Concept, [Purpose])] -> [(Origin, Counter -> [Block])]
              sctcs = let mborigin c = if null(uniquecds c) then OriginUnknown else (origin . snd . head . uniquecds) c
                      in map (\(c,exps) -> (mborigin c, cptBlock (c,exps)))
              -- | make a block for a c with all its purposes and definitions
              cptBlock :: (A_Concept, [Purpose]) -> Counter -> [Block]
              cptBlock (c,exps) cnt = concat [amPandoc (explMarkup e) | e<-exps] 
                  ++ zipWith (curry cdBlock)
                       (if length (uniquecds c) == 1 then [(cnt, "")] else
                          [(cnt, '.' : show i) | i <- [(1 :: Int) ..]])
                       (uniquecds c)
              -- | make a block for a concept definition
              cdBlock :: ((Counter,String),(String,ConceptDef)) -> Block
              cdBlock ((cnt,xcnt),(cdnm,cd)) = DefinitionList 
                                        [( [ Str (case language flags of
                                                                 Dutch   -> "Definitie "
                                                                 English -> "Definition ")
                                           , Str (show (getEisnr cnt)++xcnt)
                                           , Str ":"]
                                         , [ makeDefinition flags (getEisnr cnt,cdnm,cd) ])]

              -- | sctds prints the requirements related to relations that are introduced in this theme.
              sctds :: [Relation] -> [(Origin, Counter -> [Block])]
              sctds = map (\rel -> (origin (makeDeclaration rel), relBlock rel))
              relBlock :: Relation -> Counter -> [Block]
              relBlock rel cnt 
               = purposes2Blocks flags purps
                 ++ 
                 [DefinitionList [ ( [ Str (case language flags of
                                                      Dutch   -> "Eis "
                                                      English -> "Requirement ")
                                     , Str (show(getEisnr cnt))
                                     , Str ":"]
                                   , [ Plain [RawInline "latex" $ symReqLabel (makeDeclaration rel)]:
                                       meaning2Blocks (language flags) (makeDeclaration rel)
                                     ]
                                   )] ]++
                 ( case (language flags, length dp) of
                        (_      , 0) -> []
                        (Dutch  , 1) -> [Para [Str "Een zin die hiermee gemaakt kan worden is bijvoorbeeld:"]]
                        (English, 1) -> [Para [Str "A sentence that can be formed is for instance:"]]
                        (Dutch  , _) -> [Para [Str "Zinnen die hiermee gemaakt kunnen worden zijn bijvoorbeeld:"]]
                        (English, _) -> [Para [Str "Sentences that can be made are for instance:"]]
                 )++
                 [ Para $ applyM d srcKeyAtom tgtKeyAtom 
                 | (a,b)<-dp
                 , let srcKeyAtom = showKeyAtom fSpec (Just rel) (source rel) a 
                 , let tgtKeyAtom = showKeyAtom fSpec Nothing (target rel) b
                 ]
                 where purps = purposes fSpec (language flags) rel
                       d     = makeDeclaration rel
                       dp    = take 3 (decpopu d)
              sctrs :: [Rule] -> [(Origin,Counter -> [Block])]
              sctrs = map (\rul -> (origin rul, ruleBlock rul))
              ruleBlock :: Rule -> Counter -> [Block]
              ruleBlock rul cnt 
               =  purposes2Blocks flags purps
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
                      
  applyM :: Declaration -> String -> String -> [Inline]
  applyM decl a b
   = if fspecFormat flags==FLatex
     then let a'  = latexEscShw a
              b'  = latexEscShw b
              str = RawInline "latex"
          in case decl of
             Sgn{} | null (prL++prM++prR) 
                        -> [str (upCap a'),Space,str "(",(str.latexEscShw.unCap.name.source) decl, str ")",str "corresponds",Space,str "to",Space,str b',Space,str "(",(str.latexEscShw.unCap.name.target) decl, str ")",str "in",Space,str "relation",Space,str (decnm decl),str "."]
                   | null prL
                        -> [str "(",(str.latexEscShw.name.source) decl, str ") ",str a',Space,str prM,Space,str "(",(str.latexEscShw.unCap.name.target) decl, str ") ",str b',Space,str prR,str "."]
                   | otherwise 
                        -> [str (upCap prL),Space,str "(",(str.latexEscShw.unCap.name.source) decl, str ") ",str a',Space,str prM,Space,str "(",(str.latexEscShw.unCap.name.target) decl, str ") ",str b']++if null prR then [str "."] else [Space,str prR,str "."]
                          where prL = latexEscShw (decprL decl)
                                prM = latexEscShw (decprM decl)
                                prR = latexEscShw (decprR decl)
             Isn{}     -> [str "(",(str.latexEscShw.name.source) decl, str ") ",str (upCap a'),Space,str "equals",Space,str b',str "."]
             Iscompl{} -> [str "(",(str.latexEscShw.name.source) decl, str ") ",str (upCap a'),Space,str "differs",Space,str "from",Space,str b',str "."]
             Vs{}      -> [str (show True)]
     else case decl of
             Sgn{} | null (prL++prM++prR) 
                        -> [Str (upCap a),Space,Str "(",(Str .unCap.name.source) decl, Str ")",Str "corresponds",Space,Str "to",Space,Str b,Space,Str "(",(Str .unCap.name.target) decl, Str ")",Str "in",Space,Str "relation",Space,Str (decnm decl),Str "."]
                   | null prL
                        -> [Str "(",(Str .name.source) decl, Str ") ",Str a,Space,Str prM,Space,Str "(",(Str .unCap.name.target) decl, Str ") ",Str b,Space,Str prR,Str "."]
                   | otherwise 
                        -> [Str (upCap prL),Space,Str "(",(Str .unCap.name.source) decl, Str ") ",Str a,Space,Str prM,Space,Str "(",(Str .unCap.name.target) decl, Str ") ",Str b]++if null prR then [Str "."] else [Space,Str prR,Str "."]
                          where prL = decprL decl
                                prM = decprM decl
                                prR = decprR decl
             Isn{}     -> [Str "(",(Str .name.source) decl, Str ") ",Str (upCap a),Space,Str "equals",Space,Str b,Str "."]
             Iscompl{} -> [Str "(",(Str .name.source) decl, Str ") ",Str (upCap a),Space,Str "differs",Space,Str "from",Space,Str b,Str "."]
             Vs{}      -> [Str (show True)]


-- TODO: fix showing/not showing based on relation
-- TODO: what about relations in the target key?
-- TODO: move these to some auxiliaries or utils
showKeyAtom :: Fspc -> Maybe Relation -> A_Concept -> String -> String
showKeyAtom fSpec mRel cncpt atom =
  case mapMaybe (getKey fSpec) (cncpt : getGeneralizations fSpec cncpt) of
    []    -> atom
    key:_ -> if fmap ERel mRel `elem` justKeyRels then atom else concatMap showKeySegment $ kdats key 
     where showKeySegment (KeyText str) = str
           showKeySegment (KeyExp objDef) = 
             case [ tgtAtom | (srcAtom, tgtAtom) <- contents (objctx objDef), atom == srcAtom ] of
               []        -> ""
               keyAtom:_ -> keyAtom  
               
           justKeyRels = map (Just . objctx) [objDef | KeyExp objDef <- kdats key]
      
getKey :: Fspc -> A_Concept -> Maybe KeyDef
getKey fSpec cncpt = 
  case filter ((== cncpt) .  kdcpt) $ vkeys fSpec of
    []       -> Nothing 
    keyDef:_ -> Just keyDef 


 