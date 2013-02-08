{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
    ( module Text.Pandoc
    , module Text.Pandoc.Builder
    , Chapter(..)
    , chaptersInDoc
    , chptHeader
    , chptTitle
    , Xreferencable(..)
    , xrefFigure1
    , canXRefer
    , Purpose(..)
    , purposeOf
    , purposesDefinedIn
    , purposes2Blocks
    , meaning2Blocks
    , amPandoc
    , isMissing
    , lclForLang
    , dpRule
    , Counter(..),newCounter,incEis
    , noBlocks
    ,langSwitch
    ,(===>)
    , inlineIntercalate
    )
where
import DatabaseDesign.Ampersand.Basics  
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Fspec
import Text.Pandoc
import Text.Pandoc.Builder
import DatabaseDesign.Ampersand.Output.PredLogic        (PredLogicShow(..), showLatex)
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Output.AdlExplanation
import DatabaseDesign.Ampersand.Output.PandocAux
import Data.List             (intercalate,partition)
import System.Locale
import qualified Data.Sequence as Seq

fatal :: Int -> String -> a
fatal = fatalMsg "Output.ToPandoc.SharedAmongChapters.hs"

data Chapter = Intro 
             | NatLangReqs
             | FunctReqts 
             | Diagnosis 
             | ConceptualAnalysis
             | ProcessAnalysis
             | DataAnalysis
             | SoftwareMetrics
             | EcaRules
             | Interfaces
             | Glossary
             deriving (Eq, Show)

-- | Define the order of the chapters in the document.
chaptersInDoc :: Options -> [Chapter]  
chaptersInDoc flags
 | test flags                  = [DataAnalysis]
 | diagnosisOnly flags         = [Diagnosis]
 | theme flags == StudentTheme = [ Intro
                                 , NatLangReqs
                                 , FunctReqts 
                                 , Diagnosis 
                                 , ConceptualAnalysis
                                 , ProcessAnalysis
                                 , DataAnalysis
                                 ]
 | otherwise                   = [ Intro 
                                 , NatLangReqs
                                 , FunctReqts 
                                 , Diagnosis 
                                 , ConceptualAnalysis
                                 , ProcessAnalysis
                                 , DataAnalysis
                                 , SoftwareMetrics
                        --         , EcaRules
                                 , Interfaces
                                 , Glossary
                                 ]

-- | This function returns a header of a chapter
chptHeader :: Options -> Chapter -> Blocks
chptHeader flags cpt 
 = header 1 (chptTitle flags cpt )
 
chptTitle :: Options -> Chapter -> Inlines
chptTitle flags cpt =
     (case (cpt,language flags) of
        (Intro             , Dutch  ) -> text "Inleiding"
        (Intro             , English) -> text "Introduction"
        (NatLangReqs       , Dutch  ) -> text "Gemeenschappelijke taal"
        (NatLangReqs       , English) -> text "Shared Language"
        (FunctReqts        , Dutch  ) -> text "Functionele eisen en wensen" 
        (FunctReqts        , English) -> text "Functional requirements" 
        (Diagnosis         , Dutch  ) -> text "Diagnose" 
        (Diagnosis         , English) -> text "Diagnosis" 
        (ConceptualAnalysis, Dutch  ) -> text "Conceptuele Analyse"
        (ConceptualAnalysis, English) -> text "Conceptual Analysis"
        (ProcessAnalysis   , Dutch  ) -> text "Procesanalyse"
        (ProcessAnalysis   , English) -> text "Process Analysis"
        (DataAnalysis      , Dutch  ) -> text "Gegevensstructuur"
        (DataAnalysis      , English) -> text "Data structure"
        (SoftwareMetrics   , Dutch  ) -> text "Functiepunt Analyse"
        (SoftwareMetrics   , English) -> text "Function Point Analysis"
        (EcaRules          , Dutch  ) -> text "ECA regels" 
        (EcaRules          , English) -> text "ECA rules (Flash points)" 
        (Interfaces        , Dutch  ) -> text "Koppelvlakken" 
        (Interfaces        , English) -> text "Interfaces"
        (Glossary          , Dutch  ) -> text "Inhoud"
        (Glossary          , English) -> text "Glossary"
     )

class Xreferencable a where
  xLabel :: a  -> String
  xrefReference :: a  -> Inline   --Depreciated! TODO: use xRefReference instead
  xrefReference a = RawInline "latex" ("\\ref{"++xLabel a++"}")
  xRefReference :: Options -> a -> Inlines
  xRefReference flags a 
    | canXRefer flags = rawInline "latex" ("\\label{"++xLabel a++"}")
    | otherwise       = text "fatal 89 xreferencing is not supported!"
  xrefLabel :: a -> Inline
  xrefLabel a = RawInline "latex" ("\\label{"++xLabel a++"}")

canXRefer :: Options -> Bool
canXRefer opts = fspecFormat opts `elem` [FLatex] 

instance Xreferencable Chapter where
  xLabel a = "chapter" ++ escapeNonAlphaNum (show a)
  
instance Xreferencable Picture where
  xLabel a = "figure" ++ escapeNonAlphaNum (uniqueName a)

--Image [Inline] Target
--      alt.text (URL,title)
xrefFigure1 :: Picture -> [Inline]
xrefFigure1 pict = 
   [ RawInline "latex" "\\begin{figure}[htb]\n\\begin{center}\n\\scalebox{.3}[.3]{"
   , Image [Str $ "Here, "++uniqueName pict++" should have been visible"] (uniqueName pict, xLabel pict)
   , RawInline "latex" "}\n"
   , RawInline "latex" ("\\caption{"++latexEscShw (caption pict)++"}\n") 
   , xrefLabel pict
   , RawInline "latex" "\n\\end{center}\n\\end{figure}"]

-- | This function orders the content to print by theme. It returns a list of 
--   tripples by theme. The last tripple might not have a theme, but will contain everything
--   that isn't handled in a specific theme.
orderingByTheme :: Fspc -> [( Maybe Theme   -- A theme is about either a pattern of a process. 
                            , [Rule]        -- The rules of that theme
                            , [Relation]    -- The relations that are used in a rule of this theme, but not in any rule of a previous theme.
                            , [A_Concept]   -- The concepts that are used in a rule of this theme, but not in any rule of a previous theme.
                            )
                           ]
orderingByTheme fSpec 
 = f (allRules fSpec) (allRelations fSpec) (allConcepts fSpec) tms
 where
  -- | The themes that should be taken into account for this ordering
  tms = if null (themes fSpec)
        then map PatternTheme (patterns fSpec) ++ map (ProcessTheme . fpProc) (vprocesses fSpec)
        else [ PatternTheme pat           | pat <-patterns   fSpec, name pat  `elem` themes fSpec ]
           ++[ ProcessTheme (fpProc fprc) | fprc<-vprocesses fSpec, name fprc `elem` themes fSpec ] 
  f ruls rels cpts ts
   = case ts of
       t:ts' -> let ( (rulsOfTheme,rulsNotOfTheme)
                     , (relsOfTheme,relsNotOfTheme)
                     , (cptsOfTheme,cptsNotOfTheme)
                     ) = partitionByTheme t ruls rels cpts 
                in (Just t, rulsOfTheme, relsOfTheme, cptsOfTheme)
                   : f rulsNotOfTheme relsNotOfTheme cptsNotOfTheme ts'
       []    -> [(Nothing, ruls, rels, cpts)]
  -- | This function takes care of partitioning each of the 
  --   lists in a pair of lists of elements which do and do not belong
  --   to the theme, respectively 
  partitionByTheme :: Theme
                   -> [Rule]
                   -> [Relation]
                   -> [A_Concept]
                   -> ( ([Rule],[Rule])
                      , ([Relation],[Relation])
                      , ([A_Concept],[A_Concept])
                      )
  partitionByTheme theme ruls rels cpts
      = ((rulsOfTheme,rulsNotOfTheme), (relsOfTheme,relsNotOfTheme), (cptsOfTheme,cptsNotOfTheme))
     where 
       (rulsOfTheme,rulsNotOfTheme) = partition isRulOfTheme ruls
       isRulOfTheme r = r `elem` (case theme of
                                    PatternTheme pat -> ptrls pat
                                    ProcessTheme prc -> prcRules prc
                                 )
       (relsOfTheme,relsNotOfTheme) = partition isRelOfTheme rels
       isRelOfTheme r = r `elem` concatMap mors rulsOfTheme
       (cptsOfTheme,cptsNotOfTheme) = partition isCptOfTheme cpts
       isCptOfTheme c = c `elem` concatMap concs relsOfTheme
        
        
--GMI: What's the meaning of the Int?
dpRule :: Fspc -> Options -> [Rule] -> Int -> [A_Concept] -> [Declaration]
          -> ([([Inline], [[Block]])], Int, [A_Concept], [Declaration])
dpRule fSpec flags = dpR
 where
   dpR [] n seenConcs seenDeclarations = ([], n, seenConcs, seenDeclarations)
   dpR (r:rs) n seenConcs seenDeclarations
     = ( ( [Str (name r)]
         , [ let purps = purposesDefinedIn fSpec (language flags) r in            -- Als eerste de uitleg van de betreffende regel..
             purposes2Blocks flags purps ++
             purposes2Blocks flags [p | d<-nds, p<-purposesDefinedIn fSpec (language flags) d] ++  -- Dan de uitleg van de betreffende relaties
             [ Plain text1 | not (null nds)] ++
             pandocEqnArray [ ( texOnly_Id(name d)
                              , ":"
                              , texOnly_Id(name (source d))++(if isFunction d then texOnly_fun else texOnly_rel)++texOnly_Id(name(target d))++symDefLabel d
                              )
                            |d<-nds] ++
             [ Plain text2 | not (null rds)] ++
             [ Plain text3 ] ++
             (if showPredExpr flags
              then pandocEquation ((showLatex.toPredLogic) r++symDefLabel r)
              else pandocEquation (showMath r++symDefLabel r)
             )++
             [ Plain text4 | isSignal r] ++
             (if not (isSignal r) then [] else
              if showPredExpr flags
              then pandocEquation ((showLatex.toPredLogic) r++symDefLabel r)
              else pandocEquation (showMath r++symDefLabel r)
             )++
             [ Plain text5 | length nds>1]
           ] 
         ): dpNext
       , n'
       , seenCs 
       , seenDs
       )
       where
        text1
         = case (length nds,language flags) of
             (1,Dutch)   -> let d = head nds in
                            [Str ("Om dit te formaliseren is een "++(if isFunction d then "functie" else "relatie")++" "),Str (name d),Str " nodig (",RawInline "latex" $ symDefRef d,Str "):"]
             (1,English) -> let d = head nds in
                            [Str "In order to formalize this, a ", Str (if isFunction d then "function" else "relation"), Space, Str (name d),Str " is introduced (",RawInline "latex" $ symDefRef d,Str "):"]
             (l,Dutch)   -> [Str "Om te komen tot de formalisatie in vergelijking",RawInline "latex" "~",RawInline "latex" $ symDefRef r,Str (" zijn de volgende "++count flags l "relatie"++" nodig.")]
             (l,English) -> [Str "To arrive at the formalization in equation",RawInline "latex" "~",RawInline "latex" $ symDefRef r,Str (", the following "++count flags l "relation"++" are introduced.")]
        text2
         = (case (length nds,length rds,language flags) of
             (0,1,Dutch)   -> [Str "Definitie ",RawInline "latex" $ symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ") wordt gebruikt"]
             (0,1,English) -> [Str "We use definition ",RawInline "latex" $ symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ")"]
             (0,_,Dutch)   -> Str "We gebruiken definities ":commaNLPandoc (Str "en") [RawInline "latex" $ symDefRef d++" ("++texOnly_Id (name d)++")" |d<-rds]
             (0,_,English) -> Str "We use definitions ":commaEngPandoc (Str "and") [RawInline "latex" $ symDefRef d++" ("++texOnly_Id (name d)++")" |d<-rds]
             (_,1,Dutch)   -> [Str "Daarnaast gebruiken we definitie ",RawInline "latex" $ symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ")"]
             (_,1,English) -> [Str "Beside that, we use definition ",RawInline "latex" $ symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ")"]
             (_,_,Dutch)   -> Str "Ook gebruiken we definities ":commaNLPandoc (Str "en") [RawInline "latex" $ symDefRef d++" ("++texOnly_Id (name d)++")" |d<-rds]
             (_,_,English) -> Str "We also use definitions ":commaEngPandoc (Str "and") [RawInline "latex" $ symDefRef d++" ("++texOnly_Id (name d)++")" |d<-rds]
           )++
           (case (length nds,language flags) of
             (1,Dutch)   -> [Str " om eis",RawInline "latex" "~",RawInline "latex" $ symReqRef r, Str " (pg.",RawInline "latex" "~",RawInline "latex" $ symReqPageRef r, Str ") te formaliseren:"]
             (1,English) -> [Str " to formalize requirement",RawInline "latex" "~",RawInline "latex" $ symReqRef r, Str " (page",RawInline "latex" "~",RawInline "latex" $ symReqPageRef r, Str "):"]
             _           -> [Str ". "]
           )
        text3
         = case (language flags,isSignal r) of
            (Dutch  ,False) -> [Str "Dit betekent: "]
            (English,False) -> [Str "This means: "]
            (Dutch  ,True)  -> [Str "Activiteiten, die door deze regel zijn gedefinieerd, zijn afgerond zodra: "]
            (English,True)  -> [Str "Activities that are defined by this rule are finished when: "]
        text4
         = case language flags of
                 Dutch   -> [Str " Deze activiteiten worden opgestart door:"]
                 English -> [Str " These activities are signalled by:"]
        text5
         = case (language flags,isSignal r) of
             (Dutch  ,False) -> [Str "Dit komt overeen met eis",RawInline "latex" "~",RawInline "latex" $ symReqRef r, Str " op pg.",RawInline "latex" "~",RawInline "latex" $ symReqPageRef r, Str "."]
             (English,False) -> [Str "This corresponds to requirement",RawInline "latex" "~",RawInline "latex" $ symReqRef r, Str " on page",RawInline "latex" "~",RawInline "latex" $ symReqPageRef r, Str "."]
             (Dutch  ,True)  -> [ Str "Dit komt overeen met "
                                , Quoted  SingleQuote [Str (name r)]
                                , Str " (",RawInline "latex" $ symReqRef r, Str " op pg.",RawInline "latex" "~",RawInline "latex" $ symReqPageRef r, Str ")."]
             (English,True)  -> [Str "This corresponds to "
                                , Quoted  SingleQuote [Str (name r)]
                                , Str " (",RawInline "latex" $ symReqRef r, Str " op pg.",RawInline "latex" "~",RawInline "latex" $ symReqPageRef r, Str ")."]
        ncs = concs r >- seenConcs            -- newly seen concepts
        cds = [(c,cd) | c<-ncs, cd<-conceptDefs fSpec, cdcpt cd==name c]    -- ... and their definitions
        ds  = map makeDeclaration (mors r)
        nds = [d | d@Sgn{}<-ds >- seenDeclarations]     -- newly seen declarations
        rds = [d | d@Sgn{}<-ds `isc` seenDeclarations]  -- previously seen declarations
        ( dpNext, n', seenCs,  seenDs ) = dpR rs (n+length cds+length nds+1) (ncs++seenConcs) (nds++seenDeclarations)


data Counter = Counter { --getConc :: Int
                    --     getDecl :: Int
                    --   , getRule :: Int
                        getEisnr:: Int
                       }
newCounter :: Counter
newCounter = Counter 1
incEis :: Counter -> Counter
--incConc x = x{getConc = getConc x + 1}
--incDecl x = x{getDecl = getDecl x + 1}
--incRule x = x{getRule = getRule x + 1}
incEis x = x{getEisnr = getEisnr x + 1}

purposes2Blocks :: Options -> [Purpose] -> [Block]
purposes2Blocks flags ps
 = case ps of
    [] -> []
          -- by putting the ref after the first inline of the definition, it aligns nicely with the definition
    _  -> case concatMarkup [expl{amPandoc = insertAfterFirstInline (ref purp) $ amPandoc expl} | purp<-ps, let expl=explMarkup purp] of
           Nothing -> []
           Just p  -> amPandoc p
   where   -- The reference information, if available for this purpose, is put
    ref purp = case fspecFormat flags of
                FLatex | (not.null.explRefId) purp-> [RawInline "latex" ("\\marge{"++latexEscShw (explRefId purp)++"}\n")]
                _                                 -> []
concatMarkup :: [A_Markup] -> Maybe A_Markup
concatMarkup es
 = case eqCl f es of
    []   -> Nothing
    [cl] -> Just A_Markup { amLang   = amLang (head cl)
                          , amFormat = amFormat (head cl)
                          , amPandoc = concatMap amPandoc es
                          }
    cls  -> fatal 136 ("don't call concatMarkup with different languages and formats\n   "++
                      intercalate "\n   " [(show.f.head) cl | cl<-cls])
   where f e = (amLang e, amFormat e)


-- Insert an inline after the first inline in the list of blocks, if possible. 
insertAfterFirstInline :: [Inline] -> [Block] -> [Block]
insertAfterFirstInline inlines (            Plain (inl:inls):pblocks)        =             Plain (inl : (inlines++inls)) : pblocks
insertAfterFirstInline inlines (            Para (inl:inls):pblocks)         =             Para (inl : (inlines++inls)) : pblocks
insertAfterFirstInline inlines (BlockQuote (Para (inl:inls):pblocks):blocks) = BlockQuote (Para (inl : (inlines++inls)) : pblocks):blocks
insertAfterFirstInline inlines blocks                                        = Plain inlines : blocks

isMissing :: Maybe Purpose -> Bool
isMissing mp =
  case mp of 
    Nothing -> True
    Just p  -> (not . explUserdefd) p

lclForLang :: Options -> TimeLocale
lclForLang flags = defaultTimeLocale { months =
         case language flags of
           Dutch   -> [ ("januari","jan"),("februari","feb"),("maart","mrt"),("april","apr")
                      , ("mei","mei"),("juni","jun"),("juli","jul"),("augustus","aug")
                      , ("september","sep"),("oktober","okt"),("november","nov"),("december","dec")]
           English -> [ ("January","Jan"),("February","Feb"),("March","Mar"),("April","Apr")
                      , ("May","May"),("June","Jun"),("July","Jul"),("August","Aug")
                      , ("September","Sep"),("October","Oct"),("November","Nov"),("December","Dec")]
           }

noInlines :: Inlines
noInlines = text ""
noBlocks :: Blocks
noBlocks = singleton Null

inlineIntercalate :: Inlines -> [Inlines] -> Inlines
inlineIntercalate _   [] = noInlines
inlineIntercalate sep [x] = x
inlineIntercalate sep (x:xs) = x <> sep <> inlineIntercalate sep xs



langSwitch :: Options -> [(Lang, a)] -> a
langSwitch flags selections
  = case lookup (language flags) selections of
      Nothing -> fatal 354 $ "language "++show (language flags)++" not found in `langSwitch`."
      Just a  -> a
(===>) :: Lang -> a -> (Lang, a)
(===>) l a = (l, a)

