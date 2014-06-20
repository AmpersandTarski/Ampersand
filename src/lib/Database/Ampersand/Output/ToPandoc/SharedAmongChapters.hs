{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
    ( module Text.Pandoc
    , module Text.Pandoc.Builder
    , bulletList -- (is redefined in this module, but belongs in Text.Pandoc.Builder.)
    , math -- 
    , module Data.Monoid
    , module DatabaseDesign.Ampersand.Basics  
    , module DatabaseDesign.Ampersand.Fspec
    , module DatabaseDesign.Ampersand.Misc
    , module DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
    , Chapter(..)
    , chaptersInDoc
    , chptHeader
    , chptTitle
    , Xreferencable(..)
    , showImage
    , canXRefer
    , Purpose(..)
    , purposes2Blocks
    , isMissing
    , lclForLang
    , dpRule
    , relsInThemes
    , Counter(..),newCounter,incEis
    , inlineIntercalate
    , orderingByTheme
    )
where
import DatabaseDesign.Ampersand.Basics  
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree hiding (Meta)
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Fspec
import Text.Pandoc
import Text.Pandoc.Builder hiding (bulletList,math)
import qualified Text.Pandoc.Builder as  BuggyBuilder
import DatabaseDesign.Ampersand.Output.PredLogic        (PredLogicShow(..), showLatex)
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Output.PandocAux
import Data.List             (intercalate,partition)
import Data.Monoid
import System.Locale

fatal :: Int -> String -> a
fatal = fatalMsg "Output.ToPandoc.SharedAmongChapters"

data Chapter = Intro 
             | SharedLang
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
chaptersInDoc flags = [chp | chp<-chapters, chp `notElem` disabled]
 where
   -- temporarily switch off chapters that need too much refactoring, but keep this Haskell code compilable.
    disabled = [Interfaces]
    chapters
     | test flags                  = [SharedLang]
     | diagnosisOnly flags         = [Diagnosis]
     | theme flags == StudentTheme = [Intro,SharedLang,Diagnosis,ConceptualAnalysis,DataAnalysis]
     | otherwise                   = [ Intro 
                                     , SharedLang
                                     , Diagnosis 
                                     , ConceptualAnalysis
                                     , ProcessAnalysis
                                     , DataAnalysis
                                     , SoftwareMetrics
                                     , EcaRules
                                     , Interfaces
                                     , Glossary
                                     ]

-- | This function returns a header of a chapter
chptHeader :: Lang -> Chapter -> Blocks
chptHeader lang chap 
 = header 1 (chptTitle lang chap ) <> singleton (Para [xrefLabel chap])
 
chptTitle :: Lang -> Chapter -> Inlines
chptTitle lang cpt =
     (case (cpt,lang) of
        (Intro             , Dutch  ) -> text "Inleiding"
        (Intro             , English) -> text "Introduction"
        (SharedLang        , Dutch  ) -> text "Gemeenschappelijke taal"
        (SharedLang        , English) -> text "Shared Language"
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
        (Glossary          , Dutch  ) -> text "Begrippen"
        (Glossary          , English) -> text "Glossary"
     )

class Xreferencable a where
  xLabel :: a  -> String
  xrefReference :: a  -> Inline   --Depreciated! TODO: use xRefReference instead
  xrefReference a = RawInline (Text.Pandoc.Builder.Format "latex") ("\\ref{"++xLabel a++"}")
  xRefReference :: Options -> a -> Inlines
  xRefReference flags a 
    | canXRefer flags = rawInline "latex" ("\\ref{"++xLabel a++"}")
    | otherwise       = mempty -- "fatal 89 xreferencing is not supported!"
  xrefLabel :: a -> Inline
  xrefLabel a = RawInline (Text.Pandoc.Builder.Format "latex") ("\\label{"++xLabel a++"}")

canXRefer :: Options -> Bool
canXRefer opts = fspecFormat opts `elem` [FLatex] 

instance Xreferencable Chapter where
  xLabel a = "chapter" ++ escapeNonAlphaNum (show a)
  
instance Xreferencable Picture where
  xLabel a = "figure" ++ escapeNonAlphaNum (caption a)

--Image [Inline] Target
--      alt.text (URL,title)
showImage :: Options -> Picture -> Inlines
showImage flags pict = 
      case fspecFormat flags of
         FLatex  -> rawInline "latex" ("\\begin{figure}[htb]\n\\begin{center}\n\\scalebox{"++scale pict++"}["++scale pict++"]{")
         _       -> mempty
   <> image (imagePath flags pict) (xLabel pict) (text $ "Here, "++caption pict++" should have been visible" )
   <> case fspecFormat flags of
         FLatex  -> rawInline "latex" "}\n"
                  <>rawInline "latex" ("\\caption{"++latexEscShw (caption pict)++"}\n") 
         _       -> mempty
   <> singleton (xrefLabel pict)
   <> case fspecFormat flags of
         FLatex  -> rawInline "latex" "\n\\end{center}\n\\end{figure}"
         _       -> mempty

-- | This function orders the content to print by theme. It returns a list of 
--   tripples by theme. The last tripple might not have a theme, but will contain everything
--   that isn't handled in a specific theme.
orderingByTheme :: Fspc -> [( Maybe Theme   -- A theme is about either a pattern or a process. 
                            , [Rule]        -- The rules of that theme
                            , [Declaration] -- The relations that are used in a rule of this theme, but not in any rule of a previous theme.
                            , [A_Concept]   -- The concepts that are used in a rule of this theme, but not in any rule of a previous theme.
                            )
                           ]
orderingByTheme fSpec 
 = f (allRules fSpec) (filter isUserDefined (relsMentionedIn fSpec)) (allConcepts fSpec) tms
 where
  isUserDefined d = case d of
                       Sgn{} -> decusr d
                       _     -> False  
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
                   -> [Declaration]
                   -> [A_Concept]
                   -> ( ([Rule],[Rule])
                      , ([Declaration],[Declaration])
                      , ([A_Concept],[A_Concept])
                      )
  partitionByTheme thme ruls rels cpts
      = ((rulsOfTheme,rulsNotOfTheme), (relsOfTheme,relsNotOfTheme), (cptsOfTheme,cptsNotOfTheme))
     where 
       (rulsOfTheme,rulsNotOfTheme) = partition isRulOfTheme ruls
       isRulOfTheme r = r `elem` (case thme of
                                    PatternTheme pat -> ptrls pat
                                    ProcessTheme prc -> prcRules prc
                                 )
       (relsOfTheme,relsNotOfTheme) = partition isRelOfTheme rels
       isRelOfTheme r = r `elem` (concatMap relsDefdIn rulsOfTheme++concatMap relsUsedIn rulsOfTheme)
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
         , [ let purps = purposesDefinedIn fSpec (fsLang fSpec) r in            -- Als eerste de uitleg van de betreffende regel..
             toList (purposes2Blocks flags purps) ++
             toList (purposes2Blocks flags [p | d<-nds, p<-purposesDefinedIn fSpec (fsLang fSpec) d]) ++  -- Dan de uitleg van de betreffende relaties
             [ Plain text1 | not (null nds)] ++
             pandocEqnArray [ ( texOnly_Id(name d)
                              , ":"
                              , texOnly_Id(name (source d))++(if isFunction d then texOnly_fun else texOnly_rel)++texOnly_Id(name(target d))++symDefLabel d
                              )
                            |d<-nds] ++
             [ Plain text2 | not (null rds)] ++
             [ Plain text3 ] ++
             (if showPredExpr flags
              then pandocEqnArrayOnelabel (symDefLabel r) ((showLatex.toPredLogic) r)
              else pandocEquation (showMath r++symDefLabel r)
             )++
             (if length nds>1 then text5 else [])
           ] 
         ): dpNext
       , n'
       , seenCs 
       , seenDs
       )
       where
        text1
         = case (length nds,fsLang fSpec) of
             (1,Dutch)   -> let d = head nds in
                            [Str ("Om dit te formaliseren is een "++(if isFunction d then "functie" else "relatie")++" "),Str (name d),Str " nodig (",RawInline (Text.Pandoc.Builder.Format "latex") $ symDefRef d,Str "):"]
             (1,English) -> let d = head nds in
                            [Str "In order to formalize this, a ", Str (if isFunction d then "function" else "relation"), Space, Str (name d),Str " is introduced (",RawInline (Text.Pandoc.Builder.Format "latex") $ symDefRef d,Str "):"]
             (l,Dutch)   -> [Str "Om te komen tot de formalisatie in vergelijking",RawInline (Text.Pandoc.Builder.Format "latex") "~",RawInline (Text.Pandoc.Builder.Format "latex") $ symDefRef r,Str (" zijn de volgende "++count Dutch l "relatie"++" nodig.")]
             (l,English) -> [Str "To arrive at the formalization in equation",RawInline (Text.Pandoc.Builder.Format "latex") "~",RawInline (Text.Pandoc.Builder.Format "latex") $ symDefRef r,Str (", the following "++count English l "relation"++" are introduced.")]
        text2
         = (case (length nds,length rds,fsLang fSpec) of
             (0,1,Dutch)   -> [Str "Definitie ",RawInline (Text.Pandoc.Builder.Format "latex") $ symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ") wordt gebruikt"]
             (0,1,English) -> [Str "We use definition ",RawInline (Text.Pandoc.Builder.Format "latex") $ symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ")"]
             (0,_,Dutch)   -> Str "We gebruiken definities ":commaNLPandoc (Str "en") [RawInline (Text.Pandoc.Builder.Format "latex") $ symDefRef d++" ("++texOnly_Id (name d)++")" |d<-rds]
             (0,_,English) -> Str "We use definitions ":commaEngPandoc (Str "and") [RawInline (Text.Pandoc.Builder.Format "latex") $ symDefRef d++" ("++texOnly_Id (name d)++")" |d<-rds]
             (_,1,Dutch)   -> [Str "Daarnaast gebruiken we definitie ",RawInline (Text.Pandoc.Builder.Format "latex") $ symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ")"]
             (_,1,English) -> [Str "Beside that, we use definition ",RawInline (Text.Pandoc.Builder.Format "latex") $ symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ")"]
             (_,_,Dutch)   -> Str "Ook gebruiken we definities ":commaNLPandoc (Str "en") [RawInline (Text.Pandoc.Builder.Format "latex") $ symDefRef d++" ("++texOnly_Id (name d)++")" |d<-rds]
             (_,_,English) -> Str "We also use definitions ":commaEngPandoc (Str "and") [RawInline (Text.Pandoc.Builder.Format "latex") $ symDefRef d++" ("++texOnly_Id (name d)++")" |d<-rds]
           )++
           (case (length nds,fsLang fSpec) of
             (1,Dutch)   -> [Str " om eis",RawInline (Text.Pandoc.Builder.Format "latex") "~",RawInline (Text.Pandoc.Builder.Format "latex") $ symReqRef r, Str " (pg.",RawInline (Text.Pandoc.Builder.Format "latex") "~",RawInline (Text.Pandoc.Builder.Format "latex") $ symReqPageRef r, Str ") te formaliseren:"]
             (1,English) -> [Str " to formalize requirement",RawInline (Text.Pandoc.Builder.Format "latex") "~",RawInline (Text.Pandoc.Builder.Format "latex") $ symReqRef r, Str " (page",RawInline (Text.Pandoc.Builder.Format "latex") "~",RawInline (Text.Pandoc.Builder.Format "latex") $ symReqPageRef r, Str "):"]
             _           -> [Str ". "]
           )
        text3
         = case (fsLang fSpec,isSignal r) of
            (Dutch  ,False) -> [Str "De regel luidt: "]
            (English,False) -> [Str "This means: "]
            (Dutch  ,True)  -> [Str "Activiteiten, die door deze regel zijn gedefinieerd, zijn afgerond zodra: "]
            (English,True)  -> [Str "Activities that are defined by this rule are finished when: "]
--        text4
--         = case fsLang fSpec of
--                 Dutch   -> [Str " Deze activiteiten worden opgestart door:"]
--                 English -> [Str " These activities are signalled by:"]
        text5
         = case (fsLang fSpec,isSignal r) of
             (Dutch  ,False) -> [ Plain [ Str "Dit komt overeen met de afspraak op pg.",RawInline (Text.Pandoc.Builder.Format "latex") "~",RawInline (Text.Pandoc.Builder.Format "latex") $ symReqPageRef r, Str ":"]]++meaning2Blocks (fsLang fSpec) r
             (English,False) -> [ Plain [ Str "This corresponds to the requirement on page",RawInline (Text.Pandoc.Builder.Format "latex") "~",RawInline (Text.Pandoc.Builder.Format "latex") $ symReqPageRef r, Str ":"]]++meaning2Blocks (fsLang fSpec) r
             (Dutch  ,True)  -> [ Plain [ Str "Dit komt overeen met "
                                        , Quoted  SingleQuote [Str (name r)]
                                        , Str " (",RawInline (Text.Pandoc.Builder.Format "latex") $ symReqRef r, Str " op pg.",RawInline (Text.Pandoc.Builder.Format "latex") "~",RawInline (Text.Pandoc.Builder.Format "latex") $ symReqPageRef r, Str ")."]]
             (English,True)  -> [ Plain [ Str "This corresponds to "
                                        , Quoted  SingleQuote [Str (name r)]
                                        , Str " (",RawInline (Text.Pandoc.Builder.Format "latex") $ symReqRef r, Str " op pg.",RawInline (Text.Pandoc.Builder.Format "latex") "~",RawInline (Text.Pandoc.Builder.Format "latex") $ symReqPageRef r, Str ")."]]
        ncs = concs r >- seenConcs            -- newly seen concepts
        cds = [(c,cd) | c<-ncs, cd<-cDefsInScope fSpec, cdcpt cd==name c]    -- ... and their definitions
        ds  = relsUsedIn r
        nds = [d | d@Sgn{}<-ds >- seenDeclarations]     -- newly seen relations
        rds = [d | d@Sgn{}<-ds `isc` seenDeclarations]  -- previously seen relations
        ( dpNext, n', seenCs,  seenDs ) = dpR rs (n+length cds+length nds+1) (ncs++seenConcs) (nds++seenDeclarations)

relsInThemes :: Fspc -> [Declaration]
relsInThemes fSpec
        -- a relation is considered relevant iff it is declared or mentioned in one of the relevant themes.
 = [d | d<-relsDefdIn fSpec
   , decusr d
   , (  decpat d `elem` themes fSpec  
         || d `elem` relsMentionedIn [p | p<-            patterns fSpec   , name p `elem` themes fSpec]
         || d `elem` relsMentionedIn [p | p<-map fpProc (vprocesses fSpec), name p `elem` themes fSpec]
     )
   ]

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

purposes2Blocks :: Options -> [Purpose] -> Blocks
purposes2Blocks flags ps
 = fromList $
     case ps of
      [] -> []
            -- by putting the ref after the first inline of the definition, it aligns nicely with the definition
      _  -> case concatMarkup [expl{amPandoc = insertAfterFirstInline (ref purp) $ amPandoc expl} | purp<-ps, let expl=explMarkup purp] of
             Nothing -> []
             Just p  -> amPandoc p
       where   -- The reference information, if available for this purpose, is put
        ref purp = case fspecFormat flags of
                    FLatex | (not.null.explRefIds) purp-> [RawInline (Text.Pandoc.Builder.Format "latex") 
                                                             ("\\marge{"++intercalate "; " (map latexEscShw (explRefIds purp))++"}\n")]
                    _                                  -> []
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

lclForLang :: Lang -> TimeLocale
lclForLang lang = defaultTimeLocale { months =
         case lang of
           Dutch   -> [ ("januari","jan"),("februari","feb"),("maart","mrt"),("april","apr")
                      , ("mei","mei"),("juni","jun"),("juli","jul"),("augustus","aug")
                      , ("september","sep"),("oktober","okt"),("november","nov"),("december","dec")]
           English -> [ ("January","Jan"),("February","Feb"),("March","Mar"),("April","Apr")
                      , ("May","May"),("June","Jun"),("July","Jul"),("August","Aug")
                      , ("September","Sep"),("October","Oct"),("November","Nov"),("December","Dec")]
           }


inlineIntercalate :: Inlines -> [Inlines] -> Inlines
inlineIntercalate _  [] = mempty
inlineIntercalate _ [x] = x
inlineIntercalate sep (x:xs) = x <> sep <> inlineIntercalate sep xs

-- Temporary fixes of Pandoc builder. ---  
bulletList :: [Blocks] -> Blocks
bulletList [] = mempty
bulletList xs = BuggyBuilder.bulletList xs

math :: String -> Inlines
math s = BuggyBuilder.math ("{"++s++"}")
