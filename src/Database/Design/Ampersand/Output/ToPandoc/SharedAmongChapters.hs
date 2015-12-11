{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Design.Ampersand.Output.ToPandoc.SharedAmongChapters
    ( module Text.Pandoc
    , module Text.Pandoc.Builder
    , bulletList -- (is redefined in this module, but belongs in Text.Pandoc.Builder.)
    , math --
    , module Data.Monoid
    , module Database.Design.Ampersand.Basics
    , module Database.Design.Ampersand.FSpec
    , module Database.Design.Ampersand.Misc
    , module Database.Design.Ampersand.Core.AbstractSyntaxTree
    , module Database.Design.Ampersand.ADL1
    , module Database.Design.Ampersand.Output.PandocAux
    , module Database.Design.Ampersand.Graphic.Graphics
    , module Database.Design.Ampersand.Classes
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
    , dpRule'
    , relsInThemes
 --   , Counter(..),newCounter,incEis
    , inlineIntercalate
    , ThemeContent(..), orderingByTheme
    , Numbered(..), RuleCont(..),DeclCont(..),CptCont(..)
    , plainText
    , NLString(..)
    , ENString(..)
    , LocalizedStr
    , localize
    , sortWith)
where
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Core.AbstractSyntaxTree hiding (Meta,sortWith)
import Database.Design.Ampersand.ADL1 hiding (Meta)
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.FSpec
import Text.Pandoc
import Text.Pandoc.Builder hiding (bulletList,math)
import qualified Text.Pandoc.Builder as  BuggyBuilder
import Database.Design.Ampersand.Output.PredLogic        (PredLogicShow(..), showLatex)
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.Output.PandocAux
import Data.List      --       (intercalate,partition)
import Data.Monoid
import Data.Maybe
import Data.Ord
import qualified Data.Time.Format as DTF
import System.FilePath
import GHC.Exts(sortWith)
import Database.Design.Ampersand.Graphic.Graphics
import Database.Design.Ampersand.Classes()

data Chapter = Intro
             | SharedLang
             | Diagnosis
             | ConceptualAnalysis
             | ProcessAnalysis
             | DataAnalysis
             | SoftwareMetrics
             | EcaRules
             | Interfaces
             | FunctionPointAnalysis
             | Glossary
             deriving (Eq, Show)

-- | Define the order of the chapters in the document.
chaptersInDoc :: Options -> [Chapter]
chaptersInDoc opts = [chp | chp<-chapters, chp `notElem` disabled]
 where
   -- temporarily switch off chapters that need too much refactoring, but keep this Haskell code compilable.
    disabled = []
    chapters
     | test opts                  = [SharedLang]
     | diagnosisOnly opts         = [Diagnosis]
     | otherwise                   = [ Intro
                                     , SharedLang
                                     , Diagnosis
                                     , ConceptualAnalysis
                                     , ProcessAnalysis
                                     , DataAnalysis
                                     , SoftwareMetrics
                                     , EcaRules
                                     , Interfaces
                                     ] ++
                                     [ FunctionPointAnalysis | genFPAChap opts ] ++
                                     [ Glossary
                                     ]

-- | This function returns a header of a chapter
chptHeader :: Lang -> Chapter -> Blocks
chptHeader lang chap
 = header 1 (chptTitle lang chap ) <> (para (xrefLabel chap))

chptTitle :: Lang -> Chapter -> Inlines
chptTitle lang cpt =
     (case (cpt,lang) of
        (Intro                 , Dutch  ) -> text "Inleiding"
        (Intro                 , English) -> text "Introduction"
        (SharedLang            , Dutch  ) -> text "Gemeenschappelijke taal"
        (SharedLang            , English) -> text "Shared Language"
        (Diagnosis             , Dutch  ) -> text "Diagnose"
        (Diagnosis             , English) -> text "Diagnosis"
        (ConceptualAnalysis    , Dutch  ) -> text "Conceptuele Analyse"
        (ConceptualAnalysis    , English) -> text "Conceptual Analysis"
        (ProcessAnalysis       , Dutch  ) -> text "Procesanalyse"
        (ProcessAnalysis       , English) -> text "Process Analysis"
        (DataAnalysis          , Dutch  ) -> text "Gegevensstructuur"
        (DataAnalysis          , English) -> text "Data structure"
        (SoftwareMetrics       , Dutch  ) -> text "Functiepunt Analyse"
        (SoftwareMetrics       , English) -> text "Function Point Analysis"
        (EcaRules              , Dutch  ) -> text "ECA regels"
        (EcaRules              , English) -> text "ECA rules (Flash points)"
        (Interfaces            , Dutch  ) -> text "Koppelvlakken"
        (Interfaces            , English) -> text "Interfaces"
        (FunctionPointAnalysis , Dutch  ) -> text "Functiepuntanalyse"
        (FunctionPointAnalysis , English) -> text "Function point analysis"
        (Glossary              , Dutch  ) -> text "Begrippen"
        (Glossary              , English) -> text "Glossary"
     )

class Xreferencable a where
  xLabel :: a  -> String
  xRefReference :: Options -> a -> Inlines
  xRefReference opts a
    | canXRefer opts = rawInline "latex" ("\\ref{"++xLabel a++"}")
    | otherwise       = mempty -- "fatal 89 xreferencing is not supported!"
  xrefLabel :: a -> Inlines
  xrefLabel a = rawInline "latex" ("\\label{"++xLabel a++"}")

canXRefer :: Options -> Bool
canXRefer opts = fspecFormat opts `elem` [FLatex]

instance Xreferencable Chapter where
  xLabel a = "chapter" ++ escapeNonAlphaNum (show a)

instance Xreferencable Picture where
  xLabel a = "figure" ++ escapeNonAlphaNum (caption a)

showImage :: Options -> Picture -> Inlines
showImage opts pict =
      case fspecFormat opts of
         FLatex  -> rawInline "latex" ("\\begin{figure}[htb]\n\\begin{center}\n\\scalebox{"++scale pict++"}["++scale pict++"]{")
         _       -> mempty
   <> image fileOnly (xLabel pict) (text $ "Here, the image `"++caption pict++"` should be visible" )
   <> case fspecFormat opts of
         FLatex  -> rawInline "latex" "}\n"
                  <>rawInline "latex" ("\\caption{"++latexEscShw (caption pict)++"}\n")
         _       -> mempty
   <> (xrefLabel pict)
   <> case fspecFormat opts of
         FLatex  -> rawInline "latex" "\n\\end{center}\n\\end{figure}"
         _       -> mempty
  where
    fileOnly = ((case fspecFormat opts of
         FLatex  -> dropExtension -- let pdflatex figure out the optimal extension
         _ -> id
      ) . takeFileName . imagePath opts) pict

-- | This function orders the content to print by theme. It returns a list of
--   tripples by theme. The last tripple might not have a theme, but will contain everything
--   that isn't handled in a specific theme.

data ThemeContent =
       Thm { themeNr ::      Int
           , patOfTheme ::   Maybe Pattern -- A theme is about either a pattern or about everything outside patterns
           , rulesOfTheme :: [Numbered RuleCont] -- The (numbered) rules of that theme
           , dclsOfTheme ::  [Numbered DeclCont] -- The (numbered) relations that are used in a rule of this theme, but not in any rule of a previous theme.
           , cptsOfTheme ::  [Numbered CptCont]   -- The (numbered) concepts that are used in a rule of this theme, but not in any rule of a previous theme.
           }
data Numbered t =
 Nr { theNr ::   Int
    , theLoad :: t
    }
instance Named t => Named (Numbered t) where
 name = name . theLoad
data RuleCont = CRul { cRul ::  Rule
                     , cRulPurps :: [Purpose]
                     , cRulMeaning :: Maybe A_Markup
                     }
data DeclCont = CDcl { cDcl ::  Declaration
                     , cDclPurps :: [Purpose]
                     , cDclMeaning :: Maybe A_Markup
                     , cDclPairs :: [AAtomPair]
                     }
data CptCont  = CCpt { cCpt ::  A_Concept
                     , cCptDefs :: [ConceptDef]
                     , cCptPurps :: [Purpose]
                     }
instance Named RuleCont where
  name = name . cRul
instance Named DeclCont where
  name = name . cDcl
instance Named CptCont where
  name = name . cCpt
data Counters
  = Counter { pNr :: Int --Theme number
            , definitionNr :: Int --For Concepts
            , agreementNr ::  Int --For declarations andrules
            }
orderingByTheme :: FSpec -> [ThemeContent]
orderingByTheme fSpec
 = f ( Counter 1 1 1 --the initial numbers of the countes
     , (sortWith origin . filter rulMustBeShown . fallRules)       fSpec
     , (sortWith origin . filter relMustBeShown . relsMentionedIn) fSpec
     , (sortBy conceptOrder . filter cptMustBeShown . concs)     fSpec
     ) $
     [Just pat | pat <- vpatterns fSpec -- The patterns that should be taken into account for this ordering
        ,    null (themes fSpec)        -- all patterns if no specific themes are requested
          || name pat  `elem` themes fSpec  -- otherwise the requested ones only
        ]++[Nothing] --Make sure the last is Nothing, to take all res stuff.
 where
  conceptOrder :: A_Concept -> A_Concept -> Ordering
  conceptOrder a b =
  -- The sorting of Concepts is done by the origin of its first definition if there is one.
  -- Concepts without definition are placed last, and sorted by name.
   case (originOfFirstCDef a, originOfFirstCDef b) of
     (Just origA, Just origB) -> compare origA origB
     (Just _    , Nothing   ) -> LT
     (Nothing   , Just _    ) -> GT
     (Nothing   , Nothing   ) -> comparing name a b
  originOfFirstCDef :: A_Concept -> Maybe Origin
  originOfFirstCDef cpt
    = case sortWith origin $ concDefs fSpec cpt of
        [] -> Nothing
        cd :_ -> Just (origin cd)
  rulMustBeShown :: Rule -> Bool
  rulMustBeShown r = (hasMeaning r || hasPurpose r)
  relMustBeShown :: Declaration -> Bool
  relMustBeShown d 
    | isIdent d || name d == "V" = False  --Identity relation has no meaning defined
    | otherwise = (hasMeaning d || hasPurpose d) && (isUserDefined d || forNonUserDefdRule d)  
  isUserDefined d = case d of
                       Sgn{} -> decusr d
                       _     -> False
  hasPurpose :: Motivated a => a -> Bool
  hasPurpose = not . null . purposesDefinedIn fSpec (fsLang fSpec)
  hasMeaning :: Meaning a => a -> Bool
  hasMeaning = isJust . meaning (fsLang fSpec)
  forNonUserDefdRule :: Declaration -> Bool
  forNonUserDefdRule d = not . null
      . filter isPropRuleForDcl . fallRules $ fSpec 
    where
      isPropRuleForDcl :: Rule -> Bool
      isPropRuleForDcl rul =
        case rrdcl rul of
           Nothing -> False
           Just (_,x) -> x == d
  cptMustBeShown = not . null . concDefs fSpec
  f ::
   (Counters, [Rule], [Declaration], [A_Concept]) -> [Maybe Pattern] -> [ThemeContent]
  f stuff ts
   = case ts of
       t:ts' -> let ( thm, rest) = partitionByTheme t stuff
                in thm : f rest ts'
       []    -> case stuff of
                  (_,[],[],[]) -> []
                  _ -> fatal 247 "No stuff should be left over."

  rul2rulCont :: Rule -> RuleCont
  rul2rulCont rul
    = CRul { cRul      = rul
           , cRulPurps = fromMaybe [] $ purposeOf fSpec (fsLang fSpec) rul
           , cRulMeaning = meaning (fsLang fSpec) rul
           }
  dcl2dclCont :: Declaration -> DeclCont
  dcl2dclCont dcl
    = CDcl { cDcl      = dcl
           , cDclPurps = fromMaybe [] $ purposeOf fSpec (fsLang fSpec) dcl
           , cDclMeaning = meaning (fsLang fSpec) dcl
           , cDclPairs = pairsInExpr fSpec (EDcD dcl)
           }

  cpt2cptCont :: A_Concept -> CptCont
  cpt2cptCont cpt
    = CCpt { cCpt      = cpt
           , cCptDefs  = sortWith origin $ concDefs fSpec cpt
           , cCptPurps = fromMaybe [] $ purposeOf fSpec (fsLang fSpec) cpt
           }


  setNumbers :: Int           -- ^ the initial number
             -> (t -> a)      -- ^ the constructor function
             -> [t]           -- ^ a list of things that are numberd
             -> [Numbered a]
  setNumbers i construct items =
    case items of
      []     -> []
      (x:xs) ->  Nr { theNr   = i
                    , theLoad = construct x
                    }:setNumbers (i+1) construct xs
  -- | This function takes care of partitioning each of the
  --   lists in a pair of lists of elements which do and do not belong
  --   to the theme, respectively
  partitionByTheme :: Maybe Pattern
                   -> ( Counters, [Rule], [Declaration], [A_Concept])
                   -> ( ThemeContent , ( Counters ,[Rule], [Declaration], [A_Concept])
                      )
  partitionByTheme t (cnt, ruls, rels, cpts)
      = ( Thm { themeNr      = pNr cnt
              , patOfTheme   = t
              , rulesOfTheme = setNumbers (agreementNr cnt + length themeDcls ) rul2rulCont thmRuls
              , dclsOfTheme  = setNumbers (agreementNr cnt) dcl2dclCont themeDcls
              , cptsOfTheme  = setNumbers (definitionNr cnt) cpt2cptCont themeCpts
              }
        , (Counter {pNr = pNr cnt +1
                   ,definitionNr = definitionNr cnt + length themeCpts
                   ,agreementNr = agreementNr cnt + length themeDcls + length thmRuls
                   }
          , restRuls, restDcls, restCpts)
        )
     where
       (thmRuls,restRuls) = partition (inThisTheme ptrls) ruls
       (themeDcls,restDcls) = partition (inThisTheme relsInTheme) rels
          where relsInTheme p = relsDefdIn p `uni` relsMentionedIn p
       (themeCpts,restCpts) = partition (inThisTheme concs) cpts
       inThisTheme :: Eq a => (Pattern -> [a]) -> a -> Bool
       inThisTheme allElemsOf x
         = case t of
             Nothing -> True
             Just pat -> x `elem` allElemsOf pat
--GMI: What's the meaning of the Int? HJO: This has to do with the numbering of rules
dpRule' :: FSpec -> [Rule] -> Int -> [A_Concept] -> [Declaration]
          -> ([(Inlines, [Blocks])], Int, [A_Concept], [Declaration])
dpRule' fSpec = dpR
 where
   l lstr = text $ localize (fsLang fSpec) lstr
   dpR [] n seenConcs seenDeclarations = ([], n, seenConcs, seenDeclarations)
   dpR (r:rs) n seenConcs seenDeclarations
     = ( ( l (NL "Regel: ",EN "Rule: ") <> (text.latexEscShw.name) r
         , [theBlocks]
          ): dpNext
       , n'
       , seenCs
       , seenDs
       )
       where
        theBlocks :: Blocks
        theBlocks =
         (  (purposes2Blocks (getOpts fSpec) (purposesDefinedIn fSpec (fsLang fSpec) r)) -- Als eerste de uitleg van de betreffende regel..
         <> (purposes2Blocks (getOpts fSpec) [p | d<-nds, p<-purposesDefinedIn fSpec (fsLang fSpec) d])  -- Dan de uitleg van de betreffende relaties
         <> case (nds, fsLang fSpec) of
             ([] ,_)       -> mempty
             ([d],Dutch)   -> plain ("Om dit te formaliseren is een " <> (if isFunction d then "functie"  else "relatie" ) <> str (name d) <> " nodig ("         <> xRefTo (XRefNaturalLanguageDeclaration d) <> "):")
             ([d],English) -> plain ("In order to formalize this, a " <> (if isFunction d then "function" else "relation") <> str (name d) <> " is introduced (" <> xRefTo (XRefNaturalLanguageDeclaration d) <> "):")
             (_  ,Dutch)   -> plain ("Om te komen tot de formalisatie van " <> xRefTo (XRefNaturalLanguageRule r)
                                    <>  " (" <> (singleQuoted.str.name) r  <> ") "
                                    <> str (" zijn de volgende "++count Dutch (length nds) "in deze paragraaf geformaliseerde relatie"++" nodig."))
             (_  ,English) -> plain ("To arrive at the formalization of "   <> xRefTo (XRefNaturalLanguageRule r) <> str (", the following "++count English (length nds) "relation"++" are introduced."))
         <> (fromList $
                  pandocEqnArray
                      [ [ "("++xRefToLatexRefString (XRefConceptualAnalysisDeclaration d) ++ ")\\;\\;"
                        ,  texOnly_Id(name d)
                        , ":"
                        , texOnly_Id(name (source d))++(if isFunction d then texOnly_fun else texOnly_rel)++texOnly_Id(name(target d))
                        ] |d<-nds])
         <> (case nds of
              [] -> case rds of
                       []   -> mempty
                       [rd] -> plain (  l (NL "Relatie ", EN "We use relations ")
                                     <> xRefTo (XRefConceptualAnalysisDeclaration rd) <> "(" <> (emph.str.name) rd <> ")"
                                     <> l (NL " wordt gebruikt.", EN ".")
                                     )
                       _    -> plain (  (case fsLang fSpec of
                                          Dutch   ->  "We gebruiken de relaties "
                                                     <> commaNLPandoc'  "en"  [xRefTo (XRefConceptualAnalysisDeclaration rd) <> " (" <> (emph.str.name) rd <> ")" |rd<-rds]
                                          English ->   "We use relations "
                                                     <> commaEngPandoc' "and" [xRefTo (XRefConceptualAnalysisDeclaration rd) <> " (" <> (emph.str.name) rd <> ")" |rd<-rds]
                                        )
                                     )
              _  -> if null rds then mempty
                    else ( plain ( ( case rds of
                                      []   -> mempty
                                      [rd] ->   l (NL "Daarnaast gebruiken we relatie ", EN "Beside that, we use relation ")
                                              <> xRefTo (XRefConceptualAnalysisDeclaration rd) <> "(" <> (str.name) rd <> ")"
                                      _ -> (case fsLang fSpec of
                                          Dutch   ->   "Ook gebruiken we relaties "
                                                     <> commaNLPandoc'  "en"  [xRefTo (XRefConceptualAnalysisDeclaration rd) <> " (" <> (emph.str.name) rd <> ")" |rd<-rds]
                                          English ->   "We also use relations "
                                                     <> commaEngPandoc' "and" [xRefTo (XRefConceptualAnalysisDeclaration rd) <> " (" <> (emph.str.name) rd <> ")" |rd<-rds]
                                           )
                                   )
                                 <> l (NL " om ", EN " to formalize ")
                                 <> xRefTo (XRefNaturalLanguageRule r)
                                 <> l (NL " te formaliseren: ", EN ": ")
                                 )
                         )
           )
         <> plain (if isSignal r
                   then l ( NL "Activiteiten, die door deze regel zijn gedefinieerd, zijn afgerond zodra: "
                          , EN "Activities that are defined by this rule are finished when: ")
                   else l (NL "De regel luidt: ", EN "This means: ")
                  )
         <> (if showPredExpr (getOpts fSpec)
             then pandocEqnArrayWithLabel (XRefConceptualAnalysisRule r) ((showLatex.toPredLogic) r)
             else pandocEquationWithLabel (XRefConceptualAnalysisRule r) (showMath r)
            )
         <> (if length nds<=1
             then mempty
             else plain (  l (NL "Dit komt overeen met ", EN "This corresponds to ")
                        <> xRefTo (XRefNaturalLanguageRule r)
                        <> " (" <> (singleQuoted.str.name) r  <> ")."

                        )
            )
         )
        ncs = concs r >- seenConcs            -- newly seen concepts
        cds = [(c,cd) | c<-ncs, cd<-cDefsInScope fSpec, cdcpt cd==name c]    -- ... and their definitions
        ds  = relsUsedIn r
        nds = [d | d@Sgn{}<-ds >- seenDeclarations]     -- newly seen relations
        rds = [d | d@Sgn{}<-ds `isc` seenDeclarations]  -- previously seen relations
        ( dpNext, n', seenCs,  seenDs ) = dpR rs (n+length cds+length nds+1) (ncs++seenConcs) (nds++seenDeclarations)

relsInThemes :: FSpec -> [Declaration]
relsInThemes fSpec
        -- a relation is considered relevant iff it is declared or mentioned in one of the relevant themes.
 = [d | d<-vrels fSpec
   , decusr d
   , (  decpat d `elem` themes fSpec
         || d `elem` relsMentionedIn [p | p<-  vpatterns fSpec   , name p `elem` themes fSpec]
     )
   ]

purposes2Blocks :: Options -> [Purpose] -> Blocks
purposes2Blocks opts ps
 = fromList $
     case ps of
      [] -> []
            -- by putting the ref after the first inline of the definition, it aligns nicely with the definition
      _  -> case concatMarkup [expl{amPandoc = insertAfterFirstInline (ref purp) $ amPandoc expl} | purp<-ps, let expl=explMarkup purp] of
             Nothing -> []
             Just p  -> amPandoc p
       where   -- The reference information, if available for this purpose, is put
        ref :: Purpose -> [Inline]
        ref purp = case fspecFormat opts of
                    FLatex | (not.null.explRefIds) purp-> [RawInline (Text.Pandoc.Builder.Format "latex")
                                                             ("\\marge{"++intercalate "; " (map latexEscShw (explRefIds purp))++"}\n")]
                    _                                  -> []
concatMarkup :: [A_Markup] -> Maybe A_Markup
concatMarkup es
 = case eqCl amLang es of
    []   -> Nothing
    [cl] -> Just A_Markup { amLang   = amLang (head cl)
                          , amPandoc = concatMap amPandoc es
                          }
    cls  -> fatal 136 ("don't call concatMarkup with different languages and formats\n   "++
                      intercalate "\n   " [(show.amLang.head) cl | cl<-cls])

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

lclForLang :: Lang -> DTF.TimeLocale
lclForLang lang = DTF.defaultTimeLocale { DTF.months =
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

plainText :: String -> Blocks
plainText = plain . text

-- Temporary fixes of Pandoc builder. ---
bulletList :: [Blocks] -> Blocks
bulletList [] = mempty
bulletList xs = BuggyBuilder.bulletList xs

math :: String -> Inlines
math s = BuggyBuilder.math ("{"++s++"}")


-- Utility types and functions for handling multiple-language strings

-- If you declare a local function:   l lstr = localize (fsLang fSpec) lstr
-- you can use:  l (NL "Nederlandse tekst", EN "English text")
-- to specify strings in multiple languages.

newtype NLString = NL String
newtype ENString = EN String

type LocalizedStr = (NLString, ENString)

localize :: Lang -> LocalizedStr -> String
localize Dutch   (NL s, _) = s
localize English (_, EN s) = s
