{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.ToPandoc.SharedAmongChapters
    ( module Text.Pandoc.Builder
    , bulletList -- (is redefined in this module, but belongs in Text.Pandoc.Builder.)
    , math --
    , module Text.Pandoc
    , module Data.Monoid
    , module Ampersand.ADL1
    , module Ampersand.Basics
    , module Ampersand.Core.AbstractSyntaxTree
    , module Ampersand.Core.ShowAStruct
    , module Ampersand.FSpec
    , module Ampersand.Graphic.Graphics
    , module Ampersand.Misc
    , module Ampersand.Output.PandocAux
    , module Ampersand.Classes
    , Chapter(..)
    , chaptersInDoc
    , Xreferenceble(..)
    , XRefSection(..)
    , pandocEqnArray
    , pandocEquationWithLabel
    , Purpose(..)
    , Role
    , purposes2Blocks
    , violation2Inlines
    , isMissing
    , lclForLang
    , dpRule'
    , inlineIntercalate
    , ThemeContent(..), orderingByTheme
    , Numbered(..), RuleCont(..),DeclCont(..),CptCont(..)
    , plainText
    , sortWith)
where
import           Ampersand.ADL1 hiding (Meta)
import           Ampersand.Basics
import           Ampersand.Classes
import           Ampersand.Core.AbstractSyntaxTree hiding (Meta)
import           Ampersand.Core.ParseTree(Role)
import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec
import           Ampersand.Graphic.Graphics
import           Ampersand.Misc
import           Ampersand.Output.PandocAux
import           Data.List
import           Data.Maybe
import           Data.Monoid 
import           Data.Ord
import           Data.Typeable
import qualified Data.Time.Format as DTF
import           GHC.Exts(sortWith)
import           System.FilePath  -- (combine,addExtension,replaceExtension)
import           Text.Pandoc hiding (trace)
import           Text.Pandoc.Builder hiding (bulletList,math)
import qualified Text.Pandoc.Builder as  BuggyBuilder

-- | Define the order of the chapters in the document.
chaptersInDoc :: Options -> [Chapter]
chaptersInDoc opts 
     | test opts          = [ SharedLang]
     | diagnosisOnly opts = [ Diagnosis]
     | otherwise          = [ Intro
                            , SharedLang
                            , Diagnosis
                            , ConceptualAnalysis
                            , DataAnalysis
                            , Glossary
                            ]

data XRefSection
             = XRefSharedLangRelation Relation
             | XRefDataAnalysisRule Rule
             | XRefSharedLangRule Rule
             | XRefProcessAnalysis Pattern
             | XRefConceptualAnalysisPattern Pattern
             | XRefConceptualAnalysisRelation Relation
             | XRefConceptualAnalysisRule Rule
             | XRefConceptualAnalysisExpression Rule
             | XRefInterfacesInterface Interface
             | XRefNaturalLanguageTheme (Maybe Pattern)

xRefRawLabel :: XRefSection -> String
xRefRawLabel x'
 = show (chapterOfSection x)++typeOfSection x++":"++escapeNonAlphaNum (nameOfThing x)
  where x = refStuff x'
instance Xreferenceble XRefSection where
  xLabel = xRefRawLabel
  xRef a = citeGen (xrefPrefix (refStuff a)) a
  xDefBlck fSpec a = either id (fatal ("You should use xDefInln for:\n  "++show (refStuff a))) (xDef fSpec a)
  xDefInln fSpec a = either (fatal ("You should use xDefBlck for:\n  "++show (refStuff a))) id (xDef fSpec a)

xDef :: FSpec -> XRefSection -> Either Blocks Inlines 
xDef fSpec a =
    case a of
      XRefProcessAnalysis{}           -> Left . hdr $ (text.l) (NL "Proces: ",EN "Process: ")   <> (singleQuoted . str . nameOfThing . refStuff $ a)
      XRefConceptualAnalysisPattern{} -> Left . hdr $ (text.l) (NL "Thema: ",EN "Theme: ")      <> (singleQuoted . str . nameOfThing . refStuff $ a)
      XRefInterfacesInterface ifc     -> Left . hdr $ (text.l) (NL "Koppelvlak: ",EN "Interface: ") <> (singleQuoted . str . name $ ifc)
      XRefNaturalLanguageTheme mPat   -> Left . hdr $ 
                                       (case mPat of
                                          Nothing  -> (text.l) (NL "Losse eindjes...",EN "Loose ends...")
                                          Just pat -> text (name pat)
                                       )
      XRefSharedLangRelation d     -> Right $ spanWith (xrefPrefix (refStuff a) <> xLabel a,[],[]) (str . showMaybe . numberOf fSpec $ d)
      XRefSharedLangRule r            -> Right $ spanWith (xrefPrefix (refStuff a) <> xLabel a,[],[]) (str . showMaybe . numberOf fSpec $ r)
      XRefConceptualAnalysisRelation d 
            -> Right $ case numberOf fSpec d of
                         Nothing -> (text.l) ( NL "een ongedocumenteerde relatie"
                                             , EN "an undocumented relation")
                         Just i  -> spanWith (xrefPrefix (refStuff a) <> xLabel a,[],[]) 
                                              (    (text.l) (NL "Relatie ",EN "Relation ")
                                                <> str (show i) <> ": "
                                              )  
      XRefConceptualAnalysisRule r    
            -> Right $ case numberOf fSpec r of
                         Nothing -> (text.l) ( NL "een ongedocumenteerde afspraak"
                                             , EN "an undocumented agreement")
                         Just i  -> spanWith (xrefPrefix (refStuff a) <> xLabel a,[],[]) 
                                              (    (text.l) (NL "Regel ",EN "Rule ")
                                                <> str (show i) <> ": "
                                              ) 
      XRefConceptualAnalysisExpression r
            -> Right $ case numberOf fSpec r of
                         Nothing -> (text.l) ( NL "een ongedocumenteerde afspraak"
                                             , EN "an undocumented agreement")
                         Just i  -> spanWith (xrefPrefix (refStuff a) <> xLabel a,[],[]) 
                                              (    (text.l) (NL "Regel ",EN "Rule ")
                                                <> str (show i) <> ": "
                                              ) 
      _ ->  fatal ("xDef not yet defined for "++show (refStuff a))
   where
    showMaybe Nothing = "???"
    showMaybe (Just i)= show i
    hdr = headerWith (xRefRawLabel a, [], []) 2
    -- shorthand for easy localizing    
    l :: LocalizedStr -> String
    l = localize (fsLang fSpec)

------ Symbolic referencing to a chapter/section. ---------------------------------
class Typeable a => Xreferenceble a where
  xLabel :: a  -> String
  xRef :: a -> Inlines
  xDefBlck :: FSpec -> a -> Blocks
  xDefBlck _ a = fatal ("A "++show (typeOf a)++" cannot be labeld in <Blocks>.") --you should use xDefInln instead.
  xDefInln :: FSpec -> a -> Inlines
  xDefInln _ a = fatal ("A "++show (typeOf a)++" cannot be labeld in an <Inlines>.") --you should use xDefBlck instead.
  {-# MINIMAL xLabel, xRef, (xDefBlck | xDefInln) #-}

instance Xreferenceble Chapter where
  xLabel = show
  xRef = citeGen "sec:"
  xDefBlck fSpec a = headerWith ("sec:"<> xLabel a,[],[]) 1 (chptTitle (fsLang fSpec) a)
  
instance Xreferenceble Picture where
  xLabel = caption
  xRef = citeGen "fig:"
  xDefBlck fSpec a = para $ imageWith ("fig:"++xLabel a, [], []) src ("fig:"++xLabel a)(text (caption a))
   where
    opts = getOpts fSpec
    src = ((case fspecFormat opts of
             FLatex  -> dropExtension -- let pdflatex figure out the optimal extension
             _ -> id
           ) . takeFileName . imagePath opts) a

citeGen :: Xreferenceble a => String -> a -> Inlines
citeGen p l = cite cit mempty
  where
    cit :: [Citation]
    cit = 
      [Citation { citationId = p++xLabel l
                , citationPrefix = []
                , citationSuffix = []
                , citationHash = 0
                , citationNoteNum = 0
                , citationMode = NormalCitation
                }]

pandocEqnArray :: [Inlines] -> Blocks
pandocEqnArray [] = mempty
pandocEqnArray xs
 = orderedList (map para xs)


pandocEquationWithLabel :: FSpec -> XRefSection -> Inlines -> Blocks
pandocEquationWithLabel fSpec xref x = 
  para (strong (xDefInln fSpec xref) <> ": " <> x)

data RefStuff = 
  RefStuff { typeOfSection    :: String
           , chapterOfSection :: Chapter
           , nameOfThing      :: String
           , xrefPrefix       :: String
           } deriving Show
refStuff :: XRefSection -> RefStuff
refStuff x  = 
   case x of
     XRefSharedLangRelation d 
       -> RefStuff { typeOfSection    = "relation"
                   , chapterOfSection = SharedLang
                   , nameOfThing      = showRel d
                   , xrefPrefix       = "lst:"
                   }
     XRefDataAnalysisRule r
       -> RefStuff { typeOfSection    = "rule"
                   , chapterOfSection = DataAnalysis
                   , nameOfThing      = name r
                   , xrefPrefix       = "eq:"
                   }
     XRefSharedLangRule r
       -> RefStuff { typeOfSection    = "rule"
                   , chapterOfSection = SharedLang
                   , nameOfThing      = name r
                   , xrefPrefix       = "lst:"
                   }
     XRefProcessAnalysis p
       -> RefStuff { typeOfSection    = "process"
                   , chapterOfSection = ProcessAnalysis
                   , nameOfThing      = name p
                   , xrefPrefix       = "sec:"
                   }
     XRefConceptualAnalysisPattern p
       -> RefStuff { typeOfSection    = "pattern"
                   , chapterOfSection = ConceptualAnalysis
                   , nameOfThing      = name p
                   , xrefPrefix       = "sec:"
                   }
     XRefConceptualAnalysisRelation d
       -> RefStuff { typeOfSection    = "relation"
                   , chapterOfSection = ConceptualAnalysis
                   , nameOfThing      = showRel d
                   , xrefPrefix       = "eq:"
                   }
     XRefConceptualAnalysisRule r 
       -> RefStuff { typeOfSection    = "rule"
                   , chapterOfSection = ConceptualAnalysis
                   , nameOfThing      = name r
                   , xrefPrefix       = "eq:"
                   }
     XRefConceptualAnalysisExpression r 
       -> RefStuff { typeOfSection    = "expression"
                   , chapterOfSection = ConceptualAnalysis
                   , nameOfThing      = name r
                   , xrefPrefix       = "eq:"
                   }
     XRefInterfacesInterface i    
       -> RefStuff { typeOfSection    = "interface"
                   , chapterOfSection = Interfaces
                   , nameOfThing      = name i
                   , xrefPrefix       = "sec:"
                   }
     XRefNaturalLanguageTheme mt
       -> RefStuff { typeOfSection    = "theme"
                   , chapterOfSection = SharedLang
                   , nameOfThing      = case mt of
                                          Nothing -> ":losseEindjes"
                                          Just t  -> name t
                   , xrefPrefix       = "sec:"
                   }


class NumberedThing a where
  numberOf :: FSpec -> a -> Maybe Int

instance NumberedThing Rule where
  numberOf fSpec r = case filter isTheOne ns of
                      [] -> Nothing -- fatal ("Rule has not been numbered: "++name r)
                      [nr] -> Just $ theNr nr 
                      _ -> fatal ("Rule has been numbered multiple times: "++name r)
    where ns = concatMap rulesOfTheme (orderingByTheme fSpec)
          isTheOne :: Numbered RuleCont -> Bool
          isTheOne = (r ==) . cRul . theLoad
instance NumberedThing Relation where
  numberOf fSpec d = case filter isTheOne ns of
                      [] -> Nothing -- fatal ("Relation has not been numbered: "++showRel d)
                      [nr] -> Just $ theNr nr 
                      _ -> fatal ("Relation has been numbered multiple times: "++showRel d)
    where ns = concatMap dclsOfTheme (orderingByTheme fSpec)
          isTheOne :: Numbered DeclCont -> Bool
          isTheOne = (d ==) . cDcl . theLoad
instance NumberedThing A_Concept where
  numberOf fSpec c = case filter isTheOne ns of
                      [] -> Nothing -- fatal ("Concept has not been numbered: "++name c)
                      [nr] -> Just $ theNr nr 
                      _ -> fatal ("Concept has been numbered multiple times: "++name c)
    where ns = concatMap cptsOfTheme (orderingByTheme fSpec)
          isTheOne :: Numbered CptCont -> Bool
          isTheOne = (c ==) . cCpt . theLoad


-- | This function orders the content to print by theme. It returns a list of
--   tripples by theme. The last tripple might not have a theme, but will contain everything
--   that isn't handled in a specific theme.

data ThemeContent =
       Thm { themeNr ::      Int
           , patOfTheme ::   Maybe Pattern -- A theme is about either a pattern or about everything outside patterns
           , rulesOfTheme :: [Numbered RuleCont] -- The (numbered) rules of that theme
           , dclsOfTheme ::  [Numbered DeclCont] -- The (numbered) relations that are used in a rule of this theme, but not in any rule of a previous theme.
           , cptsOfTheme ::  [Numbered CptCont]  -- The (numbered) concepts that are used in a rule of this theme, but not in any rule of a previous theme.
           }
data Numbered t =
 Nr { theNr ::   Int
    , theLoad :: t
    }
instance Named t => Named (Numbered t) where
 name = name . theLoad
data RuleCont = CRul { cRul ::  Rule
                     , cRulPurps :: [Purpose]
                     , cRulMeaning :: Maybe Markup
                     }
data DeclCont = CDcl { cDcl ::  Relation
                     , cDclPurps :: [Purpose]
                     , cDclMeaning :: Maybe Markup
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
            , agreementNr ::  Int --For relations andrules
            }

-- orderingByTheme organizes the content of a specification in patterns according to a define-before-use policy.
-- It must ensure that all rules, relations and concepts from the context are included in the specification.
orderingByTheme :: FSpec -> [ThemeContent]
orderingByTheme fSpec
 = f ( Counter 1 1 1 --the initial numbers of the countes
     , (sortWith origin . filter rulMustBeShown . fallRules)  fSpec
     , (sortWith origin . filter relMustBeShown . relsDefdIn) fSpec 
     , (sortBy conceptOrder . filter cptMustBeShown . concs)  fSpec
     ) $
     [Just pat | pat <- vpatterns fSpec -- The patterns that should be taken into account for this ordering
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
  rulMustBeShown r = hasMeaning r || hasPurpose r
  relMustBeShown :: Relation -> Bool
  relMustBeShown d 
    | isIdent d || name d == "V" = False  --Identity relation has no meaning defined
    | otherwise = (hasMeaning d || hasPurpose d) && (decusr d || forNonUserDefdRule d)  
  hasPurpose :: Motivated a => a -> Bool
  hasPurpose = not . null . purposesDefinedIn fSpec (fsLang fSpec)
  hasMeaning :: Meaning a => a -> Bool
  hasMeaning = isJust . meaning (fsLang fSpec)
  forNonUserDefdRule :: Relation -> Bool
  forNonUserDefdRule d = any isPropRuleForDcl . fallRules $ fSpec 
    where
      isPropRuleForDcl :: Rule -> Bool
      isPropRuleForDcl rul =
        case rrdcl rul of
           Nothing -> False
           Just (_,x) -> x == d
  cptMustBeShown = not . null . concDefs fSpec
  f :: (Counters, [Rule], [Relation], [A_Concept]) -> [Maybe Pattern] -> [ThemeContent]
  f stuff pats
   = case pats of
       pat:pats' -> let ( thm, rest) = partitionByTheme pat stuff
                    in thm : f rest pats'
       []        -> case stuff of
                      (_,[],[],[]) -> []
                      _ -> fatal "No stuff should be left over."

  rul2rulCont :: Rule -> RuleCont
  rul2rulCont rul
    = CRul { cRul      = rul
           , cRulPurps = fromMaybe [] $ purposeOf fSpec (fsLang fSpec) rul
           , cRulMeaning = meaning (fsLang fSpec) rul
           }
  dcl2dclCont :: Relation -> DeclCont
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
  partitionByTheme :: Maybe Pattern  -- Just pat if this theme is from a pattern, otherwise this stuff comes from outside a pattern (but inside a context).
                   -> ( Counters, [Rule], [Relation], [A_Concept])
                   -> ( ThemeContent , ( Counters ,[Rule], [Relation], [A_Concept])
                      )
  partitionByTheme mPat (cnt, ruls, rels, cpts)
      = ( Thm { themeNr      = pNr cnt
              , patOfTheme   = mPat
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
          where relsInTheme p = relsDefdIn p `uni` bindedRelationsIn p
       (themeCpts,restCpts) = partition (inThisTheme concs) cpts
       inThisTheme :: Eq a => (Pattern -> [a]) -> a -> Bool
       inThisTheme allElemsOf x
         = case mPat of
             Nothing  -> True
             Just pat -> x `elem` allElemsOf pat

--GMI: What's the meaning of the Int? HJO: This has to do with the numbering of rules
dpRule' :: FSpec -> [Rule] -> Int -> [A_Concept] -> [Relation]
          -> ([(Inlines, [Blocks])], Int, [A_Concept], [Relation])
dpRule' fSpec = dpR
 where
   l lstr = text $ localize (fsLang fSpec) lstr
   dpR [] n seenConcs seenRelations = ([], n, seenConcs, seenRelations)
   dpR (r:rs) n seenConcs seenRelations
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
            purposes2Blocks (getOpts fSpec) (purposesDefinedIn fSpec (fsLang fSpec) r) -- Als eerste de uitleg van de betreffende regel..
         <> purposes2Blocks (getOpts fSpec) [p | d<-nds, p<-purposesDefinedIn fSpec (fsLang fSpec) d]  -- Dan de uitleg van de betreffende relaties
         <> case (nds, fsLang fSpec) of
             ([] ,_)       -> mempty
             ([d],Dutch)   -> plain ("Om dit te formaliseren is een " <> (if isFunction d then "functie"  else "relatie" ) <> " nodig:")
             ([d],English) -> plain ("In order to formalize this, a " <> (if isFunction d then "function" else "relation") <> " is introduced:")
             (_  ,Dutch)   -> plain ("Om te komen tot de formalisatie van " <> xRef (XRefSharedLangRule r)
                                    <>  " (" <> (singleQuoted.str.name) r  <> ") "
                                    <> str (" zijn de volgende "++count Dutch (length nds) "in deze paragraaf geformaliseerde relatie"++" nodig."))
             (_  ,English) -> plain ("To arrive at the formalization of "   <> xRef (XRefSharedLangRule r) <> str (", the following "++count English (length nds) "relation"++" are introduced."))
         <> (bulletList . map (plain . showRef) $ nds)
         <> (case nds of
              [] -> case rds of
                       []   -> mempty
                       [rd] -> plain (  l (NL "Om dit te formalizeren maken we gebruik van relatie "
                                          ,EN "We use relation ")
                                     <> showRef rd 
                                     <> l (NL ".", EN " to formalize this.")
                                     )
                       _    ->    plain (  l (NL "Dit formaliseren we door gebruik te maken van de volgende relaties: "
                                             ,EN "We formalize this using relations "))
                               <> (bulletList  . map (plain . showRef) $ rds)
              _  -> case rds of
                       []   -> mempty
                       [rd] -> plain (  l (NL "Daarnaast gebruiken we relatie ", EN "Beside that, we use relation ")
                                      <> showRef rd 
                                      <> l (NL " om ", EN " to formalize ")
                                      <> xRef (XRefSharedLangRule r)
                                      <> l (NL " te formaliseren: ", EN ": ")
                                     ) 
                       _    -> plain (   l (NL " Om ", EN " To formalize ")
                                      <> xRef (XRefSharedLangRule r)
                                      <> l (NL " te formaliseren, gebruiken we daarnaast ook de relaties: "
                                           ,EN " we also use relations ")
                                     ) <>
                               (bulletList  . map (plain . showRef) $ rds)
           )
         <> plain (if isSignal r
                   then l ( NL "Activiteiten, die door deze regel zijn gedefinieerd, zijn afgerond zodra: "
                          , EN "Activities that are defined by this rule are finished when: ")
                   else l (NL "De regel luidt: ", EN "This means: ")
                  )
         <> pandocEquationWithLabel fSpec (XRefConceptualAnalysisExpression r) (showMath r)
         <> (if length nds<=1
             then mempty
             else plain (  l (NL "Dit komt overeen met ", EN "This corresponds to ")
                        <> xRef (XRefSharedLangRule r)
                        <> " (" <> (singleQuoted.str.name) r  <> ")."

                        )
            )
        showRef :: Relation -> Inlines
        showRef dcl = xRef (XRefConceptualAnalysisRelation dcl) <> "(" <> (str . showRel) dcl <> ")"
        
        ncs = concs r >- seenConcs            -- newly seen concepts
        cds = [(c,cd) | c<-ncs, cd<-conceptDefs fSpec, cdcpt cd==name c]    -- ... and their definitions
        ds  = bindedRelationsIn r
        nds = ds >- seenRelations     -- newly seen relations
        rds = ds `isc` seenRelations  -- previously seen relations
        ( dpNext, n', seenCs,  seenDs ) = dpR rs (n+length cds+length nds+1) (ncs++seenConcs) (nds++seenRelations)

purposes2Blocks :: Options -> [Purpose] -> Blocks
purposes2Blocks opts ps
 = case ps of
      [] -> mempty
            -- by putting the ref after the first inline of the definition, it aligns nicely with the definition
      _  -> case concatMarkup [expl{amPandoc = insertAfterFirstInline (ref purp) $ amPandoc expl} | purp<-ps, let expl=explMarkup purp] of
             Nothing -> mempty
             Just p  -> fromList $ amPandoc p
       where   -- The reference information, if available for this purpose, is put
        ref :: Purpose -> [Inline]
        ref purp = case fspecFormat opts of
                    FLatex | (not.null.explRefIds) purp-> [RawInline (Text.Pandoc.Builder.Format "latex")
                                                             (texOnlyMarginNote (intercalate "; " (map latexEscShw (explRefIds purp))++"\n"))]
                    _                                  -> []
concatMarkup :: [Markup] -> Maybe Markup
concatMarkup es
 = case eqCl amLang es of
    []   -> Nothing
    [cl] -> Just Markup { amLang   = amLang (head cl)
                          , amPandoc = concatMap amPandoc es
                          }
    cls  -> fatal ("don't call concatMarkup with different languages and formats\n   "++
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
math s = BuggyBuilder.math s -- ("{"++s++"}")

violation2Inlines :: FSpec -> PairView Expression -> Inlines
violation2Inlines fSpec _ = (text.l) (NL "<meldingstekst moet hier nog worden gegenereerd>"
                                        ,EN "<violation message should be printed here>"
                                        )
  where
    l = localize (fsLang fSpec)
