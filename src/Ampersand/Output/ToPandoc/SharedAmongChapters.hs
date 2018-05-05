{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.ToPandoc.SharedAmongChapters
    ( module Text.Pandoc.Builder
    , module Text.Pandoc
    , module Ampersand.ADL1
    , module Ampersand.Basics
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
    , purposes2Blocks
    , violation2Inlines
    , isMissing
    , lclForLang
    , dpRule'
    , ThemeContent(..), orderingByTheme
    , Numbered(..), RuleCont(..),DeclCont(..),CptCont(..)
    , plainText
    , sortWith)
where
import           Ampersand.ADL1 hiding (Meta)
import           Ampersand.Basics
import           Ampersand.Classes
import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec
import           Ampersand.Graphic.Graphics
import           Ampersand.Misc
import           Ampersand.Output.PandocAux
import           Data.List
import           Data.Maybe
import           Data.Ord
import qualified Data.Set as Set
import qualified Data.Time.Format as DTF
import           Data.Typeable
import           GHC.Exts(sortWith)
import           System.FilePath  -- (combine,addExtension,replaceExtension)
import           Text.Pandoc hiding (trace)
import           Text.Pandoc.Builder

-- | Define the order of the chapters in the document.
chaptersInDoc :: Options -> [Chapter]
chaptersInDoc opts 
     | test opts          = [ SharedLang]
     | diagnosisOnly opts = [ Diagnosis]
     | otherwise          = [ Intro
                            , SharedLang
                       --     , Diagnosis
                            , ConceptualAnalysis
                       --     , DataAnalysis
                       --     , Glossary
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

------ Symbolic referencing to a chapter/section. ---------------------------------
class Typeable a => Xreferenceble a where
  xSafeLabel :: FSpec -> a -> String -- The full string that is used as ID for referencing
  hyperLinkTo :: FSpec -> a -> Inlines
  xDefBlck :: FSpec -> a -> Blocks
  xDefBlck _ a = fatal ("A "++show (typeOf a)++" cannot be labeld in <Blocks>.") --you should use xDefInln instead.
  xDefInln :: FSpec -> a -> Inlines
  xDefInln _ a = fatal ("A "++show (typeOf a)++" cannot be labeld in an <Inlines>.") --you should use xDefBlck instead.
  {-# MINIMAL xSafeLabel, hyperLinkTo, (xDefBlck | xDefInln) #-}

instance Xreferenceble Chapter where
  xSafeLabel _ a = show Sec++show a
  hyperLinkTo fSpec = citeGen fSpec
  xDefBlck fSpec a = headerWith (xSafeLabel fSpec a,[],[]) 1 (chptTitle (fsLang fSpec) a)
  
instance Xreferenceble Picture where
  xSafeLabel _ a = show Fig++caption a
  hyperLinkTo fSpec = citeGen fSpec
  xDefBlck fSpec a = para $ imageWith (xSafeLabel fSpec a, [], []) src (xSafeLabel fSpec a)(text (caption a))
   where
    opts = getOpts fSpec
    src  = (if fspecFormat opts `elem` [Fpdf,Flatex] then dropExtension else id)-- let pdflatex figure out the optimal extension
             . takeFileName . imagePath opts $ a
instance Xreferenceble XRefSection where
  xSafeLabel fSpec a = 
       (show . xrefPrefix . refStuff fSpec $ a)
     <> show (chapterOfSection x)
     <> typeOfSection x
     <> "-"
     <> nameOfThing x
    where 
      x = refStuff fSpec a


  hyperLinkTo fSpec = codeGen fSpec
  xDefBlck fSpec a = either id (fatal ("You should use xDefInln for:\n  "++show (refStuff fSpec a))) (hyperTarget fSpec a)
  xDefInln fSpec a = either (fatal ("You should use xDefBlck for:\n  "++show (refStuff fSpec a))) id (hyperTarget fSpec a)

hyperTarget :: FSpec -> XRefSection -> Either Blocks Inlines 
hyperTarget fSpec a =
    case a of
      XRefProcessAnalysis{}           -> Left . hdr $ (text.l) (NL "Proces: ",EN "Process: ")   <> (singleQuoted . str . nameOfThing . refStuff fSpec $ a)
      XRefConceptualAnalysisPattern{} -> Left . hdr $ (text.l) (NL "Thema: ",EN "Theme: ")      <> (singleQuoted . str . nameOfThing . refStuff fSpec $ a)
      XRefInterfacesInterface ifc     -> Left . hdr $ (text.l) (NL "Koppelvlak: ",EN "Interface: ") <> (singleQuoted . str . name $ ifc)
      XRefNaturalLanguageTheme mPat   -> Left . hdr $ 
                                       (case mPat of
                                          Nothing  -> (text.l) (NL "Losse eindjes...",EN "Loose ends...")
                                          Just pat -> text (name pat)
                                       )
      XRefSharedLangRelation d        -> -- Right $ spanWith (xSafeLabel fSpec a,[],[]) (str . show . numberOf fSpec $ d)
                                         Left $ divWith (xSafeLabel fSpec a,["listing"],[]) 
                                                        (   (para . str $ showRel d)
                                                          <>codeBlockWith 
                                                               ("", ["adl"],[("caption",name d)]) 
                                                               ( "Deze RELATIE moet nog verder worden uitgewerkt in de Haskell code")
                                                        )
      XRefSharedLangRule r            -> -- Right $ spanWith (xSafeLabel fSpec a,[],[]) (str . show . numberOf fSpec $ r)
                                         Left $ divWith (xSafeLabel fSpec a,["listing"],[])
                                                        (   (para . text $ name r)
                                                          <>codeBlockWith 
                                                               ("", ["adl"],[("caption",name r)]) 
                                                               ( "Deze REGEL moet nog verder worden uitgewerkt in de Haskell code")        

                                                          -- <>(case meaning (fsLang fSpec) r of
                                                          --     Nothing 
                                                          --         ->( plain $
                                                          --                (str.l) (NL "De regel ",EN "The rule ")
                                                          --             <> (emph.str.name) r
                                                          --             <> (str.l) (NL " is ongedocumenteerd.",EN " is undocumented.")
                                                          --           )
                                                          --     Just m
                                                          --         -> (fromList . amPandoc $ m)
                                                          --   )
                                                        )
      XRefConceptualAnalysisRelation d 
            -> Right $ spanWith (xSafeLabel fSpec a,[],[]) 
                                (    (text.l) (NL "Relatie ",EN "Relation ")
                                  <> (str . show . numberOf fSpec $ d)
                                )  
      XRefConceptualAnalysisRule r    
            -> Right $ spanWith (xSafeLabel fSpec a,[],[]) 
                                (    (text.l) (NL "Regel ",EN "Rule ")
                                  <> (str . show . numberOf fSpec $ r)
                                ) 
      XRefConceptualAnalysisExpression r
            -> Right $ spanWith (xSafeLabel fSpec a,[],[]) 
                                (    (text.l) (NL "Regel ",EN "Rule ")
                                  <> (str . show . numberOf fSpec $ r)
                                ) 
      _ ->  fatal ("hyperTarget not yet defined for "++show (refStuff fSpec a))
   where
    hdr = headerWith (xSafeLabel fSpec a, [], []) 2
    -- shorthand for easy localizing    
    l :: LocalizedStr -> String
    l = localize (fsLang fSpec)
citeGen :: Xreferenceble a => FSpec -> a -> Inlines
citeGen fSpec l = 
  cite [Citation { citationId = xSafeLabel fSpec l
                 , citationPrefix = []
                 , citationSuffix = []
                 , citationHash = 0
                 , citationNoteNum = 0
                 , citationMode = NormalCitation
                 }
       ] mempty
codeGen :: Xreferenceble a => FSpec -> a -> Inlines
codeGen fSpec = code . xSafeLabel fSpec 

pandocEqnArray :: [Inlines] -> Blocks
pandocEqnArray [] = mempty
pandocEqnArray xs
 = orderedList (map para xs)

data CrossrefType = Lst | Eq | Sec | Tbl | Fig -- Special prefixes that make pandoc-crossref work. 
instance Show CrossrefType where
  showsPrec _ x
   = showString $ case x of
                    Lst -> "lst:"
                    Eq  -> "eq:"
                    Sec -> "sec:"
                    Tbl -> "tbl:"
                    Fig -> "fig:"
pandocEquationWithLabel :: FSpec -> XRefSection -> Inlines -> Blocks
pandocEquationWithLabel fSpec xref x = 
  para (strong (xDefInln fSpec xref) <> x)

data RefStuff = 
  RefStuff { typeOfSection    :: String
           , chapterOfSection :: Chapter
           , nameOfThing      :: String
           , xrefPrefix       :: CrossrefType
           } deriving Show
refStuff :: FSpec -> XRefSection -> RefStuff
refStuff fSpec x  = 
   case x of
     XRefSharedLangRelation d 
       -> RefStuff { typeOfSection    = relation
                   , chapterOfSection = SharedLang
                   , nameOfThing      = showRel d
                   , xrefPrefix       = Lst
                   }
     XRefDataAnalysisRule r
       -> RefStuff { typeOfSection    = rule
                   , chapterOfSection = DataAnalysis
                   , nameOfThing      = name r
                   , xrefPrefix       = Eq
                   }
     XRefSharedLangRule r
       -> RefStuff { typeOfSection    = rule
                   , chapterOfSection = SharedLang
                   , nameOfThing      = name r
                   , xrefPrefix       = Lst
                   }
     XRefProcessAnalysis p
       -> RefStuff { typeOfSection    = pattern
                   , chapterOfSection = ProcessAnalysis
                   , nameOfThing      = name p
                   , xrefPrefix       = Sec
                   }
     XRefConceptualAnalysisPattern p
       -> RefStuff { typeOfSection    = pattern
                   , chapterOfSection = ConceptualAnalysis
                   , nameOfThing      = name p
                   , xrefPrefix       = Sec
                   }
     XRefConceptualAnalysisRelation d
       -> RefStuff { typeOfSection    = relation
                   , chapterOfSection = ConceptualAnalysis
                   , nameOfThing      = showRel d
                   , xrefPrefix       = Eq
                   }
     XRefConceptualAnalysisRule r 
       -> RefStuff { typeOfSection    = rule
                   , chapterOfSection = ConceptualAnalysis
                   , nameOfThing      = name r
                   , xrefPrefix       = Eq
                   }
     XRefConceptualAnalysisExpression r 
       -> RefStuff { typeOfSection    = expression
                   , chapterOfSection = ConceptualAnalysis
                   , nameOfThing      = name r
                   , xrefPrefix       = Eq
                   }
     XRefInterfacesInterface i    
       -> RefStuff { typeOfSection    = interface
                   , chapterOfSection = Interfaces
                   , nameOfThing      = name i
                   , xrefPrefix       = Sec
                   }
     XRefNaturalLanguageTheme mt
       -> RefStuff { typeOfSection    = theme
                   , chapterOfSection = SharedLang
                   , nameOfThing      = case mt of
                                          Nothing -> ":losseEindjes"
                                          Just t  -> name t
                   , xrefPrefix       = Sec
                   }
  where (                relation , rule  , expression , pattern ,interface  , theme) =
          case fsLang fSpec of
            English -> ("relation","rule" ,"expression","pattern","interface","theme")
            Dutch   -> ("relatie" ,"regel","expressie" ,"pattern","interface","thema")

class NumberedThing a where
  numberOf :: FSpec -> a -> Int

instance NumberedThing Rule where
  numberOf fSpec r = case filter isTheOne ns of
                      [] -> fatal ("Rule has not been numbered: "++name r)
                      [nr] -> theNr nr 
                      _ -> fatal ("Rule has been numbered multiple times: "++name r)
    where ns = concatMap rulesOfTheme (orderingByTheme fSpec)
          isTheOne :: Numbered RuleCont -> Bool
          isTheOne = (r ==) . cRul . theLoad
instance NumberedThing Relation where
  numberOf fSpec d = case filter isTheOne ns of
                      [] -> fatal ("Relation has not been numbered: "++showRel d)
                      [nr] -> theNr nr 
                      _ -> fatal ("Relation has been numbered multiple times: "++showRel d)
    where ns = concatMap dclsOfTheme (orderingByTheme fSpec)
          isTheOne :: Numbered DeclCont -> Bool
          isTheOne = (d ==) . cDcl . theLoad
instance NumberedThing A_Concept where
  numberOf fSpec c = case filter isTheOne ns of
                      [] -> fatal ("Concept has not been numbered: "++name c)
                      [nr] -> theNr nr 
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
                     , cDclPairs :: AAtomPairs
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
     , (sortWith origin . filter rulMustBeShown . Set.elems . fallRules)  fSpec
     , (sortWith origin . filter relMustBeShown . Set.elems . relsDefdIn) fSpec 
     , (sortBy conceptOrder . filter cptMustBeShown . Set.elems . concs)  fSpec
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
  rulMustBeShown r = 
     (not . isPropertyRule $ r) -- property rules are shown as part of the declaration
  relMustBeShown :: Relation -> Bool
  relMustBeShown = decusr
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
       (thmRuls,restRuls) = partition (inThisTheme rulesInTheme) ruls
          where rulesInTheme p = Set.filter ( \r -> Just (name p) == rrpat r) (fallRules fSpec)
       (themeDcls,restDcls) = partition (inThisTheme relsInTheme) rels
          where relsInTheme p = relsDefdIn p `Set.union` bindedRelationsIn p
       (themeCpts,restCpts) = partition (inThisTheme concs) cpts
       inThisTheme :: Eq a => (Pattern -> Set.Set a) -> a -> Bool
       inThisTheme allElemsOf x
         = case mPat of
             Nothing  -> True
             Just pat -> x `elem` allElemsOf pat

--GMI: What's the meaning of the Int? HJO: This has to do with the numbering of rules
dpRule' :: FSpec -> [Rule] -> Int -> A_Concepts -> Relations
          -> ([(Inlines, [Blocks])], Int, A_Concepts, Relations)
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
         <> purposes2Blocks (getOpts fSpec) [p | d<-Set.elems nds, p<-purposesDefinedIn fSpec (fsLang fSpec) d]  -- Dan de uitleg van de betreffende relaties
         <> case (Set.elems . Set.map EDcD $ nds, fsLang fSpec) of
             ([] ,_)       -> mempty
             ([d],Dutch)   -> plain ("Om dit te formaliseren is een " <> (if isFunction d then "functie"  else "relatie" ) <> " nodig:")
             ([d],English) -> plain ("In order to formalize this, a " <> (if isFunction d then "function" else "relation") <> " is introduced:")
             (_  ,Dutch)   -> plain ("Om te komen tot de formalisatie van " <> hyperLinkTo fSpec (XRefSharedLangRule r)
                                    <>  " (" <> (singleQuoted.str.name) r  <> ") "
                                    <> str (" zijn de volgende "++count Dutch (length nds) "in deze paragraaf geformaliseerde relatie"++" nodig."))
             (_  ,English) -> plain ("To arrive at the formalization of "   <> hyperLinkTo fSpec (XRefSharedLangRule r) <> str (", the following "++count English (length nds) "relation"++" are introduced."))
         <> (bulletList . map (plain . showRef) . Set.elems $ nds)
         <> (if null nds
             then case Set.elems rds of
                       []   -> mempty
                       [rd] -> plain (  l (NL "Om dit te formalizeren maken we gebruik van relatie "
                                          ,EN "We use relation ")
                                     <> showRef rd 
                                     <> l (NL ".", EN " to formalize this.")
                                     )
                       _    ->    plain (  l (NL "Dit formaliseren we door gebruik te maken van de volgende relaties: "
                                             ,EN "We formalize this using relations "))
                               <> (bulletList  . map (plain . showRef) . Set.elems $ rds)
             else case Set.elems rds of
                       []   -> mempty
                       [rd] -> plain (  l (NL "Daarnaast gebruiken we relatie ", EN "Beside that, we use relation ")
                                      <> showRef rd 
                                      <> l (NL " om ", EN " to formalize ")
                                      <> hyperLinkTo fSpec (XRefSharedLangRule r)
                                      <> l (NL " te formaliseren: ", EN ": ")
                                     ) 
                       _    -> plain (   l (NL " Om ", EN " To formalize ")
                                      <> hyperLinkTo fSpec (XRefSharedLangRule r)
                                      <> l (NL " te formaliseren, gebruiken we daarnaast ook de relaties: "
                                           ,EN " we also use relations ")
                                     ) <>
                               (bulletList  . map (plain . showRef) . Set.elems $ rds)
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
                        <> hyperLinkTo fSpec (XRefSharedLangRule r)
                        <> " (" <> (singleQuoted.str.name) r  <> ")."

                        )
            )
        showRef :: Relation -> Inlines
        showRef dcl = hyperLinkTo fSpec (XRefConceptualAnalysisRelation dcl) <> "(" <> (str . showRel) dcl <> ")"
        
        ncs = concs r Set.\\ seenConcs            -- newly seen concepts
        cds = [(c,cd) | c<-Set.elems ncs, cd<-conceptDefs fSpec, cdcpt cd==name c]    -- ... and their definitions
        ds  = bindedRelationsIn r
        nds = ds Set.\\ seenRelations     -- newly seen relations
        rds = ds `Set.intersection` seenRelations  -- previously seen relations
        ( dpNext, n', seenCs,  seenDs ) = dpR rs (n+length cds+length nds+1) (ncs `Set.union` seenConcs) (nds `Set.union` seenRelations)

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
        ref purp = if fspecFormat opts `elem` [Fpdf, Flatex] && (not.null.explRefIds) purp
                   then [RawInline (Text.Pandoc.Builder.Format "latex")
                            (texOnlyMarginNote (intercalate "; " (map latexEscShw (explRefIds purp))++"\n"))]
                   else []
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

plainText :: String -> Blocks
plainText = plain . text

violation2Inlines :: FSpec -> PairView Expression -> Inlines
violation2Inlines fSpec _ = (text.l) (NL "<meldingstekst moet hier nog worden gegenereerd>"
                                        ,EN "<violation message should be printed here>"
                                        )
  where
    l = localize (fsLang fSpec)
