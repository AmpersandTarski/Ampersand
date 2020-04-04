{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ampersand.Output.ToPandoc.SharedAmongChapters
    ( module Text.Pandoc.Builder
    , module Text.Pandoc
    , module Ampersand.ADL1
    , module Ampersand.Basics
    , module Ampersand.Core.ShowAStruct
    , module Ampersand.FSpec
    , module Ampersand.Graphic.Graphics
    , module Ampersand.Misc.HasClasses
    , module Ampersand.Output.PandocAux
    , module Ampersand.Classes
    , Chapter(..)
    , chaptersInDoc
    , Xreferenceble(..)
    , CustomSection(..)
    , pandocEqnArray
    , pandocEquationWithLabel
    , Purpose(..)
    , printMeaning
    , printMarkup
    , printPurposes
    , purposes2Blocks
    , violation2Inlines
    , isMissing
    , lclForLang
    , dpRule'
    , ThemeContent(..), orderingByTheme
    , Numbered(..), RuleCont(..),DeclCont(..),CptCont(..)
    , plainText
    )
where
import           Ampersand.ADL1 hiding (Meta)
import           Ampersand.Basics hiding (Reader,Identity,toList,link)
import           Ampersand.Classes
import           Ampersand.Core.ShowAStruct
import           Ampersand.Input.ADL1.FilePos
import           Ampersand.FSpec
import           Ampersand.Graphic.Graphics
import           Ampersand.Misc.HasClasses
import           Ampersand.Output.PandocAux
import           Data.Hashable
import           Data.Typeable (typeOf)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import           RIO.Time
import           Text.Pandoc hiding (trace,Verbosity,getVerbosity)
import           Text.Pandoc.Builder

-- | Define the order of the chapters in the document.
chaptersInDoc :: (HasDocumentOpts env) => env -> [Chapter]
chaptersInDoc env = view chaptersL env

data CustomSection
             = XRefSharedLangRelation Relation
             | XRefSharedLangRule Rule
             | XRefSharedLangTheme (Maybe Pattern)
             | XRefDataAnalysisRule Rule
             | XRefConceptualAnalysisPattern Pattern
             | XRefConceptualAnalysisRelation Relation
             | XRefConceptualAnalysisRule Rule
             | XRefConceptualAnalysisExpression Rule

------ Symbolic referencing to a chapter/section. ---------------------------------

-- | Things that can be referenced in a document. 
class Typeable a => Xreferenceble a where
  xDefBlck :: (HasDirOutput env, HasDocumentOpts env) => env -> FSpec -> a -> Blocks
  xDefBlck _ _ a = fatal ("A "<>tshow (typeOf a)<>" cannot be labeld in <Blocks>.") --you should use xDefInln instead.
  -- ^ function that defines the target Blocks of something that can be referenced.

  xDefInln :: (HasOutputLanguage env) => env -> FSpec -> a -> Inlines
  xDefInln _ _ a = fatal ("A "<>tshow (typeOf a)<>" cannot be labeld in an <Inlines>.") --you should use xDefBlck instead.
  -- ^ function that defines the target Inlines of something that can e referenced.

  hyperLinkTo :: a -> Inlines
  -- ^ function that returns a link to something that can be referenced.
  xSafeLabel :: a -> Text -- The full string that is used as ID for referencing
  {-# MINIMAL xSafeLabel, hyperLinkTo, (xDefBlck | xDefInln) #-}

instance Xreferenceble Chapter where
  xSafeLabel a = tshow Sec<>tshow a
  hyperLinkTo = codeGen'
  xDefBlck env fSpec a = headerWith (xSafeLabel a,[],[]) 1 (chptTitle (outputLang env fSpec) a)

instance Xreferenceble Picture where
  xSafeLabel a = tshow Fig<>caption a
  hyperLinkTo = codeGen'
  xDefBlck env _ a = para $ imageWith (xSafeLabel a, [], []) (T.pack src) (xSafeLabel a)(text (caption a))
   where
    src  = imagePathRelativeToDirOutput env $ a
instance Xreferenceble CustomSection where
  xSafeLabel a = 
       (tshow . xrefPrefix . refStuff $ a)
     <> tshow (chapterOfSection x)
     <> typeOfSection x
     <> "-"
     <> (tshow . hash . nameOfThing $ x) -- Hash, to make sure there are no fancy characters. 
    where 
      x = refStuff a
  hyperLinkTo = codeGen'
  xDefBlck env fSpec a = either id (fatal ("You should use xDefInln for:\n  "<>tshow (refStuff a))) (hyperTarget env fSpec a)
  xDefInln env fSpec a = either (fatal ("You should use xDefBlck for:\n  "<>tshow (refStuff a))) id (hyperTarget env fSpec a)

hyperTarget :: (HasOutputLanguage env) => env -> FSpec -> CustomSection -> Either Blocks Inlines 
hyperTarget env fSpec a =
    case a of
      XRefConceptualAnalysisPattern{} -> Left . hdr $ (text.l) (NL "Thema: ",EN "Theme: ")      <> (singleQuoted . str . nameOfThing . refStuff $ a)
      XRefSharedLangTheme mPat   -> Left . hdr $ 
                                       (case mPat of
                                          Nothing  -> (text.l) (NL "Losse eindjes...",EN "Loose ends...")
                                          Just pat -> text (name pat)
                                       )
      XRefSharedLangRelation d        -> Right $ spanWith (xSafeLabel a,[],[]) (str . showRel $ d)
                                      --   Left $ divWith (xSafeLabel a,[],[]) 
                                      --                  (   (para . str $ showRel d)
                                      --                    <>codeBlockWith 
                                      --                         ("", ["adl"],[("caption",showRel d)]) 
                                      --                         ( "Deze RELATIE moet nog verder worden uitgewerkt in de Haskell code")
                                      --                  )
      XRefSharedLangRule r            -> Right $ spanWith (xSafeLabel a,[],[]) (str . tshow . name $ r)
                                      --   Left $ divWith (xSafeLabel a,[],[])
                                      --                  (   (para . text $ name r)
                                      --                  --  <>codeBlockWith 
                                      --                  --       ("", ["adl"],[("caption",name r)]) 
                                      --                  --       ( "Deze REGEL moet nog verder worden uitgewerkt in de Haskell code")        
                                      --                    <>printMeaning (outputLang env fSpec) r
                                      --                  )
      XRefConceptualAnalysisRelation _d 
            -> Right $ spanWith (xSafeLabel a,[],[]) 
                                (    (text.l) (NL "Relatie ",EN "Relation ")
                               --   <> (str . show . numberOf fSpec $ d)
                                )  
      XRefConceptualAnalysisRule _r    
            -> Right $ spanWith (xSafeLabel a,[],[]) 
                                (    (text.l) (NL "Regel ",EN "Rule ")
                               --   <> (str . show . numberOf fSpec $ r)
                                ) 
      XRefConceptualAnalysisExpression _r
            -> Right $ spanWith (xSafeLabel a,[],[]) 
                                (    (text.l) (NL "Regel ",EN "Rule ")
                               --   <> (str . show . numberOf fSpec $ r)
                                ) 
      _ ->  fatal ("hyperTarget not yet defined for "<>tshow (refStuff a))
   where
    hdr = headerWith (xSafeLabel a, [], []) 2
    -- shorthand for easy localizing    
    l :: LocalizedStr -> Text
    l = localize (outputLang env fSpec)
codeGen' :: Xreferenceble a => a -> Inlines
codeGen' a = 
  cite [Citation { citationId = xSafeLabel a
                 , citationPrefix = [Space]
                 , citationSuffix = [Space]
                 , citationHash = 0
                 , citationNoteNum = 0
                 , citationMode = NormalCitation
                 }
       ] $ code (xSafeLabel a)

pandocEqnArray :: [Inlines] -> Blocks
pandocEqnArray [] = mempty
pandocEqnArray xs
 = orderedList (map para xs)

data CrossrefType = Dfn | Agr | Eq | Sec | Tbl | Fig -- Special prefixes that make pandoc-crossref work. 
instance Show CrossrefType where
  show x = case x of
            Dfn -> "lst:"
            Agr -> "agr:"
            Eq  -> "eq:"
            Sec -> "sec:"
            Tbl -> "tbl:"
            Fig -> "fig:"
pandocEquationWithLabel :: (HasOutputLanguage env) => env -> FSpec -> CustomSection -> Inlines -> Blocks
pandocEquationWithLabel env fSpec xref x = 
  para (strong (xDefInln env fSpec xref) <> x)

data RefStuff = 
  RefStuff { typeOfSection    :: Text
           , chapterOfSection :: Chapter
           , nameOfThing      :: Text
           , xrefPrefix       :: CrossrefType
           } deriving Show
refStuff :: CustomSection -> RefStuff
refStuff x  = 
   case x of
     XRefSharedLangRelation d 
       -> RefStuff { typeOfSection    = relation
                   , chapterOfSection = SharedLang
                   , nameOfThing      = showRel d
                   , xrefPrefix       = Dfn
                   }
     XRefDataAnalysisRule r
       -> RefStuff { typeOfSection    = rule
                   , chapterOfSection = DataAnalysis
                   , nameOfThing      = name r
                   , xrefPrefix       = Agr
                   }
     XRefSharedLangRule r
       -> RefStuff { typeOfSection    = rule
                   , chapterOfSection = SharedLang
                   , nameOfThing      = name r
                   , xrefPrefix       = Agr
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
     XRefSharedLangTheme mt
       -> RefStuff { typeOfSection    = theme
                   , chapterOfSection = SharedLang
                   , nameOfThing      = case mt of
                                          Nothing -> ":losseEindjes"
                                          Just t  -> name t
                   , xrefPrefix       = Sec
                   }
  where (relation , rule  , expression , pattern , theme) =
          ("relation","rule" ,"expression","pattern","theme")
         

{- 
class NumberedThing a where
  numberOf :: FSpec -> a -> Int

instance NumberedThing Rule where
  numberOf fSpec r = case filter isTheOne ns of
                      [] -> fatal ("Rule has not been numbered: "<>name r)
                      [nr] -> theNr nr 
                      _ -> fatal ("Rule has been numbered multiple times: "<>name r)
    where ns = concatMap rulesOfTheme (orderingByTheme fSpec)
          isTheOne :: Numbered RuleCont -> Bool
          isTheOne = (r ==) . cRul . theLoad
instance NumberedThing Relation where
  numberOf fSpec d = case filter isTheOne ns of
                      [] -> fatal ("Relation has not been numbered: "<>showRel d)
                      [nr] -> theNr nr 
                      _ -> fatal ("Relation has been numbered multiple times: "<>showRel d)
    where ns = concatMap dclsOfTheme (orderingByTheme fSpec)
          isTheOne :: Numbered DeclCont -> Bool
          isTheOne = (d ==) . cDcl . theLoad
instance NumberedThing A_Concept where
  numberOf fSpec c = case filter isTheOne ns of
                      [] -> fatal ("Concept has not been numbered: "<>name c)
                      [nr] -> theNr nr 
                      _ -> fatal ("Concept has been numbered multiple times: "<>name c)
    where ns = concatMap cptsOfTheme (orderingByTheme fSpec)
          isTheOne :: Numbered CptCont -> Bool
          isTheOne = (c ==) . cCpt . theLoad
-}

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
                     , cRulMeanings :: [Meaning]
                     }
data DeclCont = CDcl { cDcl ::  Relation
                     , cDclPurps :: [Purpose]
                     , cDclMeanings :: [Meaning]
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
orderingByTheme :: (HasOutputLanguage env) => env -> FSpec -> [ThemeContent]
orderingByTheme env fSpec
 = f ( Counter 1 1 1 --the initial numbers of the countes
     , (sortWithOrigins . filter rulMustBeShown . Set.elems . fallRules)  fSpec
     , (sortWithOrigins . filter relMustBeShown . Set.elems . relsDefdIn) fSpec 
     , (L.sortBy conceptOrder . filter cptMustBeShown . Set.elems . concs)  fSpec
     ) $
     [Just pat | pat <- vpatterns fSpec -- The patterns that should be taken into account for this ordering
     ]<>[Nothing] --Make sure the last is Nothing, to take all res stuff.
 where
  conceptOrder :: A_Concept -> A_Concept -> Ordering
  conceptOrder a b =
  -- The sorting of Concepts is done by the origin of its first definition if there is one.
  -- Concepts without definition are placed last, and sorted by name.
   case (originOfFirstCDef a, originOfFirstCDef b) of
     (Just origA, Just origB) -> case maybeOrdering origA origB of
                                   Just ord -> ord
                                   Nothing -> case (isFuzzyOrigin origA,isFuzzyOrigin origB) of
                                                (False,False) -> fatal "This should be impossible"
                                                (False,True)  -> LT
                                                (True,False)  -> GT
                                                (True,True)   -> comparing name a b 
     (Just _    , Nothing   ) -> LT
     (Nothing   , Just _    ) -> GT
     (Nothing   , Nothing   ) -> comparing name a b
  originOfFirstCDef :: A_Concept -> Maybe Origin
  originOfFirstCDef cpt
    = case sortWithOrigins $ concDefs fSpec cpt of
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
           , cRulPurps = purposesDefinedIn fSpec (outputLang env fSpec) rul
           , cRulMeanings = meanings rul
           }
  dcl2dclCont :: Relation -> DeclCont
  dcl2dclCont dcl
    = CDcl { cDcl      = dcl
           , cDclPurps = purposesDefinedIn fSpec (outputLang env fSpec) dcl
           , cDclMeanings = meanings dcl
           , cDclPairs = pairsInExpr fSpec (EDcD dcl)
           }

  cpt2cptCont :: A_Concept -> CptCont
  cpt2cptCont cpt
    = CCpt { cCpt      = cpt
           , cCptDefs  = sortWithOrigins $ concDefs fSpec cpt
           , cCptPurps = purposesDefinedIn fSpec (outputLang env fSpec) cpt
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
       (thmRuls,restRuls) = L.partition (inThisTheme rulesInTheme) ruls
          where rulesInTheme p = Set.filter ( \r -> Just (name p) == rrpat r) (fallRules fSpec)
       (themeDcls,restDcls) = L.partition (inThisTheme relsInTheme) rels
          where relsInTheme p = relsDefdIn p `Set.union` bindedRelationsIn p
       (themeCpts,restCpts) = L.partition (inThisTheme concs) cpts
       inThisTheme :: Eq a => (Pattern -> Set.Set a) -> a -> Bool
       inThisTheme allElemsOf x
         = case mPat of
             Nothing  -> True
             Just pat -> x `elem` allElemsOf pat

--GMI: What's the meaning of the Int? HJO: This has to do with the numbering of rules
dpRule' :: (HasDocumentOpts env) => 
    env -> FSpec -> [Rule] -> Int -> A_Concepts -> Relations
          -> ([(Inlines, [Blocks])], Int, A_Concepts, Relations)
dpRule' env fSpec = dpR
 where
   l lstr = text $ localize (outputLang env fSpec) lstr
   dpR [] n seenConcs seenRelations = ([], n, seenConcs, seenRelations)
   dpR (r:rs) n seenConcs seenRelations
     = ( ( l (NL "Regel: ",EN "Rule: ") <> (text.name) r
         , [theBlocks]
          ): dpNext
       , n'
       , seenCs
       , seenDs
       )
       where
        theBlocks :: Blocks
        theBlocks =
            purposes2Blocks env (purposesDefinedIn fSpec (outputLang env fSpec) r) -- Als eerste de uitleg van de betreffende regel..
         <> purposes2Blocks env [p | d<-Set.elems nds, p<-purposesDefinedIn fSpec (outputLang env fSpec) d]  -- Dan de uitleg van de betreffende relaties
         <> case (Set.elems . Set.map EDcD $ nds, outputLang env fSpec) of
             ([] ,_)       -> mempty
             ([d],Dutch)   -> plain ("Om dit te formaliseren is een " <> (if isFunction d then "functie"  else "relatie" ) <> " nodig:")
             ([d],English) -> plain ("In order to formalize this, a " <> (if isFunction d then "function" else "relation") <> " is introduced:")
             (_  ,Dutch)   -> plain ("Om te komen tot de formalisatie van " <> hyperLinkTo (XRefSharedLangRule r)
                                    <>  " (" <> (singleQuoted.str.name) r  <> ") "
                                    <> str (" zijn de volgende "<>count Dutch (length nds) "in deze paragraaf geformaliseerde relatie"<>" nodig."))
             (_  ,English) -> plain ("To arrive at the formalization of "   <> hyperLinkTo (XRefSharedLangRule r) <> str (", the following "<>count English (length nds) "relation"<>" are introduced."))
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
                                      <> hyperLinkTo (XRefSharedLangRule r)
                                      <> l (NL " te formaliseren: ", EN ": ")
                                     ) 
                       _    -> plain (   l (NL " Om ", EN " To formalize ")
                                      <> hyperLinkTo (XRefSharedLangRule r)
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
         <> pandocEquationWithLabel env fSpec (XRefConceptualAnalysisExpression r) (showMath r)
         <> (if length nds<=1
             then mempty
             else plain (  l (NL "Dit komt overeen met ", EN "This corresponds to ")
                        <> hyperLinkTo (XRefSharedLangRule r)
                        <> " (" <> (singleQuoted.str.name) r  <> ")."

                        )
            )
        showRef :: Relation -> Inlines
        showRef dcl = hyperLinkTo (XRefConceptualAnalysisRelation dcl) <> "(" <> (str . showRel) dcl <> ")"
        
        ncs = concs r Set.\\ seenConcs            -- newly seen concepts
        cds = [(c,cd) | c<-Set.elems ncs, cd<-conceptDefs fSpec, cdcpt cd==name c]    -- ... and their definitions
        ds  = bindedRelationsIn r
        nds = ds Set.\\ seenRelations     -- newly seen relations
        rds = ds `Set.intersection` seenRelations  -- previously seen relations
        ( dpNext, n', seenCs,  seenDs ) = dpR rs (n+length cds+length nds+1) (ncs `Set.union` seenConcs) (nds `Set.union` seenRelations)

printMeaning :: HasMeaning a => Lang -> a -> Blocks
printMeaning lang = fromMaybe mempty . fmap (printMarkup . ameaMrk) . meaning lang

printPurposes :: [Purpose] -> Blocks
printPurposes = mconcat . map (printMarkup . explMarkup)

printMarkup :: Markup -> Blocks
printMarkup = fromList . amPandoc

purposes2Blocks :: (HasDocumentOpts env) => env -> [Purpose] -> Blocks
purposes2Blocks env ps
 = case ps of
      [] -> mempty
            -- by putting the ref after the first inline of the definition, it aligns nicely with the definition
      _  -> case concatMarkup [expl{amPandoc = insertAfterFirstInline (ref purp) $ amPandoc expl} | purp<-ps, let expl=explMarkup purp] of
             Nothing -> mempty
             Just p  -> fromList $ amPandoc p
       where   -- The reference information, if available for this purpose, is put
        ref :: Purpose -> [Inline]
        ref purp = if view fspecFormatL env `elem` [Fpdf, Flatex] && (not.null.explRefIds) purp
                   then [RawInline (Text.Pandoc.Builder.Format "latex")
                            (texOnlyMarginNote (T.intercalate "; " (explRefIds purp)<>"\n"))]
                   else []
concatMarkup :: [Markup] -> Maybe Markup
concatMarkup es
 = case eqCl amLang es of
    []   -> Nothing
    [cl] -> Just Markup { amLang   = amLang (NE.head cl)
                        , amPandoc = concatMap amPandoc es
                        }
    cls  -> fatal ("don't call concatMarkup with different languages and formats\n   "<>
                   T.intercalate "\n   " (map (tshow . amLang . NE.head) cls)
                  )

-- Insert an inline after the first inline in the list of blocks, if possible.
insertAfterFirstInline :: [Inline] -> [Block] -> [Block]
insertAfterFirstInline inlines (            Plain (inl:inls):pblocks)        =             Plain (inl : (inlines<>inls)) : pblocks
insertAfterFirstInline inlines (            Para (inl:inls):pblocks)         =             Para (inl : (inlines<>inls)) : pblocks
insertAfterFirstInline inlines (BlockQuote (Para (inl:inls):pblocks):blocks) = BlockQuote (Para (inl : (inlines<>inls)) : pblocks):blocks
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

plainText :: Text -> Blocks
plainText = plain . text

violation2Inlines :: (HasOutputLanguage env) => env -> FSpec -> PairView Expression -> Inlines
violation2Inlines env fSpec _ = (text.l) (NL "<meldingstekst moet hier nog worden gegenereerd>"
                                        ,EN "<violation message should be printed here>"
                                        )
  where
    l = localize (outputLang env fSpec)
