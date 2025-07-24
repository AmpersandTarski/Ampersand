{-# LANGUAGE ScopedTypeVariables #-}

module Ampersand.Output.ToPandoc.SharedAmongChapters
  ( module Text.Pandoc.Builder,
    module Text.Pandoc,
    module Ampersand.ADL1,
    module Ampersand.Basics,
    module Ampersand.Core.ShowAStruct,
    module Ampersand.FSpec,
    module Ampersand.Graphic.Graphics,
    module Ampersand.Misc.HasClasses,
    module Ampersand.Output.PandocAux,
    module Ampersand.Classes,
    Chapter (..),
    chaptersInDoc,
    Xreferenceable (..),
    CustomSection (..),
    pandocEqnArray,
    pandocEquationWithLabel,
    Purpose (..),
    printMeaning,
    printMarkup,
    printPurposes,
    purposes2Blocks,
    meaning2Blocks,
    violation2Inlines,
    lclForLang,
    dpRule',
    ThemeContent (..),
    orderingByTheme,
    Numbered (..),
    RuleCont (..),
    DeclCont (..),
    CptCont (..),
    plainText,
    showPredLogic,
    legacyTable,
    printConcept,
  )
where

import Ampersand.ADL1 hiding (MetaData)
import Ampersand.Basics hiding (Identity, Reader, link)
import Ampersand.Classes
import Ampersand.Core.ShowAStruct
import Ampersand.FSpec
import Ampersand.Graphic.Graphics
import Ampersand.Misc.HasClasses
import Ampersand.Output.PandocAux
import Ampersand.Output.PredLogic
import Data.Typeable (typeOf)
import RIO.FilePath ((</>))
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import RIO.Time
import Text.Pandoc hiding (Verbosity, getVerbosity, trace)
import Text.Pandoc.Builder hiding (caption, toList)

-- | Define the order of the chapters in the document.
chaptersInDoc :: (HasDocumentOpts env) => env -> [Chapter]
chaptersInDoc = view chaptersL

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
class (Typeable a) => Xreferenceable a where
  xDefBlck :: (HasDirOutput env, HasDocumentOpts env) => env -> FSpec -> a -> Blocks
  xDefBlck _ _ a = fatal ("A " <> tshow (typeOf a) <> " cannot be labeled in <Blocks>.") -- you should use xDefInln instead.

  -- ^ function that defines the target Blocks of something that can be referenced.

  xDefInln :: (HasOutputLanguage env) => env -> FSpec -> a -> Inlines
  xDefInln _ _ a = fatal ("A " <> tshow (typeOf a) <> " cannot be labeled in an <Inlines>.") -- you should use xDefBlck instead.

  -- ^ function that defines the target Inlines of something that can be referenced.

  hyperLinkTo :: a -> Inlines
  -- ^ function that returns a link to something that can be referenced.

  xSafeLabel :: a -> Text -- The full string that is used as ID for referencing
  {-# MINIMAL xSafeLabel, hyperLinkTo, (xDefBlck | xDefInln) #-}

instance Xreferenceable Chapter where
  xSafeLabel a = tshow Sec <> tshow a
  hyperLinkTo = codeGen'
  xDefBlck env fSpec a = headerWith (xSafeLabel a, [], []) 1 (chptTitle (outputLang env fSpec) a)

instance Xreferenceable Picture where
  xSafeLabel a = tshow Fig <> caption a
  hyperLinkTo = codeGen'
  xDefBlck env _ a = para $ imageWith (xSafeLabel a, [], []) (T.pack src) (xSafeLabel a) (text (caption a))
    where
      dirOutput = view dirOutputL env
      src = dirOutput </> imagePathRelativeToDirOutput env a

instance Xreferenceable CustomSection where
  xSafeLabel a =
    (tshow . xrefPrefix . refStuff $ a)
      <> tshow (chapterOfSection x)
      <> typeOfSection x
      <> "-"
      <> (tshow . hash . identOfThing $ x) -- Hash, to make sure there are no fancy characters.
    where
      x = refStuff a
  hyperLinkTo = codeGen'
  xDefBlck env fSpec a = either id (fatal ("You should use xDefInln for:\n  " <> tshow (refStuff a))) (hyperTarget env fSpec a)
  xDefInln env fSpec a = either (fatal ("You should use xDefBlck for:\n  " <> tshow (refStuff a))) id (hyperTarget env fSpec a)

hyperTarget :: (HasOutputLanguage env) => env -> FSpec -> CustomSection -> Either Blocks Inlines
hyperTarget env fSpec a =
  case a of
    XRefConceptualAnalysisPattern {} -> Left . hdr $ (text . l) (NL "Thema: ", EN "Theme: ") <> (singleQuoted . str . tshow . mkId . refStuff $ a)
    XRefSharedLangTheme (Just pat) -> (Left . hdr . text . label) pat
    XRefSharedLangTheme Nothing -> (Left . hdr . text . l) (NL "Overig", EN "Remaining")
    XRefSharedLangRelation d -> Right $ spanWith (xSafeLabel a, [], []) (str . tshow $ d)
    --   Left $ divWith (xSafeLabel a,[],[])
    --                  (   (para . str $ tshow d)
    --                    <>codeBlockWith
    --                         ("", ["adl"],[("caption",tshow d)])
    --                         ( "Deze RELATIE moet nog verder worden uitgewerkt in de Haskell code")
    --                  )
    XRefSharedLangRule r -> (Right . spanWith (xSafeLabel a, [], []) . str . label) r
    --   Left $ divWith (xSafeLabel a,[],[])
    --                  (   (para . text $ tshow r)
    --                  --  <>codeBlockWith
    --                  --       ("", ["adl"],[("caption",tshow r)])
    --                  --       ( "Deze REGEL moet nog verder worden uitgewerkt in de Haskell code")
    --                    <>printMeaning (outputLang env fSpec) r
    --                  )
    XRefConceptualAnalysisRelation _d ->
      Right
        $ spanWith
          (xSafeLabel a, [], [])
          ( (text . l) (NL "Relatie ", EN "Relation ")
          --   <> (str . show . numberOf fSpec $ d)
          )
    XRefConceptualAnalysisRule _r ->
      Right
        $ spanWith
          (xSafeLabel a, [], [])
          ( (text . l) (NL "Regel ", EN "Rule ")
          --   <> (str . show . numberOf fSpec $ r)
          )
    XRefConceptualAnalysisExpression _r ->
      Right
        $ spanWith
          (xSafeLabel a, [], [])
          ( (text . l) (NL "Regel ", EN "Rule ")
          --   <> (str . show . numberOf fSpec $ r)
          )
    _ -> fatal ("hyperTarget not yet defined for " <> tshow (refStuff a))
  where
    hdr = headerWith (xSafeLabel a, [], []) 2
    -- shorthand for easy localizing
    l :: LocalizedStr -> Text
    l = localize (outputLang env fSpec)

codeGen' :: (Xreferenceable a) => a -> Inlines
codeGen' a =
  cite
    [ Citation
        { citationId = xSafeLabel a,
          citationPrefix = [Space],
          citationSuffix = [Space],
          citationHash = 0,
          citationNoteNum = 0,
          citationMode = NormalCitation
        }
    ]
    $ code (xSafeLabel a)

pandocEqnArray :: [Inlines] -> Blocks
pandocEqnArray [] = mempty
pandocEqnArray xs =
  orderedList (map para xs)

data CrossrefType = Dfn | Agr | Eq | Sec | Tbl | Fig -- Special prefixes that make pandoc-crossref work.

instance Show CrossrefType where
  show x = case x of
    Dfn -> "lst:"
    Agr -> "agr:"
    Eq -> "eq:"
    Sec -> "sec:"
    Tbl -> "tbl:"
    Fig -> "fig:"

pandocEquationWithLabel :: (HasOutputLanguage env) => env -> FSpec -> CustomSection -> Inlines -> Blocks
pandocEquationWithLabel env fSpec xref x =
  para (strong (xDefInln env fSpec xref) <> x)

data RefStuff = RefStuff
  { typeOfSection :: Text,
    chapterOfSection :: Chapter,
    identOfThing :: Ident,
    xrefPrefix :: CrossrefType
  }
  deriving (Show)

class Identifyble a where
  mkId :: a -> Ident

instance Identifyble Relation where
  mkId rel = IdentRel (name rel) (name . source $ rel) (name . target $ rel)

instance Identifyble AConceptDef where
  mkId = IdentByName . name

instance Identifyble Rule where
  mkId = IdentByName . name

instance Identifyble Pattern where
  mkId = IdentByName . name

instance Identifyble RefStuff where
  mkId = identOfThing

data Ident
  = IdentByName Name
  | IdentRel Name Name Name
  | IdentOverig -- Used to print the
  deriving (Eq)

instance Hashable Ident where
  hashWithSalt s ident = case ident of
    IdentByName nm ->
      s `hashWithSalt` nm
    IdentRel n1 n2 n3 ->
      s
        `hashWithSalt` n1
        `hashWithSalt` n2
        `hashWithSalt` n3
    IdentOverig -> s `hashWithSalt` tshow ident

instance Show Ident where
  show ident = T.unpack $ case ident of
    IdentByName nm -> fullName nm
    IdentRel nm src tgt ->
      fullName nm
        <> "["
        <> ( if src == tgt
               then fullName src
               else fullName src <> "*" <> fullName tgt
           )
        <> "]"
    IdentOverig -> ":overig"

refStuff :: CustomSection -> RefStuff
refStuff x =
  case x of
    XRefSharedLangRelation d ->
      RefStuff
        { typeOfSection = relation,
          chapterOfSection = SharedLang,
          identOfThing = mkId d,
          xrefPrefix = Dfn
        }
    XRefDataAnalysisRule r ->
      RefStuff
        { typeOfSection = rule,
          chapterOfSection = DataAnalysis,
          identOfThing = mkId r,
          xrefPrefix = Agr
        }
    XRefSharedLangRule r ->
      RefStuff
        { typeOfSection = rule,
          chapterOfSection = SharedLang,
          identOfThing = mkId r,
          xrefPrefix = Agr
        }
    XRefConceptualAnalysisPattern p ->
      RefStuff
        { typeOfSection = pattern',
          chapterOfSection = ConceptualAnalysis,
          identOfThing = mkId p,
          xrefPrefix = Sec
        }
    XRefConceptualAnalysisRelation d ->
      RefStuff
        { typeOfSection = relation,
          chapterOfSection = ConceptualAnalysis,
          identOfThing = mkId d,
          xrefPrefix = Eq
        }
    XRefConceptualAnalysisRule r ->
      RefStuff
        { typeOfSection = rule,
          chapterOfSection = ConceptualAnalysis,
          identOfThing = mkId r,
          xrefPrefix = Eq
        }
    XRefConceptualAnalysisExpression r ->
      RefStuff
        { typeOfSection = term,
          chapterOfSection = ConceptualAnalysis,
          identOfThing = mkId r,
          xrefPrefix = Eq
        }
    XRefSharedLangTheme mt ->
      RefStuff
        { typeOfSection = theme,
          chapterOfSection = SharedLang,
          identOfThing = maybe IdentOverig mkId mt,
          xrefPrefix = Sec
        }
  where
    (relation, rule, term, pattern', theme) =
      ("relation", "rule", "term", "pattern", "theme")

data ThemeContent = Thm
  { themeNr :: Int,
    patOfTheme :: Maybe Pattern, -- A theme is about either a pattern or about everything outside patterns
    rulesOfTheme :: [Numbered RuleCont], -- The (numbered) rules of that theme
    idRulesOfTheme :: [Numbered RuleCont], -- The (numbered) identity rules of that theme.
    dclsOfTheme :: [Numbered DeclCont], -- The (numbered) relations that are used in a rule of this theme, but not in any rule of a previous theme.
    cptsOfTheme :: [Numbered CptCont] -- The (numbered) concepts that are used in a rule of this theme, but not in any rule of a previous theme.
  }

data Numbered t = Nr
  { theNr :: Int,
    theLoad :: t
  }

instance (Named t) => Named (Numbered t) where
  name = name . theLoad

data RuleCont = CRul
  { cRul :: Rule,
    cRulPurps :: [Purpose],
    cRulMeanings :: [Meaning]
  }

data DeclCont = CDcl
  { cDcl :: Relation,
    cDclPurps :: [Purpose],
    cDclMeanings :: [Meaning],
    cDclPairs :: AAtomPairs
  }

data CptCont = CCpt
  { cCpt :: A_Concept,
    cCptDefs :: [AConceptDef],
    cCptPurps :: [Purpose]
  }

instance Named RuleCont where
  name = name . cRul

instance Named DeclCont where
  name = name . cDcl

instance Named CptCont where
  name = name . cCpt

instance Named ThemeContent where
  name tc =
    maybe
      ( case try2Name PatternName "Outside_of_patterns" of
          Left msg -> fatal $ "ThemeContent.name: " <> msg
          Right (nm, _) -> nm
      )
      name
      (patOfTheme tc)

-- | orderingByTheme collects materials from the fSpec to distribute over themes.
--   It ensures that all rules, relations and concepts from the context are included in the specification.
--   The principle is that every rule, relation, or concept that is defined in a pattern is documented in the corresponding theme.
--   Everything that is defined outside themes is documented in the last theme.
--   As a consequence, something that is declared in several patterns occurs in the corresponding themes and may be seen as a double occurrence.
--   However, that may be the intention of the Ampersand modeler.
--   The story: materials from the patterns are gathered in ruless, conceptss, and relationss.
--   Numbering of each item is done recursively by `numbered`, while keeping the structure intact.
--   Finally, the theme content is constructed.
orderingByTheme :: (HasOutputLanguage env) => env -> FSpec -> [ThemeContent]
orderingByTheme env fSpec =
  [ Thm
      { themeNr = i,
        patOfTheme = Just pat,
        rulesOfTheme = fmap rul2rulCont nrules,
        idRulesOfTheme = fmap idrul2rulCont nidrules,
        dclsOfTheme = fmap dcl2dclCont nrelations,
        cptsOfTheme = fmap cpt2cptCont nconcepts
      }
    | (pat, i, nrules, nidrules, nrelations, nconcepts) <- L.zip6 (vpatterns fSpec) [0 ..] (NE.init nruless) (NE.init nidruless) (NE.init nrelationss) (NE.init nconceptss)
  ]
    <> [ Thm
           { themeNr = length (instanceList fSpec :: [Pattern]),
             patOfTheme = Nothing,
             rulesOfTheme = fmap rul2rulCont (NE.last nruless),
             idRulesOfTheme = fmap idrul2rulCont (NE.last nidruless),
             dclsOfTheme = fmap dcl2dclCont (NE.last nrelationss),
             cptsOfTheme = fmap cpt2cptCont (NE.last nconceptss)
           }
       ]
  where
    nruless :: NonEmpty [Numbered Rule]
    nconceptss :: NonEmpty [Numbered AConceptDef]
    nrelationss :: NonEmpty [Numbered Relation]
    nruless = transformNonEmpty (numbering 0 (map Set.toList ruless <> [Set.toList (maybe mempty ctxrs aCtx)]))
    nidruless = transformNonEmpty (numbering 0 idruless)
    nconceptss = transformNonEmpty (numbering 0 (conceptss <> maybe mempty (Set.toList . Set.singleton . ctxcdsOutPats) aCtx))
    nrelationss = transformNonEmpty (numbering 0 (map Set.toList relationss <> [Set.toList (maybe mempty ctxds aCtx)]))
    transformNonEmpty :: [a] -> NonEmpty a
    transformNonEmpty x = case NE.nonEmpty x of Just ne -> ne; Nothing -> fatal "onbereikbare code"
    aCtx = originalContext fSpec
    ruless :: [Rules]
    idruless :: [[IdentityRule]]
    conceptss :: [[AConceptDef]]
    relationss :: [Relations]
    (ruless, idruless, conceptss, relationss) =
      L.unzip4
        ( [(ptrls pat, ptids pat, ptcds pat, ptdcs pat) | pat <- vpatterns fSpec]
            <> case originalContext fSpec of
              Nothing -> []
              Just ctx -> [(ctxrs ctx, ctxks ctx, ctxcds ctx, ctxds ctx)]
        )
    numbering :: Int -> [[a]] -> [[Numbered a]]
    numbering n (xs : xss) = [Nr i x | (x, i) <- zip xs [n ..]] : numbering (n + length xs) xss
    numbering _ _ = []

    rul2rulCont :: Numbered Rule -> Numbered RuleCont
    rul2rulCont (Nr n rul) =
      Nr
        n
        CRul
          { cRul = rul,
            cRulPurps = purposesOf fSpec (outputLang env fSpec) rul,
            cRulMeanings = meanings rul
          }

    idrul2rulCont :: Numbered IdentityRule -> Numbered RuleCont
    idrul2rulCont (Nr n rul) =
      Nr
        n
        CRul
          { cRul = ruleFromIdentity rul,
            cRulPurps = purposesOf fSpec (outputLang env fSpec) rul,
            cRulMeanings = mempty -- Identity rules have a fixed meaning, so there are no meaning fields
          }

    dcl2dclCont :: Numbered Relation -> Numbered DeclCont
    dcl2dclCont (Nr n dcl) =
      Nr
        n
        CDcl
          { cDcl = dcl,
            cDclPurps = purposesOf fSpec (outputLang env fSpec) dcl,
            cDclMeanings = meanings dcl,
            cDclPairs = pairsInExpr fSpec (EDcD dcl)
          }

    cpt2cptCont :: Numbered AConceptDef -> Numbered CptCont
    cpt2cptCont (Nr n cd) =
      Nr
        n
        CCpt
          { cCpt = c,
            cCptDefs = [cd],
            cCptPurps = purposesOf fSpec (outputLang env fSpec) c
          }
      where
        c = acdcpt cd

dpRule' ::
  (HasDocumentOpts env) =>
  env ->
  FSpec ->
  [Rule] ->
  Int ->
  A_Concepts ->
  Relations ->
  ([(Inlines, [Blocks])], Int, A_Concepts, Relations)
dpRule' env fSpec = dpR
  where
    l lstr = text $ localize (outputLang env fSpec) lstr
    dpR [] n seenConcs seenRelations = ([], n, seenConcs, seenRelations)
    dpR (r : rs) n seenConcs seenRelations =
      ( ( l (NL "Regel: ", EN "Rule: ") <> (text . tshow . mkId) r,
          [theBlocks]
        )
          : dpNext,
        n',
        seenCs,
        seenDs
      )
      where
        theBlocks :: Blocks
        theBlocks =
          purposes2Blocks env (purposesOf fSpec (outputLang env fSpec) r) -- Als eerste de uitleg van de betreffende regel..
            <> purposes2Blocks env [p | d <- toList nds, p <- purposesOf fSpec (outputLang env fSpec) d] -- Dan de uitleg van de betreffende relaties
            <> case (toList . Set.map EDcD $ nds, outputLang env fSpec) of
              ([], _) -> mempty
              ([d], Dutch) -> plain ("Om dit te formaliseren is een " <> (if isMapping d then "functie" else "relatie") <> " nodig:")
              ([d], English) -> plain ("In order to formalize this, a " <> (if isMapping d then "function" else "relation") <> " is introduced:")
              (_, Dutch) ->
                plain
                  ( "Om te komen tot de formalisatie van "
                      <> hyperLinkTo (XRefSharedLangRule r)
                      <> " ("
                      <> (singleQuoted . str . tshow . mkId) r
                      <> ") "
                      <> str (" zijn de volgende " <> count Dutch (length nds) "in deze paragraaf geformaliseerde relatie" <> " nodig.")
                  )
              (_, English) -> plain ("To arrive at the formalization of " <> hyperLinkTo (XRefSharedLangRule r) <> str (", the following " <> count English (length nds) "relation" <> " are introduced."))
            <> (bulletList . map (plain . showRef) . toList $ nds)
            <> ( if null nds
                   then case toList rds of
                     [] -> mempty
                     [rd] ->
                       plain
                         ( l
                             ( NL "Om dit te formalizeren maken we gebruik van relatie ",
                               EN "We use relation "
                             )
                             <> showRef rd
                             <> l (NL ".", EN " to formalize this.")
                         )
                     _ ->
                       plain
                         ( l
                             ( NL "Dit formaliseren we door gebruik te maken van de volgende relaties: ",
                               EN "We formalize this using relations "
                             )
                         )
                         <> (bulletList . map (plain . showRef) . toList $ rds)
                   else case toList rds of
                     [] -> mempty
                     [rd] ->
                       plain
                         ( l (NL "Daarnaast gebruiken we relatie ", EN "Beside that, we use relation ")
                             <> showRef rd
                             <> l (NL " om ", EN " to formalize ")
                             <> hyperLinkTo (XRefSharedLangRule r)
                             <> l (NL " te formaliseren: ", EN ": ")
                         )
                     _ ->
                       plain
                         ( l (NL " Om ", EN " To formalize ")
                             <> hyperLinkTo (XRefSharedLangRule r)
                             <> l
                               ( NL " te formaliseren, gebruiken we daarnaast ook de relaties: ",
                                 EN " we also use relations "
                               )
                         )
                         <> (bulletList . fmap (plain . showRef) . toList $ rds)
               )
            <> plain
              ( if isSignal fSpec r
                  then
                    l
                      ( NL "Activiteiten, die door deze regel zijn gedefinieerd, zijn afgerond zodra: ",
                        EN "Activities that are defined by this rule are finished when: "
                      )
                  else l (NL "De regel luidt: ", EN "This means: ")
              )
            <> pandocEquationWithLabel env fSpec (XRefConceptualAnalysisExpression r) (showMath r)
            <> ( if length nds <= 1
                   then mempty
                   else
                     plain
                       ( l (NL "Dit komt overeen met ", EN "This corresponds to ")
                           <> hyperLinkTo (XRefSharedLangRule r)
                           <> " ("
                           <> (singleQuoted . str . tshow . mkId) r
                           <> ")."
                       )
               )
        showRef :: Relation -> Inlines
        showRef dcl = hyperLinkTo (XRefConceptualAnalysisRelation dcl) <> "(" <> (str . tshow) dcl <> ")"

        ncs = concs r Set.\\ seenConcs -- newly seen concepts
        cds = [(c, cd) | c <- toList ncs, cd <- conceptDefs fSpec, name cd == name c] -- ... and their definitions
        ds = bindedRelationsIn r
        nds = ds Set.\\ seenRelations -- newly seen relations
        rds = ds `Set.intersection` seenRelations -- previously seen relations
        (dpNext, n', seenCs, seenDs) = dpR rs (n + length cds + length nds + 1) (ncs `Set.union` seenConcs) (nds `Set.union` seenRelations)

printMeaning :: (HasMeaning a) => Lang -> a -> Blocks
printMeaning lang = maybe mempty (printMarkup . ameaMrk) . meaning lang

printPurposes :: [Purpose] -> Blocks
printPurposes = mconcat . map (printMarkup . explMarkup)

printMarkup :: Markup -> Blocks
printMarkup = amPandoc

meaning2Blocks :: Meaning -> Blocks
meaning2Blocks =
  printMarkup . ameaMrk

purposes2Blocks :: (HasDocumentOpts env) => env -> [Purpose] -> Blocks
purposes2Blocks env ps =
  maybe mempty amPandoc (concatMarkup . map markup' $ ps)
  where
    -- The reference information, if available for this purpose, is put
    markup' purp =
      Markup
        { amLang = amLang . explMarkup $ purp,
          amPandoc = insertAfterFirstInline (ref purp) $ amPandoc . explMarkup $ purp
        }
    ref :: Purpose -> [Inline]
    ref purp =
      [ RawInline
          (Text.Pandoc.Builder.Format "latex")
          ( texOnlyMarginNote
              (T.intercalate "; " (explRefIds purp) <> "\n")
          )
        | view fspecFormatL env
            `elem` [Fpdf, Flatex]
            && (not . null . explRefIds) purp
      ]

concatMarkup :: [Markup] -> Maybe Markup
concatMarkup es =
  case eqCl amLang es of
    [] -> Nothing
    [cl] ->
      Just
        Markup
          { amLang = amLang (NE.head cl),
            amPandoc = mconcat (map amPandoc es)
          }
    cls ->
      fatal
        ( "don't call concatMarkup with different languages and formats\n   "
            <> T.intercalate "\n   " (map (tshow . amLang . NE.head) cls)
        )

-- Insert an inline after the first inline in the list of blocks, if possible.
insertAfterFirstInline :: [Inline] -> Blocks -> Blocks
insertAfterFirstInline inlines = fromList . insertAfterFirstInline' . toList
  where
    insertAfterFirstInline' (Plain (inl : inls) : pblocks) = Plain (inl : (inlines <> inls)) : pblocks
    insertAfterFirstInline' (Para (inl : inls) : pblocks) = Para (inl : (inlines <> inls)) : pblocks
    insertAfterFirstInline' (BlockQuote (Para (inl : inls) : pblocks) : blocks) = BlockQuote (Para (inl : (inlines <> inls)) : pblocks) : blocks
    insertAfterFirstInline' blocks = Plain inlines : blocks

lclForLang :: Lang -> TimeLocale
lclForLang lang =
  defaultTimeLocale
    { months =
        case lang of
          Dutch ->
            [ ("januari", "jan"),
              ("februari", "feb"),
              ("maart", "mrt"),
              ("april", "apr"),
              ("mei", "mei"),
              ("juni", "jun"),
              ("juli", "jul"),
              ("augustus", "aug"),
              ("september", "sep"),
              ("oktober", "okt"),
              ("november", "nov"),
              ("december", "dec")
            ]
          English ->
            [ ("January", "Jan"),
              ("February", "Feb"),
              ("March", "Mar"),
              ("April", "Apr"),
              ("May", "May"),
              ("June", "Jun"),
              ("July", "Jul"),
              ("August", "Aug"),
              ("September", "Sep"),
              ("October", "Oct"),
              ("November", "Nov"),
              ("December", "Dec")
            ]
    }

plainText :: Text -> Blocks
plainText = plain . text

violation2Inlines :: (HasOutputLanguage env) => env -> FSpec -> PairView Expression -> Inlines
violation2Inlines env fSpec _ =
  (text . l)
    ( NL "<meldingstekst moet hier nog worden gegenereerd>",
      EN "<violation message should be printed here>"
    )
  where
    l = localize (outputLang env fSpec)

-- Some helper function to cope with changes in Pandoc. In the newer versions of Pandoc,
-- tables have gotten more possibilities. For the time being, we do not use them. Maybe later.
legacyTable ::
  -- | Caption
  Inlines ->
  -- | Column alignments and fractional widths
  [(Alignment, Double)] ->
  -- | Headers
  [Blocks] ->
  -- | Rows
  [[Blocks]] ->
  Blocks
legacyTable caption' cellspecs headers rows =
  table tCaption tColSpec tHead tBodies tFooter
  where
    tCaption :: Caption
    tCaption
      | null caption' = emptyCaption
      | otherwise = Caption (Just . toList $ caption') []
    tColSpec :: [ColSpec]
    tColSpec = map toColSpec cellspecs
      where
        toColSpec :: (Alignment, Double) -> ColSpec
        toColSpec (a, d) = (a, ColWidth d)
    tHead :: TableHead
    tHead = (TableHead nullAttr . toList . singleton . Row nullAttr . map (toCell AlignDefault)) headers
    toCell :: Alignment -> Blocks -> Cell
    toCell a b = Cell nullAttr a (RowSpan 1) (ColSpan 1) (toList b)
    tBodies :: [TableBody]
    tBodies = map toBodyRow rows
      where
        toBodyRow :: [Blocks] -> TableBody
        toBodyRow bs = TableBody nullAttr (RowHeadColumns 0) [] [Row nullAttr $ zipWith toCell (map fst cellspecs) bs]
    tFooter :: TableFoot
    tFooter = TableFoot nullAttr []

-- | This function is used in the conceptual analysis chapter as wel as the natural language chapter. To avoid
--   code duplication, it has been placed in this shared module.
printConcept ::
  (HasDocumentOpts env) =>
  env ->
  (LocalizedStr -> Text) ->
  Numbered CptCont ->
  Blocks
printConcept env l nCpt =
  -- Purposes:
  (printPurposes . cCptPurps . theLoad) nCpt
    <> case (nubByContent . cCptDefs . theLoad) nCpt of
      [] -> mempty -- There is no definition of the concept
      [cd] -> printCDef cd Nothing
      cds ->
        mconcat
          [ printCDef cd (Just $ T.snoc "." suffx)
            | (cd, suffx) <- zip cds ['a' ..] -- There are multiple definitions. Which one is the correct one?
          ]
  where
    fspecFormat = view fspecFormatL env
    nubByContent = L.nubBy (\x y -> fun x == fun y) -- fixes https://github.com/AmpersandTarski/Ampersand/issues/617
      where
        fun = amPandoc . ameaMrk . acddef2
    printCDef ::
      AConceptDef -> -- the definition to print
      Maybe Text -> -- when multiple definitions exist of a single concept, this is to distinguish
      Blocks
    printCDef cDef suffx =
      (blockQuote . definitionList)
        [ ( str (l (NL "Definitie ", EN "Definition "))
              <> ( if fspecFormat `elem` [Fpdf, Flatex]
                     then (str . tshow . theNr) nCpt
                     else (str . tshow . mkId) cDef
                 )
              <> str (fromMaybe "" suffx)
              <> ":",
            [meaning2Blocks (acddef2 cDef)]
          )
        ]
