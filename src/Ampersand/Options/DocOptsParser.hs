{-# LANGUAGE TupleSections #-}

module Ampersand.Options.DocOptsParser (docOptsParser) where

import Ampersand.Basics
import Ampersand.Misc.HasClasses
import Ampersand.Options.FSpecGenOptsParser
import Ampersand.Options.Utils
import Options.Applicative
import Options.Applicative.Builder.Extra
import qualified RIO.List as L
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified RIO.Text.Partial as Partial

-- | Command-line parser for the document command.
docOptsParser ::
  Parser DocOpts
docOptsParser =
  DocOpts
    <$> blackWhiteP
    <*> chaptersP
    <*> datamodelOnlyP
    <*> genGraphicsP
    <*> (Set.fromList <$> visualsOutputFormatsP)
    <*> (Set.fromList <$> focusOfVisualsP)
    <*> uniEdgesP
    <*> genTextP
    <*> fSpecFormatP
    <*> fSpecGenOptsParser False
    <*> outputLanguageP
    <*> genLegalRefsP
  where
    chaptersP :: Parser [Chapter]
    chaptersP =
      build
        <$> chapterParser Intro
        <*> chapterParser SharedLang
        <*> chapterParser Diagnosis
        <*> chapterParser ConceptualAnalysis
        <*> chapterParser DataAnalysis
      where
        build intro sharedlang diagnosis conceptualanalysis dataanalysis
          | length x /= length [c :: Chapter | c <- [minBound ..]] =
              -- To fix this: make sure all chapters are handled in this function.
              fatal "Not all chapters are implemented thru options."
          | otherwise = case both (fmap fst) . L.partition isTrue . filter (isJust . snd) $ x of
              ([], []) -> [minBound ..]
              (xs, []) -> xs -- Only explicit requested chapters
              ([], ys) -> case [minBound ..] L.\\ ys of
                [] ->
                  exitWith
                    $ PosAndNegChaptersSpecified
                      ["Are you kidding? do you realy want an empty document?"]
                cs -> cs -- All chapters exept ys
              (xs, ys) ->
                let otherChapters = ([minBound ..] L.\\ xs) L.\\ ys
                 in if null otherChapters
                      then xs
                      else
                        exitWith
                          $ PosAndNegChaptersSpecified
                            [ "It is unclear what chapters you want in your document.",
                              "  You want: " <> (T.intercalate ", " . map tshow $ xs),
                              "  You don't want: " <> (T.intercalate ", " . map tshow $ ys),
                              "  What about the other chapters: " <> (T.intercalate ", " . map tshow $ otherChapters) <> " ?",
                              "  Please don't mix `--no-<chapter>` with `--<chapter>`."
                            ]
          where
            x = [intro, sharedlang, diagnosis, conceptualanalysis, dataanalysis]

        isTrue :: (Chapter, Maybe Bool) -> Bool
        isTrue (_, Just True) = True
        isTrue _ = False
        chapterParser :: Chapter -> Parser (Chapter, Maybe Bool)
        chapterParser chp =
          (chp,)
            <$> enableDisableFlags
              Nothing
              (Just True)
              (Just False)
              (show chp)
              (" printing of chapter " <> show chp <> ".")
              mods
          where
            mods = help $ "Do or do not include chapter " <> show chp <> " in the generated document."

    fSpecFormatP :: Parser FSpecFormat
    fSpecFormatP =
      toFormat
        . T.pack
        <$> strOption
          ( long "format"
              <> metavar "FORMAT"
              <> value "docx"
              <> showDefault
              <> completeWith (map (T.unpack . stripF) allFormats)
              <> help "The format in which the output is written."
          )
      where
        toFormat :: Text -> FSpecFormat
        toFormat s = case filter matches allFormats of
          -- FIXME: The fatals here should be plain parse errors. Not sure yet how that should be done.
          --        See https://hackage.haskell.org/package/optparse-applicative
          -- an issue is created on the backlog: https://github.com/AmpersandTarski/Ampersand/issues/1060
          [] ->
            fatal
              $ T.unlines
                [ "No matching formats found. Possible formats are:",
                  "  " <> T.intercalate ", " (map stripF allFormats)
                ]
          [f] -> f
          xs ->
            fatal
              $ T.unlines
                [ "Ambiguous format specified. Possible matches are:",
                  "  " <> T.intercalate ", " (map stripF xs)
                ]
          where
            matches :: FSpecFormat -> Bool
            matches fmt = T.toLower s `T.isPrefixOf` stripF fmt
        stripF :: FSpecFormat -> Text
        stripF fmt = case T.uncons . T.toLower . tshow $ fmt of
          Just ('f', tl) -> tl
          xs -> fatal $ "All formats used to start with an 'F': " <> tshow xs
        allFormats :: [FSpecFormat]
        allFormats = [minBound ..]

    genGraphicsP :: Parser Bool
    genGraphicsP =
      boolFlags
        True
        "graphics"
        "generation of graphics before generating the document."
        mempty
    uniEdgesP :: Parser Bool
    uniEdgesP =
      boolFlags
        True
        "uniEdges"
        ( "drawing of edges for univalent and/or injective relations in the LDM."
            <> " not drawing them can be useful during the development of an ampersand model."
        )
        mempty
    genTextP :: Parser Bool
    genTextP =
      boolFlags
        True
        "text"
        "generate the document file, which contains the text and graphics."
        mempty
    visualsOutputFormatsP :: Parser [VisualsOutputFormat]
    visualsOutputFormatsP =
      option
        parseList'
        ( long "graphicFormats"
            <> metavar "FORMAT1,FORMAT2,..."
            <> value [VCanon, VPng]
            <> showDefault
            <> completeWith (map (T.unpack . tshow) visualFormats)
            <> help
              ( "Comma-separated list of graphic formats to generate."
                  <> "   Possible formats are: "
                  <> (L.intercalate "," . map show $ visualFormats)
                  <> ". Note that there must not be any spaces in the list."
              )
        )
      where
        parseList' :: ReadM [VisualsOutputFormat]
        parseList' =
          eitherReader
            ( \arg -> do
                let parts = Partial.splitOn "," (T.pack arg)
                mapM visualsOutputFormatP parts
            )
        visualsOutputFormatP :: Text -> Either String VisualsOutputFormat
        visualsOutputFormatP s = case filter ((==) (T.toLower s) . T.toLower . tshow) [minBound ..] of
          [x] -> Right x
          _ ->
            Left
              $ "Invalid visual output format: "
              <> T.unpack s
              <> ". Valid formats: "
              <> (L.intercalate "," . map show $ visualFormats)

        visualFormats :: [VisualsOutputFormat]
        visualFormats = [minBound ..]
    focusOfVisualsP :: Parser [FocusOfVisual]
    focusOfVisualsP =
      option
        parseList'
        ( long "focus-of-visuals"
            <> metavar "FOCUS1,FOCUS2,..."
            <> value [VContext, VPattern]
            <> showDefault
            <> completeWith (map (T.unpack . tshow) focuses)
            <> help
              ( "Comma-separated list of things you'd like the graphics to focus on."
                  <> "   Possible choices are: "
                  <> (T.unpack . T.intercalate "," . map tshow $ focuses)
                  <> ". Note that there must not be any spaces in the list."
              )
        )
      where
        parseList' :: ReadM [FocusOfVisual]
        parseList' =
          eitherReader
            ( \arg -> do
                let parts = Partial.splitOn "," (T.pack arg)
                mapM focusOfVisualP parts
            )
        focusOfVisualP :: Text -> Either String FocusOfVisual
        focusOfVisualP s = case filter ((==) (T.toLower s) . T.toLower . tshow) [minBound ..] of
          [x] -> Right x
          _ ->
            Left
              $ "Invalid focus: "
              <> T.unpack s
              <> ". Valid choices are: "
              <> (L.intercalate "," . map show $ focuses)

        focuses :: [FocusOfVisual]
        focuses = [minBound ..]

    blackWhiteP :: Parser Bool
    blackWhiteP =
      boolFlags
        False
        "blackWhite"
        ( "avoid coloring conventions to facilitate readable pictures in "
            <> "black and white."
        )
        mempty
    datamodelOnlyP :: Parser Bool
    datamodelOnlyP =
      boolFlags
        False
        "datamodelOnly"
        "Only generate datamodel images. This implies --no-text"
        mempty
    genLegalRefsP :: Parser Bool
    genLegalRefsP =
      boolFlags
        False
        "legal-refs"
        "generation of a table of legal references in Natural Language chapter of the output document."
        mempty
