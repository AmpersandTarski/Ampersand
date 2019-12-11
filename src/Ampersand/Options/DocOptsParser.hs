{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.DocOptsParser 
   (docOptsParser)
where

import           Options.Applicative
import           Ampersand.Misc.HasClasses (DocOpts (..))
import           Ampersand.Basics
import           Ampersand.Misc.HasClasses
import           Ampersand.Options.FSpecGenOptsParser
import           Ampersand.Options.Utils
import           Options.Applicative.Builder.Extra
import           RIO.Char (toLower)
import qualified RIO.List as L


-- | Command-line parser for the proto command.
docOptsParser :: 
     Parser DocOpts
docOptsParser =
      ( \blackWhite chapters genGraphics fspecFormat fSpecGenOpts
        outputLanguage genLegalRefs -> DocOpts
                { xblackWhite = blackWhite
                , xchapters = chapters
                , xgenGraphics = genGraphics
                , xfspecFormat = fspecFormat
                , x3fSpecGenOpts = fSpecGenOpts
                , x3OutputLanguage = outputLanguage
                , xgenLegalRefs = genLegalRefs
                }
      ) <$> blackWhiteP
        <*> chaptersP
        <*> genGraphicsP
        <*> fSpecFormatP
        <*> fSpecGenOptsParser False
        <*> outputLanguageP
        <*> genLegalRefsP

chaptersP :: Parser [Chapter]
chaptersP =  
   (\intro sharedlang diagnosis conceptualanalysis dataanalysis ->
     let x = [intro,sharedlang,diagnosis,conceptualanalysis,dataanalysis]
     in 
     if length x /= length [c::Chapter | c <- [minBound..]] 
     then --To fix this: make sute all chapters are handled in this function.
          fatal "Not all chapters are implemented thru options." 
     else 
       case both (fmap fst) . L.partition isTrue . filter (isJust . snd) $ x of
         ([],[]) -> [minBound..]
         (xs,[]) -> xs -- Only explicit requested chapters
         ([],ys) -> case [minBound..] L.\\ ys of
                      [] -> exitWith $ PosAndNegChaptersSpecified
                        [ "Are you kidding? do you realy want an empty document?"]
                      cs -> cs -- All chapters exept ys
         (xs,ys) -> let otherChapters = ([minBound..] L.\\ xs) L.\\ ys
                    in if null otherChapters
                       then xs
                       else exitWith $ PosAndNegChaptersSpecified 
                        [ "It is unclear what chapters you want in your document."
                        , "  You want: "<> (L.intercalate ", " . map show $ xs)
                        , "  You don't want: "<> (L.intercalate ", " . map show $ ys)
                        , "  What about the other chapters: " <> (L.intercalate ", " . map show $ otherChapters)<>" ?"
                        , "  Please don't mix `--no-<chapter>` with `--<chapter>`."
                        ]
   ) <$> chapterParser Intro
     <*> chapterParser SharedLang
     <*> chapterParser Diagnosis
     <*> chapterParser ConceptualAnalysis
     <*> chapterParser DataAnalysis
  where
    isTrue :: (Chapter,Maybe Bool) -> Bool
    isTrue (_,Just True) = True
    isTrue _             = False
    chapterParser :: Chapter -> Parser (Chapter,Maybe Bool)
    chapterParser chp = (\x -> (chp,x)) 
          <$> enableDisableFlags Nothing (Just True) (Just False)
                (show chp) (" printing of chapter "<>show chp<>".") mods
       where 
         mods = case chp of
           _ -> help $ "Do or do not include chapter "<>show chp<>" in the generated document."
          

fSpecFormatP :: Parser FSpecFormat
fSpecFormatP = toFormat <$> strOption
         (  long "format"
         <> metavar "FORMAT"
         <> completeWith (map stripF allFormats)
         <> help "The format in which the output is written."
         )
   where toFormat :: String -> FSpecFormat
         toFormat s = case filter matches allFormats of
            -- FIXME: The fatals here should be plain parse errors. Not sure yet how that should be done.
            --        See https://hackage.haskell.org/package/optparse-applicative
                   [] -> fatal $ unlines
                        ["No matching formats found. Possible formats are:"
                        , "  "<>L.intercalate ", " (map stripF allFormats)
                        ]
                   [f] -> f
                   xs -> fatal $ unlines 
                        [ "Ambiguous format specified. Possible matches are:"
                        , "  "<>L.intercalate ", " (map stripF xs)
                        ]
            where
              matches :: FSpecFormat -> Bool
              matches fmt = map toLower s `L.isPrefixOf` stripF fmt
         stripF :: FSpecFormat -> String
         stripF fmt = case map toLower . show $ fmt of
                'f':tl -> tl
                xs -> fatal $ "All formats used to start with an 'F': "<>show xs
         allFormats :: [FSpecFormat]
         allFormats = [minBound..]

genGraphicsP :: Parser Bool
genGraphicsP = boolFlags True "graphics"
        ( "generation of graphics before generating the document."
        ) mempty

blackWhiteP :: Parser Bool
blackWhiteP = switch
        ( long "blackWhite"
        <> help ("avoid coloring conventions to facilitate readable pictures in "
                <> "black and white.")
        )

genLegalRefsP :: Parser Bool
genLegalRefsP = boolFlags True "legal-refs"
        ( "generation of a table of legal references in Natural Language chapter of the output document."
        ) mempty


-- | Apply a single function to both components of a pair.
--
-- > both succ (1,2) == (2,3)
both :: (a -> b) -> (a, a) -> (b, b)
both f (x,y) = (f x, f y)
