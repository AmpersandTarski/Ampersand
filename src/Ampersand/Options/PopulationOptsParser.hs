{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Options.PopulationOptsParser 
   (populationOptsParser)
where

import           Ampersand.Basics
import           Ampersand.Misc.HasClasses (PopulationOpts (..),PopulationOutputFormat(..))
import           Ampersand.Options.FSpecGenOptsParser
import           Options.Applicative
import qualified RIO.Text as T
-- | Command-line parser for ProofOpts.
populationOptsParser :: Parser PopulationOpts
populationOptsParser = 
     ( \fSpecGenOpts outputFormat -> 
       PopulationOpts
       { x5fSpecGenOpts = fSpecGenOpts
       , xoutputFormat = outputFormat
       }
     ) <$> fSpecGenOptsParser False
       <*> outputFormatP

outputFormatP :: Parser PopulationOutputFormat
outputFormatP = toFormat . T.pack <$> strOption
      (  long "output-format"
      <> metavar "FORMAT"
      <> value (show XLSX)
      <> showDefault
      <> completeWith (map show allformats)
      <> (  help $ "The format that Population should be written to. Allowd values are: "
         <> show allformats
         )
      )

  where 
      allformats :: [PopulationOutputFormat]
      allformats = [minBound..]
      toFormat :: Text -> PopulationOutputFormat
      toFormat s = case filter matches allformats of
            -- FIXME: The fatals here should be plain parse errors. Not sure yet how that should be done.
            --        See https://hackage.haskell.org/package/optparse-applicative
                   [] -> fatal $ T.unlines
                        ["No matching recipe found. Possible recipes are:"
                        , "  "<>T.intercalate ", " (map tshow allformats)
                        , "  You specified: `"<>s<>"`"
                        ]
                   [f] -> f
                   xs -> fatal $ T.unlines 
                        [ "Ambiguous recipe specified. Possible matches are:"
                        , "  "<>T.intercalate ", " (map tshow xs)
                        ]
            where
              matches :: PopulationOutputFormat -> Bool
              matches x = T.toLower s `T.isPrefixOf` (T.toLower $ tshow x)

