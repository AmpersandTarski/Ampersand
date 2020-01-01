{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.PopulationOptsParser 
   (populationOptsParser)
where

import           Ampersand.Basics
import           Ampersand.Misc.HasClasses (PopulationOpts (..),PopulationOutputFormat(..))
import           Ampersand.Options.FSpecGenOptsParser
import           Options.Applicative
import           RIO.Char
import qualified RIO.List as L

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
outputFormatP = toFormat <$> strOption
      (  long "outputFormat"
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
      toFormat :: String -> PopulationOutputFormat
      toFormat s = case filter matches allformats of
            -- FIXME: The fatals here should be plain parse errors. Not sure yet how that should be done.
            --        See https://hackage.haskell.org/package/optparse-applicative
                   [] -> fatal $ unlines
                        ["No matching recipe found. Possible recipes are:"
                        , "  "<>L.intercalate ", " (map show allformats)
                        , "  You specified: `"<>s<>"`"
                        ]
                   [f] -> f
                   xs -> fatal $ unlines 
                        [ "Ambiguous recipe specified. Possible matches are:"
                        , "  "<>L.intercalate ", " (map show xs)
                        ]
            where
              matches :: (Show a) => a -> Bool
              matches x = map toLower s `L.isPrefixOf` (map toLower $ show x)

