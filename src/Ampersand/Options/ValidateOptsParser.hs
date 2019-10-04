{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.ValidateOptsParser 
   (validateOptsParser)
where

import           Options.Applicative
import           Ampersand.Misc.HasClasses (ValidateOpts (..))
import           Ampersand.Basics
import           Ampersand.Options.ProtoOptsParser

-- | Command-line parser for ProofOpts.
validateOptsParser :: String -> Parser ValidateOpts
validateOptsParser out = 
     ( \fSpecGenOpts -> ValidateOpts
       { protoOpts = fSpecGenOpts
       }
     ) <$> protoOptsParser out


