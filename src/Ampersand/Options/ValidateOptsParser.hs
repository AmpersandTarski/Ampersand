{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.ValidateOptsParser 
   (validateOptsParser)
where

import           Options.Applicative
import           Ampersand.Misc.HasClasses (ValidateOpts (..))
import           Ampersand.Options.ProtoOptsParser

-- | Command-line parser for ProofOpts.
validateOptsParser :: Parser ValidateOpts
validateOptsParser= 
     ( \fSpecGenOpts -> ValidateOpts
       { protoOpts = fSpecGenOpts
       }
     ) <$> protoOptsParser


