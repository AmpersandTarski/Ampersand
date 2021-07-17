module Ampersand.Options.ValidateOptsParser (validateOptsParser) where

import Ampersand.Misc.HasClasses (ValidateOpts (..))
import Ampersand.Options.ProtoOptsParser
import Options.Applicative

-- | Command-line parser for ValidateOpts.
validateOptsParser :: Parser ValidateOpts
validateOptsParser =
  ValidateOpts
    <$> protoOptsParser
