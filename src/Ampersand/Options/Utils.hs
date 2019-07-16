{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.Utils where

import           Options.Applicative
import           Ampersand.Basics


-- | If argument is True, hides the option from usage and help
hideMods :: Bool -> Mod f a
hideMods hide = if hide then internal <> hidden else idm

