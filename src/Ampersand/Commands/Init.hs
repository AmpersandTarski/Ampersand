{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generate a configuration file for a new project.
module Ampersand.Commands.Init
    (init
    ,InitOpts(..)
    ,HasInitOpts(..)
    ) where

import           Ampersand.Basics
import           Ampersand.Misc.HasClasses
import           Ampersand.Types.Config
-- | Generate a configuration file for a new project.
--
init :: (HasRunner env) --  , Show env, HasDirCustomizations env, HasZwolleVersion env, HasProtoOpts env, HasAllowInvariantViolations env, HasDirPrototype env, HasRootFile env) 
       => RIO env ()
init = do
    logInfo "Still has to be implemented..."


