{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generate a prototype from a project.
module Ampersand.Commands.Proto
    (proto
    ,ProtoOpts(..)
    ,HasProtoOpts(..)
    ) where

import           Ampersand.Basics
import           Ampersand.Core.AbstractSyntaxTree
import           Ampersand.FSpec
import           Ampersand.Misc.HasClasses
import           Ampersand.Output.FSpec2SQL
import           Ampersand.Output.ToJSON.ToJson
import           Ampersand.Prototype.GenFrontend (doGenFrontend)
import           Ampersand.Types.Config
import qualified RIO.Text as T
import           System.Directory
-- | Builds a prototype of the current project.
--
proto :: (Show env, HasRunner env, HasRunComposer env, HasDirCustomizations env, HasZwolleVersion env, HasProtoOpts env, HasAllowInvariantViolations env, HasDirPrototype env) 
       => FSpec -> RIO env ()
proto fSpec = do
    env <- ask
    let dirPrototype = getDirPrototype env
    allowInvariantViolations <- view allowInvariantViolationsL
    let violatedRules :: [(Rule,AAtomPairs)]
        violatedRules = violationsOfInvariants fSpec
    if null violatedRules || allowInvariantViolations
    then do
       logDebug "Generating prototype..."
       liftIO $ createDirectoryIfMissing True dirPrototype
       doGenFrontend fSpec
       generateDatabaseFile fSpec
       let dir = getGenericsDir env
       generateAllJSONfiles dir fSpec
       dirPrototypeA <- liftIO $ makeAbsolute dirPrototype
       logInfo $ "Prototype files have been written to " <> display (T.pack dirPrototypeA)
    else exitWith $ NoPrototypeBecauseOfRuleViolations (violationMessages violatedRules)

violationMessages :: [(Rule,AAtomPairs)] -> [String]
violationMessages = concatMap violationMessage
  where
    violationMessage :: (Rule,AAtomPairs) -> [String]
    violationMessage (r,ps) = 
      [("There are "<>show (length ps)<>" violation(s) of RULE "<>name r<>":")]
      <> (map ("  "<>) . listPairs 3 . toList $ ps)
    listPairs :: Int -> [AAtomPair] -> [String]
    listPairs i xs = 
                case xs of
                  [] -> []
                  h:tl 
                    | i == 0 -> ["  ..."]
                    | otherwise -> showAP h : listPairs (i-1) tl
        where
          showAP :: AAtomPair -> String
          showAP x= "("<>aavstr (apLeft x)<>", "<>aavstr (apRight x)<>")"
        