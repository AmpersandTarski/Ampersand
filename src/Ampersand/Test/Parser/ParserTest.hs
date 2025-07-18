{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Ampersand.Test.Parser.ParserTest
  ( parseScripts,
    showErrors,
  )
where

import Ampersand.Basics
import Ampersand.Input.ADL1.CtxError (CtxError, Guarded (..))
import Ampersand.Input.Parsing
import Ampersand.Misc.HasClasses
import Ampersand.Options.FSpecGenOptsParser
import Ampersand.Types.Config
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

-- Tries to parse all the given files
parseScripts ::
  (HasRunner env) =>
  [FilePath] ->
  RIO env Bool
parseScripts paths =
  case paths of
    [] -> return True
    h : tl -> do
      let fSpecGenOpts = defFSpecGenOpts (h NE.:| tl)
      parsed <- snd <$> extendWith fSpecGenOpts (parseFilesTransitive (Roots (h NE.:| tl)))
      case parsed of
        Checked _ ws -> do
          logInfo $ "Parsed: " <> display (T.pack h)
          mapM_ logWarn (fmap displayShow ws)
          parseScripts tl
        Errors e -> do
          logError $ "Cannot parse: " <> display (T.pack h)
          showErrors (NE.toList e)
          return False

showErrors :: (HasLogFunc env) => [CtxError] -> RIO env ()
showErrors = mapM_ (logError . displayShow)
