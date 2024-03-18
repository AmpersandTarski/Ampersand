{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Generate a configuration file for a new project.
module Ampersand.Commands.AtlasImport
  ( atlasImport,
    InitOpts (..),
    HasInitOpts (..),
  )
where

import Ampersand.Basics
import Ampersand.Core.ParseTree
import Ampersand.Core.ShowPStruct
import Ampersand.Misc.HasClasses
import Ampersand.Types.Config
import Data.Aeson
import qualified RIO.ByteString.Lazy as B
import qualified RIO.Text as T

-- | Read a file containing the population of an Atlas.
atlasImport ::
  (HasOutputFile env, HasImportFile env, HasRunner env) =>
  RIO env ()
atlasImport = do
  env <- ask
  content <- liftIO $ B.readFile (view importFileL env)
  -- Get JSON data and decode it
  let result = myDecode content
  case result of
    Left msg -> fatal . T.pack $ "Couldn't read " <> view importFileL env <> ": " <> msg
    Right x -> do
      let outputFn = view outputfileL env
      writeFileUtf8 outputFn (showP x)
      logInfo . display . T.pack $ outputFn <> " written"

myDecode :: B.ByteString -> Either String P_Concept
myDecode = eitherDecode

instance FromJSON P_Concept

-- parseContext :: B.ByteString -> Guarded P_Context
-- parseContext contents = pure ctx
--   where
--     ctx =
--       PCtx
--         { ctx_vs = [],
--           ctx_rs = [],
--           ctx_rrules = [],
--           ctx_reprs = [],
--           ctx_ps = [],
--           ctx_pos = [],
--           ctx_pops = [],
--           ctx_pats = [],
--           ctx_nm = "Aap",
--           ctx_metas = [],
--           ctx_markup = Nothing,
--           ctx_lang = Nothing,
--           ctx_ks = [],
--           ctx_ifcs = [],
--           ctx_gs = [],
--           ctx_enfs = [],
--           ctx_ds = [],
--           ctx_cs = []
--         }
