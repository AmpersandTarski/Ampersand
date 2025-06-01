module Ampersand.Input.SemWeb.Turtle
  ( parseTurtleFile,
  )
where

import Ampersand.Basics
import Ampersand.Core.ParseTree
  ( P_Context (..),
  )
import Ampersand.Input.ADL1.CtxError (Guarded (..))
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

parseTurtleFile :: FilePath -> RIO env (Guarded P_Context)
parseTurtleFile filePath = do
  nm <- case toNamePart (camel . T.pack $ filePath) of
    Nothing -> fatal $ "No name for file: " <> T.pack filePath
    Just part -> pure . mkName ContextName $ part NE.:| []
  pure
    . pure
    $ PCtx
      { ctx_vs = mempty,
        ctx_rs = mempty,
        ctx_rrules = mempty,
        ctx_reprs = mempty,
        ctx_ps = mempty,
        ctx_pos = mempty,
        ctx_pops = mempty,
        ctx_pats = mempty,
        ctx_nm = nm,
        ctx_metas = mempty,
        ctx_markup = Just Markdown,
        ctx_lbl = Nothing,
        ctx_lang = Nothing,
        ctx_ks = mempty,
        ctx_ifcs = mempty,
        ctx_gs = mempty,
        ctx_enfs = mempty,
        ctx_ds = mempty,
        ctx_cs = mempty
      }