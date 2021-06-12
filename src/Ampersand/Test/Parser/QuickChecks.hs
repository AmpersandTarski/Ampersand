{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
--to avoid warning for trace
{-# OPTIONS_GHC -Wno-deprecations #-}

module Ampersand.Test.Parser.QuickChecks
  ( doAllQuickCheckPropertyTests,
  )
where

import Ampersand.ADL1.PrettyPrinters (prettyPrint)
import Ampersand.Basics
import Ampersand.Core.ParseTree (P_Context)
import Ampersand.Input.ADL1.CtxError (Guarded (..))
import Ampersand.Input.Parsing (parseCtx)
import Ampersand.Test.Parser.ArbitraryTree ()
import qualified RIO.Text as T
import Test.QuickCheck
    ( quickCheckWith, stdArgs, chatty )

doAllQuickCheckPropertyTests :: RIO env () -- (Bool,Text)
doAllQuickCheckPropertyTests = do
  liftIO . quickCheckWith myArgs $ prop_parserRoundtrip
     where myArgs = stdArgs {chatty = False}
prop_parserRoundtrip :: P_Context -> Bool
prop_parserRoundtrip pCtx = 
  case roundtrip pCtx of
    Checked _ _ -> True
    Errors err -> exitWith . SomeTestsFailed $
                    T.lines (tshow err)
                 <> T.lines (prettyCtx pCtx)
           
roundtrip :: P_Context -> Guarded P_Context
roundtrip pCtx= fst <$> 
                parseCtx ( "ERROR: There is something wrong with the parser and/or with the\n"
                         <>"  way an arbitrary P_Context is defined. (See ArbitraryTree.hs)\n"
                         <>"  Below file at position" )
                           (prettyCtx pCtx)

prettyCtx :: P_Context -> Text
prettyCtx =
  T.unlines
    . zipWith (curry includeLineNr) [1 ..]
    . T.lines
    . prettyPrint
  where
    includeLineNr :: (Int, Text) -> Text
    includeLineNr (nr, str) = "{-" <> T.replicate (4 - T.length (tshow nr)) "0" <> tshow nr <> "-} " <> str
