module Ampersand.Core.ShowPStruct
  (Pretty, showP)
where

import           Ampersand.ADL1.PrettyPrinters
import           Ampersand.Basics

showP :: Pretty a => a -> Text
showP = prettyPrint . pretty
