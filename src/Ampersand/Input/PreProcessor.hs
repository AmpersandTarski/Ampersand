module Ampersand.Input.PreProcessor (
      preProcess
    , PreProcDefine
) where

import  GHC.Base
import  Prelude
import  Data.List

type PreProcDefine = String

preProcess :: String -> [PreProcDefine] -> String
preProcess x _ = x
