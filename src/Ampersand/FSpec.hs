module Ampersand.FSpec
  ( module Ampersand.FSpec.FSpec,
    module Ampersand.FSpec.ShowHS,
    module Ampersand.FSpec.Instances,
    module Ampersand.FSpec.ToFSpec.Calc,
    module Ampersand.FSpec.ToFSpec.ADL2FSpec,
    module Ampersand.FSpec.ToFSpec.NormalForms,
    module Ampersand.FSpec.Motivations,
    module Ampersand.FSpec.ToFSpec.CreateFspec,
  )
where

import Ampersand.FSpec.FSpec
import Ampersand.FSpec.Instances
import Ampersand.FSpec.Motivations
  ( HasMeaning (..),
    Motivated (..),
  )
import Ampersand.FSpec.ShowHS
  ( ShowHS (..),
    ShowHSName (..),
    fSpec2Haskell,
    haskellIdentifier,
  )
import Ampersand.FSpec.ToFSpec.ADL2FSpec (makeFSpec)
import Ampersand.FSpec.ToFSpec.Calc (showPrf, showProof)
import Ampersand.FSpec.ToFSpec.CreateFspec
import Ampersand.FSpec.ToFSpec.NormalForms (conjNF)
