module Ampersand.FSpec
   ( module Ampersand.Basics
   , module Ampersand.FSpec.FSpec
   , module Ampersand.FSpec.ShowHS
   , module Ampersand.FSpec.ShowADL
   , module Ampersand.FSpec.ShowECA
   , module Ampersand.FSpec.ShowMeatGrinder
   , module Ampersand.FSpec.ToFSpec.Calc
   , module Ampersand.FSpec.ToFSpec.ADL2FSpec
   , module Ampersand.FSpec.ToFSpec.NormalForms
   , module Ampersand.FSpec.Motivations
   , module Ampersand.FSpec.ToFSpec.CreateFspec
   )
where
import Ampersand.FSpec.FSpec
import Ampersand.Basics      (fatal,Collection(..),Named(..))
import Ampersand.FSpec.ShowHS
       (ShowHS(..), ShowHSName(..), fSpec2Haskell, haskellIdentifier)
import Ampersand.FSpec.ShowADL (ShowADL(..))
import Ampersand.FSpec.ShowECA (showECA)
import Ampersand.FSpec.ShowMeatGrinder (makeMetaPopulationFile)
import Ampersand.FSpec.ToFSpec.Calc
       (showProof,showPrf, commaEngPandoc, commaNLPandoc, commaEngPandoc', commaNLPandoc', commaPandocAnd,commaPandocOr)
import Ampersand.FSpec.ToFSpec.ADL2FSpec
       (makeFSpec)
import Ampersand.FSpec.ToFSpec.NormalForms
       (conjNF, disjNF, cfProof, simplify)
import Ampersand.FSpec.Motivations
       (Meaning(..),Motivated(..))
import Ampersand.FSpec.ToFSpec.CreateFspec 
       (createFSpec)
       