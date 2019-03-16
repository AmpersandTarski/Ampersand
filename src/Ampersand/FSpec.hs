module Ampersand.FSpec
   ( module Ampersand.FSpec.FSpec
   , module Ampersand.FSpec.ShowHS
   , module Ampersand.FSpec.ToFSpec.Calc
   , module Ampersand.FSpec.ToFSpec.ADL2FSpec
   , module Ampersand.FSpec.ToFSpec.NormalForms
   , module Ampersand.FSpec.Motivations
   , module Ampersand.FSpec.ToFSpec.CreateFspec
   )
where
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.ShowHS
       (ShowHS(..), ShowHSName(..), fSpec2Haskell, haskellIdentifier)
import Ampersand.FSpec.ToFSpec.Calc (showProof, showPrf)
import Ampersand.FSpec.ToFSpec.ADL2FSpec (makeFSpec)
import Ampersand.FSpec.ToFSpec.NormalForms
       (conjNF, disjNF, cfProof, simplify)
import Ampersand.FSpec.Motivations
       (HasMeaning(..), Motivated(..))
import Ampersand.FSpec.ToFSpec.CreateFspec (createMulti)