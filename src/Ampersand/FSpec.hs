module Ampersand.FSpec (module X) where
import Ampersand.FSpec.FSpec as X
import Ampersand.Basics as X (fatal, Collection(..), Named(..))
import Ampersand.FSpec.ShowHS as X
       (ShowHS(..), ShowHSName(..), fSpec2Haskell, haskellIdentifier)
import Ampersand.FSpec.ShowMeatGrinder as X
       (makeMetaPopulationFile)
import Ampersand.FSpec.ToFSpec.Calc as X (showProof, showPrf)
import Ampersand.FSpec.ToFSpec.ADL2FSpec as X (makeFSpec)
import Ampersand.FSpec.ToFSpec.NormalForms as X
       (conjNF, disjNF, cfProof, simplify)
import Ampersand.FSpec.Motivations as X
       (Meaning(..), Motivated(..))
import Ampersand.FSpec.ToFSpec.CreateFspec as X (createMulti)