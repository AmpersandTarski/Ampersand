module Atlas.Atlas where
import Adl
import Data.Fspec
import Atlas.AtlasFspec

atlas :: Fspc -> Fspc
atlas fSpec = fSpec_atlas
