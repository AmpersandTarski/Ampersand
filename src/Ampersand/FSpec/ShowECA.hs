module Ampersand.FSpec.ShowECA (showECA) where

import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ShowAStruct
import Ampersand.FSpec.ShowPAclause (showPAclause)

class ECA a where
  showECA :: String -> a -> String

instance ECA ECArule where
  showECA indent er = showECA indent (ecaTriggr er) ++ " EXECUTE    -- (ECA rule "
                                                    ++ show (ecaNum er)
                                                    ++ ")"
                                                    ++
                      indent ++ showECA indent (ecaAction er)
instance ECA Event where
  showECA _ (On Ins rel) = "ON INSERT Delta IN " ++ showA rel
  showECA _ (On Del rel) = "ON DELETE Delta FROM " ++ showA rel

instance ECA PAclause where
  showECA indent = showPAclause indent
