
module DatabaseDesign.Ampersand.Fspec.Requierments
  (Req(..), requirements)
where

import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.Fspec.Fspec 
import DatabaseDesign.Ampersand.Output.AdlExplanation
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Basics

data Req = Req { reqId :: String
            --   , reqRef :: String
               , reqOrig :: Either Rule Declaration
               , reqPurposes :: [Purpose]
               }

instance Meaning Req where
  meaning l r = case reqOrig r of
                  Right rul -> meaning l rul
                  Left  dcl -> meaning l dcl

requirements :: Options -> Fspc -> [Req]
requirements opts fSpec
   = [decl2req d | d <- vrels  fSpec]
   ++[rule2req r | r <- vrules fSpec]
  where
    decl2req d = Req { reqId = name d
                   --  , reqRef = "DeRef--TODO" --TODO 
                     , reqOrig = Right d
                     , reqPurposes = purposesDefinedIn fSpec (language opts) d
                     }
    rule2req r = Req { reqId = name r
                   --  , reqRef = "DeRef--TODO" --TODO 
                     , reqOrig = Left r
                     , reqPurposes = purposesDefinedIn fSpec (language opts) r
                     }
                     