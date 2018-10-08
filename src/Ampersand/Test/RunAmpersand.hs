module Ampersand.Test.RunAmpersand
   ( ampersand
    ) where

import           Ampersand.Basics
import           Ampersand.FSpec.ToFSpec.CreateFspec(createMulti)
import           Ampersand.Input.ADL1.CtxError
import           Ampersand.Misc
import qualified Data.List.NonEmpty as NEL (toList)

ampersand :: [FilePath] -> IO [[CtxError]]
ampersand files = 
  do opts <- getOptions
     mapM (runAmpersand opts) files

runAmpersand :: Options -> FilePath -> IO [CtxError]
runAmpersand opts file = 
        do gFSpec <- createMulti opts{ fileName = file }
           case gFSpec of
              Errors err    -> return $ NEL.toList err
              --TODO: Do something with the fSpec
              Checked _     -> return []
