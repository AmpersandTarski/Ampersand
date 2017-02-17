module Ampersand.Test.RunAmpersand (ampersand) where

import Ampersand.Misc
import Ampersand.FSpec.ToFSpec.CreateFspec(createMulti)
import Ampersand.Input.ADL1.CtxError

ampersand :: [FilePath] -> IO [[CtxError]]
ampersand files = 
  do opts <- getOptions
     mapM (runAmpersand opts) files

runAmpersand :: Options -> FilePath -> IO [CtxError]
runAmpersand opts file = 
        do gFSpec <- createMulti opts{ fileName = file }
           case gFSpec of
              Errors err    -> return err
              --TODO: Do something with the fSpec
              Checked _     -> return []
