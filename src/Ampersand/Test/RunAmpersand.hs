module Ampersand.Test.RunAmpersand (ampersand) where

import Ampersand.Misc.Options(getOptions,Options(..))
import Ampersand.FSpec.ToFSpec.CreateFspec(createFSpec)
import Ampersand.Input.ADL1.CtxError

ampersand :: [FilePath] -> IO [(FilePath,[CtxError])]
ampersand files = 
  do opts <- getOptions
     mapM (runAmpersand opts) files

runAmpersand :: Options -> FilePath -> IO (FilePath,[CtxError])
runAmpersand opts file = 
        do gFSpec <- createFSpec opts{ fileName = file }
           case gFSpec of
              Errors err    -> return (file,err)
              --TODO: Do something with the fSpec
              Checked _     -> do return (file,[])
