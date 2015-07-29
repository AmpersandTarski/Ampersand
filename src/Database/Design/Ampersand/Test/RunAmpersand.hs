module Database.Design.Ampersand.Test.RunAmpersand (ampersand) where

import Database.Design.Ampersand.Misc.Options(getOptions,Options(..))
import Database.Design.Ampersand.FSpec.ToFSpec.CreateFspec(createFSpec)
import Database.Design.Ampersand.Input.ADL1.CtxError

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
              Checked _     -> do --generateAmpersandOutput fSpec
                                  --generateProtoStuff      fSpec
                                  return (file,[])
