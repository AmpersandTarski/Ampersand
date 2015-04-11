module Database.Design.Ampersand.Test.RunAmpersand (ampersand) where

import Database.Design.Ampersand.Misc.Options(getOptions,Options(..))
import Database.Design.Ampersand.FSpec.ToFSpec.CreateFspec(createFSpec)
import Database.Design.Ampersand.Input.ADL1.CtxError
import Data.List(intersperse)
import Debug.Trace

ampersand :: [FilePath] -> IO Bool
ampersand [] = return True
ampersand (x:xs) =
    do opts <- getOptions
       ran <- runAmpersand opts x
       if ran then ampersand xs
       else return False

runAmpersand :: Options -> FilePath -> IO Bool
runAmpersand opts file =
        do gFSpec <- createFSpec opts{ fileName = file }
           case gFSpec of
              Errors err    -> return $ trace ("Error(s) found:" ++ errs) False
                where errs = concat$ intersperse "\n" (map showErr err)
              --TODO: Do something with the fSpec
              Checked _     -> do --generateAmpersandOutput fSpec
                                  --generateProtoStuff      fSpec
                                  return True
