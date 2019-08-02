module Ampersand.Test.RunAmpersand
   ( ampersand
    ) where

import           Ampersand.Basics
import           Ampersand.FSpec.ToFSpec.CreateFspec(createMulti)
import           Ampersand.Input.ADL1.CtxError
import           Ampersand.Misc
import qualified Data.List.NonEmpty as NEL
import           MainApps(defEnv)

ampersand :: [FilePath] -> IO [[CtxError]]
ampersand files = do
     env <- defEnv
     sequence $ fmap (runRIO env . runFile) files

runFile :: FilePath -> RIO App [CtxError]
runFile file = switchFileName file $ do
   gFSpec <- createMulti
   case gFSpec of
     Errors err    -> return $ NEL.toList err
     --TODO: Do something with the fSpec
     Checked _ _   -> return []
switchFileName :: FilePath -> RIO App a -> RIO App a
switchFileName file inner = do
  app <- ask
  opts <- view optionsL
  let app' = app { options' = opts{ fileName = Just file } }
  runRIO app' inner
