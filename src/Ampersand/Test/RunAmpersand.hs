module Ampersand.Test.RunAmpersand
   ( ampersand
    ) where

import           Ampersand.Basics
import           Ampersand.FSpec.ToFSpec.CreateFspec(createFSpec)
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
   gFSpec <- createFSpec
   case gFSpec of
     Errors err    -> return $ NEL.toList err
     --TODO: Do something with the fSpec
     Checked _ _   -> return []
switchFileName :: (HasRootFile env) => FilePath -> RIO env a -> RIO env a
switchFileName file inner = do
  env <- ask
  let env' = set fileNameL (Just file) env
  runRIO env' inner
