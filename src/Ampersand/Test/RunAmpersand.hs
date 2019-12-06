module Ampersand.Test.RunAmpersand
   ( ampersand
    ) where

import           Ampersand.Basics
import           Ampersand.FSpec.ToFSpec.CreateFspec(createFspec)
import           Ampersand.Input.ADL1.CtxError
import           Ampersand.Misc
import qualified RIO.NonEmpty as NE
import qualified RIO.NonEmpty.Partial as PARTIAL
import           MainApps ()-- (defEnv)

ampersand :: [FilePath] -> IO [[CtxError]]
ampersand files = return [[]]
-- ampersand :: [FilePath] -> IO [[CtxError]]
-- ampersand files = do
--      env <- defEnv
--      sequence $ fmap (runRIO env . runFile) files

-- runFile :: FilePath -> RIO App [CtxError]
-- runFile file = switchFileName file $ do
--    let recipe = []
--    gFSpec <- createFspec recipe
--    case gFSpec of
--      Errors err    -> return $ NE.toList err
--      --TODO: Do something with the fSpec
--      Checked _ _   -> return []
-- switchFileName :: (HasRootFile env) => FilePath -> RIO env a -> RIO env a
-- switchFileName file inner = do
--   env <- ask
--   let env' = set rootFileL (Just file) env
--   runRIO env' inner
