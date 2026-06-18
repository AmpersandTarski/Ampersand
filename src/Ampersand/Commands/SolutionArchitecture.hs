-- | Implementation of the @ampersand solution-architecture@ command.
module Ampersand.Commands.SolutionArchitecture (doGenSolutionArchitecture) where

import Ampersand.Basics
import Ampersand.FSpec.FSpec
import Ampersand.Graphic.Graphics
import Ampersand.Misc.HasClasses
import Ampersand.Output.FSpec2PandocSolArch
import Ampersand.Output.PandocAux
import RIO.Time

-- | Generate a solution architecture document (with the data-model graphics it
-- references) from an FSpec.
doGenSolutionArchitecture ::
  (HasBlackWhite env, HasFSpecGenOpts env, HasDirOutput env, HasLogFunc env, HasDocumentOpts env) =>
  FSpec ->
  RIO env ()
doGenSolutionArchitecture fSpec = do
  env <- ask
  fspecFormat <- view fspecFormatL
  now <- getCurrentTime
  let (thePandoc, allPictures) = fSpec2PandocSolArch env now fSpec
  -- Pictures must be written before the document that references them.
  genGraphics <- view genGraphicsL
  when (genGraphics && fspecFormat /= FPandoc)
    $ do
      logInfo $ "Writing graphics for " <> (display . fullName) fSpec <> "..."
      mapM_ writePicture allPictures
  genText <- view genTextL
  when genText
    $ do
      logInfo $ "Generating solution architecture document for " <> (display . fullName) fSpec <> "..."
      writepandoc fSpec thePandoc
