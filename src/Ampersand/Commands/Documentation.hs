module Ampersand.Commands.Documentation (doGenDocument) where

import Ampersand.Basics
import Ampersand.FSpec.FSpec
import Ampersand.Graphic.Graphics
import Ampersand.Misc.HasClasses
import Ampersand.Output.FSpec2Pandoc
import Ampersand.Output.PandocAux
import RIO.Time

-- This function will generate all Pictures for a given FSpec.
-- the returned FSpec contains the details about the Pictures, so they
-- can be referenced while rendering the FSpec.
-- This function generates a pandoc document, possibly with pictures from an fSpec.
doGenDocument ::
  (HasBlackWhite env, HasFSpecGenOpts env, HasDirOutput env, HasLogFunc env, HasDocumentOpts env) =>
  FSpec ->
  RIO env ()
doGenDocument fSpec = do
  env <- ask
  fspecFormat <- view fspecFormatL
  now <- getCurrentTime
  logInfo $ "Generating functional design document for " <> (display . fullName) fSpec <> "..."
  logDebug $ "Requested chapters: " <> displayShow (view chaptersL env)
  let (thePandoc, allPictures) = fSpec2Pandoc env now fSpec
  -- First we need to output the pictures, because they should be present
  -- before the actual document is written
  datamodelsOnly <- view genDatamodelOnlyL
  if datamodelsOnly
    then mapM_ writePicture $ filter forDataModelsOnlySwitch allPictures
    else do
      genGraphics <- view genGraphicsL
      focusOfVisuals <- view focusOfVisualsL
      let isRequested p = visualFocus p `elem` focusOfVisuals
      when (genGraphics && fspecFormat /= FPandoc)
        . mapM_ writePicture
        . filter isRequested
        $ allPictures
      genText <- view genTextL
      when genText
        $ writepandoc fSpec thePandoc
