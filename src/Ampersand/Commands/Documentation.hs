module Ampersand.Commands.Documentation (doGenDocument) where

import Ampersand.Basics
import Ampersand.Diagnosis.Extract (extractDiagnostics)
import Ampersand.FSpec.FSpec
import Ampersand.Graphic.Graphics
import Ampersand.Misc.HasClasses
import Ampersand.Output.Diagnosis2Xlsx (diagnostics2Xlsx)
import Ampersand.Output.FSpec2Pandoc
import Ampersand.Output.PandocAux
import qualified RIO.ByteString.Lazy as BL
import RIO.Directory (createDirectoryIfMissing)
import RIO.FilePath ((-<.>), (</>), takeDirectory)
import qualified RIO.Text as T
import RIO.Time
import Text.Pandoc.Class (getPOSIXTime, runIO)
import Text.Pandoc.Error (handleError)

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
  logDebug $ "Requested chapters: " <> displayShow (view chaptersL env)
  let (thePandoc, allPictures) = fSpec2Pandoc env now fSpec
  -- First we need to output the pictures, because they should be present
  -- before the actual document is written
  datamodelsOnly <- view genDatamodelOnlyL
  genGraphics <- view genGraphicsL
  focusOfVisuals <- view focusOfVisualsL
  if datamodelsOnly
    then do
      logInfo $ "Writing datamodel graphics for " <> (display . fullName) fSpec <> "..."
      mapM_ writePicture $ filter forDataModelsOnlySwitch allPictures
    else do
      logInfo $ "Writing graphics for " <> (display . fullName) fSpec <> "..."
      when (genGraphics && fspecFormat /= FPandoc)
        . mapM_ writePicture
        . filter (\p -> visualFocus p `elem` focusOfVisuals)
        $ allPictures
      genText <- view genTextL
      when genText
        $ do
          logInfo $ "Generating functional design document for " <> (display . fullName) fSpec <> "..."
          writepandoc fSpec thePandoc
      -- When the Diagnosis chapter is requested, emit a companion
      -- spreadsheet with the full per-pattern/concept/relation/rule/interface
      -- detail.  The Pandoc chapter only shows the summary.
      when (Diagnosis `elem` view chaptersL env)
        $ writeDiagnosisXlsx env fSpec

writeDiagnosisXlsx ::
  (HasDirOutput env, HasDocumentOpts env, HasFSpecGenOpts env, HasLogFunc env) =>
  env ->
  FSpec ->
  RIO env ()
writeDiagnosisXlsx env fSpec = do
  let lang = outputLang env fSpec
      diagData = extractDiagnostics lang fSpec
      outputFile = view dirOutputL env </> baseName env <> "-diagnosis" -<.> ".xlsx"
  ct <- liftIO $ runIO getPOSIXTime >>= handleError
  liftIO $ createDirectoryIfMissing True (takeDirectory outputFile)
  liftIO $ BL.writeFile outputFile $ diagnostics2Xlsx ct diagData
  logInfo $ "Generated diagnosis spreadsheet: " <> display (T.pack outputFile)
