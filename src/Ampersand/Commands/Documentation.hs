{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Commands.Documentation
    (doGenDocument)
where
import           Ampersand.Basics
import           Ampersand.Misc.HasClasses
import           Ampersand.Output.FSpec2Pandoc
import           Ampersand.Output.PandocAux
import           Ampersand.Graphic.Graphics
import           Ampersand.FSpec.FSpec
import           RIO.Time
-- This function will generate all Pictures for a given FSpec.
-- the returned FSpec contains the details about the Pictures, so they
-- can be referenced while rendering the FSpec.
-- This function generates a pandoc document, possibly with pictures from an fSpec.
doGenDocument :: (HasBlackWhite env, HasRootFile env, HasDirOutput env, HasLogFunc env, HasDocumentOpts env) 
   => FSpec -> RIO env ()
doGenDocument fSpec = do
    env <- ask
    fspecFormat <- view fspecFormatL
    now <- getCurrentTime
    logInfo $ "Generating functional design document for " <> display (name fSpec) <> "..."
    logDebug $ "Requested chapters: "<>displayShow (view chaptersL env) 
    let (thePandoc,thePictures) = fSpec2Pandoc env now fSpec
    -- First we need to output the pictures, because they should be present 
    -- before the actual document is written
    genGraphics <- view genGraphicsL
    when (genGraphics && fspecFormat /=FPandoc) $
      mapM_ writePicture (reverse thePictures) -- NOTE: reverse is used to have the datamodels generated first. This is not required, but it is handy.
    writepandoc fSpec thePandoc
        
