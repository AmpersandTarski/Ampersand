{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generate a UML output file from a project.
module Ampersand.Commands.Uml
    (uml
    ,UmlOpts(..)
--    ,HasUmlOpts(..)
    ) where

import           Ampersand.Basics
import           Ampersand.Misc.HasClasses
import           Ampersand.FSpec
import           Ampersand.FSpec.GenerateUML
import qualified RIO.Text as T
import           System.Directory
import           System.FilePath
-- | Generate a UML output file from a project.
--
uml :: (HasDirOutput env, HasFSpecGenOpts env, HasLogFunc env, HasOutputLanguage env) 
    => FSpec -> RIO env ()
uml fSpec = do
   env <- ask
   outputFile <- outputFile' <$> ask
   logDebug "Generating UML..."
   liftIO $ createDirectoryIfMissing True (takeDirectory outputFile)
   writeFileUtf8 outputFile $ generateUML env fSpec
   logInfo ("Generated file: " <> display (T.pack outputFile) <> ".")
   where outputFile' env = view dirOutputL env </> baseName env -<.> ".xmi"



