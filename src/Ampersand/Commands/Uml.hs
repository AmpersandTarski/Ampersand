{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generate a UML output file from a project.
module Ampersand.Commands.Uml
    (uml
    ,UmlOpts(..)
--    ,HasUmlOpts(..)
    ) where

import           Ampersand.Basics
import           Ampersand.Misc
import           Ampersand.FSpec
import           Ampersand.FSpec.GenerateUML
import           System.FilePath
-- | Generate a UML output file from a project.
--
uml :: (HasDirOutput env, HasRootFile env, HasLogFunc env, HasOutputLanguage env) 
    => FSpec -> RIO env ()
uml fSpec = do
   env <- ask
   outputFile <- outputFile' <$> ask
   sayLn "Generating UML..."
   liftIO . writeFile outputFile $ generateUML env fSpec
   sayWhenLoudLn ("Generated file: " ++ outputFile ++ ".")
   where outputFile' env = view dirOutputL env </> baseName env -<.> ".xmi"



