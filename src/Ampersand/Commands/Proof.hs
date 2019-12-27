{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generate a proofs output document from a project.
module Ampersand.Commands.Proof
    (proof
    ,ProtoOpts(..)
    ,HasProtoOpts(..)
    ) where

import           Ampersand.Basics
import           Ampersand.Misc.HasClasses
import           Ampersand.FSpec
import           System.FilePath ((</>), (-<.>))
import           Text.Pandoc (runIO,writeHtml5String,def,handleError)
import           Text.Pandoc.Builder

-- | Generate a proofs output document from a project.
--
proof :: (HasDirOutput env, HasRootFile env, HasLogFunc env) 
       => FSpec -> RIO env ()
proof fSpec = do 
    env <- ask
    sayLn $ "Generating Proof for " ++ name fSpec ++ " into " ++ outputFile env ++ "..."
    content <- liftIO $ (runIO (writeHtml5String def thePandoc)) >>= handleError
    writeFileUtf8 (outputFile env) content
    logDebug "Proof written."
  where 
      outputFile env = view dirOutputL env </> "proofs_of_"++baseName env -<.> ".html"
      thePandoc = setTitle title (doc theDoc)
      title  = text $ "Proofs for "++name fSpec
      theDoc = fDeriveProofs fSpec
      --theDoc = plain (text "Aap")  -- use for testing...

