{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FunctionalDependencies #-} 
{-# LANGUAGE RecordWildCards #-}
module Ampersand.Output.ToJSON.JSONutils 
  (writeJSONFile, JSON(..), ToJSON(..)
  , module Ampersand.Basics
  , module Ampersand.Classes
  , module Ampersand.Core.ParseTree
  , module Ampersand.Core.ShowAStruct
  , module Ampersand.FSpec.ToFSpec.Populated
  , module Ampersand.FSpec.FSpec
  , module Ampersand.FSpec.SQL
  , module Ampersand.Misc
  )
where
import           Ampersand.Basics
import           Ampersand.Classes
import           Ampersand.Core.ParseTree ( Role, ViewHtmlTemplate(ViewHtmlTemplateFile))
import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec.ToFSpec.Populated
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.SQL (sqlQuery,sqlQueryWithPlaceholder,placeHolderSQL,broadQueryWithPlaceholder) 
import           Ampersand.Misc
import           Ampersand.Prototype.ProtoUtil(getGenericsDir)
import           Data.Aeson hiding (Options)
import qualified Data.Aeson.Types as AT 
import           Data.Aeson.Encode.Pretty
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as L
import           GHC.Generics
import           System.FilePath
import           System.Directory

writeJSONFile :: (ToJSON a, HasDirPrototype env, HasHandle env, HasVerbosity env) => 
                 FilePath -> a -> RIO env ()
writeJSONFile fName x = do
    opts <- ask
    let fullFile = getGenericsDir opts </> file
    sayWhenLoudLn ("  Generating "++file) 
    liftIO $ createDirectoryIfMissing True (takeDirectory fullFile)
    liftIO $ BL.writeFile fullFile (encodePretty x)
  where file = fName <.> "json"
        

-- We use aeson to generate .json in a simple and efficient way.
-- For details, see http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:ToJSON
class (GToJSON Zero (Rep b), Generic b) => JSON a b | b -> a where
  fromAmpersand :: (HasEnvironment env, HasProtoOpts env) 
       => env -> MultiFSpecs -> a -> b
  amp2Jason :: b -> Value
  amp2Jason = genericToJSON ampersandDefault

-- These are the modified defaults, to generate .json 
ampersandDefault :: AT.Options
ampersandDefault = defaultOptions {AT.fieldLabelModifier = alterLabel}
  where 
    -- The label of a field is modified before it is written in the JSON file. 
    -- this is done because of different restrictions at the Haskell side and at
    -- the .json side. In our case, we strip all characters upto the first occurence
    -- of the prefix "JSON" (which is mandatory). in the rest of that string, we 
    -- substitute all underscores with dots.
    alterLabel str =
      case filter (L.isPrefixOf pfx) (L.tails str) of
        []  -> fatal ("Label at Haskell side must contain `JSON`: "++str)
        h:_ -> replace '_' '.' . snd . L.splitAt (length pfx) $ h
      where pfx = "JSON"

-- | Replaces all instances of a value in a list by another value.
replace :: Eq a =>
           a   -- ^ Value to look for
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replace x y = map (\z -> if z == x then y else z)
  
  
