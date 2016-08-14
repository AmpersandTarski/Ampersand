{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FunctionalDependencies #-} 
module Ampersand.Output.ToJSON.JSONutils 
  (writeJSONFile, JSON(..), ToJSON(..)
  , module Ampersand.FSpec.FSpec
  , module Ampersand.Misc
  , module Ampersand.FSpec.SQL
  , module Ampersand.Basics
  , module Ampersand.Classes
  , module GHC.Generics
  )
where
import Data.Aeson
import Data.Aeson.Types
import Data.List
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.SQL (sqlQuery,sqlQueryWithPlaceholder,placeHolderSQL,broadQueryWithPlaceholder)
import Ampersand.Misc
import Ampersand.Basics
import Ampersand.Classes
import System.FilePath
import System.Directory
import qualified Data.ByteString.Lazy as BS
import Ampersand.Prototype.ProtoUtil(getGenericsDir)
import Prelude hiding (writeFile)
import GHC.Generics
import Data.Aeson.Encode.Pretty

writeJSONFile :: ToJSON a => FSpec -> FilePath -> a -> IO()
writeJSONFile fSpec fName x 
  = do verboseLn (getOpts fSpec) ("  Generating "++file)
       createDirectoryIfMissing True (takeDirectory fullFile)
       BS.writeFile fullFile (encodePretty x)
  where file = fName <.> "json"
        fullFile = getGenericsDir fSpec </> file

class (GToJSON (Rep b), Generic b) => JSON a b | b -> a where
  fromAmpersand :: FSpec -> a -> b
  amp2Jason :: b -> Value
  amp2Jason = genericToJSON ampersandDefault

ampersandDefault :: Data.Aeson.Types.Options
ampersandDefault = defaultOptions {fieldLabelModifier = stripLabel}
  where stripLabel str 
          = case filter (isPrefixOf pfx) (tails str) of
                [] -> fatal 71 $ "Label at Haskell side must contain `JSON`: "++str
                xs -> snd . splitAt (length pfx) . head $ xs
             where pfx = "JSON"    
  
  
