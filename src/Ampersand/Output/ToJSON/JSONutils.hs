{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FunctionalDependencies #-} 
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
  , module GHC.Generics
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
import qualified Data.ByteString.Lazy as BS
import           Data.List
import           GHC.Generics
import           System.FilePath
import           System.Directory

writeJSONFile :: ToJSON a => Options -> FilePath -> a -> IO()
writeJSONFile opts fName x 
  = do verboseLn opts ("  Generating "++file)
       createDirectoryIfMissing True (takeDirectory fullFile)
       BS.writeFile fullFile (encodePretty x)
  where file = fName <.> "json"
        fullFile = getGenericsDir opts </> file

class (GToJSON Zero (Rep b), Generic b) => JSON a b | b -> a where
  fromAmpersand :: MultiFSpecs -> a -> b
  amp2Jason :: b -> Value
  amp2Jason = genericToJSON ampersandDefault

ampersandDefault :: AT.Options
ampersandDefault = defaultOptions {AT.fieldLabelModifier = stripLabel}
  where stripLabel str 
          = case filter (isPrefixOf pfx) (tails str) of
                [] -> fatal ("Label at Haskell side must contain `JSON`: "++str)
                xs -> snd . splitAt (length pfx) . head $ xs
             where pfx = "JSON"    
  
  
