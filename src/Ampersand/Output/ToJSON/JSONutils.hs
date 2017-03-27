{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FunctionalDependencies #-} 
module Ampersand.Output.ToJSON.JSONutils 
  (writeJSONFile, JSON(..), ToJSON(..)
  , module X
  )
where
import Data.Aeson
import qualified Data.Aeson.Types as AT 
import Data.List
import Ampersand.Core.ParseTree as X
     ( Role
     , ViewHtmlTemplate(ViewHtmlTemplateFile)
     )
import Ampersand.Core.ShowAStruct as X
import Ampersand.FSpec.ToFSpec.Populated as X
import Ampersand.FSpec.FSpec as X
import Ampersand.FSpec.SQL as X (sqlQuery,sqlQueryWithPlaceholder,placeHolderSQL,broadQueryWithPlaceholder) 
import Ampersand.Misc as X
import Ampersand.Basics as X
import Ampersand.Classes as X
import System.FilePath
import System.Directory
import qualified Data.ByteString.Lazy as BS
import Ampersand.Prototype.ProtoUtil(getGenericsDir)
import Prelude hiding (writeFile)
import GHC.Generics as X
import Data.Aeson.Encode.Pretty

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
                [] -> fatal 71 $ "Label at Haskell side must contain `JSON`: "++str
                xs -> snd . splitAt (length pfx) . head $ xs
             where pfx = "JSON"    
  
  
