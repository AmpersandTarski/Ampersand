module Database.Design.Ampersand.Input.Xslx.XLSX 
  (parseXlsxFile)
where
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Prelude hiding (putStrLn, writeFile) -- make sure everything is UTF8
import Database.Design.Ampersand.Input.ADL1.CtxError
import Database.Design.Ampersand.ADL1

import Codec.Xlsx
import qualified Data.ByteString.Lazy as L
--import Control.Lens

fatal :: Int -> String -> a
fatal = fatalMsg "XLSX"

parseXlsxFile :: Options -> FilePath -> IO (Guarded P_Context)
parseXlsxFile _ filePath = 
  do bytestr <- L.readFile filePath
     return . xlsx2pContext . toXlsx $ bytestr

xlsx2pContext :: Xlsx -> Guarded P_Context
xlsx2pContext xlsx = fatal 14 "INCLUDE Excel files comming soon."

