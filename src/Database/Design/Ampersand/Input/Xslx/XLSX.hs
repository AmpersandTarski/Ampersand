module Database.Design.Ampersand.Input.Xslx.XLSX 
  (parseXlsxFile)
where
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Prelude hiding (putStrLn, writeFile) -- make sure everything is UTF8
import Database.Design.Ampersand.Input.ADL1.CtxError
import Database.Design.Ampersand.ADL1

fatal :: Int -> String -> a
fatal = fatalMsg "Parsing"

parseXlsxFile :: Options -> FilePath -> IO (Guarded P_Context)
parseXlsxFile _ _ = fatal 14 "INCLUDE Excel files comming soon."
