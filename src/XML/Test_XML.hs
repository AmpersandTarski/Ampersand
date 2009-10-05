
module Main
where
 import Text.XML.HaXml
 import Text.XML.HaXml.Pretty
 -- Deze module is alleen voor testdoeleinden.


 main = mainXML
 mainXML :: IO ()
 mainXML = let theFileName = "schemeXample.xsd" in
           do theFileContent <- readFile theFileName
              --putStrLn xsdtext
              let theDocument = xmlParse theFileName theFileContent
              putStr  (show (document theDocument ))
 