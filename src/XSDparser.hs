
module XSDparser --(pXSDocument, xsdkeywordstxt, xsdkeywordsops, xsdspecialchars, xsdopchars)
  where
   import UU_Parsing
   import UU_Scanner
   import Collection  (Collection(..))
   import Auxiliaries (sort)
   
  -- Moet uiteindelijk afgekeken worden van http://www.w3.org/2001/XMLSchema.xsd


   xsdkeywordstxt :: [String]
   xsdkeywordstxt       = [ ]
   
   xsdkeywordsops :: [String]
   xsdkeywordsops       = [ ] -- "-|", "|-", ":-", "-:", "-", "->", ">", "=", "~", "+", ";", "!", "*", "::", ":", "\\/", "/\\" ]
   xsdspecialchars :: String
   xsdspecialchars      = "()[].,{}"
   xsdopchars :: String
   xsdopchars           = rd (sort (concat xsdkeywordsops))





   pXSDocument    :: Parser Token XSDocument
   pXSDocument  = pString -- "Not implemented yet"

   type XSDocument = String