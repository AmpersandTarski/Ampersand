  module Fspec2Xml where
   import Char (isAlphaNum, isSpace)
   import Collection (Collection(rd))
 -- TODO: ADLdef en ShowADL hoort hier niet thuis! Deze module moet nog worden aangepast op de nieuwe architectuur. 
   import Adl
   import ShowADL


   import FspecDef
   
--   makeXML_depreciated context
--    = putStr ("\nXML representation of "++name context++"\n")>>
--      writeFile (name context++".xml") (showXML context "")>>
--      putStr ("\n"++name context++".xml written\n")
--    where
--       rs      = ctxrs context


