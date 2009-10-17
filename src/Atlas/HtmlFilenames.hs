  module Atlas.HtmlFilenames where
   import CommonClasses ( Identified(name) )
   import Adl 
   import Atlas.Hatml  (htmlname)
   import Data.Fspec
   import System.FilePath
   import Options
   import FspecDef
   
   class HTMLfile a where
      htmlFileUrl :: Options -> a -> String
      htmlFullPathAndName :: Options -> a -> String
      htmlFullPathAndName flags cpt = combine (dirAtlas flags) (htmlFileUrl flags cpt)
      
   instance HTMLfile Concept where
      htmlFileUrl flags cpt = addExtension (combine "Concepts" (name cpt)) ".html"
                            
   fnContext :: Context -> String
   fnContext context
    = ""++htmlname (name context)
   fnFspec :: Fspc -> String
   fnFspec fSpec
    = ""++htmlname (name fSpec)
   fnRelation :: Context -> Declaration -> String
   fnRelation context decl
    = "REL_"++htmlname (name context++"_"++name decl++name (source decl)++name (target decl))
   fnRule :: Context -> Rule -> String
   fnRule context rule
    = "RULE_"++htmlname (name context++"_"++show (nr rule))
   fnConcept :: Context -> Concept -> String
   fnConcept context cnpt
    = "CPT_"++htmlname (name context++name cnpt)
   fnPatConcept :: Context -> Pattern -> Concept -> String
   fnPatConcept context pat cnpt
    = htmlname (name context++"_"++name pat++name cnpt)
   fnPattern :: Context -> Pattern -> String
   fnPattern context pat
    = htmlname (name context++"_"++name pat)
