  module HtmlFilenames where
   import CommonClasses ( Identified(name) )
   import Adl ( Context, Declaration, source, target, Context, Rule
                  ,Concept, Pattern)
   import Hatml  (htmlname)
   import Data.Fspec
   --TODO : Er is g��n nut meer voor een ADL2Html, maar alleen een Fspec2Html. 
   --       hierdoor zullen veel van onderstaande spullen kunnen/moeten verdwijnen.

   fnContext :: Context -> String
   fnContext context
    = ""++htmlname (name context)
   fnRelation :: Context -> Declaration -> String
   fnRelation context decl
    = "REL_"++htmlname (name context++"_"++name decl++name (source decl)++name (target decl))
--   fnRule :: Context -> Rule -> String
--   fnRule context rule
--    = "RULE_"++htmlname (name context++"_"++show (nr rule))
   fnConcept :: Context -> Concept -> String
   fnConcept context cnpt
    = "CPT_"++htmlname (name context++name cnpt)
   fnPatConcept :: Context -> Pattern -> Concept -> String
   fnPatConcept context pat cnpt
    = htmlname (name context++"_"++name pat++name cnpt)
   fnPattern :: Context -> Pattern -> String
   fnPattern context pat
    = htmlname (name context++"_"++name pat)
