module Database.Design.Ampersand.Output.FSpec2SQL
  (dumpSQLqueries)
where
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Prototype.Generate 
  (generateDBstructQueries, generateAllDefPopQueries
  )
import Database.Design.Ampersand.FSpec
import Database.Design.Ampersand.FSpec.SQL
import Data.List

dumpSQLqueries :: FSpec -> String
dumpSQLqueries fSpec = intercalate "\n" $ 
                         header ampersandVersionStr
                       ++header "Database structure queries"
                       ++generateDBstructQueries fSpec True
                       ++header "Initial population queries"
                       ++generateAllDefPopQueries fSpec
                       ++header "Violations of conjuncts"
                       ++concatMap showConjunct (allConjuncts fSpec)
                       ++header "Queries per declaration"
                       ++concatMap showDecl (vrels fSpec)
                       ++header "Queries of interfaces"
                       ++concatMap showInterface (interfaceS fSpec ++ interfaceG fSpec)
    
   where
     showInterface :: Interface -> [String]
     showInterface ifc 
        = header ("INTERFACE: "++name ifc)
        ++(map ((++) "  ") . showObjDef . ifcObj) ifc
        where 
          showObjDef :: ObjectDef -> [String]
          showObjDef obj
            = (header . showADL . objctx) obj
            ++(lines . prettySQLQueryWithPlaceholder 2 fSpec . objctx) obj
            ++case objmsub obj of
                 Nothing  -> []
                 Just sub -> showSubInterface sub
            ++header "Broad query of above stuff"     
            ++(lines . prettyBroadQueryWithPlaceholder 2 fSpec $ obj)
          showSubInterface :: SubInterface -> [String]
          showSubInterface sub = 
            case sub of 
              Box{} -> concatMap showObjDef . siObjs $ sub
              InterfaceRef{} -> []

     showConjunct :: Conjunct -> [String]
     showConjunct conj 
        = header (rc_id conj)
        ++["Rules for this conjunc:"]
        ++map showRule (rc_orgRules conj)
        ++(lines . prettySQLQuery 2 fSpec . conjNF (getOpts fSpec) . notCpl . rc_conjunct $ conj)
        ++[""]
        where
          showRule r 
            = "  - "++name r++": "++showADL r
     showDecl :: Declaration -> [String]
     showDecl decl 
        = header (showADL decl)
        ++(lines . prettySQLQuery 2 fSpec $ decl)
        ++[""]
     header :: String -> [String]
     header title = 
         [ ""
         , replicate width '*'
         , "***"++spaces firstspaces++title++spaces (width-6-firstspaces-length title)++"***"
         , replicate width '*'
         , ""
         ]
       where width = maximum [80 , length title + 8]
             spaces :: Int -> String
             spaces i = replicate i ' '
             firstspaces :: Int
             firstspaces = (width - 6 - length title) `quot` 2 