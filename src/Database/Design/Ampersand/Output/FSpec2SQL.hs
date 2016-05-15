module Database.Design.Ampersand.Output.FSpec2SQL
  (dumpSQLqueries)
where
import Database.Design.Ampersand.Prototype.Generate 
  (generateDBstructQueries, generateAllDefPopQueries
  )
import Database.Design.Ampersand.FSpec
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.FSpec.SQL
import Data.List

dumpSQLqueries :: FSpec -> String
dumpSQLqueries fSpec = intercalate "\n" $ 
                         header "Database structure queries"
                       ++generateDBstructQueries fSpec True
                       ++header "Initial population queries"
                       ++generateAllDefPopQueries fSpec True
                       ++header "Violations of conjuncts"
                       ++concatMap showConjunct (allConjuncts fSpec)
                       ++header "Queries per declaration"
                       ++concatMap showDecl (allDecls fSpec)
    
   where
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