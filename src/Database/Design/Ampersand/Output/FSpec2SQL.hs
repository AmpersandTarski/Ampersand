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
                       ++generateDBstructQueries fSpec
                       ++header "Initial population queries"
                       ++generateAllDefPopQueries fSpec
                       ++header "Violations of conjuncts"
                       ++concatMap showConjunct (vconjs fSpec)
                       ++header "Queries per declaration"
                       ++concatMap showDecl (allDecls fSpec)
    
   where
     showConjunct :: Conjunct -> [String]
     showConjunct conj 
        = header (rc_id conj)
        ++(lines . prettySQLQuery fSpec 0 . conjNF (getOpts fSpec) . notCpl . rc_conjunct $ conj)
        ++[""]
     showDecl :: Declaration -> [String]
     showDecl decl 
        = header (showADL decl)
        ++(lines . prettySQLQuery fSpec 0 $ decl)
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