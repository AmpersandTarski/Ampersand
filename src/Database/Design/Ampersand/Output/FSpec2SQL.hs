module Database.Design.Ampersand.Output.FSpec2SQL
  (dumpSQLqueries)
where
import Database.Design.Ampersand.Prototype.Generate 
  (generateDBstructQueries, generateAllDefPopQueries
  )
import Database.Design.Ampersand.FSpec
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.Basics (fatal,name)
import Data.List

dumpSQLqueries :: FSpec -> String
dumpSQLqueries fSpec = intercalate "\n" $ 
                         header "Database structure queries"
                       ++generateDBstructQueries fSpec
                       ++header "Initial population queries"
                       ++generateAllDefPopQueries fSpec

    
   where
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