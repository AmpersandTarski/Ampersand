{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.FSpec2SQL
  (dumpSQLqueries)
where
import Ampersand.Basics
import Ampersand.Prototype.Generate 
  (generateDBstructQueries, generateInitialPopQueries
  )
import Ampersand.FSpec
import Ampersand.FSpec.SQL
import Data.Monoid
import qualified Data.Text as Text

dumpSQLqueries :: MultiFSpecs -> Text.Text
dumpSQLqueries multi
   = Text.intercalate "\n" $ 
         header (Text.pack ampersandVersionStr)
       <>header "Database structure queries"
       <>map Text.pack (generateDBstructQueries fSpec True)
       <>header "Initial population queries"
       <>generateInitialPopQueries fSpec
       <>header "Violations of conjuncts"
       <>concatMap showConjunct (allConjuncts fSpec)
       <>header "Queries per declaration"
       <>concatMap showDecl (vrels fSpec)
       <>header "Queries of interfaces"
       <>concatMap showInterface (interfaceS fSpec <> interfaceG fSpec)
    
   where
     fSpec = userFSpec multi
     showInterface :: Interface -> [Text.Text]
     showInterface ifc 
        = header ("INTERFACE: "<>Text.pack (name ifc))
        <>(map ((<>) "  ") . showObjDef . ifcObj) ifc
        where 
          showObjDef :: ObjectDef -> [Text.Text]
          showObjDef obj
            = (header . Text.pack . showADL . objctx) obj
            <>[Text.pack$ (prettySQLQueryWithPlaceholder 2 fSpec . objctx) obj]
            <>case objmsub obj of
                 Nothing  -> []
                 Just sub -> showSubInterface sub
            <>header "Broad query of above stuff"     
            <>[Text.pack$ prettyBroadQueryWithPlaceholder 2 fSpec $ obj]
          showSubInterface :: SubInterface -> [Text.Text]
          showSubInterface sub = 
            case sub of 
              Box{} -> concatMap showObjDef . siObjs $ sub
              InterfaceRef{} -> []

     showConjunct :: Conjunct -> [Text.Text]
     showConjunct conj 
        = header (Text.pack$ rc_id conj)
        <>["Rules for this conjunc:"]
        <>map showRule (rc_orgRules conj)
        <>[Text.pack$ prettySQLQuery 2 fSpec . conjNF (getOpts fSpec) . notCpl . rc_conjunct $ conj,""]
        where
          showRule r 
            = Text.pack ("  - "<>name r<>": "<>showADL r)
     showDecl :: Declaration -> [Text.Text]
     showDecl decl 
        = header (Text.pack$ showADL decl)
        <>[Text.pack$ prettySQLQuery 2 fSpec $ decl,""]
     header :: Text.Text -> [Text.Text]
     header title = 
         [ ""
         , Text.replicate width "*"
         , "***"<>spaces firstspaces<>title<>spaces (width-6-firstspaces-l)<>"***"
         , Text.replicate width "*"
         , ""
         ]
       where width = maximum [80 , l + 8]
             l = Text.length title
             spaces :: Int -> Text.Text
             spaces i = Text.replicate i " "
             firstspaces :: Int
             firstspaces = (width - 6 - l) `quot` 2 