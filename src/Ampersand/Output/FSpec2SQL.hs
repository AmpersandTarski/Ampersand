{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.FSpec2SQL
  (dumpSQLqueries,databaseStructureSql)
where
import           Ampersand.ADL1
import           Ampersand.Basics
import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec
import           Ampersand.FSpec.SQL
import           Ampersand.Prototype.TableSpec
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import qualified RIO.List as L

databaseStructureSql :: FSpec -> Text
databaseStructureSql fSpec
   = T.intercalate "\n" $ 
         header ampersandVersionStr
       <>header "Database structure queries"
       <>map (addSeparator . queryAsSQL) (generateDBstructQueries fSpec True) 


generateDBstructQueries :: FSpec -> Bool -> [SqlQuery]
generateDBstructQueries fSpec withComment 
  =    concatMap (tableSpec2Queries withComment) ([plug2TableSpec p | InternalPlug p <- plugInfos fSpec])
    <> additionalDatabaseSettings 

dumpSQLqueries :: env -> FSpec -> Text
dumpSQLqueries env fSpec
   = T.intercalate "\n" $ 
         header ampersandVersionStr
       <>header "Database structure queries"
       <>map (addSeparator . queryAsSQL) (generateDBstructQueries fSpec True) 
       <>header "Violations of conjuncts"
       <>concatMap showConjunct (allConjuncts fSpec)
       <>header "Queries per relation"
       <>concatMap showDecl (vrels fSpec)
       <>header "Queries of interfaces"
       <>concatMap showInterface y 
   where
     y :: [Interface]
     y = interfaceS fSpec <> interfaceG fSpec
     showInterface :: Interface -> [Text]
     showInterface ifc 
        = header ("INTERFACE: "<>name ifc)
        <>(map ("  " <>) . showObjDef . ifcObj) ifc
        where 
          showObjDef :: ObjectDef -> [Text]
          showObjDef obj
            = (header . showA . objExpression) obj
            <>[queryAsSQL . prettySQLQueryWithPlaceholder 2 fSpec . objExpression $ obj]
            <>case objmsub obj of
                 Nothing  -> []
                 Just sub -> showSubInterface sub
            <>header ("Broad query for the object at " <> (T.pack . show . origin) obj)
            <>[prettyBroadQueryWithPlaceholder 2 fSpec $ obj]
          showSubInterface :: SubInterface -> [Text]
          showSubInterface sub = 
            case sub of 
              Box{} -> concatMap showObjDef [e | BxExpr e <- siObjs sub]
              InterfaceRef{} -> []

     showConjunct :: Conjunct -> [Text]
     showConjunct conj 
        = header (rc_id conj)
        <>["/*"
          ,"Conjunct expression:"
          ,"  " <> (showA . rc_conjunct $ conj)
          ,"Rules for this conjunct:"]
        <>map showRule (NE.toList $ rc_orgRules conj)
        <>["*/"
          ,(queryAsSQL . prettySQLQuery 2 fSpec . conjNF env . notCpl . rc_conjunct $ conj) <> ";"
          ,""]
        where
          showRule r 
            = "  - "<>name r<>": "<>showA r
     showDecl :: Relation -> [Text]
     showDecl decl 
        = header (showA decl)
        <>[(queryAsSQL . prettySQLQuery 2 fSpec $ decl)<>";",""]

header :: Text -> [Text]
header title = 
    [ "/*"
    , T.replicate width "*"
    , "***"<>spaces firstspaces<>title<>spaces (width-6-firstspaces-l)<>"***"
    , T.replicate width "*"
    , "*/"
    ]
  where 
    width = case L.maximumMaybe [80 , l + 8] of
              Nothing -> fatal "Impossible"
              Just x  -> x
    l = T.length title
    spaces :: Int -> Text
    spaces i = T.replicate i " "
    firstspaces :: Int
    firstspaces = (width - 6 - l) `quot` 2 
addSeparator :: Text -> Text
addSeparator t = t <> ";"
