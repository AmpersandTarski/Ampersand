{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.FSpec2SQL
  (dumpSQLqueries,generateDatabaseFile)
where
import           Ampersand.ADL1
import           Ampersand.Basics
import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec
import           Ampersand.FSpec.SQL
import           Ampersand.Misc
import           Ampersand.Prototype.TableSpec
import           Ampersand.Prototype.ProtoUtil(getGenericsDir)
import qualified Data.List.NonEmpty as NEL
import qualified RIO.Text as T
import qualified RIO.List as L
import           System.Directory
import           System.FilePath

generateDatabaseFile :: MultiFSpecs -> RIO App ()
generateDatabaseFile multi = 
   do opts <- view optionsL
      sayWhenLoudLn $ "  Generating "++file
      liftIO $ createDirectoryIfMissing True (takeDirectory (fullFile opts))
      liftIO $ writeFile (fullFile opts) content
  where 
   content = T.unpack (databaseStructureSql multi)
   file = "database" <.> "sql"
   fullFile opts = getGenericsDir opts </> file

databaseStructureSql :: MultiFSpecs -> T.Text
databaseStructureSql multi
   = T.intercalate "\n" $ 
         header (T.pack ampersandVersionStr)
       <>header "Database structure queries"
       <>map (addSeparator . queryAsSQL) (generateDBstructQueries fSpec True) 
   where
     fSpec = userFSpec multi

generateDBstructQueries :: FSpec -> Bool -> [SqlQuery]
generateDBstructQueries fSpec withComment 
  =    concatMap (tableSpec2Queries withComment) ([plug2TableSpec p | InternalPlug p <- plugInfos fSpec])
    <> additionalDatabaseSettings 

dumpSQLqueries :: Options -> MultiFSpecs -> T.Text
dumpSQLqueries opts multi
   = T.intercalate "\n" $ 
         header (T.pack ampersandVersionStr)
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
     fSpec = userFSpec multi
     showInterface :: Interface -> [T.Text]
     showInterface ifc 
        = header ("INTERFACE: "<>T.pack (name ifc))
        <>(map ("  " <>) . showObjDef . ifcObj) ifc
        where 
          showObjDef :: ObjectDef -> [T.Text]
          showObjDef obj
            = (header . T.pack . showA . objExpression) obj
            <>[queryAsSQL . prettySQLQueryWithPlaceholder 2 fSpec . objExpression $ obj]
            <>case objmsub obj of
                 Nothing  -> []
                 Just sub -> showSubInterface sub
            <>header ("Broad query for the object at " <> (T.pack . show . origin) obj)
            <>[T.pack . prettyBroadQueryWithPlaceholder 2 fSpec $ obj]
          showSubInterface :: SubInterface -> [T.Text]
          showSubInterface sub = 
            case sub of 
              Box{} -> concatMap showObjDef [e | BxExpr e <- siObjs sub]
              InterfaceRef{} -> []

     showConjunct :: Conjunct -> [T.Text]
     showConjunct conj 
        = header (T.pack$ rc_id conj)
        <>["/*"
          ,"Conjunct expression:"
          ,"  " <> (T.pack . showA . rc_conjunct $ conj)
          ,"Rules for this conjunct:"]
        <>map showRule (NEL.toList $ rc_orgRules conj)
        <>["*/"
          ,(queryAsSQL . prettySQLQuery 2 fSpec . conjNF opts . notCpl . rc_conjunct $ conj) <> ";"
          ,""]
        where
          showRule r 
            = T.pack ("  - "<>name r<>": "<>showA r)
     showDecl :: Relation -> [T.Text]
     showDecl decl 
        = header (T.pack$ showA decl)
        <>[(queryAsSQL . prettySQLQuery 2 fSpec $ decl)<>";",""]

header :: T.Text -> [T.Text]
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
    spaces :: Int -> T.Text
    spaces i = T.replicate i " "
    firstspaces :: Int
    firstspaces = (width - 6 - l) `quot` 2 
addSeparator :: T.Text -> T.Text
addSeparator t = t <> ";"
