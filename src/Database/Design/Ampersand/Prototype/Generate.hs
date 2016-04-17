module Database.Design.Ampersand.Prototype.Generate 
  (generateDBstructQueries, generateAllDefPopQueries
  )
where

import Database.Design.Ampersand.Core.AbstractSyntaxTree 
import Prelude hiding (writeFile,readFile,getContents,exp)
import Data.List
import Database.Design.Ampersand.FSpec
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.Prototype.PHP (getTableName, signalTableSpec)

        
generateDBstructQueries :: FSpec -> Bool -> [String]
generateDBstructQueries fSpec withComment 
  = (if withComment then id else singleton . unwords . concatMap words) $ 
      [ "CREATE TABLE "++ show "__SessionTimeout__"
      , "   ( "++show "SESSION"++" VARCHAR(255) UNIQUE NOT NULL"
      , "   , "++show "lastAccess"++" BIGINT NOT NULL"
      , "   ) ENGINE="++dbEngine
      , ""
      , "CREATE TABLE "++ show "__History__"
      , "   ( "++show "Seconds"++" VARCHAR(255) DEFAULT NULL"
      , "   , "++show "Date"++" VARCHAR(255) DEFAULT NULL"
      , "   ) ENGINE="++dbEngine
      , ""
      , "INSERT INTO "++show "__History__"++" ("++show "Seconds"++","++show "Date"++")"
      , "   VALUES (UNIX_TIMESTAMP(NOW(6)), NOW(6))"
      , ""
      , "CREATE TABLE "++ show "__all_signals__"
      , "   ( "++show "conjId"++" VARCHAR(255) NOT NULL"
      , "   , "++show "src"++" VARCHAR(255) NOT NULL"
      , "   , "++show "tgt"++" VARCHAR(255) NOT NULL"
      , "   ) ENGINE="++dbEngine
      ] ++ 
      ( concatMap tableSpec2Queries [(plug2TableSpec p) | InternalPlug p <- plugInfos fSpec])++
      [ "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE"
      ]
   where 
        singleton a = [a]
        tableSpec2Queries :: TableSpec -> [String]
        tableSpec2Queries ts = 
            (if withComment then tsCmnt ts else [] )
          ++["CREATE TABLE "++show (tsName ts)] 
          ++(map (uncurry (++)) 
                (zip (" ( ": repeat " , " ) 
                     (  map fld2sql (tsflds ts)
                     ++ tsKey ts
                     )
                )
            )
          ++[" , "++show "ts_insertupdate"++" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP"]
          ++[" ) ENGINE="++dbEngine]
        fld2sql :: SqlAttribute -> String
        fld2sql = attributeSpec2Str . fld2AttributeSpec

data TableSpec
  = TableSpec { tsCmnt :: [String]
              , tsName :: String
              , tsflds :: [SqlAttribute]
              , tsKey ::  [String]
              , tsEngn :: String
              }
data AttributeSpec
  = AttributeSpec { fsname :: String
                  , fstype :: String
                  , fsDbNull :: Bool
                  }
fld2AttributeSpec ::SqlAttribute -> AttributeSpec
fld2AttributeSpec att 
  = AttributeSpec { fsname = name att
                  , fstype = showSQL (attType att)
                  , fsDbNull = attDBNull att 
                  }
attributeSpec2Str :: AttributeSpec -> String
attributeSpec2Str fs = intercalate " "
                        [ show (fsname fs)
                        , fstype fs
                        , if fsDbNull fs then " DEFAULT NULL" else " NOT NULL"
                        ] 
plug2TableSpec :: PlugSQL -> TableSpec
plug2TableSpec plug 
  = TableSpec 
     { tsCmnt = commentBlockSQL $
                   ["Plug "++name plug
                   ,""
                   ,"attributes:"
                   ]++ concat
                   [ [showADL (attExpr x)
                     , "  "++(show.properties.attExpr) x ]
                   | x <- plugAttributes plug
                   ]
     , tsName = name plug
     , tsflds = plugAttributes plug
     , tsKey  = case (plug, (head.plugAttributes) plug) of
                 (BinSQL{}, _)   -> []
                 (TblSQL{}, primFld) ->
                      case attUse primFld of
                         TableKey isPrim _ -> if isPrim then ["PRIMARY " ++ "KEY ("++(show . attName) primFld++")"] else []
                         ForeignKey c  -> fatal 195 ("ForeignKey "++name c++"not expected here!")
                         PlainAttr     -> []
     , tsEngn = dbEngine
     }

commentBlockSQL :: [String] -> [String]
commentBlockSQL xs = 
   map (\cmmnt -> "/* "++cmmnt++" */") $ hbar ++ xs ++ hbar
  where hbar = [replicate (maximum . map length $ xs) '-']
  
generateAllDefPopQueries :: FSpec -> Bool -> [String]
generateAllDefPopQueries fSpec withComment 
  = (if withComment then id else map (unwords . words)) $ 
        fillSignalTable (initialConjunctSignals fSpec) ++
        populateTablesWithPops
        
  where
    fillSignalTable :: [(Conjunct, [AAtomPair])] -> [String]
    fillSignalTable [] = []
    fillSignalTable conjSignals 
     = [unlines $ 
            [ "INSERT INTO "++show (getTableName signalTableSpec)
            , "   ("++intercalate ", " (map show ["conjId","src","tgt"])++")"
            ] ++ lines 
              ( "VALUES " ++ intercalate "\n     , " 
                  [ "(" ++intercalate ", " (map showAsValue [rc_id conj, showValPHP (apLeft p), showValPHP (apRight p)])++ ")" 
                  | (conj, viols) <- conjSignals
                  , p <- viols
                  ]
              )
       ]
    populateTablesWithPops :: [String]
    populateTablesWithPops =
      concatMap populatePlug [p | InternalPlug p <- plugInfos fSpec]
      where
        populatePlug :: PlugSQL -> [String]
        populatePlug plug 
          = case tableContents fSpec plug of
             []  -> []
             tblRecords 
                 -> [unlines $ 
                       [ "INSERT INTO "++show (name plug)
                       , "   ("++intercalate ", " (map (show . attName) (plugAttributes plug))++") "
                       ] ++ lines
                         ( "VALUES " ++ intercalate "\n     , " 
                          [ "(" ++valuechain md++ ")" | md<-tblRecords]
                         )
                    ]
         where
           valuechain record 
             = intercalate ", " 
                 [case att of 
                    Nothing -> "NULL"
                    Just val -> showValPHP val
                 | att <- record ]

showAsValue :: String -> String
showAsValue str = "'"++f str++"'"
  where f :: String -> String
        f str'= 
          case str' of
            []        -> []
            ('\'':cs) -> "\\\'"++ f cs  --This is required to ensure that the result of showValue will be a proper singlequoted string.
            (c:cs)    -> c : f cs

dbEngine :: String
dbEngine = "InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN"