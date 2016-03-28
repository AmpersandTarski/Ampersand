module Database.Design.Ampersand.Prototype.Generate 
  (generateDBstructQueries, generateAllDefPopQueries
  )
where

import Database.Design.Ampersand.Core.AbstractSyntaxTree 
import Prelude hiding (writeFile,readFile,getContents,exp)
import Data.List
import Database.Design.Ampersand.FSpec
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.Basics (fatal,name)
import Database.Design.Ampersand.Prototype.PHP (getTableName, signalTableSpec)

        
generateDBstructQueries :: FSpec -> [String]
generateDBstructQueries fSpec = theSQLstatements
  where
    theSQLstatements :: [String]
    theSQLstatements =
       createTableStatements ++
       [ "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE"
       ]
    createTableStatements :: [String]
    createTableStatements = 
      map concat
      [ [ "CREATE TABLE "++ show "__SessionTimeout__"
        , "   ( "++show "SESSION"++" VARCHAR(255) UNIQUE NOT NULL"
        , "   , "++show "lastAccess"++" BIGINT NOT NULL"
        , "   ) ENGINE=InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN"
        ]
      , [ "CREATE TABLE "++ show "__History__"
        , "   ( "++show "Seconds"++" VARCHAR(255) DEFAULT NULL"
        , "   , "++show "Date"++" VARCHAR(255) DEFAULT NULL"
        , "   ) ENGINE=InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN"
        ]
      , [ "INSERT INTO "++show "__History__"++" ("++show "Seconds"++","++show "Date"++")"
        , "   VALUES (UNIX_TIMESTAMP(NOW(6)), NOW(6))"
        ]
      , [ "CREATE TABLE "++ show "__all_signals__"
        , "   ( "++show "conjId"++" VARCHAR(255) NOT NULL"
        , "   , "++show "src"++" VARCHAR(255) NOT NULL"
        , "   , "++show "tgt"++" VARCHAR(255) NOT NULL"
        , "   ) ENGINE=InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN"
        ]
      ] ++ 
      ( concatMap tableSpec2Queries [(plug2TableSpec p) | InternalPlug p <- plugInfos fSpec])
     
      where 
        tableSpec2Queries :: TableSpecNew -> [String]
        tableSpec2Queries ts = 
         -- [ "DROP TABLE "++show (tsName ts)] ++
          [ concat $  
                   ( tsCmnt ts ++ 
                     ["CREATE TABLE "++show (tsName ts)] 
                     ++ (map (uncurry (++)) 
                            (zip (" ( ": repeat " , " ) 
                                 (  map fld2sql (tsflds ts)
                                 ++ tsKey ts
                                 )
                            )
                        )
                     ++ [" , "++show "ts_insertupdate"++" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP"]
                     ++ [" ) ENGINE=InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN"]
                   )
          ]
        fld2sql :: SqlAttribute -> String
        fld2sql = attributeSpec2Str . fld2AttributeSpec

data TableSpecNew 
  = TableSpec { tsCmnt :: [String]
              , tsName :: String
              , tsflds :: [SqlAttribute]
              , tsKey ::  [String]
              , tsEngn :: String
              }
data AttributeSpecNew
  = AttributeSpec { fsname :: String
                  , fstype :: String
                  , fsDbNull :: Bool
                  }
fld2AttributeSpec ::SqlAttribute -> AttributeSpecNew
fld2AttributeSpec att 
  = AttributeSpec { fsname = name att
                  , fstype = showSQL (attType att)
                  , fsDbNull = attDBNull att 
                  }
attributeSpec2Str :: AttributeSpecNew -> String
attributeSpec2Str fs = intercalate " "
                        [ show (fsname fs)
                        , fstype fs
                        , if fsDbNull fs then " DEFAULT NULL" else ""
                        ] 
plug2TableSpec :: PlugSQL -> TableSpecNew
plug2TableSpec plug 
  = TableSpec 
     { tsCmnt = commentBlockSQL (["Plug "++name plug,"","attributes:"]++map (\x->showADL (attExpr x)++"  "++(show.properties.attExpr) x) (plugAttributes plug))
     , tsName = name plug
     , tsflds = plugAttributes plug
     , tsKey  = case (plug, (head.plugAttributes) plug) of
                 (BinSQL{}, _)   -> []
                 (_,    primFld) ->
                      case attUse primFld of
                         TableKey isPrim _ -> [ (if isPrim then "PRIMARY " else "")
                                                ++ "KEY ("++(show . attName) primFld++")"
                                        ]
                         ForeignKey c  -> fatal 195 ("ForeignKey "++name c++"not expected here!")
                         PlainAttr     -> []
     , tsEngn = "InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN"
     }

commentBlockSQL :: [String] -> [String]
commentBlockSQL xs = 
   map (\cmmnt -> "/* "++cmmnt++" */") $ hbar ++ xs ++ hbar
  where hbar = [replicate (maximum . map length $ xs) '-']
  
generateAllDefPopQueries :: FSpec -> [String]
generateAllDefPopQueries fSpec = theSQLstatements
  where
    theSQLstatements
      = fillSignalTable (initialConjunctSignals fSpec) ++
        populateTablesWithPops
        

    fillSignalTable :: [(Conjunct, [AAtomPair])] -> [String]
    fillSignalTable [] = []
    fillSignalTable conjSignals 
     = [concat $ 
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
                 -> [concat $ 
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
