{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Prototype.Generate 
  (generateDBstructQueries, generateAllDefPopQueries
  )
where

import Ampersand.Core.AbstractSyntaxTree 
import Prelude hiding (writeFile,readFile,getContents,exp)
import Data.Monoid
import Data.List
import Data.String (IsString)
import qualified Data.Text as Text
import Ampersand.FSpec
import Ampersand.FSpec.ToFSpec.ADL2Plug(suitableAsKey)
import Ampersand.Prototype.PHP (getTableName, signalTableSpec)

doubleQuote :: (Data.String.IsString m, Monoid m) => m -> m
doubleQuote s = "\"" <> s <> "\""

generateDBstructQueries :: FSpec -> Bool -> [String]
generateDBstructQueries fSpec withComment 
  = (if withComment 
     then intercalate [""]
     else map (unwords . concatMap words)
    ) $ 
      [ [ "CREATE TABLE "<> doubleQuote "__SessionTimeout__"
        , "   ( "<>doubleQuote "SESSION"<>" VARCHAR(255) UNIQUE NOT NULL"
        , "   , "<>doubleQuote "lastAccess"<>" BIGINT NOT NULL"
        , "   ) ENGINE="<>dbEngine
        ]
      , [ "CREATE TABLE "<> doubleQuote "__all_signals__"
        , "   ( "<>doubleQuote "conjId"<>" VARCHAR(255) NOT NULL"
        , "   , "<>doubleQuote "src"<>" VARCHAR(255) NOT NULL"
        , "   , "<>doubleQuote "tgt"<>" VARCHAR(255) NOT NULL"
        , "   ) ENGINE="<>dbEngine
        ] 
      ]<>
      concatMap tableSpec2Queries [plug2TableSpec p | InternalPlug p <- plugInfos fSpec]<>
      [ ["SET TRANSACTION ISOLATION LEVEL SERIALIZABLE"]
      ]
   where 
        tableSpec2Queries :: TableSpec -> [[String]]
        tableSpec2Queries ts = 
         (  (if withComment then tsCmnt ts else [] )
          <>["CREATE TABLE "<>show (tsName ts)] 
          <>zipWith (<>) (" ( " : repeat " , ")
               (map fld2sql (tsflds ts) <> tsKey ts)
            
          <>[" , "<>doubleQuote "ts_insertupdate"<>" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP"]
          <>[" ) ENGINE="<>dbEngine]
         ):
         [ ["CREATE INDEX "<>show (tsName ts<>"_"<>name fld)<>" ON "<>show (tsName ts)<>" ("<>show (name fld)<>")"]
         | fld <- tsflds ts
         , not (isPrimaryKey fld)
         , suitableAsKey (attType  fld)
         ]
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
attributeSpec2Str fs = unwords
                        [ show (fsname fs)
                        , fstype fs
                        , if fsDbNull fs then " DEFAULT NULL" else " NOT NULL"
                        ] 
plug2TableSpec :: PlugSQL -> TableSpec
plug2TableSpec plug 
  = TableSpec 
     { tsCmnt = commentBlockSQL $
                   ["Plug "<>name plug
                   ,""
                   ,"attributes:"
                   ]<> concat
                   [ [showADL (attExpr x)
                     , "  "<>(show.properties.attExpr) x ]
                   | x <- plugAttributes plug
                   ]
     , tsName = name plug
     , tsflds = plugAttributes plug
     , tsKey  = case (plug, (head.plugAttributes) plug) of
                 (BinSQL{}, _)   -> [  "PRIMARY KEY (" 
                                       <> intercalate ", " (map (show . attName) (plugAttributes plug))
                                       <> ")"
                                    | all (suitableAsKey . attType) (plugAttributes plug)
                                    ] 
                 (TblSQL{}, primFld) ->
                      case attUse primFld of
                         PrimaryKey _ -> ["PRIMARY KEY (" <> (show . attName) primFld <> ")" ]
                         ForeignKey c -> fatal 195 ("ForeignKey "<>name c<>"not expected here!")
                         PlainAttr    -> []
     , tsEngn = dbEngine
     }

commentBlockSQL :: [String] -> [String]
commentBlockSQL xs = 
   map (\cmmnt -> "/* "<>cmmnt<>" */") $ hbar <> xs <> hbar
  where hbar = [replicate (maximum . map length $ xs) '-']
  
generateAllDefPopQueries :: FSpec -> [Text.Text]
generateAllDefPopQueries fSpec 
  = fillSignalTable (initialConjunctSignals fSpec) <>
    populateTablesWithPops
        
  where
    fillSignalTable :: [(Conjunct, [AAtomPair])] -> [Text.Text]
    fillSignalTable [] = []
    fillSignalTable conjSignals 
     = [Text.unlines
            [ "INSERT INTO "<>Text.pack (show (getTableName signalTableSpec))
            , "   ("<>Text.intercalate ", " (map (Text.pack . doubleQuote) ["conjId","src","tgt"])<>")"
            , "VALUES " <> Text.intercalate " , " 
                  [ "(" <>Text.intercalate ", " [showAsValue (rc_id conj), showValPHP (apLeft p), showValPHP (apRight p)]<> ")" 
                  | (conj, viols) <- conjSignals
                  , p <- viols
                  ]
            ]
       ]
    populateTablesWithPops :: [Text.Text]
    populateTablesWithPops =
      concatMap populatePlug [p | InternalPlug p <- plugInfos fSpec]
      where
        populatePlug :: PlugSQL -> [Text.Text]
        populatePlug plug 
          = case tableContents fSpec plug of
             []  -> []
             tblRecords 
                 -> [Text.unlines
                       [ "INSERT INTO "<>Text.pack (show (name plug))
                       , "   ("<>Text.intercalate ", " (map (Text.pack . show . attName) (plugAttributes plug))<>") "
                       , "VALUES " <> Text.intercalate " , " 
                          [ "(" <>valuechain md<> ")" | md<-tblRecords]
                       ]
                    ]
         where
           valuechain record 
             = Text.intercalate ", " 
                 [case att of 
                    Nothing -> "NULL"
                    Just val -> showValPHP val
                 | att <- record ]

showAsValue :: String -> Text.Text
showAsValue str = Text.pack ("'"<>f str<>"'")
  where f :: String -> String
        f str'= 
          case str' of
            []        -> []
            ('\'':cs) -> "\\\'"<> f cs  --This is required to ensure that the result of showValue will be a proper singlequoted string.
            (c:cs)    -> c : f cs

dbEngine :: String
dbEngine = "InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN"