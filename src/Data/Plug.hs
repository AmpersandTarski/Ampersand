module Data.Plug (Plug(..),SqlField,plugs)
where
  import Adl.Expression (Expression(..))
  import Adl.Concept    (Association(target))
  import CommonClasses  (Identified(..))
  import Data.Fspec
  import Adl.ObjectDef  (ObjectDef(..))
  
  data Plug = PlugSql { fields   :: [SqlField]
                      , database :: SqlDb
                      , plname:: String
                      } deriving (Eq, Show)
                      
  data SqlField = Fld { fldname     :: String
                      , fldexpr     :: Expression
                      , fldtype     :: SqlType
                      } deriving (Eq, Show)
                      
  data SqlType = SQLChar Int
               | SQLBlob -- cannot compare, but can show
               | SQLPass -- password, encrypted: cannot show, but can compare
               | SQLSingle
               | SQLDouble
               | SQLuInt Int
               | SQLsInt Int
               | SQLId -- autoincrement integer
               | SQLVarchar Int
               | SQLEval [String]
               deriving (Eq, Show)
               
  data SqlDb = Db DbHost String
             | LocalDb String
             | CurrentDb
             deriving (Eq,Show)
             
  data DbHost = Host  { dbhost :: String
                      , dbuser :: String
                      , dbpass :: String}
                deriving(Eq,Show)
                
  dbName :: SqlDb -> Maybe String
  dbName (Db _ n)       = Just n
  dbName (LocalDb n)    = Just n
  dbName CurrentDb      = Nothing
  dbHost :: SqlDb -> Maybe DbHost
  dbHost (Db host _)    = Just host
  dbHost _              = Nothing
  
  plugs :: Fspc -> [Plug]
  plugs spc = map makeSQLPlug (datasets spc)
    where
      makeSQLPlug ds
        = PlugSql { fields = getfields ds,
                    database = CurrentDb,
                    plname = objnm ds
                  }
  
  getfields :: ObjectDef -> [SqlField]
  getfields ds = (Fld { fldname=mname, fldexpr=expr, fldtype=ftype})
                      : (concat (map getfields (objats ds)))
    where mname = name (target expr) -- todo: make unique
          ftype = case name (target expr) of
                   {"BLOB" -> SQLBlob;
                    "PASS" -> SQLPass;
                    _      -> SQLId
                   } 
          expr  = objctx ds
  
  instance Identified Plug where
    name p = plname p
    typ  p = case p of
              { PlugSql{} -> "SQL"
              }