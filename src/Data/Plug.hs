module Data.Plug (Plug(..),SqlField(..),SqlDb(..),SqlType(..),showSQL)
where
  import CommonClasses  (Identified(..))
  import Classes.Morphical (Morphical(..))
  import Collection(rd)
  import Adl
  --import MorphismAndD
  
  data Plug = PlugSql { fields   :: [SqlField]
                      , database :: SqlDb
                      , plname:: String
                      } deriving (Eq, Ord, Show)
  
  data SqlField = Fld { fldname     :: String
                      , fldexpr     :: Expression
                      , fldtype     :: SqlType
                      , fldnull     :: Bool -- can there be empty field-values?
                      , flduniq     :: Bool -- are all field-values unique?
                      } deriving (Eq, Show)
  instance Ord SqlField where
    compare a b = compare (fldname a) (fldname b)
  
  data SqlType = SQLChar    Int
               | SQLBlob              -- cannot compare, but can show
               | SQLPass              -- password, encrypted: cannot show, but can compare
               | SQLSingle  
               | SQLDouble  
               | SQLuInt    Int
               | SQLsInt    Int
               | SQLId                -- autoincrement integer
               | SQLVarchar Int
               | SQLEval    [String]
               | SQLBool              -- exists y/n
               deriving (Eq, Ord,Show)
  
  showSQL :: SqlType -> String
  showSQL (SQLChar    n) = "CHAR("++show n++")"
  showSQL (SQLBlob     ) = "BLOB"
  showSQL (SQLPass     ) = "VARCHAR(255)"
  showSQL (SQLSingle   ) = "FLOAT"
  showSQL (SQLDouble   ) = "FLOAT"
  showSQL (SQLuInt    n) = "INT("++show n++") UNSIGNED"
  showSQL (SQLsInt    n) = "INT("++show n++")"
  showSQL (SQLId       ) = "INT"
  showSQL (SQLVarchar n) = "VARCHAR("++show n++")"
  showSQL (SQLBool     ) = "BOOLEAN"
          
  data SqlDb = Db DbHost String
             | LocalDb String
             | CurrentDb
             deriving (Eq, Ord,Show)
          
  data DbHost = Host  { dbhost :: String
                      , dbuser :: String
                      , dbpass :: String}
                deriving(Eq, Ord,Show)
  
  dbName :: SqlDb -> Maybe String
  dbName (Db _ n)       = Just n
  dbName (LocalDb n)    = Just n
  dbName CurrentDb      = Nothing
  dbHost :: SqlDb -> Maybe DbHost
  dbHost (Db host _)    = Just host
  dbHost _              = Nothing
  
  instance Identified Plug where
    name p = plname p
    typ  p = case p of
              { PlugSql{} -> "SQL"
              }

  instance Morphical SqlField where
    concs        f = [target e'|let e'=fldexpr f,Sur `elem` (multiplicities e')]
    conceptDefs  f = conceptDefs   (fldexpr f)
    mors         f = map makeInline (mors (fldexpr f))
    morlist      f = morlist       (fldexpr f)
    declarations f = declarations  (fldexpr f)
    closExprs    f = closExprs     (fldexpr f)
    objDefs      f = objDefs       (fldexpr f)
    keyDefs      f = keyDefs       (fldexpr f)
    
  instance Morphical Plug where
    concs        p@PlugSql{} = concs        (fields p) --rd [target e'|e'<-(map fldexpr (fields p)),Sur `elem` (multiplicities e')]
    conceptDefs  p@PlugSql{} = conceptDefs  (fields p)
    mors         p@PlugSql{} = mors         (fields p)
    morlist      p@PlugSql{} = morlist      (fields p)
    declarations p@PlugSql{} = declarations (fields p)
    closExprs    p@PlugSql{} = closExprs    (fields p)
    objDefs      p@PlugSql{} = objDefs      (fields p)
    keyDefs      p@PlugSql{} = keyDefs      (fields p)
    
