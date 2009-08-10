module Data.Plug (Plug(..)
                 ,SqlField(..)
                 ,SqlDb(..)
                 ,SqlType(..)
                 ,showSQL
                 ,PhpValue(..)
                 ,PhpType(..)
                 ,PhpArgs
                 ,PhpReturn(..)
                 ,PhpAction(..)
                 ,ActionType(..))
where
--  import CommonClasses  (Identified(..))
--  import Classes.Morphical (Morphical(..))
--  import Collection(rd)
  import Adl
  --import MorphismAndDeclaration
  
  data Plug = PlugSql { fields   :: [SqlField]
                      , database :: SqlDb
                      , plname   :: String
                      }
            | PlugPhp { args     :: PhpArgs
                      , returns  :: PhpReturn
                      , function :: PhpAction
                      , phpfile  :: String
                      , plname   :: String 
                      } deriving (Show)

  data PhpValue = PhpNull | PhpObject {object::ObjectDef,phptype::PhpType} deriving (Show)
  data PhpType = PhpString | PhpInt | PhpFloat | PhpArray deriving (Show)
  type PhpArgs = [(Int,PhpValue)]
  data PhpReturn = PhpReturn {retval::PhpValue} deriving (Show)
  --DO you need on::[Morphism]? makeFspec sets an empty list
  data PhpAction = PhpAction {action::ActionType, on::[Morphism]} deriving (Show)
  data ActionType = Create | Read | Update | Delete deriving (Show)
  
  instance Identified PhpValue where
    typ _ = "PhpValue"
    name p = case p of {PhpNull -> "0"; PhpObject{object=x} -> name x}

  --DESCR -> plugs are sorted to optimize some algoritms. 
  instance Eq Plug where
    x==y = plname x==plname y
  instance Ord Plug where
    compare x y = compare (plname x) (plname y)
  
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
              { PlugSql{} -> "SQL";
                PlugPhp{} -> "PHP"
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
    
