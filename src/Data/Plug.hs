{-# OPTIONS_GHC -Wall #-}  
module Data.Plug (Plug(..),Plugs
                 ,SqlField(..)
                 ,SqlType(..)
                 ,showSQL
                 ,field
                 ,PhpValue(..)
                 ,PhpType(..)
                 ,PhpArgs
                 ,PhpReturn(..)
                 ,PhpAction(..)
                 ,iskey
                 ,ActionType(..)
                 ,isClass
                 ,isScalar
                 ,isBinary
                 ,IsPlug(..),PlugSQL(..),PlugPHP(..),plname,plfpa,renamePlug)
where
  import Adl.Concept (Concept(..),Association(..),Signaling(..))
  import Adl.MorphismAndDeclaration
  import Adl.Expression (Expression(..))
  import Adl.ObjectDef (ObjectDef(..))
  import Adl.FilePos (FilePos(..))
  import Adl (isSur)
  import Classes.Object (Object(..))
  import Classes.Morphical (Morphical(..))
  import CommonClasses (Identified(..))
  import FPA (FPA(..))
  import Maybe (fromMaybe)
  import Auxiliaries (sort')
  import Prototype.CodeVariables (CodeVar(..))
  
  class IsPlug a where
    makePlug :: a -> Plug
    pickTypedPlug :: [Plug] -> [a]
  
  instance IsPlug Plug where
    makePlug x = x
    pickTypedPlug x = x
  instance IsPlug PlugSQL where
    makePlug x = PlugSql x
    pickTypedPlug x = [p | (PlugSql p) <- x]
  instance IsPlug PlugPHP where
    makePlug x = PlugPhp x
    pickTypedPlug x = [p | (PlugPhp p) <- x]
  
  plfpa :: (IsPlug a) => a -> FPA
  plfpa p = case (makePlug p) of
             PlugSql x -> sqlfpa x
             PlugPhp x -> phpfpa x
  plname :: (IsPlug a) => a -> String
  plname p = case (makePlug p) of
              PlugSql x -> sqlname x
              PlugPhp x -> phpname x
  renamePlug :: (IsPlug a) => a -> String -> a
  renamePlug p nname
   = case (makePlug p) of
              PlugSql x -> head$ pickTypedPlug [PlugSql (x{sqlname=nname})]
              PlugPhp x -> head$ pickTypedPlug [PlugPhp (x{phpname=nname})]
  
  type Plugs = [Plug]
  data Plug = PlugSql PlugSQL | PlugPhp PlugPHP deriving (Show,Eq)
  data PlugSQL
   = PlugSQL { sqlname   :: String
             , fields    :: [SqlField]
             , cLkpTbl   :: [(Concept,SqlField)]           -- lookup table that links concepts to column names in the plug
             , mLkpTbl   :: [(Morphism,SqlField,SqlField)] -- lookup table that links concepts to column names in the plug
             , sqlfpa    :: FPA -- ^ functie punten analyse
             } deriving (Show)
  data PlugPHP
   = PlugPHP { phpname   :: String       -- ^ the name of the function
             , phpfile	 :: Maybe String -- ^ the file in which the plug is located (Nothing means it is built in already)
             , phpinArgs :: [CodeVar]    -- ^ the input of this plug (list of arguments)
             , phpOut    :: CodeVar      -- ^ the output of this plug. When the input does not exist, the function should return false instead of an object of this type
             , phpSafe   :: Bool         -- ^ whether the input of this plug is verified. False means that the function can be called with non-existant input, such that it does not return false as output or causes undesired side effects
             , phpfpa    :: FPA          -- ^ functie punten analyse
             }
               deriving (Show)
  
  instance Object PlugSQL where
   concept p = case p of
     PlugSQL{mLkpTbl = []} -> error ("!Fatal (module Data.Plug 48): empty lookup table for plug "++plname p++".")
     PlugSQL{}             -> head [source m|(m,_,_)<-mLkpTbl p]
-- Usually source a==concept p. Otherwise, the attribute computation is somewhat more complicated. See ADL2Fspec for explanation about kernels.
   attributes p
    = [ Obj (fldname tFld)                                                   -- objnm 
            Nowhere                                                          -- objpos
            (if source a==concept p then Tm a (-1) else f (source a) [[a]])  -- objctx
            Nothing                                                          -- objctx_proof
            [] []                                                            -- objats and objstrs
      | (a,_,tFld)<-mLkpTbl p]
      where
       f c mms = if null stop                                     -- a path from c to a is not found (yet)
                 then f c mms'                                    -- so add another step to the recursion
                 else F [Tm m (-1)| m<-head (sort' length stop)]  -- pick the shortest path and turn it into an expression.
                 where
                   mms' = [a:ms | ms<-mms, (a,_,_)<-mLkpTbl p, target a==source (head ms)]
                   stop = [ms | ms<-mms', source (head ms)==c]  -- contains all found paths from c to a 
   ctx p = Tm (mIs (concept p)) (-1)
   populations p = error ("!TODO (module Data.Plug 42): evaluate population of plug "++plname p++".")

  
  isClass  :: PlugSQL -> Bool
  isClass  p = case p of
      PlugSQL{} -> not (null [1::Int|fld<-fields p, flduniq fld]) && not (null [1::Int|fld<-fields p, not (flduniq fld)])

  isBinary :: PlugSQL -> Bool
  isBinary p = case p of
      PlugSQL{} -> length (fields p)==2 && null [fld|fld<-fields p, flduniq fld]

  isScalar :: PlugSQL -> Bool
  isScalar p = case p of
      PlugSQL{} -> length (fields p)<=1 && null [fld|fld<-fields p, flduniq fld]

  instance Association PlugSQL where
     source p           = (source . fldexpr . head . fields) p
     target p | isBinary p = target m where (m,_,_) = head (mLkpTbl p)
     target p | otherwise  = error ("!Fatal (module Data/Plug 77): cannot compute the target of plug "++plname p++", because it is not binary.")

  instance Signaling PlugSQL where
   isSignal p | isBinary p = isSignal m where (m,_,_) = head (mLkpTbl p)
   isSignal _              = False



  data PhpValue = PhpNull | PhpObject {objectdf::ObjectDef,phptype::PhpType} deriving (Show)
  data PhpType = PhpString | PhpInt | PhpFloat | PhpArray deriving (Show)
  type PhpArgs = [(Int,PhpValue)]
  data PhpReturn = PhpReturn {retval::PhpValue} deriving (Show)
  --DO you need on::[Morphism]? makeFspec sets an empty list
  data PhpAction = PhpAction {action::ActionType, on::[Morphism]} deriving (Show)
  data ActionType = Create | Read | Update | Delete deriving (Show)

  field :: String->Expression->(Maybe SqlType)->Bool->Bool->SqlField
  field nm expr maybeTp nul uniq = Fld { fldname = nm
                                       , fldexpr = expr
                                       , fldtype = typ
                                       , fldnull = nul
                                       , flduniq = uniq
                                       , fldauto = (typ==SQLId) && not nul && uniq && isIdent expr
                                       }
                             where typ = fromMaybe (fldtyp (target expr)) maybeTp
  
  instance Identified PhpValue where
     name p = case p of {PhpNull -> "0"; PhpObject{objectdf=x} -> objnm x}

  --DESCR -> plugs are sorted to optimize some algoritms. 
  instance Eq PlugSQL where
    x==y = plname x==plname y
  instance Eq PlugPHP where
    x==y = plname x==plname y && phpfile x == phpfile y && phpinArgs x == phpinArgs y
  instance Ord Plug where -- WAAROM (SJ) Waarom is Plug een instance van Ord?
    compare x y = compare (plname x) (plname y)
  
  data SqlField = Fld { fldname     :: String
                      , fldexpr     :: Expression
                      , fldtype     :: SqlType
                      , fldnull     :: Bool -- can there be empty field-values?
                      , flduniq     :: Bool -- are all field-values unique?
                      , fldauto     :: Bool -- is the field auto increment?
                      } deriving (Eq, Show)

  instance Ord SqlField where
    compare a b = compare (fldname a) (fldname b)
  
  data SqlType = SQLChar    Int
               | SQLBlob              -- cannot compare, but can show (as a file)
               | SQLPass              -- password, encrypted: cannot show, but can compare
               | SQLSingle  
               | SQLDouble  
               | SQLText              -- cannot compare, but can show (as a text)
               | SQLuInt    Int
               | SQLsInt    Int
               | SQLId                -- autoincrement integer
               | SQLVarchar Int
               | SQLBool              -- exists y/n
               deriving (Eq, Ord,Show)
  
  showSQL :: SqlType -> String
  showSQL (SQLChar    n) = "CHAR("++show n++")"
  showSQL (SQLBlob     ) = "BLOB"
  showSQL (SQLPass     ) = "VARCHAR(255)"
  showSQL (SQLSingle   ) = "FLOAT" -- todo
  showSQL (SQLDouble   ) = "FLOAT"
  showSQL (SQLText     ) = "TEXT"
  showSQL (SQLuInt    n) = "INT("++show n++") UNSIGNED"
  showSQL (SQLsInt    n) = "INT("++show n++")"
  showSQL (SQLId       ) = "INT"
  showSQL (SQLVarchar n) = "VARCHAR("++show n++")"
  showSQL (SQLBool     ) = "BOOLEAN"
            
  iskey :: SqlField->Bool
  iskey f = flduniq f && not (fldnull f)
  fldtyp :: Concept->SqlType
  fldtyp c = case cptnm c of { "BLOB"   -> SQLBlob;
                              "PASS"   -> SQLPass;
                              "STRING" -> SQLVarchar 255;
                              "TEXT"   -> SQLText;
                              _        -> SQLVarchar 255
                              }



  instance Morphical SqlField where
    concs     f = [target e'|let e'=fldexpr f,isSur e']
    morlist   f = morlist   (fldexpr f)
    decls     f = decls     (fldexpr f)
    closExprs f = closExprs (fldexpr f)
    
  instance Morphical PlugSQL where
    concs     p = concs     (fields p)
    mors      p = mors      (fields p)
    morlist   p = morlist   (fields p)
    decls     p = decls     (fields p)
    closExprs p = closExprs (fields p)
  
  instance Identified Plug where
     name x = plname x
  instance Identified PlugSQL where
     name x = plname x
  instance Identified PlugPHP where
     name x = plname x
  