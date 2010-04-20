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
                 ,isBinary)
where
  import Adl
  import Collection  (rd)
  import FPA
  import Maybe
  import Auxiliaries (sort')
  
  type Plugs = [Plug]
  data Plug = PlugSql { plname   :: String
                      , fields   :: [SqlField]
                      , cLkpTbl  :: [(Concept,SqlField)]           -- lookup table that links concepts to column names in the plug
                      , mLkpTbl  :: [(Morphism,SqlField,SqlField)] -- lookup table that links concepts to column names in the plug
                      , plfpa    :: FPA
                      }
            | PlugPhp { args     :: PhpArgs
                      , returns  :: PhpReturn
                      , function :: PhpAction
                      , phpfile  :: String
                      , plname   :: String 
                      , plfpa    :: FPA
                      } deriving (Show)

  instance Object Plug where
   concept p = case p of
     PlugSql{cLkpTbl = []} -> error ("!Fatal (module Data.Plug 38): empty lookup table for plug "++name p++".")
     PlugSql{}             -> head [c|(c,_)<-cLkpTbl p]
     PlugPhp{}             -> error ("!Fatal (module Data.Plug 40): No definition for concept of plug "++name p++".")
-- Usually source a==concept p. Otherwise, the attribute computation is somewhat more complicated. See ADL2Fspec for explanation about kernels.
   attributes p = case p of 
     PlugSql{} -> 
      [ Obj (fldname tFld)                                                   -- objnm 
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
     PlugPhp{} ->  error ("!Fatal (module Data.Plug 58): No definition for attributes of plug "++name p++".")
   ctx p = Tm (mIs (concept p)) (-1)
   populations p = error ("!TODO (module Data.Plug 42): evaluate population of plug "++name p++".")

  isClass  :: Plug -> Bool
  isClass  p@PlugSql{} = not (null [1|fld<-fields p, flduniq fld]) && not (null [1|fld<-fields p, not (flduniq fld)])

  isBinary :: Plug -> Bool
  isBinary p@PlugSql{} = length (fields p)==2 && null [fld|fld<-fields p, flduniq fld]

  isScalar :: Plug -> Bool
  isScalar p@PlugSql{} = length (fields p)<=1 && null [fld|fld<-fields p, flduniq fld]

  instance Association Plug where
     source p           = (source . fldexpr . head . fields) p
     target p@PlugSql{} | isBinary p = target m where (m,_,_) = head (mLkpTbl p)
     target p                        = error ("!Fatal (module Data/Plug 77): cannot compute the target of plug "++name p++", because it is not binary.")

  instance Morphic Plug where
 {- TBD (nog omzetten van Rule naar (binaire) Plug
    multiplicities _  = []
    flp r = r{rrant = if rrsrt r == Truth
                      then error ("!Fatal (module Rule 110): illegal call to antecedent in flp ("++show r++")")
                      else flp (rrant r)
             ,rrcon = flp (rrcon r)
             ,rrtyp = (target (rrtyp r),source (rrtyp r))
             }
  --  isIdent r = error ("!Fatal (module Rule 116): isIdent not applicable to any rule:\n "++showHS "" r)
    typeUniq r | ruleType r==Truth = typeUniq (antecedent r)
               | otherwise       = typeUniq (antecedent r) && typeUniq (consequent r)
--    isIdent r = isIdent (normExpr r)
    isProp r  = isProp (normExpr r)

    isTrue r  = case ruleType r of
                 Truth       -> isTrue (consequent r)
                 Implication -> isFalse (antecedent r) || isTrue (consequent r)
                 Equivalence -> antecedent r == consequent r
                 Generalization -> error ("!Fatal (module Rule 88): isTrue not defined for a Generalisation.")
    isFalse r = case ruleType r of
                 Truth       -> isFalse (consequent r)
                 Implication -> isTrue (antecedent r) && isFalse (consequent r)
                 Equivalence -> notCp (antecedent r) == consequent r
                 Generalization -> error ("!Fatal (module Rule 93): isFalse not defined for a Generalisation.")
    isNot r   | ruleType r==Truth = isNot (consequent r)
              | otherwise         = False  -- TODO: check correctness!
 -}
    isSignal p@PlugSql{} | isBinary p = isSignal m where (m,_,_) = head (mLkpTbl p)
    isSignal _                        = False

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
     name p = case p of {PhpNull -> "0"; PhpObject{objectdf=x} -> name x}

  --DESCR -> plugs are sorted to optimize some algoritms. 
  instance Eq Plug where
    x==y = name x==name y
  instance Ord Plug where -- WAAROM (SJ) Waarom is Plug een instance van Ord?
    compare x y = compare (name x) (name y)
  
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
  fldtyp nm = case name nm of { "BLOB"   -> SQLBlob;
                                "PASS"   -> SQLPass;
                                "STRING" -> SQLVarchar 255;
                                "TEXT"   -> SQLText;
                                _        -> SQLVarchar 255
                              }

  instance Identified Plug where
    name p = plname p

  instance Morphical SqlField where
    concs     f = [target e'|let e'=fldexpr f,isSur e']
    mors      f = (rd.map makeInline.mors.fldexpr) f
    morlist   f = morlist   (fldexpr f)
    decls     f = decls     (fldexpr f)
    closExprs f = closExprs (fldexpr f)
    
  instance Morphical Plug where
    concs     p@PlugSql{} = concs     (fields p)
    concs     PlugPhp{} = []                       -- To be done...
    mors      p@PlugSql{} = mors      (fields p)
    mors      PlugPhp{} = []                       -- To be done...
    morlist   p@PlugSql{} = morlist   (fields p)
    morlist   PlugPhp{} = []                       -- To be done...
    decls     p@PlugSql{} = decls     (fields p)
    decls     PlugPhp{} = []                       -- To be done...
    closExprs p@PlugSql{} = closExprs (fields p)
    closExprs PlugPhp{} = []                       -- To be done...
