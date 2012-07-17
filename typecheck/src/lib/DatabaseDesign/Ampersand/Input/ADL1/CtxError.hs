{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Input.ADL1.CtxError
       (newcxe,newcxeif,cxelist,cxenone,nocxe,cxes,CtxError(..)) --constructors of CtxError are not exported, use constructor functions
where
import DatabaseDesign.Ampersand.Input.ADL1.FilePos
import DatabaseDesign.Ampersand.Parsing 
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Core.ParseTree

data CtxError = Cxes          {cxesibls :: [CtxError]}    -- ^ any number of errors
              | CxeAmbExpr    {cxeExpr  :: P_Expression   -- ^ an erroneous expression, which is ambiguous
                              ,cxeSrcT  :: [P_Concept]    -- ^ The applicable types for the source (length must be >1)
                              ,cxeTrgT  :: [P_Concept]    -- ^ The applicable types for the target (length must be >1)
                              ,cxeSign  :: [P_Sign]       -- ^ The applicable types for the expression (length must be >1)
                              }
              | CxeAmbBetween {cxeExpr  :: P_Expression
                              ,cxeSrcE  :: P_Expression
                              ,cxeSrcB  :: Bool
                              ,cxeTrgE  :: P_Expression
                              ,cxeTrgB  :: Bool
                              ,cxeCpts  :: [P_Concept]
                              }       
              | CxeOrig       {cxechild :: CtxError       -- ^ context information of an error   
                              ,cxetype  :: String         -- ^ the type of context e.g. a rule
                              ,cxename  :: String         -- ^ its name
                              ,cxeorigin:: Origin}        -- ^ the origin of the context e.g. a file position
              | Cxe           {cxechild :: CtxError       -- ^ lower level errors
                              ,cxemsg   :: String}        -- ^ a description of the error, e.g. "in the relation at line line 5752, file \"Zaken.adl\":"
              | CxeNone                                   -- ^ indicates the absence of an error
              | PE            {cxeMsgs  :: [ParseError]}  -- ^ list of parse-time messages 


instance Eq CtxError where
  Cxes es == Cxes es' = es == es'
  CxeOrig e t n o == CxeOrig e' t' n' o' = e == e' && t == t' && n == n' && o == o'
  Cxe e s == Cxe e' s'   = e == e' && s == s'
  CxeNone == CxeNone = True
  _ == _ = False


newcxe :: String -> CtxError
newcxe = Cxe CxeNone
newcxeif :: Bool -> String -> CtxError
newcxeif True cxe = newcxe cxe
newcxeif False _ = CxeNone
cxelist :: [CtxError] -> CtxError
cxelist = Cxes
--cxenest :: CtxError -> String -> CtxError
--cxenest = Cxe
cxenone :: CtxError
cxenone = CxeNone
--filterCtxErr :: (CtxError -> Bool) -> CtxError -> CtxError
--filterCtxErr f (err@Cxes{})    = Cxes [ filterCtxErr f sibl | sibl<-cxesibls err, f sibl]
--filterCtxErr f (err@CxeOrig{}) = if f err then err{cxechild=filterCtxErr f (cxechild err)} else filterCtxErr f (cxechild err)
--filterCtxErr f (err@Cxe{})     = if f err then err{cxechild=filterCtxErr f (cxechild err)} else filterCtxErr f (cxechild err)
--filterCtxErr f errs            = errs

-- | nocxe checks whether there are no errors
nocxe :: CtxError -> Bool
nocxe cxe = cxes cxe==CxeNone

-- | cxes filters all redundant CxeNone, Cxes and CxeOrig
cxes :: CtxError -> CtxError
cxes (Cxe    {cxechild=ccxe,cxemsg=msg})
                   = Cxe (cxes ccxe) msg
cxes (cxe@CxeAmbExpr{}) = cxe
cxes (cxe@CxeAmbBetween{}) = cxe
cxes (cxe@CxeOrig{})
 | nocxe (cxechild cxe) = CxeNone
 | otherwise       = cxe{cxechild=cxes (cxechild cxe)}
cxes  CxeNone = CxeNone
cxes (Cxes   {cxesibls=scxes})
 | null cxes'      = CxeNone
 | length cxes'==1 = head cxes'
 | otherwise       = Cxes cxes'
   where cxes'=filter (not.nocxe) (map cxes scxes)
cxes (PE x) = PE x