{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Input.ADL1.CtxError
       (newcxe,newcxeif,cxelist,cxenest,cxenone,nocxe,cxes,CtxError(..)) --constructors of CtxError are not exported, use constructor functions
where
import DatabaseDesign.Ampersand.Input.ADL1.FilePos
import DatabaseDesign.Ampersand.Parsing 
import DatabaseDesign.Ampersand.Basics
import Data.List (intercalate)

fatal :: Int -> String -> a
fatal = fatalMsg "CtxError"

data CtxError = Cxes    {cxesibls::[CtxError]} -- ^ any number of errors
              | CxeOrig {cxechild::CtxError    -- ^ context information of an error   
                        ,cxetype::String       -- ^ the type of context e.g. a rule
                        ,cxename::String       -- ^ its name
                        ,cxeorigin::Origin}    -- ^ the origin of the context e.g. a file position
              | Cxe     {cxechild::CtxError    -- ^ lower level errors
                        ,cxemsg::String}       -- ^ a description of the error
              | CxeNone                        -- ^ indicates the absence of an error
              | PE      {cxeMsgs :: [ParserError]} -- ^ list of parse-time messages 
          --    deriving (Eq)
instance Eq CtxError where
  Cxes es == Cxes es' = es == es'
  CxeOrig e t n o == CxeOrig e' t' n' o' = e == e' && t == t' && n == n' && o == o'
  Cxe e s == Cxe e' s'   = e == e' && s == s'
  CxeNone == CxeNone = True
  _ == _ = False
  
instance Show CtxError where
  showsPrec _ (Cxes xs) = showString( intercalate "\n" (map show xs))
  showsPrec _ (CxeOrig cxe t nm o)
   | nocxe cxe                                    = showString ("The " ++ t ++ " at "++ show o ++ " is correct.")
   | t `elem` ["pattern", "process", "interface"] = showString ("The " ++ t ++ " named "++ show nm ++ " contains errors " ++ show cxe)
   | otherwise                                    = showString ("in the " ++ t ++ " at line "++ show o ++ ":\n" ++ show cxe)
  showsPrec _ (Cxe cxe x) = showString (x ++ "\n" ++ show cxe)
  showsPrec _ CxeNone = showString ""
  showsPrec _ (PE msg) = showString $ "Parse error:\n"++ show (case msg of 
                                                              [] -> fatal 35 "No messages??? The impossible happened!" 
                                                              x:_ -> x)
newcxe :: String -> CtxError
newcxe = Cxe CxeNone
newcxeif :: Bool -> String -> CtxError
newcxeif True cxe = newcxe cxe
newcxeif False _ = CxeNone
cxelist :: [CtxError] -> CtxError
cxelist = Cxes
cxenest :: CtxError -> String -> CtxError
cxenest = Cxe
cxenone :: CtxError
cxenone = CxeNone
 
-- | nocxe checks whether there are no errors
nocxe :: CtxError -> Bool
nocxe cxe = cxes cxe==CxeNone
-- | cxes filters all redundant CxeNone, Cxes and CxeOrig
cxes :: CtxError -> CtxError
cxes (Cxe    {cxechild=ccxe,cxemsg=msg})
                   = Cxe (cxes ccxe) msg
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