{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Input.ADL1.CtxError
       (newcxe,newcxeif,CtxError(..),shOrig) --constructors of CtxError are not exported, use constructor functions
where
import DatabaseDesign.Ampersand.Input.ADL1.FilePos
import DatabaseDesign.Ampersand.ADL1 (Pos(..))
import DatabaseDesign.Ampersand.Fspec.ShowADL
import DatabaseDesign.Ampersand.Basics
import Data.List  (intercalate, sort)
import DatabaseDesign.Ampersand.Parsing 
import DatabaseDesign.Ampersand.Core.ParseTree

fatal :: Int -> String -> a
fatal = fatalMsg "Input.ADL1.CtxError"

shOrig :: Origin -> String
shOrig (FileLoc (FilePos (_,DatabaseDesign.Ampersand.ADL1.Pos l c,_))) = "line " ++ show l++":"++show c
shOrig (DBLoc str)   = "Database location: "++str
shOrig (Origin str)  = str
shOrig OriginUnknown = "Unknown origin"

data CtxError = CxeEqConcepts {cxeConcepts :: [P_Concept]    -- ^ The list of concepts that have been proven equal.
                              }
              | CxeEqAttribs  {cxeOrig :: Origin          -- ^ The location of the object/key definition in which different attributes have the same name.
                              ,cxeName :: String          -- ^ The name shared by different attributes.
                              ,cxeAtts :: [Term]  -- ^ The list of attributes with the same name.
                              }
              | CxeILike      {cxeExpr :: Term
                              ,cxeCpts :: [P_Concept]
                              }       
              | CxeTyp        {cxeExpr :: Term
                              ,cxeDcls :: [Term]
                              }       
              | CxeRel        {cxeExpr :: Term
                              ,cxeDecs :: [P_Declaration]
                              ,cxeSrcs :: [P_Concept]
                              ,cxeTrgs :: [P_Concept]
                              }       
              | CxeV          {cxeExpr :: Term
                              ,cxeSrcs :: [P_Concept]
                              ,cxeTrgs :: [P_Concept]
                              }       
              | CxeCpl        {cxeExpr :: Term        -- SJC: shouldn't this be an instance of CxeV instead?
                              ,cxeCpts :: [P_Concept]
                              }       
              | CxeEquLike    {cxeExpr :: Term   -- error in expression cxeExpr, lefthandside not compatable to righthandside
                              ,cxeLhs :: Term
                              ,cxeRhs :: Term
                              ,cxeSrcCpts :: [P_Concept]
                              ,cxeTrgCpts :: [P_Concept]
                              }
              | CxeCpsLike    {cxeExpr :: Term
                              ,cxeCpts :: [P_Concept]
                              }       
              | CxeOrig       {cxeSubErrors :: [CtxError] -- ^ context information of an error   
                              ,cxetype :: String         -- ^ the type of context e.g. a rule
                              ,cxename :: String         -- ^ its name
                              ,cxeorigin:: Origin}        -- ^ the origin of the context e.g. a file position
              | Cxe           {cxeSubErrors :: [CtxError] -- ^ lower level errors
                              ,cxemsg :: String}        -- ^ a description of the error, e.g. "in the relation at line line 5752, file \"Zaken.adl\":"
              | PE            {cxeMsgs :: [ParseError]}  -- ^ list of parse-time messages 

instance Show CtxError where
    showsPrec _ err = showString (showErr err)

showErr :: CtxError -> String
-- * CxeEqConcepts tells us that there are concepts with different names, which can be proven equal by the type checker.
showErr err@(CxeEqConcepts{})
 = concat
     ["The following concepts are equal:\n"++commaEng "and" (map show (cxeConcepts err))++"." | not (null (cxeConcepts err)) ]
-- * CxeEqAttribs tells us that there are concepts with different names, which can be proven equal by the type checker.
showErr err@(CxeEqAttribs{})
 = show (cxeOrig err)++": Different attributes share the same name \""++cxeName err++"\":\n   "++
   intercalate "\n   " [ show (origin term)++" : "++showADL term | term<-cxeAtts err ]
showErr err@(CxeILike{ cxeCpts=[] })
 = concat
     ( [show (origin (cxeExpr err))++"\n"]++
       ["    Cannot deduce a type for  "++showADL (cxeExpr err)++"."]
     )
showErr err@(CxeILike{})
 = concat
     ( [show (origin (cxeExpr err))++"\n"]++
       ["    Term  "++showADL (cxeExpr err)++",\n"]++
       ["    cannot be "++commaEng "and" (map showADL (cxeCpts err)) | (not.null) (cxeCpts err)]++[" at the same time."]
     )
showErr err@(CxeTyp{})
 = concat
     ( [show (origin (cxeExpr err))++"\n"]++
       ["    Relation  "++showADL (cxeExpr err)++"  is ambiguous." | null (cxeDcls err)]++
       ["    Relation  "++showADL (cxeExpr err)++"  has conflicting bindings:\n    "++
        intercalate "\n       " (map sh (cxeDcls err)) | (not.null) (cxeDcls err)]
     ) where sh term = termString++[' ' | _<-[length termString..maxLen]]++showADL term
                       where termString = show (origin term)
                             maxLen = maximum [length (show (origin term')) | term'<-cxeDcls err ]
showErr err@(CxeRel{})
 = show (origin (cxeExpr err))++"\n"++
   case (cxeDecs err, cxeSrcs err, cxeTrgs err) of
    ( _, [], []) -> "    Relation  "++showADL (cxeExpr err)++"  is ambiguous."
    ( _, [], _ ) -> "    The source of relation  "++showADL (cxeExpr err)++"  is ambiguous."
    ( _, _ , []) -> "    The target of relation  "++showADL (cxeExpr err)++"  is ambiguous."
    ([],  _, _ ) -> "    Relation  "++showADL (cxeExpr err)++"  is not declared."
    ( _, _ , _ ) -> fatal 100 "make more error messages."

showErr err@(CxeCpl{})
 = concat
     ( [show (origin (cxeExpr err))++"\n"]++
       ["    There is no universe from which to compute a complement in term  "++showADL (cxeExpr err)++"." | null (cxeCpts err)]++
       ["    There are multiple universes from which to compute a complement in term  "++showADL (cxeExpr err)++":\n    "++commaEng "and" (map showADL (cxeCpts err)) | (not.null) (cxeCpts err)]
     )
showErr err@(CxeV{ cxeExpr=x })
 = concat
     ( [show (origin (cxeExpr err))++"\n"]++
       case (cxeSrcs err, cxeTrgs err) of
            ([], [])  -> ["    No type can be derived for  "++showADL x]
            (_, [])   -> ["    The target of  "++showADL x++"  is ambiguous."]
            ([], _)   -> ["    The source of  "++showADL x++"  is ambiguous."]
            (cs, [_]) -> ["    Cannot pick a concept for the source of  "++showADL x++"\n"]++
                         ["    Concepts can be one of "++commaEng "and" (map showADL cs)++"."]
            ([_], cs) -> ["    Cannot pick a concept for the target of  "++showADL x++"\n"]++
                         ["    Concepts can be one of "++commaEng "and" (map showADL cs)++"."]
            (cs, cs') -> if sort cs==sort cs'
                         then ["    the source and target of  "++showADL x++"\n"]++
                              ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs)++"."]
                         else ["    the source of  "++showADL x++"\n"]++
                              ["    is in conflict with respect to concepts "++commaEng "and" (map showADL cs)++"\n"]++
                              ["    and the target of  "++showADL x++"\n"]++
                              ["    is in conflict with respect to concepts "++commaEng "and" (map showADL cs')++"."]     )
showErr err@(CxeEquLike { cxeExpr=Pequ _ _ _ }) = showErrEquation err
showErr err@(CxeEquLike { cxeExpr=Pimp _ _ _ }) = showErrEquation err
showErr err@(CxeEquLike { cxeExpr=PIsc _ _ _ }) = showErrBoolTerm err
showErr err@(CxeEquLike { cxeExpr=PUni _ _ _ }) = showErrBoolTerm err
showErr err@(CxeEquLike { cxeExpr=PDif _ _ _ }) = showErrBoolTerm err
showErr err@(CxeCpsLike { cxeExpr=PLrs _ a b})
 = concat
     ( [show (origin (cxeExpr err))++"\n"]++
       ["    Inside term  "++showADL (cxeExpr err)++",\n"]++
       ["    between the target of  "++showADL a++"  and the target of  "++showADL b++",\n"]++
       ["    concepts "++commaEng "and" (map showADL (cxeCpts err)) | (not.null) (cxeCpts err)]++[" are in conflict."]
     )
showErr err@(CxeCpsLike { cxeExpr=PRrs _ a b})
 = concat
     ( [show (origin (cxeExpr err))++"\n"]++
       ["    Inside term   "++showADL (cxeExpr err)++",\n"]++
       ["    between the source of  "++showADL a++"  and the source of  "++showADL b++",\n"]++
       ["    concepts "++commaEng "and" (map showADL (cxeCpts err)) | (not.null) (cxeCpts err)]++[" are in conflict."]
     )
showErr err@(CxeCpsLike { cxeExpr=PCps _ a b})
 = concat
     ( [show (origin (cxeExpr err))++"\n"]++
       ["    Inside term   "++showADL (cxeExpr err)++",\n"]++
       ["    concepts "++commaEng "and" (map showADL (cxeCpts err))++
        "\n    between the target of  "++showADL a++"  and the source of  "++showADL b++" are in conflict." | (not.null) (cxeCpts err)]++
       ["    the type between the target of  "++showADL a++"  and the source of  "++showADL b++" is undefined." | null (cxeCpts err)]
     )
showErr err@(CxeCpsLike { cxeExpr=PRad _ a b})
 = concat
     ( [show (origin (cxeExpr err))++"\n"]++
       ["    Inside term   "++showADL (cxeExpr err)++",\n"]++
       ["    concepts "++commaEng "and" (map showADL (cxeCpts err))++
        "\n    between the target of  "++showADL a++"  and the source of  "++showADL b++" are in conflict." | (not.null) (cxeCpts err)]++
       ["    the type between the target of  "++showADL a++"  and the source of  "++showADL b++" is undefined." | null (cxeCpts err)]
     )
showErr (CxeOrig typeErrors t nm o)
 | null typeErrors                              = ""
 | t `elem` ["pattern", "process", "interface"] = "The " ++ t ++ " named \""++ nm ++ "\" contains errors " ++ intercalate "\n" (map showErr typeErrors)
 | otherwise                                    = "in the " ++ t ++ " at "++ shOrig o ++ ":\n" ++ intercalate "\n" (map showErr typeErrors)
showErr (Cxe typeErrors x)                      = x ++ "\n" ++ intercalate "\n" (map showErr typeErrors)
showErr (PE msg)                                = "Parse error:\n"++ show (case msg of 
                                                                             []  -> fatal 35 "No messages??? The impossible happened!" 
                                                                             x:_ -> x)
showErr _ = fatal 580 "missing pattern in type error."
showErrEquation :: CtxError -> String
showErrEquation err@(CxeEquLike { cxeExpr=x, cxeLhs=a, cxeRhs=b})
 = concat
     ( [show (origin x)++"\n"]++
       case (cxeSrcCpts err, cxeTrgCpts err) of
            ([], [])  -> ["    Entirely ambiguous equation  "++showADL x]
            ([_], []) -> ["    The target of  "++showADL x++"  is ambiguous."]
            ([], [_]) -> ["    The source of  "++showADL x++"  is ambiguous."]
            (cs, [])  -> ["    The source of  "++showADL a++"  and the source of  "++showADL b++"\n"]++
                         ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs)++"."]
            (cs, [_]) -> ["    The source of  "++showADL a++"  and the source of  "++showADL b++"\n"]++
                         ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs)++"."]
            ([], cs') -> ["    The target of  "++showADL a++"  and the target of  "++showADL b++"\n"]++
                         ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs')++"."]
            ([_],cs') -> ["    The target of  "++showADL a++"  and the target of  "++showADL b++"\n"]++
                         ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs')++"."]
            (cs, cs') -> if sort cs==sort cs'
                         then ["    the sources and targets of  "++showADL a++"  and  "++showADL b++"\n"]++
                              ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs)++"."]
                         else ["    the source of  "++showADL a++"  and the source of  "++showADL b++"\n"]++
                              ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs)++"\n"]++
                              ["    and the target of  "++showADL a++"  and the target of  "++showADL b++"\n"]++
                              ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs')++"."]
     )
showErrEquation o = fatal 173 ("showErrEquation does not match "++show o)

showErrBoolTerm :: CtxError -> String
showErrBoolTerm err@(CxeEquLike { cxeExpr=x, cxeLhs=a, cxeRhs=b})
 = concat
     ( [show (origin x)++"\n"]++
       case (cxeSrcCpts err, cxeTrgCpts err) of
            ([], [])  -> ["    Entirely ambiguous term  "++showADL x]
            ([_], []) -> ["    Inside term   "++showADL x++"\n"]++
                         ["    the target of  "++showADL x++"  is ambiguous."]
            ([], [_]) -> ["    Inside term   "++showADL x++"\n"]++
                         ["    the source of  "++showADL x++"  is ambiguous."]
            (cs, [])  -> ["    Inside term   "++showADL x++"\n"]++
                         ["    the source of  "++showADL a++"  and the source of  "++showADL b++"\n"]++
                         ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs)++"."]
            (cs, [_]) -> ["    Inside term   "++showADL x++"\n"]++
                         ["    the source of  "++showADL a++"  and the source of  "++showADL b++"\n"]++
                         ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs)++"."]
            ([], cs') -> ["    Inside term   "++showADL x++"\n"]++
                         ["    the target of  "++showADL a++"  and the target of  "++showADL b++"\n"]++
                         ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs')++"."]
            ([_],cs') -> ["    Inside term   "++showADL x++"\n"]++
                         ["    the target of  "++showADL a++"  and the target of  "++showADL b++"\n"]++
                         ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs')++"."]
            (cs, cs') -> ["    Inside term   "++showADL x++",\n"]++
                         if sort cs==sort cs'
                         then ["    the sources and targets of  "++showADL a++"  and  "++showADL b++"\n"]++
                              ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs)++"."]
                         else ["    the source of  "++showADL a++"  and the source of  "++showADL b++"\n"]++
                              ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs)++"\n"]++
                              ["    and the target of  "++showADL a++"  and the target of  "++showADL b++"\n"]++
                              ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs')++"."]
     )
showErrBoolTerm o = fatal 206 ("showErrBoolTerm does not match "++show o)

{-
instance Eq CtxError where
  CxeOrig es t n o == CxeOrig es' t' n' o' = es == es' && t == t' && n == n' && o == o'
  Cxe es s == Cxe es' s'   = es == es' && s == s'
  _ == _ = False
-}

newcxe :: String -> CtxError
newcxe str = Cxe [] str
newcxeif :: Bool -> String -> [CtxError]
newcxeif condition cxe = [newcxe cxe | condition]

