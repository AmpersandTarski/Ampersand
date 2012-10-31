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

--WAAROM? @Stef: Why is there a difference between show and shOrig for Origin? 
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
              | CxeUnsupRoles {cxeIfc  :: P_Interface
                              ,cxeRoles :: [String]
                              }       
              | CxeNoRoles    {cxeIfc  :: P_Interface
                              }       
              | CxeNoIfcs     {cxeName :: String
                              ,cxePos  :: Origin
                              ,cxeIfcs :: [P_Interface]
                              }       
              | CxeObjMismatch{cxeExpr :: Term            --called as:  CxeObjMismatch oTerm (srcTypes (cod env)) (srcTypes (dom oTerm))
                              ,cxeEnv  :: [P_Concept]
                              ,cxeSrcs :: [P_Concept]
                              }       
              | CxeTyp        {cxeExpr :: Term
                              ,cxeDcls :: [Term]
                              }       
              | CxeRel        {cxeExpr :: Term
                              ,cxeDecs :: [(P_Declaration,[P_Concept],[P_Concept])]
                              }       
              | CxeV          {cxeExpr :: Term
                              ,cxeSrcs :: [P_Concept]
                              ,cxeTrgs :: [P_Concept]
                              }       
              | CxeCast       {cxeExpr    :: Term
                              ,cxeDomCast :: [P_Concept]
                              ,cxeCodCast :: [P_Concept]
                              }       
              | CxeCpl        {cxeExpr :: Term        -- SJC: shouldn't this be an instance of CxeV instead?
                              ,cxeSrcs :: [P_Concept]
                              ,cxeTrgs :: [P_Concept]
                              }       
              | CxeEquLike    {cxeExpr :: Term   -- error in term cxeExpr, lefthandside not compatable to righthandside
                              ,cxeLhs :: Term
                              ,cxeRhs :: Term
                              ,cxeSrcCpts :: [P_Concept]
                              ,cxeTrgCpts :: [P_Concept]
                              }
              | CxeCpsLike    {cxeExpr :: Term
                              ,cxeCpts :: [P_Concept]
                              }       
              | CxeViol       {cxeViol :: P_PairViewSegment
                              ,cxeRcpt :: P_Concept
                              ,cxeVcpt :: P_Concept
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
showErr err = case err of
  CxeEqConcepts{} -- * CxeEqConcepts tells us that there are concepts with different names, which can be proven equal by the type checker.
     -> concat
           ["The following concepts are equal:\n"++commaEng "and" (map show (cxeConcepts err))++"." | not (null (cxeConcepts err)) ]
  CxeEqAttribs{} -- * CxeEqAttribs tells us that there are concepts with different names, which can be proven equal by the type checker.
     -> show (cxeOrig err)++": Different attributes have the same name:"++
        concat [ "\n   ("++show (origin term)++")\t \""++cxeName err++"\" : "++showADL term | term<-cxeAtts err ]
  CxeILike{}
     -> concat
          ( [show (origin (cxeExpr err))++":\n"]++
              case cxeCpts err of
                 []  -> ["    Cannot deduce a type for  "++showADL (cxeExpr err)++"."]
                 cs  -> ["    The term  "++showADL (cxeExpr err)++",\n"]++
                        ["    cannot be "++commaEng "and" (map showADL cs) ++" at the same time."]
          )
  CxeNoRoles{}
     -> concat
          ( [show (origin (cxeIfc err))++":\n"]++
            ["    Interface "++name (cxeIfc err)++" supports no roles."]
          )
  CxeUnsupRoles{}
     -> concat
          ( [show (origin (cxeIfc err))++":\n"]++
            ["    Interface "++name (cxeIfc err)++" does not support the following roles:\n   "]++[commaEng "and" (cxeRoles err)]
          )
  CxeNoIfcs{}
     -> concat
          ( [show (origin (cxePos err))++":\n"]++
              case cxeIfcs err of
                 []   -> ["    There are no interfaces called \""++cxeName err++"\"."]
                 ifcs -> ["    There are multiple interfaces named " ++commaEng "and" (map (show.origin) ifcs) ++ "."]
          )
  CxeObjMismatch{}
     -> concat
          ( [show (origin (cxeExpr err))++":\n"]++
              case cxeSrcs err of
                 []  -> ["    Cannot deduce a type for  "++showADL (cxeExpr err)++"."]
                 cs  -> ["    The source of the term  "++showADL (cxeExpr err)++", which is "++commaEng "or" (map showADL cs)++"\n"]++
                        ["    cannot be matched to "++commaEng "and" (map showADL (cxeEnv err)) ++" from its environment."]
          )
  CxeTyp{}
     -> concat
          ( [show (origin (cxeExpr err))++":\n"]++
            ["    Relation  "++showADL (cxeExpr err)++"  is ambiguous." | null (cxeDcls err)]++
            ["    Relation  "++showADL (cxeExpr err)++"  has conflicting bindings:\n    "++
             intercalate "\n       " (map sh (cxeDcls err)) | (not.null) (cxeDcls err)]
          ) where sh term = termString++[' ' | _<-[length termString..maxLen]]++showADL term
                            where termString = show (origin term)
                                  maxLen = maximum [length (show (origin term')) | term'<-cxeDcls err ]
  CxeRel{}
     -> show (origin expr)++":\n"++
         case cxeDecs err of
          []                -> "    Relation  "++showADL expr++"  is not declared."
          [(d, [], [])]     -> "    Relation  "++showADL expr++"  is declared as "++name d++show (dec_sign d)++" on "++show (origin d)++",\n"++
                               "    but it is ambiguous."
          [(d, [],[_])]     -> "    Relation  "++showADL expr++"  is declared as "++name d++show (dec_sign d)++" on "++show (origin d)++",\n"++
                               "    but it is ambiguous in its source concept."
          [(d,[_], [])]     -> "    Relation  "++showADL expr++"  is declared as "++name d++show (dec_sign d)++" on "++show (origin d)++",\n"++
                               "    but it is ambiguous in its target concept."
          [(d, ss,[_])]     -> "    Relation  "++showADL expr++"  is declared as "++name d++show (dec_sign d)++" on "++show (origin d)++",\n"++
                               "    but its source concept, "++showADL s++", is not compatible with "++commaEng "and" (map show conflicts)++"."
                               where conflicts = ss>-[s]; P_Sign sgn = dec_sign d; s=head sgn
          [(d,[_], ts)]     -> "    Relation  "++showADL expr++"  is declared as "++name d++show (dec_sign d)++" on "++show (origin d)++",\n"++
                               "    but its target concept, "++showADL t++", is not compatible with "++commaEng "and" (map show conflicts)++"."
                               where conflicts = ts>-[t]; P_Sign sgn = dec_sign d; t=last sgn
          [(d, ss, ts)]     -> "    Relation  "++showADL expr++"  is declared as "++name d++show (dec_sign d)++" on "++show (origin d)++",\n"++
                               "    but its source concept cannot be both "++commaEng "and" (map show ss)++", and"++
                               "    its target concept cannot be "++commaEng "and" (map show ts)++" all at the same time."
          ds                -> "    Relation  "++showADL expr++"  is bound to multiple declarations on"++
                               "    "++commaEng "and" [show (origin d) | (d,_,_)<-ds ]++"."
         where expr=cxeExpr err
  CxeCast{}
     -> concat
          ( [show (origin (cxeExpr err))++":\n"]++
            case (cxeDomCast err, cxeCodCast err) of
                 (    []        ,    []         ) -> [ "    Ambiguous  "++showADL (cxeExpr err)++"."]
                 (    []        ,    _          ) -> [ "    Ambiguous source in term  "++showADL (cxeExpr err)++"."]
                 (    _         ,    []         ) -> [ "    Ambiguous target in term  "++showADL (cxeExpr err)++"."]
                 (    srcs      ,    [_]        ) -> [ "    Conflicts in the source of  "++showADL (cxeExpr err)++".\n    Concepts "++commaEng "and" (map show srcs)++" do not match." ]
                 (    [_]       ,    trgs       ) -> [ "    Conflicts in the target of  "++showADL (cxeExpr err)++".\n    Concepts "++commaEng "and" (map show trgs)++" do not match." ]
                 (    srcs      ,    trgs       ) -> [ "    Conflicting concepts in  "++showADL (cxeExpr err)++":\n    concepts "++commaEng "and" (map show srcs)++" do not match, and\n    concepts "++commaEng "and" (map show trgs)++" do not match."]
          )
  CxeEquLike{}
     -> case cxeExpr err of
          Pequ{}  -> showErrEquation err
          Pimp{}  -> showErrEquation err
          PIsc{}  -> showErrBoolTerm err
          PUni{}  -> showErrBoolTerm err
          PDif{}  -> showErrBoolTerm err
          _          -> fatal 144 "No match for this term!"
  CxeCpsLike{}
     -> case cxeExpr err of
          PLrs _ a b -> showErrBetweenTerm err a b "target" "target"
          PRrs _ a b -> showErrBetweenTerm err a b "source" "source"
          PCps _ a b -> showErrBetweenTerm err a b "target" "source"
          PRad _ a b -> showErrBetweenTerm err a b "target" "source"
          _          -> fatal 151 "No match for this term!"
  CxeOrig typeErrors t nm o
    | null typeErrors                              -> ""
    | t `elem` ["pattern", "process", "interface"] -> "The " ++ t ++ " named \""++ nm ++ "\" in file "++ filenm o ++ " contains errors:\n" ++ intercalate "\n\n" (map showErr typeErrors)
    | otherwise                                    -> "in the " ++ t ++ " at "++ shOrig o ++ " in file "++ filenm o ++ ":\n" ++ intercalate "\n" (map showErr typeErrors)
  Cxe typeErrors x
    -> x ++ "\n" ++ intercalate "\n" (map showErr typeErrors)
  PE msg
    -> "Parse error:\n"++ show (case msg of 
                                  []  -> fatal 35 "No messages??? The impossible happened!" 
                                  x:_ -> x)
  CxeCpl{}
    -> show (origin (cxeExpr err))++":\n"++concat (showErrSrcsTrgs err)
  CxeV{}
    -> show (origin (cxeExpr err))++":\n"++concat (showErrSrcsTrgs err)
  CxeViol v a b
    -> case v of
         P_PairViewExp srcOrTgt pexp
           -> show (origin pexp)++":\n"++
              "    Couldn't match the "++showADL srcOrTgt++" of the violation ("++name b++")\n"++
              "    with the source ("++name a++") of `"++showADL pexp++"`."
         P_PairViewText{} 
           -> fatal 172 "showErr is not defined for `P_PairViewTxt`."           
showErrSrcsTrgs :: CtxError -> [String]
showErrSrcsTrgs err 
 = let x = cxeExpr err in
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
                              ["    is in conflict with respect to concepts "++commaEng "and" (map showADL cs')++"."]


showErrBetweenTerm :: CtxError -> Term -> Term -> [Char] -> [Char] -> [Char]
showErrBetweenTerm err a b lSrcTrgText rSrcTrgText
 = concat
     ( [show (origin (cxeExpr err))++":\n"]++
       ["    Inside term  "++showADL (cxeExpr err)++",\n"]++
       case (cxeCpts err,a,b) of
            ([],PVee _,_) -> ["    the type of  "++showADL a++"  must be defined to make this term unambiguous."]
            ([],_,PVee _) -> ["    the type of  "++showADL b++"  must be defined to make this term unambiguous."]
            ([],PI _,_)   -> ["    the type of  "++showADL a++"  must be defined to make this term unambiguous."]
            ([],_,PI _)   -> ["    the type of  "++showADL b++"  must be defined to make this term unambiguous."]
            ([],_,_)      -> ["    the type between the "++lSrcTrgText++" of  "++showADL a++"  and the "++rSrcTrgText++" of  "++showADL b++"  is undefined."]
            (cs,_,_)      -> ["\n    between the "++lSrcTrgText++" of  "++showADL a++"  and the "++rSrcTrgText++" of  "++showADL b++",\n    concepts "++commaEng "and" (map showADL cs)++" are in conflict."]
     )

showErrEquation :: CtxError -> String
showErrEquation err@(CxeEquLike { cxeExpr=x, cxeLhs=a, cxeRhs=b})
 = concat
     ( [show (origin x)++"\n"]++
       case (cxeSrcCpts err, cxeTrgCpts err) of
            ([], [])  -> ["    Ambiguous equation  "++showADL x]
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
            (cs, cs') -> if sort cs==sort cs'
                         then ["    The sources and targets of (sub-)term "++showADL x++"\n"]++
                              ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs)++"."]
                         else ["    The source of  "++showADL x++"\n"]++
                              ["    cannot be both "++commaEng "and" (map showADL cs)++"\n"]++
                              ["    and the target of  "++showADL x++"\n"]++
                              ["    cannot be both "++commaEng "and" (map showADL cs)++"."]
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

