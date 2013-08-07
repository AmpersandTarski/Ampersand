{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Input.ADL1.CtxError
       (newcxe,newcxeif,TypErrTyp(..),CtxError(..),showErr, ParseError)
where
--import DatabaseDesign.Ampersand.Input.ADL1.FilePos()
import DatabaseDesign.Ampersand.ADL1 (Pos(..))
import DatabaseDesign.Ampersand.Fspec.ShowADL
import DatabaseDesign.Ampersand.Basics
import Data.List  (intercalate, sort)
import DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner (Token)
import DatabaseDesign.Ampersand.Input.ADL1.UU_Parsing (Message)
import DatabaseDesign.Ampersand.Core.ParseTree

fatal :: Int -> String -> a
fatal = fatalMsg "Input.ADL1.CtxError"

type ParseError = Message Token
data TypErrTyp = TETUnion Term | TETIsc Term | TETEq Term
               | TETPairView P_PairViewSegment
               | TETBox Term
               | TETIdent Term | TETView Term
              
instance ShowADL TypErrTyp where
  showADL (TETUnion t)    = "the union arising at "++showADL t
  showADL (TETIsc t)      = "the intersect arising at "++showADL t
  showADL (TETEq t)       = "the equality arising at "++showADL t
  showADL (TETBox _)      = "BOX"
  showADL (TETIdent _)    = "IDENT"
  showADL (TETView _)     = "VIEW"
  showADL (TETPairView t) = "the RULE View "++show t -- TODO: what to call this?
               
data CtxError = CxeEqConcepts { cxeConcepts :: [P_Concept]       -- ^ The list of concepts with different names, that have been proven equal.
                              }   
              | CxeEqAttribs  { cxeOrig ::      Origin           -- ^ The location of the object/identity definition in which different attributes have the same name.
                              , cxeName ::      String           -- ^ The name shared by different attributes.
                              , cxeAtts ::      [Term]           -- ^ The list of attributes with the same name.
                              }
              | CxeAttrib     { cxeAtt ::       P_ObjectDef      -- ^ The attribute expression, whose source should be equal to or more specific than target cxeSelf.
--                            , cxeCmp ::       Ordering         -- ^ The ordering of target cxeSelf `compare` source (objexpr (cxeAtt err))
                              , cxeSrc ::       P_Concept        -- ^ The source of obj_ctx (cxeAtt err).
                              , cxeTrg ::       P_Concept        -- ^ The target of cxeSelf err.
                              , cxeSelf ::      Term             -- ^ The object expression, whose target is relevant for the error message.
                              }
              | CxeUnsupRoles { cxeIfc ::       P_Interface
                              , cxeRoles ::     [String]
                              }   
              | CxeNoRoles    { cxeIfc ::        P_Interface
                              }   
              | CxeNoRules    { cxePos ::       Origin
                              , cxeRules ::     [String]
                              }  
              | CxeNoIfcs     { cxeName ::      String
                              , cxePos ::       Origin
                              , cxeIfcs ::      [P_Interface]
                              }     
              | CxeObjMismatch{ cxeExpr ::      Term            --called as:  CxeObjMismatch oTerm (srcTypes (cod env)) (srcTypes (dom oTerm))
                              , cxeEnv ::       [P_Concept]
                              , cxeSrcs ::      [P_Concept]
                              } 
              | CxeRel        { cxeExpr ::      Term
                              , cxeDecs ::      [P_Declaration]  -- possibilities for bindings.
                              , cxeSNDs ::      [P_Declaration]  -- Declarations with the same name
                              }  
              | CxeRelUndefined { cxeExpr ::      Term
                              }   
              | CxeSign       { cxeExpr ::      Term
                              , cxeSrcs ::      [P_Concept]
                              , cxeTrgs ::      [P_Concept]
                              }   
              | CxeBetween     { cxeLhs ::       (Term,SrcOrTgt,[P_Concept])
                              , cxeRhs ::       (Term,SrcOrTgt,[P_Concept])
                              , cxeTyp ::       TypErrTyp
                              }   
              | CxeCast       { cxeExpr ::      Term
                              , cxeDomCast ::   [P_Concept]
                              , cxeCodCast ::   [P_Concept]
                              }
              | CxeOrig       { cxeSubErrors :: [CtxError] -- ^ context information of an error   
                              , cxetype ::      String         -- ^ the type of context e.g. a rule
                              , cxename ::      String         -- ^ its name
                              , cxeorigin ::    Origin}        -- ^ the origin of the context e.g. a file position
              | Cxe           { cxeSubErrors :: [CtxError] -- ^ lower level errors
                              , cxemsg ::       String}        -- ^ a description of the error, e.g. "in the relation at line line 5752, file \"Zaken.adl\":"
              | PE            { cxeMsgs ::      [ParseError]}  -- ^ list of parse-time messages

instance Show CtxError where
    showsPrec _ err = showString (showErr err)

niceSource :: (ShowADL a1, ShowADL a) => (a, SrcOrTgt, [a1]) -> String
niceSource (t,s,[]) = "    There is no type for the "++showADL s++" of  "++showADL t++"."
niceSource (t,s,cs) = "    The "++showADL s++" of  "++showADL t++"  is  "++ commaEng "or" (map showADL cs)++"."

showErr :: CtxError -> String
showErr err = case err of
  CxeEqConcepts{}
     -> concat
           ["The following concepts are equal:\n"++commaEng "and" (map show (cxeConcepts err))++"." | not (null (cxeConcepts err)) ]
  CxeEqAttribs{}
     -> show (cxeOrig err)++": Different attributes have the same name:"++
        concat [ "\n   ("++show (origin term)++")\t \""++cxeName err++"\" : "++showADL term | term<-cxeAtts err ]
  CxeAttrib{}
     -> show (origin att)++": Type error in attribute \""++name att++"\":"++
        "\n   source of "++showADL (obj_ctx att)++" is "++show (name (cxeSrc err))++", but it should be equal to "++show (name (cxeTrg err))++" or more specific."
        where att = cxeAtt err
  CxeNoRules{}
     -> show (cxePos err)++":\n"++
        case cxeRules err of
         []  -> ""
         [r] -> "Rule '"++r++"' does not exist."
         rs  -> "Rules "++commaEng "and" [ "'"++r++"'" | r<-rs]++" do not exist."
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
                 ifcs -> ["    There are multiple interfaces called \""++cxeName err++"\" on "++ commaEng "and" (map (show.origin) ifcs) ++ "."]
          )
  CxeObjMismatch{}
     -> concat
          ( [show (origin (cxeExpr err))++":\n"]++
              case cxeSrcs err of
                 []  -> ["    Cannot deduce a type for  "++showADL (cxeExpr err)++"."]
                 cs  -> ["    The source of the term  "++showADL (cxeExpr err)++", which is "++commaEng "or" (map showADL cs)++"\n"]++
                        ["    cannot be matched to "++commaEng "and" (map showADL (cxeEnv err)) ++" from its environment."]
          )
  CxeBetween{} -- to get this error, create a "Between" data type in uType
     ->   ( show (origin ((\(x,_,_)->x) (cxeLhs err)))++":\n"++
            "    Type mismatch for "++showADL (cxeTyp err)++
              case (cxeLhs err,cxeRhs err) of
                 ((t1,s1,[]),(t2,s2,[])) -> "\n    matching the "++show s1++" of  "++showADL t1++"  and the "++show s2++" of  "++showADL t2
                 (t1,t2) -> ".\n" ++ niceSource t1 ++ (if (t1==t2) then "" else "\n" ++ niceSource t2)
          )
  CxeRelUndefined{}
     -> show (origin (cxeExpr err))++":\n    Relation  "++showADL (cxeExpr err)++"  is undefined."
  CxeRel{}
     -> show (origin term)++":\n"++
         case cxeDecs err of
          []  -> "    Relation  "++showADL term++
                 case cxeSNDs err of
                  []  -> "  is undefined."
                  [d] -> "  does not match the declaration on "++show (origin d)++": "++name d++show (dec_sign d)
                  ds  -> "  cannot be matched to any of the following declarations:"++
                         concat [ "\n      "++name d++show (dec_sign d)++":\t "++show (origin d) | d<-ds ]
          [_] -> fatal 156 "Illegal error message. This should be correct."
          ds  -> "    Relation  "++showADL term++"  must be bound to one declaration only,\n"++
                 "    but it can be matched to each of the following declarations:"++
                 concat [ "\n     "++name d++"["++show (source d)++"*"++show (target d)++"]"++" on "++show (origin d)
-- for debugging, you might add:        ++ ", ss="++show ss ++ ", ts="++show ts | (d, ss, ts)<-ds ]++"."
                        | d<-ds ]++"."
                 where source d = pSrc (dec_sign d)
                       target d = pTrg (dec_sign d)
         where term=cxeExpr err
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
  CxeOrig typeErrors t nm o
    | null typeErrors                              -> ""
    | t `elem` ["pattern", "process", "interface"] -> "The " ++ t ++ " named \""++ nm ++ "\" in file "++ filenm o ++ " contains errors:\n" ++ intercalate "\n\n" (map showErr typeErrors)
    | otherwise                                    -> "in the " ++ t ++ " at "++ shOrig o ++ " in file "++ filenm o ++ ":\n" ++ intercalate "\n" (map showErr typeErrors)
  Cxe typeErrors x
   -> x ++ "\n" ++ intercalate "\n" (map showErr typeErrors)
  PE msgs
    -> "Parse error:"++ show (case msgs of 
                                  []  -> fatal 35 "No messages??? The impossible happened!" 
                                  x:_ -> x)
  CxeSign{cxeSrcs = srcs, cxeTrgs = trgs, cxeExpr = x }
    -> show (origin (cxeExpr err))++":\n"++concat (
       case (srcs, trgs, x) of
               ([], [],_)         -> ["    No type can be derived for  "++showADL x]
               (_, [],_)          -> ["    The target of  "++showADL x++"  is ambiguous."]
               ([], _,_)          -> ["    The source of  "++showADL x++"  is ambiguous."]
               (cs, [t],PCpl _ y) -> ["    The complement of "++showADL y++" is ambiguous with respect to its source.\n"]++
                                     ["    The source concept must be either "++commaEng "or" (map showADL cs)++".\n"]++
                                     ["    Use (V[ <source concept> * "++showADL t++" ]-"++showADL y++") to disambiguate."]
               (cs, [_],_)        -> ["    Cannot pick a concept for the source of  "++showADL x++"\n"]++
                                     ["    Concepts can be one of "++commaEng "and" (map showADL cs)++"."]
               ([s], cs,PCpl _ y) -> ["    The complement of "++showADL y++" is ambiguous with respect to its target.\n"]++
                                     ["    The target concept must be either "++commaEng "or" (map showADL cs)++".\n"]++
                                     ["    Use (V[ "++showADL s++" * <target concept> ]-"++showADL y++") to disambiguate."]
               ([_], cs,_)        -> ["    Cannot pick a concept for the target of  "++showADL x++"\n"]++
                                     ["    Concepts can be one of "++commaEng "and" (map showADL cs)++"."]
               (cs, cs',PCpl _ y  )
                -> if sort cs==sort cs'
                   then ["    The complement of "++showADL y++" is ambiguous with respect to\n"]++
                        ["    concepts "++commaEng "and" (map showADL cs)++".\n"]++
                        ["    Use (V[ <source concept> * <target concept> ]-"++showADL y++") to disambiguate the complement."]
                   else ["    Ambiguous concept for the complement of "++showADL y++"\n"]++
                        ["    Concepts can be one of "++commaEng "and" (map showADL cs)++"."]++
                        ["    The target of  "++showADL x++"\n"]++
                        ["    is ambiguous too, with respect to concepts "++commaEng "and" (map showADL cs')++".\n"]++
                        ["    Use (V[ <source concept> * <target concept> ]-"++showADL y++") to disambiguate the complement."]
               (cs, cs',_)
                -> if sort cs==sort cs'
                   then ["    the source and target of  "++showADL x++"\n"]++
                        ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs)++"."]
                   else ["    the source of  "++showADL x++"\n"]++
                        ["    is in conflict with respect to concepts "++commaEng "and" (map showADL cs)++"\n"]++
                        ["    and the target of  "++showADL x++"\n"]++
                        ["    is in conflict with respect to concepts "++commaEng "and" (map showADL cs')++"."]
       )
 where shOrig orig =
          case orig of
            (FileLoc (FilePos (_,DatabaseDesign.Ampersand.ADL1.Pos l c,_)))
              -> "line " ++ show l++":"++show c
            _ -> show orig
   
{-
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
            ([],_,_)      -> ["    the type between the "++lSrcTrgText++" of  "++showADL a++"  and the "++rSrcTrgText++" of  "++showADL b++"  must be unambiguous."]
            (cs,_,_)      -> ["\n    between the "++lSrcTrgText++" of  "++showADL a++"  and the "++rSrcTrgText++" of  "++showADL b++",\n    concepts "++commaEng "and" (map showADL cs)++" are in conflict."]
     )

showErrEquation :: CtxError -> String
showErrEquation err@(CxeEquLike { cxeExpr=x, cxeLhs=a, cxeRhs=b})
 = concat
     ( [show (origin x)++"\n"]++
       case (cxeSrcCpts err, cxeTrgCpts err) of
            ([], [])  -> ["    Ambiguous equation  "++showADL x]
            ([_], []) -> ["    The target of  "++showADL x++"  is ambiguous"]
            ([], [_]) -> ["    The source of  "++showADL x++"  is ambiguous"]
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
-}

newcxe :: String -> CtxError
newcxe str = Cxe [] str
newcxeif :: Bool -> String -> [CtxError]
newcxeif condition cxe = [newcxe cxe | condition]
