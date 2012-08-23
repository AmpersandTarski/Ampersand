{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Input.ADL1.CtxError
       (newcxe,newcxeif,CtxError(..)) --constructors of CtxError are not exported, use constructor functions
where
import DatabaseDesign.Ampersand.Input.ADL1.FilePos
import DatabaseDesign.Ampersand.Parsing 
import DatabaseDesign.Ampersand.Core.ParseTree

data CtxError = CxeEqConcepts {cxeConcepts :: [P_Concept]    -- ^ The list of concepts that have been proven equal.
                              }
              | CxeEqAttribs  {cxeOrig :: Origin          -- ^ The location of the object/key definition in which different attributes have the same name.
                              ,cxeName :: String          -- ^ The name shared by different attributes.
                              ,cxeAtts :: [Term]  -- ^ The list of attributes with the same name.
                              }
              | CxeAmbExpr    {cxeExpr :: Term   -- ^ an erroneous expression, which is ambiguous
                              ,cxeSrcT :: [P_Concept]    -- ^ The applicable types for the source (length must be >1)
                              ,cxeTrgT :: [P_Concept]    -- ^ The applicable types for the target (length must be >1)
                              ,cxeSign :: [P_Sign]       -- ^ The applicable types for the expression (length must be >1)
                              }
              | CxeILike      {cxeExpr :: Term
                              ,cxeCpts :: [P_Concept]
                              }       
              | CxeCpl        {cxeExpr :: Term
                              ,cxeCpts :: [P_Concept]
                              }       
              | CxeEquLike    {cxeExpr :: Term
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
                deriving Eq
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

