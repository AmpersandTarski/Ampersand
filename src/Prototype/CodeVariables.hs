{-# OPTIONS_GHC -Wall #-}
module Prototype.CodeVariables
  (newVarFor
  ,freshSingleton
  ,singletonCV
  ,pairSourceExpr
  ,pairTargetExpr
  ,codeVariableForBinary
  ,CodeVar(..)
  ,CodeVarIndexed(..)
  ) where
 import Prototype.CodeAuxiliaries (Named(..),nameFresh)
 import Adl (Concept(..),Expression(..),Declaration(..),Morphism(..),mIs, Prop(..),makeMph,Identified(..),source,target)
 import Strings(noCollide)
 import {-# SOURCE #-} Prototype.CodeStatement (UseVar(..))

 -- | A data type containing the description of some variable in the target language.
 -- | see for example the singletonCV, or codeVariableForBinary
 data CodeVar = CodeVar
  { cvIndexed :: CodeVarIndexed
    -- | Content can either be a CodeVar, intended for indexed stuff: $var[$i] returns a codeVar,
    --                    OR  [Named CodeVar], intended for objects/associative arrays: $var["nName"] is a codeVar 
  , cvContent :: Either CodeVar [Named CodeVar] 
  , cvExpression :: Expression
  } deriving (Eq)
 data CodeVarIndexed = Indexed | NotIndexed | IndexByName deriving (Eq,Show)
 
 instance Show CodeVar where
   show (CodeVar i c e) = show i++" "++show c++" '"++show e++"'"
 

 singletonCV :: CodeVar
 singletonCV = CodeVar NotIndexed (Right []) (Tm (mIs S) (error "Term number undefined in Code.hs"))
 -- | create a new singleton scalar variable, useful for in loops (as with newSingleton, but make sure the variable name is not taken)
 freshSingleton :: [Named CodeVar] -- ^ variables with which this var should not collide by name
                -> String -- ^ preferred name
                -> Concept -- ^ (more generic) type of singleton
                -> Named CodeVar -- ^ result
 freshSingleton vars nm _
   = nameFresh vars nm (newSingleton (nm))
 -- | create a new singleton scalar variable, used by newSingleton, but also useful for in content attribute of a new CodeVarObject
 newSingleton :: String -> CodeVar
 newSingleton nm
   = singletonCV{cvExpression=(Tm (mIs (I1 (Named nm (UseVar []))))(-1))}
 -- | Create a new variable with the value of some expression, ensure that its name does not exist yet
 newVarFor :: [String] -- ^ list of forbidden names
           -> Expression -- ^ value of the variable
           -> Named CodeVar -- ^ the resulting variable
 newVarFor forbidden expr
  = Named (newVarNameFor forbidden expr) 
          CodeVar{cvContent=Left CodeVar{cvContent=Right [],cvExpression=expr,cvIndexed=NotIndexed}
                 ,cvIndexed=IndexByName
                 ,cvExpression=(Tm (V [] (S,source expr)) (error "did not assign number to Tm in Code.hs"))
                 }
 
 -- | Create a new name with the value of some expression, ensure that its name does not exist yet
 newVarNameFor :: [String] -- ^ list of forbidden names
               -> Expression -- ^ value of the variable
               -> String -- ^ the resulting name
 newVarNameFor forbidden (Tm a _) = noCollide forbidden (name a)
 newVarNameFor forbidden (F _)   = noCollide forbidden "join"
 newVarNameFor forbidden (Fix _) = noCollide forbidden "isct"
 newVarNameFor forbidden (Fux _) = noCollide forbidden "unio"
 newVarNameFor forbidden (Fdx _) = noCollide forbidden "dggr"
 newVarNameFor forbidden (K0x _) = noCollide forbidden "reflfixpt"
 newVarNameFor forbidden (K1x _) = noCollide forbidden "fixpt"
 newVarNameFor forbidden (Cpx _) = noCollide forbidden "cmplt"
 newVarNameFor forbidden _       = noCollide forbidden "expr"
 
   
 -- | Creates a codeVariable that contains the pairs indicated by some expression.
 -- | If it is possible to calculate the expression, getCodeFor should be able to get a Named CodeVar constructed via codeVariableForBinary
 codeVariableForBinary :: String       -- ^ name of the variable to be created
                       -> Expression   -- ^ value of the variable is list of tuples in this Expression
                       -> Named CodeVar
 codeVariableForBinary str expr
  = Named str CodeVar{cvContent=Right [Named "0" CodeVar{cvIndexed=NotIndexed
                                                        ,cvExpression=srcRel
                                                        ,cvContent=Right []
                                                        }
                                      ,Named "1" CodeVar{cvIndexed=NotIndexed
                                                        ,cvExpression=trgRel
                                                        ,cvContent=Right []
                                                        }
                                      ]
                     ,cvExpression=Tm (V [] (S,newConcForExpr)) (-1)
                     ,cvIndexed=Indexed
                     }
    where 
          newConcForExpr = source srcRel -- source srcRel == source trgRel
          srcRel = pairSourceExpr (noDaggers expr)
          trgRel = pairTargetExpr (noDaggers expr)
 
 -- | removes all daggers from the given expression
 noDaggers :: Expression -> Expression
 noDaggers (F (fs)) = F (map noDaggers fs)
 noDaggers (Fix (fs)) = Fix (map noDaggers fs)
 noDaggers (Fux (fs)) = Fix (map noDaggers fs)
 noDaggers (Fdx (fs)) = Cpx (F (map noDaggers (map Cpx fs)))
 noDaggers (Cpx (Cpx a)) = a
 noDaggers (Cpx (Fdx fs)) = F (map noDaggers (map Cpx fs))
 noDaggers (K0x e) = K0x (noDaggers e)
 noDaggers (K1x e) = K1x (noDaggers e)
 noDaggers c = c
 
 pairSourceExpr :: Expression -> Expression
 pairSourceExpr e = Tm (pairSource e) (error "Term number undefined for pairSourceExpr in Code.hs")
 pairTargetExpr :: Expression -> Expression
 pairTargetExpr e = Tm (pairTarget e) (error "Term number undefined for pairSourceExpr in Code.hs")
 pairSource :: Expression -> Morphism
 pairSource e = makeMph (pairSourceDecl e)
 pairTarget :: Expression -> Morphism
 pairTarget e = makeMph (pairTargetDecl e)
 pairTargetDecl :: Expression -> Declaration
 pairTargetDecl e = (pairSourceDecl e){decnm="trg",detrg=target e}
 pairSourceDecl :: Expression -> Declaration
 pairSourceDecl expr = Sgn { decnm="src", desrc=DExp expr, detrg=source expr, decplug=True
                           , decprps=[], decprps_calc=[Uni,Tot]
                           , decprL="", decprM="", decprR=""
                           , decpopu=error "Decl has no population (Code.hs)" -- TODO? If so, get population from expr, and be sure to adjust pairTargetDecl as well.
                           , decfpos=error "Decl is not on any position since it is ADL-defined (Code.hs)"
                           , decid=error "Decl has no number since it is ADL-defined (Code.hs)"
                           , deciss=False -- not a signal-relation
                           , decusr=False -- defined by ADL (i.e. not user-defined)
                           , decpat=error "Decl is not in any pattern since it is ADL-defined (Code.hs)"
                           }