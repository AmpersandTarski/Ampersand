module Prototype.CodeVariables (newVarFor,newSingleton,freshSingleton,pairSourceExpr,pairTargetExpr,codeVariableForBinary ) where
 import Prototype.CodeAuxiliaries (CodeVariable(..))
 import Adl (Concept(..),Expression(..),Declaration(..),Morphism(..),mIs, Prop(..),makeMph,Identified(..),isUni,source,target)
 import Prototype.RelBinGenBasics(noCollide)
 import NormalForms (simplify)

 singletonCV :: CodeVariable
 singletonCV = CodeVarScalar ""
                            S
                            False
                            (error "Single variable: cannot test whether it is sorted (in Code.hs)")
                            (Tm (mIs S) (error "Term number undefined in Code.hs"))
 -- | create a new singleton scalar variable, useful for in loops (as with newSingleton, but make sure the variable name is not taken)
 freshSingleton :: [CodeVariable] -- ^ variables with which this var should not collide by name
                -> String -- ^ preferred name
                -> Concept -- ^ type of singleton
                -> CodeVariable -- ^ result
 freshSingleton vars nm tp
   = newSingleton realname tp
     where realname = noCollide (map cvname vars) nm
 -- | create a new singleton scalar variable, used by newSingleton, but also useful for in content attribute of a new CodeVarObject
 newSingleton :: String -> Concept -> CodeVariable
 newSingleton nm tp
   = singletonCV{cvname=nm
                ,cvtype=tp
                ,cvexpression=Tm (Mp1 (nm) [] tp) (-1)}

 -- | Create a new variable with the value of some expression, ensure that its name does not exist yet
 newVarFor :: [String] -- ^ list of forbidden names
           -> Expression -- ^ value of the variable
           -> CodeVariable -- ^ the resulting variable
 newVarFor forbidden expr
  = CodeVarObject  (newVarNameFor forbidden expr) 
                   [CodeVarScalar "" (target expr) (isUni expr) False expr]
                   (source expr)
                   True
                   (Tm (V [] (S,source expr)) (error "did not assign number to Tm in Code.hs"))
                  
 -- | Create a new name with the value of some expression, ensure that its name does not exist yet
 newVarNameFor :: [String] -- ^ list of forbidden names
               -> Expression -- ^ value of the variable
               -> String -- ^ the resulting name
 newVarNameFor forbidden (Tm a _) = noCollide forbidden (name a)
 newVarNameFor forbidden (F _) = noCollide forbidden "join"
 newVarNameFor forbidden (Fix _) = noCollide forbidden "isct"
 newVarNameFor forbidden (Fux _) = noCollide forbidden "unio"
 newVarNameFor forbidden (Fdx _) = noCollide forbidden "dggr"
 newVarNameFor forbidden (K0x _) = noCollide forbidden "reflfixpt"
 newVarNameFor forbidden (K1x _) = noCollide forbidden "fixpt"
 newVarNameFor forbidden (Cpx _) = noCollide forbidden "cmplt"
 newVarNameFor forbidden _ = noCollide forbidden "expr"
 
   
 -- | Creates a codeVariable that contains the pairs indicated by some expression.
 -- | If it is possible to calculate the expression, getCodeFor should be able to get a CodeVariable constructed via codeVariableForBinary
 codeVariableForBinary :: String       -- ^ name of the variable to be created
                       -> Expression   -- ^ value of the variable is list of tuples in this Expression
                       -> CodeVariable
 codeVariableForBinary str expr
  = CodeVarObject { cvname=str
                  , content=[CodeVarScalar{ cvname  ="0"
                                          , cvtype  =source expr
                                          , multiple=False
                                          , sorted  =error "Sorted undefined (not multiple) in Code.hs"
                                          , cvexpression=srcRel
                                          }
                            ,CodeVarScalar{ cvname  ="1"
                                          , cvtype  =target expr
                                          , multiple=False
                                          , sorted  =error "Sorted undefined (not multiple) in Code.hs"
                                          , cvexpression=trgRel
                                          }
                            ]
                  , cvtype=newConcForExpr
                  , multiple=True
                  , cvexpression=Tm (V [] (S,newConcForExpr)) 1
                  }
    where 
          newConcForExpr = source srcRel -- source srcRel == source trgRel
          srcRel = pairSourceExpr (simplify (noDaggers expr))
          trgRel = pairTargetExpr (simplify (noDaggers expr))
 
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
 pairSourceDecl expr = Sgn { decnm="src", desrc=DExp expr, detrg=source expr
                           , decprps=[], decprps_calc=[Uni,Tot]
                           , decprL="", decprM="", decprR=""
                           , decpopu=error "Decl has no population (Code.hs)" -- TODO? If so, get population from expr, and be sure to adjust pairTargetDecl as well.
                           , decexplain=""
                           , decfpos=error "Decl is not on any position since it is ADL-defined (Code.hs)"
                           , decid=error "Decl has no number since it is ADL-defined (Code.hs)"
                           , deciss=False -- not a signal-relation
                           , decusr=False -- defined by ADL (i.e. not user-defined)
                           , decpat=error "Decl is not in any pattern since it is ADL-defined (Code.hs)"
                           }