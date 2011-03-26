{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.CodeVariables
  (newVarFor
  ,freshSingleton
  ,singletonCV
  ,pairSourceExpr
  ,pairTargetExpr
  ,codeVariableForBinary
  ,CodeVar(..)
  ,CodeVarIndexed(..)
  ) where
 import DatabaseDesign.Ampersand_Prototype.CodeAuxiliaries (Named(..),nameFresh,noCollide)
 import DatabaseDesign.Ampersand -- .ADL1 (Expression(..),Declaration(..),Relation(..), mIs, Prop(..),makeRelation,Concept(..))
 import DatabaseDesign.Ampersand_Prototype.CodeStatement (UseVar(..),CodeVar(..),CodeVarIndexed(..),PHPconcept(..),phpsource)
 import DatabaseDesign.Ampersand_Prototype.Version 

 fatal :: Int -> String -> a
 fatal i msg = error (fatalMsg "CodeVariables" i msg)

 -- | A data type containing the description of some variable in the target language.
 -- | see for example the singletonCV, or codeVariableForBinary
 singletonCV :: CodeVar
 singletonCV
  = CodeVar { cvIndexed    = NotIndexed
            , cvContent    = Right []
            , cvExpression = Tm (mIs (PHPC S)) (fatal 22 "Term number undefined")
            }
 -- | create a new singleton scalar variable, useful for in loops (as with newSingleton, but make sure the variable name is not taken)
 freshSingleton :: [Named CodeVar] -- ^ variables with which this var should not collide by name
                -> String -- ^ preferred name
                -> PHPconcept -- ^ (more generic) type of singleton
                -> Named CodeVar -- ^ result
 freshSingleton vars nm _
   = nameFresh vars nm (newSingleton (nm))
 -- | create a new singleton scalar variable, used by newSingleton, but also useful for in content attribute of a new CodeVarObject
 newSingleton :: String -> CodeVar
 newSingleton nm
   = singletonCV{cvExpression=(Tm (mIs (PHPI1 (Named nm (UseVar []))))(-1))}
-- WHY is newSingleton not defined as:
--   = singletonCV{cvExpression=(Tm (mIs (PHPC S))(-1))}
--   ???
 -- | Create a new variable with the value of some expression, ensure that its name does not exist yet
 newVarFor :: [String] -- ^ list of forbidden names
           -> Expression (Relation PHPconcept) -- ^ value of the variable
           -> Named CodeVar -- ^ the resulting variable
 newVarFor forbidden expr
  = Named (newVarNameFor forbidden expr)
          CodeVar{cvContent=Left CodeVar{ cvContent    = Right []
                                        , cvExpression = expr
                                        , cvIndexed    = NotIndexed}
                 ,cvIndexed=IndexByName
                 ,cvExpression=(Tm (V [] (PHPC S, phpsource expr)) (fatal 52 "did not assign number to Tm"))
                 }
    where
     -- | Create a new name with the value of some expression, ensure that its name does not exist yet
     newVarNameFor :: Identified r =>
                      [String] -- ^ list of forbidden names
                      -> Expression r -- ^ value of the variable
                      -> String -- ^ the resulting name
     newVarNameFor forb (Tm a _) = noCollide forb (name a)
     newVarNameFor forb (F _)    = noCollide forb "join"
     newVarNameFor forb (Fix _)  = noCollide forb "isct"
     newVarNameFor forb (Fux _)  = noCollide forb "unio"
     newVarNameFor forb (Fdx _)  = noCollide forb "dggr"
     newVarNameFor forb (K0x _)  = noCollide forb "reflfixpt"
     newVarNameFor forb (K1x _)  = noCollide forb "fixpt"
     newVarNameFor forb (Cpx _)  = noCollide forb "cmplt"
     newVarNameFor forb _        = noCollide forb "expr"
 
 -- | Creates a codeVariable that contains the pairs indicated by some expression.
 -- | If it is possible to calculate the expression, getCodeFor should be able to get a Named CodeVar constructed via codeVariableForBinary
 codeVariableForBinary :: String       -- ^ name of the variable to be created
                       -> Expression (Relation Concept)   -- ^ value of the variable is list of tuples in this Expression
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
                     ,cvExpression=Tm (V [] (PHPC S, phpsource srcRel)) (-1) -- source srcRel == source trgRel
                     ,cvIndexed=Indexed
                     }
    where 
          srcRel, trgRel :: Expression (Relation PHPconcept)
          srcRel = pairSourceExpr (noDaggers expr)
          trgRel = pairTargetExpr (noDaggers expr)
 
 -- | removes all daggers from the given expression
 noDaggers :: Expression r -> Expression r
 noDaggers (F (fs)) = F (map noDaggers fs)
 noDaggers (Fix (fs)) = Fix (map noDaggers fs)
 noDaggers (Fux (fs)) = Fix (map noDaggers fs)
 noDaggers (Fdx (fs)) = Cpx (F (map noDaggers (map Cpx fs)))
 noDaggers (Cpx (Cpx a)) = a
 noDaggers (Cpx (Fdx fs)) = F (map noDaggers (map Cpx fs))
 noDaggers (K0x e) = K0x (noDaggers e)
 noDaggers (K1x e) = K1x (noDaggers e)
 noDaggers c = c
 
 pairSourceExpr :: Expression (Relation Concept) -> Expression (Relation PHPconcept)
 pairSourceExpr e = Tm (makeRelation (pairSourceDecl e)) (fatal 102 "Term number undefined for pairSourceExpr")
 pairSourceDecl :: Expression (Relation Concept) -> Declaration PHPconcept
 pairSourceDecl expr = Sgn { decnm="src", desrc=PHPexp expr, detrg=PHPC (source expr), decplug=True
                           , decprps=[], decprps_calc=[Uni,Tot]
                           , decprL="", decprM="", decprR=""
                           , decpopu=fatal 107 "Decl has no population" -- TODO? If so, get population from expr, and be sure to adjust pairTargetDecl as well.
                           , decfpos=fatal 108 "Decl is not on any position since it was generated by Ampersand"
                           , decid=fatal 109 "Decl has no number since it was generated by Ampersand"
                           , deciss=False -- not a signal-relation
                           , decusr=False -- defined by Ampersand (i.e. not user-defined)
                           , decpat=fatal 112 "Decl is not in any pattern since it was generated by Ampersand"
                           }

 pairTargetExpr :: Expression (Relation Concept) -> Expression (Relation PHPconcept)
 pairTargetExpr e = Tm (makeRelation (pairTargetDecl e)) (fatal 116 "Term number undefined for pairTargetExpr")
 pairTargetDecl :: Expression (Relation Concept) -> Declaration PHPconcept
 pairTargetDecl e = (pairSourceDecl e){decnm="trg",detrg=PHPC (target e)}
