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
 import DatabaseDesign.Ampersand_Prototype.CoreImporter
 import DatabaseDesign.Ampersand_Prototype.CodeStatement (UseVar(..),CodeVar(..),CodeVarIndexed(..),PHPconcept(..),phpsource)
 import DatabaseDesign.Ampersand_Prototype.Version 

 fatal :: Int -> String -> a
 fatal = fatalMsg "CodeVariables"

 -- | A data type containing the description of some variable in the target language.
 -- | see for example the singletonCV, or codeVariableForBinary
 singletonCV :: CodeVar
 singletonCV
  = CodeVar { cvIndexed    = NotIndexed
            , cvContent    = Right []
            , cvExpression = ERel (I (PHPC ONE))
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
   = singletonCV{cvExpression=(ERel (I (PHPI1 (Named nm (UseVar [])))))}
-- WHY is newSingleton not defined as:
--   = singletonCV{cvExpression=(ERel (I (PHPC S)))}
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
                 ,cvExpression=(ERel (V (Sign (PHPC ONE) (phpsource expr))))
                 }
    where
     -- | Create a new name with the value of some expression, ensure that its name does not exist yet
     newVarNameFor :: Identified r =>
                      [String] -- ^ list of forbidden names
                      -> Expression r -- ^ value of the variable
                      -> String -- ^ the resulting name
     newVarNameFor forb (ERel a) = noCollide forb (name a)
     newVarNameFor forb (ECps _)    = noCollide forb "join"
     newVarNameFor forb (EIsc _)  = noCollide forb "isct"
     newVarNameFor forb (EUni _)  = noCollide forb "unio"
     newVarNameFor forb (ERad _)  = noCollide forb "dggr"
     newVarNameFor forb (EKl0 _)  = noCollide forb "reflfixpt"
     newVarNameFor forb (EKl1 _)  = noCollide forb "fixpt"
     newVarNameFor forb (ECpl _)  = noCollide forb "cmplt"
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
                     ,cvExpression=ERel (V (Sign (PHPC ONE) (phpsource srcRel))) -- source srcRel == source trgRel
                     ,cvIndexed=Indexed
                     }
    where  
          srcRel, trgRel :: Expression (Relation PHPconcept)
          srcRel = pairSourceExpr (noDaggers expr)
          trgRel = pairTargetExpr (noDaggers expr)
 
 -- | removes all daggers from the given expression
 noDaggers :: Expression r -> Expression r
 noDaggers (ECps (fs)) = ECps (map noDaggers fs)
 noDaggers (EIsc (fs)) = EIsc (map noDaggers fs)
 noDaggers (EUni (fs)) = EIsc (map noDaggers fs)
 noDaggers (ERad (fs)) = ECpl (ECps (map noDaggers (map ECpl fs)))
 noDaggers (ECpl (ECpl a)) = a
 noDaggers (ECpl (ERad fs)) = ECps (map noDaggers (map ECpl fs))
 noDaggers (EKl0 e) = EKl0 (noDaggers e)
 noDaggers (EKl1 e) = EKl1 (noDaggers e)
 noDaggers c = c
 
 pairSourceExpr :: Expression (Relation Concept) -> Expression (Relation PHPconcept)
 pairSourceExpr e = ERel (makeRelation (pairSourceDecl e))
 pairSourceDecl :: Expression (Relation Concept) -> Declaration PHPconcept
 pairSourceDecl expr = Sgn { decnm="src", decsgn=Sign (PHPexp expr) (PHPC (source expr)), decplug=True
                           , decprps=[], decprps_calc=[Uni,Tot]
                           , decprL="", decprM="", decprR="", decMean=""
                           , decpopu=fatal 107 "Decl has no population" -- TODO? If so, get population from expr, and be sure to adjust pairTargetDecl as well.
                           , decfpos=fatal 108 "Decl is not on any position since it was generated by Ampersand"
                           , deciss=False -- not a signal-relation
                           , decusr=False -- defined by Ampersand (i.e. not user-defined)
                           , decpat=fatal 112 "Decl is not in any pattern since it was generated by Ampersand"
                           }

 pairTargetExpr :: Expression (Relation Concept) -> Expression (Relation PHPconcept)
 pairTargetExpr e = ERel (makeRelation (pairTargetDecl e))
 pairTargetDecl :: Expression (Relation Concept) -> Declaration PHPconcept
 pairTargetDecl e = (pairSourceDecl e){decnm="trg",detrg=PHPC (target e)}
