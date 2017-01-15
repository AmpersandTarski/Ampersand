module Ampersand.Core.ShowAStruct
  (AStruct(..))
where

import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ShowPStruct
import Ampersand.Core.A2P_Converters
import Ampersand.Basics
import Ampersand.ADL1 (insParentheses)

class AStruct a where
 showA :: a -> String

instance AStruct A_Context where
 showA = showP . aCtx2pCtx

instance AStruct Expression where  
 --showA = aExpression2pTermPrim . showP --HJO, 20170109: TODO: This should be equivalent (and much beter for maintainability.)                                  
 showA = showExpr (" = ", " |- ", " /\\ ", " \\/ ", " - ", " / ", " \\ ", " <> ", ";", "!", "*", "*", "+", "~", ("-"++), "(", ")", "[", "*", "]")
-- NOTE: retain space after \\, because of unexpected side effects if it is used just before an 'r' or 'n'....
   where
     showExpr :: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String -> String,String,String,String,String,String)
            -> Expression -> String
     showExpr    (equiv,  inclu,  inter, union',diff,  lresi, rresi, rDia, rMul  , rAdd , rPrd ,closK0,closK1,flp',  compl,           lpar,  rpar,  lbr,   star,  rbr)  expr
      = showchar (insParentheses expr)
        where
          showchar (EEqu (l,r)) = showchar l++equiv++showchar r
          showchar (EInc (l,r)) = showchar l++inclu++showchar r
          showchar (EIsc (l,r)) = showchar l++inter++showchar r
          showchar (EUni (l,r)) = showchar l++union'++showchar r
          showchar (EDif (l,r)) = showchar l++diff ++showchar r
          showchar (ELrs (l,r)) = showchar l++lresi++showchar r
          showchar (ERrs (l,r)) = showchar l++rresi++showchar r
          showchar (EDia (l,r)) = showchar l++rDia++showchar r
          showchar (ECps (l,r)) = showchar l++rMul++showchar r
          showchar (ERad (l,r)) = showchar l++rAdd++showchar r
          showchar (EPrd (l,r)) = showchar l++rPrd++showchar r
          showchar (EKl0 e)     = showchar e++closK0
          showchar (EKl1 e)     = showchar e++closK1
          showchar (EFlp e)     = showchar e++flp'
          showchar (ECpl e)     = compl (showchar e)
          showchar (EBrk e)     = lpar++showchar e++rpar
          showchar (EDcD dcl)   = name dcl
          showchar (EDcI c)     = "I"++lbr++name c++rbr
          showchar (EEps i _)   = "I{-Eps-}"++lbr++name i++rbr
          showchar (EDcV sgn)   = "V"++lbr++name (source sgn)++star++name (target sgn)++rbr
          showchar (EMp1 val c) = "'"++showWithoutDoubleQuotes val++"'"++lbr++name c++rbr
            
          showWithoutDoubleQuotes str = 
            case showP str of
              []  -> []
              [c] -> [c]
              cs  -> if head cs == '\"' && last cs == '\"'
                     then reverse . tail . reverse .tail $ cs
                     else cs

instance AStruct A_Concept where
 showA c = show (name c)

instance AStruct A_Gen where
 showA = showP . aGen2pGen 

instance AStruct Rule where
 showA = showP . aRule2pRule

instance AStruct Declaration where
  showA = showP . aDeclaration2pDeclaration

instance AStruct AAtomPair where
 showA p = "("++showA (apLeft p)++","++ showA (apRight p)++")"

instance AStruct AAtomValue where
 showA at = case at of
              AAVString{} -> show (aavstr at)
              AAVInteger _ i   -> show i
              AAVFloat   _ f   -> show f
              AAVBoolean _ b   -> show b
              AAVDate _ day    -> show day
              AAVDateTime _ dt -> show dt
              AtomValueOfONE -> "1"

instance AStruct ExplObj where
 showA = showP . aExplObj2PRef2Obj

