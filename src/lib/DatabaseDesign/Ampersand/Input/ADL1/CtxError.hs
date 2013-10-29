{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Input.ADL1.CtxError
  ( CtxError(PE)
  , showErr
  , cannotDisamb, cannotDisambRel
  , mustBeOrdered, mustBeOrderedLst
  , mustBeBound
  , GetOneGuarded(..)
  , Guarded(..)
  , (<?>)
  )
-- SJC: I consider it ill practice to export CTXE
-- Reason: CtxError should obtain all error messages
-- By not exporting anything that takes a string, we prevent other modules from containing error message
-- If you build a function that must generate an error, put it in CtxError and call it instead
-- see `getOneExactly' / `GetOneGuarded' as a nice example
-- Although I also consider it ill practice to export PE, I did this as a quick fix for the parse errors
where
import Control.Applicative
import DatabaseDesign.Ampersand.ADL1 (Pos(..),source,target,sign,Expression(EDcV),A_Concept)
import DatabaseDesign.Ampersand.Fspec.ShowADL
import DatabaseDesign.Ampersand.Basics
-- import Data.Traversable
import Data.List  (intercalate)
import DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner (Token)
import DatabaseDesign.Ampersand.Input.ADL1.UU_Parsing (Message)
import DatabaseDesign.Ampersand.Core.ParseTree
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree (Declaration,Association)

fatal :: Int -> String -> a
fatal = fatalMsg "Input.ADL1.CtxError"

infixl 4 <?>
(<?>) :: (t -> Guarded a) -> Guarded t -> Guarded a  -- This is roughly the monadic definition for >>=, but it does not satisfy the corresponding rules so it cannot be a monad
(<?>) _ (Errors  a) = Errors a -- note the type change
(<?>) f (Checked a) = f a

data CtxError = CTXE Origin String -- SJC: I consider it ill practice to export CTXE, see remark at top
              | PE (Message Token)
              deriving Show

class GetOneGuarded a where
  getOneExactly :: (Traced a1, ShowADL a1) => a1 -> [a] -> Guarded a

instance GetOneGuarded Declaration where
  getOneExactly _ [d] = Checked d
  getOneExactly o []  = Errors [CTXE (origin o)$ "No declaration for "++showADL o]
  getOneExactly o lst = Errors [CTXE (origin o)$ "Too many declarations match "++showADL o++".\n  Be more specific. These are the matching declarations:"++concat ["\n  - "++showADL l++" at "++(showFullOrig$origin l) | l<-lst]]

cannotDisambRel :: (Traced a1, ShowADL a1, ShowADL a2, Association a2) => a1 -> [a2] -> Guarded a
cannotDisambRel o [] = Errors [CTXE (origin o)$ "No declarations match the relation: "++showADL o]
cannotDisambRel o lst = Errors [CTXE (origin o)$ "Cannot disambiguate the relation: "++showADL o++"\n  Please add a signature (e.g. [A*B]) to the relation.\n  Relations you may have intended:"++concat ["\n  "++showADL l++"["++showADL (source l)++"*"++showADL (target l)++"]"|l<-lst]]
cannotDisamb :: (Traced a1, ShowADL a1) => a1 -> Guarded a
cannotDisamb o = Errors [CTXE (origin o)$ "Cannot disambiguate: "++showADL o++"\n  Please add a signature to it"]

mustBeOrdered :: Origin -> (SrcOrTgt, Expression) -> (SrcOrTgt, Expression) -> Guarded a
mustBeOrdered o (p1,e1) (p2,e2)
 = Errors [CTXE o$ "Type error, cannot match:\n  the concept "++c1++" ("++show p1++" of "++showADL e1++")"
                                          ++"\n  and concept "++c2++" ("++show p2++" of "++showADL e2++")."
                   ++"\n  if you think there is no type error, add an order between concepts "++c1++" and "++c2++"."]
 where c1 = showADL$case p1 of {Src -> source e1;Tgt->target e1}
       c2 = showADL$case p2 of {Src -> source e2;Tgt->target e2}
mustBeOrderedLst :: Origin -> (SrcOrTgt, Expression) -> (SrcOrTgt, Expression) -> [[A_Concept]] -> Guarded a
mustBeOrderedLst o (p1,e1) (p2,e2) cs
 = Errors [CTXE o$ "Ambiguous type when matching: "++show p1++" of "++showADL e1++"\n"
                                          ++" and "++show p2++" of "++showADL e2++".\n"
                   ++"  The type can be "++intercalate " or " (map (showADL . Slash) cs)
                   ++"\n  None of these concepts is known to be the smallest, you may want to add an order between them."]

newtype Slash a = Slash [a]
instance ShowADL a => ShowADL (Slash a) where
  showADL (Slash x) = intercalate "/" (map showADL x)
  

mustBeBound :: (Show a1) => Origin -> [(a1, Expression)] -> Guarded a
mustBeBound o [(p,e)]
 = Errors [CTXE o$ "An ambiguity arises in type checking. Be more specific in the "++show p++" of the expression "++showADL e++".\n"++
                   "  You could add more types inside the expression, or just write ("++showADL e++") /\\ "++showADL (EDcV$ sign e)++".\n"]
mustBeBound o lst
 = Errors [CTXE o$ "An ambiguity arises in type checking. Be more specific in the expressions "++intercalate " and " (map (showADL . snd) lst) ++".\n"++
                   "  You could add more types inside the expression, or write:"++
                   concat ["\n  ("++showADL e++") /\\ "++showADL (EDcV$ sign e) ++ "["++showADL (source e)++"*"++showADL (target e)++"]"| (_,e)<-lst]]

data Guarded a = Errors [CtxError] | Checked a deriving Show

instance Functor Guarded where
 fmap _ (Errors a) = (Errors a)
 fmap f (Checked a) = Checked (f a)
 
instance Applicative Guarded where
 pure = Checked
 (<*>) (Checked f) (Checked a) = Checked (f a)
 (<*>) (Errors  a) (Checked _) = Errors a 
 (<*>) (Checked _) (Errors  b) = Errors b
 (<*>) (Errors  a) (Errors  b) = Errors (a ++ b) -- this line makes Guarded not a monad
 -- Guarded is NOT a monad!
 -- Reason: (<*>) has to be equal to `ap' if it is, and this definition is different
 -- Use <?> if you wish to use the monad-like thing

showErr :: CtxError -> String
showErr (CTXE o s)
 = s ++ "\n  " ++ showFullOrig o
showErr (PE s)
 = show s

showFullOrig :: Origin -> String
showFullOrig (FileLoc (FilePos (_,DatabaseDesign.Ampersand.ADL1.Pos l c,t)))
              = "Error at symbol "++ t ++ " at line " ++ show l++" : "++show c
showFullOrig x = show x
