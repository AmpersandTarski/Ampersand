{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
module DatabaseDesign.Ampersand.Input.ADL1.CtxError
  ( CtxError(PE)
  , showErr
  , cannotDisamb, cannotDisambRel
  , mustBeOrdered, mustBeOrderedLst, mustBeOrderedConcLst
  , mustBeBound
  , GetOneGuarded(..), uniqueNames
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
import DatabaseDesign.Ampersand.ADL1 (Pos(..),source,target,sign,Expression(EDcV,ECpl),A_Concept,SubInterface)
import DatabaseDesign.Ampersand.Fspec.ShowADL
import DatabaseDesign.Ampersand.Basics
-- import Data.Traversable
import Data.List  (intercalate, partition)
import DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner (Token)
import DatabaseDesign.Ampersand.Input.ADL1.UU_Parsing (Message)
import DatabaseDesign.Ampersand.Core.ParseTree (TermPrim(..),P_ViewD(..),P_SubIfc,Traced(..), Origin(..), SrcOrTgt(..),FilePos(..))
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree (Declaration,Association)

fatal,_notUsed :: Int -> String -> a
fatal = fatalMsg "Input.ADL1.CtxError"
_notUsed = fatal

infixl 4 <?>
(<?>) :: (t -> Guarded a) -> Guarded t -> Guarded a  -- This is roughly the monadic definition for >>=, but it does not satisfy the corresponding rules so it cannot be a monad
(<?>) _ (Errors  a) = Errors a -- note the type change
(<?>) f (Checked a) = f a

data CtxError = CTXE Origin String -- SJC: I consider it ill practice to export CTXE, see remark at top
              | PE (Message Token)
              deriving Show

errors :: Guarded t -> [CtxError]
errors (Checked _) = []
errors (Errors lst) = lst

class GetOneGuarded a where
  getOneExactly :: (Traced a1, ShowADL a1) => a1 -> [a] -> Guarded a
  getOneExactly _ [a] = Checked a
  getOneExactly o l@[] = hasNone l o
  getOneExactly o lst = Errors [CTXE o'$ "Found too many:\n  "++s | CTXE o' s <- errors (hasNone lst o)]
  hasNone :: (Traced a1, ShowADL a1) => [a] -- this argument should be ignored! It is only here to help indicate a type (you may put [])
                                     -> a1  -- the object where the problem is arising
                                     -> Guarded a
  hasNone _ o = getOneExactly o []

instance GetOneGuarded (P_SubIfc a) where
  hasNone _ o = Errors [CTXE (origin o)$ "Required: one subinterface in "++showADL o]

instance GetOneGuarded (SubInterface) where
  hasNone _ o = Errors [CTXE (origin o)$ "Required: one subinterface in "++showADL o]

instance GetOneGuarded Declaration where
  getOneExactly _ [d] = Checked d
  getOneExactly o []  = Errors [CTXE (origin o)$ "No declaration for "++showADL o]
  getOneExactly o lst = Errors [CTXE (origin o)$ "Too many declarations match "++showADL o++".\n  Be more specific. These are the matching declarations:"++concat ["\n  - "++showADL l++" at "++(showFullOrig$origin l) | l<-lst]]

cannotDisambRel :: (ShowADL a2, Association a2) => (TermPrim) -> [a2] -> Guarded a
cannotDisambRel o [] = Errors [CTXE (origin o)$ "No declarations match the relation: "++showADL o]
cannotDisambRel o@Prel{} lst = Errors [CTXE (origin o)$ "Cannot disambiguate the relation: "++showADL o++"\n  Please add a signature (e.g. [A*B]) to the relation.\n  Relations you may have intended:"++concat ["\n  "++showADL l++"["++showADL (source l)++"*"++showADL (target l)++"]"|l<-lst]]
cannotDisambRel o lst = Errors [CTXE (origin o)$ "Cannot disambiguate: "++showADL o++"\n  Please add a signature.\n  You may have intended one of these:"++concat ["\n  "++showADL l|l<-lst]]
cannotDisamb :: (Traced a1, ShowADL a1) => a1 -> Guarded a
cannotDisamb o = Errors [CTXE (origin o)$ "Cannot disambiguate: "++showADL o++"\n  Please add a signature to it"]

uniqueNames :: (Identified a, Traced a) =>
                     [a] -> Guarded ()
uniqueNames a = case findDuplicates (map name a) of
                  [] -> pure ()
                  [r] -> Errors [CTXE (origin (head a))$ "Names / labels must be unique. "++show r++", however, is not."]
                  r -> Errors [CTXE (origin (head a))$ "Names / labels must be unique. The following are not: "++concat ["\n  - "++l'|l'<-r]]
findDuplicates :: Eq a => [a] -> [a]
findDuplicates [] = []
findDuplicates (a:as)
 = case (partition (== a) as) of
    ([],r) -> findDuplicates r
    (_,r) -> a:findDuplicates r

class ErrorConcept a where
  showEC :: a -> String
  showMini :: a -> String

instance ErrorConcept (P_ViewD a) where
  showEC x = showADL (vd_cpt x) ++" given in VIEW "++vd_lbl x
  showMini x = showADL (vd_cpt x)

instance (ShowADL a2) => ErrorConcept (SrcOrTgt, A_Concept, a2) where
  showEC (p1,c1,e1) = showADL c1++" ("++show p1++" of "++showADL e1++")"
  showMini (_,c1,_) = showADL c1

instance (ShowADL a2, Association a2) => ErrorConcept (SrcOrTgt, a2) where
  showEC (p1,e1)
   = case p1 of
      Src -> showEC (p1,source e1,e1)
      Tgt -> showEC (p1,target e1,e1)
  showMini (p1,e1)
   = case p1 of
      Src -> showMini (p1,source e1,e1)
      Tgt -> showMini (p1,target e1,e1)

mustBeOrdered :: (Traced a1, ErrorConcept a2, ErrorConcept a3) => a1 -> a2 -> a3 -> Guarded a
mustBeOrdered o a b
 = Errors [CTXE (origin o)$ "Type error, cannot match:\n  the concept "++showEC a
                                          ++"\n  and concept "++showEC b
                   ++"\n  if you think there is no type error, add an order between concepts "++showMini a++" and "++showMini b++"."]

mustBeOrderedLst :: (Traced o, ShowADL o, ShowADL a) => o -> [(A_Concept, SrcOrTgt, a)] -> Guarded b
mustBeOrderedLst o lst
 = Errors [CTXE (origin o)$ "Type error in "++showADL o++"\n  Cannot match:"++ concat
             [ "\n  - concept "++showADL c++", "++show st++" of "++showADL a
             | (c,st,a) <- lst ] ++
             "\n  if you think there is no type error, add an order between the mismatched concepts."
          ]
 


mustBeOrderedConcLst :: Origin -> (SrcOrTgt, Expression) -> (SrcOrTgt, Expression) -> [[A_Concept]] -> Guarded a
mustBeOrderedConcLst o (p1,e1) (p2,e2) cs
 = Errors [CTXE o$ "Ambiguous type when matching: "++show p1++" of "++showADL e1++"\n"
                                          ++" and "++show p2++" of "++showADL e2++".\n"
                   ++"  The type can be "++intercalate " or " (map (showADL . Slash) cs)
                   ++"\n  None of these concepts is known to be the smallest, you may want to add an order between them."]

newtype Slash a = Slash [a]
instance ShowADL a => ShowADL (Slash a) where
  showADL (Slash x) = intercalate "/" (map showADL x)
  

mustBeBound :: Origin -> [(SrcOrTgt, Expression)] -> Guarded a
mustBeBound o [(p,e)]
 = Errors [CTXE o$ "An ambiguity arises in type checking. Be more specific by binding the "++show p++" of the expression "++showADL e++".\n"++
                   "  You could add more types inside the expression, or just write "++writeBind e++"."]
mustBeBound o lst
 = Errors [CTXE o$ "An ambiguity arises in type checking. Be more specific in the expressions "++intercalate " and " (map (showADL . snd) lst) ++".\n"++
                   "  You could add more types inside the expression, or write:"++
                   concat ["\n  "++writeBind e| (_,e)<-lst]]

writeBind :: Expression -> String
writeBind (ECpl e)
 = "("++showADL (EDcV (sign e))++"["++showADL (source e)++"*"++showADL (target e)++"]"++" - "++showADL e++")"
writeBind e
 = "("++showADL e++") /\\ "++showADL (EDcV (sign e))++"["++showADL (source e)++"*"++showADL (target e)++"]"

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
showFullOrig (FileLoc (FilePos (filename,DatabaseDesign.Ampersand.ADL1.Pos l c,t)))
              = "Error at symbol "++ t ++ " in file " ++ filename++" at line " ++ show l++" : "++show c
showFullOrig x = show x
