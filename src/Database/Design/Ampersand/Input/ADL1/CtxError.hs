{-# OPTIONS_GHC -XFlexibleInstances #-}
module Database.Design.Ampersand.Input.ADL1.CtxError
  ( CtxError(PE)
  , showErr
  , cannotDisamb, cannotDisambRel
  , mustBeOrdered, mustBeOrderedLst, mustBeOrderedConcLst
  , mustBeBound
  , GetOneGuarded(..), uniqueNames, mkDanglingPurposeError
  , mkUndeclaredError, mkMultipleInterfaceError, mkInterfaceRefCycleError, mkNonMatchingError
  , mkMultipleDefaultError
  , Guarded(..)
  , whenCheckedIO
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
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.FSpec.ShowADL
import Database.Design.Ampersand.Basics
-- import Data.Traversable
import Data.List  (intercalate)
import GHC.Exts (groupWith)
import Database.Design.Ampersand.Input.ADL1.UU_Scanner (Token)
import UU.Parsing (Message(..),Action(..))
import Database.Design.Ampersand.Core.ParseTree

fatal,_notUsed :: Int -> String -> a
fatal = fatalMsg "Input.ADL1.CtxError"
_notUsed = fatal

infixl 4 <?>
(<?>) :: (t -> Guarded a) -> Guarded t -> Guarded a  -- This is roughly the monadic definition for >>=, but it does not satisfy the corresponding rules so it cannot be a monad
(<?>) _ (Errors  a) = Errors a -- note the type change
(<?>) f (Checked a) = f a

data CtxError = CTXE Origin String -- SJC: I consider it ill practice to export CTXE, see remark at top
              | PE (Message Token (Maybe Token))
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

uniqueNames :: (Named a, Traced a) =>
                     [a] -> Guarded ()
uniqueNames a = case (filter moreThanOne . groupWith name)  a of
                  [] -> pure ()
                  xs -> Errors (map messageFor xs)
    where
     moreThanOne (_:_:_) = True
     moreThanOne  _      = False
     messageFor :: (Named a, Traced a) => [a] -> CtxError
     messageFor (x:xs) = CTXE (origin x)
                      ("Names / labels must be unique. "++(show . name) x++", however, is used at:"++
                        concatMap (("\n    "++ ) . show . origin) (x:xs)
                        ++"."
                       )
     messageFor _ = fatal 90 "messageFor must only be used on lists with more that one element!"

mkDanglingPurposeError :: Purpose -> CtxError
mkDanglingPurposeError p = CTXE (origin p) $ "Purpose refers to non-existent " ++ showADL (explObj p) 
-- Unfortunately, we cannot use position of the explanation object itself because it is not an instance of Trace.


-- TODO: Fix, entity  is not the ref'ed entity now
mkUndeclaredError :: (Traced e, Named e) => String -> e -> String -> String -> CtxError
mkUndeclaredError entity objDef containingIfcName ref = 
  CTXE (origin objDef) $ "Undeclared " ++ entity ++ " " ++ show ref ++ " referenced at field " ++ 
                         show (name objDef) ++ " of interface " ++ show containingIfcName ++ "."

mkMultipleInterfaceError :: String -> Interface -> [Interface] -> CtxError
mkMultipleInterfaceError role ifc duplicateIfcs = 
  CTXE (origin ifc) $ "Multiple interfaces named " ++ show (name ifc) ++ " for role " ++ show role ++ ":" ++ 
                      concatMap (("\n    "++ ) . show . origin) (ifc:duplicateIfcs)       

mkInterfaceRefCycleError :: [Interface] -> CtxError
mkInterfaceRefCycleError []                 = fatal 108 "mkInterfaceRefCycleError called on []"
mkInterfaceRefCycleError cyclicIfcs@(ifc:_) = -- take the first one (there will be at least one) as the origin of the error
  CTXE (origin ifc) $ "Interfaces form a reference cycle:\n" ++
                      unlines [ "- " ++ show (name i) ++ " at position " ++ show (origin i) | i <- cyclicIfcs ] 
                              
mkNonMatchingError ::  String -> ObjectDef -> A_Concept -> A_Concept -> String -> CtxError 
mkNonMatchingError entity objDef t s ref
 = CTXE (origin objDef) $ "The referenced "++entity++" "++show ref++" is of type "++show (name s)++", which does not match the required type "++show (name t)++"."

mkMultipleDefaultError :: (A_Concept, [ViewDef]) -> CtxError
mkMultipleDefaultError (_, [])              = fatal 118 "mkMultipleDefaultError called on []"
mkMultipleDefaultError (c, viewDefs@(vd0:_)) =
  CTXE (origin vd0) $ "Multiple default views for concept " ++ show (name c) ++ ":" ++ 
                      concat ["\n    VIEW " ++ vdlbl vd ++ " (at " ++ show (origin vd) ++ ")"
                             | vd <- viewDefs ]       


class ErrorConcept a where
  showEC :: a -> String
  showMini :: a -> String
  
instance ErrorConcept (P_ViewD a) where
  showEC x = showADL (vd_cpt x) ++" given in VIEW "++vd_lbl x
  showMini x = showADL (vd_cpt x)
instance ErrorConcept (P_IdentDef) where
  showEC x = showADL (ix_cpt x) ++" given in Identity "++ix_lbl x
  showMini x = showADL (ix_cpt x)

instance (ShowADL a2) => ErrorConcept (SrcOrTgt, A_Concept, a2) where
  showEC (p1,c1,e1) = showEC' (p1,c1,showADL e1)
  showMini (_,c1,_) = showADL c1
  
showEC' :: (SrcOrTgt, A_Concept, String) -> String
showEC' (p1,c1,e1) = showADL c1++" ("++show p1++" of "++e1++")"
  
instance (ShowADL a2, Association a2) => ErrorConcept (SrcOrTgt, a2) where
  showEC (p1,e1)
   = case p1 of
      Src -> showEC' (p1,source e1,showADL e1)
      Tgt -> showEC' (p1,target e1,showADL e1)
  showMini (p1,e1)
   = case p1 of
      Src -> showADL (source e1)
      Tgt -> showADL (target e1)
      
instance (ShowADL a2, Association a2) => ErrorConcept (SrcOrTgt, Origin, a2) where
  showEC (p1,o,e1)
   = case p1 of
      Src -> showEC' (p1,source e1,showADL e1 ++ ", "++showMinorOrigin o)
      Tgt -> showEC' (p1,target e1,showADL e1 ++ ", "++showMinorOrigin o)
  showMini (p1,_,e1)
   = case p1 of
      Src -> showADL (source e1)
      Tgt -> showADL (target e1)

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
 = "("++showADL (EDcV (sign e))++" - "++showADL e++")"
writeBind e
 = "("++showADL e++") /\\ "++showADL (EDcV (sign e))

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

-- Shorthand for working with Guarded in IO
whenCheckedIO :: IO  (Guarded a) -> (a -> IO (Guarded b)) -> IO (Guarded b)
whenCheckedIO ioGA fIOGB =
 do { gA <- ioGA 
    ; case gA of
         Errors errs -> return $ Errors errs
         Checked a   ->
          do { gB <- fIOGB a
             ; return gB
             }
    }

showErr :: CtxError -> String
showErr (CTXE o s) = s ++ "\n  " ++ showFullOrig o
showErr (PE msg)   = showMessage msg
 where showMessage (Msg expecting token action) =  
         let pos = case token of
                     Nothing -> "at end of file"
                     Just s  -> case action of 
                                  Insert _ -> "before " ++ show s
                                  Delete t -> "at " ++ show t  
                                  Other str -> str -- Probably never used, UU.Parsing.parseIO ignores this case
         in  "\nParse error " ++ pos ++
             "\nExpecting " ++ show expecting ++
             "\nTry " ++ show action ++ "\n"                
 

showFullOrig :: Origin -> String
showFullOrig (FileLoc (FilePos (filename,Database.Design.Ampersand.ADL1.Pos l c,t)))
              = "Error at symbol "++ t ++ " in file " ++ filename++" at line " ++ show l++" : "++show c
showFullOrig x = show x
showMinorOrigin :: Origin -> String
showMinorOrigin (FileLoc (FilePos (_,Database.Design.Ampersand.ADL1.Pos l c,_))) = "line " ++ show l++" : "++show c
showMinorOrigin v = show v