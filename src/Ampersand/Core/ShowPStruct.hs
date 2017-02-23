{-# LANGUAGE FlexibleInstances    #-}
module Ampersand.Core.ShowPStruct
  (PStruct(..), doubleQuote)
where

import Ampersand.Core.ParseTree
import Ampersand.Basics
import Data.List
import Ampersand.ADL1.PrettyPrinters

class PStruct a where
 showP :: a -> String

instance PStruct P_Concept where
 showP = name

instance PStruct ConceptDef where
 showP cd
  = "\n  CONCEPT "++show (cdcpt cd)++" "++show (cddef cd)++" "++(if null (cdref cd) then "" else show (cdref cd))


instance PStruct PAtomPair where
 showP p = "("++showP (ppLeft p)++","++ showP (ppRight p)++")"

instance PStruct PAtomValue where
 showP at = case at of
              PSingleton _ s _ -> show s
              ScriptString _ s -> show s
              XlsxString _ s   -> show s
              ScriptInt _ s    -> show s
              ScriptFloat  _ x -> show x
              XlsxDouble _ d   -> show d
              ComnBool   _ b   -> show b
              ScriptDate _ x   -> show x
              ScriptDateTime _ x -> show x

instance PStruct P_Population where
 showP pop
  = "POPULATION "++name pop
  ++ case pop of
        P_RelPopu{p_nmdr = PNamedRel _ _ (Just sgn)} -> "["++(name.pSrc) sgn++"*"++(name.pTgt) sgn++"]"
        _ -> ""
  ++ " CONTAINS\n"
  ++ if (case pop of
            P_RelPopu{} -> null (p_popps pop)
            P_CptPopu{} -> null (p_popas pop)
        )
     then ""
     else indnt++"[ "++intercalate ("\n"++indnt++", ") showContent++indnt++"]"
    where indnt = "   "
          showContent = case pop of
                          P_RelPopu{} -> map showP (p_popps pop)
                          P_CptPopu{} -> map showP  (p_popas pop)



instance PStruct P_Gen where
 showP g = 
   case g of
     PGen{} -> "CLASSIFY "++showP (gen_spc g)++" ISA "++showP (gen_gen g)
     P_Cy{} -> "CLASSIFY "++showP (gen_spc g)++" IS "++intercalate " /\\ " (map showP (gen_rhs g))

instance PStruct PRef2Obj where
 showP e = case e of
      PRef2ConceptDef str  -> "CONCEPT "++doubleQuote str
      PRef2Declaration rel -> "RELATION "++doubleQuote (name rel)
      PRef2Rule str        -> "RULE "++doubleQuote str
      PRef2IdentityDef str -> "IDENT "++doubleQuote str
      PRef2ViewDef str     -> "VIEW "++doubleQuote str
      PRef2Pattern str     -> "PATTERN "++ doubleQuote str
      PRef2Interface str   -> "INTERFACE "++doubleQuote str
      PRef2Context str     -> "CONTEXT "++doubleQuote str





instance PStruct (Maybe TType) where
  showP (Just v) = show v
  showP Nothing = "'Default'"

instance PStruct TermPrim where
 showP (PI _)              = "I"
 showP (Pid _ c)           = "I["++showP c++"]"
 showP (Patm _ val mSign)  = 
     "'"++
     (case val of
       PSingleton   _ x _ -> x 
       ScriptString   _ x -> x
       XlsxString     _ x -> concatMap escapeSingleQuote x
                               where escapeSingleQuote c=
                                       case c of
                                         '\'' -> ['\\','\'']
                                         _    -> [c]
       ScriptInt      _ x -> show x
       ScriptFloat    _ x -> show x
       XlsxDouble     _ x -> show x
       ComnBool       _ x -> show x
       ScriptDate     _ x -> show x
       ScriptDateTime _ x -> show x
     ) ++
     "'" ++
     (case mSign of
       Nothing -> ""
       Just c  -> "["++show c++"]"
     )
 showP (PVee _)           = "V"
 showP (Pfull _ s t)      = "V["++show s++"*"++show t++"]"
 showP (PNamedR rel)      = showP rel


instance (Traced a, PStruct a) => PStruct (Term a) where
 showP = showchar . insP_Parentheses
   where
    showchar :: PStruct a => Term a -> String
    showchar (Prim a) = showP a
    showchar (PEqu _ l r)  = showBin " = "   l r
    showchar (PInc _ l r)  = showBin " |- "  l r
    showchar (PIsc _ l r)  = showBin " /\\ " l r
    showchar (PUni _ l r)  = showBin " \\/ " l r
    showchar (PDif _ l r)  = showBin " - "   l r
    showchar (PLrs _ l r)  = showBin " / "   l r
    showchar (PRrs _ l r)  = showBin " \\ "  l r
    showchar (PDia _ l r)  = showBin "<>"    l r
    showchar (PCps _ l r)  = showBin ";"     l r
    showchar (PRad _ l r)  = showBin "!"     l r
    showchar (PPrd _ l r)  = showBin "*"     l r
    showchar (PKl0 _ e)    = showUnPostfix "*" e
    showchar (PKl1 _ e)    = showUnPostfix "+" e
    showchar (PFlp _ e)    = showUnPostfix "~" e
    showchar (PCpl _ e)    = showUnPrefix  "-" e
    showchar (PBrk _ e)    = "("++showchar e++")"
    showBin :: PStruct a => String -> Term a -> Term a -> String
    showBin       op l r = showchar l++op++showchar r
    showUnPostfix op e   = showchar e ++ op
    showUnPrefix  op e   = op ++ showchar e

    insP_Parentheses :: Traced a => Term a -> Term a 
    insP_Parentheses = insPar 0
      where
       wrap :: Traced a => Integer -> Integer -> Term a -> Term a
       wrap i j e' = if i<=j then e' else PBrk (origin e') e'
       insPar :: Traced a => Integer -> Term a -> Term a
       insPar i (PEqu o l r) = wrap i     0 (PEqu o (insPar 1 l) (insPar 1 r))
       insPar i (PInc o l r) = wrap i     0 (PInc o (insPar 1 l) (insPar 1 r))
       insPar i (PIsc o l r) = wrap (i+1) 2 (PIsc o (insPar 2 l) (insPar 2 r))
       insPar i (PUni o l r) = wrap (i+1) 2 (PUni o (insPar 2 l) (insPar 2 r))
       insPar i (PDif o l r) = wrap i     4 (PDif o (insPar 5 l) (insPar 5 r))
       insPar i (PLrs o l r) = wrap i     6 (PLrs o (insPar 7 l) (insPar 7 r))
       insPar i (PRrs o l r) = wrap i     6 (PRrs o (insPar 7 l) (insPar 7 r))
       insPar i (PDia o l r) = wrap i     6 (PDia o (insPar 7 l) (insPar 7 r))
       insPar i (PCps o l r) = wrap (i+1) 8 (PCps o (insPar 8 l) (insPar 8 r))
       insPar i (PRad o l r) = wrap (i+1) 8 (PRad o (insPar 8 l) (insPar 8 r))
       insPar i (PPrd o l r) = wrap (i+1) 8 (PPrd o (insPar 8 l) (insPar 8 r))
       insPar _ (PKl0 o e)   = PKl0 o (insPar 10 e)
       insPar _ (PKl1 o e)   = PKl1 o (insPar 10 e)
       insPar _ (PFlp o e)   = PFlp o (insPar 10 e)
       insPar _ (PCpl o e)   = PCpl o (insPar 10 e)
       insPar i (PBrk _ e)   = insPar i e
       insPar _ x            = x

instance PStruct (P_ObjDef TermPrim) where
 showP obj = " : "++showP (obj_ctx obj)++
               recur "\n  " (obj_msub obj)
  where 
    recur :: (Traced a, PStruct a) => String -> Maybe (P_SubIfc a) -> String
    recur _   Nothing = ""
    recur ind (Just (P_InterfaceRef _ isLink nm cruds))
         = ind++(if isLink then " LINKTO" else "")++" INTERFACE "++doubleQuote nm++showP cruds
    recur ind (Just (P_Box _ cl objs))
         = ind++" BOX" ++ showClass cl ++ " [ "++
           intercalate (ind++"     , ")
                               [ doubleQuote (name o)++
                                  " : "++showP (obj_ctx o)++
                                  recur (ind++"      ") (obj_msub o)
                               | o<-objs
                               ]++
           ind++"     ]"
    showClass Nothing = ""
    showClass (Just cl) = "<" ++ cl ++ ">" 

instance PStruct (P_SubIfc a) where --TODO : Compare with other " LINKTO" stuff to check redundancy. 
  showP P_Box{} = "BOX"
  showP (P_InterfaceRef _ isLink nm _) = (if isLink then " LINKTO" else "")++" INTERFACE "++doubleQuote nm

instance PStruct (Maybe P_Cruds) where
 showP Nothing = ""
 showP (Just (P_Cruds _ x)) = x 


instance (Traced a,PStruct a) => PStruct (P_Rule a) where
 showP r 
  = "RULE \""++rr_nm r++"\" : "++showP (rr_exp r)
     ++ concat ["\n   "++showP mng | mng <- rr_mean r ]
     ++ concat ["\n   "++showP msg | msg <- rr_msg  r ]
     ++ case rr_viol r of
          Nothing                -> ""
          Just (PairView pvSegs) -> "\n     VIOLATION ("++intercalate ", " (map showP pvSegs)++")"


instance PStruct P_RoleRule where
 showP r 
  = "ROLE "++intercalate ", " (map show (mRoles r))++" MAINTAINS "++intercalate ", " (map show (mRules r))


instance PStruct P_Declaration where
 showP decl =
  case decl of
    P_Sgn{} -> name decl++" :: "++(name . pSrc . dec_sign) decl
                                ++(if null ([Uni,Tot]>-dec_prps decl) then " -> " else " * ")
                                ++(name . pTgt . dec_sign) decl++
               (let mults=if null ([Uni,Tot]>-dec_prps decl) then dec_prps decl>-[Uni,Tot] else dec_prps decl in
                if null mults then "" else "["++intercalate "," (map showP mults)++"]")++
               (if null(unwords (dec_pragma decl)) then "" else
                " PRAGMA "++unwords (dec_pragma decl))
                ++ unwords (map showP (dec_Mean decl))

instance PStruct PMeaning where
 showP (PMeaning pmkup) = " MEANING "++showP pmkup

instance PStruct PMessage where
 showP (PMessage pmkup) = " MESSAGE "++showP pmkup

instance PStruct P_Markup where
 showP (P_Markup lng fmt str) = case lng of
                                   Nothing -> ""
                                   Just l  -> showP l++" "
                               ++
                                case fmt of
                                   Nothing -> ""
                                   Just f  -> showP f++" "
                               ++
                                "{+"++str++"+} "

instance PStruct Prop where
 showP = show

instance PStruct P_NamedRel where
 showP (PNamedRel _ rel mSgn)   = rel++maybe "" showsign mSgn
  where     showsign (P_Sign src trg) = "["++showP src++"*"++showP trg++"]"


instance PStruct Lang where
 showP Dutch   = "IN DUTCH"
 showP English = "IN ENGLISH"



doubleQuote :: String -> String
doubleQuote str = "\""++str++"\""


instance PStruct PandocFormat where
 showP LaTeX = "LATEX "
 showP HTML  = "HTML "
 showP ReST  = "REST "
 showP Markdown = "MARKDOWN "

instance PStruct Markup where
 showP m
     = showP (amLang m) ++ " " ++ showP Markdown 
    ++ " {+"++aMarkup2String ReST m++"+}"


instance PStruct a => PStruct (PairViewSegment a) where
 showP (PairViewText _ str)       = "TXT " ++ show str
 showP (PairViewExp _ srcOrTgt e) = showP srcOrTgt ++ " " ++ showP e

instance PStruct SrcOrTgt where
 showP Src = "SRC"
 showP Tgt = "TGT" 


instance PStruct P_Context where
 showP = prettyPrint  --HJO20170115: This is special: PrettyPrint must not use showP, or there will be loops...