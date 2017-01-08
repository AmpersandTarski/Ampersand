{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
  -- The purpose of ShowADL is to print things in Ampersand source format.
  -- Rule: The semantics of each fSpec produced by the compiler is identical to the semantics  of (parse (showADL fSpec)).
  -- Rule: The standard show is used only for simple error messages during testing.
  -- Question (SJC): If STRING is the code produced by showADL, would STRING == showADL (parse STRING) (context (parse STRING)) be true?
  -- Answer (SJ):   No, not for every STRING. Yet, for every fSpec we want  semantics fSpec == semantics (parse (showADL fSpec)).
  --                Note that 'parse' and 'semantics' do not exist in this shape, so the actual expression is slightly more complicated.
  --
  -- Every Expression should be disambiguated before printing to ensure unambiguity.
module Ampersand.FSpec.ShowADL
    ( ShowADL(..), showREL)
where
import Ampersand.Core.ParseTree
import Ampersand.Core.ShowPStruct
--import Ampersand.Core.ShowAStruct
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Basics      (Collection(..),Named(..))
import Ampersand.Classes
import Ampersand.ADL1 (insParentheses)
import Ampersand.FSpec.FSpec
import Data.List
import Prelude
import Data.Char


class ShowADL a where
  showADL :: a -> String

instance {-# OVERLAPPABLE #-} PStruct a => ShowADL a where
  showADL = showP
instance ShowADL (P_SubIfc a) where
  showADL (P_Box{}) = "BOX"
  showADL (P_InterfaceRef _ isLink nm _) = (if isLink then " LINKTO" else "")++" INTERFACE "++doubleQuote nm

instance ShowADL ObjectDef where
-- WHY (HJ)? In deze instance van ShowADL worden diverse zaken gebruikt die ik hier niet zou verwachten.
--              Het vertroebelt de code ook een beetje, want nu moeten er dingen als 'source' en
--              'target' hier al bekend zijn.
--              Dat lijkt me hier nog niet op z'n plaats, als je alleen maar wat wilt kunnen 'prettyprinten'.
-- BECAUSE (SJ): Dit blijft nog even zo, omdat showADL gebruikt wordt in het genereren van interfaces.
--              Zolang we dat nog niet onder de knie hebben blijft de code wat troebel.
 showADL obj = " : "++showADL (objctx obj)++
               recur "\n  " (objmsub obj)
  where recur :: String -> Maybe SubInterface -> String
        recur _   Nothing = ""
        recur ind (Just (InterfaceRef isLink nm cruds)) = ind++(if isLink then " LINKTO" else "")++" INTERFACE "++doubleQuote nm++showADL cruds
        recur ind (Just (Box _ cl objs))
         = ind++" BOX" ++ showClass cl ++ " [ "++
           intercalate (ind++"     , ")
                               [ doubleQuote (name o)++
                                  " : "++showADL (objctx o)++
                                  recur (ind++"      ") (objmsub o)
                               | o<-objs
                               ]++
           ind++"     ]"
        showClass Nothing = ""
        showClass (Just cl) = "<" ++ cl ++ ">" 
instance ShowADL Cruds where
 showADL x = " "++f crudC 'C'++f crudR 'R'++f crudU 'U'++f crudD 'D'
   where
     f :: (Cruds -> Bool) -> Char -> String
     f fun c = [(if fun x then toUpper else toLower) c]

instance ShowADL Meta where
 showADL (Meta _ metaObj nm val) =
   "META "++(if null $ showADL metaObj then "" else showADL metaObj++" ") ++show nm++" "++show val

instance ShowADL MetaObj where
 showADL ContextMeta = ""

instance ShowADL Purpose where
 showADL expl = "PURPOSE "++showADL (explObj expl)
                ++" "++showADL (amLang (explMarkup expl))
                ++(if null (explRefIds expl) then "" else " REF "++intercalate ";" (explRefIds expl))
                ++showADL (explMarkup expl)

instance ShowADL A_Markup where
 showADL m
     = showADL (amLang m)
    ++ "{+"++aMarkup2String ReST m++"+}"

instance ShowADL ExplObj where
 showADL e = case e of
      ExplConceptDef cd  -> "CONCEPT "++cdcpt cd
      ExplDeclaration d  -> "RELATION "++show (name d)
      ExplRule str       -> "RULE "++doubleQuote str
      ExplIdentityDef str-> "IDENT "++doubleQuote str
      ExplViewDef str    -> "VIEW "++doubleQuote str
      ExplPattern str    -> "PATTERN "++ doubleQuote str
      ExplInterface str  -> "INTERFACE "++doubleQuote str
      ExplContext str    -> "CONTEXT "++doubleQuote str

doubleQuote :: String -> String
doubleQuote str = "\""++str++"\""


-- TODO: making these tuples instance of ShowADL is very hacky
instance ShowADL (String,Rule) where
 showADL (role,rul) = "ROLE "++role++" MAINTAINS "++show (name rul)

instance ShowADL (String,Declaration) where
 showADL           (role,rel) = "ROLE "++role++" EDITS "++showADL rel

instance ShowADL (String,Interface) where
 showADL (role,ifc) = "ROLE "++role++" USES "++show (name ifc)

instance ShowADL Pattern where
-- TODO: This function is VERY outdated
 showADL pat
  = "PATTERN " ++ doubleQuote (name pat) ++ "\n"
    ++ (if null (ptrls pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptrls pat)) ++ "\n")
    ++ (if null (ptgns pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptgns pat)) ++ "\n")
    ++ (if null (ptdcs pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptdcs pat)) ++ "\n")
-- concept definitions are not printed, because we have no way of telling where they come from....
    ++ (if null (ptids pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptids pat)) ++ "\n")
    ++ "ENDPATTERN"

instance ShowADL (PairViewSegment Expression) where
 showADL (PairViewText _ str)       = "TXT " ++ show str
 showADL (PairViewExp _ srcOrTgt e) = showADL srcOrTgt ++ " " ++ showADL e

instance ShowADL SrcOrTgt where
 showADL Src = "SRC"
 showADL Tgt = "TGT"

instance ShowADL Rule where
 showADL r
  = "RULE \""++rrnm r++"\" : "++showADL (rrexp r)
     ++ concat ["\n     MEANING "++showADL mng | mng <- ameaMrk $ rrmean r ]
     ++ concat ["\n     MESSAGE "++showADL msg | msg <- rrmsg r]
     ++ case rrviol r of
          Nothing                -> ""
          Just (PairView pvSegs) -> "\n     VIOLATION ("++intercalate ", " (map showADL pvSegs)++")"

instance ShowADL A_Gen where
 showADL g =
   case g of
    Isa{} -> "CLASSIFY "++showADL (genspc g)++" ISA "++showADL (gengen g)
    IsE{} -> "CLASSIFY "++showADL (genspc g)++" IS "++intercalate " /\\ " (map showADL (genrhs g))

instance ShowADL A_RoleRelation where
 showADL r
  = "ROLE "++intercalate ", " (map show (rrRoles r))++" EDITS "++intercalate ", " (map showADL (rrRels r))

instance ShowADL Interface where
 showADL ifc
  = "INTERFACE "++doubleQuote(name ifc)
          ++(if null (ifcRoles ifc) then "" else " FOR "++intercalate ", " (map name (ifcRoles ifc)))
          ++showADL (ifcObj ifc)

instance ShowADL IdentityDef where
 showADL identity
  = "IDENT "++idLbl identity
          ++ ": " ++name (idCpt identity)
          ++ "(" ++intercalate ", " (map showADL $ identityAts identity) ++ ")"

instance ShowADL IdentitySegment where
 showADL (IdentityExp objDef) = (if null (name objDef) then "" else "\""++name objDef++"\":") ++ showADL (objctx objDef)

instance ShowADL ViewDef where
 showADL vd
  = "VIEW "++name vd
          ++ ": " ++name (vdcpt vd)
          ++ "(" ++intercalate ", " (map showADL $ vdats vd) ++ ")"
     --TODO: Make this output the more generic FancyViewDef 
instance ShowADL ViewSegment where
 showADL vs = ( case vsmlabel vs of
                  Nothing -> ""
                  Just s  -> s ++ " : "
              ) ++ showADL (vsmLoad vs)
instance ShowADL ViewSegmentPayLoad where
 showADL x = case x of
   (ViewExp expr)  -> showADL expr
   (ViewText str) -> "TXT " ++ show str
  
-- showADL Relation only prints complete signatures to ensure unambiguity.
-- therefore, when printing expressions, do not apply this function to print relations, but apply one that prints names only
--instance ShowADL Relation where
-- showADL rel = show rel

instance ShowADL Expression where
 showADL = showExpr (" = ", " |- ", " /\\ ", " \\/ ", " - ", " / ", " \\ ", " <> ", ";", "!", "*", "*", "+", "~", ("-"++), "(", ")", "[", "*", "]")
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
            case showADL str of
              []  -> []
              [c] -> [c]
              cs  -> if head cs == '\"' && last cs == '\"'
                     then reverse . tail . reverse .tail $ cs
                     else cs
instance ShowADL DnfClause where
 showADL dnfClause = showADL (dnf2expr dnfClause)

instance ShowADL Declaration where
 showADL decl =
  case decl of
     Sgn{} -> name decl++" :: "++name (source decl)++(if null ([Uni,Tot]>-properties decl) then " -> " else " * ")++name (target decl)++
              (let mults=if null ([Uni,Tot]>-properties decl) then properties decl>-[Uni,Tot] else properties decl in
               if null mults then "" else "["++intercalate "," (map showADL mults)++"]")++
              (if null(decprL decl++decprM decl++decprR decl) then "" else
               " PRAGMA "++unwords (map show [decprL decl,decprM decl,decprR decl]))
               ++ concatMap meaning (ameaMrk (decMean decl))
     Isn{} -> "I["++show (detyp decl)++"]" -- Isn{} is of type Declaration and it is implicitly defined
     Vs{}  -> "V"++show (decsgn decl)      -- Vs{}  is of type Declaration and it is implicitly defined
   where
     meaning :: A_Markup -> String
     meaning pmkup = " MEANING "++showADL pmkup

showREL :: Declaration-> String
showREL decl = show decl
{-
  case decl of
     Sgn{} -> name decl++showSign (sign decl)
     Isn{} -> "I["++show (detyp decl)++"]" -- Isn{} is of type Declaration and it is implicitly defined
     Vs{}  -> "V"++show (decsgn decl)
-}

instance ShowADL P_Markup where
 showADL (P_Markup lng fmt str) = case lng of
                                     Nothing -> ""
                                     Just l  -> "IN "++show l++" "
                                 ++
                                  case fmt of
                                     Nothing -> ""
                                     Just f  -> " "++show f++" "
                                 ++
                                  "{+"++str++"-} "

instance ShowADL Prop where
 showADL = show

instance ShowADL A_Concept where
 showADL c = show (name c)

instance ShowADL ConceptDef where
 showADL cd
  = "\n  CONCEPT "++show (cdcpt cd)++" "++show (cddef cd)++" "++(if null (cdref cd) then "" else show (cdref cd))

instance ShowADL A_Context where
 showADL context
  = "CONTEXT " ++name context
    ++ " " ++ (showADL (ctxlang context))
    ++ (if null (ctxmetas context) then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxmetas context))++ "\n")
    ++ (if null (ctxifcs context)  then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxifcs context)) ++ "\n")
    ++ (if null (ctxpats context)  then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxpats context)) ++ "\n")
    ++ (if null (ctxrs context)    then "" else "\n"      ++intercalate "\n"   (map showADL (ctxrs context))   ++ "\n")
    ++ (if null (ctxds context)    then "" else "\n"      ++intercalate "\n"   (map showADL (ctxds context))   ++ "\n")
    ++ (if null (ctxks context)    then "" else "\n"      ++intercalate "\n"   (map showADL (ctxks context))   ++ "\n")
    ++ (if null (ctxgs context)    then "" else "\n"      ++intercalate "\n"   (map showADL (ctxgs context))   ++ "\n")
    ++ (if null (ctxcds context)   then "" else "\n"      ++intercalate "\n"   (map showADL (ctxcds context))  ++ "\n")
    ++ (if null (ctxpopus context) then "" else "\n"      ++intercalate "\n"   (map showADL (ctxpopus context))++ "\n")
    ++ (if null (ctxsql context)   then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxsql context)) ++ "\n")
    ++ (if null (ctxphp context)   then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxphp context)) ++ "\n")
    ++ "\n\nENDCONTEXT"

instance ShowADL (Maybe String) where
  showADL _ = ""
instance (ShowADL a, ShowADL b) => ShowADL (a,b) where
 showADL (a,b) = "(" ++ showADL a ++ ", " ++ showADL b ++ ")"

instance ShowADL P_Population where
 showADL pop
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
     else indent++"[ "++intercalate ("\n"++indent++", ") showContent++indent++"]"
    where indent = "   "
          showContent = case pop of
                          P_RelPopu{} -> map showADL (p_popps pop)
                          P_CptPopu{} -> map showADL  (p_popas pop)
instance ShowADL AAtomPair where
 showADL p = "("++showADL (apLeft p)++","++ showADL (apRight p)++")"
  
instance ShowADL Population where
 showADL pop
  = "POPULATION "
  ++ case pop of
        ARelPopu{} -> (name.popdcl) pop++(show.sign.popdcl) pop
        ACptPopu{} -> (name.popcpt) pop
  ++ " CONTAINS\n"
  ++ if (case pop of
            ARelPopu{} -> null (popps pop)
            ACptPopu{} -> null (popas pop)
        )
     then ""
     else indent++"[ "++intercalate ("\n"++indent++", ") showContent++indent++"]"
    where indent = "   "
          showContent = case pop of
                          ARelPopu{} -> map showADL (popps pop)
                          ACptPopu{} -> map showADL (popas pop)

-- showADL (ARelPopu r pairs)
--  = "POPULATION "++showADL r++" CONTAINS\n"++
--    indent++"[ "++intercalate ("\n"++indent++", ") (map (\(x,y)-> showatom x++" * "++ showatom y) pairs)++indent++"]"
--    where indent = "   "

              
instance ShowADL AAtomValue where
 showADL at = case at of
              AAVString{} -> show (aavstr at)
              AAVInteger _ i   -> show i
              AAVFloat   _ f   -> show f
              AAVBoolean _ b   -> show b
              AAVDate _ day    -> show day
              AAVDateTime _ dt -> show dt
              AtomValueOfONE -> "1"

--used to compose error messages at p2a time
