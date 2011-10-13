{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
  -- The purpose of ShowADL is to print things in Ampersand source format.
  -- Rule: The semantics of each fSpec produced by the compiler is identical to the semantics  of (parse (showADL fSpec)).
  -- Rule: The standard show is used only for simple error messages during testing.
  -- Question (SJC): If STRING is the code produced by showADL, would STRING == showADL (parse STRING) (context (parse STRING)) be true?
  -- Answer (SJ):   No, not for every STRING. Yet, for every fSpec we want  semantics fSpec == semantics (parse (showADL fSpec)).
  --                Note that 'parse' and 'semantics' do not exist in this shape, so the actual expression is slightly more complicated.
module DatabaseDesign.Ampersand.Fspec.ShowADL
    ( ShowADL(..))
  where
   import DatabaseDesign.Ampersand.Core.ParseTree
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import Data.Char                                 (isAlphaNum)
   import DatabaseDesign.Ampersand.Basics      (fatalMsg,eqCl,sort',Collection(..),Identified(..))
   import DatabaseDesign.Ampersand.ADL1
   import DatabaseDesign.Ampersand.Misc
   import DatabaseDesign.Ampersand.Classes
   import DatabaseDesign.Ampersand.Fspec.Fspec
   import Data.List
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.ShowADL"

   class ShowADL a where
    disambiguate :: Language l => l -> a -> a
    showADL      :: a -> String


--------------------------------------------------------------
   --TODO -> check equivalence of generated Haskell code 
   --TODO -> comments in original script must also be printed
   --WHY -> aren't ONE Anything NOthing etc reserved words on pString, pConid, (etc?)? Answer: check if errors can be produced without reserved words. If so add reserved words, otherwise don't
   --TODO -> sort on file position
   --TODO -> move the flips from Relation to Expression data type
   --TODO -> remove application of double complement rule from the parser
   --TODO -> remove removal of brackets on ; expression from the parser

   instance ShowADL ObjectDef where
   -- WHY (HJ)? In deze instance van ShowADL worden diverse zaken gebruikt die ik hier niet zou verwachten.
   --              Het vertroebelt de code ook een beetje, want nu moeten er dingen als 'source' en
   --              'target' hier al bekend zijn.
   --              Dat lijkt me hier nog niet op z'n plaats, als je alleen maar wat wilt kunnen 'prettyprinten'. 
   -- BECAUSE (SJ): Dit blijft nog even zo, omdat showADL gebruikt wordt in het genereren van interfaces.
   --              Zolang we dat nog niet onder de knie hebben blijft de code wat troebel.
    showADL obj = "  INTERFACE "++name obj++" : "++showADL (objctx obj)++
                  recur "\n  " (objats obj)
     where recur :: String -> [ObjectDef] -> String
           recur ind objs
            = ind++" = [ "++
              intercalate (ind++"   , ") 
                                  [ name o++
                                     (if null (objstrs o) then "" else " {"++intercalate ", " [unwords (map str ss) | ss<-objstrs o]++"}")++
                                     " : "++showADL (objctx o)++
                                     if null (objats o) then "" else recur (ind++"     ") (objats o)
                                  | o<-objs
                                  ]++
              ind++"   ]"
           str ss | and [isAlphaNum c | c<-ss] = ss
                  | otherwise                 = "\""++ss++"\""
    disambiguate spc obj
     = if isTypeable (objctx obj)  -- Why? Because disambiguate exists only on typeable expressions.
       then obj {objctx = disambiguate spc (objctx obj)
                ,objats = map (disambiguate spc) (objats obj)
                }
       else fatal 66 ("Expression "++showADL (objctx obj)++" in object "++name obj++"contains untypeable elements.")
 
   instance ShowADL Explanation where
    showADL expl = "PURPOSE "++showADL (explObj expl)++" IN "++showADL (explLang expl)
                   ++(if null (explRefId expl) then "" else " REF "++explRefId expl)
                   ++ "{+"++explainContent2String (explCont expl)++"-}"
    disambiguate _ = id

   instance ShowADL Lang where
    showADL Dutch   = "DUTCH"
    showADL English = "ENGLISH"
    disambiguate _ = id
      
   instance ShowADL ExplObj where
    showADL e = case e of
         ExplConceptDef cd  -> "CONCEPT "++showADL cd
         ExplDeclaration d  -> "RELATION "++showADL (makeRelation d)
         ExplRule r         -> "RULE "++showstr (name r)
         ExplKeyDef kd      -> "KEY "++showADL kd
         ExplPattern str    -> "PATTERN "++str
         ExplProcess str    -> "PROCESS "++str
         ExplInterface str  -> "INTERFACE "++showstr str
         ExplContext str    -> "CONTEXT "++str
         ExplFspc str       -> "CONTEXT "++str
    disambiguate _ = id

   showstr :: String -> String
   showstr str = "\""++str++"\""

   -- The declarations of the pattern are supplemented by all declarations needed to define the rules.
   -- Thus, the resulting pattern is self-contained with respect to declarations.
   instance ShowADL Process where
    showADL prc
     = "PROCESS " ++ name prc 
       ++ (if null (rules prc)     then "" else "\n  " ++intercalate "\n  " (map showADL (rules prc))     ++ "\n")
       ++ (if null (maintains prc) then "" else "\n  " ++                        showRM prc               ++ "\n")
       ++ (if null (mayEdit prc)   then "" else "\n  " ++                        showRR prc               ++ "\n")
       ++ (if null (prcCds prc)    then "" else "\n  " ++intercalate "\n  " (map showADL (prcCds prc))    ++ "\n")
       ++ (if null (prcKds prc)    then "" else "\n  " ++intercalate "\n  " (map showADL (prcKds prc))    ++ "\n")
       ++ (if null (prcXps prc)    then "" else "\n  " ++intercalate "\n  " (map showADL (prcXps prc))    ++ "\n")
       ++ "ENDPROCESS"
       where -- TODO: the following definitions should be unneccessary, but 'map showADL (maintains prc)' and "map showADL (mayEdit prc)" don't work... 
         showRM :: Process -> String
         showRM pr = intercalate "\n  " [ "ROLE "++role++" MAINTAINS "++intercalate ", " [name rul | (_,rul)<-cl]
                                        | cl<-eqCl fst (maintains pr), let role = fst (head cl)]
         showRR :: Process -> String
         showRR pr = intercalate "\n  " [ "ROLE "++role++" EDITS "++intercalate ", " [name rul | (_,rul)<-cl]
                                        | cl<-eqCl fst (mayEdit pr), let role = fst (head cl)]
    disambiguate spc prc
     = prc {prcRules = map (disambiguate spc) (prcRules prc)
           ,prcKds   = map (disambiguate spc) (prcKds   prc)
           }


   instance ShowADL (String,Rule) where
    showADL (role,rul) = "ROLE "++role++" MAINTAINS "++show (name rul)
    disambiguate _ = id
 
   instance ShowADL (String,Relation) where
    showADL           (role,rel) = "ROLE "++role++" EDITS "++showADL rel
    disambiguate _ (_,rel)
     = fatal 126 ("calling disambiguate on a relation, "++showADL rel++".\nPlease turn into an expression using ERel.")

   instance ShowADL (String,Interface) where
    showADL (role,ifc) = "ROLE "++role++" USES "++show (name ifc)
    disambiguate _ = id

   -- The declarations of the pattern are supplemented by all declarations needed to define the rules.
   -- Thus, the resulting pattern is self-contained with respect to declarations.
   instance ShowADL Pattern where
    showADL pat
     = "PATTERN " ++ name pat ++ "\n"
       ++ (if null (ptrls pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptrls pat)) ++ "\n")
       ++ (if null (ptgns pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptgns pat)) ++ "\n")
       ++ (if null (ptdcs pat)  then "" else "\n  " ++intercalate "\n  " (map showADL ds         ) ++ "\n")
       ++ (if null (ptcds pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptcds pat)) ++ "\n")
       ++ (if null (ptkds pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptkds pat)) ++ "\n")
       ++ "ENDPATTERN"
       where ds = ptdcs pat++[d | d@Sgn{}<-declarations pat `uni` nub [makeDeclaration r | r<-mors (ptrls pat) `uni` mors (ptkds pat)]
                                , decusr d, d `notElem` ptdcs pat]
    disambiguate spc pat
     = pat {ptrls = map (disambiguate spc) (ptrls pat)
           ,ptkds = map (disambiguate spc) (ptkds pat)
           }

   instance ShowADL Rule where
    showADL r
     = "RULE \""++rrnm r++"\" : "++showADL (rrexp r)
        ++ if null phrs then [] else "\n     PHRASE "++ showstr (head phrs)
        where phrs = [explainContent2String econt | Means _ econt<-rrxpl r]
    disambiguate spc r = r {rrexp = disambiguate spc (rrexp r)}

   instance ShowADL A_Gen where
    showADL (Gen _ g s _) = "GEN "++showADL s++" ISA "++showADL g
    disambiguate _ = id

   instance ShowADL RoleRelation where
    showADL r
     = "ROLE "++intercalate ", " (map show (rrRoles r))++" EDITS "++intercalate ", " (map showADL (rrRels r))
    disambiguate _ _
     = fatal 165 "calling disambiguate on relation(s):\nPlease make expression(s) using ERel."

   instance ShowADL RoleRule where
    showADL r = "ROLE "++intercalate ", " (map show (mRoles r))++" MAINTAINS "++intercalate ", " (map show (mRules r))
    disambiguate _ = id

   instance ShowADL Interface where
    showADL ifc 
     = "INTERFACE "++name ifc
             ++"("++intercalate ", " [showADL r | r<-ifcParams ifc]++")\n"
             ++"{"++intercalate ", " [unwords strs | strs<-ifcArgs ifc]++"}\n"
             ++showADL (ifcObj ifc)
             ++show (ifcExpl ifc)
    disambiguate spc ifc
     = ifc {ifcObj = disambiguate spc (ifcObj ifc)}

   instance ShowADL KeyDef where
    showADL kd 
     = "KEY "++kdlbl kd
             ++": "++name (kdcpt kd)
             ++"("++intercalate ", " [(if null (name o) then "" else name o++":") ++ showADL (objctx o)
                                     | o<-kdats kd]++")"
    disambiguate spc kd
     = kd {kdats = map (disambiguate spc) (kdats kd)}

   instance ShowADL Relation where
    showADL rel@(Rel{}) = name rel
    showADL      I{}    = "I"
    showADL      V{}    = "V"
    showADL rel@(Mp1{}) = "'"++relval rel++"'"
    disambiguate _ _
     = fatal 196 "calling disambiguate on a relation.\nPlease turn into an expression using ERel."

   instance ShowADL Expression where
    showADL = showExpr (" = ", " |- ", "/\\", " \\/ ", " - ", " / ", " \\ ", ";", "!", "*", "+", "~", ("-"++), "(", ")", "[", "*", "]") . insParentheses
--  The function 'disambiguate' ensures that an expression, when printed, can be parsed with no ambiguity.
--  Removal of ambiguities is done by adding a type cast expression on a subexpression that would cause a type error when compiled.
--  A type cast is avoided if the expression is unambiguous by itself.
--  To maintain readability, effort has been spent to keep the number of type cast expressions down.
    disambiguate spc e = if isTypeable e
                         then (recur [].map fst.sort' snd) [(r,negate (length [sign d | d <- ds, name d == name r])) | r<-mors e]
                         else fatal 205 ("disambiguate is not defined on " ++ show e)
     where
      recur castRs (r:rs) = case length sgns of
                             1 -> e'
                             _ -> recur (castRs++[r]) rs
                            where (sgns,e') = dis castRs e
      recur castRs _      = case length sgns of
                             1 -> e'
                             0 -> fatal 215 ("no signature found in expression "++show e)
                             _ -> fatal 216 ("multiple signatures...\n   "++show sgns)
                            where (sgns,e') = dis castRs e

      ds = declarations spc

      dis :: [Relation]  ->         -- the available types per relation
             Expression  ->         -- the expression to be disambiguated 
             ([Sign], Expression)
      dis rs x@(EEqu (l,r))                            --  equivalence             = 
       = ( candidates
         , if length  candidates==1 then EEqu (   lExpr             ,    rExpr             ) else
           if length lSgns==1 then EEqu (   lExpr             , ETyp rExpr (sign rExpr)) else
           if length rSgns==1 then EEqu (ETyp lExpr (sign lExpr),    rExpr             ) else
                                   ETyp (EEqu (lExpr,rExpr)) (sign x)
         ) where candidates = nub [l' | l'<-lSgns, r'<-rSgns, l' == r']
                 (lSgns,lExpr) = dis rs l
                 (rSgns,rExpr) = dis rs r
      dis rs x@(EImp (l,r))                            --  implication             |-
       = ( candidates
         , if length  candidates==1 then EImp (   lExpr             ,    rExpr             ) else
           if length lSgns==1 then EImp (   lExpr             , ETyp rExpr (sign rExpr)) else
           if length rSgns==1 then EImp (ETyp lExpr (sign lExpr),    rExpr             ) else
                                   ETyp (EImp (lExpr,rExpr)) (sign x)
         ) where candidates = nub [r' | l'<-lSgns, r'<-rSgns, r' <= l']
                 (lSgns,lExpr) = dis rs l
                 (rSgns,rExpr) = dis rs r
      dis rs (EIsc es)                               --  intersection            /\
       = ( nub [foldr1 lub pth | pth<-allpaths]
         , expr
         ) where
            (expr, allpaths) = castMostAmbiguous comparable EUni rs es
      dis rs (EUni es)                               --  union                   \/
       = ( nub [foldr1 glb pth | pth<-allpaths]
         , expr
         ) where
            (expr, allpaths) = castMostAmbiguous comparable EUni rs es
      dis rs x@(EDif (l,r))                            --  difference              -
       = ( candidates
         , if length candidates==1 then EDif (   lExpr             ,    rExpr             ) else
           if length lSgns==1      then EDif (   lExpr             , ETyp rExpr (sign rExpr)) else
           if length rSgns==1      then EDif (ETyp lExpr (sign lExpr),    rExpr             ) else
                                        ETyp (EDif (lExpr,rExpr)) (sign x)
         ) where candidates = nub [l' | l'<-lSgns, r'<-rSgns, r' <= l']
                 (lSgns,lExpr) = dis rs l
                 (rSgns,rExpr) = dis rs r
      dis rs x@(ELrs (l,r))                            --  left residual           /
       = ( candidates
         , if length candidates==1 then ELrs (   lExpr             ,    rExpr             ) else
           if length lSgns==1      then ELrs (   lExpr             , ETyp rExpr (sign rExpr)) else
           if length rSgns==1      then ELrs (ETyp lExpr (sign lExpr),    rExpr             ) else
                                        ETyp (ELrs (lExpr,rExpr)) (sign x)
         ) where candidates = nub [Sign (source l') (source r') | l'<-lSgns, r'<-rSgns, target l' `comparable` target r']
                 (lSgns,lExpr) = dis rs l
                 (rSgns,rExpr) = dis rs r
      dis rs x@(ERrs (l,r))                            --  right residual          \
       = ( candidates
         , if length candidates==1 then ERrs (   lExpr             ,    rExpr             ) else
           if length lSgns==1      then ERrs (   lExpr             , ETyp rExpr (sign rExpr)) else
           if length rSgns==1      then ERrs (ETyp lExpr (sign lExpr),    rExpr             ) else
                                        ETyp (ERrs (lExpr,rExpr)) (sign x)
         ) where candidates = nub [Sign (target l') (target r') | l'<-lSgns, r'<-rSgns, source l' `comparable` source r']
                 (lSgns,lExpr) = dis rs l
                 (rSgns,rExpr) = dis rs r
      dis _  (ECps []) = fatal 268 "dis []"
      dis rs (ECps es)                               --  composition             ;
       = ( nub [Sign (source (head pth)) (target (last pth)) | pth<-allpaths]
         , expr
         ) where
            e1 `matches` e2 = target e1 `comparable` source e2
            (expr, allpaths) = castMostAmbiguous matches ECps rs es
      dis _  (ERad []) = fatal 275 "dis []"
      dis rs (ERad es)                               --  relative addition       !
       = ( nub [Sign (source (head pth)) (target (last pth)) | pth<-allpaths]
         , expr
         ) where
             e1 `matches` e2 = target e1 `comparable` source e2
             (expr, allpaths) = castMostAmbiguous matches ERad rs es
      dis rs (EKl0 expr)                                --  Rfx.Trn closure         *
       = ( candidates, EKl0 expr' ) where (candidates, expr') = dis rs expr
      dis rs (EKl1 expr)                               --  Transitive closure      +
       = ( candidates, EKl1 expr' ) where (candidates, expr') = dis rs expr
      dis rs (EFlp expr)                               --  Conversion              ~
       = ( [Sign t s | Sign s t<-candidates], EFlp expr') where (candidates, expr') = dis rs expr
      dis rs (ECpl expr)                               --  OBSOLETE.. remove later
       = ( candidates, ECpl expr' ) where (candidates, expr') = dis rs expr
      dis rs (EBrk expr)                               --  bracketed expression ( ... )
       = ( candidates, EBrk expr' ) where (candidates, expr') = dis rs expr
      dis rs (ETyp expr sgn)                          --  type cast expression ... [c]
       = ( [sgn]
         , ETyp expr' sgn
         ) where (_, expr') = dis rs expr
      dis rs x@(ERel rel@Rel{})                              --  simple relation
       = if rel `elem` rs
         then ( [sign rel], ETyp x (sign x) )
         else ( [sign d | d<-ds, name d==name rel], x)
      dis _ x@(ERel rel)                                    --  V, I, or Mp1
       = ( [sign rel], x)
       
--    in some cases, there is a choice which subexpression to disambiguate.
--    That choice is made in favour of the subexpression with the most alternatives to choose from.
      castMostAmbiguous :: (Sign -> Sign -> Bool)
                        -> ([Expression] -> Expression)
                        -> [Relation]
                        -> [Expression]
                        -> (Expression, [[Sign]])
      castMostAmbiguous criterion combinator rs es
       = case maxAlts of
            1  -> (combinator es                                                                          , combins)
            _  -> (disambiguate spc (combinator (take n es++[ETyp (es!!n) (sign (es!!n))]++drop (n+1) es)), combins)
          where
          -- Step 1: determine all possible types of all subexpressions
            ess      :: [ ([Sign], Expression) ]
            ess      = map (dis rs) es                               -- e.g.  [ (e0, [1,2,3]), (e1, [6,7]) ]
          -- Step 2: make all valid combinations
            combins  = combs [es' | (es',_)<-ess]         -- suppose combs [ [1,2,3], [6,7] ] = [ [2,6],[2,7] ]
            combs []       = [[]]
            combs (es':ess') = [ x:xs | x<-es', xs<-combs ess', null xs || (x `criterion` head xs)]
          -- Step 3: All combinations have the same length as es.
          --         Now transpose the combinations and count which subexpression has the most alternatives.
          --         That subexpression is regarded as the most ambiguous...
            eAlts    = map nub (transpose combins) -- each element corresponds to one element of es, and represents its alternatives
                                                   -- e.g. transpose [ [2,6], [2,7] ]  = [[2,2],[6,7]], so eAlts becomes  [[2],[6,7]]
            maxAlts  = if null eAlts -- also for debugging; e.g.    || ["identifier"] == map name (mors (combinator es))
                       then fatal 326 ("\ne:       "++show (combinator es)++
                                       "\ness:     "++show ess++
                                       "\ncombins: "++show combins++
                                       "\neAlts:   "++show eAlts)
                       else maximum (map length eAlts)  -- the maximum number of alternatives of these expressions
            n        = length (takeWhile f eAlts) where f e1 = length e1<maxAlts -- the index of the subexpression that we regard as the most ambiguous.

   instance ShowADL Declaration where
    showADL decl = 
     case decl of
        Sgn{decusr = False} -> fatal 323 "call to ShowADL for declarations can be done on user defined relations only." 
        Sgn{} -> name decl++" :: "++name (source decl)++(if null ([Uni,Tot]>-multiplicities decl) then " -> " else " * ")++name (target decl)++
                 (let mults=if null ([Uni,Tot]>-multiplicities decl) then multiplicities decl>-[Uni,Tot] else multiplicities decl in
                  if null mults then "" else "["++intercalate "," (map showADL mults)++"]")++
                 (if null(decprL decl++decprM decl++decprR decl) then "" else
                  " PRAGMA "++unwords (map show [decprL decl,decprM decl,decprR decl]))
                  ++ (if null (decMean decl) then "" else " MEANING \""++decMean decl++"\"")
        Isn{}     -> fatal 330 "Illegal call to ShowADL (Isn{}). Isn{} is of type Declaration and it is not user defined. A call to ShowADL for declarations can be done on user defined declarations only." 
        Iscompl{} -> fatal 331 "Illegal call to ShowADL (Iscompl{}). Iscompl{} is of type Declaration and it is not user defined. A call to ShowADL for declarations can be done on user defined declarations only." 
        Vs{}      -> fatal 332 "Illegal call to ShowADL (Vs{}). Vs{} is of type Declaration and it is not user defined. A call to ShowADL for declarations can be done on user defined declarations only." 
    disambiguate _ = id


   instance ShowADL Prop where
    showADL = show
    disambiguate _ = id

   instance ShowADL A_Concept where
    showADL c = show (name c)
    disambiguate _ = id

   instance ShowADL ConceptDef where
    showADL cd
     = "\n  CONCEPT "++show (name cd)++" "++show (cddef cd)++" "++(if null (cdref cd) then "" else show (cdref cd))
    disambiguate _ = id

   instance ShowADL Architecture where
    showADL arch = intercalate "\n\n" (map showADL (arch_Contexts arch))
    disambiguate _ _
     = fatal 352 "An architecture may consist of various contexts. Rather use the function 'disambiguate' on a specific context."
     
   instance ShowADL A_Context where
    showADL context
     = "CONTEXT " ++name context
       ++ (if null (ctxprocs context) then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxprocs context))++ "\n") -- All processes
       ++ (if null (ctxifcs context)  then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxifcs context)) ++ "\n")
       ++ (if null (ctxpats context)  then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxpats context)) ++ "\n")
       ++ (if null (ctxrs context)    then "" else "\n"      ++intercalate "\n"   (map showADL (ctxrs context))   ++ "\n")
       ++ (if null (ctxds context)    then "" else "\n"      ++intercalate "\n"   (map showADL (ctxds context))   ++ "\n")
       ++ (if null (ctxks context)    then "" else "\n"      ++intercalate "\n"   (map showADL (ctxks context))   ++ "\n")
       ++ (if null (ctxgs context)    then "" else "\n"      ++intercalate "\n"   (map showADL (ctxgs context))   ++ "\n")
       ++ (if null (ctxcs context)    then "" else "\n"      ++intercalate "\n"   (map showADL (ctxcs context))   ++ "\n")
       ++ (if null (ctxsql context)   then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxsql context)) ++ "\n")
       ++ (if null (ctxphp context)   then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxphp context)) ++ "\n")
       ++ "\n\nENDCONTEXT"
    disambiguate _ context
     = context { ctxpats  = map (disambiguate context) (ctxpats  context)
               , ctxprocs = map (disambiguate context) (ctxprocs context)
               , ctxrs    = map (disambiguate context) (ctxrs    context)
               , ctxks    = map (disambiguate context) (ctxks    context)
               , ctxifcs  = map (disambiguate context) (ctxifcs  context)
               , ctxps    = map (disambiguate context) (ctxps    context)
               , ctxsql   = map (disambiguate context) (ctxsql   context)
               , ctxphp   = map (disambiguate context) (ctxphp   context)
     --        , ctxenv   = let (e,ds) = ctxenv context in (disambiguate context e, ds)
               }

   instance ShowADL Fspc where
    showADL fSpec
     = "CONTEXT " ++name fSpec
       ++ if null (map ifcObj [] {- map fsv_ifcdef (fActivities fSpec) -})     
           then "" 
           else "\n"++intercalate "\n\n" (map (showADL . ifcObj) [] {- map fsv_ifcdef (fActivities fSpec) -})     ++ "\n"
       ++ (if null (patterns fSpec)    then "" else "\n"++intercalate "\n\n" (map showADL (patterns fSpec))    ++ "\n")
       ++ (if null (conceptDefs fSpec) then "" else "\n"++intercalate "\n"   (map showADL (conceptDefs fSpec)) ++ "\n")
       ++ (if null (fSexpls fSpec) then "" else "\n"++intercalate "\n"   (map showADL (fSexpls fSpec)) ++ "\n")
       ++ (if null (vkeys fSpec)       then "" else "\n"++intercalate "\n"   (map showADL (vkeys fSpec))       ++ "\n")
       ++ (if null showADLpops         then "" else "\n"++intercalate "\n\n" showADLpops                                    ++ "\n")
       ++ "\n\nENDCONTEXT"
       where showADLpops = [ showADL Popu{popm=makeRelation d, popps=decpopu d}
                           | d<-declarations fSpec, decusr d, not (null (decpopu d))]
    disambiguate _ fSpec
     = fatal 394 ("calling 'disambiguate' on an Fspc, "++name fSpec++". This has already been disambiguated in adl2fspec.")


{- 
   instance SelfExplained ECArule where   --TODO: Wat doet deze definitie in ShowADL??? (Omdat ShowADL er in wordt gebruikt....)
     autoMeaning flags r
      = [Means English (string2Blocks (defaultFlags {language=English})
          ( case p of
             Chc {} -> "Pick from "++show (length (paCls p))++" options, in order to maintain "++shMotivEng (paMotiv p)++"."
             All {} -> "Execute "++show (length (paCls p))++" ECA-rules, in order to maintain "++shMotivEng (paMotiv p)++"."
             Do  {paSrt=Ins} -> "Insert tuple(s) in "++showADL (paTo p)++" to maintain "++shMotivEng (paMotiv p)++"."
             Do  {paSrt=Del} -> "Remove tuple(s) from "++showADL (paTo p)++" to maintain "++shMotivEng (paMotiv p)++"."
             Sel {} -> "Select an element from "++showADL (paTo p)++" to maintain "++shMotivEng (paMotiv p)++"."
             New {} -> "Create a new element in "++showADL (paTo p)++" to maintain "++shMotivEng (paMotiv p)++"."
             Rmv {} -> "Remove an element from "++showADL (paTo p)++" to maintain "++shMotivEng (paMotiv p)++"."
             Nop {} -> "Do nothing to maintain "++shMotivEng (paMotiv p)++", because it is still valid."
             Blk {} -> "Abort to prevent violation of "++shMotivEng (paMotiv p)
           )] ++
         [Means Dutch (string2Blocks (defaultFlags {language=Dutch})
           ( case p of
             Chc {} -> "Kies uit "++show (length (paCls p))++" opties, om te garanderen dat "++shMotivDut (paMotiv p)++"."
             All {} -> "Voer "++show (length (paCls p))++" ECA-rules uit, om te garanderen dat "++shMotivDut (paMotiv p)++"."
             Do  {paSrt=Ins} -> "Voer tweetal(len) in in "++showADL (paTo p)++" om te garanderen dat "++shMotivDut (paMotiv p)++"."
             Do  {paSrt=Del} -> "Verwijder tweetal(len) uit "++showADL (paTo p)++" om te garanderen dat "++shMotivDut (paMotiv p)++"."
             Sel {} -> "Selecteer een element uit "++showADL (paTo p)++" om te garanderen dat "++shMotivDut (paMotiv p)++"."
             New {} -> "Maak een nieuw element aan in "++showADL (paTo p)++" om te garanderen dat "++shMotivDut (paMotiv p)++"."
             Rmv {} -> "Verwijder een element uit "++showADL (paTo p)++" om te garanderen dat "++shMotivDut (paMotiv p)++"."
             Nop {} -> "Doe niets om te garanderen dat "++shMotivDut (paMotiv p)++", want het geldt nog steeds."
             Blk {} -> "Breek af, om een overtreding te voorkomen van "++shMotivDut (paMotiv p)
            )]
      where
       p = ecaAction r
       shMotivEng ms = commaEng "and" [ showADL conj++" FROM "++intercalate "," ["R"++show (nr r') | r'<-rs]++")" | (conj,rs)<-ms]
       shMotivDut ms = shMotivEng ms  -- TODO: Nog even de nederlandse versie organiseren... 
-}
   instance ShowADL ECArule where
     showADL eca = "ECA #"++show (ecaNum eca)
     disambiguate _ = id
   instance ShowADL Event where
     showADL = show
     disambiguate _ = id
   instance (ShowADL a, ShowADL b) => ShowADL (a,b) where
    showADL (a,b) = "(" ++ showADL a ++ ", " ++ showADL b ++ ")"
    disambiguate spc (a,b) = (disambiguate spc a, disambiguate spc b)

   instance ShowADL Population where
    showADL (Popu r ps)
     = "POPULATION "++showADL r++" CONTAINS\n"++
       indent++"[ "++intercalate ("\n"++indent++"; ") (map show ps)++indent++"]"
       where indent = "   "
    disambiguate _ = id

   --used to compose error messages at p2a time
   instance ShowADL P_Expression where
    showADL expr = showPExpr (" = ", " |- ", "/\\", " \\/ ", " - ", " \\ ", " / ", ";", "!", "*", "+", "~", "(", ")", "[", "*", "]") expr
      where
       showPExpr (equi,impl,inter,union',diff,lresi,rresi,rMul,rAdd,closK0,closK1,flp',lpar,rpar,lbr,star,rbr)
        = showchar
         where
          showchar (Pequ (l,r))          = showchar l++equi++showchar r
          showchar (Pimp (l,r))          = showchar l++impl++showchar r
          showchar (Pisc [])             = "V"
          showchar (Pisc es)             = intercalate inter  [showchar e | e<-es]
          showchar (PUni [])             = "-V"
          showchar (PUni es)             = intercalate union' [showchar e | e<-es]
          showchar (PDif (l,r))          = showchar l++diff ++showchar r
          showchar (PLrs (l,r))          = showchar l++lresi++showchar r
          showchar (PRrs (l,r))          = showchar l++rresi++showchar r
          showchar (PCps [])             = "I"
          showchar (PCps es)             = intercalate rMul [showchar e | e<-es]
          showchar (PRad [])             = "-I"
          showchar (PRad es)             = intercalate rAdd [showchar e | e<-es]
          showchar (PKl0 e)              = showchar e++closK0
          showchar (PKl1 e)              = showchar e++closK1
          showchar (PFlp e)              = showchar e++flp'
          showchar (PCpl e)              = '-':showchar e
          showchar (PBrk e)              = lpar++showchar e++rpar
          showchar (PTyp e (P_Sign{psign=[x]}))  = showchar e++lbr++showADL x++rbr
          showchar (PTyp e (P_Sign{psign=xs }))  = showchar e++lbr++showADL (head xs)++star++showADL (last xs)++rbr
          showchar (Prel rel)            = showADL rel
    disambiguate _ _
     = fatal 585 "calling disambiguate on a P_Expression (in the Parser's data structure).\nDo it on datatype Expression (which is in the A-structure)."

   --used to compose error messages at p2a time
   instance ShowADL P_Relation where
    showADL rel = case rel of
         P_Rel{} -> rel_nm rel
         P_I{}   -> " I "
         P_V{}   -> " V "
         P_Mp1{} -> rel_1val rel
    disambiguate _ _
     = fatal 459 "calling disambiguate on a P_Relation (in the Parser's data structure).\nDo it on datatype Relation (which is in the A-structure)."

   --used to compose error messages at p2a time
   instance ShowADL P_Concept where
    showADL = name
    disambiguate _ r
     = fatal 590 ("calling disambiguate on a population in the Parser's data structure, \n"++show r++"\nIt will work only on the A-structure.")

{-
   instance ShowADL P_Sign where
    showADL sgn = show sgn
    disambiguate _ _
     = fatal 450 ("calling disambiguate on a P_Sign (in the Parser's data structure).\nDo it on datatype Sign (which is in the A-structure).")

   instance Show P_Declaration where
    showADL decl
     = name decl++" :: "++name (source decl)++(if null ([Uni,Tot]>-dec_prps decl) then " -> " else " * ")++name (target decl)++
       (let mults=if null ([Uni,Tot]>-dec_prps decl) then dec_prps decl>-[Uni,Tot] else dec_prps decl in
        if null mults then "" else "["++intercalate "," (map showADL mults)++"]")++
       (if null(dec_prL decl++dec_prM decl++dec_prR decl) then "" else
        " PRAGMA "++unwords (map show [dec_prL decl,dec_prM decl,dec_prR decl]))
        ++ (if null (dec_Mean decl) then "" else " MEANING \""++dec_Mean decl++"\"")
    disambiguate _ _
     = fatal 470 ("calling disambiguate on a P_Declaration (in the Parser's data structure).\nDo it on datatype Declaration (which is in the A-structure).")

   instance ShowADL P_Population where
    showADL (P_Popu r cs ps)
     = "POPULATION "++showADL r++" CONTAINS\n"++
       indent++"[ "++intercalate ("\n"++indent++"; ") (map show ps)++indent++"]"
       where indent = "   "
    disambiguate _ _
     = fatal 478 ("calling disambiguate on a population in the Parser's data structure.\nIt will work only on the A-structure.")

   instance ShowADL P_ObjectDef where
   -- WHY (HJ)? In deze instance van ShowADL worden diverse zaken gebruikt die ik hier niet zou verwachten.
   --              Het vertroebelt de code ook een beetje, want nu moeten er dingen als 'source' en
   --              'target' hier al bekend zijn.
   --              Dat lijkt me hier nog niet op z'n plaats, als je alleen maar wat wilt kunnen 'prettyprinten'. 
   -- BECAUSE (SJ): Dit blijft nog even zo, omdat showADL gebruikt wordt in het genereren van interfaces.
   --              Zolang we dat nog niet onder de knie hebben blijft de code wat troebel.
    showADL obj = "  INTERFACE "++name obj++" : "++showADL (obj_ctx obj)++
                  recur "\n  " (obj_ats obj)
     where recur :: String -> [P_ObjectDef] -> String
           recur ind objs
            = ind++" = [ "++
              intercalate (ind++"   , ") 
                                  [ name o++
                                     (if null (obj_strs o) then "" else " {"++intercalate ", " [unwords (map str ss) | ss<-obj_strs o]++"}")++
                                     " : "++showADL (obj_ctx o)++
                                     if null (obj_ats o) then "" else recur (ind++"     ") (obj_ats o)
                                  | o<-objs
                                  ]++
              ind++"   ]"
           str ss | and [isAlphaNum c | c<-ss] = ss
                  | otherwise                 = "\""++ss++"\""
    disambiguate spc obj
     = obj {obj_ctx = disambiguate spc (obj_ctx obj)
           ,obj_ats = map (disambiguate spc) (obj_ats obj)
           }

   instance ShowADL P_Process where
--    showADL prc
--     = "PROCESS " ++ name prc 
--       ++ (if null (gens prc)         then "" else "\n  " ++intercalate "\n  " (map showADL (gens prc)        ) ++ "\n")
--       ++ (if null (declarations prc) then "" else "\n  " ++intercalate "\n  " (map showADL (declarations prc)) ++ "\n")
--       ++ (if null (rules prc)        then "" else "\n  " ++intercalate "\n  " (map showADL (rules prc)       ) ++ "\n")
--       ++ (if null (maintains prc)    then "" else "\n  " ++showpRM prc ++ "\n")
--       ++ "ENDPROCESS"
    showADL _ = fatal 515 "ShowADL temporarily out of order. (see ticket #85) "
    disambiguate _ _
     = fatal 517 ("calling disambiguate on a P_Process in the Parser's data structure. It will work only on the A-structure.")

   instance ShowADL P_Pattern where
    showADL pat
     = "PATTERN " ++ name pat ++ "\n"
       ++ (if null (pt_rls pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (pt_rls pat)) ++ "\n")
       ++ (if null (pt_gns pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (pt_gns pat)) ++ "\n")
       ++ (if null (pt_dcs pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (pt_dcs pat)) ++ "\n")
       ++ (if null (pt_cds pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (pt_cds pat)) ++ "\n")
       ++ (if null (pt_kds pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (pt_kds pat)) ++ "\n")
       ++ "ENDPATTERN"
    disambiguate _ _
     = fatal 529 ("calling disambiguate on a rule in the Parser's data structure. It will work only on the A-structure.")

   instance ShowADL P_Rule where
    showADL r 
     = "RULE \""++rr_nm r++"\" : "++showADL (rr_exp r)
       ++ if null phrs then [] else "\n     PHRASE "++ showstr (head phrs)
        where phrs = [explainContent2String econt | Means _ econt<-rr_mean r]
    disambiguate _ _
     = fatal 537 ("calling disambiguate on a rule in the Parser's data structure. It will work only on the A-structure.")

   instance ShowADL P_Interface where
    showADL ifc 
     = "INTERFACE "++name ifc
             ++"("++intercalate ", " [showADL r | r<-ifc_Params ifc]++")\n"
             ++"{"++intercalate ", " [unwords strs | strs<-ifc_Args ifc]++"}\n"
             ++showADL (ifc_Obj ifc)
             ++show (ifc_Expl ifc)
    disambiguate spc pat
     = pat {ifc_Obj = disambiguate spc (ifc_Obj pat)}

   instance ShowADL P_KeyDef where
    showADL kd 
     = "KEY "++kd_lbl kd
             ++": "++name (kd_cpt kd)
             ++"("++intercalate ", " [(if null (name o) then "" else name o++":") ++ showADL (obj_ctx o)
                                     | o<-kd_ats kd]++")"
    disambiguate spc kd
     = kd {kd_ats = map (disambiguate spc) (kd_ats kd)}

-- WHY?  Why keep ShowADL P_Context now we have ShowADL Fspc ?
-- BECAUSE For debugging purposes, it might be useful to be able to print a P_Context in .Ampersand syntax...
   instance ShowADL P_Context where
    showADL context
     = "CONTEXT " ++name context
       ++ (if null (ctx_PPrcs context)then "" else "\n"      ++intercalate "\n\n" (map showADL (ctx_PPrcs context))++ "\n") -- All processes
       ++ (if null (ctx_ifcs context) then "" else "\n"      ++intercalate "\n\n" (map showADL (ctx_ifcs context)) ++ "\n")
       ++ (if null (ctx_pats context) then "" else "\n"      ++intercalate "\n\n" (map showADL (ctx_pats context)) ++ "\n")
       ++ (if null (ctx_rs context)   then "" else "\n"      ++intercalate "\n"   (map showADL (ctx_rs context))   ++ "\n")
       ++ (if null (ctx_ds context)   then "" else "\n"      ++intercalate "\n"   (map showADL (ctx_ds context))   ++ "\n")
       ++ (if null (ctx_ks context)   then "" else "\n"      ++intercalate "\n"   (map showADL (ctx_ks context))   ++ "\n")
       ++ (if null (ctx_cs context)   then "" else "\n"      ++intercalate "\n"   (map showADL (ctx_cs context))   ++ "\n")
       ++ (if null (ctx_pops context) then "" else "\n"      ++intercalate "\n\n" (map showADL (ctx_pops context)) ++ "\n")
       ++ (if null (ctx_sql context)  then "" else "\n"      ++intercalate "\n\n" (map showADL (ctx_sql context)) ++ "\n")
       ++ (if null (ctx_php context)  then "" else "\n"      ++intercalate "\n\n" (map showADL (ctx_php context)) ++ "\n")
       ++ "\n\nENDCONTEXT"
    disambiguate _ r
     = fatal 609 ("calling disambiguate on a context in the Parser's data structure, \n"++show r++"\nIt will work only on the A-structure.")

-}
