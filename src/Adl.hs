module Adl ( Architecture(..)
           , Context(..),Contexts
           , Pattern(..),Patterns,union
           , Rule(..),Rules,consequent
           , KeyDef(..),KeyDefs,keys
           , Population(..),Populations
           , ObjectDef(..),ObjectDefs
           , Expression(..), Expressions,isPos,isNeg
           , Gen(..),Gens
           , Morphism(..),Morphisms,inline,makeMph,makeInline
           , Declaration(..),Declarations
           , ConceptDef(..),ConceptDefs
           , Concept(..), GenR, Concepts, one,v,cptnew,cptS
           , RuleType(..)
           , Prop(..)
           , FilePos(..),posNone
           , makeDeclaration,mIs,ruleType
           , antecedent,notCp,cptAnything,cpu
           , Object(..)
           , Language(..)
           , Morphical(..)
           , Association(..)
           , Morphic(..),normExpr
           , MorphicId(..)
           , Populated(..)
           , Numbered(..)
           , Substitutive(..)
           , Identified(..)
           , Explained(..)
           , Label(..)
           , Paire,Pairs
           )
where

   import Adl.Concept                    (Concept(..),Concepts,one,cptnew,cptS,cptAnything
                                         ,GenR(..)
                                         ,Association(..)
                                         ,Morphic(..))
   import Adl.ConceptDef                 (ConceptDef(..),ConceptDefs)
   import Adl.Context                    (Context(..),Contexts
                                         ,Architecture(..))
   import Adl.Expression                 (Expression(..),Expressions
                                         ,isPos,isNeg,v,notCp)
   import Adl.FilePos                    (FilePos(..)
                                         ,posNone
                                         ,Numbered(..))
   import Adl.Gen                        (Gen(..),Gens)
   import Adl.KeyDef                     (KeyDef(..),KeyDefs
                                         ,Key(..))
   import Adl.Label                      (Label(..))
   import Adl.MorphismAndDeclaration     (Morphism(..),Morphisms
                                         ,Declaration(..),Declarations
                                         ,makeMph,makeDeclaration
                                         ,inline,makeInline
                                         ,mIs)
   import Adl.ObjectDef                  (ObjectDef(..),ObjectDefs)
   import Adl.Pair                       (Paire(..),Pairs
                                         ,Populated(..),join)
   import Adl.Pattern                    (Pattern(..),Patterns,union)
   import Adl.Population                 (Population(..),Populations)
   import Adl.Prop                       (Prop(..))
   import Adl.Rule                       (Rule(..),Rules
                                         ,RuleType(..)
                                         ,consequent,antecedent,cpu,ruleType)
   import Classes.Morphical              (Morphical(..))
   import Classes.Morphic                (Substitutive(..),MorphicId(..),normExpr)
   import Classes.Object                 (Object(..))
   import Classes.Language               (Language(..))
   import CommonClasses                  (Identified(..)
                                         ,ABoolAlg(order,lub,glb)
                                         ,Conceptual(conts)
                                         ,Explained(explain))
   import Collection                     (Collection (uni,(>-),isc))   
   import Auxiliaries                    (clos1,diag,eqCl) 


   instance Key Context where
    keys context
     = ( concat [keys p| p<-ctxpats context] ++
         [(target ctx,lbl,ats)|Kd pos lbl ctx ats<-keyDefs context]
       )
   instance Key Pattern where
    keys pat = [(target ctx,lbl,ats)|Kd pos lbl ctx ats<-keyDefs pat]
 
   instance Substitutive Expression where
    subst (Tm m,f) t@(Tm m') = if     m==m' then f     else
                               if flp m==m' then flp f else t
    subst (m,f) t@(Tm m')    = t
    subst (m,f) f'           = if m `match` f'
                               then (if m==f' then f else if flp m==f' then flp f else subs f')
                               else subs f'
     where
       subs (Tc f')    = Tc (subst (m,f) f')
       subs (F ts)     = F  (subst (m,f) ts)
       subs (Fd ts)    = Fd (subst (m,f) ts)
       subs (Fu fs)    = Fu (subst (m,f) fs)
       subs (Fi fs)    = Fi (subst (m,f) fs)
       subs (K0 e)     = K0 (subst (m,f) e)
       subs (K1 e)     = K1 (subst (m,f) e)
       subs (Cp e)     = Cp (subst (m,f) e)
       subs e          = subst (m,f) e
       Tm m  `match` Tm m'   = True
       Tc f  `match` Tc f'   = True
       F ts  `match` F  ts'  = True
       Fd ts `match` Fd ts'  = True
       Fu fs `match` Fu fs'  = True
       Fi fs `match` Fi fs'  = True
       K0 e  `match` K0 e'   = True
       K1 e  `match` K1 e'   = True
       Cp e  `match` Cp e'   = True
       _     `match` _       = False

   instance Substitutive Rule where
    subst (m,f) r@(Ru Truth antc pos cons cpu expla sgn nr pn)
     = Ru Truth (error ("(Module CC_aux:) illegal call to antecedent in subst ("++show m++","++show f++") ("++show r++")")) pos cons' cpu expla (sign cons') nr pn
       where cons' = subst (m,f) cons
    subst (m,f) r@(Ru c antc pos cons cpu expla sgn nr pn)
     = if sign antc' `order` sign cons'
       then Ru c antc' pos cons' cpu expla (sign antc' `lub` sign cons') nr pn
       else r -- error ("(module CC_aux) Fatal: cannot execute:   subst (m,f) r\nwith m="++show m++"\n     f="++show f++"\nand  r="++show r++"\n"++showHS "" r++"\nbecause "++show (sign antc')++" `order` "++show (sign cons')++" is False.\n"++gEtabG gEq [c| (a,b)<-[sign antc',sign cons'], c<-[a,b]])
       where antc' = subst (m,f) antc
             cons' = subst (m,f) cons
    subst (m,f) (Sg p rule expla sgn nr pn signal)
     = Sg p r' expla (sign r') nr pn signal
       where r'= subst (m,f) rule
    subst (m,f) (Gc pos m' expr cpu sgn nr pn)
     = Gc pos m' expr' cpu (sign expr') nr pn
       where expr' = subst (m,f) expr


   instance Populated Expression where
    contents (Tm m)        = contents m
    contents (Tc f)        = contents f
    contents f@(F ts)
     | idsOnly ts
        = if not (source f `order` target f) then error ("(module CC_aux) Fatal: no order in "++show f) else
                             [[e,e]|e<-os]
     | otherwise
        = if null css then error ("(module CC_aux) Fatal: no terms in F "++show ts) else
                             foldr1 join css
                             where os = conts (source f `lub` target f)
                                   css = [contents t|t<-ts, not (idsOnly t)]
    contents f@(Fd ts)
      = if null ts then error ("(module CC_aux) Fatal: no terms in Fd "++show ts) else joinD ts
    contents (Fu fs) = if null fs then [] else
                       (foldr1 uni .map contents) fs
    contents (Fi fs) = if null fs then [] else
                       (foldr1 isc .map contents) fs
    contents (K0 e)  = clos1 (contents e) `uni` [[c,c]|c <-conts (source e `lub` target e)]
    contents (K1 e)  = clos1 (contents e)
    contents (Cp (Cp e)) = contents e
    contents (Cp e)  = [[a,b]| [a,b]<-diag [] (conts (source e)) [] (conts (target e)), not ([a,b] `elem` contents e)]

   joinD :: [Expression] -> [Paire]
   joinD [s]      = contents s
   joinD (r:s:ts) = [ [head (head rc),last (head sc)]
                    | rc<-eqCl head (contents r)
                    , sc<-eqCl last (joinD (s:ts))
                    , null (conts (target r `glb` source s) >-(map last rc `uni` map head sc))
                    ]

  
