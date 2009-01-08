  {-# OPTIONS -XFlexibleContexts #-}
  module CC_aux ( explain
          --      ,  ShowADL(..)
          --      ,  ShowHS(..)
                , objectOfConcept
                , pKey_pos
                , pString_val_pos
                , pVarid_val_pos, pConid_val_pos
                , renumberRules
                , isSgn
                , fEmpty
                , oneMorphism
                , applyM
                , glb, lub
                , sur, inj, fun, tot
                , mkVar
                , Calc(calc)
                , order
                , isProperty 
                , Pop(update, put_gE)
                , makeConceptSpace , pMeaning
                , anything, shSigns, gEtabG
                , conts , cod, clearG, dom
                , showFullRelName
   ) where
   import Char (toLower)
   import UU_Scanner
   import UU_Parsing
   import CommonClasses ( Identified(name)
                        , ABoolAlg(glb,lub,order)
                        , Explained(explain)
                                                , Conceptual(conts)
                        , Morphics(anything)
                        )
   import Collection (Collection (uni,isc,(>-),empty,rd)  )
   import Languages (Lang,ShowLang,plural)
   import Auxiliaries  
           ( sort', chain, rEncode, commaEng, clos1, diag
            ,eqCl, sord, eqClass, rd', enumerate,  showL, haskellIdentifier
            ,unCap)
   import Classification 
             ( Classification(),preCl,mapCl
             )
   import Typology ( Inheritance(Isa), Typologic, genEq, typology)
   import ADLdef
   import ShowADL
   import ShowHS








   objectOfConcept :: Context -> Concept -> Maybe ObjectDef
   objectOfConcept context cpt = if length os == 0 then Nothing else Just (head os)
     where os = [o|o<-attributes context,concept o == cpt]


   gEtable :: [Concept] -> String
   gEtable cs
    = chain "\n" ([f l " "     ++"  "++chain " " (map (f 6.name) cs)]++
                  [f l (name c')++" |"++chain "|"[f 6 (show (c <= c'))|c<-cs]| c'<-cs])
      where l = maximum (map (length.name) cs); f n str = take n (str++forever ' ')
            forever c = c:forever c

   gEtabG :: ditDingWordtNietGebruikt -> [Concept] -> String
   gEtabG gEqa cs
    = chain "\n" ([f l " "     ++"  "++chain " " (map (f 6.name) cs)]++
                  [f l (name c')++" |"++chain "|"[f 6 (show (c <= c'))|c<-cs]| c'<-cs])
      where l = maximum (map (length.name) cs); f n str = take n (str++forever ' ')
            forever c = c:forever c















   dom, cod :: Declaration -> [String]
   dom s = rd [src l| l<-contents s]
   cod s = rd [trg l| l<-contents s]

   pMeaning Uni   = "univalent"
   pMeaning Inj   = "injective"
   pMeaning Sur   = "surjective"
   pMeaning Tot   = "total"
   pMeaning Sym   = "symmetric"
   pMeaning Asy   = "antisymmetric"
   pMeaning Trn   = "transitive"
   pMeaning Rfx   = "reflexive"
   pMeaning Aut   = "automatic if possible"
   isProperty m   = null([Sym,Asy]>-multiplicities m)








   renumberRule n r@(Ru Automatic b c d e f g _ h) = Ru Automatic (error ("(Module CC_aux:) illegal call to antecedent in renumberRule ("++showADL r++")")) c d e f g n h
   renumberRule n (Ru a b c d e f g _ h)     = Ru a b c d e f g n h
   renumberRule n (Sg p rule c d _ f g)      = Sg p (renumberRule n rule) c d n f g
   renumberRule n (Gc a b c d e _ f)         = Gc a b c d e n f
   renumberRule n r                          = r
   renumberRules n (r:rs) = (renumberRule n r):renumberRules (n+1) rs
   renumberRules _ [] = []



 {-  instance Show Context where
    showsPrec p context  -- (Ctx nm on isa world pats rs ds cs ks os pops)
     = showString ("CONTEXT "++nm++
                   (if null (extends context) then "" else " EXTENDS "++chain ", " (extends context))++"\n"++
                   chain "\n\n" (map show (patterns context))++"\n"++
                   chain "\n" (map show (rules context))++"\n"++
                   chain "\n" (map show (declarations context))++"\n"++
                   chain "\n" (map show (conceptDefs context))++"\n"++
                   chain "\n" (map show (keyDefs context))++"\n"++
                   chain "\n" (map show (objectDefs context))++"\n"++
                   chain "\n" (map show (populations context))++"\nENDCONTEXT" ) -}



--   class ShowHS a where
--    showHSname :: a -> String
--    showHS     :: String -> a -> String


   isFactor (Fu fs) = True
   isFactor (Fi fs) = True
   isFactor _ = False

   showS m = name m++"["++show (source m)++","++show (target m)++"]"
   showFullRelName m = rEncode (name m++name (source m)++name (target m))

   instance ShowADL a => ShowADL (Inheritance a) where
    showADL (Isa ts cs) = ""























   shSigns [(a,b)] = "["++show a++"*"++show b++"]"
   shSigns ss = commaEng "or" ["["++show a++"*"++show b++"]"|(a,b)<-ss]







   join::[Paire]->[Paire]->[Paire]
   join a b = merge ((sort' (trg.head).eqCl trg) a)
                    ((sort' (src.head).eqCl src) b)
              where merge (xs:xss) (ys:yss)
                     | trg (head xs)<src (head ys) = merge xss (ys:yss)
                     | trg (head xs)>src (head ys) = merge (xs:xss) yss
                     | otherwise = [[x,y]|[x,i]<-xs,[j,y]<-ys]++ merge xss yss
                    merge _ _ = []
   makeConceptSpace :: GenR -> [Morphism] -> Concepts
   makeConceptSpace gE morphisms
    = [ upd (fst (head raw)) (sord (concat (map snd raw)))
      | raw <- eqCl fst [(c,os)| m@(Mph nm pos atts (s,t) yin sgn@(Sgn _ s' t' _ _ _ _ ds _ _ _ _)) <- morphisms
                               , (c,os) <- [(s',dom sgn),(t',cod sgn)]
                        ]
      ] where
         upd c os | isC c     = c{cptos=os}
                  | otherwise = c




   displayInternalCode context c
    = null[o| o<-attributes context, c==concept o]



   class Pop a where
    put_gE     :: GenR -> Concepts  -> a -> a
    specialize :: (Concept,Concept) -> a -> a
    update     :: [Declaration] -> a -> a
    update ss c = c

   instance Pop Concept where
    put_gE gE cs c = h (head ([c'|c'<-cs, c==c']++[c]))
            where h x | isC x = x{cptgE = gE}
                      | otherwise = x

    specialize (a,b) c = if length (eqClass order [a,b,c])>1 then error ("(module CC_aux) Fatal: specialize 1 ("++show a++","++show b++") "++showHS "" c) else
                         (a `glb` b) `lub` c

   instance Pop KeyDef where
    put_gE gE cs (Kd pos lbl ctx ats) = Kd pos lbl (put_gE gE cs ctx) [put_gE gE cs a| a<-ats]
    update ss    (Kd pos lbl ctx ats) = Kd pos lbl (update ss    ctx) [update ss    a| a<-ats]
    specialize t (Kd pos lbl ctx ats) = Kd pos lbl (specialize t ctx) [specialize t a| a<-ats]

   instance Pop ObjectDef where
    put_gE gE cs obj = obj { objctx = (put_gE gE cs (objctx obj))
                           , objats = [put_gE gE cs a| a<-objats obj]
                           }
    update ss    obj = obj { objctx = (update ss    (objctx obj))
                           , objats = [update ss    a| a<-objats obj]
                           }
    specialize t obj = obj { objctx = (specialize t (objctx obj))
                           , objats = [specialize t a| a<-objats obj]
                           }


   instance (Pop a,Pop b) => Pop (a,b) where
    put_gE gE cs (x,y) = (put_gE gE cs x, put_gE gE cs y)
    update ss    (x,y) = (update ss    x, update ss    y)
    specialize t (x,y) = (specialize t x, specialize t y)







   instance Pop Gen where
    put_gE gE cs (G pos g s) = G pos (put_gE gE cs g) (put_gE gE cs s)
    update ss    (G pos g s) = G pos (update ss g)    (update ss s)
    specialize t (G pos g s) = G pos (specialize t g) (specialize t s)

   instance Pop Context where
      put_gE gE cs context
                  = context { ctxwrld = map (mapCl (put_gE gE cs)) (ctxwrld context)
                            , ctxpats = map (put_gE gE cs) (ctxpats context)
                            , ctxrs   = map (put_gE gE cs) (ctxrs context)
                            , ctxds   = map (put_gE gE cs) (ctxds context)
                            , ctxks   = map (put_gE gE cs) (ctxks context)
                            , ctxos   = map (put_gE gE cs) (ctxos context)
                            }
      update ss context
                  = context { ctxpats = map (update ss) (ctxpats context)
                            , ctxrs   = map (update ss) (ctxrs context)
                            , ctxds   = map (update ss) (ctxds context)
                            , ctxks   = map (update ss) (ctxks context)
                            , ctxos   = map (update ss) (ctxos context)
                            }

      specialize t context
                  = context { ctxpats = map (specialize t) (ctxpats context)
                            , ctxrs   = map (specialize t) (ctxrs context)
                            , ctxds   = map (specialize t) (ctxds context)
                            , ctxks   = map (specialize t) (ctxks context)
                            , ctxos   = map (specialize t) (ctxos context)
                            }

   instance Pop Pattern where
    put_gE gE cs (Pat nm rs gen pms cs' ks) = Pat nm (map (put_gE gE cs) rs) (map (put_gE gE cs) gen) (map (put_gE gE cs) pms) cs' (map (put_gE gE cs) ks)
    update ss (Pat nm rs gen pms cs ks)    = Pat nm (map (update ss) rs) (map (update ss) gen) (map (update ss) pms) cs (map (update ss) ks)
    specialize t (Pat nm rs gen pms cs ks) = Pat nm (map (specialize t) rs) (map (specialize t) gen) (map (specialize t) pms) cs (map (specialize t) ks)

   instance Pop Rule where
    put_gE gE cs r@(Ru Automatic antc pos expr cpu expla sgn nr pn) = Ru Automatic (error ("(Module CC_aux:) illegal call to antecedent in put_gE cs ("++showADL r++")")) pos (put_gE gE cs expr) (map (put_gE gE cs) cpu) expla (put_gE gE cs sgn) nr pn
    put_gE gE cs r@(Ru c antc pos cons cpu expla sgn nr pn)   = Ru c (put_gE gE cs antc) pos (put_gE gE cs (consequent r)) (map (put_gE gE cs) cpu) expla (put_gE gE cs sgn) nr pn
    put_gE gE cs r@(Sg p rule expla sgn nr pn signal)         = Sg p (put_gE gE cs rule) expla (put_gE gE cs sgn) nr pn signal
    put_gE gE cs r@(Gc pos m expr cpu sgn nr pn)              = Gc pos (put_gE gE cs m) (put_gE gE cs expr) (map (put_gE gE cs) cpu) (put_gE gE cs sgn) nr pn
    put_gE gE cs r@(Fr t d expr pn)                           = Fr t (put_gE gE cs d) (put_gE gE cs expr) pn
    update ss r@(Ru Automatic antc pos expr cpu expla sgn nr pn)    = Ru Automatic (error ("(Module CC_aux:) illegal call to antecedent in update ss ("++showADL r++")")) pos (update ss expr) (map (update ss) cpu) expla (update ss sgn) nr pn
    update ss r@(Ru c antc pos cons cpu expla sgn nr pn)      = Ru c (update ss antc) pos (update ss cons) (map (update ss) cpu) expla (update ss sgn) nr pn
    update ss r@(Sg p rule expla sgn nr pn signal)            = Sg p (update ss rule) expla (update ss sgn) nr pn signal
    update ss r@(Gc pos m expr cpu sgn nr pn)                 = Gc pos (update ss m) (update ss expr) (map (update ss) cpu) (update ss sgn) nr pn
    update ss r@(Fr t d expr pn)                              = Fr t (update ss d) (update ss expr) pn
    specialize t r@(Ru Automatic antc pos expr cpu expla sgn nr pn) = Ru Automatic (error ("(Module CC_aux:) illegal call to antecedent in specialize t ("++showADL r++")")) pos (specialize t expr) (map (specialize t) cpu) expla (specialize t sgn) nr pn
    specialize t r@(Ru c antc pos cons cpu expla sgn nr pn)   = Ru c (specialize t antc) pos (specialize t cons) (map (specialize t) cpu) expla (specialize t sgn) nr pn
    specialize t r@(Sg p rule expla sgn nr pn signal)         = Sg p (specialize t rule) expla (specialize t sgn) nr pn signal
    specialize t r@(Gc pos m expr cpu sgn nr pn)              = Gc pos (specialize t m) (specialize t expr) (map (specialize t) cpu) (specialize t sgn) nr pn
    specialize t' r@(Fr t d expr pn)                          = Fr t (specialize t' d) (specialize t' expr) pn

   single e x = length [m| m<-morlist e, m==x]==1


   instance Pop Expression where
    put_gE gE cs (Tm m)       = Tm (put_gE gE cs m)
    put_gE gE cs (Tc f)       = Tc (put_gE gE cs f)
    put_gE gE cs (F ts)       = F  (map (put_gE gE cs) ts)
    put_gE gE cs (Fd ts)      = Fd (map (put_gE gE cs) ts)
    put_gE gE cs (Fu fs)      = Fu (map (put_gE gE cs) fs)
    put_gE gE cs (Fi fs)      = Fi (map (put_gE gE cs) fs)
    put_gE gE cs (K0 e)       = K0 (put_gE gE cs e)
    put_gE gE cs (K1 e)       = K1 (put_gE gE cs e)
    put_gE gE cs (Cp e)       = Cp (put_gE gE cs e)

    update ss (Tm m)            = Tm (update ss m)
    update ss (Tc f)            = Tc (update ss f)
    update ss (F ts)            = F  (map (update ss) ts)
    update ss (Fd ts)           = Fd (map (update ss) ts)
    update ss (Fu fs)           = Fu (map (update ss) fs)
    update ss (Fi fs)           = Fi (map (update ss) fs)
    update ss (K0 e)            = K0 (update ss e)
    update ss (K1 e)            = K1 (update ss e)
    update ss (Cp e)            = Cp (update ss e)

    specialize t (Tm m)         = Tm (specialize t m)
    specialize t (Tc f)         = Tc (specialize t f)
    specialize t@(a,b) (F [])   = error ("(module CC_aux) specialize t@("++show a++","++show b++") (F [])")
    specialize t@(a,b) (F [t']) = F [specialize t t']
    specialize t@(a,b) (F ts)   = F ([specialize (a,target h) h]++init (tail ts)++[specialize (source l,b) l])
                                  where h=head ts; l=last ts
    specialize t@(a,b) (Fd [])   = error ("(module CC_aux) specialize t@("++show a++","++show b++") (Fd [])")
    specialize t@(a,b) (Fd [t']) = Fd [specialize t t']
    specialize t@(a,b) (Fd ts)   = Fd ([specialize (a,target h) h]++init (tail ts)++[specialize (source l,b) l])
                                   where h=head ts; l=last ts
    specialize t@(a,b) (Fu fs)  = Fu (map (specialize t) fs) 
    specialize t@(a,b) (Fi fs)  = Fi (map (specialize t) fs) 
    specialize t (K0 e)         = K0 (specialize t e)
    specialize t (K1 e)         = K1 (specialize t e)
    specialize t (Cp e)         = Cp (specialize t e)






   instance Pop Morphism where
    put_gE gE cs (Mph nm p atts sgn yin s)       = Mph nm p (map (put_gE gE cs) atts) (put_gE gE cs sgn) yin (put_gE gE cs s)
    put_gE gE cs (I atts g s yin)                = I (map (put_gE gE cs) atts) (put_gE gE cs g) (put_gE gE cs s) yin
    put_gE gE cs (V atts (a,b))                  = V (map (put_gE gE cs) atts) (put_gE gE cs a, put_gE gE cs b)
    update ss (Mph nm p atts sgn yin s)          = Mph nm p atts (update ss sgn) yin (update ss s)
    update ss (I atts g s yin)                   = I (map (update ss) atts) (update ss g) (update ss s) yin
    update ss (V atts (a,b))                     = V (map (update ss) atts) (update ss a, update ss b)
    specialize t@(a,b) (Mph nm p atts sgn yin s) = Mph nm p (if null atts then [] else if yin then [a,b] else [b,a]) t yin (specialize t s)
    specialize t@(a,b) (I atts g s yin)          = if yin then I atts b a yin else I atts a b yin
    specialize t@(a,b) (V atts (a',b'))          = V atts (a,b)

   instance Pop Declaration where
    put_gE gE cs (Sgn nm a b props prL prM prR cs' expla pos nr sig)
                                                 = Sgn nm (put_gE gE cs a) (put_gE gE cs b) props prL prM prR cs' expla pos nr sig
    put_gE gE cs (Isn g s)                       = Isn (put_gE gE cs g) (put_gE gE cs s)
    put_gE gE cs (Iscompl g s)                   = Iscompl (put_gE gE cs g) (put_gE gE cs s)
    put_gE gE cs (Vs g s)                        = Vs (put_gE gE cs g) (put_gE gE cs s)

    update ss s@(Sgn _ _ _ _ _ _ _ _ _ _ _ _)    = head ([c|c<-ss, s==c]++[s])
    update ss s                                  = s
    specialize (x,y) (Sgn nm a b props prL prM prR ls expla pos nr sig)
                                                 = Sgn nm x y props prL prM prR [[d,e]|[d,e]<-ls,d `elem` conts a, e `elem` conts b] expla pos nr sig
    specialize (x,y) sg@(Isn g s)                = if x <= y then Isn x y else error ("(module CC_aux) Fatal: specialize 7 "++show (x,y)++showHS "" s)
    specialize (x,y) sg@(Iscompl g s)            = if x <= y then Iscompl x y else error ("(module CC_aux) Fatal: specialize 7 "++show (x,y)++showHS "" s)



   isSgn (Sgn _ _ _ _ _ _ _ _ _ _ _ _) = True
   isSgn _ = False



   fEmpty (F [])  = True
   fEmpty (Fd []) = True
   fEmpty (Fu []) = True
   fEmpty (Fi []) = True
   fEmpty     _   = False

   oneMorphism (Tm _)    = True
   oneMorphism (Tc f)    = oneMorphism f
   oneMorphism (F [t])   = oneMorphism t
   oneMorphism (F ts)    = False
   oneMorphism (Fd [t])  = oneMorphism t
   oneMorphism (Fd ts)   = False
   oneMorphism (Fu [f])  = oneMorphism f
   oneMorphism (Fu fs)   = False
   oneMorphism (Fi [f])  = oneMorphism f
   oneMorphism (Fi fs)   = False
   oneMorphism (K0 e)    = oneMorphism e
   oneMorphism (K1 e)    = oneMorphism e
   oneMorphism (Cp e)    = oneMorphism e



   mkVar ex cs = mknew ex [[(toLower.head.(++"x").name) c]|c<-cs]
    where
     mknew ex [] = []
     mknew ex (x:xs) | x `elem` ex = mknew ex ((x++"'"):xs)
                     | otherwise = x: mknew (ex++[x]) xs

   class Calc a where
    limit     :: (Concept,Concept) -> a -> a
    calc      :: a -> [Declaration] -> [Paire]



  {-  e `elemSgn` s  = e `elem` contents s

   jnMph :: Morphism -> Morphism -> Morphism
   s `jnMph` t   | isIdent s = Mph nm' p' [] (sign signat) True signat
                 | isIdent t = Mph nm  p  [] (sign signat) True signat
                 | source t `order` target s = Mph (name signat) p' [] (sign signat) True signat
                 | otherwise = error ("(module CC_aux) unable to `;` (compose) nonequivalent relations "++show s++" and "++show t++".")
                 where
                  Mph nm  p  atts  sgn  yin  sg  = s
                  Mph nm' p' atts' sgn' yin' sg' = t
                  signat = (if yin then sg else flp sg) `jnSgn` (if yin then sg' else flp sg')

   jnSgn :: Declaration -> Declaration -> Declaration
   s `jnSgn` t   | isIdent s = Sgn nm' (a `lub` a') b' props' prL' prM' prR' cs' expla' pos' nr' False
                 | isIdent t = Sgn nm  a' (b `lub` b') props  prL  prM  prR  cs  expla  pos  nr  False
                 | source t `order` target s = Sgn (nm++";"++nm') a b' (h (multiplicities s) `isc` h (multiplicities t)) prL prM prR (cs `join` cs') "" posNone 0 False
                 | otherwise = error ("(module CC_aux) unable to `;` (compose) nonequivalent relations "++show s++" and "++show t++".")
                 where
                  h ps = ps>-[Sym,Asy,Trn,Rfx]
                  Sgn nm  a  b  props  prL  prM  prR  cs  expla  pos  nr  False = s
                  Sgn nm' a' b' props' prL' prM' prR' cs' expla' pos' nr' False = t -}

   instance Calc Declaration where
    limit (a,b) s@(Sgn nm a' b' props prL prM prR cs expla pos nr sig)
     | a `order` a' && b `order` b' = Sgn nm (a `lub` a') ( b `lub` b') props prL prM prR [[x,y]| [x,y]<-contents s, x `elem` conts a, y `elem` conts b] "" pos nr sig
     | otherwise = error ("(module CC_aux) Cannot limit "++show (a,b)++" with limit (Sgn nm "++show a'++" "++show b'++" props prL prM prR cs pos nr sig)")
    limit (a,b) (Isn g s)
     | g <= a && s <= b = Isn a b
     | otherwise = error ("(module CC_aux) Cannot limit "++show (a,b)++" with limit (Isn "++show g++" "++show s++")")
    limit (a,b) (Iscompl g s)
     | g <= a && s <= b = Iscompl a b
     | otherwise = error ("(module CC_aux) Cannot limit "++show (a,b)++" with limit (Iscompl "++show g++" "++show s++")")
    calc sg@(Sgn _ _ _ _ _ _ _ _ _ _ _ _) ss
     = contents (head([x|x<-ss, source x <= source sg && target x <= target sg]++
                     error ("(module CC_aux) Scope error1 :"++name sg)
                )    )
    calc sg ss = contents sg

   instance Calc Morphism where
    limit (a,b) (Mph nm pos atts sgn True s)  = Mph nm pos atts (a,b) True (limit (a,b) s)
    limit (a,b) (Mph nm pos atts sgn False s) = Mph nm pos atts (b,a) False (limit (b,a) s)
    limit (a,b) (I atts g s yin)              = if a <= b then (if yin then I atts b a yin else I atts a b yin)
                                                else error ("(module CC_aux) !Fatal error: "++show a++" <= "++show b++" expected.")
    limit (a,b) (V atts (a',b'))              = V atts (a,b)
    calc m@(Mph nm pos atts sgn yin s) ss
      = if null signs then error ("(module CC_aux) Scope error :"++showS s++" "++show (map showS ss)) else
        if length signs>1 then error ("(module CC_aux) Calculation error : ambiguous "++showS s++" in calc ("++show m++") "++show (map showS ss)) else
        if yin then contents (head signs) else map reverse (contents (head signs))
        where signs = [x|x<-ss, x == s]
    calc i ss  = contents i

   instance Calc Expression where
    limit sgn'  (Tm m)  = Tm (limit sgn' m)
    limit sgn'  (Tc f)  = Tc (limit sgn' f)
    limit sgn (F ts)    = F (lim sgn ts)
     where lim sgn  [x] = [limit sgn x]
           lim sgn   [] = []
           lim (a,b) (x:xs) = [limit (a,c) x]++lim (c,b) xs
                              where c = if null xs then target x else
                                        if target x `order` source (head xs) then target x `lub` source (head xs) else
                                        error ("(module CC_aux) Fatal: limit sgn ("++showHS "" (F ts)++") has incompatible types inside...")
    limit sgn (Fd ts)   = Fd (lim sgn ts)
     where lim sgn  [x] = [limit sgn x]
           lim sgn   [] = []
           lim (a,b) (x:xs) = [limit (a,c) x]++lim (c,b) xs
                              where c = if null xs then target x else
                                        if target x `order` source (head xs) then target x `lub` source (head xs) else
                                        error ("(module CC_aux) Fatal: limit sgn ("++showHS "" (Fd ts)++") has incompatible types inside...")
    limit sgn (Fu fs)   = Fu (map (limit sgn) fs)
    limit sgn (Fi fs)   = Fi (map (limit sgn) fs)
    limit sgn' (K0 e)   = K0 (limit sgn' e)
    limit sgn' (K1 e)   = K1 (limit sgn' e)
    limit sgn' (Cp e)   = Cp (limit sgn' e)

    calc (Tm m) ss      = calc m ss
    calc (Tc f) ss      = calc f ss
    calc (F  ts) ss     = if null ts then error ("(module CC_aux) Fatal: no terms in calc (F "++showHS "" ts++")") else
                          foldr1 join [calc t ss| t<-ts ]




    calc (Fu fs) ss     = foldr uni [] [calc f ss| f<-fs ]
    calc (Fi fs) ss     = if null fs then error ("(module CC_aux) Fatal: no factors in calc (Fi "++showHS "" fs++")") else
                          foldr1 isc  [calc f ss| f<-fs ]
    calc (K0 e) ss      = clos1 (calc e ss) `uni` [[a,a]|a <-conts (source e `lub` target e)]
    calc (K1 e) ss      = clos1 (calc e ss)
    calc (Cp e) ss      = --error ("(module CC_aux) Diagnosis:\nsource: "++show (conts (source e)) ++"\ntarget: "++show (conts (target e)))
                          [[a,b]| [a,b]<-diag [] (conts (source e)) [] (conts (target e)), not ([a,b] `elem` calc e ss)]

   fun,tot,inj,sur :: [Prop]->Bool
   fun = elem Uni
   tot = elem Tot
   inj = elem Inj
   sur = elem Sur
   automatic m = Aut `elem` multiplicities m

   applyM (Sgn nm _ _ _ prL prM prR _ _ _ _ _) d c = if null (prL++prM++prR) then d++" "++nm++" "++c else prL++(if null prL then d else unCap d)++prM++c++prR
   applyM (Isn _ _)                            d c = d++" equals "++c
   applyM (Iscompl _ _)                        d c = d++" differs from "++c
   applyM (Vs _ _)                             d c = show True




   clearG abs = rd [G pos g s| G pos g s<-abs, g/=s]







   get_tok_pos     (Tok _ _ s l f) = FilePos (f,l,s)
   get_tok_val_pos (Tok _ _ s l f) = (s,FilePos (f,l,s))



   gsym_pos :: IsParser p Token => TokenType -> String -> String -> p FilePos
   gsym_pos kind val val2 = get_tok_pos <$> pSym (Tok kind val val2 noPos "")

   gsym_val_pos :: IsParser p Token => TokenType -> String -> String -> p (String,FilePos)
   gsym_val_pos kind val val2 = get_tok_val_pos <$> pSym (Tok kind val val2 noPos "")

   pOperAny           ::  IsParser p Token => p String
   pOperAny           =   pOper    ""

   pOper_pos name     =   gsym_pos TkOp        name      name
   pKey_pos  keyword  =   gsym_pos TkKeyword   keyword   keyword
   pSpec_pos s        =   gsym_pos TkSymbol    [s]       [s]

   pOParen_pos, pString_pos, pChar_pos, pInteger8_pos, pInteger10_pos, pInteger16_pos,
      pVarid_pos, pConid_pos, pTextnm_pos, pTextln_pos, pInteger_pos
                      ::  IsParser p Token => p FilePos
   pOParen_pos        =   pSpec_pos '('

   pString_pos        =   gsym_pos TkString    ""        "?STR?"
   pChar_pos          =   gsym_pos TkChar      ""        "'chr'"
   pInteger8_pos      =   gsym_pos TkInteger8  ""        "1"
   pInteger10_pos     =   gsym_pos TkInteger10 ""        "1"
   pInteger16_pos     =   gsym_pos TkInteger16 ""        "1"
   pVarid_pos         =   gsym_pos TkVarid     ""        "?LC?"
   pConid_pos         =   gsym_pos TkConid     ""        "?UC?"
   pTextnm_pos        =   gsym_pos TkTextnm    ""        ""
   pTextln_pos        =   gsym_pos TkTextln    ""        ""
   pInteger_pos       =   pInteger10_pos

   pInteger10_val_pos, pString_val_pos, pChar_val_pos, pVarid_val_pos, pConid_val_pos,
      pInteger_val_pos
                      ::  IsParser p Token => p (String,FilePos)
   pInteger10_val_pos =   gsym_val_pos TkInteger10 ""        "1"
   pString_val_pos    =   gsym_val_pos TkString    ""        "?STR?"
   pChar_val_pos      =   gsym_val_pos TkChar      ""        "'chr'"
   pVarid_val_pos     =   gsym_val_pos TkVarid     ""        "?LC?"
   pConid_val_pos     =   gsym_val_pos TkConid     ""        "?UC?"
   pInteger_val_pos   =   pInteger10_val_pos

   pParens_pos        ::  IsParser p Token => p a -> p (FilePos,a)
   pParens_pos p      =   (,) <$> pOParen_pos <*> p <* pCParen




   mor2filename m = "Atlas"++rEncode (name m)++".html"











