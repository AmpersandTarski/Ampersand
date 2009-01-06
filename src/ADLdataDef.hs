  {-# OPTIONS -XTypeSynonymInstances #-}
  module ADLdataDef
                ( module Data.ADL
                  ,Association(..)
                  ,cptAnything,cptNothing,cptC,cptS,cptnew
                  ,Numbered(pos,nr)
                  ,ruleType,consequent,antecedent
                  ,makeMph,cpu,uncomp
                  ,inline,makeDeclaration
                ) where
   import Data.ADL             
   import UU_Scanner (Pos(Pos))
   import Strings(chain)
   import CommonClasses(Identified(name,typ)
                        , ABoolAlg(glb,lub,order)
                        , Explained(explain)
                        , Conceptual(conts)
                        , Morphics(anything)
                        )
   import Collection (Collection (rd))
   import Auxiliaries (eqClass, enumerate, sort', clos1,diag,eqCl) 
   
-- In deze module worden aan taalconstuctors van ADL de eigenschappen
-- toegekend voor de volgende classen:
--   Eq
--   Show
--   Identified
--   Association
--   Numbered
--   Ord
--   ABoolAlg



   -- Show   --WAAROM (HJ) wat is precies het doel van Show vs ShowADL ??
   -- ANTWOORD (SJ): De standaard-show is alleen bedoeld voor foutmeldingen tijdens het testen.
   --                Foutmeldingen voor de gebruiker zouden eigenlijk via een aparte show moeten,
   --                omdat je daarmee device-independence kunt bereiken voor foutmeldingen
   --                (voorlopig is de gewone show hiervoor goed genoeg).
   --                ShowADL is bedoeld om ADL source code te genereren.
   --                Ik kan me voorstellen dat er op den duur een showADL ontstaat die zowel
   --                inline ADL kan genereren alsook geprettyprinte ADL.


   class Association a where
     source, target :: a -> Concept
     sign           :: a -> MorphType
     sign x = (source x,target x) 

   instance Association MorphType where
     source (src, tgt) = src
     target (src, tgt) = tgt
     

   class Numbered a where
    nr :: a->Int
    pos :: a->FilePos
    nr x = nr (pos x)



-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Architecture                  ***
-- \***********************************************************************
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Context                       ***
-- \***********************************************************************
   instance Identified Context where
    name ctx = ctxnm ctx
    typ ctx = "Context_"
    
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Pattern                       ***
-- \***********************************************************************
   instance Show Pattern
   instance Identified Pattern where
    name pat = ptnm pat
    typ pat = "Pattern_"

   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Rule                          ***
-- \***********************************************************************
   instance Eq Rule
   instance Show Rule
   instance Identified Rule where
    name r = "Rule"++show (runum r)
    typ r = "Rule_"
   instance Association Rule where
    source r | ruleType r=='A' = fst (sign r)
             | otherwise       = fst (sign r)
    target r | ruleType r=='A' = snd (sign r)
             | otherwise       = snd (sign r)
    sign r   | ruleType r=='A' = sign (consequent r)
             | otherwise       = if sign (antecedent r) `order` sign (consequent r) then sign (antecedent r) `lub` sign (consequent r) else
                                 error ("(module CC_aux) Fatal: incompatible signs in "++misbruiktShowHS "" r)

   instance Numbered Rule where
    pos (Ru _ _ p _ _ _ _ _ _) = p
    pos (Sg p _ _ _ _ _ _)     = p
    pos (Gc p _ _ _ _ _ _)     = p
    pos r = posNone
    nr  (Ru _ _ _ _ _ _ _ n _) = n
    nr  (Sg _ _ _ _ n _ _)     = n
    nr  (Gc _ _ _ _ _ n _)     = n
    nr  r = 0


   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: KeyDef                        ***
-- \***********************************************************************
   instance Eq KeyDef
   instance Show KeyDef
   instance Identified KeyDef where
    name kd = kdlbl kd
    typ kd = "KeyDef_"

   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Population                    ***
-- \***********************************************************************
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ObjectDef                     ***
-- \***********************************************************************
   instance Eq ObjectDef
   instance Show ObjectDef
   instance Identified ObjectDef where
    name obj = objnm obj
    typ obj = "ObjectDef_"
   instance Numbered ObjectDef where
    pos obj = objpos obj


   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Expression                    ***
-- \***********************************************************************
   instance Eq Expression where
    F  ts == F  ts' = ts==ts'
    Fd ts == Fd ts' = ts==ts'
    Fu fs == Fu fs' = rd fs==rd fs'
    Fi fs == Fi fs' = rd fs==rd fs'
    Cp e  == Cp e'  = e==e'
    K0 e  == K0 e'  = e==e'
    K1 e  == K1 e'  = e==e'
    Tm m  == Tm m'  = m==m'
    Tc e  == e'     = e==e'
    e     == Tc e'  = e==e'
    _     == _      = False

   instance Show Expression where
    showsPrec p e  = showString (showExpr ("\\/", "/\\", "!", ";", "*", "+", "-", "(", ")") e)
      where
       showExpr (union,inter,rAdd,rMul,clos0,clos1,compl,lpar,rpar) e = showchar 0 e
         where
          wrap i j str = if i<=j then str else lpar++str++rpar
          showchar i (Tm m)  = name m++if inline m then "" else "~"
          showchar i (Fu []) = "-V"
          showchar i (Fu fs) = wrap i 4 (chain union [showchar 4 f| f<-fs])
          showchar i (Fi []) = "V"
          showchar i (Fi fs) = wrap i 5 (chain inter [showchar 5 f| f<-fs])
          showchar i (Fd []) = "-I"
          showchar i (Fd ts) = wrap i 6 (chain rAdd [showchar 6 t| t<-ts])
          showchar i (F [])  = "I"
          showchar i (F ts)  = wrap i 7 (chain rMul [showchar 7 t| t<-ts])
          showchar i (K0 e)  = showchar 8 e++clos0
          showchar i (K1 e)  = showchar 8 e++clos1
          showchar i (Cp e)  = compl++showchar 8 e
          showchar i (Tc f)  = showchar i f
    
   instance Association Expression where

    source (Tm m)          = source m
    source (Tc f)          = source f
    source (F  [])         = Anything -- error ("(module CC_aux) Fatal: source (F [])")
    source (F  ts)         = source (head ts)
    source (Fd [])         = Anything -- error ("(module CC_aux) Fatal: source (Fd [])")
    source (Fd ts)         = source (head ts)
    source (Fu fs)         = if length (eqClass order (map source fs))==1 then minimum (map source fs)
                             else Anything -- error ("(module CC_aux) Fatal: source ("++showHS "" (Fu fs)++")")
    source (Fi fs)         = if length (eqClass order (map source fs))==1 then maximum (map source fs)
                             else Anything -- error ("(module CC_aux) Fatal: source ("++showHS "" (Fi fs)++")")
    source (K0 e)          = source e
    source (K1 e)          = source e
    source (Cp e)          = source e

    target (Tm m)          = target m
    target (Tc f)          = target f
    target (F  [])         = Anything -- error ("(module CC_aux) Fatal: target (F [])")
    target (F  ts)         = target (last ts)
    target (Fd [])         = Anything -- error ("(module CC_aux) Fatal: target (Fd [])")
    target (Fd ts)         = target (last ts)
    target (Fu fs)         = if length (eqClass order (map target fs))==1 then minimum (map target fs)
                             else Anything
    target (Fi fs)         = if length (eqClass order (map target fs))==1 then maximum (map target fs)
                             else Anything
    target (K0 e)          = target e
    target (K1 e)          = target e
    target (Cp e)          = target e

    sign (Tm m)            = sign m
    sign (Tc f)            = sign f
    sign (F ts)            = if null ts 
                              then error ("(module CC_aux) Fatal: no terms in sign (F "++misbruiktShowHS "" ts++")")
                              else foldr1 jnSign (map sign ts)
                              where (s,t) `jnSign` (s',t') = (s,t')
    sign (Fd ts)           = if null ts 
                              then error ("(module CC_aux) Fatal: no terms in sign (Fd "++misbruiktShowHS "" ts++")")
                              else foldr1 jnSign (map sign ts)
                              where (s,t) `jnSign` (s',t') = (s,t')
    sign (Fu fs)           = if length (eqClass order (map sign fs))>1 then error ("(module CC_aux) Fatal: sign (Fu fs) not defined\nwith map sign fs="++show (map sign fs)) else
                             if null fs then (cptAnything, cptAnything) else
                             foldr1 lub (map sign fs)
    sign (Fi fs)           = if length (eqClass order (map sign fs))>1 then error ("(module CC_aux) Fatal: sign (Fi fs) not defined\nwith map sign fs="++show (map sign fs)) else
                             if null fs then (cptAnything, cptAnything) else
                             foldr1 lub (map sign fs)
    sign (K0 e)            = sign e
    sign (K1 e)            = sign e
    sign (Cp e)            = sign e

   instance Numbered Expression where
    pos (Tm m)  = pos m
    pos (Tc f)  = pos f
    pos (F ts)  = if not (null ts) then pos (head ts) else error "(module ADLdataDef) !!Software error 813. Please submit a complete bug report to your dealer"
    pos (Fd ts) = if not (null ts) then pos (head ts) else error "(module ADLdataDef) !!Software error 814. Please submit a complete bug report to your dealer"
    pos (Fu fs) = if not (null fs) then pos (head fs) else error "(module ADLdataDef) !!Software error 815. Please submit a complete bug report to your dealer"
    pos (Fi fs) = if not (null fs) then pos (head fs) else error "(module ADLdataDef) !!Software error 816. Please submit a complete bug report to your dealer"
    pos (K0 e)  = pos e
    pos (K1 e)  = pos e
    pos (Cp e)  = pos e

   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Gen                           ***
-- \***********************************************************************
   instance Eq Gen
   instance Show Gen where
    -- This show is used in error messages. It should therefore not display the term's type
    showsPrec p (G pos g s) = showString ("GEN "++show s++" ISA "++show g)
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Morphism                      ***
-- \***********************************************************************
   instance Eq Morphism where
 --   m == m' = name m==name m' && source m==source m' && target m==target m' && yin==yin'
    Mph nm _ _ (a,b) yin _ == Mph nm' _ _ (a',b') yin' _ = nm==nm' && yin==yin' && a==a' && b==b'
    Mph nm _ _ (a,b) yin _ == _ = False
    I _ g s yin            == I _ g' s' yin'             =            if yin==yin' then g==g' && s==s' else g==s' && s==g'
    I _ g s yin            == _ = False
    V _ (a,b)              == V _ (a',b')                = a==a' && b==b'
    V _ (a,b)              == _ = False
    Mp1 s c                == Mp1 s' c'                  = s==s' && c==c'
    Mp1 s c                == _ = False

   instance Show Morphism where
    showsPrec p (Mph nm pos  []  sgn yin m) = showString (nm  {- ++"("++show a++"*"++show b++")" where (a,b)=sgn -} ++if yin then "" else "~")
    showsPrec p (Mph nm pos atts sgn yin m) = showString (nm  {- ++"["++chain "*" (map name (rd atts))++"]" -}      ++if yin then "" else "~")
    showsPrec p (I atts g s yin)            = showString ("I"++ (if null atts then {- ++"["++name g, (if s/=g then ","++name s else "")++"]" -} "" else show atts))
    showsPrec p (V atts (a,b))              = showString ("V"++ (if null atts then "" else show atts))

   instance Identified Morphism where
--    name (Mph nm _ _ _ _ _) = nm    -- WAAROM (HJ)? zou name (makeDeclaration (Mph nm _ _ _ _ _)) het ook niet doen? Dan is de definitie op de volgende regel voldoende. 
                                    -- ANTWOORD (SJ): Tja, dat klopt. Maar het is ook wel eens lekker om gewoon te kunnen lezen hoe het is, in plaats van continu te moeten dereferencen als je code leest...
                                    -- conclusie: Toch maar verwijderd, want dat is weer een onnodige afhankelijkheid van de datastructuur minder...
    name i = name (makeDeclaration i)
    typ mph = "Morphism_"

   instance Association Morphism where
--    source (Mph nm pos atts (a,b) _ s) = a
--    source (I atts g s yin)            = if yin then s else g
--    source (V atts (a,b))              = a
--    source (Mp1 _ s) = s
--    target (Mph nm pos atts (a,b) _ s) = b
--    target (I atts g s yin)            = if yin then g else s
--    target (V atts (a,b))              = b
--    target (Mp1 _ t) = t
    sign   (Mph nm pos atts (a,b) _ s) = (a,b)
    sign   (I atts g s yin)            = if yin then (s,g) else (g,s)
    sign   (V atts (a,b))              = (a,b)
    sign   (Mp1 _ s) = (s,s)
    source m = source (sign m)
    target m = target (sign m)
   instance Numbered Morphism where
    pos (Mph _ p _ _ _ _) = p
    pos m                 = posNone
    nr m = nr (makeDeclaration m)

    
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Declaration                   ***
-- \***********************************************************************
   instance Eq Declaration where
      d == d' = name d==name d' && decsrc d==decsrc d' && dectgt d==dectgt d'
   instance Show Declaration where
    showsPrec p (Sgn nm a b props prL prM prR cs expla _ _ False)
     = showString (chain " " ([nm,"::",name a,"*",name b,show props,"PRAGMA",show prL,show prM,show prR]++if null expla then [] else ["EXPLANATION",show expla]))
    showsPrec p (Sgn nm a b props prL prM prR cs expla _ _ True)
     = showString (chain " " ["SIGNAL",nm,"ON (",name a,"*",name b,")"])
    showsPrec p _
     = showString ""
   instance Identified Declaration where
    name (Sgn nm _ _ _ _ _ _ _ _ _ _ _) = nm
    name (Isn _ _)                      = "I"
    name (Iscompl _ _)                  = "-I"
    name (Vs _ _)                       = "V"
    typ d = "Declaration_"

   instance Association Declaration where
--    source (Sgn _ a b _ _ _ _ _ _ _ _ _)         = a
--    source (Isn g s)                             = s
--    source (Iscompl g s)                         = s
--    source (Vs a b)                              = a
--    target (Sgn _ a b _ _ _ _ _ _ _ _ _)         = b
--    target (Isn g s)                             = g
--    target (Iscompl g s)                         = g
--    target (Vs a b)                              = b
--    sign   (Sgn _ a b _ _ _ _ _ _ _ _ _)         = (a,b)
--    sign   (Isn g s)                             = (s,g)
--    sign   (Iscompl g s)                         = (s,g)
--    sign   (Vs g s)                              = (s,g)
--  Bovenstaande is wel érg omslachtig. Ik heb het vervangen door:
      source d = decsrc d
      target d = dectgt d
    --sign is vanzelf al geregeld...

   instance Numbered Declaration where
    pos (Sgn _ _ _ _ _ _ _ _ _ p _ _) = p
    pos d                             = posNone
    nr (Sgn _ _ _ _ _ _ _ _ _ _ n _)  = n
    nr d                              = 0


   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ConceptDef                    ***
-- \***********************************************************************
   instance Eq ConceptDef where
    cd == cd' = cdnm cd == cdnm cd
   instance Show ConceptDef    
   instance Identified ConceptDef where
    name cd = cdnm cd
    typ cd = "ConceptDef_"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Concept                       ***
-- \***********************************************************************
   instance Eq Concept where
    C a _ _ == C b _ _ = a==b
    S == S = True
    Anything == Anything = True
    NOthing == NOthing = True
    _ == _ = False
   instance Show Concept where
    showsPrec p c = showString (name c)
   instance Identified Concept where
    name (C {cptnm = nm}) = nm
    name S = "ONE"
    name Anything   = "Anything"
    name NOthing    = "NOthing"
    typ cpt = "Concept_"
   instance Association Concept where
    source c = c
    target c = c
   instance Ord Concept where
    NOthing <= b  = False
    a <= NOthing  = True
    Anything <= b = True
    a <= Anything = False
    a@(C _ gE _) <= b = a `gE` b
    a <= b = a==b
    --TODO?: ORD is niet gedefinieerd op Singelton.... Is dat erg?


   instance ABoolAlg Concept where
    glb a b | b <= a = b
            | a <= b = a
            | otherwise = error ("(module ADLdataDef) Fatal: (C) glb undefined: a="++show a++", b="++show b)
    lub a b | a <= b = b
            | b <= a = a
            | otherwise = error ("(module ADLdataDef) Fatal: (C) lub undefined: a="++show a++", b="++show b)


   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: AutType                       ***
-- \***********************************************************************
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Prop                          ***
-- \***********************************************************************
   instance Show Prop where
    showsPrec p Uni = showString "UNI"
    showsPrec p Inj = showString "INJ"
    showsPrec p Sur = showString "SUR"
    showsPrec p Tot = showString "TOT"
    showsPrec p Sym = showString "SYM"
    showsPrec p Asy = showString "ASY"
    showsPrec p Trn = showString "TRN"
    showsPrec p Rfx = showString "RFX"
    showsPrec p Aut = showString "AUT"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FilePos                       ***
-- \***********************************************************************
   instance Numbered FilePos where
    nr (FilePos (fn,Pos l c,sym)) = l
    pos p = p




--  /---------------------Hieronder moet nog verder worden bekeken of het hier wel thuishoort.


   ruleType    (Ru c _ _ _ _ _ _ _ _) = c
   ruleType    (Sg _ rule _ _ _ _ _)  = ruleType rule
   ruleType    (Gc _ _ _ _ _ _ _)     = 'g'
   ruleType    (Fr _ _ _ _)           = 'f'
   antecedent r@(Ru 'A' _ _ _ _ _ _ _ _) = error ("(Module ADLdef:) illegal call to antecedent of rule "++show r)
   antecedent  (Ru _ a _ _ _ _ _ _ _) = a
   antecedent  (Sg _ rule _ _ _ _ _)  = antecedent rule
   antecedent  (Gc _ d _ _ _ _ _)     = Tm d
   antecedent  (Fr _ _ e _)           = e
   consequent  (Ru _ _ _ c _ _ _ _ _) = c
   consequent  (Sg _ rule _ _ _ _ _)  = consequent rule
   consequent  (Gc _ _ e _ _ _ _)     = e
   consequent  (Fr _ d _ _)           = Tm (makeMph d)
   cpu         (Ru _ _ _ _ c _ _ _ _) = c
   cpu         (Sg _ _ _ _ _ _ _)     = [] -- TODO nakijken: Moet dit niet de signaalrelatie zijn?
   cpu         (Gc _ _ _ c _ _ _)     = c
   cpu         (Fr _ d _ _)           = [Tm (makeMph d)]
   patternName (Ru _ _ _ _ _ _ _ _ p) = p
   patternName (Sg _ _ _ _ _ p _)     = p
   patternName (Gc _ _ _ _ _ _ p)     = p
   patternName (Fr _ _ _ p)           = p
   uncomp (Ru a b c d e f (g,g') h i) = Ru a b c d [] f (g,g') h i
   uncomp (Gc a b c d e f g)          = Gc a b c [] e f g
   uncomp s                           = s

   makeMph :: Declaration -> Morphism
   makeMph d = Mph (name d) (ADLdataDef.pos d) [] (sign d) True d

   inline::Morphism -> Bool
   inline (Mph _ _ _ _ yin _) = yin
   inline (I _ _ _ _ )        = True
   inline (V _ _)             = True
   inline (Mp1 _ _)           = True


   makeDeclaration :: Morphism -> Declaration
   makeDeclaration (Mph _ _ _ _ _ s) = s
   makeDeclaration (I atts g s yin)  = Isn g s
   makeDeclaration (V atts (a,b))    = Vs a b
   makeDeclaration (Mp1 s c)         = Isn c c

   misbruiktShowHS indent e = show e

   cptC nm gE os = C nm gE os  -- constructor
   cptS = S                    -- constructor
   cptAnything = Anything      -- constructor
   cptNothing = NOthing        -- constructor
   cptnew nm = cptC nm (==) []

