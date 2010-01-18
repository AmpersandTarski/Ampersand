{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS -XFlexibleContexts #-}
  module CC_aux ( pKey_pos
                , pString_val_pos
                , pVarid_val_pos, pConid_val_pos
                , Pop(..)
                , pMeaning
                , anything, shSigns , gEtabG
                , conts , cod, clearG, dom
                , showFullRelName
   ) where
   import UU_Scanner
   import UU_Parsing
   import CommonClasses ( Identified(..)
                        , Conceptual(..)
                        , Morphics(..)
                        )
   import Collection   (Collection (..))
   import Strings      (chain,commaEng)
   import Auxiliaries  (rEncode)
   import Adl
   import ShowADL
--   import Adl.Pair
--   import Adl.Concept(Sign)

--   objectOfConcept :: Context -> Concept -> Maybe ObjectDef
--   objectOfConcept context cpt = if length os == 0 then Nothing else Just (head os)
--     where os = [o|o<-attributes context,concept o == cpt]


--   gEtable :: [Concept] -> String
--   gEtable cs
--    = chain "\n" ([f l " "     ++"  "++chain " " (map (f 6.name) cs)]++
--                  [f l (name c')++" |"++chain "|"[f 6 (show (c <= c'))|c<-cs]| c'<-cs])
--      where l = maximum (map (length.name) cs); f n str = take n (str++forever ' ')
--            forever c = c:forever c
--
   gEtabG :: ditDingWordtNietGebruikt -> [Concept] -> String
   gEtabG _ cs
    = chain "\n" ([f l " "     ++"  "++chain " " (map (f 6.name) cs)]++
                  [f l (name c')++" |"++chain "|"[f 6 (show (c <= c'))|c<-cs]| c'<-cs])
      where l = maximum (map (length.name) cs); f n str = take n (str++forever ' ')
            forever c = c:forever c


   dom :: Declaration -> [String]
   dom s = rd [srcPaire l| l<-contents s]
   cod :: Declaration -> [String]
   cod s = rd [trgPaire l| l<-contents s]

   pMeaning :: Prop -> String
   pMeaning Uni   = "univalent"
   pMeaning Inj   = "injective"
   pMeaning Sur   = "surjective"
   pMeaning Tot   = "total"
   pMeaning Sym   = "symmetric"
   pMeaning Asy   = "antisymmetric"
   pMeaning Trn   = "transitive"
   pMeaning Rfx   = "reflexive"

   showFullRelName :: Declaration -> String
   showFullRelName decl = rEncode (name decl++name (source decl)++name (target decl))

   shSigns :: [(Concept,Concept)] -> String
   shSigns [(a,b)] = "["++show a++"*"++show b++"]"
   shSigns ss = commaEng "or" ["["++show a++"*"++show b++"]"|(a,b)<-ss]

{- obsolete?
   makeConceptSpace :: ditwordtnietgebruikt -> [Morphism] -> Concepts    --WAAROM is deze definitie goed?
   makeConceptSpace _ morphisms
    = [ upd (fst (head raw)) (sord (concat (map snd raw)))
      | raw <- eqCl fst [(c,os)| (Mph{mphdcl=d@Sgn{}}) <- morphisms
                               , (c,os) <- [(source d,dom d),(target d,cod d)]
                        ]
      ] where
         upd c os = case c of
                       C{} -> c{cptos=os}
                       _   -> c
-}

   class Pop a where
    put_gE     :: GenR -> Concepts  -> a -> a  -- attaches the gE function to all concepts, to allow Ord Concept. Effect: c<=d is defined for concepts c and d. It means that concept c is a generalization of concept d.
{- obsolete?
    specialize :: (Concept,Concept) -> a -> a
-}
    update     :: [Declaration] -> a -> a
    update _ c = c

   instance Pop Concept where
    put_gE gE cs c = h (head ([c'|c'<-cs, c==c']++[c]))
            where h x = case x of
                         C{} ->  x{cptgE = gE}
                         _   ->  x
{- obsolete?
    specialize (a,b) c = if length (eqClass order [a,b,c])>1 then error ("!Fatal (module CC_aux 96): specialize 1 ("++show a++","++show b++") "++show c) else
                         (a `glb` b) `lub` c
-}

   instance Pop KeyDef where
    put_gE gE cs kdef = kdef{ kdcpt = put_gE gE cs (kdcpt kdef)
                            , kdats = [put_gE gE cs a| a<-kdats kdef]
                            }
    update ss kdef    = kdef{ kdcpt = update ss (kdcpt kdef)
                            , kdats = [update ss    a| a<-kdats kdef]
                            }
{- obsolete?
    specialize t kdef = kdef{ kdcpt = specialize t (kdcpt kdef)
                            , kdats = [specialize t a| a<-kdats kdef]
                            }
-}


   instance Pop ObjectDef where
    put_gE gE cs obj = obj { objctx = put_gE gE cs (objctx obj)
                           , objats = [put_gE gE cs a| a<-objats obj]
                           }
    update ss    obj = obj { objctx = update ss    (objctx obj)
                           , objats = [update ss    a| a<-objats obj]
                           }
{- obsolete?
    specialize t obj = obj { objctx = specialize t (objctx obj)
                           , objats = [specialize t a| a<-objats obj]
                           }
-}


   instance (Pop a,Pop b) => Pop (a,b) where
    put_gE gE cs (x,y) = (put_gE gE cs x, put_gE gE cs y)
    update ss    (x,y) = (update ss    x, update ss    y)
{- obsolete?
    specialize t (x,y) = (specialize t x, specialize t y)
-}


   instance Pop Gen where
    put_gE gE cs gen = gen { gengen = put_gE gE cs (gengen gen)
                           , genspc = put_gE gE cs (genspc gen)
                           }
    update ss    gen = gen { gengen = update ss    (gengen gen)
                           , genspc = update ss    (genspc gen)
                           }
{- obsolete?
    specialize t gen = gen { gengen = specialize t (gengen gen)
                           , genspc = specialize t (genspc gen)
                           }
-}


   instance Pop Context where
      put_gE gE cs context
                  = context { ctxpats = map (put_gE gE cs) (ctxpats context)
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

{- obsolete?
      specialize t context
                  = context { ctxpats = map (specialize t) (ctxpats context)
                            , ctxrs   = map (specialize t) (ctxrs context)
                            , ctxds   = map (specialize t) (ctxds context)
                            , ctxks   = map (specialize t) (ctxks context)
                            , ctxos   = map (specialize t) (ctxos context)
                            }
-}

   instance Pop Pattern where
    put_gE gE cs pat = pat { ptrls = map (put_gE gE cs) (ptrls pat)
                           , ptgns = map (put_gE gE cs) (ptgns pat)
                           , ptdcs = map (put_gE gE cs) (ptdcs pat)
                           , ptkds = map (put_gE gE cs) (ptkds pat)
                           }
    update ss    pat = pat { ptrls = map (update ss) (ptrls pat)
                           , ptgns = map (update ss) (ptgns pat)
                           , ptdcs = map (update ss) (ptdcs pat)
                           , ptkds = map (update ss) (ptkds pat)
                           }
{- obsolete?
    specialize t pat = pat { ptrls = map (specialize t) (ptrls pat)
                           , ptgns = map (specialize t) (ptgns pat)
                           , ptdcs = map (specialize t) (ptdcs pat)
                           , ptkds = map (specialize t) (ptkds pat)
                           }
-}

   
   instance Pop Rule where
    put_gE gE cs rule 
       = case rule of
          Ru{} -> rule{ rrant = if rrsrt rule == Truth 
                                 then error ("!Fatal (module CC_aux 187): illegal call to antecedent in put_gE cs ("++showADL rule++")")
                                 else put_gE gE cs (rrant rule)
                      , rrcon = put_gE gE cs (rrcon rule)
                      , rrtyp = put_gE gE cs (rrtyp rule)
                      }
    update ss rule
       = case rule of 
          Ru{} -> rule{ rrant = if rrsrt rule == Truth 
                                 then error ("!Fatal (module CC_aux 201): illegal call to antecedent in update ss ("++showADL rule++")")
                                 else update ss (rrant rule)
                      , rrcon = update ss (rrcon rule)
                      , rrtyp = update ss (rrtyp rule)
                      }
{- obsolete?
    specialize t rule
       = case rule of 
          Ru{} -> rule{ rrant = if rrsrt rule == Truth 
                                 then error ("!Fatal (module CC_aux 215): illegal call to antecedent in specialize t ("++showADL rule++")")
                                 else specialize t (rrant rule)
                      , rrcon = specialize t (rrcon rule)
                      , rrtyp = specialize t (rrtyp rule)
                      }
-}

   instance Pop Expression where
    put_gE gE cs (Tm mph)     = Tm (put_gE gE cs mph)
    put_gE gE cs (Tc f)       = Tc (put_gE gE cs f)
    put_gE gE cs (F ts)       = F  (map (put_gE gE cs) ts)
    put_gE gE cs (Fd ts)      = Fd (map (put_gE gE cs) ts)
    put_gE gE cs (Fu fs)      = Fu (map (put_gE gE cs) fs)
    put_gE gE cs (Fi fs)      = Fi (map (put_gE gE cs) fs)
    put_gE gE cs (K0 e')      = K0 (put_gE gE cs e')
    put_gE gE cs (K1 e')      = K1 (put_gE gE cs e')
    put_gE gE cs (Cp e')      = Cp (put_gE gE cs e')

    update ss (Tm mph)          = Tm (update ss mph)
    update ss (Tc f)            = Tc (update ss f)
    update ss (F ts)            = F  (map (update ss) ts)
    update ss (Fd ts)           = Fd (map (update ss) ts)
    update ss (Fu fs)           = Fu (map (update ss) fs)
    update ss (Fi fs)           = Fi (map (update ss) fs)
    update ss (K0 e')           = K0 (update ss e')
    update ss (K1 e')           = K1 (update ss e')
    update ss (Cp e')           = Cp (update ss e')

{- obsolete?
    specialize t (Tm mph)       = Tm (specialize t mph)
    specialize t (Tc f)         = Tc (specialize t f)
    specialize (a,b) (F [])     = error ("!Fatal (module CC_aux 251): specialize ("++show a++","++show b++") (F [])")
    specialize (a,b) (F [t])    = F [specialize (a,b) t]
    specialize (a,b) (F ts)     = F ([specialize (a,target h) h]++init (tail ts)++[specialize (source l,b) l])
                                  where h=head ts; l=last ts
    specialize (a,b) (Fd [])    = error ("!Fatal (module CC_aux 255): specialize t@("++show a++","++show b++") (Fd [])")
    specialize (a,b) (Fd [t])   = Fd [specialize (a,b) t]
    specialize (a,b) (Fd ts)    = Fd ([specialize (a,target h) h]++init (tail ts)++[specialize (source l,b) l])
                                   where h=head ts; l=last ts
    specialize t (Fu fs)        = Fu (map (specialize t) fs) 
    specialize t (Fi fs)        = Fi (map (specialize t) fs) 
    specialize t (K0 e')        = K0 (specialize t e')
    specialize t (K1 e')        = K1 (specialize t e')
    specialize t (Cp e')        = Cp (specialize t e')
-}






   instance Pop Morphism where
    put_gE gE cs mph
       = case mph of
          Mph{} -> mph{ mphats = map (put_gE gE cs) (mphats mph)
                      , mphtyp = (put_gE gE cs) (mphtyp mph)
                      , mphdcl = (put_gE gE cs) (mphdcl mph)
                      }
          I{}   -> mph{ mphats = map (put_gE gE cs) (mphats mph)
                      , mphgen = (put_gE gE cs) (mphgen mph)
                      , mphspc = (put_gE gE cs) (mphspc mph)
                      }
          V{}   -> mph{ mphats = map (put_gE gE cs) (mphats mph)
                      , mphtyp = (put_gE gE cs) (mphtyp mph)
                      }
          Mp1{} -> error ("!Fatal (module CC_aux 265). Consult your dealer!")
    update ss mph
       = case mph of
          Mph{} -> mph{ mphats = map (update ss) (mphats mph)   -- FOUT WAAROM? Stef, ik heb hier een aanpassing gedaan ten opzichte van hoe het was (zie onder) (`atts` werd niet geprocessed..) Graag check, dubbel check. Ik denk dat ik hiermee een bug heb verholpen...
                      , mphtyp = (update ss) (mphtyp mph)
                      , mphdcl = (update ss) (mphdcl mph)
                      }
          I{}   -> mph{ mphats = map (update ss) (mphats mph)
                      , mphgen = (update ss) (mphgen mph)
                      , mphspc = (update ss) (mphspc mph)
                      }
          V{}   -> mph{ mphats = map (update ss) (mphats mph)
                      , mphtyp = (update ss) (mphtyp mph)
                      }
          Mp1{} -> error ("!Fatal (module CC_aux 279). Consult your dealer!")

{- obsolete?
-- De volgende functie specialiseert het type van mph naar t.
-- dit veronderstelt dat t<=mphtyp mph, ofschoon er geen noodzaak is om deze preconditie vooraf te checken.
    specialize t@(a,b) mph
       = case mph of
          Mph{} -> mph{ mphats = if null (mphats mph) then [] else if mphyin mph then [a,b] else [b,a]  --TODO DAAROM?? Dit is correct. atts zet het type van de onderliggende declaratie vast, waar de typechecker (bijv. door specialisatie) niet van af mag wijken. Zie ook de uitleg in de data definitie van Morphism.
                      , mphtyp = t
--                       , mphdcl = (specialize t) (mphdcl mph)    -- Dit was fout, neem ik aan. De gedeclareerde relatie verandert natuurlijk niet wanneer een morphisme wordt gespecialiseerd.... (SJ)
                      }
          I{}   -> mph{ mphgen = if (inline mph) then b else a    -- TODO FOUT? Stef, WAAROM? Ik heb her en der mphyin vervangen door inline. Dat is beter, want generieker. Maar hier blijkt dat voor I ook de yin van belang is. Dat is niet overal consequent doorgevoerd. Needs rethinking. Als nodig bij I, dan code aan passen. anders ook, dan moet yin verdwijnen bij I.
                      , mphspc = if (inline mph) then a else b
                      }
          V{}   -> mph{ mphtyp = t
                      }
          Mp1{} -> error ("!Fatal (module CC_aux 296). Consult your dealer!")
-- was:
--    specialize t@(a,b) (Mph nm p atts sgn yin s) = Mph nm p (if null atts then [] else if yin then [a,b] else [b,a]) t yin (specialize t s)
--    specialize t@(a,b) (I atts g s yin)          = if yin then I atts b a yin else I atts a b yin
--    specialize t@(a,b) (V atts (a',b'))          = V atts (a,b)
-}

   instance Pop Declaration where
    put_gE gE cs decl =
     case (source decl,target decl) of
      (C{},C{}) -> case decl of
                    Sgn{}    -> decl{ desrc = put_gE gE cs (desrc decl)
                                    , detrg = put_gE gE cs (detrg decl)
                                    }
                    Isn{}    -> decl{ degen = put_gE gE cs (degen decl)
                                    , despc = put_gE gE cs (despc decl)
                                    }
                    Iscompl{}-> decl{ degen = put_gE gE cs (degen decl)
                                    , despc = put_gE gE cs (despc decl)
                                    }
                    Vs{}     -> decl{ degen = put_gE gE cs (degen decl)
                                    , despc = put_gE gE cs (despc decl)
                                    }
      ( _ , _ ) -> error ("!Fatal (module CC_aux 315): calling put_gE gE cs ("++show decl++") with an argument that is not C{}, with cs="++show cs++".")

    update ss decl =
     case (source decl,target decl) of
      (C{},C{}) -> case decl of
                    Sgn{}    -> head ([c|c<-ss, decl==c]++[decl])
                    Isn{}    -> decl
                    Iscompl{}-> decl
                    Vs{}     -> decl
      ( _ , _ ) -> error ("!Fatal (module CC_aux 324): calling update ss ("++show decl++") with an argument that is not C{}, with ss="++show ss++".")

{- obsolete?
    specialize (x@C{},y@C{}) decl
       = case decl of
          Sgn{}    -> decl{ desrc = x
                          , detrg = y
                          , decpopu = [p|p<-(decpopu decl),(srcPaire p) `elem` conts (desrc decl), trgPaire p `elem` conts (detrg decl)]
                          }
          Isn{}    -> if x <= y 
                       then decl{degen = x
                                ,despc = y
                                }
                       else error ("!Fatal (module CC_aux 336): specialize "++show (x,y)++show (despc decl))
          Iscompl{}-> if x <= y 
                       then decl{degen = x
                                ,despc = y
                                }
                       else error ("!Fatal (module CC_aux 341): specialize "++show (x,y)++show (despc decl))
          Vs{}     -> error ("!Fatal (module CC_aux 342). Consult your dealer!") 
    specialize (x,y) decl
      = error ("!Fatal (module CC_aux 344): calling specialize ("++show x++","++show y++") with an argument that is not C{}, in declaration "++show decl++".")
-}



--   oneMorphism :: Expression -> Bool
--   oneMorphism (Tm _)    = True
--   oneMorphism (Tc f)    = oneMorphism f
--   oneMorphism (F [t])   = oneMorphism t
--   oneMorphism (F ts)    = False
--   oneMorphism (Fd [t])  = oneMorphism t
--   oneMorphism (Fd ts)   = False
--   oneMorphism (Fu [f])  = oneMorphism f
--   oneMorphism (Fu fs)   = False
--   oneMorphism (Fi [f])  = oneMorphism f
--   oneMorphism (Fi fs)   = False
--   oneMorphism (K0 e)    = oneMorphism e
--   oneMorphism (K1 e)    = oneMorphism e
--   oneMorphism (Cp e)    = oneMorphism e

   clearG :: [Gen] -> [Gen]
   clearG gs = rd [g| g<-gs, gengen g/=genspc g, null [genspc g1|g1<-gs>-[g], gengen g==gengen g1, g2<-gs>-[g], genspc g==genspc g2, genspc g1==gengen g2]]



   get_tok_pos :: Token -> FilePos
   get_tok_pos     (Tok _ _ s l f) = FilePos (f,l,s)
   get_tok_val_pos :: Token -> (String, FilePos)
   get_tok_val_pos (Tok _ _ s l f) = (s,FilePos (f,l,s))



   gsym_pos :: IsParser p Token => TokenType -> String -> String -> p FilePos
   gsym_pos kind val' val2' = get_tok_pos <$> pSym (Tok kind val' val2' noPos "")

   gsym_val_pos :: IsParser p Token => TokenType -> String -> String -> p (String,FilePos)
   gsym_val_pos kind val' val2' = get_tok_val_pos <$> pSym (Tok kind val' val2' noPos "")

--   pOperAny           ::  IsParser p Token => p String
--   pOperAny           =   pOper    ""

--   pOper_pos :: String -> Parser Token FilePos
--   pOper_pos n     =   gsym_pos TkOp        n      n
   pKey_pos :: String -> Parser Token FilePos
   pKey_pos  keyword  =   gsym_pos TkKeyword   keyword   keyword
--   pSpec_pos s        =   gsym_pos TkSymbol    [s]       [s]

--   pOParen_pos, pString_pos, pChar_pos, pInteger8_pos, pInteger10_pos, pInteger16_pos,
--      pVarid_pos, pConid_pos, pTextnm_pos, pTextln_pos, pInteger_pos
--                      ::  IsParser p Token => p FilePos
--   pOParen_pos        =   pSpec_pos '('
--
--   pString_pos        =   gsym_pos TkString    ""        "?STR?"
--   pChar_pos          =   gsym_pos TkChar      ""        "'chr'"
--   pInteger8_pos      =   gsym_pos TkInteger8  ""        "1"
--   pInteger10_pos     =   gsym_pos TkInteger10 ""        "1"
--   pInteger16_pos     =   gsym_pos TkInteger16 ""        "1"
--   pVarid_pos         =   gsym_pos TkVarid     ""        "?LC?"
--   pConid_pos         =   gsym_pos TkConid     ""        "?UC?"
--   pTextnm_pos        =   gsym_pos TkTextnm    ""        ""
--   pTextln_pos        =   gsym_pos TkTextln    ""        ""
--   pInteger_pos       =   pInteger10_pos

   pString_val_pos, pAtom_val_pos, pVarid_val_pos, pConid_val_pos
                      ::  IsParser p Token => p (String,FilePos)
--   pInteger10_val_pos =   gsym_val_pos TkInteger10 ""        "1"
   pString_val_pos    =   gsym_val_pos TkString    ""        "?STR?"
   pAtom_val_pos      =   gsym_val_pos TkAtom      ""        "?ATOM?"
--   pChar_val_pos      =   gsym_val_pos TkChar      ""        "'chr'"
   pVarid_val_pos     =   gsym_val_pos TkVarid     ""        "?LC?"
   pConid_val_pos     =   gsym_val_pos TkConid     ""        "?UC?"
--   pInteger_val_pos   =   pInteger10_val_pos

--   pParens_pos        ::  IsParser p Token => p a -> p (FilePos,a)
--   pParens_pos p      =   (,) <$> pOParen_pos <*> p <* pCParen


