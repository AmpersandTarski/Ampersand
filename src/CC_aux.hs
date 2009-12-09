{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS -XFlexibleContexts #-}
  module CC_aux ( pKey_pos
                , pString_val_pos
                , pVarid_val_pos, pConid_val_pos
                , renumberRules
                , Pop(..)
                , makeConceptSpace , pMeaning
                , anything, shSigns , gEtabG
                , conts , cod, clearG, dom
                , showFullRelName
   ) where
   import UU_Scanner
   import UU_Parsing
   import CommonClasses ( Identified(..)
                        , ABoolAlg(..)
                        , Conceptual(..)
                        , Morphics(..)
                        )
   import Collection   (Collection (..))
   import Strings      (chain,commaEng)
   import Auxiliaries  (rEncode
                       ,sord,eqCl,eqClass)
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
   pMeaning Aut   = "automatic if possible"


   renumberRule :: Int -> Rule -> Rule
   renumberRule n rule 
      = case rule of
          Ru{rrsrt = Automatic} -> rule{rrant = error ("(Module CC_aux:) illegal call to antecedent in renumberRule ("++showADL rule++")")
                                       ,runum = n
                                       }
          Ru{}                  -> rule{runum = n}
          Sg{}                  -> rule{srsig = renumberRule n (srsig rule)
                                       ,runum = n
                                       }
          Fr{}                  -> rule

   renumberRules :: Int -> [Rule] -> [Rule]
   renumberRules n (r:rs) = (renumberRule n r):renumberRules (n+1) rs
   renumberRules _ [] = []


   showFullRelName :: Declaration -> String
   showFullRelName decl = rEncode (name decl++name (source decl)++name (target decl))

   shSigns :: [(Concept,Concept)] -> String
   shSigns [(a,b)] = "["++show a++"*"++show b++"]"
   shSigns ss = commaEng "or" ["["++show a++"*"++show b++"]"|(a,b)<-ss]

   makeConceptSpace :: ditwordtnietgebruikt -> [Morphism] -> Concepts    --WAAROM is deze definitie goed?
   makeConceptSpace _ morphisms
    = [ upd (fst (head raw)) (sord (concat (map snd raw)))
      | raw <- eqCl fst [(c,os)| (Mph _ _ _ (_,_) _ sgn@(Sgn _ s t _ _ _ _ _ _ _ _ _ _)) <- morphisms
                               , (c,os) <- [(s,dom sgn),(t,cod sgn)]
                        ]
      ] where
         upd c os = case c of
                       C{} -> c{cptos=os}
                       _   -> c


   class Pop a where
    put_gE     :: GenR -> Concepts  -> a -> a
    specialize :: (Concept,Concept) -> a -> a
    update     :: [Declaration] -> a -> a
    update _ c = c

   instance Pop Concept where
    put_gE gE cs c = h (head ([c'|c'<-cs, c==c']++[c]))
            where h x = case x of
                         C{} ->  x{cptgE = gE}
                         _   ->  x
    specialize (a,b) c = if length (eqClass order [a,b,c])>1 then error ("!Fatal (module CC_aux 115): specialize 1 ("++show a++","++show b++") "++show c) else
                         (a `glb` b) `lub` c

   instance Pop KeyDef where
    put_gE gE cs kdef = kdef{ kdctx = put_gE gE cs (kdctx kdef)
                            , kdats = [put_gE gE cs a| a<-kdats kdef]
                            }
    update ss kdef    = kdef{ kdctx = update ss (kdctx kdef)
                            , kdats = [update ss    a| a<-kdats kdef]
                            }
    specialize t kdef = kdef{ kdctx = specialize t (kdctx kdef)
                            , kdats = [specialize t a| a<-kdats kdef]
                            }


   instance Pop ObjectDef where
    put_gE gE cs obj = obj { objctx = put_gE gE cs (objctx obj)
                           , objats = [put_gE gE cs a| a<-objats obj]
                           }
    update ss    obj = obj { objctx = update ss    (objctx obj)
                           , objats = [update ss    a| a<-objats obj]
                           }
    specialize t obj = obj { objctx = specialize t (objctx obj)
                           , objats = [specialize t a| a<-objats obj]
                           }


   instance (Pop a,Pop b) => Pop (a,b) where
    put_gE gE cs (x,y) = (put_gE gE cs x, put_gE gE cs y)
    update ss    (x,y) = (update ss    x, update ss    y)
    specialize t (x,y) = (specialize t x, specialize t y)


   instance Pop Gen where
    put_gE gE cs gen = gen { gengen = put_gE gE cs (gengen gen)
                           , genspc = put_gE gE cs (genspc gen)
                           }
    update ss    gen = gen { gengen = update ss    (gengen gen)
                           , genspc = update ss    (genspc gen)
                           }
    specialize t gen = gen { gengen = specialize t (gengen gen)
                           , genspc = specialize t (genspc gen)
                           }
   

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

      specialize t context
                  = context { ctxpats = map (specialize t) (ctxpats context)
                            , ctxrs   = map (specialize t) (ctxrs context)
                            , ctxds   = map (specialize t) (ctxds context)
                            , ctxks   = map (specialize t) (ctxks context)
                            , ctxos   = map (specialize t) (ctxos context)
                            }

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
    specialize t pat = pat { ptrls = map (specialize t) (ptrls pat)
                           , ptgns = map (specialize t) (ptgns pat)
                           , ptdcs = map (specialize t) (ptdcs pat)
                           , ptkds = map (specialize t) (ptkds pat)
                           }
    
   
   instance Pop Rule where
    put_gE gE cs rule 
       = case rule of
          Ru{} -> rule{ rrant = if rrsrt rule == Automatic 
                                 then error ("(Module CC_aux:) illegal call to antecedent in put_gE cs ("++showADL rule++")")
                                 else put_gE gE cs (rrant rule)
                      , rrcon = put_gE gE cs (rrcon rule)
                      , rrtyp = put_gE gE cs (rrtyp rule)
                      }
          Sg{} -> rule{ srsig = put_gE gE cs (srsig rule)
                      , srtyp = put_gE gE cs (srtyp rule)
                      }
          Fr{} -> rule{ frdec = put_gE gE cs (frdec rule)
                      , frcmp = put_gE gE cs (frcmp rule)
                      }
    update ss rule
       = case rule of 
          Ru{} -> rule{ rrant = if rrsrt rule == Automatic 
                                 then error ("(Module CC_aux:) illegal call to antecedent in update ss ("++showADL rule++")")
                                 else update ss (rrant rule)
                      , rrcon = update ss (rrcon rule)
                      , rrtyp = update ss (rrtyp rule)
                      }
          Sg{} -> rule{ srsig = update ss (srsig rule)
                      , srtyp = update ss (rrtyp rule)
                      }
          Fr{} -> rule{ frdec = update ss (frdec rule)
                      , frcmp = update ss (frcmp rule)
                      }
    specialize t rule
       = case rule of 
          Ru{} -> rule{ rrant = if rrsrt rule == Automatic 
                                 then error ("(Module CC_aux:) illegal call to antecedent in specialize t ("++showADL rule++")")
                                 else specialize t (rrant rule)
                      , rrcon = specialize t (rrcon rule)
                      , rrtyp = specialize t (rrtyp rule)
                      }
          Sg{} -> rule{ srsig = specialize t (srsig rule)
                      , srtyp = specialize t (rrtyp rule)
                      }
          Fr{} -> rule{ frdec = specialize t (frdec rule)
                      , frcmp = specialize t (frcmp rule)
                      }
          

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

    specialize t (Tm mph)       = Tm (specialize t mph)
    specialize t (Tc f)         = Tc (specialize t f)
    specialize (a,b) (F [])     = error ("(module CC_aux) specialize ("++show a++","++show b++") (F [])")
    specialize (a,b) (F [t])    = F [specialize (a,b) t]
    specialize (a,b) (F ts)     = F ([specialize (a,target h) h]++init (tail ts)++[specialize (source l,b) l])
                                  where h=head ts; l=last ts
    specialize (a,b) (Fd [])    = error ("(module CC_aux) specialize t@("++show a++","++show b++") (Fd [])")
    specialize (a,b) (Fd [t])   = Fd [specialize (a,b) t]
    specialize (a,b) (Fd ts)    = Fd ([specialize (a,target h) h]++init (tail ts)++[specialize (source l,b) l])
                                   where h=head ts; l=last ts
    specialize t (Fu fs)        = Fu (map (specialize t) fs) 
    specialize t (Fi fs)        = Fi (map (specialize t) fs) 
    specialize t (K0 e')        = K0 (specialize t e')
    specialize t (K1 e')        = K1 (specialize t e')
    specialize t (Cp e')        = Cp (specialize t e')






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
          Mp1{} -> undefined -- TODO WAAROM?  Stef, deze was niet gedefinieerd bij het verwijderen van warnings. Kijk jij hier nog even naar? 
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
          Mp1{} -> undefined -- TODO WAAROM?  Stef, deze was niet gedefinieerd bij het verwijderen van warnings. Kijk jij hier nog even naar? 
-- was:
--    update ss (Mph nm p atts sgn yin s)          = Mph nm p atts (update ss sgn) yin (update ss s)
--    update ss (I atts g s yin)                   = I (map (update ss) atts) (update ss g) (update ss s) yin
--    update ss (V atts (a,b))                     = V (map (update ss) atts) (update ss a, update ss b)

    specialize t@(a,b) mph
       = case mph of
          Mph{} -> mph{ mphats = if null (mphats mph) then [] else if (mphyin mph) then [a,b] else [b,a]  --TODO WAAROM?? Stef, ik heb de semantiek intact gelaten, maar ik geloof dat dit fout is. Zit er een volgorde in de atts, die relevant is? 
                      , mphtyp = t                                                                        --TODO WAAROM?? Stef, ook hier semantiek intact gelaten. Maar is het type niet afhankelijk van de yin??
                      , mphdcl = (specialize t) (mphdcl mph)
                      }
          I{}   -> mph{ mphgen = if (mphyin mph) then b else a
                      , mphspc = if (mphyin mph) then a else b
                      }
          V{}   -> mph{ mphtyp = t
                      }
          Mp1{} -> undefined -- TODO WAAROM?  Stef, deze was niet gedefinieerd bij het verwijderen van warnings. Kijk jij hier nog even naar? 
-- was:
--    specialize t@(a,b) (Mph nm p atts sgn yin s) = Mph nm p (if null atts then [] else if yin then [a,b] else [b,a]) t yin (specialize t s)
--    specialize t@(a,b) (I atts g s yin)          = if yin then I atts b a yin else I atts a b yin
--    specialize t@(a,b) (V atts (a',b'))          = V atts (a,b)

   instance Pop Declaration where
    put_gE gE cs decl 
       = case decl of
          Sgn{}    -> decl{ desrc = put_gE gE cs (desrc decl)
                          , detgt = put_gE gE cs (detgt decl)
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
    update ss decl
       = case decl of
          Sgn{}    -> head ([c|c<-ss, decl==c]++[decl])
          Isn{}    -> decl
          Iscompl{}-> decl
          Vs{}     -> decl
    specialize (x,y) decl
       = case decl of
          Sgn{}    -> decl{ desrc = x
                          , detgt = y
                          , decpopu = [p|p<-(decpopu decl),(srcPaire p) `elem` conts (desrc decl), trgPaire p `elem` conts (detgt decl)]
                          }
          Isn{}    -> if x <= y 
                       then decl{degen = x
                                ,despc = y
                                }
                       else error ("!Fatal (module CC_aux): specialize 7 "++show (x,y)++show (despc decl))
          Iscompl{}-> if x <= y 
                       then decl{degen = x
                                ,despc = y
                                }
                       else error ("!Fatal (module CC_aux): specialize 7 "++show (x,y)++show (despc decl))
          Vs{}     -> undefined -- TODO WAAROM?  Stef, deze was niet gedefinieerd bij het verwijderen van warnings. Kijk jij hier nog even naar? 
                    



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
   clearG gens = rd [g| g<-gens, (gengen g)/=(genspc g)]







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

   pString_val_pos, pVarid_val_pos, pConid_val_pos
                      ::  IsParser p Token => p (String,FilePos)
--   pInteger10_val_pos =   gsym_val_pos TkInteger10 ""        "1"
   pString_val_pos    =   gsym_val_pos TkString    ""        "?STR?"
--   pChar_val_pos      =   gsym_val_pos TkChar      ""        "'chr'"
   pVarid_val_pos     =   gsym_val_pos TkVarid     ""        "?LC?"
   pConid_val_pos     =   gsym_val_pos TkConid     ""        "?UC?"
--   pInteger_val_pos   =   pInteger10_val_pos

--   pParens_pos        ::  IsParser p Token => p a -> p (FilePos,a)
--   pParens_pos p      =   (,) <$> pOParen_pos <*> p <* pCParen


